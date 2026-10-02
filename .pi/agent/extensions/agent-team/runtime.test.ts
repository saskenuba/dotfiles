import assert from "node:assert/strict";
import { test } from "node:test";
import { mkdtemp, rm, stat, readFile, appendFile, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import type { JsonAgentSessionEvent, RpcClientOptions } from "@earendil-works/pi-coding-agent";
import { TeamRuntime, type WorkerClient, type TeamRuntimeOptions } from "./runtime.js";
import { MAX_AGENTS, MAX_HISTORY, MAX_MESSAGE_CHARS, MAX_PENDING_INSTRUCTIONS, MAX_PENDING_ACKS, MAX_DEDUPE_IDS, WORKER_MESSAGE_TOOL, type TeamMessage, type AgentRecord } from "./protocol.js";

function deferred() {
  let resolve!: () => void;
  const promise = new Promise<void>(r => { resolve = r; });
  return { promise, resolve };
}
const tick = () => new Promise<void>(r => setImmediate(r));
async function until(predicate: () => boolean) {
  for (let i = 0; i < 100; i++) { if (predicate()) return; await new Promise(r => setTimeout(r, 5)); }
  assert.fail("condition did not become true");
}

class FakeClient implements WorkerClient {
  listeners = new Set<(event: JsonAgentSessionEvent) => void>();
  prompts: { text: string; mode?: string }[] = [];
  conversation: string[] = [];
  queue: string[] = [];
  calls: string[] = [];
  running = false;
  dead = false;
  stops = 0;
  parent?: string;
  startGate?: Promise<void>;
  startError?: Error;
  promptError?: Error;
  handled = false;
  onPrompt?: (text: string) => void | Promise<void>;
  sessionFile: string;
  constructor(readonly options: RpcClientOptions) {
    this.sessionFile = join(options.args![options.args!.indexOf("--session-dir") + 1], "fake-session.jsonl");
  }
  async start() {
    this.calls.push("start");
    await this.startGate;
    if (this.startError) throw this.startError;
    await writeFile(this.sessionFile, "{\"type\":\"session\"}\n", { mode: 0o600 });
  }
  async stop() { this.calls.push("stop"); this.stops++; this.dead = true; }
  onEvent(listener: (event: JsonAgentSessionEvent) => void) { this.listeners.add(listener); return () => this.listeners.delete(listener); }
  emit(event: unknown) { for (const listener of [...this.listeners]) listener(event as JsonAgentSessionEvent); }
  async getState() {
    this.calls.push("state");
    if (this.dead) throw new Error("unexpected child exit");
    return { sessionFile: this.sessionFile };
  }
  async newSession(parent?: string) { this.calls.push("newSession"); this.parent = parent; return { cancelled: false }; }
  async clearQueue() { this.calls.push("clearQueue"); this.queue = []; }
  async abort() {
    this.calls.push("abort");
    if (this.running) this.finish("stale aborted answer", "aborted");
  }
  async prompt(text: string, _images?: unknown, mode?: "steer" | "followUp"): Promise<"started" | "queued" | "handled"> {
    this.calls.push("prompt"); this.prompts.push({ text, mode });
    if (this.promptError) throw this.promptError;
    if (this.handled) return "handled";
    if (this.onPrompt) { await this.onPrompt(text); return "started"; }
    if (this.running) { this.queue.push(text); return "queued"; }
    this.consume(text);
    return "started";
  }
  consume(text: string) {
    this.running = true;
    this.conversation.push(text);
    void appendFile(this.sessionFile, `${JSON.stringify({ role: "user", content: text })}\n`).catch(() => {});
    this.emit({ type: "agent_start" });
    this.emit({ type: "message_end", message: { role: "user", content: text } });
  }
  assistant(text: string, stopReason = "stop") {
    this.emit({ type: "message_end", message: { role: "assistant", content: [{ type: "text", text }], stopReason,
      errorMessage: stopReason === "error" ? "provider failure" : undefined } });
  }
  finish(text: string, reason = "stop") {
    this.assistant(text, reason);
    this.emit({ type: "agent_end", messages: [], willRetry: false });
    if (this.queue.length) { this.consume(this.queue.shift()!); return; }
    this.running = false;
    this.emit({ type: "agent_settled" });
  }
  message(record: AgentRecord, kind: TeamMessage["kind"], overrides: Partial<TeamMessage> = {}, wait = false) {
    const m: TeamMessage = { version: 1, id: `worker-${Math.random()}`, agentId: record.id, assignmentId: record.assignmentId,
      direction: "to-orchestrator", kind, text: "worker says", createdAt: Date.now(), ...overrides };
    this.emit({ type: "tool_execution_end", toolName: WORKER_MESSAGE_TOOL, result: { details: { teamMessage: m, wait } }, isError: false });
    return m;
  }
}

async function fixture(overrides: Partial<TeamRuntimeOptions> = {}, configure?: (client: FakeClient) => void) {
  const dir = await mkdtemp(join(tmpdir(), "team-runtime-test-"));
  const clients: FakeClient[] = [];
  const messages: TeamMessage[] = [];
  const runtime = new TeamRuntime({
    cliPath: "/fake/cli.js", workerExtensionPath: "/fake/worker.ts", storageDir: dir,
    onUpdate() {}, onMessage(m) { messages.push(m); },
    clientFactory(options) { const client = new FakeClient(options); configure?.(client); clients.push(client); return client; },
    commandTimeoutMs: 200, probeIntervalMs: 60000, ...overrides,
  });
  const start = (name = "worker", extra = {}) => runtime.start({ name, persona: "reviewer", systemPrompt: "Review carefully",
    task: "Check implementation", context: "Curated context only", cwd: dir, ...extra });
  const dispose = async () => { await runtime.shutdown(); await tick(); await rm(dir, { recursive: true, force: true }); };
  return { dir, runtime, clients, messages, start, dispose };
}

test("private file-backed sessions, explicit CLI/extension, lineage, and independent IDs", async () => {
  const f = await fixture();
  try {
    const r = await f.start("reviewer", { parentSession: "/parent.jsonl", model: "test/model", thinking: "high", tools: ["read"] });
    const c = f.clients[0];
    assert.equal(c.options.cliPath, "/fake/cli.js");
    assert.equal(c.options.env?.PI_AGENT_TEAM_WORKER, "1");
    assert.equal(c.options.env?.PI_AGENT_TEAM_AGENT_ID, r.id);
    assert.ok(Number(c.options.env?.PI_SUBAGENT_DEPTH) >= 1);
    assert.ok(c.options.args?.includes("--no-extensions"));
    assert.ok(c.options.args?.includes("/fake/worker.ts"));
    assert.ok(!c.options.args?.includes("--no-session"));
    assert.ok(c.options.args?.includes(`read,${WORKER_MESSAGE_TOOL}`));
    assert.equal(c.parent, "/parent.jsonl");
    assert.ok(c.calls.indexOf("newSession") < c.calls.indexOf("prompt"));
    assert.equal((await stat(join(f.dir, r.id))).mode & 0o777, 0o700);
    assert.match(await readFile(r.sessionFile!, "utf8"), /session/);
    assert.match(c.prompts[0].text, /Curated initial task:[\s\S]*Curated context:/);
    assert.match(c.prompts[0].text, /Coordinator instructions/);
    assert.equal(new Set([r.id, r.assignmentId, r.messages[0].id]).size, 3);
  } finally { await f.dispose(); }
});

test("conversation and process survive settlement; same assignment until explicitly replaced", async () => {
  const f = await fixture();
  try {
    const r = await f.start(); const c = f.clients[0];
    c.finish("first answer");
    assert.equal(f.runtime.status(r.id)[0].state, "idle");
    const second = await f.runtime.send({ agentId: r.id, message: "Explain further" });
    c.finish("second answer");
    assert.equal(second.assignmentId, r.assignmentId);
    assert.equal(c.conversation.length, 2);
    assert.equal(c.stops, 0);
    const third = await f.runtime.send({ agentId: r.id, message: "New job", newAssignment: true });
    assert.notEqual(third.assignmentId, r.assignmentId);
    assert.equal(f.runtime.status(r.id)[0].lastOutput, undefined);
    c.finish("new job answer");
    assert.deepEqual(f.messages.filter(m => m.kind === "result").map(m => m.text), ["first answer", "second answer", "new job answer"]);
  } finally { await f.dispose(); }
});

test("send during a run uses prompt with default steer or selectable followUp, never old output", async () => {
  const f = await fixture();
  try {
    const r = await f.start(); const c = f.clients[0];
    const m = await f.runtime.send({ agentId: r.id, message: "Steer me" });
    assert.equal(m.delivery, "accepted");
    assert.equal(c.prompts[1].mode, "steer");
    await f.runtime.send({ agentId: r.id, message: "Then do this", mode: "followUp" });
    assert.equal(c.prompts[2].mode, "followUp");
    c.finish("old output"); c.finish("steered output");
    assert.equal(f.messages.filter(m => m.kind === "result").length, 0);
    c.finish("latest authoritative output");
    assert.equal(f.messages.at(-1)?.text, "latest authoritative output");
    assert.equal(f.runtime.status(r.id)[0].state, "idle");
  } finally { await f.dispose(); }
});

test("wait question blocks, replyTo is mandatory, reply drains old abort before new work", async () => {
  const f = await fixture();
  try {
    const r = await f.start(); const c = f.clients[0];
    const queued = await f.runtime.send({ agentId: r.id, message: "Queued instruction" });
    const q = c.message(r, "question", { text: "Which file?" }, true);
    assert.equal(f.runtime.status(r.id)[0].state, "blocked");
    await assert.rejects(f.runtime.send({ agentId: r.id, message: "No reply ID" }), /matching replyTo/);
    await assert.rejects(f.runtime.send({ agentId: r.id, message: "Wrong", replyTo: "wrong" }), /pending question/);
    await assert.rejects(f.runtime.send({ agentId: r.id, message: "Abandon", newAssignment: true, replyTo: q.id }), /idle/);
    const reply = await f.runtime.send({ agentId: r.id, message: "runtime.ts", replyTo: q.id, requiresAck: true });
    assert.equal(reply.delivery, "accepted");
    assert.equal(f.runtime.status(r.id)[0].pendingQuestionId, undefined);
    assert.equal(f.runtime.status(r.id)[0].messages.find(m => m.id === queued.id)?.delivery, "failed");
    assert.equal(f.messages.filter(m => m.kind === "result" || m.kind === "error").length, 0);
    assert.deepEqual(c.calls.slice(-3), ["clearQueue", "abort", "prompt"]);
    assert.match(c.prompts.at(-1)!.text, new RegExp(`replyTo=${q.id} requiresAck=true`));
    c.finish("resolved answer");
    assert.equal(f.messages.at(-1)?.text, "resolved answer");
  } finally { await f.dispose(); }
});

test("blocked settled never publishes result or fails on bridge abort; question remains pending", async () => {
  const f = await fixture();
  try {
    const r = await f.start(); const c = f.clients[0];
    const q = c.message(r, "blocker", {}, true);
    c.finish("aborted", "aborted");
    assert.equal(f.runtime.status(r.id)[0].state, "blocked");
    assert.equal(f.runtime.status(r.id)[0].pendingQuestionId, q.id);
    assert.equal(f.messages.length, 1);
    await f.runtime.send({ agentId: r.id, message: "Proceed", replyTo: q.id });
    c.finish("done");
    assert.equal(f.runtime.status(r.id)[0].state, "idle");
  } finally { await f.dispose(); }
});

test("nonwaiting question survives idle and ordinary instructions until matching reply", async () => {
  const f = await fixture();
  try {
    const r = await f.start(); const c = f.clients[0];
    const q = c.message(r, "question");
    c.finish("partial findings");
    await assert.rejects(f.runtime.send({ agentId: r.id, message: "New job", newAssignment: true }), /pending question/);
    await f.runtime.send({ agentId: r.id, message: "More progress" });
    assert.equal(f.runtime.status(r.id)[0].pendingQuestionId, q.id);
    c.finish("more progress");
    await f.runtime.send({ agentId: r.id, message: "Answer", replyTo: q.id });
    assert.equal(f.runtime.status(r.id)[0].pendingQuestionId, undefined);
  } finally { await f.dispose(); }
});

test("accepted is not acknowledged; ack identity, assignment, replyTo and dedupe are validated", async () => {
  const f = await fixture();
  try {
    const r = await f.start(); const c = f.clients[0];
    const m = await f.runtime.send({ agentId: r.id, message: "Acknowledge this", requiresAck: true });
    assert.equal(m.delivery, "accepted");
    c.message(r, "ack", { replyTo: m.id, agentId: "imposter" });
    c.message(r, "ack", { replyTo: m.id, assignmentId: "wrong" });
    c.message(r, "ack", { replyTo: "unknown" });
    c.message(r, "finding", { text: " " });
    c.message(r, "finding", { text: "x".repeat(MAX_MESSAGE_CHARS + 1) });
    assert.equal(f.messages.length, 0);
    const ack = c.message(r, "ack", { id: "ack-id", replyTo: m.id });
    c.message(r, "ack", ack);
    assert.equal(f.messages.length, 1);
    assert.equal(f.runtime.status(r.id)[0].messages.find(item => item.id === m.id)?.delivery, "acknowledged");
  } finally { await f.dispose(); }
});

test("fast settlement and ack before prompt response are not overwritten by acceptance", async () => {
  const f = await fixture({}, c => {
    c.onPrompt = text => {
      c.consume(text);
      const record = f.runtime.status()[0];
      const id = /messageId=([^ ]+)/.exec(text)![1];
      c.message(record, "ack", { replyTo: id });
      c.finish("fast answer");
    };
  });
  try {
    const r = await f.start();
    assert.equal(r.state, "idle");
    assert.equal(r.messages[0].delivery, "acknowledged");
    const sent = await f.runtime.send({ agentId: r.id, message: "Again" });
    assert.equal(sent.delivery, "acknowledged");
    assert.equal(f.runtime.status(r.id)[0].state, "idle");
    assert.equal(f.messages.filter(m => m.kind === "result").length, 2);
  } finally { await f.dispose(); }
});

test("idle/running race cannot report old output while prompt is in flight", async () => {
  const f = await fixture();
  try {
    const r = await f.start(); const c = f.clients[0]; const gate = deferred();
    c.onPrompt = async text => { c.finish("old run"); await gate.promise; c.consume(text); c.finish("new run"); };
    const sent = f.runtime.send({ agentId: r.id, message: "race" });
    await tick();
    assert.equal(f.messages.filter(m => m.kind === "result").length, 0);
    gate.resolve(); await sent;
    assert.deepEqual(f.messages.filter(m => m.kind === "result").map(m => m.text), ["new run"]);
  } finally { await f.dispose(); }
});

test("handled disposition does not leave phantom running state or reuse old assistant text", async () => {
  const f = await fixture({}, c => { c.handled = true; });
  try {
    const r = await f.start();
    assert.equal(r.state, "idle");
    assert.equal(f.messages.length, 0);
    await f.runtime.send({ agentId: r.id, message: "Handled" });
    f.clients[0].emit({ type: "agent_settled" });
    assert.equal(f.messages.length, 0);
    assert.equal(f.runtime.status(r.id)[0].state, "idle");
  } finally { await f.dispose(); }
});

for (const reason of ["error", "aborted"]) test(`${reason} stopReason is failure, not success`, async () => {
  const f = await fixture();
  try {
    const r = await f.start();
    f.clients[0].finish("must not be a result", reason);
    assert.equal(f.runtime.status(r.id)[0].state, "failed");
    assert.equal(f.messages.at(-1)?.kind, "error");
    assert.equal(f.messages.filter(m => m.kind === "result").length, 0);
    await until(() => f.clients[0].stops === 1);
  } finally { await f.dispose(); }
});

test("retry success replaces a transient failure; low-level agent_end is not settlement", async () => {
  const f = await fixture();
  try {
    const r = await f.start(); const c = f.clients[0];
    c.assistant("failure", "error");
    c.emit({ type: "agent_end", messages: [], willRetry: true });
    assert.equal(f.runtime.status(r.id)[0].state, "running");
    c.finish("recovered");
    assert.equal(f.messages.at(-1)?.kind, "result");
    assert.equal(f.messages.at(-1)?.text, "recovered");
  } finally { await f.dispose(); }
});

test("close suppresses all late events, clears queue before abort, and is idempotent", async () => {
  const f = await fixture();
  try {
    const r = await f.start(); const c = f.clients[0];
    await f.runtime.send({ agentId: r.id, message: "queued" });
    const closing = f.runtime.close(r.id);
    c.message(r, "question", {}, true); c.finish("late result");
    await Promise.all([closing, f.runtime.close(r.id)]);
    await f.runtime.close(r.id);
    assert.equal(f.runtime.status(r.id)[0].state, "closed");
    assert.equal(f.messages.length, 0);
    assert.equal(c.stops, 1);
    assert.deepEqual(c.calls.slice(-3), ["clearQueue", "abort", "stop"]);
    await assert.rejects(f.runtime.send({ agentId: r.id, message: "late" }), /live worker/);
    assert.ok((await stat(r.sessionFile!)).isFile());
  } finally { await f.dispose(); }
});

test("capacity counts concurrent startups and names are unique", async () => {
  const gate = deferred(); const f = await fixture({}, c => { c.startGate = gate.promise; });
  try {
    const starts = Array.from({ length: MAX_AGENTS }, (_, i) => f.start(`worker-${i}`));
    await assert.rejects(f.start("WORKER-0"), /name/);
    await assert.rejects(f.start("overflow"), /live agents/);
    gate.resolve(); const agents = await Promise.all(starts);
    assert.equal(f.clients.length, MAX_AGENTS);
    await f.runtime.close(agents[0].id);
    await f.start("replacement");
    assert.equal(f.clients.length, MAX_AGENTS + 1);
  } finally { gate.resolve(); await f.dispose(); }
});

test("startup failure cleans up once; shutdown during startup suppresses initial prompt", async () => {
  const failed = await fixture({}, c => { c.startError = new Error("startup failure"); });
  try {
    await assert.rejects(failed.start(), /startup failure/);
    assert.equal(failed.runtime.status()[0].state, "failed");
    assert.equal(failed.clients[0].stops, 1);
    await failed.runtime.shutdown();
    assert.equal(failed.clients[0].stops, 1);
  } finally { await failed.dispose(); }
  const gate = deferred(); const f = await fixture({}, c => { c.startGate = gate.promise; });
  try {
    const starting = f.start(); const rejected = assert.rejects(starting, /not live/);
    await until(() => f.clients.length === 1);
    const shutdown = f.runtime.shutdown();
    gate.resolve(); await rejected; await shutdown;
    assert.equal(f.clients[0].prompts.length, 0);
    assert.equal(f.clients[0].stops, 1);
    await assert.rejects(f.start("after shutdown"), /shut down/);
  } finally { gate.resolve(); await f.dispose(); }
});

test("unexpected exit is detected while idle by bounded low-frequency state probe", async () => {
  const f = await fixture({ probeIntervalMs: 5 });
  try {
    const r = await f.start(); f.clients[0].finish("done");
    f.clients[0].dead = true;
    await until(() => f.runtime.status(r.id)[0].state === "failed");
    assert.match(f.runtime.status(r.id)[0].error!, /unexpected child exit/);
    await until(() => f.clients[0].stops === 1);
  } finally { await f.dispose(); }
});

test("restore interrupts live records, retains history/session, never restarts, and returns copies", async () => {
  const f = await fixture(); const restored = await fixture();
  try {
    const r = await f.start(); const snapshot = f.runtime.snapshot();
    await assert.rejects(async () => f.runtime.restore(snapshot), /workers exist/);
    restored.runtime.restore(snapshot);
    assert.equal(restored.runtime.status(r.id)[0].state, "interrupted");
    assert.equal(restored.runtime.status(r.id)[0].sessionFile, r.sessionFile);
    assert.equal(restored.clients.length, 0);
    await assert.rejects(restored.runtime.send({ agentId: r.id, message: "Resume" }), /live worker/);
    snapshot.agents[0].name = "mutated";
    const status = restored.runtime.status(); status[0].messages.length = 0;
    assert.equal(restored.runtime.status(r.id)[0].name, "worker");
    assert.equal(restored.runtime.status(r.id)[0].messages.length, 1);
    await restored.runtime.close(r.id);
    assert.equal(restored.runtime.status(r.id)[0].state, "closed");
  } finally { await f.dispose(); await restored.dispose(); }
});

test("history/output limits and invalid input", async () => {
  const f = await fixture();
  try {
    await assert.rejects(f.start(" "), /nonempty/);
    await assert.rejects(f.start("a", { task: "x".repeat(MAX_MESSAGE_CHARS + 1) }), /at most/);
    const r = await f.start(); const c = f.clients[0];
    await assert.rejects(f.runtime.send({ agentId: r.id, message: " " }), /nonempty/);
    await assert.rejects(f.runtime.send({ agentId: r.id, message: "new", newAssignment: true }), /idle/);
    for (let i = 0; i < MAX_HISTORY + 10; i++) c.message(r, "progress", { id: `progress-${i}` });
    c.finish("x".repeat(MAX_MESSAGE_CHARS + 100));
    const status = f.runtime.status(r.id)[0];
    assert.equal(status.messages.length, MAX_HISTORY);
    assert.equal(status.lastOutput?.length, MAX_MESSAGE_CHARS);
    assert.equal(f.messages.at(-1)?.text.length, MAX_MESSAGE_CHARS);
  } finally { await f.dispose(); }
});

test("per-agent sends serialize without serializing different agents; callback errors are contained", async () => {
  const f = await fixture({ onUpdate() { throw new Error("UI error"); }, onMessage: async () => { throw new Error("async UI error"); } });
  try {
    const r = await f.start(); const other = await f.start("other"); const c = f.clients[0]; const gate = deferred();
    c.onPrompt = async text => { await gate.promise; c.consume(text); };
    const first = f.runtime.send({ agentId: r.id, message: "first" });
    const second = f.runtime.send({ agentId: r.id, message: "second" });
    await tick(); assert.equal(c.prompts.length, 2);
    await f.runtime.send({ agentId: other.id, message: "independent" });
    gate.resolve(); await Promise.all([first, second]);
    assert.equal(c.prompts.length, 3);
    c.finish("done"); await tick();
  } finally { await f.dispose(); }
});

test("reentrant update can send during startup without overtaking worker initialization", async () => {
  let sent: Promise<TeamMessage> | undefined;
  const f = await fixture({ onUpdate(snapshot) {
    if (!sent && snapshot.agents[0]?.state === "starting") {
      sent = f.runtime.send({ agentId: snapshot.agents[0].id, message: "Send from UI callback" });
    }
  } });
  try {
    await f.start(); await sent;
    assert.equal(f.clients[0].prompts.length, 2);
    assert.equal(f.runtime.status()[0].state, "running");
  } finally { await f.dispose(); }
});

test("callback-triggered close is reentrant and suppresses message publication", async () => {
  let closing: Promise<void> | undefined;
  const f = await fixture({ onUpdate(snapshot) {
    const r = snapshot.agents[0];
    if (r && (r.state === "blocked" || r.state === "closed")) closing = f.runtime.close(r.id);
  } });
  try {
    const r = await f.start();
    f.clients[0].message(r, "question", {}, true);
    await closing;
    assert.equal(f.messages.length, 0);
    assert.equal(f.clients[0].stops, 1);
  } finally { await f.dispose(); }
});

test("close while prompt acceptance is in flight rejects submission and suppresses late output", async () => {
  const f = await fixture(); const gate = deferred();
  try {
    const r = await f.start(); const c = f.clients[0];
    c.onPrompt = async text => { await gate.promise; c.consume(text); c.finish("late answer"); };
    const sending = f.runtime.send({ agentId: r.id, message: "in flight" });
    const rejected = assert.rejects(sending, /closed/);
    await tick(); const closing = f.runtime.close(r.id);
    gate.resolve(); await rejected; await closing;
    assert.equal(f.messages.length, 0);
    assert.equal(c.stops, 1);
    assert.equal(f.runtime.status(r.id)[0].state, "closed");
  } finally { gate.resolve(); await f.dispose(); }
});

test("rejected blocked reply preserves the pending question without resurrecting a failed worker", async () => {
  const f = await fixture();
  try {
    const r = await f.start(); const c = f.clients[0];
    const q = c.message(r, "question", {}, true); c.finish("waiting");
    c.promptError = new Error("transport rejected");
    await assert.rejects(f.runtime.send({ agentId: r.id, message: "answer", replyTo: q.id }), /transport rejected/);
    assert.equal(f.runtime.status(r.id)[0].pendingQuestionId, q.id);
    assert.equal(f.runtime.status(r.id)[0].state, "failed");
    assert.equal(f.runtime.status(r.id)[0].messages.at(-2)?.delivery, "failed");
    await until(() => c.stops === 1);
  } finally { await f.dispose(); }
});

test("fast new question during reply is not cleared by acceptance of the previous reply", async () => {
  const f = await fixture();
  try {
    const r = await f.start(); const c = f.clients[0];
    const first = c.message(r, "question", {}, true); c.finish("waiting");
    c.onPrompt = text => {
      c.consume(text);
      c.message(r, "question", { id: "next-question" }, true);
      c.finish("waiting again", "aborted");
    };
    await f.runtime.send({ agentId: r.id, message: "first answer", replyTo: first.id });
    assert.equal(f.runtime.status(r.id)[0].pendingQuestionId, "next-question");
    assert.equal(f.runtime.status(r.id)[0].state, "blocked");
    assert.equal(f.messages.filter(m => m.kind === "result" || m.kind === "error").length, 0);
  } finally { await f.dispose(); }
});

test("later wait notices block without silently replacing the pending reply target", async () => {
  const f = await fixture();
  try {
    const r = await f.start(); const c = f.clients[0];
    const first = c.message(r, "question");
    c.message(r, "blocker", { id: "second-question" }, true);
    c.finish("not a result");
    assert.equal(f.runtime.status(r.id)[0].state, "blocked");
    assert.equal(f.runtime.status(r.id)[0].pendingQuestionId, first.id);
    assert.equal(f.messages.length, 2);
    await f.runtime.send({ agentId: r.id, message: "Answer both questions", replyTo: first.id });
    assert.equal(f.runtime.status(r.id)[0].pendingQuestionId, undefined);
  } finally { await f.dispose(); }
});

test("handled prompt racing an old settlement leaves the worker idle", async () => {
  const f = await fixture();
  try {
    const r = await f.start(); const c = f.clients[0];
    c.prompt = async () => { c.finish("old answer"); return "handled"; };
    await f.runtime.send({ agentId: r.id, message: "Handled at the settle boundary" });
    assert.equal(f.runtime.status(r.id)[0].state, "idle");
    assert.equal(f.messages.filter(m => m.kind === "result").length, 0);
  } finally { await f.dispose(); }
});

test("a hung health probe has a bounded deadline and triggers cleanup", async () => {
  const f = await fixture({ probeIntervalMs: 5, commandTimeoutMs: 20 });
  try {
    const r = await f.start(); const c = f.clients[0]; c.finish("done");
    c.getState = () => new Promise(() => {});
    await until(() => f.runtime.status(r.id)[0].state === "failed");
    assert.match(f.runtime.status(r.id)[0].error!, /timed out/);
    await until(() => c.stops === 1);
  } finally { await f.dispose(); }
});

test("queued instruction capacity rejects excess locally without failing the worker", async () => {
  const f = await fixture();
  try {
    const r = await f.start();
    for (let i = 0; i < MAX_PENDING_INSTRUCTIONS; i++) await f.runtime.send({ agentId: r.id, message: `Queued ${i}` });
    await assert.rejects(f.runtime.send({ agentId: r.id, message: "Overflow" }), /queued instructions/);
    assert.equal(f.clients[0].prompts.length, MAX_PENDING_INSTRUCTIONS + 1);
    assert.equal(f.runtime.status(r.id)[0].state, "running");
    assert.equal(f.messages.filter(m => m.kind === "error").length, 0);
  } finally { await f.dispose(); }
});

test("valid required acknowledgment survives eviction of the original instruction", async () => {
  const f = await fixture();
  try {
    const r = await f.start(); const c = f.clients[0];
    const instruction = await f.runtime.send({ agentId: r.id, message: "Important correction", requiresAck: true });
    c.finish("initial run");
    for (let i = 0; i <= MAX_HISTORY; i++) c.message(r, "progress", { id: `eviction-${i}` });
    assert.equal(f.runtime.status(r.id)[0].messages.some(m => m.id === instruction.id), false);
    assert.equal(f.runtime.status(r.id)[0].pendingAcknowledgments?.[0].id, instruction.id);
    c.message(r, "ack", { replyTo: instruction.id, id: "valid-late-ack" });
    assert.equal(f.messages.at(-1)?.kind, "ack");
    assert.equal(f.messages.at(-1)?.replyTo, instruction.id);
    assert.deepEqual(f.runtime.status(r.id)[0].pendingAcknowledgments, []);
  } finally { await f.dispose(); }
});

test("acknowledgment capacity is bounded and releases on acknowledgment, not history eviction", async () => {
  const f = await fixture();
  try {
    const r = await f.start(); const c = f.clients[0]; c.finish("initial");
    const instructions: TeamMessage[] = [];
    for (let i = 0; i < MAX_PENDING_ACKS; i++) {
      instructions.push(await f.runtime.send({ agentId: r.id, message: `Confirm ${i}`, requiresAck: true }));
      c.finish(`Finished ${i} without acknowledgment`);
    }
    await assert.rejects(f.runtime.send({ agentId: r.id, message: "Overflow", requiresAck: true }), /pending acknowledgments/);
    await assert.rejects(f.runtime.send({ agentId: r.id, message: "New task", newAssignment: true }), /pending acknowledgments/);
    assert.equal(f.runtime.status(r.id)[0].state, "idle");
    assert.equal(f.runtime.status(r.id)[0].pendingAcknowledgments?.length, MAX_PENDING_ACKS);
    c.message(r, "ack", { replyTo: instructions[0].id });
    await f.runtime.send({ agentId: r.id, message: "Capacity released", requiresAck: true });
    assert.equal(f.runtime.status(r.id)[0].pendingAcknowledgments?.length, MAX_PENDING_ACKS);
    await f.runtime.close(r.id);
    assert.deepEqual(f.runtime.status(r.id)[0].pendingAcknowledgments, []);
  } finally { await f.dispose(); }
});

test("deduplication storage is bounded and resets on new assignment", async () => {
  const f = await fixture();
  try {
    const r = await f.start(); const c = f.clients[0];
    for (let i = 0; i < MAX_DEDUPE_IDS + 10; i++) c.message(r, "progress", { id: `seen-${i}` });
    const internal = (f.runtime as unknown as { workers: Map<string, { seen: Set<string> }> }).workers.get(r.id)!;
    assert.equal(internal.seen.size, MAX_DEDUPE_IDS);
    c.message(r, "progress", { id: `seen-${MAX_DEDUPE_IDS + 9}` });
    assert.equal(internal.seen.size, MAX_DEDUPE_IDS);
    c.finish("done");
    await f.runtime.send({ agentId: r.id, message: "New task", newAssignment: true });
    assert.equal(internal.seen.size, 0);
    c.message(f.runtime.status(r.id)[0], "finding", { id: `seen-${MAX_DEDUPE_IDS + 9}` });
    assert.equal(f.messages.at(-1)?.kind, "finding");
  } finally { await f.dispose(); }
});

test("restore explicitly cancels outstanding acknowledgments rather than awaiting dead workers", async () => {
  const f = await fixture(); const restored = await fixture();
  try {
    const r = await f.start();
    const instruction = await f.runtime.send({ agentId: r.id, message: "Confirm", requiresAck: true });
    restored.runtime.restore(f.runtime.snapshot());
    const record = restored.runtime.status(r.id)[0];
    assert.equal(record.state, "interrupted");
    assert.deepEqual(record.pendingAcknowledgments, []);
    assert.equal(record.messages.find(m => m.id === instruction.id)?.delivery, "failed");
  } finally { await f.dispose(); await restored.dispose(); }
});
