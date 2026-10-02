import assert from "node:assert/strict";
import test from "node:test";
import { mkdtemp, mkdir, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { getPackageDir, RpcClient, type ExtensionAPI, type ExtensionContext } from "@earendil-works/pi-coding-agent";
import { registerAgentTeam } from "./index.js";
import type { TeamRuntime, TeamRuntimeOptions } from "./runtime.js";
import { TEAM_MESSAGE_TYPE, TEAM_STATE_TYPE, type AgentRecord, type StartOptions, type TeamMessage, type TeamSnapshot } from "./protocol.js";

async function fixture(mode = "tui", initial?: TeamSnapshot) {
  const handlers = new Map<string, (...args: any[]) => any>();
  const tools = new Map<string, any>();
  const commands = new Map<string, any>();
  const sent: any[] = [], entries: any[] = [], status: any[] = [], notices: any[] = [];
  let branch: any[] = initial ? [{ type: "custom", customType: TEAM_STATE_TYPE, data: initial }] : [];
  const instances: any[] = [];
  const pi = {
    on: (name: string, handler: any) => { handlers.set(name, handler); },
    registerTool: (tool: any) => tools.set(tool.name, tool), registerCommand: (name: string, command: any) => commands.set(name, command),
    getAllTools: () => ["read", "bash", "edit", "write"].map((name) => ({ name, sourceInfo: { source: "builtin" } })),
    getActiveTools: () => ["read", "edit", "subagent", "agent_send"], getThinkingLevel: () => "medium",
    appendEntry: (customType: string, data: unknown) => { entries.push({ customType, data }); },
    sendMessage: (message: unknown, options: unknown) => sent.push({ message, options }), events: { emit() {} },
  } as unknown as ExtensionAPI;
  const ctx = { mode, hasUI: mode === "tui", cwd: "/tmp", model: { provider: "test", id: "model" },
    ui: { setStatus: (...args: any[]) => status.push(args), notify: (...args: any[]) => notices.push(args), confirm: async () => false },
    sessionManager: { getSessionId: () => "parent-test", getSessionFile: () => "/tmp/parent-test.jsonl", getBranch: () => branch },
  } as unknown as ExtensionContext;
  registerAgentTeam(pi, (options: TeamRuntimeOptions) => {
    const records: AgentRecord[] = [];
    const instance = {
      options, records, starts: [] as StartOptions[], sends: [] as any[], closed: [] as string[], shutdownCount: 0,
      snapshot: (): TeamSnapshot => ({ version: 1, agents: structuredClone(records) }),
      status: (id?: string) => structuredClone(id ? records.filter((r) => r.id === id) : records),
      async start(input: StartOptions) {
        instance.starts.push(input);
        const record: AgentRecord = { id: `agent-${records.length}`, name: input.name, persona: input.persona, cwd: input.cwd,
          state: "running", assignmentId: "assignment-1", task: input.task, createdAt: Date.now(), updatedAt: Date.now(), messages: [] };
        records.push(record); options.onUpdate(instance.snapshot()); return structuredClone(record);
      },
      async send(input: any): Promise<TeamMessage> {
        instance.sends.push(input);
        const message: TeamMessage = { version: 1, id: "instruction-1", agentId: input.agentId, assignmentId: "assignment-1",
          direction: "to-worker", kind: "instruction", text: input.message, createdAt: Date.now(), delivery: "accepted" };
        return message;
      },
      async close(id: string) { instance.closed.push(id); const r = records.find((r) => r.id === id); if (r) r.state = "closed"; options.onUpdate(instance.snapshot()); },
      async shutdown() { instance.shutdownCount++; for (const r of records) r.state = "closed"; options.onUpdate(instance.snapshot()); },
      restore(snapshot: TeamSnapshot) { records.push(...structuredClone(snapshot.agents).map((r) => ({ ...r, state: "interrupted" as const }))); options.onUpdate(instance.snapshot()); },
    };
    instances.push(instance);
    return instance as unknown as TeamRuntime;
  });
  const fire = (name: string, event = {}) => handlers.get(name)?.(event, ctx);
  const call = (name: string, args: unknown, signal?: AbortSignal) => tools.get(name).execute("call", args, signal, undefined, ctx);
  await fire("session_start");
  return { tools, commands, instances, sent, entries, status, notices, ctx, fire, call, setBranch: (value: any[]) => { branch = value; }, dispose: () => fire("session_shutdown") };
}

for (const mode of ["tui", "rpc", "json", "print"]) test(`orchestrator tools initialize in ${mode}; startup yields only in TUI`, async () => {
  const f = await fixture(mode);
  try {
    assert.deepEqual([...f.tools.keys()], ["agent_start", "agent_send", "agent_status", "agent_wait", "agent_close"]);
    const started = await f.call("agent_start", { name: "reviewer", role: "Staff Engineer", task: "Review only" });
    assert.equal(started.terminate, mode === "tui");
    assert.deepEqual(f.instances[0].starts[0].tools, ["read", "edit", "orchestrator_message"]);
    assert.equal(f.instances[0].starts[0].parentSession, "/tmp/parent-test.jsonl");
    assert.equal(f.instances[0].starts[0].model, "test/model");
    assert.ok(f.entries.some((e) => e.customType === TEAM_STATE_TYPE));
  } finally { await f.dispose(); }
});

test("routine progress updates only history; significant messages wake; ack does not", async () => {
  const f = await fixture();
  try {
    await f.call("agent_start", { name: "worker", role: "Staff Engineer", task: "Review", continueParent: true });
    const message: TeamMessage = { version: 1, id: "q-1", agentId: "agent-0", assignmentId: "assignment-1", direction: "to-orchestrator", kind: "progress", text: "Update", createdAt: Date.now() };
    f.instances[0].options.onMessage(message);
    assert.equal(f.sent.length, 0);
    f.instances[0].options.onMessage({ ...message, kind: "question" });
    assert.equal(f.sent[0].message.customType, TEAM_MESSAGE_TYPE);
    assert.equal(f.sent[0].options.triggerTurn, true);
    assert.match(f.sent[0].message.content, /replyTo="q-1"/);
    const waiting = await f.call("agent_wait", {});
    assert.equal(waiting.details.id, "q-1");
    f.instances[0].options.onMessage({ ...message, id: "ack-1", kind: "ack", replyTo: "instruction-1" });
    assert.equal(f.sent[1].options.triggerTurn, false);
    assert.equal(f.sent[1].options.deliverAs, "nextTurn");
    assert.match(f.fire("context", { messages: [] }).messages[0].content[0].text, /Do not duplicate/);
  } finally { await f.dispose(); }
});

test("agent_wait renders silently without removing model-facing results; errors remain visible", async () => {
  const f = await fixture();
  try {
    const tool = f.tools.get("agent_wait");
    const theme = { fg: (_color: string, text: string) => text };
    assert.equal(tool.renderShell, "self");
    assert.deepEqual(tool.renderCall({}, theme, {}).render(80), []);
    const empty = await f.call("agent_wait", {});
    assert.match(empty.content[0].text, /No unread message/);
    assert.deepEqual(tool.renderResult(empty, {}, theme, { isError: false }).render(80), []);
    const message: TeamMessage = { version: 1, id: "result-1", agentId: "agent-0", assignmentId: "assignment-1",
      direction: "to-orchestrator", kind: "result", text: "Hello!", createdAt: Date.now() };
    f.instances[0].options.onMessage(message);
    const report = await f.call("agent_wait", {});
    assert.equal(report.details.id, message.id);
    assert.match(report.content[0].text, /Hello!/);
    assert.deepEqual(tool.renderResult(report, {}, theme, { isError: false }).render(80), []);
    const error = { content: [{ type: "text", text: "Wait failed" }], details: undefined };
    assert.match(tool.renderResult(error, {}, theme, { isError: true }).render(80).join("\n"), /Wait failed/);
  } finally { await f.dispose(); }
});

test("direct user command answers blocked question and requests ack", async () => {
  const f = await fixture();
  try {
    await f.call("agent_start", { name: "worker", role: "Staff Engineer", task: "Review" });
    Object.assign(f.instances[0].records[0], { state: "blocked", pendingQuestionId: "question-1" });
    await f.commands.get("team-send").handler("agent-0 Use v2", f.ctx);
    assert.deepEqual(f.instances[0].sends[0], { agentId: "agent-0", message: "Use v2", replyTo: "question-1", requiresAck: true });
    await assert.rejects(f.commands.get("team-send").handler("unknown Message", f.ctx), /Unknown agent/);
    await f.commands.get("team-close").handler("all", f.ctx);
    assert.deepEqual(f.instances[0].closed, ["agent-0"]);
  } finally { await f.dispose(); }
});

test("team-view selects names/IDs read-only and returns without closing workers", async () => {
  const f = await fixture();
  try {
    await f.call("agent_start", { name: "worker", role: "Engineer", task: "Review" });
    let opened = 0, selected = 0;
    Object.assign(f.ctx.ui, {
      select: async (_title: string, labels: string[]) => { selected++; return labels[0]; },
      custom: async (factory: any) => {
        opened++;
        let done = false;
        const component = factory({ terminal: { rows: 20 }, requestRender() {} }, { fg: (_c: string, text: string) => text }, {}, () => { done = true; });
        component.handleInput("q");
        assert.equal(done, true);
      },
    });
    for (const query of ["worker", "WORKER", "agent-0", ""]) await f.commands.get("team-view").handler(query, f.ctx);
    assert.equal(opened, 4); assert.equal(selected, 1);
    assert.equal(f.instances[0].shutdownCount, 0);
    assert.deepEqual(f.instances[0].closed, []); assert.deepEqual(f.instances[0].sends, []);
    assert.equal(f.instances[0].records[0].state, "running");
    await f.commands.get("team-view").handler("unknown", f.ctx);
    assert.equal(opened, 4); assert.match(f.notices.at(-1)[0], /Unknown team member/);
    Object.assign(f.ctx.ui, { select: async () => undefined });
    await f.commands.get("team-view").handler("", f.ctx);
    assert.equal(opened, 4);
  } finally { await f.dispose(); }
});

test("session shutdown dismisses the viewer and cancels its refresh lifecycle", async () => {
  const f = await fixture();
  await f.call("agent_start", { name: "worker", role: "Engineer", task: "Review" });
  let component: any;
  Object.assign(f.ctx.ui, { custom: (factory: any) => new Promise<void>((resolve) => {
    component = factory({ terminal: { rows: 20 }, requestRender() {} }, { fg: (_c: string, text: string) => text }, {}, resolve);
  }) });
  const view = f.commands.get("team-view").handler("worker", f.ctx);
  assert.ok(component);
  await f.dispose();
  await view;
  assert.equal(f.instances[0].shutdownCount, 1);
});

for (const mode of ["rpc", "json", "print"]) test(`team-view unavailable in ${mode} without custom UI or parent changes`, async () => {
  const f = await fixture(mode);
  try {
    await f.commands.get("team-view").handler("worker", f.ctx);
    assert.match(f.notices[0][0], /requires interactive TUI/);
    assert.equal(f.instances[0].shutdownCount, 0); assert.equal(f.sent.length, 0);
  } finally { await f.dispose(); }
});

test("shutdown suppresses late worker messages and persists terminal state", async () => {
  const f = await fixture();
  await f.call("agent_start", { name: "worker", role: "Staff Engineer", task: "Review" });
  await f.dispose();
  f.instances[0].options.onMessage({ version: 1, id: "late", agentId: "agent-0", assignmentId: "assignment-1", direction: "to-orchestrator", kind: "result", text: "Late", createdAt: Date.now() });
  assert.equal(f.sent.length, 0);
  assert.equal(f.instances[0].shutdownCount, 1);
  assert.equal(f.entries.at(-1).data.agents[0].state, "closed");
});

test("branch navigation closes live workers and restores only selected branch state", async () => {
  const f = await fixture();
  try {
    await f.call("agent_start", { name: "worker", role: "Staff Engineer", task: "Review" });
    const snapshot = f.instances[0].snapshot();
    await f.fire("session_before_tree");
    f.setBranch([{ type: "custom", customType: TEAM_STATE_TYPE, data: snapshot }]);
    await f.fire("session_tree");
    assert.equal(f.instances[0].shutdownCount, 1);
    assert.equal(f.instances[1].records[0].state, "interrupted");
    assert.equal(f.instances[1].starts.length, 0);
    f.instances[0].options.onMessage({ version: 1, id: "abandoned-branch", agentId: "agent-0", assignmentId: "assignment-1", direction: "to-orchestrator", kind: "result", text: "Stale result", createdAt: Date.now() });
    assert.equal(f.sent.length, 0);
  } finally { await f.dispose(); }
});

test("orchestrator rejects unknown persona or ambiguous role/persona before spawning", async () => {
  const f = await fixture();
  try {
    await assert.rejects(f.call("agent_start", { name: "x", task: "Review", agent: "not-an-agent" }), /Unknown persona/);
    await assert.rejects(f.call("agent_start", { name: "x", task: "Review", agent: "subagent-scout", role: "Engineer" }), /not both/);
    await assert.rejects(f.call("agent_start", { name: "x", task: "Review" }), /expert role/);
    assert.equal(f.instances[0].starts.length, 0);
  } finally { await f.dispose(); }
});

test("noninteractive project personas require explicit consent", async () => {
  const directory = await mkdtemp(path.join(tmpdir(), "pi-team-personas-"));
  await mkdir(path.join(directory, ".pi/agents"), { recursive: true });
  await writeFile(path.join(directory, ".pi/agents/project-reviewer.md"), "---\nname: project-reviewer\ndescription: Project test persona\ntools: read\n---\nReview only.\n");
  const f = await fixture("rpc");
  try {
    Object.assign(f.ctx, { cwd: directory });
    await assert.rejects(f.call("agent_start", { name: "reviewer", agent: "project-reviewer", agentScope: "project", task: "Review" }), /explicit approval/);
    assert.equal(f.instances[0].starts.length, 0);
    await f.call("agent_start", { name: "reviewer", agent: "project-reviewer", agentScope: "project", task: "Review", allowProjectAgent: true });
    assert.deepEqual(f.instances[0].starts[0].tools, ["read", "orchestrator_message"]);
  } finally { await f.dispose(); await rm(directory, { recursive: true, force: true }); }
});

test("actual orchestrator coexists with original subagent and inspector in installed pi", { timeout: 20000 }, async () => {
  const directory = await mkdtemp(path.join(tmpdir(), "pi-team-load-"));
  await mkdir(path.join(directory, "config"), { recursive: true });
  const client = new RpcClient({ cliPath: path.join(getPackageDir(), "dist/cli.js"), cwd: directory,
    env: { PI_CODING_AGENT_DIR: path.join(directory, "config"), PI_AGENT_TEAM_WORKER: "0", PI_OFFLINE: "1", PI_TELEMETRY: "0" },
    args: ["--no-extensions", "--extension", path.join(path.dirname(fileURLToPath(import.meta.url)), "index.ts"),
      "--extension", path.join(path.dirname(fileURLToPath(import.meta.url)), "../child-subagent/index.ts"),
      "--extension", path.join(path.dirname(fileURLToPath(import.meta.url)), "../subagent-inspector/index.ts"),
      "--extension", path.join(path.dirname(fileURLToPath(import.meta.url)), "tests/loadout-probe.ts"),
      "--no-session", "--no-skills", "--no-context-files", "--no-prompt-templates", "--no-themes", "--offline", "--no-approve"],
  });
  try {
    await client.start();
    const commands = (await client.getCommands()).map((command) => command.name);
    assert.ok(commands.includes("team"), client.getStderr());
    assert.ok(commands.includes("team-send"), client.getStderr());
    assert.ok(commands.includes("team-close"), client.getStderr());
    assert.ok(commands.includes("team-view"), client.getStderr());
    assert.equal((await client.getState()).isStreaming, false);
    assert.equal(await client.prompt("/team-test-loadout"), "handled");
    const probe = (await client.getEntries()).entries.find((entry) => entry.type === "custom" && entry.customType === "team-test-loadout");
    assert.ok(probe && probe.type === "custom");
    const tools = probe.data as string[];
    assert.ok(tools.includes("subagent"));
    assert.ok(tools.includes("agent_start"));
    assert.ok(tools.includes("agent_send"));
    assert.ok(!tools.includes("orchestrator_message"));
  } finally { await client.stop(); await rm(directory, { recursive: true, force: true }); }
});
