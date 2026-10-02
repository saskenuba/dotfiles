import { randomUUID } from "node:crypto";
import { chmod, mkdir, writeFile } from "node:fs/promises";
import { join, resolve } from "node:path";
import { RpcClient, type RpcClientOptions, type JsonAgentSessionEvent } from "@earendil-works/pi-coding-agent";
import {
  TEAM_VERSION, MAX_AGENTS, MAX_HISTORY, MAX_MESSAGE_CHARS, MAX_PENDING_INSTRUCTIONS,
  MAX_PENDING_ACKS, MAX_DEDUPE_IDS, WORKER_MESSAGE_TOOL,
  isLive, isWorkerMessageDetails,
  type AgentRecord, type TeamMessage, type TeamSnapshot, type StartOptions, type SendOptions,
} from "./protocol.js";

// Deliberately narrow seam for tests. Production always uses the installed RpcClient.
export interface WorkerClient {
  start(): Promise<void>;
  stop(): Promise<void>;
  onEvent(listener: (event: JsonAgentSessionEvent) => void): () => void;
  prompt: RpcClient["prompt"];
  getState(): Promise<{ sessionFile?: string }>;
  newSession(parentSession?: string): Promise<{ cancelled: boolean }>;
  clearQueue(): Promise<unknown>;
  abort(): Promise<void>;
}
export interface TeamRuntimeOptions {
  cliPath: string;
  workerExtensionPath: string;
  storageDir: string;
  onUpdate: (snapshot: TeamSnapshot) => void;
  onMessage: (message: TeamMessage) => void;
  clientFactory?: (options: RpcClientOptions) => WorkerClient;
  /** Test-only timing overrides; production probes every five seconds. */
  probeIntervalMs?: number;
  commandTimeoutMs?: number;
}
interface Worker {
  record: AgentRecord;
  client?: WorkerClient;
  tail: Promise<unknown>;
  closing: boolean;
  cleanup?: Promise<void>;
  unsubscribe?: () => void;
  probe?: ReturnType<typeof setTimeout>;
  active: boolean;
  latest?: { text: string; failure?: string };
  // Outbound messages not yet observed in a user-message event.
  undelivered: Map<string, TeamMessage>;
  awaitingAck: Map<string, TeamMessage>;
  seen: Set<string>;
  closingPromise?: Promise<void>;
}
const copy = <T>(value: T): T => structuredClone(value);
const bounded = (text: string): string => text.slice(0, MAX_MESSAGE_CHARS);
const errorText = (error: unknown): string => bounded(error instanceof Error ? error.message : String(error));
function required(value: string, label: string): void {
  if (typeof value !== "string" || !value.trim() || value.length > MAX_MESSAGE_CHARS) {
    throw new Error(`${label} must be nonempty and at most ${MAX_MESSAGE_CHARS} characters`);
  }
}
function notify<T>(callback: (value: T) => void, value: T): void {
  try { void Promise.resolve(callback(copy(value))).catch(() => {}); } catch { /* host callbacks cannot break RPC */ }
}

export class TeamRuntime {
  private records = new Map<string, AgentRecord>();
  private workers = new Map<string, Worker>();
  private stopped = false;
  private shutdownPromise?: Promise<void>;

  constructor(private readonly options: TeamRuntimeOptions) {
    required(options.cliPath, "cliPath");
    required(options.workerExtensionPath, "workerExtensionPath");
    required(options.storageDir, "storageDir");
  }

  async start(options: StartOptions): Promise<AgentRecord> {
    options = copy(options);
    if (this.stopped) throw new Error("Runtime is shut down");
    for (const key of ["name", "persona", "systemPrompt", "task", "cwd"] as const) required(options[key], key);
    if (options.context !== undefined) required(options.context, "context");
    if (options.model !== undefined) required(options.model, "model");
    if (options.parentSession !== undefined) required(options.parentSession, "parentSession");
    if (options.thinking !== undefined && !["off", "minimal", "low", "medium", "high", "xhigh", "max"].includes(options.thinking)) {
      throw new Error("Invalid thinking level");
    }
    if (options.tools !== undefined && (!Array.isArray(options.tools) || options.tools.some(t => typeof t !== "string" || !/^[\w.-]+$/.test(t)))) {
      throw new Error("Invalid tools");
    }
    const name = options.name.trim();
    if ([...this.records.values()].some(r => r.name.toLowerCase() === name.toLowerCase())) throw new Error("Agent name already exists");
    // Reserve synchronously, before filesystem work or process startup can yield.
    const occupied = new Set([...this.workers.keys(), ...[...this.records.values()].filter(r => isLive(r.state)).map(r => r.id)]);
    if (occupied.size >= MAX_AGENTS) throw new Error(`At most ${MAX_AGENTS} live agents are allowed`);
    const now = Date.now();
    const record: AgentRecord = {
      id: randomUUID(), assignmentId: randomUUID(), name, persona: options.persona,
      cwd: resolve(options.cwd), model: options.model, task: options.task,
      state: "starting", createdAt: now, updatedAt: now, messages: [],
    };
    const worker: Worker = { record, tail: Promise.resolve(), closing: false, active: false, undelivered: new Map(), awaitingAck: new Map(), seen: new Set() };
    this.records.set(record.id, record);
    this.workers.set(record.id, worker);
    const starting = this.serial(worker, async () => {
      try {
        this.ensureOpen(worker);
        const dir = join(resolve(this.options.storageDir), record.id);
        await mkdir(dir, { recursive: true, mode: 0o700 });
        await chmod(dir, 0o700);
        const systemFile = join(dir, "system-prompt.md");
        await writeFile(systemFile, options.systemPrompt, { mode: 0o600 });
        this.ensureOpen(worker);
        const args = ["--no-extensions", "--extension", resolve(this.options.workerExtensionPath),
          "--session-dir", dir, "--append-system-prompt", systemFile, "--name", name];
        if (options.thinking) args.push("--thinking", options.thinking);
        if (options.tools) args.push("--tools", [...new Set([...options.tools, WORKER_MESSAGE_TOOL])].join(","));
        const inheritedDepth = Number(process.env.PI_SUBAGENT_DEPTH);
        const clientOptions: RpcClientOptions = {
          cliPath: resolve(this.options.cliPath), cwd: record.cwd, model: options.model, args,
          env: { PI_AGENT_TEAM_WORKER: "1", PI_AGENT_TEAM_AGENT_ID: record.id,
            PI_SUBAGENT_DEPTH: String(Number.isSafeInteger(inheritedDepth) && inheritedDepth >= 1 ? inheritedDepth : 1) },
        };
        worker.client = this.options.clientFactory?.(clientOptions) ?? new RpcClient(clientOptions);
        worker.unsubscribe = worker.client.onEvent(event => {
          try { this.event(worker, event); } catch (error) { this.fail(worker, error); }
        });
        await this.command(worker.client.start());
        this.ensureOpen(worker);
        if (options.parentSession) {
          const result = await this.command(worker.client.newSession(options.parentSession));
          if (result.cancelled) throw new Error("Worker session creation was cancelled");
        }
        const state = await this.command(worker.client.getState());
        record.sessionFile = state.sessionFile;
        this.ensureOpen(worker);
        const text = `Curated initial task:\n${options.task}${options.context ? `\n\nCurated context:\n${options.context}` : ""}`;
        await this.submit(worker, this.instruction(record, options.task), "steer", text);
        this.monitor(worker);
        return copy(record);
      } catch (error) {
        if (!worker.closing) this.fail(worker, error);
        await this.cleanup(worker);
        throw error;
      }
    });
    this.update(record);
    return starting;
  }

  async send(options: SendOptions): Promise<TeamMessage> {
    options = copy(options);
    required(options.agentId, "agentId");
    required(options.message, "message");
    if (options.mode !== undefined && options.mode !== "steer" && options.mode !== "followUp") throw new Error("Invalid delivery mode");
    if (options.replyTo !== undefined) required(options.replyTo, "replyTo");
    if (options.requiresAck !== undefined && typeof options.requiresAck !== "boolean") throw new Error("Invalid requiresAck");
    if (options.newAssignment !== undefined && typeof options.newAssignment !== "boolean") throw new Error("Invalid newAssignment");
    const worker = this.workers.get(options.agentId);
    if (!worker) throw new Error("Agent has no live worker");
    return this.serial(worker, async () => {
      this.ensureOpen(worker);
      const r = worker.record;
      if (options.newAssignment && r.state !== "idle") throw new Error("New assignments require an idle agent; resolve any pending question first");
      if (options.newAssignment && r.pendingQuestionId) throw new Error("Resolve the pending question before starting a new assignment");
      if (options.newAssignment && worker.awaitingAck.size) throw new Error("Resolve pending acknowledgments before starting a new assignment");
      if (options.replyTo && options.replyTo !== r.pendingQuestionId) throw new Error("replyTo must match the pending question");
      if (r.state === "blocked" && options.replyTo !== r.pendingQuestionId) throw new Error("Blocked agents require a matching replyTo");
      const question = options.replyTo;
      if (r.state === "blocked") {
        // The bridge may still be finishing its abort. Do not queue a reply into that run.
        try {
          await this.command(worker.client!.clearQueue());
          this.discardQueued(worker);
          await this.command(worker.client!.abort());
        } catch (error) { this.fail(worker, error); throw error; }
        this.ensureOpen(worker);
        worker.active = false;
        worker.latest = undefined;
        if (r.pendingQuestionId !== question) throw new Error("Pending question changed while draining worker");
        this.update(r);
      }
      this.checkAdmission(worker, !!options.requiresAck);
      if (options.newAssignment) {
        worker.seen.clear();
        r.assignmentId = randomUUID();
        r.task = options.message;
      }
      const message = this.instruction(r, options.message, options.replyTo, options.requiresAck);
      // Clear only the matching question; restore it if submission is rejected.
      if (question) r.pendingQuestionId = undefined;
      try {
        return await this.submit(worker, message, options.mode ?? "steer");
      } catch (error) {
        if (question && !r.pendingQuestionId && !worker.closing) {
          r.pendingQuestionId = question;
          if (r.state !== "failed") r.state = "blocked";
          this.update(r);
        }
        throw error;
      }
    });
  }

  status(agentId?: string): AgentRecord[] {
    return copy(agentId ? (this.records.has(agentId) ? [this.records.get(agentId)!] : []) : [...this.records.values()]);
  }
  snapshot(): TeamSnapshot { return { version: TEAM_VERSION, agents: this.status() }; }

  restore(snapshot: TeamSnapshot): void {
    if (this.workers.size) throw new Error("Cannot restore while workers exist");
    if (snapshot.version !== TEAM_VERSION || !Array.isArray(snapshot.agents)) throw new Error("Unsupported team snapshot");
    const records = new Map<string, AgentRecord>();
    const names = new Set<string>();
    for (const source of snapshot.agents.slice(-MAX_HISTORY)) {
      const r = copy(source);
      required(r.id, "agent id"); required(r.name, "name"); required(r.assignmentId, "assignment id");
      if (records.has(r.id) || names.has(r.name.trim().toLowerCase())) throw new Error("Duplicate restored agent");
      if (isLive(r.state)) { r.state = "interrupted"; r.updatedAt = Date.now(); }
      r.messages = r.messages.slice(-MAX_HISTORY).map(m => ({ ...m, text: bounded(m.text),
        delivery: m.direction === "to-worker" && m.delivery === "pending" ? "failed" : m.delivery }));
      for (const pending of r.pendingAcknowledgments ?? []) {
        const historical = r.messages.find(m => m.id === pending.id);
        if (historical) historical.delivery = "failed";
      }
      r.pendingAcknowledgments = [];
      if (r.lastOutput !== undefined) r.lastOutput = bounded(r.lastOutput);
      records.set(r.id, r); names.add(r.name.trim().toLowerCase());
    }
    this.records = records;
    this.update();
  }

  close(agentId: string): Promise<void> {
    const worker = this.workers.get(agentId);
    const record = this.records.get(agentId);
    if (!record) return Promise.reject(new Error("Unknown agent"));
    if (!worker) { if (record.state !== "closed") { record.state = "closed"; this.update(record); } return Promise.resolve(); }
    if (worker.closingPromise) return worker.closingPromise;
    // Suppress events immediately, even when startup/submission is still awaiting RPC.
    worker.closing = true;
    record.state = "closed";
    this.discardQueued(worker);
    this.discardAcknowledgments(worker);
    worker.closingPromise = this.serial(worker, () => this.cleanup(worker));
    this.update(record);
    return worker.closingPromise;
  }

  shutdown(): Promise<void> {
    if (!this.shutdownPromise) {
      this.stopped = true;
      this.shutdownPromise = Promise.all([...this.workers.keys()].map(id => this.close(id))).then(() => {});
    }
    return this.shutdownPromise;
  }

  private serial<T>(worker: Worker, operation: () => Promise<T>): Promise<T> {
    const result = worker.tail.then(operation);
    worker.tail = result.catch(() => {});
    return result;
  }
  private ensureOpen(worker: Worker): void {
    if (worker.closing || !isLive(worker.record.state)) throw new Error("Agent is not live");
  }
  private async command<T>(promise: Promise<T>): Promise<T> {
    let timer: ReturnType<typeof setTimeout> | undefined;
    try {
      return await Promise.race([promise, new Promise<never>((_, reject) => {
        timer = setTimeout(() => reject(new Error("Worker RPC command timed out")), this.options.commandTimeoutMs ?? 10000);
      })]);
    } finally { if (timer) clearTimeout(timer); }
  }
  private instruction(r: AgentRecord, text: string, replyTo?: string, requiresAck = false): TeamMessage {
    return { version: TEAM_VERSION, id: randomUUID(), agentId: r.id, assignmentId: r.assignmentId,
      direction: "to-worker", kind: "instruction", text, createdAt: Date.now(), replyTo, requiresAck, delivery: "pending" };
  }
  private async submit(worker: Worker, message: TeamMessage, mode: "steer" | "followUp", initial?: string): Promise<TeamMessage> {
    const r = worker.record;
    this.ensureOpen(worker);
    this.checkAdmission(worker, !!message.requiresAck);
    worker.undelivered.set(message.id, message);
    if (message.requiresAck) worker.awaitingAck.set(message.id, message);
    if (!worker.active) { worker.latest = undefined; r.lastOutput = undefined; r.error = undefined; }
    r.state = "running";
    this.append(r, message);
    const envelope = `[agent-team assignmentId=${message.assignmentId} messageId=${message.id} replyTo=${message.replyTo ?? "none"} requiresAck=${!!message.requiresAck}]\nCoordinator instructions (authoritative, not worker output):\n${initial ?? message.text}`;
    try {
      this.ensureOpen(worker); // an onUpdate callback may have closed the agent
      const disposition = await this.command(worker.client!.prompt(envelope, undefined, mode));
      if (worker.closing) throw new Error("Agent was closed during submission");
      if (message.delivery !== "acknowledged") message.delivery = "accepted";
      if (disposition === "handled") {
        worker.undelivered.delete(message.id);
        worker.awaitingAck.delete(message.id);
        if (!worker.active && !worker.undelivered.size && r.state === "running") r.state = "idle";
      }
      // Never overwrite state here: events may already have settled or blocked the run.
      this.update(r);
      return copy(message);
    } catch (error) {
      worker.undelivered.delete(message.id);
      worker.awaitingAck.delete(message.id);
      message.delivery = "failed";
      if (!worker.closing) this.fail(worker, error);
      this.update(r);
      throw error;
    }
  }

  private event(worker: Worker, event: JsonAgentSessionEvent): void {
    if (worker.closing || !isLive(worker.record.state)) return;
    const r = worker.record;
    if (event.type === "message_end" && event.message.role === "user") {
      const content = event.message.content;
      const text = typeof content === "string" ? content : content.filter(b => b.type === "text").map(b => b.text).join("\n");
      for (const [id, message] of worker.undelivered) {
        if (!text.includes(`[agent-team assignmentId=${message.assignmentId} messageId=${id} `)) continue;
        worker.undelivered.delete(id);
        worker.active = true;
        worker.latest = undefined;
        r.lastOutput = undefined;
        r.error = undefined;
        if (!r.pendingQuestionId) r.state = "running";
        this.update(r);
      }
    } else if (event.type === "message_end" && event.message.role === "assistant" && worker.active) {
      const m = event.message;
      const text = bounded(m.content.filter(b => b.type === "text").map(b => b.text).join("\n"));
      worker.latest = { text, failure: m.stopReason === "error" || m.stopReason === "aborted" ? (m.errorMessage || `Worker ${m.stopReason}`) : undefined };
    } else if (event.type === "tool_execution_end" && event.toolName === WORKER_MESSAGE_TOOL && !event.isError) {
      const details = (event.result as { details?: unknown } | undefined)?.details;
      if (!isWorkerMessageDetails(details)) return;
      const m = details.teamMessage;
      if (m.agentId !== r.id || m.assignmentId !== r.assignmentId || !m.id.trim() || m.id.length > MAX_MESSAGE_CHARS || !m.text.trim() || !Number.isFinite(m.createdAt) || worker.seen.has(m.id)) return;
      if (m.replyTo !== undefined && (typeof m.replyTo !== "string" || !m.replyTo.trim() || m.replyTo.length > MAX_MESSAGE_CHARS)) return;
      if (m.kind === "ack") {
        const outbound = (m.replyTo ? worker.awaitingAck.get(m.replyTo) : undefined) ??
          r.messages.find(item => item.id === m.replyTo && item.direction === "to-worker" && item.assignmentId === r.assignmentId && item.delivery !== "failed");
        if (!outbound || outbound.assignmentId !== r.assignmentId || outbound.delivery === "failed") return;
        outbound.delivery = "acknowledged";
        worker.awaitingAck.delete(outbound.id);
      }
      worker.seen.add(m.id);
      if (worker.seen.size > MAX_DEDUPE_IDS) worker.seen.delete(worker.seen.values().next().value!);
      const incoming = copy(m);
      if (m.kind === "question" || m.kind === "blocker") {
        // One pending reply target: later notices cannot silently abandon it.
        // They can still request a wait and must not produce a successful result.
        r.pendingQuestionId ??= m.id;
        if (details.wait) r.state = "blocked";
      }
      this.append(r, incoming);
      if (!worker.closing) notify(this.options.onMessage, incoming);
    } else if (event.type === "agent_settled" && worker.active) {
      worker.active = false;
      if (r.state === "blocked") { this.update(r); return; }
      // A submitted-but-not-yet-consumed instruction must not inherit the old answer.
      if (worker.undelivered.size) { worker.latest = undefined; this.update(r); return; }
      if (worker.latest?.failure) { this.fail(worker, worker.latest.failure); return; }
      r.state = "idle";
      if (worker.latest?.text.trim()) {
        r.lastOutput = worker.latest.text;
        const result: TeamMessage = { version: TEAM_VERSION, id: randomUUID(), agentId: r.id,
          assignmentId: r.assignmentId, direction: "to-orchestrator", kind: "result",
          text: worker.latest.text, createdAt: Date.now() };
        this.append(r, result);
        if (!worker.closing) notify(this.options.onMessage, result);
      } else this.update(r);
      worker.latest = undefined;
    }
  }

  private fail(worker: Worker, error: unknown): void {
    if (worker.closing || worker.record.state === "failed") return;
    const r = worker.record;
    r.state = "failed";
    r.error = errorText(error);
    worker.active = false;
    this.discardQueued(worker);
    this.discardAcknowledgments(worker);
    const message: TeamMessage = { version: TEAM_VERSION, id: randomUUID(), agentId: r.id,
      assignmentId: r.assignmentId, direction: "to-orchestrator", kind: "error", text: r.error, createdAt: Date.now() };
    this.append(r, message);
    if (!worker.closing) notify(this.options.onMessage, message);
    void this.serial(worker, () => this.cleanup(worker)).catch(() => {});
  }
  private checkAdmission(worker: Worker, requiresAck: boolean): void {
    if (worker.undelivered.size >= MAX_PENDING_INSTRUCTIONS) throw new Error(`At most ${MAX_PENDING_INSTRUCTIONS} queued instructions are allowed; wait for the worker to consume them`);
    if (requiresAck && worker.awaitingAck.size >= MAX_PENDING_ACKS) throw new Error(`At most ${MAX_PENDING_ACKS} pending acknowledgments are allowed; resolve acknowledgments or close the worker`);
  }
  private discardQueued(worker: Worker): void {
    for (const m of worker.undelivered.values()) {
      m.delivery = "failed";
      worker.awaitingAck.delete(m.id);
      // Re-surface cancellations even if progress evicted the original instruction.
      if (!worker.record.messages.some(item => item.id === m.id)) worker.record.messages.push(m);
    }
    if (worker.record.messages.length > MAX_HISTORY) worker.record.messages.splice(0, worker.record.messages.length - MAX_HISTORY);
    worker.undelivered.clear();
  }
  private discardAcknowledgments(worker: Worker): void {
    for (const m of worker.awaitingAck.values()) m.delivery = "failed";
    worker.awaitingAck.clear();
    worker.record.pendingAcknowledgments = [];
  }
  private monitor(worker: Worker): void {
    if (worker.closing || !isLive(worker.record.state)) return;
    worker.probe = setTimeout(() => {
      void this.serial(worker, async () => {
        if (worker.closing || !isLive(worker.record.state)) return;
        try {
          const state = await this.command(worker.client!.getState());
          if (state.sessionFile && state.sessionFile !== worker.record.sessionFile) {
            worker.record.sessionFile = state.sessionFile;
            this.update(worker.record);
          }
          this.monitor(worker);
        } catch (error) { this.fail(worker, error); }
      }).catch(error => this.fail(worker, error));
    }, this.options.probeIntervalMs ?? 5000);
    worker.probe.unref?.();
  }
  private cleanup(worker: Worker): Promise<void> {
    if (!worker.cleanup) worker.cleanup = (async () => {
      worker.closing = true;
      if (worker.probe) clearTimeout(worker.probe);
      worker.unsubscribe?.();
      this.discardQueued(worker);
      this.discardAcknowledgments(worker);
      if (worker.client) {
        // Keep each step independent: a dead transport must not prevent stop().
        try { await this.command(worker.client.clearQueue()); } catch { /* exited */ }
        try { await this.command(worker.client.abort()); } catch { /* exited */ }
        try { await this.command(worker.client.stop()); } catch { /* best effort */ }
      }
      this.workers.delete(worker.record.id);
      this.update(worker.record);
    })();
    return worker.cleanup;
  }
  private append(r: AgentRecord, message: TeamMessage): void {
    r.messages.push(message);
    if (r.messages.length > MAX_HISTORY) r.messages.splice(0, r.messages.length - MAX_HISTORY);
    this.update(r);
  }
  private update(record?: AgentRecord): void {
    if (record) {
      record.updatedAt = Date.now();
      const worker = this.workers.get(record.id);
      if (worker) record.pendingAcknowledgments = [...worker.awaitingAck.values()];
    }
    for (const [id, r] of this.records) {
      if (this.records.size <= MAX_HISTORY) break;
      if (!isLive(r.state) && !this.workers.has(id)) this.records.delete(id);
    }
    notify(this.options.onUpdate, this.snapshot());
  }
}
