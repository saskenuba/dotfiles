import assert from "node:assert/strict";
import test from "node:test";
import { mkdtemp, mkdir, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { getPackageDir, RpcClient } from "@earendil-works/pi-coding-agent";
import { TeamRuntime } from "./runtime.js";
import type { TeamMessage } from "./protocol.js";

const DIR = path.dirname(fileURLToPath(import.meta.url));

test("supervisor and real RPC worker exchange a blocked reply, acknowledgment, and retained follow-up", { timeout: 30000 }, async () => {
  const directory = await mkdtemp(path.join(tmpdir(), "pi-team-supervisor-"));
  await mkdir(path.join(directory, "config"), { recursive: true });
  const messages: TeamMessage[] = [];
  const listeners = new Set<() => void>();
  const runtime = new TeamRuntime({
    cliPath: path.join(getPackageDir(), "dist/cli.js"), workerExtensionPath: path.join(DIR, "tests/offline-worker.ts"),
    storageDir: path.join(directory, "workers"), onUpdate() {},
    onMessage(message) { messages.push(message); for (const listener of [...listeners]) listener(); },
    clientFactory(options) {
      return new RpcClient({ ...options, env: { ...options.env, PI_CODING_AGENT_DIR: path.join(directory, "config"), PI_OFFLINE: "1", PI_TELEMETRY: "0" },
        args: [...options.args ?? [], "--no-skills", "--no-context-files", "--no-prompt-templates", "--no-themes", "--offline", "--no-approve"] });
    },
  });
  const wait = (kind: TeamMessage["kind"], offset = 0): Promise<TeamMessage> => {
    const existing = messages.slice(offset).find((m) => m.kind === kind);
    if (existing) return Promise.resolve(existing);
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => { listeners.delete(check); reject(new Error(`Missing ${kind}: ${JSON.stringify(messages)}`)); }, 10000);
      const check = () => {
        const message = messages.slice(offset).find((m) => m.kind === kind);
        if (message) { clearTimeout(timer); listeners.delete(check); resolve(message); }
      };
      listeners.add(check); check();
    });
  };
  try {
    const agent = await runtime.start({ name: "real-reviewer", persona: "Staff Engineer", systemPrompt: "Review only; communicate with the orchestrator.",
      task: "ASK_QUESTION MEMORY_TOKEN_42", cwd: directory, model: "agent-team-test/fixture", thinking: "off", tools: ["read"] });
    const question = await wait("question");
    assert.equal(question.agentId, agent.id);
    assert.equal(runtime.status(agent.id)[0].state, "blocked");
    await assert.rejects(runtime.send({ agentId: agent.id, message: "No reply ID" }), /replyTo/);
    const instruction = await runtime.send({ agentId: agent.id, message: "ACK_REPLY use v2", replyTo: question.id, requiresAck: true });
    const ack = await wait("ack");
    assert.equal(ack.replyTo, instruction.id);
    const result = await wait("result");
    assert.match(result.text, /MEMORY_TOKEN_42/);
    const finished = runtime.status(agent.id)[0];
    assert.equal(finished.state, "idle");
    assert.equal(finished.messages.find((m) => m.id === instruction.id)?.delivery, "acknowledged");
    assert.equal(finished.pendingQuestionId, undefined);
    const offset = messages.length;
    const followUp = await runtime.send({ agentId: agent.id, message: "Review follow-up with your retained context", newAssignment: true });
    assert.notEqual(followUp.assignmentId, agent.assignmentId);
    assert.match((await wait("result", offset)).text, /MEMORY_TOKEN_42/);
    const after = runtime.status(agent.id)[0];
    assert.equal(after.sessionFile, finished.sessionFile);
    assert.ok(after.sessionFile);
    await runtime.close(agent.id);
    assert.equal(runtime.status(agent.id)[0].state, "closed");
    assert.match(await readFile(after.sessionFile!, "utf8"), /MEMORY_TOKEN_42/);
    assert.equal(messages.filter((m) => m.kind === "error").length, 0);
  } finally {
    await runtime.shutdown();
    await rm(directory, { recursive: true, force: true });
  }
});
