import assert from "node:assert/strict";
import test from "node:test";
import { mkdtemp, mkdir, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { getPackageDir, RpcClient } from "@earendil-works/pi-coding-agent";
import { isWorkerMessageDetails } from "./protocol.js";

const DIR = path.dirname(fileURLToPath(import.meta.url));

test("real pi RPC worker yields, receives correlated reply/ack, and retains conversation", { timeout: 30000 }, async () => {
  const directory = await mkdtemp(path.join(tmpdir(), "pi-team-bridge-"));
  await mkdir(path.join(directory, "config"), { recursive: true });
  const client = new RpcClient({
    cliPath: path.join(getPackageDir(), "dist/cli.js"), cwd: directory,
    env: { PI_AGENT_TEAM_WORKER: "1", PI_AGENT_TEAM_AGENT_ID: "agent-bridge-test",
      PI_CODING_AGENT_DIR: path.join(directory, "config"), PI_OFFLINE: "1", PI_TELEMETRY: "0" },
    args: ["--no-extensions", "--extension", path.join(DIR, "worker.ts"),
      "--extension", path.join(DIR, "tests/offline-provider.ts"), "--provider", "agent-team-test", "--model", "fixture",
      "--thinking", "off", "--tools", "orchestrator_message,read", "--no-skills", "--no-context-files",
      "--no-prompt-templates", "--no-themes", "--offline", "--no-approve", "--session-dir", path.join(directory, "sessions")],
  });
  try {
    await client.start();
    const initial = await client.promptAndWait("[agent-team assignmentId=assign-1 messageId=initial]\nASK_QUESTION MEMORY_TOKEN_42", undefined, 15000);
    const event = initial.find((e) => e.type === "tool_execution_end" && e.toolName === "orchestrator_message");
    assert.ok(event && event.type === "tool_execution_end", JSON.stringify(initial));
    assert.ok(isWorkerMessageDetails(event.result.details));
    assert.equal(event.result.details.wait, true);
    assert.equal(event.result.details.teamMessage.agentId, "agent-bridge-test");
    assert.equal(initial.some((e) => e.type === "tool_execution_start" && e.toolName === "read"), false);
    const questionId = event.result.details.teamMessage.id;
    const sessionBefore = await client.getState();
    assert.ok(sessionBefore.sessionFile);
    const replied = await client.promptAndWait(`[agent-team assignmentId=assign-1 messageId=reply-1 replyTo=${questionId} requiresAck=true]\nACK_REPLY use v2`, undefined, 15000);
    const ack = replied.find((e) => e.type === "tool_execution_end" && e.toolName === "orchestrator_message");
    assert.ok(ack && ack.type === "tool_execution_end");
    assert.ok(isWorkerMessageDetails(ack.result.details));
    assert.equal(ack.result.details.teamMessage.kind, "ack");
    assert.equal(ack.result.details.teamMessage.replyTo, "reply-1");
    assert.match(await client.getLastAssistantText() ?? "", /MEMORY_TOKEN_42/);
    assert.equal((await client.getState()).sessionId, sessionBefore.sessionId);
    await client.promptAndWait("[agent-team assignmentId=assign-2 messageId=next]\nReview follow-up", undefined, 15000);
    assert.match(await client.getLastAssistantText() ?? "", /MEMORY_TOKEN_42/);
    assert.equal((await client.getState()).sessionId, sessionBefore.sessionId);
  } finally {
    await client.stop();
    await rm(directory, { recursive: true, force: true });
  }
});
