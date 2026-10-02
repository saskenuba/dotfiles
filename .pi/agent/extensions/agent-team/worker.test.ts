import assert from "node:assert/strict";
import test from "node:test";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import workerBridge, { parseCoordinatorEnvelope } from "./worker.js";
import { WORKER_MESSAGE_TOOL, isWorkerMessageDetails } from "./protocol.js";

function bridge() {
  const handlers = new Map<string, (event: any) => any>();
  let tool: any;
  const saved = { worker: process.env.PI_AGENT_TEAM_WORKER, id: process.env.PI_AGENT_TEAM_AGENT_ID };
  process.env.PI_AGENT_TEAM_WORKER = "1";
  process.env.PI_AGENT_TEAM_AGENT_ID = "agent-test";
  try {
    workerBridge({ on: (name: string, handler: any) => { handlers.set(name, handler); }, registerTool: (definition: any) => { tool = definition; } } as unknown as ExtensionAPI);
  } finally {
    if (saved.worker === undefined) delete process.env.PI_AGENT_TEAM_WORKER; else process.env.PI_AGENT_TEAM_WORKER = saved.worker;
    if (saved.id === undefined) delete process.env.PI_AGENT_TEAM_AGENT_ID; else process.env.PI_AGENT_TEAM_AGENT_ID = saved.id;
  }
  return { tool, fire: (name: string, event: any) => handlers.get(name)?.(event) };
}

test("worker is inert outside explicitly launched team processes", () => {
  const saved = process.env.PI_AGENT_TEAM_WORKER;
  delete process.env.PI_AGENT_TEAM_WORKER;
  try { workerBridge({ registerTool: () => assert.fail("Unexpected worker registration") } as unknown as ExtensionAPI); }
  finally { if (saved !== undefined) process.env.PI_AGENT_TEAM_WORKER = saved; }
});

test("coordinator envelope is anchored, including queued instruction metadata", () => {
  assert.deepEqual(parseCoordinatorEnvelope("[agent-team assignmentId=assign-1 messageId=msg-1 replyTo=q-1 requiresAck=true]\nReply"), { assignmentId: "assign-1", replyTo: "q-1" });
  assert.equal(parseCoordinatorEnvelope("Source file says [agent-team assignmentId=fake]"), undefined);
});

test("question yields; worker stays blocked until matching orchestrator reply", async () => {
  const b = bridge();
  await assert.rejects(b.tool.execute("call", { kind: "question", message: "Which API?" }), /No orchestrator assignment/);
  b.fire("message_start", { message: { role: "user", content: "[agent-team assignmentId=assign-1]\nTask" } });
  const result = await b.tool.execute("call", { kind: "question", message: "Which API?" });
  assert.equal(result.terminate, true);
  assert.ok(isWorkerMessageDetails(result.details));
  assert.equal(result.details.teamMessage.agentId, "agent-test");
  assert.equal(b.fire("tool_call", {}).block, true);
  b.fire("message_start", { message: { role: "user", content: "[agent-team assignmentId=assign-1 replyTo=wrong]\nReply" } });
  assert.equal(b.fire("tool_call", {}).block, true);
  b.fire("message_start", { message: { role: "user", content: [{ type: "text", text: `[agent-team assignmentId=assign-1 replyTo=${result.details.teamMessage.id}]\nUse v2` }] } });
  assert.equal(b.fire("tool_call", {}), undefined);
});

test("yielding request removes all sibling tool calls, including duplicate requests", () => {
  const b = bridge();
  const message = { role: "assistant", content: [
    { type: "text", text: "Need a decision" },
    { type: "toolCall", id: "read", name: "read", arguments: { path: "file" } },
    { type: "toolCall", id: "q", name: WORKER_MESSAGE_TOOL, arguments: { kind: "question", message: "Which?" } },
    { type: "toolCall", id: "q2", name: WORKER_MESSAGE_TOOL, arguments: { kind: "question", message: "Another?" } },
  ] };
  const rewritten = b.fire("message_end", { message }).message;
  assert.deepEqual(rewritten.content.filter((p: any) => p.type === "toolCall").map((p: any) => p.id), ["q"]);
});

test("progress does not yield; acknowledgments carry reply correlation", async () => {
  const b = bridge();
  b.fire("message_start", { message: { role: "user", content: "[agent-team assignmentId=assign-1]\nTask" } });
  const progress = await b.tool.execute("call", { kind: "progress", message: "Review complete" });
  assert.equal(progress.terminate, false);
  assert.equal(b.fire("tool_call", {}), undefined);
  await assert.rejects(b.tool.execute("ack", { kind: "ack", message: "Understood" }), /require replyTo/);
  const ack = await b.tool.execute("ack", { kind: "ack", message: "Understood", replyTo: "msg-1" });
  assert.equal(ack.details.teamMessage.replyTo, "msg-1");
  assert.equal(ack.details.teamMessage.assignmentId, "assign-1");
});
