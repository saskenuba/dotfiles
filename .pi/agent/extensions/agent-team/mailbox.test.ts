import assert from "node:assert/strict";
import test from "node:test";
import { Mailbox } from "./mailbox.js";
import type { TeamMessage } from "./protocol.js";

function message(id: string, agentId = "a", kind: TeamMessage["kind"] = "finding"): TeamMessage {
  return { version: 1, id, agentId, kind, assignmentId: "assignment", direction: "to-orchestrator", text: "Update", createdAt: Date.now() };
}

test("inbox retains messages that arrive before a wait and consumes once", async () => {
  const box = new Mailbox();
  box.push(message("first"));
  assert.equal((await box.wait(undefined, 10))?.id, "first");
  assert.equal(await box.wait(undefined, 1), undefined);
});

test("inbox filters agents and never wakes for progress/ack", async () => {
  const box = new Mailbox();
  box.push(message("other", "b"));
  box.push(message("progress", "a", "progress"));
  box.push(message("ack", "a", "ack"));
  const wait = box.wait("a", 100);
  box.push(message("answer", "a", "question"));
  assert.equal((await wait)?.id, "answer");
  assert.equal((await box.wait("b", 10))?.id, "other");
  assert.equal(await box.wait(undefined, 1), undefined);
});

test("cancelled waits release listeners and do not consume later messages", async () => {
  const box = new Mailbox();
  const controller = new AbortController();
  const wait = box.wait(undefined, 100, controller.signal);
  controller.abort(new Error("Cancelled"));
  await assert.rejects(wait, /Cancelled/);
  box.push(message("later"));
  assert.equal((await box.wait(undefined, 10))?.id, "later");
});

test("disposal resolves outstanding waits and suppresses late messages", async () => {
  const box = new Mailbox();
  const waiting = box.wait(undefined, 100);
  box.dispose();
  assert.equal(await waiting, undefined);
  box.push(message("late"));
  assert.equal(await box.wait(undefined, 1), undefined);
});
