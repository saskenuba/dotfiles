import assert from "node:assert/strict";
import test from "node:test";
import type { AssistantMessage } from "@earendil-works/pi-ai";
import { exclusiveStartMessage } from "./guard.js";

function message(continueParent?: boolean): AssistantMessage {
  return { role: "assistant", content: [
    { type: "text", text: "Delegating" },
    { type: "toolCall", id: "start", name: "agent_start", arguments: { name: "reviewer", continueParent } },
    { type: "toolCall", id: "read", name: "read", arguments: { path: "same-file" } },
  ] } as AssistantMessage;
}

test("exclusive team startup removes sibling work by default", () => {
  const guarded = exclusiveStartMessage(message());
  assert.ok(guarded);
  assert.deepEqual(guarded.content.filter((p) => p.type === "toolCall").map((p) => p.name), ["agent_start"]);
  assert.equal(guarded.content[0].type, "text");
});

test("continueParent explicitly preserves independent sibling work", () => {
  assert.equal(exclusiveStartMessage(message(true)), undefined);
});
