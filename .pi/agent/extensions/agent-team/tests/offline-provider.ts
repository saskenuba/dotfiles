/** Deterministic test provider: never makes network requests or uses real credentials. */
import { createAssistantMessageEventStream, type AssistantMessage } from "@earendil-works/pi-ai";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function offlineProvider(pi: ExtensionAPI) {
  pi.registerProvider("agent-team-test", {
    api: "agent-team-test-api", baseUrl: "http://invalid.local", apiKey: "test-only",
    models: [{ id: "fixture", name: "Offline team fixture", reasoning: false, input: ["text"],
      contextWindow: 100000, maxTokens: 1000, cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 } }],
    streamSimple(model, context) {
      const stream = createAssistantMessageEventStream();
      queueMicrotask(() => {
        const output: AssistantMessage = {
          role: "assistant", api: model.api, provider: model.provider, model: model.id, timestamp: Date.now(),
          content: [], stopReason: "pending", usage: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0,
            totalTokens: 0, cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, total: 0 } },
        };
        const messages = context.messages;
        const lastUserIndex = messages.findLastIndex((m) => m.role === "user");
        const lastUser = messages[lastUserIndex];
        const text = lastUser?.role === "user" ? (typeof lastUser.content === "string" ? lastUser.content
          : lastUser.content.filter((p) => p.type === "text").map((p) => p.text).join("\n")) : "";
        const messageId = /messageId=([^\s\]]+)/.exec(text)?.[1];
        const didAck = messages.slice(lastUserIndex + 1).some((m) => m.role === "toolResult" && m.toolName === "orchestrator_message");
        stream.push({ type: "start", partial: output });
        if (text.includes("ASK_QUESTION")) {
          output.content.push({ type: "toolCall", id: "question-test", name: "orchestrator_message", arguments: { kind: "question", message: "Which API version should I use?" } });
          output.content.push({ type: "toolCall", id: "forbidden-sibling", name: "read", arguments: { path: "must-not-read.txt" } });
        } else if (text.includes("ACK_REPLY") && !didAck) {
          output.content.push({ type: "toolCall", id: "ack-test", name: "orchestrator_message", arguments: { kind: "ack", message: "Understood: use v2", replyTo: messageId ?? "missing-instruction-id" } });
        }
        if (output.content.length) {
          output.content.forEach((part, index) => {
            if (part.type !== "toolCall") return;
            stream.push({ type: "toolcall_start", contentIndex: index, partial: output });
            stream.push({ type: "toolcall_end", contentIndex: index, toolCall: part, partial: output });
          });
          output.stopReason = "toolUse";
        } else {
          const remembered = JSON.stringify(messages).includes("MEMORY_TOKEN_42");
          const answer = `Completed. Retained original context: ${remembered ? "MEMORY_TOKEN_42" : "MISSING"}. User messages: ${messages.filter((m) => m.role === "user").length}.`;
          output.content.push({ type: "text", text: answer });
          stream.push({ type: "text_start", contentIndex: 0, partial: output });
          stream.push({ type: "text_delta", contentIndex: 0, delta: answer, partial: output });
          stream.push({ type: "text_end", contentIndex: 0, content: answer, partial: output });
          output.stopReason = "stop";
        }
        stream.push({ type: "done", reason: output.stopReason, message: output });
        stream.end();
      });
      return stream;
    },
  });
}
