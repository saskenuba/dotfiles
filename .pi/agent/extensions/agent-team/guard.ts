import type { AssistantMessage } from "@earendil-works/pi-ai";

/** Match the original subagent's exclusive-delegation behavior without changing it. */
export function exclusiveStartMessage(message: AssistantMessage): AssistantMessage | undefined {
  const calls = message.content.filter((part) => part.type === "toolCall");
  if (!calls.some((call) => call.name === "agent_start" && call.arguments.continueParent !== true)) return;
  if (!calls.some((call) => call.name !== "agent_start")) return;
  return { ...message, content: message.content.filter((part) => part.type !== "toolCall" || part.name === "agent_start") };
}
