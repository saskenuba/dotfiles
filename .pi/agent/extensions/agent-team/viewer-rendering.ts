import {
  AssistantMessageComponent, UserMessageComponent, ToolExecutionComponent, CustomMessageComponent,
  CompactionSummaryMessageComponent, BranchSummaryMessageComponent, SkillInvocationMessageComponent, parseSkillBlock, getMarkdownTheme,
  createReadToolDefinition, createBashToolDefinition, createEditToolDefinition, createWriteToolDefinition,
  createFindToolDefinition, createGrepToolDefinition, createLsToolDefinition,
} from "@earendil-works/pi-coding-agent";
import { Container, Spacer, type TUI } from "@earendil-works/pi-tui";

// Sanitize persisted data before giving it to native renderers; preserve renderer-generated ANSI.
export function cleanViewerText(text: string): string {
  return text.replace(/\x1b\][^\x07\x1b]*(?:\x07|\x1b\\)/g, "")
    .replace(/\x1b\[[0-?]*[ -/]*[@-~]/g, "")
    .replace(/[\x00-\x08\x0b-\x1f\x7f-\x9f]/g, "").replace(/\t/g, "  ");
}
export function transcriptBranch(jsonl: string): any[] {
  const entries: any[] = [];
  for (const line of jsonl.split("\n")) {
    try {
      const entry = JSON.parse(line);
      if (entry && typeof entry === "object" && entry.type !== "session") entries.push(entry);
    } catch { /* A trailing partial append is retried on refresh. */ }
  }
  const leaf = entries.at(-1);
  if (!leaf || typeof leaf.id !== "string" || !("parentId" in leaf)) return entries;
  const byId = new Map(entries.map((entry) => [entry.id, entry]));
  const seen = new Set<string>();
  const branch = [];
  let entry = leaf;
  while (entry && !seen.has(entry.id)) {
    seen.add(entry.id); branch.push(entry); entry = byId.get(entry.parentId);
  }
  return branch.reverse();
}
function sanitize(value: any): any {
  if (typeof value === "string") return cleanViewerText(value);
  if (Array.isArray(value)) return value.map(sanitize);
  if (value && typeof value === "object") {
    // Images cannot safely be line-sliced by this viewport; never trigger native image conversion.
    if (value.type === "image") return { type: "text", text: "[image]" };
    return Object.fromEntries(Object.entries(value).map(([key, item]) => [cleanViewerText(key), sanitize(item)]));
  }
  return value;
}
export interface ViewerRenderSettings { hideThinkingBlock?: boolean; outputPad?: 0 | 1; codeBlockIndent?: string; }

/** Mirrors InteractiveMode.renderSessionItems without opening a session or executing tools.
 * Child extension renderers and live parent display toggles are not exposed by ExtensionContext.
 * Built-in definitions are used only for their renderCall/renderResult functions.
 */
export function renderTranscript(jsonl: string, tui: Pick<TUI, "requestRender">, cwd: string,
  settings: ViewerRenderSettings = {}): Container {
  const container = new Container();
  const pending = new Map<string, ToolExecutionComponent>();
  const markdown = { ...getMarkdownTheme(), ...(settings.codeBlockIndent !== undefined ? { codeBlockIndent: settings.codeBlockIndent } : {}) };
  const pad = settings.outputPad ?? 1;
  const factories = { read: createReadToolDefinition, bash: createBashToolDefinition, edit: createEditToolDefinition,
    write: createWriteToolDefinition, find: createFindToolDefinition, grep: createGrepToolDefinition, ls: createLsToolDefinition };
  const tool = (name: string, id: string, args: any) => {
    const factory = factories[name as keyof typeof factories];
    const definition = factory?.(cwd);
    const component = new ToolExecutionComponent(name, id, args, { showImages: false }, definition,
      tui as TUI, cwd);
    // A read-only viewer has no tool expansion action: expose all persisted output.
    component.setExpanded(true);
    component.setArgsComplete();
    container.addChild(component);
    return component;
  };
  for (const raw of transcriptBranch(jsonl)) {
    const entry = sanitize(raw);
    if (entry.type === "message" && entry.message) {
      const message = entry.message;
      if (message.role === "system") continue;
      if (message.role === "user") {
        const text = typeof message.content === "string" ? message.content :
          (message.content ?? []).filter((b: any) => b.type === "text").map((b: any) => b.text).join("\n");
        if (container.children.length) container.addChild(new Spacer(1));
        const skill = parseSkillBlock(text);
        if (skill) {
          const component = new SkillInvocationMessageComponent(skill, markdown);
          component.setExpanded(true);
          container.addChild(component);
          if (skill.userMessage) {
            container.addChild(new Spacer(1));
            container.addChild(new UserMessageComponent(skill.userMessage, markdown, pad));
          }
        } else {
          container.addChild(new UserMessageComponent(text, markdown, pad));
        }
      } else if (message.role === "assistant") {
        message.content = Array.isArray(message.content) ? message.content : [{ type: "text", text: message.content ?? "" }];
        container.addChild(new AssistantMessageComponent(message, settings.hideThinkingBlock ?? false, markdown, "Thinking...", pad));
        for (const call of message.content.filter((b: any) => b.type === "toolCall")) {
          const component = tool(call.name, call.id, call.arguments);
          if (message.stopReason === "error" || message.stopReason === "aborted") {
            component.updateResult({ content: [{ type: "text", text: message.errorMessage || "Operation aborted" }], isError: true });
          }
          pending.set(call.id, component);
        }
      } else if (message.role === "toolResult") {
        // Preserve orphan results (e.g. a branch whose call predates persisted history).
        const component = pending.get(message.toolCallId) ?? tool(message.toolName, message.toolCallId, {});
        component.updateResult({ ...message, content: typeof message.content === "string" ? [{ type: "text", text: message.content }] : message.content ?? [], isError: !!message.isError });
        pending.delete(message.toolCallId);
      } else if (message.role === "custom") {
        container.addChild(new CustomMessageComponent(message, undefined, markdown, pad));
      }
    } else if (entry.type === "custom_message" && entry.display !== false) {
      container.addChild(new Spacer(1));
      container.addChild(new CustomMessageComponent({ ...entry, role: "custom" }, undefined, markdown, pad));
    } else if (entry.type === "compaction" || entry.type === "branch_summary") {
      container.addChild(new Spacer(1));
      const component = entry.type === "compaction"
        ? new CompactionSummaryMessageComponent({ ...entry, role: "compactionSummary" }, markdown)
        : new BranchSummaryMessageComponent({ ...entry, role: "branchSummary" }, markdown);
      component.setExpanded(true);
      container.addChild(component);
    }
  }
  return container;
}
