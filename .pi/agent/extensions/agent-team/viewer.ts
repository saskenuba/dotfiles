import { readFile, stat } from "node:fs/promises";
import { readFileSync } from "node:fs";
import { getAgentDir, type ExtensionContext, type Theme } from "@earendil-works/pi-coding-agent";
import { join } from "node:path";
import { cleanViewerText, renderTranscript, transcriptBranch, type ViewerRenderSettings } from "./viewer-rendering.js";
export { cleanViewerText } from "./viewer-rendering.js";
import { truncateToWidth, wrapTextWithAnsi, type Component, type TUI } from "@earendil-works/pi-tui";
import type { AgentRecord } from "./protocol.js";
import { ViewerKeybindings, VIEWER_HELP_TEXT, type ViewerBindingResolver } from "./keybindings.js";

function stringify(value: unknown): string {
  return typeof value === "string" ? value : JSON.stringify(value, null, 2) ?? "";
}
function contentText(content: unknown): string {
  if (typeof content === "string") return content;
  if (!Array.isArray(content)) return "";
  return content.map((block) => {
    if (block?.type === "text") return block.text ?? "";
    if (block?.type === "thinking") return `[thinking]\n${block.thinking ?? ""}`;
    if (block?.type === "toolCall") return `[tool call: ${block.name} (${block.id})]\n${stringify(block.arguments)}`;
    if (block?.type === "image") return "[image]";
    return `[${block?.type ?? "unknown content"}]`;
  }).join("\n");
}

/** Raw history on the most recently persisted branch, including pre-compaction messages.
 * Tolerate a partially appended final line without modifying/migrating the session. */
export function transcriptText(jsonl: string): string {
  const branch = transcriptBranch(jsonl);
  const messages = branch.flatMap((entry) => {
    if (entry.type === "message" && entry.message) {
      const m = entry.message;
      // The system prompt/tool schema is not conversation and can be very large.
      if (m.role === "system") return [];
      const title = m.role === "toolResult"
        ? `tool result: ${m.toolName} (${m.toolCallId})${m.isError ? " — ERROR" : ""}` : m.role;
      return [`[${title}]\n${contentText(m.content)}${m.errorMessage ? `\nError: ${m.errorMessage}` : ""}`];
    }
    if (entry.type === "custom_message") return [`[custom: ${entry.customType}]\n${contentText(entry.content)}`];
    if (entry.type === "compaction" || entry.type === "branch_summary") return [`[${entry.type}]\n${entry.summary ?? ""}`];
    return [];
  });
  return cleanViewerText(messages.join("\n\n") || "No persisted conversation yet.");
}

export class TeamSessionViewer implements Component {
  private timer?: ReturnType<typeof setTimeout>;
  private disposed = false;
  private record?: AgentRecord;
  private text = "Loading persisted conversation…";
  private transcript?: ReturnType<typeof renderTranscript>;
  private signature?: string;
  private offset = 0;
  private follow = true;
  private height = 1;
  private maxOffset = 0;
  private layout?: { width: number; lines: string[] };

  constructor(private tui: Pick<TUI, "terminal" | "requestRender">, private theme: Theme,
    private getRecord: () => AgentRecord | undefined, private done: () => void,
    private refreshMs = 1000, private bindings: ViewerBindingResolver = new ViewerKeybindings(),
    private settings: ViewerRenderSettings = {}) {
    void this.refresh();
  }
  async refresh(): Promise<void> {
    if (this.disposed) return;
    const previousText = this.text;
    const previousMetadata = JSON.stringify(this.record && [this.record.task, this.record.sessionFile, this.record.pendingQuestionId, this.record.error]);
    try {
      this.record = this.getRecord();
      const file = this.record?.sessionFile;
      if (!file) {
        this.transcript = undefined;
        this.signature = undefined;
        this.text = "Session file is not available yet. Waiting for the worker to persist its conversation.";
      } else {
        const info = await stat(file);
        const signature = `${file}:${info.size}:${info.mtimeMs}`;
        if (signature !== this.signature) {
          const jsonl = await readFile(file, "utf8");
          if (this.disposed) return;
          this.transcript = renderTranscript(jsonl, this.tui, this.record?.cwd ?? ".", this.settings);
          this.text = this.transcript.children.length ? "" : "No persisted conversation yet.";
          this.signature = signature;
          this.invalidate();
        }
      }
    } catch (error) {
      if (this.disposed) return;
      this.signature = undefined;
      this.transcript = undefined;
      this.text = `Cannot read session: ${error instanceof Error ? error.message : String(error)}`;
    }
    if (this.disposed) return;
    const metadata = JSON.stringify(this.record && [this.record.task, this.record.sessionFile, this.record.pendingQuestionId, this.record.error]);
    if (previousText !== this.text || previousMetadata !== metadata) this.invalidate();
    this.tui.requestRender();
    this.timer = setTimeout(() => { void this.refresh(); }, this.refreshMs);
  }
  dispose(): void {
    this.disposed = true;
    if (this.timer) clearTimeout(this.timer);
    this.timer = undefined;
  }
  invalidate(): void { this.layout = undefined; this.transcript?.invalidate(); }
  handleInput(data: string): void {
    const motion = this.bindings.resolve(data);
    if (!motion) return;
    const { action, count, counted } = motion;
    switch (action) {
      case "dismiss": this.dispose(); this.done(); return;
      case "jumpStart": this.follow = false; this.offset = counted ? count - 1 : 0; break;
      case "jumpEnd": this.follow = !counted; this.offset = counted ? count - 1 : this.maxOffset; break;
      case "scrollUp": this.follow = false; this.offset -= count; break;
      case "scrollDown": this.follow = false; this.offset += count; break;
      case "halfPageUp": this.follow = false; this.offset -= counted ? count : Math.max(1, Math.floor(this.height / 2)); break;
      case "halfPageDown": this.follow = false; this.offset += counted ? count : Math.max(1, Math.floor(this.height / 2)); break;
      case "pageUp": this.follow = false; this.offset -= this.height * count; break;
      case "pageDown": this.follow = false; this.offset += this.height * count; break;
    }
    this.offset = Math.max(0, Math.min(this.maxOffset, this.offset));
    this.tui.requestRender();
  }
  render(width: number): string[] {
    width = Math.max(1, width);
    const rows = Math.max(1, this.tui.terminal.rows - 2);
    // Header/footer remain visible; body follows the tail until manually scrolled.
    this.height = Math.max(1, rows - 3);
    if (!this.layout || this.layout.width !== width) {
      const r = this.record;
      const metadata = r ? `Task: ${r.task}\nSession: ${r.sessionFile ?? "pending"}${r.pendingQuestionId ? `\nPending question: ${r.pendingQuestionId}` : ""}${r.error ? `\nError: ${r.error}` : ""}` : "Member is no longer on this branch.";
      const lines = cleanViewerText(`${metadata}\n\n${this.text}`).split("\n").flatMap((line) => wrapTextWithAnsi(line, width));
      // Do not sanitize native output: its ANSI carries Pi's message backgrounds and Markdown.
      lines.push(...(this.transcript?.render(width) ?? []));
      this.layout = { width, lines };
    }
    const lines = this.layout.lines;
    this.maxOffset = Math.max(0, lines.length - this.height);
    this.offset = this.follow ? this.maxOffset : Math.min(this.offset, this.maxOffset);
    const clip = (text: string) => truncateToWidth(text, width, "");
    const title = cleanViewerText(`Team session: ${this.record?.name ?? "unknown"} · ${this.record?.state ?? "unavailable"} · ${this.record?.id ?? ""} (read-only)`);
    return [clip(this.theme.fg("accent", title)), clip(this.theme.fg("muted", "Persisted messages (completed turns); worker continues independently.")),
      ...lines.slice(this.offset, this.offset + this.height).map(clip),
      clip(this.theme.fg("muted", `${VIEWER_HELP_TEXT} · ${this.follow ? "following" : "paused"} ${this.offset + 1}/${lines.length}`))];
  }
}

export async function openTeamSessionViewer(ctx: ExtensionContext, getRecord: () => AgentRecord | undefined, signal?: AbortSignal): Promise<void> {
  if (signal?.aborted) return;
  // Read saved parent display preferences directly, avoiding SettingsManager/session writes.
  // In-memory parent toggles and child extension renderers are not public context APIs.
  const settings: ViewerRenderSettings = {};
  for (const file of [join(getAgentDir(), "settings.json"), join(ctx.cwd ?? ".", ".pi", "settings.json")]) {
    try {
      const saved = JSON.parse(readFileSync(file, "utf8"));
      if (typeof saved.hideThinkingBlock === "boolean") settings.hideThinkingBlock = saved.hideThinkingBlock;
      if (saved.outputPad === 0 || saved.outputPad === 1) settings.outputPad = saved.outputPad;
      if (typeof saved.markdown?.codeBlockIndent === "string") settings.codeBlockIndent = cleanViewerText(saved.markdown.codeBlockIndent);
    } catch { /* Missing or malformed settings use native defaults. */ }
  }
  if (signal?.aborted) return;
  let viewer: TeamSessionViewer | undefined;
  let cancel: (() => void) | undefined;
  try {
    await ctx.ui.custom<void>((tui, theme, keys, done) => {
      viewer = new TeamSessionViewer(tui, theme, getRecord, () => done(), 1000, new ViewerKeybindings(keys), settings);
      cancel = () => { viewer?.dispose(); done(); };
      signal?.addEventListener("abort", cancel, { once: true });
      if (signal?.aborted) cancel();
      return viewer;
    });
  } finally {
    if (cancel) signal?.removeEventListener("abort", cancel);
    viewer?.dispose();
  }
}
