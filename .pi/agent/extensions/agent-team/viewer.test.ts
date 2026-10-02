import assert from "node:assert/strict";
import test from "node:test";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { initTheme, AssistantMessageComponent, UserMessageComponent, ToolExecutionComponent, type ExtensionContext, type Theme } from "@earendil-works/pi-coding-agent";
import { visibleWidth, type TUI } from "@earendil-works/pi-tui";
import type { AgentRecord } from "./protocol.js";
import { cleanViewerText, openTeamSessionViewer, TeamSessionViewer, transcriptText } from "./viewer.js";

import { renderTranscript } from "./viewer-rendering.js";

initTheme("dark", false);
const theme = { fg: (_color: string, text: string) => text } as unknown as Theme;
const record: AgentRecord = { id: "agent-1", name: "worker", persona: "Engineer", cwd: "/tmp", state: "running", assignmentId: "a", task: "Review", createdAt: 0, updatedAt: 0, messages: [] };
const line = (id: string, parentId: string | null, message: unknown) => JSON.stringify({ type: "message", id, parentId, message });
const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

test("viewer parses active branch, tool arguments/results, thinking, images, custom and compaction without executing controls", () => {
  const text = transcriptText([
    JSON.stringify({ type: "session", id: "header", version: 3 }),
    line("a", null, { role: "user", content: "Hello" }),
    line("abandoned", "a", { role: "assistant", content: [{ type: "text", text: "abandoned" }] }),
    line("b", "a", { role: "assistant", content: [{ type: "thinking", thinking: "Plan" }, { type: "toolCall", id: "call-1", name: "read", arguments: { path: "file.ts" } }] }),
    line("c", "b", { role: "toolResult", toolName: "read", toolCallId: "call-1", isError: true, content: [{ type: "text", text: "\x1b[31mfailed\x1b[0m" }, { type: "image" }] }),
    JSON.stringify({ type: "custom_message", id: "d", parentId: "c", customType: "note", content: "Custom" }),
    JSON.stringify({ type: "compaction", id: "e", parentId: "d", summary: "Summary" }),
    '{"type":"message"',
  ].join("\n"));
  assert.match(text, /Hello/); assert.doesNotMatch(text, /abandoned/);
  assert.match(text, /thinking.*\nPlan/); assert.match(text, /tool call: read \(call-1\)/);
  assert.match(text, /"path": "file.ts"/); assert.match(text, /tool result: read \(call-1\) — ERROR/);
  assert.match(text, /failed\n\[image\]/); assert.match(text, /Custom/); assert.match(text, /Summary/);
  assert.doesNotMatch(text, /\x1b/);
  assert.equal(cleanViewerText("a\x1b]0;evil\x07b\x00c"), "abc");
});

test("native transcript uses Pi components, Markdown, thinking, matched tool results, and safe image placeholders", () => {
  const jsonl = [
    line("system", null, { role: "system", content: "SECRET PROMPT" }),
    line("u", "system", { role: "user", content: "**Question** 界" }),
    line("a", "u", { role: "assistant", stopReason: "toolUse", content: [
      { type: "thinking", thinking: "Consider **carefully**" },
      { type: "text", text: "## Answer\n\n```ts\nconst x = 1;\n```" },
      { type: "toolCall", id: "first", name: "read", arguments: { path: "file.ts" } },
      { type: "toolCall", id: "second", name: "unknown_tool", arguments: { value: "界" } },
    ] }),
    line("r2", "a", { role: "toolResult", toolName: "unknown_tool", toolCallId: "second", isError: true,
      content: [{ type: "text", text: "FAILED\x1b]0;evil\x07" }, { type: "image", mimeType: "image/jpeg", data: "ignored" }] }),
    line("r1", "r2", { role: "toolResult", toolName: "read", toolCallId: "first", content: [{ type: "text", text: "const result = 42;" }] }),
  ].join("\n");
  const component = renderTranscript(jsonl, { requestRender() {} }, "/tmp");
  assert.ok(component.children[0] instanceof UserMessageComponent);
  assert.ok(component.children[1] instanceof AssistantMessageComponent);
  const tools = component.children.filter((child) => child instanceof ToolExecutionComponent);
  assert.equal(tools.length, 2);
  assert.match(cleanViewerText(tools[0].render(80).join("\n")), /const result = 42/);
  assert.doesNotMatch(cleanViewerText(tools[0].render(80).join("\n")), /FAILED/);
  assert.match(cleanViewerText(tools[1].render(80).join("\n")), /FAILED/);
  assert.match(cleanViewerText(tools[1].render(80).join("\n")), /\[image\]/);
  const output = component.render(80).join("\n");
  assert.match(output, /\x1b\[/); // retain native colors, backgrounds, and syntax highlighting
  // Some terminal capabilities disable italic/bold ANSI; compare to native output instead.
  const nativeAssistant = new AssistantMessageComponent({ role: "assistant", stopReason: "toolUse", content: [
    { type: "thinking", thinking: "Consider **carefully**" },
    { type: "text", text: "## Answer\n\n```ts\nconst x = 1;\n```" },
    { type: "toolCall", id: "first", name: "read", arguments: { path: "file.ts" } },
    { type: "toolCall", id: "second", name: "unknown_tool", arguments: { value: "界" } },
  ] } as never);
  assert.deepEqual(component.children[1].render(80), nativeAssistant.render(80));
  assert.doesNotMatch(output, /SECRET PROMPT|evil|ignored|\*\*Question\*\*/);
  const user = new UserMessageComponent("**Question** 界");
  assert.deepEqual(component.children[0].render(80), user.render(80));
  for (const width of [10, 25, 80]) {
    assert.ok(component.render(width).every((line) => visibleWidth(line) <= width));
  }
  const hidden = renderTranscript(jsonl, { requestRender() {} }, "/tmp", { hideThinkingBlock: true });
  assert.match(cleanViewerText(hidden.render(80).join("\n")), /Thinking\.\.\./);
  assert.doesNotMatch(cleanViewerText(hidden.render(80).join("\n")), /Consider/);
});

test("native assistant errors, truncated responses, aborted calls, and orphan results remain visible", () => {
  for (const stopReason of ["error", "aborted", "length"]) {
    const transcript = renderTranscript(line("a", null, { role: "assistant", stopReason, errorMessage: "Failure reason",
      content: [{ type: "text", text: "Partial response" }] }), { requestRender() {} }, "/tmp");
    const output = cleanViewerText(transcript.render(80).join("\n"));
    assert.match(output, /Partial response/);
    assert.match(output, stopReason === "length" ? /truncated/ : /Failure reason/);
  }
  const aborted = renderTranscript(line("a", null, { role: "assistant", stopReason: "aborted", content: [
    { type: "toolCall", name: "bash", id: "call", arguments: { command: "pwd" } },
  ] }), { requestRender() {} }, "/tmp");
  assert.match(cleanViewerText(aborted.render(80).join("\n")), /Operation aborted/);
  const orphan = renderTranscript(line("r", null, { role: "toolResult", toolName: "unknown", toolCallId: "orphan",
    content: [{ type: "text", text: "Orphan output" }] }), { requestRender() {} }, "/tmp");
  assert.ok(orphan.children[0] instanceof ToolExecutionComponent);
  assert.match(cleanViewerText(orphan.render(80).join("\n")), /Orphan output/);
});

test("viewer refreshes read-only files and state, scrolls/resizes, and stops refresh on q", async () => {
  const dir = await mkdtemp(join(tmpdir(), "pi-team-viewer-"));
  const file = join(dir, "session.jsonl");
  const original = line("a", null, { role: "user", content: Array.from({ length: 40 }, (_, i) => `line-${i} 界`).join("\n") });
  await writeFile(file, original);
  const member = { ...record, sessionFile: file };
  let renders = 0, closed = 0;
  const tui = { terminal: { rows: 15 }, requestRender: () => { renders++; } } as unknown as TUI;
  const viewer = new TeamSessionViewer(tui, theme, () => member, () => { closed++; }, 20);
  try {
    await sleep(50);
    assert.match(viewer.render(60).join("\n"), /line-39/);
    viewer.handleInput("\x1b[H");
    assert.match(viewer.render(60).join("\n"), /Task: Review/);
    viewer.handleInput("\x1b[6~");
    assert.doesNotMatch(viewer.render(60).join("\n"), /Task: Review/);
    viewer.handleInput("\x1b[F");
    assert.match(viewer.render(60).join("\n"), /line-39/);
    viewer.handleInput("g"); viewer.handleInput("g");
    assert.match(viewer.render(60).join("\n"), /Task: Review/);
    viewer.handleInput("1"); viewer.handleInput("0"); viewer.handleInput("j");
    assert.match(viewer.render(60).at(-1)!, /paused 11\//);
    viewer.handleInput("2"); viewer.handleInput("k");
    assert.match(viewer.render(60).at(-1)!, /paused 9\//);
    viewer.handleInput("\x04"); // Ctrl+d: half-page
    assert.match(viewer.render(60).at(-1)!, /paused 14\//);
    viewer.handleInput("\x15"); // Ctrl+u
    assert.match(viewer.render(60).at(-1)!, /paused 9\//);
    viewer.handleInput("\x06"); // Ctrl+f: full-page
    assert.match(viewer.render(60).at(-1)!, /paused 19\//);
    viewer.handleInput("\x02"); // Ctrl+b
    assert.match(viewer.render(60).at(-1)!, /paused 9\//);
    viewer.handleInput("5"); viewer.handleInput("G");
    assert.match(viewer.render(60).at(-1)!, /paused 5\//);
    viewer.handleInput("G");
    assert.match(viewer.render(60).join("\n"), /line-39/);
    assert.match(viewer.render(60).at(-1)!, /following/);
    for (const width of [1, 10, 25]) assert.ok(viewer.render(width).every((l) => visibleWidth(l) <= width));
    assert.equal(await readFile(file, "utf8"), original);
    member.state = "closed";
    await writeFile(file, original + "\n" + line("b", "a", { role: "assistant", content: [{ type: "text", text: "Finished" }] }));
    await sleep(70);
    assert.match(viewer.render(100).join("\n"), /closed/);
    assert.match(viewer.render(100).join("\n"), /Finished/);
    viewer.handleInput("q");
    assert.equal(closed, 1);
    const count = renders;
    await sleep(60);
    assert.equal(renders, count);
  } finally { viewer.dispose(); await rm(dir, { recursive: true, force: true }); }
});

test("viewer custom UI inherits application cancel bindings", async () => {
  let closed = 0;
  const ctx = { ui: { custom: async (factory: Parameters<ExtensionContext["ui"]["custom"]>[0]) => {
    const viewer = await factory({ terminal: { rows: 20 }, requestRender() {} } as unknown as TUI,
      theme, { matches: (data: string, id: string) => data === "custom-cancel" && id === "tui.select.cancel" } as never,
      () => { closed++; });
    try { viewer.handleInput?.("custom-cancel"); }
    finally { viewer.dispose?.(); }
  } } } as unknown as ExtensionContext;
  await openTeamSessionViewer(ctx, () => record);
  assert.equal(closed, 1);
});

test("viewer handles unavailable and missing files and Escape cleanup", async () => {
  let closed = 0;
  const member = { ...record };
  const viewer = new TeamSessionViewer({ terminal: { rows: 20 }, requestRender() {} } as unknown as TUI, theme, () => member, () => { closed++; }, 10);
  try {
    await sleep(20);
    assert.match(viewer.render(100).join("\n"), /not available yet/);
    member.sessionFile = "/nonexistent/team-session.jsonl";
    await sleep(30);
    assert.match(viewer.render(100).join("\n"), /Cannot read session/);
    viewer.handleInput("\x1b");
    assert.equal(closed, 1);
  } finally { viewer.dispose(); }
});
