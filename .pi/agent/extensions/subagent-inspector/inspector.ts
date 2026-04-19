import type { ExtensionUIContext, KeybindingsManager, Theme } from "@mariozechner/pi-coding-agent";
import type { Component, OverlayOptions, TUI } from "@mariozechner/pi-tui";
import { truncateToWidth, visibleWidth, wrapTextWithAnsi } from "@mariozechner/pi-tui";
import {
	compareChildren,
	compareRuns,
	formatAge,
	getChildLabel,
	getRunLabel,
	getStatusColor,
	getStatusIcon,
	getStatusLabel,
	isActiveStatus,
	type SubagentChildRecord,
	type SubagentInspectorStore,
	type SubagentRunRecord,
	type SubagentStatus,
} from "./dock.js";
import { INSPECTOR_ACTIONS, INSPECTOR_HELP_TEXT, matchesInspectorBinding } from "./keybindings.js";

export interface InspectorOpenResult {
	action: "selected" | "dismissed";
	runId?: string;
	childId?: string;
}

export interface InspectorOptions {
	title?: string;
	helpText?: string;
	overlayOptions?: OverlayOptions;
	detailLineLimit?: number;
	listLineLimit?: number;
}

interface InspectorFactoryOptions extends InspectorOptions {
	onDone: (result: InspectorOpenResult) => void;
}

interface ListRow {
	type: "group" | "item";
	key: string;
	status?: SubagentStatus;
	child?: SubagentChildRecord;
	label: string;
}

const STATUS_GROUP_ORDER: readonly SubagentStatus[] = [
	"running",
	"starting",
	"queued",
	"failed",
	"aborted",
	"cancelled",
	"interrupted",
	"succeeded",
	"skipped",
];

function padRight(text: string, width: number): string {
	const rendered = truncateToWidth(text, width, "...", true);
	return rendered + " ".repeat(Math.max(0, width - visibleWidth(rendered)));
}

function clipParagraph(text: string | undefined): string | undefined {
	const clean = text?.replace(/\s+/g, " ").trim();
	return clean || undefined;
}

function formatDuration(startedAt?: number, endedAt?: number, now = Date.now()): string {
	if (!startedAt) return "";
	const end = endedAt ?? now;
	const seconds = Math.max(0, Math.floor((end - startedAt) / 1000));
	if (seconds < 60) return `${seconds}s`;
	const minutes = Math.floor(seconds / 60);
	if (minutes < 60) return `${minutes}m ${seconds % 60}s`;
	const hours = Math.floor(minutes / 60);
	return `${hours}h ${minutes % 60}m`;
}

function getRunChildren(children: readonly SubagentChildRecord[], runId: string): SubagentChildRecord[] {
	return children.filter((child) => child.runId === runId).sort(compareChildren);
}

function buildGroupedRows(children: readonly SubagentChildRecord[]): ListRow[] {
	const rows: ListRow[] = [];
	for (const status of STATUS_GROUP_ORDER) {
		const groupChildren = children.filter((child) => child.status === status);
		if (groupChildren.length === 0) continue;
		rows.push({
			type: "group",
			key: `group:${status}`,
			status,
			label: `${getStatusLabel(status)} (${groupChildren.length})`,
		});
		for (const child of groupChildren) {
			rows.push({
				type: "item",
				key: child.id,
				status,
				child,
				label: getChildLabel(child),
			});
		}
	}
	return rows;
}

function firstChildId(rows: readonly ListRow[]): string | undefined {
	return rows.find((row) => row.type === "item")?.child?.id;
}

function countStatuses(children: readonly SubagentChildRecord[]): { active: number; completed: number; failed: number } {
	let active = 0;
	let completed = 0;
	let failed = 0;
	for (const child of children) {
		if (isActiveStatus(child.status)) active++;
		else completed++;
		if (["failed", "aborted", "cancelled", "interrupted"].includes(child.status)) failed++;
	}
	return { active, completed, failed };
}

function formatDisplayItem(item: { type: string; text?: string; toolName?: string; args?: Record<string, unknown> }): string {
	if (item.type === "text") return item.text ?? "";
	if (item.type === "toolCall") {
		const name = item.toolName ?? "tool";
		const args = item.args ?? {};
		switch (name) {
			case "bash": {
				const cmd = typeof args.command === "string" ? args.command : "...";
				return `⚡ $ ${cmd}`;
			}
			case "read":
				return `⚡ read ${String(args.path ?? args.file_path ?? "...")}`;
			case "write":
				return `⚡ write ${String(args.path ?? args.file_path ?? "...")}`;
			case "edit":
				return `⚡ edit ${String(args.path ?? args.file_path ?? "...")}`;
			default: {
				const preview = JSON.stringify(args);
				return `⚡ ${name} ${preview.length > 60 ? `${preview.slice(0, 60)}...` : preview}`;
			}
		}
	}
	return "";
}

type ThinkingFgColor = "thinkingOff" | "thinkingMinimal" | "thinkingLow" | "thinkingMedium" | "thinkingHigh" | "thinkingXhigh" | "dim";

function getThinkingColorName(level: string | undefined): ThinkingFgColor {
	switch (level) {
		case "off":
			return "thinkingOff";
		case "minimal":
			return "thinkingMinimal";
		case "low":
			return "thinkingLow";
		case "medium":
			return "thinkingMedium";
		case "high":
			return "thinkingHigh";
		case "xhigh":
			return "thinkingXhigh";
		default:
			return "dim";
	}
}

export class SubagentInspectorOverlay implements Component {
	private unsubscribe?: () => void;
	private cacheWidth?: number;
	private cacheLines?: string[];
	private selectedRunId?: string;
	private selectedChildId?: string;
	private detailScrollOffset = 0;
	private lastDetailTotalLines = 0;
	private lastDetailViewHeight = 0;

	constructor(
		private readonly tui: TUI,
		private readonly theme: Theme,
		private readonly keybindings: Pick<KeybindingsManager, "matches">,
		private readonly store: SubagentInspectorStore,
		private readonly options: InspectorFactoryOptions,
	) {
		const snapshot = this.store.getSnapshot();
		const runs = [...snapshot.runs].sort(compareRuns);
		this.selectedRunId = runs[0]?.id;
		this.selectedChildId = this.findDefaultChild(snapshot, this.selectedRunId);
		this.unsubscribe = this.store.subscribe(() => {
			this.reconcileSelection();
			this.invalidate();
			this.tui.requestRender();
		});
	}

	handleInput(data: string): void {
		if (matchesInspectorBinding(this.keybindings, data, INSPECTOR_ACTIONS.dismiss)) {
			this.options.onDone({ action: "dismissed", runId: this.selectedRunId, childId: this.selectedChildId });
			return;
		}
		if (matchesInspectorBinding(this.keybindings, data, INSPECTOR_ACTIONS.select)) {
			this.options.onDone({ action: "selected", runId: this.selectedRunId, childId: this.selectedChildId });
			return;
		}
		if (matchesInspectorBinding(this.keybindings, data, INSPECTOR_ACTIONS.scrollDown)) {
			this.scrollDetail(1);
			return;
		}
		if (matchesInspectorBinding(this.keybindings, data, INSPECTOR_ACTIONS.scrollUp)) {
			this.scrollDetail(-1);
			return;
		}
		if (matchesInspectorBinding(this.keybindings, data, INSPECTOR_ACTIONS.nextRun)) {
			this.stepRun(1);
			return;
		}
		if (matchesInspectorBinding(this.keybindings, data, INSPECTOR_ACTIONS.previousRun)) {
			this.stepRun(-1);
			return;
		}
		if (matchesInspectorBinding(this.keybindings, data, INSPECTOR_ACTIONS.nextChild)) {
			this.stepChild(1);
			return;
		}
		if (matchesInspectorBinding(this.keybindings, data, INSPECTOR_ACTIONS.previousChild)) {
			this.stepChild(-1);
			return;
		}
		if (matchesInspectorBinding(this.keybindings, data, INSPECTOR_ACTIONS.jumpStart)) {
			this.scrollToEdge("start");
			return;
		}
		if (matchesInspectorBinding(this.keybindings, data, INSPECTOR_ACTIONS.jumpEnd)) {
			this.scrollToEdge("end");
		}
	}

	render(width: number): string[] {
		if (this.cacheLines && this.cacheWidth === width) return this.cacheLines;

		const safeWidth = Math.max(48, width);
		const innerWidth = safeWidth - 2;
		const border = (text: string) => this.theme.fg("border", text);
		const topTitle = this.options.title ?? "Subagent inspector";
		const lines: string[] = [];

		// Top border
		lines.push(border(`╭${"─".repeat(innerWidth)}╮`));

		// Header + run tabs
		lines.push(border("│") + padRight(this.renderHeader(innerWidth, topTitle), innerWidth) + border("│"));
		lines.push(border("│") + padRight(this.renderRunTabs(innerWidth), innerWidth) + border("│"));
		lines.push(border("├") + border("─".repeat(innerWidth)) + border("┤"));

		// Child list (compact)
		const childListLines = this.renderChildList(innerWidth);
		for (const line of childListLines) {
			lines.push(border("│") + padRight(line, innerWidth) + border("│"));
		}

		// Separator before detail
		lines.push(border("├") + border("─".repeat(innerWidth)) + border("┤"));

		// Scrollable detail section
		const termRows = this.tui.terminal?.rows ?? 40;
		const maxOverlayHeight = Math.floor(termRows * 0.9);
		const overhead = lines.length + 3; // separator + footer + bottom border
		const detailViewHeight = Math.max(6, maxOverlayHeight - overhead);
		const detailLines = this.renderScrollableDetail(innerWidth, detailViewHeight);
		for (const line of detailLines) {
			lines.push(border("│") + padRight(line, innerWidth) + border("│"));
		}

		// Footer
		lines.push(border("├") + border("─".repeat(innerWidth)) + border("┤"));
		lines.push(border("│") + padRight(this.renderFooter(innerWidth), innerWidth) + border("│"));
		lines.push(border(`╰${"─".repeat(innerWidth)}╯`));

		this.cacheWidth = width;
		this.cacheLines = lines;
		return lines;
	}

	invalidate(): void {
		this.cacheWidth = undefined;
		this.cacheLines = undefined;
	}

	dispose(): void {
		this.unsubscribe?.();
		this.unsubscribe = undefined;
	}

	private renderHeader(width: number, title: string): string {
		const snapshot = this.store.getSnapshot();
		const selectedRun = this.getSelectedRun(snapshot);
		const runChildren = selectedRun ? getRunChildren(snapshot.children, selectedRun.id) : [];
		const counts = countStatuses(runChildren);
		const left = this.theme.fg("accent", this.theme.bold(title));
		const parts = [
			left,
			this.theme.fg("dim", `${snapshot.runs.length} runs`),
			this.theme.fg(counts.active > 0 ? "accent" : "dim", `${counts.active} live`),
			this.theme.fg(counts.completed > 0 ? "success" : "dim", `${counts.completed} done`),
		];
		if (counts.failed > 0) parts.push(this.theme.fg("error", `${counts.failed} failed`));
		return truncateToWidth(parts.join("  "), width, "...", true);
	}

	private renderRunTabs(width: number): string {
		const snapshot = this.store.getSnapshot();
		const runs = [...snapshot.runs].sort(compareRuns);
		if (runs.length === 0) {
			return this.theme.fg("dim", "No subagent runs yet.");
		}

		const rendered: string[] = [];
		for (const run of runs.slice(0, 6)) {
			const selected = run.id === this.selectedRunId;
			const label = `${getStatusIcon(run.status)} ${getRunLabel(run)}`;
			const styled = selected
				? this.theme.bg("selectedBg", this.theme.fg("text", ` ${label} `))
				: this.theme.fg(getStatusColor(run.status), label);
			rendered.push(styled);
		}
		if (runs.length > 6) rendered.push(this.theme.fg("dim", `+${runs.length - 6}`));
		return truncateToWidth(rendered.join("  "), width, "...", true);
	}

	private renderChildList(width: number): string[] {
		const safeWidth = Math.max(24, width);
		const lines: string[] = [];
		const snapshot = this.store.getSnapshot();
		const selectedRun = this.getSelectedRun(snapshot);
		const rows = this.getRows(snapshot);
		const maxItems = this.options.listLineLimit ?? 7;

		if (!selectedRun || rows.length === 0) {
			lines.push(padRight(this.theme.fg("dim", "No children in this run."), safeWidth));
			return lines;
		}

		const selectedRowIndex = Math.max(0, rows.findIndex((row) => row.child?.id === this.selectedChildId));
		const windowStart = Math.max(0, selectedRowIndex - Math.floor(maxItems / 2));
		const windowRows = rows.slice(windowStart, windowStart + maxItems);

		for (const row of windowRows) {
			if (row.type === "group") {
				const title = `${getStatusIcon(row.status!)} ${row.label}`;
				lines.push(padRight(this.theme.fg(getStatusColor(row.status!), title), safeWidth));
				continue;
			}
			const selected = row.child?.id === this.selectedChildId;
			const prefix = selected ? this.theme.fg("accent", "▶ ") : "  ";
			const age = formatAge(row.child?.startedAt ?? row.child?.createdAt ?? row.child?.endedAt);
			const baseLabel = truncateToWidth(`${prefix}${row.label}`, Math.max(8, safeWidth - (age ? age.length + 1 : 0)), "...", true);
			const spacing = Math.max(1, safeWidth - visibleWidth(baseLabel) - (age ? age.length : 0));
			const line = age ? `${baseLabel}${" ".repeat(spacing)}${this.theme.fg("dim", age)}` : baseLabel;
			lines.push(selected ? padRight(this.theme.bg("selectedBg", line), safeWidth) : padRight(line, safeWidth));
		}

		return lines;
	}

	private buildAllDetailLines(width: number): string[] {
		const safeWidth = Math.max(28, width);
		const snapshot = this.store.getSnapshot();
		const child = this.getSelectedChild(snapshot);
		const lines: string[] = [];

		if (!child) {
			lines.push(padRight(this.theme.fg("dim", "No child selected."), safeWidth));
			lines.push(padRight(this.theme.fg("dim", "Use Tab / Shift-Tab to navigate children."), safeWidth));
			return lines;
		}

		// Title
		const title = `${getStatusIcon(child.status)} ${getChildLabel(child)}`;
		lines.push(padRight(this.theme.fg(getStatusColor(child.status), this.theme.bold(title)), safeWidth));
		lines.push(" ".repeat(safeWidth));

		// Metadata block
		if (child.agent) {
			lines.push(padRight(`  ${this.theme.fg("muted", "Agent:")} ${child.agent}`, safeWidth));
		}
		lines.push(padRight(`  ${this.theme.fg("muted", "Status:")} ${this.theme.fg(getStatusColor(child.status), getStatusLabel(child.status))}`, safeWidth));
		if (child.thinkingLevel) {
			const color = getThinkingColorName(child.thinkingLevel);
			lines.push(padRight(`  ${this.theme.fg("muted", "Thinking:")} ${this.theme.fg(color, child.thinkingLevel)}`, safeWidth));
		}
		if (child.model) {
			lines.push(padRight(`  ${this.theme.fg("muted", "Model:")} ${child.model}`, safeWidth));
		}
		const started = formatAge(child.startedAt ?? child.createdAt);
		const elapsed = formatDuration(child.startedAt, child.endedAt);
		if (started || elapsed) {
			const timing = [started ? `started ${started} ago` : "", elapsed ? `elapsed ${elapsed}` : ""].filter(Boolean).join(" • ");
			lines.push(padRight(`  ${this.theme.fg("muted", "Time:")} ${timing}`, safeWidth));
		}
		lines.push(" ".repeat(safeWidth));

		// Task
		const taskText = clipParagraph(child.task);
		if (taskText) {
			lines.push(padRight(this.theme.fg("muted", "Task:"), safeWidth));
			for (const wrapped of wrapTextWithAnsi(taskText, safeWidth - 2)) {
				lines.push(padRight(`  ${wrapped}`, safeWidth));
			}
			lines.push(" ".repeat(safeWidth));
		}

		// Chat transcript (displayItems)
		if (child.displayItems && child.displayItems.length > 0) {
			const rule = "─".repeat(Math.max(0, safeWidth - 14));
			lines.push(padRight(this.theme.fg("muted", `─ Transcript ${rule}`), safeWidth));
			for (const item of child.displayItems) {
				const formatted = formatDisplayItem(item);
				if (!formatted.trim()) continue;
				if (item.type === "toolCall") {
					for (const wrapped of wrapTextWithAnsi(this.theme.fg("accent", formatted), safeWidth - 2)) {
						lines.push(padRight(`  ${wrapped}`, safeWidth));
					}
				} else {
					for (const wrapped of wrapTextWithAnsi(formatted, safeWidth - 2)) {
						lines.push(padRight(`  ${wrapped}`, safeWidth));
					}
				}
			}
			lines.push(" ".repeat(safeWidth));
		}

		// Output (only if different from last text display item)
		const outputText = clipParagraph(child.output);
		if (outputText) {
			lines.push(padRight(this.theme.fg("muted", "Output:"), safeWidth));
			for (const wrapped of wrapTextWithAnsi(outputText, safeWidth - 2)) {
				lines.push(padRight(`  ${wrapped}`, safeWidth));
			}
			lines.push(" ".repeat(safeWidth));
		}

		// Error
		const errorText = clipParagraph(child.error);
		if (errorText) {
			lines.push(padRight(this.theme.fg("error", "Error:"), safeWidth));
			for (const wrapped of wrapTextWithAnsi(this.theme.fg("error", errorText), safeWidth - 2)) {
				lines.push(padRight(`  ${wrapped}`, safeWidth));
			}
		}

		return lines;
	}

	private renderScrollableDetail(width: number, viewHeight: number): string[] {
		const allLines = this.buildAllDetailLines(width);
		this.lastDetailTotalLines = allLines.length;
		this.lastDetailViewHeight = viewHeight;

		// Clamp scroll offset
		const maxOffset = Math.max(0, allLines.length - viewHeight);
		if (this.detailScrollOffset > maxOffset) this.detailScrollOffset = maxOffset;
		if (this.detailScrollOffset < 0) this.detailScrollOffset = 0;

		const windowedLines = allLines.slice(this.detailScrollOffset, this.detailScrollOffset + viewHeight);

		// Pad to fill viewport
		while (windowedLines.length < viewHeight) {
			windowedLines.push(" ".repeat(width));
		}

		// Scroll indicators
		if (this.detailScrollOffset > 0 && windowedLines.length > 0) {
			const indicator = this.theme.fg("dim", `↑ ${this.detailScrollOffset} more`);
			windowedLines[0] = padRight(indicator, width);
		}
		if (this.detailScrollOffset + viewHeight < allLines.length && windowedLines.length > 0) {
			const remaining = allLines.length - this.detailScrollOffset - viewHeight;
			const indicator = this.theme.fg("dim", `↓ ${remaining} more`);
			windowedLines[windowedLines.length - 1] = padRight(indicator, width);
		}

		return windowedLines;
	}

	private renderFooter(width: number): string {
		const help = this.options.helpText ?? INSPECTOR_HELP_TEXT;
		return truncateToWidth(this.theme.fg("dim", help), width, "...", true);
	}

	private scrollDetail(direction: 1 | -1): void {
		const maxOffset = Math.max(0, this.lastDetailTotalLines - this.lastDetailViewHeight);
		this.detailScrollOffset = Math.min(maxOffset, Math.max(0, this.detailScrollOffset + direction));
		this.invalidate();
		this.tui.requestRender();
	}

	private scrollToEdge(which: "start" | "end"): void {
		if (which === "start") {
			this.detailScrollOffset = 0;
		} else {
			this.detailScrollOffset = Math.max(0, this.lastDetailTotalLines - this.lastDetailViewHeight);
		}
		this.invalidate();
		this.tui.requestRender();
	}

	private getSelectedRun(snapshot: ReturnType<SubagentInspectorStore["getSnapshot"]>): SubagentRunRecord | undefined {
		const runs = [...snapshot.runs].sort(compareRuns);
		return runs.find((run) => run.id === this.selectedRunId) ?? runs[0];
	}

	private getSelectedChild(snapshot: ReturnType<SubagentInspectorStore["getSnapshot"]>): SubagentChildRecord | undefined {
		const run = this.getSelectedRun(snapshot);
		if (!run) return undefined;
		const children = getRunChildren(snapshot.children, run.id);
		return children.find((child) => child.id === this.selectedChildId) ?? children[0];
	}

	private getRows(snapshot: ReturnType<SubagentInspectorStore["getSnapshot"]>): ListRow[] {
		const run = this.getSelectedRun(snapshot);
		if (!run) return [];
		return buildGroupedRows(getRunChildren(snapshot.children, run.id));
	}

	private findDefaultChild(snapshot: ReturnType<SubagentInspectorStore["getSnapshot"]>, runId?: string): string | undefined {
		if (!runId) return undefined;
		return firstChildId(buildGroupedRows(getRunChildren(snapshot.children, runId)));
	}

	private reconcileSelection(): void {
		const snapshot = this.store.getSnapshot();
		const runs = [...snapshot.runs].sort(compareRuns);
		if (runs.length === 0) {
			this.selectedRunId = undefined;
			this.selectedChildId = undefined;
			return;
		}
		if (!runs.some((run) => run.id === this.selectedRunId)) this.selectedRunId = runs[0]!.id;
		const rows = this.getRows(snapshot);
		if (!rows.some((row) => row.child?.id === this.selectedChildId)) this.selectedChildId = firstChildId(rows);
	}

	private stepRun(direction: 1 | -1): void {
		const snapshot = this.store.getSnapshot();
		const runs = [...snapshot.runs].sort(compareRuns);
		if (runs.length === 0) return;
		const currentIndex = Math.max(0, runs.findIndex((run) => run.id === this.selectedRunId));
		const nextIndex = (currentIndex + direction + runs.length) % runs.length;
		this.selectedRunId = runs[nextIndex]!.id;
		this.selectedChildId = this.findDefaultChild(snapshot, this.selectedRunId);
		this.detailScrollOffset = 0;
		this.invalidate();
		this.tui.requestRender();
	}

	private stepChild(direction: 1 | -1): void {
		const rows = this.getRows(this.store.getSnapshot()).filter((row): row is ListRow & { child: SubagentChildRecord } => row.type === "item" && !!row.child);
		if (rows.length === 0) return;
		const currentIndex = Math.max(0, rows.findIndex((row) => row.child.id === this.selectedChildId));
		const nextIndex = Math.min(rows.length - 1, Math.max(0, currentIndex + direction));
		this.selectedChildId = rows[nextIndex]!.child.id;
		this.detailScrollOffset = 0;
		this.invalidate();
		this.tui.requestRender();
	}
}

export function createInspectorOverlay(
	tui: TUI,
	theme: Theme,
	keybindings: Pick<KeybindingsManager, "matches">,
	store: SubagentInspectorStore,
	onDone: (result: InspectorOpenResult) => void,
	options: InspectorOptions = {},
): Component & { dispose?(): void } {
	return new SubagentInspectorOverlay(tui, theme, keybindings, store, { ...options, onDone });
}

export function getDefaultInspectorOverlayOptions(options: InspectorOptions = {}): OverlayOptions {
	return {
		anchor: "center",
		width: "90%",
		minWidth: 48,
		maxHeight: "90%",
		margin: { top: 1, right: 1, bottom: 1, left: 1 },
		...options.overlayOptions,
	};
}

export async function openInspector(
	ui: Pick<ExtensionUIContext, "custom">,
	store: SubagentInspectorStore,
	options: InspectorOptions = {},
): Promise<InspectorOpenResult> {
	return ui.custom<InspectorOpenResult>(
		(tui, theme, keybindings, done) => createInspectorOverlay(tui, theme, keybindings, store, done, options),
		{
			overlay: true,
			overlayOptions: getDefaultInspectorOverlayOptions(options),
		},
	);
}
