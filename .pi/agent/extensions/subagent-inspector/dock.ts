import type { Theme } from "@mariozechner/pi-coding-agent";
import type { Component, OverlayHandle, OverlayOptions, TUI } from "@mariozechner/pi-tui";
import { truncateToWidth, visibleWidth } from "@mariozechner/pi-tui";
import type {
	ChildRunRecord as SubagentChildRecord,
	RunRecord as SubagentRunRecord,
	RunStatus as SubagentStatus,
} from "../subagent-core/types.js";

export interface SubagentInspectorSnapshot {
	runs: readonly SubagentRunRecord[];
	children: readonly SubagentChildRecord[];
	updatedAt?: number;
}

export interface SubagentInspectorStore {
	getSnapshot(): SubagentInspectorSnapshot;
	subscribe(listener: () => void): () => void;
}

export interface DockMetrics {
	totalRuns: number;
	totalChildren: number;
	activeRuns: number;
	activeChildren: number;
	queued: number;
	starting: number;
	running: number;
	succeeded: number;
	failed: number;
	ended: number;
}

export interface DockComponentOptions {
	label?: string;
	maxItems?: number;
	minWidth?: number;
	showRunLabel?: boolean;
	showIdleWhenEmpty?: boolean;
	compactThreshold?: number;
}

export interface DockOverlayOptions extends DockComponentOptions {
	overlayOptions?: OverlayOptions;
}

export interface DockHost {
	readonly headerFactory: (tui: TUI, theme: Theme) => Component & { dispose?(): void };
	createHeaderComponent(tui: TUI, theme: Theme): Component & { dispose?(): void };
	showOverlay(tui: TUI, theme: Theme, options?: OverlayOptions): OverlayHandle;
}

export interface DockControllerOptions extends DockOverlayOptions {
	overlayVisible?: (snapshot: SubagentInspectorSnapshot) => boolean;
}

const ACTIVE_STATUSES: ReadonlySet<SubagentStatus> = new Set(["queued", "starting", "running"]);
const FAILURE_STATUSES: ReadonlySet<SubagentStatus> = new Set(["failed", "aborted", "cancelled", "interrupted"]);
const STATUS_PRIORITY: readonly SubagentStatus[] = [
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

export function isActiveStatus(status: SubagentStatus): boolean {
	return ACTIVE_STATUSES.has(status);
}

export function isFailureStatus(status: SubagentStatus): boolean {
	return FAILURE_STATUSES.has(status);
}

export function getStatusLabel(status: SubagentStatus): string {
	switch (status) {
		case "queued":
			return "queued";
		case "starting":
			return "starting";
		case "running":
			return "running";
		case "succeeded":
			return "succeeded";
		case "failed":
			return "failed";
		case "aborted":
			return "aborted";
		case "cancelled":
			return "cancelled";
		case "interrupted":
			return "interrupted";
		case "skipped":
			return "skipped";
	}
}

export function getStatusColor(status: SubagentStatus): "accent" | "warning" | "success" | "error" | "muted" {
	switch (status) {
		case "running":
			return "accent";
		case "starting":
		case "queued":
			return "warning";
		case "succeeded":
			return "success";
		case "failed":
		case "aborted":
		case "cancelled":
		case "interrupted":
			return "error";
		case "skipped":
			return "muted";
	}
}

export function getStatusIcon(status: SubagentStatus): string {
	switch (status) {
		case "queued":
			return "○";
		case "starting":
			return "◔";
		case "running":
			return "●";
		case "succeeded":
			return "✓";
		case "failed":
			return "✕";
		case "aborted":
			return "!";
		case "cancelled":
			return "⊘";
		case "interrupted":
			return "↯";
		case "skipped":
			return "→";
	}
}

export function getRunLabel(run: SubagentRunRecord): string {
	return run.label?.trim() || run.title?.trim() || `Run ${run.id.slice(0, 6)}`;
}

export function getChildLabel(child: SubagentChildRecord): string {
	return child.label?.trim() || child.title?.trim() || child.agent?.trim() || `Child ${child.id.slice(0, 6)}`;
}

export function compareRuns(a: SubagentRunRecord, b: SubagentRunRecord): number {
	const aActive = Number(isActiveStatus(a.status));
	const bActive = Number(isActiveStatus(b.status));
	if (aActive !== bActive) return bActive - aActive;
	const aTs = a.startedAt ?? a.createdAt ?? a.endedAt ?? 0;
	const bTs = b.startedAt ?? b.createdAt ?? b.endedAt ?? 0;
	return bTs - aTs;
}

export function compareChildren(a: SubagentChildRecord, b: SubagentChildRecord): number {
	const aStatus = STATUS_PRIORITY.indexOf(a.status);
	const bStatus = STATUS_PRIORITY.indexOf(b.status);
	if (aStatus !== bStatus) return aStatus - bStatus;
	const aTs = a.startedAt ?? a.createdAt ?? a.endedAt ?? 0;
	const bTs = b.startedAt ?? b.createdAt ?? b.endedAt ?? 0;
	return bTs - aTs;
}

export function buildDockMetrics(snapshot: SubagentInspectorSnapshot): DockMetrics {
	const metrics: DockMetrics = {
		totalRuns: snapshot.runs.length,
		totalChildren: snapshot.children.length,
		activeRuns: 0,
		activeChildren: 0,
		queued: 0,
		starting: 0,
		running: 0,
		succeeded: 0,
		failed: 0,
		ended: 0,
	};

	for (const run of snapshot.runs) {
		if (isActiveStatus(run.status)) metrics.activeRuns++;
	}

	for (const child of snapshot.children) {
		if (isActiveStatus(child.status)) metrics.activeChildren++;
		if (child.status === "queued") metrics.queued++;
		if (child.status === "starting") metrics.starting++;
		if (child.status === "running") metrics.running++;
		if (child.status === "succeeded") metrics.succeeded++;
		if (isFailureStatus(child.status)) metrics.failed++;
		if (!isActiveStatus(child.status)) metrics.ended++;
	}

	return metrics;
}

export function formatAge(timestamp?: number, now = Date.now()): string {
	if (!timestamp) return "";
	const delta = Math.max(0, now - timestamp);
	const sec = Math.floor(delta / 1000);
	if (sec < 60) return `${sec}s`;
	const min = Math.floor(sec / 60);
	if (min < 60) return `${min}m`;
	const hr = Math.floor(min / 60);
	if (hr < 24) return `${hr}h`;
	const day = Math.floor(hr / 24);
	return `${day}d`;
}

function padRight(text: string, width: number): string {
	const rendered = truncateToWidth(text, width, "...", true);
	return rendered + " ".repeat(Math.max(0, width - visibleWidth(rendered)));
}

function joinSegments(segments: string[], width: number): string {
	return padRight(segments.join("  "), width);
}

function renderMetric(theme: Theme, label: string, value: number, color: "accent" | "warning" | "success" | "error" | "muted"): string {
	return theme.fg(color, `${label} ${value}`);
}

function getTopItems(snapshot: SubagentInspectorSnapshot, maxItems: number): readonly SubagentChildRecord[] {
	return [...snapshot.children].sort(compareChildren).slice(0, Math.max(0, maxItems));
}

export class SubagentDock implements Component {
	private unsub?: () => void;
	private cacheWidth?: number;
	private cacheLines?: string[];

	constructor(
		private readonly tui: TUI,
		private readonly theme: Theme,
		private readonly store: SubagentInspectorStore,
		private readonly options: DockComponentOptions = {},
	) {
		this.unsub = this.store.subscribe(() => {
			this.invalidate();
			this.tui.requestRender();
		});
	}

	render(width: number): string[] {
		if (this.cacheLines && this.cacheWidth === width) return this.cacheLines;

		const lines = renderDockLines(this.theme, this.store.getSnapshot(), width, this.options);
		this.cacheWidth = width;
		this.cacheLines = lines;
		return lines;
	}

	invalidate(): void {
		this.cacheWidth = undefined;
		this.cacheLines = undefined;
	}

	dispose(): void {
		this.unsub?.();
		this.unsub = undefined;
	}
}

class FramedDockOverlay implements Component {
	private inner: SubagentDock;
	private cacheWidth?: number;
	private cacheLines?: string[];

	constructor(
		private readonly tui: TUI,
		private readonly theme: Theme,
		private readonly store: SubagentInspectorStore,
		private readonly options: DockComponentOptions = {},
	) {
		this.inner = new SubagentDock(tui, theme, store, options);
	}

	render(width: number): string[] {
		if (this.cacheLines && this.cacheWidth === width) return this.cacheLines;
		const innerWidth = Math.max(8, width - 2);
		const body = this.inner.render(innerWidth);
		const border = this.theme.fg("border", "│");
		const lines: string[] = [];
		lines.push(this.theme.fg("border", `╭${"─".repeat(innerWidth)}╮`));
		for (const line of body) lines.push(border + padRight(line, innerWidth) + border);
		lines.push(this.theme.fg("border", `╰${"─".repeat(innerWidth)}╯`));
		this.cacheWidth = width;
		this.cacheLines = lines;
		return lines;
	}

	invalidate(): void {
		this.cacheWidth = undefined;
		this.cacheLines = undefined;
		this.inner.invalidate();
	}

	dispose(): void {
		this.inner.dispose();
	}
}

export function renderDockLines(
	theme: Theme,
	snapshot: SubagentInspectorSnapshot,
	width: number,
	options: DockComponentOptions = {},
): string[] {
	const safeWidth = Math.max(8, width);
	const metrics = buildDockMetrics(snapshot);
	const label = options.label ?? "subagents";
	const compactThreshold = options.compactThreshold ?? 26;
	const showIdle = options.showIdleWhenEmpty ?? true;
	const lines: string[] = [];

	if (metrics.totalRuns === 0 && metrics.totalChildren === 0) {
		if (!showIdle) return [];
		const idle = `${theme.fg("muted", label)} ${theme.fg("dim", "idle")}`;
		return [padRight(truncateToWidth(idle, safeWidth, "...", true), safeWidth)];
	}

	if (safeWidth <= compactThreshold) {
		const compact = [
			theme.fg("muted", label),
			theme.fg("accent", String(metrics.activeChildren || metrics.activeRuns || metrics.totalChildren)),
			metrics.failed > 0 ? theme.fg("error", `!${metrics.failed}`) : theme.fg("dim", `${metrics.ended}`),
		];
		return [padRight(truncateToWidth(compact.join(" "), safeWidth, "...", true), safeWidth)];
	}

	const summary: string[] = [
		theme.fg("muted", label),
		renderMetric(theme, "live", metrics.activeChildren, metrics.activeChildren > 0 ? "accent" : "muted"),
		renderMetric(theme, "ok", metrics.succeeded, metrics.succeeded > 0 ? "success" : "muted"),
	];
	if (metrics.failed > 0) summary.push(renderMetric(theme, "bad", metrics.failed, "error"));
	else if (metrics.ended > 0) summary.push(renderMetric(theme, "done", metrics.ended, "muted"));
	lines.push(joinSegments(summary, safeWidth));

	const detail: string[] = [];
	if (metrics.running > 0) detail.push(renderMetric(theme, "run", metrics.running, "accent"));
	if (metrics.starting > 0) detail.push(renderMetric(theme, "start", metrics.starting, "warning"));
	if (metrics.queued > 0) detail.push(renderMetric(theme, "queue", metrics.queued, "warning"));
	if (detail.length > 0) lines.push(joinSegments(detail, safeWidth));

	const topItems = getTopItems(snapshot, options.maxItems ?? 2);
	for (const child of topItems) {
		const status = theme.fg(getStatusColor(child.status), `${getStatusIcon(child.status)} ${getChildLabel(child)}`);
		const age = formatAge(child.startedAt ?? child.createdAt ?? child.endedAt);
		const runPrefix = options.showRunLabel ? `${getRunName(snapshot, child.runId)} · ` : "";
		const line = age
			? `${status} ${theme.fg("dim", `${runPrefix}${age}`)}`
			: `${status} ${theme.fg("dim", runPrefix.trim())}`;
		lines.push(padRight(truncateToWidth(line.trimEnd(), safeWidth, "...", true), safeWidth));
	}

	return lines.slice(0, 4);
}

function getRunName(snapshot: SubagentInspectorSnapshot, runId: string): string {
	const run = snapshot.runs.find((entry) => entry.id === runId);
	return run ? getRunLabel(run) : runId.slice(0, 6);
}

class PersistentDockController implements Component {
	private readonly host: DockHost;
	private readonly unsubscribe: () => void;
	private overlayHandle: OverlayHandle | null = null;

	constructor(
		private readonly tui: TUI,
		private readonly theme: Theme,
		private readonly store: SubagentInspectorStore,
		private readonly options: DockControllerOptions,
	) {
		this.host = createDockHost(store, options);
		this.unsubscribe = this.store.subscribe(() => {
			this.syncOverlay();
			this.tui.requestRender();
		});
	}

	render(_width: number): string[] {
		this.syncOverlay();
		return [];
	}

	invalidate(): void {}

	dispose(): void {
		this.unsubscribe();
		this.hideOverlay();
	}

	private syncOverlay(): void {
		const visible = this.options.overlayVisible?.(this.store.getSnapshot()) ?? true;
		if (!visible) {
			this.hideOverlay();
			return;
		}
		if (!this.overlayHandle) {
			this.overlayHandle = this.host.showOverlay(this.tui, this.theme, this.options.overlayOptions);
		}
	}

	private hideOverlay(): void {
		if (!this.overlayHandle) return;
		this.overlayHandle.hide();
		this.overlayHandle = null;
	}
}

export function createPersistentDockController(
	tui: TUI,
	theme: Theme,
	store: SubagentInspectorStore,
	options: DockControllerOptions = {},
): Component & { dispose?(): void } {
	return new PersistentDockController(tui, theme, store, options);
}

export function createDockHost(store: SubagentInspectorStore, options: DockOverlayOptions = {}): DockHost {
	const { overlayOptions, ...componentOptions } = options;

	const createHeaderComponent = (tui: TUI, theme: Theme) => new SubagentDock(tui, theme, store, componentOptions);

	return {
		headerFactory: createHeaderComponent,
		createHeaderComponent,
		showOverlay(tui: TUI, theme: Theme, overrideOptions?: OverlayOptions): OverlayHandle {
			return tui.showOverlay(new FramedDockOverlay(tui, theme, store, componentOptions), {
				nonCapturing: true,
				anchor: "top-right",
				width: 34,
				minWidth: componentOptions.minWidth ?? 24,
				margin: { top: 1, right: 1 },
				...overlayOptions,
				...overrideOptions,
			});
		},
	};
}
