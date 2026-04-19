import type { ExtensionAPI, ExtensionContext } from "@mariozechner/pi-coding-agent";
import type { ChildRunRecord, DetailsSnapshot, RunRecord } from "./types.js";
import {
	SUBAGENT_INSPECTOR_SNAPSHOT_VERSION,
	SUBAGENT_INSPECTOR_STATE_TYPE,
	isTerminalStatus,
} from "./types.js";

interface CustomEntryLike {
	type: string;
	customType?: string;
	data?: unknown;
}

type Listener = () => void;

function cloneChild(child: ChildRunRecord): ChildRunRecord {
	return {
		...child,
		usage: { ...child.usage },
		displayItems: child.displayItems.map((item) =>
			item.type === "text" ? { ...item } : { ...item, args: { ...item.args } },
		),
		metadata: child.metadata ? { ...child.metadata } : undefined,
	};
}

function cloneRun(run: RunRecord): RunRecord {
	return {
		...run,
		childIds: run.childIds ? [...run.childIds] : undefined,
		children: run.children.map(cloneChild),
		metadata: run.metadata ? { ...run.metadata } : undefined,
	};
}

function cloneSnapshot(snapshot: DetailsSnapshot): DetailsSnapshot {
	return {
		version: snapshot.version,
		runs: snapshot.runs.map(cloneRun),
		children: snapshot.children.map(cloneChild),
		updatedAt: snapshot.updatedAt,
	};
}

function normalizeSnapshot(data: unknown): DetailsSnapshot {
	const candidate = data as Partial<DetailsSnapshot> | undefined;
	if (!candidate || !Array.isArray(candidate.runs) || !Array.isArray(candidate.children)) {
		return { version: SUBAGENT_INSPECTOR_SNAPSHOT_VERSION, runs: [], children: [], updatedAt: Date.now() };
	}
	return {
		version:
			typeof candidate.version === "number" ? candidate.version : SUBAGENT_INSPECTOR_SNAPSHOT_VERSION,
		runs: (candidate.runs as RunRecord[]).map(cloneRun),
		children: (candidate.children as ChildRunRecord[]).map(cloneChild),
		updatedAt: typeof candidate.updatedAt === "number" ? candidate.updatedAt : Date.now(),
	};
}

function markInterrupted(snapshot: DetailsSnapshot): DetailsSnapshot {
	const now = Date.now();
	const runs = snapshot.runs.map((run) => {
		const next = cloneRun(run);
		if (!isTerminalStatus(next.status)) {
			next.status = "interrupted";
			next.endedAt ??= now;
			next.error ??= "Interrupted during previous session";
		}
		return next;
	});

	const children = snapshot.children.map((child) => {
		const next = cloneChild(child);
		if (!isTerminalStatus(next.status)) {
			next.status = "interrupted";
			next.endedAt ??= now;
			next.error ??= "Interrupted during previous session";
			next.summary ??= next.output || next.error;
		}
		return next;
	});

	const byRunId = new Map<string, ChildRunRecord[]>();
	for (const child of children) {
		const list = byRunId.get(child.runId) ?? [];
		list.push(child);
		byRunId.set(child.runId, list);
	}

	for (const run of runs) {
		run.children = (byRunId.get(run.id) ?? []).map(cloneChild);
		run.childIds = run.children.map((child) => child.id);
		run.activeChildId = run.children.find((child) => !isTerminalStatus(child.status))?.id;
		run.summary ??= run.children[run.children.length - 1]?.summary;
	}

	return {
		version: SUBAGENT_INSPECTOR_SNAPSHOT_VERSION,
		runs,
		children,
		updatedAt: now,
	};
}

export class SubagentInspectorStore {
	private snapshot: DetailsSnapshot = {
		version: SUBAGENT_INSPECTOR_SNAPSHOT_VERSION,
		runs: [],
		children: [],
		updatedAt: Date.now(),
	};

	private readonly listeners = new Set<Listener>();
	private persistTimer: ReturnType<typeof setTimeout> | undefined;
	private lastPersistedSerialized = "";

	constructor(private readonly pi: ExtensionAPI) {}

	getSnapshot(): DetailsSnapshot {
		return cloneSnapshot(this.snapshot);
	}

	subscribe(listener: Listener): () => void {
		this.listeners.add(listener);
		return () => this.listeners.delete(listener);
	}

	restore(ctx: ExtensionContext): DetailsSnapshot {
		this.clearPersistTimer();
		const branch = ctx.sessionManager.getBranch() as CustomEntryLike[];
		const latest = branch
			.filter((entry) => entry.type === "custom" && entry.customType === SUBAGENT_INSPECTOR_STATE_TYPE)
			.pop();
		const restored = normalizeSnapshot(latest?.data);
		const interrupted = markInterrupted(restored);
		const changed = JSON.stringify(interrupted) !== JSON.stringify(restored);
		this.snapshot = interrupted;
		if (changed) this.persistNow();
		else this.lastPersistedSerialized = JSON.stringify(this.snapshot);
		this.emit();
		return this.getSnapshot();
	}

	upsertRun(run: RunRecord): DetailsSnapshot {
		const runs = this.snapshot.runs.map(cloneRun);
		const children = this.snapshot.children.map(cloneChild);
		const runIndex = runs.findIndex((candidate) => candidate.id === run.id);
		const nextRun = cloneRun(run);
		if (runIndex >= 0) runs[runIndex] = nextRun;
		else runs.push(nextRun);

		const childIds = new Set(nextRun.children.map((child) => child.id));
		const filteredChildren = children.filter((child) => child.runId !== nextRun.id || !childIds.has(child.id));
		for (const child of nextRun.children) filteredChildren.push(cloneChild(child));

		this.snapshot = {
			version: SUBAGENT_INSPECTOR_SNAPSHOT_VERSION,
			runs,
			children: filteredChildren,
			updatedAt: Date.now(),
		};
		if (nextRun.children.every((child) => isTerminalStatus(child.status)) && isTerminalStatus(nextRun.status)) {
			this.persistNow();
		} else {
			this.schedulePersist();
		}
		this.emit();
		return this.getSnapshot();
	}

	flush(): void {
		this.persistNow();
	}

	private schedulePersist(): void {
		if (this.persistTimer) return;
		this.persistTimer = setTimeout(() => {
			this.persistTimer = undefined;
			this.persistNow();
		}, 750);
		this.persistTimer.unref?.();
	}

	private clearPersistTimer(): void {
		if (!this.persistTimer) return;
		clearTimeout(this.persistTimer);
		this.persistTimer = undefined;
	}

	private persistNow(): void {
		this.clearPersistTimer();
		const snapshot = this.getSnapshot();
		const serialized = JSON.stringify(snapshot);
		if (serialized === this.lastPersistedSerialized) return;
		this.lastPersistedSerialized = serialized;
		this.pi.appendEntry(SUBAGENT_INSPECTOR_STATE_TYPE, snapshot);
	}

	private emit(): void {
		for (const listener of this.listeners) listener();
	}
}

export function createSubagentInspectorStore(pi: ExtensionAPI): SubagentInspectorStore {
	return new SubagentInspectorStore(pi);
}
