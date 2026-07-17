export const SUBAGENT_INSPECTOR_STATE_TYPE = "subagent-inspector-state";
export const SUBAGENT_INSPECTOR_SNAPSHOT_VERSION = 1;

export type {
	AgentConfig,
	AgentDiscoveryResult,
	AgentScope,
	AgentSource,
	ChildRunRecord,
	DisplayItem,
	RunMode,
	RunRecord,
	RunStatus,
	UsageStats,
} from "../subagent-core/types.js";
export { createEmptyUsage, isTerminalStatus } from "../subagent-core/types.js";

import type { ChildRunRecord, RunRecord } from "../subagent-core/types.js";

export interface ContextWindowUsage {
	tokens: number | null;
	contextWindow: number;
	percent: number | null;
	model?: string;
}

export interface DetailsSnapshot {
	version: number;
	runs: RunRecord[];
	children: ChildRunRecord[];
	updatedAt?: number;
}
