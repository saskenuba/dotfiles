export type AgentScope = "user" | "project" | "both";
export type AgentSource = "bundled" | "user" | "project" | "unknown";
export type RunMode = "single" | "parallel" | "chain";
export type RunStatus =
	| "queued"
	| "starting"
	| "running"
	| "succeeded"
	| "failed"
	| "aborted"
	| "cancelled"
	| "interrupted"
	| "skipped";

export interface AgentConfig {
	name: string;
	description: string;
	tools?: string[];
	model?: string;
	systemPrompt: string;
	source: Exclude<AgentSource, "unknown">;
	filePath: string;
}

export interface AgentDiscoveryResult {
	agents: AgentConfig[];
	projectAgentsDir: string | null;
	bundledAgentsDir: string;
	userAgentsDir: string;
}

export interface UsageStats {
	input: number;
	output: number;
	cacheRead: number;
	cacheWrite: number;
	cost: number;
	contextTokens: number;
	turns: number;
}

export interface TextDisplayItem {
	type: "text";
	text: string;
}

export interface ToolCallDisplayItem {
	type: "toolCall";
	toolName: string;
	args: Record<string, unknown>;
}

export type DisplayItem = TextDisplayItem | ToolCallDisplayItem;

export interface ChildRunRecord {
	id: string;
	runId: string;
	status: RunStatus;
	agent?: string;
	agentSource?: AgentSource;
	label?: string;
	title?: string;
	summary?: string;
	task?: string;
	output?: string;
	error?: string;
	cwd?: string;
	model?: string;
	thinkingLevel?: string;
	createdAt?: number;
	startedAt?: number;
	endedAt?: number;
	step?: number;
	exitCode?: number;
	stopReason?: string;
	usage: UsageStats;
	displayItems: DisplayItem[];
	metadata?: Record<string, unknown>;
}

export interface RunRecord {
	id: string;
	status: RunStatus;
	label?: string;
	title?: string;
	summary?: string;
	prompt?: string;
	createdAt?: number;
	startedAt?: number;
	endedAt?: number;
	activeChildId?: string;
	childIds?: readonly string[];
	mode: RunMode;
	agentScope: AgentScope;
	cwd: string;
	projectAgentsDir: string | null;
	error?: string;
	children: ChildRunRecord[];
	metadata?: Record<string, unknown>;
}

export interface SubagentRunUpdateEvent {
	version: 1;
	run: RunRecord;
}

export interface SubagentResultDetails {
	jobId: string;
	mode: RunMode;
	status: RunStatus;
	agent: string;
	agentScope?: AgentScope;
	usage?: UsageStats;
	model?: string;
	error?: string;
	runId?: string;
}

export function createEmptyUsage(): UsageStats {
	return {
		input: 0,
		output: 0,
		cacheRead: 0,
		cacheWrite: 0,
		cost: 0,
		contextTokens: 0,
		turns: 0,
	};
}

export function isTerminalStatus(status: RunStatus): boolean {
	return (
		status === "succeeded" ||
		status === "failed" ||
		status === "aborted" ||
		status === "cancelled" ||
		status === "interrupted" ||
		status === "skipped"
	);
}
