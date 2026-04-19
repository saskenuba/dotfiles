import { randomUUID } from "node:crypto";
import type { ExtensionAPI, ExtensionContext } from "@mariozechner/pi-coding-agent";
import { StringEnum } from "@mariozechner/pi-ai";
import { Text } from "@mariozechner/pi-tui";
import { Type } from "@sinclair/typebox";
import { discoverAgents } from "./agents.js";
import { MAX_SUBAGENT_DEPTH, SUBAGENT_DEPTH_ENV, SUBAGENT_RESULT_MESSAGE_TYPE, SUBAGENT_RUN_UPDATE_EVENT } from "./constants.js";
import {
	buildFinalOutput,
	runChainTasks,
	runParallelTasks,
	runSingleTask,
	type ChainTaskInput,
	type RunnerContext,
	type SingleTaskInput,
} from "./runner.js";
import type {
	AgentConfig,
	AgentScope,
	RunMode,
	RunRecord,
	SubagentResultDetails,
	SubagentRunUpdateEvent,
	UsageStats,
} from "./types.js";
import type { ThinkingLevel } from "./thinking.js";

const DEFAULT_AGENT_SCOPE: AgentScope = "user";
const DEFAULT_SCOUT_AGENT = "subagent-scout";
const BUNDLED_AGENT_NAMES = [DEFAULT_SCOUT_AGENT] as const;

const TaskItem = Type.Object({
	agent: Type.Optional(Type.String({ description: `Agent name to run. Defaults to ${DEFAULT_SCOUT_AGENT} in single mode.` })),
	task: Type.String({ description: "Delegated task for that agent" }),
	role: Type.Optional(Type.String({ description: "Expert role for ad-hoc mode when no named agent fits (e.g., 'Senior UX Designer', 'Staff Engineer', 'Principal Architect'). Ignored when a matching agent is found." })),
	cwd: Type.Optional(Type.String({ description: "Optional working directory for that child run" })),
});

const SubagentParams = Type.Object({
	agent: Type.Optional(Type.String({ description: `Single-mode agent name. Defaults to ${DEFAULT_SCOUT_AGENT}.` })),
	task: Type.Optional(Type.String({ description: "Single-mode delegated task" })),
	role: Type.Optional(Type.String({ description: "Expert role for ad-hoc single-mode (e.g., 'Senior UX Designer', 'Staff Engineer'). Used when no named agent fits the task." })),
	tasks: Type.Optional(Type.Array(TaskItem, { minItems: 1, description: "Parallel-mode child tasks" })),
	chain: Type.Optional(
		Type.Array(TaskItem, {
			minItems: 1,
			description: "Chain-mode steps. Later tasks may interpolate {previous}.",
		}),
	),
	agentScope: Type.Optional(
		StringEnum(["user", "project", "both"] as const, {
			description:
				'Agent discovery scope. Default: "user". "user" includes bundled agents plus ~/.pi/agent/agents; "project" adds only bundled + .pi/agents; "both" includes all three.',
		}),
	),
	confirmProjectAgents: Type.Optional(
		Type.Boolean({
			description: "When true, prompt before using repo-controlled .pi/agents definitions in interactive UI mode.",
			default: true,
		}),
	),
	cwd: Type.Optional(Type.String({ description: "Single-mode working directory override" })),
});

type SubagentToolParams = {
	agent?: string;
	task?: string;
	role?: string;
	tasks?: SingleTaskInput[];
	chain?: ChainTaskInput[];
	agentScope?: AgentScope;
	confirmProjectAgents?: boolean;
	cwd?: string;
};

interface ActiveJob {
	jobId: string;
	mode: RunMode;
	agent: string;
	abortController: AbortController;
	killed: boolean;
	promise: Promise<void>;
}

interface StartedDetails {
	kind: "started";
	jobId: string;
	mode: RunMode;
	agent: string;
	agentScope: AgentScope;
}

interface RegisterSubagentToolOptions {
	bundledAgentsDir: string;
	getDefaultModel: (ctx: ExtensionContext) => string | undefined;
	getDefaultTools: () => string[];
	getParentThinkingLevel: () => ThinkingLevel;
}

function isSingleMode(params: SubagentToolParams): params is SubagentToolParams & { task: string } {
	return typeof params.task === "string";
}

function normalizeSingleInput(params: SubagentToolParams): SingleTaskInput {
	const role = params.role?.trim() || undefined;
	return {
		agent: params.agent?.trim() || (role ? "" : DEFAULT_SCOUT_AGENT),
		task: params.task?.trim() ?? "",
		cwd: params.cwd,
		role,
	};
}

function normalizeParallelInputs(inputs: SingleTaskInput[] = []): SingleTaskInput[] {
	return inputs.map((input) => {
		const role = input.role?.trim() || undefined;
		return {
			agent: input.agent?.trim() || (role ? "" : DEFAULT_SCOUT_AGENT),
			task: input.task,
			cwd: input.cwd,
			role,
		};
	});
}

function buildAgentCatalog(
	scope: AgentScope,
	cwd: string,
	bundledAgentsDir: string,
): { agents: AgentConfig[]; projectAgentsDir: string | null } {
	const discovery = discoverAgents(cwd, scope, bundledAgentsDir);
	return {
		agents: discovery.agents,
		projectAgentsDir: discovery.projectAgentsDir,
	};
}

function renderAgentsText(scope: AgentScope, cwd: string, bundledAgentsDir: string): string {
	const discovery = discoverAgents(cwd, scope, bundledAgentsDir);
	if (discovery.agents.length === 0) return `No agents found for scope "${scope}".`;
	const lines = discovery.agents.map((agent) => {
		const model = agent.model ? ` • ${agent.model}` : " • current parent model";
		const tools = agent.tools?.length ? ` • tools: ${agent.tools.join(", ")}` : " • tools: default";
		return `- ${agent.name} [${agent.source}] — ${agent.description}${model}${tools}`;
	});
	return lines.join("\n");
}

function detectMode(params: SubagentToolParams): "single" | "parallel" | "chain" | undefined {
	const hasSingle = isSingleMode(params);
	const hasParallel = Array.isArray(params.tasks) && params.tasks.length > 0;
	const hasChain = Array.isArray(params.chain) && params.chain.length > 0;
	const count = Number(hasSingle) + Number(hasParallel) + Number(hasChain);
	if (count !== 1) return undefined;
	if (hasChain) return "chain";
	if (hasParallel) return "parallel";
	return "single";
}

async function confirmProjectAgentsIfNeeded(
	ctx: ExtensionContext,
	params: SubagentToolParams,
	agents: AgentConfig[],
	projectAgentsDir: string | null,
	agentScope: AgentScope,
): Promise<boolean> {
	if (!ctx.hasUI) return true;
	if ((params.confirmProjectAgents ?? true) === false) return true;
	if (agentScope !== "project" && agentScope !== "both") return true;

	const requestedNames = new Set<string>();
	if (isSingleMode(params)) requestedNames.add(normalizeSingleInput(params).agent);
	for (const task of params.tasks ?? []) requestedNames.add(task.agent?.trim() || DEFAULT_SCOUT_AGENT);
	for (const step of params.chain ?? []) requestedNames.add(step.agent?.trim() || DEFAULT_SCOUT_AGENT);

	const requestedProjectAgents = Array.from(requestedNames)
		.map((name) => agents.find((agent) => agent.name === name))
		.filter((agent): agent is AgentConfig => agent?.source === "project");

	if (requestedProjectAgents.length === 0) return true;

	const names = requestedProjectAgents.map((agent) => agent.name).join(", ");
	return ctx.ui.confirm(
		"Run project-local agents?",
		`Using repo-controlled agents: ${names}\nSource: ${projectAgentsDir ?? "(unknown)"}\n\nContinue only if this repository is trusted.`,
	);
}

function aggregateRunUsage(run: RunRecord): UsageStats {
	const total: UsageStats = {
		input: 0,
		output: 0,
		cacheRead: 0,
		cacheWrite: 0,
		cost: 0,
		contextTokens: 0,
		turns: 0,
	};
	for (const child of run.children) {
		total.input += child.usage.input;
		total.output += child.usage.output;
		total.cacheRead += child.usage.cacheRead;
		total.cacheWrite += child.usage.cacheWrite;
		total.cost += child.usage.cost;
		total.contextTokens = Math.max(total.contextTokens, child.usage.contextTokens);
		total.turns += child.usage.turns;
	}
	return total;
}

function describePrimary(params: SubagentToolParams, mode: RunMode): string {
	if (mode === "single") {
		const single = normalizeSingleInput(params);
		return single.agent || single.role || "ad-hoc";
	}
	if (mode === "parallel") return `parallel (${params.tasks?.length ?? 0})`;
	return `chain (${params.chain?.length ?? 0})`;
}

export function registerSubagentTool(pi: ExtensionAPI, options: RegisterSubagentToolOptions): void {
	const activeJobs = new Map<string, ActiveJob>();

	const emitRunUpdate = (run: RunRecord) => {
		pi.events.emit(SUBAGENT_RUN_UPDATE_EVENT, {
			version: 1,
			run,
		} satisfies SubagentRunUpdateEvent);
	};

	const deliverResultNudge = (jobId: string, run: RunRecord, primaryAgent: string) => {
		const rawOutput = buildFinalOutput(run) || "(no subagent output)";
		const header = `[${SUBAGENT_RESULT_MESSAGE_TYPE} jobId=${jobId} agent=${primaryAgent} status=${run.status}]`;
		const text = `${header}\n${rawOutput}`;
		pi.sendMessage<SubagentResultDetails>(
			{
				customType: SUBAGENT_RESULT_MESSAGE_TYPE,
				content: text,
				display: true,
				details: {
					jobId,
					runId: run.id,
					mode: run.mode,
					status: run.status,
					agent: primaryAgent,
					agentScope: run.agentScope,
					usage: aggregateRunUsage(run),
					model: run.children.find((child) => child.model)?.model,
					error: run.children.find((child) => child.error)?.error,
				},
			},
			{
				deliverAs: "followUp",
				triggerTurn: true,
			},
		);
	};

	const deliverErrorNudge = (jobId: string, mode: RunMode, primaryAgent: string, errorMessage: string) => {
		pi.sendMessage<SubagentResultDetails>(
			{
				customType: SUBAGENT_RESULT_MESSAGE_TYPE,
				content: `[${SUBAGENT_RESULT_MESSAGE_TYPE} jobId=${jobId} agent=${primaryAgent} status=failed]\nSubagent job ${jobId} failed before completion: ${errorMessage}`,
				display: true,
				details: {
					jobId,
					mode,
					status: "failed",
					agent: primaryAgent,
					error: errorMessage,
				},
			},
			{
				deliverAs: "followUp",
				triggerTurn: true,
			},
		);
	};

	const launchJob = async (
		params: SubagentToolParams,
		ctx: ExtensionContext,
	): Promise<
		| { ok: true; jobId: string; mode: RunMode; agent: string; agentScope: AgentScope }
		| { ok: false; reason: string }
	> => {
		const mode = detectMode(params);
		if (!mode) {
			return {
				ok: false,
				reason:
					"Provide exactly one mode: {task} or {agent, task} for single, {tasks:[...]} for parallel, or {chain:[...]} for chain.",
			};
		}

		if (mode === "single" && normalizeSingleInput(params).task.length === 0) {
			return { ok: false, reason: "Single-mode subagent runs require a non-empty task." };
		}
		if (mode === "parallel" && normalizeParallelInputs(params.tasks ?? []).some((task) => task.task.trim().length === 0)) {
			return { ok: false, reason: "Every parallel subagent task needs a non-empty task string." };
		}
		if (mode === "chain" && normalizeParallelInputs(params.chain ?? []).some((task) => task.task.trim().length === 0)) {
			return { ok: false, reason: "Every chain subagent step needs a non-empty task string." };
		}

		const agentScope = params.agentScope ?? DEFAULT_AGENT_SCOPE;
		const catalog = buildAgentCatalog(agentScope, ctx.cwd, options.bundledAgentsDir);
		if (!(await confirmProjectAgentsIfNeeded(ctx, params, catalog.agents, catalog.projectAgentsDir, agentScope))) {
			return { ok: false, reason: "Canceled: project-local agents were not approved." };
		}

		const jobId = `job-${randomUUID().slice(0, 8)}`;
		const primaryAgent = describePrimary(params, mode);
		const jobAbort = new AbortController();
		const currentDepth = parseInt(process.env[SUBAGENT_DEPTH_ENV] || "0", 10);
		if (currentDepth >= MAX_SUBAGENT_DEPTH) {
			return {
				ok: false,
				reason: `Subagent depth limit reached (${MAX_SUBAGENT_DEPTH}). Cannot spawn nested subagents beyond this depth.`,
			};
		}

		const job: ActiveJob = { jobId, mode, agent: primaryAgent, abortController: jobAbort, killed: false, promise: Promise.resolve() };
		activeJobs.set(jobId, job);

		const runnerContext: RunnerContext = {
			cwd: ctx.cwd,
			agents: catalog.agents,
			agentScope,
			projectAgentsDir: catalog.projectAgentsDir,
			defaultModel: options.getDefaultModel(ctx),
			defaultTools: options.getDefaultTools(),
			parentThinkingLevel: options.getParentThinkingLevel(),
			signal: jobAbort.signal,
			onRunUpdate: emitRunUpdate,
		};

		const runPromise: Promise<RunRecord> = (async () => {
			if (mode === "single") {
				return runSingleTask(
					{ ...normalizeSingleInput(params), cwd: params.cwd },
					runnerContext,
				);
			}
			if (mode === "parallel") {
				return runParallelTasks(normalizeParallelInputs(params.tasks ?? []), runnerContext);
			}
			return runChainTasks(normalizeParallelInputs(params.chain ?? []), runnerContext);
		})();

		job.promise = runPromise
			.then((run) => {
				if (job.killed) return;
				deliverResultNudge(jobId, run, primaryAgent);
			})
			.catch((err) => {
				if (job.killed) return;
				const errorMessage = err instanceof Error ? err.message : String(err);
				deliverErrorNudge(jobId, mode, primaryAgent, errorMessage);
			})
			.finally(() => {
				activeJobs.delete(jobId);
			});

		return { ok: true, jobId, mode, agent: primaryAgent, agentScope };
	};

	pi.registerTool({
		name: "subagent",
		label: "Subagent",
		description:
			`Delegate work to isolated subagents (single, parallel, or chain). FIRE-AND-FORGET: returns immediately with a job handle; the actual result arrives later as a follow-up "${SUBAGENT_RESULT_MESSAGE_TYPE}" message. Default bundled scout: ${DEFAULT_SCOUT_AGENT}. When no named agent fits, omit agent and provide role for an ad-hoc expert.`,
		promptSnippet:
			`Delegate work to the default scout (${BUNDLED_AGENT_NAMES.map((name) => `\`${name}\``).join(", ")}), discovered agents, or ad-hoc experts via role in single, parallel, or chain mode. Fire-and-forget: returns a job handle immediately; the result arrives as a follow-up message.`,
		promptGuidelines: [
			"`subagent` is fire-and-forget: it returns immediately with a job handle. Do NOT call it again to wait for previous calls. Continue with other useful work in the meantime.",
			`When a subagent completes, you receive a follow-up message with \`customType: ${SUBAGENT_RESULT_MESSAGE_TYPE}\` containing its full output. Act on it then.`,
			`If a \`${SUBAGENT_RESULT_MESSAGE_TYPE}\` nudge fires while you are idle, its content may arrive alongside the next user message. The nudge content is always prefixed with \`[${SUBAGENT_RESULT_MESSAGE_TYPE} jobId=... agent=... status=...]\`\\n before the subagent's output — inspect each turn's input for that marker and react to it even when it appears merged with user text.`,
			`Use \`${DEFAULT_SCOUT_AGENT}\` for fast reconnaissance, file location, and scoped codebase discovery.`,
			"When no discovered agent fits the task, omit `agent` and provide `role` with a senior expert title (e.g., 'Senior UX Designer', 'Staff Engineer', 'Principal Architect'). The subagent will adopt that expert persona. Always delegate to the highest appropriate seniority level.",
			"Prefer single mode for one focused task, parallel mode for independent investigations, and chain mode when later steps should consume `{previous}` output from earlier steps.",
			"Keep delegated tasks narrow and explicit; include file paths, success criteria, and relevant constraints in the child task text.",
		],
		parameters: SubagentParams,
		async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
			const result = await launchJob(params as SubagentToolParams, ctx);
			if (!result.ok) {
				const reason = result.reason;
				return {
					content: [{ type: "text", text: reason }],
					isError: true,
				};
			}
			const { jobId, mode, agent, agentScope } = result;
			const startedDetails: StartedDetails = { kind: "started", jobId, mode, agent, agentScope };
			return {
				content: [
					{
						type: "text",
						text:
							`Subagent job ${jobId} started in background (mode: ${mode}, agent: ${agent}). ` +
							`Continue with other work — the result will arrive as a follow-up "${SUBAGENT_RESULT_MESSAGE_TYPE}" ` +
							"message when the job completes. Do not poll; do not call `subagent` again to wait for this job.",
					},
				],
				details: startedDetails,
			};
		},
		renderCall(args, theme) {
			const params = args as SubagentToolParams;
			const mode = detectMode(params);
			const scope = params.agentScope ?? DEFAULT_AGENT_SCOPE;
			if (mode === "single") {
				const single = normalizeSingleInput(params);
				const displayName = single.agent || single.role || "ad-hoc";
				return new Text(
					`${theme.fg("toolTitle", theme.bold("subagent "))}${theme.fg("accent", displayName)}${theme.fg("dim", ` [${scope}]`)}\n${theme.fg("dim", single.task)}`,
					0,
					0,
				);
			}
			if (mode === "parallel") {
				return new Text(
					`${theme.fg("toolTitle", theme.bold("subagent "))}${theme.fg("accent", `parallel (${params.tasks?.length ?? 0})`)}${theme.fg("dim", ` [${scope}]`)}`,
					0,
					0,
				);
			}
			if (mode === "chain") {
				return new Text(
					`${theme.fg("toolTitle", theme.bold("subagent "))}${theme.fg("accent", `chain (${params.chain?.length ?? 0})`)}${theme.fg("dim", ` [${scope}]`)}`,
					0,
					0,
				);
			}
			return new Text(theme.fg("warning", "subagent: invalid arguments"), 0, 0);
		},
		renderResult(result, _options, theme) {
			const details = result.details as StartedDetails | undefined;
			if (details?.kind === "started") {
				return new Text(
					`${theme.fg("muted", "↪")} ${theme.fg("toolTitle", "subagent ")}${theme.fg("accent", details.agent)}` +
						`${theme.fg("muted", ` [${details.jobId}]`)}` +
						`\n${theme.fg("dim", "running in background — result will arrive as a follow-up message")}`,
					0,
					0,
				);
			}
			const first = result.content.find((item) => item.type === "text");
			return new Text(first?.type === "text" ? first.text : "(no subagent output)", 0, 0);
		},
	});

	pi.registerCommand("agent-catalog", {
		description: "List discovered bundled, user, and project subagent definitions",
		getArgumentCompletions: (prefix) => {
			const values = ["user", "project", "both"];
			const items = values.filter((value) => value.startsWith(prefix)).map((value) => ({ value, label: value }));
			return items.length > 0 ? items : null;
		},
		handler: async (args, ctx) => {
			const raw = args.trim();
			const scope: AgentScope = raw === "project" || raw === "both" || raw === "user" ? raw : "both";
			const text = renderAgentsText(scope, ctx.cwd, options.bundledAgentsDir);
			if (ctx.hasUI) {
				ctx.ui.notify(text, "info");
				return;
			}
			console.log(text);
		},
	});

	pi.on("session_shutdown", async () => {
		const pendingJobs = [...activeJobs.values()];
		for (const job of pendingJobs) {
			job.killed = true;
			try {
				job.abortController.abort();
			} catch {
				/* ignore */
			}
		}
		if (pendingJobs.length > 0) {
			const timeout = new Promise<void>((resolve) => {
				const timer = setTimeout(resolve, 5000);
				(timer as { unref?: () => void }).unref?.();
			});
			await Promise.race([
				Promise.allSettled(pendingJobs.map((j) => j.promise)),
				timeout,
			]);
		}
		activeJobs.clear();
	});
}
