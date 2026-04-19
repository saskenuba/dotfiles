import { spawn } from "node:child_process";
import * as fsSync from "node:fs";
import * as fs from "node:fs/promises";
import * as os from "node:os";
import * as path from "node:path";
import type { Message } from "@mariozechner/pi-ai";
import { SUBAGENT_DEPTH_ENV } from "./constants.js";
import { formatTokens, formatUsage } from "./formatting.js";
import { chooseDelegatedThinkingLevel, type ThinkingLevel } from "./thinking.js";
import type { AgentConfig, ChildRunRecord, DisplayItem, RunMode, RunRecord, RunStatus, UsageStats } from "./types.js";
import { createEmptyUsage } from "./types.js";

const MAX_PARALLEL_TASKS = 8;
const MAX_CONCURRENCY = 4;
const DEFAULT_COLLAPSED_ITEM_COUNT = 8;

type RunUpdateHandler = (run: RunRecord) => void;

export interface SingleTaskInput {
	agent: string;
	task: string;
	cwd?: string;
	role?: string;
}

export interface ChainTaskInput extends SingleTaskInput {}

export interface RunnerContext {
	cwd: string;
	agents: AgentConfig[];
	agentScope: RunRecord["agentScope"];
	projectAgentsDir: string | null;
	defaultModel?: string;
	defaultTools?: string[];
	parentThinkingLevel: ThinkingLevel;
	signal?: AbortSignal;
	onRunUpdate?: RunUpdateHandler;
}

function randomId(prefix: string): string {
	return `${prefix}-${Math.random().toString(36).slice(2, 10)}`;
}

function cloneUsage(usage: UsageStats): UsageStats {
	return { ...usage };
}

function cloneDisplayItem(item: DisplayItem): DisplayItem {
	return item.type === "text" ? { ...item } : { ...item, args: { ...item.args } };
}

function cloneChild(child: ChildRunRecord): ChildRunRecord {
	return {
		...child,
		usage: cloneUsage(child.usage),
		displayItems: child.displayItems.map(cloneDisplayItem),
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

function isActiveStatus(status: RunStatus): boolean {
	return status === "queued" || status === "starting" || status === "running";
}

function setRunStatusFromChildren(run: RunRecord): void {
	const statuses = run.children.map((child) => child.status);
	if (statuses.length === 0) {
		run.status = "queued";
		return;
	}
	if (statuses.some((status) => status === "running")) {
		run.status = "running";
		return;
	}
	if (statuses.some((status) => status === "starting")) {
		run.status = "starting";
		return;
	}
	if (statuses.some((status) => status === "queued")) {
		run.status = "queued";
		return;
	}
	if (statuses.some((status) => status === "failed")) {
		run.status = "failed";
		return;
	}
	if (statuses.some((status) => status === "aborted")) {
		run.status = "aborted";
		return;
	}
	if (statuses.some((status) => status === "cancelled")) {
		run.status = "cancelled";
		return;
	}
	if (statuses.some((status) => status === "interrupted")) {
		run.status = "interrupted";
		return;
	}
	run.status = "succeeded";
}

function summarizeRun(run: RunRecord): string {
	if (run.mode === "single") {
		const child = run.children[0];
		return child?.output || child?.error || "No subagent output.";
	}
	if (run.mode === "chain") {
		const child = run.children[run.children.length - 1];
		return child?.output || child?.error || "No subagent output.";
	}
	return run.children
		.map(
			(child) =>
				`[${child.agent ?? child.id}] ${child.status === "succeeded" ? "completed" : child.status}: ${child.output || child.error || "No subagent output."}`,
		)
		.join("\n\n");
}

export function buildFinalOutput(run: RunRecord): string {
	return run.summary || summarizeRun(run);
}

export { formatTokens, formatUsage };

export function buildCompactDisplayItems(items: DisplayItem[], limit = DEFAULT_COLLAPSED_ITEM_COUNT): DisplayItem[] {
	if (limit <= 0 || items.length <= limit) return items.map(cloneDisplayItem);
	return items.slice(-limit).map(cloneDisplayItem);
}

export function formatToolCall(toolName: string, args: Record<string, unknown>): string {
	const shortenPath = (value: unknown): string => {
		const p = typeof value === "string" ? value : "...";
		const home = os.homedir();
		return p.startsWith(home) ? `~${p.slice(home.length)}` : p;
	};

	switch (toolName) {
		case "bash": {
			const command = typeof args.command === "string" ? args.command : "...";
			return `$ ${command.length > 80 ? `${command.slice(0, 80)}...` : command}`;
		}
		case "read": {
			const filePath = shortenPath(args.path ?? args.file_path);
			const offset = typeof args.offset === "number" ? args.offset : 1;
			const limit = typeof args.limit === "number" ? args.limit : undefined;
			const suffix = limit ? `:${offset}-${offset + limit - 1}` : "";
			return `read ${filePath}${suffix}`;
		}
		case "write":
			return `write ${shortenPath(args.path ?? args.file_path)}`;
		case "edit":
			return `edit ${shortenPath(args.path ?? args.file_path)}`;
		case "ls":
			return `ls ${shortenPath(args.path)}`;
		case "find":
			return `find ${String(args.pattern ?? "*")} in ${shortenPath(args.path)}`;
		case "grep":
			return `grep /${String(args.pattern ?? "")}/ in ${shortenPath(args.path)}`;
		default: {
			const preview = JSON.stringify(args);
			return `${toolName} ${preview.length > 72 ? `${preview.slice(0, 72)}...` : preview}`;
		}
	}
}

export function getPiInvocation(args: string[]): { command: string; args: string[] } {
	const currentScript = process.argv[1];
	if (currentScript && fsSync.existsSync(currentScript)) {
		return { command: process.execPath, args: [currentScript, ...args] };
	}
	const execName = path.basename(process.execPath).toLowerCase();
	const isGenericRuntime = /^(node|bun)(\.exe)?$/.test(execName);
	if (!isGenericRuntime) return { command: process.execPath, args };
	return { command: "pi", args };
}

async function writePromptToTempFile(agentName: string, prompt: string): Promise<{ dir: string; filePath: string }> {
	const dir = await fs.mkdtemp(path.join(os.tmpdir(), "pi-subagent-core-"));
	const safeName = agentName.replace(/[^\w.-]+/g, "_");
	const filePath = path.join(dir, `${safeName}.md`);
	await fs.writeFile(filePath, prompt, { encoding: "utf8", mode: 0o600 });
	return { dir, filePath };
}

function pushDisplayItem(items: DisplayItem[], item: DisplayItem): void {
	const last = items[items.length - 1];
	if (!last) {
		items.push(item);
		return;
	}
	if (item.type === "text" && last.type === "text" && last.text === item.text) return;
	if (
		item.type === "toolCall" &&
		last.type === "toolCall" &&
		last.toolName === item.toolName &&
		JSON.stringify(last.args) === JSON.stringify(item.args)
	) {
		return;
	}
	items.push(item);
}

function extractAssistantText(message: Message): string {
	if (message.role !== "assistant" || !Array.isArray(message.content)) return "";
	return message.content
		.filter((part): part is { type: "text"; text: string } => part.type === "text" && typeof part.text === "string")
		.map((part) => part.text)
		.join("")
		.trim();
}

function applyAssistantUsage(child: ChildRunRecord, message: Message): void {
	if (message.role !== "assistant") return;
	child.usage.turns += 1;
	const usage = (message as Message & {
		usage?: {
			input?: number;
			output?: number;
			cacheRead?: number;
			cacheWrite?: number;
			totalTokens?: number;
			cost?: { total?: number };
		};
		model?: string;
		stopReason?: string;
		errorMessage?: string;
	}).usage;
	if (usage) {
		child.usage.input += usage.input ?? 0;
		child.usage.output += usage.output ?? 0;
		child.usage.cacheRead += usage.cacheRead ?? 0;
		child.usage.cacheWrite += usage.cacheWrite ?? 0;
		child.usage.cost += usage.cost?.total ?? 0;
		child.usage.contextTokens = usage.totalTokens ?? child.usage.contextTokens;
	}
	const meta = message as Message & { model?: string; stopReason?: string; errorMessage?: string };
	if (meta.model && !child.model) child.model = meta.model;
	if (meta.stopReason) child.stopReason = meta.stopReason;
	if (meta.errorMessage) child.error = meta.errorMessage;
}

function finalizeChildStatus(child: ChildRunRecord, exitCode: number | undefined, aborted: boolean): RunStatus {
	if (aborted) return "aborted";
	if (child.stopReason === "aborted") return "aborted";
	if (child.stopReason === "error") return "failed";
	if ((exitCode ?? 0) !== 0) return "failed";
	return "succeeded";
}

function createChildRecord(
	runId: string,
	agentName: string,
	task: string,
	cwd: string,
	thinkingLevel: ThinkingLevel,
	step?: number,
): ChildRunRecord {
	const now = Date.now();
	return {
		id: randomId("child"),
		runId,
		status: "queued",
		agent: agentName,
		agentSource: "unknown",
		label: agentName,
		title: agentName,
		task,
		cwd,
		thinkingLevel,
		createdAt: now,
		startedAt: now,
		step,
		error: "",
		usage: createEmptyUsage(),
		displayItems: [],
		output: "",
		metadata: {},
	};
}

function emitRunUpdate(run: RunRecord, onRunUpdate?: RunUpdateHandler): void {
	run.childIds = run.children.map((child) => child.id);
	run.activeChildId = [...run.children].reverse().find((child) => isActiveStatus(child.status))?.id;
	run.summary = summarizeRun(run);
	setRunStatusFromChildren(run);
	if (!isActiveStatus(run.status)) run.endedAt = Date.now();
	onRunUpdate?.(cloneRun(run));
}

function buildAdHocSystemPrompt(role: string): string {
	return [
		`You are a ${role}.`,
		"",
		"Bring your full expertise to this task. Think critically, consider edge cases and trade-offs, and deliver high-quality, precise output.",
		"Do not hedge unnecessarily \u2014 commit to well-reasoned decisions and explain your rationale when relevant.",
	].join("\n");
}

function buildAdHocAgent(role: string): AgentConfig {
	return {
		name: `adhoc-${role.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "")}`,
		description: role,
		systemPrompt: buildAdHocSystemPrompt(role),
		source: "user",
		filePath: "",
	};
}

async function runSingleChild(
	run: RunRecord,
	agents: AgentConfig[],
	input: SingleTaskInput,
	ctx: RunnerContext,
	step?: number,
): Promise<ChildRunRecord> {
	const cwd = path.resolve(input.cwd ?? run.cwd);
	const thinkingLevel = chooseDelegatedThinkingLevel(input.task, ctx.parentThinkingLevel, {
		role: input.role,
		agentName: input.agent,
	});
	const child = createChildRecord(run.id, input.agent || input.role || "ad-hoc", input.task, cwd, thinkingLevel, step);
	run.children.push(child);
	emitRunUpdate(run, ctx.onRunUpdate);

	let agent = agents.find((candidate) => candidate.name === input.agent);
	let isAdHoc = false;
	if (!agent) {
		if (input.role?.trim()) {
			agent = buildAdHocAgent(input.role.trim());
			isAdHoc = true;
			child.label = input.role.trim();
			child.title = input.role.trim();
		} else {
			child.status = "failed";
			child.endedAt = Date.now();
			child.exitCode = 1;
			child.error = `Unknown agent: ${input.agent}`;
			child.summary = child.error;
			emitRunUpdate(run, ctx.onRunUpdate);
			return cloneChild(child);
		}
	}

	if (!isAdHoc) child.agentSource = agent.source;
	child.status = "starting";
	child.model = agent.model ?? ctx.defaultModel;
	emitRunUpdate(run, ctx.onRunUpdate);

	const args: string[] = ["--mode", "json", "-p", "--no-session"];
	if (isAdHoc) args.push("--no-extensions");
	if (child.model) args.push("--model", child.model);
	args.push("--thinking", thinkingLevel);
	const tools = agent.tools && agent.tools.length > 0 ? agent.tools : ctx.defaultTools;
	if (tools && tools.length > 0) args.push("--tools", tools.join(","));

	let tmpDir: string | undefined;
	let tmpPromptPath: string | undefined;
	let aborted = false;
	let assistantBuffer = "";

	const flushAssistantBuffer = () => {
		const text = assistantBuffer.trim();
		if (text) pushDisplayItem(child.displayItems, { type: "text", text });
		assistantBuffer = "";
	};

	try {
		if (agent.systemPrompt.trim()) {
			const tmp = await writePromptToTempFile(agent.name, agent.systemPrompt);
			tmpDir = tmp.dir;
			tmpPromptPath = tmp.filePath;
			args.push("--append-system-prompt", tmpPromptPath);
		}
		args.push(input.task);

		child.status = "running";
		emitRunUpdate(run, ctx.onRunUpdate);

		const currentDepth = parseInt(process.env[SUBAGENT_DEPTH_ENV] || "0", 10);

		const exitCode = await new Promise<number>((resolve) => {
			const invocation = getPiInvocation(args);
			const proc = spawn(invocation.command, invocation.args, {
				cwd,
				shell: false,
				stdio: ["ignore", "pipe", "pipe"],
				env: { ...process.env, [SUBAGENT_DEPTH_ENV]: String(currentDepth + 1) },
			});

			let stdoutBuffer = "";
			const processLine = (line: string) => {
				if (!line.trim()) return;
				let event: any;
				try {
					event = JSON.parse(line);
				} catch {
					return;
				}

				if (event.type === "message_update" && event.assistantMessageEvent?.type === "text_delta") {
					assistantBuffer += String(event.assistantMessageEvent.delta ?? "");
					emitRunUpdate(run, ctx.onRunUpdate);
					return;
				}

				if (event.type === "message_end" && event.message?.role === "assistant") {
					const message = event.message as Message;
					applyAssistantUsage(child, message);
					const text = extractAssistantText(message);
					if (text) assistantBuffer = text;
					flushAssistantBuffer();
					if (text) child.output = text;
					child.summary = child.output || child.error;
					emitRunUpdate(run, ctx.onRunUpdate);
					return;
				}

				if (event.type === "tool_execution_start") {
					pushDisplayItem(child.displayItems, {
						type: "toolCall",
						toolName: String(event.toolName ?? "tool"),
						args: typeof event.args === "object" && event.args ? event.args : {},
					});
					emitRunUpdate(run, ctx.onRunUpdate);
				}
			};

			proc.stdout.on("data", (chunk) => {
				stdoutBuffer += chunk.toString();
				const lines = stdoutBuffer.split("\n");
				stdoutBuffer = lines.pop() ?? "";
				for (const line of lines) processLine(line);
			});

			proc.stderr.on("data", (chunk) => {
				child.error = `${child.error ?? ""}${chunk.toString()}`;
				child.summary = child.output || child.error;
				emitRunUpdate(run, ctx.onRunUpdate);
			});

			const abortHandler = () => {
				aborted = true;
				proc.kill("SIGTERM");
				const killTimer = setTimeout(() => {
					if (!proc.killed) proc.kill("SIGKILL");
				}, 1000);
				(killTimer as { unref?: () => void }).unref?.();
			};

			proc.on("close", (code) => {
				if (stdoutBuffer.trim()) processLine(stdoutBuffer);
				ctx.signal?.removeEventListener("abort", abortHandler);
				resolve(code ?? 0);
			});

			proc.on("error", (error) => {
				child.error = `${child.error ?? ""}${error.message}\n`;
				ctx.signal?.removeEventListener("abort", abortHandler);
				resolve(1);
			});

			if (ctx.signal?.aborted) abortHandler();
			else ctx.signal?.addEventListener("abort", abortHandler, { once: true });
		});

		flushAssistantBuffer();
		child.exitCode = exitCode;
		child.status = finalizeChildStatus(child, exitCode, aborted);
		if (!child.output && child.error) child.output = child.error;
		child.summary = child.output || child.error;
		child.endedAt = Date.now();
		emitRunUpdate(run, ctx.onRunUpdate);
		return cloneChild(child);
	} finally {
		if (tmpPromptPath) await fs.rm(tmpPromptPath, { force: true }).catch(() => undefined);
		if (tmpDir) await fs.rm(tmpDir, { recursive: true, force: true }).catch(() => undefined);
	}
}

function createRun(mode: RunMode, ctx: RunnerContext): RunRecord {
	const now = Date.now();
	return {
		id: randomId("run"),
		status: "queued",
		label: mode,
		title: `${mode} run`,
		summary: "",
		prompt: undefined,
		createdAt: now,
		startedAt: now,
		mode,
		agentScope: ctx.agentScope,
		cwd: ctx.cwd,
		projectAgentsDir: ctx.projectAgentsDir,
		children: [],
		metadata: {},
	};
}

export async function mapWithConcurrencyLimit<T, R>(items: readonly T[], concurrency: number, fn: (item: T, index: number) => Promise<R>): Promise<R[]> {
	const results: R[] = new Array(items.length);
	let nextIndex = 0;
	const limit = Math.max(1, Math.min(concurrency, items.length));
	await Promise.all(
		new Array(limit).fill(null).map(async () => {
			while (true) {
				const current = nextIndex++;
				if (current >= items.length) return;
				results[current] = await fn(items[current], current);
			}
		}),
	);
	return results;
}

export async function runSingleTask(input: SingleTaskInput, ctx: RunnerContext): Promise<RunRecord> {
	const run = createRun("single", ctx);
	run.prompt = input.task;
	run.label = input.agent;
	run.title = input.agent;
	await runSingleChild(run, ctx.agents, input, ctx);
	emitRunUpdate(run, ctx.onRunUpdate);
	return cloneRun(run);
}

export async function runChainTasks(inputs: readonly ChainTaskInput[], ctx: RunnerContext): Promise<RunRecord> {
	const run = createRun("chain", ctx);
	run.prompt = inputs.map((input) => input.task).join("\n\n---\n\n");
	run.label = "chain";
	let previous = "";
	for (let index = 0; index < inputs.length; index += 1) {
		const input = inputs[index];
		const task = input.task.replace(/\{previous\}/g, previous);
		const child = await runSingleChild(run, ctx.agents, { ...input, task }, ctx, index + 1);
		previous = child.output || "";
		if (child.status !== "succeeded") break;
	}
	emitRunUpdate(run, ctx.onRunUpdate);
	return cloneRun(run);
}

export async function runParallelTasks(inputs: readonly SingleTaskInput[], ctx: RunnerContext): Promise<RunRecord> {
	if (inputs.length > MAX_PARALLEL_TASKS) {
		throw new Error(`Too many parallel tasks (${inputs.length}). Max is ${MAX_PARALLEL_TASKS}.`);
	}
	const run = createRun("parallel", ctx);
	run.prompt = inputs.map((input) => `[${input.agent}] ${input.task}`).join("\n");
	run.label = `parallel (${inputs.length})`;
	await mapWithConcurrencyLimit(inputs, MAX_CONCURRENCY, async (input) => {
		await runSingleChild(run, ctx.agents, input, ctx);
	});
	emitRunUpdate(run, ctx.onRunUpdate);
	return cloneRun(run);
}
