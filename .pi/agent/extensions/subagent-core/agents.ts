import * as fs from "node:fs";
import * as path from "node:path";
import { getAgentDir, parseFrontmatter } from "@mariozechner/pi-coding-agent";
import type { AgentConfig, AgentDiscoveryResult, AgentScope, AgentSource } from "./types.js";

function isDirectory(dir: string): boolean {
	try {
		return fs.statSync(dir).isDirectory();
	} catch {
		return false;
	}
}

function parseTools(raw: unknown): string[] | undefined {
	if (typeof raw !== "string") return undefined;
	const tools = raw
		.split(",")
		.map((tool) => tool.trim())
		.filter(Boolean);
	return tools.length > 0 ? tools : undefined;
}

function loadAgentsFromDir(dir: string, source: AgentSource): AgentConfig[] {
	if (source === "unknown") return [];
	if (!isDirectory(dir)) return [];

	let entries: fs.Dirent[] = [];
	try {
		entries = fs.readdirSync(dir, { withFileTypes: true });
	} catch {
		return [];
	}

	const agents: AgentConfig[] = [];
	for (const entry of entries) {
		if (!entry.name.endsWith(".md")) continue;
		if (!entry.isFile() && !entry.isSymbolicLink()) continue;

		const filePath = path.join(dir, entry.name);
		let raw = "";
		try {
			raw = fs.readFileSync(filePath, "utf8");
		} catch {
			continue;
		}

		const { frontmatter, body } = parseFrontmatter<Record<string, unknown>>(raw);
		const name = typeof frontmatter.name === "string" ? frontmatter.name.trim() : "";
		const description = typeof frontmatter.description === "string" ? frontmatter.description.trim() : "";
		if (!name || !description) continue;

		agents.push({
			name,
			description,
			tools: parseTools(frontmatter.tools),
			model: typeof frontmatter.model === "string" ? frontmatter.model.trim() : undefined,
			systemPrompt: body.trim(),
			source,
			filePath,
		});
	}

	return agents;
}

function findNearestProjectAgentsDir(cwd: string): string | null {
	let current = path.resolve(cwd);
	while (true) {
		const candidate = path.join(current, ".pi", "agents");
		if (isDirectory(candidate)) return candidate;
		const parent = path.dirname(current);
		if (parent === current) return null;
		current = parent;
	}
}

function mergeAgents(layers: AgentConfig[][]): AgentConfig[] {
	const byName = new Map<string, AgentConfig>();
	for (const layer of layers) {
		for (const agent of layer) byName.set(agent.name, agent);
	}
	return Array.from(byName.values()).sort((a, b) => a.name.localeCompare(b.name));
}

export function discoverAgents(
	cwd: string,
	scope: AgentScope,
	bundledAgentsDir: string,
): AgentDiscoveryResult {
	const userAgentsDir = path.join(getAgentDir(), "agents");
	const projectAgentsDir = findNearestProjectAgentsDir(cwd);

	const bundledAgents = loadAgentsFromDir(bundledAgentsDir, "bundled");
	const userAgents = scope === "project" ? [] : loadAgentsFromDir(userAgentsDir, "user");
	const projectAgents = scope === "user" || !projectAgentsDir ? [] : loadAgentsFromDir(projectAgentsDir, "project");

	const agents =
		scope === "user"
			? mergeAgents([bundledAgents, userAgents])
			: scope === "project"
				? mergeAgents([bundledAgents, projectAgents])
				: mergeAgents([bundledAgents, userAgents, projectAgents]);

	return {
		agents,
		projectAgentsDir,
		bundledAgentsDir,
		userAgentsDir,
	};
}

export function formatAgentList(agents: AgentConfig[], maxItems = 8): { text: string; remaining: number } {
	if (agents.length === 0) return { text: "none", remaining: 0 };
	const listed = agents.slice(0, maxItems);
	const remaining = Math.max(0, agents.length - listed.length);
	return {
		text: listed.map((agent) => `${agent.name} (${agent.source}): ${agent.description}`).join("; "),
		remaining,
	};
}
