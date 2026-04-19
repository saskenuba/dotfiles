import { access } from "node:fs/promises";
import { dirname, join, resolve } from "node:path";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

async function exists(path: string) {
	try {
		await access(path);
		return true;
	} catch {
		return false;
	}
}

async function findClaudeSkillDirs(startDir: string) {
	const dirs: string[] = [];
	let current = resolve(startDir);

	while (true) {
		const candidate = join(current, ".claude", "skills");
		if (await exists(candidate)) dirs.push(candidate);

		const parent = dirname(current);
		if (parent === current) break;
		current = parent;
	}

	return [...new Set(dirs)];
}

export default function (pi: ExtensionAPI) {
	pi.on("resources_discover", async (event) => {
		const skillPaths = await findClaudeSkillDirs(event.cwd);
		return skillPaths.length > 0 ? { skillPaths } : undefined;
	});
}
