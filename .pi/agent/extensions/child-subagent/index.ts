import * as path from "node:path";
import { fileURLToPath } from "node:url";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { registerSubagentTool } from "../subagent-core/subagent-tool.js";
import { normalizeThinkingLevel } from "../subagent-core/thinking.js";

const EXTENSION_DIR = path.dirname(fileURLToPath(import.meta.url));
const BUNDLED_AGENTS_DIR = path.join(EXTENSION_DIR, "agents");

function getActiveBuiltinTools(pi: ExtensionAPI): string[] {
	const builtinTools = new Set(
		pi
			.getAllTools()
			.filter((tool) => tool.sourceInfo.source === "builtin")
			.map((tool) => tool.name),
	);
	return pi.getActiveTools().filter((name) => builtinTools.has(name));
}

export default function (pi: ExtensionAPI) {
	let currentModel: string | undefined;

	pi.on("session_start", async (_event, ctx) => {
		currentModel = ctx.model ? `${ctx.model.provider}/${ctx.model.id}` : currentModel;
	});

	pi.on("model_select", async (event) => {
		currentModel = `${event.model.provider}/${event.model.id}`;
	});

	registerSubagentTool(pi, {
		bundledAgentsDir: BUNDLED_AGENTS_DIR,
		getDefaultModel: (ctx) => (ctx.model ? `${ctx.model.provider}/${ctx.model.id}` : currentModel),
		getDefaultTools: () => getActiveBuiltinTools(pi),
		getParentThinkingLevel: () => normalizeThinkingLevel(pi.getThinkingLevel()),
	});
}
