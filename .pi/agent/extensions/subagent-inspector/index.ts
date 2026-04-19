import type { ExtensionAPI, ExtensionContext } from "@mariozechner/pi-coding-agent";
import { Text } from "@mariozechner/pi-tui";
import { SUBAGENT_RESULT_MESSAGE_TYPE, SUBAGENT_RUN_UPDATE_EVENT } from "../subagent-core/constants.js";
import { formatUsage } from "../subagent-core/formatting.js";
import type { SubagentResultDetails, SubagentRunUpdateEvent } from "../subagent-core/types.js";
import { createPersistentDockController } from "./dock.js";
import { SUBAGENT_INSPECTOR_SHORTCUT } from "./keybindings.js";
import { openInspector } from "./inspector.js";
import { createSubagentInspectorStore } from "./state.js";

const DOCK_WIDGET_ID = "subagent-inspector-dock-anchor";

export default function subagentInspectorExtension(pi: ExtensionAPI) {
	const runtimeStore = createSubagentInspectorStore(pi);
	let overlayMode: "hidden" | "overlay" | "modal" = "hidden";

	const applyDockWidget = (ctx: ExtensionContext) => {
		if (!ctx.hasUI) return;
		if (overlayMode !== "overlay") {
			ctx.ui.setWidget(DOCK_WIDGET_ID, undefined);
			return;
		}
		ctx.ui.setWidget(DOCK_WIDGET_ID, (tui, theme) =>
			createPersistentDockController(tui, theme, runtimeStore, {
				label: "subagents",
				maxItems: 2,
				showRunLabel: true,
				showIdleWhenEmpty: true,
				overlayOptions: {
					anchor: "top-right",
					width: 34,
					minWidth: 24,
					margin: { top: 1, right: 1 },
				},
			}),
		);
	};

	const restoreState = (ctx: ExtensionContext) => {
		runtimeStore.restore(ctx);
		applyDockWidget(ctx);
	};

	pi.events.on(SUBAGENT_RUN_UPDATE_EVENT, (payload) => {
		const event = payload as SubagentRunUpdateEvent;
		if (!event?.run) return;
		runtimeStore.upsertRun(event.run);
	});

	pi.registerMessageRenderer<SubagentResultDetails>(
		SUBAGENT_RESULT_MESSAGE_TYPE,
		(message, _options, theme) => {
			const d = (message.details ?? {}) as Partial<SubagentResultDetails>;
			const ok = d.status === "succeeded";
			const icon = ok ? theme.fg("success", "✓") : theme.fg("error", "✗");
			const label = d.agent ?? d.mode ?? "subagent";
			const idTag = d.jobId ? theme.fg("muted", ` [${d.jobId}]`) : "";
			const usage = d.usage ? formatUsage(d.usage, d.model) : "";
			const tail = d.error
				? `  ${theme.fg("error", `(${d.error.split("\n")[0].slice(0, 60)})`)}`
				: `  ${theme.fg("muted", "(open inspector for full transcript)")}`;
			return new Text(
				`${icon} ${theme.fg("toolTitle", "subagent ")}${theme.fg("accent", label)}${idTag}` +
					(usage ? `  ${theme.fg("dim", usage)}` : "") +
					tail,
				0,
				0,
			);
		},
	);

	const openInspectorCommand = async (ctx: ExtensionContext) => {
		if (!ctx.hasUI) {
			console.log("Subagent inspector overlay requires interactive UI mode.");
			return;
		}
		await openInspector(ctx.ui, runtimeStore, { title: "Subagent inspector" });
	};

	pi.registerCommand("agents", {
		description: "Open the subagent inspector overlay",
		handler: async (_args, ctx) => {
			await openInspectorCommand(ctx);
		},
	});

	pi.registerCommand("subagents", {
		description: "Open the subagent inspector overlay",
		handler: async (_args, ctx) => {
			await openInspectorCommand(ctx);
		},
	});

	pi.registerShortcut(SUBAGENT_INSPECTOR_SHORTCUT.toggleOverlay.key, {
		description: SUBAGENT_INSPECTOR_SHORTCUT.toggleOverlay.description,
		handler: async (ctx) => {
			if (!ctx.hasUI) return;
			if (overlayMode === "hidden") {
				overlayMode = "overlay";
				applyDockWidget(ctx);
				return;
			}
			if (overlayMode === "overlay") {
				overlayMode = "modal";
				applyDockWidget(ctx);
				try {
					await openInspector(ctx.ui, runtimeStore, { title: "Subagent inspector" });
				} finally {
					overlayMode = "hidden";
					applyDockWidget(ctx);
				}
				return;
			}
			overlayMode = "hidden";
			applyDockWidget(ctx);
		},
	});

	pi.on("session_start", async (_event, ctx) => {
		restoreState(ctx);
	});

	pi.on("session_tree", async (_event, ctx) => {
		restoreState(ctx);
	});

	pi.on("session_shutdown", async (_event, ctx) => {
		runtimeStore.flush();
		if (ctx.hasUI) ctx.ui.setWidget(DOCK_WIDGET_ID, undefined);
	});
}
