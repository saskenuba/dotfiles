import type { KeybindingsManager } from "@mariozechner/pi-coding-agent";
import { matchesKey } from "@mariozechner/pi-tui";

export interface InspectorActionBinding {
	ids?: string[];
	keys?: string[];
}

export const SUBAGENT_INSPECTOR_SHORTCUT = {
	toggleOverlay: {
		key: "ctrl+a",
		description: "Cycle subagent overlay (hidden → dock → modal → hidden)",
	},
} as const;

export const INSPECTOR_ACTIONS = {
	dismiss: {
		ids: ["tui.select.cancel"],
		keys: ["q", "ctrl+a"],
	},
	select: {
		ids: ["tui.select.confirm"],
		keys: ["return"],
	},
	nextRun: {
		keys: ["right", "l", "]"],
	},
	previousRun: {
		keys: ["left", "h", "["],
	},
	nextChild: {
		keys: ["tab"],
	},
	previousChild: {
		keys: ["shift+tab"],
	},
	scrollDown: {
		keys: ["j", "down"],
	},
	scrollUp: {
		keys: ["k", "up"],
	},
	jumpStart: {
		keys: ["home", "g"],
	},
	jumpEnd: {
		keys: ["end", "G"],
	},
} as const satisfies Record<string, InspectorActionBinding>;

export const INSPECTOR_HELP_TEXT = "j/k scroll • Tab child • h/l run • g/G jump • q close";

export function matchesInspectorBinding(
	keybindings: Pick<KeybindingsManager, "matches">,
	data: string,
	binding: InspectorActionBinding,
): boolean {
	if (binding.ids?.some((id) => keybindings.matches(data, id))) return true;
	return binding.keys?.some((key) => matchesKey(data, key) || data === key) ?? false;
}
