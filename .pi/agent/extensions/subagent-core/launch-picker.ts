import type { ExtensionContext } from "@mariozechner/pi-coding-agent";
import { fuzzyFilter, Input, Key, matchesKey, truncateToWidth, visibleWidth } from "@mariozechner/pi-tui";
import type { ThinkingLevel } from "./thinking.js";

const AUTO_SELECT_TIMEOUT_MS = 5_000;
const MAX_VISIBLE_MODELS = 7;

interface ModelOption {
	provider: string;
	id: string;
	name: string;
	reasoning: boolean;
	thinkingLevelMap?: Partial<Record<ThinkingLevel, string | null>>;
}

export interface LaunchSelection {
	model: string | undefined;
	thinkingLevel: ThinkingLevel;
}

function modelKey(model: Pick<ModelOption, "provider" | "id">): string {
	return `${model.provider}/${model.id}`;
}

function supportedThinkingLevels(model: ModelOption | undefined): ThinkingLevel[] {
	if (!model?.reasoning) return ["off"];

	const map = model.thinkingLevelMap;
	const standard: ThinkingLevel[] = ["off", "minimal", "low", "medium", "high"];
	const extended: ThinkingLevel[] = ["xhigh", "max"];
	return [...standard, ...extended].filter((level) => {
		if (map?.[level] === null) return false;
		return !extended.includes(level) || typeof map?.[level] === "string";
	});
}

function clampThinkingLevel(level: ThinkingLevel, model: ModelOption | undefined): ThinkingLevel {
	const supported = supportedThinkingLevels(model);
	if (supported.includes(level)) return level;
	return supported.includes("high") ? "high" : (supported[supported.length - 1] ?? "off");
}

function thinkingColor(level: ThinkingLevel): string {
	return `thinking${level[0].toUpperCase()}${level.slice(1)}`;
}

class LaunchPicker {
	private readonly searchInput = new Input();
	private selectedIndex: number;
	private preferredThinkingLevel: ThinkingLevel;
	private touched = false;
	private finished = false;
	private secondsRemaining = Math.ceil(AUTO_SELECT_TIMEOUT_MS / 1000);
	private timer: ReturnType<typeof setInterval> | undefined;

	get focused(): boolean {
		return this.searchInput.focused;
	}

	set focused(value: boolean) {
		this.searchInput.focused = value;
	}

	constructor(
		private readonly models: ModelOption[],
		private readonly defaultModel: string | undefined,
		defaultThinkingLevel: ThinkingLevel,
		private readonly theme: {
			fg: (color: any, text: string) => string;
			bg: (color: any, text: string) => string;
			bold: (text: string) => string;
		},
		private readonly tui: { requestRender: () => void },
		private readonly done: (result: LaunchSelection | undefined) => void,
	) {
		this.selectedIndex = Math.max(0, models.findIndex((model) => modelKey(model) === defaultModel));
		this.preferredThinkingLevel = defaultThinkingLevel;
		this.timer = setInterval(() => {
			this.secondsRemaining = Math.max(0, this.secondsRemaining - 1);
			if (this.secondsRemaining === 0) this.finish(undefined);
			else this.tui.requestRender();
		}, 1_000);
	}

	private filteredModels(): ModelOption[] {
		const query = this.searchInput.getValue().trim();
		return query ? fuzzyFilter(this.models, query, (model) => `${model.provider} ${model.id} ${model.name}`) : this.models;
	}

	private currentModel(): ModelOption | undefined {
		return this.filteredModels()[this.selectedIndex];
	}

	private effectiveThinkingLevel(): ThinkingLevel {
		return clampThinkingLevel(this.preferredThinkingLevel, this.currentModel());
	}

	private stopCountdown(): void {
		if (this.touched) return;
		this.touched = true;
		if (this.timer) clearInterval(this.timer);
		this.timer = undefined;
	}

	private finish(result: LaunchSelection | undefined): void {
		if (this.finished) return;
		this.finished = true;
		if (this.timer) clearInterval(this.timer);
		this.timer = undefined;
		this.done(result);
	}

	private selectModel(delta: number): void {
		const models = this.filteredModels();
		this.selectedIndex = Math.max(0, Math.min(models.length - 1, this.selectedIndex + delta));
	}

	private changeThinking(delta: number): void {
		const model = this.currentModel();
		if (!model?.reasoning) return;
		const levels = supportedThinkingLevels(model);
		const currentIndex = levels.indexOf(this.effectiveThinkingLevel());
		const nextIndex = Math.max(0, Math.min(levels.length - 1, currentIndex + delta));
		this.preferredThinkingLevel = levels[nextIndex] ?? this.preferredThinkingLevel;
	}

	handleInput(data: string): void {
		if (matchesKey(data, Key.escape)) {
			this.stopCountdown();
			this.finish(undefined);
			return;
		}
		if (matchesKey(data, Key.enter)) {
			const model = this.currentModel();
			if (!model) return;
			this.stopCountdown();
			this.finish({ model: modelKey(model), thinkingLevel: this.effectiveThinkingLevel() });
			return;
		}
		if (matchesKey(data, Key.tab)) {
			this.stopCountdown();
			this.changeThinking(1);
			this.tui.requestRender();
			return;
		}
		if (matchesKey(data, Key.shift("tab"))) {
			this.stopCountdown();
			this.changeThinking(-1);
			this.tui.requestRender();
			return;
		}

		const pageSize = Math.max(1, MAX_VISIBLE_MODELS - 1);
		const modelDelta = matchesKey(data, Key.up)
			? -1
			: matchesKey(data, Key.down)
				? 1
				: matchesKey(data, Key.pageUp)
					? -pageSize
					: matchesKey(data, Key.pageDown)
						? pageSize
						: matchesKey(data, Key.home)
							? -Number.MAX_SAFE_INTEGER
							: matchesKey(data, Key.end)
								? Number.MAX_SAFE_INTEGER
								: 0;
		if (modelDelta !== 0) {
			this.stopCountdown();
			this.selectModel(modelDelta);
			this.tui.requestRender();
			return;
		}

		const previousQuery = this.searchInput.getValue();
		this.searchInput.handleInput(data);
		if (this.searchInput.getValue() !== previousQuery) {
			this.stopCountdown();
			this.selectedIndex = 0;
			this.tui.requestRender();
		}
	}

	render(width: number): string[] {
		const contentWidth = Math.max(1, width - 4);
		const pad = (text: string) => {
			const clipped = truncateToWidth(text, contentWidth);
			return `${clipped}${" ".repeat(Math.max(0, contentWidth - visibleWidth(clipped)))}`;
		};
		const row = (text = "") => `│ ${pad(text)} │`;
		const rule = "─".repeat(contentWidth + 2);
		const status = this.touched ? "Custom" : `Auto in ${this.secondsRemaining}s`;
		const filtered = this.filteredModels();
		const current = this.currentModel();
		const thinking = this.effectiveThinkingLevel();
		const lines = [
			`╭${rule}╮`,
			row(`${this.theme.fg("accent", this.theme.bold("Launch subagent"))}  ${this.theme.fg("muted", status)}`),
			row(this.theme.fg("muted", `Model · ${current?.provider ?? "automatic provider"}`)),
			row(this.searchInput.render(contentWidth)[0] ?? "> "),
		];

		if (filtered.length === 0) {
			lines.push(row(this.theme.fg("warning", `No models match “${this.searchInput.getValue()}”`)));
		} else {
			const start = Math.max(0, Math.min(this.selectedIndex - Math.floor(MAX_VISIBLE_MODELS / 2), filtered.length - MAX_VISIBLE_MODELS));
			const visible = filtered.slice(start, start + MAX_VISIBLE_MODELS);
			for (let index = 0; index < visible.length; index += 1) {
				const model = visible[index]!;
				const selected = start + index === this.selectedIndex;
				const auto = modelKey(model) === this.defaultModel ? "  auto" : "";
				const name = model.name === model.id ? "" : `  ${model.name}`;
				const text = `${selected ? "›" : " "} ${model.id}${name}${auto}`;
				const styled = selected
					? this.theme.bg("selectedBg", this.theme.fg("accent", this.theme.bold(text)))
					: `  ${model.id}${this.theme.fg("muted", `${name}${auto}`)}`;
				lines.push(row(styled));
			}
			if (filtered.length > MAX_VISIBLE_MODELS) lines.push(row(this.theme.fg("dim", `${this.selectedIndex + 1}/${filtered.length}`)));
		}

		const thinkingText = current?.reasoning
			? `Thinking  ${this.theme.fg(thinkingColor(thinking), this.theme.bold(thinking.toUpperCase()))}  ${this.theme.fg("dim", "Shift+Tab less · Tab more")}`
			: `Thinking  ${this.theme.fg("thinkingOff", "OFF")}  ${this.theme.fg("dim", "unavailable for this model")}`;
		lines.push(row(thinkingText));
		lines.push(row(this.theme.fg("dim", "↑↓ model · type to search · Enter launch · Esc automatic")));
		lines.push(`╰${rule}╯`);
		return lines.map((line) => truncateToWidth(line, width));
	}

	invalidate(): void {
		this.searchInput.invalidate();
	}
}

export async function chooseLaunchSelection(
	ctx: ExtensionContext,
	autoModel: string | undefined,
	autoThinkingLevel: ThinkingLevel,
): Promise<LaunchSelection | undefined> {
	if (ctx.mode !== "tui") return undefined;
	const [provider] = autoModel?.split("/", 1) ?? [];
	if (!provider) return undefined;

	const toOption = (model: {
		provider: string;
		id: string;
		name: string;
		reasoning: boolean;
		thinkingLevelMap?: unknown;
	}): ModelOption => ({
		provider: model.provider,
		id: model.id,
		name: model.name,
		reasoning: model.reasoning,
		thinkingLevelMap: model.thinkingLevelMap as ModelOption["thinkingLevelMap"],
	});
	// Pi's registry is catalog-ascending; reverse it so newer catalog entries appear first.
	const models = ctx.modelRegistry
		.getAvailable()
		.filter((model) => model.provider === provider)
		.map(toOption)
		.reverse();
	const [, modelId] = autoModel.split("/", 2);
	const configuredDefault = modelId ? ctx.modelRegistry.find(provider, modelId) : undefined;
	if (configuredDefault && !models.some((model) => modelKey(model) === autoModel)) models.push(toOption(configuredDefault));

	return ctx.ui.custom<LaunchSelection | undefined>(
		(tui, theme, _keybindings, done) => new LaunchPicker(models, autoModel, autoThinkingLevel, theme, tui, done),
		{
			overlay: true,
			overlayOptions: { width: 68, minWidth: 46, maxHeight: "70%", anchor: "center", margin: 1 },
		},
	);
}
