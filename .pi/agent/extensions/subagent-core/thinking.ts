export const THINKING_LEVELS = ["off", "minimal", "low", "medium", "high", "xhigh"] as const;

export type ThinkingLevel = (typeof THINKING_LEVELS)[number];

export interface DelegatedThinkingContext {
	role?: string;
	agentName?: string;
}

export function normalizeThinkingLevel(level: string | undefined): ThinkingLevel {
	return THINKING_LEVELS.includes((level ?? "") as ThinkingLevel) ? (level as ThinkingLevel) : "medium";
}

function stripPreviousPayload(text: string): string {
	const marker = "{previous}";
	if (!text.includes(marker)) return text;
	return text.replaceAll(marker, " ");
}

function truncateForHeuristics(text: string, maxLength = 1600): string {
	return text.length > maxLength ? text.slice(0, maxLength) : text;
}

function hasArchitectLevelPersona(text: string): boolean {
	return /\b(architect|principal engineer|staff engineer|distinguished engineer|engineering fellow|chief engineer)\b/.test(text);
}

function hasArchitectLevelPrompt(text: string): boolean {
	if (
		/\b(principal architect|software architect|systems architect|solution architect|enterprise architect|chief architect|principal engineer|staff engineer|distinguished engineer|engineering fellow|chief engineer)\b/.test(
			text,
		)
	) {
		return true;
	}

	return /\b(architect|architectural)\b/.test(text) && /\b(review|design|plan|strategy|trade-?off|decision|recommend|risk)\b/.test(text);
}

export function chooseDelegatedThinkingLevel(
	prompt: string,
	parentThinkingLevel: ThinkingLevel,
	context?: DelegatedThinkingContext,
): ThinkingLevel {
	const text = truncateForHeuristics(stripPreviousPayload(prompt.trim()));
	if (!text) return parentThinkingLevel;
	const lower = text.toLowerCase();
	const personaText = [context?.role, context?.agentName].filter((value): value is string => Boolean(value?.trim())).join(" ").toLowerCase();

	if (/\b(xhigh|ultra ?think|ultra-?think|maximum thinking|deepest reasoning)\b/.test(lower)) return "xhigh";
	if (/\b(no thinking|thinking off)\b/.test(lower)) return "off";
	if (/\b(minimal thinking)\b/.test(lower)) return "minimal";
	if (/\b(low thinking|quick scout|fast scout)\b/.test(lower)) return "low";
	if (/\b(medium thinking)\b/.test(lower)) return "medium";
	if (/\b(high thinking|deep reasoning)\b/.test(lower)) return "high";

	if (hasArchitectLevelPrompt(lower) || hasArchitectLevelPersona(personaText)) return "xhigh";

	// High-complexity patterns first to avoid false matches on ambiguous tasks
	// (e.g. "Find the root cause" should match "root cause" → high, not "find" → low)
	if (
		/\b(why|root cause|debug|diagnose|plan|refactor|design|architecture|review|risk|trade-?off|best approach|recommend|evaluate|security|synthesize|merge findings|strategy)\b/.test(
			lower,
		)
	) {
		return "high";
	}

	const bulletCount = (text.match(/^\s*[-*]\s+/gm) ?? []).length;
	if (text.length > 1200 || bulletCount >= 6) return "high";

	if (
		/\b(research|inspect|investigate|trace|summari[sz]e|explain|understand|walk through|compare|analy[sz]e|audit)\b/.test(
			lower,
		)
	) {
		return "medium";
	}

	if (/\b(find|locate|list|scan|grep|enumerate|inventory|which files|where is|show me all|collect paths)\b/.test(lower)) {
		return "low";
	}

	if (
		text.length < 240 &&
		/\b(greet|greeting|say hi|say hello|good morning|good afternoon|good evening|wish them|reply warmly|reply briefly|rewrite|rephrase|polish|fix grammar|translate)\b/.test(
			lower,
		)
	)
		return "minimal";

	return parentThinkingLevel;
}
