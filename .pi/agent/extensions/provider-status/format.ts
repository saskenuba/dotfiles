import { stripVTControlCharacters } from "node:util";
import type { QuotaSnapshot } from "./registry.ts";

/** Strip terminal controls even for labels supplied by future adapters. */
export function displayText(text: string): string {
  return stripVTControlCharacters(text).replace(/[\u0000-\u001f\u007f-\u009f\u2028\u2029\u202a-\u202e\u2066-\u2069]/g, "").slice(0, 300);
}

export function formatQuota(provider: string, snapshot: QuotaSnapshot): string {
  const lines = [`Quota · ${displayText(provider)}`];
  if (snapshot.plan) lines.push(`Plan: ${displayText(snapshot.plan)}`);
  if (!snapshot.windows.length) lines.push("Quota windows not reported.");
  for (const window of snapshot.windows) {
    const remaining = typeof window.remainingPercent === "number" && Number.isFinite(window.remainingPercent)
      ? `${Math.round(Math.max(0, Math.min(100, window.remainingPercent)) * 10) / 10}% remaining` : "remaining unknown";
    const reset = window.resetsAt === undefined ? undefined : new Date(window.resetsAt);
    lines.push(`${displayText(window.label)}: ${remaining}${reset && Number.isFinite(reset.getTime()) ? ` · resets ${reset.toISOString()}` : ""}`);
  }
  const credits = snapshot.credits;
  if (credits?.unlimited) lines.push("Credits: unlimited");
  else if (credits?.balance !== undefined) lines.push(`Credits: ${displayText(credits.balance)}`);
  else if (credits?.hasCredits !== undefined) lines.push(`Credits: ${credits.hasCredits ? "available" : "none"}`);
  return lines.join("\n");
}
