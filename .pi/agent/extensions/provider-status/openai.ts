import { Buffer } from "node:buffer";
import type { QuotaAdapter, QuotaResult, QuotaSnapshot, QuotaWindow } from "./registry.ts";

const USAGE_URL = "https://chatgpt.com/backend-api/wham/usage";
const CODEX_CLIENT_ID = "app_EMoamEEZ73f0CkXaXp7hrann";
const TIMEOUT_MS = 10_000;
type JsonObject = Record<string, unknown>;
const object = (value: unknown): JsonObject | undefined =>
  value !== null && typeof value === "object" && !Array.isArray(value) ? value as JsonObject : undefined;
const finite = (value: unknown): value is number => typeof value === "number" && Number.isFinite(value);
const safeText = (value: unknown): string | undefined =>
  typeof value === "string" && value.length > 0 && value.length <= 80 && /^[a-zA-Z0-9 ._+-]+$/.test(value) ? value : undefined;
const unavailable = (message: string): QuotaResult => ({ kind: "unavailable", message });

/** Decode only in memory. This is not signature verification; trust comes from Pi's OAuth resolver. */
function tokenClaims(token: string): JsonObject | undefined {
  try {
    const parts = token.split(".");
    if (parts.length !== 3 || !parts.every((part) => /^[A-Za-z0-9_-]+$/.test(part))) return;
    return object(JSON.parse(Buffer.from(parts[1], "base64url").toString("utf8")));
  } catch { return; }
}

function windowLabel(seconds: unknown, fallback: string): string {
  if (!finite(seconds) || seconds <= 0) return fallback;
  if (seconds === 18_000) return "5h";
  if (seconds === 604_800) return "Weekly";
  if (seconds % 86_400 === 0) return `${seconds / 86_400}d`;
  if (seconds % 3_600 === 0) return `${seconds / 3_600}h`;
  if (seconds % 60 === 0) return `${seconds / 60}m`;
  return `${seconds}s`;
}

function parseWindow(value: unknown, fallback: string, now: number): QuotaWindow | undefined {
  const window = object(value);
  if (!window) return;
  const remainingPercent = finite(window.used_percent) ? Math.max(0, Math.min(100, 100 - window.used_percent)) : undefined;
  let resetsAt: number | undefined;
  const validReset = (value: number) => Number.isFinite(value) && Math.abs(value) <= 8.64e15;
  if (finite(window.reset_at) && window.reset_at >= 0 && validReset(window.reset_at * 1000)) resetsAt = window.reset_at * 1000;
  if (resetsAt === undefined && finite(window.reset_after_seconds) && window.reset_after_seconds >= 0) {
    const relativeReset = now + window.reset_after_seconds * 1000;
    if (validReset(relativeReset)) resetsAt = relativeReset;
  }
  if (remainingPercent === undefined && resetsAt === undefined) return;
  return { label: windowLabel(window.limit_window_seconds, fallback), remainingPercent, resetsAt };
}

// Schema evidence: openai/codex b707714ae4200db0a0385da24b3981d99139fa62,
// codex-rs/backend-client/src/client.rs (map_rate_limit_window/map_credits)
// and src/client/rate_limit_resets.rs (rate_limit_status_url).
export function parseOpenAIUsage(value: unknown, now = Date.now()): QuotaSnapshot | undefined {
  const payload = object(value);
  if (!payload) return;
  const windows: QuotaWindow[] = [];
  const addLimits = (value: unknown, prefix = "") => {
    const limits = object(value);
    if (!limits) return;
    for (const [key, fallback] of [["primary_window", "Primary window"], ["secondary_window", "Secondary window"]]) {
      const window = parseWindow(limits[key], fallback, now);
      if (window) windows.push({ ...window, label: prefix + window.label });
    }
  };
  addLimits(payload.rate_limit);
  addLimits(payload.code_review_rate_limit, "Code review · ");
  if (Array.isArray(payload.additional_rate_limits)) {
    for (const value of payload.additional_rate_limits.slice(0, 20)) {
      const additional = object(value);
      if (additional) addLimits(additional.rate_limit, `${safeText(additional.limit_name) ?? "Additional limit"} · `);
    }
  }
  const plan = safeText(payload.plan_type);
  const rawCredits = object(payload.credits);
  const balance = finite(rawCredits?.balance) ? String(rawCredits.balance) :
    typeof rawCredits?.balance === "string" && /^-?\d+(\.\d+)?$/.test(rawCredits.balance) && rawCredits.balance.length <= 40 ? rawCredits.balance : undefined;
  const credits = rawCredits ? {
    unlimited: typeof rawCredits.unlimited === "boolean" ? rawCredits.unlimited : undefined,
    hasCredits: typeof rawCredits.has_credits === "boolean" ? rawCredits.has_credits : undefined,
    balance,
  } : undefined;
  const hasCredits = credits && Object.values(credits).some((value) => value !== undefined);
  if (!windows.length && !plan && !hasCredits) return;
  return { windows, plan, credits: hasCredits ? credits : undefined };
}

export function createOpenAIQuotaAdapter(fetcher: typeof fetch = globalThis.fetch): QuotaAdapter {
  return {
    id: "openai-chatgpt", displayName: "OpenAI / ChatGPT", providers: ["openai-codex", "openai"],
    async fetchQuota(ctx, provider) {
      // Public resolver refreshes OAuth as necessary; never inspect credential storage.
      let resolved;
      try { resolved = await ctx.modelRegistry.getProviderAuth(provider); }
      catch { return unavailable("Authentication could not be resolved or refreshed. Reconnect with /login."); }
      if (resolved?.source !== "OAuth" || !resolved.auth.apiKey) {
        return unavailable("ChatGPT quota requires OAuth; API-key usage is not available through this endpoint.");
      }
      const token = resolved.auth.apiKey;
      const claims = tokenClaims(token);
      const accountId = object(claims?.["https://api.openai.com/auth"])?.chatgpt_account_id;
      if (typeof accountId !== "string" || !/^[A-Za-z0-9_-]{1,200}$/.test(accountId)) {
        return unavailable("Quota unavailable: the resolved OAuth token has no usable ChatGPT account ID.");
      }
      // Current openai OAuth uses a dynamically registered client and an API resource token.
      // Account ID alone does not establish wham compatibility. Only accept the known Codex flow.
      const scopes = Array.isArray(claims?.scp) ? claims.scp : typeof claims?.scope === "string" ? claims.scope.split(/\s+/) : [];
      if (scopes.includes("chatgpt.tokens.use.direct") || (provider === "openai" &&
          (claims?.client_id !== CODEX_CLIENT_ID || claims?.iss !== "https://auth.openai.com"))) {
        return unavailable("Quota unavailable for the current OpenAI ChatGPT API-scoped OAuth flow; wham compatibility is not verified. Use /status openai-codex if configured.");
      }
      if (finite(claims?.exp) && claims.exp * 1000 <= Date.now()) return unavailable("The resolved OAuth token is expired. Reconnect with /login.");
      const signal = AbortSignal.timeout(TIMEOUT_MS);
      try {
        const response = await fetcher(USAGE_URL, {
          method: "GET", redirect: "error", signal,
          headers: { Authorization: `Bearer ${token}`, "ChatGPT-Account-Id": accountId, Accept: "application/json" },
        });
        if (!response.ok) {
          // Never read or display provider error bodies, statusText, or thrown errors.
          await response.body?.cancel().catch(() => undefined);
          if (response.status === 401 || response.status === 403) return unavailable("ChatGPT quota access was denied. Reconnect with /login; this token may not support quota access.");
          if (response.status === 429) return unavailable("ChatGPT quota endpoint is rate-limited. Try again later.");
          return unavailable("ChatGPT quota endpoint is unavailable. Try again later.");
        }
        const snapshot = parseOpenAIUsage(await response.json());
        return snapshot ? { kind: "available", snapshot } : unavailable("ChatGPT returned no recognizable quota data.");
      } catch {
        return unavailable(signal.aborted ? "ChatGPT quota request timed out." : "ChatGPT quota request failed or returned invalid data.");
      }
    },
  };
}
