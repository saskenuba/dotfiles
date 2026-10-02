import type { ExtensionAPI, ExtensionCommandContext } from "@earendil-works/pi-coding-agent";
import { displayText, formatQuota } from "./format.ts";
import { createOpenAIQuotaAdapter } from "./openai.ts";
import { collectQuotaAdapters, type QuotaAdapter } from "./registry.ts";

export { registerQuotaAdapter, QuotaAdapterRegistry } from "./registry.ts";
export type { QuotaAdapter, QuotaResult, QuotaSnapshot, QuotaWindow } from "./registry.ts";

const HELP = "Usage: /status [provider] | /status list | /status help\nDefaults to the selected model's provider. Reports account quota, not session token usage.\nChecking quota resolves authentication (which may refresh OAuth) and makes a provider request.";

/** Also supports composition: createStatusExtension([myAdapter])(pi). */
export function createStatusExtension(extraAdapters: readonly QuotaAdapter[] = []) {
  return (pi: ExtensionAPI): void => {
    const builtins = [createOpenAIQuotaAdapter(), ...extraAdapters];
    let activeCheck: object | undefined;
    // Session replacement/reload invalidates contexts belonging to pending checks.
    const invalidate = () => { activeCheck = undefined; };
    pi.on("session_start", invalidate);
    pi.on("session_shutdown", invalidate);
    pi.registerCommand("status", {
      description: "Show provider account quota (/status help)",
      handler: async (args: string, ctx: ExtensionCommandContext) => {
        // No transcript/LLM messages or network activity in UI-less modes.
        if (!ctx.hasUI) return;
        const words = args.trim().split(/\s+/).filter(Boolean);
        if (words.length > 1 || words[0] === "help" || words[0] === "--help") {
          ctx.ui.notify(HELP, "info");
          return;
        }
        let registry;
        try { registry = collectQuotaAdapters(pi, builtins); }
        catch { ctx.ui.notify("Quota adapter registration failed (invalid or duplicate provider).", "error"); return; }
        if (words[0] === "list") {
          ctx.ui.notify(`Quota providers: ${registry.list().join(", ")}`, "info");
          return;
        }
        const provider = words[0] ?? ctx.model?.provider;
        if (!provider) { ctx.ui.notify("No model selected. Use /status <provider> or /status list.", "warning"); return; }
        const adapter = registry.get(provider);
        if (!adapter) { ctx.ui.notify(`No quota adapter for ${displayText(provider)}. Use /status list.`, "warning"); return; }
        if (activeCheck) { ctx.ui.notify("A quota check is already running.", "info"); return; }
        const check = activeCheck = {};
        try {
          ctx.ui.notify(`Checking ${displayText(provider)} quota; resolving authentication (may refresh OAuth) and contacting the provider…`, "info");
          const result = await adapter.fetchQuota(ctx, provider);
          if (activeCheck !== check) return;
          if (result.kind === "available") ctx.ui.notify(formatQuota(provider, result.snapshot), "info");
          else ctx.ui.notify(displayText(result.message), "warning");
        } catch {
          if (activeCheck === check) ctx.ui.notify("Quota check failed. No provider error details were displayed.", "error");
        } finally { if (activeCheck === check) activeCheck = undefined; }
      },
    });
  };
}

export default createStatusExtension();
