import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

export interface QuotaWindow {
  label: string;
  remainingPercent?: number;
  resetsAt?: number; // Unix milliseconds
}

export interface QuotaSnapshot {
  windows: readonly QuotaWindow[];
  plan?: string;
  credits?: { unlimited?: boolean; hasCredits?: boolean; balance?: string };
}

export type QuotaResult =
  | { kind: "available"; snapshot: QuotaSnapshot }
  | { kind: "unavailable"; message: string };

/** Messages and labels must be safe for display: never include credentials or raw errors. */
export interface QuotaAdapter {
  readonly id: string;
  readonly displayName: string;
  readonly providers: readonly string[];
  fetchQuota(ctx: ExtensionContext, provider: string): Promise<QuotaResult>;
}

export class QuotaAdapterRegistry {
  private readonly byProvider = new Map<string, QuotaAdapter>();
  private readonly ids = new Set<string>();

  register(adapter: QuotaAdapter): void {
    const validId = (value: unknown): value is string => typeof value === "string" && /^[a-z0-9][a-z0-9_-]*$/.test(value);
    if (!adapter || !validId(adapter.id) || typeof adapter.displayName !== "string" || !adapter.displayName.trim() ||
        typeof adapter.fetchQuota !== "function" || !Array.isArray(adapter.providers) || adapter.providers.length === 0 || new Set(adapter.providers).size !== adapter.providers.length ||
        Array.from(adapter.providers).some((id) => !validId(id) || ["help", "list"].includes(id))) {
      throw new Error("Invalid quota adapter registration");
    }
    if (this.ids.has(adapter.id) || adapter.providers.some((id) => this.byProvider.has(id))) {
      throw new Error("Duplicate quota adapter registration");
    }
    this.ids.add(adapter.id);
    for (const provider of adapter.providers) this.byProvider.set(provider, adapter);
  }

  get(provider: string): QuotaAdapter | undefined { return this.byProvider.get(provider); }
  list(): string[] { return [...this.byProvider.keys()].sort(); }
}

const COLLECT_EVENT = "provider-status:collect-adapters:v1";
interface CollectRequest { register: (adapter: QuotaAdapter) => void }

/**
 * Call from another extension's factory: registerQuotaAdapter(pi, myAdapter).
 * Discovery happens per command, so extension load order and reloads need no shared singleton.
 * Pi tracks event subscriptions; the returned function also permits explicit withdrawal.
 */
export function registerQuotaAdapter(pi: ExtensionAPI, adapter: QuotaAdapter): () => void {
  return pi.events.on(COLLECT_EVENT, (data) => {
    const request = data as Partial<CollectRequest> | null;
    if (typeof request?.register === "function") request.register(adapter);
  });
}

export function collectQuotaAdapters(pi: ExtensionAPI, builtins: readonly QuotaAdapter[]): QuotaAdapterRegistry {
  const registry = new QuotaAdapterRegistry();
  for (const adapter of builtins) registry.register(adapter);
  // Pi's event bus catches listener errors. Capture registration failures here so
  // a bad plugin cannot silently leave a partially populated registry usable.
  let failed = false;
  pi.events.emit(COLLECT_EVENT, { register: (adapter: QuotaAdapter) => {
    try { registry.register(adapter); } catch { failed = true; }
  } } satisfies CollectRequest);
  if (failed) throw new Error("Invalid or duplicate quota adapter registration");
  return registry;
}
