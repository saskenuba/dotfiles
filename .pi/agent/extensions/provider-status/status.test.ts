import assert from "node:assert/strict";
import test from "node:test";
import type { ExtensionAPI, ExtensionCommandContext } from "@earendil-works/pi-coding-agent";
import { createStatusExtension } from "./index.ts";
import { createOpenAIQuotaAdapter, parseOpenAIUsage } from "./openai.ts";
import { displayText, formatQuota } from "./format.ts";
import { collectQuotaAdapters, QuotaAdapterRegistry, registerQuotaAdapter, type QuotaAdapter } from "./registry.ts";

const claims = {
  "https://api.openai.com/auth": { chatgpt_account_id: "account-test" },
  client_id: "app_EMoamEEZ73f0CkXaXp7hrann", iss: "https://auth.openai.com",
};
const jwt = (payload: unknown) => `eyJhbGciOiJSUzI1NiJ9.${Buffer.from(JSON.stringify(payload)).toString("base64url")}.fake-signature`;
const auth = (payload: unknown = claims) => ({ source: "OAuth", auth: { apiKey: jwt(payload) } });
function context(resolved: unknown = auth(), provider = "openai-codex", hasUI = true) {
  const notices: { message: string; type?: string }[] = [];
  const calls: string[] = [];
  return {
    notices, calls,
    ctx: {
      hasUI, model: { provider },
      modelRegistry: { getProviderAuth: async (id: string) => { calls.push(id); return resolved; } },
      ui: { notify: (message: string, type?: string) => notices.push({ message, type }) },
    } as unknown as ExtensionCommandContext,
  };
}
function harness() {
  let handler!: (args: string, ctx: ExtensionCommandContext) => Promise<void>;
  const listeners = new Map<string, Set<(data: unknown) => void>>();
  const pi = {
    registerCommand: (name: string, command: { handler: typeof handler }) => { assert.equal(name, "status"); handler = command.handler; },
    registerShortcut: () => { assert.fail("Quota checks must not hijack application keybindings"); },
    on: (event: string, fn: (data: unknown) => void) => {
      const set = listeners.get(event) ?? new Set(); listeners.set(event, set); set.add(fn);
      return () => { set.delete(fn); };
    },
    events: {
      on: (channel: string, fn: (data: unknown) => void) => {
        const set = listeners.get(channel) ?? new Set(); listeners.set(channel, set); set.add(fn);
        return () => { set.delete(fn); };
      },
      emit: (channel: string, data: unknown) => { for (const fn of listeners.get(channel) ?? []) fn(data); },
    },
  } as unknown as ExtensionAPI;
  return { pi, run: (args: string, ctx: ExtensionCommandContext) => handler(args, ctx),
    lifecycle: (event: string) => { for (const fn of listeners.get(event) ?? []) fn({}); } };
}
const custom: QuotaAdapter = {
  id: "mock", displayName: "Mock", providers: ["mock-provider"],
  fetchQuota: async () => ({ kind: "available", snapshot: { windows: [{ label: "Daily", remainingPercent: 55 }] } }),
};

test("schema: duration labels, clamping, reset timestamps, plan and optional credits", () => {
  const snapshot = parseOpenAIUsage({
    plan_type: "plus", rate_limit: {
      primary_window: { used_percent: -5, limit_window_seconds: 604800, reset_at: 1700000000 },
      secondary_window: { used_percent: 150, limit_window_seconds: 18000, reset_after_seconds: 60 },
    }, credits: { unlimited: false, has_credits: true, balance: "12.5" },
  }, 1000)!;
  assert.deepEqual(snapshot.windows, [
    { label: "Weekly", remainingPercent: 100, resetsAt: 1700000000000 },
    { label: "5h", remainingPercent: 0, resetsAt: 61000 },
  ]);
  assert.equal(snapshot.plan, "plus");
  const output = formatQuota("openai-codex", snapshot);
  assert.match(output, /Weekly: 100% remaining/);
  assert.match(output, /resets 2023-11-14T22:13:20.000Z/);
  assert.match(output, /Credits: 12.5/);
});

test("schema: null-safe, partial and generic windows; no fabricated quota", () => {
  for (const value of [null, undefined, [], 1, {}, { rate_limit: null, credits: null }, { credits: {} }]) {
    assert.equal(parseOpenAIUsage(value), undefined);
  }
  const snapshot = parseOpenAIUsage({ rate_limit: {
    primary_window: { used_percent: 20, limit_window_seconds: 3600, reset_at: Infinity },
    secondary_window: { reset_at: 100, used_percent: "bad" },
  } })!;
  assert.equal(snapshot.windows[0].label, "1h");
  assert.equal(snapshot.windows[0].resetsAt, undefined);
  assert.equal(snapshot.windows[1].remainingPercent, undefined);
  assert.match(formatQuota("x", snapshot), /remaining unknown/);
  assert.match(formatQuota("x", parseOpenAIUsage({ plan_type: "free", credits: { unlimited: true } })!), /Quota windows not reported/);
  assert.equal(parseOpenAIUsage({ plan_type: "\u001b[31msecret", credits: { balance: "not-a-number" } }), undefined);
});

test("schema: review/additional limits, no positional 5h/weekly assumptions", () => {
  const snapshot = parseOpenAIUsage({
    code_review_rate_limit: { primary_window: { used_percent: 4, limit_window_seconds: 86400 } },
    additional_rate_limits: [null, { limit_name: "Fast", rate_limit: { primary_window: { used_percent: 5, limit_window_seconds: 90 } } }],
  })!;
  assert.deepEqual(snapshot.windows.map((w) => w.label), ["Code review · 1d", "Fast · 90s"]);
});

test("request uses refreshed public auth, fixed host, safe headers, timeout and redirect:error", async () => {
  const c = context();
  let count = 0;
  const fetcher: typeof fetch = async (input, init) => {
    count++;
    assert.equal(input, "https://chatgpt.com/backend-api/wham/usage");
    assert.equal(init?.method, "GET"); assert.equal(init?.redirect, "error");
    assert.ok(init?.signal instanceof AbortSignal);
    const headers = new Headers(init?.headers);
    assert.equal(headers.get("authorization"), `Bearer ${jwt(claims)}`);
    assert.equal(headers.get("chatgpt-account-id"), "account-test");
    return Response.json({ rate_limit: { primary_window: { used_percent: 25, limit_window_seconds: 18000 } } });
  };
  assert.equal((await createOpenAIQuotaAdapter(fetcher).fetchQuota(c.ctx, "openai-codex")).kind, "available");
  assert.deepEqual(c.calls, ["openai-codex"]); assert.equal(count, 1);
});

test("API keys, missing/bad claims, expired OAuth and current OpenAI flow never fetch", async () => {
  const adapter = createOpenAIQuotaAdapter(async () => { assert.fail("must not fetch"); });
  const cases: [unknown, string][] = [
    [undefined, "openai"], [{ source: "OPENAI_API_KEY", auth: { apiKey: jwt(claims) } }, "openai-codex"],
    [{ source: "OAuth", auth: { apiKey: "invalid" } }, "openai-codex"],
    [auth(null), "openai-codex"], [auth({}), "openai-codex"],
    [auth({ ...claims, "https://api.openai.com/auth": { chatgpt_account_id: "bad\r\nheader" } }), "openai-codex"],
    [auth({ ...claims, exp: 1 }), "openai-codex"],
    [auth({ ...claims, client_id: "dynamic-client" }), "openai"],
    [auth({ ...claims, scope: "chatgpt.tokens.use.direct" }), "openai"],
    [auth({ ...claims, scp: ["chatgpt.tokens.use.direct"] }), "openai-codex"],
  ];
  for (const [resolved, provider] of cases) {
    const c = context(resolved ?? null);
    assert.equal((await adapter.fetchQuota(c.ctx, provider)).kind, "unavailable");
  }
});

test("OpenAI override only accepts known Codex-compatible OAuth claims", async () => {
  const adapter = createOpenAIQuotaAdapter(async () => Response.json({ plan_type: "plus" }));
  assert.equal((await adapter.fetchQuota(context().ctx, "openai")).kind, "available");
});

test("provider HTTP/error/JSON failures are sanitized", async () => {
  for (const fetcher of [
    async () => new Response("secret-token", { status: 401 }),
    async () => new Response("secret-token", { status: 403 }),
    async () => new Response("secret-token", { status: 429 }),
    async () => new Response("secret-token", { status: 500 }),
    async () => new Response("not JSON secret-token"),
    async () => Response.json(null),
    async () => { throw new Error("secret-token"); },
  ] satisfies (typeof fetch)[]) {
    const result = await createOpenAIQuotaAdapter(fetcher).fetchQuota(context().ctx, "openai-codex");
    assert.equal(result.kind, "unavailable"); assert.doesNotMatch(JSON.stringify(result), /secret-token/);
  }
  const c = context();
  c.ctx.modelRegistry.getProviderAuth = async () => { throw new Error("secret-token"); };
  assert.doesNotMatch(JSON.stringify(await createOpenAIQuotaAdapter().fetchQuota(c.ctx, "openai")), /secret-token/);
});

test("timeout error is sanitized (mocked timer, no network)", async () => {
  const original = AbortSignal.timeout;
  AbortSignal.timeout = () => AbortSignal.abort();
  try {
    const adapter = createOpenAIQuotaAdapter(async (_input, init) => { init?.signal?.throwIfAborted(); assert.fail(); });
    assert.deepEqual(await adapter.fetchQuota(context().ctx, "openai-codex"), { kind: "unavailable", message: "ChatGPT quota request timed out." });
  } finally { AbortSignal.timeout = original; }
});

test("registry rejects duplicate/invalid registrations atomically", () => {
  const registry = new QuotaAdapterRegistry(); registry.register(custom);
  assert.throws(() => registry.register(custom), /Duplicate/);
  assert.throws(() => registry.register({ ...custom, id: "other", providers: ["new", "mock-provider"] }), /Duplicate/);
  assert.equal(registry.get("new"), undefined);
  assert.throws(() => registry.register({ ...custom, id: "other", providers: ["help"] }), /Invalid/);
  assert.deepEqual(registry.list(), ["mock-provider"]);
});

test("cross-extension registration is load-order independent and can be withdrawn", () => {
  const h = harness(); const unregister = registerQuotaAdapter(h.pi, custom);
  for (let i = 0; i < 2; i++) assert.equal(collectQuotaAdapters(h.pi, []).get("mock-provider"), custom);
  unregister(); assert.deepEqual(collectQuotaAdapters(h.pi, []).list(), []);
});

test("plugin registration failures are caught inside the event callback, not leaked to Pi's logger", async () => {
  const h = harness();
  registerQuotaAdapter(h.pi, custom);
  registerQuotaAdapter(h.pi, { ...custom, id: "second" });
  assert.throws(() => collectQuotaAdapters(h.pi, []), /Invalid or duplicate/);
  createStatusExtension()(h.pi);
  const c = context(null, "mock-provider"); await h.run("", c.ctx);
  assert.equal(c.notices.length, 1);
  assert.match(c.notices[0].message, /registration failed/);
});

test("formatter strips terminal controls and guards non-finite plugin data", () => {
  const output = formatQuota("mock\u001b-provider", {
    windows: [{ label: "Daily\u0007", remainingPercent: NaN, resetsAt: Infinity }],
    credits: { balance: "1\u009b" },
  });
  assert.doesNotMatch(output, /[\u0000-\u0009\u000b-\u001f\u007f-\u009f]/);
  assert.match(output, /remaining unknown/);
  assert.doesNotMatch(output, /resets/);
});

test("command defaults to selected provider, supports override/list/help, never sends LLM messages", async () => {
  const h = harness(); registerQuotaAdapter(h.pi, custom); createStatusExtension()(h.pi);
  const c = context(null, "mock-provider"); await h.run("", c.ctx);
  assert.match(c.notices.at(-1)!.message, /Daily: 55% remaining/);
  assert.match(c.notices[0].message, /may refresh OAuth/);
  assert.deepEqual(c.calls, []);
  await h.run("mock-provider", context(null, "other").ctx);
  await h.run("list", c.ctx); assert.match(c.notices.at(-1)!.message, /mock-provider, openai, openai-codex/);
  await h.run("help", c.ctx); assert.match(c.notices.at(-1)!.message, /Usage:/);
  await h.run("a b", c.ctx); assert.match(c.notices.at(-1)!.message, /Usage:/);
  await h.run("unknown", c.ctx); assert.match(c.notices.at(-1)!.message, /No quota adapter/);
  const noModel = context(); noModel.ctx.model = undefined; await h.run("", noModel.ctx);
  assert.match(noModel.notices[0].message, /No model selected/);
});

test("hasUI guard prevents all work; adapter errors sanitized and busy reset", async () => {
  let calls = 0;
  const adapter: QuotaAdapter = { ...custom, fetchQuota: async () => { calls++; throw new Error("secret-token"); } };
  const h = harness(); createStatusExtension([adapter])(h.pi);
  const headless = context(null, "mock-provider", false); await h.run("", headless.ctx);
  assert.equal(calls, 0); assert.deepEqual(headless.notices, []);
  const c = context(null, "mock-provider"); await h.run("", c.ctx); await h.run("", c.ctx);
  assert.equal(calls, 2); assert.doesNotMatch(JSON.stringify(c.notices), /secret-token/);
});

test("command coalesces concurrent checks", async () => {
  let complete!: () => void;
  const wait = new Promise<void>((resolve) => { complete = resolve; });
  let calls = 0;
  const h = harness(); createStatusExtension([{ ...custom, fetchQuota: async () => {
    calls++; await wait; return { kind: "unavailable", message: "Unavailable" };
  } }])(h.pi);
  const c = context(null, "mock-provider"); const first = h.run("", c.ctx);
  await h.run("", c.ctx); assert.equal(calls, 1); assert.match(c.notices.at(-1)!.message, /already running/);
  complete(); await first;
});

test("display strips entire CSI/OSC sequences, bidi and Unicode line controls", () => {
  assert.equal(displayText("\u001b[31mRed\u001b[0m"), "Red");
  assert.equal(displayText("\u001b]8;;https://example.invalid\u001b\\Link\u001b]8;;\u001b\\"), "Link");
  assert.equal(displayText("\u009b31mRed\u009b0m"), "Red");
  assert.equal(displayText("a\u202eb\u2066c\u2028d\u2029e"), "abcde");
  assert.equal(displayText("x".repeat(500)).length, 300);
});

test("invalid absolute reset falls back to valid relative reset", () => {
  for (const reset_at of [1e20, Number.MAX_VALUE, -1, NaN]) {
    const snapshot = parseOpenAIUsage({ rate_limit: { primary_window: {
      used_percent: 25, reset_at, reset_after_seconds: 60,
    } } }, 1000)!;
    assert.equal(snapshot.windows[0].resetsAt, 61000);
  }
  const snapshot = parseOpenAIUsage({ rate_limit: { primary_window: {
    used_percent: 25, reset_after_seconds: 60,
  } } }, -1e20)!;
  assert.equal(snapshot.windows[0].resetsAt, undefined);
});

test("registry rejects malformed runtime plugins without partial registration", () => {
  const registry = new QuotaAdapterRegistry();
  for (const patch of [
    { id: undefined }, { id: null }, { id: 123 }, { displayName: 123 }, { displayName: "  " },
    { providers: "mock-provider" }, { providers: [undefined] }, { providers: [123] },
    { providers: Array(1) }, { providers: ["valid", undefined] },
  ]) {
    assert.throws(() => registry.register({ ...custom, ...patch } as unknown as QuotaAdapter), /Invalid/);
    assert.deepEqual(registry.list(), []);
  }
  registry.register(custom);
  assert.deepEqual(registry.list(), ["mock-provider"]);
});

test("session transitions discard stale results/errors without releasing a newer check", async () => {
  for (const event of ["session_start", "session_shutdown"]) {
    for (const fail of [false, true]) {
      const pending: { resolve: (value: { kind: "unavailable"; message: string }) => void; reject: (error: Error) => void }[] = [];
      const h = harness();
      createStatusExtension([{ ...custom, fetchQuota: () => new Promise((resolve, reject) => pending.push({ resolve, reject })) }])(h.pi);
      const old = context(null, "mock-provider");
      const first = h.run("", old.ctx);
      h.lifecycle(event);
      const current = context(null, "mock-provider");
      const second = h.run("", current.ctx);
      assert.equal(pending.length, 2);
      if (fail) pending[0].reject(new Error("secret-token"));
      else pending[0].resolve({ kind: "unavailable", message: "Stale result" });
      await first;
      assert.equal(old.notices.length, 1);
      await h.run("", current.ctx);
      assert.match(current.notices.at(-1)!.message, /already running/);
      pending[1].resolve({ kind: "unavailable", message: "Current result" });
      await second;
      assert.equal(current.notices.at(-1)!.message, "Current result");
    }
  }
});

test("RPC notifications remain functional without terminal-specific UI", async () => {
  const h = harness(); createStatusExtension([custom])(h.pi);
  const c = context(null, "mock-provider"); c.ctx.mode = "rpc";
  await h.run("", c.ctx);
  assert.match(c.notices.at(-1)!.message, /Daily: 55% remaining/);
});
