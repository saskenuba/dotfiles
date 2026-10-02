import { MAX_HISTORY, type TeamMessage } from "./protocol.js";

/** Parent-side inbox. Messages can arrive before a waiter subscribes. */
export class Mailbox {
  private unread: TeamMessage[] = [];
  private listeners = new Set<() => void>();
  private disposed = false;

  push(message: TeamMessage): void {
    if (this.disposed || message.kind === "progress" || message.kind === "ack") return;
    this.unread.push(structuredClone(message));
    this.unread = this.unread.slice(-MAX_HISTORY);
    for (const listener of [...this.listeners]) listener();
  }

  private take(agentId?: string): TeamMessage | undefined {
    const index = this.unread.findIndex((message) => !agentId || message.agentId === agentId);
    return index < 0 ? undefined : this.unread.splice(index, 1)[0];
  }

  async wait(agentId: string | undefined, timeoutMs: number, signal?: AbortSignal): Promise<TeamMessage | undefined> {
    signal?.throwIfAborted();
    if (this.disposed) return undefined;
    const ready = this.take(agentId);
    if (ready) return ready;
    return new Promise((resolve, reject) => {
      let timer: ReturnType<typeof setTimeout>;
      const cleanup = () => {
        clearTimeout(timer);
        this.listeners.delete(check);
        signal?.removeEventListener("abort", abort);
      };
      const check = () => {
        const message = this.take(agentId);
        if (message || this.disposed) { cleanup(); resolve(message); }
      };
      const abort = () => { cleanup(); reject(signal?.reason ?? new Error("Wait cancelled")); };
      this.listeners.add(check);
      signal?.addEventListener("abort", abort, { once: true });
      timer = setTimeout(() => { cleanup(); resolve(undefined); }, timeoutMs);
      // Covers a signal cancelled just before registration.
      if (signal?.aborted) abort();
      else check();
    });
  }

  dispose(): void {
    this.disposed = true;
    this.unread = [];
    for (const listener of [...this.listeners]) listener();
    this.listeners.clear();
  }
}
