import type { KeybindingsManager } from "@earendil-works/pi-coding-agent";
import { matchesKey, type Keybinding, type KeyId } from "@earendil-works/pi-tui";

export interface ViewerActionBinding {
  ids?: readonly Keybinding[];
  keys?: readonly (KeyId | "G")[];
  sequences?: readonly (readonly (KeyId | "G")[])[];
}

/** Viewer-local defaults; application actions inherit Pi's active keybindings. */
export const VIEWER_ACTIONS = {
  dismiss: { ids: ["tui.select.cancel"], keys: ["escape", "q"] },
  jumpStart: { keys: ["home"], sequences: [["g", "g"]] },
  jumpEnd: { keys: ["end", "G"] },
  scrollUp: { keys: ["up", "k", "ctrl+y"] },
  scrollDown: { keys: ["down", "j", "ctrl+e"] },
  halfPageUp: { keys: ["ctrl+u"] },
  halfPageDown: { keys: ["ctrl+d"] },
  pageUp: { keys: ["pageUp", "ctrl+b"] },
  pageDown: { keys: ["pageDown", "ctrl+f"] },
} as const satisfies Record<string, ViewerActionBinding>;

export const VIEWER_HELP_TEXT = "Esc/q return · j/k · gg/G · ^u/d · ^b/f";
export type ViewerAction = keyof typeof VIEWER_ACTIONS;
export interface ViewerMotion { action: ViewerAction; count: number; counted: boolean }
export interface ViewerBindingResolver { resolve(data: string): ViewerMotion | undefined }

function matches(data: string, key: KeyId | "G"): boolean {
  // g/G are case-sensitive: matchesKey normalizes printable letter case.
  return key === "g" || key === "G" ? data === key : matchesKey(data, key);
}

/** One resolver per viewer: partial sequences and counts never leak between screens. */
export class ViewerKeybindings implements ViewerBindingResolver {
  private motionCount = "";
  private sequence: string[] = [];

  constructor(private keybindings?: Pick<KeybindingsManager, "matches">) {}

  reset(): void { this.motionCount = ""; this.sequence = []; }

  resolve(data: string): ViewerMotion | undefined {
    const bindings = Object.entries(VIEWER_ACTIONS) as [ViewerAction, ViewerActionBinding][];
    const dismiss = VIEWER_ACTIONS.dismiss;
    if (dismiss.ids.some((id) => this.keybindings?.matches?.(data, id)) || dismiss.keys.some((key) => matches(data, key))) {
      this.reset();
      return { action: "dismiss", count: 1, counted: false };
    }
    if (/^[0-9]$/.test(data) && (data !== "0" || this.motionCount)) {
      this.motionCount = (this.motionCount + data).slice(0, 6);
      this.sequence = [];
      return undefined;
    }
    const sequence = [...this.sequence, data];
    const sequenceBinding = bindings.find(([, binding]) => binding.sequences?.some((keys) =>
      keys.length === sequence.length && keys.every((key, i) => matches(sequence[i], key))));
    if (!sequenceBinding && bindings.some(([, binding]) => binding.sequences?.some((keys) =>
      keys.length > sequence.length && sequence.every((input, i) => matches(input, keys[i]))))) {
      this.sequence = sequence;
      return undefined;
    }
    const action = sequenceBinding?.[0] ?? bindings.find(([, binding]) =>
      binding.ids?.some((id) => this.keybindings?.matches?.(data, id)) || binding.keys?.some((key) => matches(data, key)))?.[0];
    const count = this.motionCount ? Number(this.motionCount) : 1;
    const counted = this.motionCount !== "";
    this.reset();
    return action ? { action, count, counted } : undefined;
  }
}
