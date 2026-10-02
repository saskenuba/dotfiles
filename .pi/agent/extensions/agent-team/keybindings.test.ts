import assert from "node:assert/strict";
import test from "node:test";
import { ViewerKeybindings, type ViewerAction } from "./keybindings.js";

const motion = (action: ViewerAction, count = 1, counted = false) => ({ action, count, counted });

test("viewer resolver preserves navigation aliases", () => {
  const bindings = new ViewerKeybindings();
  const aliases: [ViewerAction, string[]][] = [
    ["dismiss", ["q", "\x1b"]],
    ["jumpStart", ["\x1b[H"]], ["jumpEnd", ["\x1b[F", "G"]],
    ["scrollUp", ["k", "\x1b[A", "\x19"]], ["scrollDown", ["j", "\x1b[B", "\x05"]],
    ["halfPageUp", ["\x15"]], ["halfPageDown", ["\x04"]],
    ["pageUp", ["\x1b[5~", "\x02"]], ["pageDown", ["\x1b[6~", "\x06"]],
  ];
  for (const [action, keys] of aliases) for (const key of keys) assert.deepEqual(bindings.resolve(key), motion(action));
});

test("gg is a sequence, G is case-sensitive, and counts apply once", () => {
  const bindings = new ViewerKeybindings();
  assert.equal(bindings.resolve("g"), undefined);
  assert.deepEqual(bindings.resolve("g"), motion("jumpStart"));
  assert.equal(bindings.resolve("g"), undefined);
  assert.deepEqual(bindings.resolve("G"), motion("jumpEnd"));
  for (const key of ["1", "0", "g"]) assert.equal(bindings.resolve(key), undefined);
  assert.deepEqual(bindings.resolve("g"), motion("jumpStart", 10, true));
  for (const key of ["2", "5"]) assert.equal(bindings.resolve(key), undefined);
  assert.deepEqual(bindings.resolve("G"), motion("jumpEnd", 25, true));
  assert.deepEqual(bindings.resolve("j"), motion("scrollDown"));
  bindings.resolve("3");
  assert.deepEqual(bindings.resolve("\x04"), motion("halfPageDown", 3, true));
});

test("unknown inputs, zero, interrupted sequences, and reset clear pending state", () => {
  const bindings = new ViewerKeybindings();
  assert.equal(bindings.resolve("0"), undefined);
  assert.deepEqual(bindings.resolve("j"), motion("scrollDown"));
  bindings.resolve("4"); bindings.resolve("g"); bindings.resolve("?");
  assert.deepEqual(bindings.resolve("k"), motion("scrollUp"));
  bindings.resolve("g"); bindings.resolve("2");
  assert.equal(bindings.resolve("g"), undefined);
  assert.deepEqual(bindings.resolve("g"), motion("jumpStart", 2, true));
  bindings.resolve("9"); bindings.resolve("g"); bindings.reset();
  assert.equal(bindings.resolve("g"), undefined);
  assert.deepEqual(bindings.resolve("g"), motion("jumpStart"));
  for (const key of "123456789") bindings.resolve(key);
  assert.deepEqual(bindings.resolve("j"), motion("scrollDown", 123456, true));
});

test("application cancel is inherited, takes precedence, and retains q/Escape", () => {
  const calls: string[] = [];
  const bindings = new ViewerKeybindings({ matches(data, id) { calls.push(id); return data === "j" && id === "tui.select.cancel"; } });
  bindings.resolve("3"); bindings.resolve("g");
  assert.deepEqual(bindings.resolve("j"), motion("dismiss"));
  assert.deepEqual(bindings.resolve("G"), motion("jumpEnd"));
  assert.deepEqual(bindings.resolve("q"), motion("dismiss"));
  assert.deepEqual(bindings.resolve("\x1b"), motion("dismiss"));
  assert.ok(calls.length > 0 && calls.every((id) => id === "tui.select.cancel"));
});
