# child-subagent

Adds the `subagent` tool for managed background subagent runs.
Owns the bundled agent prompts and subagent runtime behavior.

## Behavior

- Uses the current model by default, unless the selected agent explicitly pins a model.
- Shows a five-second launch preview for every child run in the interactive TUI.
- Chooses the child thinking level automatically from the delegated task.
- Uses the current working directory by default.
- Forwards the currently active built-in tools to child runs.
- Keeps managed `subagent` runs fire-and-forget.
- Stops the parent turn by default after launch so delegated work is not duplicated.
- Removes sibling non-subagent tool calls when the parent asks a default subagent run to wait.
- Sends `subagent-result` follow-up messages when managed runs finish.

## Bundled agents

The default agent prompts live here.

- `~/.pi/agent/extensions/child-subagent/agents/subagent-scout.md`
- `~/.pi/agent/extensions/child-subagent/agents/subagent-analyzer.md`

Agent prompts do not pin a model.
The runtime passes the current parent session model unless the agent explicitly pins one.
Before each interactive child launch, a five-second preview shows that automatic choice.
If untouched, the countdown launches with the existing automatic model and thinking behavior.
Models follow Pi's automatically synchronized catalog order, with newer catalog entries first.
Type to fuzzy-search authenticated models from the automatic model's provider.
Use Up and Down to navigate the matching models.
Press Tab for more thinking or Shift+Tab for less thinking.
Press Enter to launch the selection.
Press Escape to keep the automatic choices.
Non-interactive modes preserve the automatic behavior without a popup.

## Tools

### `subagent`

Use this for managed scout-style delegation.
It starts a background run, emits live inspector updates, and later sends `subagent-result`.
By default it terminates the parent turn after launch.
If the same turn includes sibling non-subagent tool calls, the delegation guard removes them.
Set `continueParent: true` only for explicitly independent, non-overlapping work.

Supports single, parallel, and chain modes.
Supports agent discovery (bundled, user, project agents) and ad-hoc roles.

## Companion inspector

The tracing UI lives in `subagent-inspector`.
That extension only renders dock, overlay, history, and result messages.
It does not own agent definitions or runtime execution.

## Install

This folder is already in an auto-discovered location.

- `~/.pi/agent/extensions/child-subagent/index.ts`

Reload pi with `/reload`.
You can also restart pi.

## Examples

### Managed scout

- "Use subagent-scout to find the auth entry points"
- "Run two scouts in parallel, one for routes and one for tests"
- "Use a scout chain where the second step narrows the first step's findings"

### Ad-hoc expert

- "Use a Senior UX Designer subagent to review the onboarding flow"
- "Delegate to a Staff Engineer to evaluate the caching strategy"

## Auto thinking examples

Typical defaults are these.

- Simple greeting or lightweight rewrite → `minimal`.
- Find, list, or locate tasks → `low`.
- Research, inspect, trace, or summarize tasks → `medium`.
- Planning, review, root-cause, or design tasks → `high`.
- `xhigh` is only chosen for very explicit deep-reasoning requests.
- `max` is only chosen for explicit maximum-thinking requests.
