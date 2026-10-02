# child-subagent

Adds the `subagent` tool for managed delegation.
Runs in the background in TUI mode and waits for completion in print, JSON, and RPC modes.
Owns the bundled agent prompts and subagent runtime behavior.

## Behavior

- Uses the current model by default, unless the selected agent explicitly pins a model.
- Shows a five-second TUI launch preview when an automatic provider/model is available.
- Chooses the child thinking level automatically from the delegated task.
- Uses the current working directory by default.
- Uses the agent's tool list when specified; otherwise forwards the active parent built-in tools.
- In TUI mode, returns a background job handle after launch preparation.
- In TUI mode, stops the parent turn by default and sends a `subagent-result` follow-up when done.
- In print, JSON, and RPC modes, waits and returns output, run details, error state, and usage.
- In those waiting modes, does not terminate the parent turn or send a duplicate result follow-up.
- In every mode, removes sibling non-subagent tool calls unless `continueParent: true` is explicit.

## Bundled agents

The default agent prompts live here.

- `~/.pi/agent/extensions/child-subagent/agents/subagent-scout.md`
- `~/.pi/agent/extensions/child-subagent/agents/subagent-analyzer.md`

Agent prompts do not pin a model.
The runtime passes the current parent session model unless the agent explicitly pins one.
When an automatic provider/model is available, a five-second TUI preview shows that choice.
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

Use this for scoped delegation to named agents or ad-hoc experts.
It emits runtime updates in every mode; the companion inspector renders them in the TUI.
In TUI mode, it starts a background run and later sends `subagent-result` to resume the parent.
By default, it stops the current parent turn after launch.
In print, JSON, and RPC modes, the tool call waits for the completed result instead.
The delegation guard removes sibling non-subagent tool calls in every mode unless
`continueParent: true` is explicit.
Set that option only for independent, non-overlapping parent work.
Do not poll by calling `subagent` again; resume delegated work from the returned result.
The tool is model-only and cannot be called through codemode or nested `ctx.executeTool()` calls.

Supports single, parallel, and chain modes.
Supports agent discovery (bundled, user, project agents) and ad-hoc roles.
The scout defaults to file-location reconnaissance; explicit delegated analysis is allowed.
The analyzer defaults to explaining implementation; explicit review or bug finding is allowed.
Recommendations require an explicit request in either role.

## Cancellation and branches

Before tree navigation, the runtime cancels active jobs and suppresses their background follow-ups.
This prevents results from the launching branch from arriving on a different branch.
Session shutdown also cancels active jobs and suppresses follow-ups.
In print, JSON, and RPC modes, tool-call cancellation is forwarded while the call waits.
Child cancellation sends SIGTERM, then SIGKILL after one second if the Pi child has not closed.
This targets the Pi child, not a process group, and does not guarantee cleanup of descendants.

## Child isolation limits

Named and ad-hoc children start separate Pi processes with `--no-extensions` and `--no-session`.
Disabling extensions prevents inheritance of parent extension prompt injections and runtime hooks.
It also disables extension-defined providers and tools, including built-in extensions.
Those capabilities are not transparently inherited from the parent.
Agent tool lists still take precedence over the forwarded built-in tool defaults.
Context files and skills are not disabled and may still influence child prompts.
Children inherit the process environment and operating-system permissions.
This is extension isolation, not a security sandbox.
The working directory is not an access boundary.

## Companion inspector

The tracing UI lives in `subagent-inspector`.
That extension renders dock, overlay, history, and result messages and persists inspector snapshots.
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
