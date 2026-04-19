# child-subagent

Adds the `subagent` tool for managed background subagent runs.
Owns the bundled agent prompts and subagent runtime behavior.

## Behavior

- Uses the current model by default.
- Chooses the child thinking level automatically from the delegated task.
- Uses the current working directory by default.
- Forwards the currently active built-in tools to child runs.
- Keeps managed `subagent` runs fire-and-forget.
- Sends `subagent-result` follow-up messages when managed runs finish.

## Bundled agents

The default agent prompts live here.

- `~/.pi/agent/extensions/child-subagent/agents/subagent-scout.md`
- `~/.pi/agent/extensions/child-subagent/agents/subagent-analyzer.md`

Agent prompts do not pin a model.
The runtime passes the current parent session model unless the agent explicitly pins one.
The runtime also chooses the thinking level automatically from the delegated task.

## Tools

### `subagent`

Use this for managed scout-style delegation.
It starts a background run, emits live updates for the inspector, and later sends a
`subagent-result` follow-up message.

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
