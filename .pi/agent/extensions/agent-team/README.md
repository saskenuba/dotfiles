# Persistent agent teams

`agent-team` adds bidirectional collaboration without replacing the existing `subagent` tool.
Workers run as long-lived Pi RPC processes with their own retained conversations.

## Activate

Run `/reload`, or restart Pi.
Pi discovers `agent-team/index.ts` automatically from your personal extensions directory.
No settings changes or dependency installation are required for normal use.

Ask Pi to start a persistent specialist and send it further context as the work develops.
The orchestrator can answer worker questions without recreating the worker.

## Orchestrator tools

- `agent_start`: Start a uniquely named member using an existing persona or an expert `role`.
- `agent_send`: Send context, corrections, a correlated reply, or a new assignment.
- `agent_status`: Inspect member states, session paths, pending questions, and recent messages.
- `agent_wait`: Receive the next unread significant report without polling.
- `agent_close`: Cancel pending work and close the member while retaining its history.

Example tool arguments:

```json
{
  "name": "auth-reviewer",
  "role": "Staff Security Engineer",
  "task": "Review the authentication flow. Do not edit files. Ask me if requirements are unclear.",
  "context": "Sessions must remain compatible with existing clients.",
  "continueParent": true
}
```

`agent_start` returns an `agentId` and an independent `assignmentId`.
In the TUI, starting a member yields the orchestrator unless `continueParent=true`.
Continue only with independent, non-overlapping work.
In noninteractive modes, use `agent_wait` instead of ending the parent while workers are running.

Send an important correction:

```json
{
  "agentId": "<returned agent ID>",
  "message": "Preserve the legacy refresh-token endpoint. Confirm before proceeding.",
  "mode": "steer",
  "requiresAck": true
}
```

A question or blocker includes a message ID and leaves the member blocked.
Reply with that ID:

```json
{
  "agentId": "<returned agent ID>",
  "replyTo": "<question message ID>",
  "message": "Use API v2; keep v1 behavior unchanged."
}
```

Use `newAssignment=true` to give an idle member a new task while retaining its conversation.
Ordinary updates remain part of the existing assignment.
Completed assignments leave the member idle, not closed.
Resolve requested acknowledgments before starting a new assignment.

## Direct user controls

- `/team`: List members, their states, and pending question IDs.
- `/team-send <agentId> <message>`: Send guidance and automatically answer a pending question.
- `/team-close <agentId|all>`: Close one member or all live members.

`/team-send` requests an acknowledgment.
It does not require the orchestrator to make another model call to send your instruction.

## Worker communication

Only explicitly launched workers load `worker.ts` and its `orchestrator_message` tool.
They cannot spawn further team members.

Workers can send `progress`, `finding`, `question`, `blocker`, or `ack` messages.
Questions and blockers must be the only tool call in a turn and yield immediately.
The bridge removes sibling calls from that turn to make yielding reliable.
Routine progress updates the status/history without waking the orchestrator.
Significant findings, questions, results, and failures wake it through a follow-up message.
Acknowledgments become context on the next turn without waking it independently.

Messages have IDs, assignment IDs, optional reply IDs, and delivery states.
RPC acceptance is not evidence that a worker understood or acted on an instruction.
Use `requiresAck=true` and look for an `ack` referencing the instruction's message ID.
The same message may appear through both a notification and `agent_wait`; deduplicate by ID.
Replying to a blocked worker clears unconsumed steering and follow-up instructions.
These instructions are marked failed; inspect status and re-send updates that still apply.

## Lifecycle and limits

At most four members can remain live, including idle and blocked members.
Close unused members to free capacity.
Recent message history is bounded; child session files retain the full conversation.
Each member accepts at most 32 unconsumed instructions and 32 outstanding acknowledgment requests.
Excess sends are rejected locally without failing the worker.
Required acknowledgments remain tracked independently of the 100-message display history.
Deduplication retains the last 512 worker message IDs and resets for a new assignment.

Workers are closed on parent shutdown, reload, session replacement, or branch navigation.
Parent snapshots follow the active session branch.
Restored live records are marked interrupted rather than silently restarting work.
This version does not reconnect workers after a crash or reopen a closed member.

Child sessions live under `<agent-dir>/agent-team/<parent-session-id>/`.
The default agent directory is `~/.pi/agent`.
Session files can contain sensitive task context; retain or delete them according to your needs.

## Safety and compatibility

- `steer` applies after current tool calls; `followUp` applies after the current run finishes.
- Neither delivery mode interrupts an edit already underway; close cancels pending work.
- Workers inherit OS permissions and credentials; a working directory is not a sandbox.
- Assign explicit file ownership or use separate worktrees for overlapping edits.
- Workers load only the bridge extension, not the parent's extension toolbox.
- Persona tool lists can select built-in tools, not arbitrary parent extensions.
- User and bundled personas are the default discovery scope.
- Project personas require confirmation, or explicit `allowProjectAgent=true` without a UI.
- Use team tools separately from exclusive `subagent` calls; its existing guard strips siblings.

The original subagent implementation and inspector are not modified.
Team updates use a separate `agent-team:update` event and compact status display.

## Development checks

With Pi and TypeScript installed:

```bash
node ~/.pi/agent/extensions/agent-team/scripts/check.mjs
```

Set `PI_PACKAGE_DIR` if the check script cannot locate your installed Pi package.
The checks use deterministic fixtures and do not make paid model calls.

Run the bridge and mailbox unit checks alone:

```bash
node ~/.pi/agent/extensions/agent-team/scripts/check.mjs --unit
```

Run the real RPC bridge check with an offline test provider:

```bash
node ~/.pi/agent/extensions/agent-team/scripts/check.mjs --bridge
```
