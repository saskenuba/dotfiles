# Subagent Inspector

Provides the tracing UI for managed subagent runs.
Shows a dock, an inspector overlay, persisted history, and compact result rendering.
Does not own agent definitions or runtime execution.

## Responsibilities

- Renders the top-right dock for live and recent runs.
- Opens the full inspector overlay.
- Persists inspector snapshots across reloads and resumed sessions.
- Renders TUI background `subagent-result` follow-up messages compactly in chat.
- Subscribes to live run updates emitted by the runtime extension.

## Modes and branches

The dock, overlay, and hotkey require interactive TUI mode.
RPC supports notifications and dialogs, but it cannot render this inspector's terminal components.
Outside the TUI, overlay commands report unavailability through `ctx.ui.notify()`, not raw stdout.
Print and JSON modes have no UI.

The runtime delegates in the background only in TUI mode.
Print, JSON, and RPC tool calls wait for completion and return results directly, without a separate
`subagent-result` follow-up.

The inspector flushes snapshots before tree navigation and restores the active branch afterward.
The runtime cancels active jobs before navigation and suppresses their background follow-ups.
Persisted history does not mean interrupted processes can resume after a reload or session restore.

## Commands

- `/agents` opens the subagent inspector overlay.
- `/subagents` opens the subagent inspector overlay.

## Hotkey

- `Ctrl+A` cycles hidden → dock → modal → hidden.
- Inspector navigation uses vim-style keys.
- `j`/`k` move, `h`/`l` switch runs, `g`/`G` jump, and `q` closes.
- The extension-level bindings live in `keybindings.ts` to keep them decoupled from the UI code.

## Runtime ownership

Managed subagent execution and bundled agent prompts live in `child-subagent`.
This extension only observes runtime events, persists inspector state, and renders it.
Shared execution code lives in `subagent-core`.
Children disable extensions, including this inspector and extension-defined providers or tools.
Context files and skills may still affect children; this isolation is not a security sandbox.
See [child-subagent](../child-subagent/README.md) for modes, cancellation, and isolation limits.

## Install

This folder is already in an auto-discovered location.

- `~/.pi/agent/extensions/subagent-inspector/index.ts`

Reload pi with `/reload`.
You can also restart pi.
