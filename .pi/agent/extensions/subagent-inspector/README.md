# Subagent Inspector

Provides the tracing UI for managed subagent runs.
Shows a dock, an inspector overlay, persisted history, and compact result rendering.
Does not own agent definitions or runtime execution.

## Responsibilities

- Renders the top-right dock for live and recent runs.
- Opens the full inspector overlay.
- Persists inspector snapshots across reloads and resumed sessions.
- Renders `subagent-result` follow-up messages compactly in chat.
- Subscribes to live run updates emitted by the runtime extension.

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
This extension only observes runtime events and renders them.

## Install

This folder is already in an auto-discovered location.

- `~/.pi/agent/extensions/subagent-inspector/index.ts`

Reload pi with `/reload`.
You can also restart pi.
