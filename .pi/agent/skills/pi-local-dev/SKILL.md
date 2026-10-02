---
name: pi-local-dev
description: Use when developing or modifying pi extensions, agents, skills, themes, custom tools, or local pi config. Prefer direct path navigation over broad search.
---

# Pi Local Development

Go to likely pi paths first.
Search only when the target is unclear.
Read existing files before editing.

## User Paths

Choose the form that fits the OS.

- Root: `~/.pi/` or `%USERPROFILE%\.pi\`
- Agent root: `~/.pi/agent/` or `%USERPROFILE%\.pi\agent\`
- Extensions: `~/.pi/agent/extensions/`
- Agents: `~/.pi/agent/agents/`
- Skills: `~/.pi/agent/skills/`
- Themes: `~/.pi/agent/themes/`
- Settings: `~/.pi/agent/settings.json`
- Keybindings: `~/.pi/agent/keybindings.json`
- Sessions: `~/.pi/agent/sessions/`
- Project agents: `.pi/agents/`

## Pi Docs

Use when the task involves pi APIs or behavior.

- Main docs: `/opt/homebrew/Cellar/pi-coding-agent/0.99.2/libexec/lib/node_modules/@earendil-works/pi-coding-agent/README.md`
- Docs dir: `/opt/homebrew/Cellar/pi-coding-agent/0.99.2/libexec/lib/node_modules/@earendil-works/pi-coding-agent/docs`
- Examples dir: `/opt/homebrew/Cellar/pi-coding-agent/0.99.2/libexec/lib/node_modules/@earendil-works/pi-coding-agent/examples`

## Procedure

1. Pick the most likely path.
2. Read files there.
3. Check settings or keybindings if relevant.
4. Search only if still unclear.

## Targeted Commands

Use bash for directory listings and scoped searches.
Use the `read` tool to examine file contents, including settings and keybindings, instead of `cat` or
`sed`.

```bash
find ~/.pi/agent/extensions -maxdepth 2 -type f 2>/dev/null
find ~/.pi/agent/agents -maxdepth 2 -type f 2>/dev/null
find ~/.pi/agent/skills -maxdepth 2 -type f 2>/dev/null
find ~/.pi/agent/themes -maxdepth 2 -type f 2>/dev/null
```

## Fallback

Choose the relevant resource directory above and expand only that scoped search.
Avoid blanket scans of `~/.pi`, which include sessions and credentials.
