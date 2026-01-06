# Claude Commands Style Guide

This document captures the patterns, conventions, and best practices used across all command files in `.claude/commands/`. Use this as a reference when creating or modifying commands.

---

## 1. File Structure

Every command file follows this exact structure:

```markdown
---
description: [Concise action-oriented description]
model: opus  # Optional: only when specific model needed
---

# [Command Title]

[Opening paragraph with role/task definition]

## [Section 1]
[Content]

## [Section 2]
[Content]
```

### YAML Frontmatter Rules

- **Always required** at the top of every file
- **description**: Single line, 5-15 words, present tense, action-oriented
- **model**: Optional field, values: `opus` or `sonnet`
- No quotes around values
- Three dashes before and after

**Examples:**
```yaml
---
description: Create git commits with user approval and no Claude attribution
---
```

```yaml
---
description: Create detailed implementation plans with thorough research and iteration
model: opus
---
```

---

## 2. Header Conventions

### Primary Header (H1)
- Immediately after frontmatter
- Title case for proper nouns, sentence case otherwise
- Examples: `# Commit Changes`, `# Implementation Plan`, `# Debug`

### Opening Paragraph Pattern
Always start with a role/task definition:

```markdown
You are tasked with [verb]ing [object] [additional context/constraints].
```

**Examples:**
- "You are tasked with creating git commits for the changes made during this session."
- "You are tasked with generating a comprehensive pull request description following the repository's standard template."

---

## 3. Section Naming Conventions

### Standard Sections (H2) - In Order of Appearance

| Section Name | Purpose |
|--------------|---------|
| `## Initial Setup` or `## Initial Response` | What happens when command is invoked |
| `## Process` or `## Process Steps` | Step-by-step workflow |
| `## Important Notes` or `## Important Guidelines` | Critical constraints |
| `## Guidelines` | Behavioral rules |
| `## Success Criteria Guidelines` | For plan-related files |
| `## Common Patterns` | Design patterns for specific scenarios |
| `## Sub-task Spawning Best Practices` | Task delegation guidelines |
| `## Example Interaction Flow` | Concrete usage examples |

### Subsection Naming (H3)

**Numbered Steps:**
```markdown
### Step 1: [Action Name]
### Step 2: [Action Name]
```

**Named Phases:**
```markdown
### For [scenario]:
### For [another scenario]:
```

---

## 4. Instruction Formatting

### Primary Workflow Pattern

```markdown
1. **[Action Step Header]:**
   - [Sub-point details]
   - [More details]
   - [Nested actions]

2. **[Next Action]:**
   - [Details]
```

**Rules:**
- Primary number is plain: `1.`, `2.`, `3.`
- First phrase after number is **bold**
- Colon after bold phrase
- Bullet points use `-` (hyphen, never `*`)
- Sub-bullets indent with proper spacing

### Lettered Sub-steps

For complex steps requiring sub-actions:
```markdown
2a. [sub-action]
2b. [another sub-action]
```

### Conditional Formatting

**If-Then Blocks:**
```markdown
**If [condition]**:
- [Action]
- [Another action]

**If [different condition]**:
- [Different action]
```

**Inline conditionals:**
```markdown
- If [condition], [action]
- Otherwise, [different action]
```

---

## 5. Emphasis Hierarchy

### Emphasis Levels (Most to Least Severe)

| Level | Format | Usage |
|-------|--------|-------|
| Critical | `**CRITICAL**:` | Violations cause complete failure |
| Important | `**IMPORTANT**:` | Key constraints to follow |
| Remember | `**Remember**:` | Contextual reminders |
| Note | `**Note**:` | Additional information |

### Action Keywords (Always Bold + Caps)

- `**NEVER**` - Prohibited actions
- `**ALWAYS**` - Required actions
- `**DO NOT**` - Explicit prohibitions
- `**MUST**` - Mandatory requirements

**Example:**
```markdown
- **CRITICAL**: Read the entire file before making changes
- **NEVER** commit the `thoughts/` directory
- **ALWAYS** wait for all sub-tasks to complete
```

---

## 6. Code and Path Formatting

### Code Blocks

**Bash commands:**
````markdown
```bash
command --flag value
```
````

**Multi-line examples:**
````markdown
```bash
# Comment explaining command
command \
  --long-flag value \
  --another-flag
```
````

**Template content:**
````markdown
```markdown
# Template content here
```
````

### Inline Code

| Element | Format | Example |
|---------|--------|---------|
| File paths | Backticks | `` `path/to/file.ext` `` |
| Line references | Path:line | `` `file.ext:123` `` |
| Line ranges | Path:start-end | `` `file.ext:45-67` `` |
| Commands | Backticks | `` `make test` `` |
| Tool names | Backticks | `` `gh` ``, `` `git` `` |

### Placeholder Conventions

| Type | Format | Example |
|------|--------|---------|
| Dates | `YYYY-MM-DD` | `2024-01-15` |
| Timestamps | `YYYY-MM-DD_HH-MM-SS` | `2024-01-15_13-55-22` |
| Tickets | `ENG-XXXX` | `ENG-1234` |
| Variables | `{name}` | `{number}`, `{repo_name}` |
| Branch names | `BRANCH_NAME` | - |

---

## 7. Tone and Writing Style

### Voice Patterns

| Context | Voice | Example |
|---------|-------|---------|
| Instructions | Directive/Imperative | "Read", "Create", "Verify" |
| Role definition | Second person | "You are tasked with..." |
| Responses | Collaborative | "Let me...", "I'll help you..." |
| User interaction | Polite | "Please provide...", "Would you like to..." |

### Style Rules

- **NO emojis** - Clean, professional style throughout
- **Present tense** for descriptions and actions
- **Imperative mood** for instructions
- **Collaborative tone** for user-facing messages
- **Concise** - Avoid unnecessary words

---

## 8. Common Phrases and Terminology

### Recurring Action Phrases

```
"Spawn [sub-tasks/agents]"
"Read [file] COMPLETELY/FULLY"
"Wait for ALL sub-tasks to complete"
"Sync the thoughts directory"
"Think deeply about [topic]"
"Ultrathink about [complex concept]"
"Present findings/draft"
"Get buy-in/confirmation"
"Iterate based on feedback"
```

### Technical Terms

- **Sub-tasks** / **sub-agents** / **Task agents** - Parallel work units
- **Thoughts directory** - Documentation storage (treat as proper noun)
- **Plan phases** - Implementation stages
- **Success criteria** - Always paired with Automated/Manual
- **File:line references** - Code location format
- **Worktree** - Git worktree concept
- **MCP tools** - Model Context Protocol tools

---

## 9. Success Criteria Pattern

For implementation plans, always use this two-category structure:

```markdown
### Success Criteria:

#### Automated Verification:
- [ ] [Test description]: `make test`
- [ ] [Type check]: `npm run typecheck`
- [ ] [Build check]: `make build`

#### Manual Verification:
- [ ] [UI test description]
- [ ] [Performance check description]
- [ ] [User acceptance criteria]
```

**Rules:**
- Use checkboxes: `- [ ]` (unchecked), `- [x]` (checked)
- Automated items include **executable command** after colon
- Prefer `make` commands: `make -C directory check`
- Manual items describe human testing steps

**Pause Pattern:**
```markdown
**Implementation Note**: After completing this phase and all automated verification passes, pause here for manual confirmation from the human that the manual testing was successful before proceeding to the next phase.
```

---

## 10. Tool Usage Specifications

### Read Tool
```markdown
- **IMPORTANT**: Use the Read tool WITHOUT limit/offset parameters
- **CRITICAL**: DO NOT spawn sub-tasks before reading these files yourself
- **NEVER** read files partially
```

### Git Commands
```markdown
- Always be specific: `git add [specific files]`
- Never use wildcards: avoid `-A` or `.`
- Prefer explicit file lists
```

### GitHub CLI
```bash
gh pr view --json url,number,title,state
gh pr diff {number}
gh repo view --json owner,name
```

### TodoWrite
```markdown
- Create a research todo list using TodoWrite to track exploration tasks
- Update todos as you complete research
```

### Specialized Agents

| Agent | Purpose |
|-------|---------|
| `codebase-locator` | Find files |
| `codebase-analyzer` | Understand implementation |
| `codebase-pattern-finder` | Find similar patterns |
| `thoughts-locator` | Find documents |
| `thoughts-analyzer` | Extract insights |
| `linear-ticket-reader` | Get ticket details |
| `web-search-researcher` | External research |

---

## 11. Scenario and Conditional Handling

### Multiple Scenario Pattern

```markdown
**If NO [resource] provided**:
```
[Response text]
```
Wait for user input.

**If [resource] provided but NO [other thing]**:
```
[Response text]
```
Wait for user input.

**If BOTH [resource] AND [other thing] provided**:
- Proceed immediately to Step 1
```

### Example Interaction Flows

```markdown
## Example Interaction Flows

**Scenario 1: User provides everything upfront**
```
User: /command args
Assistant: [action]
```

**Scenario 2: User provides partial info**
```
User: /command partial_args
Assistant: [clarifying question]
User: [additional info]
Assistant: [proceeds]
```
```

---

## 12. Template Responses

### Quote Block Pattern
````markdown
```
I'll help you [action]. [Details]

[Formatted list or structure]

[Question or next steps]?
```
````

### XML Tag Pattern (for exact text)
```markdown
<template_response>
[Exact text to use]
</template_response>

<example_response>
[Example implementation]
</example_response>
```

**Note:** Instructions should specify "do NOT include the tags in your response"

---

## 13. File Path Conventions

### Thoughts Directory Structure

| Type | Path Pattern |
|------|--------------|
| Plans | `thoughts/shared/plans/YYYY-MM-DD-ENG-XXXX-description.md` |
| Research | `thoughts/shared/research/YYYY-MM-DD-ENG-XXXX-description.md` |
| Handoffs | `thoughts/shared/handoffs/ENG-XXXX/YYYY-MM-DD_HH-MM-SS_ENG-XXXX_description.md` |
| Tickets | `thoughts/shared/tickets/eng_XXXX.md` |
| PRs | `thoughts/shared/prs/{number}_description.md` |

### Filename Format

- **With Ticket**: `YYYY-MM-DD-ENG-XXXX-description.md`
- **Without Ticket**: `YYYY-MM-DD-description.md`
- **Handoffs**: `YYYY-MM-DD_HH-MM-SS_ENG-XXXX_description.md`
- **Description**: Always kebab-case, 2-5 words

---

## 14. Behavioral Patterns

### Common Instruction Blocks

**"Think Deeply" Instruction:**
```markdown
think deeply

[or]

think deeply about [specific aspect]

[or]

Take time to ultrathink about [complex concept]
```

**"Wait for Completion" Pattern:**
```markdown
3. **Wait for ALL sub-tasks to complete** before proceeding
```

**"Read FULLY" Pattern:**
```markdown
- Read the [document] COMPLETELY
- Use the Read tool WITHOUT limit/offset parameters
- **NEVER** read files partially
```

**"Sync Thoughts" Pattern:**
```markdown
- Run `humanlayer thoughts sync` to sync the thoughts directory
```

### Quality Guidelines

**What NOT to Do Sections:**
```markdown
## What NOT to Do

- Don't guess about [aspect]
- Don't skip [important step]
- Don't ignore [consideration]
```

**Behavioral Qualities:**
- **Skeptical** - Question assumptions
- **Interactive** - Get buy-in
- **Thorough** - Complete coverage
- **Practical** - Focus on working solutions
- **Surgical** - Precise, minimal changes

---

## 15. Cross-Reference Patterns

### Reading Other Commands
```markdown
read `.claude/commands/[command].md`
```

### Related Commands Section
```markdown
## Relationship to Other Commands

Recommended workflow:
1. `/[command1]` - [Purpose]
2. `/[command2]` - [Purpose]
3. `/[command3]` - [Purpose]
```

### Invocation Tips
```markdown
Tip: You can also invoke this command with [pattern]: `/command [args]`
For deeper analysis, try: `/command think deeply about [args]`
```

---

## 16. Checklist for New Commands

When creating a new command, verify:

- [ ] YAML frontmatter with description (and model if needed)
- [ ] H1 title immediately after frontmatter
- [ ] Opening paragraph with "You are tasked with..."
- [ ] Clear section hierarchy (H2 for main sections, H3 for subsections)
- [ ] Numbered steps with **bold headers** and colons
- [ ] Proper emphasis hierarchy (CRITICAL > IMPORTANT > Remember > Note)
- [ ] All file paths in backticks
- [ ] All commands in code blocks
- [ ] Success criteria split into Automated/Manual (if applicable)
- [ ] Example interaction flows (if applicable)
- [ ] No emojis
- [ ] Consistent placeholder format
- [ ] Tool-specific usage patterns followed
- [ ] Cross-references to related commands

---

## Quick Reference

### Frontmatter
```yaml
---
description: [Action-oriented description]
model: opus  # Optional
---
```

### Section Template
```markdown
## [Section Name]

1. **[Action Header]:**
   - [Detail]
   - [Detail]

2. **[Next Action]:**
   - [Detail]
```

### Emphasis
```markdown
- **CRITICAL**: [Most severe]
- **IMPORTANT**: [Key constraint]
- **NEVER** [prohibited action]
- **ALWAYS** [required action]
```

### Success Criteria
```markdown
#### Automated Verification:
- [ ] [Description]: `command`

#### Manual Verification:
- [ ] [Human testing step]
```
