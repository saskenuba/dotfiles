---
description: Amend a ShapeUp analysis with Product's answers and re-evaluate
model: opus
---

# ShapeUp Analysis Amendment

You are continuing an existing ShapeUp analysis by incorporating new information from Product.

## Initial Setup

When this command is invoked, respond with:

```
I'm ready to amend a ShapeUp analysis with new information from Product.

Please provide:
1. The path to the existing ShapeUp analysis document (e.g., shapeup/2025-01-15-shapeup-feature-x.md)
2. Product's answers or new information to incorporate
```

Then wait for the user's input.

---

## Amendment Process

### Step 1: Read the Existing Document

Read the existing ShapeUp analysis document FULLY using the Read tool WITHOUT limit/offset parameters.

Understand:
- The original pitch and analysis
- Previous amendments (if any)
- Current verdict and open questions
- Code references already identified

### Step 2: Parse the New Information

Identify what Product has clarified:
- Which questions were answered?
- What new requirements or constraints emerged?
- Are there new examples or use cases?
- Did anything change from the original pitch?

### Step 3: Determine if Additional Research is Needed

Based on Product's answers:
- Do we need to research additional parts of the codebase?
- Did the scope change in a way that affects different components?
- Are there new technical constraints to investigate?

If yes, spawn `codebase-analyzer` agents to research the new areas.
Wait for all research to complete before proceeding.

### Step 4: Re-evaluate the Analysis

With the new information, reconsider:
- Does the Core Problem change?
- Are there new Constraints?
- Does the Simplest Path change?
- Are Risks reduced or new ones identified?
- Should the Verdict change?
- Are there remaining questions?

### Step 5: Update the Document

**IMPORTANT**: The document update has TWO parts:

#### Part A: Add Amendment Log Entry

Add a new entry to the `## Amendment Log` section (create it if it doesn't exist, place it before `## Code References`):

```markdown
## Amendment Log

### Amendment 1 - [YYYY-MM-DD HH:MM]

**Questions Answered:**
- Q: [Original question from analysis]
  A: [Product's answer]
- Q: [Another question]
  A: [Product's answer]

**New Information:**
- [Any additional context Product provided]

**Impact on Analysis:**
- [Brief summary of what changed in the main analysis]

**New Research Conducted:**
- [List any additional codebase research done, or "None required"]
```

#### Part B: Update Main Sections

Update the relevant main sections to reflect the new information:
- Update `### 1. Core Problem` if it's clearer now
- Update `### 4. Expected Deliverable` with concrete examples
- Update `### 5. Constraints` with new constraints
- Update `### 6. Simplest Path` if the path changed
- Update `### 7. Risks and Ambiguities` - remove resolved risks, add new ones
- Update `### 9. Verdict` if it changed
- Update `### 10. Questions for Product` - remove answered questions, add new ones
- Update `### 11. Code References` if new areas were researched

**Mark changed sections** with `[Updated in Amendment N]` at the end of the section header.

### Step 6: Update Frontmatter

Update the document frontmatter:
```yaml
last_updated: [YYYY-MM-DD]
amendment_count: [N]
verdict: [Current verdict - may have changed]
status: [in_progress | ready_for_engineering]
```

Add `status: ready_for_engineering` when:
- Verdict is Accept
- No critical questions remain
- Simplest path is clear and actionable

### Step 7: Write the Updated Document

Write the complete updated document back to the same file path.

### Step 8: Present Summary

Present a concise summary:

```
## Amendment Summary

**Document**: [filename]
**Amendment #**: [N]
**Previous Verdict**: [Accept/Reject]
**Current Verdict**: [Accept/Reject]

### What Changed:
- [List key changes to the analysis]

### Resolved Questions:
- [Questions that are now answered]

### Remaining Questions:
- [Questions still open, if any]

### Status: [In Progress / Ready for Engineering]

[If ready] The analysis is now complete and ready for engineering implementation.
[If not ready] Awaiting answers to the remaining questions above.
```

---

## Important Notes

- ALWAYS read the full existing document first
- PRESERVE the amendment history - never delete previous amendments
- UPDATE main sections to reflect current understanding (don't leave stale info)
- The Amendment Log is the historical record; main sections are current state
- Re-evaluate the Simplest Path against actual codebase constraints
- Be clear about what changed and why
- If Product's answers raise NEW questions, add them
- The goal is a living document that converges toward "Ready for Engineering"
