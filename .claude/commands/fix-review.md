---
description: Fix all unresolved review comments from differ-review
allowed-tools: Bash(git:*), Read, Write, Edit, Glob, Grep, mcp__differ-review__*
---

Fix all unresolved review comments in the current differ-review session.

## Steps

1. Get the review session using `mcp__differ-review__get_or_create_session` with the current repo path
2. Get pending feedback using `mcp__differ-review__get_pending_feedback`
3. For each unresolved comment:
   - Read the relevant file and understand the context
   - Implement the fix or improvement suggested
   - Use `mcp__differ-review__add_comment` to reply explaining what was done (as a child comment with parent_id)
   - Mark the comment as resolved using `mcp__differ-review__resolve_comment`
4. After fixing all comments, provide a summary of changes made

If a comment suggests something you disagree with or cannot implement, reply explaining why and ask for clarification rather than marking it resolved.
