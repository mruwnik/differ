---
description: Review current changes using differ-review
allowed-tools: Bash(git:*), Read, Glob, Grep, mcp__differ-review__*
argument-hint: [focus area or specific instructions]
---

Review the current uncommitted changes in this repository.

Additional context from user: $ARGUMENTS

## Steps

1. Get or create a review session using `mcp__differ-review__get_or_create_session` with the current repo path
2. Check for untracked files using `git status --porcelain` (lines starting with `??`)
   - Filter by file extension/path to find likely source files (e.g., .cljs, .ts, .py, .js, .go, etc.)
   - Exclude obvious non-review paths: node_modules/, .git/, build/, dist/, target/, __pycache__/, *.log, .DS_Store
   - If there are potentially relevant untracked source files, ask the user if they should be added to the review
   - Register confirmed files using `mcp__differ-review__register_files`
3. Get the diff using `mcp__differ-review__get_session_diff` to see all current changes
   - If the diff is empty or unavailable, fall back to `git diff HEAD`
4. For any new files in the session, use `mcp__differ-review__get_file_versions` to see the full content
5. Provide a thorough code review covering:
   - Overall summary of the changes
   - Architecture and design decisions
   - Potential bugs or issues
   - Performance concerns
   - Security considerations
   - Code style and best practices
6. Add review comments using `mcp__differ-review__add_comment` for specific issues found
   - Use whatever you feel like as the author, as long as you'll know it was you
   - Include file path and line number for specific comments
7. Submit the review using `mcp__differ-review__submit_review`
   - The body should be **3-4 lines max**: verdict, critical issue count, one-sentence summary
   - Example: "**Request Changes** - 2 critical, 4 major issues. Pagination not implemented (data loss) and open redirect in OAuth need fixing before merge."
   - All detail goes in line comments, not the summary

## Reporting

- **To user (terminal)**: Be detailed. List all issues by severity, include file references, explain the "why"
- **To GitHub (submit_review body)**: Be terse. Verdict + issue counts + one-liner. That's it.

## Review Style

Be direct and unvarnished. No praise sandwiches. If code is bad, say so and why.
Challenge assumptions. Call out "works but is wrong". Tolerate no errors.
Skip the "nice work!" fluff - the user wants truth, not comfort.
