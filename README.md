# Differ

A local code review tool for AI-generated code.

## Why?

Reviewing agent-generated code changes has friction:

- Jumping between files to find what changed
- No persistent place to leave feedback
- Copy-pasting feedback back into agent conversations
- Multiple agents may work on the same codebase simultaneously

Differ provides a GitHub PR-like review UX, but local and integrated with AI agents via MCP. You review changes in a web UI, leave inline comments, and agents can see your feedback and respond—all in real-time.

## How It Works

1. Agent starts work, calls `get_or_create_session` to join/create a review session
2. Agent creates/modifies files. Any git tracked files will be automatically shown, and the agent can call `register_files` to add new files to the review
3. You open the web UI, see the diff, leave inline comments
4. Agent calls `get_pending_feedback` to see your comments
5. Agent addresses feedback, replies to comments, marks them resolved
6. Repeat until you're satisfied, then push

Sessions are tied to a (project, branch) pair. Multiple agents on the same project+branch share a session, so comments are visible to all participants.

## Features

- **GitHub-style diff viewer** - Split or unified view modes with syntax highlighting
- **MCP integration** - AI agents can register files, read comments, and respond
- **Real-time updates** - SSE-based live sync between UI and agents
- **Session management** - Track multiple review sessions across projects

## Quick Start

```bash
# Start dev environment (compiles + watches + runs server)
npm install
clj -M:dev
```

Or using npm/npx:

```bash
npm install
npx shadow-cljs watch server ui
```

Open <http://localhost:8081> for development (with hot reload) or <http://localhost:8576> for production.

## Production Build

```bash
# Using clj
clj -M:build

# Or using npx
npx shadow-cljs release server ui

# Start the server
node target/server.js
```

## MCP Integration

Differ exposes an MCP endpoint at `http://localhost:8576/mcp` for AI agent integration.

**Note:** The MCP server implements the OAuth flow for clients that require it, but accepts all requests without actually verifying credentials. This is intentional as local services are assumed to be secure. Don't expose it to the internet.

### Claude Code

Add to your MCP settings file (`~/.claude/settings.json` or project-level `.claude/settings.json`):

```json
{
  "mcpServers": {
    "differ-review": {
      "type": "http",
      "url": "http://localhost:8576/mcp"
    }
  }
}
```

See [Claude Code MCP docs](https://docs.anthropic.com/en/docs/claude-code/mcp) for more details.

### Other Agents

The endpoint accepts standard MCP JSON-RPC over HTTP POST. Any agent that supports MCP can connect by pointing to the URL.

### Available MCP Tools

| Tool | Description |
|------|-------------|
| `get_or_create_session` | Get or create a review session for a repo |
| `list_sessions` | List all active review sessions |
| `register_files` | Add files to the review set |
| `unregister_files` | Remove files from the review set |
| `get_review_state` | Get current session state with all comments |
| `get_pending_feedback` | Get unresolved comments |
| `add_comment` | Add a comment or reply |
| `resolve_comment` | Mark a comment as resolved |
| `unresolve_comment` | Reopen a resolved comment |
| `submit_review` | Finish your review with an optional summary comment |
| `get_session_diff` | Get the diff content for a session |
| `get_file_versions` | Get original and modified versions of a file |
| `get_context` | Get session context (path, branch, PR info) |
| `list_directory` | List directory contents at a ref |
| `get_file_content` | Get file content at a ref |
| `get_history` | Get commit/change history for the session |
| `create_pull_request` | Push branch and create a GitHub PR |
| `list_github_prs` | List open GitHub PRs for a repo, annotated with differ session state |
| `wait_for_event` | Block until new events appear for a scope (GitHub PR activity or session updates); see below |

### `wait_for_event`

Blocks inside differ until new events arrive for a watched scope, then returns them. Designed for agents that want to react without polling — one tool call covers a long stretch of idle time, so an idle agent logs one entry in its context per wait instead of dozens of empty poll responses.

**Scopes:**

| Scope format | Source | Event types |
|---|---|---|
| `github:owner/repo` | Internal poller (every ~30s, lazy-started on first call, grace-stops after 5 min idle) | `pr-opened`, `pr-head-changed`, `pr-feedback-changed`, `pr-closed` |
| `session:<session-id>` | Push: emitted by MCP handlers on the same session | `comment-added`, `comment-resolved`, `comment-unresolved`, `files-registered`, `files-unregistered`, `review-submitted` |

The github and session scopes share the same ring buffer + wait infrastructure (`differ.event-stream`). Agents use the same loop pattern for both — only the scope string changes.

**Arguments:**

- `scope` (required) — `"github:owner/repo"` or `"session:<session-id>"`
- `since_seq` (default `0`) — return events with `seq > since_seq`
- `timeout_ms` (default `300000`, max `600000`) — max ms to block; `0` = peek (return immediately)
- `max_events` (default `50`, capped at `500`) — upper bound per response

**Response shape:**

Every event has a common envelope (`seq`, `received_at`, `event_type`, `scope`) plus event-type-specific fields. Example GitHub event:

```json
{
  "events": [
    {
      "seq": 42,
      "received_at": "2026-04-08T14:23:10.542Z",
      "event_type": "pr-head-changed",
      "scope": "github:EquiStamp/cairn",
      "project": "EquiStamp/cairn",
      "pr_number": 31,
      "pr_url": "https://github.com/EquiStamp/cairn/pull/31",
      "head_branch": "feature/foo",
      "base_branch": "main",
      "author": "octocat",
      "title": "Add widget",
      "draft": false,
      "session_id": null,
      "details": {
        "new_head_sha": "abc123",
        "previous_head_sha": "000111"
      }
    }
  ],
  "next_seq": 42,
  "timed_out": false
}
```

Example session event:

```json
{
  "events": [
    {
      "seq": 7,
      "received_at": "2026-04-08T14:24:05.101Z",
      "event_type": "comment-added",
      "scope": "session:github:EquiStamp/cairn:31",
      "session_id": "github:EquiStamp/cairn:31",
      "comment_id": "c-abc",
      "author": "reviewer",
      "file": "src/foo.clj",
      "line": 42,
      "parent_id": null,
      "created_at": "2026-04-08T14:24:04.950Z"
    }
  ],
  "next_seq": 7,
  "timed_out": false
}
```

**Typical agent loop:**

```python
since = 0
while True:
    result = wait_for_event(scope="github:EquiStamp/cairn", since_seq=since)
    since = result["next_seq"]
    for event in result["events"]:
        handle(event)
```

**Configuration** (in `resources/config.edn` or via env vars):

- `DIFFER_EVENT_BUFFER_SIZE` (default 10000) — shared ring buffer capacity per scope (both `github:` and `session:`)
- `DIFFER_GITHUB_POLL_INTERVAL_MS` (default 30000) — GitHub poller interval
- `DIFFER_GITHUB_POLLER_GRACE_MS` (default 300000) — grace period before the poller stops

Events are kept in an in-memory ring buffer per scope and are lost on differ restart. Agents that reconnect after a restart should reconcile state (call `list_github_prs` / `get_review_state`) and then resume watching from the new `next_seq`.

**Duplicate event story (github + session scopes).** When a GitHub PR has an associated differ session, mutating MCP calls (`add_comment`, `resolve_comment`, `unresolve_comment`, `register_files`, `unregister_files`, `submit_review`) will surface on BOTH scopes:

- Immediately on `session:<id>` as the specific event (`comment-added`, `comment-resolved`, etc.)
- Within one poll interval (up to ~30 s) on `github:owner/repo` as a `pr-feedback-changed` envelope

Choose the scope that matches your latency and coverage requirements; DO NOT subscribe to both expecting deduplication — you will see the same underlying change twice, on two different scopes, with possibly different ordering.

## Slash Commands

Differ includes Claude Code slash commands for reviewing and fixing code:

- `/review` - Review current uncommitted changes, add comments via differ
- `/fix-review` - Fix all unresolved review comments

These are available automatically when working in the differ project.

### Using globally (all projects)

To use these commands in any project, copy them to your global commands directory:

```bash
cp .claude/commands/*.md ~/.claude/commands/
```

Then `/review` and `/fix-review` will work in any repo where the differ MCP server is configured.

## Review stop hook

In `.claude/hooks/pr_review_loop.py` there is a script that can be used to force
agents to go through a proper internal review -> GitHub PR -> PR closed flow, by
prompting the agent to push all changes to a proper PR, and then wait until it's closed.
To use it, add the following to the `"hooks"` section of the appropriate `settings.json` file:

```json
    "Stop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "python3 shared/hooks/pr_review_loop.py",
            "timeout": 43200
          }
        ]
      }
    ]
```

This will not run if the folder in which Claude Code is running is not a git repo, or if
the `PR_REVIEW_LOOP_DISABLED` env variable is set to `1`, `true` or `yes`.
By default, once all changes have been pushed etc., the script will wait up to 12h polling
for unresolved comments on the PR, after which it will just let the agent continue. The hook
definition has a timeout (default is 30s) after which it will just continue. So the lower of
these two values limits how long the agent will wait for feedback.

If the PR is closed, and there are no local changes, the hook will ask the Claude session to
commit sepukku for doing such a good job.

## Configuration

Edit `resources/config.edn` to customize settings:

```edn
{:port 8576

 ;; File display thresholds (client)
 :large-file-threshold 50000      ;; Characters - files larger require explicit load
 :line-count-threshold 400        ;; Diff lines - more than this requires explicit expand
 :context-expand-size 15          ;; Lines to expand at a time

 ;; Watcher settings (server)
 :watcher-debounce-ms 300         ;; Debounce file change events

 ;; Push whitelist - controls which repos/branches can be pushed via create_pull_request
 ;; Empty map = all repos/branches allowed
 ;; Example: {"owner/repo" ["feature/*" "fix/*"], "myorg/*" ["*"]}
 :push-whitelist {}}
```

### Push Whitelist

The `create_pull_request` tool can be restricted via a whitelist in `config.edn`:

```edn
{:push-whitelist {"owner/repo" ["feature/*" "fix/*"]
                  "myorg/*" ["*"]}}
```

- **Empty whitelist** (default): All repos/branches allowed
- **Repo keys**: Can use wildcards like `"owner/*"` to match any repo from that owner
- **Branch patterns**: Can use wildcards like `"feature/*"` to match branches
- `"*"` matches anything

The whitelist can also be set via the `PUSH_WHITELIST` environment variable as JSON:

```bash
export PUSH_WHITELIST='{"owner/repo": ["main", "develop"]}'
```

Environment variables:

- `PORT` - Server port (default 8576)
- `DIFFER_URL` - Base URL for OAuth callbacks (e.g., `http://localhost:8576`)
