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
