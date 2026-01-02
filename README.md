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

## Configuration

Create `resources/config.edn` to customize settings:

```edn
{:port 8576}
```

Or use the `PORT` environment variable.
