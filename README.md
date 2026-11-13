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

Sessions are tied to a (project, branch) pair. Multiple agents on the same project+branch share a session, so comments are visible to all participants. A checkout with a detached HEAD has no branch, so it gets its own session, keyed by its path.

## Features

- **GitHub-style diff viewer** - Split or unified view modes with syntax highlighting
- **MCP integration** - AI agents can register files, read comments, and respond
- **Real-time updates** - SSE-based live sync between UI and agents
- **Session management** - Track multiple review sessions across projects
- **GitHub PRs** - Review GitHub pull requests, push branches and open PRs via `request_review`
- **Kanban board** - Per-repo task board for coordinating several agents

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

Then connect your agent, e.g. Claude Code (details in [MCP Integration](#mcp-integration)):

```bash
claude mcp add --transport http differ http://localhost:8576/mcp
# then, inside Claude Code: /mcp -> differ -> authenticate
```

Read the [Security notes](#security-notes) before running it anywhere but a trusted machine.

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

**Note:** Tool calls require a Bearer token, which clients obtain through the server's OAuth flow (`initialize` and `tools/list` work without one). By default the authorization endpoint approves every request without asking for credentials, and any client can register itself, so by default the token check does not limit who can call tools: anyone who can reach the port can get a token. Set `DIFFER_AUTH_*` (below) to change that. This is intentional as local services are assumed to be secure. Don't expose it to the internet.

**Optional login:** Set both `DIFFER_AUTH_USERNAME` and `DIFFER_AUTH_PASSWORD` (env vars or `.env`) to require a username/password before the authorization endpoint issues a token. When set, the OAuth authorize page shows a login form and only grants access after the credentials match. When unset, the auto-approve behavior above is used.

> **Scope of the optional login — read before relying on it.** These credentials gate **only the MCP OAuth flow** — i.e. who can complete `/oauth/authorize` to obtain a Bearer token for the `/mcp` endpoint. They do **not** protect the web UI or its REST API:
>
> - The web UI (served at `/`) and every `/api/*` route are **unauthenticated**. Anyone who can reach the port can browse diffs, read file contents, and add/resolve comments and board tasks without a password.
> - The login is not a network boundary. Regardless of whether it is set, do **not** expose the server to the internet — keep it on localhost/trusted networks; it has no host setting and listens on all interfaces (see [Security notes](#security-notes)), so use a firewall (or put it behind your own authenticating reverse proxy / VPN if you need remote access).
>
> In short: the optional login raises the bar for issuing MCP tokens; it is not a substitute for keeping the server off untrusted networks.

### Claude Code

Add the server with the CLI (add `--scope user` to have it in every project):

```bash
claude mcp add --transport http differ http://localhost:8576/mcp
```

Or check a `.mcp.json` into the project root (Claude Code asks for approval before using project servers):

```json
{
  "mcpServers": {
    "differ": {
      "type": "http",
      "url": "http://localhost:8576/mcp"
    }
  }
}
```

The bundled slash commands expect the server to be named `differ` (their tools are `mcp__differ__*`). If you registered it as `differ-review` before, rename that entry to `differ` (or re-add it with the command above) or edit the prefix in the copied commands; otherwise their `allowed-tools` match nothing.

**First use:** run `/mcp` in Claude Code, select `differ` and authenticate. Claude Code opens the server's authorization page in a browser; with the default settings it approves at once (with the optional login, enter the username and password). Access tokens issued by differ are valid for a year.

**Browser on another machine** (e.g. Claude Code over SSH): the OAuth redirect goes to a callback that Claude Code listens on at `http://localhost:<port>/callback` on the machine where Claude Code runs, and the browser also has to reach differ's authorization page (at `DIFFER_URL`, default `http://localhost:8576`). Claude Code picks a random callback port unless you fix it with `--callback-port`. Forward both ports from the browser machine, e.g.:

```bash
claude mcp add --transport http --callback-port 8577 differ http://localhost:8576/mcp
# on the machine with the browser:
ssh -L 8576:localhost:8576 -L 8577:localhost:8577 user@host
```

See [Claude Code MCP docs](https://code.claude.com/docs/en/mcp) for more details.

### Other Agents

The endpoint accepts standard MCP JSON-RPC over HTTP POST. Any agent that supports MCP over HTTP can connect by pointing to the URL. How the server is configured differs between clients; the client must support the OAuth flow (the server publishes `/.well-known/oauth-authorization-server` and `/.well-known/oauth-protected-resource` and supports dynamic client registration, PKCE and refresh tokens) or send a valid `Authorization: Bearer` token. Redirect URIs must point to localhost or a private-network address.

### Available MCP Tools

Required arguments are listed; see each tool's schema (`tools/list`) for the optional ones.

**Sessions and diff**

| Tool | Required | Description |
|------|----------|-------------|
| `list_sessions` | – | List active review sessions, optionally filtered by `project` |
| `get_or_create_session` | `repo_path` or `github_pr` | Get or create the session for a local repo/branch or a GitHub PR URL |
| `register_files` | `session_id`, `paths`, `agent_id` | Add files to the review set |
| `unregister_files` | `session_id`, `paths`, `agent_id` | Remove files from the review set |
| `get_session_diff` | `session_id` | Get the parsed diff for a session (optionally one `file`) |
| `get_file_versions` | `session_id`, `file` | Get original and modified versions of a file |
| `get_context` | `session_id` | Get session context (local: path, branch; GitHub: PR info) |
| `list_directory` | `session_id` | List directory contents at a ref |
| `get_file_content` | `session_id`, `file` | Get file content at a ref, optionally a line range |
| `get_history` | `session_id` | Get commit/change history for the session |

**Comments and review**

| Tool | Required | Description |
|------|----------|-------------|
| `get_review_state` | `session_id` | Get session state with files and all comments |
| `get_pending_feedback` | `session_id` | Get unresolved comments and CI status |
| `add_comment` | `session_id`, `text`, `author` | Add a comment, or a reply with `parent_id` |
| `resolve_comment` | `session_id`, `comment_id`, `author` | Mark a comment thread as resolved |
| `unresolve_comment` | `session_id`, `comment_id`, `author` | Reopen a resolved comment thread |
| `submit_review` | `session_id` | Finish your review with an optional summary comment |

**GitHub and events**

| Tool | Required | Description |
|------|----------|-------------|
| `request_review` | `session_id` | Push the branch; for local sessions create a GitHub PR, for GitHub sessions push local commits (with `repo_path`) and return the PR |
| `list_github_prs` | `project` (`owner/repo`) | List GitHub PRs for a repo, annotated with differ session state |
| `wait_for_event` | `scope` | Block until new events appear for a scope (GitHub PR activity or session updates); see below |

**Kanban board** (see [Kanban Board](#kanban-board))

| Tool | Required | Description |
|------|----------|-------------|
| `create_task` | `repo_path`, `title` | Create a task (creates the repo's board if needed) |
| `list_tasks` | `repo_path` | List tasks, most urgent first; filters and field projection |
| `take_task` | `worker_name` | Claim a task by `task_id`, or auto-assign the next one from `repo_path` |
| `update_task` | `task_id` | Change status, fields, checklist, commits, dependencies; add a note |
| `release_task` | `task_id` | Release your claim without changing the status |
| `add_note` | `task_id`, `content` | Add a note to a task |
| `get_upstream` | `task_id` | Tasks the given task transitively depends on |
| `get_downstream` | `task_id` | Tasks that transitively depend on the given task |
| `add_board_column` | `repo_path`, `status` | Add a status column |
| `remove_board_column` | `repo_path`, `status` | Remove a status column (`move_to` moves its tasks) |
| `board_active` | `repo_path` | Compact "who is working on what" view |
| `board_stats` | `repo_path` | Throughput statistics over a trailing window |

**Google Docs** (see [Google Docs](#google-docs))

| Tool | Required | Description |
|------|----------|-------------|
| `gdocs_info` | `doc` | Show a document's title and tabs |
| `gdocs_read` | `doc` | Read a document, or one `tab`, as markdown |
| `gdocs_write` | `doc`, `tab` | Render markdown (`markdown` or `file`) into an existing tab |

**Misc**

| Tool | Required | Description |
|------|----------|-------------|
| `random_name` | – | Pick a random name (e.g. for a reviewer or worker) |

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

## Kanban Board

Each repo (keyed by its absolute path) can have a task board for coordinating several agents. It is created on first use by `create_task` or `add_board_column` and shown in the web UI under **Boards** (`/boards`), where cards can be dragged between columns.

- Default columns: `pending`, `needs_owner`, `planning`, `plan_review`, `ready`, `in_progress`, `testing`, `in_review`, `done`, `rejected`; `blocked` is computed from `blocked_by` dependencies.
- Agents claim work with `take_task`; changing a task's status releases the claim, so each phase is claimed separately. A claim lapses after `:claim-lease-hours` (default 4) without activity on the task.
- Tasks carry priority, tags, an assignee, a verification checklist, related commits, a parent task and notes.

## Google Docs

The `gdocs_*` tools read and write existing Google Docs as markdown using a Google Cloud service account. Put the service-account key at `resources/gcp-creds.json` (see `resources/gcp-creds.json.example`) or point `GDOCS_CREDS` at it, and share each document with the service account's e-mail address. The service account cannot create documents or tabs.

## REST API

The web UI uses a JSON API under `/api` that scripts can call as well (no authentication, see [Security notes](#security-notes)). Commonly useful:

| Endpoint | Purpose |
|----------|---------|
| `GET /api/sessions` | List sessions (`?project=` to filter) |
| `GET /api/sessions/:id` | Session details |
| `GET /api/sessions/:id/pending` | Unresolved comments (`?since=`) |
| `GET`, `POST /api/sessions/:id/comments` | List or add comments |
| `GET /api/boards` | List boards |
| `GET`, `POST /api/boards/:id/tasks` | List or create tasks (`:id` is the URL-encoded repo path) |
| `GET /api/boards/:id/stats` | Board throughput statistics |
| `GET`, `PATCH /api/tasks/:id` | Get or update a task |
| `POST /api/tasks/:id/notes` | Add a note to a task |
| `GET`, `POST /api/github/tokens` | List or add GitHub personal access tokens |
| `GET /events` | Server-sent events stream used by the UI |

The full list is in `src/differ/api.cljs`.

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

Then `/review` and `/fix-review` will work in any repo where the differ MCP server is configured under the name `differ`.

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
            "command": "python3 /path/to/differ/.claude/hooks/pr_review_loop.py",
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
definition has a `timeout` in seconds (43200 = 12h above; without it Claude Code's default
hook timeout applies) after which it will just continue. So the lower of these two values
limits how long the agent will wait for feedback. The script talks to differ's REST API at
`DIFFER_URL` (default `http://localhost:8576`).

If the PR is closed, and there are no local changes, the hook will ask the Claude session to
commit sepukku for doing such a good job.

## Configuration

Edit `resources/config.edn` to customize settings:

```edn
{:port 8576
 :base-url "http://localhost:8576"  ;; Used in links returned by MCP tools

 ;; File display thresholds (client)
 :large-file-threshold 50000      ;; Characters - files larger require explicit load
 :line-count-threshold 400        ;; Diff lines - more than this requires explicit expand
 :context-expand-size 15          ;; Lines to expand at a time

 ;; Watcher settings (server)
 :watcher-debounce-ms 300         ;; Debounce file change events

 ;; wait_for_event (see above)
 :event-stream {:buffer-size 10000}
 :github-poller {:poll-interval-ms 30000
                 :poller-grace-ms 300000}

 ;; Kanban: hours without activity after which a task claim lapses
 :kanban {:claim-lease-hours 4}

 ;; Push whitelist - controls which repos/branches can be pushed via request_review
 ;; Empty map = all repos/branches allowed
 ;; Example: {"owner/repo" ["feature/*" "fix/*"], "myorg/*" ["*"]}
 :push-whitelist {}}
```

### Push Whitelist

The pushes `request_review` makes when it creates a new PR can be restricted via a whitelist in `config.edn` (pushing further commits to an already open PR of a GitHub session is not checked):

```edn
{:push-whitelist {"owner/repo" ["feature/*" "fix/*"]
                  "myorg/*" ["*"]}}
```

- **Empty whitelist** (default): All repos/branches allowed
- **Repo keys**: Can use wildcards like `"owner/*"` to match any repo from that owner
- **Branch patterns**: Can use wildcards like `"feature/*"` to match branches
- `"*"` matches anything

The whitelist can also be set via the `PUSH_WHITELIST` environment variable (or `.env`) as JSON:

```bash
export PUSH_WHITELIST='{"owner/repo": ["main", "develop"]}'
```

Its entries are merged over those in `config.edn` (an entry for the same repo key replaces it). An unset or empty `PUSH_WHITELIST`, `{}`, or invalid JSON (logged as a warning) leaves the `config.edn` whitelist unchanged, so with the shipped config all repos and branches are allowed.

### GitHub access

GitHub PR sessions and `request_review` need a GitHub token. Either create a GitHub OAuth App with the callback URL `http://localhost:8576/oauth/github/callback` (the port is `:port` from `config.edn`) and set `GITHUB_CLIENT_ID` / `GITHUB_CLIENT_SECRET` (see `.env.example`), then sign in from the web UI; or add a personal access token in the web UI or with `scripts/setup.sh --github-pat <token>`.

### Environment variables

| Variable | Purpose |
|----------|---------|
| `PORT` | Port the server listens on (default: `:port` from `config.edn`, 8576) |
| `DIFFER_URL` | Base URL advertised in the MCP OAuth metadata (default `http://localhost:<:port>`); also the URL the review stop hook calls |
| `GITHUB_CLIENT_ID` / `GITHUB_CLIENT_SECRET` | GitHub OAuth App credentials |
| `DIFFER_AUTH_USERNAME` / `DIFFER_AUTH_PASSWORD` | Optional. When both are set, require this username/password on the OAuth login page before issuing an MCP token. When unset, all authorization requests are auto-approved. Gates the MCP OAuth flow only — the web UI and `/api/*` remain unauthenticated (see MCP Integration). |
| `PUSH_WHITELIST` | Push whitelist as JSON (see above) |
| `DIFFER_EVENT_BUFFER_SIZE`, `DIFFER_GITHUB_POLL_INTERVAL_MS`, `DIFFER_GITHUB_POLLER_GRACE_MS` | Override the `wait_for_event` settings |
| `DIFFER_CLAIM_LEASE_HOURS` | Override `:claim-lease-hours` |
| `GDOCS_CREDS` | Path to the Google service-account key (default `resources/gcp-creds.json`) |
| `XDG_DATA_HOME` | Data directory base; the SQLite database is `$XDG_DATA_HOME/differ/review.db` (default `~/.local/share/differ/review.db`) |

`GITHUB_*`, `DIFFER_AUTH_*` and `PUSH_WHITELIST` may also be set in a `.env` file in the project root, which takes precedence over the environment; the other variables are read from the environment only.

## Security notes

- The server listens on all network interfaces (no host is configured), not only on localhost.
- The web UI, the REST API under `/api` and the `/events` stream have no login. The optional `DIFFER_AUTH_*` login protects only the issuing of MCP tokens.
- The server runs `git` in the repositories it reviews and can push branches with the stored GitHub tokens. Anyone who can reach the API can open a session on any git repository the server's user can read (`POST /api/sessions` or `get_or_create_session`) and read its files.
- Web pages in your browser can reach it too. The API sends permissive CORS headers (`Access-Control-Allow-Origin: *`) and accepts form-encoded POSTs, so any website you visit while differ is running can read reviewed files and trigger actions such as comments, token changes and pushes. Binding, firewalling or an SSH tunnel does not prevent this.

Run it on a trusted machine and network, block the port from other hosts with a firewall if the machine is shared or reachable from untrusted networks, and use an SSH tunnel (`ssh -L 8576:localhost:8576 host`) for remote access rather than exposing the port. Stop it when you are not using it, since the browser risk above applies whenever it runs.
