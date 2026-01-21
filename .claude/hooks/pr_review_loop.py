#!/usr/bin/env python3
"""
StopHook for continuous PR review workflow.

When agent stops:
- No PR session → tell agent to commit changes and use request_review
- PR with unresolved comments → tell agent to address them
- PR idle → poll every 5 min, kill after 12h
- PR merged/closed → exit cleanly
"""

import hashlib
import json
import os
import subprocess
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path
from typing import Any, NoReturn

# =============================================================================
# Configuration
# =============================================================================

DIFFER_URL = os.environ.get("DIFFER_URL", "http://localhost:8576")
POLL_INTERVAL = 60  # 1 minute
MAX_IDLE = 43200  # 12 hours
MAX_REPEATED_BLOCKS = 3  # Give up if same block reason repeats this many times
BLOCK_HISTORY_FILE = Path("/tmp/pr_review_loop_block_history")

COMMIT_REMINDER = (
    "IMPORTANT: Before requesting review, you must `git add` and `git commit` all changes. "
    "Commits are automatically pushed to the PR when you use request_review."
)

# =============================================================================
# Hook control flow (all exit the process)
# =============================================================================


def get_reason_hash(reason: str) -> str:
    """Hash a block reason for comparison (ignores minor variations)."""
    return hashlib.md5(reason.encode()).hexdigest()[:16]


def check_repeated_block(reason: str) -> bool:
    """Check if this reason has been repeated too many times. Returns True if should allow."""
    reason_hash = get_reason_hash(reason)

    # Read existing history
    try:
        history = BLOCK_HISTORY_FILE.read_text().strip().split("\n")
    except FileNotFoundError:
        history = []

    # Add current hash and keep last N entries
    history.append(reason_hash)
    history = history[-MAX_REPEATED_BLOCKS:]
    BLOCK_HISTORY_FILE.write_text("\n".join(history))

    # Check if all recent entries are identical
    if len(history) >= MAX_REPEATED_BLOCKS and len(set(history)) == 1:
        BLOCK_HISTORY_FILE.unlink()
        print(
            f"Same block reason repeated {MAX_REPEATED_BLOCKS} times. Giving up.",
            file=sys.stderr,
        )
        return True
    return False


def block(reason: str) -> NoReturn:
    """Output block decision and exit. Agent will continue with given instructions."""
    if check_repeated_block(reason):
        allow()
    print(json.dumps({"decision": "block", "reason": reason}))
    sys.exit(0)


def allow() -> NoReturn:
    """Allow agent to stop (normal exit)."""
    sys.exit(0)


# =============================================================================
# Hook input
# =============================================================================


def read_hook_input() -> dict[str, Any]:
    """Read JSON input from stdin (provided by Claude Code)."""
    try:
        return json.loads(sys.stdin.read())
    except (json.JSONDecodeError, OSError):
        return {}


def is_disabled() -> bool:
    """Check if hook is disabled via PR_REVIEW_LOOP_DISABLED env var."""
    return os.environ.get("PR_REVIEW_LOOP_DISABLED", "").lower() in ("1", "true", "yes")


# =============================================================================
# Git helpers (pure - no side effects)
# =============================================================================


def find_repo_root(start: Path | None = None) -> Path:
    """Walk up from start (or cwd) to find git repo root."""
    current = (start or Path.cwd()).resolve()
    while current != current.parent:
        if (current / ".git").exists():
            return current
        current = current.parent
    return start or Path.cwd()


def is_git_repo(path: Path) -> bool:
    """Check if path is inside a git repository."""
    return (find_repo_root(path) / ".git").exists()


def get_current_branch(repo_path: Path) -> str:
    """Get current git branch name, or empty string on failure."""
    try:
        return subprocess.check_output(
            ["git", "rev-parse", "--abbrev-ref", "HEAD"],
            cwd=repo_path,
            stderr=subprocess.DEVNULL,
            text=True,
        ).strip()
    except (subprocess.CalledProcessError, FileNotFoundError):
        return ""


def get_main_branch(repo_path: Path) -> str:
    """Get the main/default branch name (usually 'main' or 'master')."""
    for branch in ["main", "master"]:
        try:
            subprocess.check_output(
                ["git", "rev-parse", "--verify", f"origin/{branch}"],
                cwd=repo_path,
                stderr=subprocess.DEVNULL,
            )
            return branch
        except subprocess.CalledProcessError:
            continue
    return "main"


def check_conflicts_with_main(repo_path: Path, branch: str, main_branch: str) -> list[str]:
    """Check if merging origin/main_branch would cause conflicts.

    Returns list of conflicting file paths, or empty list if no conflicts.
    """
    if branch == main_branch:
        return []

    try:
        # Fetch latest
        subprocess.run(
            ["git", "fetch", "origin", main_branch],
            cwd=repo_path,
            stderr=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL,
            timeout=30,
        )

        # Check if main is already merged in
        result = subprocess.run(
            ["git", "merge-base", "--is-ancestor", f"origin/{main_branch}", "HEAD"],
            cwd=repo_path,
        )
        if result.returncode == 0:
            return []  # Already up to date

        # Use merge-tree to detect conflicts (Git 2.38+)
        result = subprocess.run(
            ["git", "merge-tree", "--write-tree", "HEAD", f"origin/{main_branch}"],
            cwd=repo_path,
            capture_output=True,
            text=True,
        )

        if result.returncode == 0:
            return []  # No conflicts

        # Parse conflicting files from output
        # Format includes lines like "CONFLICT (content): Merge conflict in <path>"
        conflicts = []
        for line in result.stdout.split("\n"):
            if line.startswith("CONFLICT") and " in " in line:
                path = line.split(" in ", 1)[-1].strip()
                conflicts.append(path)

        return conflicts if conflicts else ["(unable to determine specific files)"]

    except (subprocess.CalledProcessError, subprocess.TimeoutExpired, FileNotFoundError):
        return []  # Can't check, assume no conflicts


def get_uncommitted_changes(repo_path: Path) -> str:
    """Get git status --porcelain output (empty if clean)."""
    try:
        return subprocess.check_output(
            ["git", "status", "--porcelain"],
            cwd=repo_path,
            stderr=subprocess.DEVNULL,
            text=True,
        ).strip()
    except (subprocess.CalledProcessError, FileNotFoundError):
        return ""


def get_unpushed_commit_count(repo_path: Path, branch: str) -> int:
    """Count local commits not yet on origin/{branch}."""
    try:
        remote_branch = f"origin/{branch}"
        subprocess.check_output(
            ["git", "rev-parse", "--verify", remote_branch],
            cwd=repo_path,
            stderr=subprocess.DEVNULL,
        )
        output = subprocess.check_output(
            ["git", "rev-list", "--count", f"{remote_branch}..HEAD"],
            cwd=repo_path,
            stderr=subprocess.DEVNULL,
            text=True,
        ).strip()
        return int(output)
    except (subprocess.CalledProcessError, FileNotFoundError, ValueError):
        return 0


def api_get(path: str) -> dict[str, Any]:
    """GET from differ REST API. Returns empty dict on any error."""
    try:
        with urllib.request.urlopen(f"{DIFFER_URL}{path}", timeout=30) as resp:
            return json.loads(resp.read().decode())
    except (urllib.error.URLError, json.JSONDecodeError, OSError):
        return {}


def find_session_for_branch(repo_path: str, branch: str) -> dict[str, Any]:
    """Find best session for repo/branch. Prefers open GitHub PRs over closed/merged."""
    sessions = api_get("/api/sessions")
    repo_name = Path(repo_path).name

    local_session = None
    github_sessions: list[dict[str, Any]] = []

    for s in sessions.get("sessions", []):
        s_branch = s.get("branch", "")
        s_project = s.get("project", "")
        s_type = s.get("session-type", "")

        matches_repo = (
            s.get("repo-path") == repo_path
            or repo_name in s_project
            or s_project.endswith(f"/{repo_name}.git")
        )

        if matches_repo and s_branch == branch:
            if s_type == "github":
                github_sessions.append(s)
            elif s_type == "local" and not local_session:
                local_session = s

    if github_sessions:
        # Prefer open PRs over merged/closed, then highest PR number
        def session_priority(s: dict[str, Any]) -> tuple[int, int]:
            state = s.get("state", "").lower()
            pr_num = s.get("github-pr-number", 0) or 0
            is_open = 1 if state == "open" else 0
            return (is_open, pr_num)

        github_sessions.sort(key=session_priority, reverse=True)
        return github_sessions[0]

    return local_session or {}


def get_pending_feedback(session_id: str) -> dict[str, Any]:
    """Get pending feedback: unresolved comments and CI status.

    Returns: {
        'comment_count': int,
        'ci_state': str | None,  # 'success', 'failure', 'pending', 'error', 'unknown'
        'ci_failures': list[dict]  # [{name, state, url}, ...]
    }
    """
    encoded_id = urllib.parse.quote(session_id, safe="")
    pending = api_get(f"/api/sessions/{encoded_id}/pending")

    comments = pending.get("comments", [])
    ci = pending.get("ci", {})
    ci_state = ci.get("state")
    checks = ci.get("checks", [])

    # Extract failed checks
    ci_failures = [
        {"name": c.get("name", "unknown"), "state": c.get("state"), "url": c.get("url")}
        for c in checks
        if c.get("state") in ("failure", "error")
    ]

    return {
        "comment_count": len(comments),
        "ci_state": ci_state,
        "ci_failures": ci_failures,
    }


def get_pr_status(session_id: str) -> str | None:
    """Get PR state: 'merged', 'closed', 'open', or None if unknown."""
    encoded_id = urllib.parse.quote(session_id, safe="")
    session = api_get(f"/api/sessions/{encoded_id}")
    if not session:
        return None
    return session.get("state", "").lower() or None


# =============================================================================
# Blocking helpers (may call block() and exit)
# =============================================================================


def block_for_pending_comments(
    pr_info: str, count: int, session_id: str, repo_path: str
) -> NoReturn:
    """Block with instructions to address review comments."""
    block(
        f"ACTION REQUIRED: Address {count} unresolved comment(s) on {pr_info}.\n"
        "1. Call get_pending_feedback to see the comments\n"
        "2. Address each comment by making the necessary code changes\n"
        f'3. Call: request_review(session_id="{session_id}", repo_path="{repo_path}")\n'
        "4. Call resolve_comment for each issue you've addressed\n"
        f"5. {COMMIT_REMINDER}"
    )


def block_for_ci_failures(
    pr_info: str, failures: list[dict[str, Any]], session_id: str, repo_path: str
) -> NoReturn:
    """Block with instructions to fix CI failures."""
    failure_list = "\n".join(
        f"  • **{f['name']}**: {f.get('url', 'no URL')}" for f in failures
    )
    block(
        f"ACTION REQUIRED: Fix {len(failures)} failing CI check(s) on {pr_info}.\n\n"
        f"{failure_list}\n\n"
        "1. Review the CI logs at the URLs above\n"
        "2. Fix the failing tests or checks\n"
        f"3. {COMMIT_REMINDER}\n"
        f'4. Call: request_review(session_id="{session_id}", repo_path="{repo_path}")'
    )


def block_for_merge_conflicts(
    main_branch: str, conflicts: list[str], session_id: str, repo_path: str
) -> NoReturn:
    """Block with instructions to resolve merge conflicts with main branch."""
    conflict_list = "\n".join(f"  • {f}" for f in conflicts[:10])
    if len(conflicts) > 10:
        conflict_list += f"\n  ... and {len(conflicts) - 10} more"

    block(
        f"ACTION REQUIRED: Resolve merge conflicts with {main_branch}.\n\n"
        f"Conflicting files:\n{conflict_list}\n\n"
        "Steps to resolve:\n"
        f"1. Run: `git fetch origin {main_branch}`\n"
        f"2. Run: `git merge origin/{main_branch}`\n"
        "3. Resolve conflicts in each file listed above\n"
        "4. Run: `git add <resolved-files>` for each resolved file\n"
        "5. Run: `git commit` to complete the merge\n"
        f"6. {COMMIT_REMINDER}\n"
        f'7. Call: request_review(session_id="{session_id}", repo_path="{repo_path}")'
    )


def require_clean_working_tree(
    repo_path: Path, branch: str, session_id: str, pr_info: str
) -> None:
    """Block if there are uncommitted changes or unpushed commits."""
    uncommitted = get_uncommitted_changes(repo_path)
    if uncommitted:
        change_lines = [line for line in uncommitted.split("\n") if line.strip()]
        block(
            f"ACTION REQUIRED: Handle {len(change_lines)} uncommitted change(s):\n"
            f"```\n{uncommitted}\n```\n"
            "For each file, decide:\n"
            "• **Commit** if it's intentional work (source code, config, docs)\n"
            "• **Add to .gitignore** if it's generated/cache files (__pycache__/, .pyc, "
            ".claude/, .lsp/, .clj-kondo/, node_modules/, .DS_Store, etc.)\n"
            "• **Discard** with `git checkout -- <file>` if it's accidental\n\n"
            "Then run `git add` and `git commit` for files that belong in the PR."
        )

    unpushed = get_unpushed_commit_count(repo_path, branch)
    if unpushed > 0:
        block(
            f"ACTION REQUIRED: Push {unpushed} commit(s) to {pr_info} now.\n"
            f'Call: request_review(session_id="{session_id}", repo_path="{repo_path}")\n\n'
            "This will create a new PR if the original was merged/closed."
        )


def require_github_session(repo_path: str, branch: str) -> dict[str, Any]:
    """Get session for branch, blocking if none exists or no PR created yet."""
    session = find_session_for_branch(repo_path, branch)

    if not session:
        print(f"No differ session found for branch '{branch}'", file=sys.stderr)
        block(
            f"ACTION REQUIRED: Create a review session for branch '{branch}'.\n"
            f"1. {COMMIT_REMINDER}\n"
            "2. Call get_or_create_session to create a review session\n"
            "3. Call get_session_diff to review your changes\n"
            "4. Run the code-reviewer agent to find issues, then fix them\n"
            "5. Call request_review to create a PR and request external review"
        )

    session_type = session.get("session-type", "local")
    pr_number = session.get("github-pr-number")

    if session_type == "github" and pr_number:
        print(f"Found PR #{pr_number} for branch '{branch}'", file=sys.stderr)
    elif session_type == "local":
        session_id = session.get("id", "")
        block(
            f"ACTION REQUIRED: Create a PR for branch '{branch}'.\n"
            f"{COMMIT_REMINDER}\n"
            f'Then call: request_review(session_id="{session_id}", repo_path="{repo_path}")'
        )

    return session


# =============================================================================
# Main polling loop
# =============================================================================


def poll_for_comments(session_id: str, pr_info: str, repo_path: str) -> None:
    """Poll for review comments and CI status. Blocks on issues, exits when PR closed/merged."""
    idle_start = time.time()

    while True:
        feedback = get_pending_feedback(session_id)

        # Check for unresolved comments first
        if feedback["comment_count"] > 0:
            block_for_pending_comments(
                pr_info, feedback["comment_count"], session_id, repo_path
            )

        # Check for CI failures
        if feedback["ci_failures"]:
            block_for_ci_failures(
                pr_info, feedback["ci_failures"], session_id, repo_path
            )

        pr_status = get_pr_status(session_id)
        if pr_status in ["merged", "closed"]:
            print(f"PR {pr_status}.", file=sys.stderr)
            block(
                f"ACTION REQUIRED: End this session - {pr_info} has been {pr_status}.\n\n"
                "Run: `kill -INT $PPID`"
            )

        if time.time() - idle_start >= MAX_IDLE:
            print("12h idle. Ending.", file=sys.stderr)
            sys.exit(1)

        time.sleep(POLL_INTERVAL)


# =============================================================================
# Entry point
# =============================================================================


def main() -> None:
    if is_disabled():
        allow()

    hook_input = read_hook_input()
    cwd = Path(hook_input.get("cwd", ".")).resolve()

    if not is_git_repo(cwd):
        print("Not in a git repository", file=sys.stderr)
        allow()

    repo_path = find_repo_root(cwd)
    branch = get_current_branch(repo_path)

    if not branch:
        print("Could not determine current branch", file=sys.stderr)
        allow()

    session = require_github_session(str(repo_path), branch)
    session_id = session.get("id", "")
    if not session_id:
        allow()

    pr_number = session.get("github-pr-number")
    pr_info = f"PR #{pr_number}" if pr_number else "the PR"

    # Check for merge conflicts with main branch
    main_branch = get_main_branch(repo_path)
    conflicts = check_conflicts_with_main(repo_path, branch, main_branch)
    if conflicts:
        block_for_merge_conflicts(main_branch, conflicts, session_id, str(repo_path))

    require_clean_working_tree(repo_path, branch, session_id, pr_info)
    poll_for_comments(session_id, pr_info, str(repo_path))


if __name__ == "__main__":
    main()
