#!/usr/bin/env bash
#
# Setup script for differ MCP server
# Creates a user with an API key for Claude Code authentication
# Optionally stores a GitHub PAT for PR reviews
#
# Usage:
#   ./scripts/setup.sh                           # Generate new API key
#   ./scripts/setup.sh --api-key "my-key"        # Use specific API key
#   ./scripts/setup.sh --github-pat "ghp_xxx"   # Also add GitHub PAT
#   ./scripts/setup.sh --github-pat "ghp_xxx" --pat-name "my-token"

set -euo pipefail

# Default values
API_KEY=""
GITHUB_PAT=""
PAT_NAME="default"
USER_EMAIL="claude@local"
USER_NAME="Claude Code"

# XDG-compliant data directory
DATA_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/differ"
DB_PATH="$DATA_DIR/review.db"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --api-key)
            API_KEY="$2"
            shift 2
            ;;
        --github-pat)
            GITHUB_PAT="$2"
            shift 2
            ;;
        --pat-name)
            PAT_NAME="$2"
            shift 2
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --api-key KEY      Use specific API key (default: generate random)"
            echo "  --github-pat PAT   Store a GitHub Personal Access Token"
            echo "  --pat-name NAME    Name for the GitHub PAT (default: 'default')"
            echo "  --help, -h         Show this help"
            echo ""
            echo "Examples:"
            echo "  $0                                    # Generate new API key"
            echo "  $0 --api-key 'sk-my-secret-key'       # Use specific key"
            echo "  $0 --github-pat 'ghp_xxxx'            # Also add GitHub token"
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            exit 1
            ;;
    esac
done

# Generate API key if not provided
if [[ -z "$API_KEY" ]]; then
    API_KEY="dk_$(openssl rand -hex 24)"
fi

# Ensure data directory exists
mkdir -p "$DATA_DIR"

# Check if sqlite3 is available
if ! command -v sqlite3 &> /dev/null; then
    echo "Error: sqlite3 is required but not installed" >&2
    exit 1
fi

# Create tables if they don't exist (minimal schema for setup)
sqlite3 "$DB_PATH" <<'SQL'
CREATE TABLE IF NOT EXISTS users (
    id TEXT PRIMARY KEY,
    email TEXT UNIQUE NOT NULL,
    name TEXT,
    api_key TEXT UNIQUE,
    created_at TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_users_email ON users(email);
CREATE INDEX IF NOT EXISTS idx_users_api_key ON users(api_key);

CREATE TABLE IF NOT EXISTS github_tokens (
    id TEXT PRIMARY KEY,
    github_user_id TEXT NOT NULL,
    github_username TEXT NOT NULL,
    access_token TEXT NOT NULL,
    refresh_token TEXT,
    scope TEXT,
    expires_at TEXT,
    created_at TEXT NOT NULL,
    updated_at TEXT NOT NULL,
    token_type TEXT DEFAULT 'oauth',
    name TEXT
);
CREATE UNIQUE INDEX IF NOT EXISTS idx_github_tokens_user ON github_tokens(github_user_id);
CREATE INDEX IF NOT EXISTS idx_github_tokens_username ON github_tokens(github_username);
CREATE INDEX IF NOT EXISTS idx_github_tokens_type ON github_tokens(token_type);
SQL

# Generate UUIDs (portable)
gen_uuid() {
    if command -v uuidgen &> /dev/null; then
        uuidgen | tr '[:upper:]' '[:lower:]'
    else
        # Fallback using /dev/urandom
        od -x /dev/urandom | head -1 | awk '{print $2$3"-"$4"-"$5"-"$6"-"$7$8$9}'
    fi
}

NOW=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
USER_ID=$(gen_uuid)

# Insert or update user with API key
sqlite3 "$DB_PATH" <<SQL
INSERT INTO users (id, email, name, api_key, created_at)
VALUES ('$USER_ID', '$USER_EMAIL', '$USER_NAME', '$API_KEY', '$NOW')
ON CONFLICT(email) DO UPDATE SET
    api_key = '$API_KEY',
    name = '$USER_NAME';
SQL

echo "API key configured successfully!"
echo ""
echo "API Key: $API_KEY"
echo ""

# Add GitHub PAT if provided
if [[ -n "$GITHUB_PAT" ]]; then
    # Validate the token by calling GitHub API
    echo "Validating GitHub token..."

    GITHUB_RESPONSE=$(curl -s -H "Authorization: Bearer $GITHUB_PAT" \
        -H "Accept: application/vnd.github.v3+json" \
        https://api.github.com/user 2>&1)

    GITHUB_USERNAME=$(echo "$GITHUB_RESPONSE" | grep -o '"login": *"[^"]*"' | head -1 | cut -d'"' -f4)

    if [[ -z "$GITHUB_USERNAME" ]]; then
        echo "Warning: Could not validate GitHub token. Response:" >&2
        echo "$GITHUB_RESPONSE" >&2
        echo ""
        echo "Storing token anyway..."
        GITHUB_USERNAME="unknown"
    else
        echo "GitHub user: $GITHUB_USERNAME"
    fi

    PAT_ID=$(gen_uuid)

    sqlite3 "$DB_PATH" <<SQL
INSERT INTO github_tokens (id, github_user_id, github_username, access_token, scope, token_type, name, created_at, updated_at)
VALUES ('$PAT_ID', '$PAT_ID', '$GITHUB_USERNAME', '$GITHUB_PAT', 'repo', 'pat', '$PAT_NAME', '$NOW', '$NOW');
SQL

    echo "GitHub PAT stored successfully!"
    echo ""
fi

echo "To configure Claude Code, run:"
echo ""
echo "  claude mcp add --transport http differ-review <YOUR_SERVER_URL>/mcp \\"
echo "    --header \"Authorization: Bearer $API_KEY\""
echo ""
echo "Or add to .mcp.json:"
echo ""
cat <<EOF
{
  "mcpServers": {
    "differ-review": {
      "type": "http",
      "url": "<YOUR_SERVER_URL>/mcp",
      "headers": {
        "Authorization": "Bearer $API_KEY"
      }
    }
  }
}
EOF
