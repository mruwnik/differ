#!/usr/bin/env bash
#
# Setup script for differ MCP server
# Imports OAuth credentials and/or GitHub PAT for authentication
#
# Usage:
#   ./scripts/setup.sh --oauth-file credentials.json
#   ./scripts/setup.sh --github-pat "ghp_xxx"
#   ./scripts/setup.sh --github-pat "ghp_xxx" --pat-name "my-token"

set -euo pipefail

# Default values
GITHUB_PAT=""
PAT_NAME="default"
USER_EMAIL="claude@local"
USER_NAME="Claude Code"

# OAuth credential migration (for moving credentials to a new server)
# JSON file should contain: {"clientId":"...","accessToken":"...","refreshToken":"...","expiresAt":123,"scope":"read"}
OAUTH_FILE=""

# XDG-compliant data directory
DATA_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/differ"
DB_PATH="$DATA_DIR/review.db"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --github-pat)
            GITHUB_PAT="$2"
            shift 2
            ;;
        --pat-name)
            PAT_NAME="$2"
            shift 2
            ;;
        --oauth-file)
            OAUTH_FILE="$2"
            shift 2
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --oauth-file FILE    Import OAuth credentials from JSON file"
            echo "  --github-pat PAT     Store a GitHub Personal Access Token"
            echo "  --pat-name NAME      Name for the GitHub PAT (default: 'default')"
            echo "  --help, -h           Show this help"
            echo ""
            echo "Examples:"
            echo "  $0 --oauth-file /path/to/credentials.json"
            echo "  $0 --github-pat 'ghp_xxxx'"
            echo "  $0 --github-pat 'ghp_xxxx' --pat-name 'my-token'"
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            exit 1
            ;;
    esac
done

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

-- OAuth tables for MCP authentication
CREATE TABLE IF NOT EXISTS oauth_clients (
    client_id TEXT PRIMARY KEY,
    client_secret TEXT,
    client_name TEXT,
    redirect_uris TEXT NOT NULL DEFAULT '[]',
    scope TEXT DEFAULT 'read',
    client_uri TEXT,
    logo_uri TEXT,
    created_at TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS oauth_access_tokens (
    id TEXT PRIMARY KEY,
    user_id TEXT NOT NULL,
    oauth_state_id TEXT,
    expires_at TEXT NOT NULL,
    created_at TEXT NOT NULL,
    FOREIGN KEY (user_id) REFERENCES users(id)
);
CREATE INDEX IF NOT EXISTS idx_oauth_access_tokens_user ON oauth_access_tokens(user_id);

CREATE TABLE IF NOT EXISTS oauth_refresh_tokens (
    id TEXT PRIMARY KEY,
    token TEXT UNIQUE NOT NULL,
    client_id TEXT NOT NULL,
    user_id TEXT NOT NULL,
    scopes TEXT DEFAULT '[]',
    access_token_id TEXT,
    revoked INTEGER DEFAULT 0,
    expires_at TEXT NOT NULL,
    created_at TEXT NOT NULL,
    FOREIGN KEY (client_id) REFERENCES oauth_clients(client_id),
    FOREIGN KEY (user_id) REFERENCES users(id),
    FOREIGN KEY (access_token_id) REFERENCES oauth_access_tokens(id)
);
CREATE INDEX IF NOT EXISTS idx_oauth_refresh_tokens_token ON oauth_refresh_tokens(token);
CREATE INDEX IF NOT EXISTS idx_oauth_refresh_tokens_client ON oauth_refresh_tokens(client_id);
SQL

# Generate UUIDs (portable)
gen_uuid() {
    if command -v uuidgen &> /dev/null; then
        uuidgen | tr '[:upper:]' '[:lower:]'
    else
        # Fallback: read exactly 16 bytes from /dev/urandom and format as UUID
        local hex
        hex=$(head -c 16 /dev/urandom | od -A n -t x1 | tr -d ' \n')
        echo "${hex:0:8}-${hex:8:4}-${hex:12:4}-${hex:16:4}-${hex:20:12}"
    fi
}

NOW=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
USER_ID=$(gen_uuid)

# Ensure a local user exists for OAuth tokens to reference
sqlite3 "$DB_PATH" <<SQL
INSERT INTO users (id, email, name, created_at)
VALUES ('$USER_ID', '$USER_EMAIL', '$USER_NAME', '$NOW')
ON CONFLICT(email) DO NOTHING;
SQL

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

# Import OAuth credentials if provided
if [[ -n "$OAUTH_FILE" ]]; then
    # Check if file exists
    if [[ ! -f "$OAUTH_FILE" ]]; then
        echo "Error: OAuth file not found: $OAUTH_FILE" >&2
        exit 1
    fi

    # Check if jq is available
    if ! command -v jq &> /dev/null; then
        echo "Error: jq is required for --oauth-file but not installed" >&2
        exit 1
    fi

    echo "Importing OAuth credentials from $OAUTH_FILE..."

    # Parse JSON fields from file
    OAUTH_CLIENT_ID=$(jq -r '.clientId // empty' "$OAUTH_FILE")
    OAUTH_ACCESS_TOKEN=$(jq -r '.accessToken // empty' "$OAUTH_FILE")
    OAUTH_REFRESH_TOKEN=$(jq -r '.refreshToken // empty' "$OAUTH_FILE")
    OAUTH_EXPIRES_AT=$(jq -r '.expiresAt // empty' "$OAUTH_FILE")
    OAUTH_SCOPE=$(jq -r '.scope // "read"' "$OAUTH_FILE")

    # Validate required fields
    if [[ -z "$OAUTH_CLIENT_ID" || -z "$OAUTH_ACCESS_TOKEN" ]]; then
        echo "Error: OAuth JSON must contain at least 'clientId' and 'accessToken'" >&2
        exit 1
    fi

    # Convert expiresAt from milliseconds to ISO date
    if [[ -n "$OAUTH_EXPIRES_AT" ]]; then
        # Convert milliseconds to seconds for date command
        EXPIRES_SECONDS=$((OAUTH_EXPIRES_AT / 1000))
        # macOS and GNU date have different syntax
        if date --version >/dev/null 2>&1; then
            # GNU date
            EXPIRES_ISO=$(date -u -d "@$EXPIRES_SECONDS" +"%Y-%m-%dT%H:%M:%SZ")
        else
            # macOS date
            EXPIRES_ISO=$(date -u -r "$EXPIRES_SECONDS" +"%Y-%m-%dT%H:%M:%SZ")
        fi
    else
        # Default to 30 days from now
        EXPIRES_ISO=$(date -u -v+30d +"%Y-%m-%dT%H:%M:%SZ" 2>/dev/null || date -u -d "+30 days" +"%Y-%m-%dT%H:%M:%SZ")
    fi

    # Get the user ID we created earlier
    OAUTH_USER_ID=$(sqlite3 "$DB_PATH" "SELECT id FROM users WHERE email = '$USER_EMAIL'")

    # Insert OAuth client (upsert)
    sqlite3 "$DB_PATH" <<SQL
INSERT INTO oauth_clients (client_id, client_name, redirect_uris, scope, created_at)
VALUES ('$OAUTH_CLIENT_ID', 'Imported MCP Client', '[]', '$OAUTH_SCOPE', '$NOW')
ON CONFLICT(client_id) DO UPDATE SET scope = '$OAUTH_SCOPE';
SQL

    # Insert access token (the token ID IS the access token)
    sqlite3 "$DB_PATH" <<SQL
INSERT INTO oauth_access_tokens (id, user_id, expires_at, created_at)
VALUES ('$OAUTH_ACCESS_TOKEN', '$OAUTH_USER_ID', '$EXPIRES_ISO', '$NOW')
ON CONFLICT(id) DO UPDATE SET expires_at = '$EXPIRES_ISO';
SQL

    # Insert refresh token if provided
    if [[ -n "$OAUTH_REFRESH_TOKEN" ]]; then
        REFRESH_TOKEN_ID=$(gen_uuid)
        SCOPES_JSON=$(echo "$OAUTH_SCOPE" | jq -R 'split(" ")')

        sqlite3 "$DB_PATH" <<SQL
INSERT INTO oauth_refresh_tokens (id, token, client_id, user_id, scopes, access_token_id, expires_at, created_at)
VALUES ('$REFRESH_TOKEN_ID', '$OAUTH_REFRESH_TOKEN', '$OAUTH_CLIENT_ID', '$OAUTH_USER_ID', '$SCOPES_JSON', '$OAUTH_ACCESS_TOKEN', '$EXPIRES_ISO', '$NOW')
ON CONFLICT(token) DO UPDATE SET
    client_id = '$OAUTH_CLIENT_ID',
    access_token_id = '$OAUTH_ACCESS_TOKEN',
    expires_at = '$EXPIRES_ISO';
SQL
        echo "Refresh token imported."
    fi

    echo "OAuth credentials imported successfully!"
    echo "  Client ID: $OAUTH_CLIENT_ID"
    echo "  Access Token: $OAUTH_ACCESS_TOKEN"
    echo "  Expires: $EXPIRES_ISO"
fi

echo ""
echo "Setup complete."
