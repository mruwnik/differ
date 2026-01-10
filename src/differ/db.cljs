(ns differ.db
  "SQLite database operations using better-sqlite3."
  (:require ["better-sqlite3" :as Database]
            ["path" :as path]
            ["os" :as os]
            ["fs" :as fs]
            [clojure.string :as str]
            [differ.util :as util]))

(defonce ^:private db-instance (atom nil))

(defn- in-clause
  "Generate placeholder string for SQL IN clause.
   (in-clause 3) => \"?,?,?\""
  [n]
  (str/join "," (repeat n "?")))

(defn- data-dir
  "Get XDG-compliant data directory."
  []
  (let [xdg (.-XDG_DATA_HOME js/process.env)
        base (or xdg (path/join (.homedir os) ".local" "share"))]
    (path/join base "differ")))

(defn- ensure-dir [dir]
  (when-not (fs/existsSync dir)
    (fs/mkdirSync dir #js {:recursive true})))

(defn- db-path []
  (let [dir (data-dir)]
    (ensure-dir dir)
    (path/join dir "review.db")))

(defn- migrate-comments-table [db]
  "Add new columns for better comment anchoring."
  ;; Check if columns exist by trying to select them
  (try
    (.exec db "SELECT side FROM comments LIMIT 1")
    (catch :default _
      ;; Columns don't exist, add them
      (.exec db "
        ALTER TABLE comments ADD COLUMN side TEXT DEFAULT 'new';
        ALTER TABLE comments ADD COLUMN line_content TEXT;
        ALTER TABLE comments ADD COLUMN context_before TEXT;
        ALTER TABLE comments ADD COLUMN context_after TEXT;
      "))))

(defn- migrate-sessions-table
  "Add columns for GitHub PR session support."
  [db]
  ;; Check if columns exist by trying to select them
  (try
    (.exec db "SELECT session_type FROM sessions LIMIT 1")
    (catch :default _
      ;; Columns don't exist, add them
      (.exec db "
        ALTER TABLE sessions ADD COLUMN session_type TEXT DEFAULT 'local';
        ALTER TABLE sessions ADD COLUMN github_owner TEXT;
        ALTER TABLE sessions ADD COLUMN github_repo TEXT;
        ALTER TABLE sessions ADD COLUMN github_pr_number INTEGER;
      "))))

(defn- create-tables [db]
  (.exec db "
    CREATE TABLE IF NOT EXISTS sessions (
      id TEXT PRIMARY KEY,
      project TEXT NOT NULL,
      branch TEXT NOT NULL,
      target_branch TEXT NOT NULL,
      repo_path TEXT NOT NULL,
      registered_files TEXT DEFAULT '{}',
      manual_additions TEXT DEFAULT '[]',
      manual_removals TEXT DEFAULT '[]',
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    );

    CREATE INDEX IF NOT EXISTS idx_sessions_project ON sessions(project);
    CREATE INDEX IF NOT EXISTS idx_sessions_branch ON sessions(project, branch);

    CREATE TABLE IF NOT EXISTS comments (
      id TEXT PRIMARY KEY,
      session_id TEXT NOT NULL,
      parent_id TEXT,
      file TEXT,
      line INTEGER,
      line_content_hash TEXT,
      text TEXT NOT NULL,
      author TEXT NOT NULL,
      resolved INTEGER DEFAULT 0,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE,
      FOREIGN KEY (parent_id) REFERENCES comments(id) ON DELETE CASCADE
    );

    CREATE INDEX IF NOT EXISTS idx_comments_session ON comments(session_id);
    CREATE INDEX IF NOT EXISTS idx_comments_file ON comments(session_id, file);
    CREATE INDEX IF NOT EXISTS idx_comments_parent ON comments(parent_id);
    CREATE INDEX IF NOT EXISTS idx_comments_resolved ON comments(session_id, resolved);

    -- OAuth tables
    CREATE TABLE IF NOT EXISTS users (
      id TEXT PRIMARY KEY,
      email TEXT UNIQUE NOT NULL,
      name TEXT,
      api_key TEXT UNIQUE,
      created_at TEXT NOT NULL
    );

    CREATE INDEX IF NOT EXISTS idx_users_email ON users(email);
    CREATE INDEX IF NOT EXISTS idx_users_api_key ON users(api_key);

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

    CREATE TABLE IF NOT EXISTS oauth_state (
      id TEXT PRIMARY KEY,
      state TEXT UNIQUE NOT NULL,
      client_id TEXT NOT NULL,
      redirect_uri TEXT NOT NULL,
      redirect_uri_provided_explicitly INTEGER DEFAULT 0,
      code_challenge TEXT,
      scopes TEXT DEFAULT '[]',
      code TEXT UNIQUE,
      user_id TEXT,
      stale INTEGER DEFAULT 0,
      expires_at TEXT NOT NULL,
      created_at TEXT NOT NULL,
      FOREIGN KEY (client_id) REFERENCES oauth_clients(client_id),
      FOREIGN KEY (user_id) REFERENCES users(id)
    );

    CREATE INDEX IF NOT EXISTS idx_oauth_state_state ON oauth_state(state);
    CREATE INDEX IF NOT EXISTS idx_oauth_state_code ON oauth_state(code);

    CREATE TABLE IF NOT EXISTS oauth_access_tokens (
      id TEXT PRIMARY KEY,
      user_id TEXT NOT NULL,
      oauth_state_id TEXT,
      expires_at TEXT NOT NULL,
      created_at TEXT NOT NULL,
      FOREIGN KEY (user_id) REFERENCES users(id),
      FOREIGN KEY (oauth_state_id) REFERENCES oauth_state(id)
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

    -- GitHub tokens for PR review
    CREATE TABLE IF NOT EXISTS github_tokens (
      id TEXT PRIMARY KEY,
      github_user_id TEXT NOT NULL,
      github_username TEXT NOT NULL,
      access_token TEXT NOT NULL,
      refresh_token TEXT,
      scope TEXT,
      expires_at TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    );

    CREATE UNIQUE INDEX IF NOT EXISTS idx_github_tokens_user ON github_tokens(github_user_id);
    CREATE INDEX IF NOT EXISTS idx_github_tokens_username ON github_tokens(github_username);

    -- Temporary state for GitHub OAuth flow (CSRF protection)
    CREATE TABLE IF NOT EXISTS github_oauth_states (
      state TEXT PRIMARY KEY,
      return_to TEXT,
      expires_at TEXT NOT NULL,
      created_at TEXT NOT NULL
    );
  "))

(defn init!
  "Initialize database connection."
  []
  (when-not @db-instance
    (let [db (Database (db-path))]
      (.pragma db "journal_mode = WAL")
      (create-tables db)
      (migrate-comments-table db)
      (migrate-sessions-table db)
      (reset! db-instance db)))
  @db-instance)

(defn close!
  "Close database connection."
  []
  (when-let [db @db-instance]
    (.close db)
    (reset! db-instance nil)))

(defn ^js db
  "Get database instance, initializing if needed."
  []
  (or @db-instance (init!)))

;; Session operations

(defn- row->session [^js row]
  (when row
    (cond-> {:id (.-id row)
             :project (.-project row)
             :branch (.-branch row)
             :target-branch (.-target_branch row)
             :repo-path (.-repo_path row)
             :session-type (or (.-session_type row) "local")
             :registered-files (js->clj (js/JSON.parse (.-registered_files row)))
             :manual-additions (js->clj (js/JSON.parse (.-manual_additions row)))
             :manual-removals (js->clj (js/JSON.parse (.-manual_removals row)))
             :created-at (.-created_at row)
             :updated-at (.-updated_at row)}
      ;; GitHub-specific fields (only when present)
      (.-github_owner row) (assoc :github-owner (.-github_owner row))
      (.-github_repo row) (assoc :github-repo (.-github_repo row))
      (.-github_pr_number row) (assoc :github-pr-number (.-github_pr_number row)))))

(defn get-session
  "Get session by ID."
  [session-id]
  (let [^js stmt (.prepare (db) "SELECT * FROM sessions WHERE id = ?")]
    (row->session (.get stmt session-id))))

(defn get-session-by-project-branch
  "Get session by project and branch."
  [project branch]
  (let [^js stmt (.prepare (db) "SELECT * FROM sessions WHERE project = ? AND branch = ?")]
    (row->session (.get stmt project branch))))

(defn list-sessions
  "List all sessions, optionally filtered by project."
  ([] (list-sessions nil))
  ([project]
   (let [^js stmt (if project
                    (.prepare (db) "SELECT * FROM sessions WHERE project = ? ORDER BY updated_at DESC")
                    (.prepare (db) "SELECT * FROM sessions ORDER BY updated_at DESC"))
         rows (if project (.all stmt project) (.all stmt))]
     (mapv row->session rows))))

(defn create-session!
  "Create a new session."
  [{:keys [id project branch target-branch repo-path session-type
           github-owner github-repo github-pr-number]}]
  (let [now (util/now-iso)
        session-type (or session-type "local")
        ^js stmt (.prepare (db)
                           "INSERT INTO sessions (id, project, branch, target_branch, repo_path,
                            session_type, github_owner, github_repo, github_pr_number,
                            created_at, updated_at)
                            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")]
    (.run stmt id project branch target-branch repo-path
          session-type github-owner github-repo github-pr-number
          now now)
    (get-session id)))

(defn update-session!
  "Update session fields."
  [session-id updates]
  (let [session (get-session session-id)
        now (util/now-iso)
        new-data (merge session updates {:updated-at now})
        ^js stmt (.prepare (db)
                           "UPDATE sessions SET
                project = ?,
                target_branch = ?,
                repo_path = ?,
                registered_files = ?,
                manual_additions = ?,
                manual_removals = ?,
                updated_at = ?
                WHERE id = ?")]
    (.run stmt
          (:project new-data)
          (:target-branch new-data)
          (:repo-path new-data)
          (js/JSON.stringify (clj->js (:registered-files new-data)))
          (js/JSON.stringify (clj->js (:manual-additions new-data)))
          (js/JSON.stringify (clj->js (:manual-removals new-data)))
          now
          session-id)
    (get-session session-id)))

(defn delete-session!
  "Delete a session and all its comments."
  [session-id]
  (let [^js stmt (.prepare (db) "DELETE FROM sessions WHERE id = ?")]
    (.run stmt session-id)))

;; Comment operations

(defn- row->comment [^js row]
  (when row
    {:id (.-id row)
     :session-id (.-session_id row)
     :parent-id (.-parent_id row)
     :file (.-file row)
     :line (.-line row)
     :line-content-hash (.-line_content_hash row)
     :side (or (.-side row) "new")
     :line-content (.-line_content row)
     :context-before (.-context_before row)
     :context-after (.-context_after row)
     :text (.-text row)
     :author (.-author row)
     :resolved (= 1 (.-resolved row))
     :created-at (.-created_at row)
     :updated-at (.-updated_at row)}))

(defn get-comment
  "Get comment by ID."
  [comment-id]
  (let [^js stmt (.prepare (db) "SELECT * FROM comments WHERE id = ?")]
    (row->comment (.get stmt comment-id))))

(defn list-comments
  "List all comments for a session."
  [session-id]
  (let [^js stmt (.prepare (db)
                           "SELECT * FROM comments WHERE session_id = ? ORDER BY created_at ASC")]
    (mapv row->comment (.all stmt session-id))))

(defn list-comments-for-file
  "List all comments for a specific file in a session."
  [session-id file]
  (let [^js stmt (.prepare (db)
                           "SELECT * FROM comments WHERE session_id = ? AND file = ? ORDER BY line ASC, created_at ASC")]
    (mapv row->comment (.all stmt session-id file))))

(defn list-unresolved-comments
  "List unresolved comments for a session, optionally since a timestamp."
  ([session-id] (list-unresolved-comments session-id nil))
  ([session-id since]
   (let [^js stmt (if since
                    (.prepare (db)
                              "SELECT * FROM comments WHERE session_id = ? AND resolved = 0 AND updated_at > ?
                   ORDER BY created_at ASC")
                    (.prepare (db)
                              "SELECT * FROM comments WHERE session_id = ? AND resolved = 0
                   ORDER BY created_at ASC"))
         rows (if since
                (.all stmt session-id since)
                (.all stmt session-id))]
     (mapv row->comment rows))))

(defn count-unresolved-comments
  "Count unresolved top-level comments for a session.
   Optionally filter to only count comments on specific files."
  ([session-id]
   (let [^js stmt (.prepare (db)
                            "SELECT COUNT(*) as count FROM comments
                 WHERE session_id = ? AND resolved = 0 AND parent_id IS NULL")]
     (.-count (.get stmt session-id))))
  ([session-id files]
   (if (empty? files)
     0
     (let [query (str "SELECT COUNT(*) as count FROM comments
                       WHERE session_id = ? AND resolved = 0 AND parent_id IS NULL
                       AND file IN (" (in-clause (count files)) ")")
           ^js stmt (.prepare (db) query)
           params (into-array (cons session-id files))
           ;; Use .bind to safely bind parameters, then .get
           ^js bound (.apply (.-bind stmt) stmt params)]
       (.-count (.get bound))))))

(defn create-comment!
  "Create a new comment."
  [{:keys [session-id parent-id file line line-content-hash side line-content context-before context-after text author]}]
  (let [id (util/gen-uuid)
        now (util/now-iso)
        ^js stmt (.prepare (db)
                           "INSERT INTO comments (id, session_id, parent_id, file, line, line_content_hash, side, line_content, context_before, context_after, text, author, created_at, updated_at)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")]
    (.run stmt id session-id parent-id file line line-content-hash (or side "new") line-content context-before context-after text author now now)
    ;; Update session timestamp
    (let [^js update-stmt (.prepare (db) "UPDATE sessions SET updated_at = ? WHERE id = ?")]
      (.run update-stmt now session-id))
    (get-comment id)))

(defn update-comment!
  "Update comment fields (text or resolved status)."
  [comment-id updates]
  (let [comment (get-comment comment-id)
        now (util/now-iso)
        new-text (or (:text updates) (:text comment))
        new-resolved (if (contains? updates :resolved)
                       (if (:resolved updates) 1 0)
                       (if (:resolved comment) 1 0))
        ^js stmt (.prepare (db)
                           "UPDATE comments SET text = ?, resolved = ?, updated_at = ? WHERE id = ?")]
    (.run stmt new-text new-resolved now comment-id)
    ;; Update session timestamp
    (let [^js update-stmt (.prepare (db) "UPDATE sessions SET updated_at = ? WHERE id = ?")]
      (.run update-stmt now (:session-id comment)))
    (get-comment comment-id)))

(defn resolve-comment!
  "Mark a comment as resolved."
  [comment-id]
  (update-comment! comment-id {:resolved true}))

(defn unresolve-comment!
  "Mark a comment as unresolved."
  [comment-id]
  (update-comment! comment-id {:resolved false}))

(defn delete-comment!
  "Delete a comment and all its replies."
  [comment-id]
  (let [comment (get-comment comment-id)
        ^js stmt (.prepare (db) "DELETE FROM comments WHERE id = ?")]
    (.run stmt comment-id)
    ;; Update session timestamp
    (when comment
      (let [now (util/now-iso)
            ^js update-stmt (.prepare (db) "UPDATE sessions SET updated_at = ? WHERE id = ?")]
        (.run update-stmt now (:session-id comment))))))

;; User operations

(defn- row->user [^js row]
  (when row
    {:id (.-id row)
     :email (.-email row)
     :name (.-name row)
     :api-key (.-api_key row)
     :created-at (.-created_at row)}))

(defn get-user [user-id]
  (let [^js stmt (.prepare (db) "SELECT * FROM users WHERE id = ?")]
    (row->user (.get stmt user-id))))

(defn get-user-by-email [email]
  (let [^js stmt (.prepare (db) "SELECT * FROM users WHERE email = ?")]
    (row->user (.get stmt email))))

(defn get-user-by-api-key [api-key]
  (let [^js stmt (.prepare (db) "SELECT * FROM users WHERE api_key = ?")]
    (row->user (.get stmt api-key))))

(defn create-user! [{:keys [email name api-key]}]
  (let [id (util/gen-uuid)
        now (util/now-iso)
        ^js stmt (.prepare (db)
                           "INSERT INTO users (id, email, name, api_key, created_at)
                            VALUES (?, ?, ?, ?, ?)")]
    (.run stmt id email name api-key now)
    (get-user id)))

(defn get-or-create-user! [email name]
  (or (get-user-by-email email)
      (create-user! {:email email :name name})))

;; OAuth Client operations

(defn- row->oauth-client [^js row]
  (when row
    {:client-id (.-client_id row)
     :client-secret (.-client_secret row)
     :client-name (.-client_name row)
     :redirect-uris (js->clj (js/JSON.parse (or (.-redirect_uris row) "[]")))
     :scope (.-scope row)
     :client-uri (.-client_uri row)
     :logo-uri (.-logo_uri row)
     :created-at (.-created_at row)}))

(defn get-oauth-client [client-id]
  (let [^js stmt (.prepare (db) "SELECT * FROM oauth_clients WHERE client_id = ?")]
    (row->oauth-client (.get stmt client-id))))

(defn create-oauth-client! [{:keys [client-id client-secret client-name redirect-uris scope client-uri logo-uri]}]
  (let [now (util/now-iso)
        ^js stmt (.prepare (db)
                           "INSERT OR REPLACE INTO oauth_clients
                            (client_id, client_secret, client_name, redirect_uris, scope, client_uri, logo_uri, created_at)
                            VALUES (?, ?, ?, ?, ?, ?, ?, ?)")]
    (.run stmt client-id client-secret client-name
          (js/JSON.stringify (clj->js (or redirect-uris [])))
          (or scope "read") client-uri logo-uri now)
    (get-oauth-client client-id)))

;; OAuth State operations

(defn- row->oauth-state [^js row]
  (when row
    {:id (.-id row)
     :state (.-state row)
     :client-id (.-client_id row)
     :redirect-uri (.-redirect_uri row)
     :redirect-uri-provided-explicitly (= 1 (.-redirect_uri_provided_explicitly row))
     :code-challenge (.-code_challenge row)
     :scopes (js->clj (js/JSON.parse (or (.-scopes row) "[]")))
     :code (.-code row)
     :user-id (.-user_id row)
     :stale (= 1 (.-stale row))
     :expires-at (.-expires_at row)
     :created-at (.-created_at row)}))

(defn get-oauth-state [id]
  (let [^js stmt (.prepare (db) "SELECT * FROM oauth_state WHERE id = ?")]
    (row->oauth-state (.get stmt id))))

(defn get-oauth-state-by-state [state]
  (let [^js stmt (.prepare (db) "SELECT * FROM oauth_state WHERE state = ?")]
    (row->oauth-state (.get stmt state))))

(defn get-oauth-state-by-code [code]
  (let [^js stmt (.prepare (db) "SELECT * FROM oauth_state WHERE code = ?")]
    (row->oauth-state (.get stmt code))))

(defn create-oauth-state! [{:keys [state client-id redirect-uri redirect-uri-provided-explicitly
                                   code-challenge scopes expires-at]}]
  (let [id (util/gen-uuid)
        now (util/now-iso)
        ^js stmt (.prepare (db)
                           "INSERT INTO oauth_state
                            (id, state, client_id, redirect_uri, redirect_uri_provided_explicitly,
                             code_challenge, scopes, expires_at, created_at)
                            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)")]
    (.run stmt id state client-id redirect-uri
          (if redirect-uri-provided-explicitly 1 0)
          code-challenge
          (js/JSON.stringify (clj->js (or scopes [])))
          expires-at now)
    (get-oauth-state-by-state state)))

(defn update-oauth-state! [state updates]
  (let [oauth-state (get-oauth-state-by-state state)]
    (when oauth-state
      (let [^js stmt (.prepare (db)
                               "UPDATE oauth_state SET code = ?, user_id = ?, stale = ? WHERE state = ?")]
        (.run stmt
              (or (:code updates) (:code oauth-state))
              (or (:user-id updates) (:user-id oauth-state))
              (if (contains? updates :stale)
                (if (:stale updates) 1 0)
                (if (:stale oauth-state) 1 0))
              state)
        (get-oauth-state-by-state state)))))

;; OAuth Access Token operations

(defn- row->access-token [^js row]
  (when row
    {:id (.-id row)
     :user-id (.-user_id row)
     :oauth-state-id (.-oauth_state_id row)
     :expires-at (.-expires_at row)
     :created-at (.-created_at row)}))

(defn get-access-token [token-id]
  (let [^js stmt (.prepare (db) "SELECT * FROM oauth_access_tokens WHERE id = ?")]
    (row->access-token (.get stmt token-id))))

(defn create-access-token! [{:keys [user-id oauth-state-id expires-at]}]
  (let [id (util/gen-uuid)
        now (util/now-iso)
        ^js stmt (.prepare (db)
                           "INSERT INTO oauth_access_tokens (id, user_id, oauth_state_id, expires_at, created_at)
                            VALUES (?, ?, ?, ?, ?)")]
    (.run stmt id user-id oauth-state-id expires-at now)
    (get-access-token id)))

(defn delete-access-token! [token-id]
  (let [^js stmt (.prepare (db) "DELETE FROM oauth_access_tokens WHERE id = ?")]
    (.run stmt token-id)))

;; OAuth Refresh Token operations

(defn- row->refresh-token [^js row]
  (when row
    {:id (.-id row)
     :token (.-token row)
     :client-id (.-client_id row)
     :user-id (.-user_id row)
     :scopes (js->clj (js/JSON.parse (or (.-scopes row) "[]")))
     :access-token-id (.-access_token_id row)
     :revoked (= 1 (.-revoked row))
     :expires-at (.-expires_at row)
     :created-at (.-created_at row)}))

(defn get-refresh-token [token]
  (let [^js stmt (.prepare (db) "SELECT * FROM oauth_refresh_tokens WHERE token = ?")]
    (row->refresh-token (.get stmt token))))

(defn get-valid-refresh-token [token client-id]
  (let [^js stmt (.prepare (db)
                           "SELECT * FROM oauth_refresh_tokens
                            WHERE token = ? AND client_id = ? AND revoked = 0")]
    (row->refresh-token (.get stmt token client-id))))

(defn create-refresh-token! [{:keys [token client-id user-id scopes access-token-id expires-at]}]
  (let [id (util/gen-uuid)
        now (util/now-iso)
        ^js stmt (.prepare (db)
                           "INSERT INTO oauth_refresh_tokens
                            (id, token, client_id, user_id, scopes, access_token_id, expires_at, created_at)
                            VALUES (?, ?, ?, ?, ?, ?, ?, ?)")]
    (.run stmt id token client-id user-id
          (js/JSON.stringify (clj->js (or scopes [])))
          access-token-id expires-at now)
    (get-refresh-token token)))

(defn revoke-refresh-token! [token]
  (let [^js stmt (.prepare (db) "UPDATE oauth_refresh_tokens SET revoked = 1 WHERE token = ?")]
    (.run stmt token)))

;; GitHub Token operations

(defn- row->github-token [^js row]
  (when row
    {:id (.-id row)
     :github-user-id (.-github_user_id row)
     :github-username (.-github_username row)
     :access-token (.-access_token row)
     :refresh-token (.-refresh_token row)
     :scope (.-scope row)
     :expires-at (.-expires_at row)
     :created-at (.-created_at row)
     :updated-at (.-updated_at row)}))

(defn get-github-token
  "Get GitHub token by github user ID."
  [github-user-id]
  (let [^js stmt (.prepare (db) "SELECT * FROM github_tokens WHERE github_user_id = ?")]
    (row->github-token (.get stmt github-user-id))))

(defn get-any-github-token
  "Get any stored GitHub token (for single-user scenarios)."
  []
  (let [^js stmt (.prepare (db) "SELECT * FROM github_tokens ORDER BY updated_at DESC LIMIT 1")]
    (row->github-token (.get stmt))))

(defn list-github-tokens
  "List all stored GitHub tokens (without actual token values for security)."
  []
  (let [^js stmt (.prepare (db)
                           "SELECT id, github_user_id, github_username, scope, expires_at, created_at, updated_at
                            FROM github_tokens ORDER BY updated_at DESC")]
    (->> (.all stmt)
         (mapv (fn [^js row]
                 {:id (.-id row)
                  :github-user-id (.-github_user_id row)
                  :github-username (.-github_username row)
                  :scope (.-scope row)
                  :expires-at (.-expires_at row)
                  :created-at (.-created_at row)
                  :updated-at (.-updated_at row)})))))

(defn create-github-token!
  "Create or update a GitHub token."
  [{:keys [github-user-id github-username access-token refresh-token scope expires-at]}]
  (let [id (util/gen-uuid)
        now (util/now-iso)
        ;; Check if token exists for this user
        existing (get-github-token github-user-id)]
    (if existing
      ;; Update existing
      (let [^js stmt (.prepare (db)
                               "UPDATE github_tokens SET
                                access_token = ?, refresh_token = ?, scope = ?, expires_at = ?, updated_at = ?
                                WHERE github_user_id = ?")]
        (.run stmt access-token refresh-token scope expires-at now github-user-id)
        (get-github-token github-user-id))
      ;; Insert new
      (let [^js stmt (.prepare (db)
                               "INSERT INTO github_tokens
                                (id, github_user_id, github_username, access_token, refresh_token, scope, expires_at, created_at, updated_at)
                                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)")]
        (.run stmt id github-user-id github-username access-token refresh-token scope expires-at now now)
        (get-github-token github-user-id)))))

(defn delete-github-token!
  "Delete a GitHub token by ID."
  [token-id]
  (let [^js stmt (.prepare (db) "DELETE FROM github_tokens WHERE id = ?")]
    (.run stmt token-id)))

;; GitHub OAuth State (temporary, for CSRF protection during OAuth flow)

(defn create-github-oauth-state!
  "Create a temporary OAuth state for GitHub auth flow."
  [{:keys [state return-to expires-at]}]
  (let [now (util/now-iso)
        ^js stmt (.prepare (db)
                           "INSERT INTO github_oauth_states (state, return_to, expires_at, created_at)
                            VALUES (?, ?, ?, ?)")]
    (.run stmt state return-to expires-at now)
    {:state state :return-to return-to}))

(defn get-github-oauth-state
  "Get GitHub OAuth state by state token."
  [state]
  (let [^js stmt (.prepare (db) "SELECT * FROM github_oauth_states WHERE state = ?")]
    (when-let [^js row (.get stmt state)]
      {:state (.-state row)
       :return-to (.-return_to row)
       :expires-at (.-expires_at row)
       :created-at (.-created_at row)})))

(defn delete-github-oauth-state!
  "Delete a GitHub OAuth state (after use or expiry)."
  [state]
  (let [^js stmt (.prepare (db) "DELETE FROM github_oauth_states WHERE state = ?")]
    (.run stmt state)))

(defn cleanup-expired-github-oauth-states!
  "Remove expired GitHub OAuth states."
  []
  (let [now (util/now-iso)
        ^js stmt (.prepare (db) "DELETE FROM github_oauth_states WHERE expires_at < ?")]
    (.run stmt now)))
