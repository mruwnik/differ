(ns differ.test-helpers
  "Shared test utilities for fixtures, mocks, and helpers."
  (:require ["better-sqlite3" :as Database]
            ["child_process" :as cp]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [clojure.string :as str]))

;; ============================================================================
;; Temp Directory Utilities
;; ============================================================================

(defn create-temp-dir
  "Create a temporary directory and return its path."
  [prefix]
  (let [tmp-base (os/tmpdir)
        dir-path (path/join tmp-base (str prefix "-" (.now js/Date) "-" (rand-int 10000)))]
    (fs/mkdirSync dir-path #js {:recursive true})
    dir-path))

(defn remove-dir
  "Recursively remove a directory."
  [dir-path]
  (when (fs/existsSync dir-path)
    (fs/rmSync dir-path #js {:recursive true :force true})))

;; ============================================================================
;; Test Database Utilities
;; ============================================================================

(defonce ^:private test-db-atom (atom nil))
(defonce ^:private test-db-path-atom (atom nil))

(defn init-test-db!
  "Initialize a fresh test database. Returns the db instance."
  []
  (when-let [old-db @test-db-atom]
    (.close old-db))
  (when-let [old-path @test-db-path-atom]
    (remove-dir (path/dirname old-path)))
  (let [dir (create-temp-dir "differ-test-db")
        db-path (path/join dir "test.db")
        db (Database db-path)]
    (.pragma db "journal_mode = WAL")
    ;; Create tables (same as main db.cljs)
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
      CREATE TABLE IF NOT EXISTS comments (
        id TEXT PRIMARY KEY,
        session_id TEXT NOT NULL,
        parent_id TEXT,
        file TEXT NOT NULL,
        line INTEGER NOT NULL,
        line_content_hash TEXT NOT NULL,
        text TEXT NOT NULL,
        author TEXT NOT NULL,
        resolved INTEGER DEFAULT 0,
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL,
        FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE,
        FOREIGN KEY (parent_id) REFERENCES comments(id) ON DELETE CASCADE
      );
      CREATE TABLE IF NOT EXISTS users (
        id TEXT PRIMARY KEY,
        email TEXT UNIQUE NOT NULL,
        name TEXT,
        api_key TEXT UNIQUE,
        created_at TEXT NOT NULL
      );
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
        created_at TEXT NOT NULL
      );
      CREATE TABLE IF NOT EXISTS oauth_access_tokens (
        id TEXT PRIMARY KEY,
        user_id TEXT NOT NULL,
        oauth_state_id TEXT,
        expires_at TEXT NOT NULL,
        created_at TEXT NOT NULL
      );
      CREATE TABLE IF NOT EXISTS oauth_refresh_tokens (
        id TEXT PRIMARY KEY,
        token TEXT UNIQUE NOT NULL,
        client_id TEXT NOT NULL,
        user_id TEXT NOT NULL,
        scopes TEXT DEFAULT '[]',
        access_token_id TEXT,
        revoked INTEGER DEFAULT 0,
        expires_at TEXT NOT NULL,
        created_at TEXT NOT NULL
      );
    ")
    (reset! test-db-atom db)
    (reset! test-db-path-atom db-path)
    db))

(defn test-db
  "Get the current test database instance."
  []
  @test-db-atom)

(defn cleanup-test-db!
  "Close and remove the test database."
  []
  (when-let [db @test-db-atom]
    (.close db)
    (reset! test-db-atom nil))
  (when-let [db-path @test-db-path-atom]
    (remove-dir (path/dirname db-path))
    (reset! test-db-path-atom nil)))

(defn clear-test-db!
  "Clear all data from test database tables."
  []
  (when-let [db @test-db-atom]
    (.exec db "DELETE FROM oauth_refresh_tokens")
    (.exec db "DELETE FROM oauth_access_tokens")
    (.exec db "DELETE FROM oauth_state")
    (.exec db "DELETE FROM oauth_clients")
    (.exec db "DELETE FROM users")
    (.exec db "DELETE FROM comments")
    (.exec db "DELETE FROM sessions")))

;; ============================================================================
;; Git Fixture Utilities
;; ============================================================================

(defn- exec-sync [cmd opts]
  (try
    (str/trim (cp/execSync cmd (clj->js (merge {:encoding "utf8"} opts))))
    (catch :default _e nil)))

(defn create-test-repo
  "Create a temporary git repository with some initial content.
   Returns {:path <repo-path> :cleanup <fn>}."
  []
  (let [repo-path (create-temp-dir "differ-test-repo")]
    ;; Initialize git repo (use -q to suppress hints and -b main to set default branch)
    (exec-sync "git init -q -b main" {:cwd repo-path})
    (exec-sync "git config user.email 'test@test.com'" {:cwd repo-path})
    (exec-sync "git config user.name 'Test User'" {:cwd repo-path})

    ;; Create initial file and commit
    (fs/writeFileSync (path/join repo-path "README.md") "# Test Repo\n\nInitial content.\n")
    (exec-sync "git add ." {:cwd repo-path})
    (exec-sync "git commit -q -m 'Initial commit'" {:cwd repo-path})

    {:path repo-path
     :cleanup #(remove-dir repo-path)}))

(defn create-test-branch
  "Create a new branch in the test repo and make changes."
  [repo-path branch-name]
  (exec-sync (str "git checkout -b " branch-name) {:cwd repo-path}))

(defn add-test-file
  "Add a file to the test repo."
  [repo-path file-path content]
  (let [full-path (path/join repo-path file-path)
        dir (path/dirname full-path)]
    (fs/mkdirSync dir #js {:recursive true})
    (fs/writeFileSync full-path content)))

(defn commit-test-changes
  "Stage and commit all changes in the test repo."
  [repo-path message]
  (exec-sync "git add ." {:cwd repo-path})
  (exec-sync (str "git commit -m '" message "'") {:cwd repo-path}))

(defn modify-test-file
  "Modify an existing file in the test repo."
  [repo-path file-path content]
  (fs/writeFileSync (path/join repo-path file-path) content))

;; ============================================================================
;; Assertion Helpers
;; ============================================================================

(defn maps-match?
  "Check if two maps have the same values for specified keys."
  [expected actual keys]
  (every? #(= (get expected %) (get actual %)) keys))

;; ============================================================================
;; Sample Data Factories
;; ============================================================================

(defn make-session
  "Create a sample session map with optional overrides."
  [& {:as overrides}]
  (merge {:id "test-session-id"
          :project "test-project"
          :branch "feature-branch"
          :target-branch "main"
          :repo-path "/tmp/test-repo"
          :registered-files {}
          :manual-additions []
          :manual-removals []
          :created-at "2024-01-01T00:00:00.000Z"
          :updated-at "2024-01-01T00:00:00.000Z"}
         overrides))

(defn make-comment
  "Create a sample comment map with optional overrides."
  [& {:as overrides}]
  (merge {:id "test-comment-id"
          :session-id "test-session-id"
          :parent-id nil
          :file "src/main.cljs"
          :line 42
          :line-content-hash "abc123"
          :text "This is a test comment"
          :author "test-user"
          :resolved false
          :created-at "2024-01-01T00:00:00.000Z"
          :updated-at "2024-01-01T00:00:00.000Z"}
         overrides))
