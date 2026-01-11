(ns differ.sessions
  "Session management logic.

   Pure functions operate on session data.
   Impure functions (ending in !) persist to database.

   Session types:
   - Local: repo-path + branch, session-id = local:<hash>
   - GitHub: owner/repo + PR number, session-id = github:<owner>/<repo>:<pr-number>"
  (:require [differ.db :as db]
            [differ.git :as git]
            [differ.util :as util]
            [differ.config :as config]
            [differ.backend.protocol :as proto]
            [differ.backend.local :as local]
            [differ.backend.github :as github]
            [differ.github-oauth :as github-oauth]
            [clojure.set :as set]
            [clojure.string :as str]
            ["fs" :as fs]
            ["path" :as path]))

;; Forward declarations for functions used before definition
(declare try-tokens-sequentially)

;; ============================================================================
;; Session Type Detection
;; ============================================================================

(defn parse-session-id
  "Parse session ID to determine type and components.
   Returns {:type :local|:github, ...} with type-specific keys."
  [session-id]
  (cond
    (str/starts-with? session-id "github:")
    (let [rest (subs session-id 7)  ; After "github:"
          [owner-repo pr-str] (str/split rest #":" 2)
          [owner repo] (str/split owner-repo #"/" 2)]
      {:type :github
       :owner owner
       :repo repo
       :pr-number (js/parseInt pr-str 10)})

    (str/starts-with? session-id "local:")
    {:type :local
     :hash (subs session-id 6)}

    ;; Backwards compatibility: no prefix = local
    :else
    {:type :local
     :hash session-id}))

(defn session-type
  "Get the type of a session from its ID."
  [session-id]
  (:type (parse-session-id session-id)))

(def ^:private github-pr-patterns
  "Regex patterns for GitHub PR URLs."
  [;; https://github.com/owner/repo/pull/123
   #"https?://github\.com/([^/]+)/([^/]+)/pull/(\d+)"
   ;; github.com/owner/repo/pull/123 (without protocol)
   #"github\.com/([^/]+)/([^/]+)/pull/(\d+)"
   ;; owner/repo#123
   #"^([^/]+)/([^/#]+)#(\d+)$"])

(defn parse-github-pr-url
  "Parse a GitHub PR URL or shorthand into components.
   Returns {:owner :repo :pr-number} or nil if not a valid PR reference."
  [url-or-ref]
  (when (and url-or-ref (string? url-or-ref))
    (some (fn [pattern]
            (when-let [match (re-matches pattern (str/trim url-or-ref))]
              {:owner (nth match 1)
               :repo (nth match 2)
               :pr-number (js/parseInt (nth match 3) 10)}))
          github-pr-patterns)))

(defn github-session-id
  "Generate session ID for a GitHub PR."
  [owner repo pr-number]
  (str "github:" owner "/" repo ":" pr-number))

(defn local-session-id
  "Generate session ID for a local repository."
  [project branch]
  (str "local:" (util/session-id project branch)))

;; ============================================================================
;; Backend Factory
;; ============================================================================

(defn create-backend
  "Create appropriate backend for a session.
   Returns Promise of {:backend <ReviewBackend>} or {:error <message>} or {:requires-auth/:requires-pat}.
   For local sessions, returns immediately. For GitHub, tries tokens with fallback."
  [session]
  (let [session-id (:id session)]
    (case (session-type session-id)
      :local
      (let [repo-path (:repo-path session)
            target-branch (:target-branch session)]
        (js/Promise.resolve
         {:backend (local/create-local-backend repo-path target-branch session-id)}))

      :github
      (let [{:keys [owner repo pr-number]} (parse-session-id session-id)
            tokens (github-oauth/get-all-tokens)]
        (if (empty? tokens)
          (let [base (config/base-url)]
            (js/Promise.resolve
             {:requires-auth true
              :message "GitHub authentication required."
              :auth-url (str base "/oauth/github?return_to=/session/" session-id)}))
          ;; Try tokens with fallback (same logic as session creation)
          (-> (try-tokens-sequentially owner repo pr-number tokens)
              (.then (fn [result]
                       (if (:success result)
                         {:backend (:backend result)}
                         (if (:oauth-restricted result)
                           (let [base (config/base-url)]
                             {:requires-pat true
                              :message "OAuth token cannot access this repo. Add a PAT."
                              :settings-url (str base "/#github")})
                           {:error (str "Failed to access repo: " (:last-error result))})))))))

      (js/Promise.resolve
       {:error (str "Unknown session type for ID: " session-id)}))))

;; ============================================================================
;; Pure Functions - operate on data, no side effects
;; ============================================================================

(defn compute-file-set
  "Compute the set of files in review for a session.
   Pure function: (session, git-changed-files, untracked-files) -> sorted file list

   Registered files and manual additions are only included if they:
   - Have changes (in git-changed-files), OR
   - Are untracked files"
  [session git-changed-files untracked-files]
  (let [{:keys [registered-files manual-additions manual-removals]} session
        git-files (set (map :path git-changed-files))
        untracked (set untracked-files)
        ;; Only keep registered files that still have changes or are untracked
        relevant-registered (filter #(or (git-files %) (untracked %))
                                    (keys registered-files))
        ;; Only keep manual additions that still have changes or are untracked
        relevant-additions (filter #(or (git-files %) (untracked %))
                                   manual-additions)
        all-files (set/union git-files (set relevant-registered) (set relevant-additions))]
    (-> all-files
        (set/difference (set manual-removals))
        sort
        vec)))

(defn compute-registrations
  "Compute new registration map after adding paths.
   Pure function: (current-registrations, paths, agent-id) -> new-registrations"
  [current paths agent-id]
  (reduce (fn [acc path]
            (if (contains? acc path)
              acc  ; Don't overwrite existing
              (assoc acc path agent-id)))
          current
          paths))

(defn compute-unregistrations
  "Compute new registration map after removing paths.
   Only removes paths registered by the given agent.
   Pure function: (current-registrations, paths, agent-id) -> new-registrations"
  [current paths agent-id]
  (let [removable (filter #(= agent-id (get current %)) paths)]
    (apply dissoc current removable)))

(defn compute-file-addition
  "Compute new session state after manually adding a file.
   Pure function: session -> updated-session"
  [session path]
  (update session :manual-additions
          (fn [additions] (vec (conj (set additions) path)))))

(defn compute-file-removal
  "Compute new session state after removing a file.
   Returns [updated-session action] where action is :removed-from-additions or :added-to-removals"
  [session path]
  (let [additions (set (:manual-additions session))]
    (if (contains? additions path)
      ;; Was manually added - remove from additions
      [(update session :manual-additions
               (fn [a] (vec (disj (set a) path))))
       :removed-from-additions]
      ;; Tracked file - add to removals
      [(update session :manual-removals
               (fn [r] (vec (conj (set r) path))))
       :added-to-removals])))

(defn compute-file-restoration
  "Compute new session state after restoring an excluded file.
   Pure function: session -> updated-session"
  [session path]
  (update session :manual-removals
          (fn [removals] (vec (disj (set removals) path)))))

;; ============================================================================
;; Validation
;; ============================================================================

(defn validate-repo-path
  "Validate that repo-path exists and is a directory.
   Returns {:valid true :path <resolved-path>} or {:valid false :error <message>}"
  [repo-path]
  (if (nil? repo-path)
    {:valid false :error "repo_path is required"}
    (let [resolved (path/resolve repo-path)]
      (cond
        (not (fs/existsSync resolved))
        {:valid false :error (str "Path does not exist: " resolved)}

        (not (.isDirectory (fs/statSync resolved)))
        {:valid false :error (str "Path is not a directory: " resolved)}

        :else
        {:valid true :path resolved}))))

;; ============================================================================
;; Impure Functions - interact with database
;; ============================================================================

(defn- get-or-create-local-session
  "Get or create a local repository session.
   Always returns a Promise for consistency with GitHub sessions."
  [{:keys [repo-path project branch target-branch]}]
  (js/Promise.resolve
   (let [validation (validate-repo-path repo-path)]
     (if-not (:valid validation)
       {:error (:error validation)}
       (let [resolved-path (:path validation)
             project (or project (git/get-project-id resolved-path))
             branch (or branch (git/get-current-branch resolved-path))
             target-branch (or target-branch (git/detect-default-branch resolved-path))
             session-id (local-session-id project branch)]
         (if-let [existing (db/get-session session-id)]
           {:session existing :is-new false}
           {:session (db/create-session!
                      {:id session-id
                       :session-type "local"
                       :project project
                       :branch branch
                       :target-branch target-branch
                       :repo-path resolved-path})
            :is-new true}))))))

(defn- oauth-restriction-error?
  "Check if error message indicates OAuth app access restrictions.
   Only matches specific OAuth restriction messages - not generic 'not found' errors
   which could indicate a typo in the repo URL or genuinely deleted repo."
  [error-msg]
  (and (string? error-msg)
       (or (str/includes? error-msg "OAuth App access restrictions")
           (str/includes? error-msg "Resource not accessible by integration"))))

(defn- try-token-for-pr
  "Try to fetch PR context with a single token.
   Returns promise of {:success backend :context ctx} or {:error msg :oauth-restricted bool}."
  [owner repo pr-number token-record]
  (let [backend (github/create-github-backend
                 owner repo pr-number (:access-token token-record))]
    (-> (proto/get-context backend)
        (.then (fn [context]
                 {:success true :backend backend :context context}))
        (.catch (fn [err]
                  (let [msg (or (.-message err) (str err))]
                    {:success false
                     :error msg
                     :oauth-restricted (oauth-restriction-error? msg)}))))))

(defn- try-tokens-sequentially
  "Try tokens one by one until success. Returns promise.
   Returns {:success ... :context ...} or {:all-failed true :last-error ... :oauth-restricted bool}.
   Always tries remaining tokens on failure (token might be revoked, rate limited, etc.)."
  [owner repo pr-number tokens]
  (if (empty? tokens)
    (js/Promise.resolve {:all-failed true :last-error "No tokens available"})
    (let [[token & remaining] tokens]
      (-> (try-token-for-pr owner repo pr-number token)
          (.then (fn [result]
                   (if (:success result)
                     result
                     ;; Token failed - try remaining tokens if any
                     (if (seq remaining)
                       (-> (try-tokens-sequentially owner repo pr-number remaining)
                           (.then (fn [next-result]
                                    ;; Propagate oauth-restricted flag if any token hit it
                                    (if (and (:all-failed next-result)
                                             (:oauth-restricted result)
                                             (not (:oauth-restricted next-result)))
                                      (assoc next-result :oauth-restricted true)
                                      next-result))))
                       ;; No more tokens
                       {:all-failed true
                        :last-error (:error result)
                        :oauth-restricted (:oauth-restricted result)}))))))))

(defn- get-or-create-github-session
  "Get or create a GitHub PR session.
   Always returns a Promise for consistency with local sessions.
   Tries OAuth token first, falls back to PATs on org restriction errors."
  [{:keys [owner repo pr-number]}]
  (let [session-id (github-session-id owner repo pr-number)
        tokens (github-oauth/get-all-tokens)]
    ;; Check if session exists
    (if-let [existing (db/get-session session-id)]
      (js/Promise.resolve {:session existing :is-new false})
      ;; Need to create - check for tokens
      (if (empty? tokens)
        ;; No tokens - require auth
        (let [base (config/base-url)]
          (js/Promise.resolve
           {:requires-auth true
            :message "GitHub authentication required. Click the auth_url to connect your GitHub account, then retry this request."
            :auth-url (str base "/oauth/github?return_to=/session/" session-id)}))
        ;; Try tokens with fallback
        (-> (try-tokens-sequentially owner repo pr-number tokens)
            (.then (fn [result]
                     (if (:success result)
                       ;; Success - create session
                       (let [context (:context result)]
                         {:session (db/create-session!
                                    {:id session-id
                                     :session-type "github"
                                     :project (str owner "/" repo)
                                     :branch (:head-branch context)
                                     :target-branch (:base-branch context)
                                     :repo-path (str "https://github.com/" owner "/" repo)
                                     :github-owner owner
                                     :github-repo repo
                                     :github-pr-number pr-number})
                          :is-new true})
                       ;; All tokens failed
                       (if (:oauth-restricted result)
                         ;; OAuth restriction - suggest PAT
                         (let [base (config/base-url)]
                           {:requires-pat true
                            :message (str "This repository restricts OAuth app access (common for forks from organizations with security policies). "
                                          "Create a Personal Access Token at github_pat_url, then add it at settings_url and retry.")
                            :settings-url (str base "/#github")
                            :github-pat-url "https://github.com/settings/tokens/new?scopes=repo&description=differ-access"})
                         ;; Other error
                         {:error (str "Failed to fetch PR: " (:last-error result))})))))))))

(defn get-or-create-session
  "Get existing session or create new one.
   Accepts either:
   - {:repo-path ...} for local repositories
   - {:github-pr ...} with PR URL like 'https://github.com/owner/repo/pull/123'
   - {:owner :repo :pr-number} for GitHub PRs directly

   Returns {:session ... :is-new bool} or {:error ...} or {:requires-auth :auth-url}."
  [{:keys [repo-path github-pr owner repo pr-number] :as params}]
  (cond
    ;; GitHub PR by URL
    github-pr
    (if-let [parsed (parse-github-pr-url github-pr)]
      (get-or-create-github-session parsed)
      {:error (str "Invalid GitHub PR URL: " github-pr)})

    ;; GitHub PR by components
    (and owner repo pr-number)
    (get-or-create-github-session {:owner owner :repo repo :pr-number pr-number})

    ;; Local repository
    repo-path
    (get-or-create-local-session params)

    :else
    {:error "Must provide either repo-path for local or github-pr/owner+repo+pr-number for GitHub"}))

(defn- with-unresolved-count
  "Add unresolved comment count to session."
  [session]
  (when session
    (assoc session :unresolved-count
           (db/count-unresolved-comments (:id session)))))

(defn list-sessions
  "List all sessions with unresolved counts."
  ([] (list-sessions nil))
  ([project]
   (mapv with-unresolved-count (db/list-sessions project))))

(defn get-session
  "Get session by ID with unresolved count."
  [session-id]
  (with-unresolved-count (db/get-session session-id)))

(defn register-files!
  "Register files with a session. Returns newly registered paths."
  [session-id paths agent-id]
  (when-let [session (db/get-session session-id)]
    (let [current (:registered-files session)
          updated (compute-registrations current paths agent-id)
          newly-added (remove #(contains? current %) paths)]
      (db/update-session! session-id {:registered-files updated})
      (vec newly-added))))

(defn unregister-files!
  "Unregister files from a session. Returns unregistered paths."
  [session-id paths agent-id]
  (when-let [session (db/get-session session-id)]
    (let [current (:registered-files session)
          updated (compute-unregistrations current paths agent-id)
          removed (filter #(= agent-id (get current %)) paths)]
      (db/update-session! session-id {:registered-files updated})
      (vec removed))))

(defn add-manual-file!
  "Manually add a file to the review set."
  [session-id path]
  (when-let [session (db/get-session session-id)]
    (let [updated (compute-file-addition session path)]
      (db/update-session! session-id
                          {:manual-additions (:manual-additions updated)})
      path)))

(defn remove-manual-file!
  "Remove a file from the review set.
   Returns {:action :removed-from-additions|:added-to-removals :path path}"
  [session-id path]
  (when-let [session (db/get-session session-id)]
    (let [[updated action] (compute-file-removal session path)]
      (db/update-session! session-id
                          (select-keys updated [:manual-additions :manual-removals]))
      {:action action :path path})))

(defn restore-file!
  "Restore an excluded file back to the review set."
  [session-id path]
  (when-let [session (db/get-session session-id)]
    (let [updated (compute-file-restoration session path)]
      (db/update-session! session-id
                          {:manual-removals (:manual-removals updated)})
      path)))

(defn get-review-state
  "Get full review state for a session including files and comments."
  [session-id repo-path]
  (when-let [session (get-session session-id)]
    (let [git-files (git/get-changed-files repo-path (:target-branch session))
          untracked (git/get-untracked-files repo-path)
          files (compute-file-set session git-files untracked)
          ;; Only count unresolved comments on files currently in review
          unresolved-count (db/count-unresolved-comments session-id files)]
      {:session-id session-id
       :project (:project session)
       :branch (:branch session)
       :target-branch (:target-branch session)
       :repo-path (:repo-path session)
       :files files
       :excluded-files (:manual-removals session)
       :comments (db/list-comments session-id)
       :unresolved-count unresolved-count})))

(defn archive-session!
  "Archive/delete a session and all its data."
  [session-id]
  (db/delete-session! session-id))
