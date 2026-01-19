(ns differ.api
  "REST API handlers for web UI."
  (:require [clojure.string :as str]
            [differ.sessions :as sessions]
            [differ.comments :as comments]
            [differ.diff :as diff]
            [differ.git :as git]
            [differ.db :as db]
            [differ.oauth :as oauth]
            [differ.github-oauth :as github-oauth]
            [differ.sse :as sse]
            [differ.util :as util]
            [differ.config :as config]
            [differ.backend.protocol :as proto]
            ["path" :as path]))

;; ============================================================================
;; HTTP Response Helpers
;; ============================================================================

(defn- handle-by-session-type
  "Execute different logic based on session type.
   handlers is a map with :local and :github keys, each a 0-arg fn."
  [session-id {:keys [local github]}]
  (if (= :local (sessions/session-type session-id))
    (local)
    (github)))

(defn- json-response
  "Send JSON response. Keys stay as kebab-case for internal UI client."
  [^js res data]
  (-> res
      (.status 200)
      (.json (clj->js data))))

(defn- error-response [^js res status message]
  (-> res
      (.status status)
      (.json #js {:error message})))

;; ============================================================================
;; Request Body Normalization
;; ============================================================================

(defn- get-body
  "Get request body with snake->kebab key conversion."
  [^js req]
  (-> (.-body req)
      (js->clj :keywordize-keys true)
      util/keys->kebab))

;; ============================================================================
;; Path Validation
;; ============================================================================

(defn- safe-file-path?
  "Check if file-path is safe (doesn't escape repo-path via path traversal).
   Returns true if the resolved path is within repo-path."
  [repo-path file-path]
  (let [resolved (path/resolve repo-path file-path)
        repo-normalized (str (path/resolve repo-path) "/")]
    (str/starts-with? (str resolved "/") repo-normalized)))

(defn- validate-file-path
  "Validate file path is safe and return error response if not.
   Returns true if safe (caller should proceed), false if error was sent."
  [repo-path file-path res]
  (if (safe-file-path? repo-path file-path)
    true
    (do
      (error-response res 400 "Invalid file path: path traversal not allowed")
      false)))

;; Session endpoints

(defn- fetch-github-session-state
  "Fetch review state for a GitHub session.
   Returns promise of state map with :session-id, :project, :branch, :target-branch,
   :repo-path, :state, :title, :files, :excluded-files, :comments, :unresolved-count."
  [session-id session]
  (-> (sessions/create-backend session)
      (.then (fn [result]
               (if (:error result)
                 (throw (js/Error. (:error result)))
                 (let [backend (:backend result)]
                   (js/Promise.all
                    #js [(proto/get-context backend)
                         (proto/get-changed-files backend)
                         (proto/get-comments backend)])))))
      (.then (fn [[ctx files comments]]
               {:session-id session-id
                :project (:project session)
                :branch (:head-branch ctx)
                :target-branch (:base-branch ctx)
                :repo-path (:repo-path session)
                :state (:state ctx)
                :title (:title ctx)
                :files (mapv :path files)
                :excluded-files (:manual-removals session)
                :comments comments
                :unresolved-count (count (remove :resolved comments))}))))

(defn list-sessions-handler
  "GET /api/sessions"
  [^js req res]
  (let [project (.. req -query -project)
        sessions (sessions/list-sessions project)]
    (json-response res {:sessions sessions})))

(defn get-session-handler
  "GET /api/sessions/:id"
  [^js req res]
  (let [session-id (.. req -params -id)]
    (if-let [session (db/get-session session-id)]
      (if (= :github (sessions/session-type session-id))
        ;; For GitHub sessions, use extracted helper
        (-> (fetch-github-session-state session-id session)
            (.then (fn [state] (json-response res state)))
            (.catch (fn [err]
                      (error-response res 500 (or (.-message err) (str err))))))
        ;; For local sessions, use existing get-review-state
        (if-let [state (sessions/get-review-state session-id (:repo-path session))]
          (json-response res state)
          (error-response res 500 "Could not get review state")))
      (error-response res 404 "Session not found"))))

(defn create-session-handler
  "POST /api/sessions"
  [^js req res]
  (let [body (get-body req)
        result (sessions/get-or-create-session body)]
    (if-let [error (:error result)]
      (error-response res 400 error)
      (json-response res result))))

(defn delete-session-handler
  "DELETE /api/sessions/:id"
  [^js req res]
  (let [session-id (.. req -params -id)]
    (sessions/archive-session! session-id)
    (json-response res {:success true})))

(defn update-session-handler
  "PATCH /api/sessions/:id"
  [^js req res]
  (let [session-id (.. req -params -id)
        body (get-body req)]
    (if-let [session (db/update-session! session-id body)]
      (json-response res {:session session})
      (error-response res 404 "Session not found"))))

;; Diff endpoints

(defn get-branches-handler
  "GET /api/sessions/:id/branches"
  [^js req res]
  (let [session-id (.. req -params -id)]
    (if-let [session (db/get-session session-id)]
      (-> (sessions/create-backend session)
          (.then (fn [result]
                   (if (:error result)
                     (throw (js/Error. (:error result)))
                     (proto/get-branches (:backend result)))))
          (.then (fn [branches]
                   (json-response res {:branches (mapv :name (or branches []))})))
          (.catch (fn [err]
                    (error-response res 500 (or (.-message err) (str err))))))
      (error-response res 404 "Session not found"))))

(defn get-staged-files-handler
  "GET /api/sessions/:id/staged
   Local-only: returns staged/unstaged files. Returns error for GitHub sessions."
  [^js req res]
  (let [session-id (.. req -params -id)]
    (if-let [session (db/get-session session-id)]
      (if (not= :local (sessions/session-type session-id))
        (error-response res 400 "Staging operations not available for GitHub PR sessions")
        (let [repo-path (:repo-path session)
              staged (git/get-staged-files repo-path)
              unstaged (git/get-unstaged-files repo-path)]
          (json-response res {:staged (vec staged)
                              :unstaged (vec unstaged)})))
      (error-response res 404 "Session not found"))))

(defn get-untracked-files-handler
  "GET /api/sessions/:id/untracked
   Local-only: returns untracked files. Returns error for GitHub sessions."
  [^js req res]
  (let [session-id (.. req -params -id)]
    (if-let [session (db/get-session session-id)]
      (if (not= :local (sessions/session-type session-id))
        (error-response res 400 "Untracked files not available for GitHub PR sessions")
        (let [repo-path (:repo-path session)
              untracked (git/get-untracked-files repo-path)]
          (json-response res {:untracked (vec untracked)})))
      (error-response res 404 "Session not found"))))

(defn stage-file-handler
  "POST /api/sessions/:id/stage
   Local-only: stages a file. Returns error for GitHub sessions."
  [^js req res]
  (let [session-id (.. req -params -id)
        {:keys [path]} (get-body req)]
    (if-let [session (db/get-session session-id)]
      (if (not= :local (sessions/session-type session-id))
        (error-response res 400 "Staging operations not available for GitHub PR sessions")
        (let [repo-path (:repo-path session)]
          (git/stage-file! repo-path path)
          (let [staged (git/get-staged-files repo-path)
                unstaged (git/get-unstaged-files repo-path)]
            (json-response res {:success true
                                :staged (vec staged)
                                :unstaged (vec unstaged)}))))
      (error-response res 404 "Session not found"))))

(defn get-diff-handler
  "GET /api/sessions/:id/diff"
  [^js req res]
  (let [session-id (.. req -params -id)]
    (if-let [session (db/get-session session-id)]
      (handle-by-session-type
       session-id
       {:local #(json-response res (diff/get-diff-data session))
        :github #(-> (sessions/create-backend session)
                     (.then (fn [result]
                              (if (:error result)
                                (throw (js/Error. (:error result)))
                                (let [backend (:backend result)]
                                  (js/Promise.all
                                   #js [(proto/get-diff backend)
                                        (proto/get-changed-files backend)])))))
                     (.then (fn [[raw-diff changed-files]]
                              (json-response res
                                             {:diff raw-diff
                                              :parsed (git/parse-diff-hunks raw-diff)
                                              :files (mapv :path changed-files)
                                              :files-with-size (mapv (fn [f]
                                                                       {:path (:path f)
                                                                        :additions (:additions f)
                                                                        :deletions (:deletions f)})
                                                                     changed-files)
                                              :changed-files changed-files
                                              :is-git-repo true})))
                     (.catch (fn [err]
                               (error-response res 500 (or (.-message err) (str err))))))})
      (error-response res 404 "Session not found"))))

(defn get-file-diff-handler
  "GET /api/sessions/:id/diff/:file"
  [^js req res]
  (let [session-id (.. req -params -id)
        file (js/decodeURIComponent (.. req -params -file))]
    (if-let [session (db/get-session session-id)]
      (when (validate-file-path (:repo-path session) file res)
        (handle-by-session-type
         session-id
         {:local #(if-let [data (diff/get-file-diff-data session file)]
                    (json-response res data)
                    (error-response res 404 "Could not read file"))
          :github #(-> (sessions/create-backend session)
                       (.then (fn [result]
                                (if (:error result)
                                  (throw (js/Error. (:error result)))
                                  (proto/get-file-diff (:backend result) file))))
                       (.then (fn [raw-diff]
                                (json-response res
                                               {:file file
                                                :diff raw-diff
                                                :parsed (git/parse-diff-hunks raw-diff)
                                                :is-git-repo true})))
                       (.catch (fn [err]
                                 (error-response res 500 (or (.-message err) (str err))))))}))
      (error-response res 404 "Session not found"))))

(defn get-file-content-handler
  "GET /api/sessions/:id/file-content/:file
   Load file content on demand (for large files)."
  [^js req res]
  (let [session-id (.. req -params -id)
        file (js/decodeURIComponent (.. req -params -file))]
    (if-let [session (db/get-session session-id)]
      (when (validate-file-path (:repo-path session) file res)
        (-> (sessions/create-backend session)
            (.then (fn [result]
                     (if (:error result)
                       (throw (js/Error. (:error result)))
                       ;; nil ref means working tree for local, head for GitHub
                       (proto/get-file-content (:backend result) nil file))))
            (.then (fn [content]
                     (if content
                       (let [parsed (git/file-to-diff-format file content)]
                         (json-response res {:file file
                                             :content content
                                             :size (count content)
                                             :parsed parsed}))
                       (error-response res 404 "Could not read file"))))
            (.catch (fn [err]
                      (error-response res 500 (or (.-message err) (str err)))))))
      (error-response res 404 "Session not found"))))

(defn get-context-lines-handler
  "GET /api/sessions/:id/context/:file
   Get context lines for expanding diff view.
   Query params: from (line number), to (line number)"
  [^js req res]
  (let [session-id (.. req -params -id)
        file (js/decodeURIComponent (.. req -params -file))
        from-line (js/parseInt (or (.. req -query -from) "1") 10)
        to-line (js/parseInt (or (.. req -query -to) "1") 10)]
    ;; Validate line numbers
    (cond
      (or (js/isNaN from-line) (js/isNaN to-line))
      (error-response res 400 "Invalid line numbers: must be integers")

      (or (< from-line 1) (< to-line 1))
      (error-response res 400 "Invalid line numbers: must be positive")

      (> from-line to-line)
      (error-response res 400 "Invalid range: from must be <= to")

      :else
      (if-let [session (db/get-session session-id)]
        (when (validate-file-path (:repo-path session) file res)
          (-> (sessions/create-backend session)
              (.then (fn [result]
                       (if (:error result)
                         (throw (js/Error. (:error result)))
                         ;; Use protocol with line range options
                         (proto/get-file-content (:backend result) nil file
                                                 {:from from-line :to to-line}))))
              (.then (fn [lines]
                       (if lines
                         (json-response res {:file file
                                             :from from-line
                                             :to to-line
                                             :lines lines})
                         (error-response res 404 "Could not read file"))))
              (.catch (fn [err]
                        (error-response res 500 (or (.-message err) (str err)))))))
        (error-response res 404 "Session not found")))))

;; File management endpoints

(defn register-files-handler
  "POST /api/sessions/:id/files"
  [^js req res]
  (let [session-id (.. req -params -id)
        {:keys [paths agent-id]} (get-body req)
        registered (sessions/register-files! session-id paths agent-id)]
    (json-response res {:registered registered})))

(defn unregister-files-handler
  "DELETE /api/sessions/:id/files"
  [^js req res]
  (let [session-id (.. req -params -id)
        {:keys [paths agent-id]} (get-body req)
        unregistered (sessions/unregister-files! session-id paths agent-id)]
    (json-response res {:unregistered unregistered})))

(defn add-manual-file-handler
  "POST /api/sessions/:id/manual-files"
  [^js req res]
  (let [session-id (.. req -params -id)
        {:keys [path]} (get-body req)]
    (sessions/add-manual-file! session-id path)
    (json-response res {:success true :path path})))

(defn remove-manual-file-handler
  "DELETE /api/sessions/:id/manual-files"
  [^js req res]
  (let [session-id (.. req -params -id)
        {:keys [path]} (get-body req)]
    (sessions/remove-manual-file! session-id path)
    (json-response res {:success true :path path})))

(defn restore-file-handler
  "POST /api/sessions/:id/restore-file"
  [^js req res]
  (let [session-id (.. req -params -id)
        {:keys [path]} (get-body req)]
    (sessions/restore-file! session-id path)
    (json-response res {:success true :path path})))

;; Review request endpoint

(defn request-review-handler
  "POST /api/sessions/:id/request-review
   Request external review - pushes branch and creates PR (for local sessions)
   or returns existing PR (for GitHub sessions)."
  [^js req res]
  (let [session-id (.. req -params -id)
        body (get-body req)]
    (if-let [session (db/get-session session-id)]
      (-> (sessions/request-review! session body)
          (.then (fn [result] (json-response res result)))
          (.catch (fn [err]
                    (error-response res 500 (or (.-message err) (str err))))))
      (error-response res 404 "Session not found"))))

;; Comment endpoints

(defn list-comments-handler
  "GET /api/sessions/:id/comments"
  [^js req res]
  (let [session-id (.. req -params -id)
        file (.. req -query -file)]
    (if-let [session (db/get-session session-id)]
      (-> (sessions/create-backend session)
          (.then (fn [result]
                   (if (:error result)
                     (throw (js/Error. (:error result)))
                     (proto/get-comments (:backend result)))))
          (.then (fn [comments]
                   ;; Filter by file if specified
                   (let [filtered (if file
                                    (filter #(= file (:file %)) comments)
                                    comments)]
                     ;; LocalBackend.get-comments already annotates staleness,
                     ;; so we don't need to annotate again here
                     (json-response res {:comments filtered}))))
          (.catch (fn [err]
                    (error-response res 500 (or (.-message err) (str err))))))
      (error-response res 404 "Session not found"))))

(defn get-pending-handler
  "GET /api/sessions/:id/pending
   Returns pending comments and CI status for the session."
  [^js req res]
  (let [session-id (.. req -params -id)
        since (.. req -query -since)]
    (if-let [session (db/get-session session-id)]
      (-> (sessions/create-backend session)
          (.then (fn [result]
                   (if (:error result)
                     (throw (js/Error. (:error result)))
                     (let [backend (:backend result)]
                       ;; Fetch both pending comments and CI status in parallel
                       (js/Promise.all
                        #js [(proto/get-pending-comments backend {:since since})
                             (proto/get-ci-status backend)])))))
          (.then (fn [results]
                   (let [[comments ci-status] results]
                     ;; LocalBackend.get-pending-comments already annotates staleness,
                     ;; so we don't need to annotate again here
                     (json-response res {:comments comments :ci ci-status}))))
          (.catch (fn [err]
                    (error-response res 500 (or (.-message err) (str err))))))
      (error-response res 404 "Session not found"))))

(defn add-comment-handler
  "POST /api/sessions/:id/comments"
  [^js req res]
  (let [session-id (.. req -params -id)
        body (get-body req)]
    (if-let [session (db/get-session session-id)]
      (-> (sessions/create-backend session)
          (.then (fn [result]
                   (if (:error result)
                     (throw (js/Error. (:error result)))
                     (proto/add-comment! (:backend result) body))))
          (.then (fn [comment]
                   (sse/emit-comment-added! session-id comment)
                   (json-response res {:comment comment})))
          (.catch (fn [err]
                    (error-response res 500 (or (.-message err) (str err))))))
      (error-response res 404 "Session not found"))))

(defn resolve-comment-handler
  "PATCH /api/comments/:id/resolve
   Body: {:session-id string, :author string}"
  [^js req res]
  (let [comment-id (.. req -params -id)
        {:keys [session-id author]} (get-body req)]
    (if-not session-id
      ;; Fallback for backwards compatibility with local sessions
      (if-let [comment (comments/resolve-comment! comment-id author)]
        (do
          (when-let [sid (:session-id comment)]
            (sse/emit-comment-resolved! sid comment-id))
          (json-response res {:success true}))
        (error-response res 404 "Comment not found"))
      ;; Use backend protocol when session-id is provided
      (if-let [session (db/get-session session-id)]
        (-> (sessions/create-backend session)
            (.then (fn [result]
                     (if (:error result)
                       (throw (js/Error. (:error result)))
                       (proto/resolve-comment! (:backend result) comment-id author))))
            (.then (fn [_]
                     (sse/emit-comment-resolved! session-id comment-id)
                     (json-response res {:success true})))
            (.catch (fn [err]
                      (error-response res 500 (or (.-message err) (str err))))))
        (error-response res 404 "Session not found")))))

(defn unresolve-comment-handler
  "PATCH /api/comments/:id/unresolve
   Body: {:session-id string, :author string}"
  [^js req res]
  (let [comment-id (.. req -params -id)
        {:keys [session-id author]} (get-body req)]
    (if-not session-id
      ;; Fallback for backwards compatibility with local sessions
      (if-let [comment (comments/unresolve-comment! comment-id author)]
        (do
          (when-let [sid (:session-id comment)]
            (sse/emit-comment-unresolved! sid comment-id))
          (json-response res {:success true}))
        (error-response res 404 "Comment not found"))
      ;; Use backend protocol when session-id is provided
      (if-let [session (db/get-session session-id)]
        (-> (sessions/create-backend session)
            (.then (fn [result]
                     (if (:error result)
                       (throw (js/Error. (:error result)))
                       (proto/unresolve-comment! (:backend result) comment-id author))))
            (.then (fn [_]
                     (sse/emit-comment-unresolved! session-id comment-id)
                     (json-response res {:success true})))
            (.catch (fn [err]
                      (error-response res 500 (or (.-message err) (str err))))))
        (error-response res 404 "Session not found")))))

(defn delete-comment-handler
  "DELETE /api/comments/:id
   Local-only: Deleting GitHub comments requires editing/deleting via GitHub's UI."
  [^js req res]
  (let [comment-id (.. req -params -id)
        comment (db/get-comment comment-id)]
    (if comment
      ;; Use the comment's actual session-id to determine session type (security)
      (let [comment-session-id (:session-id comment)]
        (if (= :github (sessions/session-type comment-session-id))
          (error-response res 400 "Deleting comments on GitHub PRs is not supported via API")
          (do
            (db/delete-comment! comment-id)
            (when comment-session-id
              (sse/emit-comment-deleted! comment-session-id comment-id))
            (json-response res {:success true}))))
      (error-response res 404 "Comment not found"))))

;; OAuth endpoints

(defn oauth-metadata-handler
  "GET /.well-known/oauth-authorization-server"
  [^js _req res]
  (json-response res (oauth/get-authorization-server-metadata)))

(defn oauth-protected-resource-handler
  "GET /.well-known/oauth-protected-resource"
  [^js _req res]
  (json-response res (oauth/get-protected-resource-metadata)))

(defn oauth-register-handler
  "POST /oauth/register - RFC 7591 Dynamic Client Registration"
  [^js req res]
  (let [body (get-body req)
        result (oauth/register-client body)]
    (if-let [error (:error result)]
      (error-response res 400 error)
      ;; OAuth spec requires snake_case - use JS object directly
      (-> res
          (.status 200)
          (.json #js {:client_id (:client-id result)
                      :client_name (:client-name result)
                      :redirect_uris (clj->js (:redirect-uris result))
                      :scope (:scope result)
                      :token_endpoint_auth_method "none"
                      :grant_types #js ["authorization_code" "refresh_token"]
                      :response_types #js ["code"]})))))

(defn- get-query
  "Get query params with snake->kebab key conversion."
  [^js req]
  (-> (.-query req)
      (js->clj :keywordize-keys true)
      util/keys->kebab))

(defn oauth-authorize-handler
  "GET /oauth/authorize"
  [^js req res]
  (let [query (get-query req)
        result (oauth/authorize
                {:client-id (:client-id query)
                 :redirect-uri (:redirect-uri query)
                 :scopes (when-let [s (:scope query)]
                           (str/split s #"\s+"))
                 :state (:state query)
                 :code-challenge (:code-challenge query)})]
    (if-let [error (:error result)]
      (error-response res 400 error)
      (.redirect res (:redirect-url result)))))

(defn oauth-token-handler
  "POST /oauth/token"
  [^js req res]
  (let [body (get-body req)
        grant-type (:grant-type body)]
    (case grant-type
      "authorization_code"
      (let [result (oauth/exchange-authorization-code
                    {:code (:code body)
                     :client-id (:client-id body)})]
        (if-let [error (:error result)]
          (error-response res 400 error)
          (json-response res result)))

      "refresh_token"
      (let [result (oauth/exchange-refresh-token
                    {:refresh-token (:refresh-token body)
                     :client-id (:client-id body)
                     :scopes (when-let [s (:scope body)]
                               (str/split s #"\s+"))})]
        (if-let [error (:error result)]
          (error-response res 400 error)
          (json-response res result)))

      (error-response res 400 (str "Unsupported grant_type: " grant-type)))))

(defn oauth-revoke-handler
  "POST /oauth/revoke"
  [^js req res]
  (let [body (get-body req)
        result (oauth/revoke-token
                {:token (:token body)
                 :token-type-hint (:token-type-hint body)})]
    (json-response res result)))

;; Config endpoint

(defn get-config-handler
  "GET /api/config"
  [^js _req res]
  (json-response res {:config (config/client-config)}))

;; GitHub OAuth endpoints

(defn github-oauth-start-handler
  "GET /oauth/github - Start GitHub OAuth flow"
  [^js req res]
  (if-not (github-oauth/configured?)
    (error-response res 400 "GitHub OAuth not configured. Set GITHUB_CLIENT_ID and GITHUB_CLIENT_SECRET env vars.")
    (let [return-to (or (.. req -query -return_to) "/")
          state (util/gen-token "gh_")
          port (config/get-value :port)
          redirect-uri (str "http://localhost:" port "/oauth/github/callback")]
      ;; Store state for CSRF verification
      (db/create-github-oauth-state! {:state state
                                      :return-to return-to
                                      :expires-at (util/expires-at 600)})
      (.redirect res (github-oauth/authorization-url redirect-uri state)))))

(defn- validate-oauth-callback
  "Validate OAuth callback params. Returns {:ok oauth-state} or {:error [status message]}."
  [state error]
  (cond
    error
    {:error [400 (str "GitHub OAuth error: " error)]}

    (not state)
    {:error [400 "Missing state parameter"]}

    :else
    (if-let [oauth-state (db/get-github-oauth-state state)]
      (if (util/expired? (:expires-at oauth-state))
        (do (db/delete-github-oauth-state! state)
            {:error [400 "OAuth state expired, please try again"]})
        {:ok oauth-state})
      {:error [400 "Invalid or expired state parameter"]})))

(defn github-oauth-callback-handler
  "GET /oauth/github/callback - GitHub OAuth callback"
  [^js req res]
  (let [code (.. req -query -code)
        state (.. req -query -state)
        error (.. req -query -error)
        validation (validate-oauth-callback state error)]
    (if-let [[status msg] (:error validation)]
      (error-response res status msg)
      (let [oauth-state (:ok validation)]
        (-> (github-oauth/complete-oauth-flow code)
            (.then (fn [_]
                     (db/delete-github-oauth-state! state)
                     (.redirect res (str (or (:return-to oauth-state) "/")
                                         "?github_connected=true"))))
            (.catch (fn [err]
                      (js/console.error "GitHub OAuth error:" err)
                      (db/delete-github-oauth-state! state)
                      (error-response res 400 (str "GitHub OAuth failed: " (.-message err))))))))))

(defn list-github-tokens-handler
  "GET /api/github/tokens - List stored GitHub tokens"
  [^js _req res]
  (json-response res {:tokens (db/list-github-tokens)}))

(defn delete-github-token-handler
  "DELETE /api/github/tokens/:id - Revoke a GitHub token"
  [^js req res]
  (let [token-id (.. req -params -id)]
    (db/delete-github-token! token-id)
    (json-response res {:success true})))

(defn add-github-pat-handler
  "POST /api/github/tokens - Add a Personal Access Token"
  [^js req res]
  (let [body (get-body req)
        {:keys [name token]} body]
    (cond
      (or (nil? name) (str/blank? name))
      (error-response res 400 "Name is required")

      (or (nil? token) (str/blank? token))
      (error-response res 400 "Token is required")

      :else
      ;; Validate the token by calling GitHub API
      (-> (github-oauth/validate-pat token)
          (.then (fn [result]
                   (if (:valid result)
                     (let [username (get-in result [:user :login])
                           stored (github-oauth/store-pat! name token username)]
                       (json-response res {:success true
                                           :token {:id (:id stored)
                                                   :name name
                                                   :github-username username
                                                   :token-type "pat"}}))
                     (error-response res 400 (str "Invalid token: " (:error result))))))
          (.catch (fn [err]
                    (error-response res 500 (str "Failed to validate token: " (.-message err)))))))))

(defn github-status-handler
  "GET /api/github/status - Check GitHub OAuth configuration and token status"
  [^js _req res]
  (let [configured (github-oauth/configured?)
        token (when configured (github-oauth/get-any-token))]
    (json-response res {:configured configured
                        :connected (some? token)
                        :username (when token (:github-username token))})))

;; Route setup

(defn setup-routes [^js app]
  ;; Config
  (.get app "/api/config" get-config-handler)

  ;; Sessions
  (.get app "/api/sessions" list-sessions-handler)
  (.post app "/api/sessions" create-session-handler)
  (.get app "/api/sessions/:id" get-session-handler)
  (.patch app "/api/sessions/:id" update-session-handler)
  (.delete app "/api/sessions/:id" delete-session-handler)

  ;; Branches
  (.get app "/api/sessions/:id/branches" get-branches-handler)

  ;; Git staging
  (.get app "/api/sessions/:id/staged" get-staged-files-handler)
  (.post app "/api/sessions/:id/stage" stage-file-handler)
  (.get app "/api/sessions/:id/untracked" get-untracked-files-handler)

  ;; Diff
  (.get app "/api/sessions/:id/diff" get-diff-handler)
  (.get app "/api/sessions/:id/diff/:file" get-file-diff-handler)
  (.get app "/api/sessions/:id/file-content/:file" get-file-content-handler)
  (.get app "/api/sessions/:id/context/:file" get-context-lines-handler)

  ;; Files
  (.post app "/api/sessions/:id/files" register-files-handler)
  (.delete app "/api/sessions/:id/files" unregister-files-handler)
  (.post app "/api/sessions/:id/manual-files" add-manual-file-handler)
  (.delete app "/api/sessions/:id/manual-files" remove-manual-file-handler)
  (.post app "/api/sessions/:id/restore-file" restore-file-handler)

  ;; Review
  (.post app "/api/sessions/:id/request-review" request-review-handler)

  ;; Comments
  (.get app "/api/sessions/:id/comments" list-comments-handler)
  (.get app "/api/sessions/:id/pending" get-pending-handler)
  (.post app "/api/sessions/:id/comments" add-comment-handler)
  (.patch app "/api/comments/:id/resolve" resolve-comment-handler)
  (.patch app "/api/comments/:id/unresolve" unresolve-comment-handler)
  (.delete app "/api/comments/:id" delete-comment-handler)

  ;; OAuth
  (.get app "/.well-known/oauth-authorization-server" oauth-metadata-handler)
  (.get app "/.well-known/oauth-protected-resource" oauth-protected-resource-handler)
  (.post app "/oauth/register" oauth-register-handler)
  (.get app "/oauth/authorize" oauth-authorize-handler)
  (.post app "/oauth/token" oauth-token-handler)
  (.post app "/oauth/revoke" oauth-revoke-handler)

  ;; GitHub OAuth
  (.get app "/oauth/github" github-oauth-start-handler)
  (.get app "/oauth/github/callback" github-oauth-callback-handler)
  (.get app "/api/github/status" github-status-handler)
  (.get app "/api/github/tokens" list-github-tokens-handler)
  (.post app "/api/github/tokens" add-github-pat-handler)
  (.delete app "/api/github/tokens/:id" delete-github-token-handler))
