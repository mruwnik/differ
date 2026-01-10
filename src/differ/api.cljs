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
            [differ.config :as config]))

;; ============================================================================
;; HTTP Response Helpers
;; ============================================================================

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

;; Session endpoints

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
      (if-let [state (sessions/get-review-state session-id (:repo-path session))]
        (json-response res state)
        (error-response res 500 "Could not get review state"))
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
      (let [repo-path (:repo-path session)
            branches (git/list-branches repo-path)]
        (json-response res {:branches branches}))
      (error-response res 404 "Session not found"))))

(defn get-staged-files-handler
  "GET /api/sessions/:id/staged"
  [^js req res]
  (let [session-id (.. req -params -id)]
    (if-let [session (db/get-session session-id)]
      (let [repo-path (:repo-path session)
            staged (git/get-staged-files repo-path)
            unstaged (git/get-unstaged-files repo-path)]
        (json-response res {:staged (vec staged)
                            :unstaged (vec unstaged)}))
      (error-response res 404 "Session not found"))))

(defn get-untracked-files-handler
  "GET /api/sessions/:id/untracked"
  [^js req res]
  (let [session-id (.. req -params -id)]
    (if-let [session (db/get-session session-id)]
      (let [repo-path (:repo-path session)
            untracked (git/get-untracked-files repo-path)]
        (json-response res {:untracked (vec untracked)}))
      (error-response res 404 "Session not found"))))

(defn stage-file-handler
  "POST /api/sessions/:id/stage"
  [^js req res]
  (let [session-id (.. req -params -id)
        {:keys [path]} (get-body req)]
    (if-let [session (db/get-session session-id)]
      (let [repo-path (:repo-path session)]
        (git/stage-file! repo-path path)
        (let [staged (git/get-staged-files repo-path)
              unstaged (git/get-unstaged-files repo-path)]
          (json-response res {:success true
                              :staged (vec staged)
                              :unstaged (vec unstaged)})))
      (error-response res 404 "Session not found"))))

(defn get-diff-handler
  "GET /api/sessions/:id/diff"
  [^js req res]
  (let [session-id (.. req -params -id)]
    (if-let [session (db/get-session session-id)]
      (json-response res (diff/get-diff-data session))
      (error-response res 404 "Session not found"))))

(defn get-file-diff-handler
  "GET /api/sessions/:id/diff/:file"
  [^js req res]
  (let [session-id (.. req -params -id)
        file (js/decodeURIComponent (.. req -params -file))]
    (if-let [session (db/get-session session-id)]
      (if-let [data (diff/get-file-diff-data session file)]
        (json-response res data)
        (error-response res 404 "Could not read file"))
      (error-response res 404 "Session not found"))))

(defn get-file-content-handler
  "GET /api/sessions/:id/file-content/:file
   Load file content on demand (for large files)."
  [^js req res]
  (let [session-id (.. req -params -id)
        file (js/decodeURIComponent (.. req -params -file))]
    (if-let [session (db/get-session session-id)]
      (let [repo-path (:repo-path session)
            info (git/get-file-info repo-path file true)]
        (if (:error info)
          (error-response res 404 (:error info))
          (let [parsed (git/file-to-diff-format file (:content info))]
            (json-response res {:file file
                                :content (:content info)
                                :size (:size info)
                                :parsed parsed}))))
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
        (let [repo-path (:repo-path session)
              lines (git/get-lines-range repo-path file from-line to-line)]
          (if lines
            (json-response res {:file file
                                :from from-line
                                :to to-line
                                :lines lines})
            (error-response res 404 "Could not read file")))
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

;; Comment endpoints

(defn list-comments-handler
  "GET /api/sessions/:id/comments"
  [^js req res]
  (let [session-id (.. req -params -id)
        file (.. req -query -file)]
    (if-let [session (db/get-session session-id)]
      (let [threaded (if file
                       (comments/get-comments-for-file session-id file)
                       (comments/get-all-comments session-id))
            with-staleness (comments/annotate-comments-with-staleness threaded (:repo-path session))]
        (json-response res {:comments with-staleness}))
      (error-response res 404 "Session not found"))))

(defn get-pending-handler
  "GET /api/sessions/:id/pending"
  [^js req res]
  (let [session-id (.. req -params -id)
        since (.. req -query -since)]
    (if-let [session (db/get-session session-id)]
      (let [pending (comments/get-pending-feedback session-id since)
            with-staleness (comments/annotate-comments-with-staleness pending (:repo-path session))]
        (json-response res {:comments with-staleness}))
      (error-response res 404 "Session not found"))))

(defn add-comment-handler
  "POST /api/sessions/:id/comments"
  [^js req res]
  (let [session-id (.. req -params -id)
        body (get-body req)]
    (if-let [session (db/get-session session-id)]
      (let [comment (comments/add-comment!
                     (assoc body
                            :session-id session-id
                            :repo-path (:repo-path session)))]
        (sse/emit-comment-added! session-id comment)
        (json-response res {:comment comment}))
      (error-response res 404 "Session not found"))))

(defn resolve-comment-handler
  "PATCH /api/comments/:id/resolve"
  [^js req res]
  (let [comment-id (.. req -params -id)
        {:keys [author]} (get-body req)
        comment (comments/resolve-comment! comment-id author)]
    (when-let [session-id (:session-id comment)]
      (sse/emit-comment-resolved! session-id comment-id))
    (json-response res {:success true})))

(defn unresolve-comment-handler
  "PATCH /api/comments/:id/unresolve"
  [^js req res]
  (let [comment-id (.. req -params -id)
        {:keys [author]} (get-body req)
        comment (comments/unresolve-comment! comment-id author)]
    (when-let [session-id (:session-id comment)]
      (sse/emit-comment-unresolved! session-id comment-id))
    (json-response res {:success true})))

(defn delete-comment-handler
  "DELETE /api/comments/:id"
  [^js req res]
  (let [comment-id (.. req -params -id)
        comment (db/get-comment comment-id)]
    (if comment
      (do
        (db/delete-comment! comment-id)
        (when-let [session-id (:session-id comment)]
          (sse/emit-comment-deleted! session-id comment-id))
        (json-response res {:success true}))
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
        client (oauth/register-client body)]
    ;; OAuth spec requires snake_case - use JS object directly
    (-> res
        (.status 200)
        (.json #js {:client_id (:client-id client)
                    :client_name (:client-name client)
                    :redirect_uris (clj->js (:redirect-uris client))
                    :scope (:scope client)
                    :token_endpoint_auth_method "none"
                    :grant_types #js ["authorization_code" "refresh_token"]
                    :response_types #js ["code"]}))))

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
  (.delete app "/api/github/tokens/:id" delete-github-token-handler))
