(ns differ.api
  "REST API handlers for web UI."
  (:require [clojure.string :as str]
            [differ.sessions :as sessions]
            [differ.comments :as comments]
            [differ.git :as git]
            [differ.db :as db]
            [differ.oauth :as oauth]))

(defn- json-response [^js res data]
  (-> res
      (.status 200)
      (.json (clj->js data))))

(defn- error-response [^js res status message]
  (-> res
      (.status status)
      (.json #js {:error message})))

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
  (let [body (js->clj (.-body req) :keywordize-keys true)
        result (sessions/get-or-create-session
                {:repo-path (or (:repo_path body) (:repo-path body))
                 :project (:project body)
                 :branch (:branch body)
                 :target-branch (or (:target_branch body) (:target-branch body))})]
    (if-let [error (:error result)]
      (error-response res 400 error)
      (json-response res {:session (:session result)
                          :is-new (:is-new result)}))))

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
        body (js->clj (.-body req) :keywordize-keys true)
        updates (cond-> {}
                  (:target_branch body) (assoc :target-branch (:target_branch body))
                  (:project body) (assoc :project (:project body))
                  (:repo_path body) (assoc :repo-path (:repo_path body)))]
    (if-let [session (db/update-session! session-id updates)]
      (json-response res {:session session})
      (error-response res 404 "Session not found"))))

;; Diff endpoints

(defn- get-files-with-size
  "Get file size info for each file in the list."
  [repo-path files]
  (mapv (fn [file-path]
          (let [info (git/get-file-info repo-path file-path false)]
            {:path file-path
             :size (or (:size info) 0)
             :error (:error info)}))
        files))

(defn- get-file-content-for-diff
  "Get file content and convert to diff format. Used for non-git or files without diff."
  [repo-path file-path max-size]
  (let [info (git/get-file-info repo-path file-path true)]
    (if (:error info)
      nil
      (when (<= (:size info) max-size)
        (git/file-to-diff-format file-path (:content info))))))

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
        body (js->clj (.-body req) :keywordize-keys true)
        file-path (:path body)]
    (if-let [session (db/get-session session-id)]
      (let [repo-path (:repo-path session)]
        (git/stage-file! repo-path file-path)
        (let [staged (git/get-staged-files repo-path)
              unstaged (git/get-unstaged-files repo-path)]
          (json-response res {:success true
                              :staged (vec staged)
                              :unstaged (vec unstaged)})))
      (error-response res 404 "Session not found"))))

(defn get-diff-handler
  "GET /api/sessions/:id/diff"
  [^js req res]
  (let [session-id (.. req -params -id)
        ;; Max size threshold - files larger than this won't have content loaded initially
        max-size-threshold 50000]
    (if-let [session (db/get-session session-id)]
      (let [repo-path (:repo-path session)
            is-git-repo (git/git-repo? repo-path)
            files (sessions/compute-file-set session repo-path)
            ;; Get file sizes for all files
            files-with-size (get-files-with-size repo-path files)]
        (if is-git-repo
          ;; Git repo - use git diff
          (let [diff (git/get-diff repo-path (:target-branch session))
                parsed (git/parse-diff-hunks diff)
                changed-files (git/get-changed-files repo-path (:target-branch session))]
            (json-response res {:diff diff
                                :parsed parsed
                                :files files
                                :files-with-size files-with-size
                                :changed-files changed-files
                                :is-git-repo true}))
          ;; Non-git - show full file contents (respecting size threshold)
          (let [parsed (reduce
                        (fn [acc file-path]
                          (if-let [file-diff (get-file-content-for-diff repo-path file-path max-size-threshold)]
                            (conj acc file-diff)
                            acc))
                        []
                        files)]
            (json-response res {:diff nil
                                :parsed parsed
                                :files files
                                :files-with-size files-with-size
                                :changed-files []
                                :is-git-repo false}))))
      (error-response res 404 "Session not found"))))

(defn get-file-diff-handler
  "GET /api/sessions/:id/diff/:file"
  [^js req res]
  (let [session-id (.. req -params -id)
        file (js/decodeURIComponent (.. req -params -file))]
    (if-let [session (db/get-session session-id)]
      (let [repo-path (:repo-path session)
            is-git-repo (git/git-repo? repo-path)]
        (if is-git-repo
          ;; Git repo - use git diff
          (let [diff (git/get-file-diff repo-path (:target-branch session) file)
                parsed (git/parse-diff-hunks diff)]
            (json-response res {:file file
                                :diff diff
                                :parsed parsed
                                :is-git-repo true}))
          ;; Non-git - get full file content
          (let [info (git/get-file-info repo-path file true)]
            (if (:error info)
              (error-response res 404 (:error info))
              (let [parsed (git/file-to-diff-format file (:content info))]
                (json-response res {:file file
                                    :diff nil
                                    :parsed [parsed]
                                    :size (:size info)
                                    :is-git-repo false}))))))
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
    (if-let [session (db/get-session session-id)]
      (let [repo-path (:repo-path session)
            lines (git/get-lines-range repo-path file from-line to-line)]
        (if lines
          (json-response res {:file file
                              :from from-line
                              :to to-line
                              :lines lines})
          (error-response res 404 "Could not read file")))
      (error-response res 404 "Session not found"))))

;; File management endpoints

(defn register-files-handler
  "POST /api/sessions/:id/files"
  [^js req res]
  (let [session-id (.. req -params -id)
        body (js->clj (.-body req) :keywordize-keys true)
        paths (:paths body)
        agent-id (:agent-id body)]
    (let [registered (sessions/register-files! session-id paths agent-id)]
      (json-response res {:registered registered}))))

(defn unregister-files-handler
  "DELETE /api/sessions/:id/files"
  [^js req res]
  (let [session-id (.. req -params -id)
        body (js->clj (.-body req) :keywordize-keys true)
        paths (:paths body)
        agent-id (:agent-id body)]
    (let [unregistered (sessions/unregister-files! session-id paths agent-id)]
      (json-response res {:unregistered unregistered}))))

(defn add-manual-file-handler
  "POST /api/sessions/:id/manual-files"
  [^js req res]
  (let [session-id (.. req -params -id)
        body (js->clj (.-body req) :keywordize-keys true)
        path (:path body)]
    (sessions/add-manual-file! session-id path)
    (json-response res {:success true :path path})))

(defn remove-manual-file-handler
  "DELETE /api/sessions/:id/manual-files"
  [^js req res]
  (let [session-id (.. req -params -id)
        body (js->clj (.-body req) :keywordize-keys true)
        path (:path body)]
    (sessions/remove-manual-file! session-id path)
    (json-response res {:success true :path path})))

(defn restore-file-handler
  "POST /api/sessions/:id/restore-file"
  [^js req res]
  (let [session-id (.. req -params -id)
        body (js->clj (.-body req) :keywordize-keys true)
        path (:path body)]
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
        body (js->clj (.-body req) :keywordize-keys true)]
    (if-let [session (db/get-session session-id)]
      (let [comment (comments/add-comment!
                     {:session-id session-id
                      :parent-id (:parent-id body)
                      :file (:file body)
                      :line (:line body)
                      :text (:text body)
                      :author (:author body)
                      :repo-path (:repo-path session)})]
        (json-response res {:comment comment}))
      (error-response res 404 "Session not found"))))

(defn resolve-comment-handler
  "PATCH /api/comments/:id/resolve"
  [^js req res]
  (let [comment-id (.. req -params -id)
        body (js->clj (.-body req) :keywordize-keys true)
        author (:author body)]
    (comments/resolve-comment! comment-id author)
    (json-response res {:success true})))

(defn unresolve-comment-handler
  "PATCH /api/comments/:id/unresolve"
  [^js req res]
  (let [comment-id (.. req -params -id)
        body (js->clj (.-body req) :keywordize-keys true)
        author (:author body)]
    (comments/unresolve-comment! comment-id author)
    (json-response res {:success true})))

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
  "POST /oauth/register"
  [^js req res]
  (let [body (js->clj (.-body req) :keywordize-keys true)
        client (oauth/register-client
                {:client-id (:client_id body)
                 :client-secret (:client_secret body)
                 :client-name (:client_name body)
                 :redirect-uris (:redirect_uris body)
                 :scope (:scope body)
                 :client-uri (:client_uri body)
                 :logo-uri (:logo_uri body)})]
    (json-response res {:client_id (:client-id client)
                        :client_name (:client-name client)
                        :redirect_uris (:redirect-uris client)
                        :scope (:scope client)
                        :token_endpoint_auth_method "none"
                        :grant_types ["authorization_code" "refresh_token"]
                        :response_types ["code"]})))

(defn oauth-authorize-handler
  "GET /oauth/authorize"
  [^js req res]
  (let [query (.-query req)
        result (oauth/authorize
                {:client-id (.-client_id query)
                 :redirect-uri (.-redirect_uri query)
                 :scopes (when-let [s (.-scope query)]
                           (str/split s #"\s+"))
                 :state (.-state query)
                 :code-challenge (.-code_challenge query)})]
    (if-let [error (:error result)]
      (error-response res 400 error)
      (.redirect res (:redirect-url result)))))

(defn oauth-token-handler
  "POST /oauth/token"
  [^js req res]
  (let [body (js->clj (.-body req) :keywordize-keys true)
        grant-type (:grant_type body)]
    (case grant-type
      "authorization_code"
      (let [result (oauth/exchange-authorization-code
                    {:code (:code body)
                     :client-id (:client_id body)})]
        (if-let [error (:error result)]
          (error-response res 400 error)
          (json-response res result)))

      "refresh_token"
      (let [result (oauth/exchange-refresh-token
                    {:refresh-token (:refresh_token body)
                     :client-id (:client_id body)
                     :scopes (when-let [s (:scope body)]
                               (str/split s #"\s+"))})]
        (if-let [error (:error result)]
          (error-response res 400 error)
          (json-response res result)))

      (error-response res 400 (str "Unsupported grant_type: " grant-type)))))

(defn oauth-revoke-handler
  "POST /oauth/revoke"
  [^js req res]
  (let [body (js->clj (.-body req) :keywordize-keys true)
        result (oauth/revoke-token
                {:token (:token body)
                 :token-type-hint (:token_type_hint body)})]
    (json-response res result)))

;; Route setup

(defn setup-routes [^js app]
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

  ;; OAuth
  (.get app "/.well-known/oauth-authorization-server" oauth-metadata-handler)
  (.get app "/.well-known/oauth-protected-resource" oauth-protected-resource-handler)
  (.post app "/oauth/register" oauth-register-handler)
  (.get app "/oauth/authorize" oauth-authorize-handler)
  (.post app "/oauth/token" oauth-token-handler)
  (.post app "/oauth/revoke" oauth-revoke-handler))
