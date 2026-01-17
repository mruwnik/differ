(ns differ.mcp
  "MCP (Model Context Protocol) JSON-RPC handler over HTTP."
  (:require [differ.sessions :as sessions]
            [differ.db :as db]
            [differ.git :as git]
            [differ.backend.protocol :as proto]
            [differ.sse :as sse]
            [differ.util :as util]
            [differ.pull-request :as pr]
            [differ.oauth :as oauth]
            [clojure.string :as str]))

;; MCP Protocol version
(def protocol-version "2024-11-05")

;; Server info
(def server-info
  {:name "differ"
   :version "0.1.0"})

;; Tool definitions
(def tools
  [{:name "list_sessions"
    :description "List all active review sessions, optionally filtered by project."
    :inputSchema {:type "object"
                  :properties {:project {:type "string"
                                         :description "Filter by project (optional)"}}}}

   {:name "get_or_create_session"
    :description "Get or create a review session for a project/branch. Call once at agent start."
    :inputSchema {:type "object"
                  :properties {:repo_path {:type "string"
                                           :description "Absolute path to the project directory (required)"}
                               :github_pr {:type "string"
                                           :description "GitHub PR URL (e.g., 'https://github.com/owner/repo/pull/123')"}
                               :project {:type "string"
                                         :description "Remote URL or repo directory name (auto-detected if omitted)"}
                               :branch {:type "string"
                                        :description "Branch name (auto-detected if omitted)"}
                               :target_branch {:type "string"
                                               :description "Branch to diff against (default: main/master)"}}}}

   {:name "register_files"
    :description "Register files the agent created or modified. Adds to review set."
    :inputSchema {:type "object"
                  :properties {:session_id {:type "string"}
                               :paths {:type "array"
                                       :items {:type "string"}}
                               :agent_id {:type "string"}}
                  :required ["session_id" "paths" "agent_id"]}}

   {:name "unregister_files"
    :description "Remove files from the review set."
    :inputSchema {:type "object"
                  :properties {:session_id {:type "string"}
                               :paths {:type "array"
                                       :items {:type "string"}}
                               :agent_id {:type "string"}}
                  :required ["session_id" "paths" "agent_id"]}}

   {:name "get_review_state"
    :description "Get current session state including files and all comments."
    :inputSchema {:type "object"
                  :properties {:session_id {:type "string"}}
                  :required ["session_id"]}}

   {:name "get_pending_feedback"
    :description "Get unresolved comments, optionally since a timestamp."
    :inputSchema {:type "object"
                  :properties {:session_id {:type "string"}
                               :since {:type "string"
                                       :description "ISO datetime - only comments after this"}}
                  :required ["session_id"]}}

   {:name "add_comment"
    :description "Add a new comment or reply to provide context or respond to feedback."
    :inputSchema {:type "object"
                  :properties {:session_id {:type "string"}
                               :file {:type "string"}
                               :line {:type "integer"}
                               :text {:type "string"}
                               :author {:type "string"}
                               :model {:type "string"
                                       :description "AI model name (e.g., 'Claude Opus 4.5')"}
                               :parent_id {:type "string"
                                           :description "If replying to a comment"}}
                  :required ["session_id" "text" "author"]}}

   {:name "resolve_comment"
    :description "Mark a comment thread as resolved/addressed."
    :inputSchema {:type "object"
                  :properties {:session_id {:type "string"}
                               :comment_id {:type "string"}
                               :author {:type "string"}}
                  :required ["session_id" "comment_id" "author"]}}

   {:name "unresolve_comment"
    :description "Reopen a previously resolved comment thread."
    :inputSchema {:type "object"
                  :properties {:session_id {:type "string"}
                               :comment_id {:type "string"}
                               :author {:type "string"}}
                  :required ["session_id" "comment_id" "author"]}}

   {:name "submit_review"
    :description "Finish your review with an optional summary comment."
    :inputSchema {:type "object"
                  :properties {:session_id {:type "string"}
                               :body {:type "string"
                                      :description "Optional review summary comment"}
                               :author {:type "string"}
                               :model {:type "string"
                                       :description "AI model name (e.g., 'Claude Opus 4.5')"}}
                  :required ["session_id"]}}

   {:name "get_session_diff"
    :description "Get the diff content for a session. Returns parsed hunks with file paths, line numbers, and changes."
    :inputSchema {:type "object"
                  :properties {:session_id {:type "string"}
                               :file {:type "string"
                                      :description "Optional - get diff for specific file only"}
                               :from {:type "integer"
                                      :description "Start line (1-indexed, inclusive)"}
                               :to {:type "integer"
                                    :description "End line (inclusive)"}}
                  :required ["session_id"]}}

   {:name "get_file_versions"
    :description "Get the original and modified versions of a file for side-by-side comparison."
    :inputSchema {:type "object"
                  :properties {:session_id {:type "string"}
                               :file {:type "string"
                                      :description "Path to the file relative to repo root"}
                               :from {:type "integer"
                                      :description "Start line (1-indexed, inclusive)"}
                               :to {:type "integer"
                                    :description "End line (inclusive)"}}
                  :required ["session_id" "file"]}}

   {:name "get_context"
    :description "Get session context. Local: path, branch. GitHub: PR title, author, state, etc."
    :inputSchema {:type "object"
                  :properties {:session_id {:type "string"}}
                  :required ["session_id"]}}

   {:name "list_directory"
    :description "List directory contents at a ref."
    :inputSchema {:type "object"
                  :properties {:session_id {:type "string"}
                               :ref {:type "string"
                                     :description "Ref: branch/SHA/'base'/'head'/nil for working tree"}
                               :path {:type "string"
                                      :description "Directory path (empty for root)"}}
                  :required ["session_id"]}}

   {:name "get_file_content"
    :description "Get file content at a ref, optionally a specific line range."
    :inputSchema {:type "object"
                  :properties {:session_id {:type "string"}
                               :ref {:type "string"
                                     :description "Ref: branch/SHA/'base'/'head'/nil for working tree"}
                               :file {:type "string"}
                               :from {:type "integer"
                                      :description "Start line (1-indexed, inclusive)"}
                               :to {:type "integer"
                                    :description "End line (inclusive)"}}
                  :required ["session_id" "file"]}}

   {:name "get_history"
    :description "Get commit/change history for the session."
    :inputSchema {:type "object"
                  :properties {:session_id {:type "string"}
                               :path {:type "string"
                                      :description "Filter to commits touching path"}
                               :limit {:type "integer"
                                       :description "Max entries (default 50)"}}
                  :required ["session_id"]}}

   {:name "create_pull_request"
    :description "Push current branch to remote and create a GitHub PR. Returns existing PR if one already exists for the branch."
    :inputSchema {:type "object"
                  :properties {:repo_path {:type "string"
                                           :description "Absolute path to the local git repository"}
                               :title {:type "string"
                                       :description "PR title (defaults to last commit message)"}
                               :body {:type "string"
                                      :description "PR description/body"}
                               :base_branch {:type "string"
                                             :description "Base branch to merge into (defaults to main/master)"}
                               :draft {:type "boolean"
                                       :description "Create as draft PR (default: false)"}}
                  :required ["repo_path"]}}])

;; JSON-RPC helpers

(defn- json-rpc-response [id result]
  {:jsonrpc "2.0"
   :id id
   :result result})

(defn- json-rpc-error [id code message & [data]]
  {:jsonrpc "2.0"
   :id id
   :error (cond-> {:code code :message message}
            data (assoc :data data))})

;; Error codes
(def parse-error -32700)
(def invalid-request -32600)
(def method-not-found -32601)
(def invalid-params -32602)
(def internal-error -32603)

;; Helper to get backend for a session

(defn- get-backend
  "Get backend for a session. Returns Promise of {:backend} or {:error} or {:requires-auth}.
   Now async because GitHub sessions need to try tokens with fallback."
  [session-id]
  (if-let [session (db/get-session session-id)]
    (sessions/create-backend session)
    (js/Promise.resolve {:error "Session not found"})))

(defn- with-backend
  "Execute fn with backend for session. Handles auth requirements and errors.
   Returns Promise. The callback f can return a value or Promise."
  [session-id f]
  (-> (get-backend session-id)
      (.then (fn [result]
               (cond
                 (:error result)
                 (throw (ex-info (:error result) {:code invalid-params}))

                 (:requires-auth result)
                 {:requires-auth true
                  :auth-url (:auth-url result)
                  :message (:message result)}

                 (:requires-pat result)
                 {:requires-pat true
                  :message (:message result)
                  :settings-url (:settings-url result)}

                 :else
                 (f (:backend result)))))))

;; Tool handlers

(defmulti handle-tool (fn [tool-name _params] tool-name))

(defmethod handle-tool "list_sessions" [_ params]
  (let [sessions (sessions/list-sessions (:project params))]
    {:sessions (mapv (fn [s]
                       {:session-id (:id s)
                        :project (:project s)
                        :branch (:branch s)
                        :target-branch (:target-branch s)
                        :unresolved-count (:unresolved-count s)
                        :updated-at (:updated-at s)})
                     sessions)}))

(defn- format-session-result
  "Format a successful session creation result."
  [r]
  (let [session (:session r)]
    {:session-id (:id session)
     :session-type (:session-type session)
     :target-branch (:target-branch session)
     :repo-path (:repo-path session)
     :is-new (:is-new r)}))

(defn- format-auth-result
  "Format an auth-required or PAT-required result."
  [r]
  (cond
    (:requires-pat r)
    {:requires-pat true
     :message (:message r)
     :settings-url (:settings-url r)
     :github-pat-url (:github-pat-url r)}

    (:requires-auth r)
    {:requires-auth true
     :message (:message r)
     :auth-url (:auth-url r)}

    :else nil))

(defmethod handle-tool "get_or_create_session" [_ params]
  (let [result (sessions/get-or-create-session params)]
    ;; Handle promise (for GitHub) or plain value (for local)
    (if (instance? js/Promise result)
      (-> result
          (.then (fn [r]
                   (cond
                     ;; Check auth requirements BEFORE error - they may include
                     ;; an :error key for debugging but shouldn't throw
                     (or (:requires-auth r) (:requires-pat r))
                     (format-auth-result r)

                     (:error r)
                     (throw (ex-info (:error r) {:code invalid-params}))

                     :else
                     (format-session-result r)))))
      ;; Synchronous result
      (cond
        (or (:requires-auth result) (:requires-pat result))
        (format-auth-result result)

        (:error result)
        (throw (ex-info (:error result) {:code invalid-params}))

        :else
        (format-session-result result)))))

(defmethod handle-tool "register_files" [_ {:keys [session-id paths agent-id]}]
  (let [registered (sessions/register-files! session-id paths agent-id)]
    (sse/emit-files-changed! session-id registered)
    {:registered registered}))

(defmethod handle-tool "unregister_files" [_ {:keys [session-id paths agent-id]}]
  (let [unregistered (sessions/unregister-files! session-id paths agent-id)]
    (sse/emit-files-changed! session-id unregistered)
    {:unregistered unregistered}))

(defmethod handle-tool "get_review_state" [_ {:keys [session-id]}]
  (with-backend session-id
    (fn [backend]
      (let [session (db/get-session session-id)
            files-promise (proto/get-changed-files backend)
            comments-promise (proto/get-comments backend)]
        (-> (js/Promise.all #js [files-promise comments-promise])
            (.then (fn [[files comments]]
                     {:session-id session-id
                      :project (:project session)
                      :branch (:branch session)
                      :target-branch (:target-branch session)
                      :repo-path (:repo-path session)
                      :files (mapv :path files)
                      :comments comments})))))))

(defmethod handle-tool "get_pending_feedback" [_ {:keys [session-id since]}]
  (with-backend session-id
    (fn [backend]
      (let [result (proto/get-pending-comments backend {:since since})]
        (if (instance? js/Promise result)
          (-> result (.then (fn [comments] {:comments comments})))
          {:comments result})))))

(defmethod handle-tool "add_comment" [_ {:keys [session-id] :as params}]
  (with-backend session-id
    (fn [backend]
      (let [result (proto/add-comment! backend params)]
        (if (instance? js/Promise result)
          (-> result (.then (fn [comment]
                              (sse/emit-comment-added! session-id comment)
                              {:comment-id (:id comment)})))
          (do
            (sse/emit-comment-added! session-id result)
            {:comment-id (:id result)}))))))

(defmethod handle-tool "resolve_comment" [_ {:keys [session-id comment-id author]}]
  (with-backend session-id
    (fn [backend]
      (let [result (proto/resolve-comment! backend comment-id author)]
        (if (instance? js/Promise result)
          (-> result (.then (fn [_]
                              (sse/emit-comment-resolved! session-id comment-id)
                              {:success true})))
          (do
            (sse/emit-comment-resolved! session-id comment-id)
            {:success true}))))))

(defmethod handle-tool "unresolve_comment" [_ {:keys [session-id comment-id author]}]
  (with-backend session-id
    (fn [backend]
      (let [result (proto/unresolve-comment! backend comment-id author)]
        (if (instance? js/Promise result)
          (-> result (.then (fn [_]
                              (sse/emit-comment-resolved! session-id comment-id)
                              {:success true})))
          (do
            (sse/emit-comment-resolved! session-id comment-id)
            {:success true}))))))

(defmethod handle-tool "submit_review" [_ {:keys [session-id body author model]}]
  (with-backend session-id
    (fn [backend]
      (let [result (proto/submit-review! backend {:body body :author author :model model})]
        (if (instance? js/Promise result)
          result
          (js/Promise.resolve result))))))

(defmethod handle-tool "get_session_diff" [_ {:keys [session-id file from to]}]
  (with-backend session-id
    (fn [backend]
      (let [opts (when (or from to) {:from from :to to})
            diff-result (if file
                          (proto/get-file-diff backend file opts)
                          (proto/get-diff backend opts))]
        ;; Handle promise or value
        (if (instance? js/Promise diff-result)
          (-> diff-result
              (.then (fn [raw-diff]
                       (let [parsed (git/parse-diff-hunks raw-diff)]
                         {:session-id session-id
                          :files (if file
                                   (when parsed [file])
                                   (mapv :file-b parsed))
                          :diff parsed}))))
          (let [parsed (git/parse-diff-hunks diff-result)]
            {:session-id session-id
             :files (if file
                      (when parsed [file])
                      (mapv :file-b parsed))
             :diff parsed}))))))

(defmethod handle-tool "get_file_versions" [_ {:keys [session-id file from to]}]
  (with-backend session-id
    (fn [backend]
      (let [opts (when (or from to) {:from from :to to})
            original (proto/get-file-content backend "base" file opts)
            modified (proto/get-file-content backend nil file opts)]
        ;; Handle promise or value
        (if (or (instance? js/Promise original) (instance? js/Promise modified))
          (-> (js/Promise.all #js [original modified])
              (.then (fn [[orig mod]]
                       {:session-id session-id
                        :file file
                        :original orig
                        :modified mod
                        :is-new (nil? orig)
                        :is-deleted (nil? mod)})))
          {:session-id session-id
           :file file
           :original original
           :modified modified
           :is-new (nil? original)
           :is-deleted (nil? modified)})))))

(defmethod handle-tool "get_context" [_ {:keys [session-id]}]
  (with-backend session-id
    (fn [backend]
      (let [result (proto/get-context backend)]
        (if (instance? js/Promise result)
          result
          result)))))

(defmethod handle-tool "list_directory" [_ {:keys [session-id ref path]}]
  (with-backend session-id
    (fn [backend]
      (let [result (proto/list-directory backend ref (or path ""))]
        (if (instance? js/Promise result)
          (-> result (.then (fn [entries] {:entries entries})))
          {:entries result})))))

(defmethod handle-tool "get_file_content" [_ {:keys [session-id ref file from to]}]
  (with-backend session-id
    (fn [backend]
      (let [opts (when (or from to) {:from from :to to})
            result (proto/get-file-content backend ref file opts)]
        (if (instance? js/Promise result)
          (-> result (.then (fn [content]
                              {:session-id session-id
                               :file file
                               :content content})))
          {:session-id session-id
           :file file
           :content result})))))

(defmethod handle-tool "get_history" [_ {:keys [session-id path limit]}]
  (with-backend session-id
    (fn [backend]
      (let [opts (cond-> {}
                   path (assoc :path path)
                   limit (assoc :limit limit))
            result (proto/get-history backend opts)]
        (if (instance? js/Promise result)
          (-> result (.then (fn [entries] {:entries entries})))
          {:entries result})))))

(defmethod handle-tool "create_pull_request" [_ {:keys [repo-path title body base-branch draft]}]
  ;; Validate required parameter before calling downstream
  (when-not (and (some? repo-path) (string? repo-path) (seq repo-path))
    (throw (ex-info "repo_path is required and must be a non-empty string"
                    {:code invalid-params})))
  (-> (pr/create-pull-request! {:repo-path repo-path
                                :title title
                                :body body
                                :base-branch base-branch
                                :draft draft})
      (.then (fn [result]
               (if (:error result)
                 (throw (ex-info (:error result)
                                 {:code (or (:code result) invalid-params)}))
                 result)))))

(defmethod handle-tool :default [tool-name _]
  (throw (ex-info "Unknown tool" {:tool tool-name})))

;; MCP method handlers

(defmulti handle-method (fn [method _params] method))

(defmethod handle-method "initialize" [_ params]
  {:protocolVersion protocol-version
   :capabilities {:tools {}}
   :serverInfo server-info})

(defmethod handle-method "tools/list" [_ _]
  {:tools tools})

(defn- format-tool-result [result]
  (let [normalized (util/keys->snake result)]
    {:content [{:type "text"
                :text (js/JSON.stringify (clj->js normalized) nil 2)}]}))

(defn- format-tool-error [e]
  {:content [{:type "text"
              :text (str "Error: " (or (.-message e) (ex-message e) (str e)))}]
   :isError true})

(defn- ensure-promise
  "Wrap value in a Promise if it isn't already one."
  [x]
  (if (instance? js/Promise x)
    x
    (js/Promise.resolve x)))

(defmethod handle-method "tools/call" [_ params]
  (let [tool-name (:name params)
        ;; Normalize incoming args: snake_case -> kebab-case
        arguments (util/keys->kebab (or (:arguments params) {}))]
    (try
      (let [result (handle-tool tool-name arguments)]
        ;; Normalize to Promise for consistent handling
        (-> (ensure-promise result)
            (.then format-tool-result)
            (.catch format-tool-error)))
      (catch :default e
        (format-tool-error e)))))

(defmethod handle-method :default [method _]
  (throw (ex-info "Method not found" {:method method :code method-not-found})))

;; HTTP handler

(defn- extract-bearer-token
  "Extract Bearer token from Authorization header."
  [^js req]
  (when-let [auth-header (.. req -headers -authorization)]
    (when (str/starts-with? auth-header "Bearer ")
      (subs auth-header 7))))

(defn- send-response! [^js res id result]
  (let [response (json-rpc-response id result)]
    (-> res
        (.status 200)
        (.json (clj->js response)))))

(defn- send-error! [^js res id code message]
  (let [response (json-rpc-error id code message)]
    (-> res
        (.status 200)
        (.json (clj->js response)))))

;; Methods that don't require authentication (MCP protocol handshake)
(def unauthenticated-methods
  #{"initialize" "tools/list"})

(defn- method-requires-auth?
  "Check if a method requires authentication.
   MCP protocol methods like 'initialize' and 'tools/list' must work
   without auth since clients call them before obtaining tokens."
  [method]
  (not (contains? unauthenticated-methods method)))

(defn mcp-handler
  "POST /mcp - MCP JSON-RPC endpoint
   Most methods require Bearer token authentication via API key or OAuth access token.
   Protocol methods (initialize, tools/list) are allowed without auth."
  [^js req ^js res]
  (let [body (.-body req)]
    (try
      (let [request (js->clj body :keywordize-keys true)
            id (:id request)
            method (:method request)
            params (:params request)
            token (extract-bearer-token req)]
        ;; Check auth for methods that require it
        (if (and (method-requires-auth? method)
                 (not (oauth/verify-token token)))
          ;; No valid token for authenticated method - return 401
          (-> res
              (.status 401)
              (.json #js {:error "Unauthorized"
                          :message "Valid Bearer token required"}))
          ;; Auth OK or not required - process request
          (try
            (let [result (handle-method method params)]
              ;; Handle Promise results (for future async tools)
              (if (instance? js/Promise result)
                (-> result
                    (.then (fn [r] (send-response! res id r)))
                    (.catch (fn [e]
                              (let [data (ex-data e)
                                    code (or (:code data) internal-error)]
                                (send-error! res id code (.-message e))))))
                ;; Synchronous result
                (send-response! res id result)))
            (catch :default e
              (let [data (ex-data e)
                    code (or (:code data) internal-error)]
                (send-error! res id code (.-message e)))))))
      (catch :default _
        (send-error! res nil parse-error "Parse error")))))

(defn setup-routes [^js app]
  (.post app "/mcp" mcp-handler))
