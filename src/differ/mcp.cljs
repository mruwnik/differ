(ns differ.mcp
  "MCP (Model Context Protocol) JSON-RPC handler over HTTP."
  (:require [differ.sessions :as sessions]
            [differ.comments :as comments]
            [differ.db :as db]
            [differ.git :as git]
            [differ.sse :as sse]
            [differ.util :as util]))

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
                               :project {:type "string"
                                         :description "Remote URL or repo directory name (auto-detected if omitted)"}
                               :branch {:type "string"
                                        :description "Branch name (auto-detected if omitted)"}
                               :target_branch {:type "string"
                                               :description "Branch to diff against (default: main/master)"}}
                  :required ["repo_path"]}}

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

   {:name "get_session_diff"
    :description "Get the diff content for a session. Returns parsed hunks with file paths, line numbers, and changes."
    :inputSchema {:type "object"
                  :properties {:session_id {:type "string"}
                               :file {:type "string"
                                      :description "Optional - get diff for specific file only"}}
                  :required ["session_id"]}}

   {:name "get_file_versions"
    :description "Get the original and modified versions of a file for side-by-side comparison."
    :inputSchema {:type "object"
                  :properties {:session_id {:type "string"}
                               :file {:type "string"
                                      :description "Path to the file relative to repo root"}}
                  :required ["session_id" "file"]}}])

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

(defmethod handle-tool "get_or_create_session" [_ params]
  (let [result (sessions/get-or-create-session params)]
    (if-let [error (:error result)]
      (throw (ex-info error {:code invalid-params}))
      (let [session (:session result)]
        {:session-id (:id session)
         :target-branch (:target-branch session)
         :repo-path (:repo-path session)
         :is-new (:is-new result)}))))

(defmethod handle-tool "register_files" [_ {:keys [session-id paths agent-id]}]
  (let [registered (sessions/register-files! session-id paths agent-id)]
    (sse/emit-files-changed! session-id registered)
    {:registered registered}))

(defmethod handle-tool "unregister_files" [_ {:keys [session-id paths agent-id]}]
  (let [unregistered (sessions/unregister-files! session-id paths agent-id)]
    (sse/emit-files-changed! session-id unregistered)
    {:unregistered unregistered}))

(defmethod handle-tool "get_review_state" [_ {:keys [session-id]}]
  (if-let [session (db/get-session session-id)]
    (let [state (sessions/get-review-state session-id (:repo-path session))]
      (when state
        {:session-id (:session-id state)
         :project (:project state)
         :branch (:branch state)
         :target-branch (:target-branch state)
         :repo-path (:repo-path session)
         :files (:files state)
         :comments (:comments state)}))
    (throw (ex-info "Session not found" {:code invalid-params}))))

(defmethod handle-tool "get_pending_feedback" [_ {:keys [session-id since]}]
  (let [pending (comments/get-pending-feedback session-id since)]
    {:comments pending}))

(defmethod handle-tool "add_comment" [_ {:keys [session-id parent-id] :as params}]
  (if-let [session (db/get-session session-id)]
    (let [;; Check if parent was resolved before adding comment
          parent-was-resolved (when parent-id
                                (:resolved (db/get-comment parent-id)))
          comment (comments/add-comment!
                   (assoc params :repo-path (:repo-path session)))]
      ;; If parent was resolved and now is unresolved, emit unresolve event
      (when parent-was-resolved
        (sse/emit-comment-unresolved! session-id parent-id))
      (sse/emit-comment-added! session-id comment)
      {:comment-id (:id comment)})
    (throw (ex-info "Session not found" {:code invalid-params}))))

(defmethod handle-tool "resolve_comment" [_ {:keys [session-id comment-id author]}]
  (comments/resolve-comment! comment-id author)
  (sse/emit-comment-resolved! session-id comment-id)
  {:success true})

(defmethod handle-tool "unresolve_comment" [_ {:keys [session-id comment-id author]}]
  (comments/unresolve-comment! comment-id author)
  (sse/emit-comment-resolved! session-id comment-id)
  {:success true})

(defmethod handle-tool "get_session_diff" [_ {:keys [session-id file]}]
  (if-let [session (db/get-session session-id)]
    (let [repo-path (:repo-path session)
          target-branch (:target-branch session)
          ;; Get diff - either for specific file or all
          raw-diff (if file
                     (git/get-file-diff repo-path target-branch file)
                     (git/get-diff repo-path target-branch))
          ;; Parse into structured format
          parsed (git/parse-diff-hunks raw-diff)]
      {:session-id session-id
       :target-branch target-branch
       :files (if file
                (when parsed [file])
                (mapv :file-b parsed))
       :diff parsed})
    (throw (ex-info "Session not found" {:code invalid-params}))))

(defmethod handle-tool "get_file_versions" [_ {:keys [session-id file]}]
  (if-let [session (db/get-session session-id)]
    (let [repo-path (:repo-path session)
          target-branch (:target-branch session)
          ;; Get original content from target branch
          original (git/get-file-content repo-path target-branch file)
          ;; Get modified content from working tree
          modified (git/get-file-content repo-path nil file)]
      {:session-id session-id
       :file file
       :original original
       :modified modified
       :is-new (nil? original)
       :is-deleted (nil? modified)})
    (throw (ex-info "Session not found" {:code invalid-params}))))

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
              :text (str "Error: " (.-message e))}]
   :isError true})

(defmethod handle-method "tools/call" [_ params]
  (let [tool-name (:name params)
        ;; Normalize incoming args: snake_case -> kebab-case
        arguments (util/keys->kebab (or (:arguments params) {}))]
    (try
      (let [result (handle-tool tool-name arguments)]
        ;; Check if result is a Promise
        (if (instance? js/Promise result)
          ;; Return Promise that resolves to formatted result
          (.then result format-tool-result format-tool-error)
          ;; Synchronous result
          (format-tool-result result)))
      (catch :default e
        (format-tool-error e)))))

(defmethod handle-method :default [method _]
  (throw (ex-info "Method not found" {:method method :code method-not-found})))

;; HTTP handler

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

(defn mcp-handler
  "POST /mcp - MCP JSON-RPC endpoint"
  [^js req ^js res]
  (let [body (.-body req)]
    (try
      (let [request (js->clj body :keywordize-keys true)
            id (:id request)
            method (:method request)
            params (:params request)]
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
              (send-error! res id code (.-message e))))))
      (catch :default _
        (send-error! res nil parse-error "Parse error")))))

(defn setup-routes [^js app]
  (.post app "/mcp" mcp-handler))
