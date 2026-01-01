(ns differ.mcp
  "MCP (Model Context Protocol) JSON-RPC handler over HTTP."
  (:require [differ.sessions :as sessions]
            [differ.comments :as comments]
            [differ.db :as db]
            [differ.sse :as sse]
            [clojure.walk :as walk]))

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
                  :required ["session_id" "comment_id" "author"]}}])

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
                       {:session_id (:id s)
                        :project (:project s)
                        :branch (:branch s)
                        :target_branch (:target-branch s)
                        :unresolved_count (:unresolved-count s)
                        :updated_at (:updated-at s)})
                     sessions)}))

(defmethod handle-tool "get_or_create_session" [_ params]
  (let [result (sessions/get-or-create-session
                {:repo-path (:repo_path params)
                 :project (:project params)
                 :branch (:branch params)
                 :target-branch (:target_branch params)})]
    (if-let [error (:error result)]
      (throw (ex-info error {:code invalid-params}))
      (let [session (:session result)]
        {:session_id (:id session)
         :target_branch (:target-branch session)
         :repo_path (:repo-path session)
         :is_new (:is-new result)}))))

(defmethod handle-tool "register_files" [_ params]
  (let [registered (sessions/register-files!
                    (:session_id params)
                    (:paths params)
                    (:agent_id params))]
    (sse/emit-files-changed! (:session_id params) registered)
    {:registered registered}))

(defmethod handle-tool "unregister_files" [_ params]
  (let [unregistered (sessions/unregister-files!
                      (:session_id params)
                      (:paths params)
                      (:agent_id params))]
    (sse/emit-files-changed! (:session_id params) unregistered)
    {:unregistered unregistered}))

(defmethod handle-tool "get_review_state" [_ params]
  (if-let [session (db/get-session (:session_id params))]
    (let [state (sessions/get-review-state (:session_id params) (:repo-path session))]
      (when state
        {:session_id (:session-id state)
         :project (:project state)
         :branch (:branch state)
         :target_branch (:target-branch state)
         :repo_path (:repo-path session)
         :files (:files state)
         :comments (mapv #(walk/stringify-keys %) (:comments state))}))
    (throw (ex-info "Session not found" {:code invalid-params}))))

(defmethod handle-tool "get_pending_feedback" [_ params]
  (let [pending (comments/get-pending-feedback
                 (:session_id params)
                 (:since params))]
    {:comments (mapv #(walk/stringify-keys %) pending)}))

(defmethod handle-tool "add_comment" [_ params]
  (if-let [session (db/get-session (:session_id params))]
    (let [comment (comments/add-comment!
                   {:session-id (:session_id params)
                    :parent-id (:parent_id params)
                    :file (:file params)
                    :line (:line params)
                    :text (:text params)
                    :author (:author params)
                    :repo-path (:repo-path session)})]
      (sse/emit-comment-added! (:session_id params) comment)
      {:comment_id (:id comment)})
    (throw (ex-info "Session not found" {:code invalid-params}))))

(defmethod handle-tool "resolve_comment" [_ params]
  (comments/resolve-comment! (:comment_id params) (:author params))
  (sse/emit-comment-resolved! (:session_id params) (:comment_id params))
  {:success true})

(defmethod handle-tool "unresolve_comment" [_ params]
  (comments/unresolve-comment! (:comment_id params) (:author params))
  (sse/emit-comment-resolved! (:session_id params) (:comment_id params))
  {:success true})

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

(defmethod handle-method "tools/call" [_ params]
  (let [tool-name (:name params)
        arguments (or (:arguments params) {})]
    (try
      (let [result (handle-tool tool-name arguments)]
        {:content [{:type "text"
                    :text (js/JSON.stringify (clj->js result) nil 2)}]})
      (catch :default e
        {:content [{:type "text"
                    :text (str "Error: " (.-message e))}]
         :isError true}))))

(defmethod handle-method :default [method _]
  (throw (ex-info "Method not found" {:method method :code method-not-found})))

;; HTTP handler

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
          (let [result (handle-method method params)
                response (json-rpc-response id result)]
            (-> res
                (.status 200)
                (.json (clj->js response))))
          (catch :default e
            (let [data (ex-data e)
                  code (or (:code data) internal-error)
                  response (json-rpc-error id code (.-message e))]
              (-> res
                  (.status 200)
                  (.json (clj->js response)))))))
      (catch :default _
        (-> res
            (.status 200)
            (.json (clj->js (json-rpc-error nil parse-error "Parse error"))))))))

(defn setup-routes [^js app]
  (.post app "/mcp" mcp-handler))
