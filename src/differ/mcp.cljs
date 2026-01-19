(ns differ.mcp
  "MCP (Model Context Protocol) JSON-RPC handler over HTTP."
  (:require [differ.sessions :as sessions]
            [differ.db :as db]
            [differ.git :as git]
            [differ.backend.protocol :as proto]
            [differ.sse :as sse]
            [differ.util :as util]
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
    :description "Get unresolved comments and CI status. Returns {comments: [...], ci: {state, checks}}."
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

   {:name "request_review"
    :description "Request external review of changes. Pushes branch and makes changes reviewable. For local sessions, creates a GitHub PR. For GitHub sessions, optionally pushes local commits (if repo_path provided) and returns existing PR info. Returns a review session ID for collaborative review."
    :inputSchema {:type "object"
                  :properties {:session_id {:type "string"
                                            :description "Session ID for the review"}
                               :repo_path {:type "string"
                                           :description "Local repo path (optional; enables pushing local commits for GitHub sessions)"}
                               :title {:type "string"
                                       :description "PR title (defaults to last commit message, local sessions only)"}
                               :body {:type "string"
                                      :description "PR description/body (local sessions only)"}
                               :draft {:type "boolean"
                                       :description "Create as draft PR (default: false, local sessions only)"}}
                  :required ["session_id"]}}])

;; Error codes
(def parse-error -32700)
(def invalid-request -32600)
(def method-not-found -32601)
(def invalid-params -32602)
(def internal-error -32603)

;; Tool schema lookup
(def tools-by-name
  "Map of tool name -> tool definition for schema lookup."
  (into {} (map (fn [t] [(:name t) t]) tools)))

;; Input validation

(defn- get-json-type
  "Return JSON schema type for a value."
  [v]
  (cond
    (nil? v) nil
    (string? v) "string"
    (boolean? v) "boolean"
    (int? v) "integer"
    (number? v) "number"
    (vector? v) "array"
    (sequential? v) "array"
    (map? v) "object"
    :else nil))

(defn- validate-type
  "Validate that a value matches the expected JSON schema type.
   Returns nil if valid, error message if invalid."
  [field-name value expected-type]
  (when-not (nil? value)
    (let [actual-type (get-json-type value)]
      ;; Allow integer where number is expected
      (when-not (or (= actual-type expected-type)
                    (and (= expected-type "number") (= actual-type "integer")))
        (str "Field '" field-name "' expected " expected-type " but got " (or actual-type (type value)))))))

(defn- schema-key->arg-key
  "Convert a schema field name (string or keyword with underscores) to an arg key (kebab-case keyword).
   Schema: :session_id or \"session_id\" -> Args: :session-id"
  [field]
  (let [field-str (if (keyword? field) (name field) field)]
    (keyword (util/snake->kebab field-str))))

(defn- validate-required-fields
  "Validate that all required fields are present and have correct types.
   Throws ex-info with :code :invalid-params on validation failure."
  [tool-name args schema]
  (let [required (get schema :required [])
        properties (get schema :properties {})]
    ;; Check required fields are present (required is a vector of strings)
    (doseq [field required]
      (let [field-kw (schema-key->arg-key field)
            value (get args field-kw)]
        (when (nil? value)
          (throw (ex-info (str "Missing required field: " field)
                          {:code invalid-params
                           :tool tool-name
                           :field field})))))
    ;; Check types for all provided fields (properties has keyword keys)
    (doseq [[field-name field-schema] properties]
      (let [field-kw (schema-key->arg-key field-name)
            value (get args field-kw)
            expected-type (get field-schema :type)
            ;; Use the original keyword name for error messages
            display-name (if (keyword? field-name) (name field-name) field-name)]
        (when-let [type-error (validate-type display-name value expected-type)]
          (throw (ex-info type-error
                          {:code invalid-params
                           :tool tool-name
                           :field display-name
                           :expected expected-type
                           :actual (get-json-type value)})))))))

(defn- validate-tool-args
  "Validate arguments for a tool call. Returns args if valid, throws on error."
  [tool-name args]
  (if-let [tool (get tools-by-name tool-name)]
    (let [schema (:inputSchema tool)]
      (validate-required-fields tool-name args schema)
      args)
    ;; Unknown tool - let handle-tool :default handle it
    args))

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

(defn- ensure-promise
  "Wrap value in a Promise if it isn't already one."
  [x]
  (if (instance? js/Promise x)
    x
    (js/Promise.resolve x)))

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
      (let [session (db/get-session session-id)]
        ;; Fetch context, files, and comments via protocol
        (-> (js/Promise.all #js [(proto/get-context backend)
                                 (proto/get-changed-files backend)
                                 (proto/get-comments backend)])
            (.then (fn [[ctx files comments]]
                     (cond-> {:session-id session-id
                              :project (:project session)
                              :branch (or (:head-branch ctx) (:branch session))
                              :target-branch (or (:base-branch ctx) (:target-branch session))
                              :repo-path (:repo-path session)
                              :files (mapv :path files)
                              :comments comments
                              :unresolved-count (count (remove :resolved comments))}
                       ;; Include state/title for GitHub sessions
                       (:state ctx) (assoc :state (:state ctx))
                       (:title ctx) (assoc :title (:title ctx))))))))))

(defmethod handle-tool "get_pending_feedback" [_ {:keys [session-id since]}]
  (with-backend session-id
    (fn [backend]
      (-> (js/Promise.all
           #js [(ensure-promise (proto/get-pending-comments backend {:since since}))
                (ensure-promise (proto/get-ci-status backend))])
          (.then (fn [results]
                   (let [[comments ci-status] results]
                     {:comments comments :ci ci-status})))))))

(defmethod handle-tool "add_comment" [_ {:keys [session-id] :as params}]
  (with-backend session-id
    (fn [backend]
      (-> (ensure-promise (proto/add-comment! backend params))
          (.then (fn [comment]
                   (sse/emit-comment-added! session-id comment)
                   {:comment-id (:id comment)}))))))

(defmethod handle-tool "resolve_comment" [_ {:keys [session-id comment-id author]}]
  (with-backend session-id
    (fn [backend]
      (-> (ensure-promise (proto/resolve-comment! backend comment-id author))
          (.then (fn [_]
                   (sse/emit-comment-resolved! session-id comment-id)
                   {:success true}))))))

(defmethod handle-tool "unresolve_comment" [_ {:keys [session-id comment-id author]}]
  (with-backend session-id
    (fn [backend]
      (-> (ensure-promise (proto/unresolve-comment! backend comment-id author))
          (.then (fn [_]
                   (sse/emit-comment-unresolved! session-id comment-id)
                   {:success true}))))))

(defmethod handle-tool "submit_review" [_ {:keys [session-id body author model]}]
  (with-backend session-id
    (fn [backend]
      (ensure-promise (proto/submit-review! backend {:body body :author author :model model})))))

(defmethod handle-tool "get_session_diff" [_ {:keys [session-id file from to]}]
  (with-backend session-id
    (fn [backend]
      (let [opts (when (or from to) {:from from :to to})
            diff-result (if file
                          (proto/get-file-diff backend file opts)
                          (proto/get-diff backend opts))]
        (-> (ensure-promise diff-result)
            (.then (fn [raw-diff]
                     (let [parsed (git/parse-diff-hunks raw-diff)]
                       {:session-id session-id
                        :files (if file
                                 (when parsed [file])
                                 (mapv :file-b parsed))
                        :diff parsed}))))))))

(defmethod handle-tool "get_file_versions" [_ {:keys [session-id file from to]}]
  (with-backend session-id
    (fn [backend]
      (let [opts (when (or from to) {:from from :to to})
            original (proto/get-file-content backend "base" file opts)
            modified (proto/get-file-content backend nil file opts)]
        (-> (js/Promise.all #js [(ensure-promise original) (ensure-promise modified)])
            (.then (fn [[orig mod]]
                     {:session-id session-id
                      :file file
                      :original orig
                      :modified mod
                      :is-new (nil? orig)
                      :is-deleted (nil? mod)})))))))

(defmethod handle-tool "get_context" [_ {:keys [session-id]}]
  (with-backend session-id
    (fn [backend]
      (proto/get-context backend))))

(defmethod handle-tool "list_directory" [_ {:keys [session-id ref path]}]
  (with-backend session-id
    (fn [backend]
      (-> (ensure-promise (proto/list-directory backend ref (or path "")))
          (.then (fn [entries] {:entries entries}))))))

(defmethod handle-tool "get_file_content" [_ {:keys [session-id ref file from to]}]
  (with-backend session-id
    (fn [backend]
      (let [opts (when (or from to) {:from from :to to})]
        (-> (ensure-promise (proto/get-file-content backend ref file opts))
            (.then (fn [content]
                     {:session-id session-id
                      :file file
                      :content content})))))))

(defmethod handle-tool "get_history" [_ {:keys [session-id path limit]}]
  (with-backend session-id
    (fn [backend]
      (let [opts (cond-> {}
                   path (assoc :path path)
                   limit (assoc :limit limit))]
        (-> (ensure-promise (proto/get-history backend opts))
            (.then (fn [entries] {:entries entries})))))))

(defmethod handle-tool "request_review" [_ {:keys [session-id repo-path title body draft]}]
  (if-let [session (db/get-session session-id)]
    (sessions/request-review! session {:repo-path repo-path :title title :body body :draft draft})
    (throw (ex-info "Session not found" {:code :invalid-params :session-id session-id}))))

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

(defmethod handle-method "tools/call" [_ params]
  (let [tool-name (:name params)
        ;; Normalize incoming args: snake_case -> kebab-case
        arguments (util/keys->kebab (or (:arguments params) {}))]
    (try
      ;; Validate required fields and types before dispatch
      (validate-tool-args tool-name arguments)
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
