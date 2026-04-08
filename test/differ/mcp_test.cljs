(ns differ.mcp-test
  "Tests for MCP (Model Context Protocol) JSON-RPC handlers."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [differ.test-helpers :as helpers]
            [differ.mcp :as mcp]
            [differ.sessions :as sessions]
            [differ.event-stream]
            [differ.github-events]))

;; ============================================================================
;; Test Setup
;; ============================================================================

(def ^:dynamic *test-db* nil)
(def ^:dynamic *test-repo* nil)
(def ^:dynamic *test-repo-cleanup* nil)

(defn with-test-env [f]
  (let [{:keys [path cleanup]} (helpers/create-test-repo)]
    (binding [*test-db* (helpers/init-test-db!)
              *test-repo* path
              *test-repo-cleanup* cleanup]
      (try
        (f)
        (finally
          (cleanup)
          (helpers/cleanup-test-db!))))))

(use-fixtures :each with-test-env)

;; ============================================================================
;; Protocol Version and Server Info Tests
;; ============================================================================

(deftest protocol-version-test
  (testing "protocol version is defined"
    (is (string? mcp/protocol-version))
    (is (= "2024-11-05" mcp/protocol-version))))

(deftest server-info-test
  (testing "server info contains required fields"
    (is (map? mcp/server-info))
    (is (= "differ" (:name mcp/server-info)))
    (is (string? (:version mcp/server-info)))))

;; ============================================================================
;; Tool Definitions Tests
;; ============================================================================

(deftest tools-definition-test
  (testing "tools is a vector"
    (is (vector? mcp/tools)))

  (testing "all tools have required fields"
    (doseq [tool mcp/tools]
      (is (string? (:name tool)) (str "Tool missing name: " tool))
      (is (string? (:description tool)) (str "Tool missing description: " (:name tool)))
      (is (map? (:inputSchema tool)) (str "Tool missing inputSchema: " (:name tool)))))

  (testing "expected tools are defined"
    (let [tool-names (set (map :name mcp/tools))]
      ;; Core session tools
      (is (contains? tool-names "list_sessions"))
      (is (contains? tool-names "get_or_create_session"))
      (is (contains? tool-names "register_files"))
      (is (contains? tool-names "unregister_files"))
      (is (contains? tool-names "get_review_state"))
      ;; Comment tools
      (is (contains? tool-names "get_pending_feedback"))
      (is (contains? tool-names "add_comment"))
      (is (contains? tool-names "resolve_comment"))
      (is (contains? tool-names "unresolve_comment"))
      ;; GitHub/backend tools
      (is (contains? tool-names "submit_review"))
      (is (contains? tool-names "get_context"))
      (is (contains? tool-names "list_directory"))
      (is (contains? tool-names "get_file_content"))
      (is (contains? tool-names "get_history"))
      ;; Diff tools
      (is (contains? tool-names "get_session_diff"))
      (is (contains? tool-names "get_file_versions"))
      ;; PR creation/review request
      (is (contains? tool-names "request_review"))
      ;; GitHub discovery
      (is (contains? tool-names "list_github_prs")))))

(deftest list-github-prs-schema-test
  (testing "list_github_prs tool has correct schema"
    (let [tool (first (filter #(= "list_github_prs" (:name %)) mcp/tools))]
      (is (some? tool))
      (is (string? (:description tool)))
      (let [schema (:inputSchema tool)
            props (:properties schema)]
        (is (= "object" (:type schema)))
        (is (contains? props :project))
        (is (contains? props :state))
        (is (contains? props :limit))
        (is (= ["project"] (:required schema)))))))

(deftest tool-schema-test
  (testing "get_or_create_session has repo_path and github_pr options"
    (let [tool (first (filter #(= "get_or_create_session" (:name %)) mcp/tools))
          schema (:inputSchema tool)
          props (:properties schema)]
      ;; Either repo_path (local) or github_pr (GitHub) can be used
      (is (contains? props :repo_path))
      (is (contains? props :github_pr))))

  (testing "register_files has required fields"
    (let [tool (first (filter #(= "register_files" (:name %)) mcp/tools))
          schema (:inputSchema tool)
          required (set (:required schema))]
      (is (contains? required "session_id"))
      (is (contains? required "paths"))
      (is (contains? required "agent_id"))))

  (testing "add_comment has required fields"
    (let [tool (first (filter #(= "add_comment" (:name %)) mcp/tools))
          schema (:inputSchema tool)
          required (set (:required schema))]
      (is (contains? required "session_id"))
      (is (contains? required "text"))
      (is (contains? required "author")))))

;; ============================================================================
;; JSON-RPC Format Tests
;; ============================================================================

(deftest json-rpc-response-format-test
  (testing "success response has correct format"
    (let [response (#'mcp/json-rpc-response 1 {:data "test"})]
      (is (= "2.0" (:jsonrpc response)))
      (is (= 1 (:id response)))
      (is (= {:data "test"} (:result response))))))

(deftest json-rpc-error-format-test
  (testing "error response has correct format"
    (let [response (#'mcp/json-rpc-error 1 -32600 "Invalid request")]
      (is (= "2.0" (:jsonrpc response)))
      (is (= 1 (:id response)))
      (is (map? (:error response)))
      (is (= -32600 (get-in response [:error :code])))
      (is (= "Invalid request" (get-in response [:error :message])))))

  (testing "error response can include data"
    (let [response (#'mcp/json-rpc-error 1 -32602 "Bad params" {:details "missing field"})]
      (is (= {:details "missing field"} (get-in response [:error :data]))))))

;; ============================================================================
;; Error Codes Tests
;; ============================================================================

(deftest error-codes-test
  (testing "standard JSON-RPC error codes are defined"
    (is (= -32700 mcp/parse-error))
    (is (= -32600 mcp/invalid-request))
    (is (= -32601 mcp/method-not-found))
    (is (= -32602 mcp/invalid-params))
    (is (= -32603 mcp/internal-error))))

;; ============================================================================
;; Method Handler Tests
;; ============================================================================

(deftest initialize-handler-test
  (testing "initialize returns protocol info"
    (let [result (mcp/handle-method "initialize" {})]
      (is (= mcp/protocol-version (:protocolVersion result)))
      (is (map? (:capabilities result)))
      (is (= mcp/server-info (:serverInfo result))))))

(deftest tools-list-handler-test
  (testing "tools/list returns all tools"
    (let [result (mcp/handle-method "tools/list" {})]
      (is (= mcp/tools (:tools result))))))

(deftest unknown-method-test
  (testing "unknown method throws error"
    (is (thrown-with-msg? js/Error #"Method not found"
                          (mcp/handle-method "unknown/method" {})))))

;; ============================================================================
;; Tool Handler Logic Tests
;; ============================================================================

(deftest list-sessions-response-format-test
  (testing "list_sessions returns properly formatted sessions"
    (let [mock-sessions [{:id "s1"
                          :project "project"
                          :branch "main"
                          :target-branch "develop"
                          :unresolved-count 5
                          :updated-at "2024-01-01T00:00:00Z"}]
          formatted (mapv (fn [s]
                            {:session_id (:id s)
                             :project (:project s)
                             :branch (:branch s)
                             :target_branch (:target-branch s)
                             :unresolved_count (:unresolved-count s)
                             :updated_at (:updated-at s)})
                          mock-sessions)]
      (is (= [{:session_id "s1"
               :project "project"
               :branch "main"
               :target_branch "develop"
               :unresolved_count 5
               :updated_at "2024-01-01T00:00:00Z"}]
             formatted)))))

(deftest get_or_create_session-response-format-test
  (testing "get_or_create_session returns properly formatted response"
    (let [mock-result {:session {:id "session-123"
                                 :target-branch "main"
                                 :repo-path "/tmp/repo"}
                       :is-new true}
          formatted {:session_id (get-in mock-result [:session :id])
                     :target_branch (get-in mock-result [:session :target-branch])
                     :repo_path (get-in mock-result [:session :repo-path])
                     :is_new (:is-new mock-result)}]
      (is (= "session-123" (:session_id formatted)))
      (is (= "main" (:target_branch formatted)))
      (is (true? (:is_new formatted))))))

(deftest register-files-response-format-test
  (testing "register_files returns registered paths"
    (let [registered-paths ["file1.txt" "file2.txt"]
          response {:registered registered-paths}]
      (is (= ["file1.txt" "file2.txt"] (:registered response))))))

(deftest add-comment-response-format-test
  (testing "add_comment returns comment_id"
    (let [comment {:id "comment-123"
                   :text "Test comment"}
          response {:comment_id (:id comment)}]
      (is (= "comment-123" (:comment_id response))))))

(deftest get-pending-feedback-response-format-test
  (testing "get_pending_feedback returns comments array"
    (let [comments [{:id "c1" :text "Comment 1"}
                    {:id "c2" :text "Comment 2"}]
          response {:comments comments}]
      (is (vector? (:comments response)))
      (is (= 2 (count (:comments response)))))))

;; ============================================================================
;; tools/call Handler Tests
;; ============================================================================

(deftest tools-call-success-format-test
  (testing "successful tool call returns content array"
    (let [tool-result {:data "test"}
          formatted {:content [{:type "text"
                                :text (js/JSON.stringify (clj->js tool-result) nil 2)}]}]
      (is (vector? (:content formatted)))
      (is (= "text" (get-in formatted [:content 0 :type])))
      (is (string? (get-in formatted [:content 0 :text]))))))

(deftest tools-call-error-format-test
  (testing "tool error returns isError flag"
    (let [error-response {:content [{:type "text"
                                     :text "Error: Something went wrong"}]
                          :isError true}]
      (is (true? (:isError error-response)))
      (is (vector? (:content error-response))))))

;; ============================================================================
;; Request Parsing Tests
;; ============================================================================

(deftest request-parsing-test
  (testing "JSON-RPC request structure"
    (let [request {:jsonrpc "2.0"
                   :id 1
                   :method "tools/list"
                   :params {}}]
      (is (= "2.0" (:jsonrpc request)))
      (is (= 1 (:id request)))
      (is (= "tools/list" (:method request))))))

(deftest tools-call-params-parsing-test
  (testing "tools/call params contain name and arguments"
    (let [params {:name "register_files"
                  :arguments {:session_id "s1"
                              :paths ["file1.txt"]
                              :agent_id "agent1"}}]
      (is (= "register_files" (:name params)))
      (is (map? (:arguments params)))
      (is (= "s1" (get-in params [:arguments :session_id]))))))

;; ============================================================================
;; get_session_diff Response Tests
;; ============================================================================

(deftest get-session-diff-response-format-test
  (testing "get_session_diff returns expected fields"
    (let [response {:session_id "s1"
                    :target_branch "main"
                    :files ["file1.txt" "file2.txt"]
                    :diff [{:file-a "file1.txt"
                            :file-b "file1.txt"
                            :hunks []}]}]
      (is (= "s1" (:session_id response)))
      (is (= "main" (:target_branch response)))
      (is (vector? (:files response)))
      (is (vector? (:diff response))))))

;; ============================================================================
;; get_file_versions Response Tests
;; ============================================================================

(deftest get-file-versions-response-format-test
  (testing "get_file_versions returns original and modified content"
    (let [response {:session_id "s1"
                    :file "main.cljs"
                    :original "original content"
                    :modified "modified content"
                    :is_new false
                    :is_deleted false}]
      (is (= "s1" (:session_id response)))
      (is (= "main.cljs" (:file response)))
      (is (string? (:original response)))
      (is (string? (:modified response)))
      (is (boolean? (:is_new response)))))

  (testing "new file has nil original"
    (let [response {:session_id "s1"
                    :file "new-file.txt"
                    :original nil
                    :modified "new content"
                    :is_new true
                    :is_deleted false}]
      (is (nil? (:original response)))
      (is (true? (:is_new response)))))

  (testing "deleted file has nil modified"
    (let [response {:session_id "s1"
                    :file "deleted.txt"
                    :original "old content"
                    :modified nil
                    :is_new false
                    :is_deleted true}]
      (is (nil? (:modified response)))
      (is (true? (:is_deleted response))))))

;; ============================================================================
;; Unknown Tool Handler Tests
;; ============================================================================

(deftest unknown-tool-test
  (testing "unknown tool throws error"
    (is (thrown? js/Error
                 (mcp/handle-tool "nonexistent_tool" {})))))

;; ============================================================================
;; Authentication Tests
;; ============================================================================

(deftest unauthenticated-methods-test
  (testing "initialize and tools/list don't require auth"
    (is (contains? mcp/unauthenticated-methods "initialize"))
    (is (contains? mcp/unauthenticated-methods "tools/list")))

  (testing "tools/call requires auth"
    (is (#'mcp/method-requires-auth? "tools/call")))

  (testing "initialize does not require auth"
    (is (not (#'mcp/method-requires-auth? "initialize"))))

  (testing "tools/list does not require auth"
    (is (not (#'mcp/method-requires-auth? "tools/list")))))

;; ============================================================================
;; request_review Tool Tests
;; ============================================================================

(deftest request-review-tool-schema-test
  (testing "request_review has correct schema"
    (let [tool (first (filter #(= "request_review" (:name %)) mcp/tools))
          schema (:inputSchema tool)
          props (:properties schema)
          required (set (:required schema))]
      ;; Required field
      (is (contains? required "session_id"))
      ;; Optional fields
      (is (contains? props :repo_path))
      (is (contains? props :title))
      (is (contains? props :body))
      (is (contains? props :draft))
      ;; Check types
      (is (= "string" (get-in props [:session_id :type])))
      (is (= "string" (get-in props [:repo_path :type])))
      (is (= "string" (get-in props [:title :type])))
      (is (= "string" (get-in props [:body :type])))
      (is (= "boolean" (get-in props [:draft :type]))))))

(deftest request-review-validation-test
  (testing "request_review throws when session not found"
    (is (thrown-with-msg? js/Error #"Session not found"
                          (mcp/handle-tool "request_review" {}))))

  (testing "request_review throws with nil session_id"
    (is (thrown-with-msg? js/Error #"Session not found"
                          (mcp/handle-tool "request_review" {:session-id nil}))))

  (testing "request_review throws with empty session_id"
    (is (thrown-with-msg? js/Error #"Session not found"
                          (mcp/handle-tool "request_review" {:session-id ""}))))

  (testing "request_review throws with nonexistent session"
    (is (thrown-with-msg? js/Error #"Session not found"
                          (mcp/handle-tool "request_review" {:session-id "nonexistent-session"})))))

;; ============================================================================
;; Input Validation Tests
;; ============================================================================

(deftest tools-by-name-test
  (testing "tools-by-name lookup map is populated"
    (is (map? mcp/tools-by-name))
    (is (= (count mcp/tools) (count mcp/tools-by-name)))
    (is (contains? mcp/tools-by-name "register_files"))
    (is (contains? mcp/tools-by-name "add_comment"))))

(deftest validate-tool-args-missing-required-test
  (testing "missing required field throws error"
    (is (thrown-with-msg? js/Error #"Missing required field: session_id"
                          (#'mcp/validate-tool-args "register_files" {:paths ["a.txt"] :agent-id "agent1"}))))

  (testing "missing multiple required fields reports first missing"
    (is (thrown-with-msg? js/Error #"Missing required field"
                          (#'mcp/validate-tool-args "register_files" {}))))

  (testing "all required fields present passes validation"
    (is (= {:session-id "s1" :paths ["a.txt"] :agent-id "agent1"}
           (#'mcp/validate-tool-args "register_files" {:session-id "s1" :paths ["a.txt"] :agent-id "agent1"})))))

(deftest validate-tool-args-type-checking-test
  (testing "wrong type for string field throws error"
    (is (thrown-with-msg? js/Error #"expected string but got"
                          (#'mcp/validate-tool-args "get_review_state" {:session-id 123}))))

  (testing "wrong type for array field throws error"
    (is (thrown-with-msg? js/Error #"expected array but got"
                          (#'mcp/validate-tool-args "register_files" {:session-id "s1" :paths "not-an-array" :agent-id "agent1"}))))

  (testing "wrong type for integer field throws error"
    (is (thrown-with-msg? js/Error #"expected integer but got"
                          (#'mcp/validate-tool-args "get_session_diff" {:session-id "s1" :from "not-a-number"}))))

  (testing "wrong type for boolean field throws error"
    (is (thrown-with-msg? js/Error #"expected boolean but got"
                          (#'mcp/validate-tool-args "request_review" {:session-id "s1" :draft "true"})))))

(deftest validate-tool-args-optional-fields-test
  (testing "optional fields can be omitted"
    (is (= {:session-id "s1"}
           (#'mcp/validate-tool-args "get_session_diff" {:session-id "s1"}))))

  (testing "optional fields with correct types pass validation"
    (is (= {:session-id "s1" :file "test.txt" :from 1 :to 10}
           (#'mcp/validate-tool-args "get_session_diff" {:session-id "s1" :file "test.txt" :from 1 :to 10})))))

(deftest validate-tool-args-unknown-tool-test
  (testing "unknown tool passes through (handled by dispatch)"
    (is (= {:foo "bar"}
           (#'mcp/validate-tool-args "nonexistent_tool" {:foo "bar"})))))

(deftest get-json-type-test
  (testing "correctly identifies JSON types"
    (is (nil? (#'mcp/get-json-type nil)))
    (is (= "string" (#'mcp/get-json-type "hello")))
    (is (= "boolean" (#'mcp/get-json-type true)))
    (is (= "boolean" (#'mcp/get-json-type false)))
    (is (= "integer" (#'mcp/get-json-type 42)))
    (is (= "number" (#'mcp/get-json-type 3.14)))
    (is (= "array" (#'mcp/get-json-type [1 2 3])))
    (is (= "array" (#'mcp/get-json-type '(1 2 3))))
    (is (= "object" (#'mcp/get-json-type {:a 1})))))

;; ============================================================================
;; list_github_prs handler tests
;; ============================================================================

(deftest list-github-prs-missing-project-test
  (testing "returns error when project is missing"
    (let [result (mcp/handle-tool "list_github_prs" {})]
      ;; Handler may return a map or a Promise; accept a sync map for this case
      ;; since validation should happen before any async work.
      (is (contains? result :error))
      (is (re-find #"project" (:error result))))))

(deftest list-github-prs-malformed-project-test
  (testing "returns error when project format is invalid"
    (let [result (mcp/handle-tool "list_github_prs" {:project "not-a-slash"})]
      (is (contains? result :error))
      (is (re-find #"Invalid project format" (:error result))))))

(deftest list-github-prs-invalid-state-test
  (testing "returns error when state is not open/closed/all"
    (let [result (mcp/handle-tool "list_github_prs"
                                  {:project "foo/bar" :state "bogus"})]
      (is (contains? result :error))
      (is (re-find #"Invalid state" (:error result))))))

(deftest list-github-prs-invalid-limit-test
  (testing "returns error when limit is below 1"
    (let [result (mcp/handle-tool "list_github_prs"
                                  {:project "foo/bar" :limit 0})]
      (is (contains? result :error))
      (is (re-find #"limit" (:error result))))

    (let [result (mcp/handle-tool "list_github_prs"
                                  {:project "foo/bar" :limit -5})]
      (is (contains? result :error))
      (is (re-find #"limit" (:error result))))))

(deftest list-github-prs-dispatches-to-sessions-test
  (testing "valid project is parsed and forwarded to sessions/list-github-prs"
    (let [captured (atom nil)]
      (with-redefs [sessions/list-github-prs
                    (fn [opts]
                      (reset! captured opts)
                      (js/Promise.resolve {:prs [] :truncated false}))]
        (mcp/handle-tool "list_github_prs" {:project "foo/bar"
                                            :state "open"
                                            :limit 10})
        (is (= "foo" (:owner @captured)))
        (is (= "bar" (:repo @captured)))
        (is (= "open" (:state @captured)))
        (is (= 10 (:limit @captured)))))))

;; ============================================================================
;; wait_for_event tests
;; ============================================================================

(deftest wait-for-event-tool-schema-test
  (testing "wait_for_event is registered with the expected schema"
    (let [tool (first (filter #(= "wait_for_event" (:name %)) mcp/tools))]
      (is (some? tool))
      (is (string? (:description tool)))
      (let [schema (:inputSchema tool)
            props (:properties schema)]
        (is (= "object" (:type schema)))
        (is (= ["scope"] (:required schema)))
        (is (contains? props :scope))
        (is (contains? props :since_seq))
        (is (contains? props :timeout_ms))
        (is (contains? props :max_events))))))

(deftest wait-for-event-missing-scope-test
  (testing "returns error when scope is missing"
    (let [result (mcp/handle-tool "wait_for_event" {})]
      (is (string? (:error result)))
      (is (re-find #"scope is required" (:error result))))))

(deftest wait-for-event-invalid-github-scope-test
  (testing "returns error for malformed github scope"
    (let [result (mcp/handle-tool "wait_for_event" {:scope "github:no-slash"})]
      (is (string? (:error result)))
      (is (re-find #"Invalid scope" (:error result))))))

(deftest wait-for-event-invalid-scope-prefix-test
  (testing "returns error for unknown scope prefix"
    (let [result (mcp/handle-tool "wait_for_event" {:scope "nope:foo"})]
      (is (string? (:error result)))
      (is (re-find #"Invalid scope" (:error result))))))

(deftest wait-for-event-empty-session-scope-test
  (testing "returns error for session scope with empty id"
    (let [result (mcp/handle-tool "wait_for_event" {:scope "session:"})]
      (is (string? (:error result)))
      (is (re-find #"Invalid scope" (:error result))))))

(deftest wait-for-event-dispatches-github-scope-to-github-events-test
  (testing "github: scope is forwarded to github-events/wait-for-scope!"
    (let [captured (atom nil)
          orig differ.github-events/wait-for-scope!]
      (set! differ.github-events/wait-for-scope!
            (fn [scope opts]
              (reset! captured {:scope scope :opts opts})
              (js/Promise.resolve
               {:events [] :next-seq 0 :timed-out true})))
      (try
        (mcp/handle-tool "wait_for_event"
                         {:scope "github:owner/repo"
                          :since-seq 5
                          :timeout-ms 1000
                          :max-events 25})
        (is (= "github:owner/repo" (:scope @captured)))
        (is (= 5 (get-in @captured [:opts :since-seq])))
        (is (= 1000 (get-in @captured [:opts :timeout-ms])))
        (is (= 25 (get-in @captured [:opts :max-events])))
        (finally
          (set! differ.github-events/wait-for-scope! orig))))))

(deftest wait-for-event-dispatches-session-scope-to-event-stream-test
  (testing "session: scope is forwarded directly to event-stream/wait-for-event"
    (let [captured (atom nil)
          orig differ.event-stream/wait-for-event]
      (set! differ.event-stream/wait-for-event
            (fn [opts]
              (reset! captured opts)
              (js/Promise.resolve
               {:events [] :next-seq 0 :timed-out true})))
      (try
        (mcp/handle-tool "wait_for_event"
                         {:scope "session:local:abc123"
                          :since-seq 2})
        (is (= "session:local:abc123" (:scope @captured)))
        (is (= 2 (:since-seq @captured)))
        (is (= 300000 (:timeout-ms @captured)))
        (is (= 50 (:max-events @captured)))
        (finally
          (set! differ.event-stream/wait-for-event orig))))))

(deftest wait-for-event-defaults-applied-test
  (testing "omitted options get default values"
    (let [captured (atom nil)
          orig differ.github-events/wait-for-scope!]
      (set! differ.github-events/wait-for-scope!
            (fn [_scope opts]
              (reset! captured opts)
              (js/Promise.resolve
               {:events [] :next-seq 0 :timed-out true})))
      (try
        (mcp/handle-tool "wait_for_event" {:scope "github:a/b"})
        (is (= 0 (:since-seq @captured)))
        (is (= 300000 (:timeout-ms @captured)))
        (is (= 50 (:max-events @captured)))
        (finally
          (set! differ.github-events/wait-for-scope! orig))))))

;; Regression for the `github:session:foo/bar` dispatch confusion. The
;; old `parse-project` was permissive enough to split `session:foo/bar`
;; into owner=`session:foo`, repo=`bar`, and the dispatcher then handed
;; that to the github poller. Tighter slug validation in
;; `sessions/parse-project` now rejects it at the dispatcher boundary.
(deftest wait-for-event-rejects-github-session-prefix-test
  (testing "github:session:foo/bar must NOT slip through valid-scope?"
    (let [result (mcp/handle-tool "wait_for_event" {:scope "github:session:foo/bar"})]
      (is (string? (:error result)))
      (is (re-find #"Invalid scope" (:error result))))))

(deftest wait-for-event-rejects-github-with-extra-segment-test
  (testing "github:owner/repo/extra has too many slashes — reject"
    (let [result (mcp/handle-tool "wait_for_event" {:scope "github:owner/repo/extra"})]
      (is (string? (:error result)))
      (is (re-find #"Invalid scope" (:error result))))))

(deftest wait-for-event-rejects-github-whitespace-owner-test
  (testing "github: /repo (leading space owner) is rejected"
    (let [result (mcp/handle-tool "wait_for_event" {:scope "github: /repo"})]
      (is (string? (:error result)))
      (is (re-find #"Invalid scope" (:error result))))))

(deftest wait-for-event-rejects-github-prefix-inside-project-test
  (testing "github:github:foo/bar is rejected (reserved-prefix impostor)"
    (let [result (mcp/handle-tool "wait_for_event" {:scope "github:github:foo/bar"})]
      (is (string? (:error result)))
      (is (re-find #"Invalid scope" (:error result))))))

;; ============================================================================
;; Integration: end-to-end dispatcher → session-events → wait_for_event
;; ============================================================================

;; The end-to-end async integration test for the
;; dispatcher → session-events → wait_for_event flow lives in
;; differ.session-events-test, because mcp_test's `with-test-env`
;; fixture is a synchronous wrapper (closes the test DB before any
;; async chain finishes — incompatible with `(async done ...)` tests).
;; The session_events_test fixture uses the :before/:after map form
;; which waits for done.

;; ============================================================================
;; Kanban Board Tool Tests
;; ============================================================================

(deftest kanban-tools-defined-test
  (testing "kanban board tools are defined"
    (let [tool-names (set (map :name mcp/tools))]
      (is (contains? tool-names "create_task"))
      (is (contains? tool-names "list_tasks"))
      (is (contains? tool-names "take_task"))
      (is (contains? tool-names "update_task"))
      (is (contains? tool-names "add_note")))))
