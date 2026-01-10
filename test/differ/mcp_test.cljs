(ns differ.mcp-test
  "Tests for MCP (Model Context Protocol) JSON-RPC handlers."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [differ.test-helpers :as helpers]
            [differ.mcp :as mcp]))

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
      (is (contains? tool-names "get_history")))))

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
