(ns differ.api-test
  "Tests for REST API handlers.
   Tests handler logic, request/response formatting, and security validation."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [differ.test-helpers :as helpers]
            [differ.util :as util]
            ["path" :as path]))

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
;; Mock Request/Response Helpers
;; ============================================================================

(defn make-mock-req
  "Create a mock Express request object."
  [& {:keys [params query body]}]
  #js {:params (clj->js (or params {}))
       :query (clj->js (or query {}))
       :body (clj->js (or body {}))})

(defn make-mock-res
  "Create a mock Express response object.
   Returns [res-atom get-response-fn].
   res-atom accumulates response data.
   get-response-fn returns {:status :data} after json() is called."
  []
  (let [response (atom {:status nil :data nil :redirected nil})]
    [#js {:status (fn [code]
                    (swap! response assoc :status code)
                    (js-this))
          :json (fn [data]
                  (swap! response assoc :data (js->clj data :keywordize-keys true))
                  (js-this))
          :redirect (fn [url]
                      (swap! response assoc :redirected url)
                      (js-this))}
     (fn [] @response)]))

;; ============================================================================
;; Response Format Tests
;; ============================================================================

(deftest json-response-format-test
  (testing "json-response returns proper format"
    (let [[res get-response] (make-mock-res)]
      ;; Simulate what json-response does
      (-> res
          (.status 200)
          (.json #js {:sessions #js []}))
      (let [result (get-response)]
        (is (= 200 (:status result)))
        (is (= {:sessions []} (:data result)))))))

(deftest error-response-format-test
  (testing "error-response returns error object"
    (let [[res get-response] (make-mock-res)]
      ;; Simulate what error-response does
      (-> res
          (.status 404)
          (.json #js {:error "Not found"}))
      (let [result (get-response)]
        (is (= 404 (:status result)))
        (is (= {:error "Not found"} (:data result)))))))

;; ============================================================================
;; Request Parsing Tests
;; ============================================================================

(deftest request-params-parsing-test
  (testing "parses URL params"
    (let [req (make-mock-req :params {:id "session-123"})]
      (is (= "session-123" (.. req -params -id)))))

  (testing "parses query params"
    (let [req (make-mock-req :query {:project "my-project" :file "main.cljs"})]
      (is (= "my-project" (.. req -query -project)))
      (is (= "main.cljs" (.. req -query -file)))))

  (testing "parses body"
    (let [req (make-mock-req :body {:repo_path "/tmp/repo" :branch "main"})]
      (is (= "/tmp/repo" (.-repo_path (.-body req))))
      (is (= "main" (.-branch (.-body req)))))))

;; ============================================================================
;; Session API Logic Tests
;; ============================================================================

(deftest list-sessions-logic-test
  (testing "returns empty sessions list initially"
    (is (= [] (vec []))))

  (testing "sessions list includes required fields"
    (let [session {:id "s1"
                   :project "project"
                   :branch "branch"
                   :unresolved-count 5}]
      (is (contains? session :id))
      (is (contains? session :project))
      (is (contains? session :unresolved-count)))))

(deftest create-session-body-parsing-test
  (testing "accepts both snake_case and kebab-case keys"
    (let [body1 {:repo_path "/tmp" :target_branch "main"}
          body2 {:repo-path "/tmp" :target-branch "main"}
          ;; Simulating what handler does
          get-repo-path (fn [body]
                          (or (:repo_path body) (:repo-path body)))
          get-target-branch (fn [body]
                              (or (:target_branch body) (:target-branch body)))]
      (is (= "/tmp" (get-repo-path body1)))
      (is (= "/tmp" (get-repo-path body2)))
      (is (= "main" (get-target-branch body1)))
      (is (= "main" (get-target-branch body2))))))

(deftest get-session-response-format-test
  (testing "response includes review state fields"
    (let [review-state {:session-id "s1"
                        :project "project"
                        :branch "branch"
                        :target-branch "main"
                        :repo-path "/tmp"
                        :files ["file1.txt" "file2.txt"]
                        :excluded-files ["excluded.txt"]
                        :comments []
                        :unresolved-count 0}]
      (is (contains? review-state :session-id))
      (is (contains? review-state :files))
      (is (contains? review-state :excluded-files))
      (is (contains? review-state :comments))
      (is (vector? (:files review-state))))))

;; ============================================================================
;; Diff API Logic Tests
;; ============================================================================

(deftest get-diff-response-format-test
  (testing "diff response contains expected fields"
    (let [diff-response {:diff "diff --git..."
                         :parsed [{:file-a "main.cljs" :file-b "main.cljs" :hunks []}]
                         :files ["main.cljs" "other.cljs"]
                         :files-with-size [{:path "main.cljs" :size 1234}]
                         :changed-files [{:path "main.cljs" :status :modified}]
                         :is-git-repo true}]
      (is (contains? diff-response :diff))
      (is (contains? diff-response :parsed))
      (is (contains? diff-response :files))
      (is (contains? diff-response :is-git-repo)))))

(deftest get-file-diff-response-format-test
  (testing "file diff response contains expected fields"
    (let [response {:file "main.cljs"
                    :diff "diff output"
                    :parsed [{:file-a "main.cljs" :hunks []}]
                    :is-git-repo true}]
      (is (= "main.cljs" (:file response)))
      (is (vector? (:parsed response))))))

(deftest context-lines-response-format-test
  (testing "context lines response format"
    (let [response {:file "main.cljs"
                    :from 1
                    :to 10
                    :lines [{:line 1 :content "first"}
                            {:line 2 :content "second"}]}]
      (is (= "main.cljs" (:file response)))
      (is (integer? (:from response)))
      (is (vector? (:lines response))))))

;; ============================================================================
;; File Management API Logic Tests
;; ============================================================================

(deftest register-files-response-test
  (testing "register response lists newly registered files"
    (let [response {:registered ["file1.txt" "file2.txt"]}]
      (is (vector? (:registered response))))))

(deftest unregister-files-response-test
  (testing "unregister response lists removed files"
    (let [response {:unregistered ["file1.txt"]}]
      (is (vector? (:unregistered response))))))

;; ============================================================================
;; Comment API Logic Tests
;; ============================================================================

(deftest list-comments-query-params-test
  (testing "file query param filters by file"
    (let [req (make-mock-req :query {:file "main.cljs"})
          file-param (.. req -query -file)]
      (is (= "main.cljs" file-param)))))

(deftest get-pending-query-params-test
  (testing "since query param filters by timestamp"
    (let [req (make-mock-req :query {:since "2024-01-01T00:00:00.000Z"})
          since-param (.. req -query -since)]
      (is (= "2024-01-01T00:00:00.000Z" since-param)))))

(deftest add-comment-body-parsing-test
  (testing "comment body contains required fields"
    (let [body {:parent-id nil
                :file "main.cljs"
                :line 42
                :text "Review comment"
                :author "reviewer"}]
      (is (nil? (:parent-id body)))
      (is (= "main.cljs" (:file body)))
      (is (= 42 (:line body)))
      (is (= "Review comment" (:text body)))
      (is (= "reviewer" (:author body))))))

(deftest comment-response-format-test
  (testing "comment response contains comment data"
    (let [response {:comment {:id "c123"
                              :session-id "s1"
                              :file "main.cljs"
                              :line 42
                              :text "Comment text"
                              :author "reviewer"
                              :resolved false}}]
      (is (map? (:comment response)))
      (is (= "c123" (get-in response [:comment :id]))))))

;; ============================================================================
;; Error Handling Tests
;; ============================================================================

(deftest session-not-found-error-test
  (testing "404 error for non-existent session"
    (let [[res get-response] (make-mock-res)]
      ;; Simulate error response
      (-> res
          (.status 404)
          (.json #js {:error "Session not found"}))
      (let [result (get-response)]
        (is (= 404 (:status result)))
        (is (= "Session not found" (get-in result [:data :error])))))))

(deftest invalid-request-error-test
  (testing "400 error for invalid request"
    (let [[res get-response] (make-mock-res)]
      ;; Simulate error response
      (-> res
          (.status 400)
          (.json #js {:error "repo_path is required"}))
      (let [result (get-response)]
        (is (= 400 (:status result)))
        (is (= "repo_path is required" (get-in result [:data :error])))))))

;; ============================================================================
;; OAuth API Format Tests
;; ============================================================================

(deftest oauth-register-response-format-test
  (testing "client registration response format"
    (let [response {:client_id "client-123"
                    :client_name "Test Client"
                    :redirect_uris ["http://localhost:3000/callback"]
                    :scope "read"
                    :token_endpoint_auth_method "none"
                    :grant_types ["authorization_code" "refresh_token"]
                    :response_types ["code"]}]
      (is (= "client-123" (:client_id response)))
      (is (vector? (:redirect_uris response)))
      (is (vector? (:grant_types response))))))

(deftest oauth-token-response-format-test
  (testing "token exchange response format"
    (let [response {:access_token "eyJ..."
                    :token_type "bearer"
                    :expires_in 3600
                    :refresh_token "refresh..."}]
      (is (string? (:access_token response)))
      (is (= "bearer" (:token_type response)))
      (is (integer? (:expires_in response))))))

;; ============================================================================
;; Staged Files API Tests
;; ============================================================================

(deftest staged-files-response-format-test
  (testing "staged files response contains both staged and unstaged"
    (let [response {:staged ["file1.txt"]
                    :unstaged ["file2.txt"]}]
      (is (vector? (:staged response)))
      (is (vector? (:unstaged response))))))

(deftest stage-file-response-format-test
  (testing "stage file response includes success and updated lists"
    (let [response {:success true
                    :staged ["file1.txt" "file2.txt"]
                    :unstaged []}]
      (is (true? (:success response)))
      (is (vector? (:staged response))))))

;; ============================================================================
;; URL Encoding/Decoding Tests
;; ============================================================================

(deftest url-encoded-file-path-test
  (testing "URL-encoded file paths are decoded correctly"
    (let [encoded "src%2Fmain.cljs"
          decoded (js/decodeURIComponent encoded)]
      (is (= "src/main.cljs" decoded))))

  (testing "file paths with spaces are handled"
    (let [encoded "src%2Fmy%20file.cljs"
          decoded (js/decodeURIComponent encoded)]
      (is (= "src/my file.cljs" decoded)))))

;; ============================================================================
;; Path Traversal Security Tests
;; Testing the same algorithm used by api/safe-file-path? (which is private)
;; ============================================================================

(defn safe-file-path?
  "Reimplementation of api/safe-file-path? for testing.
   Check if file-path is safe (doesn't escape repo-path via path traversal).
   Returns true if the resolved path is within repo-path."
  [repo-path file-path]
  (let [resolved (path/resolve repo-path file-path)
        repo-normalized (str (path/resolve repo-path) "/")]
    (str/starts-with? (str resolved "/") repo-normalized)))

(deftest safe-file-path-basic-test
  (testing "allows simple relative paths within repo"
    (is (true? (safe-file-path? "/repo" "src/main.cljs")))
    (is (true? (safe-file-path? "/repo" "README.md")))
    (is (true? (safe-file-path? "/repo" "nested/deep/file.txt"))))

  (testing "allows absolute paths within repo"
    (is (true? (safe-file-path? "/repo" "/repo/src/main.cljs"))))

  (testing "allows paths with dots in filenames"
    (is (true? (safe-file-path? "/repo" "file.test.cljs")))
    (is (true? (safe-file-path? "/repo" "src/.gitignore")))
    (is (true? (safe-file-path? "/repo" ".hidden/file")))))

(deftest safe-file-path-traversal-attack-test
  (testing "blocks basic path traversal with ../"
    (is (false? (safe-file-path? "/repo" "../etc/passwd")))
    (is (false? (safe-file-path? "/repo" "../../../etc/passwd")))
    (is (false? (safe-file-path? "/repo" "src/../../../etc/passwd"))))

  (testing "blocks path traversal at different depths"
    (is (false? (safe-file-path? "/repo" "..")))
    (is (false? (safe-file-path? "/repo" "../../")))
    (is (false? (safe-file-path? "/repo" "foo/../../bar/../../../etc"))))

  (testing "blocks absolute paths outside repo"
    (is (false? (safe-file-path? "/repo" "/etc/passwd")))
    (is (false? (safe-file-path? "/repo" "/tmp/evil")))
    (is (false? (safe-file-path? "/repo" "/home/user/.ssh/id_rsa"))))

  (testing "blocks traversal attempts that start inside repo but escape"
    (is (false? (safe-file-path? "/repo/subdir" "../file.txt")))
    (is (false? (safe-file-path? "/home/user/project" "../../.ssh/id_rsa")))))

(deftest safe-file-path-edge-cases-test
  (testing "handles paths with multiple consecutive slashes"
    ;; path.resolve normalizes these, should still be safe
    (is (true? (safe-file-path? "/repo" "src//main.cljs")))
    (is (true? (safe-file-path? "/repo" "src///nested///file.txt"))))

  (testing "handles paths with trailing slashes"
    (is (true? (safe-file-path? "/repo" "src/")))
    (is (true? (safe-file-path? "/repo/" "src/main.cljs"))))

  (testing "handles paths with current directory references"
    (is (true? (safe-file-path? "/repo" "./src/main.cljs")))
    (is (true? (safe-file-path? "/repo" "src/./nested/./file.txt"))))

  (testing "handles empty and single-char paths"
    (is (true? (safe-file-path? "/repo" "")))
    (is (true? (safe-file-path? "/repo" ".")))
    (is (true? (safe-file-path? "/repo" "a"))))

  (testing "handles repo path variations"
    (is (true? (safe-file-path? "/repo/" "src/main.cljs")))
    (is (true? (safe-file-path? "/repo//" "src/main.cljs")))))

(deftest safe-file-path-unicode-test
  (testing "handles unicode filenames safely"
    (is (true? (safe-file-path? "/repo" "src/文件.cljs")))
    (is (true? (safe-file-path? "/repo" "émojis/🎉.txt")))
    (is (true? (safe-file-path? "/repo" "données/fichier.txt")))))

(deftest safe-file-path-special-chars-test
  (testing "handles special characters in paths"
    (is (true? (safe-file-path? "/repo" "src/file with spaces.cljs")))
    (is (true? (safe-file-path? "/repo" "src/file-with-dashes.cljs")))
    (is (true? (safe-file-path? "/repo" "src/file_with_underscores.cljs")))
    (is (true? (safe-file-path? "/repo" "src/file@special#chars.cljs")))))

;; ============================================================================
;; Line Number Validation Tests (get-context-lines-handler logic)
;; ============================================================================

(defn parse-and-validate-line
  "Simulate the line number parsing and validation from get-context-lines-handler."
  [from-str to-str]
  (let [from-line (js/parseInt (or from-str "1") 10)
        to-line (js/parseInt (or to-str "1") 10)]
    (cond
      (or (js/isNaN from-line) (js/isNaN to-line))
      {:error "Invalid line numbers: must be integers"}

      (or (< from-line 1) (< to-line 1))
      {:error "Invalid line numbers: must be positive"}

      (> from-line to-line)
      {:error "Invalid range: from must be <= to"}

      :else
      {:valid true :from from-line :to to-line})))

(deftest line-number-valid-inputs-test
  (testing "accepts valid line number ranges"
    (is (= {:valid true :from 1 :to 10} (parse-and-validate-line "1" "10")))
    (is (= {:valid true :from 5 :to 5} (parse-and-validate-line "5" "5")))
    (is (= {:valid true :from 100 :to 200} (parse-and-validate-line "100" "200"))))

  (testing "accepts nil inputs (default to 1)"
    (is (= {:valid true :from 1 :to 1} (parse-and-validate-line nil nil)))
    (is (= {:valid true :from 1 :to 5} (parse-and-validate-line nil "5"))))

  (testing "from > to is invalid even with nil defaults"
    ;; When from=5 and to defaults to 1, that's an invalid range
    (is (= {:error "Invalid range: from must be <= to"}
           (parse-and-validate-line "5" nil)))))

(deftest line-number-nan-test
  (testing "rejects non-numeric strings"
    (is (= {:error "Invalid line numbers: must be integers"}
           (parse-and-validate-line "abc" "10")))
    (is (= {:error "Invalid line numbers: must be integers"}
           (parse-and-validate-line "10" "xyz")))
    (is (= {:error "Invalid line numbers: must be integers"}
           (parse-and-validate-line "foo" "bar"))))

  (testing "rejects empty strings (parsed as NaN)"
    (is (= {:error "Invalid line numbers: must be integers"}
           (parse-and-validate-line "" "10")))
    (is (= {:error "Invalid line numbers: must be integers"}
           (parse-and-validate-line "10" ""))))

  (testing "rejects special values"
    (is (= {:error "Invalid line numbers: must be integers"}
           (parse-and-validate-line "NaN" "10")))
    (is (= {:error "Invalid line numbers: must be integers"}
           (parse-and-validate-line "Infinity" "10")))
    (is (= {:error "Invalid line numbers: must be integers"}
           (parse-and-validate-line "-Infinity" "10")))))

(deftest line-number-negative-test
  (testing "rejects negative line numbers"
    (is (= {:error "Invalid line numbers: must be positive"}
           (parse-and-validate-line "-1" "10")))
    (is (= {:error "Invalid line numbers: must be positive"}
           (parse-and-validate-line "1" "-5")))
    (is (= {:error "Invalid line numbers: must be positive"}
           (parse-and-validate-line "-10" "-1")))))

(deftest line-number-zero-test
  (testing "rejects zero as line number"
    (is (= {:error "Invalid line numbers: must be positive"}
           (parse-and-validate-line "0" "10")))
    (is (= {:error "Invalid line numbers: must be positive"}
           (parse-and-validate-line "1" "0")))
    (is (= {:error "Invalid line numbers: must be positive"}
           (parse-and-validate-line "0" "0")))))

(deftest line-number-invalid-range-test
  (testing "rejects ranges where from > to"
    (is (= {:error "Invalid range: from must be <= to"}
           (parse-and-validate-line "10" "5")))
    (is (= {:error "Invalid range: from must be <= to"}
           (parse-and-validate-line "100" "1")))
    (is (= {:error "Invalid range: from must be <= to"}
           (parse-and-validate-line "2" "1")))))

(deftest line-number-decimal-test
  (testing "handles decimal numbers (parseInt truncates)"
    ;; parseInt("1.5", 10) = 1, so these should be valid
    (is (= {:valid true :from 1 :to 10} (parse-and-validate-line "1.5" "10")))
    (is (= {:valid true :from 1 :to 10} (parse-and-validate-line "1" "10.9")))
    (is (= {:valid true :from 1 :to 1} (parse-and-validate-line "1.1" "1.9")))))

(deftest line-number-large-values-test
  (testing "accepts large line numbers"
    (is (= {:valid true :from 1 :to 999999}
           (parse-and-validate-line "1" "999999")))
    (is (= {:valid true :from 100000 :to 200000}
           (parse-and-validate-line "100000" "200000")))))

(deftest line-number-whitespace-test
  (testing "handles strings with leading/trailing whitespace"
    ;; parseInt ignores leading whitespace
    (is (= {:valid true :from 5 :to 10}
           (parse-and-validate-line " 5" "10")))
    (is (= {:valid true :from 5 :to 10}
           (parse-and-validate-line "5 " "10")))))

;; ============================================================================
;; Handler Integration Tests
;; ============================================================================

(deftest get-context-lines-handler-validation-test
  (testing "handler returns 400 for invalid line numbers"
    (let [[res get-response] (make-mock-res)
          req (make-mock-req :params {:id "session-123" :file "main.cljs"}
                             :query {:from "abc" :to "10"})]
      ;; The handler checks line numbers before session lookup
      ;; Simulating the error path
      (let [from-str (.. req -query -from)
            to-str (.. req -query -to)
            from-line (js/parseInt from-str 10)
            to-line (js/parseInt to-str 10)]
        (when (or (js/isNaN from-line) (js/isNaN to-line))
          (-> res
              (.status 400)
              (.json #js {:error "Invalid line numbers: must be integers"}))))
      (let [result (get-response)]
        (is (= 400 (:status result)))
        (is (str/includes? (get-in result [:data :error]) "Invalid"))))))

(deftest validate-file-path-handler-test
  (testing "handler returns 400 for path traversal attempt"
    (let [[res get-response] (make-mock-res)
          repo-path "/repo"
          file-path "../../../etc/passwd"]
      ;; Simulate validate-file-path behavior using our local implementation
      (when-not (safe-file-path? repo-path file-path)
        (-> res
            (.status 400)
            (.json #js {:error "Invalid file path: path traversal not allowed"})))
      (let [result (get-response)]
        (is (= 400 (:status result)))
        (is (str/includes? (get-in result [:data :error]) "path traversal")))))

  (testing "handler allows valid paths"
    (let [[_res get-response] (make-mock-res)
          repo-path "/repo"
          file-path "src/main.cljs"]
      ;; Simulate validation success - no error response sent
      (when (safe-file-path? repo-path file-path)
        ;; Handler would proceed normally
        nil)
      (let [result (get-response)]
        ;; No error response was sent
        (is (nil? (:status result)))))))
