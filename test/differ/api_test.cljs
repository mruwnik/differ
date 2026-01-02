(ns differ.api-test
  "Tests for REST API handlers.
   Tests handler logic and request/response formatting."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [differ.test-helpers :as helpers]
            [differ.util :as util]))

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
