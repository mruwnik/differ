(ns differ.backend.github-test
  "Tests for the GitHub PR backend implementation.
   Tests pure functions and URL parsing - network calls are not tested here."
  (:require [clojure.test :refer [deftest testing is]]
            [differ.backend.github :as github]
            [differ.backend.protocol :as proto]))

;; ============================================================================
;; parse-pr-url tests
;; ============================================================================

(deftest parse-pr-url-test
  (testing "parses full GitHub PR URL"
    (is (= {:owner "facebook" :repo "react" :pr-number 12345}
           (github/parse-pr-url "https://github.com/facebook/react/pull/12345"))))

  (testing "parses URL without https://"
    (is (= {:owner "facebook" :repo "react" :pr-number 12345}
           (github/parse-pr-url "github.com/facebook/react/pull/12345"))))

  (testing "parses short format owner/repo#number"
    (is (= {:owner "owner" :repo "repo" :pr-number 123}
           (github/parse-pr-url "owner/repo#123"))))

  (testing "handles various owner/repo formats"
    (is (= {:owner "my-org" :repo "my-repo" :pr-number 1}
           (github/parse-pr-url "my-org/my-repo#1")))
    (is (= {:owner "user123" :repo "project_name" :pr-number 999}
           (github/parse-pr-url "user123/project_name#999"))))

  (testing "returns nil for invalid URLs"
    (is (nil? (github/parse-pr-url "not-a-url")))
    (is (nil? (github/parse-pr-url "https://gitlab.com/owner/repo/pull/123")))
    (is (nil? (github/parse-pr-url "")))))
    ;; Note: passing nil to parse-pr-url throws - function doesn't guard against nil)

;; ============================================================================
;; GraphQL query format tests
;; ============================================================================

(deftest graphql-queries-test
  (testing "pr-query is valid GraphQL"
    (is (string? github/pr-query))
    (is (re-find #"query\(" github/pr-query))
    (is (re-find #"pullRequest" github/pr-query)))

  (testing "pr-files-query uses pagination"
    (is (string? github/pr-files-query))
    (is (re-find #"\$cursor" github/pr-files-query))
    (is (re-find #"pageInfo" github/pr-files-query)))

  (testing "pr-commits-query uses pagination"
    (is (string? github/pr-commits-query))
    (is (re-find #"\$cursor" github/pr-commits-query)))

  (testing "pr-threads-query includes comments"
    (is (string? github/pr-threads-query))
    (is (re-find #"reviewThreads" github/pr-threads-query))
    (is (re-find #"comments" github/pr-threads-query)))

  (testing "file-content-query uses expression"
    (is (string? github/file-content-query))
    (is (re-find #"\$expression" github/file-content-query))
    (is (re-find #"Blob" github/file-content-query)))

  (testing "mutation queries are valid"
    (is (re-find #"mutation\(" github/add-review-thread-mutation))
    (is (re-find #"mutation\(" github/add-issue-comment-mutation))
    (is (re-find #"mutation\(" github/add-thread-reply-mutation))
    (is (re-find #"mutation\(" github/resolve-thread-mutation))
    (is (re-find #"mutation\(" github/unresolve-thread-mutation))
    (is (re-find #"mutation\(" github/submit-review-mutation))))

;; ============================================================================
;; create-github-backend tests
;; ============================================================================

(deftest create-github-backend-test
  (testing "creates backend with correct session ID"
    (let [backend (github/create-github-backend "owner" "repo" 123 "token")]
      (is (some? backend))
      (is (= "github:owner/repo:123" (proto/session-id backend)))))

  (testing "returns :github session type"
    (let [backend (github/create-github-backend "owner" "repo" 123 "token")]
      (is (= :github (proto/session-type backend)))))

  (testing "returns correct session descriptor"
    (let [backend (github/create-github-backend "owner" "repo" 456 "token")
          descriptor (proto/session-descriptor backend)]
      (is (= :github (:type descriptor)))
      (is (= "owner" (:owner descriptor)))
      (is (= "repo" (:repo descriptor)))
      (is (= 456 (:pr-number descriptor))))))

;; ============================================================================
;; Session ID format tests
;; ============================================================================

(deftest session-id-format-test
  (testing "session ID follows github:owner/repo:number format"
    (let [backend (github/create-github-backend "facebook" "react" 12345 "token")
          session-id (proto/session-id backend)]
      (is (= "github:facebook/react:12345" session-id))
      (is (clojure.string/starts-with? session-id "github:"))))

  (testing "session ID handles special characters in owner/repo"
    (let [backend (github/create-github-backend "my-org" "my-project.js" 1 "token")
          session-id (proto/session-id backend)]
      (is (= "github:my-org/my-project.js:1" session-id)))))
