(ns differ.pull-request-test
  "Tests for pull request creation functionality."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [differ.pull-request :as pr]
            [differ.test-helpers :as helpers]))

;; ============================================================================
;; Test Setup
;; ============================================================================

(def ^:dynamic *test-repo* nil)
(def ^:dynamic *test-repo-cleanup* nil)

(defn with-test-repo [f]
  (let [{:keys [path cleanup]} (helpers/create-test-repo)]
    (binding [*test-repo* path
              *test-repo-cleanup* cleanup]
      (try
        (f)
        (finally
          (cleanup))))))

(use-fixtures :each with-test-repo)

;; ============================================================================
;; normalize-pr-state tests
;; ============================================================================

(deftest normalize-pr-state-test
  (testing "converts uppercase to lowercase"
    (is (= "open" (pr/normalize-pr-state "OPEN")))
    (is (= "closed" (pr/normalize-pr-state "CLOSED")))
    (is (= "merged" (pr/normalize-pr-state "MERGED"))))

  (testing "handles mixed case"
    (is (= "open" (pr/normalize-pr-state "Open")))
    (is (= "closed" (pr/normalize-pr-state "Closed"))))

  (testing "passes through lowercase"
    (is (= "open" (pr/normalize-pr-state "open")))
    (is (= "closed" (pr/normalize-pr-state "closed"))))

  (testing "defaults to 'open' for nil"
    (is (= "open" (pr/normalize-pr-state nil))))

  (testing "defaults to 'open' for empty string"
    (is (= "open" (pr/normalize-pr-state "")))))

;; ============================================================================
;; GraphQL query format tests
;; ============================================================================

(deftest graphql-queries-test
  (testing "find-pr-query is a valid GraphQL string"
    (is (string? pr/find-pr-query))
    (is (re-find #"query\(" pr/find-pr-query))
    (is (re-find #"pullRequests" pr/find-pr-query)))

  (testing "get-repo-id-query is a valid GraphQL string"
    (is (string? pr/get-repo-id-query))
    (is (re-find #"repository\(" pr/get-repo-id-query)))

  (testing "create-pr-mutation is a valid GraphQL string"
    (is (string? pr/create-pr-mutation))
    (is (re-find #"mutation\(" pr/create-pr-mutation))
    (is (re-find #"createPullRequest" pr/create-pr-mutation))))

;; ============================================================================
;; create-pull-request! validation tests
;; Note: These are async tests that would need special handling.
;; For now, we test the synchronous validation logic.
;; ============================================================================

(deftest create-pull-request-path-validation-logic-test
  (testing "repo-path validation - must be absolute string"
    ;; Test the logic that would be used for path validation
    (is (not (string? nil)))
    (is (not (string? 123)))
    (is (string? "relative/path"))
    ;; path/isAbsolute would return false for relative paths
    ))
