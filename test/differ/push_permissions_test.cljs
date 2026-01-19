(ns differ.push-permissions-test
  "Tests for push permissions whitelist functionality."
  (:require [clojure.test :refer [deftest testing is]]
            [differ.push-permissions :as perms]))

;; ============================================================================
;; parse-remote-url tests
;; ============================================================================

(deftest parse-remote-url-test
  (testing "HTTPS format with .git suffix"
    (is (= "owner/repo" (perms/parse-remote-url "https://github.com/owner/repo.git"))))

  (testing "HTTPS format without .git suffix"
    (is (= "owner/repo" (perms/parse-remote-url "https://github.com/owner/repo"))))

  (testing "SSH format with .git suffix"
    (is (= "owner/repo" (perms/parse-remote-url "git@github.com:owner/repo.git"))))

  (testing "SSH format without .git suffix"
    (is (= "owner/repo" (perms/parse-remote-url "git@github.com:owner/repo"))))

  (testing "already normalized owner/repo format"
    (is (= "owner/repo" (perms/parse-remote-url "owner/repo"))))

  (testing "handles whitespace"
    (is (= "owner/repo" (perms/parse-remote-url "  https://github.com/owner/repo.git  "))))

  (testing "returns nil for nil input"
    (is (nil? (perms/parse-remote-url nil))))

  (testing "returns nil for empty string"
    (is (nil? (perms/parse-remote-url ""))))

  (testing "returns nil for non-string input"
    (is (nil? (perms/parse-remote-url 123))))

  (testing "returns nil for non-github URLs"
    (is (nil? (perms/parse-remote-url "https://gitlab.com/owner/repo.git"))))

  (testing "returns nil for malformed URLs"
    (is (nil? (perms/parse-remote-url "not-a-url")))
    (is (nil? (perms/parse-remote-url "github.com")))))

;; ============================================================================
;; pattern-matches? tests
;; ============================================================================

(deftest pattern-matches?-test
  (testing "exact match"
    (is (true? (perms/pattern-matches? "feature" "feature")))
    (is (false? (perms/pattern-matches? "feature" "fix"))))

  (testing "wildcard * matches anything"
    (is (true? (perms/pattern-matches? "*" "anything")))
    (is (true? (perms/pattern-matches? "*" "")))
    (is (true? (perms/pattern-matches? "*" "a/b/c"))))

  (testing "prefix wildcard pattern"
    (is (true? (perms/pattern-matches? "feature/*" "feature/foo")))
    (is (true? (perms/pattern-matches? "feature/*" "feature/bar")))
    (is (true? (perms/pattern-matches? "feature/*" "feature/")))
    (is (false? (perms/pattern-matches? "feature/*" "fix/foo")))
    (is (false? (perms/pattern-matches? "feature/*" "feature"))))

  (testing "suffix wildcard pattern"
    (is (true? (perms/pattern-matches? "*/main" "foo/main")))
    (is (true? (perms/pattern-matches? "*/main" "bar/main")))
    (is (false? (perms/pattern-matches? "*/main" "foo/develop"))))

  (testing "middle wildcard pattern"
    (is (true? (perms/pattern-matches? "feature/*/fix" "feature/something/fix")))
    (is (false? (perms/pattern-matches? "feature/*/fix" "feature/something/bug"))))

  (testing "multiple wildcards"
    (is (true? (perms/pattern-matches? "*/*" "a/b")))
    (is (true? (perms/pattern-matches? "*/foo/*" "bar/foo/baz"))))

  (testing "regex special chars are escaped"
    (is (true? (perms/pattern-matches? "feature.branch" "feature.branch")))
    (is (false? (perms/pattern-matches? "feature.branch" "featureXbranch")))
    (is (true? (perms/pattern-matches? "foo+bar" "foo+bar")))
    (is (false? (perms/pattern-matches? "foo+bar" "foobar")))))

;; ============================================================================
;; check-permission tests
;; ============================================================================

(deftest check-permission-test
  ;; Note: These tests rely on the config module's :push-whitelist value.
  ;; By default (empty whitelist), all pushes are allowed.

  (testing "empty whitelist allows all pushes"
    ;; With default config (empty whitelist), all should be allowed
    (let [result (perms/check-permission "any/repo" "any-branch")]
      (is (true? (:allowed result))))))

;; ============================================================================
;; check-permission with explicit whitelist tests (using internal functions)
;; ============================================================================

;; Test the permission checking logic directly by simulating whitelist behavior
;; This tests the algorithm without needing to mock config

(deftest permission-checking-logic-test
  (testing "repo matches exact key"
    ;; Simulate: whitelist = {"owner/repo" ["main"]}
    ;; repo = "owner/repo", branch = "main" -> allowed
    (is (true? (perms/pattern-matches? "owner/repo" "owner/repo")))
    (is (true? (perms/pattern-matches? "main" "main"))))

  (testing "repo matches wildcard key"
    ;; Simulate: whitelist = {"owner/*" ["*"]}
    ;; repo = "owner/any-repo" -> matches
    (is (true? (perms/pattern-matches? "owner/*" "owner/any-repo")))
    (is (false? (perms/pattern-matches? "owner/*" "other/repo"))))

  (testing "branch pattern matching"
    ;; Simulate: whitelist = {"org/repo" ["feature/*" "fix/*"]}
    (is (true? (perms/pattern-matches? "feature/*" "feature/new-thing")))
    (is (true? (perms/pattern-matches? "fix/*" "fix/bug-123")))
    (is (false? (perms/pattern-matches? "feature/*" "develop")))
    (is (false? (perms/pattern-matches? "fix/*" "main"))))

  (testing "multiple branch patterns - at least one must match"
    (let [patterns ["feature/*" "fix/*" "release/*"]
          branch-matches? (fn [branch]
                            (some #(perms/pattern-matches? % branch) patterns))]
      (is (true? (branch-matches? "feature/foo")))
      (is (true? (branch-matches? "fix/bar")))
      (is (true? (branch-matches? "release/1.0")))
      ;; some returns nil (not false) when nothing matches
      (is (not (branch-matches? "main")))
      (is (not (branch-matches? "develop"))))))

;; ============================================================================
;; validate-push! tests
;; ============================================================================

(deftest validate-push!-test
  (testing "throws on unparseable remote URL"
    (is (thrown-with-msg?
         js/Error
         #"Could not parse remote URL"
         (perms/validate-push! "/path/to/repo" "not-a-valid-url" "main")))))
