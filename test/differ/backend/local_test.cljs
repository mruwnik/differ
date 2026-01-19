(ns differ.backend.local-test
  "Tests for the local directory backend implementation.
   Tests pure functions and synchronous helpers."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [differ.backend.local :as local]
            [differ.backend.protocol :as proto]
            [differ.git :as git]
            [differ.test-helpers :as helpers]))

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
;; Git helper function tests
;; ============================================================================

(deftest git-repo?-test
  (testing "returns true for git repository"
    (is (true? (local/git-repo? *test-repo*))))

  (testing "returns false for non-git directory"
    (let [temp-dir (helpers/create-temp-dir "non-git")]
      (try
        (is (false? (local/git-repo? temp-dir)))
        (finally
          (helpers/remove-dir temp-dir)))))

  (testing "returns false for non-existent directory"
    (is (false? (local/git-repo? "/non/existent/path")))))

(deftest get-project-id-test
  (testing "returns directory name when no remote"
    ;; Test repo doesn't have a remote by default
    (is (some? (local/get-project-id *test-repo*)))
    (is (string? (local/get-project-id *test-repo*)))))

(deftest get-current-branch-test
  (testing "returns current branch name"
    (is (= "main" (local/get-current-branch *test-repo*))))

  (testing "returns 'working' for non-git directory"
    (let [temp-dir (helpers/create-temp-dir "non-git")]
      (try
        (is (= "working" (local/get-current-branch temp-dir)))
        (finally
          (helpers/remove-dir temp-dir))))))

(deftest detect-default-branch-test
  (testing "detects main branch"
    (is (= "main" (local/detect-default-branch *test-repo*)))))

(deftest list-branches-test
  (testing "lists branches in repo"
    (let [branches (local/list-branches *test-repo*)]
      (is (vector? branches))
      (is (contains? (set branches) "main"))))

  (testing "returns empty vector for non-git dir"
    (let [temp-dir (helpers/create-temp-dir "non-git")]
      (try
        (is (= [] (local/list-branches temp-dir)))
        (finally
          (helpers/remove-dir temp-dir))))))

;; ============================================================================
;; Diff parsing tests
;; ============================================================================

(deftest parse-diff-hunks-test
  (testing "parses single file diff"
    (let [diff "diff --git a/file.txt b/file.txt
index abc123..def456 100644
--- a/file.txt
+++ b/file.txt
@@ -1,3 +1,4 @@
 line1
 line2
+new line
 line3"
          parsed (git/parse-diff-hunks diff)]
      (is (= 1 (count parsed)))
      (is (= "file.txt" (:file-a (first parsed))))
      (is (= "file.txt" (:file-b (first parsed))))
      (is (= 1 (count (:hunks (first parsed)))))
      (let [hunk (first (:hunks (first parsed)))]
        (is (= 1 (:old-start hunk)))
        (is (= 3 (:old-count hunk)))
        (is (= 1 (:new-start hunk)))
        (is (= 4 (:new-count hunk))))))

  (testing "parses multiple file diffs"
    (let [diff "diff --git a/file1.txt b/file1.txt
@@ -1 +1 @@
-old
+new
diff --git a/file2.txt b/file2.txt
@@ -1 +1,2 @@
 unchanged
+added"
          parsed (git/parse-diff-hunks diff)]
      (is (= 2 (count parsed)))
      (is (= "file1.txt" (:file-a (first parsed))))
      (is (= "file2.txt" (:file-a (second parsed))))))

  (testing "parses diff with multiple hunks"
    (let [diff "diff --git a/file.txt b/file.txt
@@ -1,2 +1,2 @@
-line1 old
+line1 new
 unchanged
@@ -10,2 +10,2 @@
-line10 old
+line10 new
 unchanged"
          parsed (git/parse-diff-hunks diff)]
      (is (= 1 (count parsed)))
      (is (= 2 (count (:hunks (first parsed)))))
      (is (= 1 (:old-start (first (:hunks (first parsed))))))
      (is (= 10 (:old-start (second (:hunks (first parsed))))))))

  (testing "handles nil/empty input"
    (is (nil? (git/parse-diff-hunks nil)))
    (is (nil? (git/parse-diff-hunks "")))))

;; ============================================================================
;; LocalBackend creation tests
;; ============================================================================

(deftest create-local-backend-test
  (testing "creates backend with auto-detected branch"
    (let [backend (local/create-local-backend *test-repo*)]
      (is (some? backend))
      (is (string? (proto/session-id backend)))
      (is (= :local (proto/session-type backend)))))

  (testing "creates backend with explicit target branch"
    (let [backend (local/create-local-backend *test-repo* "main")]
      (is (some? backend))
      (is (= :local (proto/session-type backend)))))

  (testing "session-id starts with 'local:'"
    (let [backend (local/create-local-backend *test-repo*)]
      (is (clojure.string/starts-with? (proto/session-id backend) "local:")))))

(deftest backend-session-descriptor-test
  (testing "returns correct descriptor"
    (let [backend (local/create-local-backend *test-repo* "main")
          descriptor (proto/session-descriptor backend)]
      (is (= :local (:type descriptor)))
      (is (= "main" (:target-branch descriptor)))
      (is (string? (:repo-path descriptor))))))
