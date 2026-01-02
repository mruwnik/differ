(ns differ.git-test
  "Tests for git operations - both pure parsing and integration tests."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [differ.git :as git]
            [differ.test-helpers :as helpers]))

;; ============================================================================
;; Pure Function Tests - parse-diff-hunks
;; ============================================================================

(deftest parse-diff-hunks-test
  (testing "returns nil for nil input"
    (is (nil? (git/parse-diff-hunks nil))))

  (testing "returns nil for empty string"
    (is (nil? (git/parse-diff-hunks ""))))

  (testing "parses single file diff"
    (let [diff-text "diff --git a/file.txt b/file.txt
index abc123..def456 100644
--- a/file.txt
+++ b/file.txt
@@ -1,3 +1,4 @@
 line 1
+new line
 line 2
 line 3"
          result (git/parse-diff-hunks diff-text)]
      (is (= 1 (count result)))
      (is (= "file.txt" (:file-a (first result))))
      (is (= "file.txt" (:file-b (first result))))
      (is (= 1 (count (:hunks (first result)))))
      (let [hunk (first (:hunks (first result)))]
        (is (= 1 (:old-start hunk)))
        (is (= 3 (:old-count hunk)))
        (is (= 1 (:new-start hunk)))
        (is (= 4 (:new-count hunk))))))

  (testing "parses multiple file diffs"
    (let [diff-text "diff --git a/file1.txt b/file1.txt
--- a/file1.txt
+++ b/file1.txt
@@ -1 +1 @@
-old
+new
diff --git a/file2.txt b/file2.txt
--- a/file2.txt
+++ b/file2.txt
@@ -1 +1 @@
-foo
+bar"
          result (git/parse-diff-hunks diff-text)]
      (is (= 2 (count result)))
      (is (= "file1.txt" (:file-a (first result))))
      (is (= "file2.txt" (:file-a (second result))))))

  (testing "parses multiple hunks in single file"
    (let [diff-text "diff --git a/file.txt b/file.txt
--- a/file.txt
+++ b/file.txt
@@ -1,3 +1,3 @@
-old line 1
+new line 1
 unchanged
 unchanged
@@ -10,3 +10,3 @@
-old line 10
+new line 10
 unchanged
 unchanged"
          result (git/parse-diff-hunks diff-text)
          file-diff (first result)]
      (is (= 1 (count result)))
      (is (= 2 (count (:hunks file-diff))))
      (is (= 1 (:old-start (first (:hunks file-diff)))))
      (is (= 10 (:old-start (second (:hunks file-diff)))))))

  (testing "preserves hunk lines"
    (let [diff-text "diff --git a/file.txt b/file.txt
--- a/file.txt
+++ b/file.txt
@@ -1,2 +1,2 @@
-removed
+added
 context"
          result (git/parse-diff-hunks diff-text)
          hunk (first (:hunks (first result)))]
      (is (= 3 (count (:lines hunk))))
      (is (= "-removed" (first (:lines hunk))))
      (is (= "+added" (second (:lines hunk))))
      (is (= " context" (nth (:lines hunk) 2))))))

;; ============================================================================
;; Pure Function Tests - file-to-diff-format
;; ============================================================================

(deftest file-to-diff-format-test
  (testing "converts file content to diff format"
    (let [result (git/file-to-diff-format "test.txt" "line 1\nline 2\nline 3")]
      (is (= "test.txt" (:file-a result)))
      (is (= "test.txt" (:file-b result)))
      (is (true? (:is-full-file result)))
      (is (= 1 (count (:hunks result))))
      (let [hunk (first (:hunks result))]
        (is (= 0 (:old-start hunk)))
        (is (= 0 (:old-count hunk)))
        (is (= 1 (:new-start hunk)))
        (is (= 3 (:new-count hunk)))
        (is (= ["+line 1" "+line 2" "+line 3"] (:lines hunk))))))

  (testing "handles empty content"
    ;; split-lines on "" returns [""], so count is 1
    (let [result (git/file-to-diff-format "empty.txt" "")]
      (is (= 1 (:new-count (first (:hunks result)))))
      (is (= ["+"] (:lines (first (:hunks result)))))))

  (testing "handles nil content"
    ;; nil is treated as "", so same behavior as empty
    (let [result (git/file-to-diff-format "nil.txt" nil)]
      (is (= 1 (:new-count (first (:hunks result))))))))

;; ============================================================================
;; Integration Tests - Git Repository Operations
;; ============================================================================

(def ^:dynamic *test-repo* nil)

(defn with-test-repo [f]
  (let [{:keys [path cleanup]} (helpers/create-test-repo)]
    (binding [*test-repo* path]
      (try
        (f)
        (finally
          (cleanup))))))

(use-fixtures :each with-test-repo)

(deftest git-repo?-test
  (testing "returns true for valid git repo"
    (is (git/git-repo? *test-repo*)))

  (testing "returns false for non-repo directory"
    (let [tmp-dir (helpers/create-temp-dir "not-a-repo")]
      (try
        (is (not (git/git-repo? tmp-dir)))
        (finally
          (helpers/remove-dir tmp-dir)))))

  (testing "returns false for non-existent directory"
    (is (not (git/git-repo? "/nonexistent/path")))))

(deftest get-current-branch-test
  (testing "returns current branch name"
    (is (= "main" (git/get-current-branch *test-repo*))))

  (testing "returns new branch after checkout"
    (helpers/create-test-branch *test-repo* "feature-branch")
    (is (= "feature-branch" (git/get-current-branch *test-repo*)))))

(deftest list-branches-test
  (testing "returns list of branches"
    (let [branches (git/list-branches *test-repo*)]
      (is (vector? branches))
      (is (some #(= "main" %) branches))))

  (testing "includes new branches"
    (helpers/create-test-branch *test-repo* "test-branch")
    (let [branches (git/list-branches *test-repo*)]
      (is (some #(= "test-branch" %) branches)))))

(deftest detect-default-branch-test
  (testing "detects main as default branch"
    (is (= "main" (git/detect-default-branch *test-repo*)))))

(deftest branch-exists?-test
  (testing "returns true for existing branch"
    (is (git/branch-exists? *test-repo* "main")))

  (testing "returns false for non-existent branch"
    (is (not (git/branch-exists? *test-repo* "nonexistent-branch")))))

(deftest get-file-content-test
  (testing "returns content from working tree"
    (let [content (git/get-file-content *test-repo* nil "README.md")]
      (is (string? content))
      (is (str/includes? content "Test Repo"))))

  (testing "returns nil for non-existent file"
    (is (nil? (git/get-file-content *test-repo* nil "nonexistent.txt")))))

(deftest get-line-content-test
  (testing "returns specific line content"
    (let [line (git/get-line-content *test-repo* "README.md" 1)]
      (is (= "# Test Repo" line))))

  (testing "returns nil for invalid line number"
    (is (nil? (git/get-line-content *test-repo* "README.md" 0)))
    (is (nil? (git/get-line-content *test-repo* "README.md" -1))))

  (testing "returns nil for line beyond file length"
    (is (nil? (git/get-line-content *test-repo* "README.md" 1000)))))

(deftest get-lines-range-test
  (testing "returns range of lines"
    (let [lines (git/get-lines-range *test-repo* "README.md" 1 2)]
      (is (vector? lines))
      (is (= 2 (count lines)))
      (is (= 1 (:line (first lines))))
      (is (= 2 (:line (second lines))))))

  (testing "returns nil for invalid range"
    (is (nil? (git/get-lines-range *test-repo* "README.md" 0 1)))
    (is (nil? (git/get-lines-range *test-repo* "README.md" 3 1)))))

(deftest get-changed-files-test
  (testing "returns empty list for no changes"
    (let [changes (git/get-changed-files *test-repo* "main")]
      (is (empty? changes))))

  (testing "detects modified files"
    (helpers/modify-test-file *test-repo* "README.md" "Modified content")
    (let [changes (git/get-changed-files *test-repo* "main")]
      (is (seq changes))
      (is (some #(= "README.md" (:path %)) changes))
      (is (some #(= :modified (:status %)) changes))))

  (testing "detects added files"
    (helpers/add-test-file *test-repo* "new-file.txt" "New content")
    (helpers/commit-test-changes *test-repo* "Add new file")
    (helpers/create-test-branch *test-repo* "feature")
    (helpers/add-test-file *test-repo* "another.txt" "Another file")
    (helpers/commit-test-changes *test-repo* "Add another file")
    (let [changes (git/get-changed-files *test-repo* "main")]
      (is (some #(= :added (:status %)) changes)))))

(deftest get-diff-test
  (testing "returns diff for changes"
    (helpers/modify-test-file *test-repo* "README.md" "Modified content\n")
    (let [diff (git/get-diff *test-repo* "main")]
      (is (string? diff))
      (is (str/includes? diff "diff --git"))
      (is (str/includes? diff "README.md")))))

(deftest get-file-diff-test
  (testing "returns diff for specific file"
    (helpers/modify-test-file *test-repo* "README.md" "Changed content\n")
    (let [diff (git/get-file-diff *test-repo* "main" "README.md")]
      (is (string? diff))
      (is (str/includes? diff "Changed content")))))

(deftest get-staged-files-test
  (testing "returns empty set when nothing staged"
    (is (empty? (git/get-staged-files *test-repo*))))

  (testing "returns staged files after git add"
    (helpers/add-test-file *test-repo* "staged.txt" "Staged content")
    (git/stage-file! *test-repo* "staged.txt")
    (let [staged (git/get-staged-files *test-repo*)]
      (is (contains? staged "staged.txt")))))

(deftest get-unstaged-files-test
  (testing "returns empty set when no unstaged changes"
    (is (empty? (git/get-unstaged-files *test-repo*))))

  (testing "returns unstaged modified files"
    (helpers/modify-test-file *test-repo* "README.md" "Unstaged change")
    (let [unstaged (git/get-unstaged-files *test-repo*)]
      (is (contains? unstaged "README.md")))))

(deftest get-untracked-files-test
  (testing "returns untracked files"
    (helpers/add-test-file *test-repo* "untracked.txt" "Untracked content")
    (let [untracked (git/get-untracked-files *test-repo*)]
      (is (some #(= "untracked.txt" %) untracked)))))

(deftest file-exists-in-working-tree?-test
  (testing "returns true for existing file"
    (is (git/file-exists-in-working-tree? *test-repo* "README.md")))

  (testing "returns false for non-existent file"
    (is (not (git/file-exists-in-working-tree? *test-repo* "nonexistent.txt")))))

(deftest get-project-id-test
  (testing "returns directory name when no remote"
    (let [project-id (git/get-project-id *test-repo*)]
      (is (string? project-id))
      (is (str/includes? project-id "differ-test-repo")))))
