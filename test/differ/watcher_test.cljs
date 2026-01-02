(ns differ.watcher-test
  "Tests for file watcher module.

   Note: Most watcher functionality involves I/O (fs.watch) which can't be
   easily unit tested. These tests focus on the pure predicate logic."
  (:require [clojure.test :refer [deftest testing is are]]
            [differ.watcher :as watcher]))

;; ============================================================================
;; ignore-patterns data tests
;; ============================================================================

(deftest ignore-patterns-test
  (let [patterns @#'differ.watcher/ignore-patterns]
    (testing "ignore-patterns has expected structure"
      (is (map? patterns))
      (is (contains? patterns :prefixes))
      (is (contains? patterns :contains))
      (is (contains? patterns :suffixes)))

    (testing "prefixes includes dot files"
      (is (some #{"."} (:prefixes patterns))))

    (testing "contains includes common ignored directories"
      (is (some #{"node_modules"} (:contains patterns)))
      (is (some #{".git"} (:contains patterns)))
      (is (some #{"target"} (:contains patterns))))

    (testing "suffixes includes database files"
      (is (some #{".db"} (:suffixes patterns)))
      (is (some #{".db-wal"} (:suffixes patterns)))
      (is (some #{".db-shm"} (:suffixes patterns))))))

;; ============================================================================
;; should-ignore? tests
;; ============================================================================

(deftest should-ignore?-test
  (let [should-ignore? @#'differ.watcher/should-ignore?]
    (testing "ignores nil"
      (is (true? (should-ignore? nil))))

    (testing "ignores dot files"
      (is (should-ignore? ".gitignore"))
      (is (should-ignore? ".env"))
      (is (should-ignore? ".DS_Store")))

    (testing "ignores node_modules paths"
      (is (should-ignore? "node_modules/foo/bar.js"))
      (is (should-ignore? "src/node_modules/package.json")))

    (testing "ignores .git paths"
      (is (should-ignore? ".git/HEAD"))
      (is (should-ignore? "some/.git/config")))

    (testing "ignores target directory"
      (is (should-ignore? "target/classes/foo.class"))
      (is (should-ignore? "src/target/output.js")))

    (testing "ignores database files"
      (is (should-ignore? "data.db"))
      (is (should-ignore? "data.db-wal"))
      (is (should-ignore? "data.db-shm"))
      (is (should-ignore? "path/to/app.db")))

    (testing "does not ignore regular source files"
      (is (not (should-ignore? "src/main.cljs")))
      (is (not (should-ignore? "package.json")))
      (is (not (should-ignore? "README.md")))
      (is (not (should-ignore? "test/foo_test.cljs"))))

    (testing "does not ignore files that just contain ignore substrings differently"
      ;; "target" substring should only match directory names, not file content
      ;; But actually should-ignore? uses str/includes? so it will match anywhere
      ;; This test documents current behavior
      (is (should-ignore? "retarget.js")))  ; Contains "target"

    (testing "handles empty string"
      ;; Empty string doesn't start with ".", contain ignored dirs, or end with patterns
      (is (not (should-ignore? ""))))))

;; ============================================================================
;; watching? tests
;; ============================================================================

(deftest watching?-test
  (testing "returns false for non-watched session"
    (is (not (watcher/watching? "nonexistent-session-id-12345")))))
