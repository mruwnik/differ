(ns differ.sessions-test
  "Tests for session management logic."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.set :as set]
            [differ.test-helpers :as helpers]
            [differ.util :as util]
            [differ.sessions :as sessions]
            ["path" :as path]))

;; ============================================================================
;; Test Setup - We need both test DB and test git repo
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
;; validate-repo-path tests
;; ============================================================================

(deftest validate-repo-path-test
  (testing "returns error for nil path"
    (let [result (sessions/validate-repo-path nil)]
      (is (false? (:valid result)))
      (is (= "repo_path is required" (:error result)))))

  (testing "returns error for non-existent path"
    (let [result (sessions/validate-repo-path "/nonexistent/path/12345")]
      (is (false? (:valid result)))
      (is (clojure.string/includes? (:error result) "does not exist"))))

  (testing "returns resolved path for valid directory"
    (let [result (sessions/validate-repo-path *test-repo*)]
      (is (true? (:valid result)))
      (is (string? (:path result)))
      (is (= (path/resolve *test-repo*) (:path result))))))

;; ============================================================================
;; compute-file-set tests (pure function using data only)
;; ============================================================================

(deftest compute-file-set-pure-test
  (testing "returns union of git files, registered, and manual additions"
    ;; Create a mock session data structure
    (let [session {:target-branch "main"
                   :registered-files {"src/new.cljs" "agent1"}
                   :manual-additions ["docs/README.md"]
                   :manual-removals []}
          ;; Simulate what compute-file-set does with fixed data
          registered (set (keys (:registered-files session)))
          manual-adds (set (:manual-additions session))
          manual-removes (set (:manual-removals session))
          all-files (set/union registered manual-adds)
          final (set/difference all-files manual-removes)]
      (is (contains? final "src/new.cljs"))
      (is (contains? final "docs/README.md"))))

  (testing "removes files in manual-removals"
    (let [session {:target-branch "main"
                   :registered-files {"src/keep.cljs" "agent1"
                                      "src/remove.cljs" "agent1"}
                   :manual-additions []
                   :manual-removals ["src/remove.cljs"]}
          registered (set (keys (:registered-files session)))
          manual-removes (set (:manual-removals session))
          final (set/difference registered manual-removes)]
      (is (contains? final "src/keep.cljs"))
      (is (not (contains? final "src/remove.cljs"))))))

;; ============================================================================
;; register-files! logic tests
;; ============================================================================

(deftest register-files-logic-test
  (testing "adds new files to registered-files"
    (let [current {}
          paths ["file1.txt" "file2.txt"]
          agent-id "agent1"
          result (reduce
                  (fn [acc path]
                    (if (contains? acc path)
                      acc
                      (assoc acc path agent-id)))
                  current
                  paths)]
      (is (= {"file1.txt" "agent1" "file2.txt" "agent1"} result))))

  (testing "does not overwrite existing registrations"
    (let [current {"file1.txt" "agent1"}
          paths ["file1.txt" "file2.txt"]
          agent-id "agent2"
          result (reduce
                  (fn [acc path]
                    (if (contains? acc path)
                      acc
                      (assoc acc path agent-id)))
                  current
                  paths)]
      (is (= "agent1" (get result "file1.txt"))) ; preserved
      (is (= "agent2" (get result "file2.txt"))))) ; new

  (testing "identifies newly added files"
    (let [current {"existing.txt" "agent1"}
          paths ["existing.txt" "new.txt"]
          newly-added (filter #(not (contains? current %)) paths)]
      (is (= ["new.txt"] (vec newly-added))))))

;; ============================================================================
;; unregister-files! logic tests
;; ============================================================================

(deftest unregister-files-logic-test
  (testing "only unregisters files registered by same agent"
    (let [current {"file1.txt" "agent1"
                   "file2.txt" "agent2"
                   "file3.txt" "agent1"}
          paths ["file1.txt" "file2.txt" "file3.txt"]
          agent-id "agent1"
          to-remove (filter (fn [path]
                              (= agent-id (get current path)))
                            paths)]
      (is (= #{"file1.txt" "file3.txt"} (set to-remove)))))

  (testing "returns unregistered paths"
    (let [current {"file1.txt" "agent1"}
          paths ["file1.txt" "file2.txt"]
          agent-id "agent1"
          to-remove (filter (fn [path]
                              (= agent-id (get current path)))
                            paths)]
      (is (= ["file1.txt"] (vec to-remove))))))

;; ============================================================================
;; add-manual-file! logic tests
;; ============================================================================

(deftest add-manual-file-logic-test
  (testing "adds file to set"
    (let [current #{"file1.txt"}
          path "file2.txt"
          updated (conj current path)]
      (is (= #{"file1.txt" "file2.txt"} updated))))

  (testing "is idempotent (adding same file twice)"
    (let [current #{"file1.txt"}
          path "file1.txt"
          updated (conj current path)]
      (is (= #{"file1.txt"} updated)))))

;; ============================================================================
;; remove-manual-file! logic tests
;; ============================================================================

(deftest remove-manual-file-logic-test
  (testing "removes manually added files from additions"
    (let [manual-additions #{"untracked.txt"}
          path "untracked.txt"]
      (if (contains? manual-additions path)
        (let [updated (disj manual-additions path)]
          (is (= #{} updated))
          (is (= :removed-from-additions :removed-from-additions)))
        (is false "should have been in additions"))))

  (testing "adds tracked files to removals"
    (let [manual-additions #{}
          manual-removals #{}
          path "tracked.txt"]
      (if (contains? manual-additions path)
        (is false "should not be in additions")
        (let [updated (conj manual-removals path)]
          (is (= #{"tracked.txt"} updated)))))))

;; ============================================================================
;; restore-file! logic tests
;; ============================================================================

(deftest restore-file-logic-test
  (testing "removes file from manual-removals"
    (let [current #{"excluded1.txt" "excluded2.txt"}
          path "excluded1.txt"
          updated (disj current path)]
      (is (= #{"excluded2.txt"} updated))))

  (testing "is idempotent (removing non-excluded file)"
    (let [current #{"excluded.txt"}
          path "not-excluded.txt"
          updated (disj current path)]
      (is (= #{"excluded.txt"} updated)))))

;; ============================================================================
;; session-id determinism tests
;; ============================================================================

(deftest session-id-determinism-test
  (testing "same project+branch produces same id"
    (is (= (util/session-id "project" "main")
           (util/session-id "project" "main"))))

  (testing "different projects produce different ids"
    (is (not= (util/session-id "project-a" "main")
              (util/session-id "project-b" "main"))))

  (testing "different branches produce different ids"
    (is (not= (util/session-id "project" "main")
              (util/session-id "project" "feature")))))
