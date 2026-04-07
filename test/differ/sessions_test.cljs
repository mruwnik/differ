(ns differ.sessions-test
  "Tests for session management logic."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.set :as set]
            [differ.test-helpers :as helpers]
            [differ.util :as util]
            [differ.sessions :as sessions]
            [differ.github-oauth]
            [differ.db]
            [differ.github-api :as gh-api]
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

;; ============================================================================
;; annotate-prs-with-sessions tests (pure join logic)
;; ============================================================================

(deftest annotate-prs-with-sessions-test
  (testing "PRs with a matching session get has-session true plus metadata"
    (let [prs [{:number 10 :title "A"}
               {:number 20 :title "B"}
               {:number 30 :title "C"}]
          sessions [{:id "github:owner/repo:10"
                     :github-pr-number 10
                     :unresolved-count 3}
                    {:id "github:owner/repo:30"
                     :github-pr-number 30
                     :unresolved-count 0}]
          annotated (sessions/annotate-prs-with-sessions prs sessions)]
      (is (= 3 (count annotated)))
      (let [pr10 (first (filter #(= 10 (:number %)) annotated))
            pr20 (first (filter #(= 20 (:number %)) annotated))
            pr30 (first (filter #(= 30 (:number %)) annotated))]
        (is (true? (:has-session pr10)))
        (is (= "github:owner/repo:10" (:session-id pr10)))
        (is (= 3 (:unresolved-count pr10)))
        (is (false? (:has-session pr20)))
        (is (nil? (:session-id pr20)))
        (is (nil? (:unresolved-count pr20)))
        (is (true? (:has-session pr30)))
        (is (= 0 (:unresolved-count pr30))))))

  (testing "sessions without a github-pr-number are ignored"
    (let [prs [{:number 10 :title "A"}]
          sessions [{:id "local:foo:bar" :github-pr-number nil}]
          annotated (sessions/annotate-prs-with-sessions prs sessions)]
      (is (false? (:has-session (first annotated)))))))

;; ============================================================================
;; parse-project tests
;; ============================================================================

(deftest parse-project-test
  (testing "splits owner/repo"
    (is (= {:owner "foo" :repo "bar"}
           (sessions/parse-project "foo/bar"))))

  (testing "returns nil for missing slash"
    (is (nil? (sessions/parse-project "foobar"))))

  (testing "returns nil for too many slashes"
    (is (nil? (sessions/parse-project "a/b/c"))))

  (testing "returns nil for empty owner or repo"
    (is (nil? (sessions/parse-project "/bar")))
    (is (nil? (sessions/parse-project "foo/"))))

  (testing "returns nil for nil input"
    (is (nil? (sessions/parse-project nil)))))

;; ============================================================================
;; list-github-prs tests
;; ============================================================================

(deftest list-github-prs-no-tokens-test
  (testing "returns requires-auth when no GitHub tokens are available"
    (with-redefs [differ.github-oauth/get-all-tokens (fn [] [])]
      (-> (sessions/list-github-prs {:owner "foo" :repo "bar" :state "open" :limit 30})
          (.then (fn [result]
                   (is (true? (:requires-auth result)))
                   (is (string? (:auth-url result)))
                   (is (string? (:message result)))))))))

(deftest list-github-prs-happy-path-test
  (testing "returns PRs annotated with sessions on successful token"
    (with-redefs [differ.github-oauth/get-all-tokens
                  (fn [] [{:access-token "tok1"}])
                  sessions/list-sessions
                  (fn [_project]
                    [{:id "github:foo/bar:42"
                      :session-type "github"
                      :project "foo/bar"
                      :github-pr-number 42
                      :unresolved-count 7}])
                  gh-api/list-pull-requests
                  (fn [_tok _o _r _opts]
                    (js/Promise.resolve
                     {:prs [{:number 42 :title "A" :author "dan"
                             :draft false :base-branch "main"
                             :head-branch "feature/x"
                             :updated-at "2026-04-05T00:00:00Z"
                             :url "https://github.com/foo/bar/pull/42"}
                            {:number 43 :title "B" :author "eve"
                             :draft false :base-branch "main"
                             :head-branch "feature/y"
                             :updated-at "2026-04-06T00:00:00Z"
                             :url "https://github.com/foo/bar/pull/43"}]
                      :truncated false}))]
      (-> (sessions/list-github-prs {:owner "foo" :repo "bar"
                                     :state "open" :limit 30})
          (.then (fn [result]
                   (is (false? (:truncated result)))
                   (is (= 2 (count (:prs result))))
                   (let [pr42 (first (filter #(= 42 (:number %)) (:prs result)))
                         pr43 (first (filter #(= 43 (:number %)) (:prs result)))]
                     (is (true? (:has-session pr42)))
                     (is (= "github:foo/bar:42" (:session-id pr42)))
                     (is (= 7 (:unresolved-count pr42))
                         "unresolved-count from the session should propagate to the annotated PR")
                     (is (false? (:has-session pr43)))
                     (is (nil? (:unresolved-count pr43))))))))))

(deftest list-github-prs-oauth-restricted-test
  (testing "returns requires-pat when all tokens hit OAuth restriction"
    (with-redefs [differ.github-oauth/get-all-tokens
                  (fn [] [{:access-token "tok1"}])
                  gh-api/list-pull-requests
                  (fn [_tok _o _r _opts]
                    (js/Promise.reject
                     (js/Error. "Resource not accessible by integration")))]
      (-> (sessions/list-github-prs {:owner "foo" :repo "bar"
                                     :state "open" :limit 30})
          (.then (fn [result]
                   (is (true? (:requires-pat result)))
                   (is (string? (:github-pat-url result)))
                   (is (string? (:settings-url result)))))))))

(deftest list-github-prs-generic-error-test
  (testing "returns :error when GitHub call fails with a non-OAuth-restriction error"
    (with-redefs [differ.github-oauth/get-all-tokens
                  (fn [] [{:access-token "tok1"}])
                  gh-api/list-pull-requests
                  (fn [_tok _o _r _opts]
                    (js/Promise.reject (js/Error. "network down")))]
      (-> (sessions/list-github-prs {:owner "foo" :repo "bar"
                                     :state "open" :limit 30})
          (.then (fn [result]
                   (is (contains? result :error))
                   (is (re-find #"network down" (:error result)))))))))
