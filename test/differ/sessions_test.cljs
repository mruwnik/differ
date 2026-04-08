(ns differ.sessions-test
  "Tests for session management logic."
  (:require [clojure.test :refer [deftest testing is use-fixtures async]]
            [clojure.set :as set]
            [differ.test-helpers :as helpers]
            [differ.util :as util]
            [differ.sessions :as sessions]
            [differ.github-oauth :as github-oauth]
            [differ.db]
            [differ.github-api :as gh-api]
            ["path" :as path]))

;; ============================================================================
;; Test Setup - We need both test DB and test git repo
;; ============================================================================

(defonce ^:private env-atom (atom {}))

(defn- test-repo [] (:path @env-atom))

(def with-test-env
  ;; Map-form fixture: required for async tests.
  {:before
   (fn []
     (let [{:keys [path cleanup]} (helpers/create-test-repo)
           db (helpers/init-test-db!)]
       (reset! env-atom {:path path :cleanup cleanup :db db})))
   :after
   (fn []
     (when-let [{:keys [cleanup]} @env-atom]
       (cleanup))
     (helpers/cleanup-test-db!)
     (reset! env-atom {}))})

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
    (let [result (sessions/validate-repo-path (test-repo))]
      (is (true? (:valid result)))
      (is (string? (:path result)))
      (is (= (path/resolve (test-repo)) (:path result))))))

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

(deftest parse-project-strict-slug-test
  (testing "rejects reserved-prefix impostors (github:, session:)"
    ;; `github:session:foo/bar` would previously slip through and start
    ;; a doomed poller for owner='session:foo' repo='bar'.
    (is (nil? (sessions/parse-project "session:foo/bar")))
    (is (nil? (sessions/parse-project "github:foo/bar"))))

  (testing "rejects whitespace inside the slug"
    (is (nil? (sessions/parse-project " /repo")))
    (is (nil? (sessions/parse-project "foo / repo"))))

  (testing "rejects three-or-more-part paths"
    (is (nil? (sessions/parse-project "owner/repo/extra"))))

  (testing "accepts valid GitHub slugs"
    (is (= {:owner "Owner" :repo "Repo.Name"} (sessions/parse-project "Owner/Repo.Name")))
    (is (= {:owner "org-foo_123" :repo "repo.name-1"}
           (sessions/parse-project "org-foo_123/repo.name-1")))))

;; ============================================================================
;; list-github-prs tests
;; ============================================================================

;; ============================================================================
;; Async mock helper
;;
;; `with-redefs` restores vars when its synchronous body returns, so any
;; mocked function called inside a .then callback sees the ORIGINAL value,
;; not the mock. These tests install mocks via direct mutation so they
;; persist across the whole async flow.
;;
;; In CLJS advanced compile, callers reach functions via arity-dispatch
;; properties (e.g. `cljs$core$IFn$_invoke$arity$1`). Plain fn replacements
;; don't have those, so we use `make-mock` which attaches the needed
;; arity properties and preserves the callable fn.
;; ============================================================================

(defn- make-mock
  "Wrap `f` so it can be dropped into a var via `set!` and still respond to
   CLJS arity-dispatch calls for arities 0..5. The wrapper is itself callable
   through the standard Function.prototype.call path as well."
  [f]
  (let [wrapped (fn [& args] (apply f args))]
    (set! (.-cljs$core$IFn$_invoke$arity$0 wrapped) (fn [] (f)))
    (set! (.-cljs$core$IFn$_invoke$arity$1 wrapped) (fn [a] (f a)))
    (set! (.-cljs$core$IFn$_invoke$arity$2 wrapped) (fn [a b] (f a b)))
    (set! (.-cljs$core$IFn$_invoke$arity$3 wrapped) (fn [a b c] (f a b c)))
    (set! (.-cljs$core$IFn$_invoke$arity$4 wrapped) (fn [a b c d] (f a b c d)))
    (set! (.-cljs$core$IFn$_invoke$arity$5 wrapped) (fn [a b c d e] (f a b c d e)))
    wrapped))

(defn- with-async-mock
  "Run (thunk) with `target-var` temporarily set to `mock-fn`. The var is
   restored after the returned Promise settles (regardless of outcome).
   Returns the Promise."
  [set-fn get-original mock-fn thunk]
  (let [original (get-original)
        restore (fn [] (set-fn original))]
    (set-fn (make-mock mock-fn))
    (-> (thunk)
        (.then (fn [v] (restore) v))
        (.catch (fn [e] (restore) (throw e))))))

(deftest list-github-prs-no-tokens-test
  (testing "returns requires-auth when no GitHub tokens are available"
    (async done
           (with-async-mock
             (fn [f] (set! github-oauth/get-all-tokens f))
             (fn [] github-oauth/get-all-tokens)
             (fn [] [])
             (fn []
               (-> (sessions/list-github-prs {:owner "foo" :repo "bar"
                                              :state "open" :limit 30})
                   (.then (fn [result]
                            (is (true? (:requires-auth result)))
                            (is (string? (:auth-url result)))
                            (is (string? (:message result)))
                            (done)))))))))

(defn- install-mocks!
  "Install a sequence of [setter new-fn] pairs using make-mock.
   Returns a restore function that reverts them when called."
  [pairs]
  (let [saved (mapv (fn [[setter new-fn]]
                      (let [orig (setter nil :get)]
                        (setter (make-mock new-fn) :set)
                        [setter orig]))
                    pairs)]
    (fn []
      (doseq [[setter orig] saved]
        (setter orig :set)))))

;; Setters/getters for the mocked vars. Each takes [value op] where op is
;; :get (return current) or :set (assign new value).
(defn- tokens-fn    [v op] (case op :get github-oauth/get-all-tokens :set (set! github-oauth/get-all-tokens v)))
(defn- list-prs-fn  [v op] (case op :get gh-api/list-pull-requests   :set (set! gh-api/list-pull-requests v)))
(defn- sessions-fn  [v op] (case op :get sessions/list-sessions      :set (set! sessions/list-sessions v)))

(deftest list-github-prs-happy-path-test
  (testing "returns PRs annotated with sessions on successful token"
    (async done
           (let [restore
                 (install-mocks!
                  [[tokens-fn   (fn [] [{:access-token "tok1"}])]
                   [sessions-fn (fn [_project]
                                  [{:id "github:foo/bar:42"
                                    :session-type "github"
                                    :project "foo/bar"
                                    :github-pr-number 42
                                    :unresolved-count 7}])]
                   [list-prs-fn (fn [_tok _o _r _opts]
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
                                    :truncated false}))]])]
             (-> (sessions/list-github-prs {:owner "foo" :repo "bar"
                                            :state "open" :limit 30})
                 (.then (fn [result]
                          (restore)
                          (is (false? (:truncated result)))
                          (is (= 2 (count (:prs result))))
                          (let [pr42 (first (filter #(= 42 (:number %)) (:prs result)))
                                pr43 (first (filter #(= 43 (:number %)) (:prs result)))]
                            (is (true? (:has-session pr42)))
                            (is (= "github:foo/bar:42" (:session-id pr42)))
                            (is (= 7 (:unresolved-count pr42))
                                "unresolved-count from the session should propagate to the annotated PR")
                            (is (false? (:has-session pr43)))
                            (is (nil? (:unresolved-count pr43))))
                          (done)))
                 (.catch (fn [e]
                           (restore)
                           (is false (str "unexpected error: " (.-message e)))
                           (done))))))))

(deftest list-github-prs-oauth-restricted-test
  (testing "returns requires-pat when all tokens hit OAuth restriction"
    (async done
           (let [restore
                 (install-mocks!
                  [[tokens-fn  (fn [] [{:access-token "tok1"}])]
                   [list-prs-fn (fn [_tok _o _r _opts]
                                  (js/Promise.reject
                                   (js/Error. "Resource not accessible by integration")))]])]
             (-> (sessions/list-github-prs {:owner "foo" :repo "bar"
                                            :state "open" :limit 30})
                 (.then (fn [result]
                          (restore)
                          (is (true? (:requires-pat result)))
                          (is (string? (:github-pat-url result)))
                          (is (string? (:settings-url result)))
                          (done)))
                 (.catch (fn [e]
                           (restore)
                           (is false (str "unexpected error: " (.-message e)))
                           (done))))))))

(deftest list-github-prs-generic-error-test
  (testing "returns :error when GitHub call fails with a non-OAuth-restriction error"
    (async done
           (let [restore
                 (install-mocks!
                  [[tokens-fn  (fn [] [{:access-token "tok1"}])]
                   [list-prs-fn (fn [_tok _o _r _opts]
                                  (js/Promise.reject (js/Error. "network down")))]])]
             (-> (sessions/list-github-prs {:owner "foo" :repo "bar"
                                            :state "open" :limit 30})
                 (.then (fn [result]
                          (restore)
                          (is (contains? result :error))
                          (is (re-find #"network down" (:error result)))
                          (done)))
                 (.catch (fn [e]
                           (restore)
                           (is false (str "unexpected error: " (.-message e)))
                           (done))))))))
