(ns differ.github-events-test
  "Tests for differ.github-events — GitHub PR poller + diff detection.
   The generic ring buffer / subscriber / blocking wait live in
   differ.event-stream and are tested in differ.event-stream-test."
  (:require [clojure.test :refer [deftest testing is use-fixtures async]]
            [differ.config :as config]
            [differ.event-stream :as es]
            [differ.github-events :as ge]))

;; Stub `resolve-session-id` to avoid touching the production DB. Tests
;; that want to assert a present session-id override this locally.
(defn- stub-resolve-session-id [_project _pr-number] nil)

(defonce ^:private orig-resolve-session-id ge/resolve-session-id)

(use-fixtures :each
  {:before (fn []
             (set! ge/resolve-session-id stub-resolve-session-id)
             (ge/reset-for-tests!)
             (es/reset-for-tests!))
   :after  (fn []
             (ge/reset-for-tests!)
             (es/reset-for-tests!)
             (set! ge/resolve-session-id orig-resolve-session-id))})

;; ============================================================================
;; Persistent mock helper
;;
;; `with-redefs` restores vars when its synchronous body returns, which
;; clobbers mocks that are needed across async boundaries. These helpers
;; install mocks via direct `set!` and restore them later. A make-mock
;; wrapper attaches CLJS arity-dispatch properties so dispatched calls
;; still work.
;;
;; TEST-ONLY: assumes shadow-cljs `:none` optimizations (the default for
;; dev/test). Under `:advanced` the compiler can inline call sites and
;; the arity-shim below would silently break. Handles arities 0-4.
;; ============================================================================

(defn- make-mock [f]
  (let [wrapped (fn [& args] (apply f args))]
    (set! (.-cljs$core$IFn$_invoke$arity$0 wrapped) (fn [] (f)))
    (set! (.-cljs$core$IFn$_invoke$arity$1 wrapped) (fn [a] (f a)))
    (set! (.-cljs$core$IFn$_invoke$arity$2 wrapped) (fn [a b] (f a b)))
    (set! (.-cljs$core$IFn$_invoke$arity$3 wrapped) (fn [a b c] (f a b c)))
    (set! (.-cljs$core$IFn$_invoke$arity$4 wrapped) (fn [a b c d] (f a b c d)))
    wrapped))

(defn- fetch-fn       [v op] (case op :get ge/fetch-poller-prs        :set (set! ge/fetch-poller-prs v)))
(defn- merge-fn       [v op] (case op :get ge/resolve-merge-status    :set (set! ge/resolve-merge-status v)))
(defn- interval-fn    [v op] (case op :get config/github-poll-interval-ms :set (set! config/github-poll-interval-ms v)))
(defn- grace-fn       [v op] (case op :get config/github-poller-grace-ms  :set (set! config/github-poller-grace-ms v)))

(defn- touch-activity!
  "Test helper: stamp `:last-activity` on a poller entry so the grace-
   period check in the next tick doesn't immediately stop the poller."
  [scope]
  (swap! ge/poller-state update scope
         (fn [state]
           (-> (or state (ge/empty-poller-state))
               (assoc :last-activity (js/Date.now))))))

(defn- install-mocks!
  "Install persistent mocks. `pairs` is a seq of [setter-fn mock-fn] tuples.
   Returns a restore function that reverts everything."
  [pairs]
  (let [saved (mapv (fn [[setter mock-fn]]
                      (let [orig (setter nil :get)]
                        (setter (make-mock mock-fn) :set)
                        [setter orig]))
                    pairs)]
    (fn []
      (doseq [[setter orig] saved]
        (setter orig :set)))))

;; Test project + corresponding github scope.
(def ^:private test-project "o/r")
(def ^:private test-scope "github:o/r")

;; ============================================================================
;; Diff detection tests
;; ============================================================================

(defn- pr
  "Shortcut for building a PR map in tests."
  [n & {:as overrides}]
  (merge {:number n
          :title (str "PR " n)
          :url (str "https://github.com/o/r/pull/" n)
          :author "alice"
          :draft false
          :base-branch "main"
          :head-branch (str "feature/" n)
          :head-sha (str "sha-" n "-a")
          :unresolved-count 0
          :review-count 0
          :checks-status nil}
         overrides))

(defn- pr-with-updated-at
  [n updated-at & {:as overrides}]
  (apply pr n :updated-at updated-at (mapcat identity overrides)))

(deftest detect-events-baseline-emits-nothing-test
  (testing "nil old-cache (first poll) emits no events and populates new-cache"
    (let [result (ge/detect-events test-project nil [(pr 1) (pr 2)])]
      (is (= [] (:events result)))
      (is (= 2 (count (:new-cache result))))
      (is (= "sha-1-a" (get-in result [:new-cache 1 :head-sha]))))))

(deftest detect-events-pr-opened-test
  (testing "PR present in new list but not in old-cache emits :pr-opened"
    (let [old {1 {:head-sha "sha-1-a" :unresolved-count 0 :review-count 0 :checks-status nil}}
          result (ge/detect-events test-project old [(pr 1) (pr 2)])
          events (:events result)]
      (is (= 1 (count events)))
      (let [ev (first events)]
        (is (= :pr-opened (:event-type ev)))
        (is (= 2 (:pr-number ev)))
        (is (= test-project (:project ev)))
        (is (= "https://github.com/o/r/pull/2" (:pr-url ev)))
        (is (nil? (:session-id ev)))
        (is (= {} (:details ev)))))))

(deftest detect-events-pr-head-changed-test
  (testing "head-sha change emits :pr-head-changed with both shas"
    (let [old {1 {:head-sha "old-sha" :unresolved-count 0 :review-count 0 :checks-status nil}}
          result (ge/detect-events test-project old [(pr 1 :head-sha "new-sha")])
          ev (first (:events result))]
      (is (= :pr-head-changed (:event-type ev)))
      (is (= {:new-head-sha "new-sha"
              :previous-head-sha "old-sha"}
             (:details ev))))))

(deftest detect-events-pr-feedback-changed-comments-test
  (testing "unresolved-count change alone emits :pr-feedback-changed"
    (let [old {1 {:head-sha "sha-1-a" :unresolved-count 10 :review-count 1 :checks-status :success}}
          result (ge/detect-events test-project old
                                   [(pr 1 :unresolved-count 15 :review-count 1 :checks-status :success)])
          ev (first (:events result))]
      (is (= :pr-feedback-changed (:event-type ev)))
      (is (= 15 (get-in ev [:details :unresolved-count])))
      (is (= 10 (get-in ev [:details :previous-unresolved-count])))
      (is (= 1 (get-in ev [:details :review-count])))
      (is (= :success (get-in ev [:details :checks-status])))
      (is (string? (get-in ev [:details :summary]))))))

(deftest detect-events-pr-feedback-changed-checks-test
  (testing "checks-status change alone also triggers :pr-feedback-changed"
    (let [old {1 {:head-sha "sha-1-a" :unresolved-count 0 :review-count 0 :checks-status :pending}}
          result (ge/detect-events test-project old [(pr 1 :checks-status :failure)])
          ev (first (:events result))]
      (is (= :pr-feedback-changed (:event-type ev)))
      (is (= :failure (get-in ev [:details :checks-status])))
      (is (= :pending (get-in ev [:details :previous-checks-status]))))))

(deftest detect-events-pr-closed-test
  (testing "PR present in cache but not in new list emits :pr-closed"
    (let [old {1 {:head-sha "sha-1-a" :unresolved-count 0 :review-count 0 :checks-status nil
                  :last-seen {:url "https://example.com/o/r/pull/1" :title "PR 1"
                              :author "alice" :head-branch "f/1" :base-branch "main" :draft false}}
               2 {:head-sha "sha-2-a" :unresolved-count 0 :review-count 0 :checks-status nil
                  :last-seen {:url "https://example.com/o/r/pull/2" :title "PR 2"
                              :author "bob" :head-branch "f/2" :base-branch "main" :draft true}}}
          result (ge/detect-events test-project old [(pr 1)])
          ev (first (:events result))]
      (is (= :pr-closed (:event-type ev)))
      (is (= 2 (:pr-number ev)))
      ;; URL/title/author/branches/draft come from cached :last-seen — GHE-friendly
      (is (= "https://example.com/o/r/pull/2" (:pr-url ev)))
      (is (= "PR 2" (:title ev)))
      (is (= "bob" (:author ev)))
      (is (= "f/2" (:head-branch ev)))
      (is (= "main" (:base-branch ev)))
      (is (= true (:draft ev)))
      (is (contains? (:details ev) :merged))
      (is (nil? (get-in ev [:details :merged]))
          "merge status is filled in later by poller after async fetch")
      (is (nil? (:session-id ev))))))

(deftest detect-events-head-and-feedback-simultaneously-test
  (testing "same PR with both head-sha and comment changes emits both events"
    (let [old {1 {:head-sha "old-sha" :unresolved-count 5 :review-count 1 :checks-status :success}}
          result (ge/detect-events test-project old
                                   [(pr 1 :head-sha "new-sha" :unresolved-count 10)])
          types (set (map :event-type (:events result)))]
      (is (contains? types :pr-head-changed))
      (is (contains? types :pr-feedback-changed)))))

(deftest detect-events-no-changes-emits-nothing-test
  (testing "identical old and new state emits no events"
    (let [old {1 {:head-sha "sha-1-a" :unresolved-count 0 :review-count 0 :checks-status nil}}
          result (ge/detect-events test-project old [(pr 1)])]
      (is (= [] (:events result))))))

(deftest detect-events-session-id-nil-when-no-session-test
  (testing "session-id is nil when no differ session exists for the PR"
    (let [result (ge/detect-events test-project {} [(pr 42)])
          ev (first (:events result))]
      (is (nil? (:session-id ev))))))

(deftest detect-events-session-id-set-when-session-exists-test
  (testing "session-id reflects the resolved candidate when a session does exist"
    (let [orig ge/resolve-session-id]
      (set! ge/resolve-session-id
            (fn [project pr-number] (str "github:" project ":" pr-number)))
      (try
        (let [result (ge/detect-events test-project {} [(pr 42)])
              ev (first (:events result))]
          (is (= "github:o/r:42" (:session-id ev))))
        (finally
          (set! ge/resolve-session-id orig))))))

;; ============================================================================
;; Truncation handling tests
;; ============================================================================

(deftest detect-events-truncated-skips-likely-out-of-window-closes-test
  (testing "when truncated, only PRs whose cached updated-at is at/above the
            window floor are eligible for :pr-closed"
    (let [old {1 {:head-sha "s1" :unresolved-count 0 :review-count 0 :checks-status nil
                  :updated-at "2024-07-01T00:00:00Z"
                  :last-seen {:url "u1" :title "PR 1" :author "a"
                              :head-branch "h" :base-branch "b" :draft false}}
               3 {:head-sha "s3" :unresolved-count 0 :review-count 0 :checks-status nil
                  :updated-at "2024-01-01T00:00:00Z"
                  :last-seen {:url "u3" :title "PR 3" :author "a"
                              :head-branch "h" :base-branch "b" :draft false}}}
          new-prs [(pr-with-updated-at 4 "2024-06-01T00:00:00Z")]
          {:keys [events]} (ge/detect-events test-project old new-prs true)
          closed (filter #(= :pr-closed (:event-type %)) events)
          opened (filter #(= :pr-opened (:event-type %)) events)]
      (is (= 1 (count opened)) "PR 4 is brand new")
      (is (= 1 (count closed))
          "only PR 1 (above window floor) should be flagged closed")
      (is (= 1 (:pr-number (first closed)))))))

(deftest detect-events-not-truncated-still-emits-all-closes-test
  (testing "when result is not truncated, missing PRs are closed regardless of updated-at"
    (let [old {1 {:head-sha "s1" :unresolved-count 0 :review-count 0 :checks-status nil
                  :updated-at "2024-01-01T00:00:00Z"
                  :last-seen {:url "u1" :title "PR 1" :author "a"
                              :head-branch "h" :base-branch "b" :draft false}}}
          {:keys [events]} (ge/detect-events test-project old [] false)]
      (is (= 1 (count events)))
      (is (= :pr-closed (:event-type (first events)))))))

(deftest detect-events-filters-nil-pr-number-test
  (testing "PRs with nil :number are filtered out so they don't corrupt the cache"
    (let [{:keys [new-cache events]}
          (ge/detect-events test-project nil [(pr 1) (assoc (pr 2) :number nil) (pr 3)])]
      (is (= #{1 3} (set (keys new-cache))))
      (is (= [] events)))))

(deftest detect-events-truncated-empty-new-prs-no-closes-test
  (testing "truncated=true with empty new-prs emits NO :pr-closed events"
    (let [old {1 {:head-sha "s1" :unresolved-count 0 :review-count 0 :checks-status nil
                  :updated-at "2024-07-01T00:00:00Z"
                  :last-seen {:url "u1" :title "PR 1" :author "a"
                              :head-branch "h" :base-branch "b" :draft false}}}
          {:keys [events]} (ge/detect-events test-project old [] true)]
      (is (= [] events)))))

(deftest detect-events-truncated-cache-missing-updated-at-skipped-test
  (testing "truncated=true and a cache entry missing :updated-at is skipped"
    (let [old {1 {:head-sha "s1" :unresolved-count 0 :review-count 0 :checks-status nil
                  :last-seen {:url "u1" :title "PR 1" :author "a"
                              :head-branch "h" :base-branch "b" :draft false}}}
          new-prs [(pr-with-updated-at 2 "2024-06-01T00:00:00Z")]
          {:keys [events]} (ge/detect-events test-project old new-prs true)
          closed (filter #(= :pr-closed (:event-type %)) events)]
      (is (= 0 (count closed))))))

;; ============================================================================
;; Scope parsing tests
;; ============================================================================

(deftest parse-scope-valid-test
  (testing "parse-scope extracts [owner repo project]"
    (is (= ["owner" "repo" "owner/repo"] (ge/parse-scope "github:owner/repo")))
    (is (= ["EquiStamp" "cairn" "EquiStamp/cairn"]
           (ge/parse-scope "github:EquiStamp/cairn")))))

(deftest parse-scope-rejects-invalid-test
  (testing "parse-scope returns nil for malformed scopes"
    (is (nil? (ge/parse-scope nil)))
    (is (nil? (ge/parse-scope "")))
    (is (nil? (ge/parse-scope "github:")))
    (is (nil? (ge/parse-scope "github:owner")))
    (is (nil? (ge/parse-scope "github:owner/")))
    (is (nil? (ge/parse-scope "github:/repo")))
    (is (nil? (ge/parse-scope "session:owner/repo")))
    (is (nil? (ge/parse-scope "not-a-scope")))))

;; ============================================================================
;; poll-once! tests
;; ============================================================================

(deftest poll-once-baseline-populates-cache-test
  (testing "first poll (nil cache) populates cache and emits no events"
    (async done
           (let [fetched (atom 0)
                 restore (install-mocks!
                          [[fetch-fn (fn [_scope]
                                       (swap! fetched inc)
                                       (js/Promise.resolve
                                        {:success true :prs [(pr 1) (pr 2)]}))]
                           [merge-fn (fn [_s _num] (js/Promise.resolve false))]])]
             (-> (ge/poll-once! test-scope)
                 (.then (fn [_]
                          (let [poller (ge/get-poller-state test-scope)
                                stream (es/get-scope-state test-scope)]
                            (is (= 2 (count (:cache poller))))
                            (is (or (nil? stream) (= [] (:events stream))))
                            (is (= 1 @fetched)))
                          (restore)
                          (done))))))))

(deftest poll-once-after-baseline-emits-events-test
  (testing "second poll with changed PR list emits events through event-stream"
    (async done
           (let [prs-atom (atom [(pr 1) (pr 2)])
                 restore (install-mocks!
                          [[fetch-fn (fn [_scope]
                                       (js/Promise.resolve {:success true :prs @prs-atom}))]
                           [merge-fn (fn [_s _num] (js/Promise.resolve true))]])]
             (-> (ge/poll-once! test-scope)
                 (.then (fn [_]
                          (reset! prs-atom [(pr 1) (pr 3)]) ; close #2, add #3
                          (ge/poll-once! test-scope)))
                 (.then (fn [_]
                          (let [stream (es/get-scope-state test-scope)
                                events (:events stream)
                                types (set (map :event-type events))]
                            (is (contains? types :pr-opened))
                            (is (contains? types :pr-closed))
                            (let [closed (first (filter #(= :pr-closed (:event-type %)) events))]
                              (is (true? (get-in closed [:details :merged])))))
                          (restore)
                          (done))))))))

(deftest poll-once-fetch-error-skips-cache-update-test
  (testing "poll-once on fetch error logs and does not update cache"
    (async done
           (let [restore (install-mocks!
                          [[fetch-fn (fn [_scope]
                                       (js/Promise.resolve {:success false :error "network down"}))]
                           [merge-fn (fn [_s _num] (js/Promise.resolve false))]])]
             (swap! ge/poller-state assoc test-scope
                    (assoc (ge/empty-poller-state)
                           :cache {1 {:head-sha "original"
                                      :unresolved-count 0
                                      :review-count 0
                                      :checks-status nil
                                      :updated-at "2024-01-01T00:00:00Z"
                                      :last-seen {:url "u" :title "t"
                                                  :author "a" :head-branch "h"
                                                  :base-branch "b" :draft false}}}))
             (-> (ge/poll-once! test-scope)
                 (.then (fn [_]
                          (let [poller (ge/get-poller-state test-scope)
                                stream (es/get-scope-state test-scope)]
                            (is (= "original" (get-in poller [:cache 1 :head-sha])))
                            (is (or (nil? stream) (= [] (:events stream)))))
                          (restore)
                          (done))))))))

(deftest poll-once-closed-without-merge-test
  (testing "resolve-merge-status false yields :pr-closed with :merged false"
    (async done
           (let [prs-atom (atom [(pr 1) (pr 2)])
                 restore (install-mocks!
                          [[fetch-fn (fn [_scope]
                                       (js/Promise.resolve {:success true :prs @prs-atom}))]
                           [merge-fn (fn [_s _num] (js/Promise.resolve false))]])]
             (-> (ge/poll-once! test-scope)
                 (.then (fn [_]
                          (reset! prs-atom [(pr 1)])
                          (ge/poll-once! test-scope)))
                 (.then (fn [_]
                          (let [events (:events (es/get-scope-state test-scope))
                                closed (first (filter #(= :pr-closed (:event-type %)) events))]
                            (is (some? closed))
                            (is (false? (get-in closed [:details :merged]))))
                          (restore)
                          (done))))))))

;; ============================================================================
;; Lifecycle tests
;; ============================================================================

(deftest start-poller-schedules-timer-test
  (testing "start-poller-if-needed! kicks off a poll and schedules a next tick"
    (async done
           (let [polls (atom 0)
                 restore (install-mocks!
                          [[fetch-fn (fn [_] (swap! polls inc)
                                       (js/Promise.resolve {:success true :prs []}))]
                           [merge-fn (fn [_s _n] (js/Promise.resolve false))]
                           [interval-fn (fn [] 30)]
                           [grace-fn (fn [] 1000000)]])]
             (touch-activity! test-scope)
             (ge/start-poller-if-needed! test-scope)
             (js/setTimeout
              (fn []
                (let [state (ge/get-poller-state test-scope)]
                  (is (some? (:timer state)) "scheduled next tick should set :timer")
                  (is (not= :starting (:timer state))))
                (let [baseline @polls]
                  (js/setTimeout
                   (fn []
                     (is (> @polls baseline)
                         (str "scheduled tick should grow polls past baseline "
                              baseline "; observed " @polls))
                     (ge/stop-poller! test-scope)
                     (restore)
                     (done))
                   120)))
              30)))))

(deftest stop-poller-clears-timer-test
  (testing "stop-poller! clears the timer; entries with no cache are reclaimed"
    (async done
           (let [restore (install-mocks!
                          [[fetch-fn (fn [_] (js/Promise.resolve {:success true :prs []}))]
                           [merge-fn (fn [_s _n] (js/Promise.resolve false))]
                           [interval-fn (fn [] 100000)]])]
             (ge/start-poller-if-needed! test-scope)
             (js/setTimeout
              (fn []
           ;; Pre-seed poller state with nil cache so the entry is
           ;; reclaimable (the baseline poll returned empty PRs, so cache
           ;; becomes {}, but empty-cache still counts as "has cache"
           ;; unless we explicitly null it).
                (swap! ge/poller-state update test-scope assoc :cache nil)
                (ge/stop-poller! test-scope)
                (is (nil? (ge/get-poller-state test-scope)))
                (restore)
                (done))
              50)))))

(deftest stop-poller-keeps-entry-with-cache-test
  (testing "stop-poller! preserves the entry while the cache is populated"
    (async done
           (let [restore (install-mocks!
                          [[fetch-fn (fn [_] (js/Promise.resolve {:success true :prs [(pr 1)]}))]
                           [merge-fn (fn [_s _n] (js/Promise.resolve false))]
                           [interval-fn (fn [] 100000)]])]
             (ge/start-poller-if-needed! test-scope)
             (js/setTimeout
              (fn []
                (ge/stop-poller! test-scope)
                (let [state (ge/get-poller-state test-scope)]
                  (is (some? state))
                  (is (nil? (:timer state)))
                  (is (= #{1} (set (keys (:cache state))))))
                (restore)
                (done))
              50)))))

(deftest tick-stops-when-grace-period-elapsed-test
  (testing "after grace period elapses, tick stops the poller"
    (async done
           (let [restore (install-mocks!
                          [[grace-fn (fn [] 1)]
                           [interval-fn (fn [] 100000)]
                           [fetch-fn (fn [_] (js/Promise.resolve {:success true :prs []}))]
                           [merge-fn (fn [_s _n] (js/Promise.resolve false))]])]
             (swap! ge/poller-state assoc test-scope
                    (assoc (ge/empty-poller-state) :last-activity 0))
             (-> (ge/tick! test-scope)
                 (.then (fn [_]
                          (let [state (ge/get-poller-state test-scope)]
                       ;; Either reclaimed (nil) or timer cleared.
                            (is (or (nil? state) (nil? (:timer state)))))
                          (restore)
                          (done))))))))

(deftest tick-reschedules-within-grace-period-test
  (testing "tick keeps polling while within grace period"
    (async done
           (let [restore (install-mocks!
                          [[grace-fn (fn [] 100000)]
                           [interval-fn (fn [] 100000)]
                           [fetch-fn (fn [_] (js/Promise.resolve {:success true :prs []}))]
                           [merge-fn (fn [_s _n] (js/Promise.resolve false))]])]
             (swap! ge/poller-state assoc test-scope
                    (assoc (ge/empty-poller-state)
                           :last-activity (js/Date.now)))
             (-> (ge/tick! test-scope)
                 (.then (fn [_]
                          (let [state (ge/get-poller-state test-scope)]
                            (is (some? (:timer state))
                                "within grace period, timer should still be running"))
                          (ge/stop-poller! test-scope)
                          (restore)
                          (done))))))))

;; ============================================================================
;; wait-for-scope! integration — github scope lazy-starts the poller
;; ============================================================================

(deftest wait-for-scope-starts-poller-test
  (testing "wait-for-scope! on a github scope starts the poller lazily"
    (async done
           (let [polls (atom 0)
                 restore (install-mocks!
                          [[fetch-fn (fn [_]
                                       (swap! polls inc)
                                       (js/Promise.
                                        (fn [resolve _]
                                          (js/setTimeout
                                           #(resolve {:success true :prs [(pr 1)]})
                                           10))))]
                           [merge-fn (fn [_s _n] (js/Promise.resolve false))]
                           [interval-fn (fn [] 100000)]
                           [grace-fn (fn [] 100000)]])]
             (-> (ge/wait-for-scope! test-scope
                                     {:since-seq 0 :timeout-ms 50 :max-events 50})
                 (.then (fn [result]
                     ;; Baseline poll ran (no events emitted yet because
                     ;; baseline populates cache silently) and the wait
                     ;; timed out because no detectable changes happened.
                          (is (true? (:timed-out result)))
                          (is (>= @polls 1))
                          (is (some? (ge/get-poller-state test-scope))
                              "poller state should exist after wait-for-scope!")
                          (ge/stop-poller! test-scope)
                          (restore)
                          (done))))))))

(deftest wait-for-scope-asserts-on-invalid-scope-test
  (testing "wait-for-scope! throws (precondition) on a scope the MCP dispatcher
            would already have rejected. Invalid scopes are a programming
            error at this boundary, not a user-facing envelope."
    (is (thrown? js/Error
                 (ge/wait-for-scope! "github:not-valid"
                                     {:since-seq 0 :timeout-ms 10 :max-events 50})))))

;; ============================================================================
;; Shutdown tests
;; ============================================================================

(deftest shutdown-stops-pollers-and-wakes-subscribers-test
  (testing "event-stream/shutdown-all! + stop-all-pollers! together clear
            timers and resolve pending waits as timed-out, even when the
            baseline poll is still in flight"
    (async done
           (let [restore (install-mocks!
                          [[fetch-fn (fn [_]
                                       (js/Promise.
                                        (fn [resolve _]
                                          (js/setTimeout
                                           #(resolve {:success true :prs []})
                                           50))))]
                           [merge-fn (fn [_s _n] (js/Promise.resolve false))]
                           [interval-fn (fn [] 100000)]])]
             (let [p (ge/wait-for-scope! test-scope
                                         {:since-seq 0 :timeout-ms 60000 :max-events 50})]
               (js/setTimeout (fn []
                                (es/shutdown-all!)
                                (ge/stop-all-pollers!)) 10)
               (-> p
                   (.then (fn [result]
                            (is (true? (:timed-out result)))
                            (is (true? (es/shutdown?-value)))
                       ;; Give the baseline poll time to settle before restoring.
                            (js/setTimeout
                             (fn []
                               (restore)
                               (done))
                             80)))))))))

(deftest shutdown-flag-prevents-new-pollers-test
  (testing "after event-stream/shutdown-all!, start-poller-if-needed! is a no-op
            and wait-for-scope! times out immediately"
    (async done
           (let [polls (atom 0)
                 restore (install-mocks!
                          [[fetch-fn (fn [_]
                                       (swap! polls inc)
                                       (js/Promise.resolve {:success true :prs []}))]
                           [merge-fn (fn [_s _n] (js/Promise.resolve false))]
                           [interval-fn (fn [] 100000)]])]
             (es/shutdown-all!)
             (ge/start-poller-if-needed! "github:post/shutdown")
             (js/setTimeout
              (fn []
                (is (= 0 @polls) "no poll should fire after shutdown")
                (is (nil? (ge/get-poller-state "github:post/shutdown"))
                    "no poller entry should be created after shutdown")
                (-> (ge/wait-for-scope! test-scope
                                        {:since-seq 0 :timeout-ms 60000 :max-events 50})
                    (.then (fn [result]
                             (is (true? (:timed-out result)))
                             (restore)
                             (done)))))
              30)))))

;; ============================================================================
;; Concurrent start / timer-leak regression test
;; ============================================================================

(deftest concurrent-start-poller-installs-only-one-timer-test
  (testing "two concurrent start-poller-if-needed! calls must NOT race to install
            two setTimeout handles. Regression for the double-start leak."
    (async done
           (let [polls (atom 0)
                 restore (install-mocks!
                          [[fetch-fn (fn [_]
                                       (swap! polls inc)
                                       (js/Promise.
                                        (fn [resolve _]
                                          (js/setTimeout
                                           #(resolve {:success true :prs []})
                                           20))))]
                           [merge-fn (fn [_s _n] (js/Promise.resolve false))]
                           [interval-fn (fn [] 100000)]
                           [grace-fn (fn [] 1000000)]])]
             (touch-activity! "github:race/start")
             (ge/start-poller-if-needed! "github:race/start")
             (ge/start-poller-if-needed! "github:race/start")
             (js/setTimeout
              (fn []
                (let [state (ge/get-poller-state "github:race/start")]
                  (is (some? (:timer state)))
                  (is (not= :starting (:timer state)))
                  (is (= 1 @polls)
                      "only one caller should have initiated a baseline poll"))
                (ge/stop-poller! "github:race/start")
                (restore)
                (done))
              80)))))

(deftest start-poller-shutdown-race-cleans-up-starting-marker-test
  (testing "if shutdown fires while a baseline poll is in flight, the
            `:starting` entry must NOT leak into poller-state. Regression
            for the shutdown race in start-poller-if-needed!."
    (async done
           (let [restore (install-mocks!
                          [[fetch-fn (fn [_]
                                       (js/Promise.
                                        (fn [resolve _]
                                          ;; Slow baseline poll so shutdown
                                          ;; fires before install! runs.
                                          (js/setTimeout
                                           #(resolve {:success true :prs []})
                                           60))))]
                           [merge-fn (fn [_s _n] (js/Promise.resolve false))]
                           [interval-fn (fn [] 100000)]
                           [grace-fn (fn [] 1000000)]])
                 race-scope "github:race/shutdown"]
             (ge/start-poller-if-needed! race-scope)
             ;; Shutdown fires while the baseline poll's setTimeout is still
             ;; pending. install! should observe shutdown? = true and clean
             ;; up its `:starting` marker instead of scheduling a tick.
             (js/setTimeout (fn [] (es/shutdown-all!)) 10)
             (js/setTimeout
              (fn []
                ;; After install! has had a chance to run, no zombie entry
                ;; should remain.
                (is (nil? (ge/get-poller-state race-scope))
                    "shutdown race must not leave a `:starting` entry behind")
                (restore)
                (done))
              140)))))

(deftest shutdown-during-in-flight-poll-cleans-up-cache-populated-entry-test
  (testing "if `event-stream/shutdown-all!` + `stop-all-pollers!` fire while
            a baseline poll is in flight, the resolved fetch must NOT
            strand a cache-populated entry in poller-state. Regression for
            the secondary shutdown leak: stop-poller! bumps generation
            mid-flight, then run-poll-cycle! writes :cache, then install!'s
            generation guard fails and the dissoc never fires.

            Fixed by having run-poll-cycle! short-circuit on (shutdown?)
            after the fetch resolves and dissoc its scope before writing
            cache or appending events."
    (async done
           (let [restore (install-mocks!
                          [[fetch-fn (fn [_]
                                       (js/Promise.
                                        (fn [resolve _]
                                          ;; Slow fetch so shutdown +
                                          ;; stop-all-pollers! fire
                                          ;; before run-poll-cycle!
                                          ;; resolves.
                                          (js/setTimeout
                                           #(resolve {:success true
                                                      :prs [(pr 1) (pr 2)]})
                                           60))))]
                           [merge-fn (fn [_s _n] (js/Promise.resolve false))]
                           [interval-fn (fn [] 100000)]
                           [grace-fn (fn [] 1000000)]])
                 race-scope "github:race/cache-strand"]
             (ge/start-poller-if-needed! race-scope)
             ;; Production sequence: shutdown flag, then stop-all-pollers!.
             ;; The latter bumps :generation while :in-flight is still set,
             ;; which is what would defeat install!'s generation guard.
             (js/setTimeout
              (fn []
                (es/shutdown-all!)
                (ge/stop-all-pollers!))
              10)
             ;; Wait long enough for the fetch to resolve and the .then
             ;; chain (run-poll-cycle! → poll-once! cleanup → install!)
             ;; to settle.
             (js/setTimeout
              (fn []
                (let [state (ge/get-poller-state race-scope)]
                  (is (nil? state)
                      (str "shutdown-mid-poll must not strand a cache-"
                           "populated entry; got " (pr-str state))))
                (restore)
                (done))
              160)))))

;; ============================================================================
;; try-tokens-with-fallback / rate-limit tests
;; ============================================================================

(defn- make-error [msg]
  (ex-info msg {}))

(deftest try-tokens-rate-limit-on-token-1-does-not-try-token-2-test
  (testing "a rate-limit error on token 1 stops the fallback chain"
    (async done
           (let [calls (atom [])
                 tokens [{:access-token "t1"} {:access-token "t2"}]
                 action (fn [token-record]
                          (swap! calls conj (:access-token token-record))
                          (js/Promise.reject
                           (make-error "GitHub request failed: GitHub API error: 403 {\"message\":\"API rate limit exceeded\"}")))]
             (-> (ge/try-tokens-with-fallback tokens action)
                 (.then (fn [outcome]
                          (is (false? (:success outcome)))
                          (is (true? (:rate-limited outcome)))
                          (is (= ["t1"] @calls))
                          (done))))))))

(deftest try-tokens-non-rate-limit-error-falls-through-to-token-2-test
  (testing "a non-rate-limit error on token 1 falls through to token 2"
    (async done
           (let [calls (atom [])
                 tokens [{:access-token "t1"} {:access-token "t2"}]
                 action (fn [token-record]
                          (swap! calls conj (:access-token token-record))
                          (if (= "t1" (:access-token token-record))
                            (js/Promise.reject (make-error "network unreachable"))
                            (js/Promise.resolve {:prs [] :truncated false})))]
             (-> (ge/try-tokens-with-fallback tokens action)
                 (.then (fn [outcome]
                          (is (true? (:success outcome)))
                          (is (= ["t1" "t2"] @calls))
                          (done))))))))

(deftest try-tokens-aggregated-error-labels-each-token-test
  (testing "aggregated error string labels each token distinctly"
    (async done
           (let [tokens [{:access-token "t1"} {:access-token "t2"}]
                 action (fn [token-record]
                          (js/Promise.reject
                           (make-error (str "boom-" (:access-token token-record)))))]
             (-> (ge/try-tokens-with-fallback tokens action)
                 (.then (fn [outcome]
                          (is (false? (:success outcome)))
                          (is (clojure.string/includes? (:error outcome) "token-1"))
                          (is (clojure.string/includes? (:error outcome) "token-2"))
                          (is (clojure.string/includes? (:error outcome) "boom-t1"))
                          (is (clojure.string/includes? (:error outcome) "boom-t2"))
                          (done))))))))

(deftest try-tokens-permission-403-falls-through-to-next-token-test
  (testing "permission-restricted 403 (non-rate-limit body) falls through"
    (async done
           (let [calls (atom [])
                 tokens [{:access-token "t1"} {:access-token "t2"}]
                 action (fn [token-record]
                          (swap! calls conj (:access-token token-record))
                          (if (= "t1" (:access-token token-record))
                            (js/Promise.reject
                             (make-error "GitHub request failed: GitHub API error: 403 {\"message\":\"Resource not accessible by integration\"}"))
                            (js/Promise.resolve {:prs [] :truncated false})))]
             (-> (ge/try-tokens-with-fallback tokens action)
                 (.then (fn [outcome]
                          (is (true? (:success outcome)))
                          (is (= ["t1" "t2"] @calls))
                          (done))))))))

(deftest rate-limit-error-matches-true-positives-and-rejects-bare-403-test
  (testing "rate-limit-error? recognizes GitHub's rate-limit strings and rejects bare 403"
    (is (ge/rate-limit-error? "GitHub API error: 403 {\"message\":\"API rate limit exceeded for installation ID xyz\"}"))
    (is (ge/rate-limit-error? "GitHub API error: 403 {\"message\":\"You have exceeded a secondary rate limit\"}"))
    (is (ge/rate-limit-error? "GitHub API error: 429"))
    (is (not (ge/rate-limit-error? "GitHub API error: 403 {\"message\":\"Resource not accessible by integration\"}")))
    (is (not (ge/rate-limit-error? "GitHub API error: 403 {\"message\":\"Must have push access to repository\"}")))
    (is (not (ge/rate-limit-error? "GitHub API error: 404")))
    (is (not (ge/rate-limit-error? nil)))))
