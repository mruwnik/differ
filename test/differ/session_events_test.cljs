(ns differ.session-events-test
  "Tests for differ.session-events — thin emitters that push session events
   into the central event stream."
  (:require [clojure.test :refer [deftest testing is use-fixtures async]]
            [differ.event-stream :as es]
            [differ.mcp :as mcp]
            [differ.session-events :as se]))

(use-fixtures :each
  {:before (fn [] (es/reset-for-tests!))
   :after  (fn [] (es/reset-for-tests!))})

(def ^:private sid "local:abc123")

(defn- events-for [session-id]
  (:events (es/get-scope-state (se/session-scope session-id))))

;; ============================================================================
;; session-scope
;; ============================================================================

(deftest session-scope-format-test
  (testing "session-scope produces 'session:<session-id>'"
    (is (= "session:local:abc123" (se/session-scope "local:abc123")))
    (is (= "session:github:owner/repo:42" (se/session-scope "github:owner/repo:42")))))

;; ============================================================================
;; emit-comment-added!
;; ============================================================================

(deftest emit-comment-added-appends-event-test
  (testing "emit-comment-added! publishes to the session scope"
    (se/emit-comment-added! sid
                            {:id "c1" :author "alice" :file "x.clj"
                             :line 10 :parent-id nil
                             :created-at "2026-04-08T10:00:00Z"
                             :text "hey — these should not leak into the ring buffer"})
    (let [evs (events-for sid)]
      (is (= 1 (count evs)))
      (let [ev (first evs)]
        (is (= :comment-added (:event-type ev)))
        (is (= sid (:session-id ev)))
        (is (= "c1" (:comment-id ev)))
        (is (= "alice" (:author ev)))
        (is (= "x.clj" (:file ev)))
        (is (= 10 (:line ev)))
        (is (nil? (:parent-id ev)))
        (is (= "2026-04-08T10:00:00Z" (:created-at ev)))
        (is (not (contains? ev :text))
            "text intentionally omitted so ring buffer doesn't balloon")))))

(deftest emit-comment-added-missing-session-id-noop-test
  (testing "nil / blank session ids must not write to the event stream"
    (doseq [bad [nil "" "   "]]
      (se/emit-comment-added! bad {:id "c1"})
      (is (nil? (es/get-scope-state (se/session-scope bad)))
          (str "scope leak for session-id=" (pr-str bad))))))

(deftest emit-comment-resolved-missing-session-id-noop-test
  (testing "emit-comment-resolved! is a no-op for nil / blank session ids"
    (doseq [bad [nil "" "   "]]
      (se/emit-comment-resolved! bad "c1" "alice")
      (is (nil? (es/get-scope-state (se/session-scope bad)))))))

(deftest emit-comment-unresolved-missing-session-id-noop-test
  (testing "emit-comment-unresolved! is a no-op for nil / blank session ids"
    (doseq [bad [nil "" "   "]]
      (se/emit-comment-unresolved! bad "c1" "alice")
      (is (nil? (es/get-scope-state (se/session-scope bad)))))))

(deftest emit-files-registered-missing-session-id-noop-test
  (testing "emit-files-registered! is a no-op for nil / blank session ids"
    (doseq [bad [nil "" "   "]]
      (se/emit-files-registered! bad ["a.clj"] "agent-1")
      (is (nil? (es/get-scope-state (se/session-scope bad)))))))

(deftest emit-files-unregistered-missing-session-id-noop-test
  (testing "emit-files-unregistered! is a no-op for nil / blank session ids"
    (doseq [bad [nil "" "   "]]
      (se/emit-files-unregistered! bad ["a.clj"] "agent-1")
      (is (nil? (es/get-scope-state (se/session-scope bad)))))))

(deftest emit-review-submitted-missing-session-id-noop-test
  (testing "emit-review-submitted! is a no-op for nil / blank session ids"
    (doseq [bad [nil "" "   "]]
      (se/emit-review-submitted! bad "alice")
      (is (nil? (es/get-scope-state (se/session-scope bad)))))))

;; ============================================================================
;; emit-comment-resolved! / emit-comment-unresolved!
;; ============================================================================

(deftest emit-comment-resolved-appends-event-test
  (testing "emit-comment-resolved! publishes a :comment-resolved event"
    (se/emit-comment-resolved! sid "c1" "alice")
    (let [ev (first (events-for sid))]
      (is (= :comment-resolved (:event-type ev)))
      (is (= "c1" (:comment-id ev)))
      (is (= "alice" (:author ev))))))

(deftest emit-comment-unresolved-appends-event-test
  (testing "emit-comment-unresolved! publishes a :comment-unresolved event"
    (se/emit-comment-unresolved! sid "c1" "alice")
    (let [ev (first (events-for sid))]
      (is (= :comment-unresolved (:event-type ev)))
      (is (= "c1" (:comment-id ev))))))

;; ============================================================================
;; emit-files-registered! / emit-files-unregistered!
;; ============================================================================

(deftest emit-files-registered-appends-event-test
  (testing "emit-files-registered! publishes a :files-registered event"
    (se/emit-files-registered! sid ["src/a.clj" "src/b.clj"] "agent-1")
    (let [ev (first (events-for sid))]
      (is (= :files-registered (:event-type ev)))
      (is (= ["src/a.clj" "src/b.clj"] (:paths ev)))
      (is (= "agent-1" (:agent-id ev))))))

(deftest emit-files-registered-empty-paths-noop-test
  (testing "empty paths collection is a no-op (no noise in the stream)"
    (se/emit-files-registered! sid [] "agent-1")
    (is (nil? (es/get-scope-state (se/session-scope sid))))))

(deftest emit-files-unregistered-appends-event-test
  (testing "emit-files-unregistered! publishes a :files-unregistered event"
    (se/emit-files-unregistered! sid ["src/a.clj"] "agent-1")
    (let [ev (first (events-for sid))]
      (is (= :files-unregistered (:event-type ev)))
      (is (= ["src/a.clj"] (:paths ev))))))

;; ============================================================================
;; emit-review-submitted!
;; ============================================================================

(deftest emit-review-submitted-appends-event-test
  (testing "emit-review-submitted! publishes a :review-submitted event"
    (se/emit-review-submitted! sid "alice")
    (let [ev (first (events-for sid))]
      (is (= :review-submitted (:event-type ev)))
      (is (= sid (:session-id ev)))
      (is (= "alice" (:author ev))))))

;; ============================================================================
;; Integration with wait-for-event
;; ============================================================================

(deftest session-scope-events-flow-through-wait-for-event-test
  (testing "an agent that waits on session:<id> sees events emitted by the session emitters"
    (let [scope (se/session-scope sid)]
      ;; Peek-style: emit first, then wait with timeout=0.
      (se/emit-comment-added! sid
                              {:id "c1" :author "alice" :file "x.clj" :line 1
                               :parent-id nil :created-at "2026-04-08T10:00:00Z"})
      (se/emit-files-registered! sid ["src/a.clj"] "agent-1")
      (let [events (es/events-since scope 0 50)]
        (is (= 2 (count events)))
        (is (= [:comment-added :files-registered] (mapv :event-type events)))
        (is (every? #(= scope (:scope %)) events))))))

;; ============================================================================
;; End-to-end: MCP wait_for_event dispatcher → session-events emit → resolve
;; ============================================================================

(deftest wait-for-event-end-to-end-session-comment-flow-test
  (testing "calling mcp/handle-tool 'wait_for_event' on a session scope blocks
            until session-events publishes a :comment-added event on the
            same scope. This pins the dispatcher → event-stream → emitter
            wiring that the session-events refactor enables.

            We publish via `session-events/emit-comment-added!` directly
            instead of going through `handle-tool 'add_comment'` because
            the latter would require mocking the entire `with-backend` /
            DB stack — which would in turn add test-only seams to
            production code. The mutating handlers are wired to call
            this exact emitter (see `defmethod handle-tool 'add_comment'`
            in mcp.cljs), so the path under test is the same one a real
            add_comment call would take from the point the backend
            returns its comment."
    (async done
           (let [session-id "local:integration-test"
                 wait-promise (mcp/handle-tool
                               "wait_for_event"
                               {:scope (str "session:" session-id)
                                :since-seq 0
                                :timeout-ms 5000
                                :max-events 50})]
             (js/setTimeout
              (fn []
                (se/emit-comment-added!
                 session-id
                 {:id "c-1" :author "alice" :file "src/foo.clj" :line 10
                  :parent-id nil :created-at "2026-04-08T00:00:00Z"}))
              20)
             (.then wait-promise
                    (fn [result]
                      (is (false? (:timed-out result))
                          "wait should resolve via the comment-added event")
                      (let [evs (:events result)
                            ev (first evs)]
                        (is (= 1 (count evs)))
                        (is (= :comment-added (:event-type ev)))
                        (is (= session-id (:session-id ev)))
                        (is (= "c-1" (:comment-id ev)))
                        (is (= (str "session:" session-id) (:scope ev))))
                      (done)))))))
