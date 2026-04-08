(ns differ.event-stream-test
  "Tests for differ.event-stream — generic ring buffer, cursor, blocking
   wait, shutdown. Source-agnostic: no GitHub polling or session events."
  (:require [clojure.test :refer [deftest testing is use-fixtures async]]
            [differ.config :as config]
            [differ.event-stream :as es]))

(use-fixtures :each
  {:before (fn [] (es/reset-for-tests!))
   :after  (fn [] (es/reset-for-tests!))})

;; ============================================================================
;; Ring buffer tests
;; ============================================================================

(deftest append-events-assigns-monotonic-seqs-test
  (testing "first append starts seq at 1 and increments per event"
    (es/append-events! "scope/a" [{:event-type :x :n 1}
                                  {:event-type :x :n 2}])
    (let [state (es/get-scope-state "scope/a")]
      (is (= [1 2] (mapv :seq (:events state))))
      (is (= 3 (:next-seq state))))))

(deftest append-events-assigns-received-at-and-scope-test
  (testing "every appended event has an ISO-8601 :received-at and :scope"
    (es/append-events! "scope/b" [{:event-type :x}])
    (let [ev (-> (es/get-scope-state "scope/b") :events first)]
      (is (re-find #"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$"
                   (:received-at ev)))
      (is (= "scope/b" (:scope ev))))))

(deftest append-events-trims-to-buffer-size-test
  (testing "ring buffer drops oldest events once capacity is exceeded"
    (with-redefs [config/event-buffer-size (fn [] 3)]
      (es/append-events! "p" [{:n 1} {:n 2} {:n 3}])
      (es/append-events! "p" [{:n 4} {:n 5}])
      (let [state (es/get-scope-state "p")
            seqs (mapv :seq (:events state))]
        (is (= [3 4 5] seqs))
        (is (= 6 (:next-seq state)))))))

(deftest events-since-filters-by-seq-test
  (testing "events-since returns only events with :seq > since-seq"
    (es/append-events! "p" [{:n 1} {:n 2} {:n 3}])
    (let [result (es/events-since "p" 1 50)]
      (is (= [2 3] (mapv :seq result))))))

(deftest events-since-returns-empty-when-caught-up-test
  (testing "events-since returns [] when since-seq >= last seq"
    (es/append-events! "p" [{:n 1}])
    (is (= [] (es/events-since "p" 1 50)))
    (is (= [] (es/events-since "p" 99 50)))))

(deftest events-since-respects-max-events-test
  (testing "events-since limits results to max-events"
    (es/append-events! "p" (mapv (fn [n] {:n n}) (range 10)))
    (is (= 3 (count (es/events-since "p" 0 3))))))

(deftest events-since-handles-fell-off-tail-test
  (testing "when since-seq < oldest-buffered, returns from oldest available"
    (with-redefs [config/event-buffer-size (fn [] 3)]
      (es/append-events! "p" (mapv (fn [n] {:n n}) (range 10)))
      ;; Only seqs 8,9,10 are in the buffer now; since-seq=1 fell off the tail
      (let [result (es/events-since "p" 1 50)]
        (is (= [8 9 10] (mapv :seq result)))))))

(deftest reset-for-tests-clears-all-state-test
  (testing "reset-for-tests! empties the scopes atom and clears shutdown flag"
    (es/append-events! "p" [{:n 1}])
    (es/reset-for-tests!)
    (is (nil? (es/get-scope-state "p")))
    (is (false? (es/shutdown?-value)))))

(deftest append-events-assigned-seqs-returned-test
  (testing "append-events! returns the assigned seq numbers in order"
    (let [seqs (es/append-events! "p" [{:n 1} {:n 2}])]
      (is (= [1 2] seqs)))))

(deftest append-events-returns-full-range-even-when-trimmed-test
  (testing "return value covers the full assigned range even if the ring
            buffer trimmed the oldest of the new events"
    (with-redefs [config/event-buffer-size (fn [] 3)]
      (let [seqs (es/append-events! "p" [{:n 1} {:n 2} {:n 3} {:n 4} {:n 5}])]
        (is (= [1 2 3 4 5] seqs)
            "seqs 1-5 were assigned even though 1 and 2 were trimmed from the buffer")
        (is (= [3 4 5] (mapv :seq (:events (es/get-scope-state "p")))))))))

;; ============================================================================
;; wait-for-event (blocking wait) tests
;; ============================================================================

(deftest wait-returns-buffered-events-immediately-test
  (testing "wait returns events already in the buffer without blocking"
    (async done
           (es/append-events! "p" [{:event-type :x :n 1}
                                   {:event-type :x :n 2}])
           (-> (es/wait-for-event {:scope "p" :since-seq 0
                                   :timeout-ms 10000 :max-events 50})
               (.then (fn [result]
                        (is (= 2 (count (:events result))))
                        (is (= 2 (:next-seq result)))
                        (is (false? (:timed-out result)))
                        (done)))))))

(deftest wait-blocks-then-wakes-on-new-events-test
  (testing "when no buffered events, wait blocks until append-events! arrives"
    (async done
           (let [p (es/wait-for-event {:scope "p2" :since-seq 0
                                       :timeout-ms 10000 :max-events 50})]
             (js/setTimeout
              (fn []
                (es/append-events! "p2" [{:event-type :x :n 9}]))
              20)
             (-> p
                 (.then (fn [result]
                          (is (= 1 (count (:events result))))
                          (is (= 9 (-> result :events first :n)))
                          (is (false? (:timed-out result)))
                          (done))))))))

(deftest wait-timeout-returns-timed-out-test
  (testing "wait returns timed-out=true with empty events when no events arrive"
    (async done
           (-> (es/wait-for-event {:scope "p3" :since-seq 0
                                   :timeout-ms 30 :max-events 50})
               (.then (fn [result]
                        (is (= [] (:events result)))
                        (is (true? (:timed-out result)))
                        (is (= 0 (:next-seq result)))
                        (done)))))))

(deftest wait-timeout-zero-is-immediate-peek-test
  (testing "timeout-ms=0 returns buffered events immediately"
    (async done
           (es/append-events! "p4" [{:event-type :x :n 1}])
           (-> (es/wait-for-event {:scope "p4" :since-seq 0
                                   :timeout-ms 0 :max-events 50})
               (.then (fn [result]
                        (is (= 1 (count (:events result))))
                        (is (false? (:timed-out result)))
                        (done)))))))

(deftest wait-timeout-zero-no-events-returns-timed-out-test
  (testing "timeout-ms=0 with empty buffer returns timed-out immediately"
    (async done
           (-> (es/wait-for-event {:scope "p5" :since-seq 0
                                   :timeout-ms 0 :max-events 50})
               (.then (fn [result]
                        (is (= [] (:events result)))
                        (is (true? (:timed-out result)))
                        (done)))))))

(deftest wait-since-seq-past-end-blocks-test
  (testing "since-seq beyond last buffered event blocks instead of returning immediately"
    (async done
           (es/append-events! "p6" [{:event-type :x :n 1}])
           (-> (es/wait-for-event {:scope "p6" :since-seq 99
                                   :timeout-ms 30 :max-events 50})
               (.then (fn [result]
                        (is (= [] (:events result)))
                        (is (true? (:timed-out result)))
                        (done)))))))

(deftest wait-max-events-cap-respected-test
  (testing "max-events limits the returned events"
    (async done
           (es/append-events! "p7" (mapv (fn [n] {:n n}) (range 10)))
           (-> (es/wait-for-event {:scope "p7" :since-seq 0
                                   :timeout-ms 0 :max-events 3})
               (.then (fn [result]
                        (is (= 3 (count (:events result))))
                        (is (= 3 (:next-seq result)))
                        (done)))))))

(deftest wait-multiple-concurrent-subscribers-wake-test
  (testing "all pending subscribers wake when events arrive"
    (async done
           (let [p1 (es/wait-for-event {:scope "p8" :since-seq 0
                                        :timeout-ms 5000 :max-events 50})
                 p2 (es/wait-for-event {:scope "p8" :since-seq 0
                                        :timeout-ms 5000 :max-events 50})]
             (js/setTimeout
              (fn [] (es/append-events! "p8" [{:event-type :x :n 1}]))
              20)
             (-> (js/Promise.all #js [p1 p2])
                 (.then (fn [results]
                          (let [arr (js->clj results :keywordize-keys true)]
                            (is (= 2 (count arr)))
                            (doseq [r arr]
                              (is (= 1 (count (:events r))))
                              (is (false? (:timed-out r))))
                            (done)))))))))

(deftest concurrent-waiters-different-since-seq-test
  (testing "two waiters with different since-seq see different slices of the same drain"
    (async done
           (es/append-events! "p-multi"
                              [{:event-type :x :n 1}
                               {:event-type :x :n 2}])
           (let [p1 (es/wait-for-event {:scope "p-multi" :since-seq 1
                                        :timeout-ms 5000 :max-events 50})
                 p2 (es/wait-for-event {:scope "p-multi" :since-seq 2
                                        :timeout-ms 5000 :max-events 50})]
             (js/setTimeout
              (fn []
                (es/append-events! "p-multi" [{:event-type :x :n 3}]))
              20)
             (-> (js/Promise.all #js [p1 p2])
                 (.then (fn [results]
                          (let [[r1 r2] (js->clj results :keywordize-keys true)]
                       ;; p1 returns immediately because since-seq=1 has seq 2 ready
                            (is (= [2] (mapv :seq (:events r1))))
                       ;; p2 starts at 2, nothing buffered yet → blocks for seq 3
                            (is (= [3] (mapv :seq (:events r2))))
                            (is (false? (:timed-out r1)))
                            (is (false? (:timed-out r2)))
                            (done)))))))))

;; ============================================================================
;; Scope validation tests
;; ============================================================================

(deftest append-events-rejects-nil-scope-test
  (testing "append-events! throws on nil scope"
    (is (thrown? js/Error (es/append-events! nil [{:n 1}])))))

(deftest append-events-rejects-empty-scope-test
  (testing "append-events! throws on empty-string scope"
    (is (thrown? js/Error (es/append-events! "" [{:n 1}])))))

(deftest append-events-rejects-blank-scope-test
  (testing "append-events! throws on whitespace-only scope"
    (is (thrown? js/Error (es/append-events! "   " [{:n 1}])))))

(deftest append-events-rejects-non-string-scope-test
  (testing "append-events! throws on non-string scope"
    (is (thrown? js/Error (es/append-events! 42 [{:n 1}])))
    (is (thrown? js/Error (es/append-events! :kw [{:n 1}])))))

(deftest wait-for-event-rejects-nil-scope-test
  (testing "wait-for-event resolves to a rejected Promise on nil scope"
    (async done
           (-> (es/wait-for-event {:scope nil :timeout-ms 0})
               (.then (fn [_]
                        (is false "wait-for-event should have rejected")
                        (done))
                      (fn [err]
                        (is (some? err))
                        (done)))))))

(deftest wait-for-event-rejects-empty-scope-test
  (testing "wait-for-event resolves to a rejected Promise on empty scope"
    (async done
           (-> (es/wait-for-event {:scope "" :timeout-ms 0})
               (.then (fn [_]
                        (is false "wait-for-event should have rejected")
                        (done))
                      (fn [err]
                        (is (some? err))
                        (done)))))))

;; ============================================================================
;; Reclamation tests
;; ============================================================================

(deftest drain-reclaims-empty-scope-test
  (testing "a scope with no events and no subscribers is reclaimed after drain"
    (async done
           ;; Register a wait that times out quickly, then drain. The
           ;; timeout-side resolver removes the subscriber, then after
           ;; drain the empty scope should be reclaimed.
           (-> (es/wait-for-event {:scope "reclaim/test"
                                   :since-seq 0 :timeout-ms 10 :max-events 50})
               (.then (fn [_]
                        ;; Wait one tick so drain-all can run reclamation.
                        (js/setTimeout
                         (fn []
                           (is (nil? (es/get-scope-state "reclaim/test"))
                               "empty scope should be reclaimed")
                           (done))
                         5)))))))

(deftest release-scope-drops-empty-entry-test
  (testing "release-scope! removes an empty scope entry"
    ;; Touch the scope with a wait-with-timeout-0 (peek) to create the
    ;; entry, then manually drop any residual state and call release.
    (es/append-events! "release/test" [])
    ;; Entry should not exist because no events were appended.
    (is (nil? (es/get-scope-state "release/test")))
    ;; Even if we release a nonexistent scope, it's a safe no-op.
    (es/release-scope! "release/test")
    (is (nil? (es/get-scope-state "release/test")))))

(deftest release-scope-preserves-populated-entry-test
  (testing "release-scope! does NOT drop a scope that still has events"
    (es/append-events! "keep/test" [{:event-type :x}])
    (es/release-scope! "keep/test")
    (is (some? (es/get-scope-state "keep/test")))
    (is (= 1 (count (:events (es/get-scope-state "keep/test")))))))

(deftest peek-on-unknown-scope-does-not-leak-entry-test
  (testing "wait-for-event with timeout-ms 0 against a never-populated scope
            must NOT leak a `{scope nil}` entry into scopes-state. Regression
            for the touch-scope! `update`-on-missing-key bug."
    (async done
           (-> (es/wait-for-event {:scope "peek/no-leak"
                                   :since-seq 0
                                   :timeout-ms 0
                                   :max-events 50})
               (.then (fn [result]
                        (is (true? (:timed-out result)))
                        (is (= [] (:events result)))
                        (is (not (contains? @es/scopes-state "peek/no-leak"))
                            "peek must not leave a `{scope nil}` entry behind")
                        (done)))))))

;; ============================================================================
;; Shutdown tests
;; ============================================================================

(deftest shutdown-all-wakes-subscribers-test
  (testing "shutdown-all! drains pending subscribers as timed-out"
    (async done
           (let [p (es/wait-for-event {:scope "shutdown/test" :since-seq 0
                                       :timeout-ms 60000 :max-events 50})]
             (js/setTimeout (fn [] (es/shutdown-all!)) 20)
             (-> p
                 (.then (fn [result]
                          (is (true? (:timed-out result)))
                          (is (true? (es/shutdown?-value)))
                          (done))))))))

(deftest wait-after-shutdown-returns-timed-out-immediately-test
  (testing "new waits after shutdown-all! resolve immediately as timed-out"
    (async done
           (es/shutdown-all!)
           (-> (es/wait-for-event {:scope "post/shutdown"
                                   :since-seq 0
                                   :timeout-ms 60000
                                   :max-events 50})
               (.then (fn [result]
                        (is (true? (:timed-out result)))
                        (done)))))))

(deftest reset-shutdown-clears-flag-and-allows-new-waits-test
  (testing "reset-shutdown! clears the flag so new waits behave normally
            again. Regression: defonce keeps the atom alive across
            shadow-cljs hot-reload, so without an explicit reset on
            `start`, polling is permanently broken after the first
            ^:dev/before-load → ^:dev/after-load cycle."
    (async done
           (es/shutdown-all!)
           (is (true? (es/shutdown?-value)))
           (es/reset-shutdown!)
           (is (false? (es/shutdown?-value)))
           ;; A new wait should now block normally rather than insta-timeout.
           (let [p (es/wait-for-event {:scope "post/reset"
                                       :since-seq 0
                                       :timeout-ms 5000
                                       :max-events 50})]
             (js/setTimeout
              (fn [] (es/append-events! "post/reset" [{:event-type :x :n 1}]))
              20)
             (-> p
                 (.then (fn [result]
                          (is (false? (:timed-out result))
                              "after reset-shutdown!, waits must accept new events")
                          (is (= 1 (count (:events result))))
                          (done))))))))
