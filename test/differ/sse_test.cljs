(ns differ.sse-test
  "Tests for Server-Sent Events functionality.
   Note: These tests primarily verify the state management and event emission
   logic through the public API."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [differ.sse :as sse]
            [differ.test-helpers :as helpers]))

;; ============================================================================
;; Mock Response Helper
;; ============================================================================

(defn make-mock-response
  "Create a mock Express response object for SSE testing.
   Returns [res-obj written-chunks-atom] where written-chunks-atom
   accumulates data written via .write()."
  []
  (let [written (atom [])
        closed? (atom false)]
    [#js {:write (fn [data]
                   (when-not @closed?
                     (swap! written conj data)
                     true))
          :setHeader (fn [_k _v] nil)
          :flushHeaders (fn [] nil)
          :on (fn [_event _callback] nil)
          :end (fn []
                 (reset! closed? true))}
     written
     closed?]))

;; ============================================================================
;; Test Setup
;; ============================================================================

(def ^:dynamic *test-db* nil)

(defn with-test-db [f]
  (binding [*test-db* (helpers/init-test-db!)]
    (try
      (f)
      (finally
        (helpers/cleanup-test-db!)))))

(use-fixtures :each with-test-db)

;; ============================================================================
;; Connection Management Tests
;; ============================================================================

(deftest add-connection-test
  (testing "returns a client-id string"
    (let [[res _ _] (make-mock-response)
          client-id (sse/add-connection! res)]
      (is (string? client-id))
      (is (seq client-id))
      ;; Clean up
      (sse/remove-connection! client-id))))

(deftest remove-connection-test
  (testing "removes connection without error"
    (let [[res _ _] (make-mock-response)
          client-id (sse/add-connection! res)]
      ;; Should not throw
      (sse/remove-connection! client-id)))

  (testing "handles removing non-existent connection"
    ;; Should not throw
    (sse/remove-connection! "non-existent-client-id")))

;; ============================================================================
;; Subscription Tests
;; ============================================================================

(deftest subscribe-unsubscribe-test
  (testing "subscribe and unsubscribe without error"
    (let [[res _ _] (make-mock-response)
          client-id (sse/add-connection! res)]
      ;; Subscribe
      (sse/subscribe! client-id "test-session-1")
      (sse/subscribe! client-id "test-session-2")
      ;; Unsubscribe
      (sse/unsubscribe! client-id "test-session-1")
      ;; Clean up
      (sse/remove-connection! client-id)))

  (testing "subscribe with repo-path"
    (let [[res _ _] (make-mock-response)
          client-id (sse/add-connection! res)]
      (sse/subscribe! client-id "test-session" "/path/to/repo")
      (sse/remove-connection! client-id))))

;; ============================================================================
;; Event Emission Tests
;; ============================================================================

(deftest emit-test
  (testing "emit sends event to subscribed clients"
    (let [[res written _] (make-mock-response)
          client-id (sse/add-connection! res)]
      (sse/subscribe! client-id "test-session")
      (sse/emit! "test-session" :test-event {:key "value"})
      ;; Check that something was written
      (is (pos? (count @written)))
      ;; Written data should contain event type
      (is (some #(re-find #"event: test-event" %) @written))
      ;; Clean up
      (sse/remove-connection! client-id)))

  (testing "emit does not send to unsubscribed clients"
    (let [[res1 written1 _] (make-mock-response)
          [res2 written2 _] (make-mock-response)
          client1 (sse/add-connection! res1)
          client2 (sse/add-connection! res2)]
      (sse/subscribe! client1 "session-a")
      (sse/subscribe! client2 "session-b")
      ;; Reset written to clear any connection messages
      (reset! written1 [])
      (reset! written2 [])
      ;; Emit to session-a only
      (sse/emit! "session-a" :test-event {:data "test"})
      ;; Client 1 should receive, client 2 should not
      (is (pos? (count @written1)))
      (is (zero? (count @written2)))
      ;; Clean up
      (sse/remove-connection! client1)
      (sse/remove-connection! client2))))

;; ============================================================================
;; Broadcast Tests
;; ============================================================================

(deftest broadcast-test
  (testing "broadcast sends to all clients subscribed to session"
    (let [[res1 written1 _] (make-mock-response)
          [res2 written2 _] (make-mock-response)
          client1 (sse/add-connection! res1)
          client2 (sse/add-connection! res2)]
      (sse/subscribe! client1 "shared-session")
      (sse/subscribe! client2 "shared-session")
      (reset! written1 [])
      (reset! written2 [])
      (sse/broadcast! "shared-session" :broadcast-event {:msg "hello"})
      ;; Both should receive
      (is (pos? (count @written1)))
      (is (pos? (count @written2)))
      ;; Clean up
      (sse/remove-connection! client1)
      (sse/remove-connection! client2))))

(deftest broadcast-all-test
  (testing "broadcast-all sends to all connected clients"
    (let [[res1 written1 _] (make-mock-response)
          [res2 written2 _] (make-mock-response)
          client1 (sse/add-connection! res1)
          client2 (sse/add-connection! res2)]
      ;; Subscribe to different sessions
      (sse/subscribe! client1 "session-x")
      (sse/subscribe! client2 "session-y")
      (reset! written1 [])
      (reset! written2 [])
      (sse/broadcast-all! :global-event {:announcement "hello all"})
      ;; Both should receive even though subscribed to different sessions
      (is (pos? (count @written1)))
      (is (pos? (count @written2)))
      ;; Clean up
      (sse/remove-connection! client1)
      (sse/remove-connection! client2))))

;; ============================================================================
;; Convenience Emit Functions Tests
;; ============================================================================

(deftest emit-convenience-functions-test
  (testing "emit-comment-added!"
    (let [[res written _] (make-mock-response)
          client-id (sse/add-connection! res)]
      (sse/subscribe! client-id "test-session")
      (reset! written [])
      (sse/emit-comment-added! "test-session" {:id "c1" :text "test"})
      (is (some #(re-find #"comment-added" %) @written))
      (sse/remove-connection! client-id)))

  (testing "emit-comment-resolved!"
    (let [[res written _] (make-mock-response)
          client-id (sse/add-connection! res)]
      (sse/subscribe! client-id "test-session")
      (reset! written [])
      (sse/emit-comment-resolved! "test-session" "comment-id-1")
      (is (some #(re-find #"comment-resolved" %) @written))
      (sse/remove-connection! client-id)))

  (testing "emit-comment-unresolved!"
    (let [[res written _] (make-mock-response)
          client-id (sse/add-connection! res)]
      (sse/subscribe! client-id "test-session")
      (reset! written [])
      (sse/emit-comment-unresolved! "test-session" "comment-id-1")
      (is (some #(re-find #"comment-unresolved" %) @written))
      (sse/remove-connection! client-id)))

  (testing "emit-comment-deleted!"
    (let [[res written _] (make-mock-response)
          client-id (sse/add-connection! res)]
      (sse/subscribe! client-id "test-session")
      (reset! written [])
      (sse/emit-comment-deleted! "test-session" "comment-id-1")
      (is (some #(re-find #"comment-deleted" %) @written))
      (sse/remove-connection! client-id)))

  (testing "emit-session-updated!"
    (let [[res written _] (make-mock-response)
          client-id (sse/add-connection! res)]
      (sse/subscribe! client-id "test-session")
      (reset! written [])
      (sse/emit-session-updated! "test-session")
      (is (some #(re-find #"session-updated" %) @written))
      (sse/remove-connection! client-id)))

  (testing "emit-files-changed!"
    (let [[res written _] (make-mock-response)
          client-id (sse/add-connection! res)]
      (sse/subscribe! client-id "test-session")
      (reset! written [])
      (sse/emit-files-changed! "test-session" ["file1.txt" "file2.txt"])
      (is (some #(re-find #"files-changed" %) @written))
      (sse/remove-connection! client-id)))

  (testing "emit-diff-changed!"
    (let [[res written _] (make-mock-response)
          client-id (sse/add-connection! res)]
      (sse/subscribe! client-id "test-session")
      (reset! written [])
      (sse/emit-diff-changed! "test-session")
      (is (some #(re-find #"diff-changed" %) @written))
      (sse/remove-connection! client-id))))

;; ============================================================================
;; Event Data Format Tests
;; ============================================================================

(deftest event-data-format-test
  (testing "events include session-id in data"
    (let [[res written _] (make-mock-response)
          client-id (sse/add-connection! res)]
      (sse/subscribe! client-id "my-session-123")
      (reset! written [])
      (sse/emit! "my-session-123" :test-event {:extra "data"})
      ;; All written data combined should contain the session-id
      (let [all-written (apply str @written)]
        (is (seq all-written))
        (is (re-find #"my-session-123" all-written)))
      (sse/remove-connection! client-id))))

;; ============================================================================
;; Error Handling Tests
;; ============================================================================

(deftest send-to-disconnected-client-test
  (testing "handles write errors gracefully"
    (let [;; Mock response that throws on write
          error-res #js {:write (fn [_] (throw (js/Error. "Connection closed")))
                         :setHeader (fn [_k _v] nil)
                         :flushHeaders (fn [] nil)
                         :on (fn [_event _callback] nil)}
          client-id (sse/add-connection! error-res)]
      (sse/subscribe! client-id "test-session")
      ;; Should not throw, just remove the client
      (sse/emit! "test-session" :test {:data "test"}))))
