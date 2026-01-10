(ns differ.sse
  "Server-Sent Events for live updates.

   State structure:
   {:clients {client-id {:res Response, :subscriptions #{session-id ...}}}
    :sessions {session-id {:repo-path string, :subscriber-count int}}}"
  (:require [differ.db :as db]
            [differ.watcher :as watcher]
            [differ.util :as util]))

;; Single atom for all SSE state
(defonce ^:private state
  (atom {:clients {}
         :sessions {}}))

(defn- generate-client-id []
  (str (random-uuid)))

;; Pure functions for state transitions

(defn- add-client [state client-id res]
  (assoc-in state [:clients client-id] {:res res :subscriptions #{}}))

(defn- remove-client [state client-id]
  (let [subscriptions (get-in state [:clients client-id :subscriptions] #{})]
    (-> state
        (update :clients dissoc client-id)
        ;; Decrement subscriber counts
        (update :sessions
                (fn [sessions]
                  (reduce (fn [s session-id]
                            (update-in s [session-id :subscriber-count] dec))
                          sessions
                          subscriptions))))))

(defn- subscribe-client [state client-id session-id repo-path]
  (-> state
      (update-in [:clients client-id :subscriptions] conj session-id)
      (update-in [:sessions session-id]
                 (fn [session]
                   (-> (or session {:subscriber-count 0})
                       (update :subscriber-count inc)
                       (cond-> repo-path (assoc :repo-path repo-path)))))))

(defn- unsubscribe-client [state client-id session-id]
  (-> state
      (update-in [:clients client-id :subscriptions] disj session-id)
      (update-in [:sessions session-id :subscriber-count] dec)))

(defn- subscriber-count [state session-id]
  (get-in state [:sessions session-id :subscriber-count] 0))

(defn- session-repo-path [state session-id]
  (get-in state [:sessions session-id :repo-path]))

;; Stateful operations

(defn add-connection!
  "Register a new SSE connection. Returns client-id."
  [res]
  (let [client-id (generate-client-id)]
    (swap! state add-client client-id res)
    client-id))

(defn remove-connection!
  "Remove a connection. Stops watchers for sessions with no remaining subscribers.
   Atomically computes which sessions need unwatching."
  [client-id]
  (let [sessions-to-unwatch (atom #{})]
    (swap! state
           (fn [s]
             (let [subscriptions (get-in s [:clients client-id :subscriptions] #{})
                   new-s (remove-client s client-id)]
               ;; Find sessions that went from >0 to 0 subscribers
               (doseq [session-id subscriptions]
                 (when (and (pos? (subscriber-count s session-id))
                            (zero? (subscriber-count new-s session-id)))
                   (swap! sessions-to-unwatch conj session-id)))
               new-s)))
    ;; Stop watchers outside swap
    (doseq [session-id @sessions-to-unwatch]
      (watcher/unwatch-session! session-id))))

(defn subscribe!
  "Subscribe a client to events for a session.
   Atomically updates state and starts watcher if first subscriber."
  ([client-id session-id]
   (subscribe! client-id session-id nil))
  ([client-id session-id repo-path]
   ;; Track whether we need to start watcher (determined atomically during swap)
   (let [start-watcher? (atom false)
         watcher-path (atom nil)]
     (swap! state
            (fn [s]
              (let [old-count (subscriber-count s session-id)
                    new-s (subscribe-client s client-id session-id repo-path)
                    new-count (subscriber-count new-s session-id)]
                ;; Check if we're the first subscriber
                (when (and (zero? old-count) (pos? new-count))
                  (reset! start-watcher? true)
                  (reset! watcher-path (session-repo-path new-s session-id)))
                new-s)))
     ;; Start watcher outside swap (has side effects)
     (when (and @start-watcher? @watcher-path)
       (watcher/watch-session! session-id @watcher-path)))))

(defn unsubscribe!
  "Unsubscribe a client from a session.
   Atomically updates state and stops watcher if last subscriber."
  [client-id session-id]
  (let [stop-watcher? (atom false)]
    (swap! state
           (fn [s]
             (let [old-count (subscriber-count s session-id)
                   new-s (unsubscribe-client s client-id session-id)
                   new-count (subscriber-count new-s session-id)]
               (when (and (pos? old-count) (zero? new-count))
                 (reset! stop-watcher? true))
               new-s)))
    (when @stop-watcher?
      (watcher/unwatch-session! session-id))))

;; Event sending

(defn- send-event!
  "Send an SSE event to a specific client. Returns true on success."
  [client-id event-type data]
  (when-let [conn (get-in @state [:clients client-id])]
    (let [^js res (:res conn)
          event-str (str "event: " (name event-type) "\n"
                         "data: " (js/JSON.stringify (clj->js data)) "\n\n")]
      (try
        (.write res event-str)
        true
        (catch :default _
          (remove-connection! client-id)
          false)))))

(defn broadcast!
  "Broadcast an event to all clients subscribed to a session."
  [session-id event-type data]
  (doseq [[client-id {:keys [subscriptions]}] (:clients @state)]
    (when (contains? subscriptions session-id)
      (send-event! client-id event-type data))))

(defn broadcast-all!
  "Broadcast an event to all connected clients."
  [event-type data]
  (doseq [client-id (keys (:clients @state))]
    (send-event! client-id event-type data)))

;; Single emit function - simpler than 5 separate functions
(defn emit!
  "Emit an event to all subscribers of a session.
   Event types: :comment-added, :comment-resolved, :session-updated,
                :files-changed, :diff-changed"
  [session-id event-type data]
  (broadcast! session-id event-type (assoc data :session-id session-id)))

;; Convenience wrappers (for backwards compatibility)
(defn emit-comment-added! [session-id comment]
  (emit! session-id :comment-added {:comment comment}))

(defn emit-comment-resolved! [session-id comment-id]
  (emit! session-id :comment-resolved {:comment-id comment-id}))

(defn emit-comment-unresolved! [session-id comment-id]
  (emit! session-id :comment-unresolved {:comment-id comment-id}))

(defn emit-comment-deleted! [session-id comment-id]
  (emit! session-id :comment-deleted {:comment-id comment-id}))

(defn emit-session-updated! [session-id]
  (emit! session-id :session-updated {}))

(defn emit-files-changed! [session-id files]
  (emit! session-id :files-changed {:files files}))

(defn emit-diff-changed! [session-id]
  (emit! session-id :diff-changed {}))

;; HTTP handlers

(defn sse-handler
  "GET /events - SSE endpoint"
  [^js req ^js res]
  (.setHeader res "Content-Type" "text/event-stream")
  (.setHeader res "Cache-Control" "no-cache")
  (.setHeader res "Connection" "keep-alive")
  (.setHeader res "Access-Control-Allow-Origin" "*")
  (.flushHeaders res)

  (let [client-id (add-connection! res)
        session-id (.. req -query -session)
        session (when session-id (db/get-session session-id))
        repo-path (:repo-path session)]
    (when session-id
      (subscribe! client-id session-id repo-path))
    (send-event! client-id :connected {:client-id client-id})
    (.on req "close" #(remove-connection! client-id))))

(defn subscribe-handler
  "POST /events/subscribe - Subscribe to additional sessions"
  [^js req ^js res]
  (let [body (-> (.-body req)
                 (js->clj :keywordize-keys true)
                 util/keys->kebab)
        {:keys [client-id session-id]} body]
    (when (and client-id session-id)
      (subscribe! client-id session-id))
    (-> res (.status 200) (.json #js {:success true}))))

(defn setup-routes [^js app]
  (.get app "/events" sse-handler)
  (.post app "/events/subscribe" subscribe-handler))

;; Initialize watcher callback
(watcher/set-on-change-callback! emit-diff-changed!)
