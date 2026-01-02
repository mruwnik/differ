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

(defn- sessions-needing-unwatch
  "Returns session-ids that now have zero subscribers."
  [old-state new-state]
  (->> (get-in old-state [:clients])
       keys
       (mapcat #(get-in old-state [:clients % :subscriptions]))
       (filter #(and (pos? (subscriber-count old-state %))
                     (zero? (subscriber-count new-state %))))
       set))

;; Stateful operations

(defn add-connection!
  "Register a new SSE connection. Returns client-id."
  [res]
  (let [client-id (generate-client-id)]
    (swap! state add-client client-id res)
    client-id))

(defn remove-connection!
  "Remove a connection. Stops watchers for sessions with no remaining subscribers."
  [client-id]
  (let [old-state @state
        new-state (swap! state remove-client client-id)]
    ;; Stop watchers for sessions that lost their last subscriber
    (doseq [session-id (sessions-needing-unwatch old-state new-state)]
      (watcher/unwatch-session! session-id))))

(defn subscribe!
  "Subscribe a client to events for a session."
  ([client-id session-id]
   (subscribe! client-id session-id nil))
  ([client-id session-id repo-path]
   (let [old-state @state
         new-state (swap! state subscribe-client client-id session-id repo-path)]
     ;; Start watcher if this is the first subscriber
     (when (and (zero? (subscriber-count old-state session-id))
                (pos? (subscriber-count new-state session-id)))
       (when-let [path (session-repo-path new-state session-id)]
         (watcher/watch-session! session-id path))))))

(defn unsubscribe!
  "Unsubscribe a client from a session."
  [client-id session-id]
  (let [old-state @state
        new-state (swap! state unsubscribe-client client-id session-id)]
    (when (and (pos? (subscriber-count old-state session-id))
               (zero? (subscriber-count new-state session-id)))
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
