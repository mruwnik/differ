(ns differ.sse
  "Server-Sent Events for live updates."
  (:require [clojure.string :as str]))

;; Connection registry - maps client-id to response object
(defonce ^:private connections (atom {}))

(defn- generate-client-id []
  (str (random-uuid)))

(defn add-connection!
  "Register a new SSE connection. Returns client-id."
  [res]
  (let [client-id (generate-client-id)]
    (swap! connections assoc client-id {:res res :subscriptions #{}})
    client-id))

(defn remove-connection!
  "Remove a connection from registry."
  [client-id]
  (swap! connections dissoc client-id))

(defn subscribe!
  "Subscribe a client to events for a session."
  [client-id session-id]
  (swap! connections update-in [client-id :subscriptions] conj session-id))

(defn unsubscribe!
  "Unsubscribe a client from a session."
  [client-id session-id]
  (swap! connections update-in [client-id :subscriptions] disj session-id))

(defn- send-event!
  "Send an SSE event to a specific client."
  [client-id event-type data]
  (when-let [conn (get @connections client-id)]
    (let [^js res (:res conn)
          event-str (str "event: " (name event-type) "\n"
                         "data: " (js/JSON.stringify (clj->js data)) "\n\n")]
      (try
        (.write res event-str)
        true
        (catch :default _
          ;; Connection closed
          (remove-connection! client-id)
          false)))))

(defn broadcast!
  "Broadcast an event to all clients subscribed to a session."
  [session-id event-type data]
  (doseq [[client-id conn] @connections]
    (when (contains? (:subscriptions conn) session-id)
      (send-event! client-id event-type data))))

(defn broadcast-all!
  "Broadcast an event to all connected clients."
  [event-type data]
  (doseq [client-id (keys @connections)]
    (send-event! client-id event-type data)))

;; Event types
(defn emit-comment-added! [session-id comment]
  (broadcast! session-id :comment-added {:session-id session-id :comment comment}))

(defn emit-comment-resolved! [session-id comment-id]
  (broadcast! session-id :comment-resolved {:session-id session-id :comment-id comment-id}))

(defn emit-session-updated! [session-id]
  (broadcast! session-id :session-updated {:session-id session-id}))

(defn emit-files-changed! [session-id files]
  (broadcast! session-id :files-changed {:session-id session-id :files files}))

;; SSE endpoint handler

(defn sse-handler
  "GET /events - SSE endpoint"
  [^js req ^js res]
  ;; Set SSE headers
  (.setHeader res "Content-Type" "text/event-stream")
  (.setHeader res "Cache-Control" "no-cache")
  (.setHeader res "Connection" "keep-alive")
  (.setHeader res "Access-Control-Allow-Origin" "*")

  ;; Disable buffering
  (.flushHeaders res)

  ;; Register connection
  (let [client-id (add-connection! res)
        session-id (.. req -query -session)]

    ;; Subscribe to session if specified
    (when session-id
      (subscribe! client-id session-id))

    ;; Send initial connection event
    (send-event! client-id :connected {:client-id client-id})

    ;; Handle client disconnect
    (.on req "close"
         (fn []
           (remove-connection! client-id)))))

(defn subscribe-handler
  "POST /events/subscribe - Subscribe to session events"
  [^js req ^js res]
  (let [body (js->clj (.-body req) :keywordize-keys true)
        client-id (:client-id body)
        session-id (:session-id body)]
    (when (and client-id session-id)
      (subscribe! client-id session-id))
    (-> res
        (.status 200)
        (.json #js {:success true}))))

(defn setup-routes [^js app]
  (.get app "/events" sse-handler)
  (.post app "/events/subscribe" subscribe-handler))
