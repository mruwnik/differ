(ns differ.watcher
  "File system watcher for live diff updates.

   State structure:
   {session-id {:watcher FSWatcher
                :repo-path string
                :debounce-timeout timeout-id}}"
  (:require ["fs" :as fs]
            [clojure.string :as str]))

;; Single atom for all watcher state
(defonce ^:private state (atom {}))

;; Callback set by SSE module (avoids circular dependency)
(defonce ^:private on-change-callback (atom nil))

(defn set-on-change-callback!
  "Set the callback to invoke when files change. Called with session-id."
  [f]
  (reset! on-change-callback f))

;; Patterns to ignore - data, not code
(def ^:private ignore-patterns
  {:prefixes ["."]
   :contains ["node_modules" ".git" "target"]
   :suffixes [".db" ".db-wal" ".db-shm"]})

(defn- should-ignore?
  "Return true if this file change should be ignored."
  [filename]
  (or (nil? filename)
      (some #(str/starts-with? filename %) (:prefixes ignore-patterns))
      (some #(str/includes? filename %) (:contains ignore-patterns))
      (some #(str/ends-with? filename %) (:suffixes ignore-patterns))))

(defn- clear-debounce! [session-id]
  (when-let [timeout (get-in @state [session-id :debounce-timeout])]
    (js/clearTimeout timeout)
    (swap! state update session-id dissoc :debounce-timeout)))

(defn- emit-debounced!
  "Emit a change event after debounce period."
  [session-id]
  (clear-debounce! session-id)
  (let [timeout (js/setTimeout
                 (fn []
                   (swap! state update session-id dissoc :debounce-timeout)
                   (when-let [callback @on-change-callback]
                     (callback session-id)))
                 300)]
    (swap! state assoc-in [session-id :debounce-timeout] timeout)))

(defn- on-file-change [session-id _event filename]
  (when-not (should-ignore? filename)
    (emit-debounced! session-id)))

(defn watch-session!
  "Start watching a repo directory for changes."
  [session-id repo-path]
  (when-not (get @state session-id)
    (try
      (let [watcher (fs/watch repo-path
                              #js {:recursive true}
                              #(on-file-change session-id %1 %2))]
        (swap! state assoc session-id {:watcher watcher :repo-path repo-path})
        (js/console.log (str "[watcher] Started: " repo-path)))
      (catch :default e
        (js/console.error (str "[watcher] Failed: " repo-path " - " (.-message e)))))))

(defn unwatch-session!
  "Stop watching a session's repo directory."
  [session-id]
  (when-let [{:keys [watcher repo-path]} (get @state session-id)]
    (.close watcher)
    (clear-debounce! session-id)
    (swap! state dissoc session-id)
    (js/console.log (str "[watcher] Stopped: " repo-path))))

(defn watching?
  "Check if a session is being watched."
  [session-id]
  (contains? @state session-id))
