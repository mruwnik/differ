(ns differ.watcher
  "File system watcher for live diff updates.

   State structure:
   {session-id {:watcher FSWatcher
                :git-watcher FSWatcher  ; watches .git/refs/heads for commits
                :repo-path string
                :debounce-timeout timeout-id}}"
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [differ.config :as config]))

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
  (let [debounce-ms (config/get-value :watcher-debounce-ms)
        timeout (js/setTimeout
                 (fn []
                   (swap! state update session-id dissoc :debounce-timeout)
                   (when-let [callback @on-change-callback]
                     (callback session-id)))
                 debounce-ms)]
    (swap! state assoc-in [session-id :debounce-timeout] timeout)))

(defn- on-file-change [session-id _event filename]
  (when-not (should-ignore? filename)
    (emit-debounced! session-id)))

(defn- start-git-watcher!
  "Watch .git/refs/heads for commit changes."
  [session-id repo-path]
  (let [git-refs-path (path/join repo-path ".git" "refs" "heads")]
    (when (fs/existsSync git-refs-path)
      (try
        (let [watcher (fs/watch git-refs-path
                                #js {:recursive true}
                                (fn [_event _filename]
                                  ;; Any change in refs/heads means a commit or branch change
                                  (emit-debounced! session-id)))]
          (js/console.log (str "[watcher] Git refs: " git-refs-path))
          watcher)
        (catch :default e
          (js/console.warn (str "[watcher] Git refs failed: " (.-message e)))
          nil)))))

(defn watch-session!
  "Start watching a repo directory for changes.
   Uses atomic check-and-set to prevent duplicate watchers."
  [session-id repo-path]
  ;; Atomically check and claim the slot with a placeholder
  (let [claimed? (atom false)
        _ (swap! state
                 (fn [s]
                   (if (get s session-id)
                     s  ; Already exists, no change
                     (do (reset! claimed? true)
                         (assoc s session-id {:pending true})))))]
    ;; Only proceed if we claimed the slot
    (when @claimed?
      (try
        (let [watcher (fs/watch repo-path
                                #js {:recursive true}
                                #(on-file-change session-id %1 %2))
              git-watcher (start-git-watcher! session-id repo-path)]
          (swap! state assoc session-id {:watcher watcher
                                         :git-watcher git-watcher
                                         :repo-path repo-path})
          (js/console.log (str "[watcher] Started: " repo-path)))
        (catch :default e
          ;; Clean up the placeholder on failure
          (swap! state dissoc session-id)
          (js/console.error (str "[watcher] Failed: " repo-path " - " (.-message e))))))))

(defn unwatch-session!
  "Stop watching a session's repo directory."
  [session-id]
  (when-let [{:keys [watcher git-watcher repo-path]} (get @state session-id)]
    (.close watcher)
    (when git-watcher (.close git-watcher))
    (clear-debounce! session-id)
    (swap! state dissoc session-id)
    (js/console.log (str "[watcher] Stopped: " repo-path))))

(defn watching?
  "Check if a session is being watched."
  [session-id]
  (contains? @state session-id))
