(ns differ.config
  "Configuration management.

   Reads from resources/config.edn with sensible defaults.
   Exposes config values for both server and client use."
  (:require ["path" :as path]
            ["fs" :as fs]
            [clojure.edn :as edn]))

;; Default configuration values
(def defaults
  {:port 8576

   ;; File display thresholds (client)
   :large-file-threshold 50000      ;; Characters - files larger require explicit load
   :line-count-threshold 400        ;; Diff lines - more than this requires explicit expand
   :context-expand-size 15          ;; Lines to expand at a time

   ;; Watcher settings (server)
   :watcher-debounce-ms 300})       ;; Debounce file change events

(defonce ^:private config-cache (atom nil))

(defn- config-path []
  ;; NOTE: Uses js/__dirname which requires Node.js CommonJS mode.
  ;; If switching to ES modules, use import.meta.url instead.
  (let [dir (path/dirname js/__dirname)]
    (path/join dir "resources" "config.edn")))

(defn- read-config-file []
  (try
    (-> (fs/readFileSync (config-path) "utf8")
        edn/read-string)
    (catch :default e
      (js/console.warn "Failed to read config.edn:" (.-message e))
      {})))

(defn get-config
  "Get merged configuration (defaults + config.edn).
   Caches result after first read."
  []
  (if-let [cached @config-cache]
    cached
    (let [merged (merge defaults (read-config-file))]
      (reset! config-cache merged)
      merged)))

(defn get-value
  "Get a specific config value."
  [key]
  (get (get-config) key))

(defn reload!
  "Force reload config from disk."
  []
  (reset! config-cache nil)
  (get-config))

;; Client-safe config (values that can be exposed to the browser)
(def client-config-keys
  [:large-file-threshold
   :line-count-threshold
   :context-expand-size])

(defn client-config
  "Get config values safe to expose to the client."
  []
  (select-keys (get-config) client-config-keys))
