(ns differ.config
  "Configuration management.

   Reads from resources/config.edn with sensible defaults.
   Environment variables override file config for secrets:
   - GITHUB_CLIENT_ID
   - GITHUB_CLIENT_SECRET

   Supports .env file in project root (takes precedence over process.env).

   Exposes config values for both server and client use."
  (:require ["path" :as path]
            ["fs" :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]))

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

(defn- env-var
  "Get environment variable value, returns nil if not set or empty."
  [name]
  (let [value (aget js/process.env name)]
    (when (and value (not= value ""))
      value)))

(defn- parse-env-line
  "Parse a single .env line. Returns [key value] or nil."
  [line]
  (let [trimmed (str/trim line)]
    (when (and (seq trimmed)
               (not (str/starts-with? trimmed "#")))
      (let [eq-idx (str/index-of trimmed "=")]
        (when (and eq-idx (pos? eq-idx))
          (let [key (str/trim (subs trimmed 0 eq-idx))
                raw-value (str/trim (subs trimmed (inc eq-idx)))
                ;; Strip surrounding quotes if present
                value (cond
                        (and (str/starts-with? raw-value "\"")
                             (str/ends-with? raw-value "\""))
                        (subs raw-value 1 (dec (count raw-value)))

                        (and (str/starts-with? raw-value "'")
                             (str/ends-with? raw-value "'"))
                        (subs raw-value 1 (dec (count raw-value)))

                        :else raw-value)]
            [key value]))))))

(defn- load-env-file
  "Load .env file from project root if it exists.
   Returns map of {key value} pairs."
  []
  (let [env-path (path/join (path/dirname js/__dirname) ".env")]
    (try
      (when (fs/existsSync env-path)
        (let [content (fs/readFileSync env-path "utf8")
              lines (str/split-lines content)]
          (->> lines
               (keep parse-env-line)
               (into {}))))
      (catch :default _
        nil))))

(defn- get-env
  "Get environment value, checking .env file first, then process.env."
  [env-file name]
  (or (get env-file name)
      (env-var name)))

(defn- merge-github-from-env
  "Merge GitHub credentials from .env file or environment variables.
   .env file takes precedence over process.env."
  [config]
  (let [env-file (load-env-file)
        client-id (get-env env-file "GITHUB_CLIENT_ID")
        client-secret (get-env env-file "GITHUB_CLIENT_SECRET")]
    (if (or client-id client-secret)
      (update config :github merge
              (cond-> {}
                client-id (assoc :client-id client-id)
                client-secret (assoc :client-secret client-secret)))
      config)))

(defn get-config
  "Get merged configuration (defaults + config.edn + env vars).
   Environment variables override file config for secrets.
   Caches result after first read."
  []
  (if-let [cached @config-cache]
    cached
    (let [merged (-> defaults
                     (merge (read-config-file))
                     merge-github-from-env)]
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
