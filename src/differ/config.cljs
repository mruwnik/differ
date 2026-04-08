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
   :base-url "http://localhost:8576"  ;; Used in MCP responses and OAuth redirects

   ;; File display thresholds (client)
   :large-file-threshold 50000      ;; Characters - files larger require explicit load
   :line-count-threshold 400        ;; Diff lines - more than this requires explicit expand
   :context-expand-size 15          ;; Lines to expand at a time

   ;; Watcher settings (server)
   :watcher-debounce-ms 300         ;; Debounce file change events

   ;; Event stream — central ring buffer used by both github: and
   ;; session: scopes. Poll interval / grace are github-specific; the
   ;; buffer size is shared across all scopes.
   :event-stream
   {:buffer-size 10000}

   ;; GitHub poller (wait_for_event MCP tool, github: scope)
   :github-poller
   {:poll-interval-ms 30000
    :poller-grace-ms 300000}

   ;; Push whitelist - controls which repos/branches can be pushed
   ;; Empty map = all repos/branches allowed
   ;; Example: {"owner/repo" ["feature/*" "fix/*"], "myorg/*" ["*"]}
   :push-whitelist {}})

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
   .env file takes precedence over process.env.
   env-file should be pre-loaded via load-env-file."
  [config env-file]
  (let [client-id (get-env env-file "GITHUB_CLIENT_ID")
        client-secret (get-env env-file "GITHUB_CLIENT_SECRET")]
    (if (or client-id client-secret)
      (update config :github merge
              (cond-> {}
                client-id (assoc :client-id client-id)
                client-secret (assoc :client-secret client-secret)))
      config)))

(defn- valid-whitelist-value?
  "Check if a whitelist value is a valid array of strings."
  [v]
  (and (sequential? v)
       (every? string? v)))

(defn- parse-push-whitelist-env
  "Parse PUSH_WHITELIST from env var as JSON.
   Expected format: {\"owner/repo\": [\"branch1\", \"branch2\"]}
   Values must be arrays of strings.
   Returns nil if not set or invalid."
  [env-file]
  (when-let [json-str (get-env env-file "PUSH_WHITELIST")]
    (try
      (let [parsed (js/JSON.parse json-str)
            as-clj (js->clj parsed)]
        ;; Validate structure: must be map with arrays of strings as values
        (if (and (map? as-clj)
                 (every? valid-whitelist-value? (vals as-clj)))
          as-clj
          (do
            (js/console.warn "PUSH_WHITELIST has invalid structure: expected {\"repo\": [\"branch\", ...]}")
            nil)))
      (catch :default e
        (js/console.warn "Failed to parse PUSH_WHITELIST env var:" (.-message e))
        nil))))

(defn- merge-push-whitelist-from-env
  "Merge push whitelist from PUSH_WHITELIST env var.
   Uses shallow merge: env entries replace file entries with the same key.
   For example, if config.edn has {\"org/repo\" [\"main\"]} and env has
   {\"org/repo\" [\"develop\"]}, the result is {\"org/repo\" [\"develop\"]}.
   New keys from env are added to the whitelist.
   env-file should be pre-loaded via load-env-file."
  [config env-file]
  (if-let [env-whitelist (parse-push-whitelist-env env-file)]
    (update config :push-whitelist merge env-whitelist)
    config))

(defn get-config
  "Get merged configuration (defaults + config.edn + env vars).
   Environment variables override file config for secrets.
   Caches result after first read."
  []
  (if-let [cached @config-cache]
    cached
    (let [env-file (load-env-file)
          merged (-> defaults
                     (merge (read-config-file))
                     (merge-github-from-env env-file)
                     (merge-push-whitelist-from-env env-file))]
      (reset! config-cache merged)
      merged)))

(defn get-value
  "Get a specific config value."
  [key]
  (get (get-config) key))

(defn base-url
  "Get the configured base URL for the server.
   Used in MCP responses and OAuth redirects."
  []
  (:base-url (get-config)))

(defn reload!
  "Force reload config from disk."
  []
  (reset! config-cache nil)
  (get-config))

;; Hot-reload hook: `config-cache` is `defonce` and survives hot-reloads,
;; which means new config keys added during dev (e.g. a renamed config
;; block) are invisible to the running process until you explicitly
;; restart it. Force a fresh read after every reload so dev code matches
;; runtime config. Discovered live: a stale cache from before an
;; `:event-stream` block rename caused `event-buffer-size` to silently
;; return nil, which then crashed `trim-to-capacity` and made every
;; `wait_for_event` look broken with no observable error.
(defn ^:dev/after-load reload-after-hot-reload! []
  (reload!))

;; ============================================================================
;; Event stream / GitHub poller config
;; ============================================================================

(defn- parse-int-env
  "Read and parse an env var as an integer. Returns nil if unset or invalid."
  [var-name]
  (when-let [raw (env-var var-name)]
    (let [n (js/parseInt raw 10)]
      (when-not (js/Number.isNaN n) n))))

(defn- nested-config-value
  "Read `[block-key config-key]` from the loaded config map, honoring an
   env-var override (integer)."
  [block-key config-key env-var-name]
  (or (parse-int-env env-var-name)
      (get-in (get-config) [block-key config-key])))

(defn event-buffer-size
  "Shared ring-buffer capacity for `differ.event-stream`. Applies to
   every scope (`github:...`, `session:...`, and any future producer)."
  []
  (nested-config-value :event-stream :buffer-size "DIFFER_EVENT_BUFFER_SIZE"))

(defn github-poll-interval-ms []
  (nested-config-value :github-poller :poll-interval-ms "DIFFER_GITHUB_POLL_INTERVAL_MS"))

(defn github-poller-grace-ms []
  (nested-config-value :github-poller :poller-grace-ms "DIFFER_GITHUB_POLLER_GRACE_MS"))

;; Client-safe config (values that can be exposed to the browser)
(def client-config-keys
  [:large-file-threshold
   :line-count-threshold
   :context-expand-size])

(defn client-config
  "Get config values safe to expose to the client."
  []
  (select-keys (get-config) client-config-keys))
