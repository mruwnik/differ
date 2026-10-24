(ns differ.main
  "Main entry point for the Differ server."
  (:require ["express" :as express]
            ["path" :as path]
            ["fs" :as fs]
            [clojure.edn :as edn]
            [differ.db :as db]
            [differ.api :as api]
            [differ.mcp :as mcp]
            [differ.sse :as sse]
            [differ.event-stream :as event-stream]
            [differ.github-events :as github-events]))

(defonce ^:private server (atom nil))

(defn- config-path []
  (let [dir (path/dirname js/__dirname)]
    (path/join dir "resources" "config.edn")))

(defn- read-config []
  (try
    (-> (fs/readFileSync (config-path) "utf8")
        edn/read-string)
    (catch :default _
      {})))

(defn- get-port []
  (or (some-> js/process.env.PORT js/parseInt)
      (:port (read-config))
      8576))

(defn- static-path []
  ;; In dev, resources/public. In prod, same relative to server.js
  (let [dir (path/dirname js/__dirname)]
    (path/join dir "resources" "public")))

(defn create-app []
  (let [^js app (express)]
    ;; Middleware
    (.use app (.json express))
    (.use app (.urlencoded express #js {:extended true}))

    ;; CORS for development
    (.use app (fn [_req ^js res next]
                (.header res "Access-Control-Allow-Origin" "*")
                (.header res "Access-Control-Allow-Methods" "GET, POST, PATCH, DELETE, OPTIONS")
                (.header res "Access-Control-Allow-Headers" "Content-Type, X-Repo-Path")
                (next)))

    ;; Handle preflight
    (.options app "*" (fn [_req ^js res] (.sendStatus res 200)))

    ;; API routes
    (api/setup-routes app)

    ;; MCP routes
    (mcp/setup-routes app)

    ;; SSE routes
    (sse/setup-routes app)

    ;; Static files (must be after API routes)
    (.use app (express/static (static-path)))

    ;; SPA fallback - serve index.html for all non-API routes
    (.get app "*" (fn [_req ^js res]
                    (.sendFile res (path/join (static-path) "index.html"))))

    app))

(defn start []
  (when-not @server
    ;; Clear the event-stream shutdown flag first. `defonce` keeps the
    ;; flag's atom alive across `^:dev/before-load` → `^:dev/after-load`
    ;; reload cycles, so without this reset the flag would stay true
    ;; after the first hot-reload and polling would silently break.
    (event-stream/reset-shutdown!)
    (db/init!)
    (let [^js app (create-app)
          port (get-port)]
      (reset! server
              (.listen app port
                       (fn []
                         (println (str "Differ server running at http://localhost:" port))
                         (println (str "MCP endpoint: http://localhost:" port "/mcp"))
                         (println (str "SSE endpoint: http://localhost:" port "/events"))))))))

(defn stop []
  (when-let [^js s @server]
    ;; Shutdown order: flag first (via event-stream), then pollers, so
    ;; any in-flight startup .then callbacks see the shutdown flag and
    ;; short-circuit before they install fresh timers.
    (event-stream/shutdown-all!)
    (github-events/stop-all-pollers!)
    (.close s)
    (reset! server nil)
    (db/close!)
    (println "Server stopped")))

(defn main []
  (start))

;; For shadow-cljs hot reload.
;; Note: we don't need to manually clear stale `:cache` entries here.
;; `run-poll-cycle!` short-circuits on `(shutdown?)` after the fetch
;; resolves and dissocs its scope, so any poll cycle that overlaps the
;; shutdown-then-stop sequence cleans up after itself rather than
;; stranding a populated entry. `start` resets the shutdown flag on
;; the next reload.
(defn ^:dev/before-load stop-for-reload []
  (stop))

(defn ^:dev/after-load start-after-reload []
  (start))

;; glibc only reads MALLOC_* at process start, so re-exec once with it set.
;; Pinning the mmap threshold stops glibc ratcheting it up after big
;; execSync git outputs, which otherwise fragments the main arena and
;; holds ~100MB+ of freed memory; the small semi-space stops V8 lazily
;; growing a heap that's mostly garbage.
(defn- relaunch-with-memory-tuning! []
  (when (and (not js/process.env.MALLOC_MMAP_THRESHOLD_)
             (fn? (.-execve js/process)))
    (.execve js/process
             js/process.execPath
             (clj->js (concat [js/process.execPath "--max-semi-space-size=4"]
                              js/process.execArgv
                              (rest js/process.argv)))
             (js/Object.assign #js {} js/process.env #js {:MALLOC_MMAP_THRESHOLD_ "131072"}))))

(relaunch-with-memory-tuning!)

;; Auto-start when loaded (works with shadow-cljs watch)
(start)
