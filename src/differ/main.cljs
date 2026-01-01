(ns differ.main
  "Main entry point for the Differ server."
  (:require ["express" :as express]
            ["path" :as path]
            ["fs" :as fs]
            [clojure.edn :as edn]
            [differ.db :as db]
            [differ.api :as api]
            [differ.mcp :as mcp]
            [differ.sse :as sse]))

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
    (.close s)
    (reset! server nil)
    (db/close!)
    (println "Server stopped")))

(defn main []
  (start))

;; For shadow-cljs hot reload
(defn ^:dev/before-load stop-for-reload []
  (stop))

(defn ^:dev/after-load start-after-reload []
  (start))

;; Auto-start when loaded (works with shadow-cljs watch)
(start)
