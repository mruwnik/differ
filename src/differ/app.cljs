(ns differ.app
  "Express app: middleware and routes. Kept apart from differ.main so tests
   can build the app without starting the server."
  (:require ["express" :as express]
            ["path" :as path]
            [differ.api :as api]
            [differ.mcp :as mcp]
            [differ.sse :as sse]))

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
