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
    ;; No CORS headers: the web UI is served from this origin (and the dev
    ;; server proxies to it), so other origins get no access to the API.
    (.use app (.json express))
    ;; Form-encoded bodies only where OAuth needs them: the login form
    ;; (POST /oauth/authorize), token and revoke. Everywhere else, including
    ;; /oauth/register, only JSON bodies are parsed. Flat keys are enough.
    (let [form (.urlencoded express #js {:extended false})]
      (doseq [route ["/oauth/authorize" "/oauth/token" "/oauth/revoke"]]
        (.use app route form)))

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
