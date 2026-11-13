(ns differ.app
  "Express app: middleware and routes. Kept apart from differ.main so tests
   can build the app without starting the server."
  (:require ["express" :as express]
            ["path" :as path]
            [clojure.string :as str]
            [differ.api :as api]
            [differ.config :as config]
            [differ.mcp :as mcp]
            [differ.sse :as sse]))

(defn- static-path []
  ;; In dev, resources/public. In prod, same relative to server.js
  (let [dir (path/dirname js/__dirname)]
    (path/join dir "resources" "public")))

(defn- hostname
  "Lower-cased hostname of url (port dropped), or nil when it doesn't parse,
   as for `Origin: null`."
  [url]
  (when (seq url)
    (try
      (not-empty (str/lower-case (.-hostname (js/URL. url))))
      (catch :default _ nil))))

(defn cross-site-request?
  "True when a state-changing request comes from a page on another site.
   GET, HEAD and OPTIONS never are, and neither are requests with no Origin
   and no Sec-Fetch-Site (CLI and MCP clients). Otherwise it is cross-site
   when Sec-Fetch-Site says so, or when the Origin's hostname matches neither
   the Host header's nor one of trusted-hostnames (the configured base URL,
   for reverse proxies). Ports are ignored, so the dev UI on another port,
   which proxies to this server, keeps working. Host itself is not checked."
  [{:keys [method origin host sec-fetch-site]} trusted-hostnames]
  (boolean
   (and (not (contains? #{"GET" "HEAD" "OPTIONS"} (str/upper-case (or method ""))))
        (or (= "cross-site" sec-fetch-site)
            (when (some? origin)
              (let [from (hostname origin)]
                (not (and from
                          (or (= from (hostname (str "http://" host)))
                              (contains? trusted-hostnames from))))))))))

(defn- refuse-cross-site
  "Middleware: answer 403 to cross-site state-changing requests."
  [^js req ^js res next]
  (if (cross-site-request? {:method (.-method req)
                            :origin (.get req "origin")
                            :host (.get req "host")
                            :sec-fetch-site (.get req "sec-fetch-site")}
                           (set (keep hostname [(config/base-url)
                                                (.-DIFFER_URL js/process.env)])))
    (-> res (.status 403) (.json #js {:error "Cross-site request refused"}))
    (next)))

(defn create-app []
  (let [^js app (express)]
    ;; Middleware
    ;; No CORS headers: the web UI is served from this origin (and the dev
    ;; server proxies to it), so pages on other origins cannot read API
    ;; responses. They can still send simple requests (a form or text/plain
    ;; POST needs no preflight), so state-changing requests from another
    ;; site are refused here. Host is not checked, so this does not stop DNS
    ;; rebinding.
    (.use app refuse-cross-site)
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
