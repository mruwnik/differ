(ns differ.gdocs.auth
  "Service-account authentication via the OAuth2 JWT-bearer flow.

   Mirrors the Python tool's google.oauth2.service_account path, but signs the
   RS256 JWT assertion with node's built-in `crypto` (no google-auth / no extra
   npm dependency). We read the service-account key file, sign a short-lived JWT
   asserting the requested scopes, exchange it at the creds file's token_uri for
   a bearer token, and cache that token until shortly before it expires. The
   private key is only ever used to sign locally — it never leaves the process."
  (:require ["fs" :as fs]
            ["path" :as path]
            ["crypto" :as crypto]
            [clojure.string :as str]))

(defn default-creds-path
  "Default service-account key location: resources/gcp-creds.json, resolved
   relative to the project root (the parent of the compiled output dir, the
   same anchor differ.config uses for config.edn). Overridable via GDOCS_CREDS."
  []
  ;; NOTE: js/__dirname points at the build output dir (e.g. target/); its
  ;; parent is the project root. Requires Node CommonJS mode, like config.cljs.
  (path/join (path/dirname js/__dirname) "resources" "gcp-creds.json"))

(def ^:private scopes
  ["https://www.googleapis.com/auth/documents"
   "https://www.googleapis.com/auth/drive.readonly"])

;; token cache: creds-path -> {:token str :expires-at-ms number}
(defonce ^:private token-cache (atom {}))

;; in-flight refreshes: creds-path -> Promise, so concurrent cache-miss callers
;; share one JWT signing + token exchange instead of each doing their own.
(defonce ^:private pending-cache (atom {}))

;; Refresh a bit before the real expiry so an in-flight request can't race the
;; token going stale.
(def ^:private expiry-skew-ms 60000)

(defn- b64url
  "base64url-encode a string or Buffer (no padding)."
  [x]
  (.toString (js/Buffer.from x) "base64url"))

(defn reescape-json-control-chars
  "Service-account keys are frequently pasted with the PEM private_key's real
   newlines left intact, producing JSON that is technically invalid (raw control
   chars inside a string literal) and rejected by JSON.parse. Re-escape any
   control char that appears *inside* a JSON string so such files still load.
   Already-valid JSON is returned byte-for-byte unchanged."
  [text]
  (let [n (count text)
        out #js []]
    (loop [i 0 in-str false esc false]
      (if (>= i n)
        (.join out "")
        (let [ch (.charAt text i)
              code (.charCodeAt text i)]
          (cond
            esc (do (.push out ch) (recur (inc i) in-str false))
            (= ch "\\") (do (.push out ch) (recur (inc i) in-str true))
            (= ch "\"") (do (.push out ch) (recur (inc i) (not in-str) false))
            (and in-str (< code 0x20))
            (do (.push out (case code
                             10 "\\n" 9 "\\t" 13 "\\r"
                             (str "\\u" (.padStart (.toString code 16) 4 "0"))))
                (recur (inc i) in-str false))
            :else (do (.push out ch) (recur (inc i) in-str false))))))))

(defn- read-creds
  "Read and parse the service-account JSON key file (string-keyed map)."
  [creds-path]
  (let [raw (try
              (fs/readFileSync creds-path "utf8")
              (catch :default e
                (throw (ex-info (str "gdocs: cannot read credentials at " creds-path
                                     " (" (or (.-message e) e) "). "
                                     "Set GDOCS_CREDS to the service-account key path.")
                                {:type :creds-missing :path creds-path}))))]
    (js->clj (js/JSON.parse (reescape-json-control-chars raw)))))

(defn- sign-jwt
  "Build and RS256-sign a JWT assertion for the service account."
  [creds now-sec]
  (let [token-uri (get creds "token_uri" "https://oauth2.googleapis.com/token")
        header (cond-> {:alg "RS256" :typ "JWT"}
                 (get creds "private_key_id") (assoc :kid (get creds "private_key_id")))
        claims {:iss (get creds "client_email")
                :scope (str/join " " scopes)
                :aud token-uri
                :iat now-sec
                :exp (+ now-sec 3600)}
        signing-input (str (b64url (js/JSON.stringify (clj->js header)))
                           "."
                           (b64url (js/JSON.stringify (clj->js claims))))
        signer (doto (crypto/createSign "RSA-SHA256")
                 (.update signing-input)
                 (.end))
        signature (.toString (.sign signer (get creds "private_key")) "base64url")]
    {:assertion (str signing-input "." signature)
     :token-uri token-uri}))

(defn- exchange-assertion
  "POST the signed assertion to the token endpoint. Returns a Promise of
   {:token :expires-in}."
  [{:keys [assertion token-uri]}]
  (let [body (str "grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer"
                  "&assertion=" assertion)]
    (-> (js/fetch token-uri
                  #js {:method "POST"
                       :headers #js {"Content-Type" "application/x-www-form-urlencoded"}
                       :body body})
        (.then (fn [^js response]
                 (if (.-ok response)
                   (-> (.json response)
                       (.then (fn [json]
                                (let [m (js->clj json)
                                      token (get m "access_token")]
                                  ;; A 2xx without an access_token (odd response
                                  ;; shape, intercepting proxy) must NOT be cached
                                  ;; as a nil token, or every call for the next
                                  ;; hour would send an empty bearer and 401.
                                  (if (str/blank? token)
                                    (throw (ex-info "gdocs: token endpoint returned no access_token"
                                                    {:type :auth-failed}))
                                    {:token token
                                     :expires-in (get m "expires_in" 3600)})))))
                   (-> (.text response)
                       (.then (fn [text]
                                (throw (ex-info (str "gdocs: token exchange failed: "
                                                     (.-status response) " " text)
                                                {:type :auth-failed
                                                 :status (.-status response)})))))))))))

(defn get-access-token
  "Return a Promise of a fresh OAuth2 bearer token for the service account,
   reusing a cached token until it is within the skew window of expiry.
   Concurrent cache-miss callers share a single in-flight exchange."
  [creds-path]
  (let [now-ms (js/Date.now)
        cached (get @token-cache creds-path)]
    (cond
      (and cached (> (:expires-at-ms cached) (+ now-ms expiry-skew-ms)))
      (js/Promise.resolve (:token cached))

      (get @pending-cache creds-path)
      (get @pending-cache creds-path)

      :else
      (let [promise (-> (exchange-assertion (sign-jwt (read-creds creds-path) (quot now-ms 1000)))
                        (.then (fn [{:keys [token expires-in]}]
                                 (swap! token-cache assoc creds-path
                                        {:token token
                                         :expires-at-ms (+ now-ms (* expires-in 1000))})
                                 token))
                        ;; Clear the in-flight entry on both success and failure
                        ;; so a failed refresh doesn't wedge future attempts.
                        (.finally (fn [] (swap! pending-cache dissoc creds-path))))]
        (swap! pending-cache assoc creds-path promise)
        promise))))
