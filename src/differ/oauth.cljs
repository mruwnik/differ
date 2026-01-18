(ns differ.oauth
  "OAuth 2.0 provider implementation for MCP authentication."
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.edn :as edn]
            ["path" :as path]
            ["fs" :as fs]
            [differ.db :as db]
            [differ.util :as util]))

;; Configuration
(def allowed-scopes #{"read" "write"})
(def base-scopes ["read"])
(def access-token-lifetime (* 365 24 3600))  ; 1 year
(def refresh-token-lifetime (* 365 24 3600)) ; 1 year
(def auth-code-lifetime 600)                 ; 10 minutes

(defn- read-config []
  (try
    (let [dir (path/dirname js/__dirname)
          config-path (path/join dir "resources" "config.edn")]
      (-> (fs/readFileSync config-path "utf8")
          edn/read-string))
    (catch :default _
      {})))

(defn- server-url []
  (or (.-DIFFER_URL js/process.env)
      (str "http://localhost:" (or (:port (read-config)) 8576))))

;; Token creation

(defn- create-tokens
  "Create access and refresh tokens for a user."
  [{:keys [user-id client-id oauth-state-id scopes]}]
  (let [access-token (db/create-access-token!
                      {:user-id user-id
                       :oauth-state-id oauth-state-id
                       :expires-at (util/expires-at access-token-lifetime)})
        refresh-token (db/create-refresh-token!
                       {:token (util/gen-token "rt_")
                        :client-id client-id
                        :user-id user-id
                        :scopes scopes
                        :access-token-id (:id access-token)
                        :expires-at (util/expires-at refresh-token-lifetime)})]
    {:access_token (:id access-token)
     :token_type "Bearer"
     :expires_in access-token-lifetime
     :refresh_token (:token refresh-token)
     :scope (str/join " " scopes)}))

;; Token verification

(defn verify-token
  "Verify an access token or API key. Returns token info or nil."
  [token]
  (when token
    (or
     ;; Try as OAuth access token
     (when-let [access-token (db/get-access-token token)]
       (when-not (util/expired? (:expires-at access-token))
         (let [oauth-state (when-let [state-id (:oauth-state-id access-token)]
                             (db/get-oauth-state state-id))]
           {:token token
            :user-id (:user-id access-token)
            :scopes (or (:scopes oauth-state) base-scopes)})))
     ;; Try as API key
     (when-let [user (db/get-user-by-api-key token)]
       {:token token
        :user-id (:id user)
        :client-id (:name user)
        :scopes ["read" "write"]}))))

;; Client registration

(defn register-client
  "Register or update an OAuth client. Generates client-id if not provided."
  [{:keys [client-id client-secret client-name redirect-uris scope client-uri logo-uri]}]
  (let [generated-id (or client-id (util/gen-token "client_"))
        final-scope (or scope "read write")]
    (db/create-oauth-client!
     {:client-id generated-id
      :client-secret client-secret
      :client-name (or client-name "MCP Client")
      :redirect-uris (or redirect-uris [])
      :scope final-scope
      :client-uri client-uri
      :logo-uri logo-uri})))

(defn get-client
  "Get OAuth client information."
  [client-id]
  (db/get-oauth-client client-id))

;; Authorization flow

(defn validate-redirect-uri
  "Validate that redirect URI is registered for the client."
  [client redirect-uri]
  (let [registered (set (:redirect-uris client))]
    (contains? registered redirect-uri)))

(defn validate-scopes
  "Validate requested scopes against allowed and client scopes."
  [requested-scopes client]
  (let [requested-set (set requested-scopes)
        client-scopes (when-let [s (:scope client)]
                        (set (str/split s #"\s+")))]
    (cond
      (not (set/subset? requested-set allowed-scopes))
      {:valid false
       :error (str "Invalid scopes: " (str/join ", " (set/difference requested-set allowed-scopes)))}

      (and (seq client-scopes) (not (set/subset? requested-set client-scopes)))
      {:valid false
       :error (str "Client not registered for scopes: "
                   (str/join ", " (set/difference requested-set client-scopes)))}

      :else
      {:valid true :scopes (vec requested-set)})))

(defn authorize
  "Start authorization flow. Auto-approves for local use (no login required)."
  [{:keys [client-id redirect-uri scopes state code-challenge]}]
  (let [client (get-client client-id)]
    (cond
      (not client)
      {:error "Unknown client"}

      (not (validate-redirect-uri client redirect-uri))
      {:error (str "Invalid redirect_uri: " redirect-uri)}

      :else
      (let [requested-scopes (or (seq scopes) base-scopes)
            scope-validation (validate-scopes requested-scopes client)]
        (if-not (:valid scope-validation)
          {:error (:error scope-validation)}
          ;; Auto-approve: create local user, generate code, redirect immediately
          (let [user (db/get-or-create-user! "local@localhost" "Local User")
                gen-state (or state (util/gen-token "state_"))
                _ (db/create-oauth-state!
                   {:state gen-state
                    :client-id client-id
                    :redirect-uri redirect-uri
                    :redirect-uri-provided-explicitly (boolean redirect-uri)
                    :code-challenge code-challenge
                    :scopes (:scopes scope-validation)
                    :expires-at (util/expires-at auth-code-lifetime)})
                code (util/gen-token "code_")
                _ (db/update-oauth-state! gen-state {:code code :user-id (:id user)})
                separator (if (str/includes? redirect-uri "?") "&" "?")]
            {:redirect-url (str redirect-uri separator
                                "code=" code
                                "&state=" gen-state)}))))))

;; Token exchange

(defn exchange-authorization-code
  "Exchange authorization code for tokens."
  [{:keys [code client-id]}]
  (let [oauth-state (db/get-oauth-state-by-code code)]
    (cond
      (not oauth-state)
      {:error "Invalid authorization code"}

      (not= client-id (:client-id oauth-state))
      {:error "Client ID mismatch"}

      (util/expired? (:expires-at oauth-state))
      {:error "Authorization code expired"}

      (not (:user-id oauth-state))
      {:error "Authorization not completed"}

      :else
      (create-tokens
       {:user-id (:user-id oauth-state)
        :client-id client-id
        :oauth-state-id (:id oauth-state)
        :scopes (:scopes oauth-state)}))))

(defn exchange-refresh-token
  "Exchange refresh token for new access token."
  [{:keys [refresh-token client-id scopes]}]
  (let [db-token (db/get-valid-refresh-token refresh-token client-id)]
    (cond
      (not db-token)
      {:error "Invalid refresh token"}

      (util/expired? (:expires-at db-token))
      (do
        (db/revoke-refresh-token! refresh-token)
        {:error "Refresh token expired"})

      :else
      (let [requested-scopes (or (seq scopes) (:scopes db-token))
            original-scopes (set (:scopes db-token))]
        (if (and (seq scopes) (not (set/subset? (set scopes) original-scopes)))
          {:error "Requested scopes exceed original authorization"}
          (create-tokens
           {:user-id (:user-id db-token)
            :client-id client-id
            :scopes requested-scopes}))))))

;; Token revocation

(defn revoke-token
  "Revoke an access or refresh token."
  [{:keys [token token-type-hint]}]
  (let [revoked-access (when (or (nil? token-type-hint) (= token-type-hint "access_token"))
                         (when (db/get-access-token token)
                           (db/delete-access-token! token)
                           true))
        revoked-refresh (when (and (not revoked-access)
                                   (or (nil? token-type-hint) (= token-type-hint "refresh_token")))
                          (when (db/get-refresh-token token)
                            (db/revoke-refresh-token! token)
                            true))]
    {:revoked (or revoked-access revoked-refresh false)}))

;; OAuth metadata

(defn get-authorization-server-metadata
  "Return OAuth 2.0 Authorization Server Metadata."
  []
  (let [base (server-url)]
    {:issuer base
     :authorization_endpoint (str base "/oauth/authorize")
     :token_endpoint (str base "/oauth/token")
     :revocation_endpoint (str base "/oauth/revoke")
     :registration_endpoint (str base "/oauth/register")
     :scopes_supported (vec allowed-scopes)
     :response_types_supported ["code"]
     :grant_types_supported ["authorization_code" "refresh_token"]
     :token_endpoint_auth_methods_supported ["none" "client_secret_basic" "client_secret_post"]
     :code_challenge_methods_supported ["S256" "plain"]}))

(defn get-protected-resource-metadata
  "Return OAuth 2.0 Protected Resource Metadata."
  []
  (let [base (server-url)]
    {:resource base
     :authorization_servers [base]
     :scopes_supported (vec allowed-scopes)
     :bearer_methods_supported ["header"]
     :resource_documentation (str base "/docs")}))
