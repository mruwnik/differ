(ns differ.github-oauth
  "GitHub OAuth flow implementation.
   Handles authorization, token exchange, and token storage."
  (:require [differ.config :as config]
            [differ.db :as db]
            [differ.util :as util]))

(def github-oauth-base "https://github.com")
(def github-api-base "https://api.github.com")

;; Required scope for PR review operations
(def required-scopes ["repo"])

(defn get-github-config
  "Get GitHub OAuth configuration from config."
  []
  (get (config/get-config) :github))

(defn configured?
  "Check if GitHub OAuth is configured."
  []
  (let [{:keys [client-id client-secret]} (get-github-config)]
    (and (some? client-id) (some? client-secret))))

(defn authorization-url
  "Generate GitHub OAuth authorization URL."
  [redirect-uri state]
  (let [{:keys [client-id]} (get-github-config)]
    (str github-oauth-base "/login/oauth/authorize?"
         "client_id=" client-id
         "&redirect_uri=" (js/encodeURIComponent redirect-uri)
         "&scope=" (js/encodeURIComponent (clojure.string/join " " required-scopes))
         "&state=" state)))

(defn exchange-code-for-token
  "Exchange authorization code for access token.
   Returns promise of token data."
  [code]
  (let [{:keys [client-id client-secret]} (get-github-config)]
    (-> (js/fetch (str github-oauth-base "/login/oauth/access_token")
                  #js {:method "POST"
                       :headers #js {"Content-Type" "application/json"
                                     "Accept" "application/json"}
                       :body (js/JSON.stringify
                              #js {:client_id client-id
                                   :client_secret client-secret
                                   :code code})})
        (.then (fn [response]
                 (if (.-ok response)
                   (.json response)
                   (throw (ex-info "Token exchange failed" {:status (.-status response)})))))
        (.then (fn [json]
                 (js->clj json :keywordize-keys true))))))

(defn get-user-info
  "Get authenticated user info from GitHub.
   Returns promise of user data."
  [token]
  (-> (js/fetch (str github-api-base "/user")
                #js {:headers #js {"Authorization" (str "Bearer " token)
                                   "Accept" "application/vnd.github.v3+json"}})
      (.then (fn [response]
               (if (.-ok response)
                 (.json response)
                 (throw (ex-info "Failed to get user info" {:status (.-status response)})))))
      (.then (fn [json]
               (js->clj json :keywordize-keys true)))))

(defn store-token!
  "Store GitHub OAuth token in database.
   Returns the stored token record."
  [token-data user-info]
  (db/create-github-token!
   {:github-user-id (str (:id user-info))
    :github-username (:login user-info)
    :access-token (:access_token token-data)
    :refresh-token (:refresh_token token-data)
    :scope (:scope token-data)
    :expires-at (when-let [exp (:expires_in token-data)]
                  (util/expires-at exp))}))

(defn get-token
  "Get stored GitHub token for a user.
   Returns token record or nil."
  [github-user-id]
  (db/get-github-token github-user-id))

(defn get-any-token
  "Get any stored GitHub token (for single-user scenarios).
   Returns token record or nil."
  []
  (db/get-any-github-token))

(defn list-tokens
  "List all stored GitHub tokens.
   Returns list of token records (without actual token values)."
  []
  (db/list-github-tokens))

(defn delete-token!
  "Delete a stored GitHub token."
  [token-id]
  (db/delete-github-token! token-id))

(defn validate-token
  "Check if a token is still valid by making a test API call.
   Returns promise of boolean."
  [token]
  (-> (get-user-info token)
      (.then (constantly true))
      (.catch (constantly false))))

(defn complete-oauth-flow
  "Complete the OAuth flow after receiving the callback.
   Exchanges code for token, gets user info, stores token.
   Returns promise of {:token :user}."
  [code]
  (let [token-data-atom (atom nil)]
    (-> (exchange-code-for-token code)
        (.then (fn [token-data]
                 (if (:error token-data)
                   (throw (ex-info (or (:error_description token-data) "OAuth error")
                                   {:error (:error token-data)}))
                   (do
                     (reset! token-data-atom token-data)
                     (get-user-info (:access_token token-data))))))
        (.then (fn [user-info]
                 (let [token-data @token-data-atom
                       stored (store-token! token-data user-info)]
                   {:token stored
                    :user user-info}))))))
