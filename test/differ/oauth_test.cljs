(ns differ.oauth-test
  "Tests for OAuth 2.0 provider implementation.
   Tests client registration, authorization flow, token exchange, and validation."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [clojure.set :as set]
            [differ.oauth :as oauth]
            [differ.db :as db]
            [differ.test-helpers :as helpers]
            [differ.util :as util]))

;; ============================================================================
;; Test Setup
;; ============================================================================

(def ^:dynamic *test-db* nil)

(defn with-test-db [f]
  (binding [*test-db* (helpers/init-test-db!)]
    (try
      (f)
      (finally
        (helpers/cleanup-test-db!)))))

(use-fixtures :each with-test-db)

;; ============================================================================
;; Constants Tests
;; ============================================================================

(deftest allowed-scopes-test
  (testing "allowed scopes contains read and write"
    (is (set? oauth/allowed-scopes))
    (is (contains? oauth/allowed-scopes "read"))
    (is (contains? oauth/allowed-scopes "write"))))

(deftest base-scopes-test
  (testing "base scopes defaults to read"
    (is (vector? oauth/base-scopes))
    (is (some #(= "read" %) oauth/base-scopes))))

(deftest token-lifetimes-test
  (testing "access token lifetime is set"
    (is (pos? oauth/access-token-lifetime)))

  (testing "refresh token lifetime is set"
    (is (pos? oauth/refresh-token-lifetime)))

  (testing "auth code lifetime is short (10 minutes)"
    (is (= 600 oauth/auth-code-lifetime))))

;; ============================================================================
;; Scope Validation Tests
;; ============================================================================

(deftest validate-scopes-valid-test
  (testing "accepts valid single scope"
    (let [result (oauth/validate-scopes ["read"] nil)]
      (is (true? (:valid result)))
      (is (= ["read"] (:scopes result)))))

  (testing "accepts valid multiple scopes"
    (let [result (oauth/validate-scopes ["read" "write"] nil)]
      (is (true? (:valid result)))
      (is (= 2 (count (:scopes result))))))

  (testing "accepts empty scopes (defaults to base)"
    (let [result (oauth/validate-scopes [] nil)]
      (is (true? (:valid result))))))

(deftest validate-scopes-invalid-test
  (testing "rejects unknown scopes"
    (let [result (oauth/validate-scopes ["read" "admin"] nil)]
      (is (false? (:valid result)))
      (is (str/includes? (:error result) "Invalid scopes"))))

  (testing "rejects completely invalid scope"
    (let [result (oauth/validate-scopes ["superadmin"] nil)]
      (is (false? (:valid result)))))

  (testing "error message includes invalid scope name"
    (let [result (oauth/validate-scopes ["invalid-scope"] nil)]
      (is (str/includes? (:error result) "invalid-scope")))))

(deftest validate-scopes-client-restriction-test
  (testing "respects client scope restrictions"
    (let [;; Client only registered for "read" scope
          client {:scope "read"}
          result (oauth/validate-scopes ["read" "write"] client)]
      (is (false? (:valid result)))
      (is (str/includes? (:error result) "not registered")))))

;; ============================================================================
;; Redirect URI Validation Tests
;; ============================================================================

(deftest validate-redirect-uri-test
  (testing "accepts registered redirect URI"
    (let [client {:redirect-uris ["http://localhost:3000/callback"
                                  "http://localhost:8080/callback"]}]
      (is (true? (oauth/validate-redirect-uri client "http://localhost:3000/callback")))))

  (testing "rejects unregistered redirect URI"
    (let [client {:redirect-uris ["http://localhost:3000/callback"]}]
      (is (false? (oauth/validate-redirect-uri client "http://evil.com/callback")))))

  (testing "rejects when no URIs registered"
    (let [client {:redirect-uris []}]
      (is (false? (oauth/validate-redirect-uri client "http://localhost:3000/callback")))))

  (testing "handles nil redirect-uris"
    (let [client {}]
      (is (false? (oauth/validate-redirect-uri client "http://localhost:3000/callback"))))))

;; ============================================================================
;; Client Registration Tests
;; ============================================================================

(deftest register-client-test
  (testing "generates client-id when not provided"
    (let [client (oauth/register-client {})]
      (is (some? (:client-id client)))
      (is (str/starts-with? (:client-id client) "client_"))))

  (testing "uses provided client-id"
    (let [client (oauth/register-client {:client-id "my-client-123"})]
      (is (= "my-client-123" (:client-id client)))))

  (testing "sets default client name"
    (let [client (oauth/register-client {})]
      (is (= "MCP Client" (:client-name client)))))

  (testing "uses provided client name"
    (let [client (oauth/register-client {:client-name "My App"})]
      (is (= "My App" (:client-name client)))))

  (testing "sets default scope"
    (let [client (oauth/register-client {})]
      (is (= "read write" (:scope client)))))

  (testing "stores redirect URIs"
    (let [uris ["http://localhost:3000/callback"]
          client (oauth/register-client {:redirect-uris uris})]
      (is (= uris (:redirect-uris client))))))

(deftest register-client-redirect-uri-validation-test
  (testing "accepts localhost redirect URIs"
    (let [client (oauth/register-client {:redirect-uris ["http://localhost:3000/callback"]})]
      (is (nil? (:error client)))
      (is (some? (:client-id client)))))

  (testing "accepts 127.0.0.1 redirect URIs"
    (let [client (oauth/register-client {:redirect-uris ["http://127.0.0.1:8080/callback"]})]
      (is (nil? (:error client)))
      (is (some? (:client-id client)))))

  (testing "accepts 192.168.x.x redirect URIs (local network)"
    (let [client (oauth/register-client {:redirect-uris ["http://192.168.1.100:3000/callback"]})]
      (is (nil? (:error client)))
      (is (some? (:client-id client)))))

  (testing "accepts 10.x.x.x redirect URIs (local network)"
    (let [client (oauth/register-client {:redirect-uris ["http://10.0.0.5:3000/callback"]})]
      (is (nil? (:error client)))
      (is (some? (:client-id client)))))

  (testing "rejects external domain redirect URIs"
    (let [result (oauth/register-client {:redirect-uris ["https://evil.com/callback"]})]
      (is (some? (:error result)))
      (is (str/includes? (:error result) "localhost or local network"))
      (is (str/includes? (:error result) "evil.com"))))

  (testing "rejects mixed valid and invalid redirect URIs"
    (let [result (oauth/register-client {:redirect-uris ["http://localhost:3000/callback"
                                                         "https://attacker.com/steal"]})]
      (is (some? (:error result)))
      (is (str/includes? (:error result) "attacker.com"))))

  (testing "accepts empty redirect URIs"
    (let [client (oauth/register-client {:redirect-uris []})]
      (is (nil? (:error client)))
      (is (some? (:client-id client)))))

  (testing "accepts no redirect URIs specified"
    (let [client (oauth/register-client {})]
      (is (nil? (:error client)))
      (is (some? (:client-id client)))))

  (testing "rejects malformed URIs"
    (let [result (oauth/register-client {:redirect-uris ["not-a-valid-url"]})]
      (is (some? (:error result))))))

(deftest get-client-test
  (testing "retrieves registered client"
    (let [registered (oauth/register-client {:client-id "test-client"})
          retrieved (oauth/get-client "test-client")]
      (is (some? retrieved))
      (is (= "test-client" (:client-id retrieved)))))

  (testing "returns nil for unknown client"
    (let [client (oauth/get-client "nonexistent-client")]
      (is (nil? client)))))

;; ============================================================================
;; Authorization Flow Tests
;; ============================================================================

(deftest authorize-unknown-client-test
  (testing "rejects unknown client"
    (let [result (oauth/authorize {:client-id "unknown-client"
                                   :redirect-uri "http://localhost/callback"
                                   :scopes ["read"]})]
      (is (= "Unknown client" (:error result))))))

(deftest authorize-invalid-redirect-test
  (testing "rejects invalid redirect URI"
    ;; Register a client first
    (oauth/register-client {:client-id "test-client"
                            :redirect-uris ["http://localhost:3000/callback"]})
    (let [result (oauth/authorize {:client-id "test-client"
                                   :redirect-uri "http://evil.com/callback"
                                   :scopes ["read"]})]
      (is (str/includes? (:error result) "Invalid redirect_uri")))))

(deftest authorize-invalid-scopes-test
  (testing "rejects invalid scopes"
    (oauth/register-client {:client-id "test-client"
                            :redirect-uris ["http://localhost:3000/callback"]})
    (let [result (oauth/authorize {:client-id "test-client"
                                   :redirect-uri "http://localhost:3000/callback"
                                   :scopes ["admin"]})]
      (is (str/includes? (:error result) "Invalid scopes")))))

(deftest authorize-success-test
  (testing "returns redirect URL on success"
    (oauth/register-client {:client-id "test-client"
                            :redirect-uris ["http://localhost:3000/callback"]})
    (let [state (str "user-state-" (random-uuid))
          result (oauth/authorize {:client-id "test-client"
                                   :redirect-uri "http://localhost:3000/callback"
                                   :scopes ["read"]
                                   :state state})]
      (is (some? (:redirect-url result)))
      (is (str/starts-with? (:redirect-url result) "http://localhost:3000/callback"))
      (is (str/includes? (:redirect-url result) "code="))
      (is (str/includes? (:redirect-url result) "state=")))))

(deftest authorize-preserves-state-test
  (testing "includes provided state in redirect"
    (oauth/register-client {:client-id "test-client"
                            :redirect-uris ["http://localhost:3000/callback"]})
    (let [state (str "csrf-" (random-uuid))
          result (oauth/authorize {:client-id "test-client"
                                   :redirect-uri "http://localhost:3000/callback"
                                   :scopes ["read"]
                                   :state state})]
      (is (str/includes? (:redirect-url result) (str "state=" state))))))

(deftest authorize-handles-redirect-with-query-test
  (testing "appends to existing query string in redirect URI"
    (oauth/register-client {:client-id "test-client"
                            :redirect-uris ["http://localhost:3000/callback?existing=param"]})
    (let [result (oauth/authorize {:client-id "test-client"
                                   :redirect-uri "http://localhost:3000/callback?existing=param"
                                   :scopes ["read"]})]
      ;; Should use & instead of ? for additional params
      (is (str/includes? (:redirect-url result) "existing=param&code=")))))

;; ============================================================================
;; Token Exchange Tests
;; ============================================================================

(deftest exchange-authorization-code-invalid-code-test
  (testing "rejects invalid authorization code"
    (let [result (oauth/exchange-authorization-code {:code "invalid-code"
                                                     :client-id "test-client"})]
      (is (= "Invalid authorization code" (:error result))))))

(deftest exchange-authorization-code-client-mismatch-test
  (testing "rejects mismatched client ID"
    ;; Set up a valid auth code first
    (oauth/register-client {:client-id "real-client"
                            :redirect-uris ["http://localhost/callback"]})
    (let [auth-result (oauth/authorize {:client-id "real-client"
                                        :redirect-uri "http://localhost/callback"
                                        :scopes ["read"]})
          ;; Extract code from redirect URL
          code (when-let [url (:redirect-url auth-result)]
                 (second (re-find #"code=([^&]+)" url)))]
      (when code
        (let [result (oauth/exchange-authorization-code {:code code
                                                         :client-id "wrong-client"})]
          (is (= "Client ID mismatch" (:error result))))))))

(deftest exchange-authorization-code-success-test
  (testing "returns tokens on success"
    (oauth/register-client {:client-id "test-client"
                            :redirect-uris ["http://localhost/callback"]})
    (let [auth-result (oauth/authorize {:client-id "test-client"
                                        :redirect-uri "http://localhost/callback"
                                        :scopes ["read"]})
          code (when-let [url (:redirect-url auth-result)]
                 (second (re-find #"code=([^&]+)" url)))]
      (when code
        (let [result (oauth/exchange-authorization-code {:code code
                                                         :client-id "test-client"})]
          (is (nil? (:error result)))
          (is (string? (:access_token result)))
          (is (= "Bearer" (:token_type result)))
          (is (number? (:expires_in result)))
          (is (string? (:refresh_token result))))))))

;; ============================================================================
;; Refresh Token Exchange Tests
;; ============================================================================

(deftest exchange-refresh-token-invalid-test
  (testing "rejects invalid refresh token"
    (let [result (oauth/exchange-refresh-token {:refresh-token "invalid-token"
                                                :client-id "test-client"})]
      (is (= "Invalid refresh token" (:error result))))))

;; ============================================================================
;; Token Revocation Tests
;; ============================================================================

(deftest revoke-token-nonexistent-test
  (testing "handles revocation of nonexistent token"
    (let [result (oauth/revoke-token {:token "nonexistent-token"})]
      (is (false? (:revoked result))))))

(deftest revoke-token-with-hint-test
  (testing "respects token type hint for access token"
    (let [result (oauth/revoke-token {:token "fake-token"
                                      :token-type-hint "access_token"})]
      (is (map? result))
      (is (contains? result :revoked))))

  (testing "respects token type hint for refresh token"
    (let [result (oauth/revoke-token {:token "fake-token"
                                      :token-type-hint "refresh_token"})]
      (is (map? result))
      (is (contains? result :revoked)))))

;; ============================================================================
;; Token Verification Tests
;; ============================================================================

(deftest verify-token-nil-test
  (testing "returns nil for nil token"
    (is (nil? (oauth/verify-token nil)))))

(deftest verify-token-nonexistent-test
  (testing "returns nil for nonexistent token"
    (is (nil? (oauth/verify-token "nonexistent-token")))))

;; ============================================================================
;; Metadata Endpoint Tests
;; ============================================================================

(deftest authorization-server-metadata-test
  (testing "returns required OAuth metadata fields"
    (let [metadata (oauth/get-authorization-server-metadata)]
      (is (string? (:issuer metadata)))
      (is (string? (:authorization_endpoint metadata)))
      (is (string? (:token_endpoint metadata)))
      (is (string? (:revocation_endpoint metadata)))
      (is (string? (:registration_endpoint metadata)))
      (is (vector? (:scopes_supported metadata)))
      (is (vector? (:response_types_supported metadata)))
      (is (vector? (:grant_types_supported metadata)))))

  (testing "endpoints have correct paths"
    (let [metadata (oauth/get-authorization-server-metadata)]
      (is (str/ends-with? (:authorization_endpoint metadata) "/oauth/authorize"))
      (is (str/ends-with? (:token_endpoint metadata) "/oauth/token"))
      (is (str/ends-with? (:revocation_endpoint metadata) "/oauth/revoke"))
      (is (str/ends-with? (:registration_endpoint metadata) "/oauth/register"))))

  (testing "supported scopes include read and write"
    (let [metadata (oauth/get-authorization-server-metadata)
          scopes (set (:scopes_supported metadata))]
      (is (contains? scopes "read"))
      (is (contains? scopes "write"))))

  (testing "supports authorization_code and refresh_token grants"
    (let [metadata (oauth/get-authorization-server-metadata)
          grants (set (:grant_types_supported metadata))]
      (is (contains? grants "authorization_code"))
      (is (contains? grants "refresh_token")))))

(deftest protected-resource-metadata-test
  (testing "returns required protected resource metadata"
    (let [metadata (oauth/get-protected-resource-metadata)]
      (is (string? (:resource metadata)))
      (is (vector? (:authorization_servers metadata)))
      (is (vector? (:scopes_supported metadata)))
      (is (vector? (:bearer_methods_supported metadata)))))

  (testing "supports bearer token in header"
    (let [metadata (oauth/get-protected-resource-metadata)
          methods (set (:bearer_methods_supported metadata))]
      (is (contains? methods "header")))))

;; ============================================================================
;; PKCE Verification Tests
;; ============================================================================

(deftest pkce-without-challenge-test
  (testing "token exchange succeeds without PKCE when no challenge was provided"
    (oauth/register-client {:client-id "test-client"
                            :redirect-uris ["http://localhost/callback"]})
    ;; Authorize without code_challenge
    (let [auth-result (oauth/authorize {:client-id "test-client"
                                        :redirect-uri "http://localhost/callback"
                                        :scopes ["read"]})
          code (when-let [url (:redirect-url auth-result)]
                 (second (re-find #"code=([^&]+)" url)))]
      (when code
        ;; Exchange without code_verifier should succeed
        (let [result (oauth/exchange-authorization-code {:code code
                                                         :client-id "test-client"})]
          (is (nil? (:error result)))
          (is (string? (:access_token result))))))))

(deftest pkce-with-valid-verifier-test
  (testing "token exchange succeeds with valid PKCE verifier"
    (oauth/register-client {:client-id "test-client"
                            :redirect-uris ["http://localhost/callback"]})
    ;; Generate a code_verifier and compute the challenge (SHA256 base64url encoded)
    (let [code-verifier "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
          code-challenge (util/sha256-base64url code-verifier)
          auth-result (oauth/authorize {:client-id "test-client"
                                        :redirect-uri "http://localhost/callback"
                                        :scopes ["read"]
                                        :code-challenge code-challenge})
          code (when-let [url (:redirect-url auth-result)]
                 (second (re-find #"code=([^&]+)" url)))]
      (when code
        ;; Exchange with matching code_verifier should succeed
        (let [result (oauth/exchange-authorization-code {:code code
                                                         :client-id "test-client"
                                                         :code-verifier code-verifier})]
          (is (nil? (:error result)))
          (is (string? (:access_token result))))))))

(deftest pkce-missing-verifier-test
  (testing "token exchange fails when challenge was provided but verifier is missing"
    (oauth/register-client {:client-id "test-client"
                            :redirect-uris ["http://localhost/callback"]})
    (let [code-verifier "test-verifier-string-12345"
          code-challenge (util/sha256-base64url code-verifier)
          auth-result (oauth/authorize {:client-id "test-client"
                                        :redirect-uri "http://localhost/callback"
                                        :scopes ["read"]
                                        :code-challenge code-challenge})
          code (when-let [url (:redirect-url auth-result)]
                 (second (re-find #"code=([^&]+)" url)))]
      (when code
        ;; Exchange without code_verifier should fail
        (let [result (oauth/exchange-authorization-code {:code code
                                                         :client-id "test-client"})]
          (is (= "Invalid code_verifier" (:error result))))))))

(deftest pkce-wrong-verifier-test
  (testing "token exchange fails with incorrect PKCE verifier"
    (oauth/register-client {:client-id "test-client"
                            :redirect-uris ["http://localhost/callback"]})
    (let [code-verifier "correct-verifier-string"
          code-challenge (util/sha256-base64url code-verifier)
          auth-result (oauth/authorize {:client-id "test-client"
                                        :redirect-uri "http://localhost/callback"
                                        :scopes ["read"]
                                        :code-challenge code-challenge})
          code (when-let [url (:redirect-url auth-result)]
                 (second (re-find #"code=([^&]+)" url)))]
      (when code
        ;; Exchange with wrong code_verifier should fail
        (let [result (oauth/exchange-authorization-code {:code code
                                                         :client-id "test-client"
                                                         :code-verifier "wrong-verifier-string"})]
          (is (= "Invalid code_verifier" (:error result))))))))
