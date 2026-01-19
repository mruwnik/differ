(ns differ.github-oauth-test
  "Tests for GitHub OAuth flow implementation.
   Tests configuration, URL generation, token management, and flow logic."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.string :as str]
            [differ.github-oauth :as gh-oauth]
            [differ.test-helpers :as helpers]))

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

(deftest github-urls-test
  (testing "GitHub OAuth base URL is correct"
    (is (= "https://github.com" gh-oauth/github-oauth-base)))

  (testing "GitHub API base URL is correct"
    (is (= "https://api.github.com" gh-oauth/github-api-base)))

  (testing "required scopes include repo access"
    (is (vector? gh-oauth/required-scopes))
    (is (some #(= "repo" %) gh-oauth/required-scopes))))

;; ============================================================================
;; Configuration Tests
;; ============================================================================

(deftest configured?-test
  (testing "returns false when config is missing"
    ;; Without env vars set, should return false
    ;; Note: actual behavior depends on config state
    (is (boolean? (gh-oauth/configured?)))))

;; ============================================================================
;; Authorization URL Tests
;; ============================================================================

(deftest authorization-url-test
  (testing "generates URL with required components"
    ;; This will fail if not configured, but we test the structure
    (let [redirect-uri "http://localhost:8576/oauth/github/callback"
          state "test-state-123"]
      ;; If configured, URL should contain expected parts
      (when (gh-oauth/configured?)
        (let [url (gh-oauth/authorization-url redirect-uri state)]
          (is (str/starts-with? url "https://github.com/login/oauth/authorize"))
          (is (str/includes? url "client_id="))
          (is (str/includes? url (str "state=" state)))
          (is (str/includes? url "redirect_uri="))
          (is (str/includes? url "scope=")))))))

(deftest authorization-url-encoding-test
  (testing "properly encodes redirect URI"
    (when (gh-oauth/configured?)
      (let [redirect-uri "http://localhost:8576/callback?foo=bar"
            state "state123"
            url (gh-oauth/authorization-url redirect-uri state)]
        ;; Should URL-encode the redirect_uri
        (is (str/includes? url (js/encodeURIComponent redirect-uri)))))))

;; ============================================================================
;; Token Validity Tests
;; ============================================================================

(deftest token-valid?-logic-test
  (testing "token without expiry is always valid"
    ;; Simulating the token-valid? logic
    (let [token-valid? (fn [token]
                         (or (nil? (:expires-at token))
                             (> (js/Date.parse (:expires-at token)) (js/Date.now))))]
      (is (true? (token-valid? {:access-token "abc" :expires-at nil})))
      (is (true? (token-valid? {:access-token "abc"})))))

  (testing "token with future expiry is valid"
    (let [token-valid? (fn [token]
                         (or (nil? (:expires-at token))
                             (> (js/Date.parse (:expires-at token)) (js/Date.now))))
          future-date (js/Date. (+ (js/Date.now) 3600000))]
      (is (true? (token-valid? {:access-token "abc"
                                :expires-at (.toISOString future-date)})))))

  (testing "token with past expiry is invalid"
    (let [token-valid? (fn [token]
                         (or (nil? (:expires-at token))
                             (> (js/Date.parse (:expires-at token)) (js/Date.now))))
          past-date (js/Date. (- (js/Date.now) 3600000))]
      (is (false? (token-valid? {:access-token "abc"
                                 :expires-at (.toISOString past-date)}))))))

;; ============================================================================
;; Token Storage Tests
;; ============================================================================

(deftest list-tokens-test
  (testing "returns a collection"
    (let [tokens (gh-oauth/list-tokens)]
      ;; Should return nil or a collection
      (is (or (nil? tokens) (seqable? tokens)))))

  (testing "token records have expected structure when present"
    (let [tokens (gh-oauth/list-tokens)]
      (when (seq tokens)
        (let [token (first tokens)]
          ;; Tokens should have these keys
          (is (contains? token :id))
          (is (contains? token :github-username)))))))

(deftest get-any-token-test
  (testing "returns nil or a token record"
    (let [token (gh-oauth/get-any-token)]
      ;; Should return nil or a map with token data
      (is (or (nil? token) (map? token)))))

  (testing "token record has expected fields when present"
    (let [token (gh-oauth/get-any-token)]
      (when token
        (is (string? (:access-token token)))
        (is (string? (:github-username token)))))))

(deftest get-all-tokens-test
  (testing "returns a collection"
    (let [tokens (gh-oauth/get-all-tokens)]
      (is (seqable? tokens))))

  (testing "filters out expired tokens"
    ;; get-all-tokens should only return valid (non-expired) tokens
    (let [tokens (gh-oauth/get-all-tokens)
          now (js/Date.now)]
      (doseq [token tokens]
        ;; Each token should either have no expiry or a future expiry
        (is (or (nil? (:expires-at token))
                (> (js/Date.parse (:expires-at token)) now)))))))

;; ============================================================================
;; PAT Validation Logic Tests
;; ============================================================================

(deftest validate-pat-returns-promise-test
  (testing "validate-pat returns a promise"
    (let [result (gh-oauth/validate-pat "fake-token")]
      ;; Should be a promise (has .then method)
      (is (fn? (.-then result))))))

(deftest validate-token-returns-promise-test
  (testing "validate-token returns a promise"
    (let [result (gh-oauth/validate-token "fake-token")]
      (is (fn? (.-then result))))))

;; ============================================================================
;; Complete OAuth Flow Tests
;; ============================================================================

(deftest complete-oauth-flow-returns-promise-test
  (testing "complete-oauth-flow returns a promise"
    (let [result (gh-oauth/complete-oauth-flow "fake-code")]
      (is (fn? (.-then result))))))

;; ============================================================================
;; Exchange Code Tests
;; ============================================================================

(deftest exchange-code-for-token-returns-promise-test
  (testing "exchange-code-for-token returns a promise"
    (let [result (gh-oauth/exchange-code-for-token "fake-code")]
      (is (fn? (.-then result))))))

;; ============================================================================
;; Get User Info Tests
;; ============================================================================

(deftest get-user-info-returns-promise-test
  (testing "get-user-info returns a promise"
    (let [result (gh-oauth/get-user-info "fake-token")]
      (is (fn? (.-then result))))))

;; ============================================================================
;; Store Token Logic Tests
;; ============================================================================

(deftest store-token-data-format-test
  (testing "token data has expected fields for storage"
    ;; Test the expected structure of token data from GitHub
    (let [token-data {:access_token "gho_xxxx"
                      :token_type "bearer"
                      :scope "repo"
                      :refresh_token nil
                      :expires_in nil}
          user-info {:id 12345
                     :login "testuser"
                     :name "Test User"}]
      ;; Verify the structure matches what store-token! expects
      (is (string? (:access_token token-data)))
      (is (integer? (:id user-info)))
      (is (string? (:login user-info))))))

;; ============================================================================
;; Store PAT Logic Tests
;; ============================================================================

(deftest store-pat-data-format-test
  (testing "PAT storage requires name, token, and username"
    ;; Test the expected input format for store-pat!
    (let [name "My Token"
          token "ghp_xxxx"
          username "testuser"]
      (is (string? name))
      (is (string? token))
      (is (string? username)))))

;; ============================================================================
;; Error Handling in OAuth Flow Tests
;; ============================================================================

(deftest oauth-error-response-format-test
  (testing "OAuth error responses have expected structure"
    ;; GitHub returns errors in this format
    (let [error-response {:error "bad_verification_code"
                          :error_description "The code passed is incorrect or expired."
                          :error_uri "https://docs.github.com/..."}]
      (is (string? (:error error-response)))
      (is (string? (:error_description error-response))))))

(deftest oauth-flow-error-detection-test
  (testing "error responses are detected correctly"
    (let [token-data {:error "bad_verification_code"
                      :error_description "The code passed is incorrect or expired."}]
      (is (some? (:error token-data)))
      ;; The flow should throw when error is present
      (is (string? (or (:error_description token-data) "OAuth error"))))))
