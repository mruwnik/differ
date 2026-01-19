(ns differ.github-api-test
  "Tests for GitHub API utilities."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [differ.github-api :as gh-api]))

;; ============================================================================
;; Test Setup - Reset rate limit state between tests
;; ============================================================================

(defn reset-rate-limit-state [f]
  ;; Reset to default values before each test
  (reset! gh-api/rate-limit-state {:remaining 5000 :reset-at nil})
  (f))

(use-fixtures :each reset-rate-limit-state)

;; ============================================================================
;; Constants Tests
;; ============================================================================

(deftest graphql-endpoint-test
  (testing "endpoint is correct GitHub GraphQL URL"
    (is (= "https://api.github.com/graphql" gh-api/graphql-endpoint))))

;; ============================================================================
;; Rate Limit State Tests
;; ============================================================================

(deftest initial-rate-limit-state-test
  (testing "rate limit state starts with default values"
    (let [state @gh-api/rate-limit-state]
      (is (= 5000 (:remaining state)))
      (is (nil? (:reset-at state))))))

(deftest update-rate-limit!-test
  (testing "updates rate limit from response headers"
    (let [mock-headers #js {"get" (fn [key]
                                    (case key
                                      "x-ratelimit-remaining" "4500"
                                      "x-ratelimit-reset" "1704067200"
                                      nil))}]
      (gh-api/update-rate-limit! mock-headers)
      (let [state @gh-api/rate-limit-state]
        (is (= 4500 (:remaining state)))
        ;; Reset-at is Unix timestamp * 1000 (milliseconds)
        (is (= 1704067200000 (:reset-at state))))))

  (testing "uses defaults when headers missing"
    (let [mock-headers #js {"get" (fn [_] nil)}]
      ;; First set some non-default values
      (reset! gh-api/rate-limit-state {:remaining 100 :reset-at 12345})
      (gh-api/update-rate-limit! mock-headers)
      ;; Should reset to defaults
      (let [state @gh-api/rate-limit-state]
        (is (= 5000 (:remaining state)))
        (is (= 0 (:reset-at state))))))

  (testing "handles nil headers gracefully"
    ;; Should not throw
    (gh-api/update-rate-limit! nil)
    ;; State should be unchanged
    (let [state @gh-api/rate-limit-state]
      (is (= 5000 (:remaining state))))))

(deftest rate-limit-exhausted-test
  (testing "graphql-request rejects when rate limited"
    ;; Set up exhausted rate limit
    (reset! gh-api/rate-limit-state {:remaining 0
                                     :reset-at (+ (js/Date.now) 60000)})
    (let [result-promise (gh-api/graphql-request "fake-token" "query" {})]
      (-> result-promise
          (.catch (fn [err]
                    (is (re-find #"rate limit" (ex-message err)))))))))

(deftest rate-limit-not-exceeded-when-remaining-test
  (testing "allows requests when remaining > 0"
    (reset! gh-api/rate-limit-state {:remaining 100
                                     :reset-at (+ (js/Date.now) 60000)})
    ;; This will fail with network error (no actual GitHub), but shouldn't fail due to rate limit
    (let [result-promise (gh-api/graphql-request "fake-token" "query {}" {})]
      (-> result-promise
          (.catch (fn [err]
                    ;; Should fail for network/fetch reasons, not rate limit
                    (is (not (re-find #"rate limit" (ex-message err))))))))))

(deftest rate-limit-reset-expired-test
  (testing "allows requests when reset time has passed"
    ;; Set up exhausted rate limit but reset time in the past
    (reset! gh-api/rate-limit-state {:remaining 0
                                     :reset-at (- (js/Date.now) 1000)})
    ;; This should not throw rate limit error
    (let [result-promise (gh-api/graphql-request "fake-token" "query {}" {})]
      (-> result-promise
          (.catch (fn [err]
                    ;; Should fail for network/fetch reasons, not rate limit
                    (is (not (re-find #"rate limit" (ex-message err))))))))))

;; ============================================================================
;; Request Error Handling Tests
;; ============================================================================

(deftest graphql-request-returns-promise-test
  (testing "graphql-request always returns a promise"
    (let [result (gh-api/graphql-request "token" "query" {})]
      ;; Should be a promise (has .then method)
      (is (fn? (.-then result))))))

(deftest rest-request-returns-promise-test
  (testing "rest-request always returns a promise"
    (let [result (gh-api/rest-request "token" "https://api.github.com/repos" {})]
      ;; Should be a promise
      (is (fn? (.-then result))))))

;; ============================================================================
;; Pagination Helper Tests
;; ============================================================================

(deftest paginate-graphql-returns-promise-test
  (testing "paginate-graphql returns a promise"
    (let [result (gh-api/paginate-graphql
                  "token"
                  "query($cursor: String) { viewer { repositories(first: 10, after: $cursor) { nodes { name } pageInfo { hasNextPage endCursor } } } }"
                  {}
                  [:viewer :repositories])]
      ;; Should be a promise
      (is (fn? (.-then result))))))
