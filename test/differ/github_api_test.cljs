(ns differ.github-api-test
  "Tests for GitHub API utilities."
  (:require [clojure.test :refer [deftest testing is use-fixtures async]]
            [differ.github-api :as gh-api]))

;; ============================================================================
;; Test Setup - Reset rate limit state between tests
;; ============================================================================

(def reset-rate-limit-state
  ;; Map-form fixture (required for async tests in cljs.test).
  {:before (fn [] (reset! gh-api/rate-limit-state {:remaining 5000 :reset-at nil}))
   :after  (fn [] nil)})

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
      (is (fn? (.-then result)))
      ;; The fake token will cause an auth failure — swallow the rejection
      ;; so it doesn't become an unhandled rejection that crashes the runner.
      (.catch result (fn [_])))))

(deftest rest-request-returns-promise-test
  (testing "rest-request always returns a promise"
    (let [result (gh-api/rest-request "token" "https://api.github.com/repos" {})]
      ;; Should be a promise
      (is (fn? (.-then result)))
      (.catch result (fn [_])))))

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
      (is (fn? (.-then result)))
      (.catch result (fn [_])))))

;; ============================================================================
;; list-pull-requests tests
;; ============================================================================

(deftest state->states-test
  (testing "maps 'open' to [OPEN]"
    (is (= ["OPEN"] (gh-api/state->states "open"))))

  (testing "maps 'closed' to [CLOSED MERGED]"
    (is (= ["CLOSED" "MERGED"] (gh-api/state->states "closed"))))

  (testing "maps 'all' to [OPEN CLOSED MERGED]"
    (is (= ["OPEN" "CLOSED" "MERGED"] (gh-api/state->states "all"))))

  (testing "defaults to [OPEN] for unknown input"
    (is (= ["OPEN"] (gh-api/state->states "bogus"))))

  (testing "defaults to [OPEN] for nil"
    (is (= ["OPEN"] (gh-api/state->states nil)))))

(deftest list-pull-requests-clamps-limit-test
  (testing "limit over 100 is clamped to 100"
    (let [captured-variables (atom nil)]
      (with-redefs [gh-api/graphql-request
                    (fn [_token _query variables]
                      (reset! captured-variables variables)
                      (js/Promise.resolve
                       {:repository
                        {:pullRequests
                         {:totalCount 0
                          :pageInfo {:hasNextPage false}
                          :nodes []}}}))]
        (-> (gh-api/list-pull-requests "tok" "owner" "repo" {:state "open" :limit 500})
            (.then (fn [_]
                     (is (= 100 (:first @captured-variables))))))))))

(deftest list-pull-requests-default-limit-test
  (testing "missing limit defaults to 30"
    (let [captured-variables (atom nil)]
      (with-redefs [gh-api/graphql-request
                    (fn [_token _query variables]
                      (reset! captured-variables variables)
                      (js/Promise.resolve
                       {:repository
                        {:pullRequests
                         {:totalCount 0
                          :pageInfo {:hasNextPage false}
                          :nodes []}}}))]
        (-> (gh-api/list-pull-requests "tok" "owner" "repo" {:state "open"})
            (.then (fn [_]
                     (is (= 30 (:first @captured-variables))))))))))

(deftest list-pull-requests-normalizes-nodes-test
  (testing "converts GraphQL nodes into flat PR maps"
    (with-redefs [gh-api/graphql-request
                  (fn [_token _query _variables]
                    (js/Promise.resolve
                     {:repository
                      {:pullRequests
                       {:totalCount 2
                        :pageInfo {:hasNextPage true}
                        :nodes [{:number 42
                                 :title "Add auth"
                                 :isDraft false
                                 :author {:login "dan"}
                                 :baseRefName "main"
                                 :headRefName "feature/auth"
                                 :updatedAt "2026-04-05T14:23:00Z"
                                 :url "https://github.com/owner/repo/pull/42"}
                                {:number 43
                                 :title "Fix bug"
                                 :isDraft true
                                 :author nil
                                 :baseRefName "main"
                                 :headRefName "fix/bug"
                                 :updatedAt "2026-04-06T10:00:00Z"
                                 :url "https://github.com/owner/repo/pull/43"}]}}}))]
      (-> (gh-api/list-pull-requests "tok" "owner" "repo" {:state "open" :limit 30})
          (.then (fn [result]
                   (is (true? (:truncated result)))
                   (is (= 2 (count (:prs result))))
                   (let [pr (first (:prs result))]
                     (is (= 42 (:number pr)))
                     (is (= "Add auth" (:title pr)))
                     (is (= "dan" (:author pr)))
                     (is (false? (:draft pr)))
                     (is (= "main" (:base-branch pr)))
                     (is (= "feature/auth" (:head-branch pr))))
                   (let [pr2 (second (:prs result))]
                     (is (nil? (:author pr2))
                         "deleted author should produce nil"))))))))

;; ============================================================================
;; list-prs-for-poller tests
;; ============================================================================

(deftest list-prs-for-poller-normalizes-nodes-test
  (testing "returns flat PR maps with head-sha, counts, and checks-status"
    (async done
           (with-redefs [gh-api/graphql-request
                         (fn [_token _query _variables]
                           (js/Promise.resolve
                            {:repository
                             {:pullRequests
                              {:totalCount 1
                               :pageInfo {:hasNextPage false}
                               :nodes
                               [{:number 42
                                 :title "Add widget"
                                 :isDraft false
                                 :author {:login "octocat"}
                                 :baseRefName "main"
                                 :headRefName "feature/widget"
                                 :headRefOid "abc123"
                                 :updatedAt "2026-04-08T10:00:00Z"
                                 :url "https://github.com/owner/repo/pull/42"
                                 :comments {:totalCount 3}
                                 :reviews {:totalCount 1}
                                 :reviewThreads {:nodes [{:isResolved false}
                                                         {:isResolved false}
                                                         {:isResolved true}]}
                                 :commits {:nodes [{:commit {:statusCheckRollup {:state "FAILURE"}}}]}}]}}}))]
             (-> (gh-api/list-prs-for-poller "tok" "owner" "repo" {:limit 30})
                 (.then (fn [result]
                          (is (false? (:truncated result)))
                          (let [pr (first (:prs result))]
                            (is (= 42 (:number pr)))
                            (is (= "abc123" (:head-sha pr)))
                            (is (= 2 (:unresolved-count pr)))
                            (is (= 1 (:review-count pr)))
                            (is (= 3 (:comment-count pr))
                                "PR-level conversation comments come from comments.totalCount")
                            (is (= :failure (:checks-status pr)))
                            (is (= "octocat" (:author pr)))
                            (is (= "feature/widget" (:head-branch pr))))
                          (done))))))))

(deftest list-prs-for-poller-handles-missing-rollup-test
  (testing "nil statusCheckRollup yields nil checks-status"
    (async done
           (with-redefs [gh-api/graphql-request
                         (fn [_token _query _variables]
                           (js/Promise.resolve
                            {:repository
                             {:pullRequests
                              {:totalCount 1
                               :pageInfo {:hasNextPage false}
                               :nodes
                               [{:number 1 :title "T" :isDraft false
                                 :author {:login "x"} :baseRefName "main"
                                 :headRefName "f" :headRefOid "sha"
                                 :updatedAt "2026-04-08T10:00:00Z"
                                 :url "u"
                                 :comments {:totalCount 0}
                                 :reviews {:totalCount 0}
                                 :reviewThreads {:nodes []}
                                 :commits {:nodes [{:commit {:statusCheckRollup nil}}]}}]}}}))]
             (-> (gh-api/list-prs-for-poller "tok" "owner" "repo" {})
                 (.then (fn [result]
                          (let [pr (first (:prs result))]
                            (is (nil? (:checks-status pr)))
                            (is (= 0 (:unresolved-count pr)))
                            (is (= 0 (:review-count pr)))
                            (is (= 0 (:comment-count pr))))
                          (done))))))))

(deftest list-prs-for-poller-clamps-limit-test
  (testing "limit over 100 is clamped to 100"
    (async done
           (let [captured (atom nil)]
             (with-redefs [gh-api/graphql-request
                           (fn [_token _query variables]
                             (reset! captured variables)
                             (js/Promise.resolve
                              {:repository
                               {:pullRequests
                                {:totalCount 0 :pageInfo {:hasNextPage false} :nodes []}}}))]
               (-> (gh-api/list-prs-for-poller "tok" "o" "r" {:limit 500})
                   (.then (fn [_]
                            (is (= 100 (:first @captured)))
                            (done)))))))))

;; ============================================================================
;; get-pr-merge-status tests
;; ============================================================================

(deftest get-pr-merge-status-returns-true-when-merged-test
  (testing "returns true when merged"
    (async done
           (with-redefs [gh-api/graphql-request
                         (fn [_tok _query _vars]
                           (js/Promise.resolve
                            {:repository {:pullRequest {:merged true}}}))]
             (-> (gh-api/get-pr-merge-status "tok" "owner" "repo" 42)
                 (.then (fn [result]
                          (is (true? result))
                          (done))))))))

(deftest get-pr-merge-status-returns-false-when-closed-test
  (testing "returns false when closed without merge"
    (async done
           (with-redefs [gh-api/graphql-request
                         (fn [_tok _query _vars]
                           (js/Promise.resolve
                            {:repository {:pullRequest {:merged false}}}))]
             (-> (gh-api/get-pr-merge-status "tok" "owner" "repo" 42)
                 (.then (fn [result]
                          (is (false? result))
                          (done))))))))

(deftest get-pr-merge-status-falls-back-to-false-on-error-test
  (testing "resolves to false if the GraphQL call throws"
    (async done
           (with-redefs [gh-api/graphql-request
                         (fn [_tok _query _vars]
                           (js/Promise.reject (js/Error. "boom")))]
             (-> (gh-api/get-pr-merge-status "tok" "owner" "repo" 42)
                 (.then (fn [result]
                          (is (false? result))
                          (done))))))))
