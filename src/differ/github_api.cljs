(ns differ.github-api
  "Shared GitHub GraphQL API utilities.

   Provides common functionality for making authenticated GitHub API requests,
   including rate limit tracking and error handling."
  (:require [clojure.string :as str]))

;; GraphQL endpoint
(def graphql-endpoint "https://api.github.com/graphql")

;; Rate limit tracking
(defonce rate-limit-state (atom {:remaining 5000
                                 :reset-at nil}))

(defn update-rate-limit!
  "Update rate limit state from response headers."
  [headers]
  (when-let [h headers]
    (let [remaining (js/parseInt (or (.get h "x-ratelimit-remaining") "5000") 10)
          reset-at (js/parseInt (or (.get h "x-ratelimit-reset") "0") 10)]
      (swap! rate-limit-state assoc
             :remaining remaining
             :reset-at (* reset-at 1000)))))

(defn- try-consume-rate-limit!
  "Atomically check and consume one rate limit slot.
   Returns true if request can proceed, false if rate limited.
   Uses swap! returning the decision in state to avoid race conditions."
  []
  (let [result (swap! rate-limit-state
                      (fn [{:keys [remaining reset-at] :as state}]
                        (if (and (zero? remaining) reset-at (< (js/Date.now) reset-at))
                          (assoc state :allowed false)
                          (-> state
                              (update :remaining dec)
                              (assoc :allowed true)))))]
    (:allowed result)))

(defn- check-rate-limit!
  "Throw if rate limited. Uses atomic check-and-consume to prevent races."
  []
  (when-not (try-consume-rate-limit!)
    (let [{:keys [reset-at]} @rate-limit-state]
      (throw (ex-info "GitHub API rate limit exceeded"
                      {:type :rate-limit
                       :reset-at reset-at
                       :retry-after (- reset-at (js/Date.now))})))))

(defn graphql-request
  "Make GraphQL request to GitHub API.
   Always returns a Promise (rejected on error) for consistent async handling.
   Includes rate limit checking and tracking.

   On non-OK HTTP responses the rejected error's message includes BOTH the
   status code and the response body text, so callers (e.g.
   `try-tokens-with-fallback`) can distinguish rate-limit 403s from
   permission-denied 403s by matching on body text."
  [token query variables]
  (try
    (check-rate-limit!)
    (-> (js/fetch graphql-endpoint
                  #js {:method "POST"
                       :headers #js {"Authorization" (str "Bearer " token)
                                     "Content-Type" "application/json"}
                       :body (js/JSON.stringify
                              #js {:query query
                                   :variables (clj->js variables)})})
        (.then (fn [response]
                 (update-rate-limit! (.-headers response))
                 (if (.-ok response)
                   (.json response)
                   ;; Read the body as text before throwing so the error
                   ;; message can include GitHub's explanation (rate limit
                   ;; vs. permission vs. other 403s are distinguishable
                   ;; only in the body, not the status code).
                   (-> (.text response)
                       (.then (fn [body]
                                (throw (ex-info (str "GitHub API error: "
                                                     (.-status response)
                                                     " " body)
                                                {:status (.-status response)
                                                 :body body}))))))))
        (.then (fn [json]
                 (let [data (js->clj json :keywordize-keys true)
                       errors (:errors data)
                       payload (:data data)]
                   (cond
                     ;; GitHub returns partial data + errors in the same
                     ;; response sometimes; treat that as an error rather
                     ;; than silently returning the partial payload.
                     (seq errors)
                     (throw (ex-info (str "GraphQL error: " (-> errors first :message))
                                     {:errors errors}))

                     (nil? payload)
                     (throw (ex-info "GraphQL response missing :data"
                                     {:response data}))

                     :else payload))))
        (.catch (fn [err]
                  (throw (ex-info (str "GitHub request failed: " (or (.-message err) (ex-message err) (str err)))
                                  {:original-error err})))))
    (catch :default e
      (js/Promise.reject e))))

(def ^:private default-max-items
  "Default maximum number of items to fetch during pagination to prevent OOM."
  5000)

(defn paginate-graphql
  "Fetch all pages of a paginated GraphQL query.
   Args:
     token - GitHub access token
     query - GraphQL query string with $cursor variable
     params - Query parameters (without cursor)
     path-to-connection - Vector path to the connection object (e.g., [:repository :pullRequest :files])
     opts - Optional map with :max-items (default 5000) to limit total items fetched
   Returns a Promise resolving to a vector of nodes with metadata:
     {:truncated boolean} - true if max-items limit was reached
   Callers can use (meta result) to check truncation status."
  ([token query params path-to-connection]
   (paginate-graphql token query params path-to-connection {}))
  ([token query params path-to-connection opts]
   (let [max-items (or (:max-items opts) default-max-items)
         fetch-page (fn fetch-page [cursor accumulated]
                      (-> (graphql-request token query (assoc params :cursor cursor))
                          (.then (fn [data]
                                   (let [connection (get-in data path-to-connection)
                                         nodes (:nodes connection)
                                         page-info (:pageInfo connection)
                                         all-nodes (into accumulated nodes)]
                                     (if (>= (count all-nodes) max-items)
                                       (do
                                         (js/console.warn
                                          (str "Pagination limit reached: " (count all-nodes)
                                               " items (max: " max-items "). "
                                               "Results may be incomplete."))
                                         (with-meta (vec all-nodes) {:truncated true}))
                                       (if (:hasNextPage page-info)
                                         (fetch-page (:endCursor page-info) all-nodes)
                                         (with-meta (vec all-nodes) {:truncated false}))))))))]
     (fetch-page nil []))))

(defn rest-request
  "Make a REST API request to GitHub.
   Returns a Promise resolving to the response text or JSON based on Accept header.
   Supports :method, :accept, and :body options."
  [token url opts]
  (let [accept (or (:accept opts) "application/vnd.github.v3+json")
        return-text? (str/includes? accept ".diff")
        fetch-opts (cond-> {:method (or (:method opts) "GET")
                            :headers {"Authorization" (str "Bearer " token)
                                      "Accept" accept
                                      "Content-Type" "application/json"}}
                     (:body opts) (assoc :body (js/JSON.stringify (clj->js (:body opts)))))]
    (-> (js/fetch url (clj->js fetch-opts))
        (.then (fn [response]
                 (update-rate-limit! (.-headers response))
                 (if (.-ok response)
                   (if return-text?
                     (.text response)
                     (.json response))
                   (throw (ex-info (str "GitHub API error: " (.-status response))
                                   {:status (.-status response)})))))
        (.catch (fn [err]
                  (throw (ex-info (str "GitHub request failed: " (or (.-message err) (ex-message err) (str err)))
                                  {:original-error err})))))))

;; ============================================================================
;; Pull request listing
;; ============================================================================

(defn state->states
  "Map user-facing state string to GraphQL PullRequestState enum values."
  [state]
  (case state
    "closed" ["CLOSED" "MERGED"]
    "all"    ["OPEN" "CLOSED" "MERGED"]
    ["OPEN"]))

(def ^:private list-prs-query
  "query ListPRs($owner: String!, $repo: String!, $first: Int!, $states: [PullRequestState!]) {
     repository(owner: $owner, name: $repo) {
       pullRequests(first: $first, states: $states,
                    orderBy: {field: UPDATED_AT, direction: DESC}) {
         totalCount
         pageInfo { hasNextPage }
         nodes {
           number
           title
           isDraft
           author { login }
           baseRefName
           headRefName
           updatedAt
           url
         }
       }
     }
   }")

(defn- clamp-limit [n]
  (cond
    (nil? n)   30
    (< n 1)    1
    (> n 100)  100
    :else      n))

(defn- normalize-pr-node
  "Convert a GraphQL PR node into differ's flat PR shape."
  [node]
  {:number       (:number node)
   :title        (:title node)
   :author       (get-in node [:author :login])
   :draft        (:isDraft node)
   :base-branch  (:baseRefName node)
   :head-branch  (:headRefName node)
   :updated-at   (:updatedAt node)
   :url          (:url node)})

(defn list-pull-requests
  "Fetch PRs for a GitHub repo via GraphQL.
   opts: {:state \"open\"|\"closed\"|\"all\" :limit int}
   Returns a Promise of {:prs [...] :truncated bool}."
  [token owner repo {:keys [state limit]}]
  (let [variables {:owner  owner
                   :repo   repo
                   :first  (clamp-limit limit)
                   :states (state->states state)}]
    (-> (graphql-request token list-prs-query variables)
        (.then (fn [data]
                 (let [conn (get-in data [:repository :pullRequests])]
                   {:prs       (mapv normalize-pr-node (:nodes conn))
                    :truncated (boolean (get-in conn [:pageInfo :hasNextPage]))}))))))

;; ============================================================================
;; Poller-specific queries
;; ============================================================================

(def ^:private list-prs-for-poller-query
  "query ListPRsForPoller($owner: String!, $repo: String!, $first: Int!) {
     repository(owner: $owner, name: $repo) {
       pullRequests(first: $first, states: [OPEN],
                    orderBy: {field: UPDATED_AT, direction: DESC}) {
         totalCount
         pageInfo { hasNextPage }
         nodes {
           number
           title
           isDraft
           author { login }
           baseRefName
           headRefName
           headRefOid
           updatedAt
           url
           comments { totalCount }
           reviews { totalCount }
           reviewThreads(first: 100) { nodes { isResolved } }
           commits(last: 1) {
             nodes { commit { statusCheckRollup { state } } }
           }
         }
       }
     }
   }")

(defn- normalize-checks-state
  "Map GitHub statusCheckRollup.state to a lowercase keyword, or nil if absent."
  [raw]
  (when raw
    (keyword (str/lower-case raw))))

(defn- count-unresolved-threads
  "Count review threads with :isResolved false.

   IMPORTANT: capped at 100 by the GraphQL query (reviewThreads(first: 100)).
   A PR with 150 unresolved threads is reported as 100; the poller will
   not fire `:pr-feedback-changed` events until the true count drops
   below 100. We deliberately do not paginate here because the extra cost
   is substantial for typical repos and the 100-thread ceiling is a fine
   approximation in practice. Agents consuming `:unresolved-count` should
   treat it as a best-effort coarse count, not an exact value."
  [threads-nodes]
  (count (filter #(false? (:isResolved %)) threads-nodes)))

(defn- normalize-pr-for-poller
  "Convert a GraphQL node (from list-prs-for-poller-query) to the flat shape
   the poller uses for caching and event emission."
  [node]
  (let [rollup (get-in node [:commits :nodes 0 :commit :statusCheckRollup])]
    {:number           (:number node)
     :title            (:title node)
     :author           (get-in node [:author :login])
     :draft            (:isDraft node)
     :base-branch      (:baseRefName node)
     :head-branch      (:headRefName node)
     :head-sha         (:headRefOid node)
     :updated-at       (:updatedAt node)
     :url              (:url node)
     :unresolved-count (count-unresolved-threads (get-in node [:reviewThreads :nodes] []))
     :review-count     (get-in node [:reviews :totalCount] 0)
     ;; PR-level conversation comments (issue comments). Distinct from
     ;; review-thread comments — these are top-level PR discussion.
     ;; Tracking this lets `pr-feedback-changed` fire when an agent
     ;; adds a comment via differ's `add_comment` MCP handler against
     ;; a github session, which posts an issue comment via the GitHub
     ;; API but does NOT touch reviewThreads.
     :comment-count    (get-in node [:comments :totalCount] 0)
     :checks-status    (normalize-checks-state (:state rollup))}))

(defn list-prs-for-poller
  "Fetch open PRs for a repo with the extra fields needed by the event poller
   (head SHA, unresolved thread count, review count, checks rollup state).
   Separate from list-pull-requests because normal callers don't want to pay
   for the extra GraphQL selections.
   Returns a Promise of {:prs [normalized] :truncated bool}."
  [token owner repo {:keys [limit]}]
  (let [variables {:owner owner :repo repo :first (clamp-limit limit)}]
    (-> (graphql-request token list-prs-for-poller-query variables)
        (.then (fn [data]
                 (let [conn (get-in data [:repository :pullRequests])]
                   {:prs       (mapv normalize-pr-for-poller (:nodes conn))
                    :truncated (boolean (get-in conn [:pageInfo :hasNextPage]))}))))))

(def ^:private get-pr-merged-query
  "query GetPRMerged($owner: String!, $repo: String!, $number: Int!) {
     repository(owner: $owner, name: $repo) {
       pullRequest(number: $number) { merged }
     }
   }")

(defn get-pr-merge-status
  "Returns a Promise of boolean: true if the PR is merged, false otherwise.
   Never rejects — resolves to false on any error. The poller uses this to
   decide whether a disappeared PR produced a merged-close or a regular close."
  [token owner repo pr-number]
  (-> (graphql-request token get-pr-merged-query
                       {:owner owner :repo repo :number pr-number})
      (.then (fn [data]
               (boolean (get-in data [:repository :pullRequest :merged]))))
      (.catch (fn [_err] false))))
