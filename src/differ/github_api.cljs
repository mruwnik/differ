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
   Includes rate limit checking and tracking."
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
                   (throw (ex-info (str "GitHub API error: " (.-status response))
                                   {:status (.-status response)})))))
        (.then (fn [json]
                 (let [data (js->clj json :keywordize-keys true)]
                   (if-let [errors (:errors data)]
                     (throw (ex-info (str "GraphQL error: " (-> errors first :message))
                                     {:errors errors}))
                     (:data data)))))
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
