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

(defn- check-rate-limit!
  "Throw if rate limited."
  []
  (let [{:keys [remaining reset-at]} @rate-limit-state]
    (when (and (zero? remaining) reset-at (< (js/Date.now) reset-at))
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

(defn paginate-graphql
  "Fetch all pages of a paginated GraphQL query.
   Args:
     token - GitHub access token
     query - GraphQL query string with $cursor variable
     params - Query parameters (without cursor)
     path-to-connection - Vector path to the connection object (e.g., [:repository :pullRequest :files])
   Returns a Promise resolving to all accumulated nodes."
  [token query params path-to-connection]
  (let [fetch-page (fn fetch-page [cursor accumulated]
                     (-> (graphql-request token query (assoc params :cursor cursor))
                         (.then (fn [data]
                                  (let [connection (get-in data path-to-connection)
                                        nodes (:nodes connection)
                                        page-info (:pageInfo connection)
                                        all-nodes (into accumulated nodes)]
                                    (if (:hasNextPage page-info)
                                      (fetch-page (:endCursor page-info) all-nodes)
                                      all-nodes))))))]
    (fetch-page nil [])))

(defn rest-request
  "Make a REST API request to GitHub.
   Returns a Promise resolving to the response text or JSON based on Accept header."
  [token url opts]
  (let [accept (or (:accept opts) "application/vnd.github.v3+json")
        return-text? (str/includes? accept ".diff")]
    (-> (js/fetch url
                  #js {:method (or (:method opts) "GET")
                       :headers #js {"Authorization" (str "Bearer " token)
                                     "Accept" accept}})
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
