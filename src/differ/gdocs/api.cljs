(ns differ.gdocs.api
  "Thin REST wrappers over the Google Docs API v1, plus id/URL parsing and the
   pure location/range request helpers.

   Documents are returned as string-keyed Clojure maps (js->clj without
   keywordizing) so downstream code reads the Docs API's camelCase keys
   directly. Request builders, by contrast, use camelCase *keywords* — clj->js
   preserves their case on the way out to JSON."
  (:require [clojure.string :as str]))

(def ^:private docs-base "https://docs.googleapis.com/v1/documents")
(def ^:private timeout-ms 60000)

(defn extract-doc-id
  "Accept a full /d/<id>/ URL or a bare document id."
  [value]
  (if-let [[_ id] (re-find #"/d/([a-zA-Z0-9_-]+)" value)]
    id
    (str/trim value)))

(defn location
  "Docs API Location: an index, optionally scoped to a tab."
  [index tab-id]
  (cond-> {:index index}
    tab-id (assoc :tabId tab-id)))

(defn text-range
  "Docs API Range: [start end), optionally scoped to a tab."
  [start end tab-id]
  (cond-> {:startIndex start :endIndex end}
    tab-id (assoc :tabId tab-id)))

(defn- auth-headers [token]
  #js {"Authorization" (str "Bearer " token)
       "Content-Type" "application/json"})

(defn- handle-json-response
  "Resolve to parsed JSON (string-keyed) on 2xx, otherwise reject with the Docs
   API's error body text included in the message (like the Python CLI printed)."
  [^js response label]
  (if (.-ok response)
    (.then (.json response) (fn [json] (js->clj json)))
    (.then (.text response)
           (fn [body]
             (throw (ex-info (str "gdocs API error (" label "): "
                                  (.-status response) " " body)
                             {:status (.-status response) :body body}))))))

(defn- fetch-json
  "fetch + JSON handling with a hard timeout. Uses an AbortController so the
   timer is always cleared and the underlying request is actually cancelled on
   timeout, rather than leaving a dangling timer/socket that keeps the Node
   event loop alive."
  [url ^js js-opts label]
  (let [controller (js/AbortController.)
        timer (js/setTimeout #(.abort controller) timeout-ms)]
    (js/Reflect.set js-opts "signal" (.-signal controller))
    (-> (js/fetch url js-opts)
        (.then #(handle-json-response % label))
        (.catch (fn [e]
                  ;; fetch rejects with an AbortError when we time it out;
                  ;; translate that into a clear timeout message.
                  (if (= (.-name e) "AbortError")
                    (throw (ex-info (str "gdocs: " label " timed out after " timeout-ms "ms")
                                    {:type :timeout}))
                    (throw e))))
        (.finally (fn [] (js/clearTimeout timer))))))

(defn get-document
  "Fetch a document, including all tab content by default. Returns a Promise of
   the string-keyed document map."
  ([token doc-id] (get-document token doc-id true))
  ([token doc-id include-tabs?]
   (let [url (str docs-base "/" doc-id
                  (when include-tabs? "?includeTabsContent=true"))]
     (fetch-json url #js {:method "GET" :headers (auth-headers token)} "get_document"))))

(defn batch-update
  "POST a batchUpdate. Returns a Promise of the string-keyed response map."
  [token doc-id requests-list]
  (fetch-json (str docs-base "/" doc-id ":batchUpdate")
              #js {:method "POST"
                   :headers (auth-headers token)
                   :body (js/JSON.stringify (clj->js {:requests requests-list}))}
              "batch_update"))
