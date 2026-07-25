(ns differ.gdocs.core
  "High-level Google Docs operations backing the gdocs_* MCP tools:
   info / read / write. Each returns a Promise of a plain result map (kebab
   keys) that differ.mcp serializes to JSON.

   This is the ClojureScript equivalent of the Python `gdocs` CLI's three
   subcommands, sharing the same markdown<->Docs mapping and write strategy."
  (:require [clojure.string :as str]
            [differ.gdocs.auth :as auth]
            [differ.gdocs.api :as api]
            [differ.gdocs.render :as render]
            [differ.gdocs.parse :as parse]
            [differ.gdocs.tabs :as tabs]
            [differ.gdocs.writer :as writer]))

(defn creds-path
  "Path to the service-account key file: GDOCS_CREDS env var or the default."
  []
  (let [env (aget js/process.env "GDOCS_CREDS")]
    (if (and env (not= env "")) env (auth/default-creds-path))))

(defn- with-token
  "Resolve a bearer token, then call (f token). Returns f's Promise."
  [f]
  (-> (auth/get-access-token (creds-path))
      (.then f)))

(defn- select-tab-or-throw
  "Resolve a tab selector against a document, or throw a helpful error listing
   the available tabs."
  [document selector]
  (or (tabs/resolve-tab document selector)
      (let [titles (str/join ", " (map :title (tabs/flatten-tabs document)))]
        (throw (ex-info (str "gdocs: no tab matching '" selector
                             "'. Available tabs: " titles)
                        {:type :tab-not-found :selector selector})))))

(defn info
  "Return {:title :document-id :tab-count :tabs [{:title :tab-id :nesting :words}]}."
  [doc]
  (with-token
    (fn [token]
      (-> (api/get-document token (api/extract-doc-id doc))
          (.then (fn [document]
                   (let [tab-list (tabs/flatten-tabs document)]
                     {:title (or (get document "title") "Untitled")
                      :document-id (get document "documentId" "")
                      :tab-count (count tab-list)
                      :tabs (mapv (fn [tab]
                                    {:title (:title tab)
                                     :tab-id (:tab-id tab)
                                     :nesting (:nesting tab)
                                     :words (tabs/word-count (:body tab))})
                                  tab-list)})))))))

(defn read-doc
  "Return {:markdown ...}. With a tab selector, render just that tab; otherwise
   render the whole document (each tab prefixed by a heading)."
  [doc tab]
  (with-token
    (fn [token]
      (-> (api/get-document token (api/extract-doc-id doc))
          (.then (fn [document]
                   (if (seq tab)
                     (let [selected (select-tab-or-throw document tab)]
                       {:markdown (render/render-body (:body selected))
                        :tab (:title selected)})
                     {:markdown
                      (str/join
                       "\n"
                       (mapcat (fn [t]
                                 [(str (apply str (repeat (inc (:nesting t)) "#"))
                                       " " (:title t) "\n")
                                  (render/render-body (:body t))])
                               (tabs/flatten-tabs document)))})))))))

(defn write-doc
  "Write markdown into a tab. opts: {:tab :markdown :append? :dry-run?}.
   Returns the writer result map, annotated with :tab and :mode."
  [doc {:keys [tab markdown append? dry-run?]}]
  ;; `tab` is schema-required, but the MCP validator only rejects nil, so a
  ;; blank string slips through. Refuse it here: an empty selector could
  ;; otherwise resolve to the implicit whole-document tab and clobber content
  ;; the caller never named.
  (when (str/blank? tab)
    (throw (ex-info "gdocs_write requires a non-blank `tab` (title or tabId)."
                    {:type :invalid-tab})))
  (let [blocks (parse/parse-markdown markdown)]
    (with-token
      (fn [token]
        (let [doc-id (api/extract-doc-id doc)]
          (-> (api/get-document token doc-id)
              (.then (fn [document]
                       (let [selected (select-tab-or-throw document tab)]
                         (-> (writer/write-tab token doc-id selected blocks
                                               {:append? append? :dry-run? dry-run?})
                             (.then (fn [result]
                                      (assoc result
                                             :tab (:title selected)
                                             :mode (if append? "append" "replace"))))))))))))))
