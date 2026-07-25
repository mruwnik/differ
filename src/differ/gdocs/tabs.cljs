(ns differ.gdocs.tabs
  "Tab discovery, resolution, and body helpers for Google Docs documents.

   With includeTabsContent=true the Docs API returns a `tabs` array; each tab
   has tabProperties (tabId/title/index) plus a documentTab.body, and may nest
   childTabs. Older documents have no tabs at all, in which case the top-level
   `body` acts as a single implicit tab (tabId=nil).

   Documents come off the wire as string-keyed maps (js->clj without
   keywordizing), so every access here uses the Docs API's camelCase string
   keys directly."
  (:require [clojure.string :as str]))

(defn- collect-tabs
  "Depth-first flatten of a Docs `tabs`/`childTabs` array into differ's
   kebab-keyword tab maps."
  [tab-list depth]
  (mapcat (fn [tab]
            (let [props (get tab "tabProperties" {})]
              ;; `or`, not a get-default: the Docs API can send an explicit
              ;; null title, and a nil title would later crash str/lower-case
              ;; in resolve-tab / word-count.
              (cons {:tab-id (get props "tabId")
                     :title (or (get props "title") "Untitled")
                     :nesting depth
                     :body (get-in tab ["documentTab" "body"] {})}
                    (collect-tabs (get tab "childTabs" []) (inc depth)))))
          tab-list))

(defn flatten-tabs
  "Return a flat, depth-annotated list of every tab in the document. A document
   with no tabs is treated as a single implicit tab (tab-id nil) wrapping the
   top-level body."
  [document]
  (let [tabs (get document "tabs")]
    (if (empty? tabs)
      [{:tab-id nil
        :title (or (get document "title") "Untitled")
        :nesting 0
        :body (get document "body" {})}]
      (vec (collect-tabs tabs 0)))))

(defn resolve-tab
  "Find a tab by exact tabId first, then case-insensitive title. nil if no match."
  [document selector]
  (let [tabs (flatten-tabs document)]
    (or (first (filter #(= (:tab-id %) selector) tabs))
        (let [lowered (str/lower-case selector)]
          (first (filter #(= (str/lower-case (:title %)) lowered) tabs))))))

(defn tab-body
  "Body segment for a tab-id (nil selects the top-level body)."
  [document tab-id]
  (if (nil? tab-id)
    (get document "body" {})
    (or (some (fn [tab] (when (= (:tab-id tab) tab-id) (:body tab)))
              (flatten-tabs document))
        {})))

(defn body-end-index
  "Index just past the last character of the body segment."
  [body]
  (let [content (get body "content" [])]
    (if (empty? content)
      2
      (get (last content) "endIndex" 2))))

(defn plain-text
  "Flatten all textRun content in a body into a single string."
  [body]
  (->> (get body "content" [])
       (mapcat (fn [element]
                 (when-let [paragraph (get element "paragraph")]
                   (keep (fn [run]
                           (when-let [text-run (get run "textRun")]
                             (get text-run "content" "")))
                         (get paragraph "elements" [])))))
       (str/join)))

(defn word-count
  "Rough word count for a body segment."
  [body]
  (->> (str/split (plain-text body) #"\s+")
       (remove str/blank?)
       count))
