(ns differ.gdocs.writer
  "Orchestrate the write path: build batches, execute, resolve tables.

   Batch 1: delete the tab body (unless appending), insert all text, then apply
            heading/inline styles and bullets.
   Batch 2: re-fetch, insert an empty table at each sentinel paragraph.
   Batch 3: re-fetch, fill every table cell from real cell indices and delete
            the now-redundant sentinel paragraphs.

   Batches 2 and 3 only run when the markdown actually contains tables. The
   sentinel dance is required because Docs cell indices aren't predictable by
   arithmetic — they can only be read back from a real fetch."
  (:require [clojure.string :as str]
            [differ.gdocs.api :as api :refer [location text-range]]
            [differ.gdocs.build :refer [build-insertion preview-text]]
            [differ.gdocs.parse :refer [parse-inline plain-from-runs]]
            [differ.gdocs.tabs :refer [body-end-index tab-body]]))

(defn- cell-plain-text [value]
  (plain-from-runs (parse-inline value)))

(defn build-batch-one
  "Build batch 1: optional delete + insert + paragraph/text styles + bullets.
   Returns {:requests :full-text :tables}."
  [blocks tab append?]
  (let [tab-id (:tab-id tab)
        end (body-end-index (:body tab))
        base-index (if append? (max (dec end) 1) 1)
        leading-newline? (if append? (> end 2) false)
        {:keys [full-text paragraph-styles text-styles bullet-requests tables]}
        (build-insertion blocks base-index tab-id leading-newline?)
        requests (cond-> []
                   (and (not append?) (> (dec end) 1))
                   (conj {:deleteContentRange {:range (text-range 1 (dec end) tab-id)}})

                   :always
                   (conj {:insertText {:location (location base-index tab-id)
                                       :text full-text}}))
        requests (-> requests
                     (into paragraph-styles)
                     (into text-styles)
                     (into bullet-requests))]
    {:requests requests :full-text full-text :tables tables}))

(defn- paragraph-marker
  "If a paragraph element contains any of the markers, return that marker."
  [element markers]
  (when-let [paragraph (get element "paragraph")]
    (let [joined (str/join (map #(get-in % ["textRun" "content"] "")
                                (get paragraph "elements" [])))]
      (some (fn [marker] (when (str/includes? joined marker) marker)) markers))))

(defn- locate-markers
  "Map each marker to the start index of the paragraph that contains it."
  [body markers]
  (reduce (fn [found element]
            (if-let [marker (paragraph-marker element markers)]
              (assoc found marker (get element "startIndex"))
              found))
          {}
          (get body "content" [])))

(defn- insert-tables-batch
  "Batch 2: re-fetch, insert an empty table at each sentinel paragraph.
   Highest index first so earlier insertions don't shift later positions."
  [token doc-id tab-id tables]
  (-> (api/get-document token doc-id)
      (.then (fn [document]
               (let [body (tab-body document tab-id)
                     positions (locate-markers body (map :marker tables))
                     ordered (sort-by #(positions (:marker %)) > tables)
                     requests (mapv (fn [table]
                                      (let [rows (:rows table)
                                            ncols (reduce max 1 (map count rows))]
                                        {:insertTable
                                         {:location (location (positions (:marker table)) tab-id)
                                          :rows (count rows)
                                          :columns ncols}}))
                                    ordered)]
                 (api/batch-update token doc-id requests))))))

(defn- tables-with-markers
  "Pair each sentinel paragraph with the table element that precedes it."
  [body marker-set]
  (:results
   (reduce (fn [acc element]
             (if (contains? element "table")
               (assoc acc :last-table element)
               (if-let [marker (paragraph-marker element marker-set)]
                 (update acc :results conj
                         {:marker marker
                          :table (:last-table acc)
                          :start (get element "startIndex")
                          :end (get element "endIndex")})
                 acc)))
           {:results [] :last-table nil}
           (get body "content" []))))

(defn- fill-ops-for-table
  "Return [index request] insert ops that fill a real table's cells from rows."
  [table-element rows tab-id]
  (for [[r row] (map-indexed vector (get-in table-element ["table" "tableRows"] []))
        [c cell] (map-indexed vector (get row "tableCells" []))
        :let [content (get cell "content" [])]
        :when (seq content)
        :let [index (get-in content [0 "startIndex"])
              value (get-in rows [r c] "")
              text (cell-plain-text value)]
        :when (seq text)]
    [index {:insertText {:location (location index tab-id) :text text}}]))

(defn- fill-tables-batch
  "Batch 3: re-fetch, fill every cell and delete the sentinel paragraphs.
   Ops are applied in descending index order so earlier edits don't invalidate
   later indices."
  [token doc-id tab-id tables]
  (-> (api/get-document token doc-id)
      (.then (fn [document]
               (let [body (tab-body document tab-id)
                     marker-rows (into {} (map (juxt :marker :rows) tables))
                     placements (tables-with-markers body (set (keys marker-rows)))
                     ops (mapcat
                          (fn [placement]
                            (let [table-element (:table placement)]
                              (concat
                               (when table-element
                                 (fill-ops-for-table table-element
                                                     (marker-rows (:marker placement))
                                                     tab-id))
                               [[(:start placement)
                                 {:deleteContentRange
                                  {:range (text-range (:start placement) (:end placement) tab-id)}}]])))
                          placements)
                     ordered (sort-by first > ops)]
                 (api/batch-update token doc-id (mapv second ordered)))))))

(defn write-tab
  "Replace (or append to) a tab's content with rendered markdown blocks.
   Returns a Promise of a result map. When dry-run?, sends nothing."
  [token doc-id tab blocks {:keys [append? dry-run?]}]
  (let [{:keys [requests full-text tables]} (build-batch-one blocks tab append?)]
    (if dry-run?
      (js/Promise.resolve
       {:dry-run true
        :batch-one-requests (count requests)
        :extra-table-batches (if (seq tables) 2 0)
        :tables (count tables)
        :preview (preview-text full-text tables)})
      (-> (api/batch-update token doc-id requests)
          (.then (fn [_]
                   (if (seq tables)
                     (-> (insert-tables-batch token doc-id (:tab-id tab) tables)
                         (.then (fn [_] (fill-tables-batch token doc-id (:tab-id tab) tables))))
                     (js/Promise.resolve nil))))
          (.then (fn [_]
                   {:dry-run false
                    :batch-one-requests (count requests)
                    :tables (count tables)}))))))
