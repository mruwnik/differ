(ns differ.gdocs.build
  "Turn parsed markdown blocks into Docs API batchUpdate requests.

   Index bookkeeping is the whole game. Everything except createParagraphBullets
   is length-preserving, so we:

     1. Concatenate every block into one plain-text string (tables become a
        unique one-line sentinel paragraph, resolved later against a real fetch).
     2. Record inline text-style ranges and heading paragraph-style ranges as
        absolute offsets into that string.
     3. Bullets are created from leading TAB characters (the Docs API derives a
        bullet's nesting level from the tab count). createParagraphBullets
        deletes those tabs, shifting later indices, so those requests are
        emitted last and in descending index order — each deletion then only
        touches text after the requests still waiting to apply."
  (:require [clojure.string :as str]
            [differ.gdocs.api :refer [text-range]]
            [differ.gdocs.parse :refer [parse-inline plain-from-runs]]))

(def ^:private heading-styles
  {1 "HEADING_1" 2 "HEADING_2" 3 "HEADING_3"
   4 "HEADING_4" 5 "HEADING_5" 6 "HEADING_6"})

(def ^:private bullet-preset "BULLET_DISC_CIRCLE_SQUARE")
(def ^:private code-font "Consolas")
(def ^:private hr-text (apply str (repeat 20 "—")))

;; Plain-ASCII sentinel. Non-ASCII/control chars (e.g. private-use U+E000) get
;; silently stripped by Docs on insert, which would break marker lookup; this
;; distinctive ASCII string survives insertion and is vanishingly unlikely to
;; occur in real content.
(def ^:private sentinel "zZqGDOCSTABLEqZz")

(defn table-marker
  "Unique one-line placeholder for a table, resolved against a real fetch."
  [index]
  (str sentinel index sentinel))

(defn- make-text-style [start end style tab-id]
  (let [[text-style fields]
        (cond-> [{} []]
          (:bold style)   (-> (assoc-in [0 :bold] true) (update 1 conj "bold"))
          (:italic style) (-> (assoc-in [0 :italic] true) (update 1 conj "italic"))
          (:code style)   (-> (assoc-in [0 :weightedFontFamily] {:fontFamily code-font})
                              (update 1 conj "weightedFontFamily")))]
    {:updateTextStyle {:range (text-range start end tab-id)
                       :textStyle text-style
                       :fields (str/join "," fields)}}))

(defn- make-paragraph-style [start end named-style tab-id]
  {:updateParagraphStyle {:range (text-range start end tab-id)
                          :paragraphStyle {:namedStyleType named-style}
                          :fields "namedStyleType"}})

(defn- make-bullets [start end tab-id]
  {:createParagraphBullets {:range (text-range start end tab-id)
                            :bulletPreset bullet-preset}})

(defn- block-line
  "Return {:text :runs :run-offset :paragraph-style :bullet-level} for a block."
  [block]
  (case (:type block)
    :heading (let [runs (parse-inline (:text block))]
               {:text (plain-from-runs runs) :runs runs :run-offset 0
                :paragraph-style (heading-styles (:level block)) :bullet-level nil})
    :paragraph (let [runs (parse-inline (:text block))]
                 {:text (plain-from-runs runs) :runs runs :run-offset 0
                  :paragraph-style nil :bullet-level nil})
    :bullet (let [prefix (apply str (repeat (:level block) "\t"))
                  runs (parse-inline (:text block))]
              {:text (str prefix (plain-from-runs runs)) :runs runs
               :run-offset (count prefix) :paragraph-style nil
               :bullet-level (:level block)})
    :hr {:text hr-text :runs [] :run-offset 0 :paragraph-style nil :bullet-level nil}
    {:text "" :runs [] :run-offset 0 :paragraph-style nil :bullet-level nil}))

(defn build-insertion
  "Build the full insert text plus all length-preserving / bullet requests.

   Returns {:full-text :paragraph-styles :text-styles :bullet-requests :tables}
   where :tables is a vector of {:marker :rows} to resolve post-insert."
  [blocks base-index tab-id leading-newline?]
  (let [init {:parts (if leading-newline? ["\n"] [])
              :cursor (if leading-newline? (inc base-index) base-index)
              :text-styles []
              :paragraph-styles []
              :bullet-groups []
              :group-start nil
              :group-end nil
              :tables []}
        ;; A contiguous run of bullet paragraphs must become one list (one
        ;; createParagraphBullets call) so leading tabs set nesting within a
        ;; shared list. Any non-bullet block closes the current run.
        close-group (fn [state]
                      (if (:group-start state)
                        (-> state
                            (update :bullet-groups conj [(:group-start state) (:group-end state)])
                            (assoc :group-start nil))
                        state))
        state
        (reduce
         (fn [state block]
           (let [state (if (and (not= (:type block) :bullet) (:group-start state))
                         (close-group state)
                         state)]
             (if (= (:type block) :table)
               (let [marker (table-marker (count (:tables state)))]
                 (-> state
                     (update :parts conj (str marker "\n"))
                     (update :tables conj {:marker marker :rows (:rows block)})
                     (update :cursor + (inc (count marker)))))
               (let [{:keys [text runs run-offset paragraph-style bullet-level]} (block-line block)
                     paragraph-start (:cursor state)
                     paragraph-end (+ paragraph-start (count text) 1)
                     ;; Absolute inline-style ranges into the single inserted string.
                     text-styles (:text-styles
                                  (reduce (fn [acc [segment style]]
                                            (let [offset (:offset acc)]
                                              (-> acc
                                                  (cond-> (and (seq segment) (seq style))
                                                    (update :text-styles conj
                                                            (make-text-style offset (+ offset (count segment)) style tab-id)))
                                                  (assoc :offset (+ offset (count segment))))))
                                          {:offset (+ paragraph-start run-offset)
                                           :text-styles (:text-styles state)}
                                          runs))]
                 (cond-> (assoc state :text-styles text-styles)
                   paragraph-style
                   (update :paragraph-styles conj
                           (make-paragraph-style paragraph-start paragraph-end paragraph-style tab-id))

                   (some? bullet-level)
                   (-> (update :group-start #(or % paragraph-start))
                       (assoc :group-end paragraph-end))

                   :always
                   (-> (update :parts conj (str text "\n"))
                       (update :cursor + (inc (count text)))))))))
         init
         blocks)
        state (close-group state)
        ;; Descending order: createParagraphBullets strips leading tabs (shifting
        ;; later indices), so applying the highest-index group first keeps the
        ;; ranges of the groups still waiting to apply valid.
        bullet-requests (->> (:bullet-groups state)
                             (sort-by first >)
                             (mapv (fn [[start end]] (make-bullets start end tab-id))))]
    {:full-text (apply str (:parts state))
     :paragraph-styles (:paragraph-styles state)
     :text-styles (:text-styles state)
     :bullet-requests bullet-requests
     :tables (:tables state)}))

(defn preview-text
  "Human-readable rendering of the insert text for dry-run."
  [full-text tables]
  (reduce-kv
   (fn [text index table]
     (let [nrows (count (:rows table))
           ncols (reduce max 0 (map count (:rows table)))]
       (str/replace text (table-marker index)
                               (str "[table " nrows "x" ncols "]"))))
   full-text
   (vec tables)))
