(ns differ.client.task-filter
  "Pure client-side filtering for kanban board tasks: fuzzy title search,
   tag filter and minimum-priority filter."
  (:require [clojure.string :as str]))

(def default-filter
  {:search "" :tags #{} :min-priority nil})

(defn active?
  "True when any filter would narrow the task list."
  [{:keys [search tags min-priority]}]
  (boolean (or (not (str/blank? search))
               (seq tags)
               (some? min-priority))))

(defn toggle-tag
  "Add tag to the filter's tag set, or remove it if already present."
  [filter-state tag]
  (update filter-state :tags (fn [tags]
                               (let [tags (or tags #{})]
                                 (if (tags tag) (disj tags tag) (conj tags tag))))))

(defn- subsequence?
  "True when every char of needle appears in haystack, in order."
  [needle haystack]
  (loop [n (seq needle)
         h (seq haystack)]
    (cond
      (empty? n) true
      (empty? h) false
      (= (first n) (first h)) (recur (rest n) (rest h))
      :else (recur n (rest h)))))

(def ^:private word-separator
  "Anything that isn't a letter or digit (Unicode-aware, hence the u flag)."
  (js/RegExp. "[^\\p{L}\\p{N}]+" "u"))

(defn- term-matches?
  "A term matches when it is a substring of the text, or a subsequence of a
   single word in it (so \"drv\" finds \"drive\" but letters scattered across
   several words don't count)."
  [term text]
  (or (str/includes? text term)
      (some #(subsequence? term %) (str/split text word-separator))))

(defn fuzzy-match?
  "Case-insensitive fuzzy match. Every whitespace-separated term of query
   must match the text (see term-matches?), in any order. A blank query
   matches everything."
  [query text]
  (let [terms (remove str/blank? (str/split (str/lower-case (or query "")) #"\s+"))
        haystack (str/lower-case (or text ""))]
    (or (empty? terms)
        (and (some? text)
             (every? #(term-matches? % haystack) terms)))))

(defn filter-tasks
  "Filter tasks, preserving order. All given filters must hold:
     :search       - fuzzy-matched against :title
     :tags         - set of tags; task must have ANY of them (empty = no filter)
     :min-priority - task priority (missing = 0) must be >= this"
  [tasks {:keys [search tags min-priority]}]
  (filterv (fn [task]
             (and (fuzzy-match? search (:title task))
                  (or (empty? tags) (some tags (:tags task)))
                  (or (nil? min-priority) (>= (or (:priority task) 0) min-priority))))
           tasks))

(defn board-tags
  "All distinct tags used on the board, sorted."
  [tasks]
  (vec (sort (distinct (mapcat :tags tasks)))))

(defn board-priorities
  "All distinct task priorities on the board, most urgent first."
  [tasks]
  (vec (sort > (distinct (map #(or (:priority %) 0) tasks)))))
