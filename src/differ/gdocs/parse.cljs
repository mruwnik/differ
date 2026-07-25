(ns differ.gdocs.parse
  "Parse markdown text into a flat list of block maps.

   Block shapes:
     {:type :heading   :level 1..6 :text str}
     {:type :paragraph :text str}
     {:type :bullet    :level 0..  :text str}
     {:type :hr}
     {:type :table     :rows [[cell ...] ...]}   ; rows[0] is the header"
  (:require [clojure.string :as str]))

(def ^:private heading-re #"^(#{1,6})\s+(.*)$")
(def ^:private bullet-re #"^([ \t]*)[-*+]\s+(.*)$")
(def ^:private hr-values #{"---" "***" "___"})
(def ^:private table-sep-re #"^\s*\|?[\s:|-]*-[\s:|-]*\|?\s*$")

;; Inline markup. Groups, in order: code, bold+italic, bold, italic,
;; bold(underscore), italic(underscore). A fresh RegExp is built per call so the
;; global-flag lastIndex state never leaks between invocations.
(def ^:private inline-pattern
  (str "`([^`]+)`"
       "|\\*\\*\\*([^*]+)\\*\\*\\*"
       "|\\*\\*([^*]+)\\*\\*"
       "|\\*([^*]+)\\*"
       "|__([^_]+)__"
       "|_([^_]+)_"))

(defn parse-inline
  "Split text into [segment style-map] runs, stripping markdown markers.
   style-map keys: :bold :italic :code (each true when present)."
  [text]
  (let [re (js/RegExp. inline-pattern "g")]
    (loop [runs [] pos 0]
      (if-let [m (.exec re text)]
        (let [start (.-index m)
              whole (aget m 0)
              end (+ start (.-length whole))
              code (aget m 1)
              bold-italic (aget m 2)
              bold (aget m 3)
              italic (aget m 4)
              bold-u (aget m 5)
              italic-u (aget m 6)
              runs (cond-> runs
                     (> start pos) (conj [(subs text pos start) {}]))
              runs (conj runs
                         (cond
                           (some? code) [code {:code true}]
                           (some? bold-italic) [bold-italic {:bold true :italic true}]
                           (or (some? bold) (some? bold-u))
                           [(if (some? bold) bold bold-u) {:bold true}]
                           :else [(if (some? italic) italic italic-u) {:italic true}]))]
          (recur runs end))
        (let [runs (cond-> runs
                     (< pos (count text)) (conj [(subs text pos) {}]))]
          (if (empty? runs) [[text {}]] runs))))))

(defn plain-from-runs [runs]
  (str/join (map first runs)))

(defn match-heading
  "Return [level text] for an ATX heading line, or nil."
  [line]
  (when-let [[_ hashes text] (re-find heading-re line)]
    [(count hashes) (str/trim text)]))

(defn match-bullet
  "Return [level text] for a bullet line (nesting from leading indent), or nil."
  [line]
  (when-let [[_ indent text] (re-find bullet-re line)]
    (let [spaces (str/replace indent "\t" "  ")]
      [(quot (count spaces) 2) (str/trim text)])))

(defn- table-row? [line]
  (and (str/includes? line "|") (not= (str/trim line) "")))

(defn- table-start? [lines i]
  (and (< (inc i) (count lines))
       (table-row? (nth lines i))
       (some? (re-find table-sep-re (nth lines (inc i))))))

(defn- split-table-row [line]
  (let [line (str/trim line)
        line (cond-> line (str/starts-with? line "|") (subs 1))
        line (cond-> line (str/ends-with? line "|") (#(subs % 0 (dec (count %)))))]
    (mapv (fn [cell] (str/replace (str/trim cell) "\\|" "|"))
          (str/split line #"\|" -1))))

(defn- parse-table
  "Return [table-block consumed-line-count] starting at header line i."
  [lines i]
  (loop [rows [(split-table-row (nth lines i))]
         j (+ i 2)] ; skip header + separator
    (if (and (< j (count lines))
             (table-row? (nth lines j))
             (str/includes? (nth lines j) "|"))
      (recur (conj rows (split-table-row (nth lines j))) (inc j))
      [{:type :table :rows rows} (- j i)])))

(defn- special-line? [lines i]
  (let [line (nth lines i)
        stripped (str/trim line)]
    (or (= stripped "")
        (contains? hr-values stripped)
        (some? (match-heading stripped))
        (some? (match-bullet line))
        (table-start? lines i))))

(defn parse-markdown
  "Parse markdown into a flat vector of block maps."
  [md-text]
  (let [lines (str/split (str/replace md-text "\r\n" "\n") #"\n" -1)
        n (count lines)]
    (loop [i 0 blocks []]
      (if (>= i n)
        blocks
        (let [line (nth lines i)
              stripped (str/trim line)]
          (cond
            (table-start? lines i)
            (let [[table consumed] (parse-table lines i)]
              (recur (+ i consumed) (conj blocks table)))

            (= stripped "")
            (recur (inc i) blocks)

            (contains? hr-values stripped)
            (recur (inc i) (conj blocks {:type :hr}))

            (match-heading stripped)
            (let [[level text] (match-heading stripped)]
              (recur (inc i) (conj blocks {:type :heading :level level :text text})))

            (match-bullet line)
            (let [[level text] (match-bullet line)]
              (recur (inc i) (conj blocks {:type :bullet :level level :text text})))

            :else
            ;; Gather a paragraph: consecutive non-special lines joined by spaces.
            (let [[end para] (loop [j (inc i) para [stripped]]
                               (if (and (< j n) (not (special-line? lines j)))
                                 (recur (inc j) (conj para (str/trim (nth lines j))))
                                 [j para]))]
              (recur end (conj blocks {:type :paragraph :text (str/join " " para)})))))))))
