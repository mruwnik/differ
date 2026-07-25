(ns differ.gdocs.render
  "Render a Docs API body (list of structural elements) into markdown.

   Lossy by design: the goal is a readable markdown approximation, not a
   round-trippable serialization. Operates on string-keyed maps (the raw Docs
   document shape)."
  (:require [clojure.string :as str]))

(def ^:private heading-prefix
  {"TITLE" "#"
   "SUBTITLE" "##"
   "HEADING_1" "#"
   "HEADING_2" "##"
   "HEADING_3" "###"
   "HEADING_4" "####"
   "HEADING_5" "#####"
   "HEADING_6" "######"})

(def ^:private monospace-fonts
  #{"Courier New"
    "Consolas"
    "Roboto Mono"
    "Source Code Pro"
    "Cousine"
    "monospace"})

(defn render-text-run
  "Render one paragraph element (a textRun) to inline markdown, mapping bold /
   italic / monospace-font styles to markers. Newlines inside a run are dropped
   so each paragraph renders on one line."
  [element]
  (let [run (get element "textRun")]
    (if-not run
      ""
      (let [text (str/replace (get run "content" "") "\n" "")]
        (if (empty? text)
          ""
          (let [style (get run "textStyle" {})
                font (get-in style ["weightedFontFamily" "fontFamily"] "")
                bold (get style "bold")
                italic (get style "italic")]
            (cond
              (contains? monospace-fonts font) (str "`" text "`")
              (and bold italic) (str "***" text "***")
              bold (str "**" text "**")
              italic (str "*" text "*")
              :else text)))))))

(defn- paragraph-text [paragraph]
  (str/join (map render-text-run (get paragraph "elements" []))))

(defn render-paragraph
  "Render a paragraph structural element: bullets, headings, or plain text."
  [paragraph]
  (let [bullet (get paragraph "bullet")]
    (if (some? bullet)
      (let [indent (apply str (repeat (get bullet "nestingLevel" 0) "  "))]
        (str indent "- " (str/trim (paragraph-text paragraph))))
      (let [named (get-in paragraph ["paragraphStyle" "namedStyleType"] "NORMAL_TEXT")
            prefix (get heading-prefix named "")]
        (if (seq prefix)
          (str prefix " " (str/trim (paragraph-text paragraph)))
          (str/trimr (paragraph-text paragraph)))))))

(defn- cell-text [cell]
  (let [parts (->> (get cell "content" [])
                   (keep (fn [element]
                           (when-let [paragraph (get element "paragraph")]
                             (str/trim (paragraph-text paragraph)))))
                   (remove str/blank?))]
    (str/replace (str/join " " parts) "|" "\\|")))

(defn render-table
  "Render a Docs table element as a GitHub pipe table."
  [table]
  (let [rows (get table "tableRows" [])]
    (if (empty? rows)
      ""
      (let [ncols (count (get (first rows) "tableCells" []))
            md-rows (mapv (fn [row]
                            (str "| "
                                 (str/join " | " (map cell-text (get row "tableCells" [])))
                                 " |"))
                          rows)
            separator (str "| " (str/join " | " (repeat ncols "---")) " |")]
        (str/join "\n" (concat [(first md-rows) separator] (rest md-rows)))))))

(defn- render-element
  "Return [markdown tight?] where tight items join without a blank line."
  [element]
  (cond
    (contains? element "table")
    [(render-table (get element "table")) false]

    (contains? element "paragraph")
    (let [paragraph (get element "paragraph")]
      [(render-paragraph paragraph) (some? (get paragraph "bullet"))])

    :else [nil false]))

(defn- normalize-blanks [text]
  (str (str/trim (str/replace text #"\n{3,}" "\n\n")) "\n"))

(defn render-body
  "Render a Docs body segment into markdown."
  [body]
  (normalize-blanks
   (str/join
    "\n"
    (reduce (fn [out element]
              (let [[block tight] (render-element element)]
                (if (nil? block)
                  out
                  (if (and (not tight) (seq out))
                    (conj out "" block)
                    (conj out block)))))
            []
            (get body "content" [])))))
