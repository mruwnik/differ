(ns differ.gdocs-test
  "Tests for the pure Google Docs port: markdown parsing, Docs->markdown
   rendering, request building, and tab discovery. Network paths (auth/api/
   writer) are exercised only for their pure helpers."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [differ.gdocs.api :as api]
            [differ.gdocs.auth :as auth]
            [differ.gdocs.core :as core]
            [differ.gdocs.parse :as parse]
            [differ.gdocs.render :as render]
            [differ.gdocs.build :as build]
            [differ.gdocs.tabs :as tabs]))

;; ---------------------------------------------------------------------------
;; api: id/URL parsing + range helpers
;; ---------------------------------------------------------------------------

(deftest extract-doc-id-test
  (testing "bare id passes through"
    (is (= "abc123_-XYZ" (api/extract-doc-id "abc123_-XYZ"))))
  (testing "full URL yields the id"
    (is (= "1AbC_dEf-99"
           (api/extract-doc-id "https://docs.google.com/document/d/1AbC_dEf-99/edit"))))
  (testing "trims surrounding whitespace on bare id"
    (is (= "xyz" (api/extract-doc-id "  xyz  ")))))

(deftest range-helpers-test
  (testing "text-range omits tabId when nil, includes it otherwise"
    (is (= {:startIndex 1 :endIndex 5} (api/text-range 1 5 nil)))
    (is (= {:startIndex 1 :endIndex 5 :tabId "t.0"} (api/text-range 1 5 "t.0"))))
  (testing "location omits tabId when nil"
    (is (= {:index 3} (api/location 3 nil)))
    (is (= {:index 3 :tabId "t.1"} (api/location 3 "t.1")))))

;; ---------------------------------------------------------------------------
;; auth: tolerant creds JSON (PEM keys pasted with real newlines)
;; ---------------------------------------------------------------------------

(deftest reescape-json-control-chars-test
  (testing "valid JSON passes through unchanged"
    (let [s "{\"a\": \"b\", \"n\": 1}"]
      (is (= s (auth/reescape-json-control-chars s)))))
  (testing "raw newline/tab/CR inside a string literal are escaped so JSON.parse accepts it"
    (let [malformed (str "{\"private_key\": \"-----BEGIN-----\nline2\ttab\r-----END-----\"}")
          fixed (auth/reescape-json-control-chars malformed)
          parsed (js->clj (js/JSON.parse fixed))]
      (is (= "-----BEGIN-----\nline2\ttab\r-----END-----" (get parsed "private_key")))))
  (testing "structural newlines between tokens are untouched (still valid)"
    (let [pretty "{\n  \"a\": \"b\"\n}"]
      (is (= (js->clj (js/JSON.parse pretty))
             (js->clj (js/JSON.parse (auth/reescape-json-control-chars pretty))))))))

;; ---------------------------------------------------------------------------
;; parse: inline runs + block parsing
;; ---------------------------------------------------------------------------

(deftest parse-inline-test
  (testing "plain text is a single unstyled run"
    (is (= [["hello" {}]] (parse/parse-inline "hello"))))
  (testing "bold / italic / code / bold-italic"
    (is (= [["x" {:bold true}]] (parse/parse-inline "**x**")))
    (is (= [["x" {:italic true}]] (parse/parse-inline "*x*")))
    (is (= [["x" {:code true}]] (parse/parse-inline "`x`")))
    (is (= [["x" {:bold true :italic true}]] (parse/parse-inline "***x***"))))
  (testing "underscore emphasis"
    (is (= [["x" {:bold true}]] (parse/parse-inline "__x__")))
    (is (= [["x" {:italic true}]] (parse/parse-inline "_x_"))))
  (testing "mixed runs preserve surrounding plain text"
    (is (= [["a " {}] ["b" {:bold true}] [" c" {}]]
           (parse/parse-inline "a **b** c"))))
  (testing "plain-from-runs strips markers"
    (is (= "a b c" (parse/plain-from-runs (parse/parse-inline "a **b** c"))))))

(deftest parse-markdown-blocks-test
  (testing "headings capture level and text"
    (is (= [{:type :heading :level 1 :text "Title"}]
           (parse/parse-markdown "# Title")))
    (is (= [{:type :heading :level 3 :text "Sub"}]
           (parse/parse-markdown "### Sub"))))
  (testing "bullets carry nesting from indentation"
    (is (= [{:type :bullet :level 0 :text "a"}
            {:type :bullet :level 1 :text "b"}]
           (parse/parse-markdown "- a\n  - b"))))
  (testing "horizontal rule variants"
    (is (= [{:type :hr}] (parse/parse-markdown "---")))
    (is (= [{:type :hr}] (parse/parse-markdown "***"))))
  (testing "blank lines are skipped; paragraphs join wrapped lines"
    (is (= [{:type :paragraph :text "one two"}]
           (parse/parse-markdown "one\ntwo")))
    (is (= [{:type :paragraph :text "p1"} {:type :paragraph :text "p2"}]
           (parse/parse-markdown "p1\n\np2"))))
  (testing "pipe tables parse into rows (header first)"
    (is (= [{:type :table :rows [["a" "b"] ["1" "2"]]}]
           (parse/parse-markdown "| a | b |\n| --- | --- |\n| 1 | 2 |")))))

;; ---------------------------------------------------------------------------
;; render: Docs body -> markdown (string-keyed API shape)
;; ---------------------------------------------------------------------------

(defn- para
  "Build a Docs paragraph structural element from [text style-map] runs."
  [runs & [{:keys [named bullet-level]}]]
  {"paragraph"
   (cond-> {"elements" (mapv (fn [[content style]]
                               {"textRun" {"content" content
                                           "textStyle" style}})
                             runs)}
     named (assoc "paragraphStyle" {"namedStyleType" named})
     (some? bullet-level) (assoc "bullet" {"nestingLevel" bullet-level}))})

(deftest render-body-test
  (testing "heading paragraph renders with hashes"
    (is (= "# Title\n"
           (render/render-body {"content" [(para [["Title\n" {}]] {:named "HEADING_1"})]}))))
  (testing "bold and code runs map to markers"
    (is (= "**b** and `c`\n"
           (render/render-body
            {"content" [(para [["b" {"bold" true}] [" and " {}] ["c" {"weightedFontFamily" {"fontFamily" "Consolas"}}]])]}))))
  (testing "bullets render with indentation by nesting level"
    (is (= "- top\n  - child\n"
           (render/render-body
            {"content" [(para [["top" {}]] {:bullet-level 0})
                        (para [["child" {}]] {:bullet-level 1})]}))))
  (testing "table renders as a github pipe table"
    (let [table {"table" {"tableRows"
                          [{"tableCells" [{"content" [(para [["a" {}]])]}
                                          {"content" [(para [["b" {}]])]}]}
                           {"tableCells" [{"content" [(para [["1" {}]])]}
                                          {"content" [(para [["2" {}]])]}]}]}}]
      (is (= "| a | b |\n| --- | --- |\n| 1 | 2 |\n"
             (render/render-body {"content" [table]}))))))

;; ---------------------------------------------------------------------------
;; build: blocks -> batchUpdate requests + index bookkeeping
;; ---------------------------------------------------------------------------

(deftest build-insertion-indices-test
  (testing "full text concatenates blocks with newlines"
    (let [{:keys [full-text]}
          (build/build-insertion [{:type :heading :level 1 :text "T"}
                                  {:type :paragraph :text "body"}]
                                 1 nil false)]
      (is (= "T\nbody\n" full-text))))
  (testing "heading paragraph style spans the heading line + its newline"
    (let [{:keys [paragraph-styles]}
          (build/build-insertion [{:type :heading :level 2 :text "Hi"}] 1 nil false)]
      (is (= 1 (count paragraph-styles)))
      (is (= {:startIndex 1 :endIndex 4}
             (get-in (first paragraph-styles) [:updateParagraphStyle :range])))
      (is (= "HEADING_2"
             (get-in (first paragraph-styles) [:updateParagraphStyle :paragraphStyle :namedStyleType])))))
  (testing "inline bold produces a text-style range at the right offset"
    (let [{:keys [text-styles]}
          (build/build-insertion [{:type :paragraph :text "a **b**"}] 1 nil false)]
      (is (= 1 (count text-styles)))
      ;; "a " occupies indices 1-2, bold "b" is index 3-4
      (is (= {:startIndex 3 :endIndex 4}
             (get-in (first text-styles) [:updateTextStyle :range])))
      (is (true? (get-in (first text-styles) [:updateTextStyle :textStyle :bold]))))))

(deftest build-bullets-descending-test
  (testing "a contiguous bullet run yields one createParagraphBullets request"
    (let [{:keys [bullet-requests full-text]}
          (build/build-insertion [{:type :bullet :level 0 :text "a"}
                                  {:type :bullet :level 1 :text "b"}]
                                 1 nil false)]
      ;; leading tab encodes nesting for the level-1 bullet
      (is (str/includes? full-text "\tb"))
      (is (= 1 (count bullet-requests)))))
  (testing "bullet runs separated by a paragraph produce two requests, highest index first"
    (let [{:keys [bullet-requests]}
          (build/build-insertion [{:type :bullet :level 0 :text "a"}
                                  {:type :paragraph :text "sep"}
                                  {:type :bullet :level 0 :text "b"}]
                                 1 nil false)
          starts (map #(get-in % [:createParagraphBullets :range :startIndex]) bullet-requests)]
      (is (= 2 (count bullet-requests)))
      (is (= (reverse (sort starts)) starts) "emitted in descending index order"))))

(deftest build-table-marker-test
  (testing "a table becomes a unique sentinel line and a resolved table entry"
    (let [{:keys [full-text tables]}
          (build/build-insertion [{:type :table :rows [["a" "b"]]}] 1 nil false)]
      (is (= 1 (count tables)))
      (is (str/includes? full-text (:marker (first tables))))
      (is (= [["a" "b"]] (:rows (first tables))))))
  (testing "preview-text replaces markers with a size summary"
    (let [{:keys [full-text tables]}
          (build/build-insertion [{:type :table :rows [["a" "b"] ["1" "2"]]}] 1 nil false)]
      (is (= "[table 2x2]\n" (build/preview-text full-text tables))))))

;; ---------------------------------------------------------------------------
;; tabs: discovery / resolution over the raw Docs document shape
;; ---------------------------------------------------------------------------

(def ^:private doc-with-tabs
  {"title" "Doc"
   "documentId" "d1"
   "tabs" [{"tabProperties" {"tabId" "t.0" "title" "Design"}
            "documentTab" {"body" {"content" []}}
            "childTabs" [{"tabProperties" {"tabId" "t.1" "title" "Details"}
                          "documentTab" {"body" {"content" []}}}]}]})

(deftest flatten-tabs-test
  (testing "nested tabs flatten depth-first with nesting annotations"
    (is (= [{:tab-id "t.0" :title "Design" :nesting 0 :body {"content" []}}
            {:tab-id "t.1" :title "Details" :nesting 1 :body {"content" []}}]
           (tabs/flatten-tabs doc-with-tabs))))
  (testing "a document with no tabs is one implicit tab from the top-level body"
    (is (= [{:tab-id nil :title "Legacy" :nesting 0 :body {"content" []}}]
           (tabs/flatten-tabs {"title" "Legacy" "body" {"content" []}})))))

(deftest resolve-tab-test
  (testing "exact tabId wins"
    (is (= "Details" (:title (tabs/resolve-tab doc-with-tabs "t.1")))))
  (testing "case-insensitive title match"
    (is (= "t.0" (:tab-id (tabs/resolve-tab doc-with-tabs "design")))))
  (testing "no match returns nil"
    (is (nil? (tabs/resolve-tab doc-with-tabs "nope")))))

(deftest body-end-index-test
  (testing "empty body defaults to 2"
    (is (= 2 (tabs/body-end-index {"content" []}))))
  (testing "uses last element endIndex"
    (is (= 42 (tabs/body-end-index {"content" [{"endIndex" 10} {"endIndex" 42}]})))))

(deftest null-title-hardening-test
  (testing "an explicit null tab title falls back to 'Untitled' (not nil)"
    (let [doc {"tabs" [{"tabProperties" {"tabId" "t.0" "title" nil}
                        "documentTab" {"body" {"content" []}}}]}]
      (is (= "Untitled" (:title (first (tabs/flatten-tabs doc)))))
      ;; resolve-tab lower-cases titles; a nil title would have thrown here
      (is (nil? (tabs/resolve-tab doc "design")))
      (is (= "t.0" (:tab-id (tabs/resolve-tab doc "t.0"))))))
  (testing "a null document title on an implicit-tab doc falls back to 'Untitled'"
    (is (= "Untitled"
           (:title (first (tabs/flatten-tabs {"title" nil "body" {"content" []}})))))))

;; ---------------------------------------------------------------------------
;; core: write-doc input guards
;; ---------------------------------------------------------------------------

(deftest write-doc-blank-tab-test
  (testing "write-doc refuses a blank tab selector before doing any network work"
    (is (thrown? js/Error
                 (core/write-doc "some-doc-id" {:tab "" :markdown "# hi"})))
    (is (thrown? js/Error
                 (core/write-doc "some-doc-id" {:tab "   " :markdown "# hi"})))))
