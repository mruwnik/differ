(ns differ.client.highlight
  "Syntax highlighting using highlight.js."
  (:require ["highlight.js/lib/core" :as hljs]
            ["highlight.js/lib/languages/javascript" :as javascript]
            ["highlight.js/lib/languages/typescript" :as typescript]
            ["highlight.js/lib/languages/clojure" :as clojure]
            ["highlight.js/lib/languages/python" :as python]
            ["highlight.js/lib/languages/ruby" :as ruby]
            ["highlight.js/lib/languages/go" :as go]
            ["highlight.js/lib/languages/rust" :as rust]
            ["highlight.js/lib/languages/java" :as java]
            ["highlight.js/lib/languages/css" :as css]
            ["highlight.js/lib/languages/xml" :as xml]
            ["highlight.js/lib/languages/json" :as json]
            ["highlight.js/lib/languages/yaml" :as yaml]
            ["highlight.js/lib/languages/markdown" :as markdown]
            ["highlight.js/lib/languages/bash" :as bash]
            ["highlight.js/lib/languages/sql" :as sql]
            ["highlight.js/lib/languages/dockerfile" :as dockerfile]
            [clojure.string :as str]))

;; Register languages
(.registerLanguage hljs "javascript" javascript)
(.registerLanguage hljs "typescript" typescript)
(.registerLanguage hljs "clojure" clojure)
(.registerLanguage hljs "python" python)
(.registerLanguage hljs "ruby" ruby)
(.registerLanguage hljs "go" go)
(.registerLanguage hljs "rust" rust)
(.registerLanguage hljs "java" java)
(.registerLanguage hljs "css" css)
(.registerLanguage hljs "xml" xml)
(.registerLanguage hljs "json" json)
(.registerLanguage hljs "yaml" yaml)
(.registerLanguage hljs "markdown" markdown)
(.registerLanguage hljs "bash" bash)
(.registerLanguage hljs "sql" sql)
(.registerLanguage hljs "dockerfile" dockerfile)

;; Map file extensions to highlight.js language names
(def extension->language
  {"js"         "javascript"
   "mjs"        "javascript"
   "cjs"        "javascript"
   "jsx"        "javascript"
   "ts"         "typescript"
   "tsx"        "typescript"
   "clj"        "clojure"
   "cljs"       "clojure"
   "cljc"       "clojure"
   "edn"        "clojure"
   "py"         "python"
   "rb"         "ruby"
   "go"         "go"
   "rs"         "rust"
   "java"       "java"
   "css"        "css"
   "scss"       "css"
   "less"       "css"
   "html"       "xml"
   "htm"        "xml"
   "xml"        "xml"
   "svg"        "xml"
   "json"       "json"
   "yaml"       "yaml"
   "yml"        "yaml"
   "md"         "markdown"
   "markdown"   "markdown"
   "sh"         "bash"
   "bash"       "bash"
   "zsh"        "bash"
   "sql"        "sql"
   "dockerfile" "dockerfile"})

(defn get-language
  "Get highlight.js language name from file path."
  [file-path]
  (when file-path
    (let [filename (last (str/split file-path #"/"))
          ;; Handle special filenames like Dockerfile
          special-name (str/lower-case filename)
          extension (when-let [parts (seq (str/split filename #"\."))]
                      (when (> (count parts) 1)
                        (str/lower-case (last parts))))]
      (or (get extension->language special-name)
          (get extension->language extension)))))

(defn highlight-code
  "Highlight code string and return HTML.
   Returns the original content if language is unknown or highlighting fails."
  [content language]
  (if (and content language)
    (try
      (let [result (.highlight hljs content #js {:language language :ignoreIllegals true})]
        (.-value result))
      (catch :default _
        content))
    content))

(defn highlight-line
  "Highlight a single line of code.
   Returns hiccup that renders the highlighted HTML."
  [content file-path]
  (if-let [lang (get-language file-path)]
    (let [highlighted (highlight-code content lang)]
      [:span {:dangerouslySetInnerHTML {:__html highlighted}}])
    content))
