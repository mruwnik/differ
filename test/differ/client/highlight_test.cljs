(ns differ.client.highlight-test
  "Tests for syntax highlighting module."
  (:require [clojure.test :refer [deftest testing is]]
            [differ.client.highlight :as highlight]))

;; ============================================================================
;; get-language Tests
;; ============================================================================

(deftest get-language-test
  (testing "returns nil for nil path"
    (is (nil? (highlight/get-language nil))))

  (testing "returns nil for unknown extension"
    (is (nil? (highlight/get-language "file.xyz")))
    (is (nil? (highlight/get-language "noextension"))))

  (testing "detects JavaScript files"
    (is (= "javascript" (highlight/get-language "app.js")))
    (is (= "javascript" (highlight/get-language "src/main.jsx")))
    (is (= "javascript" (highlight/get-language "lib.mjs")))
    (is (= "javascript" (highlight/get-language "config.cjs"))))

  (testing "detects TypeScript files"
    (is (= "typescript" (highlight/get-language "app.ts")))
    (is (= "typescript" (highlight/get-language "component.tsx"))))

  (testing "detects Clojure files"
    (is (= "clojure" (highlight/get-language "core.clj")))
    (is (= "clojure" (highlight/get-language "client.cljs")))
    (is (= "clojure" (highlight/get-language "shared.cljc")))
    (is (= "clojure" (highlight/get-language "config.edn"))))

  (testing "detects Python files"
    (is (= "python" (highlight/get-language "script.py"))))

  (testing "detects Ruby files"
    (is (= "ruby" (highlight/get-language "app.rb"))))

  (testing "detects Go files"
    (is (= "go" (highlight/get-language "main.go"))))

  (testing "detects Rust files"
    (is (= "rust" (highlight/get-language "lib.rs"))))

  (testing "detects Java files"
    (is (= "java" (highlight/get-language "Main.java"))))

  (testing "detects CSS files"
    (is (= "css" (highlight/get-language "style.css")))
    (is (= "css" (highlight/get-language "theme.scss")))
    (is (= "css" (highlight/get-language "vars.less"))))

  (testing "detects HTML/XML files"
    (is (= "xml" (highlight/get-language "index.html")))
    (is (= "xml" (highlight/get-language "page.htm")))
    (is (= "xml" (highlight/get-language "config.xml")))
    (is (= "xml" (highlight/get-language "icon.svg"))))

  (testing "detects JSON files"
    (is (= "json" (highlight/get-language "package.json"))))

  (testing "detects YAML files"
    (is (= "yaml" (highlight/get-language "config.yaml")))
    (is (= "yaml" (highlight/get-language "ci.yml"))))

  (testing "detects Markdown files"
    (is (= "markdown" (highlight/get-language "README.md")))
    (is (= "markdown" (highlight/get-language "docs.markdown"))))

  (testing "detects shell scripts"
    (is (= "bash" (highlight/get-language "build.sh")))
    (is (= "bash" (highlight/get-language "run.bash")))
    (is (= "bash" (highlight/get-language "setup.zsh"))))

  (testing "detects SQL files"
    (is (= "sql" (highlight/get-language "schema.sql"))))

  (testing "detects Dockerfile"
    (is (= "dockerfile" (highlight/get-language "Dockerfile")))
    (is (= "dockerfile" (highlight/get-language "dockerfile"))))

  (testing "handles full paths"
    (is (= "clojure" (highlight/get-language "src/differ/client/views/diff.cljs")))
    (is (= "javascript" (highlight/get-language "/Users/dan/project/src/index.js"))))

  (testing "is case insensitive for extensions"
    (is (= "javascript" (highlight/get-language "App.JS")))
    (is (= "json" (highlight/get-language "CONFIG.JSON")))))

;; ============================================================================
;; highlight-code Tests
;; ============================================================================

(deftest highlight-code-test
  (testing "returns original content for nil language"
    (is (= "const x = 1" (highlight/highlight-code "const x = 1" nil))))

  (testing "returns original content for nil content"
    (is (nil? (highlight/highlight-code nil "javascript"))))

  (testing "highlights JavaScript code"
    (let [result (highlight/highlight-code "const x = 1;" "javascript")]
      (is (string? result))
      (is (not= "const x = 1;" result))  ; Should have HTML spans
      (is (re-find #"hljs-" result))))   ; Should contain highlight.js classes

  (testing "highlights Clojure code"
    (let [result (highlight/highlight-code "(defn foo [x] x)" "clojure")]
      (is (string? result))
      (is (re-find #"hljs-" result)))))

;; ============================================================================
;; highlight-line Tests
;; ============================================================================

(deftest highlight-line-test
  (testing "returns plain content for unknown file type"
    (is (= "some content" (highlight/highlight-line "some content" "file.xyz"))))

  (testing "returns hiccup with highlighted HTML for known file types"
    (let [result (highlight/highlight-line "const x = 1;" "app.js")]
      (is (vector? result))
      (is (= :span (first result)))
      (is (contains? (second result) :dangerouslySetInnerHTML)))))
