(ns differ.backend.protocol-test
  "Tests for the ReviewBackend protocol helpers."
  (:require [clojure.test :refer [deftest testing is]]
            [differ.backend.protocol :as proto]))

;; ============================================================================
;; extract-lines tests
;; ============================================================================

(deftest extract-lines-test
  (testing "returns nil for nil content"
    (is (nil? (proto/extract-lines nil {})))
    (is (nil? (proto/extract-lines nil {:from 1 :to 5}))))

  (testing "returns full content when no range specified"
    (let [content "line1\nline2\nline3\nline4\nline5"]
      (is (= content (proto/extract-lines content {})))
      (is (= content (proto/extract-lines content nil)))))

  (testing "extracts line range (1-indexed, inclusive)"
    (let [content "line1\nline2\nline3\nline4\nline5"]
      (is (= "line1" (proto/extract-lines content {:from 1 :to 1})))
      (is (= "line1\nline2\nline3" (proto/extract-lines content {:from 1 :to 3})))
      (is (= "line2\nline3\nline4" (proto/extract-lines content {:from 2 :to 4})))
      (is (= "line5" (proto/extract-lines content {:from 5 :to 5})))))

  (testing "handles :from only (to end of file)"
    (let [content "line1\nline2\nline3\nline4\nline5"]
      (is (= "line3\nline4\nline5" (proto/extract-lines content {:from 3})))
      (is (= "line1\nline2\nline3\nline4\nline5" (proto/extract-lines content {:from 1})))))

  (testing "handles :to only (from start)"
    (let [content "line1\nline2\nline3\nline4\nline5"]
      (is (= "line1\nline2\nline3" (proto/extract-lines content {:to 3})))
      (is (= "line1" (proto/extract-lines content {:to 1})))))

  (testing "clamps to file boundaries"
    (let [content "line1\nline2\nline3"]
      ;; Beyond file end - should return up to last line
      (is (= "line2\nline3" (proto/extract-lines content {:from 2 :to 100})))
      ;; Before file start - should start at first line
      (is (= "line1\nline2" (proto/extract-lines content {:from 0 :to 2})))
      (is (= "line1\nline2" (proto/extract-lines content {:from -5 :to 2})))))

  (testing "returns nil for invalid range (from > to)"
    (let [content "line1\nline2\nline3"]
      (is (nil? (proto/extract-lines content {:from 5 :to 2})))))

  (testing "returns nil for range entirely beyond file"
    ;; When from > line-count, start becomes > end after clamping, so nil is returned
    (let [content "line1\nline2\nline3"]
      (is (nil? (proto/extract-lines content {:from 10 :to 15})))))

  (testing "handles single-line content"
    (let [content "only line"]
      (is (= "only line" (proto/extract-lines content {:from 1 :to 1})))
      (is (= "only line" (proto/extract-lines content {})))
      ;; Range entirely beyond file returns nil
      (is (nil? (proto/extract-lines content {:from 2 :to 3})))))

  (testing "handles empty content"
    (let [content ""]
      (is (= "" (proto/extract-lines content {})))
      (is (= "" (proto/extract-lines content {:from 1 :to 1})))))

  (testing "preserves content with special characters"
    (let [content "line with tabs\there\nline with émojis 🎉\nline with \"quotes\""]
      (is (= "line with tabs\there" (proto/extract-lines content {:from 1 :to 1})))
      (is (= "line with émojis 🎉" (proto/extract-lines content {:from 2 :to 2})))
      (is (= "line with \"quotes\"" (proto/extract-lines content {:from 3 :to 3}))))))
