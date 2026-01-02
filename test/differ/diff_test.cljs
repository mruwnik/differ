(ns differ.diff-test
  "Tests for diff computation module."
  (:require [clojure.test :refer [deftest testing is]]
            [differ.diff :as diff]))

;; ============================================================================
;; default-max-file-size tests
;; ============================================================================

(deftest default-max-file-size-test
  (testing "returns a positive number"
    (let [size (diff/default-max-file-size)]
      (is (number? size))
      (is (pos? size))))

  (testing "returns reasonable default (at least 1KB)"
    (is (>= (diff/default-max-file-size) 1000))))

;; ============================================================================
;; Private helper tests using #'var syntax
;; ============================================================================

;; Test mark-as-untracked (private fn)
(deftest mark-as-untracked-test
  (let [mark-fn @#'differ.diff/mark-as-untracked]
    (testing "converts paths to untracked status"
      (is (= [{:path "foo.txt" :status :untracked}]
             (vec (mark-fn ["foo.txt"])))))

    (testing "handles multiple files"
      (let [result (vec (mark-fn ["a.txt" "b.txt" "c.txt"]))]
        (is (= 3 (count result)))
        (is (every? #(= :untracked (:status %)) result))
        (is (= #{"a.txt" "b.txt" "c.txt"} (set (map :path result))))))

    (testing "handles empty list"
      (is (empty? (mark-fn []))))))

;; Test find-untracked-in-review (private fn)
(deftest find-untracked-in-review-test
  (let [find-fn @#'differ.diff/find-untracked-in-review]
    (testing "finds intersection of review files and untracked"
      (is (= #{"c.txt"}
             (find-fn ["a.txt" "b.txt" "c.txt"]
                      ["c.txt" "d.txt"]))))

    (testing "returns empty set when no overlap"
      (is (empty? (find-fn ["a.txt" "b.txt"]
                           ["c.txt" "d.txt"]))))

    (testing "handles empty inputs"
      (is (empty? (find-fn [] ["a.txt"])))
      (is (empty? (find-fn ["a.txt"] [])))
      (is (empty? (find-fn [] []))))

    (testing "handles complete overlap"
      (is (= #{"a.txt" "b.txt"}
             (find-fn ["a.txt" "b.txt"]
                      ["a.txt" "b.txt"]))))))
