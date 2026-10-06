(ns differ.client.board-stats-test
  (:require [clojure.test :refer [deftest is are]]
            [differ.client.board-stats :as bs]
            [differ.client.db :as db]))

(deftest active-test
  (are [totals expected] (= expected (bs/active? {:totals totals}))
    {:done 0 :reverted 0}  false
    {:done 0 :created 1}   true
    {}                     false
    nil                    false))

(deftest last-bucket-count-test
  (are [buckets expected] (= expected (bs/last-bucket-count {:buckets buckets} :done))
    [{:done 1} {:done 4}] 4
    [{:done 1} {}]        0
    []                    0))

(deftest format-rate-test
  (are [x expected] (= expected (bs/format-rate x))
    nil   "–"
    0     "0"
    0.04  "0.04"
    0.333 "0.33"
    1     "1.0"
    2.25  "2.3"
    12.4  "12"))

(deftest format-minutes-test
  (are [m expected] (= expected (bs/format-minutes m))
    nil   "–"
    0     "0m"
    42.4  "42m"
    90    "1.5h"
    2880  "2.0d"))

(deftest nice-max-test
  (are [n expected] (= expected (bs/nice-max n))
    0   1
    1   1
    5   5
    6   10
    11  20
    20  20
    37  50
    51  100
    120 200))

(deftest bar-path-test
  (is (= "M0,50L0,44Q0,40 4,40L6,40Q10,40 10,44L10,50Z" (bs/bar-path 0 50 10 10)))
  (is (= "M0,50L0,50Q0,48 2,48L8,48Q10,48 10,50L10,50Z" (bs/bar-path 0 50 10 2))
      "radius never exceeds the bar height"))

(deftest chart-bars-test
  (let [{:keys [y-max bars]} (bs/chart-bars [{:done 2 :reverted 1} {:done 0 :reverted 0} {:done 0 :reverted 4}]
                                            {:slot-width 20 :bar-width 6 :gap 2 :plot-height 50})]
    (is (= 4 y-max))
    (is (= [{:bucket 0 :key :done :color "#2a78d6" :count 2 :x 3 :w 6 :h 25}
            {:bucket 0 :key :reverted :color "#eb6834" :count 1 :x 11 :w 6 :h 12.5}
            {:bucket 2 :key :reverted :color "#eb6834" :count 4 :x 51 :w 6 :h 50}]
           bars))))

(deftest default-db-board-stats-test
  (is (nil? (:board-stats db/default-db)))
  (is (false? (:board-stats-collapsed db/default-db))))

(deftest chart-bars-empty-test
  (is (= {:y-max 1 :bars []}
         (bs/chart-bars [{:done 0 :reverted 0}] {:slot-width 20 :bar-width 6 :gap 2 :plot-height 50}))))
