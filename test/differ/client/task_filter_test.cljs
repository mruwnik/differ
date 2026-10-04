(ns differ.client.task-filter-test
  (:require [clojure.test :refer [deftest testing is are]]
            [differ.client.task-filter :as tf]
            [differ.client.db]))

(deftest fuzzy-match-test
  (are [query text expected] (= expected (tf/fuzzy-match? query text))
    ""             "anything"                  true
    "   "          "anything"                  true
    "login"        "Fix login bug"             true
    "LOGIN"        "fix login bug"             true
    "lgn"          "Fix login bug"             true
    "fxlgn"        "Fix login bug"             false
    "bt drv"       "Boats 3: jobs.movement.boat-drive" true
    "bt drv"       "Trigger: player joined and player-left body events" false
    "boat-drive"   "jobs.movement.boat-drive"  true
    "x b"          "fix login bug"             true
    "fix bug"      "Fix the login bug"         true
    "bug fix"      "Fix the login bug"         true
    "lgnx"         "Fix login bug"             false
    "fix crash"    "Fix the login bug"         false
    "a"            nil                         false))

(def tasks
  [{:id "1" :title "Fix login bug" :priority 5 :tags ["auth" "bug"]}
   {:id "2" :title "Write docs" :priority 0 :tags []}
   {:id "3" :title "Harden login form" :priority 2 :tags ["security"]}
   {:id "4" :title "Old cleanup" :priority -1 :tags ["chore"]}])

(defn ids [filters] (mapv :id (tf/filter-tasks tasks filters)))

(deftest filter-tasks-test
  (testing "no filters keeps everything in order"
    (is (= ["1" "2" "3" "4"] (ids {}))))
  (testing "search"
    (is (= ["1" "3"] (ids {:search "login"}))))
  (testing "tags match any selected tag"
    (is (= ["1" "3"] (ids {:tags #{"bug" "security"}}))))
  (testing "empty tag set is no filter"
    (is (= ["1" "2" "3" "4"] (ids {:tags #{}}))))
  (testing "min priority is inclusive"
    (is (= ["1" "3"] (ids {:min-priority 2}))))
  (testing "filters combine with AND"
    (is (= ["3"] (ids {:search "login" :tags #{"security"} :min-priority 0}))))
  (testing "tasks missing priority count as 0"
    (is (= ["x"] (mapv :id (tf/filter-tasks [{:id "x" :title "t"}] {:min-priority 0}))))))

(deftest board-tags-test
  (is (= ["auth" "bug" "chore" "security"] (tf/board-tags tasks))))

(deftest board-priorities-test
  (testing "distinct priorities, most urgent first"
    (is (= [5 2 0 -1] (tf/board-priorities tasks)))))

(deftest toggle-tag-test
  (testing "adds an absent tag"
    (is (= #{"a" "b"} (:tags (tf/toggle-tag {:tags #{"a"}} "b")))))
  (testing "removes a present tag"
    (is (= #{} (:tags (tf/toggle-tag {:tags #{"a"}} "a")))))
  (testing "works on a filter with no tags yet"
    (is (= #{"a"} (:tags (tf/toggle-tag {} "a"))))))

(deftest active-filter-test
  (are [f expected] (= expected (tf/active? f))
    tf/default-filter                         false
    (assoc tf/default-filter :search "  ")    false
    (assoc tf/default-filter :search "x")     true
    (assoc tf/default-filter :tags #{"a"})    true
    (assoc tf/default-filter :min-priority 0) true))

(deftest default-db-has-board-filter-test
  (is (= tf/default-filter (:board-filter differ.client.db/default-db))))
