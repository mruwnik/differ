(ns differ.client.board-dnd-test
  (:require [clojure.test :refer [deftest is are]]
            [differ.client.board-dnd :as dnd]))

(deftest drop-update-test
  (are [task target expected] (= expected (dnd/drop-update task target))
    {:id "t" :status "pending"}     "testing"     {:status "testing"}
    {:id "t" :status "in_progress"} "needs_owner" {:status "needs_owner"}
    {:id "t" :status "blocked"}     "done"        {:status "done"}
    ;; same column: nothing to do
    {:id "t" :status "testing"}     "testing"     nil
    ;; blocked is computed from dependencies, never a stored status
    {:id "t" :status "pending"}     "blocked"     nil
    ;; nothing being dragged
    nil                             "done"        nil))

(deftest board-columns-test
  (are [statuses tasks show-done expected]
       (= expected (dnd/board-columns statuses tasks show-done))
    ;; blocked (computed) sits right after in_progress; done/rejected hidden
    ["pending" "in_progress" "in_review" "done" "rejected"] [] false
    ["pending" "in_progress" "blocked" "in_review"]

    ["pending" "in_progress" "in_review" "done" "rejected"] [] true
    ["pending" "in_progress" "blocked" "in_review" "done" "rejected"]

    ;; custom column keeps the board's order
    ["pending" "qa" "in_progress" "done"] [] true
    ["pending" "qa" "in_progress" "blocked" "done"]

    ;; no in_progress: blocked goes last among the board's statuses
    ["todo" "doing" "done"] [] true
    ["todo" "doing" "done" "blocked"]

    ;; tasks in statuses no longer on the board still get a column
    ["pending" "in_progress" "done"] [{:status "legacy"} {:status "pending"} {:status "legacy"}] true
    ["pending" "in_progress" "blocked" "done" "legacy"]

    ;; board statuses not loaded yet: fall back to the default lifecycle
    nil [] true
    dnd/default-columns))
