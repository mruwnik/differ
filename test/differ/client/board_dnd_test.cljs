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
