(ns differ.client.board-dnd
  "Pure rules for kanban columns and dragging task cards between them.")

(def default-columns
  "Columns shown before the board's own statuses have loaded."
  ["pending" "needs_owner" "planning" "plan_review" "ready" "in_progress"
   "blocked" "testing" "in_review" "done" "rejected"])

(defn board-columns
  "Column statuses, in order, for a board with `statuses` (nil = not loaded
   yet). The computed 'blocked' column sits right after in_progress (or
   last when there is none). Tasks in statuses no longer on the board are
   appended in first-seen order so they don't silently vanish. done and
   rejected are dropped unless `show-done`."
  [statuses tasks show-done]
  (let [base (if statuses
               (let [[head tail] (split-with #(not= "in_progress" %) statuses)]
                 (if (seq tail)
                   (concat head [(first tail) "blocked"] (rest tail))
                   (concat statuses ["blocked"])))
               default-columns)
        extras (->> tasks (map :status) distinct (remove (set base)))]
    (vec (cond->> (concat base extras)
           (not show-done) (remove #{"done" "rejected"})))))

(defn drop-update
  "The task update for dropping `task` on the `target` status column, or nil
   when the drop does nothing: same column, nothing dragged, or the computed
   'blocked' column (blocked comes from dependencies, not a stored status)."
  [task target]
  (when (and task
             (not= "blocked" target)
             (not= target (:status task)))
    {:status target}))
