(ns differ.client.board-dnd
  "Pure rules for dragging task cards between kanban columns.")

(defn drop-update
  "The task update for dropping `task` on the `target` status column, or nil
   when the drop does nothing: same column, nothing dragged, or the computed
   'blocked' column (blocked comes from dependencies, not a stored status)."
  [task target]
  (when (and task
             (not= "blocked" target)
             (not= target (:status task)))
    {:status target}))
