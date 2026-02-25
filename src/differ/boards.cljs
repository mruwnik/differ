(ns differ.boards
  "Kanban board, task, and note CRUD operations."
  (:require [clojure.string :as str]
            [differ.db :as db]
            [differ.util :as util]))

;; ============================================================================
;; Row converters
;; ============================================================================

(defn- row->board [^js row]
  (when row
    {:id (.-id row)
     :repo-path (.-repo_path row)
     :statuses (db/safe-parse-json (.-statuses row) ["pending" "in_progress" "done" "rejected" "in_review"])
     :created-at (.-created_at row)
     :updated-at (.-updated_at row)}))

(defn- row->task [^js row]
  (when row
    {:id (.-id row)
     :board-id (.-board_id row)
     :title (.-title row)
     :description (.-description row)
     :status (.-status row)
     :worker-name (.-worker_name row)
     :worker-id (.-worker_id row)
     :persist (= 1 (.-persist row))
     :created-at (.-created_at row)
     :updated-at (.-updated_at row)}))

(defn- row->note [^js row]
  (when row
    {:id (.-id row)
     :task-id (.-task_id row)
     :author (.-author row)
     :content (.-content row)
     :created-at (.-created_at row)}))

;; ============================================================================
;; Task dependency operations
;; ============================================================================

(defn set-task-dependencies!
  "Replace all dependencies for a task. blocked-by is a vector of task IDs."
  [task-id blocked-by]
  (let [^js del-stmt (.prepare (db/db)
                               "DELETE FROM task_dependencies WHERE task_id = ?")]
    (.run del-stmt task-id))
  (when (seq blocked-by)
    (let [^js ins-stmt (.prepare (db/db)
                                 "INSERT INTO task_dependencies (task_id, depends_on_task_id) VALUES (?, ?)")]
      (doseq [dep-id blocked-by]
        (.run ins-stmt task-id dep-id)))))

(defn get-task-dependencies
  "Get list of task IDs that this task depends on."
  [task-id]
  (let [^js stmt (.prepare (db/db)
                           "SELECT depends_on_task_id FROM task_dependencies WHERE task_id = ?")
        rows (.all stmt task-id)]
    (mapv #(.-depends_on_task_id %) rows)))

(defn get-tasks-blocked-by
  "Get list of task IDs that are blocked by the given task."
  [task-id]
  (let [^js stmt (.prepare (db/db)
                           "SELECT task_id FROM task_dependencies WHERE depends_on_task_id = ?")
        rows (.all stmt task-id)]
    (mapv #(.-task_id %) rows)))

(defn- enrich-task
  "Add effective status and blocked-by to a single task."
  [task]
  (when task
    (let [^js stmt (.prepare (db/db)
                             "SELECT td.depends_on_task_id FROM task_dependencies td
                              JOIN tasks t ON t.id = td.depends_on_task_id
                              WHERE td.task_id = ? AND t.status != 'done'")
          rows (.all stmt (:id task))
          unresolved (mapv #(.-depends_on_task_id %) rows)
          effective-status (if (seq unresolved) "blocked" (:status task))]
      (assoc task
             :status effective-status
             :blocked-by unresolved))))

(defn- enrich-tasks
  "Batch-enrich multiple tasks. Uses a single query instead of N+1."
  [tasks]
  (if (empty? tasks)
    tasks
    (let [ids (mapv :id tasks)
          placeholders (str/join "," (repeat (count ids) "?"))
          sql (str "SELECT td.task_id, td.depends_on_task_id
                    FROM task_dependencies td
                    JOIN tasks t ON t.id = td.depends_on_task_id
                    WHERE td.task_id IN (" placeholders ") AND t.status != 'done'")
          ^js stmt (.prepare (db/db) sql)
          rows (.all stmt (to-array ids))
          deps-by-task (group-by #(.-task_id %) rows)]
      (mapv (fn [task]
              (let [unresolved (mapv #(.-depends_on_task_id %) (get deps-by-task (:id task) []))
                    effective-status (if (seq unresolved) "blocked" (:status task))]
                (assoc task :status effective-status :blocked-by unresolved)))
            tasks))))

;; ============================================================================
;; Board operations
;; ============================================================================

(defn get-board
  "Get board by ID."
  [board-id]
  (let [^js stmt (.prepare (db/db) "SELECT * FROM boards WHERE id = ?")]
    (row->board (.get stmt board-id))))

(defn get-board-by-repo
  "Get board by repo path."
  [repo-path]
  (let [^js stmt (.prepare (db/db) "SELECT * FROM boards WHERE repo_path = ?")]
    (row->board (.get stmt repo-path))))

(defn list-boards
  "List all boards, ordered by updated_at DESC."
  []
  (let [^js stmt (.prepare (db/db) "SELECT * FROM boards ORDER BY updated_at DESC")]
    (mapv row->board (.all stmt))))

(defn get-or-create-board!
  "Return existing board for repo-path, or create a new one.
   Uses INSERT OR IGNORE to avoid race conditions with concurrent creates."
  [repo-path]
  (let [id (util/gen-uuid)
        now (util/now-iso)
        ^js stmt (.prepare (db/db)
                           "INSERT OR IGNORE INTO boards (id, repo_path, created_at, updated_at)
                            VALUES (?, ?, ?, ?)")]
    (.run stmt id repo-path now now)
    (get-board-by-repo repo-path)))

;; ============================================================================
;; Task operations
;; ============================================================================

(defn- get-task-raw
  "Get single task by ID without enrichment (raw DB status)."
  [task-id]
  (let [^js stmt (.prepare (db/db) "SELECT * FROM tasks WHERE id = ?")]
    (row->task (.get stmt task-id))))

(defn get-task
  "Get single task by ID. Returns effective status and blocked-by."
  [task-id]
  (enrich-task (get-task-raw task-id)))

(defn create-task!
  "Create a task, auto-creating board for repo-path if needed.
   Accepts optional :blocked-by vector of task IDs."
  [{:keys [repo-path title description blocked-by]}]
  (let [board (get-or-create-board! repo-path)
        id (util/gen-uuid)
        now (util/now-iso)
        ^js stmt (.prepare (db/db)
                           "INSERT INTO tasks (id, board_id, title, description, created_at, updated_at)
                            VALUES (?, ?, ?, ?, ?, ?)")]
    (.run stmt id (:id board) title description now now)
    ;; Update board timestamp
    (let [^js update-stmt (.prepare (db/db) "UPDATE boards SET updated_at = ? WHERE id = ?")]
      (.run update-stmt now (:id board)))
    ;; Set dependencies if provided
    (when (seq blocked-by)
      (set-task-dependencies! id blocked-by))
    (get-task id)))

(defn take-task!
  "Atomically claim a pending task. Fails if task is not pending or blocked.
   Uses better-sqlite3 transaction for atomicity."
  [{:keys [task-id worker-name worker-id note]}]
  (let [txn (.transaction (db/db)
                          (fn []
                            (let [^js read-stmt (.prepare (db/db) "SELECT * FROM tasks WHERE id = ?")
                                  ^js row (.get read-stmt task-id)]
                              (when-not row
                                (throw (js/Error. (str "Task not found: " task-id))))
                              (when-not (= "pending" (.-status row))
                                (throw (js/Error. (str "Task is not pending (status: " (.-status row) ")"))))
                              ;; Check for unresolved dependencies
                              (let [^js deps-stmt (.prepare (db/db)
                                                            "SELECT COUNT(*) as count FROM task_dependencies td
                                                             JOIN tasks t ON t.id = td.depends_on_task_id
                                                             WHERE td.task_id = ? AND t.status != 'done'")
                                    ^js deps-row (.get deps-stmt task-id)]
                                (when (pos? (.-count deps-row))
                                  (throw (js/Error. "Task is blocked by unresolved dependencies"))))
                              (let [now (util/now-iso)
                                    ^js update-stmt (.prepare (db/db)
                                                              "UPDATE tasks SET status = 'in_progress', worker_name = ?, worker_id = ?, updated_at = ?
                                                               WHERE id = ?")]
                                (.run update-stmt worker-name worker-id now task-id)
                                ;; Update board timestamp
                                (let [^js board-stmt (.prepare (db/db) "UPDATE boards SET updated_at = ? WHERE id = ?")]
                                  (.run board-stmt now (.-board_id row)))
                                ;; Add note if provided
                                (when note
                                  (let [note-id (util/gen-uuid)
                                        ^js note-stmt (.prepare (db/db)
                                                                "INSERT INTO task_notes (id, task_id, author, content, created_at)
                                                                 VALUES (?, ?, ?, ?, ?)")]
                                    (.run note-stmt note-id task-id worker-name note now)))))))]
    (txn)
    (get-task task-id)))

(defn update-task!
  "Update task fields. Validates status against board's allowed statuses.
   Optionally adds a note when :note and :author are provided.
   Accepts optional :blocked-by to set task dependencies.
   The :description field uses contains? to distinguish 'not provided' from 'set to nil'.
   Wrapped in a transaction to prevent race conditions between concurrent agents."
  [task-id opts]
  (let [{:keys [status title persist note author]} opts
        txn (.transaction (db/db)
                          (fn []
                            (let [task (get-task-raw task-id)]
                              (when-not task
                                (throw (js/Error. (str "Task not found: " task-id))))
                              ;; Validate status if provided
                              (when status
                                (let [board (get-board (:board-id task))]
                                  (when-not (some #{status} (:statuses board))
                                    (throw (js/Error. (str "Invalid status '" status "'. Allowed: " (str/join ", " (:statuses board))))))))
                              (let [now (util/now-iso)
                                    new-status (or status (:status task))
                                    new-title (or title (:title task))
                                    new-description (if (contains? opts :description)
                                                      (:description opts)
                                                      (:description task))
                                    new-persist (if (contains? opts :persist)
                                                  (if persist 1 0)
                                                  (if (:persist task) 1 0))
                                    ^js stmt (.prepare (db/db)
                                                       "UPDATE tasks SET status = ?, title = ?, description = ?, persist = ?, updated_at = ?
                                                        WHERE id = ?")]
                                (.run stmt new-status new-title new-description new-persist now task-id)
                                ;; Update board timestamp
                                (let [^js board-stmt (.prepare (db/db) "UPDATE boards SET updated_at = ? WHERE id = ?")]
                                  (.run board-stmt now (:board-id task)))
                                ;; Update dependencies if provided
                                (when (contains? opts :blocked-by)
                                  (set-task-dependencies! task-id (:blocked-by opts)))
                                ;; Add note if provided
                                (when (and note author)
                                  (let [note-id (util/gen-uuid)
                                        ^js note-stmt (.prepare (db/db)
                                                                "INSERT INTO task_notes (id, task_id, author, content, created_at)
                                                                 VALUES (?, ?, ?, ?, ?)")]
                                    (.run note-stmt note-id task-id author note now)))))))]
    (txn)
    (get-task task-id)))

;; ============================================================================
;; Note operations
;; ============================================================================

(defn add-note!
  "Add a note to a task. Updates task and board timestamps.
   Wrapped in a transaction for consistency with take-task! and update-task!."
  [{:keys [task-id author content]}]
  (let [id (util/gen-uuid)
        txn (.transaction (db/db)
                          (fn []
                            (let [task (get-task task-id)]
                              (when-not task
                                (throw (js/Error. (str "Task not found: " task-id))))
                              (let [now (util/now-iso)
                                    ^js stmt (.prepare (db/db)
                                                       "INSERT INTO task_notes (id, task_id, author, content, created_at)
                                                        VALUES (?, ?, ?, ?, ?)")]
                                (.run stmt id task-id author content now)
                                ;; Update task timestamp
                                (let [^js task-stmt (.prepare (db/db) "UPDATE tasks SET updated_at = ? WHERE id = ?")]
                                  (.run task-stmt now task-id))
                                ;; Update board timestamp
                                (let [^js board-stmt (.prepare (db/db) "UPDATE boards SET updated_at = ? WHERE id = ?")]
                                  (.run board-stmt now (:board-id task)))))))]
    (txn)
    (let [^js get-stmt (.prepare (db/db) "SELECT * FROM task_notes WHERE id = ?")]
      (row->note (.get get-stmt id)))))

(defn list-notes
  "List notes for a task, ordered by created_at ASC."
  [task-id]
  (let [^js stmt (.prepare (db/db)
                           "SELECT * FROM task_notes WHERE task_id = ? ORDER BY created_at ASC")]
    (mapv row->note (.all stmt task-id))))

;; ============================================================================
;; Task listing (after note operations for forward reference)
;; ============================================================================

(defn list-tasks
  "List tasks for a board with optional filters.
   Options:
     :status      - vector of status strings to include
     :worker-id   - filter by worker
     :show-done   - if false (default), exclude done+rejected
     :include-notes - if true, attach :notes array to each task"
  [board-id {:keys [status worker-id show-done include-notes]}]
  (let [conditions ["t.board_id = ?"]
        params [board-id]
        ;; Build status filter
        [conditions params]
        (if (seq status)
          (let [placeholders (str/join "," (repeat (count status) "?"))]
            [(conj conditions (str "t.status IN (" placeholders ")"))
             (into params status)])
          (if-not show-done
            [(conj conditions "t.status NOT IN ('done', 'rejected')")
             params]
            [conditions params]))
        ;; Build worker filter
        [conditions params]
        (if worker-id
          [(conj conditions "t.worker_id = ?")
           (conj params worker-id)]
          [conditions params])
        where-clause (str/join " AND " conditions)
        sql (str "SELECT t.* FROM tasks t WHERE " where-clause " ORDER BY t.created_at ASC")
        ^js stmt (.prepare (db/db) sql)
        rows (.all stmt (to-array params))
        tasks (enrich-tasks (mapv row->task rows))]
    (if include-notes
      (mapv (fn [task]
              (assoc task :notes (list-notes (:id task))))
            tasks)
      tasks)))

;; ============================================================================
;; Constants
;; ============================================================================

(def update-task-allowed-keys
  "Keys that callers may pass through to update-task!. Used by both the REST API
   and the MCP handler to whitelist incoming fields."
  #{:status :title :description :persist :note :author :blocked-by})

;; ============================================================================
;; Summary operations
;; ============================================================================

(defn board-summary
  "Get board info with task count per status."
  [board-id]
  (let [board (get-board board-id)]
    (when board
      (let [^js stmt (.prepare (db/db)
                               "SELECT status, COUNT(*) as count FROM tasks
                                WHERE board_id = ? GROUP BY status")
            rows (.all stmt board-id)
            status-counts (into {} (map (fn [^js row]
                                          [(.-status row) (.-count row)])
                                        rows))]
        (assoc board :task-counts status-counts)))))

(defn list-boards-with-summary
  "List all boards with task counts per status."
  []
  (let [boards (list-boards)]
    (mapv (fn [board]
            (let [^js stmt (.prepare (db/db)
                                     "SELECT status, COUNT(*) as count FROM tasks
                                      WHERE board_id = ? GROUP BY status")
                  rows (.all stmt (:id board))
                  status-counts (into {} (map (fn [^js row]
                                                [(.-status row) (.-count row)])
                                              rows))]
              (assoc board :task-counts status-counts)))
          boards)))
