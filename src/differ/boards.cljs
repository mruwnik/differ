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
     :statuses (db/safe-parse-json (.-statuses row) db/default-board-statuses)
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
     :assignee (.-assignee row)
     :persist (= 1 (.-persist row))
     :priority (or (.-priority row) 0)
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
;; Priority, tag & assignee helpers
;; ============================================================================

(defn validate-priority
  "Throw unless priority is nil (not provided) or an integer.
   Higher priority = more urgent; default is 0, negatives mean 'low'."
  [priority]
  (when-not (or (nil? priority) (integer? priority))
    (throw (js/Error. (str "priority must be an integer, got: " (pr-str priority))))))

(defn normalize-assignee
  "Trimmed assignee name, or nil when absent/blank (i.e. unassigned)."
  [assignee]
  (when-not (str/blank? assignee)
    (str/trim assignee)))

(defn normalize-tags
  "Trim, lowercase, drop blanks, dedupe and sort a seq of tag strings."
  [tags]
  (->> tags
       (map (comp str/lower-case str/trim str))
       (remove str/blank?)
       distinct
       sort
       vec))

(defn set-task-tags!
  "Replace all tags for a task."
  [task-id tags]
  (.run (.prepare (db/db) "DELETE FROM task_tags WHERE task_id = ?") task-id)
  (let [^js ins-stmt (.prepare (db/db) "INSERT INTO task_tags (task_id, tag) VALUES (?, ?)")]
    (doseq [tag (normalize-tags tags)]
      (.run ins-stmt task-id tag))))

(defn- tags-in-clause
  "SQL condition (on task alias t) matching tasks having any of `tags`,
   plus its params. Returns nil when tags is empty."
  [tags]
  (let [tags (normalize-tags tags)]
    (when (seq tags)
      [(str "EXISTS (SELECT 1 FROM task_tags tt WHERE tt.task_id = t.id AND tt.tag IN ("
            (str/join "," (repeat (count tags) "?")) "))")
       tags])))

(def ^:private task-order-sql
  "Queue/listing order: most urgent first, then oldest. rowid breaks
   created_at ties between tasks created in the same millisecond."
  " ORDER BY t.priority DESC, t.created_at ASC, t.rowid ASC")

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
    (mapv (fn [^js row] (.-depends_on_task_id row)) rows)))

(defn get-tasks-blocked-by
  "Get list of task IDs that are blocked by the given task."
  [task-id]
  (let [^js stmt (.prepare (db/db)
                           "SELECT task_id FROM task_dependencies WHERE depends_on_task_id = ?")
        rows (.all stmt task-id)]
    (mapv (fn [^js row] (.-task_id row)) rows)))

(def ^:private sqlite-param-chunk-size
  "Conservative ceiling under SQLite's SQLITE_MAX_VARIABLE_NUMBER (default 32766
   on modern builds, 999 on older ones). Keeps us well below either."
  500)

(defn- dep-rows-batch
  "Core of the task-dependency batch queries. For each id in `task-ids`,
   collects the value of `value-col` from every task_dependencies row whose
   `key-col` matches. Returns {key-col-value [value-col-value ...]}; ids
   with no matching rows are omitted from the map.

   Wide frontiers are chunked to stay under SQLite's parameter limit.
   Column names are hardcoded callsite strings ('task_id' /
   'depends_on_task_id'), never user input — no injection risk."
  [task-ids key-col value-col]
  (if (empty? task-ids)
    {}
    (reduce
     (fn [acc chunk]
       (let [placeholders (str/join "," (repeat (count chunk) "?"))
             sql (str "SELECT " key-col ", " value-col " FROM task_dependencies"
                      " WHERE " key-col " IN (" placeholders ")")
             ^js stmt (.prepare (db/db) sql)
             rows (.all stmt (to-array chunk))]
         (reduce (fn [m ^js r]
                   (update m (aget r key-col) (fnil conj []) (aget r value-col)))
                 acc
                 rows)))
     {}
     (partition-all sqlite-param-chunk-size task-ids))))

(defn get-task-dependencies-batch
  "Batch variant of get-task-dependencies. Given a seq of task IDs, returns
   {parent-id [child-ids]} using a single SQL query. IDs with no deps are
   omitted from the map. Used by dep-graph traversal to avoid N+1 queries."
  [task-ids]
  (dep-rows-batch task-ids "task_id" "depends_on_task_id"))

(defn get-tasks-blocked-by-batch
  "Batch variant of get-tasks-blocked-by. Given a seq of task IDs, returns
   {parent-id [child-ids]} where child-ids are tasks blocked by parent-id.
   Single SQL query. IDs with no dependents are omitted from the map."
  [task-ids]
  (dep-rows-batch task-ids "depends_on_task_id" "task_id"))

(defn- tags-by-task
  "Map of task-id -> sorted tag vector for the given task IDs (single query).
   Tasks without tags are omitted."
  [ids]
  (let [placeholders (str/join "," (repeat (count ids) "?"))
        ^js stmt (.prepare (db/db)
                           (str "SELECT task_id, tag FROM task_tags WHERE task_id IN ("
                                placeholders ") ORDER BY tag ASC"))]
    (reduce (fn [m ^js r] (update m (.-task_id r) (fnil conj []) (.-tag r)))
            {}
            (.all stmt (to-array ids)))))

(defn- enrich-tasks
  "Batch-enrich multiple tasks with effective status, blocked-by and tags.
   Uses one query per concern instead of N+1."
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
          deps-by-task (group-by (fn [^js r] (.-task_id r)) rows)
          tags (tags-by-task ids)]
      (mapv (fn [task]
              (let [unresolved (mapv (fn [^js r] (.-depends_on_task_id r)) (get deps-by-task (:id task) []))
                    effective-status (if (seq unresolved) "blocked" (:status task))]
                (assoc task
                       :status effective-status
                       :blocked-by unresolved
                       :tags (get tags (:id task) []))))
            tasks))))

(defn- enrich-task
  "Add effective status, blocked-by and tags to a single task."
  [task]
  (when task
    (first (enrich-tasks [task]))))

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
        ;; Explicitly set statuses rather than relying on the column DEFAULT:
        ;; CREATE TABLE IF NOT EXISTS never updates an existing table's default,
        ;; so on DBs created by an older DDL the default is a stale status list.
        ^js stmt (.prepare (db/db)
                           "INSERT OR IGNORE INTO boards (id, repo_path, statuses, created_at, updated_at)
                            VALUES (?, ?, ?, ?, ?)")]
    (.run stmt id repo-path (db/statuses-json db/default-board-statuses) now now)
    (get-board-by-repo repo-path)))

;; ============================================================================
;; Task operations
;; ============================================================================

(defn- get-task-raw
  "Get single task by ID without enrichment (raw DB status)."
  [task-id]
  (let [^js stmt (.prepare (db/db) "SELECT * FROM tasks WHERE id = ?")]
    (row->task (.get stmt task-id))))

(def ^:private ambiguous-candidate-limit 5)

(defn- ambiguous-prefix-error
  [prefix ^js rows]
  (let [total (.-length rows)
        shown (->> (array-seq rows)
                   (take ambiguous-candidate-limit)
                   (map (fn [^js r] (str (.-id r) " (" (.-title r) ")"))))
        more (- total ambiguous-candidate-limit)]
    (js/Error. (str "Task id prefix '" prefix "' is ambiguous; it matches " total " tasks: "
                    (str/join ", " shown)
                    (when (pos? more) (str ", and " more " more"))))))

(defn- find-task-id
  "Resolve an exact task id or unique id prefix to the full id. Returns nil
   when nothing matches; throws when the prefix matches several tasks.
   Uses substr rather than LIKE so `%`/`_` in the input match literally."
  [task-id]
  (when-not (str/blank? task-id)
    (let [^js exact (.get (.prepare (db/db) "SELECT id FROM tasks WHERE id = ?") task-id)]
      (if exact
        (.-id exact)
        (let [^js rows (.all (.prepare (db/db)
                                       "SELECT id, title FROM tasks WHERE substr(id, 1, ?) = ? ORDER BY id")
                             (count task-id) task-id)]
          (case (.-length rows)
            0 nil
            1 (.-id (aget rows 0))
            (throw (ambiguous-prefix-error task-id rows))))))))

(defn resolve-task-id
  "Resolve an exact task id or unique id prefix to the full id. Exact match
   wins; throws `Task not found` when nothing matches and an ambiguity error
   listing candidates when the prefix matches several tasks."
  [task-id]
  (or (find-task-id task-id)
      (throw (js/Error. (str "Task not found: " task-id)))))

(defn get-task
  "Get single task by ID or unique ID prefix. Returns effective status and
   blocked-by, or nil when no task matches."
  [task-id]
  (some-> (find-task-id task-id) get-task-raw enrich-task))

(defn- validate-board-status
  "Throw unless status is in the board's allowed statuses."
  [board status]
  (when-not (some #{status} (:statuses board))
    (throw (js/Error. (str "Invalid status '" status "'. Allowed: "
                           (str/join ", " (:statuses board)))))))

(defn create-task!
  "Create a task, auto-creating board for repo-path if needed.
   Accepts optional :blocked-by vector of task IDs, :priority (integer,
   higher = more urgent, default 0), :tags (seq of strings) and :assignee
   (agent name; only that agent may claim the task)."
  [{:keys [repo-path title description blocked-by priority tags assignee]}]
  (validate-priority priority)
  (let [blocked-by (mapv resolve-task-id blocked-by)
        board (get-or-create-board! repo-path)
        id (util/gen-uuid)
        now (util/now-iso)
        txn (.transaction (db/db)
                          (fn []
                            (let [^js stmt (.prepare (db/db)
                                                     "INSERT INTO tasks (id, board_id, title, description, priority, assignee, created_at, updated_at)
                                                      VALUES (?, ?, ?, ?, ?, ?, ?, ?)")]
                              (.run stmt id (:id board) title description (or priority 0)
                                    (normalize-assignee assignee) now now))
                            (let [^js update-stmt (.prepare (db/db) "UPDATE boards SET updated_at = ? WHERE id = ?")]
                              (.run update-stmt now (:id board)))
                            (when (seq blocked-by)
                              (set-task-dependencies! id blocked-by))
                            (when (seq tags)
                              (set-task-tags! id tags))))]
    (txn)
    (get-task id)))

(defn- find-next-available-task-row
  "Find the most urgent (highest priority, then oldest) unclaimed task in the
   given queue with no unresolved dependencies, that is unassigned or assigned
   to `worker-name`, optionally restricted to tasks having any of `tags`.
   Returns raw JS row or nil. Must be called inside a transaction."
  [board-id queue-status worker-name tags]
  (let [[tag-sql tag-params] (tags-in-clause tags)
        sql (str "SELECT t.* FROM tasks t
                  WHERE t.board_id = ?
                  AND t.status = ?
                  AND t.worker_name IS NULL
                  AND (t.assignee IS NULL OR t.assignee = ?)
                  AND NOT EXISTS (
                    SELECT 1 FROM task_dependencies td
                    JOIN tasks dep ON dep.id = td.depends_on_task_id
                    WHERE td.task_id = t.id AND dep.status != 'done'
                  )"
                 (when tag-sql (str " AND " tag-sql))
                 task-order-sql
                 " LIMIT 1")
        ^js stmt (.prepare (db/db) sql)]
    (.get stmt (to-array (into [board-id queue-status worker-name] tag-params)))))

(defn take-task!
  "Atomically claim a task from a queue.

   :status  - queue to pull from (default \"pending\")
   :move-to - status set on claim (default \"in_progress\")

   With :task-id, claims that specific task (must be in the queue, unclaimed,
   and unblocked). Otherwise auto-assigns the highest-priority (then oldest)
   available task on the board identified by :repo-path, restricted to tasks
   having any of :tags when given (:tags is ignored with :task-id). Tasks
   with an assignee can only be claimed by that worker-name. Claiming sets the worker; a status change
   via update-task! releases it again, so each lifecycle phase is claimed
   separately. Uses better-sqlite3 transaction for atomicity."
  [{:keys [task-id repo-path worker-name worker-id note status move-to tags]}]
  (when (str/blank? worker-name)
    (throw (js/Error. "worker-name is required")))
  (when (and (nil? task-id) (nil? repo-path))
    (throw (js/Error. "Either task-id or repo-path is required")))
  (let [task-id (when task-id (resolve-task-id task-id))
        queue-status (or status "pending")
        target-status (or move-to "in_progress")
        txn (.transaction (db/db)
                          (fn []
                ;; When task-id is provided it takes precedence; repo-path is ignored.
                            (let [^js row
                                  (if task-id
                                    (let [^js read-stmt (.prepare (db/db) "SELECT * FROM tasks WHERE id = ?")
                                          ^js r (.get read-stmt task-id)]
                                      (when-not r
                                        (throw (js/Error. (str "Task not found: " task-id))))
                                      (let [board (get-board (.-board_id r))]
                                        (validate-board-status board queue-status)
                                        (validate-board-status board target-status))
                                      (when-not (= queue-status (.-status r))
                                        (throw (js/Error. (str "Task is not in queue '" queue-status
                                                               "' (status: " (.-status r) ")"))))
                                      (when (.-worker_name r)
                                        (throw (js/Error. (str "Task is already claimed by " (.-worker_name r)))))
                                      (when (and (.-assignee r) (not= (.-assignee r) worker-name))
                                        (throw (js/Error. (str "Task is assigned to " (.-assignee r)))))
                                      r)
                        ;; Auto-assign: find next available task in the queue
                                    (let [board (get-board-by-repo repo-path)]
                                      (when-not board
                                        (throw (js/Error. (str "No board found for repo: " repo-path))))
                                      (validate-board-status board queue-status)
                                      (validate-board-status board target-status)
                                      (let [^js r (find-next-available-task-row (:id board) queue-status worker-name tags)]
                                        (when-not r
                                          (throw (js/Error. (str "No available tasks in queue '" queue-status "'"
                                                                 (when (seq tags)
                                                                   (str " with tags " (str/join ", " (normalize-tags tags))))))))
                                        r)))]
                  ;; Check for unresolved dependencies (only needed for explicit task-id;
                  ;; auto-assign already filters these out in the SQL query)
                              (when task-id
                                (let [^js deps-stmt (.prepare (db/db)
                                                              "SELECT COUNT(*) as count FROM task_dependencies td
                                                   JOIN tasks t ON t.id = td.depends_on_task_id
                                                   WHERE td.task_id = ? AND t.status != 'done'")
                                      ^js deps-row (.get deps-stmt task-id)]
                                  (when (pos? (.-count deps-row))
                                    (throw (js/Error. "Task is blocked by unresolved dependencies")))))
                              (let [tid (.-id row)
                                    now (util/now-iso)
                                    ^js update-stmt (.prepare (db/db)
                                                              "UPDATE tasks SET status = ?, worker_name = ?, worker_id = ?, updated_at = ?
                                                   WHERE id = ?")]
                                (.run update-stmt target-status worker-name worker-id now tid)
                    ;; Update board timestamp
                                (let [^js board-stmt (.prepare (db/db) "UPDATE boards SET updated_at = ? WHERE id = ?")]
                                  (.run board-stmt now (.-board_id row)))
                    ;; Add note if provided
                                (when note
                                  (let [note-id (util/gen-uuid)
                                        ^js note-stmt (.prepare (db/db)
                                                                "INSERT INTO task_notes (id, task_id, author, content, created_at)
                                                     VALUES (?, ?, ?, ?, ?)")]
                                    (.run note-stmt note-id tid worker-name note now)))
                                tid))))]
    (get-task (txn))))

(defn update-task!
  "Update task fields. Validates status against board's allowed statuses.
   Changing status releases the task's worker (worker_name/worker_id set to NULL)
   so the next phase can be claimed via take-task!.
   Optionally adds a note when :note and :author are provided.
   Accepts optional :blocked-by to set task dependencies, :priority (integer),
   :tags (replaces all tags; [] clears them) and :assignee (nil/blank clears;
   unlike the worker, it survives status changes).
   :description and :assignee use contains? to distinguish 'not provided' from 'set to nil'.
   Wrapped in a transaction to prevent race conditions between concurrent agents."
  [task-id opts]
  (let [{:keys [status title persist note author priority]} opts
        _ (validate-priority priority)
        task-id (resolve-task-id task-id)
        opts (cond-> opts
               (contains? opts :blocked-by) (update :blocked-by #(mapv resolve-task-id %)))
        txn (.transaction (db/db)
                          (fn []
                            (let [task (get-task-raw task-id)]
                              (when-not task
                                (throw (js/Error. (str "Task not found: " task-id))))
                              ;; Validate status if provided
                              (when status
                                (let [board (get-board (:board-id task))]
                                  (validate-board-status board status)))
                              (let [now (util/now-iso)
                                    new-status (or status (:status task))
                                    status-changed? (and status (not= status (:status task)))
                                    new-worker-name (when-not status-changed? (:worker-name task))
                                    new-worker-id (when-not status-changed? (:worker-id task))
                                    new-title (or title (:title task))
                                    new-description (if (contains? opts :description)
                                                      (:description opts)
                                                      (:description task))
                                    new-persist (if (contains? opts :persist)
                                                  (if persist 1 0)
                                                  (if (:persist task) 1 0))
                                    new-priority (if (some? priority) priority (:priority task))
                                    new-assignee (if (contains? opts :assignee)
                                                   (normalize-assignee (:assignee opts))
                                                   (:assignee task))
                                    ^js stmt (.prepare (db/db)
                                                       "UPDATE tasks SET status = ?, title = ?, description = ?, persist = ?, priority = ?, worker_name = ?, worker_id = ?, assignee = ?, updated_at = ?
                                                        WHERE id = ?")]
                                (.run stmt new-status new-title new-description new-persist new-priority new-worker-name new-worker-id new-assignee now task-id)
                                ;; Update board timestamp
                                (let [^js board-stmt (.prepare (db/db) "UPDATE boards SET updated_at = ? WHERE id = ?")]
                                  (.run board-stmt now (:board-id task)))
                                ;; Update dependencies if provided
                                (when (contains? opts :blocked-by)
                                  (set-task-dependencies! task-id (:blocked-by opts)))
                                (when (contains? opts :tags)
                                  (set-task-tags! task-id (:tags opts)))
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
  (let [task-id (resolve-task-id task-id)
        id (util/gen-uuid)
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
  "List notes for a task (ID or unique ID prefix), ordered by created_at ASC."
  [task-id]
  (let [task-id (or (find-task-id task-id) task-id)
        ^js stmt (.prepare (db/db)
                           "SELECT * FROM task_notes WHERE task_id = ? ORDER BY created_at ASC")]
    (mapv row->note (.all stmt task-id))))

;; ============================================================================
;; Task listing (after note operations for forward reference)
;; ============================================================================

(def ^:private iso-timestamp-re
  "Date-only (taken as midnight UTC), or date+time with an explicit Z or
   offset. A bare local time is rejected: JS would read it in the server's
   timezone, which the caller can't see."
  #"^\d{4}-\d{2}-\d{2}(T\d{2}:\d{2}(:\d{2}(\.\d+)?)?(Z|[+-]\d{2}:\d{2}))?$")

(defn- normalize-updated-since
  "Validate an ISO-8601 timestamp and return it in the exact format
   util/now-iso writes (Date.toISOString: UTC, millis, Z), so a plain
   string comparison against updated_at is chronological."
  [ts]
  (let [ms (when (and (string? ts) (re-matches iso-timestamp-re ts))
             (js/Date.parse ts))]
    (when (or (nil? ms) (js/isNaN ms))
      (throw (ex-info (str "updated_since must be an ISO-8601 timestamp with timezone "
                           "(e.g. 2026-01-31T09:00:00Z) or a date (2026-01-31), got: "
                           (pr-str ts))
                      {:invalid-updated-since ts})))
    (.toISOString (js/Date. ms))))

(defn- validate-min-priority [p]
  (when-not (integer? p)
    (throw (ex-info (str "min_priority must be an integer, got: " (pr-str p))
                    {:invalid-min-priority p}))))

(defn- title-matches?
  "Case-insensitive substring match. Done in CLJS rather than SQL LIKE so
   % and _ in the query are literal and non-ASCII case folding works."
  [search task]
  (str/includes? (str/lower-case (or (:title task) "")) (str/lower-case search)))

(defn list-tasks
  "List tasks for a board with optional filters.
   Options:
     :status      - vector of status strings to include
     :worker-id   - filter by worker
     :assignee    - filter by assigned agent name
     :tags        - only tasks having any of these tags
     :show-done   - if false (default), exclude done+rejected
     :min-priority  - only tasks with priority >= this integer
     :updated-since - only tasks with updated_at >= this ISO-8601 timestamp
     :search        - case-insensitive substring match on title
     :include-notes - if true, attach :notes array to each task"
  [board-id {:keys [status worker-id assignee tags show-done include-notes
                    min-priority updated-since search]}]
  (when (some? min-priority) (validate-min-priority min-priority))
  (let [updated-since (some-> updated-since normalize-updated-since)
        conditions ["t.board_id = ?"]
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
        [conditions params]
        (if assignee
          [(conj conditions "t.assignee = ?")
           (conj params assignee)]
          [conditions params])
        ;; Build tag filter
        [conditions params]
        (if-let [[tag-sql tag-params] (tags-in-clause tags)]
          [(conj conditions tag-sql) (into params tag-params)]
          [conditions params])
        [conditions params]
        (if (some? min-priority)
          [(conj conditions "t.priority >= ?") (conj params min-priority)]
          [conditions params])
        [conditions params]
        (if updated-since
          [(conj conditions "t.updated_at >= ?") (conj params updated-since)]
          [conditions params])
        where-clause (str/join " AND " conditions)
        sql (str "SELECT t.* FROM tasks t WHERE " where-clause task-order-sql)
        ^js stmt (.prepare (db/db) sql)
        rows (.all stmt (to-array params))
        tasks (enrich-tasks
               (cond->> (mapv row->task rows)
                 (seq search) (filterv (partial title-matches? search))))]
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
  #{:status :title :description :persist :note :author :blocked-by :priority :tags :assignee})

;; ============================================================================
;; Dependency graph traversal
;; ============================================================================

(def dep-graph-fields
  "Fields allowed in get-upstream / get-downstream :fields options.
   :id is always included in the result regardless of :fields, and :depth
   is synthesized by the traversal (hop count). Listing :depth in :fields
   raises 'Unknown fields: depth' — it's not a column, so just omit it."
  #{:id :board-id :title :description :status :worker-name :worker-id :assignee
    :persist :priority :tags :blocked-by :created-at :updated-at})

(def default-dep-graph-fields
  "Fields returned on each related task when :fields is not specified."
  [:title :status :blocked-by])

(defn- traverse-dep-graph
  "BFS over the dependency graph from start-id, following next-batch-fn (a
   function from a seq of task IDs to a map of {parent-id [child-ids]}).
   One SQL round-trip per BFS level.

   Returns a vector of [id depth] pairs in BFS order. start-id appears in the
   result only if it's reachable from itself through a cycle (so callers can
   compose `(some #{task-id} (map :id (get-upstream task-id {})))` as a
   cycle-detection check).

   Cycle-safe via visited set. `max-depth` nil = unlimited, 0 = empty, N = up
   to N hops. Note: `(and max-depth (> hop max-depth))` is intentional — in
   Clojure 0 is truthy, so `:depth 0` is handled by max-depth being non-nil,
   not by falsy-0 semantics.

   There is no cap on result size. With `max-depth` nil and a highly-connected
   graph this can return every reachable task. Callers that may hit large
   closures should pass an explicit `:depth` to bound the traversal."
  [start-id next-batch-fn max-depth]
  (loop [frontier [start-id]
         visited #{}
         result []
         hop 1]
    (if (or (empty? frontier)
            (and max-depth (> hop max-depth)))
      result
      (let [edges (next-batch-fn frontier)
            next-ids (->> frontier
                          (mapcat edges)
                          (remove visited)
                          distinct
                          vec)]
        (recur next-ids
               (into visited next-ids)
               (into result (map #(vector % hop)) next-ids)
               (inc hop))))))

(defn- fetch-enriched-tasks-by-ids
  "Fetch and enrich tasks by IDs. Returns id -> task map. Chunks wide id lists
   to stay under SQLite's parameter limit."
  [ids]
  (if (empty? ids)
    {}
    (reduce
     (fn [acc chunk]
       (let [placeholders (str/join "," (repeat (count chunk) "?"))
             sql (str "SELECT * FROM tasks WHERE id IN (" placeholders ")")
             ^js stmt (.prepare (db/db) sql)
             rows (.all stmt (to-array chunk))
             tasks (enrich-tasks (mapv row->task rows))]
         (into acc (map (juxt :id identity)) tasks)))
     {}
     (partition-all sqlite-param-chunk-size ids))))

(defn- coerce-field
  "Accept a keyword as-is, or a string (converted to a kebab-case keyword).
   Anything else is rejected with an ex-info; the MCP tools/call path
   surfaces this as an MCP tool error (isError: true) rather than a
   JSON-RPC server error."
  [f]
  (cond
    (keyword? f) f
    (string? f) (keyword (str/replace f #"_" "-"))
    :else
    (throw (ex-info (str "Invalid field: " (pr-str f)
                         " (expected string or keyword)")
                    {:invalid-field f}))))

(defn- field->snake
  "Render a field keyword as the snake_case name the MCP schema advertises,
   so error messages echo what the caller typed rather than the internal
   kebab-case form."
  [k]
  (str/replace (name k) "-" "_"))

(defn- validate-dep-graph-fields
  "Reject fields not in `dep-graph-fields`. The ex-info surfaces as an
   MCP tool error (isError: true) rather than a JSON-RPC server error
   on the tools/call path. The 2-arity checks against a different allowed set."
  ([fields] (validate-dep-graph-fields fields dep-graph-fields))
  ([fields allowed]
   (when-let [invalid (seq (remove allowed fields))]
     (throw (ex-info (str "Unknown fields: " (str/join ", " (map field->snake invalid))
                          ". Allowed: "
                          (str/join ", " (sort (map field->snake allowed))))
                     {:invalid-fields (vec invalid)
                      :allowed (vec (sort (map field->snake allowed)))})))))

(defn- validate-depth
  "Reject non-integer or negative depth. The ex-info surfaces as an MCP
   tool error (isError: true) rather than a JSON-RPC server error on the
   tools/call path."
  [depth]
  (when (and (some? depth) (or (not (integer? depth)) (neg? depth)))
    (throw (ex-info (str "depth must be a non-negative integer, got: " (pr-str depth))
                    {:invalid-depth depth}))))

(defn- get-related-tasks
  [start-id next-batch-fn {:keys [depth fields]}]
  (validate-depth depth)
  (let [start-id (or (find-task-id start-id) start-id)
        fields (if (seq fields)
                 (mapv coerce-field fields)
                 default-dep-graph-fields)
        _ (validate-dep-graph-fields fields)
        select-keys-set (conj (set fields) :id)
        id-depth-pairs (traverse-dep-graph start-id next-batch-fn depth)
        by-id (fetch-enriched-tasks-by-ids (map first id-depth-pairs))]
    (into []
          (keep (fn [[id hop]]
                  (when-let [task (get by-id id)]
                    (-> (select-keys task select-keys-set)
                        (assoc :depth hop)))))
          id-depth-pairs)))

(defn get-upstream
  "Return tasks that task-id transitively depends on (its ancestors in the
   blocked-by graph). Each task is returned with :id, :depth (hops from
   task-id), and the fields requested via opts :fields (default: title,
   status, blocked-by). Results are in BFS order.

   Cycle-safe. If task-id reaches itself through a cycle (including a
   self-loop), task-id appears in the result at the hop it was rediscovered.
   That makes `(some #{id} (map :id (get-upstream id {})))` a valid
   cycle-detection check. Returns [] for unknown task-id.

   The returned :blocked-by field on each task reflects *currently-unresolved*
   dependencies (deps whose status is not 'done') — not the raw graph edges.
   A task whose deps are all done will show :blocked-by [] even though it
   has predecessors in the DAG.

   Traversal crosses board boundaries: if a dependency points to a task on
   another board, the result will include tasks from multiple boards.

   opts: :depth (non-negative int, nil = unlimited, 0 = empty),
         :fields (collection of kebab-case keywords or snake_case strings)."
  [task-id opts]
  (get-related-tasks task-id get-task-dependencies-batch opts))

(defn get-downstream
  "Return tasks that transitively depend on task-id (its descendants in the
   blocked-by graph). Same return shape, options, cycle semantics, and
   cross-board behavior as `get-upstream`. :blocked-by on each returned task
   reflects currently-unresolved deps only, not the raw graph edges."
  [task-id opts]
  (get-related-tasks task-id get-tasks-blocked-by-batch opts))

;; ============================================================================
;; Task query (list-tasks + projection / limit; after dep-graph field helpers)
;; ============================================================================

(def list-task-fields
  "Fields selectable via query-tasks :fields: the dep-graph fields plus
   :notes (requesting it fetches notes, like :include-notes)."
  (conj dep-graph-fields :notes))

(defn- validate-limit [limit]
  (when-not (and (integer? limit) (pos? limit))
    (throw (ex-info (str "limit must be a positive integer, got: " (pr-str limit))
                    {:invalid-limit limit}))))

(defn query-tasks
  "list-tasks with a projection and a cap, for callers (MCP agents) that
   can't afford the full payload. Returns {:tasks [...] :total N} where
   :total is the match count before :limit.
   Extra options on top of list-tasks':
     :fields - collection of field names (snake_case strings or kebab
               keywords) from `list-task-fields`; each task is projected
               to these plus :id. Projection runs after enrichment, so
               computed status/blocked-by/tags are selectable.
     :limit  - positive integer, applied after list-tasks' ordering.
   Notes are attached when :include-notes is set or :notes is in :fields,
   and only for tasks that survive the limit."
  [board-id {:keys [fields limit include-notes] :as opts}]
  (when (some? limit) (validate-limit limit))
  (let [fields (when (seq fields) (mapv coerce-field fields))
        _ (some-> fields (validate-dep-graph-fields list-task-fields))
        with-notes? (or include-notes (some #{:notes} fields))
        tasks (list-tasks board-id (assoc opts :include-notes false))
        page (cond->> tasks limit (take limit))]
    {:total (count tasks)
     :tasks (mapv (fn [task]
                    (cond-> task
                      with-notes? (assoc :notes (list-notes (:id task)))
                      fields (select-keys (cond-> (conj (set fields) :id)
                                            with-notes? (conj :notes)))))
                  page)}))

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
