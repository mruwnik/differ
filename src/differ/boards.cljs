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

(defn- enrich-task
  "Add effective status and blocked-by to a single task."
  [task]
  (when task
    (let [^js stmt (.prepare (db/db)
                             "SELECT td.depends_on_task_id FROM task_dependencies td
                              JOIN tasks t ON t.id = td.depends_on_task_id
                              WHERE td.task_id = ? AND t.status != 'done'")
          rows (.all stmt (:id task))
          unresolved (mapv (fn [^js r] (.-depends_on_task_id r)) rows)
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
          deps-by-task (group-by (fn [^js r] (.-task_id r)) rows)]
      (mapv (fn [task]
              (let [unresolved (mapv (fn [^js r] (.-depends_on_task_id r)) (get deps-by-task (:id task) []))
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

(defn- find-next-available-task-row
  "Find the first pending task with no unresolved dependencies on a board.
   Returns raw JS row or nil. Must be called inside a transaction."
  [board-id]
  (let [^js stmt (.prepare (db/db)
                           "SELECT t.* FROM tasks t
                            WHERE t.board_id = ?
                            AND t.status = 'pending'
                            AND NOT EXISTS (
                              SELECT 1 FROM task_dependencies td
                              JOIN tasks dep ON dep.id = td.depends_on_task_id
                              WHERE td.task_id = t.id AND dep.status != 'done'
                            )
                            ORDER BY t.created_at ASC
                            LIMIT 1")]
    (.get stmt board-id)))

(defn take-task!
  "Atomically claim a pending task. Fails if task is not pending or blocked.
   If task-id is nil, auto-assigns the first available task on the board
   identified by repo-path. Uses better-sqlite3 transaction for atomicity."
  [{:keys [task-id repo-path worker-name worker-id note]}]
  (when (and (nil? task-id) (nil? repo-path))
    (throw (js/Error. "Either task-id or repo-path is required")))
  (let [txn (.transaction (db/db)
                          (fn []
                            ;; When task-id is provided it takes precedence; repo-path is ignored.
                            (let [^js row (if task-id
                                            (let [^js read-stmt (.prepare (db/db) "SELECT * FROM tasks WHERE id = ?")
                                                  ^js r (.get read-stmt task-id)]
                                              (when-not r
                                                (throw (js/Error. (str "Task not found: " task-id))))
                                              (when-not (= "pending" (.-status r))
                                                (throw (js/Error. (str "Task is not pending (status: " (.-status r) ")"))))
                                              r)
                                           ;; Auto-assign: find next available task on the board
                                            (let [board (get-board-by-repo repo-path)]
                                              (when-not board
                                                (throw (js/Error. (str "No board found for repo: " repo-path))))
                                              (let [^js r (find-next-available-task-row (:id board))]
                                                (when-not r
                                                  (throw (js/Error. "No available tasks on this board")))
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
                                                              "UPDATE tasks SET status = 'in_progress', worker_name = ?, worker_id = ?, updated_at = ?
                                                               WHERE id = ?")]
                                (.run update-stmt worker-name worker-id now tid)
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
;; Dependency graph traversal
;; ============================================================================

(def dep-graph-fields
  "Fields allowed in get-upstream / get-downstream :fields options.
   :id is always included in the result regardless of :fields, and :depth
   is synthesized by the traversal (hop count). Listing :depth in :fields
   raises 'Unknown fields: depth' — it's not a column, so just omit it."
  #{:id :board-id :title :description :status :worker-name :worker-id
    :persist :blocked-by :created-at :updated-at})

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
   on the tools/call path."
  [fields]
  (when-let [invalid (seq (remove dep-graph-fields fields))]
    (throw (ex-info (str "Unknown fields: " (str/join ", " (map field->snake invalid))
                         ". Allowed: "
                         (str/join ", " (sort (map field->snake dep-graph-fields))))
                    {:invalid-fields (vec invalid)
                     :allowed (vec (sort (map field->snake dep-graph-fields)))}))))

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
  (let [fields (if (seq fields)
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
