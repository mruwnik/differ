(ns differ.boards-test
  "Tests for kanban board, task, and note CRUD operations."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [differ.test-helpers :as helpers]
            [differ.boards :as boards]
            [differ.db :as db]
            [differ.util :as util]))

;; ============================================================================
;; Test Database Setup
;; Patch differ.db/db-instance to use the test database
;; ============================================================================

(defn with-test-db [f]
  (let [test-db (helpers/init-test-db!)]
    (reset! db/db-instance test-db)
    (try
      (f)
      (finally
        (reset! db/db-instance nil)
        (helpers/cleanup-test-db!)))))

(use-fixtures :each with-test-db)

;; ============================================================================
;; Board Creation Tests
;; ============================================================================

(deftest get-or-create-board-test
  (testing "creates a new board for a repo path"
    (let [board (boards/get-or-create-board! "/tmp/my-repo")]
      (is (string? (:id board)))
      (is (= "/tmp/my-repo" (:repo-path board)))
      (is (vector? (:statuses board)))
      (is (some #{"pending"} (:statuses board)))
      (is (some #{"in_progress"} (:statuses board)))
      (is (some #{"done"} (:statuses board)))
      (is (string? (:created-at board)))
      (is (string? (:updated-at board)))))

  (testing "returns existing board on second call"
    (let [board1 (boards/get-or-create-board! "/tmp/existing-repo")
          board2 (boards/get-or-create-board! "/tmp/existing-repo")]
      (is (= (:id board1) (:id board2)))
      (is (= (:created-at board1) (:created-at board2))))))

(deftest get-board-test
  (testing "returns nil for non-existent board"
    (is (nil? (boards/get-board "nonexistent"))))

  (testing "returns board by ID"
    (let [board (boards/get-or-create-board! "/tmp/find-me")]
      (is (= board (boards/get-board (:id board)))))))

(deftest get-board-by-repo-test
  (testing "returns nil for non-existent repo"
    (is (nil? (boards/get-board-by-repo "/no/such/repo"))))

  (testing "returns board by repo path"
    (let [board (boards/get-or-create-board! "/tmp/by-repo")]
      (is (= (:id board) (:id (boards/get-board-by-repo "/tmp/by-repo")))))))

(deftest list-boards-test
  (testing "returns empty list when no boards"
    (is (= [] (boards/list-boards))))

  (testing "returns all boards"
    (let [b1 (boards/get-or-create-board! "/tmp/repo-1")
          b2 (boards/get-or-create-board! "/tmp/repo-2")]
      (let [all-boards (boards/list-boards)
            ids (set (map :id all-boards))]
        (is (= 2 (count all-boards)))
        (is (contains? ids (:id b1)))
        (is (contains? ids (:id b2)))))))

;; ============================================================================
;; Task Creation Tests
;; ============================================================================

(deftest create-task-test
  (testing "creates task with correct fields"
    (let [task (boards/create-task! {:repo-path "/tmp/task-repo"
                                     :title "Fix bug"
                                     :description "There is a bug"})]
      (is (string? (:id task)))
      (is (string? (:board-id task)))
      (is (= "Fix bug" (:title task)))
      (is (= "There is a bug" (:description task)))
      (is (= "pending" (:status task)))
      (is (nil? (:worker-name task)))
      (is (nil? (:worker-id task)))
      (is (false? (:persist task)))
      (is (string? (:created-at task)))
      (is (string? (:updated-at task)))))

  (testing "auto-creates board for repo"
    (let [task (boards/create-task! {:repo-path "/tmp/auto-board"
                                     :title "New task"})
          board (boards/get-board-by-repo "/tmp/auto-board")]
      (is (some? board))
      (is (= (:board-id task) (:id board)))))

  (testing "reuses existing board"
    (let [board (boards/get-or-create-board! "/tmp/shared-board")
          task (boards/create-task! {:repo-path "/tmp/shared-board"
                                     :title "Task on shared board"})]
      (is (= (:id board) (:board-id task)))))

  (testing "creates task with nil description"
    (let [task (boards/create-task! {:repo-path "/tmp/no-desc"
                                     :title "No description"})]
      (is (nil? (:description task))))))

;; ============================================================================
;; Take Task Tests
;; ============================================================================

(deftest take-task-test
  (testing "claims a pending task"
    (let [task (boards/create-task! {:repo-path "/tmp/take-repo"
                                     :title "Available task"})
          taken (boards/take-task! {:task-id (:id task)
                                    :worker-name "agent-1"
                                    :worker-id "w-001"})]
      (is (= "in_progress" (:status taken)))
      (is (= "agent-1" (:worker-name taken)))
      (is (= "w-001" (:worker-id taken)))))

  (testing "fails on non-pending task"
    (let [task (boards/create-task! {:repo-path "/tmp/take-repo"
                                     :title "Already taken"})
          _ (boards/take-task! {:task-id (:id task)
                                :worker-name "agent-1"
                                :worker-id "w-001"})]
      (is (thrown-with-msg? js/Error #"not pending"
                            (boards/take-task! {:task-id (:id task)
                                                :worker-name "agent-2"
                                                :worker-id "w-002"})))))

  (testing "fails on non-existent task"
    (is (thrown-with-msg? js/Error #"not found"
                          (boards/take-task! {:task-id "no-such-task"
                                              :worker-name "agent"
                                              :worker-id "w"}))))

  (testing "adds note when provided"
    (let [task (boards/create-task! {:repo-path "/tmp/take-repo"
                                     :title "Task with take note"})
          _ (boards/take-task! {:task-id (:id task)
                                :worker-name "agent-1"
                                :worker-id "w-001"
                                :note "Starting work on this"})
          notes (boards/list-notes (:id task))]
      (is (= 1 (count notes)))
      (is (= "Starting work on this" (:content (first notes))))
      (is (= "agent-1" (:author (first notes))))))

  (testing "auto-assigns first available task when task-id omitted"
    (let [t1 (boards/create-task! {:repo-path "/tmp/auto-repo" :title "First task"})
          _t2 (boards/create-task! {:repo-path "/tmp/auto-repo" :title "Second task"})
          taken (boards/take-task! {:repo-path "/tmp/auto-repo"
                                    :worker-name "auto-agent"
                                    :worker-id "w-auto"})]
      (is (= (:id t1) (:id taken)))
      (is (= "in_progress" (:status taken)))
      (is (= "auto-agent" (:worker-name taken)))))

  (testing "auto-assign skips blocked tasks"
    (let [blocker (boards/create-task! {:repo-path "/tmp/auto-blocked" :title "Blocker"})
          _blocked (boards/create-task! {:repo-path "/tmp/auto-blocked" :title "Blocked"
                                         :blocked-by [(:id blocker)]})
          _free (boards/create-task! {:repo-path "/tmp/auto-blocked" :title "Free task"})
          taken (boards/take-task! {:repo-path "/tmp/auto-blocked"
                                    :worker-name "agent"
                                    :worker-id "w"})]
      ;; Should skip the blocked task and take blocker (first pending unblocked)
      (is (= (:id blocker) (:id taken)))))

  (testing "auto-assign fails when no available tasks"
    (let [task (boards/create-task! {:repo-path "/tmp/auto-empty" :title "Only task"})
          _ (boards/take-task! {:task-id (:id task)
                                :worker-name "agent"
                                :worker-id "w"})]
      (is (thrown-with-msg? js/Error #"No available tasks"
                            (boards/take-task! {:repo-path "/tmp/auto-empty"
                                                :worker-name "agent-2"
                                                :worker-id "w2"})))))

  (testing "auto-assign fails when no board exists"
    (is (thrown-with-msg? js/Error #"No board found"
                          (boards/take-task! {:repo-path "/tmp/no-board-here"
                                              :worker-name "agent"
                                              :worker-id "w"})))))

;; ============================================================================
;; Update Task Tests
;; ============================================================================

(deftest update-task-test
  (testing "changes status"
    (let [task (boards/create-task! {:repo-path "/tmp/update-repo"
                                     :title "To update"})
          updated (boards/update-task! (:id task) {:status "in_progress"})]
      (is (= "in_progress" (:status updated)))))

  (testing "changes title"
    (let [task (boards/create-task! {:repo-path "/tmp/update-repo"
                                     :title "Old title"})
          updated (boards/update-task! (:id task) {:title "New title"})]
      (is (= "New title" (:title updated)))))

  (testing "changes description"
    (let [task (boards/create-task! {:repo-path "/tmp/update-repo"
                                     :title "With desc"
                                     :description "Old desc"})
          updated (boards/update-task! (:id task) {:description "New desc"})]
      (is (= "New desc" (:description updated)))))

  (testing "clears description with nil"
    (let [task (boards/create-task! {:repo-path "/tmp/update-repo"
                                     :title "Clear desc"
                                     :description "Has desc"})
          updated (boards/update-task! (:id task) {:description nil})]
      (is (nil? (:description updated)))))

  (testing "validates status against board's allowed statuses"
    (let [task (boards/create-task! {:repo-path "/tmp/update-repo"
                                     :title "Bad status"})]
      (is (thrown-with-msg? js/Error #"Invalid status"
                            (boards/update-task! (:id task) {:status "nonexistent_status"})))))

  (testing "adds note when provided"
    (let [task (boards/create-task! {:repo-path "/tmp/update-repo"
                                     :title "With note"})
          _ (boards/update-task! (:id task) {:status "in_progress"
                                             :note "Status changed"
                                             :author "reviewer"})
          notes (boards/list-notes (:id task))]
      (is (= 1 (count notes)))
      (is (= "Status changed" (:content (first notes))))
      (is (= "reviewer" (:author (first notes))))))

  (testing "sets persist flag"
    (let [task (boards/create-task! {:repo-path "/tmp/update-repo"
                                     :title "Persist me"})
          updated (boards/update-task! (:id task) {:persist true})]
      (is (true? (:persist updated)))))

  (testing "fails on non-existent task"
    (is (thrown-with-msg? js/Error #"not found"
                          (boards/update-task! "no-such-task" {:title "nope"})))))

;; ============================================================================
;; List Tasks Tests
;; ============================================================================

(deftest list-tasks-test
  (let [board (boards/get-or-create-board! "/tmp/list-repo")]

    (testing "returns empty list when no tasks"
      (is (= [] (boards/list-tasks (:id board) {}))))

    ;; Create some tasks with different statuses
    (let [t-pending (boards/create-task! {:repo-path "/tmp/list-repo"
                                          :title "Pending task"})
          t-progress (boards/create-task! {:repo-path "/tmp/list-repo"
                                           :title "In progress task"})
          _ (boards/update-task! (:id t-progress) {:status "in_progress"})
          t-done (boards/create-task! {:repo-path "/tmp/list-repo"
                                       :title "Done task"})
          _ (boards/update-task! (:id t-done) {:status "done"})
          t-rejected (boards/create-task! {:repo-path "/tmp/list-repo"
                                           :title "Rejected task"})
          _ (boards/update-task! (:id t-rejected) {:status "rejected"})]

      (testing "hides done+rejected by default"
        (let [tasks (boards/list-tasks (:id board) {})]
          (is (= 2 (count tasks)))
          (is (every? #(not (#{"done" "rejected"} (:status %))) tasks))))

      (testing "shows done+rejected when show-done is true"
        (let [tasks (boards/list-tasks (:id board) {:show-done true})]
          (is (= 4 (count tasks)))))

      (testing "filters by status vector"
        (let [tasks (boards/list-tasks (:id board) {:status ["pending"]})]
          (is (= 1 (count tasks)))
          (is (= "pending" (:status (first tasks))))))

      (testing "filters by multiple statuses"
        (let [tasks (boards/list-tasks (:id board) {:status ["pending" "in_progress"]})]
          (is (= 2 (count tasks)))))

      (testing "filters by worker-id"
        (boards/take-task! {:task-id (:id t-pending)
                            :worker-name "worker-a"
                            :worker-id "wid-a"})
        (let [tasks (boards/list-tasks (:id board) {:worker-id "wid-a" :show-done true})]
          (is (= 1 (count tasks)))
          (is (= "wid-a" (:worker-id (first tasks))))))

      (testing "includes notes when requested"
        (boards/add-note! {:task-id (:id t-progress)
                           :author "user"
                           :content "A note"})
        ;; t-pending was taken above so there are now 2 in_progress tasks
        (let [tasks (boards/list-tasks (:id board) {:status ["in_progress"]
                                                    :include-notes true})
              task-with-note (first (filter #(= (:id t-progress) (:id %)) tasks))]
          (is (= 2 (count tasks)))
          (is (every? #(vector? (:notes %)) tasks))
          (is (= 1 (count (:notes task-with-note)))))))))

;; ============================================================================
;; Note Tests
;; ============================================================================

(deftest add-note-test
  (testing "creates a note"
    (let [task (boards/create-task! {:repo-path "/tmp/note-repo"
                                     :title "Task for notes"})
          note (boards/add-note! {:task-id (:id task)
                                  :author "commenter"
                                  :content "This is a note"})]
      (is (string? (:id note)))
      (is (= (:id task) (:task-id note)))
      (is (= "commenter" (:author note)))
      (is (= "This is a note" (:content note)))
      (is (string? (:created-at note)))))

  (testing "updates task timestamp"
    (let [task (boards/create-task! {:repo-path "/tmp/note-repo"
                                     :title "Timestamp task"})
          original-task (boards/get-task (:id task))
          _ (boards/add-note! {:task-id (:id task)
                               :author "user"
                               :content "Updates timestamp"})
          updated-task (boards/get-task (:id task))]
      (is (>= (compare (:updated-at updated-task) (:updated-at original-task)) 0))))

  (testing "updates board timestamp"
    (let [board (boards/get-or-create-board! "/tmp/note-board-ts")
          task (boards/create-task! {:repo-path "/tmp/note-board-ts"
                                     :title "Board timestamp task"})
          original-board (boards/get-board (:id board))
          _ (boards/add-note! {:task-id (:id task)
                               :author "user"
                               :content "Updates board timestamp"})
          updated-board (boards/get-board (:id board))]
      (is (>= (compare (:updated-at updated-board) (:updated-at original-board)) 0))))

  (testing "fails on non-existent task"
    (is (thrown-with-msg? js/Error #"not found"
                          (boards/add-note! {:task-id "no-such-task"
                                             :author "user"
                                             :content "orphan note"})))))

(deftest list-notes-test
  (testing "returns empty list when no notes"
    (let [task (boards/create-task! {:repo-path "/tmp/empty-notes"
                                     :title "No notes"})]
      (is (= [] (boards/list-notes (:id task))))))

  (testing "returns notes in creation order"
    (let [task (boards/create-task! {:repo-path "/tmp/ordered-notes"
                                     :title "With notes"})
          _ (boards/add-note! {:task-id (:id task) :author "a" :content "First"})
          _ (boards/add-note! {:task-id (:id task) :author "b" :content "Second"})
          _ (boards/add-note! {:task-id (:id task) :author "c" :content "Third"})
          notes (boards/list-notes (:id task))]
      (is (= 3 (count notes)))
      (is (= "First" (:content (first notes))))
      (is (= "Third" (:content (last notes)))))))

;; ============================================================================
;; Task Dependency Tests
;; ============================================================================

(deftest task-dependencies-test
  (testing "task with unresolved dependency shows as blocked"
    (let [t1 (boards/create-task! {:repo-path "/tmp/dep-repo" :title "Blocking task"})
          t2 (boards/create-task! {:repo-path "/tmp/dep-repo" :title "Blocked task"
                                   :blocked-by [(:id t1)]})]
      (is (= "blocked" (:status t2)))
      (is (= [(:id t1)] (:blocked-by t2)))))

  (testing "task unblocks when dependency completes"
    (let [t1 (boards/create-task! {:repo-path "/tmp/dep-repo2" :title "Will complete"})
          t2 (boards/create-task! {:repo-path "/tmp/dep-repo2" :title "Waiting"
                                   :blocked-by [(:id t1)]})]
      (is (= "blocked" (:status t2)))
      (boards/update-task! (:id t1) {:status "done"})
      (let [t2-after (boards/get-task (:id t2))]
        (is (= "pending" (:status t2-after)))
        (is (= [] (:blocked-by t2-after))))))

  (testing "take-task fails on blocked task"
    (let [t1 (boards/create-task! {:repo-path "/tmp/dep-repo3" :title "Blocker"})
          t2 (boards/create-task! {:repo-path "/tmp/dep-repo3" :title "Blocked"
                                   :blocked-by [(:id t1)]})]
      (is (thrown-with-msg? js/Error #"blocked"
                            (boards/take-task! {:task-id (:id t2)
                                                :worker-name "agent"
                                                :worker-id "w"})))))

  (testing "update-task can change dependencies"
    (let [t1 (boards/create-task! {:repo-path "/tmp/dep-repo4" :title "Dep 1"})
          t2 (boards/create-task! {:repo-path "/tmp/dep-repo4" :title "Dep 2"})
          t3 (boards/create-task! {:repo-path "/tmp/dep-repo4" :title "Main"
                                   :blocked-by [(:id t1)]})]
      (is (= [(:id t1)] (:blocked-by t3)))
      (boards/update-task! (:id t3) {:blocked-by [(:id t2)]})
      (let [t3-after (boards/get-task (:id t3))]
        (is (= [(:id t2)] (:blocked-by t3-after))))))

  (testing "list-tasks shows effective status"
    (let [board (boards/get-or-create-board! "/tmp/dep-list-repo")
          t1 (boards/create-task! {:repo-path "/tmp/dep-list-repo" :title "Blocker"})
          t2 (boards/create-task! {:repo-path "/tmp/dep-list-repo" :title "Blocked"
                                   :blocked-by [(:id t1)]})
          tasks (boards/list-tasks (:id board) {})]
      ;; t2 should show as blocked
      (let [blocked-task (first (filter #(= (:id t2) (:id %)) tasks))]
        (is (= "blocked" (:status blocked-task))))))

  (testing "task without dependencies has empty blocked-by"
    (let [t1 (boards/create-task! {:repo-path "/tmp/dep-repo5" :title "Independent task"})]
      (is (= "pending" (:status t1)))
      (is (= [] (:blocked-by t1)))))

  (testing "multiple dependencies all must be done to unblock"
    (let [t1 (boards/create-task! {:repo-path "/tmp/dep-repo6" :title "Dep A"})
          t2 (boards/create-task! {:repo-path "/tmp/dep-repo6" :title "Dep B"})
          t3 (boards/create-task! {:repo-path "/tmp/dep-repo6" :title "Blocked by both"
                                   :blocked-by [(:id t1) (:id t2)]})]
      (is (= "blocked" (:status t3)))
      ;; Complete one dep — still blocked
      (boards/update-task! (:id t1) {:status "done"})
      (let [t3-mid (boards/get-task (:id t3))]
        (is (= "blocked" (:status t3-mid)))
        (is (= [(:id t2)] (:blocked-by t3-mid))))
      ;; Complete second dep — unblocked
      (boards/update-task! (:id t2) {:status "done"})
      (let [t3-after (boards/get-task (:id t3))]
        (is (= "pending" (:status t3-after)))
        (is (= [] (:blocked-by t3-after)))))))

;; ============================================================================
;; Board Summary Tests
;; ============================================================================

(deftest board-summary-test
  (testing "returns nil for non-existent board"
    (is (nil? (boards/board-summary "no-such-board"))))

  (testing "returns correct counts per status"
    (let [board (boards/get-or-create-board! "/tmp/summary-repo")
          _ (boards/create-task! {:repo-path "/tmp/summary-repo" :title "P1"})
          _ (boards/create-task! {:repo-path "/tmp/summary-repo" :title "P2"})
          t3 (boards/create-task! {:repo-path "/tmp/summary-repo" :title "IP1"})
          _ (boards/update-task! (:id t3) {:status "in_progress"})
          t4 (boards/create-task! {:repo-path "/tmp/summary-repo" :title "D1"})
          _ (boards/update-task! (:id t4) {:status "done"})
          summary (boards/board-summary (:id board))]
      (is (= (:id board) (:id summary)))
      (is (= "/tmp/summary-repo" (:repo-path summary)))
      (is (= 2 (get (:task-counts summary) "pending")))
      (is (= 1 (get (:task-counts summary) "in_progress")))
      (is (= 1 (get (:task-counts summary) "done"))))))

(deftest list-boards-with-summary-test
  (testing "returns empty list when no boards"
    (is (= [] (boards/list-boards-with-summary))))

  (testing "returns all boards with task counts"
    (let [_ (boards/create-task! {:repo-path "/tmp/sum-repo-1" :title "T1"})
          _ (boards/create-task! {:repo-path "/tmp/sum-repo-1" :title "T2"})
          _ (boards/create-task! {:repo-path "/tmp/sum-repo-2" :title "T3"})
          summaries (boards/list-boards-with-summary)]
      (is (= 2 (count summaries)))
      (is (every? #(contains? % :task-counts) summaries))
      ;; Each board should have its own counts
      (let [repo1 (first (filter #(= "/tmp/sum-repo-1" (:repo-path %)) summaries))
            repo2 (first (filter #(= "/tmp/sum-repo-2" (:repo-path %)) summaries))]
        (is (= 2 (get (:task-counts repo1) "pending")))
        (is (= 1 (get (:task-counts repo2) "pending")))))))

;; ============================================================================
;; Dependency graph traversal tests (get-upstream / get-downstream)
;; ============================================================================
;;
;; Test graph. Arrow A --> B means "A depends on B" (A is blocked by B):
;;
;;   D --> B --> A
;;         ^
;;         |
;;   E --> C --> A
;;
;; So for A: upstream = [], downstream = [B, C, D, E]
;; For B: upstream = [A], downstream = [D]
;; For D: upstream = [B, A], downstream = []

(defn- build-diamond-graph [repo]
  (let [a (boards/create-task! {:repo-path repo :title "A"})
        b (boards/create-task! {:repo-path repo :title "B" :blocked-by [(:id a)]})
        c (boards/create-task! {:repo-path repo :title "C" :blocked-by [(:id a)]})
        d (boards/create-task! {:repo-path repo :title "D" :blocked-by [(:id b)]})
        e (boards/create-task! {:repo-path repo :title "E" :blocked-by [(:id c)]})]
    {:a a :b b :c c :d d :e e}))

(deftest get-upstream-empty-cases-test
  (testing "returns [] for unknown task id"
    (is (= [] (boards/get-upstream "does-not-exist" {}))))

  (testing "returns [] for task with no dependencies"
    (let [t (boards/create-task! {:repo-path "/tmp/up-empty" :title "Standalone"})]
      (is (= [] (boards/get-upstream (:id t) {}))))))

(deftest get-upstream-transitive-test
  (testing "returns all transitive ancestors in BFS order"
    (let [{:keys [a b d]} (build-diamond-graph "/tmp/up-trans")
          result (boards/get-upstream (:id d) {})
          ids (mapv :id result)]
      (is (= 2 (count result)))
      ;; BFS: B at depth 1 first, then A at depth 2
      (is (= (:id b) (first ids)))
      (is (= (:id a) (second ids)))
      (is (= 1 (:depth (first result))))
      (is (= 2 (:depth (second result)))))))

(deftest get-upstream-depth-limit-test
  (testing "depth=1 returns only direct dependencies"
    (let [{:keys [a b d]} (build-diamond-graph "/tmp/up-d1")
          result (boards/get-upstream (:id d) {:depth 1})]
      (is (= 1 (count result)))
      (is (= (:id b) (:id (first result))))
      (is (not (some #(= (:id a) (:id %)) result)))))

  (testing "depth=0 returns empty"
    (let [{:keys [d]} (build-diamond-graph "/tmp/up-d0")]
      (is (= [] (boards/get-upstream (:id d) {:depth 0}))))))

(deftest get-downstream-transitive-test
  (testing "returns all transitive descendants, converging paths deduped"
    (let [{:keys [a b c d e]} (build-diamond-graph "/tmp/down-trans")
          result (boards/get-downstream (:id a) {})
          ids (set (map :id result))]
      ;; B and C at depth 1, D and E at depth 2 — all unique, 4 total
      (is (= 4 (count result)))
      (is (= #{(:id b) (:id c) (:id d) (:id e)} ids))
      (let [depth-by-id (into {} (map (juxt :id :depth) result))]
        (is (= 1 (depth-by-id (:id b))))
        (is (= 1 (depth-by-id (:id c))))
        (is (= 2 (depth-by-id (:id d))))
        (is (= 2 (depth-by-id (:id e))))))))

(deftest get-downstream-depth-limit-test
  (testing "depth=1 returns only directly-blocked tasks"
    (let [{:keys [a b c]} (build-diamond-graph "/tmp/down-d1")
          result (boards/get-downstream (:id a) {:depth 1})
          ids (set (map :id result))]
      (is (= 2 (count result)))
      (is (= #{(:id b) (:id c)} ids)))))

(deftest dep-graph-cycle-safety-test
  (testing "cycles do not cause infinite loops"
    ;; Create X and Y, then make them depend on each other (2-cycle).
    ;; After the fix: start-id IS tracked through cycles, so upstream of X
    ;; returns [Y@1, X@2]. `(some #{X} ids)` is true -> cycle detected.
    (let [x (boards/create-task! {:repo-path "/tmp/cycle" :title "X"})
          y (boards/create-task! {:repo-path "/tmp/cycle" :title "Y" :blocked-by [(:id x)]})]
      (boards/update-task! (:id x) {:blocked-by [(:id y)]})
      ;; Upstream of X: Y at depth 1, then X rediscovered at depth 2 via cycle.
      (let [up (boards/get-upstream (:id x) {})]
        (is (= 2 (count up)))
        (is (= {:id (:id y) :depth 1} (select-keys (first up) [:id :depth])))
        (is (= {:id (:id x) :depth 2} (select-keys (second up) [:id :depth]))))
      ;; Downstream of X: same topology by symmetry.
      (let [down (boards/get-downstream (:id x) {})]
        (is (= 2 (count down)))
        (is (= {:id (:id y) :depth 1} (select-keys (first down) [:id :depth])))
        (is (= {:id (:id x) :depth 2} (select-keys (second down) [:id :depth])))))))

(deftest dep-graph-self-cycle-test
  (testing "self-loop (X blocked by X) surfaces X in its own ancestors at depth 1"
    (let [x (boards/create-task! {:repo-path "/tmp/self-cycle" :title "X"})]
      (boards/update-task! (:id x) {:blocked-by [(:id x)]})
      (let [up (boards/get-upstream (:id x) {})]
        (is (= 1 (count up)))
        (is (= {:id (:id x) :depth 1} (select-keys (first up) [:id :depth]))))
      (let [down (boards/get-downstream (:id x) {})]
        (is (= 1 (count down)))
        (is (= {:id (:id x) :depth 1} (select-keys (first down) [:id :depth]))))))

  (testing "standalone task (no deps) still returns [] — no self-entry without a cycle"
    (let [t (boards/create-task! {:repo-path "/tmp/standalone" :title "Standalone"})]
      (is (= [] (boards/get-upstream (:id t) {})))
      (is (= [] (boards/get-downstream (:id t) {}))))))

(deftest dep-graph-depth-2-on-3-chain-test
  (testing "depth=2 on a 3-chain returns both ancestors with correct depths"
    ;; A <- B <- C <- D  (A has no deps; B depends on A; C on B; D on C)
    (let [a (boards/create-task! {:repo-path "/tmp/chain3" :title "A"})
          b (boards/create-task! {:repo-path "/tmp/chain3" :title "B" :blocked-by [(:id a)]})
          c (boards/create-task! {:repo-path "/tmp/chain3" :title "C" :blocked-by [(:id b)]})
          d (boards/create-task! {:repo-path "/tmp/chain3" :title "D" :blocked-by [(:id c)]})
          result (boards/get-upstream (:id d) {:depth 2})]
      (is (= 2 (count result)))
      (is (= (:id c) (:id (first result))))
      (is (= 1 (:depth (first result))))
      (is (= (:id b) (:id (second result))))
      (is (= 2 (:depth (second result))))
      ;; A is at depth 3 and should be cut off
      (is (not (some #(= (:id a) (:id %)) result))))))

(deftest dep-graph-negative-depth-test
  (testing "negative depth throws; ex-data carries :invalid-depth for logging"
    (let [t (boards/create-task! {:repo-path "/tmp/neg-depth" :title "T"})]
      (is (thrown-with-msg? js/Error #"depth must be a non-negative integer"
                            (boards/get-upstream (:id t) {:depth -1})))
      (is (thrown-with-msg? js/Error #"depth must be a non-negative integer"
                            (boards/get-downstream (:id t) {:depth -3})))
      ;; ex-data carries the offending value; the MCP tools/call path
      ;; surfaces the ex-message as an MCP tool error (isError: true).
      (let [ed (try (boards/get-upstream (:id t) {:depth -1}) nil
                    (catch :default e (ex-data e)))]
        (is (= -1 (:invalid-depth ed)))))))

(deftest dep-graph-empty-fields-test
  (testing "empty :fields [] falls back to defaults (title, status, blocked-by)"
    (let [a (boards/create-task! {:repo-path "/tmp/empty-fields" :title "A"})
          b (boards/create-task! {:repo-path "/tmp/empty-fields" :title "B" :blocked-by [(:id a)]})
          [entry] (boards/get-upstream (:id b) {:fields []})]
      (is (= (:id a) (:id entry)))
      (is (= "A" (:title entry)))
      (is (contains? entry :status))
      (is (contains? entry :blocked-by))
      ;; Non-default fields absent
      (is (not (contains? entry :description)))
      (is (not (contains? entry :worker-name))))))

(deftest dep-graph-convergent-paths-deduped-test
  (testing "a node reachable via two paths from the start appears once, at the
            hop where it was first discovered"
    ;; A depends on both B and C directly; B also depends on C. So from A,
    ;; C is reachable via two paths (A->C direct, and A->B->C). BFS
    ;; enumerates A's full frontier at hop 1 (both B and C) before recursing,
    ;; so C is recorded at hop 1 and the `(remove visited)` step on the
    ;; hop-2 frontier drops the A->B->C rediscovery. This tests the
    ;; visited-set dedup, not any "pick the shorter path" logic — depth is
    ;; per-hop rather than per-node, so whichever path wins the race is the
    ;; one that survives.
    (let [a (boards/create-task! {:repo-path "/tmp/converge" :title "A"})
          c (boards/create-task! {:repo-path "/tmp/converge" :title "C"})
          b (boards/create-task! {:repo-path "/tmp/converge" :title "B" :blocked-by [(:id c)]})]
      (boards/update-task! (:id a) {:blocked-by [(:id b) (:id c)]})
      (let [result (boards/get-upstream (:id a) {})
            depth-by-id (into {} (map (juxt :id :depth) result))]
        ;; C appears exactly once, not twice
        (is (= 2 (count result)))
        (is (= 1 (get depth-by-id (:id b))))
        (is (= 1 (get depth-by-id (:id c))))))))

(deftest dep-graph-dangling-dependency-row-test
  (testing "a task_dependencies row pointing to a nonexistent task is skipped"
    ;; Create A, then insert a dep row pointing at a ghost task ID. The schema
    ;; enforces FK, so we temporarily disable foreign_keys for the insert —
    ;; mimicking the degenerate state that could arise from an external DB
    ;; corruption or a future migration bug. The traversal should degrade
    ;; gracefully (fetch-enriched-tasks-by-ids returns {} for the ghost,
    ;; the `keep` filter drops it).
    (let [a (boards/create-task! {:repo-path "/tmp/dangling" :title "A"})
          ^js db-inst (db/db)]
      (.exec db-inst "PRAGMA foreign_keys = OFF")
      (try
        (let [^js ins-stmt (.prepare db-inst
                                     "INSERT INTO task_dependencies (task_id, depends_on_task_id) VALUES (?, ?)")]
          (.run ins-stmt (:id a) "ghost-task-id"))
        (finally
          (.exec db-inst "PRAGMA foreign_keys = ON")))
      ;; Traversal should not crash and should return [] (ghost-id exists
      ;; in edges but not in tasks table).
      (is (= [] (boards/get-upstream (:id a) {}))))))

(deftest dep-graph-default-fields-test
  (testing "default fields include title, status, blocked-by, plus id and depth"
    (let [{:keys [a b]} (build-diamond-graph "/tmp/fields-default")
          [entry] (boards/get-upstream (:id b) {})]
      (is (= (:id a) (:id entry)))
      (is (= "A" (:title entry)))
      (is (contains? entry :status))
      (is (contains? entry :blocked-by))
      (is (= 1 (:depth entry)))
      ;; Non-default fields should be absent
      (is (not (contains? entry :description)))
      (is (not (contains? entry :worker-name))))))

(deftest dep-graph-custom-fields-test
  (testing "custom fields narrow the projection; id and depth always included"
    (let [{:keys [b d]} (build-diamond-graph "/tmp/fields-custom")
          [entry] (boards/get-upstream (:id d) {:fields [:title] :depth 1})]
      (is (= (:id b) (:id entry)))
      (is (= "B" (:title entry)))
      (is (= 1 (:depth entry)))
      (is (not (contains? entry :status)))
      (is (not (contains? entry :blocked-by))))))

(deftest dep-graph-invalid-fields-test
  (testing "unknown field names throw"
    (let [{:keys [d]} (build-diamond-graph "/tmp/fields-bad")]
      (is (thrown-with-msg? js/Error #"Unknown fields"
                            (boards/get-upstream (:id d) {:fields [:title :bogus]})))))
  (testing "error message and :allowed ex-data echo snake_case names that
            match what the MCP schema advertises — not the internal
            kebab-case form"
    (let [{:keys [d]} (build-diamond-graph "/tmp/fields-snake")]
      (try
        (boards/get-upstream (:id d) {:fields [:title :blocked-by :bogus-thing]})
        (is false "should have thrown")
        (catch js/Error e
          (let [msg (.-message e)
                ed  (ex-data e)]
            (is (re-find #"blocked_by" msg))
            (is (re-find #"board_id" msg))
            (is (not (re-find #"blocked-by" msg)))
            (is (contains? (set (:allowed ed)) "blocked_by"))
            (is (not (contains? (set (:allowed ed)) "blocked-by")))))))))

(deftest dep-graph-status-reflects-blocked-test
  (testing "returned status uses the effective (blocked) status"
    (let [{:keys [a b d]} (build-diamond-graph "/tmp/status-eff")
          result (boards/get-downstream (:id a) {})
          b-entry (first (filter #(= (:id b) (:id %)) result))
          d-entry (first (filter #(= (:id d) (:id %)) result))]
      ;; B is blocked by A (pending), so B shows as "blocked"
      (is (= "blocked" (:status b-entry)))
      ;; D is blocked by B, so D also shows as "blocked"
      (is (= "blocked" (:status d-entry))))))
