(ns differ.db-test
  "Tests for database operations.
   Uses a separate test database to avoid affecting production data."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [differ.test-helpers :as helpers]
            [differ.util :as util]))

;; ============================================================================
;; Test Database Setup
;; Using test-helpers to create an isolated test database
;; ============================================================================

(def ^:dynamic *test-db* nil)

(defn with-test-db [f]
  (binding [*test-db* (helpers/init-test-db!)]
    (try
      (f)
      (finally
        (helpers/cleanup-test-db!)))))

(use-fixtures :each with-test-db)

;; ============================================================================
;; Direct Database Operation Helpers
;; These bypass the differ.db module and work directly with the test db
;; ============================================================================

(defn- row->session [^js row]
  (when row
    {:id (.-id row)
     :project (.-project row)
     :branch (.-branch row)
     :target-branch (.-target_branch row)
     :repo-path (.-repo_path row)
     :registered-files (js->clj (js/JSON.parse (.-registered_files row)))
     :manual-additions (js->clj (js/JSON.parse (.-manual_additions row)))
     :manual-removals (js->clj (js/JSON.parse (.-manual_removals row)))
     :created-at (.-created_at row)
     :updated-at (.-updated_at row)}))

(defn- row->comment [^js row]
  (when row
    {:id (.-id row)
     :session-id (.-session_id row)
     :parent-id (.-parent_id row)
     :file (.-file row)
     :line (.-line row)
     :line-content-hash (.-line_content_hash row)
     :text (.-text row)
     :author (.-author row)
     :resolved (= 1 (.-resolved row))
     :created-at (.-created_at row)
     :updated-at (.-updated_at row)}))

(defn- row->user [^js row]
  (when row
    {:id (.-id row)
     :email (.-email row)
     :name (.-name row)
     :api-key (.-api_key row)
     :created-at (.-created_at row)}))

(defn create-session! [{:keys [id project branch target-branch repo-path]}]
  (let [now (util/now-iso)
        ^js stmt (.prepare *test-db*
                           "INSERT INTO sessions (id, project, branch, target_branch, repo_path, created_at, updated_at)
                            VALUES (?, ?, ?, ?, ?, ?, ?)")]
    (.run stmt id project branch target-branch repo-path now now)
    (get-session id)))

(defn get-session [session-id]
  (let [^js stmt (.prepare *test-db* "SELECT * FROM sessions WHERE id = ?")]
    (row->session (.get stmt session-id))))

(defn list-sessions []
  (let [^js stmt (.prepare *test-db* "SELECT * FROM sessions ORDER BY updated_at DESC")]
    (mapv row->session (.all stmt))))

(defn update-session! [session-id updates]
  (let [session (get-session session-id)
        now (util/now-iso)
        ^js stmt (.prepare *test-db*
                           "UPDATE sessions SET
                            project = ?, target_branch = ?, repo_path = ?,
                            registered_files = ?, manual_additions = ?, manual_removals = ?,
                            updated_at = ? WHERE id = ?")]
    (.run stmt
          (or (:project updates) (:project session))
          (or (:target-branch updates) (:target-branch session))
          (or (:repo-path updates) (:repo-path session))
          (js/JSON.stringify (clj->js (or (:registered-files updates) (:registered-files session))))
          (js/JSON.stringify (clj->js (or (:manual-additions updates) (:manual-additions session))))
          (js/JSON.stringify (clj->js (or (:manual-removals updates) (:manual-removals session))))
          now
          session-id)
    (get-session session-id)))

(defn delete-session! [session-id]
  (let [^js stmt (.prepare *test-db* "DELETE FROM sessions WHERE id = ?")]
    (.run stmt session-id)))

(defn create-comment! [{:keys [session-id parent-id file line line-content-hash text author]}]
  (let [id (util/gen-uuid)
        now (util/now-iso)
        ^js stmt (.prepare *test-db*
                           "INSERT INTO comments (id, session_id, parent_id, file, line, line_content_hash, text, author, created_at, updated_at)
                            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")]
    (.run stmt id session-id parent-id file line line-content-hash text author now now)
    (get-comment id)))

(defn get-comment [comment-id]
  (let [^js stmt (.prepare *test-db* "SELECT * FROM comments WHERE id = ?")]
    (row->comment (.get stmt comment-id))))

(defn list-comments [session-id]
  (let [^js stmt (.prepare *test-db*
                           "SELECT * FROM comments WHERE session_id = ? ORDER BY created_at ASC")]
    (mapv row->comment (.all stmt session-id))))

(defn resolve-comment! [comment-id]
  (let [now (util/now-iso)
        ^js stmt (.prepare *test-db*
                           "UPDATE comments SET resolved = 1, updated_at = ? WHERE id = ?")]
    (.run stmt now comment-id)
    (get-comment comment-id)))

(defn unresolve-comment! [comment-id]
  (let [now (util/now-iso)
        ^js stmt (.prepare *test-db*
                           "UPDATE comments SET resolved = 0, updated_at = ? WHERE id = ?")]
    (.run stmt now comment-id)
    (get-comment comment-id)))

(defn create-user! [{:keys [email name api-key]}]
  (let [id (util/gen-uuid)
        now (util/now-iso)
        ^js stmt (.prepare *test-db*
                           "INSERT INTO users (id, email, name, api_key, created_at) VALUES (?, ?, ?, ?, ?)")]
    (.run stmt id email name api-key now)
    (get-user id)))

(defn get-user [user-id]
  (let [^js stmt (.prepare *test-db* "SELECT * FROM users WHERE id = ?")]
    (row->user (.get stmt user-id))))

(defn get-user-by-email [email]
  (let [^js stmt (.prepare *test-db* "SELECT * FROM users WHERE email = ?")]
    (row->user (.get stmt email))))

(defn count-unresolved-comments
  "Count all unresolved comments for a session (including replies).
   This provides consistent semantics across local and GitHub sessions."
  [session-id]
  (let [^js stmt (.prepare *test-db*
                           "SELECT COUNT(*) as count FROM comments
                            WHERE session_id = ? AND resolved = 0")]
    (.-count (.get stmt session-id))))

;; ============================================================================
;; Session CRUD Tests
;; ============================================================================

(deftest create-session-test
  (testing "creates a session with required fields"
    (let [session (create-session! {:id "test-id"
                                    :project "my-project"
                                    :branch "main"
                                    :target-branch "develop"
                                    :repo-path "/tmp/repo"})]
      (is (= "test-id" (:id session)))
      (is (= "my-project" (:project session)))
      (is (= "main" (:branch session)))
      (is (= "develop" (:target-branch session)))
      (is (= "/tmp/repo" (:repo-path session)))
      (is (string? (:created-at session)))
      (is (string? (:updated-at session)))))

  (testing "initializes JSON fields to defaults"
    (let [session (create-session! {:id "test-id-2"
                                    :project "project"
                                    :branch "branch"
                                    :target-branch "main"
                                    :repo-path "/tmp"})]
      (is (= {} (:registered-files session)))
      (is (= [] (:manual-additions session)))
      (is (= [] (:manual-removals session))))))

(deftest get-session-test
  (testing "returns nil for non-existent session"
    (is (nil? (get-session "nonexistent"))))

  (testing "returns session by id"
    (create-session! {:id "find-me"
                      :project "project"
                      :branch "branch"
                      :target-branch "main"
                      :repo-path "/tmp"})
    (let [session (get-session "find-me")]
      (is (some? session))
      (is (= "find-me" (:id session))))))

(deftest list-sessions-test
  (testing "returns empty list when no sessions"
    (is (= [] (list-sessions))))

  (testing "returns all sessions"
    (create-session! {:id "s1" :project "p1" :branch "b1" :target-branch "main" :repo-path "/tmp"})
    (create-session! {:id "s2" :project "p2" :branch "b2" :target-branch "main" :repo-path "/tmp"})
    (let [sessions (list-sessions)]
      (is (= 2 (count sessions)))))

  (testing "orders by updated_at descending"
    ;; Clear sessions from previous testing blocks
    (let [^js del-stmt (.prepare *test-db* "DELETE FROM sessions")]
      (.run del-stmt))
    (create-session! {:id "old" :project "p" :branch "b" :target-branch "main" :repo-path "/tmp"})
    ;; Explicitly backdate "old" session to ensure ordering
    (let [^js update-stmt (.prepare *test-db* "UPDATE sessions SET updated_at = ? WHERE id = ?")]
      (.run update-stmt "2020-01-01T00:00:00.000Z" "old"))
    (create-session! {:id "new" :project "p" :branch "b2" :target-branch "main" :repo-path "/tmp"})
    (let [sessions (list-sessions)]
      (is (= "new" (:id (first sessions)))))))

(deftest update-session-test
  (testing "updates session fields"
    (create-session! {:id "to-update"
                      :project "old-project"
                      :branch "branch"
                      :target-branch "main"
                      :repo-path "/old/path"})
    (let [updated (update-session! "to-update" {:project "new-project"
                                                :repo-path "/new/path"})]
      (is (= "new-project" (:project updated)))
      (is (= "/new/path" (:repo-path updated)))
      (is (= "branch" (:branch updated))))) ; unchanged field

  (testing "updates JSON fields"
    (create-session! {:id "json-update"
                      :project "p"
                      :branch "b"
                      :target-branch "main"
                      :repo-path "/tmp"})
    (let [updated (update-session! "json-update"
                                   {:registered-files {"agent1" ["file1.txt"]}
                                    :manual-additions ["extra.txt"]})]
      (is (= {"agent1" ["file1.txt"]} (:registered-files updated)))
      (is (= ["extra.txt"] (:manual-additions updated)))))

  (testing "updates updated-at timestamp"
    (create-session! {:id "timestamp-test"
                      :project "p"
                      :branch "b"
                      :target-branch "main"
                      :repo-path "/tmp"})
    (let [original (get-session "timestamp-test")
          _ (update-session! "timestamp-test" {:project "updated"})
          updated (get-session "timestamp-test")]
      ;; updated-at should be >= original (may be same if within same ms)
      (is (>= (compare (:updated-at updated) (:updated-at original)) 0)))))

(deftest delete-session-test
  (testing "deletes session"
    (create-session! {:id "to-delete"
                      :project "p"
                      :branch "b"
                      :target-branch "main"
                      :repo-path "/tmp"})
    (is (some? (get-session "to-delete")))
    (delete-session! "to-delete")
    (is (nil? (get-session "to-delete")))))

;; ============================================================================
;; Comment CRUD Tests
;; ============================================================================

(deftest create-comment-test
  (testing "creates a top-level comment"
    (create-session! {:id "session-1"
                      :project "p"
                      :branch "b"
                      :target-branch "main"
                      :repo-path "/tmp"})
    (let [comment (create-comment! {:session-id "session-1"
                                    :parent-id nil
                                    :file "src/main.cljs"
                                    :line 42
                                    :line-content-hash "abc123"
                                    :text "This needs review"
                                    :author "reviewer"})]
      (is (string? (:id comment)))
      (is (= "session-1" (:session-id comment)))
      (is (nil? (:parent-id comment)))
      (is (= "src/main.cljs" (:file comment)))
      (is (= 42 (:line comment)))
      (is (= "abc123" (:line-content-hash comment)))
      (is (= "This needs review" (:text comment)))
      (is (= "reviewer" (:author comment)))
      (is (false? (:resolved comment)))))

  (testing "creates a reply comment"
    (create-session! {:id "session-2"
                      :project "p"
                      :branch "b"
                      :target-branch "main"
                      :repo-path "/tmp"})
    (let [parent (create-comment! {:session-id "session-2"
                                   :parent-id nil
                                   :file "file.txt"
                                   :line 1
                                   :line-content-hash "hash"
                                   :text "Parent"
                                   :author "user1"})
          reply (create-comment! {:session-id "session-2"
                                  :parent-id (:id parent)
                                  :file "file.txt"
                                  :line 1
                                  :line-content-hash "hash"
                                  :text "Reply"
                                  :author "user2"})]
      (is (= (:id parent) (:parent-id reply))))))

(deftest get-comment-test
  (testing "returns nil for non-existent comment"
    (is (nil? (get-comment "nonexistent"))))

  (testing "returns comment by id"
    (create-session! {:id "s" :project "p" :branch "b" :target-branch "main" :repo-path "/tmp"})
    (let [created (create-comment! {:session-id "s"
                                    :file "f"
                                    :line 1
                                    :line-content-hash "h"
                                    :text "text"
                                    :author "a"})
          found (get-comment (:id created))]
      (is (= (:id created) (:id found))))))

(deftest list-comments-test
  (testing "returns empty list when no comments"
    (create-session! {:id "empty-session" :project "p" :branch "b" :target-branch "main" :repo-path "/tmp"})
    (is (= [] (list-comments "empty-session"))))

  (testing "returns comments for session"
    (create-session! {:id "with-comments" :project "p" :branch "b" :target-branch "main" :repo-path "/tmp"})
    (create-comment! {:session-id "with-comments" :file "f" :line 1 :line-content-hash "h" :text "c1" :author "a"})
    (create-comment! {:session-id "with-comments" :file "f" :line 2 :line-content-hash "h" :text "c2" :author "a"})
    (let [comments (list-comments "with-comments")]
      (is (= 2 (count comments)))))

  (testing "only returns comments for specified session"
    (create-session! {:id "s1" :project "p" :branch "b" :target-branch "main" :repo-path "/tmp"})
    (create-session! {:id "s2" :project "p" :branch "b2" :target-branch "main" :repo-path "/tmp"})
    (create-comment! {:session-id "s1" :file "f" :line 1 :line-content-hash "h" :text "in s1" :author "a"})
    (create-comment! {:session-id "s2" :file "f" :line 1 :line-content-hash "h" :text "in s2" :author "a"})
    (let [comments (list-comments "s1")]
      (is (= 1 (count comments)))
      (is (= "in s1" (:text (first comments)))))))

(deftest resolve-comment-test
  (testing "marks comment as resolved"
    (create-session! {:id "s" :project "p" :branch "b" :target-branch "main" :repo-path "/tmp"})
    (let [comment (create-comment! {:session-id "s" :file "f" :line 1 :line-content-hash "h" :text "t" :author "a"})
          resolved (resolve-comment! (:id comment))]
      (is (true? (:resolved resolved)))))

  (testing "updates updated-at timestamp"
    (create-session! {:id "s2" :project "p" :branch "b" :target-branch "main" :repo-path "/tmp"})
    (let [comment (create-comment! {:session-id "s2" :file "f" :line 1 :line-content-hash "h" :text "t" :author "a"})
          resolved (resolve-comment! (:id comment))]
      ;; updated-at should be >= original (may be same if within same ms)
      (is (>= (compare (:updated-at resolved) (:updated-at comment)) 0)))))

(deftest unresolve-comment-test
  (testing "marks comment as unresolved"
    (create-session! {:id "s" :project "p" :branch "b" :target-branch "main" :repo-path "/tmp"})
    (let [comment (create-comment! {:session-id "s" :file "f" :line 1 :line-content-hash "h" :text "t" :author "a"})
          _ (resolve-comment! (:id comment))
          unresolved (unresolve-comment! (:id comment))]
      (is (false? (:resolved unresolved))))))

;; ============================================================================
;; User CRUD Tests
;; ============================================================================

(deftest create-user-test
  (testing "creates a user"
    (let [user (create-user! {:email "test@example.com"
                              :name "Test User"
                              :api-key "api-key-123"})]
      (is (string? (:id user)))
      (is (= "test@example.com" (:email user)))
      (is (= "Test User" (:name user)))
      (is (= "api-key-123" (:api-key user)))
      (is (string? (:created-at user))))))

(deftest get-user-test
  (testing "returns nil for non-existent user"
    (is (nil? (get-user "nonexistent"))))

  (testing "returns user by id"
    (let [created (create-user! {:email "find@me.com" :name "Find Me"})
          found (get-user (:id created))]
      (is (= (:id created) (:id found))))))

(deftest get-user-by-email-test
  (testing "returns nil for non-existent email"
    (is (nil? (get-user-by-email "nonexistent@example.com"))))

  (testing "returns user by email"
    (create-user! {:email "unique@example.com" :name "Unique User"})
    (let [user (get-user-by-email "unique@example.com")]
      (is (some? user))
      (is (= "unique@example.com" (:email user))))))

;; ============================================================================
;; Count Unresolved Comments Tests
;; ============================================================================

(deftest count-unresolved-comments-test
  (testing "counts all unresolved comments"
    (create-session! {:id "count-session" :project "p" :branch "b" :target-branch "main" :repo-path "/tmp"})
    (is (= 0 (count-unresolved-comments "count-session")))
    (create-comment! {:session-id "count-session" :file "a.cljs" :line 1 :line-content-hash "h" :text "comment 1" :author "a"})
    (is (= 1 (count-unresolved-comments "count-session")))
    (create-comment! {:session-id "count-session" :file "b.cljs" :line 2 :line-content-hash "h" :text "comment 2" :author "a"})
    (is (= 2 (count-unresolved-comments "count-session"))))

  (testing "excludes resolved comments"
    (create-session! {:id "resolved-test" :project "p" :branch "b" :target-branch "main" :repo-path "/tmp"})
    (let [c1 (create-comment! {:session-id "resolved-test" :file "a.cljs" :line 1 :line-content-hash "h" :text "t" :author "a"})
          _ (create-comment! {:session-id "resolved-test" :file "a.cljs" :line 2 :line-content-hash "h" :text "t" :author "a"})]
      (resolve-comment! (:id c1))
      (is (= 1 (count-unresolved-comments "resolved-test")))))

  (testing "includes reply comments (counts all unresolved)"
    (create-session! {:id "reply-test" :project "p" :branch "b" :target-branch "main" :repo-path "/tmp"})
    (let [parent (create-comment! {:session-id "reply-test" :file "a.cljs" :line 1 :line-content-hash "h" :text "parent" :author "a"})]
      (create-comment! {:session-id "reply-test" :parent-id (:id parent) :file "a.cljs" :line 1 :line-content-hash "h" :text "reply" :author "a"})
      ;; Now counts both parent and reply (2 total)
      (is (= 2 (count-unresolved-comments "reply-test"))))))
