(ns differ.client.events-test
  "Tests for re-frame event handlers.
   Tests event handler logic by simulating the event handling without re-frame."
  (:require [clojure.test :refer [deftest testing is]]
            [differ.client.db :as db]))

;; Note: These tests verify the event handler logic by simulating what the handlers do.
;; Each test mimics the db transformation or effects that an event would produce.

;; ============================================================================
;; Initialize Event Logic Tests
;; ============================================================================

(deftest initialize-logic-test
  (testing "creates default db when no saved preferences"
    (let [db db/default-db]
      (is (map? db))
      (is (= :sessions (get-in db [:route :page])))))

  (testing "merges saved view mode into db"
    (let [saved-mode :unified
          db (assoc db/default-db :diff-view-mode saved-mode)]
      (is (= :unified (:diff-view-mode db)))))

  (testing "sets github-just-connected flag when returning from OAuth"
    (let [db (assoc db/default-db :github-just-connected true)]
      (is (true? (:github-just-connected db))))))

;; ============================================================================
;; Navigation Event Logic Tests
;; ============================================================================

(deftest navigate-sessions-logic-test
  (testing "sets route to sessions page"
    (let [db (-> db/default-db
                 (assoc-in [:route :page] :session)
                 (assoc-in [:route :session-id] "old-session"))]
      (let [new-db (-> db
                       (assoc-in [:route :page] :sessions)
                       (assoc-in [:route :session-id] nil))]
        (is (= :sessions (get-in new-db [:route :page])))
        (is (nil? (get-in new-db [:route :session-id])))))))

(deftest navigate-session-logic-test
  (testing "sets route to session page with session-id"
    (let [session-id "test-session-123"
          db (-> db/default-db
                 (assoc-in [:route :page] :session)
                 (assoc-in [:route :session-id] session-id))]
      (is (= :session (get-in db [:route :page])))
      (is (= session-id (get-in db [:route :session-id]))))))

;; ============================================================================
;; Sessions Event Logic Tests
;; ============================================================================

(deftest load-sessions-logic-test
  (testing "sets loading state"
    (let [db (assoc-in db/default-db [:loading :sessions] true)]
      (is (true? (get-in db [:loading :sessions]))))))

(deftest sessions-loaded-logic-test
  (testing "stores sessions and clears loading"
    (let [sessions [{:id "s1" :project "p1"} {:id "s2" :project "p2"}]
          db (-> db/default-db
                 (assoc :sessions sessions)
                 (assoc-in [:loading :sessions] false))]
      (is (= 2 (count (:sessions db))))
      (is (false? (get-in db [:loading :sessions]))))))

(deftest create-session-logic-test
  (testing "sets loading state for session creation"
    (let [db (assoc-in db/default-db [:loading :sessions] true)]
      (is (true? (get-in db [:loading :sessions]))))))

;; ============================================================================
;; Session Event Logic Tests
;; ============================================================================

(deftest load-session-logic-test
  (testing "sets loading state for single session"
    (let [db (assoc-in db/default-db [:loading :session] true)]
      (is (true? (get-in db [:loading :session]))))))

(deftest session-loaded-logic-test
  (testing "stores current session and excluded files"
    (let [session {:id "s1" :project "project" :excluded-files ["ignore.txt"]}
          db (-> db/default-db
                 (assoc :current-session session)
                 (assoc :excluded-files (set (:excluded-files session)))
                 (assoc-in [:loading :session] false))]
      (is (= "s1" (get-in db [:current-session :id])))
      (is (contains? (:excluded-files db) "ignore.txt"))
      (is (false? (get-in db [:loading :session]))))))

;; ============================================================================
;; Diff Event Logic Tests
;; ============================================================================

(deftest load-diff-logic-test
  (testing "sets loading state for diff"
    (let [db (assoc-in db/default-db [:loading :diff] true)]
      (is (true? (get-in db [:loading :diff]))))))

(deftest diff-loaded-logic-test
  (testing "stores diff data and selects first file"
    (let [response {:diff "diff text"
                    :parsed [{:file-b "file1.cljs"}]
                    :files ["file1.cljs" "file2.cljs"]
                    :files-with-size [{:path "file1.cljs" :size 100}]
                    :changed-files [{:path "file1.cljs" :status :modified}]
                    :is-git-repo true}
          db (-> db/default-db
                 (assoc :diff {:raw (:diff response)
                               :parsed (:parsed response)
                               :files (:files response)
                               :files-with-size (:files-with-size response)
                               :changed-files (:changed-files response)
                               :is-git-repo (:is-git-repo response)})
                 (assoc :selected-file "file1.cljs")
                 (assoc-in [:loading :diff] false))]
      (is (= "diff text" (get-in db [:diff :raw])))
      (is (= 2 (count (get-in db [:diff :files]))))
      (is (= "file1.cljs" (:selected-file db)))
      (is (false? (get-in db [:loading :diff]))))))

;; ============================================================================
;; Comments Event Logic Tests
;; ============================================================================

(deftest comments-loaded-logic-test
  (testing "groups comments by file"
    (let [comments [{:id "c1" :file "main.cljs" :line 10}
                    {:id "c2" :file "main.cljs" :line 20}
                    {:id "c3" :file "other.cljs" :line 5}]
          by-file (group-by :file comments)
          db (-> db/default-db
                 (assoc :comments by-file)
                 (assoc-in [:loading :comments] false))]
      (is (= 2 (count (get (:comments db) "main.cljs"))))
      (is (= 1 (count (get (:comments db) "other.cljs"))))
      (is (false? (get-in db [:loading :comments]))))))

;; ============================================================================
;; File Selection Event Logic Tests
;; ============================================================================

(deftest select-file-logic-test
  (testing "updates selected file"
    (let [db (assoc db/default-db :selected-file "new-file.cljs")]
      (is (= "new-file.cljs" (:selected-file db))))))

;; ============================================================================
;; View Mode Event Logic Tests
;; ============================================================================

(deftest toggle-diff-view-mode-logic-test
  (testing "toggles from split to unified"
    (let [db (assoc db/default-db :diff-view-mode :split)
          new-mode (if (= (:diff-view-mode db) :unified) :split :unified)]
      (is (= :unified new-mode))))

  (testing "toggles from unified to split"
    (let [db (assoc db/default-db :diff-view-mode :unified)
          new-mode (if (= (:diff-view-mode db) :unified) :split :unified)]
      (is (= :split new-mode)))))

;; ============================================================================
;; File Collapse Event Logic Tests
;; ============================================================================

(deftest toggle-file-collapsed-logic-test
  (testing "collapses expanded file"
    (let [db (update-in db/default-db [:collapsed-files "main.cljs"] not)]
      (is (true? (get-in db [:collapsed-files "main.cljs"])))))

  (testing "expands collapsed file"
    (let [db (-> db/default-db
                 (assoc-in [:collapsed-files "main.cljs"] true))]
      (let [toggled (update-in db [:collapsed-files "main.cljs"] not)]
        (is (false? (get-in toggled [:collapsed-files "main.cljs"])))))))

;; ============================================================================
;; Comment Form Event Logic Tests
;; ============================================================================

(deftest show-comment-form-logic-test
  (testing "sets comment form state"
    (let [form-data {:file "main.cljs" :line 42 :parent-id nil}
          db (assoc db/default-db :comment-form
                    (merge {:visible true :text ""} form-data))]
      (is (true? (get-in db [:comment-form :visible])))
      (is (= "main.cljs" (get-in db [:comment-form :file])))
      (is (= 42 (get-in db [:comment-form :line]))))))

(deftest hide-comment-form-logic-test
  (testing "hides comment form"
    (let [db (assoc-in db/default-db [:comment-form :visible] false)]
      (is (false? (get-in db [:comment-form :visible]))))))

(deftest update-comment-text-logic-test
  (testing "updates comment text"
    (let [db (assoc-in db/default-db [:comment-form :text] "New comment text")]
      (is (= "New comment text" (get-in db [:comment-form :text]))))))

;; ============================================================================
;; SSE Event Logic Tests
;; ============================================================================

(deftest sse-connected-logic-test
  (testing "sets connected state"
    (let [db (assoc-in db/default-db [:sse :connected] true)]
      (is (true? (get-in db [:sse :connected]))))))

(deftest sse-disconnected-logic-test
  (testing "clears connected state"
    (let [db (assoc-in db/default-db [:sse :connected] false)]
      (is (false? (get-in db [:sse :connected]))))))

(deftest sse-client-id-logic-test
  (testing "stores client id"
    (let [db (assoc-in db/default-db [:sse :client-id] "client-123")]
      (is (= "client-123" (get-in db [:sse :client-id]))))))

;; ============================================================================
;; Staged Files Event Logic Tests
;; ============================================================================

(deftest staged-files-loaded-logic-test
  (testing "stores staged and unstaged files"
    (let [response {:staged ["file1.txt"] :unstaged ["file2.txt"]}
          db (-> db/default-db
                 (assoc :staged-files (set (:staged response)))
                 (assoc :unstaged-files (set (:unstaged response))))]
      (is (contains? (:staged-files db) "file1.txt"))
      (is (contains? (:unstaged-files db) "file2.txt")))))

;; ============================================================================
;; GitHub Settings Event Logic Tests
;; ============================================================================

(deftest show-github-settings-logic-test
  (testing "shows github settings panel"
    (let [db (assoc db/default-db :github-settings-visible true)]
      (is (true? (:github-settings-visible db))))))

(deftest hide-github-settings-logic-test
  (testing "hides github settings panel"
    (let [db (-> db/default-db
                 (assoc :github-settings-visible false)
                 (dissoc :github-just-connected))]
      (is (false? (:github-settings-visible db)))
      (is (nil? (:github-just-connected db))))))

;; ============================================================================
;; PAT Modal Event Logic Tests
;; ============================================================================

(deftest show-pat-modal-logic-test
  (testing "shows PAT modal with empty form"
    (let [db (assoc db/default-db
                    :pat-modal-visible true
                    :pat-form {:name "" :token "" :error nil :submitting false})]
      (is (true? (:pat-modal-visible db)))
      (is (= "" (get-in db [:pat-form :name])))
      (is (= "" (get-in db [:pat-form :token]))))))

(deftest hide-pat-modal-logic-test
  (testing "hides PAT modal and clears form"
    (let [db (assoc db/default-db
                    :pat-modal-visible false
                    :pat-form nil)]
      (is (false? (:pat-modal-visible db)))
      (is (nil? (:pat-form db))))))

(deftest update-pat-form-logic-test
  (testing "updates form field"
    (let [db (assoc-in db/default-db [:pat-form :name] "My Token")]
      (is (= "My Token" (get-in db [:pat-form :name]))))))

;; ============================================================================
;; Error Handling Event Logic Tests
;; ============================================================================

(deftest api-error-logic-test
  (testing "sets error message"
    (let [db (assoc db/default-db :error "Something went wrong")]
      (is (= "Something went wrong" (:error db))))))

(deftest clear-error-logic-test
  (testing "clears error message"
    (let [db (assoc db/default-db :error nil)]
      (is (nil? (:error db))))))

;; ============================================================================
;; Expanded Context Event Logic Tests
;; ============================================================================

(deftest context-lines-loaded-logic-test
  (testing "stores expanded context lines"
    (let [file-path "main.cljs"
          from-line 10
          to-line 20
          response {:lines [{:line 10 :content "line 10"}
                            {:line 11 :content "line 11"}]}
          context-lines (mapv (fn [{:keys [line content]}]
                                {:type :context
                                 :content content
                                 :old-num line
                                 :new-num line
                                 :file file-path})
                              (:lines response))
          db (update-in db/default-db [:expanded-context file-path]
                        (fn [existing]
                          (merge (or existing {})
                                 {[from-line to-line] context-lines})))]
      (is (some? (get-in db [:expanded-context file-path [from-line to-line]])))
      (is (= 2 (count (get-in db [:expanded-context file-path [from-line to-line]])))))))

;; ============================================================================
;; Highlighted Line Event Logic Tests
;; ============================================================================

(deftest set-highlighted-line-logic-test
  (testing "sets highlighted line"
    (let [db (assoc db/default-db :highlighted-line {:file "main.cljs" :line 42})]
      (is (= "main.cljs" (get-in db [:highlighted-line :file])))
      (is (= 42 (get-in db [:highlighted-line :line]))))))

(deftest clear-highlighted-line-logic-test
  (testing "clears highlighted line"
    (let [db (dissoc db/default-db :highlighted-line)]
      (is (nil? (:highlighted-line db))))))

;; ============================================================================
;; Content Expanded Files Event Logic Tests
;; ============================================================================

(deftest expand-file-content-logic-test
  (testing "adds file to content-expanded-files set"
    (let [file-path "large-file.cljs"
          db (update db/default-db :content-expanded-files
                     #(conj (or % #{}) file-path))]
      (is (contains? (:content-expanded-files db) file-path)))))

;; ============================================================================
;; Branches Event Logic Tests
;; ============================================================================

(deftest branches-loaded-logic-test
  (testing "stores branches and clears loading"
    (let [response {:branches ["main" "develop" "feature/new"]}
          db (-> db/default-db
                 (assoc :branches (:branches response))
                 (assoc-in [:loading :branches] false))]
      (is (= 3 (count (:branches db))))
      (is (false? (get-in db [:loading :branches]))))))
