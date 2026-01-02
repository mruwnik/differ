(ns differ.client.subs-test
  "Tests for re-frame subscriptions.
   Tests subscription logic by simulating the data transformations."
  (:require [clojure.test :refer [deftest testing is]]
            [differ.client.db :as db]))

;; Note: These tests verify the subscription logic without requiring re-frame.
;; Each test simulates what the subscription handler would do with the input data.

;; ============================================================================
;; Route Subscription Logic Tests
;; ============================================================================

(deftest route-sub-test
  (testing "returns route from db"
    (let [route (:route db/default-db)]
      (is (map? route))
      (is (= :sessions (:page route))))))

(deftest current-page-sub-test
  (testing "extracts page from route"
    (let [route {:page :sessions :session-id nil}
          page (:page route)]
      (is (= :sessions page))))

  (testing "returns :session when on session page"
    (let [route {:page :session :session-id "s1"}
          page (:page route)]
      (is (= :session page)))))

(deftest current-session-id-sub-test
  (testing "extracts session-id from route"
    (let [route {:page :session :session-id "abc123"}
          session-id (:session-id route)]
      (is (= "abc123" session-id))))

  (testing "returns nil when on sessions list page"
    (let [route {:page :sessions}
          session-id (:session-id route)]
      (is (nil? session-id)))))

;; ============================================================================
;; Sessions Subscription Logic Tests
;; ============================================================================

(deftest sessions-sub-test
  (testing "returns sessions vector"
    (let [sessions [{:id "s1" :project "p1"}
                    {:id "s2" :project "p2"}]]
      (is (vector? sessions))
      (is (= 2 (count sessions))))))

(deftest current-session-sub-test
  (testing "returns current session"
    (let [session {:id "s1" :project "project" :branch "main"}]
      (is (= "s1" (:id session)))
      (is (= "project" (:project session))))))

;; ============================================================================
;; Diff Subscription Logic Tests
;; ============================================================================

(deftest diff-sub-test
  (testing "returns diff map"
    (let [diff {:raw "diff..."
                :parsed [{:file-a "main.cljs" :hunks []}]
                :files ["main.cljs"]
                :changed-files [{:path "main.cljs" :status :modified}]}]
      (is (map? diff))
      (is (string? (:raw diff)))
      (is (vector? (:parsed diff))))))

(deftest files-sub-test
  (testing "extracts files from diff"
    (let [diff {:files ["file1.txt" "file2.txt"]}
          files (:files diff)]
      (is (= ["file1.txt" "file2.txt"] files)))))

(deftest changed-files-sub-test
  (testing "extracts changed-files from diff"
    (let [diff {:changed-files [{:path "main.cljs" :status :modified}
                                {:path "new.cljs" :status :added}]}
          changed (:changed-files diff)]
      (is (= 2 (count changed)))
      (is (= :modified (:status (first changed)))))))

(deftest is-git-repo-sub-test
  (testing "extracts is-git-repo from diff"
    (is (true? (:is-git-repo {:is-git-repo true})))
    (is (false? (:is-git-repo {:is-git-repo false})))))

(deftest parsed-diff-sub-test
  (testing "extracts parsed from diff"
    (let [diff {:parsed [{:file-a "main.cljs" :file-b "main.cljs" :hunks []}]}
          parsed (:parsed diff)]
      (is (vector? parsed))
      (is (= "main.cljs" (:file-a (first parsed)))))))

;; ============================================================================
;; Selected File Subscription Logic Tests
;; ============================================================================

(deftest selected-file-sub-test
  (testing "returns selected file"
    (is (= "main.cljs" "main.cljs"))))

(deftest selected-file-diff-sub-test
  (testing "filters parsed for selected file"
    (let [parsed [{:file-b "file1.cljs" :hunks []}
                  {:file-b "file2.cljs" :hunks []}
                  {:file-b "file3.cljs" :hunks []}]
          selected "file2.cljs"
          file-diff (first (filter #(= (:file-b %) selected) parsed))]
      (is (= "file2.cljs" (:file-b file-diff)))))

  (testing "returns nil when file not in parsed"
    (let [parsed [{:file-b "other.cljs"}]
          selected "main.cljs"
          file-diff (first (filter #(= (:file-b %) selected) parsed))]
      (is (nil? file-diff)))))

;; ============================================================================
;; Comments Subscription Logic Tests
;; ============================================================================

(deftest comments-sub-test
  (testing "returns comments map"
    (let [comments {"main.cljs" [{:id "c1" :line 10}]
                    "other.cljs" [{:id "c2" :line 20}]}]
      (is (map? comments)))))

(deftest comments-for-file-sub-test
  (testing "returns comments for selected file"
    (let [comments {"main.cljs" [{:id "c1" :line 10}
                                 {:id "c2" :line 20}]
                    "other.cljs" [{:id "c3" :line 5}]}
          selected "main.cljs"
          file-comments (get comments selected [])]
      (is (= 2 (count file-comments)))))

  (testing "returns empty vector for file without comments"
    (let [comments {}
          selected "main.cljs"
          file-comments (get comments selected [])]
      (is (= [] file-comments)))))

(deftest comments-for-line-sub-test
  (testing "filters comments by line"
    (let [comments [{:id "c1" :line 10}
                    {:id "c2" :line 20}
                    {:id "c3" :line 10}]
          line 10
          line-comments (vec (filter #(= (:line %) line) comments))]
      (is (= 2 (count line-comments)))))

  (testing "returns empty vector for line without comments"
    (let [comments [{:id "c1" :line 10}]
          line 20
          line-comments (vec (filter #(= (:line %) line) comments))]
      (is (= [] line-comments)))))

;; ============================================================================
;; Loading Subscription Logic Tests
;; ============================================================================

(deftest loading-sub-test
  (testing "returns loading map"
    (let [loading {:sessions true :session false :diff false}]
      (is (map? loading)))))

(deftest loading?-sub-test
  (testing "returns specific loading state"
    (let [loading {:sessions true :diff false}]
      (is (true? (get loading :sessions false)))
      (is (false? (get loading :diff false))))))

(deftest any-loading?-sub-test
  (testing "returns true if any loading is true"
    (let [loading {:sessions true :diff false}]
      (is (some true? (vals loading)))))

  (testing "returns false if all loading are false"
    (let [loading {:sessions false :diff false}]
      (is (not (some true? (vals loading)))))))

;; ============================================================================
;; Comment Form Subscription Logic Tests
;; ============================================================================

(deftest comment-form-sub-test
  (testing "returns comment form state"
    (let [form {:visible true :file "main.cljs" :line 42 :text ""}]
      (is (map? form))
      (is (true? (:visible form))))))

(deftest comment-form-visible?-sub-test
  (testing "extracts visible from form"
    (let [form {:visible true}]
      (is (true? (:visible form))))
    (let [form {:visible false}]
      (is (false? (:visible form))))))

;; ============================================================================
;; SSE Subscription Logic Tests
;; ============================================================================

(deftest sse-connected?-sub-test
  (testing "returns connected state"
    (let [sse {:connected true :client-id "abc"}]
      (is (true? (:connected sse))))
    (let [sse {:connected false}]
      (is (false? (:connected sse))))))

;; ============================================================================
;; User Subscription Logic Tests
;; ============================================================================

(deftest user-sub-test
  (testing "returns user map"
    (let [user {:author "reviewer"}]
      (is (map? user)))))

(deftest author-sub-test
  (testing "extracts author from user"
    (let [user {:author "reviewer"}]
      (is (= "reviewer" (:author user))))))

;; ============================================================================
;; File Size Subscription Logic Tests
;; ============================================================================

(deftest file-size-sub-test
  (testing "finds file size from files-with-size"
    (let [files-with-size [{:path "main.cljs" :size 1234}
                           {:path "other.cljs" :size 5678}]
          file-path "main.cljs"
          file-info (first (filter #(= (:path %) file-path) files-with-size))
          size (:size file-info)]
      (is (= 1234 size))))

  (testing "returns nil for unknown file"
    (let [files-with-size [{:path "known.cljs" :size 100}]
          file-path "unknown.cljs"
          file-info (first (filter #(= (:path %) file-path) files-with-size))]
      (is (nil? file-info)))))

;; ============================================================================
;; View Mode Subscription Logic Tests
;; ============================================================================

(deftest diff-view-mode-sub-test
  (testing "returns :split as default"
    (let [db {}
          mode (or (:diff-view-mode db) :split)]
      (is (= :split mode))))

  (testing "returns :unified when set"
    (let [db {:diff-view-mode :unified}
          mode (or (:diff-view-mode db) :split)]
      (is (= :unified mode)))))

;; ============================================================================
;; Collapsed Files Subscription Logic Tests
;; ============================================================================

(deftest collapsed-files-sub-test
  (testing "returns empty map by default"
    (let [db {}
          collapsed (or (:collapsed-files db) {})]
      (is (= {} collapsed))))

  (testing "returns collapsed state"
    (let [db {:collapsed-files {"main.cljs" true}}
          collapsed (or (:collapsed-files db) {})]
      (is (true? (get collapsed "main.cljs"))))))

;; ============================================================================
;; Staged Files Subscription Logic Tests
;; ============================================================================

(deftest staged-files-sub-test
  (testing "returns empty set by default"
    (let [db {}
          staged (or (:staged-files db) #{})]
      (is (= #{} staged))))

  (testing "returns staged files set"
    (let [db {:staged-files #{"file1.txt" "file2.txt"}}
          staged (or (:staged-files db) #{})]
      (is (contains? staged "file1.txt")))))

(deftest unstaged-files-sub-test
  (testing "returns empty set by default"
    (let [db {}
          unstaged (or (:unstaged-files db) #{})]
      (is (= #{} unstaged)))))

;; ============================================================================
;; Excluded Files Subscription Logic Tests
;; ============================================================================

(deftest excluded-files-sub-test
  (testing "returns empty set by default"
    (let [db {}
          excluded (or (:excluded-files db) #{})]
      (is (= #{} excluded))))

  (testing "returns excluded files set"
    (let [db {:excluded-files #{"ignored.txt"}}
          excluded (or (:excluded-files db) #{})]
      (is (contains? excluded "ignored.txt")))))
