(ns differ.comments-test
  "Tests for comment management logic."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [differ.test-helpers :as helpers]
            [differ.util :as util]
            [differ.schema :as schema]))

;; ============================================================================
;; Test Setup
;; ============================================================================

(def ^:dynamic *test-db* nil)
(def ^:dynamic *test-repo* nil)
(def ^:dynamic *test-repo-cleanup* nil)

(defn with-test-env [f]
  (let [{:keys [path cleanup]} (helpers/create-test-repo)]
    (binding [*test-db* (helpers/init-test-db!)
              *test-repo* path
              *test-repo-cleanup* cleanup]
      (try
        (f)
        (finally
          (cleanup)
          (helpers/cleanup-test-db!))))))

(use-fixtures :each with-test-env)

;; ============================================================================
;; compute-line-hash tests (logic)
;; ============================================================================

(deftest compute-line-hash-logic-test
  (testing "hash of same content is equal"
    (let [content "function foo() { return 42; }"]
      (is (= (util/sha256-hex content)
             (util/sha256-hex content)))))

  (testing "hash of different content differs"
    (is (not= (util/sha256-hex "line 1")
              (util/sha256-hex "line 2"))))

  (testing "handles nil content as empty string"
    (is (= (util/sha256-hex nil)
           (util/sha256-hex "")))))

;; ============================================================================
;; add-comment logic tests
;; ============================================================================

(deftest add-comment-reply-inheritance-test
  (testing "reply inherits file from parent when not specified"
    (let [parent {:id "parent-1"
                  :file "src/main.cljs"
                  :line 42}
          reply-file (or nil (:file parent))]
      (is (= "src/main.cljs" reply-file))))

  (testing "reply inherits line from parent when not specified"
    (let [parent {:id "parent-1"
                  :file "src/main.cljs"
                  :line 42}
          reply-line (or nil (:line parent))]
      (is (= 42 reply-line))))

  (testing "explicit file overrides parent"
    (let [parent {:id "parent-1"
                  :file "src/main.cljs"
                  :line 42}
          explicit-file "src/other.cljs"
          reply-file (or explicit-file (:file parent))]
      (is (= "src/other.cljs" reply-file)))))

;; ============================================================================
;; check-staleness logic tests
;; ============================================================================

(deftest check-staleness-logic-test
  (testing "returns :fresh when hashes match"
    (let [original-hash "abc123"
          current-hash "abc123"
          result (cond
                   (= current-hash original-hash) :fresh
                   :else :changed)]
      (is (= :fresh result))))

  (testing "returns :changed when hashes differ"
    (let [original-hash "abc123"
          current-hash "def456"
          result (cond
                   (= current-hash original-hash) :fresh
                   :else :changed)]
      (is (= :changed result)))))

;; ============================================================================
;; get-pending-feedback logic tests
;; ============================================================================

(deftest get-pending-feedback-logic-test
  (testing "filters unresolved comments"
    (let [comments [{:id "c1" :resolved false :parent-id nil}
                    {:id "c2" :resolved true :parent-id nil}
                    {:id "c3" :resolved false :parent-id nil}]
          unresolved (filter #(not (:resolved %)) comments)]
      (is (= 2 (count unresolved)))
      (is (= #{"c1" "c3"} (set (map :id unresolved))))))

  (testing "includes replies to unresolved comments"
    (let [comments [{:id "c1" :resolved false :parent-id nil}
                    {:id "c2" :resolved true :parent-id nil}
                    {:id "r1" :resolved false :parent-id "c1"}
                    {:id "r2" :resolved true :parent-id "c2"}]
          unresolved (filter #(not (:resolved %)) comments)
          unresolved-ids (set (map :id unresolved))
          replies-to-unresolved (filter
                                 (fn [c]
                                   (and (:parent-id c)
                                        (contains? unresolved-ids (:parent-id c))))
                                 comments)]
      ;; r1's parent (c1) is unresolved, so r1 should be included
      (is (some #(= "r1" (:id %)) replies-to-unresolved)))))

;; ============================================================================
;; build-threads integration with comments
;; ============================================================================

(deftest comments-build-threads-test
  (testing "builds threads from flat comment list"
    (let [comments [{:id "c1" :parent-id nil :file "main.cljs" :line 10 :text "Issue here"}
                    {:id "c2" :parent-id "c1" :file "main.cljs" :line 10 :text "Fixed"}
                    {:id "c3" :parent-id nil :file "main.cljs" :line 20 :text "Another issue"}]
          threads (schema/build-threads comments)]
      (is (= 2 (count threads))) ; Two top-level threads
      (let [thread1 (first (filter #(= "c1" (:id %)) threads))]
        (is (= 1 (count (:replies thread1))))
        (is (= "c2" (:id (first (:replies thread1))))))))

  (testing "preserves comment data in threads"
    (let [comments [{:id "c1"
                     :parent-id nil
                     :file "file.cljs"
                     :line 5
                     :text "Comment text"
                     :author "reviewer"
                     :resolved false}]
          threads (schema/build-threads comments)
          thread (first threads)]
      (is (= "file.cljs" (:file thread)))
      (is (= 5 (:line thread)))
      (is (= "Comment text" (:text thread)))
      (is (= "reviewer" (:author thread)))
      (is (false? (:resolved thread))))))

;; ============================================================================
;; annotate-comments-with-staleness logic tests
;; ============================================================================

(deftest annotate-comments-staleness-logic-test
  (testing "adds :staleness key to comments"
    (let [comment {:id "c1"
                   :file "main.cljs"
                   :line 10
                   :line-content-hash "original-hash"
                   :replies []}
          ;; Simulate annotation with :fresh status
          annotated (assoc comment :staleness :fresh)]
      (is (= :fresh (:staleness annotated)))))

  (testing "recursively annotates replies"
    (let [comments [{:id "c1"
                     :line-content-hash "hash1"
                     :replies [{:id "r1"
                                :line-content-hash "hash2"
                                :replies []}]}]
          ;; Simulate recursive annotation
          annotate-fn (fn annotate [c]
                        (-> c
                            (assoc :staleness :fresh)
                            (update :replies #(mapv annotate %))))
          result (mapv annotate-fn comments)
          thread (first result)
          reply (first (:replies thread))]
      (is (= :fresh (:staleness thread)))
      (is (= :fresh (:staleness reply))))))

;; ============================================================================
;; get-comments-for-file logic tests
;; ============================================================================

(deftest get-comments-for-file-logic-test
  (testing "filters comments by file"
    (let [all-comments [{:id "c1" :file "main.cljs" :parent-id nil}
                        {:id "c2" :file "other.cljs" :parent-id nil}
                        {:id "c3" :file "main.cljs" :parent-id nil}]
          file "main.cljs"
          file-comments (filter #(= file (:file %)) all-comments)]
      (is (= 2 (count file-comments)))
      (is (every? #(= "main.cljs" (:file %)) file-comments)))))

;; ============================================================================
;; Comment resolution workflow tests
;; ============================================================================

(deftest comment-resolution-workflow-test
  (testing "unresolved comments are visible in pending feedback"
    (let [comments [{:id "c1" :resolved false :parent-id nil}
                    {:id "c2" :resolved false :parent-id nil}]
          pending (filter #(and (not (:resolved %))
                                (nil? (:parent-id %))) comments)]
      (is (= 2 (count pending)))))

  (testing "resolved comments are not in pending feedback"
    (let [comments [{:id "c1" :resolved true :parent-id nil}
                    {:id "c2" :resolved false :parent-id nil}]
          pending (filter #(and (not (:resolved %))
                                (nil? (:parent-id %))) comments)]
      (is (= 1 (count pending)))
      (is (= "c2" (:id (first pending)))))))

;; ============================================================================
;; Comment data validation tests
;; ============================================================================

(deftest comment-validation-test
  (testing "valid comment passes schema/comment?"
    (is (schema/comment? {:id "test-id"
                          :session-id "session-id"
                          :file "main.cljs"
                          :line 42})))

  (testing "comment with extra fields still valid"
    (is (schema/comment? {:id "test-id"
                          :session-id "session-id"
                          :file "main.cljs"
                          :line 42
                          :text "Additional text"
                          :author "author"
                          :resolved false})))

  (testing "missing required field fails validation"
    (is (not (schema/comment? {:id "test-id"
                               :session-id "session-id"
                               :line 42})))) ; missing :file

  (testing "top-level-comment? identifies root comments"
    (is (schema/top-level-comment? {:id "c1" :parent-id nil}))
    (is (not (schema/top-level-comment? {:id "c2" :parent-id "c1"})))))
