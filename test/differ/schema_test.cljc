(ns differ.schema-test
  "Tests for data schema and validation functions."
  (:require [clojure.test :refer [deftest testing is are]]
            [differ.schema :as schema]))

;; ============================================================================
;; session? predicate tests
;; ============================================================================

(deftest session?-test
  (testing "returns true for valid session map"
    (is (schema/session? {:id "abc123"
                          :project "my-project"
                          :branch "main"})))

  (testing "returns true for session with extra keys"
    (is (schema/session? {:id "abc123"
                          :project "my-project"
                          :branch "main"
                          :target-branch "develop"
                          :extra-key "value"})))

  (testing "returns false when missing id"
    (is (not (schema/session? {:project "my-project"
                               :branch "main"}))))

  (testing "returns false when missing project"
    (is (not (schema/session? {:id "abc123"
                               :branch "main"}))))

  (testing "returns false when missing branch"
    (is (not (schema/session? {:id "abc123"
                               :project "my-project"}))))

  (testing "returns false when id is not a string"
    (is (not (schema/session? {:id 123
                               :project "my-project"
                               :branch "main"}))))

  (testing "returns false when project is not a string"
    (is (not (schema/session? {:id "abc123"
                               :project nil
                               :branch "main"}))))

  (testing "returns false when branch is not a string"
    (is (not (schema/session? {:id "abc123"
                               :project "my-project"
                               :branch 42}))))

  (testing "returns false for non-map"
    (is (not (schema/session? "not a map")))
    (is (not (schema/session? nil)))
    (is (not (schema/session? [])))
    (is (not (schema/session? 123)))))

;; ============================================================================
;; comment? predicate tests
;; ============================================================================

(deftest comment?-test
  (testing "returns true for valid comment map"
    (is (schema/comment? {:id "comment-123"
                          :session-id "session-456"
                          :file "src/main.cljs"
                          :line 42})))

  (testing "returns true for comment with extra keys"
    (is (schema/comment? {:id "comment-123"
                          :session-id "session-456"
                          :file "src/main.cljs"
                          :line 42
                          :text "Hello"
                          :author "user"})))

  (testing "returns false when missing id"
    (is (not (schema/comment? {:session-id "session-456"
                               :file "src/main.cljs"
                               :line 42}))))

  (testing "returns false when missing session-id"
    (is (not (schema/comment? {:id "comment-123"
                               :file "src/main.cljs"
                               :line 42}))))

  (testing "returns false when missing file"
    (is (not (schema/comment? {:id "comment-123"
                               :session-id "session-456"
                               :line 42}))))

  (testing "returns false when missing line"
    (is (not (schema/comment? {:id "comment-123"
                               :session-id "session-456"
                               :file "src/main.cljs"}))))

  (testing "returns false when line is not an integer"
    (is (not (schema/comment? {:id "comment-123"
                               :session-id "session-456"
                               :file "src/main.cljs"
                               :line "42"}))))

  (testing "returns false for non-map"
    (is (not (schema/comment? nil)))
    (is (not (schema/comment? "string")))
    (is (not (schema/comment? [])))))

;; ============================================================================
;; top-level-comment? predicate tests
;; ============================================================================

(deftest top-level-comment?-test
  (testing "returns true when parent-id is nil"
    (is (schema/top-level-comment? {:id "c1" :parent-id nil})))

  (testing "returns true when parent-id key is missing"
    (is (schema/top-level-comment? {:id "c1"})))

  (testing "returns false when parent-id is set"
    (is (not (schema/top-level-comment? {:id "c1" :parent-id "c0"})))))

;; ============================================================================
;; build-threads tests
;; ============================================================================

(deftest build-threads-test
  (testing "returns empty vector for empty input"
    (is (= [] (schema/build-threads []))))

  (testing "handles single top-level comment"
    (let [comments [{:id "c1" :parent-id nil :text "Hello"}]
          result (schema/build-threads comments)]
      (is (= 1 (count result)))
      (is (= "c1" (:id (first result))))
      (is (= [] (:replies (first result))))))

  (testing "handles multiple top-level comments"
    (let [comments [{:id "c1" :parent-id nil :text "First"}
                    {:id "c2" :parent-id nil :text "Second"}
                    {:id "c3" :parent-id nil :text "Third"}]
          result (schema/build-threads comments)]
      (is (= 3 (count result)))
      (is (= ["c1" "c2" "c3"] (mapv :id result)))))

  (testing "nests single reply"
    (let [comments [{:id "c1" :parent-id nil :text "Parent"}
                    {:id "c2" :parent-id "c1" :text "Reply"}]
          result (schema/build-threads comments)]
      (is (= 1 (count result)))
      (is (= "c1" (:id (first result))))
      (is (= 1 (count (:replies (first result)))))
      (is (= "c2" (:id (first (:replies (first result))))))))

  (testing "nests multiple replies"
    (let [comments [{:id "c1" :parent-id nil :text "Parent"}
                    {:id "c2" :parent-id "c1" :text "Reply 1"}
                    {:id "c3" :parent-id "c1" :text "Reply 2"}]
          result (schema/build-threads comments)]
      (is (= 1 (count result)))
      (is (= 2 (count (:replies (first result)))))
      (is (= #{"c2" "c3"} (set (map :id (:replies (first result))))))))

  (testing "handles nested replies (grandchildren)"
    (let [comments [{:id "c1" :parent-id nil :text "Parent"}
                    {:id "c2" :parent-id "c1" :text "Child"}
                    {:id "c3" :parent-id "c2" :text "Grandchild"}]
          result (schema/build-threads comments)]
      (is (= 1 (count result)))
      (is (= "c1" (:id (first result))))
      (let [child (first (:replies (first result)))]
        (is (= "c2" (:id child)))
        (is (= 1 (count (:replies child))))
        (is (= "c3" (:id (first (:replies child))))))))

  (testing "handles complex thread structure"
    (let [comments [{:id "c1" :parent-id nil :text "Thread 1"}
                    {:id "c2" :parent-id nil :text "Thread 2"}
                    {:id "c3" :parent-id "c1" :text "Reply to Thread 1"}
                    {:id "c4" :parent-id "c2" :text "Reply to Thread 2"}
                    {:id "c5" :parent-id "c3" :text "Nested reply"}]
          result (schema/build-threads comments)]
      (is (= 2 (count result)))
      ;; First thread
      (let [thread1 (first (filter #(= "c1" (:id %)) result))]
        (is (= 1 (count (:replies thread1))))
        (is (= "c3" (:id (first (:replies thread1)))))
        (is (= 1 (count (:replies (first (:replies thread1)))))))
      ;; Second thread
      (let [thread2 (first (filter #(= "c2" (:id %)) result))]
        (is (= 1 (count (:replies thread2))))
        (is (= "c4" (:id (first (:replies thread2))))))))

  (testing "adds empty :replies to leaf comments"
    (let [comments [{:id "c1" :parent-id nil :text "Parent"}
                    {:id "c2" :parent-id "c1" :text "Leaf"}]
          result (schema/build-threads comments)
          leaf (first (:replies (first result)))]
      (is (= [] (:replies leaf))))))

;; ============================================================================
;; schema keys tests
;; ============================================================================

(deftest schema-keys-test
  (testing "session-keys is a vector of keywords"
    (is (vector? schema/session-keys))
    (is (every? keyword? schema/session-keys))
    (is (contains? (set schema/session-keys) :id))
    (is (contains? (set schema/session-keys) :project))
    (is (contains? (set schema/session-keys) :branch)))

  (testing "comment-keys is a vector of keywords"
    (is (vector? schema/comment-keys))
    (is (every? keyword? schema/comment-keys))
    (is (contains? (set schema/comment-keys) :id))
    (is (contains? (set schema/comment-keys) :session-id))
    (is (contains? (set schema/comment-keys) :file))
    (is (contains? (set schema/comment-keys) :line))))
