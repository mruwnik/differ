(ns differ.client.db-test
  "Tests for client-side app-db schema and initial state."
  (:require [clojure.test :refer [deftest testing is]]
            [differ.client.db :as db]))

;; ============================================================================
;; Default DB Structure Tests
;; ============================================================================

(deftest default-db-structure-test
  (testing "default-db is a map"
    (is (map? db/default-db)))

  (testing "default-db contains required top-level keys"
    (is (contains? db/default-db :route))
    (is (contains? db/default-db :sessions))
    (is (contains? db/default-db :current-session))
    (is (contains? db/default-db :diff))
    (is (contains? db/default-db :comments))
    (is (contains? db/default-db :selected-file))
    (is (contains? db/default-db :loading))
    (is (contains? db/default-db :error))
    (is (contains? db/default-db :sse))
    (is (contains? db/default-db :comment-form))
    (is (contains? db/default-db :user))))

;; ============================================================================
;; Default Values Tests
;; ============================================================================

(deftest default-values-test
  (testing "route defaults to sessions page"
    (is (= :sessions (get-in db/default-db [:route :page]))))

  (testing "sessions defaults to empty vector"
    (is (= [] (:sessions db/default-db))))

  (testing "current-session defaults to nil"
    (is (nil? (:current-session db/default-db))))

  (testing "diff defaults to initial structure"
    (let [diff (:diff db/default-db)]
      (is (nil? (:raw diff)))
      (is (nil? (:parsed diff)))
      (is (= [] (:files diff)))
      (is (= [] (:changed-files diff)))))

  (testing "comments defaults to empty map"
    (is (= {} (:comments db/default-db))))

  (testing "selected-file defaults to nil"
    (is (nil? (:selected-file db/default-db))))

  (testing "loading flags default to false"
    (let [loading (:loading db/default-db)]
      (is (false? (:sessions loading)))
      (is (false? (:session loading)))
      (is (false? (:diff loading)))
      (is (false? (:comments loading)))))

  (testing "error defaults to nil"
    (is (nil? (:error db/default-db))))

  (testing "sse defaults to disconnected state"
    (let [sse (:sse db/default-db)]
      (is (nil? (:client-id sse)))
      (is (false? (:connected sse)))))

  (testing "comment-form defaults to hidden"
    (let [form (:comment-form db/default-db)]
      (is (false? (:visible form)))
      (is (nil? (:file form)))
      (is (nil? (:line form)))
      (is (nil? (:parent-id form)))
      (is (= "" (:text form)))))

  (testing "user has default author"
    (is (= "user" (get-in db/default-db [:user :author])))))

;; ============================================================================
;; Route Helper Tests
;; ============================================================================

(deftest sessions-page?-test
  (testing "returns true when on sessions page"
    (is (db/sessions-page? db/default-db)))

  (testing "returns false when on session page"
    (let [session-db (assoc-in db/default-db [:route :page] :session)]
      (is (not (db/sessions-page? session-db))))))

(deftest session-page?-test
  (testing "returns false when on sessions page"
    (is (not (db/session-page? db/default-db))))

  (testing "returns true when on session page"
    (let [session-db (assoc-in db/default-db [:route :page] :session)]
      (is (db/session-page? session-db)))))

(deftest current-session-id-test
  (testing "returns nil when no session-id set"
    (is (nil? (db/current-session-id db/default-db))))

  (testing "returns session-id when set"
    (let [db (assoc-in db/default-db [:route :session-id] "abc123")]
      (is (= "abc123" (db/current-session-id db))))))

;; ============================================================================
;; State Transition Tests
;; ============================================================================

(deftest state-transitions-test
  (testing "can add sessions to list"
    (let [session {:id "s1" :project "project" :branch "main"}
          db (update db/default-db :sessions conj session)]
      (is (= 1 (count (:sessions db))))
      (is (= "s1" (:id (first (:sessions db)))))))

  (testing "can set current session"
    (let [session {:id "s1" :project "project" :branch "main"}
          db (assoc db/default-db :current-session session)]
      (is (= session (:current-session db)))))

  (testing "can update loading state"
    (let [db (assoc-in db/default-db [:loading :sessions] true)]
      (is (true? (get-in db [:loading :sessions])))))

  (testing "can set error"
    (let [db (assoc db/default-db :error "Something went wrong")]
      (is (= "Something went wrong" (:error db)))))

  (testing "can update diff data"
    (let [db (assoc db/default-db :diff {:raw "diff..."
                                         :parsed [{:file-a "main.cljs"}]
                                         :files ["main.cljs"]
                                         :changed-files [{:path "main.cljs" :status :modified}]})]
      (is (= "diff..." (get-in db [:diff :raw])))
      (is (= ["main.cljs"] (get-in db [:diff :files])))))

  (testing "can select a file"
    (let [db (assoc db/default-db :selected-file "main.cljs")]
      (is (= "main.cljs" (:selected-file db)))))

  (testing "can add comments for a file"
    (let [comment {:id "c1" :file "main.cljs" :line 42 :text "Review comment"}
          db (assoc-in db/default-db [:comments "main.cljs"] [comment])]
      (is (= 1 (count (get-in db [:comments "main.cljs"]))))))

  (testing "can update SSE connection state"
    (let [db (-> db/default-db
                 (assoc-in [:sse :client-id] "client-123")
                 (assoc-in [:sse :connected] true))]
      (is (= "client-123" (get-in db [:sse :client-id])))
      (is (true? (get-in db [:sse :connected])))))

  (testing "can show comment form for a line"
    (let [db (assoc db/default-db :comment-form {:visible true
                                                 :file "main.cljs"
                                                 :line 42
                                                 :parent-id nil
                                                 :text ""})]
      (is (true? (get-in db [:comment-form :visible])))
      (is (= "main.cljs" (get-in db [:comment-form :file])))
      (is (= 42 (get-in db [:comment-form :line]))))))

;; ============================================================================
;; Data Type Validation Tests
;; ============================================================================

(deftest data-types-test
  (testing "sessions is a vector"
    (is (vector? (:sessions db/default-db))))

  (testing "comments is a map"
    (is (map? (:comments db/default-db))))

  (testing "loading is a map"
    (is (map? (:loading db/default-db))))

  (testing "route is a map"
    (is (map? (:route db/default-db))))

  (testing "diff is a map"
    (is (map? (:diff db/default-db))))

  (testing "sse is a map"
    (is (map? (:sse db/default-db))))

  (testing "comment-form is a map"
    (is (map? (:comment-form db/default-db))))

  (testing "user is a map"
    (is (map? (:user db/default-db)))))
