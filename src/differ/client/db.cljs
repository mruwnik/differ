(ns differ.client.db
  "Re-frame app-db schema and initial state.")

(def default-db
  {:route {:page :sessions}  ;; :sessions or :session
   :sessions []
   :current-session nil
   :diff {:raw nil
          :parsed nil
          :files []
          :changed-files []}
   :comments {}  ;; file -> threaded comments
   :selected-file nil
   :loading {:sessions false
             :session false
             :diff false
             :comments false}
   :error nil
   :sse {:client-id nil
         :connected false}
   :comment-form {:visible false
                  :file nil
                  :line nil
                  :parent-id nil
                  :text ""}
   :user {:author "user"}})  ;; Default author for human comments

;; Route helpers
(defn sessions-page? [db]
  (= :sessions (get-in db [:route :page])))

(defn session-page? [db]
  (= :session (get-in db [:route :page])))

(defn current-session-id [db]
  (get-in db [:route :session-id]))
