(ns differ.client.db
  "Re-frame app-db schema and initial state.")

;; Default config values (used until server config loads)
(def default-config
  {:large-file-threshold 50000     ;; Characters - files larger require explicit load
   :line-count-threshold 400       ;; Diff lines - more than this requires explicit expand
   :context-expand-size 15})       ;; Lines to expand at a time

(def default-db
  {:route {:page :sessions :board-repo nil}  ;; :sessions, :session, :boards, :board
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
   :user {:author "user"}  ;; Default author for human comments
   :config default-config
   ;; Board/kanban state
   :boards []
   :board-tasks []
   :board-show-done false
   :selected-task nil})

;; Route helpers
(defn sessions-page? [db]
  (= :sessions (get-in db [:route :page])))

(defn session-page? [db]
  (= :session (get-in db [:route :page])))

(defn current-session-id [db]
  (get-in db [:route :session-id]))
