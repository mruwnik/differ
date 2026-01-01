(ns differ.client.subs
  "Re-frame subscriptions."
  (:require [re-frame.core :as rf]))

;; Route
(rf/reg-sub
 :route
 (fn [db _]
   (:route db)))

(rf/reg-sub
 :current-page
 :<- [:route]
 (fn [route _]
   (:page route)))

(rf/reg-sub
 :current-session-id
 :<- [:route]
 (fn [route _]
   (:session-id route)))

;; Sessions
(rf/reg-sub
 :sessions
 (fn [db _]
   (:sessions db)))

(rf/reg-sub
 :current-session
 (fn [db _]
   (:current-session db)))

;; Diff
(rf/reg-sub
 :diff
 (fn [db _]
   (:diff db)))

(rf/reg-sub
 :files
 :<- [:diff]
 (fn [diff _]
   (:files diff)))

(rf/reg-sub
 :changed-files
 :<- [:diff]
 (fn [diff _]
   (:changed-files diff)))

(rf/reg-sub
 :files-with-size
 :<- [:diff]
 (fn [diff _]
   (:files-with-size diff)))

(rf/reg-sub
 :is-git-repo
 :<- [:diff]
 (fn [diff _]
   (:is-git-repo diff)))

(rf/reg-sub
 :loaded-files
 (fn [db _]
   (or (:loaded-files db) {})))

(rf/reg-sub
 :loading-files
 (fn [db _]
   (or (:loading-files db) {})))

(rf/reg-sub
 :file-size
 :<- [:files-with-size]
 (fn [files-with-size [_ file-path]]
   (let [file-info (first (filter #(= (:path %) file-path) files-with-size))]
     (:size file-info))))

(rf/reg-sub
 :parsed-diff
 :<- [:diff]
 (fn [diff _]
   (:parsed diff)))

(rf/reg-sub
 :selected-file
 (fn [db _]
   (:selected-file db)))

(rf/reg-sub
 :selected-file-diff
 :<- [:parsed-diff]
 :<- [:selected-file]
 (fn [[parsed selected] _]
   (first (filter #(= (:file-b %) selected) parsed))))

;; Comments
(rf/reg-sub
 :comments
 (fn [db _]
   (:comments db)))

(rf/reg-sub
 :comments-for-file
 :<- [:comments]
 :<- [:selected-file]
 (fn [[comments selected] _]
   (get comments selected [])))

(rf/reg-sub
 :comments-for-line
 :<- [:comments-for-file]
 (fn [comments [_ line]]
   (vec (filter #(= (:line %) line) (or comments [])))))

;; Loading states
(rf/reg-sub
 :loading
 (fn [db _]
   (:loading db)))

(rf/reg-sub
 :loading?
 :<- [:loading]
 (fn [loading [_ key]]
   (get loading key false)))

(rf/reg-sub
 :any-loading?
 :<- [:loading]
 (fn [loading _]
   (some true? (vals loading))))

;; Comment form
(rf/reg-sub
 :comment-form
 (fn [db _]
   (:comment-form db)))

(rf/reg-sub
 :comment-form-visible?
 :<- [:comment-form]
 (fn [form _]
   (:visible form)))

;; SSE
(rf/reg-sub
 :sse-connected?
 (fn [db _]
   (get-in db [:sse :connected])))

;; Error
(rf/reg-sub
 :error
 (fn [db _]
   (:error db)))

;; User
(rf/reg-sub
 :user
 (fn [db _]
   (:user db)))

(rf/reg-sub
 :author
 :<- [:user]
 (fn [user _]
   (:author user)))

;; File picker
(rf/reg-sub
 :dir-handle
 (fn [db _]
   (:dir-handle db)))

;; Collapsed files state
(rf/reg-sub
 :collapsed-files
 (fn [db _]
   (or (:collapsed-files db) {})))

;; Expanded folders in file tree (default all expanded)
(rf/reg-sub
 :expanded-folders
 (fn [db _]
   (or (:expanded-folders db) {})))

;; Diff view mode (split = side-by-side, unified = stacked)
(rf/reg-sub
 :diff-view-mode
 (fn [db _]
   (or (:diff-view-mode db) :split)))

;; Files whose content has been explicitly expanded (for files with many lines)
(rf/reg-sub
 :content-expanded-files
 (fn [db _]
   (or (:content-expanded-files db) #{})))

;; Currently highlighted line (from URL hash)
(rf/reg-sub
 :highlighted-line
 (fn [db _]
   (:highlighted-line db)))
