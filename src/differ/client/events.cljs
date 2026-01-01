(ns differ.client.events
  "Re-frame event handlers."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [differ.client.db :as db]
            [differ.client.api :as api]))

;; Initialize
(rf/reg-event-fx
 :initialize
 (fn [_ _]
   {:db db/default-db
    :dispatch [:load-sessions]}))

;; Navigation
(rf/reg-event-fx
 :navigate-sessions
 (fn [{:keys [db]} [_ opts]]
   (let [replace? (:replace opts)]
     {:db (-> db
              (assoc-in [:route :page] :sessions)
              (assoc-in [:route :session-id] nil))
      :sse-disconnect true
      :push-url {:path "/" :replace? replace?}
      :dispatch [:load-sessions]})))

(rf/reg-event-fx
 :navigate-session
 (fn [{:keys [db]} [_ session-id opts]]
   (let [replace? (:replace opts)]
     {:db (-> db
              (assoc-in [:route :page] :session)
              (assoc-in [:route :session-id] session-id))
      :sse-connect session-id
      :push-url {:path (str "/session/" session-id) :replace? replace?}
      :dispatch-n [[:load-session session-id]
                   [:load-diff session-id]
                   [:load-comments session-id]]})))

;; Parse URL hash for line highlighting
(rf/reg-event-fx
 :parse-url-hash
 (fn [{:keys [db]} _]
   (let [hash (.-hash js/location)]
     (if (and hash (not= hash "") (not= hash "#"))
       (let [hash-content (subs hash 1) ;; Remove leading #
             decoded (js/decodeURIComponent hash-content)
             ;; Parse format: "file/path:123"
             last-colon-idx (.lastIndexOf decoded ":")
             file-path (when (pos? last-colon-idx)
                         (subs decoded 0 last-colon-idx))
             line-str (when (pos? last-colon-idx)
                        (subs decoded (inc last-colon-idx)))
             line-num (when line-str (js/parseInt line-str 10))]
         (if (and file-path line-num (not (js/isNaN line-num)))
           {:dispatch-n [[:set-highlighted-line file-path line-num]
                         [:expand-file-content file-path]]
            :scroll-to-line {:file file-path :line line-num}}
           {}))
       {}))))

;; Effect to scroll to a specific line after render
(rf/reg-fx
 :scroll-to-line
 (fn [{:keys [file line]}]
   ;; Use setTimeout to wait for content expansion and render
   (js/setTimeout
    (fn []
      (let [line-id (str "file-"
                         (-> file
                             (str/replace #"[^a-zA-Z0-9]" "-")
                             (str/replace #"-+" "-"))
                         "-L" line)
            element (.getElementById js/document line-id)]
        (when element
          (.scrollIntoView element #js {:behavior "smooth" :block "center"}))))
    1000)))

;; URL effect (preserves hash)
(rf/reg-fx
 :push-url
 (fn [{:keys [path replace?]}]
   (let [current-hash (.-hash js/location)
         full-path (str path current-hash)]
     (if replace?
       (.replaceState js/history nil "" full-path)
       (.pushState js/history nil "" full-path)))))

;; Sessions
(rf/reg-event-fx
 :load-sessions
 (fn [{:keys [db]} _]
   {:db (assoc-in db [:loading :sessions] true)
    :http (api/fetch-sessions)}))

(rf/reg-event-db
 :sessions-loaded
 (fn [db [_ response]]
   (-> db
       (assoc :sessions (:sessions response))
       (assoc-in [:loading :sessions] false))))

(rf/reg-event-fx
 :create-session
 (fn [{:keys [db]} [_ repo-path]]
   {:db (assoc-in db [:loading :sessions] true)
    :http (api/create-session repo-path)}))

(rf/reg-event-fx
 :session-created
 (fn [_ [_ response]]
   (let [session-id (get-in response [:session :id])]
     {:dispatch-n [[:load-sessions]
                   [:navigate-session session-id]]})))

;; Single session
(rf/reg-event-fx
 :load-session
 (fn [{:keys [db]} [_ session-id]]
   {:db (assoc-in db [:loading :session] true)
    :http (api/fetch-session session-id)}))

(rf/reg-event-db
 :session-loaded
 (fn [db [_ response]]
   (-> db
       (assoc :current-session response)
       (assoc-in [:loading :session] false))))

;; Diff
(rf/reg-event-fx
 :load-diff
 (fn [{:keys [db]} [_ session-id]]
   {:db (assoc-in db [:loading :diff] true)
    :http (api/fetch-diff session-id)}))

(rf/reg-event-fx
 :diff-loaded
 (fn [{:keys [db]} [_ response]]
   (let [first-file (first (:files response))]
     {:db (-> db
              (assoc :diff {:raw (:diff response)
                            :parsed (:parsed response)
                            :files (:files response)
                            :files-with-size (:files-with-size response)
                            :changed-files (:changed-files response)
                            :is-git-repo (:is-git-repo response)})
              (assoc :selected-file (or (:selected-file db) first-file))
              (assoc-in [:loading :diff] false))
      ;; Parse URL hash after diff is loaded
      :dispatch [:parse-url-hash]})))

;; Comments
(rf/reg-event-fx
 :load-comments
 (fn [{:keys [db]} [_ session-id]]
   {:db (assoc-in db [:loading :comments] true)
    :http (api/fetch-comments session-id)}))

(rf/reg-event-db
 :comments-loaded
 (fn [db [_ response]]
   (let [comments (:comments response)
          ;; Group by file
         by-file (group-by :file comments)]
     (-> db
         (assoc :comments by-file)
         (assoc-in [:loading :comments] false)))))

;; Select file
(rf/reg-event-db
 :select-file
 (fn [db [_ file]]
   (assoc db :selected-file file)))

;; Toggle diff view mode (side-by-side vs unified)
(rf/reg-event-db
 :toggle-diff-view-mode
 (fn [db _]
   (update db :diff-view-mode #(if (= % :unified) :split :unified))))

;; Toggle file collapsed state
(rf/reg-event-db
 :toggle-file-collapsed
 (fn [db [_ file]]
   (update-in db [:collapsed-files file] not)))

;; Toggle folder expanded state in file tree
(rf/reg-event-db
 :toggle-folder-expanded
 (fn [db [_ folder-name]]
   ;; Default is expanded (true), so we toggle from that
   (update-in db [:expanded-folders folder-name]
              #(if (nil? %) false (not %)))))

;; Expand file content (for files with many lines)
(rf/reg-event-db
 :expand-file-content
 (fn [db [_ file-path]]
   (update db :content-expanded-files #(conj (or % #{}) file-path))))

;; Set highlighted line (from URL hash)
(rf/reg-event-db
 :set-highlighted-line
 (fn [db [_ file-path line-num]]
   (assoc db :highlighted-line {:file file-path :line line-num})))

;; Clear highlighted line
(rf/reg-event-db
 :clear-highlighted-line
 (fn [db _]
   (dissoc db :highlighted-line)))

;; Expand context (placeholder - needs backend support)
(rf/reg-event-fx
 :expand-context
 (fn [{:keys [db]} [_ {:keys [file from to direction]}]]
   ;; TODO: Implement backend API call to fetch additional context lines
   ;; For now, just log the request
   (js/console.log "Expand context requested:" file "lines" from "-" to direction)
   {:db db}))

;; Comment form
(rf/reg-event-db
 :show-comment-form
 (fn [db [_ {:keys [file line parent-id]}]]
   (-> db
       (assoc :comment-form {:visible true
                             :file file
                             :line line
                             :parent-id parent-id
                             :text ""}))))

(rf/reg-event-db
 :hide-comment-form
 (fn [db _]
   (assoc-in db [:comment-form :visible] false)))

(rf/reg-event-db
 :update-comment-text
 (fn [db [_ text]]
   (assoc-in db [:comment-form :text] text)))

(rf/reg-event-fx
 :submit-comment
 (fn [{:keys [db]} _]
   (let [{:keys [file line parent-id text]} (:comment-form db)
         session-id (db/current-session-id db)
         author (get-in db [:user :author])]
     {:http (api/add-comment session-id {:file file
                                         :line line
                                         :text text
                                         :author author
                                         :parent-id parent-id})
      :db (assoc-in db [:comment-form :visible] false)})))

(rf/reg-event-fx
 :comment-added
 (fn [{:keys [db]} [_ response]]
   (let [session-id (db/current-session-id db)]
     {:dispatch [:load-comments session-id]})))

;; Resolve comment
(rf/reg-event-fx
 :resolve-comment
 (fn [{:keys [db]} [_ comment-id]]
   (let [author (get-in db [:user :author])]
     {:http (api/resolve-comment comment-id author)})))

(rf/reg-event-fx
 :comment-resolved
 (fn [{:keys [db]} [_ comment-id _response]]
   (let [session-id (db/current-session-id db)]
     {:dispatch [:load-comments session-id]})))

;; Unresolve comment
(rf/reg-event-fx
 :unresolve-comment
 (fn [{:keys [db]} [_ comment-id]]
   (let [author (get-in db [:user :author])]
     {:http (api/unresolve-comment comment-id author)})))

(rf/reg-event-fx
 :comment-unresolved
 (fn [{:keys [db]} [_ comment-id _response]]
   (let [session-id (db/current-session-id db)]
     {:dispatch [:load-comments session-id]})))

;; Delete session
(rf/reg-event-fx
 :delete-session
 (fn [_ [_ session-id]]
   {:http (api/delete-session session-id)}))

(rf/reg-event-fx
 :session-deleted
 (fn [_ [_ _session-id _response]]
   {:dispatch [:load-sessions]}))

;; SSE events
(rf/reg-event-db
 :sse-connected
 (fn [db _]
   (assoc-in db [:sse :connected] true)))

(rf/reg-event-db
 :sse-disconnected
 (fn [db _]
   (assoc-in db [:sse :connected] false)))

(rf/reg-event-db
 :sse-error
 (fn [db _]
   (assoc-in db [:sse :connected] false)))

(rf/reg-event-db
 :sse-client-id
 (fn [db [_ client-id]]
   (assoc-in db [:sse :client-id] client-id)))

(rf/reg-event-fx
 :sse-comment-added
 (fn [{:keys [db]} [_ _comment]]
   (let [session-id (db/current-session-id db)]
     (when session-id
       {:dispatch [:load-comments session-id]}))))

(rf/reg-event-fx
 :sse-comment-resolved
 (fn [{:keys [db]} [_ _comment-id]]
   (let [session-id (db/current-session-id db)]
     (when session-id
       {:dispatch [:load-comments session-id]}))))

(rf/reg-event-fx
 :sse-session-updated
 (fn [{:keys [db]} [_ _session-id]]
   (let [session-id (db/current-session-id db)]
     (when session-id
       {:dispatch [:load-session session-id]}))))

(rf/reg-event-fx
 :sse-files-changed
 (fn [{:keys [db]} [_ _files]]
   (let [session-id (db/current-session-id db)]
     (when session-id
       {:dispatch [:load-diff session-id]}))))

;; File picker directory handle
(rf/reg-event-db
 :set-dir-handle
 (fn [db [_ handle]]
   (assoc db :dir-handle handle)))

;; Manual file management
(rf/reg-event-fx
 :add-manual-file
 (fn [{:keys [db]} [_ path]]
   (let [session-id (db/current-session-id db)]
     {:http (api/add-manual-file session-id path)})))

(rf/reg-event-fx
 :manual-file-added
 (fn [{:keys [db]} [_ _response]]
   (let [session-id (db/current-session-id db)]
     {:dispatch [:load-diff session-id]})))

(rf/reg-event-fx
 :remove-manual-file
 (fn [{:keys [db]} [_ path]]
   (let [session-id (db/current-session-id db)]
     {:http (api/remove-manual-file session-id path)})))

(rf/reg-event-fx
 :manual-file-removed
 (fn [{:keys [db]} [_ _response]]
   (let [session-id (db/current-session-id db)]
     {:dispatch [:load-diff session-id]})))

;; Load file content on demand (for large files)
(rf/reg-event-fx
 :load-file-content
 (fn [{:keys [db]} [_ file-path]]
   (let [session-id (db/current-session-id db)]
     {:db (assoc-in db [:loading-files file-path] true)
      :http (api/fetch-file-content session-id file-path)})))

(rf/reg-event-db
 :file-content-loaded
 (fn [db [_ file-path response]]
   (let [parsed-diff (:parsed response)
         ;; Add the loaded file's parsed diff to the existing parsed diffs
         current-parsed (get-in db [:diff :parsed] [])
         ;; Replace any existing entry for this file, or add new one
         updated-parsed (if (some #(= (:file-b %) file-path) current-parsed)
                          (mapv #(if (= (:file-b %) file-path)
                                   parsed-diff
                                   %)
                                current-parsed)
                          (conj current-parsed parsed-diff))]
     (-> db
         (assoc-in [:diff :parsed] updated-parsed)
         (assoc-in [:loaded-files file-path] true)
         (assoc-in [:loading-files file-path] false)))))

;; Error handling
(rf/reg-event-db
 :api-error
 (fn [db [_ message]]
   (assoc db :error message)))

(rf/reg-event-db
 :clear-error
 (fn [db _]
   (assoc db :error nil)))
