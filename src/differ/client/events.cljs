(ns differ.client.events
  "Re-frame event handlers."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [differ.client.db :as db]
            [differ.client.api :as api]))

;; LocalStorage helpers
(defn- load-preference [key]
  (try
    (when-let [value (.getItem js/localStorage key)]
      (keyword value))
    (catch :default _ nil)))

(defn- save-preference! [key value]
  (try
    (.setItem js/localStorage key (name value))
    (catch :default _ nil)))

;; Initialize
(rf/reg-event-fx
 :initialize
 (fn [_ _]
   (let [saved-view-mode (load-preference "differ-view-mode")]
     {:db (cond-> db/default-db
            saved-view-mode (assoc :diff-view-mode saved-view-mode))
      :dispatch-n [[:load-config]
                   [:load-sessions]]})))

;; Config
(rf/reg-event-fx
 :load-config
 (fn [_ _]
   {:http (api/fetch-config)}))

(rf/reg-event-db
 :config-loaded
 (fn [db [_ response]]
   (assoc db :config (:config response))))

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
      :dispatch-n [[:load-sessions]  ;; Ensure sessions list is loaded for selector
                   [:load-session session-id]
                   [:load-diff session-id]
                   [:load-comments session-id]
                   [:load-staged-files session-id]
                   [:load-untracked-files session-id]]})))

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
       (assoc :excluded-files (set (:excluded-files response)))
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
   (let [new-mode (if (= (:diff-view-mode db) :unified) :split :unified)]
     (save-preference! "differ-view-mode" new-mode)
     (assoc db :diff-view-mode new-mode))))

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

;; Expand context - fetch additional lines from server
(rf/reg-event-fx
 :expand-context
 (fn [{:keys [db]} [_ {:keys [file from to direction]}]]
   (let [session-id (db/current-session-id db)
         expand-size (get-in db [:config :context-expand-size] 15)
         total-lines (- to from -1)
         ;; Expand N lines at a time, from the appropriate end
         [fetch-from fetch-to]
         (if (<= total-lines expand-size)
           ;; Small gap - fetch all
           [from to]
           ;; Large gap - fetch N lines from the end closest to existing content
           (case direction
             :up   [(- to expand-size -1) to]      ;; Bottom N (closest to hunk below)
             :down [from (+ from expand-size -1)]  ;; Top N (closest to hunk above)
             :both [(- to expand-size -1) to]))]   ;; Default to bottom N
     {:http (api/fetch-context-lines session-id file fetch-from fetch-to)})))

;; Context lines loaded - merge into parsed diff
(rf/reg-event-db
 :context-lines-loaded
 (fn [db [_ file-path from-line to-line response]]
   (let [lines (:lines response)
         ;; Convert to diff line format (context lines with space prefix)
         context-lines (mapv (fn [{:keys [line content]}]
                               {:type :context
                                :content content
                                :old-num line
                                :new-num line
                                :file file-path})
                             lines)]
     ;; Store expanded context in db
     (update-in db [:expanded-context file-path]
                (fn [existing]
                  (merge (or existing {})
                         {[from-line to-line] context-lines}))))))

;; Comment form
(rf/reg-event-db
 :show-comment-form
 (fn [db [_ {:keys [file line parent-id side line-content context-before context-after]}]]
   (-> db
       (assoc :comment-form {:visible true
                             :file file
                             :line line
                             :parent-id parent-id
                             :side side
                             :line-content line-content
                             :context-before context-before
                             :context-after context-after
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
   (let [{:keys [file line parent-id text side line-content context-before context-after]} (:comment-form db)
         session-id (db/current-session-id db)
         author (get-in db [:user :author])]
     {:http (api/add-comment session-id {:file file
                                         :line line
                                         :text text
                                         :author author
                                         :parent-id parent-id
                                         :side side
                                         :line-content line-content
                                         :context-before context-before
                                         :context-after context-after})
      :db (assoc-in db [:comment-form :visible] false)})))

(rf/reg-event-fx
 :comment-added
 (fn [_ _]
   ;; SSE handles the refresh, no need to reload here
   {}))

;; Reply to resolved comment (from resolved comments panel)
(rf/reg-event-fx
 :submit-resolved-reply
 (fn [{:keys [db]} [_ {:keys [parent-id text]}]]
   (let [session-id (db/current-session-id db)
         author (get-in db [:user :author])]
     {:http (api/add-comment session-id {:text text
                                         :author author
                                         :parent-id parent-id})})))

;; Resolve comment
(rf/reg-event-fx
 :resolve-comment
 (fn [{:keys [db]} [_ comment-id]]
   (let [author (get-in db [:user :author])]
     {:http (api/resolve-comment comment-id author)})))

(rf/reg-event-fx
 :comment-resolved
 (fn [_ _]
   ;; SSE handles the refresh
   {}))

;; Unresolve comment
(rf/reg-event-fx
 :unresolve-comment
 (fn [{:keys [db]} [_ comment-id]]
   (let [author (get-in db [:user :author])]
     {:http (api/unresolve-comment comment-id author)})))

(rf/reg-event-fx
 :comment-unresolved
 (fn [_ _]
   ;; SSE handles the refresh
   {}))

;; Delete session
(rf/reg-event-fx
 :delete-session
 (fn [_ [_ session-id]]
   {:http (api/delete-session session-id)}))

(rf/reg-event-fx
 :session-deleted
 (fn [_ [_ _session-id _response]]
   {:dispatch [:load-sessions]}))

;; Update session settings
(rf/reg-event-fx
 :update-session
 (fn [{:keys [db]} [_ updates]]
   (let [session-id (get-in db [:current-session :session-id])]
     {:http (api/update-session session-id updates)})))

(rf/reg-event-fx
 :session-updated
 (fn [{:keys [db]} [_ response]]
   (let [session-id (get-in db [:current-session :session-id])]
     {:db (assoc db :current-session (:session response))
      :dispatch-n [[:load-diff session-id]
                   [:hide-session-settings]]})))

;; Session settings modal
(rf/reg-event-fx
 :show-session-settings
 (fn [{:keys [db]} _]
   (let [session-id (db/current-session-id db)]
     {:db (assoc db :session-settings-visible true)
      :dispatch [:load-branches session-id]})))

(rf/reg-event-db
 :hide-session-settings
 (fn [db _]
   (-> db
       (assoc :session-settings-visible false)
       (dissoc :branches))))

;; Load branches for autocomplete
(rf/reg-event-fx
 :load-branches
 (fn [{:keys [db]} [_ session-id]]
   {:db (assoc-in db [:loading :branches] true)
    :http (api/fetch-branches session-id)}))

(rf/reg-event-db
 :branches-loaded
 (fn [db [_ response]]
   (-> db
       (assoc :branches (:branches response))
       (assoc-in [:loading :branches] false))))

;; Git staging
(rf/reg-event-fx
 :load-staged-files
 (fn [{:keys [db]} [_ session-id]]
   {:http (api/fetch-staged-files session-id)}))

(rf/reg-event-db
 :staged-files-loaded
 (fn [db [_ response]]
   (-> db
       (assoc :staged-files (set (:staged response)))
       (assoc :unstaged-files (set (:unstaged response))))))

(rf/reg-event-fx
 :stage-file
 (fn [{:keys [db]} [_ file-path]]
   (let [session-id (db/current-session-id db)]
     {:http (api/stage-file session-id file-path)})))

(rf/reg-event-db
 :file-staged
 (fn [db [_ response]]
   (-> db
       (assoc :staged-files (set (:staged response)))
       (assoc :unstaged-files (set (:unstaged response))))))

;; Untracked files
(rf/reg-event-fx
 :load-untracked-files
 (fn [{:keys [db]} [_ session-id]]
   {:http (api/fetch-untracked-files session-id)}))

(rf/reg-event-db
 :untracked-files-loaded
 (fn [db [_ response]]
   (assoc db :untracked-files-list (vec (:untracked response)))))

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
 :sse-comment-unresolved
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

(rf/reg-event-fx
 :sse-diff-changed
 (fn [{:keys [db]} [_ _session-id]]
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
     {:dispatch-n [[:load-session session-id]
                   [:load-diff session-id]
                   [:load-untracked-files session-id]]})))

(rf/reg-event-fx
 :remove-manual-file
 (fn [{:keys [db]} [_ path]]
   (let [session-id (db/current-session-id db)]
     {:http (api/remove-manual-file session-id path)})))

(rf/reg-event-fx
 :manual-file-removed
 (fn [{:keys [db]} [_ _response]]
   (let [session-id (db/current-session-id db)]
     {:dispatch-n [[:load-session session-id]
                   [:load-diff session-id]
                   [:load-untracked-files session-id]]})))

(rf/reg-event-fx
 :restore-file
 (fn [{:keys [db]} [_ path]]
   (let [session-id (db/current-session-id db)]
     {:http (api/restore-file session-id path)})))

(rf/reg-event-fx
 :file-restored
 (fn [{:keys [db]} [_ _response]]
   (let [session-id (db/current-session-id db)]
     {:dispatch-n [[:load-session session-id]
                   [:load-diff session-id]
                   [:load-untracked-files session-id]]})))

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
