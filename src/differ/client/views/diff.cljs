(ns differ.client.views.diff
  "Diff viewer component."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [differ.client.views.comments :as comments]))

(defn- file-id
  "Generate a DOM id for a file section."
  [file-path]
  (str "file-" (-> file-path
                   (str/replace #"[^a-zA-Z0-9]" "-")
                   (str/replace #"-+" "-"))))

(defn- file-status [file changed-files]
  (let [info (first (filter #(= (:path %) file) changed-files))]
    (or (:status info) :modified)))

;; File System Access API helpers
(defn- fs-api-supported? []
  (and (exists? js/window.showOpenFilePicker)
       (exists? js/window.showDirectoryPicker)))

(defn- resolve-path
  "Resolve a file/folder handle to a path relative to the root directory."
  [root-handle handle]
  (-> (.resolve root-handle handle)
      (.then (fn [parts]
               (when parts (str/join "/" (js->clj parts)))))
      (.catch (fn [_] nil))))

(defn- pick-files! []
  (let [root-handle @(rf/subscribe [:dir-handle])]
    (if-not root-handle
      ;; No root yet - ask user to set it first
      (js/alert "Please set the project folder first")
      ;; Pick files relative to root
      (-> (js/window.showOpenFilePicker
           #js {:multiple true
                :startIn root-handle})
          (.then (fn [handles]
                   (js/Promise.all
                    (clj->js (map #(resolve-path root-handle %) handles)))))
          (.then (fn [paths]
                   (doseq [path (filter some? (js->clj paths))]
                     (rf/dispatch [:add-manual-file path]))))
          (.catch (fn [e]
                    (when-not (= (.-name e) "AbortError")
                      (js/console.error "File picker error:" e))))))))

(defn- pick-folder! []
  (let [root-handle @(rf/subscribe [:dir-handle])]
    (if-not root-handle
      (js/alert "Please set the project folder first")
      (-> (js/window.showDirectoryPicker
           #js {:startIn root-handle})
          (.then (fn [handle]
                   (resolve-path root-handle handle)))
          (.then (fn [path]
                   (when path
                     (rf/dispatch [:add-manual-file path]))))
          (.catch (fn [e]
                    (when-not (= (.-name e) "AbortError")
                      (js/console.error "Directory picker error:" e))))))))

(defn- set-project-folder! []
  (-> (js/window.showDirectoryPicker #js {:mode "read"})
      (.then (fn [handle]
               (rf/dispatch [:set-dir-handle handle])))
      (.catch (fn [e]
                (when-not (= (.-name e) "AbortError")
                  (js/console.error "Directory picker error:" e))))))

(defn add-file-form []
  (let [dir-handle @(rf/subscribe [:dir-handle])]
    (if (fs-api-supported?)
      [:div.add-file-form {:style {:margin-bottom "8px"}}
       (if dir-handle
         ;; Have project folder - show file/folder buttons
         [:div {:style {:display "flex" :gap "4px"}}
          [:button.btn.btn-secondary
           {:on-click pick-files!
            :style {:flex 1 :font-size "12px"}}
           "+ Files"]
          [:button.btn.btn-secondary
           {:on-click pick-folder!
            :style {:flex 1 :font-size "12px"}}
           "+ Folder"]]
         ;; No project folder yet
         [:button.btn.btn-secondary
          {:on-click set-project-folder!
           :style {:width "100%" :font-size "12px"}}
          "Set project folder"])]
      ;; Fallback for browsers without File System Access API
      (let [input-value (atom "")]
        (fn []
          [:div.add-file-form
           [:div {:style {:display "flex" :gap "4px" :margin-bottom "8px"}}
            [:input {:type "text"
                     :placeholder "path/to/file or folder"
                     :value @input-value
                     :on-change #(reset! input-value (.. % -target -value))
                     :on-key-down #(when (= (.-key %) "Enter")
                                     (when (seq @input-value)
                                       (rf/dispatch [:add-manual-file @input-value])
                                       (reset! input-value "")))
                     :style {:flex 1
                             :padding "6px 8px"
                             :border "1px solid #d1d5da"
                             :border-radius "4px"
                             :font-size "12px"}}]
            [:button.btn.btn-primary
             {:on-click #(when (seq @input-value)
                           (rf/dispatch [:add-manual-file @input-value])
                           (reset! input-value ""))
              :style {:padding "6px 12px" :font-size "12px"}}
             "Add"]]])))))

(defn- scroll-to-file! [file-path]
  (when-let [element (.getElementById js/document (file-id file-path))]
    (.scrollIntoView element #js {:behavior "smooth" :block "start"})))

;; Tree building functions
(defn- insert-path
  "Insert a file path into a tree structure."
  [tree parts full-path]
  (if (empty? parts)
    tree
    (let [[part & rest-parts] parts
          is-file? (empty? rest-parts)
          children (or (:children tree) [])
          existing-idx (first (keep-indexed #(when (= (:name %2) part) %1) children))]
      (if existing-idx
        ;; Node exists - update it
        (if is-file?
          tree ;; File already exists, skip
          (update-in tree [:children existing-idx]
                     #(insert-path % rest-parts full-path)))
        ;; Node doesn't exist - create it
        (let [new-node (if is-file?
                         {:name part :path full-path :type :file}
                         (insert-path {:name part :type :folder :children []}
                                      rest-parts full-path))]
          (update tree :children #(conj (or % []) new-node)))))))

(defn- build-tree
  "Build a nested tree structure from a list of file paths."
  [files]
  (reduce
   (fn [tree file-path]
     (let [parts (str/split file-path #"/")]
       (insert-path tree parts file-path)))
   {:name "root" :type :folder :children []}
   (sort files)))

(defn- sort-tree-children
  "Sort children: folders first, then files, both alphabetically."
  [children]
  (sort-by (juxt #(if (= (:type %) :folder) 0 1) :name) children))

(declare tree-node)

(defn tree-folder
  "Render a folder node with collapsible children."
  [node depth selected-file changed-files expanded-folders]
  (let [folder-path (str/join "/" (take (inc depth) (iterate identity (:name node))))
        expanded? (get expanded-folders (:name node) true)
        children (sort-tree-children (:children node))]
    [:div.tree-folder
     [:div.tree-folder-header
      {:on-click #(rf/dispatch [:toggle-folder-expanded (:name node)])}
      [:span.tree-indent {:style {:width (str (* depth 12) "px")}}]
      [:span.tree-chevron (if expanded? "▼" "▶")]
      [:span.tree-folder-icon "📁"]
      [:span.tree-folder-name (:name node)]]
     (when expanded?
       [:div.tree-folder-children
        (for [child children]
          ^{:key (or (:path child) (:name child))}
          [tree-node child (inc depth) selected-file changed-files expanded-folders])])]))

(defn tree-file
  "Render a file node."
  [node depth selected-file changed-files]
  (let [status (file-status (:path node) changed-files)
        selected? (= (:path node) selected-file)]
    [:div.tree-file
     {:class (str (name status) (when selected? " active"))
      :on-click #(do
                   (rf/dispatch [:select-file (:path node)])
                   (scroll-to-file! (:path node)))
      :title (:path node)}
     [:span.tree-indent {:style {:width (str (* depth 12) "px")}}]
     [:span.tree-file-icon "📄"]
     [:span.tree-file-name (:name node)]]))

(defn tree-node
  "Render a tree node (either folder or file)."
  [node depth selected-file changed-files expanded-folders]
  (if (= (:type node) :folder)
    [tree-folder node depth selected-file changed-files expanded-folders]
    [tree-file node depth selected-file changed-files]))

(defn file-tree
  "Render the file tree."
  [files selected-file changed-files expanded-folders]
  (let [tree (build-tree files)
        children (sort-tree-children (:children tree))]
    [:div.file-tree
     (for [child children]
       ^{:key (or (:path child) (:name child))}
       [tree-node child 0 selected-file changed-files expanded-folders])]))

(defn file-list []
  (let [files @(rf/subscribe [:files])
        changed-files @(rf/subscribe [:changed-files])
        selected @(rf/subscribe [:selected-file])
        expanded-folders @(rf/subscribe [:expanded-folders])]
    [:div.file-list
     [:h3 "Files"]
     [add-file-form]
     (if (empty? files)
       [:p {:style {:color "#6a737d" :font-size "13px"}}
        "No files in review"]
       [file-tree files selected changed-files expanded-folders])]))

(defn- parse-line-type [line]
  (cond
    (str/starts-with? line "+") :addition
    (str/starts-with? line "-") :deletion
    :else :context))

(defn- line-content [line]
  (if (or (str/starts-with? line "+")
          (str/starts-with? line "-")
          (str/starts-with? line " "))
    (subs line 1)
    line))

(defn- line-id
  "Generate a DOM id for a specific line in a file."
  [file-path line-num]
  (str (file-id file-path) "-L" line-num))

(defn- update-line-hash!
  "Update the URL hash to point to a specific line."
  [file-path line-num]
  (let [hash (str "#" (js/encodeURIComponent file-path) ":" line-num)]
    (.replaceState js/history nil "" hash)
    (rf/dispatch [:set-highlighted-line file-path line-num])))

(defn diff-line-split
  "Render a diff line in split (side-by-side) mode with separate old/new columns."
  [{:keys [type content old-num new-num file]} file-comments comment-form highlighted-line]
  (let [line-num (or new-num old-num)
        comments-for-line (filterv #(= (:line %) line-num) (or file-comments []))
        is-addition (= type :addition)
        is-deletion (= type :deletion)
        is-context (= type :context)
        is-highlighted (and (= (:file highlighted-line) file)
                            (= (:line highlighted-line) line-num))
        ;; Check if comment form should appear after this line
        show-form-here? (and (:visible comment-form)
                             (= (:file comment-form) file)
                             (= (:line comment-form) line-num)
                             (not (:parent-id comment-form)))]
    [:<>
     [:tr.diff-line.split
      {:id (line-id file line-num)
       :class (when is-highlighted "highlighted")}
      ;; Left side (old file)
      [:td.diff-line-num.old
       {:class (when is-addition "empty")}
       (when-not is-addition old-num)]
      [:td.diff-line-content.old
       {:class (cond is-deletion "deletion" is-addition "empty" :else nil)}
       [:pre {:style {:margin 0 :font-family "inherit"}}
        (cond
          is-deletion content
          is-context content
          :else "")]]
      ;; Right side (new file)
      [:td.diff-line-num.new
       {:class (str (when is-deletion "empty") " line-num-with-button")}
       (when-not is-deletion
         [:<>
          [:button.add-comment-btn
           {:on-click #(rf/dispatch [:show-comment-form {:file file :line new-num}])
            :title "Add comment"}
           "+"]
          [:a.line-num-link
           {:href (str "#" (js/encodeURIComponent file) ":" new-num)
            :on-click (fn [e]
                        (.preventDefault e)
                        (update-line-hash! file new-num))}
           new-num]])]
      [:td.diff-line-content.new
       {:class (cond is-addition "addition" is-deletion "empty" :else nil)}
       [:pre {:style {:margin 0 :font-family "inherit"}}
        (cond
          is-addition content
          is-context content
          :else "")]]]
     ;; Inline comments for this line
     (when (seq comments-for-line)
       [:tr
        [:td {:col-span 4 :style {:padding 0}}
         [comments/comment-threads comments-for-line file line-num]]])
     ;; Inline comment form after this line
     (when show-form-here?
       [:tr.comment-form-row
        [:td {:col-span 4 :style {:padding 0}}
         [comments/comment-form-inline comment-form]]])]))

(defn diff-line-unified
  "Render a diff line in unified (stacked) mode with +/- prefix."
  [{:keys [type content old-num new-num file]} file-comments comment-form highlighted-line]
  (let [line-num (or new-num old-num)
        comments-for-line (filterv #(= (:line %) line-num) (or file-comments []))
        prefix (case type
                 :addition "+"
                 :deletion "-"
                 " ")
        is-highlighted (and (= (:file highlighted-line) file)
                            (= (:line highlighted-line) line-num))
        ;; Check if comment form should appear after this line
        show-form-here? (and (:visible comment-form)
                             (= (:file comment-form) file)
                             (= (:line comment-form) line-num)
                             (not (:parent-id comment-form)))]
    [:<>
     [:tr.diff-line
      {:id (line-id file line-num)
       :class (str (name type) (when is-highlighted " highlighted"))}
      [:td.diff-line-num.unified.line-num-with-button
       [:button.add-comment-btn
        {:on-click #(rf/dispatch [:show-comment-form {:file file :line line-num}])
         :title "Add comment"}
        "+"]
       [:a.line-num-link
        {:href (str "#" (js/encodeURIComponent file) ":" line-num)
         :on-click (fn [e]
                     (.preventDefault e)
                     (update-line-hash! file line-num))}
        line-num]]
      [:td.diff-line-content.unified
       [:pre {:style {:margin 0 :font-family "inherit"}}
        (str prefix " " content)]]]
     ;; Inline comments for this line
     (when (seq comments-for-line)
       [:tr
        [:td {:col-span 2 :style {:padding 0}}
         [comments/comment-threads comments-for-line file line-num]]])
     ;; Inline comment form after this line
     (when show-form-here?
       [:tr.comment-form-row
        [:td {:col-span 2 :style {:padding 0}}
         [comments/comment-form-inline comment-form]]])]))

(defn diff-line
  "Render a diff line in the current view mode."
  [line-data file-comments view-mode comment-form highlighted-line]
  (if (= view-mode :unified)
    [diff-line-unified line-data file-comments comment-form highlighted-line]
    [diff-line-split line-data file-comments comment-form highlighted-line]))

(defn- expand-context-row
  "Renders a clickable row to expand context lines."
  [file from-line to-line direction view-mode]
  (let [lines-hidden (- to-line from-line)
        col-span (if (= view-mode :unified) 2 4)]
    (when (pos? lines-hidden)
      [:tr.diff-expand-row
       [:td {:col-span col-span}
        [:button.diff-expand-btn
         {:on-click #(rf/dispatch [:expand-context {:file file
                                                    :from from-line
                                                    :to to-line
                                                    :direction direction}])
          :title (str "Load " lines-hidden " more lines")}
         [:span.diff-expand-icon
          (case direction
            :up "↑"
            :down "↓"
            :both "↕")]
         [:span.diff-expand-text
          (str " Expand " lines-hidden " hidden lines")]]]])))

(defn diff-hunk [{:keys [header lines file old-start new-start]} start-old start-new file-comments show-expand-above? view-mode comment-form highlighted-line]
  (let [line-data (loop [remaining lines
                         old-num start-old
                         new-num start-new
                         result []]
                    (if (empty? remaining)
                      result
                      (let [line (first remaining)
                            type (parse-line-type line)
                            content (line-content line)
                            [next-old next-new entry]
                            (case type
                              :addition [(inc old-num) new-num
                                         {:type type :content content
                                          :old-num nil :new-num old-num :file file}]
                              :deletion [old-num (inc new-num)
                                         {:type type :content content
                                          :old-num new-num :new-num nil :file file}]
                              ;; context
                              [(inc old-num) (inc new-num)
                               {:type type :content content
                                :old-num old-num :new-num new-num :file file}])]
                        (recur (rest remaining)
                               next-old
                               next-new
                               (conj result entry)))))
        col-span (if (= view-mode :unified) 2 4)]
    [:<>
     ;; Expand button above hunk if there's a gap from line 1 or previous hunk
     (when show-expand-above?
       [expand-context-row file 1 (dec start-new) :up view-mode])
     [:tr.diff-hunk-header
      [:td {:col-span col-span} header]]
     (for [[idx line] (map-indexed vector line-data)]
       ^{:key idx}
       [diff-line line file-comments view-mode comment-form highlighted-line])]))

(def large-file-threshold 50000) ;; Characters - files larger than this require explicit load
(def line-count-threshold 100) ;; Files with more lines require explicit expansion

(defn- format-file-size [bytes]
  (cond
    (< bytes 1024) (str bytes " B")
    (< bytes (* 1024 1024)) (str (.toFixed (/ bytes 1024) 1) " KB")
    :else (str (.toFixed (/ bytes (* 1024 1024)) 1) " MB")))

(defn- count-diff-lines
  "Count total lines in a file diff's hunks."
  [file-diff]
  (reduce + 0 (map #(count (:lines %)) (:hunks file-diff))))

(defn diff-file-section
  "Renders a single file's diff with header and hunks."
  [file-path file-diff all-comments collapsed?]
  (let [comment-form @(rf/subscribe [:comment-form])
        file-comments (get all-comments file-path [])
        files-with-size @(rf/subscribe [:files-with-size])
        loaded-files @(rf/subscribe [:loaded-files])
        loading-files @(rf/subscribe [:loading-files])
        content-expanded @(rf/subscribe [:content-expanded-files])
        highlighted-line @(rf/subscribe [:highlighted-line])
        view-mode @(rf/subscribe [:diff-view-mode])
        file-info (first (filter #(= (:path %) file-path) files-with-size))
        file-size (or (:size file-info) 0)
        is-large-file (> file-size large-file-threshold)
        is-loaded (get loaded-files file-path false)
        is-loading (get loading-files file-path false)
        ;; Show content if: has diff, or is loaded, or is small file
        show-content (or file-diff is-loaded (not is-large-file))
        hunks (:hunks file-diff)
        line-count (count-diff-lines file-diff)
        needs-expansion (and (> line-count line-count-threshold)
                             (not (contains? content-expanded file-path)))
        is-content-expanded (contains? content-expanded file-path)]
    [:div.diff-file {:id (file-id file-path)}
     [:div.diff-file-header
      {:on-click #(rf/dispatch [:toggle-file-collapsed file-path])}
      [:span.diff-file-collapse
       (if collapsed? "▶" "▼")]
      [:span.diff-file-path file-path]
      (when (pos? file-size)
        [:span.diff-file-size {:style {:margin-left "8px" :color "#6a737d" :font-weight "normal"}}
         (format-file-size file-size)])]
     (when-not collapsed?
       [:<>
        (cond
          ;; File is loading
          is-loading
          [:div.diff-loading-file
           {:style {:padding "24px" :text-align "center" :color "#6a737d"}}
           "Loading file content..."]

          ;; Large file that hasn't been loaded yet and has no diff
          (and is-large-file (not file-diff) (not is-loaded))
          [:div.diff-large-file
           {:style {:padding "24px" :text-align "center"}}
           [:p {:style {:color "#6a737d" :margin-bottom "12px"}}
            (str "Large file (" (format-file-size file-size) ")")]
           [:button.btn.btn-secondary
            {:on-click #(rf/dispatch [:load-file-content file-path])}
            "Load file content"]]

          ;; File has many lines and needs explicit expansion
          (and show-content file-diff (seq hunks) needs-expansion)
          [:div.diff-expand-content
           {:style {:padding "12px 16px" :background "#f6f8fa" :border-top "1px solid #e1e4e8"}}
           [:button.btn.btn-secondary
            {:on-click #(rf/dispatch [:expand-file-content file-path])
             :style {:width "100%"}}
            (str "Show " line-count " lines")]]

          ;; Has diff content to show (either small file or explicitly expanded)
          (and show-content file-diff (seq hunks))
          [:table.diff-table {:class (when (= view-mode :unified) "unified")}
           [:tbody
            (for [[idx hunk] (map-indexed vector hunks)]
              (let [prev-hunk (when (pos? idx) (nth hunks (dec idx)))
                    prev-end-line (when prev-hunk
                                    (+ (:new-start prev-hunk) (:new-count prev-hunk)))
                    current-start (:new-start hunk)
                    has-gap? (if prev-hunk
                               (> current-start prev-end-line)
                               (> current-start 1))]
                ^{:key idx}
                [:<>
                 (when has-gap?
                   [expand-context-row file-path
                    (or prev-end-line 1)
                    (dec current-start)
                    (if prev-hunk :both :up)
                    view-mode])
                 [diff-hunk (assoc hunk :file file-path)
                  (:new-start hunk)
                  (:old-start hunk)
                  file-comments
                  false
                  view-mode
                  comment-form
                  highlighted-line]]))]]

          ;; No diff available
          :else
          [:div.diff-empty-file
           [:p "No diff available for this file"]])])]))

(defn all-files-diff
  "Renders all files stacked vertically."
  []
  (let [files @(rf/subscribe [:files])
        parsed-diff @(rf/subscribe [:parsed-diff])
        all-comments @(rf/subscribe [:comments])
        collapsed-files @(rf/subscribe [:collapsed-files])]
    (if (empty? files)
      [:div.empty-state
       [:p "No files in review"]]
      [:div.all-files-container
       (for [file files]
         (let [file-diff (first (filter #(= (:file-b %) file) parsed-diff))
               collapsed? (get collapsed-files file false)]
           ^{:key file}
           [diff-file-section file file-diff all-comments collapsed?]))])))

(defn session-header []
  (let [session @(rf/subscribe [:current-session])
        view-mode @(rf/subscribe [:diff-view-mode])]
    (when session
      [:div {:style {:margin-bottom "16px"
                     :padding "12px 16px"
                     :background "#f6f8fa"
                     :border-radius "6px"
                     :display "flex"
                     :justify-content "space-between"
                     :align-items "center"}}
       [:div
        [:strong (:project session)]
        [:span {:style {:margin "0 8px" :color "#6a737d"}} "/"]
        [:span (:branch session)]
        [:span {:style {:margin "0 8px" :color "#6a737d"}} "→"]
        [:span {:style {:color "#6a737d"}} (:target-branch session)]]
       [:div {:style {:display "flex" :gap "8px" :align-items "center"}}
        ;; Diff view mode toggle
        [:div.diff-view-toggle
         [:button.btn-toggle
          {:class (when (= view-mode :split) "active")
           :on-click #(when (not= view-mode :split)
                        (rf/dispatch [:toggle-diff-view-mode]))
           :title "Split view"}
          "Split"]
         [:button.btn-toggle
          {:class (when (= view-mode :unified) "active")
           :on-click #(when (not= view-mode :unified)
                        (rf/dispatch [:toggle-diff-view-mode]))
           :title "Unified view"}
          "Unified"]]
        [:span {:class (str "session-badge"
                            (when (zero? (or (:unresolved-count session) 0)) " zero"))}
         (let [count (or (:unresolved-count session) 0)]
           (if (zero? count)
             "All resolved"
             (str count " unresolved")))]
        [:button.btn.btn-secondary
         {:on-click #(when (js/confirm "Archive this session?")
                       (rf/dispatch [:delete-session (:session-id session)])
                       (rf/dispatch [:navigate-sessions]))}
         "Archive"]]])))

(defn diff-view []
  [:div
   [session-header]
   [:div.diff-container
    [file-list]
    [:div.diff-content
     [all-files-diff]]]])
