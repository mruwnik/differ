(ns differ.client.views.diff
  "Diff viewer component."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
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
  (let [expanded? (get expanded-folders (:name node) true)
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
     [:span.tree-file-name (:name node)]
     [:button.tree-file-remove
      {:on-click (fn [e]
                   (.stopPropagation e)
                   (rf/dispatch [:remove-manual-file (:path node)]))
       :title "Remove from review"}
      "×"]]))

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

(defn- untracked-tree-file
  "Render an untracked file node - clicking adds it to review."
  [node depth]
  [:div.tree-file.untracked
   {:on-click #(rf/dispatch [:add-manual-file (:path node)])
    :title (str "Add " (:path node) " to review")
    :style {:color "#6a737d"}}
   [:span.tree-indent {:style {:width (str (* depth 12) "px")}}]
   [:span.tree-file-icon "📄"]
   [:span.tree-file-name {:style {:font-style "italic"}} (:name node)]])

(declare untracked-tree-node)

(defn- untracked-tree-folder
  "Render an untracked folder node."
  [node depth expanded-folders]
  (let [expanded? (get expanded-folders (str "untracked-" (:name node)) true)
        children (sort-tree-children (:children node))]
    [:div.tree-folder
     [:div.tree-folder-header
      {:on-click #(rf/dispatch [:toggle-folder-expanded (str "untracked-" (:name node))])
       :style {:color "#6a737d"}}
      [:span.tree-indent {:style {:width (str (* depth 12) "px")}}]
      [:span.tree-chevron (if expanded? "▼" "▶")]
      [:span.tree-folder-icon "📁"]
      [:span.tree-folder-name {:style {:font-style "italic"}} (:name node)]]
     (when expanded?
       [:div.tree-folder-children
        (for [child children]
          ^{:key (or (:path child) (:name child))}
          [untracked-tree-node child (inc depth) expanded-folders])])]))

(defn- untracked-tree-node
  "Render an untracked tree node."
  [node depth expanded-folders]
  (if (= (:type node) :folder)
    [untracked-tree-folder node depth expanded-folders]
    [untracked-tree-file node depth]))

(defn- untracked-file-tree
  "Render the untracked files tree."
  [untracked-files expanded-folders]
  (let [tree (build-tree untracked-files)
        children (sort-tree-children (:children tree))]
    [:div.file-tree
     (for [child children]
       ^{:key (or (:path child) (:name child))}
       [untracked-tree-node child 0 expanded-folders])]))

(defn- excluded-file-item
  "Render an excluded file - clicking restores it to review."
  [file-path]
  [:div.tree-file.excluded
   {:on-click #(rf/dispatch [:restore-file file-path])
    :title (str "Restore " file-path " to review")
    :style {:color "#cf222e" :text-decoration "line-through" :opacity 0.7}}
   [:span.tree-file-icon "📄"]
   [:span.tree-file-name (last (str/split file-path #"/"))]])

(defn file-list []
  (let [files @(rf/subscribe [:files])
        files-set (set files)
        changed-files @(rf/subscribe [:changed-files])
        changed-paths (set (map :path changed-files))
        selected @(rf/subscribe [:selected-file])
        expanded-folders @(rf/subscribe [:expanded-folders])
        untracked-files @(rf/subscribe [:untracked-files-list])
        untracked-set (set untracked-files)
        excluded-files @(rf/subscribe [:excluded-files])
        ;; Only show excluded files that still have changes or are untracked
        relevant-excluded (filter #(or (changed-paths %) (untracked-set %)) excluded-files)
        ;; Filter out files that are already in review or excluded
        available-untracked (remove #(or (files-set %) (excluded-files %)) untracked-files)]
    [:div.file-list
     [:h3 "Files in Review"]
     (if (empty? files)
       [:p {:style {:color "#6a737d" :font-size "13px"}}
        "No files in review"]
       [file-tree files selected changed-files expanded-folders])
     (when (seq relevant-excluded)
       [:<>
        [:h3 {:style {:margin-top "16px"}} "Excluded"]
        [:p {:style {:color "#6a737d" :font-size "11px" :margin "0 0 8px 0"}}
         "Click to restore"]
        [:div.file-tree
         (for [file (sort relevant-excluded)]
           ^{:key file}
           [excluded-file-item file])]])
     (when (seq available-untracked)
       [:<>
        [:h3 {:style {:margin-top "16px"}} "Untracked Files"]
        [:p {:style {:color "#6a737d" :font-size "11px" :margin "0 0 8px 0"}}
         "Click to add to review"]
        [untracked-file-tree available-untracked expanded-folders]])]))

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

(defn- comment-matches-line?
  "Check if a comment matches a specific line.
   New-style comments (with line-content) match by content+context+side.
   Old-style comments (without line-content) match by line number only."
  [comment line-num line-side content prev-content next-content]
  (let [{c-line :line c-side :side c-content :line-content
         c-before :context-before c-after :context-after} comment]
    (if c-content
      ;; New style: match by content + context + side
      (and (= c-content content)
           (= c-side line-side)
           ;; Context matching: only require match if comment has context stored
           (or (nil? c-before) (= c-before prev-content))
           (or (nil? c-after) (= c-after next-content)))
      ;; Old style: match by line number only
      (= c-line line-num))))

(defn diff-line-split
  "Render a diff line in split (side-by-side) mode with separate old/new columns."
  [{:keys [type content old-num new-num file]} file-comments comment-form highlighted-line
   {:keys [prev-content next-content]}]
  (let [line-num (or new-num old-num)
        is-addition (= type :addition)
        is-deletion (= type :deletion)
        is-context (= type :context)
        ;; Determine side for comment anchoring
        side (if is-deletion "old" "new")
        ;; Only show comments on additions and context lines, not deletions
        ;; Match by content+context for new comments, line number for old comments
        comments-for-line (when-not is-deletion
                            (filterv #(comment-matches-line? % line-num side content prev-content next-content)
                                     (or file-comments [])))
        is-highlighted (and (= (:file highlighted-line) file)
                            (= (:line highlighted-line) line-num))
        ;; Check if comment form should appear after this line (not on deletions)
        show-form-here? (and (not is-deletion)
                             (:visible comment-form)
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
           {:on-click #(rf/dispatch [:show-comment-form {:file file
                                                         :line new-num
                                                         :side side
                                                         :line-content content
                                                         :context-before prev-content
                                                         :context-after next-content}])
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
     ;; Inline comments for this line (not on deletions)
     (when (seq comments-for-line)
       [:tr
        [:td {:col-span 4 :style {:padding 0}}
         [comments/comment-threads comments-for-line file line-num]]])
     ;; Inline comment form after this line (not on deletions)
     (when show-form-here?
       [:tr.comment-form-row
        [:td {:col-span 4 :style {:padding 0}}
         [comments/comment-form-inline comment-form]]])]))

(defn diff-line-unified
  "Render a diff line in unified (stacked) mode with +/- prefix."
  [{:keys [type content old-num new-num file]} file-comments comment-form highlighted-line
   {:keys [prev-content next-content]}]
  (let [line-num (or new-num old-num)
        ;; Only show comments on additions and context lines, not deletions
        ;; This prevents duplicate comments when a line is modified (deleted + added with same line num)
        is-deletion (= type :deletion)
        ;; Determine side for comment anchoring
        side (if is-deletion "old" "new")
        ;; Match by content+context for new comments, line number for old comments
        comments-for-line (when-not is-deletion
                            (filterv #(comment-matches-line? % line-num side content prev-content next-content)
                                     (or file-comments [])))
        prefix (case type
                 :addition "+"
                 :deletion "-"
                 " ")
        is-highlighted (and (= (:file highlighted-line) file)
                            (= (:line highlighted-line) line-num))
        ;; Check if comment form should appear after this line (not on deletions)
        show-form-here? (and (not is-deletion)
                             (:visible comment-form)
                             (= (:file comment-form) file)
                             (= (:line comment-form) line-num)
                             (not (:parent-id comment-form)))]
    [:<>
     [:tr.diff-line
      {:id (line-id file line-num)
       :class (str (name type) (when is-highlighted " highlighted"))}
      [:td.diff-line-num.unified.line-num-with-button
       (when-not is-deletion
         [:button.add-comment-btn
          {:on-click #(rf/dispatch [:show-comment-form {:file file
                                                        :line line-num
                                                        :side side
                                                        :line-content content
                                                        :context-before prev-content
                                                        :context-after next-content}])
           :title "Add comment"}
          "+"])
       [:a.line-num-link
        {:href (str "#" (js/encodeURIComponent file) ":" line-num)
         :on-click (fn [e]
                     (.preventDefault e)
                     (update-line-hash! file line-num))}
        line-num]]
      [:td.diff-line-content.unified
       [:pre {:style {:margin 0 :font-family "inherit"}}
        (str prefix " " content)]]]
     ;; Inline comments for this line (not on deletions)
     (when (seq comments-for-line)
       [:tr
        [:td {:col-span 2 :style {:padding 0}}
         [comments/comment-threads comments-for-line file line-num]]])
     ;; Inline comment form after this line (not on deletions)
     (when show-form-here?
       [:tr.comment-form-row
        [:td {:col-span 2 :style {:padding 0}}
         [comments/comment-form-inline comment-form]]])]))

(defn diff-line
  "Render a diff line in the current view mode."
  [line-data file-comments view-mode comment-form highlighted-line line-context]
  (if (= view-mode :unified)
    [diff-line-unified line-data file-comments comment-form highlighted-line line-context]
    [diff-line-split line-data file-comments comment-form highlighted-line line-context]))

(defn- expanded-context-lines
  "Render already-expanded context lines."
  [context-lines file-comments view-mode comment-form highlighted-line]
  (into [:<>]
        (map-indexed
         (fn [idx {:keys [old-num new-num file] :as line-data}]
           (let [prev-line (when (pos? idx) (nth context-lines (dec idx)))
                 next-line (when (< idx (dec (count context-lines))) (nth context-lines (inc idx)))
                 line-context {:prev-content (:content prev-line)
                               :next-content (:content next-line)}]
             ^{:key (str file "-ctx-" (or new-num old-num))}
             [diff-line line-data file-comments view-mode comment-form highlighted-line line-context]))
         context-lines)))

(defn- find-expanded-in-range
  "Find all expanded ranges that fall within [from-line, to-line].
   Returns sorted list of [from to lines] tuples."
  [from-line to-line expanded-context]
  (->> expanded-context
       (filter (fn [[[f t] _]]
                 (and (>= f from-line) (<= t to-line))))
       (sort-by (comp first first))
       (map (fn [[[f t] lines]] [f t lines]))))

(defn- expand-button
  "Render an expand button for a gap.
   When direction is :both, shows two buttons for up/down expansion."
  [file from-line to-line direction view-mode]
  (let [lines-hidden (inc (- to-line from-line))
        col-span (if (= view-mode :unified) 2 4)]
    (when (pos? lines-hidden)
      [:tr.diff-expand-row
       [:td {:col-span col-span}
        (if (= direction :both)
          ;; Show two buttons for bidirectional expansion
          [:div {:style {:display "flex" :justify-content "center" :gap "16px"}}
           [:button.diff-expand-btn
            {:on-click #(rf/dispatch [:expand-context {:file file
                                                       :from from-line
                                                       :to to-line
                                                       :direction :up}])
             :title "Expand upward"}
            [:span.diff-expand-icon "↑"]
            [:span.diff-expand-text " Expand up"]]
           [:span {:style {:color "#6a737d"}} (str lines-hidden " hidden")]
           [:button.diff-expand-btn
            {:on-click #(rf/dispatch [:expand-context {:file file
                                                       :from from-line
                                                       :to to-line
                                                       :direction :down}])
             :title "Expand downward"}
            [:span.diff-expand-icon "↓"]
            [:span.diff-expand-text " Expand down"]]]
          ;; Single button for unidirectional expansion
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
              "↕")]
           [:span.diff-expand-text
            (str " Expand " lines-hidden " hidden lines")]])]])))

(defn- expand-context-row
  "Renders expanded context lines and/or expand buttons for remaining gaps."
  [file from-line to-line direction view-mode expanded-context file-comments comment-form highlighted-line]
  (let [total-hidden (inc (- to-line from-line))
        expanded-ranges (find-expanded-in-range from-line to-line expanded-context)]
    (when (pos? total-hidden)
      (if (empty? expanded-ranges)
        ;; Nothing expanded yet - show single expand button
        [expand-button file from-line to-line direction view-mode]
        ;; Some ranges expanded - show gaps and expanded content
        ;; Preserve original direction for sub-gaps (if :both, all sub-gaps are :both)
        (let [sub-gap-dir (if (= direction :both) :both nil)
              items (loop [pos from-line
                           ranges expanded-ranges
                           result []]
                      (if (empty? ranges)
                        ;; Add trailing gap if any
                        (if (< pos to-line)
                          (conj result {:type :gap :from pos :to to-line :dir (or sub-gap-dir :down)})
                          result)
                        (let [[f t lines] (first ranges)
                              ;; Gap before this expanded range?
                              with-gap (if (< pos f)
                                         (conj result {:type :gap :from pos :to (dec f) :dir (or sub-gap-dir :up)})
                                         result)]
                          (recur (inc t)
                                 (rest ranges)
                                 (conj with-gap {:type :expanded :from f :to t :lines lines})))))]
          (into [:<>]
                (map-indexed
                 (fn [idx {:keys [type from to lines dir]}]
                   (with-meta
                     (if (= type :gap)
                       [expand-button file from to dir view-mode]
                       [expanded-context-lines lines file-comments view-mode comment-form highlighted-line])
                     {:key (str file "-" type "-" from "-" to "-" idx)}))
                 items)))))))

(defn diff-hunk [{:keys [header lines file old-start new-start]} start-old start-new file-comments show-expand-above? view-mode comment-form highlighted-line expanded-context]
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
       [expand-context-row file 1 (dec start-new) :up view-mode expanded-context file-comments comment-form highlighted-line])
     [:tr.diff-hunk-header
      [:td {:col-span col-span} header]]
     (for [[idx line] (map-indexed vector line-data)]
       (let [prev-line (when (pos? idx) (nth line-data (dec idx)))
             next-line (when (< idx (dec (count line-data))) (nth line-data (inc idx)))
             line-context {:prev-content (:content prev-line)
                           :next-content (:content next-line)}]
         ^{:key idx}
         [diff-line line file-comments view-mode comment-form highlighted-line line-context]))]))

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
        expanded-context @(rf/subscribe [:expanded-context-for-file file-path])
        staged-files @(rf/subscribe [:staged-files])
        unstaged-files @(rf/subscribe [:unstaged-files])
        file-info (first (filter #(= (:path %) file-path) files-with-size))
        file-size (or (:size file-info) 0)
        is-large-file (> file-size large-file-threshold)
        is-loaded (get loaded-files file-path false)
        is-loading (get loading-files file-path false)
        ;; File is fully staged if it's staged AND has no unstaged modifications
        is-fully-staged (and (contains? staged-files file-path)
                             (not (contains? unstaged-files file-path)))
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
         (format-file-size file-size)])
      [:button.btn-stage
       {:on-click (fn [e]
                    (.stopPropagation e)
                    (when-not is-fully-staged
                      (rf/dispatch [:stage-file file-path])))
        :disabled is-fully-staged
        :title (if is-fully-staged "Already staged" "Stage file for commit")
        :style {:margin-left "auto"
                :padding "2px 8px"
                :font-size "11px"
                :background (if is-fully-staged "#e1e4e8" "#2ea44f")
                :color (if is-fully-staged "#6a737d" "white")
                :border "none"
                :border-radius "4px"
                :cursor (if is-fully-staged "default" "pointer")
                :opacity (if is-fully-staged 0.6 1)}}
       (if is-fully-staged "Staged" "Stage")]]
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
                    view-mode
                    expanded-context
                    file-comments
                    comment-form
                    highlighted-line])
                 [diff-hunk (assoc hunk :file file-path)
                  (:new-start hunk)
                  (:old-start hunk)
                  file-comments
                  false
                  view-mode
                  comment-form
                  highlighted-line
                  expanded-context]]))]]

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

(defn session-settings-modal []
  (let [form-state (r/atom {:initialized false
                            :show-branches false})]
    (fn []
      (let [session @(rf/subscribe [:current-session])
            visible? @(rf/subscribe [:session-settings-visible?])
            branches @(rf/subscribe [:branches])
            {:keys [project repo-path target-branch initialized show-branches]} @form-state
            ;; Filter branches based on current input
            filtered-branches (when (and branches show-branches)
                                (if (str/blank? target-branch)
                                  branches
                                  (filterv #(str/includes? (str/lower-case %)
                                                           (str/lower-case target-branch))
                                           branches)))]
        ;; Initialize form when modal becomes visible
        (when (and visible? (not initialized) session)
          (reset! form-state {:project (or (:project session) "")
                              :repo-path (or (:repo-path session) "")
                              :target-branch (or (:target-branch session) "")
                              :initialized true
                              :show-branches false}))
        ;; Reset when closing
        (when (and (not visible?) initialized)
          (reset! form-state {:initialized false :show-branches false}))
        (when visible?
          [:div {:style {:position "fixed"
                         :top 0 :left 0 :right 0 :bottom 0
                         :background "rgba(0,0,0,0.5)"
                         :display "flex"
                         :align-items "center"
                         :justify-content "center"
                         :z-index 1000}
                 :on-click #(rf/dispatch [:hide-session-settings])}
           [:div {:style {:background "white"
                          :border-radius "8px"
                          :padding "24px"
                          :min-width "400px"
                          :max-width "500px"}
                  :on-click #(.stopPropagation %)}
            [:h3 {:style {:margin "0 0 16px 0"}} "Session Settings"]
            ;; Project name
            [:div {:style {:margin-bottom "16px"}}
             [:label {:style {:display "block" :margin-bottom "4px" :font-weight 500}}
              "Project Name"]
             [:input {:type "text"
                      :value (or project "")
                      :on-change #(swap! form-state assoc :project (.. % -target -value))
                      :placeholder "e.g., my-project"
                      :style {:width "100%"
                              :padding "8px 12px"
                              :border "1px solid #d0d7de"
                              :border-radius "6px"
                              :font-size "14px"}}]]
            ;; Repo path
            [:div {:style {:margin-bottom "16px"}}
             [:label {:style {:display "block" :margin-bottom "4px" :font-weight 500}}
              "Repository Path"]
             [:input {:type "text"
                      :value (or repo-path "")
                      :on-change #(swap! form-state assoc :repo-path (.. % -target -value))
                      :placeholder "e.g., /path/to/repo"
                      :style {:width "100%"
                              :padding "8px 12px"
                              :border "1px solid #d0d7de"
                              :border-radius "6px"
                              :font-size "14px"}}]
             [:p {:style {:margin "4px 0 0 0" :font-size "12px" :color "#6a737d"}}
              "Absolute path to the project directory"]]
            ;; Target branch with autocomplete
            [:div {:style {:margin-bottom "16px" :position "relative"}}
             [:label {:style {:display "block" :margin-bottom "4px" :font-weight 500}}
              "Target Branch"]
             [:input {:type "text"
                      :value (or target-branch "")
                      :on-change #(swap! form-state assoc :target-branch (.. % -target -value))
                      :on-focus #(swap! form-state assoc :show-branches true)
                      :on-blur #(js/setTimeout
                                 (fn [] (swap! form-state assoc :show-branches false))
                                 150)
                      :placeholder "e.g., main, master, develop"
                      :style {:width "100%"
                              :padding "8px 12px"
                              :border "1px solid #d0d7de"
                              :border-radius "6px"
                              :font-size "14px"}}]
             ;; Branch suggestions dropdown
             (when (and show-branches (seq filtered-branches))
               [:div {:style {:position "absolute"
                              :top "100%"
                              :left 0
                              :right 0
                              :max-height "200px"
                              :overflow-y "auto"
                              :background "white"
                              :border "1px solid #d0d7de"
                              :border-radius "6px"
                              :box-shadow "0 4px 12px rgba(0,0,0,0.15)"
                              :z-index 10
                              :margin-top "4px"}}
                (for [branch filtered-branches]
                  ^{:key branch}
                  [:div {:on-mouse-down #(do
                                           (.preventDefault %)
                                           (swap! form-state assoc
                                                  :target-branch branch
                                                  :show-branches false))
                         :style {:padding "8px 12px"
                                 :cursor "pointer"
                                 :font-size "14px"
                                 :border-bottom "1px solid #f0f0f0"}
                         :on-mouse-over #(set! (.. % -target -style -background) "#f6f8fa")
                         :on-mouse-out #(set! (.. % -target -style -background) "white")}
                   branch])])
             [:p {:style {:margin "4px 0 0 0" :font-size "12px" :color "#6a737d"}}
              "The branch to compare against"]]
            [:div {:style {:display "flex" :gap "8px" :justify-content "flex-end"}}
             [:button.btn.btn-secondary
              {:on-click #(rf/dispatch [:hide-session-settings])}
              "Cancel"]
             [:button.btn.btn-primary
              {:on-click #(rf/dispatch [:update-session {:target_branch target-branch
                                                         :project project
                                                         :repo_path repo-path}])}
              "Save"]]]])))))

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
       [:div {:style {:display "flex" :align-items "center" :gap "8px"}}
        [:div
         [:strong (:project session)]
         [:span {:style {:margin "0 8px" :color "#6a737d"}} "/"]
         [:span (:branch session)]
         [:span {:style {:margin "0 8px" :color "#6a737d"}} "→"]
         [:span {:style {:color "#6a737d"}} (:target-branch session)]]
        [:button {:on-click #(rf/dispatch [:show-session-settings])
                  :title "Session settings"
                  :style {:background "none"
                          :border "none"
                          :cursor "pointer"
                          :padding "4px"
                          :color "#6a737d"
                          :font-size "14px"}}
         "⚙️"]]
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
   [session-settings-modal]
   [session-header]
   [:div.diff-container
    [file-list]
    [:div.diff-content
     [all-files-diff]]]])
