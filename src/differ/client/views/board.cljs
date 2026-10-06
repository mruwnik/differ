(ns differ.client.views.board
  "Kanban board views for task coordination."
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [clojure.string :as str]
            [differ.client.task-filter :as task-filter]
            [differ.client.board-stats :as board-stats]))

(def status-order ["pending" "needs_owner" "planning" "plan_review" "ready" "in_progress" "blocked" "testing" "in_review" "done" "rejected"])

(def status-labels
  {"pending" "Pending"
   "needs_owner" "Needs Owner"
   "planning" "Planning"
   "plan_review" "Plan Review"
   "ready" "Ready"
   "in_progress" "In Progress"
   "blocked" "Blocked"
   "testing" "Testing"
   "in_review" "In Review"
   "done" "Done"
   "rejected" "Rejected"})

(def status-colors
  {"pending" "#6a737d"
   "needs_owner" "#bf3989"
   "planning" "#6f42c1"
   "plan_review" "#b08800"
   "ready" "#00838f"
   "in_progress" "#0366d6"
   "blocked" "#d73a49"
   "testing" "#1b7c83"
   "in_review" "#e36209"
   "done" "#28a745"
   "rejected" "#959da5"})

(defn format-age [iso-string]
  (when iso-string
    (let [now (.now js/Date)
          then (.getTime (js/Date. iso-string))
          diff-ms (- now then)
          diff-mins (js/Math.floor (/ diff-ms 60000))
          diff-hours (js/Math.floor (/ diff-mins 60))
          diff-days (js/Math.floor (/ diff-hours 24))]
      (cond
        (< diff-mins 1) "now"
        (< diff-mins 60) (str diff-mins "m")
        (< diff-hours 24) (str diff-hours "h")
        :else (str diff-days "d")))))

(defn extract-repo-name [repo-path]
  (last (str/split (or repo-path "") #"/")))

;; ============================================================================
;; Priority & Tags
;; ============================================================================

(defn priority-label
  "Higher priority = more urgent: ▲ for positive, ▼ for negative."
  [priority]
  (if (neg? priority) (str "\u25bc" (- priority)) (str "\u25b2" priority)))

(defn priority-badge [priority]
  (let [priority (or priority 0)]
    (when-not (zero? priority)
      [:span {:title (str "Priority " priority)
              :style {:font-size "10px" :font-weight "600" :padding "0 5px"
                      :border-radius "8px" :white-space "nowrap"
                      :background (if (pos? priority) "#ffebe9" "#f6f8fa")
                      :color (if (pos? priority) "#cf222e" "#6a737d")}}
       (priority-label priority)])))

(defn tag-chip
  "A tag chip; clicking it toggles that tag in the board filter."
  [tag selected?]
  [:span {:title (if selected? "Remove tag filter" "Filter by this tag")
          :style {:font-size "10px" :padding "0 6px" :border-radius "8px"
                  :cursor "pointer" :white-space "nowrap"
                  :background (if selected? "#0366d6" "#f1f8ff")
                  :color (if selected? "#fff" "#0366d6")
                  :border "1px solid #c8e1ff"}
          :on-click (fn [e]
                      (.stopPropagation e)
                      (rf/dispatch [:toggle-board-tag-filter tag]))}
   tag])

;; ============================================================================
;; Stale claims
;; ============================================================================

(defn stale-claim-badge
  "Warning badge for a claim whose lease lapsed (worker idle too long)."
  [task]
  (when (:claim-stale task)
    [:span {:title (str "Stale claim: no activity for " (format-age (:updated-at task)))
            :style {:font-size "10px" :font-weight "600" :padding "0 5px"
                    :border-radius "8px" :white-space "nowrap"
                    :background "#fff8c5" :color "#9a6700"
                    :border "1px solid #d4a72c"}}
     "⚠ stale"]))

;; ============================================================================
;; Checklist
;; ============================================================================

(defn checklist-chip
  "Compact progress chip, e.g. ☑ 2/4; green once every item is ticked."
  [checklist]
  (when (seq checklist)
    (let [done (count (filter :done checklist))
          complete? (= done (count checklist))]
      [:span {:title (str/join "\n" (for [{:keys [item done]} checklist]
                                      (str (if done "☑ " "☐ ") item)))
              :style {:font-size "10px" :font-weight "600" :padding "0 5px"
                      :border-radius "8px" :white-space "nowrap"
                      :background (if complete? "#dafbe1" "#f6f8fa")
                      :color (if complete? "#1a7f37" "#6a737d")}}
       (str "☑ " done "/" (count checklist))])))

(defn checklist-editor
  "Detail-panel checklist: tick/untick, remove, and add items."
  [task]
  (let [draft (r/atom "")]
    (fn [task]
      (let [checklist (:checklist task)
            add! (fn []
                   (when-not (str/blank? @draft)
                     ;; No :done, so re-adding an existing item keeps its flag
                     (rf/dispatch [:update-task-from-ui (:id task)
                                   {:checklist [{:item (str/trim @draft)}]}])
                     (reset! draft "")))]
        [:div {:style {:margin-bottom "12px"}}
         [:div {:style {:display "flex" :gap "6px" :align-items "center" :margin-bottom "4px"}}
          [:h4 {:style {:margin "0" :font-size "13px" :color "#24292e"}} "Checklist"]
          [checklist-chip checklist]
          (when (and (= "done" (:status task)) (not-every? :done checklist))
            [:span {:style {:font-size "11px" :color "#cf222e"}} "done with unticked items"])]
         (for [{:keys [item done]} checklist]
           ^{:key item}
           [:div {:style {:display "flex" :align-items "center" :gap "6px" :font-size "12px"}}
            [:label {:style {:flex "1" :display "flex" :align-items "center" :gap "6px"
                             :cursor "pointer" :color "#24292e"}}
             [:input {:type "checkbox"
                      :checked done
                      :on-change #(rf/dispatch [:update-task-from-ui (:id task)
                                                {:checklist [{:item item :done (not done)}]}])}]
             item]
            [:button {:title "Remove item"
                      :on-click #(rf/dispatch [:update-task-from-ui (:id task)
                                               {:remove-checklist-items [item]}])
                      :style {:background "none" :border "none" :color "#959da5"
                              :cursor "pointer" :font-size "14px" :line-height "1"}}
             "×"]])
         [:div {:style {:display "flex" :gap "6px" :margin-top "4px"}}
          [:input {:value @draft
                   :placeholder "Add item (e.g. unit, live)"
                   :on-change #(reset! draft (.. % -target -value))
                   :on-key-down #(when (= "Enter" (.-key %)) (add!))
                   :style {:flex "1" :padding "2px 6px" :font-size "12px"
                           :border "1px solid #e1e4e8" :border-radius "4px"}}]
          [:button {:disabled (str/blank? @draft)
                    :on-click add!
                    :style {:font-size "12px" :padding "2px 8px" :cursor "pointer"
                            :opacity (if (str/blank? @draft) "0.5" "1")
                            :background "#fff" :border "1px solid #e1e4e8"
                            :border-radius "4px"}}
           "Add"]]]))))

;; ============================================================================
;; Commits & parent/child links
;; ============================================================================

(defn short-sha [sha]
  (subs sha 0 (min 8 (count sha))))

(defn task-by-id
  "The loaded board task with this id, or nil (e.g. done tasks while
   'Show completed' is off)."
  [tasks id]
  (some #(when (= id (:id %)) %) tasks))

(defn children-label [{:keys [total done]}]
  (str done "/" total " done"))

(defn task-link
  "Clickable task title that selects that task in the detail panel."
  [task]
  [:span {:title "Open task"
          :style {:color "#0366d6" :cursor "pointer"}
          :on-click (fn [e]
                      (.stopPropagation e)
                      (rf/dispatch [:select-task task]))}
   (:title task)])

(defn detail-commits [commits]
  (when (seq commits)
    [:div {:style {:margin-bottom "12px"}}
     [:div {:style {:font-size "12px" :font-weight "600" :color "#24292e" :margin-bottom "4px"}}
      "Commits"]
     [:div {:style {:display "flex" :gap "6px" :flex-wrap "wrap"}}
      (for [sha commits]
        ^{:key sha}
        [:code {:title sha
                :style {:font-size "11px" :padding "1px 6px" :border-radius "4px"
                        :background "#f6f8fa" :border "1px solid #e1e4e8" :color "#24292e"}}
         (short-sha sha)])]]))

(defn detail-family
  "Parent link and child list for the detail panel."
  [task]
  (let [tasks @(rf/subscribe [:board-tasks])
        parent-id (:parent-id task)
        parent (when parent-id (task-by-id tasks parent-id))
        children (filter #(= (:id task) (:parent-id %)) tasks)]
    [:<>
     (when parent-id
       [:div {:style {:font-size "12px" :color "#6a737d" :margin-bottom "12px"}}
        "Parent: "
        (if parent
          [task-link parent]
          [:span {:style {:font-family "monospace"}} parent-id])])
     (when (or (seq children) (:children task))
       [:div {:style {:margin-bottom "12px"}}
        [:div {:style {:font-size "12px" :font-weight "600" :color "#24292e" :margin-bottom "4px"}}
         "Children"
         (when (:children task)
           [:span {:style {:font-weight "400" :color "#6a737d" :margin-left "6px"}}
            (children-label (:children task))])]
        (for [child children]
          ^{:key (:id child)}
          [:div {:style {:display "flex" :gap "6px" :align-items "center"
                         :font-size "12px" :margin-bottom "2px"}}
           [:span {:style {:font-size "10px" :padding "0 6px" :border-radius "8px"
                           :white-space "nowrap" :color "white"
                           :background (get status-colors (:status child) "#e1e4e8")}}
            (get status-labels (:status child) (:status child))]
           [task-link child]])])]))

;; ============================================================================
;; Task Card
;; ============================================================================

(defn task-card [task]
  (let [selected @(rf/subscribe [:selected-task])
        selected-tags (:tags @(rf/subscribe [:board-filter]))
        is-selected (and selected (= (:id selected) (:id task)))]
    [:div {:style {:padding "8px 12px"
                   :margin-bottom "6px"
                   :background (if is-selected "#ddf4ff" "#fff")
                   :border (str "1px solid " (if is-selected "#0366d6" "#e1e4e8"))
                   :border-radius "4px"
                   :cursor "pointer"}
           :on-click #(rf/dispatch [:select-task task])}
     [:div {:style {:display "flex" :gap "6px" :align-items "flex-start" :margin-bottom "4px"}}
      [priority-badge (:priority task)]
      [:div {:style {:font-size "13px" :font-weight "500" :color "#24292e"}}
       (:title task)]]
     (when (seq (:tags task))
       [:div {:style {:display "flex" :gap "4px" :flex-wrap "wrap" :margin-bottom "4px"}}
        (for [tag (:tags task)]
          ^{:key tag}
          [tag-chip tag (contains? selected-tags tag)])])
     (when-let [parent (some->> (:parent-id task) (task-by-id @(rf/subscribe [:board-tasks])))]
       [:div {:title (str "Part of: " (:title parent))
              :style {:font-size "11px" :color "#6a737d" :margin-bottom "4px"
                      :overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"}}
        (str "↳ " (:title parent))])
     [:div {:style {:display "flex" :justify-content "space-between" :align-items "center"}}
      [:div {:style {:display "flex" :gap "6px" :align-items "center"}}
       (when (:worker-name task)
         [:span {:style {:font-size "11px" :color "#6a737d"}}
          (:worker-name task)])
       [stale-claim-badge task]
       (when (and (:assignee task) (not= (:assignee task) (:worker-name task)))
         [:span {:title "Assigned to"
                 :style {:font-size "11px" :color "#8250df"}}
          (str "\u2192 " (:assignee task))])
       (when (seq (:blocked-by task))
         [:span {:style {:font-size "10px" :color "#d73a49" :font-weight "500"}}
          (str "blocked by " (count (:blocked-by task)))])
       [checklist-chip (:checklist task)]
       (when (:children task)
         [:span {:title "Child tasks done"
                 :style {:font-size "10px" :color "#6a737d"}}
          (children-label (:children task))])
       (when (seq (:commits task))
         [:span {:title (str/join "\n" (:commits task))
                 :style {:font-size "10px" :color "#6a737d" :font-family "monospace"}}
          (str (count (:commits task)) (if (= 1 (count (:commits task))) " commit" " commits"))])]
      [:span {:style {:font-size "11px" :color "#959da5"}}
       (format-age (:created-at task))]]]))

;; ============================================================================
;; Kanban Column
;; ============================================================================

(defn kanban-column [status tasks]
  [:div {:style {:min-width "200px"
                 :max-width "280px"
                 :flex "1 1 200px"}}
   [:div {:style {:display "flex" :align-items "center" :gap "8px"
                  :margin-bottom "12px" :padding-bottom "8px"
                  :border-bottom (str "2px solid " (get status-colors status "#e1e4e8"))}}
    [:span {:style {:font-size "13px" :font-weight "600" :color "#24292e"}}
     (get status-labels status status)]
    [:span {:style {:font-size "12px" :color "#6a737d"
                    :background "#f1f8ff" :padding "0 6px"
                    :border-radius "10px" :min-width "18px" :text-align "center"}}
     (count tasks)]]
   [:div {:style {:min-height "80px"}}
    (for [task tasks]
      ^{:key (:id task)}
      [task-card task])]])

;; ============================================================================
;; Task Detail Panel
;; ============================================================================

(defn assignee-editor
  "Inline editor for the optional assignee. Suggests names of agents already
   seen on this board."
  [task]
  (let [draft (r/atom (or (:assignee task) ""))]
    (fn [task]
      (let [known (->> @(rf/subscribe [:board-tasks])
                       (mapcat (juxt :worker-name :assignee))
                       (remove str/blank?)
                       distinct
                       sort)
            changed? (not= (str/trim @draft) (or (:assignee task) ""))]
        [:div {:style {:display "flex" :gap "6px" :align-items "center" :margin-bottom "12px"}}
         [:label {:style {:font-size "12px" :color "#6a737d"}} "Assignee"]
         [:input {:value @draft
                  :list "board-agent-names"
                  :placeholder "Anyone"
                  :on-change #(reset! draft (.. % -target -value))
                  :style {:flex "1" :padding "2px 6px" :font-size "12px"
                          :border "1px solid #e1e4e8" :border-radius "4px"}}]
         [:datalist {:id "board-agent-names"}
          (for [n known] ^{:key n} [:option {:value n}])]
         [:button {:disabled (not changed?)
                   :on-click #(rf/dispatch [:update-task-from-ui (:id task)
                                            {:assignee (str/trim @draft)}])
                   :style {:font-size "12px" :padding "2px 8px" :cursor "pointer"
                           :opacity (if changed? "1" "0.5")
                           :background "#fff" :border "1px solid #e1e4e8"
                           :border-radius "4px"}}
          "Save"]]))))

(defn task-detail []
  (let [note-text (r/atom "")]
    (fn []
      (let [task @(rf/subscribe [:selected-task])]
        (when task
          [:div {:style {:position "fixed" :right 0 :top 0 :bottom 0
                         :width "400px" :background "#fff"
                         :border-left "1px solid #e1e4e8"
                         :overflow-y "auto" :padding "20px" :z-index 100
                         :box-shadow "-4px 0 12px rgba(0,0,0,0.1)"}}
           ;; Close button
           [:button {:style {:position "absolute" :right "12px" :top "12px"
                             :background "none" :border "none" :color "#6a737d"
                             :cursor "pointer" :font-size "18px" :line-height "1"}
                     :on-click #(rf/dispatch [:deselect-task])}
            "\u00d7"]

           ;; Title
           [:h3 {:style {:margin "0 0 12px 0" :padding-right "32px" :color "#24292e"
                         :font-size "16px"}}
            (:title task)]

           ;; Status + worker
           [:div {:style {:display "flex" :align-items "center" :gap "8px"
                          :margin-bottom "12px" :flex-wrap "wrap"}}
            [:span {:style {:font-size "12px" :padding "2px 8px" :border-radius "10px"
                            :background (get status-colors (:status task) "#e1e4e8")
                            :color "white" :font-weight "500"}}
             (get status-labels (:status task) (:status task))]
            (when (:worker-name task)
              [:span {:style {:font-size "12px" :color "#6a737d"}}
               (str "Worker: " (:worker-name task))])
            [stale-claim-badge task]
            (when (:worker-id task)
              [:span {:style {:font-size "11px" :color "#959da5"}}
               (str "(" (:worker-id task) ")")])
            [priority-badge (:priority task)]]

           [assignee-editor task]

           ;; Tags
           (when (seq (:tags task))
             [:div {:style {:display "flex" :gap "4px" :flex-wrap "wrap" :margin-bottom "12px"}}
              (for [tag (:tags task)]
                ^{:key tag}
                [tag-chip tag (contains? (:tags @(rf/subscribe [:board-filter])) tag)])])

           ;; Blocked by
           (when (seq (:blocked-by task))
             [:div {:style {:margin-bottom "12px" :padding "8px 12px"
                            :background "#fffbdd" :border "1px solid #e36209"
                            :border-radius "4px"}}
              [:div {:style {:font-size "12px" :font-weight "600" :color "#e36209"
                             :margin-bottom "4px"}}
               "Blocked by:"]
              (for [dep-id (:blocked-by task)]
                ^{:key dep-id}
                [:div {:style {:font-size "11px" :color "#6a737d" :font-family "monospace"}}
                 dep-id])])

           [checklist-editor task]
           [detail-family task]
           [detail-commits (:commits task)]

           ;; Description
           (when (seq (:description task))
             [:div {:style {:margin-bottom "16px" :color "#24292e" :font-size "13px"
                            :padding "12px" :background "#f6f8fa" :border-radius "4px"
                            :border "1px solid #e1e4e8" :white-space "pre-wrap"}}
              (:description task)])

           ;; Reject button for pending tasks
           (when (= "pending" (:status task))
             [:button {:style {:margin-bottom "16px" :padding "4px 12px"
                               :background "#d73a49" :color "white" :border "none"
                               :border-radius "4px" :cursor "pointer" :font-size "12px"}
                       :on-click #(rf/dispatch [:update-task-from-ui (:id task)
                                                {:status "rejected"}])}
              "Reject Task"])

           ;; Notes section
           [:div {:style {:margin-top "8px"}}
            [:h4 {:style {:margin "0 0 8px 0" :font-size "13px" :color "#24292e"}}
             "Notes"]
            (if (seq (:notes task))
              [:div
               (for [note (:notes task)]
                 ^{:key (:id note)}
                 [:div {:style {:padding "8px" :margin-bottom "6px"
                                :background "#f6f8fa" :border-radius "4px"
                                :border "1px solid #e1e4e8" :font-size "12px"}}
                  [:div {:style {:display "flex" :justify-content "space-between"
                                 :margin-bottom "4px"}}
                   [:span {:style {:color "#0366d6" :font-weight "500"}}
                    (or (:author note) "anonymous")]
                   [:span {:style {:color "#959da5"}}
                    (format-age (:created-at note))]]
                  [:div {:style {:color "#24292e" :white-space "pre-wrap"}}
                   (:content note)]])]
              [:p {:style {:color "#6a737d" :font-size "12px" :margin "0 0 8px 0"}}
               "No notes yet."])

            ;; Add note form
            [:div {:style {:margin-top "8px"}}
             [:textarea {:value @note-text
                         :on-change #(reset! note-text (.. % -target -value))
                         :placeholder "Add a note..."
                         :style {:width "100%" :min-height "60px" :resize "vertical"
                                 :background "#fff" :border "1px solid #e1e4e8"
                                 :border-radius "4px" :padding "8px" :color "#24292e"
                                 :font-size "12px" :box-sizing "border-box"
                                 :font-family "inherit"}}]
             [:button {:style {:margin-top "6px" :padding "4px 12px"
                               :background "#28a745" :color "white" :border "none"
                               :border-radius "4px" :cursor "pointer" :font-size "12px"
                               :opacity (if (str/blank? @note-text) "0.5" "1")}
                       :disabled (str/blank? @note-text)
                       :on-click (fn []
                                   (rf/dispatch [:add-task-note-from-ui (:id task) @note-text])
                                   (reset! note-text ""))}
              "Add Note"]]]])))))

;; ============================================================================
;; Boards List
;; ============================================================================

(defn boards-list []
  (let [boards @(rf/subscribe [:boards])]
    [:div
     [:h2 {:style {:margin-bottom "16px" :color "#24292e"}} "Task Boards"]
     (if (empty? boards)
       [:div.empty-state
        [:h3 "No boards yet"]
        [:p "Boards are created automatically when agents create tasks via MCP."]]
       [:div
        (for [board boards]
          ^{:key (:id board)}
          [:div {:style {:padding "12px 16px" :margin-bottom "8px"
                         :background "#f6f8fa" :border "1px solid #e1e4e8"
                         :border-radius "6px" :cursor "pointer"}
                 :on-click #(rf/dispatch [:navigate-board (:repo-path board)])}
           [:div {:style {:display "flex" :justify-content "space-between" :align-items "center"}}
            [:div
             [:span {:style {:font-weight "600" :font-size "14px" :color "#24292e"}}
              (extract-repo-name (:repo-path board))]
             [:span {:style {:color "#6a737d" :font-size "12px" :margin-left "8px"}}
              (:repo-path board)]]
            [:div {:style {:display "flex" :gap "6px" :flex-wrap "wrap"}}
             (for [[status cnt] (sort-by (fn [[s _]] (.indexOf status-order s))
                                         (:task-counts board))
                   :when (pos? cnt)]
               ^{:key status}
               [:span {:style {:font-size "11px" :padding "1px 6px"
                               :border-radius "10px"
                               :background (get status-colors status "#e1e4e8")
                               :color "white"}}
                (str cnt " " (get status-labels status status))])]]])])]))

;; ============================================================================
;; Filter Bar
;; ============================================================================

(defn filter-bar
  "Search box, min-priority select and tag chips. Options come from the
   unfiltered task list so selecting a filter never hides its own option."
  [all-tasks shown-count]
  (let [{:keys [search tags min-priority] :as filter-state} @(rf/subscribe [:board-filter])
        board-tags (task-filter/board-tags all-tasks)
        priorities (task-filter/board-priorities all-tasks)]
    [:div {:style {:display "flex" :gap "8px" :align-items "center" :flex-wrap "wrap"
                   :margin-bottom "16px"}}
     [:input {:type "search"
              :value search
              :placeholder "Search titles (fuzzy)\u2026"
              :on-change #(rf/dispatch [:set-board-search (.. % -target -value)])
              :style {:flex "0 1 260px" :padding "4px 8px" :font-size "12px"
                      :border "1px solid #e1e4e8" :border-radius "4px"}}]
     [:select {:value (if (some? min-priority) (str min-priority) "")
               :on-change (fn [e]
                            (let [v (.. e -target -value)]
                              (rf/dispatch [:set-board-min-priority
                                            (when-not (str/blank? v) (js/parseInt v 10))])))
               :style {:padding "4px" :font-size "12px"
                       :border "1px solid #e1e4e8" :border-radius "4px"}}
      [:option {:value ""} "Any priority"]
      (for [p priorities]
        ^{:key p}
        [:option {:value (str p)} (str "Priority \u2265 " p)])]
     (for [tag board-tags]
       ^{:key tag}
       [tag-chip tag (contains? tags tag)])
     (when (task-filter/active? filter-state)
       [:<>
        [:span {:style {:font-size "12px" :color "#6a737d"}}
         (str shown-count " of " (count all-tasks) " tasks")]
        [:button {:on-click #(rf/dispatch [:clear-board-filters])
                  :style {:font-size "12px" :padding "2px 8px" :cursor "pointer"
                          :background "none" :border "1px solid #e1e4e8"
                          :border-radius "4px" :color "#6a737d"}}
         "Clear filters"]])]))

;; ============================================================================
;; Throughput Stats
;; ============================================================================

(def stats-chart-dims
  {:slot-width 20 :bar-width 7 :gap 2 :plot-height 56})

(defn clock-label [iso]
  (.toLocaleTimeString (js/Date. iso) #js [] #js {:hour "2-digit" :minute "2-digit"}))

(defn bucket-label
  "\"14:00–15:00\" for a bucket starting at `iso`."
  [iso bucket-minutes]
  (str (clock-label iso) "–"
       (clock-label (+ (.getTime (js/Date. iso)) (* bucket-minutes 60000)))))

(defn bucket-summary [bucket bucket-minutes]
  (str (bucket-label (:start bucket) bucket-minutes) ": "
       (:done bucket 0) " done, " (:reverted bucket 0) " reverted"))

(defn stat-tile [label value hint]
  [:div {:style {:min-width "92px" :padding "6px 10px" :background "#fff"
                 :border "1px solid #e1e4e8" :border-radius "6px"}}
   [:div {:style {:font-size "11px" :color "#6a737d"}} label]
   [:div {:style {:font-size "18px" :font-weight "600" :color "#24292e"}} value]
   [:div {:style {:font-size "10px" :color "#959da5"}} hint]])

(defn throughput-chart
  "Grouped columns of done vs reverted per bucket. Hovering or focusing a
   bucket highlights it and shows its counts in the caption below."
  [_buckets _bucket-minutes]
  (let [hovered (r/atom nil)]
    (fn [buckets bucket-minutes]
      (let [{:keys [slot-width plot-height]} stats-chart-dims
            {:keys [y-max bars]} (board-stats/chart-bars buckets stats-chart-dims)
            axis-w 24
            top 6
            n (count buckets)
            width (+ axis-w (* slot-width n))
            y0 (+ top plot-height)
            slot-x (fn [i] (+ axis-w (* i slot-width)))
            axis-text {:font-size 10 :fill "#6a737d"}]
        [:div
         [:div {:style {:display "flex" :gap "12px" :font-size "11px" :color "#24292e"
                        :margin-bottom "4px"}}
          (for [{:keys [key label color]} board-stats/series]
            ^{:key key}
            [:span {:style {:display "flex" :align-items "center" :gap "4px"}}
             [:span {:style {:width "8px" :height "8px" :border-radius "2px" :background color}}]
             label])]
         [:svg {:width width :height (+ y0 14) :role "img"
                :aria-label (str "Done and reverted transitions per " bucket-minutes
                                 " minutes over the last " n " buckets")
                :style {:display "block"}}
          [:line {:x1 axis-w :x2 width :y1 top :y2 top :stroke "#eaecef" :stroke-width 1}]
          [:line {:x1 axis-w :x2 width :y1 y0 :y2 y0 :stroke "#d1d5da" :stroke-width 1}]
          [:text (merge axis-text {:x (- axis-w 4) :y (+ top 4) :text-anchor "end"}) y-max]
          [:text (merge axis-text {:x (- axis-w 4) :y y0 :text-anchor "end"}) 0]
          (when-let [i @hovered]
            [:rect {:x (slot-x i) :y top :width slot-width :height plot-height :fill "#f1f8ff"}])
          (for [{:keys [bucket key x w h color]} bars]
            ^{:key (str bucket "-" (name key))}
            [:path {:d (board-stats/bar-path (+ axis-w x) y0 w h) :fill color}])
          (when (pos? n)
            [:<>
             [:text (merge axis-text {:x axis-w :y (+ y0 12)}) (clock-label (:start (first buckets)))]
             [:text (merge axis-text {:x width :y (+ y0 12) :text-anchor "end"}) "now"]])
          ;; Hit targets span the full slot height, wider than the bars.
          (for [[i bucket] (map-indexed vector buckets)]
            ^{:key i}
            [:rect {:x (slot-x i) :y top :width slot-width :height plot-height
                    :fill "transparent" :tab-index 0
                    :on-mouse-enter #(reset! hovered i)
                    :on-mouse-leave #(reset! hovered nil)
                    :on-focus #(reset! hovered i)
                    :on-blur #(reset! hovered nil)}
             [:title (bucket-summary bucket bucket-minutes)]])]
         [:div {:style {:font-size "11px" :color "#6a737d" :min-height "15px"}}
          (when-let [bucket (some->> @hovered (get buckets))]
            (bucket-summary bucket bucket-minutes))]]))))

(defn stats-table [buckets bucket-minutes]
  (let [cell {:padding "1px 8px" :text-align "right"
              :font-variant-numeric "tabular-nums"}]
    [:details {:style {:font-size "11px" :color "#6a737d"}}
     [:summary {:style {:cursor "pointer"}} "Table view"]
     [:table {:style {:border-collapse "collapse" :color "#24292e" :margin-top "4px"}}
      [:thead
       [:tr
        [:th {:style (assoc cell :text-align "left")} "Time"]
        (for [k ["Done" "Reverted" "Rejected" "Created" "Reopened"]]
          ^{:key k} [:th {:style cell} k])]]
      [:tbody
       (for [b buckets]
         ^{:key (:start b)}
         [:tr
          [:td {:style (assoc cell :text-align "left")} (bucket-label (:start b) bucket-minutes)]
          (for [k [:done :reverted :rejected :created :reopened]]
            ^{:key k} [:td {:style cell} (get b k 0)])])]]]))

(defn stats-strip
  "Collapsible throughput strip: per-hour rate tiles plus an hourly chart."
  []
  (let [stats @(rf/subscribe [:board-stats])
        collapsed? @(rf/subscribe [:board-stats-collapsed])
        {:keys [hours bucket-minutes buckets rates-per-hour]} stats
        window (str "last " hours "h")]
    (when stats
      [:div {:style {:margin-bottom "16px" :padding "8px 12px" :background "#f6f8fa"
                     :border "1px solid #e1e4e8" :border-radius "6px"}}
       [:button {:aria-expanded (not collapsed?)
                 :on-click #(rf/dispatch [:toggle-board-stats-collapsed])
                 :style {:background "none" :border "none" :padding "0" :cursor "pointer"
                         :font-size "12px" :font-weight "600" :color "#24292e"}}
        (str (if collapsed? "▸" "▾") " Throughput · " window)
        (when collapsed?
          [:span {:style {:font-weight "400" :color "#6a737d" :margin-left "8px"}}
           (str (board-stats/format-rate (:done rates-per-hour)) " done/h · "
                (board-stats/format-rate (:reverted rates-per-hour)) " reverted/h")])]
       (when-not collapsed?
         (if-not (board-stats/active? stats)
           [:p {:style {:margin "8px 0 0" :font-size "12px" :color "#6a737d"}}
            "No activity recorded yet. Status changes are logged from this version onward; earlier history isn't counted."]
           [:div {:style {:display "flex" :gap "16px" :flex-wrap "wrap" :align-items "flex-start"
                          :margin-top "8px"}}
            [:div {:style {:display "flex" :gap "8px" :flex-wrap "wrap"}}
             [stat-tile "Done per hour" (board-stats/format-rate (:done rates-per-hour)) window]
             [stat-tile "Reverted per hour" (board-stats/format-rate (:reverted rates-per-hour)) window]
             [stat-tile "Done" (board-stats/last-bucket-count stats :done) (str "last " bucket-minutes "m")]
             [stat-tile "Reverted" (board-stats/last-bucket-count stats :reverted) (str "last " bucket-minutes "m")]
             [stat-tile "Median cycle" (board-stats/format-minutes (:median-cycle-minutes stats))
              (str "in progress → done, n=" (:cycle-sample-size stats))]]
            [:div {:style {:overflow-x "auto" :max-width "100%"}}
             [throughput-chart buckets bucket-minutes]
             [stats-table buckets bucket-minutes]]]))])))

;; ============================================================================
;; Board View (Kanban)
;; ============================================================================

(defn board-view []
  (let [repo-path @(rf/subscribe [:board-repo])
        all-tasks @(rf/subscribe [:board-tasks])
        tasks @(rf/subscribe [:filtered-board-tasks])
        show-done @(rf/subscribe [:board-show-done])
        selected @(rf/subscribe [:selected-task])
        stale-count (count (filter :claim-stale all-tasks))
        grouped (group-by :status tasks)
        ;; status-order is the hardcoded canonical order; boards with
        ;; customized statuses (deliberately untouched by the statuses
        ;; migration) can have tasks whose status isn't in it. Append any
        ;; such statuses, in stable (first-seen) order, so they still get a
        ;; column instead of silently vanishing from the board.
        extra-statuses (->> all-tasks
                            (map :status)
                            distinct
                            (remove (set status-order)))
        all-statuses (into status-order extra-statuses)
        visible-statuses (if show-done
                           all-statuses
                           (remove #(#{"done" "rejected"} %) all-statuses))]
    [:div {:style {:padding-right (when selected "420px")
                   :transition "padding-right 0.2s"}}
     ;; Header
     [:div {:style {:display "flex" :justify-content "space-between"
                    :align-items "center" :margin-bottom "16px"}}
      [:div
       [:h2 {:style {:margin "0" :color "#24292e"}}
        (extract-repo-name repo-path)]
       [:span {:style {:color "#6a737d" :font-size "12px"}} repo-path]
       (when (pos? stale-count)
         [:span {:title "Claimed tasks whose worker has gone quiet past the claim lease"
                 :style {:margin-left "8px" :font-size "12px" :color "#9a6700"}}
          (str "⚠ " stale-count " stale")])]
      [:label {:style {:font-size "12px" :color "#6a737d" :cursor "pointer"
                       :display "flex" :align-items "center" :gap "4px"}}
       [:input {:type "checkbox"
                :checked (boolean show-done)
                :on-change #(rf/dispatch [:toggle-board-show-done])}]
       "Show completed"]]

     [stats-strip]

     [filter-bar all-tasks (count tasks)]

     ;; Kanban columns
     [:div {:style {:display "flex" :gap "16px" :overflow-x "auto"
                    :padding-bottom "16px"}}
      (for [status visible-statuses]
        ^{:key status}
        [kanban-column status (get grouped status [])])]

     ;; Detail panel - keyed on task id so note-text atom resets when switching tasks
     (when selected
       ^{:key (:id selected)}
       [task-detail])]))
