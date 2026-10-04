(ns differ.client.views.board
  "Kanban board views for task coordination."
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [clojure.string :as str]
            [differ.client.task-filter :as task-filter]))

(def status-order ["pending" "planning" "plan_review" "ready" "in_progress" "blocked" "in_review" "done" "rejected"])

(def status-labels
  {"pending" "Pending"
   "planning" "Planning"
   "plan_review" "Plan Review"
   "ready" "Ready"
   "in_progress" "In Progress"
   "blocked" "Blocked"
   "in_review" "In Review"
   "done" "Done"
   "rejected" "Rejected"})

(def status-colors
  {"pending" "#6a737d"
   "planning" "#6f42c1"
   "plan_review" "#b08800"
   "ready" "#00838f"
   "in_progress" "#0366d6"
   "blocked" "#d73a49"
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
     [:div {:style {:display "flex" :justify-content "space-between" :align-items "center"}}
      [:div {:style {:display "flex" :gap "6px" :align-items "center"}}
       (when (:worker-name task)
         [:span {:style {:font-size "11px" :color "#6a737d"}}
          (:worker-name task)])
       (when (seq (:blocked-by task))
         [:span {:style {:font-size "10px" :color "#d73a49" :font-weight "500"}}
          (str "blocked by " (count (:blocked-by task)))])]
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
            (when (:worker-id task)
              [:span {:style {:font-size "11px" :color "#959da5"}}
               (str "(" (:worker-id task) ")")])
            [priority-badge (:priority task)]]

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
;; Board View (Kanban)
;; ============================================================================

(defn board-view []
  (let [repo-path @(rf/subscribe [:board-repo])
        all-tasks @(rf/subscribe [:board-tasks])
        tasks @(rf/subscribe [:filtered-board-tasks])
        show-done @(rf/subscribe [:board-show-done])
        selected @(rf/subscribe [:selected-task])
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
       [:span {:style {:color "#6a737d" :font-size "12px"}} repo-path]]
      [:label {:style {:font-size "12px" :color "#6a737d" :cursor "pointer"
                       :display "flex" :align-items "center" :gap "4px"}}
       [:input {:type "checkbox"
                :checked (boolean show-done)
                :on-change #(rf/dispatch [:toggle-board-show-done])}]
       "Show completed"]]

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
