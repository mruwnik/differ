(ns differ.client.views.board
  "Kanban board views for task coordination."
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [clojure.string :as str]))

(def status-order ["pending" "in_progress" "blocked" "in_review" "done" "rejected"])

(def status-labels
  {"pending" "Pending"
   "in_progress" "In Progress"
   "blocked" "Blocked"
   "in_review" "In Review"
   "done" "Done"
   "rejected" "Rejected"})

(def status-colors
  {"pending" "#6a737d"
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
;; Task Card
;; ============================================================================

(defn task-card [task]
  (let [selected @(rf/subscribe [:selected-task])
        is-selected (and selected (= (:id selected) (:id task)))]
    [:div {:style {:padding "8px 12px"
                   :margin-bottom "6px"
                   :background (if is-selected "#ddf4ff" "#fff")
                   :border (str "1px solid " (if is-selected "#0366d6" "#e1e4e8"))
                   :border-radius "4px"
                   :cursor "pointer"}
           :on-click #(rf/dispatch [:select-task task])}
     [:div {:style {:font-size "13px" :font-weight "500" :margin-bottom "4px"
                    :color "#24292e"}}
      (:title task)]
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
  [:div {:style {:min-width "240px"
                 :max-width "300px"
                 :flex "1 1 240px"}}
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
               (str "(" (:worker-id task) ")")])]

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
;; Board View (Kanban)
;; ============================================================================

(defn board-view []
  (let [repo-path @(rf/subscribe [:board-repo])
        tasks @(rf/subscribe [:board-tasks])
        show-done @(rf/subscribe [:board-show-done])
        selected @(rf/subscribe [:selected-task])
        grouped (group-by :status tasks)
        visible-statuses (if show-done
                           status-order
                           (remove #(#{"done" "rejected"} %) status-order))]
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
