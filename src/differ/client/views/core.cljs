(ns differ.client.views.core
  "Root component and routing."
  (:require [re-frame.core :as rf]
            [differ.client.views.sessions :as sessions]
            [differ.client.views.diff :as diff]
            [differ.client.views.board :as board]
            [differ.client.theme :as theme]))

(defn- session-selector []
  (let [sessions @(rf/subscribe [:sessions])
        current-id @(rf/subscribe [:current-session-id])]
    (when (seq sessions)
      [:select.session-selector
       {:value (or current-id "")
        :on-change #(let [id (-> % .-target .-value)]
                      (when (seq id)
                        (rf/dispatch [:navigate-session id])))
        :style {:background "transparent"
                :border "1px solid var(--color-border)"
                :border-radius "6px"
                :color "var(--color-fg)"
                :padding "4px 8px"
                :font-size "14px"
                :cursor "pointer"
                :max-width "300px"}}
       (for [s sessions]
         ^{:key (:id s)}
         [:option {:value (:id s)}
          (str (:project s) " / " (:branch s)
               (when (pos? (:unresolved-count s 0))
                 (str " (" (:unresolved-count s) ")")))])])))

(defn- theme-toggle []
  (let [current @(rf/subscribe [:theme])]
    [:div.diff-view-toggle {:role "group" :aria-label "Color theme"}
     (for [[value label] theme/choices]
       ^{:key value}
       [:button.btn-toggle
        {:type "button"
         :class (when (= current value) "active")
         :aria-pressed (= current value)
         :title (if (= value "system") "Follow the system setting" (str label " theme"))
         :on-click #(rf/dispatch [:set-theme value])}
        label])]))

(defn header []
  (let [page @(rf/subscribe [:current-page])
        connected? @(rf/subscribe [:sse-connected?])]
    [:header.header
     [:div {:style {:display "flex" :align-items "center" :gap "16px"}}
      [:h1 {:style {:cursor "pointer"}
            :on-click #(rf/dispatch [:navigate-sessions])}
       "Differ"]
      [:span {:style {:cursor "pointer"
                      :color (if (#{:boards :board} page) "var(--color-fg)" "var(--color-fg-muted)")
                      :font-size "14px"
                      :font-weight (if (#{:boards :board} page) "600" "400")}
              :on-click #(rf/dispatch [:navigate-boards])}
       "Boards"]
      (when (= page :session)
        [session-selector])]
     [:div {:style {:display "flex" :align-items "center" :gap "12px"}}
      (when (= page :session)
        [:span {:style {:font-size "12px"
                        :color (if connected? "var(--color-success-text)" "var(--color-danger-text)")}}
         (if connected? "Live" "Disconnected")])
      [theme-toggle]]]))

(defn error-banner []
  (let [error @(rf/subscribe [:error])]
    (when error
      [:div {:style {:background "var(--color-danger-subtle)"
                     :border "1px solid var(--color-danger-emphasis)"
                     :padding "12px 16px"
                     :margin-bottom "16px"
                     :border-radius "6px"
                     :display "flex"
                     :justify-content "space-between"
                     :align-items "center"}}
       [:span error]
       [:button.btn-link {:on-click #(rf/dispatch [:clear-error])}
        "Dismiss"]])))

(defn loading-overlay []
  (let [loading? @(rf/subscribe [:any-loading?])]
    (when loading?
      [:div {:style {:position "fixed"
                     :top "8px"
                     :right "8px"
                     :background "var(--color-accent-emphasis)"
                     :color "var(--color-fg-on-emphasis)"
                     :padding "4px 12px"
                     :border-radius "4px"
                     :font-size "12px"}}
       "Loading..."])))

(defn main-content []
  (let [page @(rf/subscribe [:current-page])]
    (case page
      :sessions [sessions/session-list]
      :session [diff/diff-view]
      :boards [board/boards-list]
      :board [board/board-view]
      [:div.empty-state
       [:h2 "Welcome to Differ"]
       [:p "Select a session to start reviewing"]])))

(defn app []
  [:div.app-container
   {:class (when (= :board @(rf/subscribe [:current-page])) "full-width")}
   [header]
   [error-banner]
   [loading-overlay]
   [main-content]])
