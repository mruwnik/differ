(ns differ.client.views.core
  "Root component and routing."
  (:require [re-frame.core :as rf]
            [differ.client.views.sessions :as sessions]
            [differ.client.views.diff :as diff]))

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
                :border "1px solid #30363d"
                :border-radius "6px"
                :color "#c9d1d9"
                :padding "4px 8px"
                :font-size "14px"
                :cursor "pointer"
                :max-width "300px"}}
       (for [s sessions]
         ^{:key (:id s)}
         [:option {:value (:id s)
                   :style {:background "#0d1117"}}
          (str (:project s) " / " (:branch s)
               (when (pos? (:unresolved-count s 0))
                 (str " (" (:unresolved-count s) ")")))])])))

(defn header []
  (let [page @(rf/subscribe [:current-page])
        connected? @(rf/subscribe [:sse-connected?])]
    [:header.header
     [:div {:style {:display "flex" :align-items "center" :gap "16px"}}
      [:h1 {:style {:cursor "pointer"}
            :on-click #(rf/dispatch [:navigate-sessions])}
       "Differ"]
      (when (= page :session)
        [session-selector])]
     [:div {:style {:display "flex" :align-items "center" :gap "12px"}}
      (when (= page :session)
        [:span {:style {:font-size "12px"
                        :color (if connected? "#28a745" "#d73a49")}}
         (if connected? "Live" "Disconnected")])]]))

(defn error-banner []
  (let [error @(rf/subscribe [:error])]
    (when error
      [:div {:style {:background "#ffebe9"
                     :border "1px solid #d73a49"
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
                     :background "#0366d6"
                     :color "white"
                     :padding "4px 12px"
                     :border-radius "4px"
                     :font-size "12px"}}
       "Loading..."])))

(defn main-content []
  (let [page @(rf/subscribe [:current-page])]
    (case page
      :sessions [sessions/session-list]
      :session [diff/diff-view]
      [:div.empty-state
       [:h2 "Welcome to Differ"]
       [:p "Select a session to start reviewing"]])))

(defn app []
  [:div.app-container
   [header]
   [error-banner]
   [loading-overlay]
   [main-content]])
