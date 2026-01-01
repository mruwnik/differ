(ns differ.client.views.core
  "Root component and routing."
  (:require [re-frame.core :as rf]
            [differ.client.views.sessions :as sessions]
            [differ.client.views.diff :as diff]))

(defn header []
  (let [page @(rf/subscribe [:current-page])
        session @(rf/subscribe [:current-session])
        connected? @(rf/subscribe [:sse-connected?])]
    [:header.header
     [:div {:style {:display "flex" :align-items "center" :gap "16px"}}
      [:h1 {:style {:cursor "pointer"}
            :on-click #(rf/dispatch [:navigate-sessions])}
       "Differ"]
      (when (= page :session)
        [:span {:style {:color "#6a737d"}}
         (str (:project session) " / " (:branch session))])]
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
