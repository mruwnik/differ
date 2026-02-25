(ns differ.client.main
  "Client entry point."
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdc]
            [re-frame.core :as rf]
            [clojure.string :as str]
            [differ.client.db]
            [differ.client.events]
            [differ.client.subs]
            [differ.client.api]
            [differ.client.sse]
            [differ.client.views.core :as views]))

(defonce root (atom nil))

(defn- parse-route
  "Parse the current URL path into a route."
  []
  (let [path (.-pathname js/location)]
    (cond
      (= path "/boards")
      {:page :boards}

      (str/starts-with? path "/boards/")
      {:page :board :board-repo (js/decodeURIComponent (subs path 8))}

      (str/starts-with? path "/session/")
      {:page :session :session-id (subs path 9)}

      :else
      {:page :sessions})))

(defn- setup-popstate-handler
  "Handle browser back/forward navigation."
  []
  (.addEventListener js/window "popstate"
                     (fn [_]
                       (let [{:keys [page session-id board-repo]} (parse-route)]
                         (case page
                           :boards (rf/dispatch [:navigate-boards {:replace true}])
                           :board (rf/dispatch [:navigate-board board-repo {:replace true}])
                           :session (rf/dispatch [:navigate-session session-id {:replace true}])
                           (rf/dispatch [:navigate-sessions {:replace true}]))))))

(defn ^:dev/after-load mount-root []
  (rf/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (if-let [react-root @root]
      (.render react-root (r/as-element [views/app]))
      (let [react-root (rdc/create-root root-el)]
        (reset! root react-root)
        (.render react-root (r/as-element [views/app]))))))

(defn init []
  (rf/dispatch-sync [:initialize])
  (setup-popstate-handler)
  ;; Navigate based on current URL
  (let [{:keys [page session-id board-repo]} (parse-route)]
    (case page
      :session (rf/dispatch [:navigate-session session-id {:replace true}])
      :boards (rf/dispatch [:navigate-boards {:replace true}])
      :board (rf/dispatch [:navigate-board board-repo {:replace true}])
      nil))
  (mount-root))
