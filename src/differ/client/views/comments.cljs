(ns differ.client.views.comments
  "Comment thread components."
  (:require [re-frame.core :as rf]
            [clojure.string :as str]))

(defn- format-time [iso-string]
  (when iso-string
    (let [date (js/Date. iso-string)]
      (.toLocaleString date))))

(defn- staleness-indicator [staleness]
  (case staleness
    :changed [:span.stale-warning "Line content has changed since this comment was made"]
    :shifted [:span.stale-warning "Line may have shifted since this comment was made"]
    nil))

(defn comment-item [{:keys [id text author resolved created-at staleness] :as comment} depth]
  [:div.comment {:class (str (when resolved "resolved ")
                             (when (= staleness :changed) "stale"))}
   [:div.comment-header
    [:span.comment-author author]
    [:span.comment-time (format-time created-at)]]
   (when (= staleness :changed)
     [staleness-indicator staleness])
   [:div.comment-body text]
   [:div.comment-actions
    [:button.btn-link
     {:on-click #(rf/dispatch [:show-comment-form {:parent-id id}])}
     "Reply"]
    (if resolved
      [:button.btn-link
       {:on-click #(rf/dispatch [:unresolve-comment id])}
       "Unresolve"]
      [:button.btn-link
       {:on-click #(rf/dispatch [:resolve-comment id])}
       "Resolve"])]
   ;; Nested replies
   (when-let [replies (seq (:replies comment))]
     [:div {:style {:margin-left "16px" :margin-top "8px"}}
      (for [reply replies]
        ^{:key (:id reply)}
        [comment-item reply (inc depth)])])])

(defn comment-threads [comments file line]
  [:div.comment-thread
   (for [comment comments]
     ^{:key (:id comment)}
     [comment-item comment 0])])

(defn comment-form-inline [{:keys [file line parent-id]}]
  (let [form @(rf/subscribe [:comment-form])
        text (:text form)]
    [:div.comment-thread
     [:div.comment-form
      [:div {:style {:margin-bottom "8px" :font-size "13px" :color "#6a737d"}}
       (if parent-id
         "Reply to comment"
         (str "Comment on line " line))]
      [:textarea
       {:value text
        :placeholder "Leave a comment..."
        :auto-focus true
        :on-change #(rf/dispatch [:update-comment-text (.. % -target -value)])}]
      [:div.comment-form-actions
       [:button.btn.btn-secondary
        {:on-click #(rf/dispatch [:hide-comment-form])}
        "Cancel"]
       [:button.btn.btn-primary
        {:on-click #(rf/dispatch [:submit-comment])
         :disabled (str/blank? text)}
        "Comment"]]]]))
