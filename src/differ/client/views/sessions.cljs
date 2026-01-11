(ns differ.client.views.sessions
  "Session list view."
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [clojure.string :as str]))

(defn- format-date [iso-string]
  (when iso-string
    (let [date (js/Date. iso-string)
          year (.getFullYear date)
          month (-> (.getMonth date) inc (.toString) (.padStart 2 "0"))
          day (-> (.getDate date) (.toString) (.padStart 2 "0"))]
      (str year "/" month "/" day))))

(defn- format-time [iso-string]
  (when iso-string
    (let [date (js/Date. iso-string)
          now (js/Date.)
          diff-ms (- now date)
          diff-mins (js/Math.floor (/ diff-ms 60000))
          diff-hours (js/Math.floor (/ diff-mins 60))
          diff-days (js/Math.floor (/ diff-hours 24))]
      (cond
        (< diff-mins 1) "just now"
        (< diff-mins 60) (str diff-mins " min ago")
        (< diff-hours 24) (str diff-hours " hour" (when (> diff-hours 1) "s") " ago")
        :else (str diff-days " day" (when (> diff-days 1) "s") " ago")))))

(defn- extract-project-name [project]
  ;; Extract repo name from git URL or path
  (let [cleaned (-> project
                    (str/replace #"\.git$" "")
                    (str/replace #"^.*[:/]" ""))]
    cleaned))

(defn session-item [{:keys [id project branch target-branch unresolved-count updated-at]}]
  (let [count (or unresolved-count 0)]
    [:li.session-item
     {:on-click #(rf/dispatch [:navigate-session id])}
     [:div.session-info
      [:span.session-project (extract-project-name project)]
      [:span.session-branch
       (str branch " → " target-branch)
       [:span {:style {:color "#6a737d" :margin-left "8px"}}
        (format-time updated-at)]]]
     [:span {:class (str "session-badge" (when (zero? count) " zero"))}
      (if (zero? count)
        "No comments"
        (str count " unresolved"))]]))

(defn- get-recent-paths []
  (try
    (-> (.getItem js/localStorage "differ-recent-paths")
        js/JSON.parse
        (js->clj)
        (or []))
    (catch :default _ [])))

(defn- save-recent-path [path]
  (let [recent (get-recent-paths)
        updated (->> (cons path recent)
                     distinct
                     (take 5)
                     vec)]
    (.setItem js/localStorage "differ-recent-paths" (js/JSON.stringify (clj->js updated)))))

(defn new-session-form []
  (let [repo-path (r/atom "")
        show-form? (r/atom false)]
    (fn []
      (let [recent-paths (get-recent-paths)]
        (if @show-form?
          [:div {:style {:background "#f6f8fa"
                         :border "1px solid #d1d5da"
                         :border-radius "6px"
                         :padding "16px"
                         :margin-bottom "16px"}}
           [:div {:style {:margin-bottom "12px"}}
            [:label {:style {:display "block" :font-weight "600" :margin-bottom "4px"}}
             "Repository path"]
            [:input {:type "text"
                     :placeholder "/path/to/your/project"
                     :value @repo-path
                     :auto-focus true
                     :on-change #(reset! repo-path (.. % -target -value))
                     :on-key-down #(when (= (.-key %) "Enter")
                                     (when (seq @repo-path)
                                       (save-recent-path @repo-path)
                                       (rf/dispatch [:create-session @repo-path])
                                       (reset! repo-path "")
                                       (reset! show-form? false)))
                     :style {:width "100%"
                             :padding "8px 12px"
                             :border "1px solid #d1d5da"
                             :border-radius "4px"
                             :font-size "14px"}}]
            (when (seq recent-paths)
              [:div {:style {:margin-top "8px"}}
               [:span {:style {:font-size "12px" :color "#6a737d"}} "Recent: "]
               (for [path recent-paths]
                 ^{:key path}
                 [:button.btn-link
                  {:on-click #(reset! repo-path path)
                   :style {:font-size "12px" :margin-right "8px"}}
                  (last (str/split path #"/"))])])]
           [:div {:style {:display "flex" :gap "8px"}}
            [:button.btn.btn-primary
             {:on-click #(when (seq @repo-path)
                           (save-recent-path @repo-path)
                           (rf/dispatch [:create-session @repo-path])
                           (reset! repo-path "")
                           (reset! show-form? false))}
             "Create Session"]
            [:button.btn.btn-secondary
             {:on-click #(do (reset! repo-path "")
                             (reset! show-form? false))}
             "Cancel"]]]
          [:button.btn.btn-primary
           {:on-click #(reset! show-form? true)
            :style {:margin-bottom "16px"}}
           "+ New Session"])))))

(defn- token-type-badge [token-type]
  (let [is-pat? (= token-type "pat")]
    [:span {:class (str "token-type-badge" (when is-pat? " pat"))
            :style {:font-size "10px"
                    :padding "2px 6px"
                    :border-radius "3px"
                    :margin-left "8px"
                    :background (if is-pat? "#fff3cd" "#d1f7d6")
                    :color (if is-pat? "#856404" "#22863a")
                    :font-weight "500"}}
     (if is-pat? "PAT" "OAuth")]))

(defn- token-item [& _]
  (let [confirming? (r/atom false)
        last-id (r/atom nil)]
    (fn [{:keys [id github-username name token-type scope created-at]}]
      ;; Reset confirmation state if token ID changed (list reordered via SSE)
      (when (not= @last-id id)
        (reset! last-id id)
        (reset! confirming? false))
      (let [is-pat? (= token-type "pat")
            display-name (if is-pat? name github-username)]
        [:div.token-item
         [:div.token-info
          [:span.token-username
           display-name
           [token-type-badge token-type]]
          [:span.token-meta
           (str "Added " (format-date created-at))
           (when (and (not is-pat?) scope)
             (str " · " scope))]]
         (if @confirming?
           [:div {:style {:display "flex" :gap "8px"}}
            [:button.btn.btn-danger
             {:on-click #(do (rf/dispatch [:delete-github-token id])
                             (reset! confirming? false))}
             "Confirm"]
            [:button.btn.btn-secondary
             {:on-click #(reset! confirming? false)}
             "Cancel"]]
           [:button.btn.btn-secondary
            {:on-click #(reset! confirming? true)}
            "Revoke"])]))))

(defn pat-modal []
  (let [visible? @(rf/subscribe [:pat-modal-visible?])
        form @(rf/subscribe [:pat-form])
        {:keys [name token error submitting]} form
        loading? submitting]
    (when visible?
      [:div.modal-overlay
       {:on-click #(rf/dispatch [:hide-pat-modal])}
       [:div.modal-content
        {:on-click #(.stopPropagation %)}
        [:div.modal-header
         [:h3 {:style {:margin 0}} "Add Personal Access Token"]
         [:button.btn-close
          {:on-click #(rf/dispatch [:hide-pat-modal])}
          "×"]]
        [:div.modal-body
         [:p {:style {:color "#6a737d" :margin "0 0 16px 0" :font-size "13px"}}
          "Personal Access Tokens can access repositories that restrict OAuth apps. "
          "Some organizations block third-party OAuth applications, requiring PATs instead."]

         (when error
           [:div {:style {:background "#ffeef0"
                          :border "1px solid #f97583"
                          :padding "8px 12px"
                          :border-radius "4px"
                          :margin-bottom "12px"
                          :color "#cb2431"
                          :font-size "13px"}}
            error])

         [:div {:style {:margin-bottom "12px"}}
          [:label {:style {:display "block" :font-weight "600" :margin-bottom "4px"}}
           "Name"]
          [:input {:type "text"
                   :placeholder "e.g., Work repos access"
                   :value (or name "")
                   :on-change #(rf/dispatch [:update-pat-form :name (.. % -target -value)])
                   :disabled loading?
                   :style {:width "100%"
                           :padding "8px 12px"
                           :border "1px solid #d1d5da"
                           :border-radius "4px"
                           :font-size "14px"
                           :box-sizing "border-box"}}]
          [:span {:style {:font-size "11px" :color "#6a737d"}}
           "A name to help you remember what this token is for"]]

         [:div {:style {:margin-bottom "16px"}}
          [:label {:style {:display "block" :font-weight "600" :margin-bottom "4px"}}
           "Token"]
          [:input {:type "password"
                   :placeholder "ghp_xxxxxxxxxxxxxxxxxxxx"
                   :value (or token "")
                   :on-change #(rf/dispatch [:update-pat-form :token (.. % -target -value)])
                   :on-key-down #(when (and (= (.-key %) "Enter")
                                            (not (str/blank? name))
                                            (not (str/blank? token))
                                            (not loading?))
                                   (rf/dispatch [:submit-pat]))
                   :disabled loading?
                   :style {:width "100%"
                           :padding "8px 12px"
                           :border "1px solid #d1d5da"
                           :border-radius "4px"
                           :font-size "14px"
                           :font-family "monospace"
                           :box-sizing "border-box"}}]
          [:span {:style {:font-size "11px" :color "#6a737d"}}
           "Classic PAT or fine-grained token with repo access"]]

         [:div {:style {:background "#f6f8fa"
                        :border "1px solid #d1d5da"
                        :border-radius "4px"
                        :padding "12px"
                        :margin-bottom "16px"}}
          [:p {:style {:margin "0 0 8px 0" :font-weight "600" :font-size "13px"}}
           "Create a token:"]
          [:ol {:style {:margin "0" :padding-left "20px" :font-size "12px" :color "#6a737d"}}
           [:li "Go to "
            [:a {:href "https://github.com/settings/tokens/new?scopes=repo&description=differ-access"
                 :target "_blank"
                 :rel "noopener noreferrer"}
             "GitHub → Settings → Personal Access Tokens"]]
           [:li "Select \"repo\" scope (full control of private repositories)"]
           [:li "Generate and copy the token"]]]]
        [:div.modal-footer
         [:button.btn.btn-secondary
          {:on-click #(rf/dispatch [:hide-pat-modal])
           :disabled loading?}
          "Cancel"]
         [:button.btn.btn-primary
          {:on-click #(rf/dispatch [:submit-pat])
           :disabled (or loading? (str/blank? name) (str/blank? token))}
          (if loading? "Adding..." "Add Token")]]]])))

(defn github-settings []
  (let [visible? @(rf/subscribe [:github-settings-visible?])
        status @(rf/subscribe [:github-status])
        tokens @(rf/subscribe [:github-tokens])
        loading? @(rf/subscribe [:loading? :github-tokens])
        just-connected? @(rf/subscribe [:github-just-connected?])]
    [:div.github-settings
     [:div.github-settings-header
      {:on-click #(if visible?
                    (rf/dispatch [:hide-github-settings])
                    (rf/dispatch [:show-github-settings]))}
      [:span {:style {:display "flex" :align-items "center" :gap "8px"}}
       [:span {:style {:font-size "12px"}}
        (if visible? "▼" "▶")]
       "GitHub Integration"]
      (when (:connected status)
        [:span.github-connected-badge "Connected"])]
     (when visible?
       [:div.github-settings-content
        (when just-connected?
          [:div {:style {:background "#dcffe4"
                         :border "1px solid #28a745"
                         :padding "8px 12px"
                         :border-radius "4px"
                         :margin-bottom "12px"
                         :color "#22863a"
                         :font-size "13px"}}
           "GitHub account connected successfully!"])
        (cond
          loading?
          [:p {:style {:color "#6a737d" :margin "8px 0"}} "Loading..."]

          (not (:configured status))
          [:p {:style {:color "#6a737d" :margin "8px 0"}}
           "GitHub OAuth not configured. Set GITHUB_CLIENT_ID and GITHUB_CLIENT_SECRET environment variables."]

          :else
          [:div
           (when (seq tokens)
             [:div
              [:p {:style {:color "#6a737d" :margin "0 0 8px 0" :font-size "12px"}}
               "Connected accounts have access to all repositories you can access on GitHub."]
              [:ul.token-list
               (for [token tokens]
                 ^{:key (:id token)}
                 [token-item token])]])
           [:div {:style {:display "flex" :gap "8px" :margin-top "12px" :flex-wrap "wrap"}}
            [:a.btn.btn-primary
             {:href "/oauth/github"
              :style {:display "inline-block"}}
             (if (empty? tokens) "Connect GitHub Account" "Connect Another Account")]
            [:button.btn.btn-secondary
             {:on-click #(rf/dispatch [:show-pat-modal])}
             "Add Personal Access Token"]]
           (when (seq tokens)
             [:p {:style {:color "#6a737d" :margin "8px 0 0 0" :font-size "11px"}}
              "To add a different account, first log out of GitHub or use a private window."])
           [:p {:style {:color "#6a737d" :margin "8px 0 0 0" :font-size "11px"}}
            "Use PATs to access repositories in organizations that restrict OAuth apps."]])
        [pat-modal]])]))

(defn session-list []
  (let [sessions @(rf/subscribe [:sessions])
        loading? @(rf/subscribe [:loading? :sessions])]
    [:div
     [:div {:style {:display "flex"
                    :justify-content "space-between"
                    :align-items "center"
                    :margin-bottom "16px"}}
      [:h2 {:style {:margin 0}} "Active Sessions"]
      [:button.btn.btn-secondary
       {:on-click #(rf/dispatch [:load-sessions])}
       "Refresh"]]

     [new-session-form]

     (cond
       loading?
       [:div.empty-state [:p "Loading sessions..."]]

       (empty? sessions)
       [:div.empty-state
        [:h2 "No active sessions"]
        [:p "Enter a repository path above to start a new review session."]]

       :else
       [:ul.session-list
        (for [session sessions]
          ^{:key (:id session)}
          [session-item session])])

     ;; GitHub settings at the bottom
     [:div {:style {:margin-top "32px" :border-top "1px solid #e1e4e8" :padding-top "24px"}}
      [github-settings]]]))
