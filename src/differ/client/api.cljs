(ns differ.client.api
  "HTTP API client for the Differ server."
  (:require [re-frame.core :as rf]))

(def base-url "")  ;; Same origin

(defn- fetch-json
  "Fetch JSON from URL, dispatch success/error events."
  [{:keys [method url body on-success on-failure]}]
  (-> (js/fetch (str base-url url)
                (clj->js (cond-> {:method (or method "GET")
                                  :headers {"Content-Type" "application/json"}}
                           body (assoc :body (js/JSON.stringify (clj->js body))))))
      (.then (fn [response]
               (if (.-ok response)
                 (.json response)
                 (throw (js/Error. (str "HTTP " (.-status response)))))))
      (.then (fn [data]
               (when on-success
                 (rf/dispatch (conj on-success (js->clj data :keywordize-keys true))))))
      (.catch (fn [error]
                (when on-failure
                  (rf/dispatch (conj on-failure (.-message error))))))))

;; Re-frame effect for API calls
(rf/reg-fx
 :http
 (fn [request]
   (fetch-json request)))

;; Convenience functions for common API calls

(defn fetch-config []
  {:method "GET"
   :url "/api/config"
   :on-success [:config-loaded]
   :on-failure [:api-error]})

(defn fetch-sessions []
  {:method "GET"
   :url "/api/sessions"
   :on-success [:sessions-loaded]
   :on-failure [:api-error]})

(defn create-session [repo-path]
  {:method "POST"
   :url "/api/sessions"
   :body {:repo_path repo-path}
   :on-success [:session-created]
   :on-failure [:api-error]})

(defn fetch-session [session-id]
  {:method "GET"
   :url (str "/api/sessions/" session-id)
   :on-success [:session-loaded]
   :on-failure [:api-error]})

(defn fetch-diff [session-id]
  {:method "GET"
   :url (str "/api/sessions/" session-id "/diff")
   :on-success [:diff-loaded]
   :on-failure [:api-error]})

(defn fetch-comments [session-id]
  {:method "GET"
   :url (str "/api/sessions/" session-id "/comments")
   :on-success [:comments-loaded]
   :on-failure [:api-error]})

(defn add-comment [session-id {:keys [file line text author parent-id side line-content context-before context-after]}]
  {:method "POST"
   :url (str "/api/sessions/" session-id "/comments")
   :body {:file file
          :line line
          :text text
          :author author
          :parent-id parent-id
          :side side
          :line-content line-content
          :context-before context-before
          :context-after context-after}
   :on-success [:comment-added]
   :on-failure [:api-error]})

(defn resolve-comment [comment-id author]
  {:method "PATCH"
   :url (str "/api/comments/" comment-id "/resolve")
   :body {:author author}
   :on-success [:comment-resolved comment-id]
   :on-failure [:api-error]})

(defn unresolve-comment [comment-id author]
  {:method "PATCH"
   :url (str "/api/comments/" comment-id "/unresolve")
   :body {:author author}
   :on-success [:comment-unresolved comment-id]
   :on-failure [:api-error]})

(defn delete-session [session-id]
  {:method "DELETE"
   :url (str "/api/sessions/" session-id)
   :on-success [:session-deleted session-id]
   :on-failure [:api-error]})

(defn update-session [session-id updates]
  {:method "PATCH"
   :url (str "/api/sessions/" session-id)
   :body updates
   :on-success [:session-updated]
   :on-failure [:api-error]})

(defn fetch-branches [session-id]
  {:method "GET"
   :url (str "/api/sessions/" session-id "/branches")
   :on-success [:branches-loaded]
   :on-failure [:api-error]})

(defn fetch-staged-files [session-id]
  {:method "GET"
   :url (str "/api/sessions/" session-id "/staged")
   :on-success [:staged-files-loaded]
   :on-failure [:api-error]})

(defn stage-file [session-id file-path]
  {:method "POST"
   :url (str "/api/sessions/" session-id "/stage")
   :body {:path file-path}
   :on-success [:file-staged]
   :on-failure [:api-error]})

(defn fetch-untracked-files [session-id]
  {:method "GET"
   :url (str "/api/sessions/" session-id "/untracked")
   :on-success [:untracked-files-loaded]
   :on-failure [:api-error]})

(defn add-manual-file [session-id path]
  {:method "POST"
   :url (str "/api/sessions/" session-id "/manual-files")
   :body {:path path}
   :on-success [:manual-file-added]
   :on-failure [:api-error]})

(defn remove-manual-file [session-id path]
  {:method "DELETE"
   :url (str "/api/sessions/" session-id "/manual-files")
   :body {:path path}
   :on-success [:manual-file-removed]
   :on-failure [:api-error]})

(defn restore-file [session-id path]
  {:method "POST"
   :url (str "/api/sessions/" session-id "/restore-file")
   :body {:path path}
   :on-success [:file-restored]
   :on-failure [:api-error]})

(defn fetch-file-content [session-id file-path]
  {:method "GET"
   :url (str "/api/sessions/" session-id "/file-content/" (js/encodeURIComponent file-path))
   :on-success [:file-content-loaded file-path]
   :on-failure [:api-error]})

(defn fetch-context-lines [session-id file-path from-line to-line]
  {:method "GET"
   :url (str "/api/sessions/" session-id "/context/" (js/encodeURIComponent file-path)
             "?from=" from-line "&to=" to-line)
   :on-success [:context-lines-loaded file-path from-line to-line]
   :on-failure [:api-error]})
