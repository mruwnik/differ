(ns differ.client.sse
  "Server-Sent Events client for live updates."
  (:require [re-frame.core :as rf]))

(defonce ^:private event-source (atom nil))

(defn connect!
  "Connect to SSE endpoint, optionally for a specific session."
  [& [session-id]]
  (when @event-source
    (.close @event-source))

  (let [url (if session-id
              (str "/events?session=" session-id)
              "/events")
        es (js/EventSource. url)]

    ;; Connection opened
    (set! (.-onopen es)
          (fn [_]
            (rf/dispatch [:sse-connected])))

    ;; Connection error
    (set! (.-onerror es)
          (fn [_]
            (rf/dispatch [:sse-error])))

    ;; Handle custom events
    (.addEventListener es "connected"
                       (fn [e]
                         (let [data (js->clj (js/JSON.parse (.-data e)) :keywordize-keys true)]
                           (rf/dispatch [:sse-client-id (:client-id data)]))))

    (.addEventListener es "comment-added"
                       (fn [e]
                         (let [data (js->clj (js/JSON.parse (.-data e)) :keywordize-keys true)]
                           (rf/dispatch [:sse-comment-added (:comment data)]))))

    (.addEventListener es "comment-resolved"
                       (fn [e]
                         (let [data (js->clj (js/JSON.parse (.-data e)) :keywordize-keys true)]
                           (rf/dispatch [:sse-comment-resolved (:comment-id data)]))))

    (.addEventListener es "session-updated"
                       (fn [e]
                         (let [data (js->clj (js/JSON.parse (.-data e)) :keywordize-keys true)]
                           (rf/dispatch [:sse-session-updated (:session-id data)]))))

    (.addEventListener es "files-changed"
                       (fn [e]
                         (let [data (js->clj (js/JSON.parse (.-data e)) :keywordize-keys true)]
                           (rf/dispatch [:sse-files-changed (:files data)]))))

    (.addEventListener es "diff-changed"
                       (fn [e]
                         (let [data (js->clj (js/JSON.parse (.-data e)) :keywordize-keys true)]
                           (rf/dispatch [:sse-diff-changed (:session-id data)]))))

    (reset! event-source es)))

(defn disconnect! []
  (when-let [es @event-source]
    (.close es)
    (reset! event-source nil)
    (rf/dispatch [:sse-disconnected])))

;; Re-frame effect
(rf/reg-fx
 :sse-connect
 (fn [session-id]
   (connect! session-id)))

(rf/reg-fx
 :sse-disconnect
 (fn [_]
   (disconnect!)))
