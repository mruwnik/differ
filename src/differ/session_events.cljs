(ns differ.session-events
  "Per-session event emitters. Thin wrappers around `differ.event-stream`
   that publish events to the scope `\"session:<session-id>\"` whenever a
   mutating MCP call lands on a session — comment added/resolved,
   files registered/unregistered, review submitted.

   Agents can then call `wait_for_event(scope=\"session:<session-id>\")`
   and block until any of these events arrive, instead of polling
   `get_review_state`. Existing SSE broadcasting to the browser is
   orthogonal and untouched — both buses run in parallel.

   IMPORTANT: when a GitHub PR has an associated differ session, a
   single mutating MCP call (e.g. `add_comment`) surfaces on BOTH
   scopes: immediately on `session:<id>` (via these emitters) AND
   within one poll interval on `github:owner/repo` as
   `:pr-feedback-changed`. Two events, two scopes, same underlying
   change. Agents should pick one scope based on their latency
   requirements — subscribing to both will produce duplicates."
  (:require [clojure.string :as str]
            [differ.event-stream :as event-stream]))

(defn session-scope
  "Build the scope string for a given session id."
  [session-id]
  (str "session:" session-id))

;; ============================================================================
;; Emitters
;;
;; All emitters short-circuit on a missing session-id (nil, empty
;; string, or whitespace) so MCP handlers don't need to guard at every
;; call site. They never throw. The deeper defence is in
;; `event-stream/append-events!`, which rejects non-string / blank
;; scopes outright.
;; ============================================================================

(defn- valid-session-id? [session-id]
  (and (string? session-id) (not (str/blank? session-id))))

(defn- append-one! [session-id event]
  (when (valid-session-id? session-id)
    (event-stream/append-events! (session-scope session-id) [event])))

(defn emit-comment-added!
  "Publish a :comment-added event. `comment` is the comment map returned
   by the backend; we keep a small subset of fields to avoid ballooning
   the ring buffer with repeated large payloads."
  [session-id comment]
  (append-one!
   session-id
   {:event-type  :comment-added
    :session-id  session-id
    :comment-id  (:id comment)
    :author      (:author comment)
    :file        (:file comment)
    :line        (:line comment)
    :parent-id   (:parent-id comment)
    :created-at  (:created-at comment)}))

(defn emit-comment-resolved!
  [session-id comment-id author]
  (append-one!
   session-id
   {:event-type :comment-resolved
    :session-id session-id
    :comment-id comment-id
    :author     author}))

(defn emit-comment-unresolved!
  [session-id comment-id author]
  (append-one!
   session-id
   {:event-type :comment-unresolved
    :session-id session-id
    :comment-id comment-id
    :author     author}))

(defn emit-files-registered!
  [session-id paths agent-id]
  (when (seq paths)
    (append-one!
     session-id
     {:event-type :files-registered
      :session-id session-id
      :paths      (vec paths)
      :agent-id   agent-id})))

(defn emit-files-unregistered!
  [session-id paths agent-id]
  (when (seq paths)
    (append-one!
     session-id
     {:event-type :files-unregistered
      :session-id session-id
      :paths      (vec paths)
      :agent-id   agent-id})))

(defn emit-review-submitted!
  [session-id author]
  (append-one!
   session-id
   {:event-type :review-submitted
    :session-id session-id
    :author     author}))
