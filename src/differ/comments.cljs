(ns differ.comments
  "Comment management logic."
  (:require [differ.db :as db]
            [differ.git :as git]
            [differ.util :as util]
            [differ.schema :as schema]))

(defn compute-line-hash
  "Compute hash of line content for staleness detection."
  [repo-path file line]
  (let [content (git/get-line-content repo-path file line)]
    (util/sha256-hex (or content ""))))

(defn add-comment!
  "Add a new comment or reply.
   Returns the created comment."
  [{:keys [session-id parent-id file line text author repo-path]}]
  (let [;; For replies, inherit file/line from parent
        parent (when parent-id (db/get-comment parent-id))
        file (or file (:file parent))
        line (or line (:line parent))
        ;; Compute line hash
        line-hash (compute-line-hash repo-path file line)]
    (db/create-comment!
     {:session-id session-id
      :parent-id parent-id
      :file file
      :line line
      :line-content-hash line-hash
      :text text
      :author author})))

(defn resolve-comment!
  "Mark a comment as resolved."
  [comment-id author]
  ;; author param is for audit trail, not currently stored
  (db/resolve-comment! comment-id))

(defn unresolve-comment!
  "Mark a comment as unresolved."
  [comment-id author]
  ;; author param is for audit trail, not currently stored
  (db/unresolve-comment! comment-id))

(defn get-pending-feedback
  "Get unresolved comments for a session, optionally since a timestamp.
   Returns threaded comments (top-level with replies nested)."
  ([session-id] (get-pending-feedback session-id nil))
  ([session-id since]
   (let [comments (db/list-unresolved-comments session-id since)
         ;; Also include replies to unresolved comments
         all-comments (db/list-comments session-id)
         unresolved-ids (set (map :id comments))
         ;; Get all replies to unresolved comments
         replies-to-unresolved (filter
                                (fn [c]
                                  (and (:parent-id c)
                                       (contains? unresolved-ids (:parent-id c))))
                                all-comments)
         ;; Combine
         relevant-comments (concat comments replies-to-unresolved)]
     (schema/build-threads relevant-comments))))

(defn get-comments-for-file
  "Get all comments for a specific file, threaded."
  [session-id file]
  (let [comments (db/list-comments-for-file session-id file)]
    (schema/build-threads comments)))

(defn get-all-comments
  "Get all comments for a session, threaded."
  [session-id]
  (let [comments (db/list-comments session-id)]
    (schema/build-threads comments)))

(defn check-staleness
  "Check if a comment's line has changed since it was created.
   Returns :fresh, :shifted, or :changed."
  [comment repo-path]
  (let [{:keys [file line line-content-hash]} comment
        current-hash (compute-line-hash repo-path file line)]
    (cond
      (= current-hash line-content-hash) :fresh
      ;; TODO: Implement line shift detection by searching for content
      ;; For now, if hash doesn't match, assume changed
      :else :changed)))

(defn annotate-comments-with-staleness
  "Add :staleness key to each comment based on current file state."
  [comments repo-path]
  (letfn [(annotate [comment]
            (-> comment
                (assoc :staleness (check-staleness comment repo-path))
                (update :replies #(mapv annotate %))))]
    (mapv annotate comments)))
