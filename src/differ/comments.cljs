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
   If file/line are nil, creates a session-level comment.
   For replies, file/line/side are inherited from parent if not provided.
   Returns the created comment."
  [{:keys [session-id parent-id file line side line-content context-before context-after text author repo-path]}]
  (let [;; For replies, inherit file/line/side from parent
        parent (when parent-id (db/get-comment parent-id))
        ;; Inherit from parent, but only if parent has these values
        ;; (prevents accidentally making reply to session comment look like file comment)
        file (or file (when parent (:file parent)))
        line (or line (when parent (:line parent)))
        side (or side (when parent (:side parent)) "new")
        ;; For replies, inherit content context from parent if not provided
        line-content (or line-content (when parent (:line-content parent)))
        context-before (or context-before (when parent (:context-before parent)))
        context-after (or context-after (when parent (:context-after parent)))
        ;; Only compute line hash if we have file and line
        line-hash (when (and file line repo-path)
                    (compute-line-hash repo-path file line))]
    (db/create-comment!
     {:session-id session-id
      :parent-id parent-id
      :file file
      :line line
      :line-content-hash line-hash
      :side side
      :line-content line-content
      :context-before context-before
      :context-after context-after
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

(defn- find-shifted-line
  "Search nearby lines for matching content hash.
   Returns the new line number if found, nil otherwise.
   Searches within +/- search-range lines of original position."
  [repo-path file original-line target-hash search-range]
  (let [;; Get range of lines to search
        from-line (max 1 (- original-line search-range))
        to-line (+ original-line search-range)
        lines (git/get-lines-range repo-path file from-line to-line)]
    (when lines
      ;; Search for matching hash, preferring closest to original line
      (->> lines
           (map (fn [{:keys [line content]}]
                  {:line line
                   :hash (util/sha256-hex (or content ""))
                   :distance (js/Math.abs (- line original-line))}))
           (filter #(= (:hash %) target-hash))
           (filter #(not= (:line %) original-line))  ; Skip original line (already checked)
           (sort-by :distance)
           first
           :line))))

(defn check-staleness
  "Check if a comment's line has changed since it was created.
   Returns :fresh, :shifted, :changed, or nil for session-level comments.
   For :shifted, also returns :shifted-to with the new line number."
  [comment repo-path]
  (let [{:keys [file line line-content-hash]} comment]
    (if-not (and file line line-content-hash)
      nil  ; Session-level comments don't have staleness
      (let [current-hash (compute-line-hash repo-path file line)]
        (cond
          (= current-hash line-content-hash)
          :fresh

          ;; Content moved - search nearby lines
          :else
          (if-let [new-line (find-shifted-line repo-path file line line-content-hash 50)]
            {:status :shifted :shifted-to new-line}
            :changed))))))

(defn annotate-comments-with-staleness
  "Add :staleness key to each comment based on current file state."
  [comments repo-path]
  (letfn [(annotate [comment]
            (-> comment
                (assoc :staleness (check-staleness comment repo-path))
                (update :replies #(mapv annotate %))))]
    (mapv annotate comments)))
