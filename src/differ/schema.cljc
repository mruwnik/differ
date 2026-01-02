(ns differ.schema
  "Data model definitions shared between server and client.")

;; Session represents a review context tied to (project, branch)
;; Sessions are identified deterministically by hashing project + branch

;; Comment represents feedback on a specific line in a file
;; Comments form threads via parent_id (null = top-level)

(def session-keys
  [:id :project :branch :target-branch :registered-files
   :manual-additions :manual-removals :created-at :updated-at])

(def comment-keys
  [:id :session-id :parent-id :file :line :line-content-hash
   :text :author :resolved :created-at :updated-at])

(defn session?
  "Returns true if m looks like a valid session map."
  [m]
  (and (map? m)
       (string? (:id m))
       (string? (:project m))
       (string? (:branch m))))

(defn comment?
  "Returns true if m looks like a valid comment map.
   File and line are optional (nil for session-level comments),
   but must both be present or both be nil."
  [m]
  (and (map? m)
       (string? (:id m))
       (string? (:session-id m))
       ;; file and line are optional but must be correct types if present
       (or (nil? (:file m)) (string? (:file m)))
       (or (nil? (:line m)) (integer? (:line m)))
       ;; Both present or both nil (consistency check)
       (= (some? (:file m)) (some? (:line m)))))

(defn top-level-comment?
  "Returns true if comment has no parent (is a thread root)."
  [comment]
  (nil? (:parent-id comment)))

(defn session-level-comment?
  "Returns true if comment is a session-level comment (no file/line)."
  [comment]
  (nil? (:file comment)))

(defn build-threads
  "Given a flat list of comments, build nested thread structure.
   Returns list of top-level comments with :replies key added."
  [comments]
  (let [by-parent (group-by :parent-id comments)
        add-replies (fn add-replies [comment]
                      (let [replies (get by-parent (:id comment) [])]
                        (assoc comment :replies (mapv add-replies replies))))]
    (->> (get by-parent nil [])
         (mapv add-replies))))
