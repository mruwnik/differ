(ns differ.backend.local
  "Local directory backend implementation of ReviewBackend protocol.
   Handles both git repositories and plain directories."
  (:require [differ.backend.protocol :as proto]
            [differ.db :as db]
            [differ.schema :as schema]
            [differ.util :as util]
            [differ.pull-request :as pr]
            ["child_process" :as cp]
            ["path" :as path]
            ["fs" :as fs]
            [clojure.string :as str]))

;; Git command execution

(defn- exec-git
  "Execute git command in given directory.
   Uses spawnSync with args array to avoid command injection."
  [repo-path & args]
  (try
    (let [result (cp/spawnSync "git" (clj->js (vec args))
                               #js {:cwd repo-path
                                    :encoding "utf8"
                                    :maxBuffer (* 50 1024 1024)})]  ; 50MB buffer
      (when (zero? (.-status result))
        (str/trim (.-stdout result))))
    (catch :default e
      (js/console.warn "[local] Git command failed:" (pr-str args) (.-message e))
      nil)))

;; Git helpers

(defn git-repo?
  "Check if directory is a git repository."
  [dir]
  (some? (exec-git dir "rev-parse" "--git-dir")))

(defn get-remote-url
  "Get the remote origin URL, or nil if not set."
  [repo-path]
  (exec-git repo-path "remote" "get-url" "origin"))

(defn get-project-id
  "Get project identifier: remote URL if available, else directory name."
  [repo-path]
  (or (get-remote-url repo-path)
      (path/basename (path/resolve repo-path))))

(defn get-current-branch
  "Get the current branch name, or 'working' if not in a git repo."
  [repo-path]
  (or (exec-git repo-path "rev-parse" "--abbrev-ref" "HEAD")
      "working"))

(defn detect-default-branch
  "Detect the default branch (main or master)."
  [repo-path]
  (let [remote-head (exec-git repo-path "symbolic-ref" "refs/remotes/origin/HEAD" "--short")
        remote-branch (when remote-head
                        (last (str/split remote-head #"/")))]
    (or remote-branch
        (when (exec-git repo-path "rev-parse" "--verify" "main") "main")
        (when (exec-git repo-path "rev-parse" "--verify" "master") "master")
        "main")))

(defn list-branches
  "List all local branches."
  [repo-path]
  (if-let [output (exec-git repo-path "branch" "--format" "%(refname:short)")]
    (->> (str/split-lines output)
         (filter seq)
         sort
         vec)
    []))

;; Comment staleness helpers

(defn- git-show-file
  "Return the raw, UNTRIMMED content of file-path at a git ref, or nil when the
   file does not exist at that ref. Distinct from exec-git, which trims: leading
   blank lines and boundary whitespace must survive so line offsets line up for
   staleness matching."
  [repo-path ref file-path]
  (let [result (cp/spawnSync "git" #js ["show" (str ref ":" file-path)]
                             #js {:cwd repo-path
                                  :encoding "utf8"
                                  :maxBuffer (* 50 1024 1024)})]
    (when (zero? (.-status result))
      (.-stdout result))))

(defn- read-file-lines
  "Read file-path's lines at `source` (a git ref), or from repo-path's working
   tree when source is nil. Returns a vector of lines, or nil when the file
   can't be read — missing on disk, or absent from the ref (file-not-in-branch)."
  [repo-path source file-path]
  (let [content (if source
                  (git-show-file repo-path source file-path)
                  (let [full-path (path/join repo-path file-path)]
                    (try
                      (fs/readFileSync full-path "utf8")
                      (catch :default _ nil))))]
    (when content
      (str/split-lines content))))

(defn- get-line-content
  "Get content of a specific line at `source` (git ref) or the working tree."
  [repo-path source file-path line]
  (when (pos? line)
    (when-let [lines (read-file-lines repo-path source file-path)]
      (get lines (dec line)))))

(defn- compute-line-hash
  "Compute hash of line content for staleness detection, reading from `source`
   (git ref) or the working tree when source is nil."
  [repo-path source file line]
  (let [content (get-line-content repo-path source file line)]
    (util/sha256-hex (or content ""))))

(defn- get-lines-range
  "Get a range of lines from file-path at `source` (git ref) or the working tree."
  [repo-path source file-path from-line to-line]
  (when (and (pos? from-line) (pos? to-line) (<= from-line to-line))
    (when-let [lines (read-file-lines repo-path source file-path)]
      (->> (range (dec from-line) (min to-line (count lines)))
           (mapv (fn [idx]
                   {:line (inc idx)
                    :content (get lines idx)}))))))

(defn- find-shifted-line
  "Search nearby lines for matching content hash."
  [repo-path source file original-line target-hash search-range]
  (let [from-line (max 1 (- original-line search-range))
        to-line (+ original-line search-range)
        lines (get-lines-range repo-path source file from-line to-line)]
    (when lines
      (->> lines
           (map (fn [{:keys [line content]}]
                  {:line line
                   :hash (util/sha256-hex (or content ""))
                   :distance (js/Math.abs (- line original-line))}))
           (filter #(= (:hash %) target-hash))
           (filter #(not= (:line %) original-line))
           (sort-by :distance)
           first
           :line))))

(defn- check-staleness
  "Check if a comment's line has changed, reading current content from `source`
   (the session branch tip) or the working tree when source is nil."
  [comment repo-path source]
  (let [{:keys [file line line-content-hash]} comment]
    (if-not (and file line line-content-hash)
      nil
      (let [current-hash (compute-line-hash repo-path source file line)]
        (cond
          (= current-hash line-content-hash)
          :fresh

          :else
          (if-let [new-line (find-shifted-line repo-path source file line line-content-hash 50)]
            {:status :shifted :shifted-to new-line}
            :changed))))))

(defn- annotate-comments-with-staleness
  "Add :staleness key to each comment. `source` is the git ref to read current
   content from (nil = working tree)."
  [comments repo-path source]
  (letfn [(annotate [comment]
            (-> comment
                (assoc :staleness (check-staleness comment repo-path source))
                (update :replies #(mapv annotate %))))]
    (mapv annotate comments)))

;; LocalBackend record

(defn- source-branch-arg
  "Decide whether to diff against an explicit source branch or fall back to a
   working-tree diff.

   Returns the session's `branch` when it is set and differs from the branch
   currently checked out at `repo-path` — the worktree case, where the session
   branch lives in another working tree and is NOT reachable from repo-path's
   HEAD. Callers then diff three-dot (`target...branch`).

   Returns nil when `branch` is blank/nil or already equals the checked-out
   HEAD, so callers do a plain `git diff <target>` working-tree diff. This
   preserves the original same-branch behavior (uncommitted changes included)."
  [repo-path branch]
  (when (and (not (str/blank? branch))
             (not= branch (get-current-branch repo-path)))
    branch))

(defn- resolve-effective-ref
  "Resolve a caller-facing ref to a concrete git ref, or nil meaning 'read
   repo-path's working tree'. Shared by get-file-content, file-exists?, and
   list-directory so all three agree on ref semantics:

   - 'base' -> the before-side of the review diff. In the worktree case the diff
     is three-dot (target...source), whose before-side is the merge-base of
     target and source — NOT target's tip — so read that, staying correct once
     target advances past the merge-base. Falls back to target-branch when there
     is no source (same-branch/working-tree diff, whose base IS target's tip) or
     when merge-base is blank (e.g. unrelated histories).
   - 'head'/nil -> the session branch when it isn't the checked-out HEAD (the
     worktree case: read the branch tip), else nil to read the working tree.
   - any other ref (SHA, branch name) is returned unchanged."
  [repo-path target-branch branch ref]
  (let [source (source-branch-arg repo-path branch)]
    (case ref
      "base" (or (when source
                   (let [mb (exec-git repo-path "merge-base" target-branch source)]
                     (when-not (str/blank? mb) mb)))
                 target-branch)
      "head" source
      nil source
      ref)))

(defrecord LocalBackend [repo-path target-branch session-id-str branch]
  proto/ReviewBackend

  (session-id [_]
    session-id-str)

  (session-type [_]
    :local)

  (session-descriptor [_]
    {:type :local
     :repo-path repo-path
     :target-branch target-branch})

  (get-context [_]
    (js/Promise.resolve
     {:type :local
      :path repo-path
      :branch (get-current-branch repo-path)
      :target-branch target-branch
      :is-git-repo (git-repo? repo-path)
      :project (get-project-id repo-path)}))

  (get-diff
    [this] (proto/get-diff this nil))
  (get-diff
    [_ opts]
    (js/Promise.resolve
     (let [source (source-branch-arg repo-path branch)
           diff (if source
                  (exec-git repo-path "diff" (str target-branch "..." source))
                  (exec-git repo-path "diff" target-branch))]
       (if opts
         (proto/extract-lines diff opts)
         diff))))

  (get-file-diff
    [this path] (proto/get-file-diff this path nil))
  (get-file-diff
    [_ file-path opts]
    (js/Promise.resolve
     (let [source (source-branch-arg repo-path branch)
           diff (if source
                  (exec-git repo-path "diff" (str target-branch "..." source) "--" file-path)
                  (exec-git repo-path "diff" target-branch "--" file-path))]
       (if opts
         (proto/extract-lines diff opts)
         diff))))

  (get-changed-files [_]
    (js/Promise.resolve
     (if-let [output (let [source (source-branch-arg repo-path branch)]
                       (if source
                         (exec-git repo-path "diff" "--name-status" (str target-branch "..." source))
                         (exec-git repo-path "diff" "--name-status" target-branch)))]
       (->> (str/split-lines output)
            (filter seq)
            (map (fn [line]
                   (let [[status & path-parts] (str/split line #"\t")
                         path (str/join "\t" path-parts)]
                     {:path path
                      :status (case (first status)
                                \A :added
                                \M :modified
                                \D :deleted
                                \R :renamed
                                :modified)}))))
       [])))

  (get-file-content
    [this ref file-path] (proto/get-file-content this ref file-path nil))
  (get-file-content
    [_ ref file-path opts]
    (js/Promise.resolve
     (let [effective-ref (resolve-effective-ref repo-path target-branch branch ref)
           content (if effective-ref
                     (exec-git repo-path "show" (str effective-ref ":" file-path))
                     (let [full-path (path/join repo-path file-path)]
                       (try
                         (fs/readFileSync full-path "utf8")
                         (catch :default _ nil))))]
       (if opts
         (proto/extract-lines content opts)
         content))))

  (list-directory [_ ref dir-path]
    (js/Promise.resolve
     (let [effective-ref (resolve-effective-ref repo-path target-branch branch ref)]
       (if effective-ref
         ;; Use git ls-tree for ref. Omit the pathspec entirely for the root:
         ;; git rejects an empty-string pathspec ("empty string is not a valid
         ;; pathspec"), which would otherwise return nil (empty listing).
         (if-let [output (if (str/blank? dir-path)
                           (exec-git repo-path "ls-tree" effective-ref)
                           (exec-git repo-path "ls-tree" effective-ref dir-path))]
           (->> (str/split-lines output)
                (filter seq)
                (map (fn [line]
                       ;; re-find returns [whole type name] — the pattern has two
                       ;; capture groups, so name is the third element.
                       (let [[_ type name] (re-find #"^\d+ (\w+) [a-f0-9]+\t(.+)$" line)]
                         {:name (path/basename name)
                          :path name
                          :type (if (= type "tree") :dir :file)
                          :size nil}))))
           [])
         ;; Use fs for working tree
         (let [full-path (path/join repo-path (or dir-path ""))]
           (try
             (->> (fs/readdirSync full-path)
                  (map (fn [name]
                         (let [entry-path (path/join full-path name)
                               stats (fs/statSync entry-path)
                               is-dir (.isDirectory stats)]
                           {:name name
                            :path (if dir-path (str dir-path "/" name) name)
                            :type (if is-dir :dir :file)
                            :size (when-not is-dir (.-size stats))}))))
             (catch :default _ [])))))))

  (file-exists? [_ ref file-path]
    (js/Promise.resolve
     (let [effective-ref (resolve-effective-ref repo-path target-branch branch ref)]
       (if effective-ref
         (some? (exec-git repo-path "cat-file" "-e" (str effective-ref ":" file-path)))
         (try
           (fs/statSync (path/join repo-path file-path))
           true
           (catch :default _ false))))))

  (get-history [_ opts]
    (js/Promise.resolve
     (let [{:keys [path limit since]} opts
           limit (or limit 50)
           args (cond-> ["log" (str "--max-count=" limit) "--format=%H|%s|%an|%aI"]
                  path (conj "--" path)
                  since (conj (str "--since=" since)))]
       (if-let [output (apply exec-git repo-path args)]
         (->> (str/split-lines output)
              (filter seq)
              (map (fn [line]
                     (let [[sha message author date] (str/split line #"\|" 4)]
                       {:sha sha
                        :message message
                        :author author
                        :date date}))))
         []))))

  (get-branches [_]
    (js/Promise.resolve
     (->> (list-branches repo-path)
          (map (fn [name] {:name name :type :branch})))))

  ;; Comments - delegate to db/comments module
  (get-comments [this]
    (js/Promise.resolve
     (let [source (source-branch-arg repo-path branch)
           comments (db/list-comments (proto/session-id this))]
       (annotate-comments-with-staleness
        (schema/build-threads comments)
        repo-path source))))

  (get-pending-comments [this opts]
    (js/Promise.resolve
     (let [source (source-branch-arg repo-path branch)
           {:keys [since]} opts
           comments (db/list-unresolved-comments (proto/session-id this) since)
           all-comments (db/list-comments (proto/session-id this))
           unresolved-ids (set (map :id comments))
           replies-to-unresolved (filter
                                  (fn [c]
                                    (and (:parent-id c)
                                         (contains? unresolved-ids (:parent-id c))))
                                  all-comments)
           relevant-comments (concat comments replies-to-unresolved)]
       (annotate-comments-with-staleness
        (schema/build-threads relevant-comments)
        repo-path source))))

  (add-comment! [this comment]
    (js/Promise.resolve
     (let [{:keys [file line side text author parent-id line-content context-before context-after]} comment
           parent (when parent-id (db/get-comment parent-id))
           ;; Inherit file/line/side from parent if not provided
           file (or file (when parent (:file parent)))
           line (or line (when parent (:line parent)))
           side (or side (when parent (:side parent)) "new")
           ;; For replies, inherit content context from parent if not provided
           line-content (or line-content (when parent (:line-content parent)))
           context-before (or context-before (when parent (:context-before parent)))
           context-after (or context-after (when parent (:context-after parent)))
           ;; Compute line hash for staleness detection, against the same ref
           ;; staleness will later read from (session branch tip, or working
           ;; tree for same-branch sessions) so the two always agree.
           source (source-branch-arg repo-path branch)
           line-hash (when (and file line)
                       (compute-line-hash repo-path source file line))]
       (db/create-comment!
        {:session-id (proto/session-id this)
         :parent-id parent-id
         :file file
         :line line
         :line-content-hash line-hash
         :side side
         :line-content line-content
         :context-before context-before
         :context-after context-after
         :text text
         :author author}))))

  (resolve-comment! [_ comment-id _author]
    (js/Promise.resolve
     (db/resolve-comment! comment-id)))

  (unresolve-comment! [_ comment-id _author]
    (js/Promise.resolve
     (db/unresolve-comment! comment-id)))

  (submit-review! [this opts]
    (js/Promise.resolve
     (let [{:keys [body author]} opts]
       (when (seq body)
         (db/create-comment!
          {:session-id (proto/session-id this)
           :text body
           :author (or author "reviewer")}))
       {:submitted true})))

  (request-review! [_ opts]
    (-> (pr/create-pull-request! {:repo-path repo-path
                                  :title (:title opts)
                                  :body (:body opts)
                                  :draft (:draft opts)})
        (.then (fn [result]
                 (if (:error result)
                   (throw (ex-info (:error result)
                                   {:code (or (:code result) :unknown)}))
                   (let [{:keys [pr-url pr-number pr-state]} result
                         ;; Parse owner/repo from the PR URL
                         parsed (re-find #"github\.com/([^/]+)/([^/]+)/pull" pr-url)]
                     (if-not parsed
                       (throw (ex-info (str "Invalid PR URL format: " pr-url)
                                       {:code :invalid-pr-url :pr-url pr-url}))
                       (let [[_ owner repo] parsed
                             github-session-id (str "github:" owner "/" repo ":" pr-number)]
                         {:review-url pr-url
                          :review-session-id github-session-id
                          :state (pr/normalize-pr-state pr-state)
                          :status (if (:created result) :created :existing)}))))))))

  (get-ci-status [_]
    ;; Local sessions don't have CI - return unknown
    (js/Promise.resolve {:state :unknown :checks []})))

;; Constructor

(defn create-local-backend
  "Create a LocalBackend for a local directory.
   repo-path:     absolute path to the directory
   target-branch: branch to diff against (default: auto-detect)
   session-id:    the session's stored id, stored verbatim. When omitted it is
                  recomputed from project + the checked-out branch — only safe
                  when repo-path's HEAD IS the session branch. Callers that own
                  a session row MUST pass its id so the backend never drifts to
                  a phantom id (see differ.sessions/create-backend).
   branch:        the session's source branch, stored verbatim and used for
                  branch-aware diffs/file reads. nil = diff the working tree."
  [repo-path & [target-branch session-id branch]]
  (let [resolved-path (path/resolve repo-path)
        target (or target-branch
                   (when (git-repo? resolved-path)
                     (detect-default-branch resolved-path))
                   "main")
        sid (or session-id
                (let [project (get-project-id resolved-path)
                      current-branch (get-current-branch resolved-path)]
                  (str "local:" (util/session-id project current-branch))))]
    (->LocalBackend resolved-path target sid branch)))
