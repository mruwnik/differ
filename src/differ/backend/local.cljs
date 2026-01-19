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

(defn- get-line-content
  "Get content of a specific line."
  [repo-path file-path line]
  (when (pos? line)
    (let [full-path (path/join repo-path file-path)]
      (try
        (let [content (fs/readFileSync full-path "utf8")
              lines (str/split-lines content)]
          (get lines (dec line)))
        (catch :default _ nil)))))

(defn- compute-line-hash
  "Compute hash of line content for staleness detection."
  [repo-path file line]
  (let [content (get-line-content repo-path file line)]
    (util/sha256-hex (or content ""))))

(defn- get-lines-range
  "Get a range of lines from file."
  [repo-path file-path from-line to-line]
  (when (and (pos? from-line) (pos? to-line) (<= from-line to-line))
    (let [full-path (path/join repo-path file-path)]
      (try
        (let [content (fs/readFileSync full-path "utf8")
              lines (str/split-lines content)]
          (->> (range (dec from-line) (min to-line (count lines)))
               (mapv (fn [idx]
                       {:line (inc idx)
                        :content (get lines idx)}))))
        (catch :default _ nil)))))

(defn- find-shifted-line
  "Search nearby lines for matching content hash."
  [repo-path file original-line target-hash search-range]
  (let [from-line (max 1 (- original-line search-range))
        to-line (+ original-line search-range)
        lines (get-lines-range repo-path file from-line to-line)]
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
  "Check if a comment's line has changed."
  [comment repo-path]
  (let [{:keys [file line line-content-hash]} comment]
    (if-not (and file line line-content-hash)
      nil
      (let [current-hash (compute-line-hash repo-path file line)]
        (cond
          (= current-hash line-content-hash)
          :fresh

          :else
          (if-let [new-line (find-shifted-line repo-path file line line-content-hash 50)]
            {:status :shifted :shifted-to new-line}
            :changed))))))

(defn- annotate-comments-with-staleness
  "Add :staleness key to each comment."
  [comments repo-path]
  (letfn [(annotate [comment]
            (-> comment
                (assoc :staleness (check-staleness comment repo-path))
                (update :replies #(mapv annotate %))))]
    (mapv annotate comments)))

;; LocalBackend record

(defrecord LocalBackend [repo-path target-branch session-id-str]
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
     (let [diff (exec-git repo-path "diff" target-branch)]
       (if opts
         (proto/extract-lines diff opts)
         diff))))

  (get-file-diff
    [this path] (proto/get-file-diff this path nil))
  (get-file-diff
    [_ file-path opts]
    (js/Promise.resolve
     (let [diff (exec-git repo-path "diff" target-branch "--" file-path)]
       (if opts
         (proto/extract-lines diff opts)
         diff))))

  (get-changed-files [_]
    (js/Promise.resolve
     (if-let [output (exec-git repo-path "diff" "--name-status" target-branch)]
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
     (let [;; Interpret 'base' as target-branch, 'head' as current
           effective-ref (case ref
                           "base" target-branch
                           "head" nil
                           ref)
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
     (let [effective-ref (case ref
                           "base" target-branch
                           "head" nil
                           ref)]
       (if effective-ref
         ;; Use git ls-tree for ref
         (if-let [output (exec-git repo-path "ls-tree" effective-ref (or dir-path ""))]
           (->> (str/split-lines output)
                (filter seq)
                (map (fn [line]
                       (let [[_ type _ name] (re-find #"^\d+ (\w+) [a-f0-9]+\t(.+)$" line)]
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
     (let [effective-ref (case ref
                           "base" target-branch
                           "head" nil
                           ref)]
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
     (let [comments (db/list-comments (proto/session-id this))]
       (annotate-comments-with-staleness
        (schema/build-threads comments)
        repo-path))))

  (get-pending-comments [this opts]
    (js/Promise.resolve
     (let [{:keys [since]} opts
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
        repo-path))))

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
           ;; Compute line hash for staleness detection
           line-hash (when (and file line)
                       (compute-line-hash repo-path file line))]
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
                          :status (if (:created result) :created :existing)})))))))))

;; Constructor

(defn create-local-backend
  "Create a LocalBackend for a local directory.
   repo-path: absolute path to the directory
   target-branch: branch to diff against (default: auto-detect)"
  [repo-path & [target-branch]]
  (let [resolved-path (path/resolve repo-path)
        branch (or target-branch
                   (when (git-repo? resolved-path)
                     (detect-default-branch resolved-path))
                   "main")
        project (get-project-id resolved-path)
        current-branch (get-current-branch resolved-path)
        session-id (str "local:" (util/session-id project current-branch))]
    (->LocalBackend resolved-path branch session-id)))
