(ns differ.git
  "Git operations - shells out to git commands."
  (:require ["child_process" :as cp]
            ["path" :as path]
            ["fs" :as fs]
            [clojure.string :as str]))

(defn- exec-sync
  "Execute command synchronously, return stdout or nil on error."
  [cmd opts]
  (try
    (let [result (cp/execSync cmd (clj->js (merge {:encoding "utf8"} opts)))]
      (str/trim result))
    (catch :default _e
      nil)))

(defn- exec-git
  "Execute git command in given directory."
  [repo-path & args]
  (let [cmd (str "git " (str/join " " args))]
    (exec-sync cmd {:cwd repo-path})))

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
  (let [;; Try to get from remote
        remote-head (exec-git repo-path "symbolic-ref" "refs/remotes/origin/HEAD" "--short")
        ;; Extract branch name from "origin/main" format
        remote-branch (when remote-head
                        (last (str/split remote-head #"/")))]
    (or remote-branch
        ;; Check if main exists
        (when (exec-git repo-path "rev-parse" "--verify" "main") "main")
        ;; Check if master exists
        (when (exec-git repo-path "rev-parse" "--verify" "master") "master")
        ;; Fall back to main
        "main")))

(defn list-branches
  "List all local branches."
  [repo-path]
  (if-let [output (exec-git repo-path "branch" "--format" "'%(refname:short)'")]
    (->> (str/split-lines output)
         (map #(str/replace % #"^'|'$" ""))  ;; Remove surrounding quotes
         (filter seq)
         sort
         vec)
    []))

(defn get-staged-files
  "Get list of files that are staged for commit."
  [repo-path]
  (if-let [output (exec-git repo-path "diff" "--cached" "--name-only")]
    (set (filter seq (str/split-lines output)))
    #{}))

(defn get-unstaged-files
  "Get list of files with unstaged modifications."
  [repo-path]
  (if-let [output (exec-git repo-path "diff" "--name-only")]
    (set (filter seq (str/split-lines output)))
    #{}))

(defn stage-file!
  "Stage a file for commit (git add)."
  [repo-path file-path]
  (exec-git repo-path "add" (str "\"" file-path "\"")))

(defn branch-exists?
  "Check if a branch exists."
  [repo-path branch]
  (some? (exec-git repo-path "rev-parse" "--verify" branch)))

(defn get-merge-base
  "Get the merge base between current HEAD and target branch."
  [repo-path target-branch]
  (exec-git repo-path "merge-base" target-branch "HEAD"))

(defn get-diff
  "Get unified diff between target branch and working tree.
   Returns raw diff string."
  [repo-path target-branch]
  ;; Get diff of staged + unstaged changes relative to target branch
  (exec-git repo-path "diff" target-branch))

(defn get-diff-stat
  "Get diff statistics (files changed, insertions, deletions)."
  [repo-path target-branch]
  (exec-git repo-path "diff" "--stat" target-branch))

(defn get-changed-files
  "Get list of files that changed relative to target branch.
   Returns list of {:path :status} maps, or empty list if not in git repo.
   Status is :added, :modified, :deleted, or :renamed."
  [repo-path target-branch]
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
    []))

(defn get-file-diff
  "Get diff for a specific file."
  [repo-path target-branch file-path]
  (exec-git repo-path "diff" target-branch "--" file-path))

(defn get-file-content
  "Get file content at a specific ref (or working tree if ref is nil)."
  [repo-path ref file-path]
  (if ref
    (exec-git repo-path "show" (str ref ":" file-path))
    (exec-sync (str "cat \"" file-path "\"") {:cwd repo-path})))

(defn get-line-content
  "Get content of a specific line in working tree.
   Line is 1-indexed."
  [repo-path file-path line]
  (when (pos? line)
    (let [content (get-file-content repo-path nil file-path)]
      (when content
        (let [lines (str/split-lines content)]
          (get lines (dec line)))))))

(defn get-lines-range
  "Get a range of lines from working tree.
   Lines are 1-indexed, inclusive on both ends.
   Returns vector of {:line <num> :content <string>}."
  [repo-path file-path from-line to-line]
  (when (and (pos? from-line) (pos? to-line) (<= from-line to-line))
    (let [content (get-file-content repo-path nil file-path)]
      (when content
        (let [lines (str/split-lines content)]
          (->> (range (dec from-line) (min to-line (count lines)))
               (mapv (fn [idx]
                       {:line (inc idx)
                        :content (get lines idx)}))))))))

(defn get-untracked-files
  "Get list of untracked files."
  [repo-path]
  (when-let [output (exec-git repo-path "ls-files" "--others" "--exclude-standard")]
    (->> (str/split-lines output)
         (filter seq))))

(defn file-exists-in-working-tree?
  "Check if file exists in working tree."
  [repo-path file-path]
  (let [full-path (path/join repo-path file-path)]
    (try
      (cp/execSync (str "test -f \"" full-path "\""))
      true
      (catch :default _
        false))))

(defn parse-diff-hunks
  "Parse unified diff into structured format.
   Returns vector of file diffs, each containing:
   {:file-a :file-b :hunks [{:old-start :old-count :new-start :new-count :lines [...]}]}"
  [diff-text]
  (when (and diff-text (seq diff-text))
    (let [lines (str/split-lines diff-text)
          ;; Split into file sections
          file-sections (reduce
                         (fn [acc line]
                           (if (str/starts-with? line "diff --git")
                             (conj acc [line])
                             (if (seq acc)
                               (update acc (dec (count acc)) conj line)
                               acc)))
                         []
                         lines)]
      (mapv
       (fn [section]
         (let [;; Parse file header
               diff-line (first section)
               [_ file-a file-b] (re-find #"diff --git a/(.+) b/(.+)" diff-line)
                ;; Find hunks
               hunk-lines (drop-while #(not (str/starts-with? % "@@")) section)
                ;; Parse hunks
               hunks (loop [remaining hunk-lines
                            result []]
                       (if (empty? remaining)
                         result
                         (let [hunk-header (first remaining)
                               [_ old-start old-count new-start new-count]
                               (re-find #"@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@" hunk-header)
                                ;; Collect lines until next hunk or end
                               hunk-content (take-while #(not (str/starts-with? % "@@"))
                                                        (rest remaining))
                               hunk {:old-start (js/parseInt old-start)
                                     :old-count (js/parseInt (or old-count "1"))
                                     :new-start (js/parseInt new-start)
                                     :new-count (js/parseInt (or new-count "1"))
                                     :header hunk-header
                                     :lines (vec hunk-content)}]
                           (recur (drop (inc (count hunk-content)) remaining)
                                  (conj result hunk)))))]
           {:file-a file-a
            :file-b file-b
            :hunks hunks}))
       file-sections))))

(defn get-file-info
  "Get file size and optionally content from working tree.
   Returns {:size :content :error} - content only included if include-content? is true."
  [repo-path file-path include-content?]
  (let [full-path (path/join repo-path file-path)]
    (try
      (let [stats (fs/statSync full-path)
            size (.-size stats)]
        (if include-content?
          (let [content (fs/readFileSync full-path "utf8")]
            {:size size :content content})
          {:size size}))
      (catch :default e
        {:error (.-message e)}))))

(defn file-to-diff-format
  "Convert a full file into a diff-like format (all lines as additions).
   Returns structure compatible with parse-diff-hunks output."
  [file-path content]
  (let [lines (str/split-lines (or content ""))
        line-count (count lines)]
    {:file-a file-path
     :file-b file-path
     :is-full-file true
     :hunks [{:old-start 0
              :old-count 0
              :new-start 1
              :new-count line-count
              :header (str "@@ -0,0 +1," line-count " @@ (full file)")
              :lines (mapv #(str "+" %) lines)}]}))
