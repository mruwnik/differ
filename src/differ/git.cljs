(ns differ.git
  "Git operations - thin wrapper delegating to local backend.
   This module exists for backward compatibility.
   New code should use differ.backend.local or the ReviewBackend protocol."
  (:require [differ.backend.local :as local]
            ["child_process" :as cp]
            ["path" :as path]
            ["fs" :as fs]
            [clojure.string :as str]))

;; Re-export commonly used functions from local backend
(def git-repo? local/git-repo?)
(def get-remote-url local/get-remote-url)
(def get-project-id local/get-project-id)
(def get-current-branch local/get-current-branch)
(def detect-default-branch local/detect-default-branch)
(def list-branches local/list-branches)
(def parse-diff-hunks local/parse-diff-hunks)

;; These functions need repo-path + target-branch, so they call git directly
;; using the local backend's internal functions

(defn- exec-sync
  "Execute command synchronously, return stdout or nil on error."
  [cmd opts]
  (try
    (let [result (cp/execSync cmd (clj->js (merge {:encoding "utf8"} opts)))]
      (str/trim result))
    (catch :default e
      (js/console.warn "[git] Command failed:" cmd (.-message e))
      nil)))

(defn- exec-git
  "Execute git command in given directory."
  [repo-path & args]
  (let [cmd (str "git " (str/join " " args))]
    (exec-sync cmd {:cwd repo-path})))

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
  "Get the merge base between source and target branch.
   If source-branch is nil, uses HEAD (working tree)."
  [repo-path target-branch & [source-branch]]
  (exec-git repo-path "merge-base" target-branch (or source-branch "HEAD")))

(defn get-diff
  "Get unified diff between target branch and source branch.
   If source-branch is nil, compares against working tree.
   Uses three-dot diff (A...B) for branch comparison to show changes
   introduced on source-branch since it diverged from target-branch."
  [repo-path target-branch & [source-branch]]
  (if source-branch
    (exec-git repo-path "diff" (str target-branch "..." source-branch))
    (exec-git repo-path "diff" target-branch)))

(defn get-diff-stat
  "Get diff statistics (files changed, insertions, deletions).
   If source-branch is nil, compares against working tree.
   Uses three-dot diff for branch comparison."
  [repo-path target-branch & [source-branch]]
  (if source-branch
    (exec-git repo-path "diff" "--stat" (str target-branch "..." source-branch))
    (exec-git repo-path "diff" "--stat" target-branch)))

(defn get-changed-files
  "Get list of files that changed relative to target branch.
   If source-branch is nil, compares against working tree.
   Uses three-dot diff for branch comparison."
  [repo-path target-branch & [source-branch]]
  (let [output (if source-branch
                 (exec-git repo-path "diff" "--name-status" (str target-branch "..." source-branch))
                 (exec-git repo-path "diff" "--name-status" target-branch))]
    (if output
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

(defn get-file-diff
  "Get diff for a specific file.
   If source-branch is nil, compares against working tree.
   Uses three-dot diff for branch comparison."
  [repo-path target-branch file-path & [source-branch]]
  (if source-branch
    (exec-git repo-path "diff" (str target-branch "..." source-branch) "--" file-path)
    (exec-git repo-path "diff" target-branch "--" file-path)))

(defn get-file-content
  "Get file content at a specific ref (or working tree if ref is nil)."
  [repo-path ref file-path]
  (if ref
    (exec-git repo-path "show" (str ref ":" file-path))
    (let [full-path (path/join repo-path file-path)]
      (try
        (fs/readFileSync full-path "utf8")
        (catch :default _ nil)))))

(defn get-line-content
  "Get content of a specific line in working tree."
  [repo-path file-path line]
  (when (pos? line)
    (let [content (get-file-content repo-path nil file-path)]
      (when content
        (let [lines (str/split-lines content)]
          (get lines (dec line)))))))

(defn get-lines-range
  "Get a range of lines from working tree."
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
      (fs/statSync full-path)
      true
      (catch :default _ false))))

(defn get-file-info
  "Get file size and optionally content from working tree."
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
  "Convert a full file into a diff-like format (all lines as additions)."
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
