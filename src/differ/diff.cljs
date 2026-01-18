(ns differ.diff
  "Diff computation logic.

   Provides functions to compute diff data for sessions, handling both
   git-tracked changes and untracked files."
  (:require [differ.git :as git]
            [differ.sessions :as sessions]
            [differ.config :as config]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn default-max-file-size
  "Files larger than this (in bytes) won't have content loaded initially.
   Uses :large-file-threshold from config."
  []
  (config/get-value :large-file-threshold))

;; ============================================================================
;; File Info Helpers
;; ============================================================================

(defn- get-file-size
  "Get file size, or nil if file doesn't exist."
  [repo-path file-path]
  (let [info (git/get-file-info repo-path file-path false)]
    (when-not (:error info)
      (:size info))))

(defn get-files-with-size
  "Get file sizes for all files in the list.
   Returns vector of {:path :size} maps."
  [repo-path files]
  (->> files
       (map (fn [f] {:path f :size (get-file-size repo-path f)}))
       (filter :size)
       vec))

;; ============================================================================
;; Diff Content Helpers
;; ============================================================================

(defn- file-content-as-diff
  "Read file content and convert to diff format (all lines as additions).
   Returns nil if file is too large or can't be read."
  [repo-path file-path max-size]
  (let [info (git/get-file-info repo-path file-path true)]
    (when (and (not (:error info))
               (<= (:size info) max-size))
      (git/file-to-diff-format file-path (:content info)))))

(defn- collect-file-diffs
  "Collect diff content for multiple files.
   Skips files that are too large or can't be read."
  [repo-path files max-size]
  (->> files
       (keep #(file-content-as-diff repo-path % max-size))
       vec))

;; ============================================================================
;; Changed Files Computation
;; ============================================================================

(defn- mark-as-untracked
  "Convert file paths to changed-file format with :untracked status."
  [file-paths]
  (map (fn [f] {:path f :status :untracked}) file-paths))

(defn- find-untracked-in-review
  "Find files that are both in the review set and untracked by git."
  [files-in-review untracked-files]
  (set/intersection (set files-in-review) (set untracked-files)))

;; ============================================================================
;; Main Diff Computation
;; ============================================================================

(defn- compute-git-diff-data
  "Compute diff data for a git repository.
   source-branch is optional - if nil, compares against working tree."
  [repo-path target-branch source-branch files untracked-files max-size]
  (let [;; Get the raw diff and parse it
        raw-diff (git/get-diff repo-path target-branch source-branch)
        git-parsed (git/parse-diff-hunks raw-diff)
        git-changed-files (git/get-changed-files repo-path target-branch source-branch)

        ;; Untracked files only matter when comparing against working tree
        ;; (when source-branch is nil)
        untracked-in-review (if source-branch
                              #{}
                              (find-untracked-in-review files untracked-files))

        ;; Get content for untracked files (they won't be in git diff)
        untracked-parsed (collect-file-diffs repo-path untracked-in-review max-size)

        ;; Combine everything
        all-parsed (concat git-parsed untracked-parsed)
        all-changed (concat git-changed-files (mark-as-untracked untracked-in-review))]

    {:diff raw-diff
     :parsed all-parsed
     :changed-files all-changed}))

(defn- compute-non-git-diff-data
  "Compute diff data for a non-git directory (shows full file contents)."
  [repo-path files max-size]
  {:diff nil
   :parsed (collect-file-diffs repo-path files max-size)
   :changed-files []})

(defn get-diff-data
  "Compute complete diff data for a session.

   Returns a map with:
   - :diff          - raw diff string (nil for non-git)
   - :parsed        - parsed diff structures for rendering
   - :files         - list of files in review
   - :files-with-size - files with their sizes
   - :changed-files - files with change status (:modified, :added, :deleted, :untracked)
   - :is-git-repo   - whether this is a git repository

   Options:
   - :max-size - max file size to load content for (uses config :large-file-threshold)"
  [session & {:keys [max-size]}]
  (let [max-size (or max-size (default-max-file-size))
        repo-path (:repo-path session)
        target-branch (:target-branch session)
        ;; source-branch nil means compare against working tree
        ;; Trim whitespace to handle accidental spaces in branch names
        source-branch (not-empty (some-> (:branch session) str/trim))
        is-git-repo (git/git-repo? repo-path)

        ;; Get git info (needed for computing file set and diff)
        git-changed (when is-git-repo
                      (git/get-changed-files repo-path target-branch source-branch))
        untracked (when is-git-repo
                    (git/get-untracked-files repo-path))

        ;; Compute which files are in review
        files (sessions/compute-file-set session (or git-changed []) (or untracked []))
        files-with-size (get-files-with-size repo-path files)

        ;; Compute diff data based on repo type
        diff-data (if is-git-repo
                    (compute-git-diff-data repo-path target-branch source-branch files untracked max-size)
                    (compute-non-git-diff-data repo-path files max-size))]

    (merge diff-data
           {:files files
            :files-with-size files-with-size
            :is-git-repo is-git-repo})))

(defn get-file-diff-data
  "Get diff data for a single file.

   Returns a map with:
   - :file       - the file path
   - :diff       - raw diff string for the file (nil for non-git or untracked)
   - :parsed     - parsed diff structure
   - :is-git-repo - whether this is a git repository
   - :size       - file size (only for non-git or untracked files)

   Returns nil if file cannot be read."
  [session file-path & {:keys [_max-size]}]
  (let [repo-path (:repo-path session)
        target-branch (:target-branch session)
        ;; Trim whitespace to handle accidental spaces in branch names
        source-branch (not-empty (some-> (:branch session) str/trim))
        is-git-repo (git/git-repo? repo-path)]
    (if is-git-repo
      ;; Try git diff first
      (let [raw-diff (git/get-file-diff repo-path target-branch file-path source-branch)]
        (if (seq raw-diff)
          {:file file-path
           :diff raw-diff
           :parsed (git/parse-diff-hunks raw-diff)
           :is-git-repo true}
          ;; No git diff - file might be untracked, show full content
          (let [info (git/get-file-info repo-path file-path true)]
            (when-not (:error info)
              {:file file-path
               :diff nil
               :parsed [(git/file-to-diff-format file-path (:content info))]
               :size (:size info)
               :is-git-repo true}))))
      ;; Non-git - show full content
      (let [info (git/get-file-info repo-path file-path true)]
        (when-not (:error info)
          {:file file-path
           :diff nil
           :parsed [(git/file-to-diff-format file-path (:content info))]
           :size (:size info)
           :is-git-repo false})))))
