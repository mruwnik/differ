(ns differ.backend.protocol
  "ReviewBackend protocol - abstraction for local directories and GitHub PRs."
  (:require [clojure.string :as str]))

(defprotocol ReviewBackend
  "Abstraction for review operations across local directories and GitHub PRs.

   All backends must implement this protocol. The MCP tools use this protocol
   so they remain backend-agnostic - callers don't know or care whether they're
   reviewing a local directory or a GitHub PR."

  ;; Identity
  (session-id [this]
    "Returns the session ID for this backend.
     Local: 'local:<sha256(project|branch)>'
     GitHub: 'github:<owner>/<repo>:<pr-number>'")

  (session-type [this]
    "Returns :local or :github")

  (session-descriptor [this]
    "Returns full descriptor to reconstruct this backend.
     Local:  {:type :local :repo-path string :target-branch string}
     GitHub: {:type :github :owner string :repo string :pr-number int}")

  ;; Context - useful info about the session
  (get-context [this]
    "Get session context information.
     Local returns:  {:type :local :path '/abs/path' :branch 'feature'
                      :target-branch 'main' :is-git-repo true/false}
     GitHub returns: {:type :github :owner 'x' :repo 'y' :pr-number 123
                      :title 'PR title' :body 'description' :author 'user'
                      :state 'open' :base-branch 'main' :head-branch 'feature'
                      :base-sha 'abc' :head-sha 'def'}
     Returns a promise.")

  ;; Diff operations
  (get-diff
    [this]
    [this opts]
    "Get unified diff for the session.
     opts: {:from int :to int} for line range (1-indexed, inclusive)
     Returns promise of diff string.")

  (get-file-diff
    [this path]
    [this path opts]
    "Get diff for a specific file.
     opts: {:from int :to int} for line range
     Returns promise of diff string.")

  (get-changed-files [this]
    "Get list of changed files.
     Returns promise of [{:path string :status keyword}]
     where status is :added, :modified, :deleted, or :renamed")

  ;; File operations
  (get-file-content
    [this ref path]
    [this ref path opts]
    "Get file content at a specific ref.
     ref can be:
       - nil (working tree for local, head for GitHub)
       - branch/tag name
       - commit SHA
       - 'base' or 'head' (GitHub PR specific, local interprets as target/current branch)
     opts: {:from int :to int} for line range (1-indexed, inclusive)
     Returns promise of content string.")

  (list-directory [this ref path]
    "List directory contents at ref.
     Returns promise of [{:name string :type (:file | :dir) :path string :size int?}]")

  (file-exists? [this ref path]
    "Check if file exists at ref.
     Returns promise of boolean.")

  ;; History
  (get-history [this opts]
    "Get commit/change history.
     opts: {:path string - filter to path
            :limit int - max entries (default 50)
            :since string - ISO date}
     Returns promise of [{:sha string :message string :author string :date string}]")

  (get-branches [this]
    "List available branches/refs.
     Returns promise of [{:name string :type keyword}]
     For GitHub PRs, returns base and head branches.")

  ;; Comments (abstracted - local=SQLite, GitHub=GitHub API)
  (get-comments [this]
    "Get all comments with threading.
     Returns promise of comment trees.")

  (get-pending-comments [this opts]
    "Get unresolved comments.
     opts: {:since string - ISO datetime, only comments after this}
     Returns promise of comments.")

  (add-comment! [this comment]
    "Add a comment.
     comment: {:file string? :line int? :text string :author string :parent-id string?}
     Local: stores in SQLite
     GitHub: posts to GitHub PR (review comment if file/line, issue comment otherwise)
     Returns promise of created comment.")

  (resolve-comment! [this comment-id author]
    "Mark comment thread as resolved.
     Local: updates SQLite
     GitHub: resolves the review thread via GraphQL
     Returns promise.")

  (unresolve-comment! [this comment-id author]
    "Reopen a resolved comment thread.
     Returns promise.")

  (submit-review! [this opts]
    "Submit/publish a review with pending comments.
     opts: {:body string - optional review summary}
     Local: no-op (comments are already visible)
     GitHub: submits pending review comments as COMMENT (not approve/request-changes)
     Returns promise of {:submitted true} or {:submitted false :reason string}.")

  (request-review! [this opts]
    "Request external review of the current changes.
     Pushes current branch and makes changes available for review by others.

     opts: {:title string - PR title (defaults to last commit message)
            :body string - PR description
            :draft boolean - create as draft PR}

     Returns Promise of:
       {:review-url string - URL where review can be accessed
        :review-session-id string - session ID for collaborative review
        :status :created|:existing}

     Local: pushes branch, creates PR (or finds existing), returns GitHub session ID
     GitHub: pushes branch (if repo-path in opts), returns existing PR info"))

;; Helper for line range extraction
(defn extract-lines
  "Extract lines from content string based on opts.
   opts: {:from int :to int} - 1-indexed, inclusive
   Clamps ranges to file boundaries:
   - from < 1 becomes 1
   - to > line-count becomes line-count
   Returns nil for nil content, nil for from > to (invalid range),
   nil for range entirely beyond file (from > line-count).
   Returns full content if no range specified."
  [content {:keys [from to]}]
  (when content
    (if (or from to)
      (let [lines (str/split-lines content)
            line-count (count lines)]
        (cond
          ;; Empty file: return empty string for any range request
          (zero? line-count) ""
          ;; Invalid range: from > to
          (and from to (> from to)) nil
          ;; Range entirely beyond file
          (and from (> from line-count)) nil
          ;; Valid range: clamp to boundaries and extract
          :else
          (let [start (max 0 (dec (or from 1)))
                end (min (or to line-count) line-count)]
            (str/join "\n" (subvec (vec lines) start end)))))
      content)))
