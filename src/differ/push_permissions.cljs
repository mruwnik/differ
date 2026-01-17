(ns differ.push-permissions
  "Whitelist-based permission system for git push operations.

   Controls which repos/branches can be pushed to via the create_pull_request tool.

   NOTE: Currently only supports GitHub repositories. Remote URLs must be in one of:
     - https://github.com/owner/repo.git
     - git@github.com:owner/repo.git
   Other git hosting providers (GitLab, Bitbucket, etc.) are not supported.

   Whitelist structure (in config.edn):
     :push-whitelist {\"owner/repo\" [\"feature/*\" \"fix/*\"]
                      \"myorg/*\" [\"*\"]}

   - Empty whitelist = all repos/branches allowed (default-allow for ease of setup)
   - Repo keys can use wildcards: \"owner/*\" matches any repo from that owner
   - Branch patterns can use wildcards: \"feature/*\" matches feature/foo, feature/bar, etc.
   - \"*\" as branch pattern matches any branch
   - Only * wildcards are supported, not full regex syntax"
  (:require [differ.config :as config]
            [clojure.string :as str]))

;; Regex cache for pattern matching (avoids recompiling same patterns)
(defonce ^:private regex-cache (atom {}))

(defn parse-remote-url
  "Normalize git remote URL to owner/repo format.
   Handles:
   - https://github.com/owner/repo.git
   - https://github.com/owner/repo
   - git@github.com:owner/repo.git
   - git@github.com:owner/repo
   - owner/repo (already normalized)
   Returns nil if URL can't be parsed."
  [url]
  (when (and url (string? url))
    (let [url (str/trim url)]
      (or
       ;; HTTPS format: https://github.com/owner/repo.git or https://github.com/owner/repo/
       (when-let [[_ owner repo] (re-find #"github\.com/([^/]+)/([^/]+?)(?:\.git)?/?$" url)]
         (str owner "/" repo))

       ;; SSH format: git@github.com:owner/repo.git or git@github.com:owner/repo
       (when-let [[_ owner repo] (re-find #"github\.com:([^/]+)/([^/]+?)(?:\.git)?/?$" url)]
         (str owner "/" repo))

       ;; Already in owner/repo format
       (when (re-matches #"^[^/]+/[^/]+$" url)
         url)))))

(defn- escape-regex-char
  "Escape a single character if it's a regex special char."
  [c]
  (if (#{\. \+ \? \^ \$ \{ \} \( \) \| \[ \] \\} c)
    (str "\\" c)
    (str c)))

(defn- pattern-to-regex
  "Convert a simple wildcard pattern to a regex (memoized).
   * matches any sequence of characters.
   Only * wildcards are supported, not full regex syntax."
  [pattern]
  (if-let [cached (get @regex-cache pattern)]
    cached
    (let [escaped (->> pattern
                       (map #(if (= % \*)
                               ".*"
                               (escape-regex-char %)))
                       (apply str))
          regex (re-pattern (str "^" escaped "$"))]
      (swap! regex-cache assoc pattern regex)
      regex)))

(defn pattern-matches?
  "Check if a string matches a wildcard pattern.
   * matches any sequence of characters (only * wildcards, not full regex).
   Public for testing purposes."
  [pattern s]
  (if (= pattern "*")
    true
    (boolean (re-matches (pattern-to-regex pattern) s))))

(defn- repo-matches-key?
  "Check if a normalized repo (owner/repo) matches a whitelist key.
   Supports wildcards: owner/* matches any repo from that owner."
  [whitelist-key repo]
  (pattern-matches? whitelist-key repo))

(defn- branch-matches-patterns?
  "Check if a branch matches any of the allowed patterns."
  [patterns branch]
  (some #(pattern-matches? % branch) patterns))

(defn get-whitelist
  "Get push whitelist from config. Returns nil if not configured or empty."
  []
  (let [whitelist (config/get-value :push-whitelist)]
    (when (and whitelist (seq whitelist))
      whitelist)))

(defn check-permission
  "Check if push is allowed for repo/branch combination.
   Args:
     repo - normalized repo in owner/repo format
     branch - branch name
   Returns:
     {:allowed true} if push is allowed
     {:allowed false :reason string} if push is denied"
  [repo branch]
  (if-let [whitelist (get-whitelist)]
    ;; Whitelist is configured - check if repo/branch is allowed
    (let [matching-keys (filter #(repo-matches-key? % repo) (keys whitelist))]
      (if (empty? matching-keys)
        {:allowed false
         :reason (str "Repository '" repo "' is not in the push whitelist")}
        ;; Found matching repo key(s) - check branch patterns
        (let [all-patterns (mapcat #(get whitelist %) matching-keys)]
          (if (branch-matches-patterns? all-patterns branch)
            {:allowed true}
            {:allowed false
             :reason (str "Branch '" branch "' is not allowed for repository '" repo "'. "
                          "Allowed patterns: " (str/join ", " all-patterns))}))))
    ;; No whitelist configured - allow all
    {:allowed true}))

(defn validate-push!
  "Validate that push is allowed, throwing if not.
   Args:
     repo-path - path to local git repo (for error context)
     remote-url - git remote URL
     branch - branch name
   Returns:
     {:repo normalized-repo :branch branch} on success
   Throws:
     ExceptionInfo with :code :permission-denied on failure"
  [repo-path remote-url branch]
  (let [repo (parse-remote-url remote-url)]
    (if-not repo
      (throw (ex-info (str "Could not parse remote URL: " remote-url)
                      {:code :invalid-remote
                       :repo-path repo-path
                       :remote-url remote-url}))
      (let [result (check-permission repo branch)]
        (if (:allowed result)
          {:repo repo :branch branch}
          (throw (ex-info (:reason result)
                          {:code :permission-denied
                           :repo repo
                           :branch branch
                           :repo-path repo-path})))))))
