(ns differ.pull-request
  "GitHub Pull Request creation operations.

   Provides functionality to:
   1. Push a local branch to GitHub
   2. Create a PR (or return existing one)
   3. Validate permissions via whitelist"
  (:require [differ.push-permissions :as perms]
            [differ.github-oauth :as github-oauth]
            [differ.github-api :as gh-api]
            [differ.config :as config]
            [clojure.string :as str]
            ["child_process" :as cp]
            ["path" :as path]))

;; Git command execution (sync version for local checks)

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

(defn- git-repo?
  "Check if directory is a git repository."
  [dir]
  (some? (exec-git dir "rev-parse" "--git-dir")))

(defn- get-remote-url
  "Get the remote origin URL, or nil if not set."
  [repo-path]
  (exec-git repo-path "remote" "get-url" "origin"))

(defn- get-current-branch
  "Get the current branch name, or 'working' if not in a git repo."
  [repo-path]
  (or (exec-git repo-path "rev-parse" "--abbrev-ref" "HEAD")
      "working"))

(defn- detect-default-branch
  "Detect the default branch (main or master)."
  [repo-path]
  (let [remote-head (exec-git repo-path "symbolic-ref" "refs/remotes/origin/HEAD" "--short")
        remote-branch (when remote-head
                        (last (str/split remote-head #"/")))]
    (or remote-branch
        (when (exec-git repo-path "rev-parse" "--verify" "main") "main")
        (when (exec-git repo-path "rev-parse" "--verify" "master") "master")
        "main")))

;; Git command execution (async version)

(defn- exec-git-async
  "Execute git command asynchronously, returning a Promise.
   Resolves to {:success true :stdout string} or {:success false :error string}.
   Uses spawn with args array to avoid shell injection vulnerabilities."
  [repo-path args]
  (js/Promise.
   (fn [resolve _reject]
     (let [proc (cp/spawn "git" (clj->js args)
                          #js {:cwd repo-path :encoding "utf8"})
           stdout-chunks #js []
           stderr-chunks #js []]
       (.on (.-stdout proc) "data" #(.push stdout-chunks %))
       (.on (.-stderr proc) "data" #(.push stderr-chunks %))
       (.on proc "close"
            (fn [code]
              (let [stdout-str (str/trim (.join stdout-chunks ""))
                    stderr-str (.join stderr-chunks "")]
                (if (zero? code)
                  (resolve {:success true
                            :stdout stdout-str})
                  (resolve {:success false
                            :error (if (seq stderr-str) stderr-str (str "Exit code: " code))
                            :exit-code code})))))
       (.on proc "error"
            (fn [err]
              (resolve {:success false
                        :error (.-message err)
                        :exit-code nil})))))))

;; Use shared GraphQL request from github-api
(def ^:private graphql-request gh-api/graphql-request)

;; GraphQL queries/mutations

(def find-pr-query
  "query($owner: String!, $repo: String!, $head: String!) {
    repository(owner: $owner, name: $repo) {
      pullRequests(headRefName: $head, states: [OPEN], first: 1) {
        nodes {
          number
          url
          title
          baseRefName
          headRefName
        }
      }
    }
  }")

(def get-repo-id-query
  "query($owner: String!, $repo: String!) {
    repository(owner: $owner, name: $repo) {
      id
    }
  }")

(def create-pr-mutation
  "mutation($input: CreatePullRequestInput!) {
    createPullRequest(input: $input) {
      pullRequest {
        number
        url
        title
        baseRefName
        headRefName
      }
    }
  }")

;; Core functions

(defn- get-last-commit-message
  "Get the last commit message for default PR title.
   Returns a Promise of the commit message or nil."
  [repo-path]
  (-> (exec-git-async repo-path ["log" "-1" "--format=%s"])
      (.then (fn [result]
               (when (:success result)
                 (:stdout result))))
      (.catch (fn [_] nil))))

(defn- push-branch!
  "Push current branch to origin with tracking.
   Uses 'git push -u origin <branch>' which:
   - Creates the remote branch if it doesn't exist
   - Fast-forwards if remote branch exists and is an ancestor
   - FAILS if remote branch has diverged (no force push)
   Returns Promise of {:success true} or {:success false :error string}."
  [repo-path branch]
  (exec-git-async repo-path ["push" "-u" "origin" branch]))

(defn- get-existing-pr
  "Check if a PR already exists for this branch.
   Returns Promise of PR data or nil."
  [token owner repo branch]
  (-> (graphql-request token find-pr-query
                       {:owner owner :repo repo :head branch})
      (.then (fn [data]
               (first (get-in data [:repository :pullRequests :nodes]))))))

(defn- get-repository-id
  "Get the GitHub node ID for a repository."
  [token owner repo]
  (-> (graphql-request token get-repo-id-query {:owner owner :repo repo})
      (.then (fn [data]
               (get-in data [:repository :id])))))

(defn- create-pr!
  "Create a new PR via GitHub GraphQL API.
   Returns Promise of PR data."
  [token owner repo {:keys [title body base head draft]}]
  (-> (get-repository-id token owner repo)
      (.then (fn [repo-id]
               (when-not repo-id
                 (throw (ex-info (str "Repository not found: " owner "/" repo)
                                 {:code :repo-not-found})))
               (graphql-request token create-pr-mutation
                                {:input {:repositoryId repo-id
                                         :title title
                                         :body (or body "")
                                         :baseRefName base
                                         :headRefName head
                                         :draft (boolean draft)}})))))

(defn- try-tokens-for-pr
  "Try tokens sequentially until one works for PR creation.
   Returns Promise of {:token ... :owner ... :repo ...} or {:error ... :details [...]}."
  ([owner repo tokens]
   (try-tokens-for-pr owner repo tokens []))
  ([owner repo tokens errors]
   (if (empty? tokens)
     (js/Promise.resolve
      {:error (if (seq errors)
                (str "Could not access repository with any token. Errors: "
                     (str/join "; " errors))
                "No tokens available")
       :details errors})
     (let [[token-record & remaining] tokens
           token-name (or (:name token-record) "token")]
       (-> (get-repository-id (:access-token token-record) owner repo)
           (.then (fn [repo-id]
                    (if repo-id
                      {:token (:access-token token-record)
                       :owner owner
                       :repo repo}
                      (try-tokens-for-pr owner repo remaining
                                         (conj errors (str token-name ": repository not found"))))))
           (.catch (fn [err]
                     (try-tokens-for-pr owner repo remaining
                                        (conj errors (str token-name ": " (ex-message err)))))))))))

(defn- push-and-create-pr!
  "Push branch and create/find PR. Returns Promise of PR result map.
   This is the async core of create-pull-request!, extracted for clarity.

   Args:
     token - GitHub access token
     opts - Map with :repo-path, :owner, :repo-name, :branch, :base, :title, :body, :draft

   Returns Promise resolving to:
     {:pr-url ... :pr-number ... :created true/false ...} on success
     Rejects with ex-info on failure."
  [token {:keys [repo-path owner repo-name branch base title body draft]}]
  (-> (push-branch! repo-path branch)
      (.then (fn [push-result]
               (when-not (:success push-result)
                 (throw (ex-info (str "Push failed: " (:error push-result))
                                 {:code :push-failed
                                  :details (:error push-result)})))
               (get-existing-pr token owner repo-name branch)))
      (.then (fn [existing-pr]
               (if existing-pr
                 ;; Return existing PR
                 {:pr-url (:url existing-pr)
                  :pr-number (:number existing-pr)
                  :created false
                  :branch branch
                  :base-branch (:baseRefName existing-pr)
                  :title (:title existing-pr)}
                 ;; Create new PR
                 (-> (if title
                       (js/Promise.resolve title)
                       (get-last-commit-message repo-path))
                     (.then (fn [commit-msg]
                              (create-pr! token owner repo-name
                                          {:title (or commit-msg branch)
                                           :body body
                                           :base base
                                           :head branch
                                           :draft draft})))
                     (.then (fn [result]
                              (let [pr (get-in result [:createPullRequest :pullRequest])]
                                {:pr-url (:url pr)
                                 :pr-number (:number pr)
                                 :created true
                                 :branch (:headRefName pr)
                                 :base-branch (:baseRefName pr)
                                 :title (:title pr)})))))))))

(defn create-pull-request!
  "Main entry point: validate permissions, push, and create PR.

   Args:
     params - Map with:
       :repo-path - Absolute path to local git repo (required)
       :title - PR title (optional, defaults to last commit message)
       :body - PR description (optional)
       :base-branch - Base branch to merge into (optional, auto-detected)
       :draft - Create as draft PR (optional, default false)

   Returns Promise that always resolves (never rejects) with one of:
     {:pr-url ... :pr-number ... :created true/false ...} on success
     {:error ... :code ...} on failure

   Note: This 'never-rejects' pattern simplifies caller error handling but
   requires callers to check for :error in the result rather than using .catch."
  [{:keys [repo-path title body base-branch draft]}]
  (js/Promise.
   (fn [resolve _reject]
     (try
       ;; Step 1: Validate repo path is absolute (security: prevent path traversal)
       (when-not (and (string? repo-path) (path/isAbsolute repo-path))
         (throw (ex-info "repo-path must be an absolute path"
                         {:code :invalid-path :repo-path repo-path})))

       ;; Step 2: Validate it's a git repository
       (when-not (git-repo? repo-path)
         (throw (ex-info "Not a git repository"
                         {:code :not-git-repo :repo-path repo-path})))

       ;; Step 3: Get branch and remote info
       (let [branch (get-current-branch repo-path)
             remote-url (get-remote-url repo-path)
             base (or base-branch (detect-default-branch repo-path))]

         ;; Step 4: Check for detached HEAD state
         (when (= branch "HEAD")
           (throw (ex-info "Cannot create PR from detached HEAD state - please checkout a branch first"
                           {:code :detached-head :repo-path repo-path})))

         (when-not remote-url
           (throw (ex-info "No remote 'origin' configured"
                           {:code :no-remote :repo-path repo-path})))

         ;; Step 5: Validate permissions
         (let [{:keys [repo]} (perms/validate-push! repo-path remote-url branch)
               [owner repo-name] (str/split repo #"/" 2)
               tokens (github-oauth/get-all-tokens)]

           ;; Step 6: Check for tokens
           (when (empty? tokens)
             (throw (ex-info "GitHub authentication required"
                             {:code :no-auth
                              :auth-url (str (config/base-url) "/oauth/github")})))

           ;; Step 7: Find working token and execute PR workflow
           (-> (try-tokens-for-pr owner repo-name tokens)
               (.then (fn [token-result]
                        (if (:error token-result)
                          (throw (ex-info (:error token-result)
                                          {:code :token-failed}))
                          (push-and-create-pr! (:token token-result)
                                               {:repo-path repo-path
                                                :owner owner
                                                :repo-name repo-name
                                                :branch branch
                                                :base base
                                                :title title
                                                :body body
                                                :draft draft}))))
               (.then resolve)
               (.catch (fn [err]
                         (resolve {:error (ex-message err)
                                   :code (or (:code (ex-data err)) :unknown)}))))))
       (catch :default e
         (resolve {:error (ex-message e)
                   :code (or (:code (ex-data e)) :unknown)}))))))
