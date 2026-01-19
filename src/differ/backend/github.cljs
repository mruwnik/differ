(ns differ.backend.github
  "GitHub PR backend implementation of ReviewBackend protocol.
   Uses GitHub's GraphQL API for all operations."
  (:require [differ.backend.protocol :as proto]
            [differ.github-api :as gh-api]
            [differ.pull-request :as pr]
            [differ.util :as util]
            [clojure.string :as str]
            ["child_process" :as cp]))

(defn- escape-regex
  "Escape special regex characters in a string."
  [s]
  (str/replace s #"[.*+?^${}()|\[\]\\]" "\\$&"))

(defn- push-branch-async
  "Push current branch to origin. Returns Promise of {:success bool :error string?}."
  [repo-path branch]
  (js/Promise.
   (fn [resolve _]
     (let [proc (cp/spawn "git" #js ["push" "-u" "origin" branch]
                          #js {:cwd repo-path :encoding "utf8"})
           stderr-chunks #js []]
       (.on (.-stderr proc) "data" #(.push stderr-chunks %))
       (.on proc "close"
            (fn [code]
              (if (zero? code)
                (resolve {:success true})
                (resolve {:success false
                          :error (str/trim (.join stderr-chunks ""))}))))
       (.on proc "error"
            (fn [err]
              (resolve {:success false :error (.-message err)})))))))

;; Use shared graphql-request from github-api
(def ^:private graphql-request gh-api/graphql-request)

(defn- format-attribution
  "Format author/model attribution prefix for GitHub comments."
  [{:keys [author model]}]
  (cond
    (and author model) (str "**" author "** *(" model ")*")
    author             (str "**" author "**")
    model              (str "*(" model ")*")
    :else              nil))

(defn- with-attribution
  "Prepend attribution to text if available."
  [text opts]
  (if-let [attr (format-attribution opts)]
    (str attr ": " text)
    text))

(defn- resolve-ref
  "Resolve symbolic refs (base/head) to actual SHAs."
  [pr ref]
  (case ref
    "base" (:baseRefOid pr)
    "head" (:headRefOid pr)
    (nil "") (:headRefOid pr)
    ref))

(defn- pr-closed?
  "Check if PR context indicates a terminal state (closed/merged)."
  [ctx]
  (pr/pr-terminal-state? (:state ctx)))

;; Use shared paginate-graphql from github-api
(def ^:private paginate-graphql gh-api/paginate-graphql)

;; GraphQL queries

(def pr-query
  "query($owner: String!, $repo: String!, $number: Int!) {
    repository(owner: $owner, name: $repo) {
      pullRequest(number: $number) {
        id
        number
        title
        body
        state
        author { login }
        baseRefName
        headRefName
        baseRefOid
        headRefOid
        createdAt
        updatedAt
        mergeable
      }
    }
  }")

(def pr-files-query
  "query($owner: String!, $repo: String!, $number: Int!, $cursor: String) {
    repository(owner: $owner, name: $repo) {
      pullRequest(number: $number) {
        files(first: 100, after: $cursor) {
          pageInfo { hasNextPage endCursor }
          nodes {
            path
            additions
            deletions
            changeType
          }
        }
      }
    }
  }")

(def pr-commits-query
  "query($owner: String!, $repo: String!, $number: Int!, $cursor: String) {
    repository(owner: $owner, name: $repo) {
      pullRequest(number: $number) {
        commits(first: 100, after: $cursor) {
          pageInfo { hasNextPage endCursor }
          nodes {
            commit {
              oid
              message
              author { name date }
            }
          }
        }
      }
    }
  }")

(def pr-threads-query
  "query($owner: String!, $repo: String!, $number: Int!, $cursor: String) {
    repository(owner: $owner, name: $repo) {
      pullRequest(number: $number) {
        reviewThreads(first: 100, after: $cursor) {
          pageInfo { hasNextPage endCursor }
          nodes {
            id
            isResolved
            path
            line
            comments(first: 100) {
              nodes {
                id
                databaseId
                body
                author { login }
                createdAt
                updatedAt
              }
            }
          }
        }
      }
    }
  }")

(def file-content-query
  "query($owner: String!, $repo: String!, $expression: String!) {
    repository(owner: $owner, name: $repo) {
      object(expression: $expression) {
        ... on Blob {
          text
          byteSize
        }
      }
    }
  }")

(def tree-query
  "query($owner: String!, $repo: String!, $expression: String!) {
    repository(owner: $owner, name: $repo) {
      object(expression: $expression) {
        ... on Tree {
          entries {
            name
            path
            type
            object {
              ... on Blob { byteSize }
            }
          }
        }
      }
    }
  }")

(def add-review-thread-mutation
  "mutation($input: AddPullRequestReviewThreadInput!) {
    addPullRequestReviewThread(input: $input) {
      thread {
        id
        comments(first: 1) {
          nodes {
            id
            body
            createdAt
            author { login }
          }
        }
      }
    }
  }")

(def add-issue-comment-mutation
  "mutation($input: AddCommentInput!) {
    addComment(input: $input) {
      commentEdge {
        node {
          id
          databaseId
          body
          createdAt
        }
      }
    }
  }")

(def add-thread-reply-mutation
  "mutation($input: AddPullRequestReviewThreadReplyInput!) {
    addPullRequestReviewThreadReply(input: $input) {
      comment {
        id
        body
        createdAt
        author { login }
      }
    }
  }")

(def resolve-thread-mutation
  "mutation($input: ResolveReviewThreadInput!) {
    resolveReviewThread(input: $input) {
      thread { id isResolved }
    }
  }")

(def unresolve-thread-mutation
  "mutation($input: UnresolveReviewThreadInput!) {
    unresolveReviewThread(input: $input) {
      thread { id isResolved }
    }
  }")

(def submit-review-mutation
  "mutation($input: SubmitPullRequestReviewInput!) {
    submitPullRequestReview(input: $input) {
      pullRequestReview {
        id
        state
        body
        submittedAt
      }
    }
  }")

(def pending-reviews-query
  "query($owner: String!, $repo: String!, $number: Int!) {
    repository(owner: $owner, name: $repo) {
      pullRequest(number: $number) {
        reviews(first: 10, states: [PENDING]) {
          nodes {
            id
            viewerDidAuthor
          }
        }
      }
    }
  }")

(defn format-new-pr-result
  "Format result when a new PR was created from request-review!."
  [{:keys [pr-url pr-number pr-state]}]
  (let [parsed (re-find #"github\.com/([^/]+)/([^/]+)/pull" pr-url)]
    (if-not parsed
      (throw (ex-info (str "Invalid PR URL format: " pr-url)
                      {:code :invalid-pr-url :pr-url pr-url}))
      (let [[_ owner repo] parsed]
        {:review-url pr-url
         ;; NOTE: Duplicates sessions/github-session-id logic.
         ;; Cannot import sessions.cljs here - it would create a circular
         ;; dependency (sessions.cljs -> github.cljs -> sessions.cljs).
         :review-session-id (str "github:" owner "/" repo ":" pr-number)
         :state (pr/normalize-pr-state pr-state)
         :status :created}))))

(defn format-existing-pr-result
  "Format result when returning an existing PR from request-review!."
  [session-id pr-url state]
  {:review-url pr-url
   :review-session-id session-id
   :state (pr/normalize-pr-state state)
   :status :existing})

;; Helper to get diff via REST API (GraphQL doesn't support diff format)
(defn- get-pr-diff
  "Get PR diff in unified format via REST API."
  [token owner repo pr-number]
  (gh-api/rest-request token
                       (str "https://api.github.com/repos/" owner "/" repo "/pulls/" pr-number)
                       {:accept "application/vnd.github.v3.diff"}))

;; GitHubBackend record

(defrecord GitHubBackend [owner repo pr-number token pr-id session-id-str]
  proto/ReviewBackend

  (session-id [_]
    session-id-str)

  (session-type [_]
    :github)

  (session-descriptor [_]
    {:type :github
     :owner owner
     :repo repo
     :pr-number pr-number})

  (get-context [_]
    (-> (graphql-request token pr-query {:owner owner :repo repo :number pr-number})
        (.then (fn [data]
                 (let [pr (get-in data [:repository :pullRequest])]
                   {:type :github
                    :owner owner
                    :repo repo
                    :pr-number pr-number
                    :title (:title pr)
                    :body (:body pr)
                    :author (get-in pr [:author :login])
                    :state (str/lower-case (or (:state pr) ""))
                    :base-branch (:baseRefName pr)
                    :head-branch (:headRefName pr)
                    :base-sha (:baseRefOid pr)
                    :head-sha (:headRefOid pr)
                    :created-at (:createdAt pr)
                    :updated-at (:updatedAt pr)
                    :mergeable (:mergeable pr)})))))

  (get-diff
    [this] (proto/get-diff this nil))
  (get-diff
    [_ opts]
    (-> (get-pr-diff token owner repo pr-number)
        (.then (fn [diff]
                 (if opts
                   (proto/extract-lines diff opts)
                   diff)))))

  (get-file-diff
    [this path] (proto/get-file-diff this path nil))
  (get-file-diff
    [_ file-path opts]
    ;; Get full diff and filter to specific file
    (-> (get-pr-diff token owner repo pr-number)
        (.then (fn [diff]
                 (when diff
                   ;; Parse and find the file
                   (let [file-pattern (re-pattern (str "diff --git a/" (escape-regex file-path) " b/"))
                         ;; Find section for this file
                         sections (str/split diff #"(?=diff --git)")
                         file-section (some #(when (re-find file-pattern %) %) sections)]
                     (if opts
                       (proto/extract-lines file-section opts)
                       file-section)))))))

  (get-changed-files [_]
    (-> (paginate-graphql token pr-files-query
                          {:owner owner :repo repo :number pr-number}
                          [:repository :pullRequest :files])
        (.then (fn [files]
                 (mapv (fn [f]
                         {:path (:path f)
                          :status (case (:changeType f)
                                    "ADDED" :added
                                    "DELETED" :deleted
                                    "RENAMED" :renamed
                                    :modified)
                          :additions (:additions f)
                          :deletions (:deletions f)})
                       files)))))

  (get-file-content
    [this ref path] (proto/get-file-content this ref path nil))
  (get-file-content
    [_ ref path opts]
    (-> (graphql-request token pr-query {:owner owner :repo repo :number pr-number})
        (.then (fn [pr-data]
                 (let [pr (get-in pr-data [:repository :pullRequest])
                       effective-ref (resolve-ref pr ref)]
                   (graphql-request token file-content-query
                                    {:owner owner
                                     :repo repo
                                     :expression (str effective-ref ":" path)}))))
        (.then (fn [data]
                 (let [content (get-in data [:repository :object :text])]
                   (if opts
                     (proto/extract-lines content opts)
                     content))))))

  (list-directory [_ ref dir-path]
    (-> (graphql-request token pr-query {:owner owner :repo repo :number pr-number})
        (.then (fn [pr-data]
                 (let [pr (get-in pr-data [:repository :pullRequest])
                       effective-ref (resolve-ref pr ref)
                       expression (if (seq dir-path)
                                    (str effective-ref ":" dir-path)
                                    (str effective-ref ":"))]
                   (graphql-request token tree-query
                                    {:owner owner
                                     :repo repo
                                     :expression expression}))))
        (.then (fn [data]
                 (let [entries (get-in data [:repository :object :entries])]
                   (mapv (fn [e]
                           {:name (:name e)
                            :path (:path e)
                            :type (if (= (:type e) "tree") :dir :file)
                            :size (get-in e [:object :byteSize])})
                         entries))))))

  (file-exists? [_ ref path]
    (-> (graphql-request token pr-query {:owner owner :repo repo :number pr-number})
        (.then (fn [pr-data]
                 (let [pr (get-in pr-data [:repository :pullRequest])
                       effective-ref (resolve-ref pr ref)]
                   (graphql-request token file-content-query
                                    {:owner owner
                                     :repo repo
                                     :expression (str effective-ref ":" path)}))))
        (.then (fn [data]
                 (some? (get-in data [:repository :object]))))
        (.catch (constantly false))))

  (get-history [_ opts]
    (let [{:keys [limit]} opts
          limit (or limit 50)]
      (-> (paginate-graphql token pr-commits-query
                            {:owner owner :repo repo :number pr-number}
                            [:repository :pullRequest :commits])
          (.then (fn [commits]
                   (->> commits
                        (take limit)
                        (mapv (fn [n]
                                (let [c (:commit n)]
                                  {:sha (:oid c)
                                   :message (:message c)
                                   :author (get-in c [:author :name])
                                   :date (get-in c [:author :date])})))))))))

  (get-branches [_]
    (-> (graphql-request token pr-query {:owner owner :repo repo :number pr-number})
        (.then (fn [data]
                 (let [pr (get-in data [:repository :pullRequest])]
                   [{:name (:baseRefName pr) :type :base}
                    {:name (:headRefName pr) :type :head}])))))

  ;; Comments - use GitHub's review threads
  (get-comments [_]
    (-> (paginate-graphql token pr-threads-query
                          {:owner owner :repo repo :number pr-number}
                          [:repository :pullRequest :reviewThreads])
        (.then (fn [threads]
                 (mapv (fn [thread]
                         (let [comments (get-in thread [:comments :nodes])
                               first-comment (first comments)]
                           {:id (:id thread)
                            :file (:path thread)
                            :line (:line thread)
                            :resolved (:isResolved thread)
                            :text (:body first-comment)
                            :author (get-in first-comment [:author :login])
                            :created-at (:createdAt first-comment)
                            :replies (mapv (fn [c]
                                             {:id (:id c)
                                              :text (:body c)
                                              :author (get-in c [:author :login])
                                              :created-at (:createdAt c)})
                                           (rest comments))}))
                       threads)))))

  (get-pending-comments [this opts]
    (-> (proto/get-comments this)
        (.then (fn [comments]
                 (let [{:keys [since]} opts]
                   (cond->> comments
                     true (filter #(not (:resolved %)))
                     since (filter #(> (js/Date. (:created-at %)) (js/Date. since)))))))))

  (add-comment! [_ comment]
    (let [{:keys [file line text parent-id side]} comment
          body (with-attribution text comment)]
      (cond
        ;; Reply to existing review thread
        parent-id
        (-> (graphql-request token add-thread-reply-mutation
                             {:input {:pullRequestReviewThreadId parent-id
                                      :body body}})
            (.then (fn [data]
                     (let [c (get-in data [:addPullRequestReviewThreadReply :comment])]
                       {:id (:id c)
                        :text (:body c)
                        :author (get-in c [:author :login])
                        :created-at (:createdAt c)}))))

        ;; Add review thread on specific line
        (and file line)
        (-> (graphql-request token pr-query {:owner owner :repo repo :number pr-number})
            (.then (fn [data]
                     (let [pr-node-id (get-in data [:repository :pullRequest :id])]
                       (if pr-node-id
                         (graphql-request token add-review-thread-mutation
                                          {:input {:pullRequestId pr-node-id
                                                   :body body
                                                   :path file
                                                   :line line
                                                   :side (or side "RIGHT")}})
                         (throw (ex-info "PR not found or inaccessible"
                                         {:owner owner :repo repo :pr-number pr-number}))))))
            (.then (fn [data]
                     (let [thread (get-in data [:addPullRequestReviewThread :thread])
                           c (first (get-in thread [:comments :nodes]))]
                       {:id (:id thread)
                        :text (:body c)
                        :author (get-in c [:author :login])
                        :created-at (:createdAt c)}))))

        ;; Add general PR comment (issue comment)
        :else
        (-> (graphql-request token pr-query {:owner owner :repo repo :number pr-number})
            (.then (fn [data]
                     (let [pr-node-id (get-in data [:repository :pullRequest :id])]
                       (if pr-node-id
                         (graphql-request token add-issue-comment-mutation
                                          {:input {:subjectId pr-node-id
                                                   :body body}})
                         (throw (ex-info "PR not found or inaccessible"
                                         {:owner owner :repo repo :pr-number pr-number}))))))
            (.then (fn [data]
                     (let [c (get-in data [:addComment :commentEdge :node])]
                       {:id (:id c)
                        :text (:body c)
                        :created-at (:createdAt c)})))))))

  (resolve-comment! [_ thread-id _author]
    (-> (graphql-request token resolve-thread-mutation
                         {:input {:threadId thread-id}})
        (.then (fn [_] {:resolved true}))))

  (unresolve-comment! [_ thread-id _author]
    (-> (graphql-request token unresolve-thread-mutation
                         {:input {:threadId thread-id}})
        (.then (fn [_] {:resolved false}))))

  (submit-review! [_ opts]
    (let [{:keys [body]} opts
          formatted-body (when body (with-attribution body opts))
          ;; Only COMMENT supported - no approve/request-changes via this tool
          gh-event "COMMENT"]
      ;; First, find any pending reviews authored by the current user
      (-> (graphql-request token pending-reviews-query
                           {:owner owner :repo repo :number pr-number})
          (.then (fn [data]
                   (let [reviews (get-in data [:repository :pullRequest :reviews :nodes])
                         my-review (first (filter :viewerDidAuthor reviews))]
                     (if my-review
                       ;; Submit the pending review
                       (graphql-request token submit-review-mutation
                                        {:input (cond-> {:pullRequestReviewId (:id my-review)
                                                         :event gh-event}
                                                  formatted-body (assoc :body formatted-body))})
                       ;; No pending review found
                       {:submitted false :reason "No pending review found"}))))
          (.then (fn [result]
                   (if (:submitted result)
                     result
                     (if-let [review (get-in result [:submitPullRequestReview :pullRequestReview])]
                       {:submitted true
                        :review-id (:id review)
                        :state (:state review)}
                       result)))))))

  (request-review! [this opts]
    (let [pr-url (str "https://github.com/" owner "/" repo "/pull/" pr-number)]
      ;; Always fetch context to get PR state
      (-> (proto/get-context this)
          (.then (fn [ctx]
                   (cond
                     ;; PR is closed/merged and we have a repo-path - create new PR
                     (and (pr-closed? ctx) (:repo-path opts))
                     (-> (pr/create-pull-request! {:repo-path (:repo-path opts)
                                                   :title (:title opts)
                                                   :body (:body opts)
                                                   :draft (:draft opts)})
                         (.then (fn [result]
                                  (if (:error result)
                                    (throw (ex-info (:error result)
                                                    {:code (or (:code result) :pr-creation-failed)}))
                                    (format-new-pr-result result)))))

                     ;; PR is closed/merged but no repo-path - error
                     (pr-closed? ctx)
                     (throw (ex-info (str "PR #" pr-number " is " (:state ctx)
                                          ". Provide repo_path to create a new PR.")
                                     {:code :pr-closed
                                      :state (:state ctx)}))

                     ;; PR is open - push if repo-path provided, then return existing
                     (:repo-path opts)
                     (-> (push-branch-async (:repo-path opts) (:head-branch ctx))
                         (.then (fn [push-result]
                                  (when-not (:success push-result)
                                    (throw (ex-info (str "Push failed: " (:error push-result))
                                                    {:code :push-failed})))
                                  (format-existing-pr-result (proto/session-id this) pr-url (:state ctx)))))

                     ;; PR is open, no repo-path - just return existing
                     :else
                     (format-existing-pr-result (proto/session-id this) pr-url (:state ctx)))))))))

;; Constructor

(defn create-github-backend
  "Create a GitHubBackend for a GitHub PR.
   owner: repository owner
   repo: repository name
   pr-number: pull request number
   token: GitHub access token"
  [owner repo pr-number token]
  (let [session-id (str "github:" owner "/" repo ":" pr-number)]
    (->GitHubBackend owner repo pr-number token nil session-id)))

(defn parse-pr-url
  "Parse a GitHub PR URL into {:owner :repo :pr-number}.
   Supports formats:
   - https://github.com/owner/repo/pull/123
   - github.com/owner/repo/pull/123
   - owner/repo#123"
  [url]
  (or
   ;; Full URL format
   (when-let [[_ owner repo pr] (re-find #"github\.com/([^/]+)/([^/]+)/pull/(\d+)" url)]
     {:owner owner :repo repo :pr-number (js/parseInt pr 10)})
   ;; Short format: owner/repo#123
   (when-let [[_ owner repo pr] (re-find #"^([^/]+)/([^#]+)#(\d+)$" url)]
     {:owner owner :repo repo :pr-number (js/parseInt pr 10)})))
