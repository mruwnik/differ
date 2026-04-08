(ns differ.github-events
  "GitHub PR polling producer.

   Maintains one background poller per watched project. Fetches the open
   PR list on an interval, diffs against a cached per-PR state map, and
   appends the resulting events (`:pr-opened`, `:pr-head-changed`,
   `:pr-feedback-changed`, `:pr-closed`) to the central `differ.event-stream`
   under the scope `\"github:owner/repo\"`.

   Pollers start lazily on the first wait for a given scope (see
   `wait-for-scope!`) and stop after a grace period with no active callers.
   Ring buffer, cursor, subscribers, and blocking wait all live in
   `differ.event-stream` — this namespace only owns the polling lifecycle
   plus the GitHub-specific diff detection."
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [differ.config :as config]
            [differ.db :as db]
            [differ.event-stream :as event-stream]
            [differ.github-api :as gh-api]
            [differ.github-oauth :as github-oauth]))

;; ============================================================================
;; State
;; ============================================================================

;; Module-level poller state. Not `^:private` — tests mutate it directly,
;; and hiding it behind an accessor just adds ceremony without any
;; encapsulation benefit. Public consumers should use `wait-for-scope!`.
(defonce poller-state (atom {}))

(defn empty-poller-state []
  {:cache         nil
   :last-activity 0
   :timer         nil
   :generation    0
   :in-flight     nil})

(defn get-poller-state
  "Return the current poller state for `scope`, or nil if unknown.
   Read-only debugging accessor — usable from a REPL or from tests."
  [scope]
  (get @poller-state scope))

;; Global shutdown flag — shared with event-stream. Once set, no new
;; pollers start and existing pollers stop on their next tick.
(defn- shutdown? [] (event-stream/shutdown?-value))

;; ============================================================================
;; Scope parsing
;; ============================================================================

(def ^:private scope-prefix "github:")

(defn github-scope
  "Build a github scope string from an owner/repo pair or an 'owner/repo'
   string. Shape: 'github:owner/repo'."
  ([project] (str scope-prefix project))
  ([owner repo] (str scope-prefix owner "/" repo)))

(defn parse-scope
  "Parse a 'github:owner/repo' scope into [owner repo project-str].
   Returns nil if the scope is malformed."
  [scope]
  (when (and (string? scope) (str/starts-with? scope scope-prefix))
    (let [project (subs scope (count scope-prefix))
          parts (str/split project #"/")]
      (when (and (= 2 (count parts)) (seq (first parts)) (seq (second parts)))
        [(first parts) (second parts) project]))))

;; ============================================================================
;; Diff detection (pure)
;; ============================================================================

(def ^:private cache-entry-keys
  [:head-sha :unresolved-count :review-count :checks-status
   :updated-at :last-seen])

(defn- pr->cache-entry
  "Build a cache entry for a PR. Includes diffable fields plus a :last-seen
   snapshot of envelope-worthy attributes (url, title, author, branches,
   draft) that we want to preserve for `:pr-closed` event emission after
   the PR has disappeared from GitHub's response."
  [pr]
  {:head-sha         (:head-sha pr)
   :unresolved-count (:unresolved-count pr)
   :review-count     (:review-count pr)
   :checks-status    (:checks-status pr)
   :updated-at       (:updated-at pr)
   :last-seen        {:url         (:url pr)
                      :title       (:title pr)
                      :author      (:author pr)
                      :head-branch (:head-branch pr)
                      :base-branch (:base-branch pr)
                      :draft       (:draft pr)}})

(defn- prs->cache [prs]
  (into {} (map (juxt :number pr->cache-entry)) prs))

(defn resolve-session-id
  "Look up whether a differ session already exists for (project, pr-number).
   Returns the session id string if found, nil otherwise. We go directly
   through `differ.db` rather than `differ.sessions` to avoid a circular
   dependency. Defined as a defn (not defn-) so tests can stub it without
   requiring a live database."
  [project pr-number]
  (let [candidate (str "github:" project ":" pr-number)]
    (try
      (when (db/get-session candidate)
        candidate)
      (catch :default _
        nil))))

(defn- envelope
  "Build the common GitHub event envelope (no :seq, no :received-at, no
   :scope — the central event-stream adds those)."
  [project pr event-type]
  {:event-type  event-type
   :project     project
   :pr-number   (:number pr)
   :pr-url      (:url pr)
   :head-branch (:head-branch pr)
   :base-branch (:base-branch pr)
   :author      (:author pr)
   :title       (:title pr)
   :draft       (:draft pr)
   :session-id  (resolve-session-id project (:number pr))})

(defn- feedback-changed? [old-entry new-entry]
  (or (not= (:unresolved-count old-entry) (:unresolved-count new-entry))
      (not= (:review-count old-entry) (:review-count new-entry))
      (not= (:checks-status old-entry) (:checks-status new-entry))))

(defn- feedback-summary
  "Build a short human-readable summary of what changed in the feedback delta."
  [old-entry new-entry]
  (let [parts (cond-> []
                (not= (:unresolved-count old-entry) (:unresolved-count new-entry))
                (conj (let [delta (- (:unresolved-count new-entry)
                                     (:unresolved-count old-entry))]
                        (cond
                          (pos? delta)
                          (str delta " new comment" (when (not= delta 1) "s"))
                          (neg? delta)
                          (str (- delta) " comment"
                               (when (not= delta -1) "s") " resolved")
                          :else "comment count changed")))

                (not= (:review-count old-entry) (:review-count new-entry))
                (conj (let [delta (- (:review-count new-entry)
                                     (:review-count old-entry))]
                        (if (pos? delta)
                          (str delta " new review" (when (not= delta 1) "s"))
                          "review count changed")))

                (not= (:checks-status old-entry) (:checks-status new-entry))
                (conj (str "checks now "
                           (or (some-> (:checks-status new-entry) name)
                               "unknown"))))]
    (if (seq parts)
      (str/join ", " parts)
      "feedback changed")))

(defn- head-changed-event [project pr old-entry]
  (assoc (envelope project pr :pr-head-changed)
         :details {:new-head-sha      (:head-sha pr)
                   :previous-head-sha (:head-sha old-entry)}))

(defn- feedback-changed-event [project pr old-entry new-entry]
  (assoc (envelope project pr :pr-feedback-changed)
         :details {:unresolved-count          (:unresolved-count new-entry)
                   :previous-unresolved-count (:unresolved-count old-entry)
                   :review-count              (:review-count new-entry)
                   :previous-review-count     (:review-count old-entry)
                   :checks-status             (:checks-status new-entry)
                   :previous-checks-status    (:checks-status old-entry)
                   :summary                   (feedback-summary old-entry new-entry)}))

(defn- opened-event [project pr]
  (assoc (envelope project pr :pr-opened) :details {}))

(defn- closed-event
  "Emit a :pr-closed event. The poller fills in :merged asynchronously
   before append — this stub carries :merged = nil.

   Since the PR is absent from the new response, we rely on the :last-seen
   attributes stashed in the cache entry from the prior poll. This keeps
   GHE-friendly URLs correct (no hardcoded github.com) and populates the
   envelope fields that would otherwise all be nil."
  [project pr-number old-entry]
  (let [last-seen (:last-seen old-entry)]
    {:event-type  :pr-closed
     :project     project
     :pr-number   pr-number
     :pr-url      (:url last-seen)
     :head-branch (:head-branch last-seen)
     :base-branch (:base-branch last-seen)
     :author      (:author last-seen)
     :title       (:title last-seen)
     :draft       (:draft last-seen)
     :session-id  (resolve-session-id project pr-number)
     :details     {:merged nil}}))

(defn- oldest-updated-at
  "The minimum :updated-at string in `prs`, or nil if prs is empty.
   GitHub's updatedAt values are ISO-8601, so lexical compare works."
  [prs]
  (let [updated-ats (keep :updated-at prs)]
    (when (seq updated-ats)
      (reduce (fn [a b] (if (neg? (compare a b)) a b)) updated-ats))))

(defn detect-events
  "Pure function: compare old cache to new PR list, return
   {:events [events without :seq/:received-at/:merged], :new-cache {pr-number entry}}.

   If `old-cache` is nil, this is a baseline poll — the cache is populated
   but no events are emitted.

   When `truncated?` is true, the new PR list may be missing PRs that are
   still open but fell out of GitHub's UPDATED_AT DESC window. To avoid
   emitting spurious `:pr-closed` events, only mark a cached PR as closed
   when its cached `:updated-at` is at or above the oldest `:updated-at`
   in the truncated response (meaning it really should still be in the
   window, so its absence is meaningful)."
  ([project old-cache new-prs]
   (detect-events project old-cache new-prs false))
  ([project old-cache new-prs truncated?]
   (let [valid-prs (filterv #(some? (:number %)) new-prs)
         new-cache (prs->cache valid-prs)
         window-floor (when truncated? (oldest-updated-at valid-prs))]
     (if (nil? old-cache)
       {:events [] :new-cache new-cache}
       (let [new-by-number (into {} (map (juxt :number identity)) valid-prs)
             all-numbers (set/union (set (keys old-cache))
                                    (set (keys new-by-number)))
             likely-closed?
             (fn [old-entry]
               (if truncated?
                 (let [cached-updated-at (:updated-at old-entry)]
                   (and cached-updated-at window-floor
                        (not (neg? (compare cached-updated-at window-floor)))))
                 true))
             events
             (reduce
              (fn [acc pr-number]
                (let [old-entry (get old-cache pr-number)
                      new-entry (get new-cache pr-number)
                      new-pr (get new-by-number pr-number)]
                  (cond
                    (and (nil? old-entry) new-pr)
                    (conj acc (opened-event project new-pr))

                    (and old-entry (nil? new-pr))
                    (if (likely-closed? old-entry)
                      (conj acc (closed-event project pr-number old-entry))
                      acc)

                    :else
                    (cond-> acc
                      (not= (:head-sha old-entry) (:head-sha new-entry))
                      (conj (head-changed-event project new-pr old-entry))

                      (feedback-changed? old-entry new-entry)
                      (conj (feedback-changed-event project new-pr old-entry new-entry))))))
              []
              (sort all-numbers))]
         {:events events :new-cache new-cache})))))

;; ============================================================================
;; Poller: fetch + merge-status
;; ============================================================================

(defn rate-limit-error?
  "Heuristic: does an error message look like a GitHub rate limit? If so,
   burning through the remaining tokens won't help and risks racing through
   every token in a hot loop.

   Matches on body-text phrases GitHub actually emits (`rate limit exceeded`
   for primary, `secondary rate limit` for abuse-style limiters) and on
   status 429. Does NOT match bare 403 — GitHub returns 403 for MANY
   reasons (permission denied, SSO required, repo blocked), and the whole
   point of `try-tokens-with-fallback` is to try the next token when the
   current one lacks access. Relies on `graphql-request` surfacing the
   response body in the error message."
  [msg]
  (and (string? msg)
       (let [lower (str/lower-case msg)]
         (or (str/includes? lower "rate limit exceeded")
             (str/includes? lower "secondary rate limit")
             (re-find #"GitHub API error: 429" msg)))))

(defn try-tokens-with-fallback
  "Generic per-token fallback runner, shared by `fetch-poller-prs` and
   `resolve-merge-status`. `action` is a 1-arg fn from token-record to a
   Promise. On rate-limit errors we stop early rather than burning through
   the remaining tokens. Per-token errors are aggregated into the final
   error string so debugging isn't limited to the last token.

   Returns a Promise of {:success true :result <value>} or
   {:success false :error <aggregated string>}. Never rejects."
  [tokens action]
  (if (empty? tokens)
    (js/Promise.resolve {:success false :error "no tokens available"})
    (let [step (fn step [remaining errs]
                 (if (empty? remaining)
                   (js/Promise.resolve
                    {:success false
                     :error (if (seq errs)
                              (str/join "; " errs)
                              "no tokens available")})
                   (let [[token & more] remaining]
                     (-> (action token)
                         (.then (fn [result] {:success true :result result}))
                         (.catch (fn [err]
                                   (let [msg (or (.-message err) (str err))
                                         labelled (str "token-" (inc (count errs))
                                                       ": " msg)]
                                     (if (rate-limit-error? msg)
                                       (js/Promise.resolve
                                        {:success false
                                         :error (str/join "; " (conj errs labelled))
                                         :rate-limited true})
                                       (step more (conj errs labelled))))))))))]
      (step tokens []))))

(defn fetch-poller-prs
  "Fetch the poller-shaped PR list for a github scope. Returns a Promise of
   {:success true :prs [...] :truncated bool} or
   {:success false :error string}. Never rejects. `defn` so tests can stub."
  [scope]
  (if-let [[owner repo _project] (parse-scope scope)]
    (-> (try-tokens-with-fallback
         (github-oauth/get-all-tokens)
         (fn [token-record]
           (gh-api/list-prs-for-poller (:access-token token-record) owner repo {})))
        (.then (fn [outcome]
                 (if (:success outcome)
                   (let [{:keys [prs truncated]} (:result outcome)]
                     {:success true :prs prs :truncated (boolean truncated)})
                   (select-keys outcome [:success :error])))))
    (js/Promise.resolve {:success false :error (str "invalid scope: " scope)})))

(defn resolve-merge-status
  "Determine whether a closed PR was merged. Returns a Promise of boolean.
   Never rejects. `defn` so tests can stub."
  [scope pr-number]
  (if-let [[owner repo _project] (parse-scope scope)]
    (-> (try-tokens-with-fallback
         (github-oauth/get-all-tokens)
         (fn [token-record]
           (gh-api/get-pr-merge-status (:access-token token-record)
                                       owner repo pr-number)))
        (.then (fn [outcome]
                 (boolean (:result outcome))))
        (.catch (fn [_] false)))
    (js/Promise.resolve false)))

(defn- resolve-closed-event-merge-flags
  "For each :pr-closed event, fetch the merge status and update
   :details. `closed-event` always stamps `:details {:merged nil}` so
   we don't need a nil-guard — every `:pr-closed` goes through the
   merge-status resolver exactly once. Returns a Promise of the updated
   events vector."
  [scope events]
  (let [updates (mapv (fn [ev]
                        (if (= :pr-closed (:event-type ev))
                          (-> (resolve-merge-status scope (:pr-number ev))
                              (.then (fn [merged?]
                                       (assoc-in ev [:details :merged] merged?))))
                          (js/Promise.resolve ev)))
                      events)]
    (-> (js/Promise.all (into-array updates))
        (.then (fn [js-arr]
                 (vec (array-seq js-arr)))))))

(defn- run-poll-cycle!
  "The body of a single poll cycle. Assumed to be called only from
   `poll-once!`, which serializes concurrent calls per scope via the
   in-flight promise chain.

   Short-circuits on shutdown AFTER the fetch resolves: a poll cycle
   that started before `shutdown-all!` should not publish events or
   write cache after shutdown. Without this guard, an in-flight cycle
   that overlaps shutdown can leave a `{:cache populated, :timer nil,
   :in-flight nil, :generation N}` entry stranded in `poller-state`
   (the install! shutdown branch's generation guard would miss it
   because `stop-all-pollers!` bumped the generation while the fetch
   was in flight). Also plugs the related issue that `append-events!`
   accepts appends after shutdown."
  [scope]
  (let [[_ _ project] (parse-scope scope)]
    (-> (fetch-poller-prs scope)
        (.then (fn [result]
                 (cond
                   (shutdown?)
                   (do
                     (swap! poller-state dissoc scope)
                     (js/Promise.resolve nil))

                   (not (:success result))
                   (do
                     (js/console.warn
                      (str "[github-events] poll failed for " scope ": "
                           (:error result)))
                     (js/Promise.resolve nil))

                   :else
                   (let [current-state (or (get @poller-state scope)
                                           (empty-poller-state))
                         old-cache (:cache current-state)
                         truncated? (boolean (:truncated result))
                         _ (when truncated?
                             (js/console.warn
                              (str "[github-events] PR list for " scope
                                   " truncated (>100 open PRs); "
                                   ":pr-closed detection narrowed to the "
                                   "updatedAt window.")))
                         {:keys [events new-cache]}
                         (detect-events project old-cache (:prs result) truncated?)]
                     (-> (resolve-closed-event-merge-flags scope events)
                         (.then (fn [resolved-events]
                                  (if (shutdown?)
                                    (do (swap! poller-state dissoc scope)
                                        nil)
                                    (do
                                      (swap! poller-state update scope
                                             (fn [state]
                                               (assoc (or state (empty-poller-state))
                                                      :cache new-cache)))
                                      (when (seq resolved-events)
                                        (event-stream/append-events! scope resolved-events))
                                      nil)))))))))
        (.catch (fn [err]
                  (js/console.warn
                   (str "[github-events] poll cycle threw for " scope ": "
                        (or (.-message err) (str err))))
                  nil)))))

(defn poll-once!
  "Run a single poll cycle for `scope`. Serialized per-scope via an in-flight
   promise chain stored in `poller-state`. Returns a Promise that resolves
   when the caller's cycle completes. Never rejects."
  [scope]
  (let [chained (atom nil)]
    (swap! poller-state update scope
           (fn [state]
             (let [base (or state (empty-poller-state))
                   prev (or (:in-flight base) (js/Promise.resolve nil))
                   next-p (.then prev
                                 (fn [_] (run-poll-cycle! scope))
                                 (fn [_] (run-poll-cycle! scope)))]
               (reset! chained next-p)
               (assoc base :in-flight next-p))))
    (-> @chained
        (.then (fn [_]
                 ;; Use `update-when-present` semantics — if `run-poll-cycle!`
                 ;; dissoc'd the scope (shutdown short-circuit), don't
                 ;; resurrect it via `update`'s nil-key behavior.
                 (swap! poller-state
                        (fn [states]
                          (let [state (get states scope)]
                            (cond
                              (nil? state) states
                              (identical? (:in-flight state) @chained)
                              (assoc states scope (assoc state :in-flight nil))
                              :else states)))))))
    @chained))

;; ============================================================================
;; Lifecycle: lazy start, grace-period stop
;; ============================================================================

(declare schedule-next-tick!)

(defn- grace-period-elapsed? [state]
  (let [grace (config/github-poller-grace-ms)
        now (js/Date.now)]
    (> (- now (:last-activity state)) grace)))

(defn- entry-reclaimable?
  "A stopped poller entry is safe to reclaim if it has no timer, no
   in-flight poll, and no cache. We intentionally keep entries while
   `:cache` is populated so a grace-period reconnect can still diff
   against the previous state."
  [state]
  (and state
       (nil? (:timer state))
       (nil? (:in-flight state))
       (nil? (:cache state))))

(defn stop-poller!
  "Cancel any scheduled timer for `scope` and invalidate any in-flight
   startup by bumping the generation counter. Does not clear the cache
   (a grace-period reconnect can still diff against it). If the resulting
   entry is fully empty, the whole entry is removed to bound memory growth."
  [scope]
  (swap! poller-state
         (fn [states]
           (let [state (get states scope)]
             (if-not state
               states
               (let [t (:timer state)]
                 (when (and t (not (keyword? t)))
                   (js/clearTimeout t))
                 (let [stopped (-> state
                                   (assoc :timer nil)
                                   (update :generation (fnil inc 0)))]
                   (if (entry-reclaimable? stopped)
                     (dissoc states scope)
                     (assoc states scope stopped)))))))))

(defn tick!
  "One iteration of the poll loop for `scope`. Polls, then either stops
   (if the grace period has elapsed) or reschedules.

   Captures the scope's generation at entry. If `stop-poller!` (which bumps
   the generation) runs while the poll is in flight, the post-poll `.then`
   sees the mismatch and aborts the reschedule."
  [scope]
  (let [entry-gen (:generation (get @poller-state scope) 0)]
    (-> (poll-once! scope)
        (.then (fn [_]
                 (let [state (get @poller-state scope)]
                   (cond
                     (nil? state) nil
                     (shutdown?) nil
                     (not= entry-gen (:generation state 0)) nil
                     (grace-period-elapsed? state) (stop-poller! scope)
                     :else (schedule-next-tick! scope))))))))

(defn- schedule-next-tick! [scope]
  (when-not (shutdown?)
    (let [current-gen (:generation (get @poller-state scope) 0)
          interval (config/github-poll-interval-ms)
          timer (js/setTimeout (fn [] (tick! scope)) interval)
          [old-states new-states]
          (swap-vals! poller-state update scope
                      (fn [state]
                        (let [base (or state (empty-poller-state))]
                          (if (= current-gen (:generation base 0))
                            (assoc base :timer timer)
                            base))))
          prev-timer (get-in old-states [scope :timer])
          installed? (identical? timer (get-in new-states [scope :timer]))]
      (if installed?
        (when (and prev-timer (not (keyword? prev-timer)))
          (js/clearTimeout prev-timer))
        (js/clearTimeout timer)))))

(defn- touch-last-activity! [scope]
  (swap! poller-state update scope
         (fn [state]
           (-> (or state (empty-poller-state))
               (assoc :last-activity (js/Date.now))))))

(defn start-poller-if-needed!
  "Start the poller for `scope` if not already running. Uses `swap-vals!`
   compare-and-set semantics so only the caller that atomically transitions
   `:timer nil → :starting` kicks off the baseline poll — concurrent
   callers see `:timer :starting` (or a real handle) and no-op.

   After the global shutdown flag is set, this is a no-op. The shutdown
   check is repeated INSIDE the `swap-vals!` function so a concurrent
   `event-stream/shutdown-all!` that sets the flag between our entry
   check and the CAS cannot leak a `:starting` entry into
   `poller-state`. The post-swap reconciliation also cleans up any
   entry that slipped through (belt-and-suspenders for the case where
   shutdown fires after the swap commits but before `install!` runs)."
  [scope]
  (when-not (shutdown?)
    (let [[old-states new-states]
          (swap-vals! poller-state update scope
                      (fn [s]
                        (let [base (or s (empty-poller-state))]
                          (if (and (nil? (:timer base)) (not (shutdown?)))
                            (assoc base :timer :starting)
                            base))))
          old-timer (get-in old-states [scope :timer])
          new-entry (get new-states scope)]
      (cond
        ;; We did not win the CAS — another caller is already starting
        ;; (or running) the poller. Nothing to do.
        (not (and (nil? old-timer) (= :starting (:timer new-entry))))
        nil

        ;; We won the CAS but shutdown already fired. Drop the
        ;; `:starting` marker we just wrote so `stop-all-pollers!`
        ;; doesn't leave a zombie entry behind.
        (shutdown?)
        (swap! poller-state dissoc scope)

        :else
        (let [captured-gen (:generation new-entry 0)
              install! (fn []
                         (let [current (get @poller-state scope)]
                           (cond
                             ;; Shutdown fired mid-poll. Drop the entry
                             ;; outright — `entry-reclaimable?` keeps
                             ;; entries alive while `:cache` is non-nil
                             ;; (so a grace-period reconnect can still
                             ;; diff against it), but a shutdown means
                             ;; the process is going down and there's
                             ;; nothing to reconnect to.
                             (shutdown?)
                             (when (and current
                                        (= captured-gen (:generation current 0)))
                               (swap! poller-state dissoc scope))

                             (and current
                                  (= captured-gen (:generation current 0)))
                             (schedule-next-tick! scope))))]
          (.then (poll-once! scope)
                 (fn [_] (install!))
                 (fn [_] (install!))))))))

(defn stop-all-pollers!
  "Stop every running poller. Called from `shutdown-all!` after the global
   shutdown flag is set; also available for explicit cleanup."
  []
  (doseq [scope (keys @poller-state)]
    (stop-poller! scope)))

;; ============================================================================
;; Public wait entry point
;; ============================================================================

(defn wait-for-scope!
  "Public entry point for `wait_for_event` when the scope is a github scope.
   Touches last-activity (grace timer), ensures the poller is running, then
   delegates the blocking wait to `event-stream/wait-for-event`.

   Assumes the caller (the MCP dispatcher) has already validated `scope`
   against `sessions/parse-project` — passing an invalid scope is a
   programming error and will surface as a rejected Promise from
   `event-stream/wait-for-event`."
  [scope opts]
  (assert (parse-scope scope)
          (str "wait-for-scope!: invalid github scope: " (pr-str scope)))
  (when-not (shutdown?)
    (touch-last-activity! scope)
    (start-poller-if-needed! scope))
  (event-stream/wait-for-event (assoc opts :scope scope)))

;; ============================================================================
;; Test helpers
;; ============================================================================

(defn reset-for-tests!
  "Stop all pollers and clear state. Test-only."
  []
  (stop-all-pollers!)
  (reset! poller-state {}))
