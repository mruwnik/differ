(ns differ.event-stream
  "Central in-memory event log with monotonic seq and blocking wait.
   Scope-keyed: each scope has its own ring buffer, sequence counter,
   and subscribers. Producers append events for a given scope; consumers
   (the `wait_for_event` MCP tool) read events with a cursor and block
   until new ones arrive.

   Scopes are namespaced strings:
     'github:owner/repo'     — GitHub PR poller events
     'session:<session-id>'  — differ session events (comments, files, reviews)

   This module is source-agnostic: it knows nothing about GitHub polling
   or session lifecycles. Producers (`differ.github-events`,
   `differ.session-events`) own their own state and call `append-events!`
   here when they have something to publish.

   Reclamation policy: scope entries are reclaimed from `scopes-state`
   whenever `drain-subscribers!` leaves them with no buffered events and
   no remaining subscribers. Producers can also call `release-scope!`
   explicitly when their side is definitively done. `:last-touched` is
   updated on every append and on wait entry so external reclaimers can
   add age-based policies if ever needed.

   Scope validation: `append-events!` and `wait-for-event` both reject
   nil / blank / non-string scope at the boundary — no producer can
   leak a zombie scope entry by passing in a bad key.

   Note: `:next-seq` is a JS number. At the safe integer limit
   (2^53 - 1), even at 1000 events/poll on a 30s interval, overflow is
   on the order of 8500 years — no rollover handling needed."
  (:require [clojure.string :as str]
            [differ.config :as config]))

;; ============================================================================
;; State
;; ============================================================================

;; Module-level scope state. Not `^:private` because sibling producer
;; namespaces (and tests) touch it directly; treat it as module-internal
;; anyway — public consumers should use `append-events!` / `wait-for-event`.
(defonce scopes-state (atom {}))

;; Global shutdown flag. Once set, no new waits are accepted and all
;; currently-pending waits drain as timed-out. Reset by `reset-for-tests!`.
(defonce shutdown? (atom false))

(defn empty-scope-state []
  {:events      []
   :next-seq    1
   :subscribers []
   :last-touched 0})

(defn get-scope-state
  "Return the current state for `scope`, or nil if unknown. Read-only
   debugging accessor — usable from a REPL or from tests; production
   code should consume the stream via `wait-for-event` / `events-since`."
  [scope]
  (get @scopes-state scope))

(defn shutdown?-value
  "Return the current shutdown flag value. Read by sibling producer
   namespaces (e.g. `differ.github-events`) that need to know when the
   event stream has stopped accepting new waits."
  []
  @shutdown?)

;; ============================================================================
;; Timestamps
;; ============================================================================

(defn- iso-now []
  (.toISOString (js/Date.)))

(defn- valid-scope?
  "A scope must be a non-blank string. Validated at every public entry
   point so no producer or consumer can create a zombie `nil` / `\"\"`
   entry in `scopes-state`."
  [scope]
  (and (string? scope) (not (str/blank? scope))))

;; ============================================================================
;; Ring buffer
;; ============================================================================

(defn- trim-to-capacity
  "Trim a vector of events down to `capacity` (keeping the tail). Throws
   if `capacity` isn't a positive integer — CLJS silently coerces nil to 0
   in arithmetic comparisons, so a nil capacity (e.g. from a stale
   defonce config-cache that doesn't have the new `:event-stream` block
   yet) would otherwise drop every event without any error and the bug
   would only manifest as `wait_for_event` returning empty forever.
   Discovered live in production after a config rename — see commit
   that introduced this guard."
  [events capacity]
  (when-not (and (integer? capacity) (pos? capacity))
    (throw (ex-info "trim-to-capacity: capacity must be a positive integer"
                    {:capacity capacity})))
  (if (<= (count events) capacity)
    events
    (vec (subvec events (- (count events) capacity)))))

(defn- append-events-to-state
  "Pure updater: append `new-events` (each without :seq / :received-at) to
   the scope state, assigning monotonic seqs and current timestamp,
   trimming to the configured buffer capacity. Returns updated state."
  [state new-events capacity now-iso scope]
  (let [start-seq (:next-seq state)
        stamped (map-indexed
                 (fn [idx ev]
                   (assoc ev
                          :seq (+ start-seq idx)
                          :received-at now-iso
                          :scope scope))
                 new-events)
        all-events (trim-to-capacity (into (:events state) stamped) capacity)]
    (assoc state
           :events all-events
           :next-seq (+ start-seq (count new-events))
           :last-touched (js/Date.now))))

(declare drain-subscribers!)

(defn append-events!
  "Append events to `scope`'s log. Each event must be a map without :seq
   or :received-at — those are assigned here, along with :scope. After
   the append, any pending subscribers for this scope are woken.

   Throws if `scope` is not a non-blank string — this is a boundary that
   every producer must uphold, otherwise nil/\"\" entries pile up in
   `scopes-state` forever and no consumer can ever read them.

   Returns the assigned seq numbers in order as a vector. If the caller
   appends more events than fit in the ring buffer, the oldest returned
   seqs will no longer be present in `:events`."
  [scope new-events]
  (when-not (valid-scope? scope)
    (throw (ex-info "append-events!: scope must be a non-blank string"
                    {:scope scope})))
  (if (empty? new-events)
    ;; No-op for empty appends — do not create a scope entry just to
    ;; record an empty append. This keeps `release-scope!` semantics
    ;; consistent (a scope that has never received an event should not
    ;; appear in `scopes-state`).
    []
    (let [capacity (config/event-buffer-size)
          now-iso (iso-now)
          [old-states _new-states]
          (swap-vals! scopes-state update scope
                      (fn [state]
                        (append-events-to-state (or state (empty-scope-state))
                                                new-events capacity now-iso scope)))
          start-seq (:next-seq (or (get old-states scope) (empty-scope-state)))
          assigned (vec (range start-seq (+ start-seq (count new-events))))]
      (drain-subscribers! scope)
      assigned)))

(defn events-since
  "Return up to `max-events` events from `scope`'s log whose :seq is greater
   than `since-seq`, in seq order. If `since-seq` falls below the oldest
   buffered event (caller fell off the tail of the ring buffer), logs a
   warning and returns from the oldest buffered event — the caller may
   detect this via :seq gaps."
  [scope since-seq max-events]
  (let [events (:events (get @scopes-state scope) [])
        ;; Events are in monotonic seq order, so drop-while is O(skipped)
        ;; rather than O(n) on a 10k+ buffer.
        unread (drop-while #(<= (:seq %) since-seq) events)]
    (when (and (seq events)
               (seq unread)
               (> (:seq (first events)) (inc since-seq)))
      (js/console.warn
       (str "[event-stream] caller " scope " since-seq=" since-seq
            " fell off the ring buffer tail (oldest buffered seq="
            (:seq (first events)) "); events missed.")))
    (vec (take max-events unread))))

;; ============================================================================
;; Subscribers
;; ============================================================================

(defn- register-subscriber!
  "Add a resolver fn to a scope's subscriber list."
  [scope resolver]
  (swap! scopes-state update scope
         (fn [state]
           (-> (or state (empty-scope-state))
               (update :subscribers conj resolver)))))

(declare reclaim-if-empty)

(defn- remove-subscriber!
  "Remove a resolver from a scope's subscriber list (used on timeout).
   Reclaims the scope entry if no events and no other subscribers
   remain — otherwise a subscriber that times out on a never-populated
   scope would leave a zombie entry in `scopes-state`."
  [scope resolver]
  (swap! scopes-state
         (fn [states]
           (let [state (get states scope)]
             (if-not state
               states
               (let [pruned (update state :subscribers
                                    (fn [subs]
                                      (vec (remove #(identical? % resolver) subs))))]
                 (if-let [kept (reclaim-if-empty pruned)]
                   (assoc states scope kept)
                   (dissoc states scope))))))))

(defn- reclaim-if-empty
  "If `state` has no events and no subscribers, return nil (caller will
   dissoc the scope). Otherwise return `state` unchanged. Keeps
   `scopes-state` from growing monotonically as ephemeral scopes come
   and go."
  [state]
  (when (and state
             (or (seq (:events state))
                 (seq (:subscribers state))))
    state))

(defn- drain-subscribers!
  "Pop all subscriber resolvers for a scope and call each one. If the
   scope has no buffered events and no remaining subscribers after the
   drain, reclaim the entry."
  [scope]
  (let [resolvers (atom nil)]
    (swap! scopes-state
           (fn [states]
             (let [state (get states scope)]
               (if-not state
                 states
                 (do
                   (reset! resolvers (:subscribers state))
                   (let [drained (assoc state :subscribers [])]
                     (if-let [kept (reclaim-if-empty drained)]
                       (assoc states scope kept)
                       (dissoc states scope))))))))
    (doseq [r @resolvers]
      (r))))

(defn drain-all-subscribers!
  "Drain every scope's pending subscribers. Called during shutdown after
   the flag is set — pending waits will re-read the buffer, see nothing
   new, and resolve with :timed-out true."
  []
  (doseq [scope (keys @scopes-state)]
    (when (seq (:subscribers (get @scopes-state scope)))
      (drain-subscribers! scope))))

(defn release-scope!
  "Remove a scope entry from the event stream if it has no events and
   no subscribers. Producers (github-events, session-events) call this
   when their side of the stream is definitively done (e.g. after a
   session is deleted). Safe no-op if the scope still has content."
  [scope]
  (swap! scopes-state
         (fn [states]
           (let [state (get states scope)]
             (if (reclaim-if-empty state)
               states
               (dissoc states scope))))))

;; ============================================================================
;; Blocking wait
;; ============================================================================

(defn- build-wait-result
  "Compute {:events :next-seq :timed-out} for a wait call."
  [scope since-seq max-events timed-out?]
  (let [events (events-since scope since-seq max-events)
        highest-seq (if (seq events)
                      (:seq (last events))
                      since-seq)]
    {:events events
     :next-seq highest-seq
     :timed-out timed-out?}))

(defn- touch-scope!
  "Stamp `:last-touched` on an existing scope state. Must NOT use
   `update`, which would call its updater with `nil` for a missing
   key and then re-associate the result — that would leak a
   `{scope nil}` entry on every peek (`timeout-ms 0`) against a
   never-populated scope."
  [scope]
  (swap! scopes-state
         (fn [states]
           (if (contains? states scope)
             (update states scope assoc :last-touched (js/Date.now))
             states))))

(defn wait-for-event
  "Block (via Promise) until new events arrive for `scope` past `since-seq`,
   or until `timeout-ms` elapses.

   Options map:
     :scope       required, non-blank string (e.g. 'github:owner/repo' or 'session:<id>')
     :since-seq   default 0  — return all buffered events
     :timeout-ms  default 300000 (5 min) — 0 means 'peek', return immediately
     :max-events  default 50, capped at 500

   Returns a Promise of {:events [...], :next-seq int, :timed-out bool}.
   After `shutdown-all!` has been invoked, every call resolves with a
   timed-out envelope immediately.

   Producer-side lifecycle (starting a poller, touching activity counters,
   etc.) is the caller's responsibility — do that in your producer wrapper
   before delegating here. `wait-for-event` is source-agnostic."
  [{:keys [scope since-seq timeout-ms max-events]
    :or   {since-seq 0 timeout-ms 300000 max-events 50}}]
  (cond
    (not (valid-scope? scope))
    (js/Promise.reject
     (ex-info "wait-for-event: scope must be a non-blank string"
              {:scope scope}))

    @shutdown?
    (js/Promise.resolve (build-wait-result scope since-seq (min max-events 500) true))

    :else
    (let [capped-max (min max-events 500)
          _ (touch-scope! scope)
          buffered (events-since scope since-seq capped-max)]
      (cond
        (seq buffered)
        (js/Promise.resolve (build-wait-result scope since-seq capped-max false))

        (zero? timeout-ms)
        (js/Promise.resolve (build-wait-result scope since-seq capped-max true))

        :else
        (js/Promise.
         (fn [resolve _reject]
           (let [settled? (atom false)
                 ;; Forward-declared timer cell so an immediate drain
                 ;; still sees the handle to clearTimeout.
                 timer-cell (atom nil)
                 resolver (fn []
                            (when (compare-and-set! settled? false true)
                              (when-let [t @timer-cell]
                                (js/clearTimeout t))
                              (let [buf (events-since scope since-seq capped-max)]
                                (resolve
                                 (build-wait-result scope since-seq capped-max
                                                    (empty? buf))))))]
             (reset! timer-cell
                     (js/setTimeout
                      (fn []
                        (when (compare-and-set! settled? false true)
                          (remove-subscriber! scope resolver)
                          (resolve
                           (build-wait-result scope since-seq capped-max true))))
                      timeout-ms))
             (register-subscriber! scope resolver))))))))

;; ============================================================================
;; Shutdown
;; ============================================================================

(defn shutdown-all!
  "Block new waits and drain all pending subscribers. Producers should also
   stop their own machinery (e.g. via `github-events/stop-all-pollers!`)."
  []
  (reset! shutdown? true)
  (drain-all-subscribers!))

(defn reset-shutdown!
  "Clear the global shutdown flag so the event stream accepts new waits
   again. Called from `differ.main/start` on (re)start — `defonce` keeps
   `shutdown?` alive across `^:dev/before-load` → `^:dev/after-load`
   reload cycles, so without this the flag would stay true forever after
   the first hot-reload and polling would silently break."
  []
  (reset! shutdown? false))

;; ============================================================================
;; Test helpers
;; ============================================================================

(defn reset-for-tests!
  "Clear all scope state and the shutdown flag. Test-only."
  []
  (reset! shutdown? false)
  (reset! scopes-state {}))
