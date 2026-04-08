(ns differ.config-test
  "Tests for configuration module."
  (:require [clojure.test :refer [deftest testing is]]
            [differ.config :as config]))

;; ============================================================================
;; defaults tests
;; ============================================================================

(deftest defaults-test
  (testing "defaults contains expected keys"
    (is (contains? config/defaults :port))
    (is (contains? config/defaults :large-file-threshold))
    (is (contains? config/defaults :line-count-threshold))
    (is (contains? config/defaults :context-expand-size))
    (is (contains? config/defaults :watcher-debounce-ms)))

  (testing "defaults values are reasonable types"
    (is (number? (:port config/defaults)))
    (is (number? (:large-file-threshold config/defaults)))
    (is (number? (:line-count-threshold config/defaults)))
    (is (number? (:context-expand-size config/defaults)))
    (is (number? (:watcher-debounce-ms config/defaults)))
    (is (pos? (:port config/defaults)))
    (is (pos? (:large-file-threshold config/defaults))))

  (testing "default port is in valid range"
    (let [port (:port config/defaults)]
      (is (>= port 1024))
      (is (<= port 65535)))))

;; ============================================================================
;; Event stream / GitHub poller config tests
;; ============================================================================

(deftest event-stream-defaults-test
  (testing "defaults contain :event-stream and :github-poller blocks"
    (is (contains? config/defaults :event-stream))
    (is (contains? config/defaults :github-poller))
    (let [es (:event-stream config/defaults)
          gh (:github-poller config/defaults)]
      (is (= 10000 (:buffer-size es)))
      (is (= 30000 (:poll-interval-ms gh)))
      (is (= 300000 (:poller-grace-ms gh))))))

(deftest event-buffer-size-returns-positive-int-test
  ;; Regression: a stale `defonce config-cache` from before the
  ;; `:event-stream` rename caused `event-buffer-size` to return nil
  ;; in the running server, which then crashed `trim-to-capacity`
  ;; silently and made `wait_for_event` look broken. Pin the contract.
  (testing "event-buffer-size returns a positive integer for a fresh config"
    (let [orig (aget js/process.env "DIFFER_EVENT_BUFFER_SIZE")]
      (try
        (js-delete js/process.env "DIFFER_EVENT_BUFFER_SIZE")
        (config/reload!)
        (let [v (config/event-buffer-size)]
          (is (integer? v) (str "expected integer, got " (pr-str v)))
          (is (pos? v) (str "expected positive, got " (pr-str v))))
        (finally
          (when orig (aset js/process.env "DIFFER_EVENT_BUFFER_SIZE" orig))
          (config/reload!))))))

(deftest github-poll-interval-ms-test
  (testing "returns the config value when env var is absent"
    (let [orig (aget js/process.env "DIFFER_GITHUB_POLL_INTERVAL_MS")]
      (try
        (js-delete js/process.env "DIFFER_GITHUB_POLL_INTERVAL_MS")
        (with-redefs [config/get-config
                      (fn [] {:github-poller {:poll-interval-ms 45000}})]
          (is (= 45000 (config/github-poll-interval-ms))))
        (finally
          (when orig (aset js/process.env "DIFFER_GITHUB_POLL_INTERVAL_MS" orig)))))))

(deftest github-poll-interval-env-override-test
  (testing "DIFFER_GITHUB_POLL_INTERVAL_MS env var overrides config"
    (let [orig (aget js/process.env "DIFFER_GITHUB_POLL_INTERVAL_MS")]
      (try
        (aset js/process.env "DIFFER_GITHUB_POLL_INTERVAL_MS" "5000")
        (with-redefs [config/get-config
                      (fn [] {:github-poller {:poll-interval-ms 30000}})]
          (is (= 5000 (config/github-poll-interval-ms))))
        (finally
          (if orig
            (aset js/process.env "DIFFER_GITHUB_POLL_INTERVAL_MS" orig)
            (js-delete js/process.env "DIFFER_GITHUB_POLL_INTERVAL_MS")))))))

(deftest github-poller-grace-ms-test
  (testing "returns config value and honors env override"
    (with-redefs [config/get-config
                  (fn [] {:github-poller {:poller-grace-ms 120000}})]
      (let [orig (aget js/process.env "DIFFER_GITHUB_POLLER_GRACE_MS")]
        (try
          (js-delete js/process.env "DIFFER_GITHUB_POLLER_GRACE_MS")
          (is (= 120000 (config/github-poller-grace-ms)))
          (aset js/process.env "DIFFER_GITHUB_POLLER_GRACE_MS" "99")
          (is (= 99 (config/github-poller-grace-ms)))
          (finally
            (if orig
              (aset js/process.env "DIFFER_GITHUB_POLLER_GRACE_MS" orig)
              (js-delete js/process.env "DIFFER_GITHUB_POLLER_GRACE_MS"))))))))

(deftest event-buffer-size-test
  (testing "returns config value and honors env override"
    (with-redefs [config/get-config
                  (fn [] {:event-stream {:buffer-size 2048}})]
      (let [orig (aget js/process.env "DIFFER_EVENT_BUFFER_SIZE")]
        (try
          (js-delete js/process.env "DIFFER_EVENT_BUFFER_SIZE")
          (is (= 2048 (config/event-buffer-size)))
          (aset js/process.env "DIFFER_EVENT_BUFFER_SIZE" "50")
          (is (= 50 (config/event-buffer-size)))
          (finally
            (if orig
              (aset js/process.env "DIFFER_EVENT_BUFFER_SIZE" orig)
              (js-delete js/process.env "DIFFER_EVENT_BUFFER_SIZE"))))))))

;; ============================================================================
;; client-config-keys tests
;; ============================================================================

(deftest client-config-keys-test
  (testing "client-config-keys is a vector"
    (is (vector? config/client-config-keys)))

  (testing "client-config-keys contains expected keys"
    (is (some #{:large-file-threshold} config/client-config-keys))
    (is (some #{:line-count-threshold} config/client-config-keys))
    (is (some #{:context-expand-size} config/client-config-keys)))

  (testing "client-config-keys excludes server-only keys"
    (is (not (some #{:port} config/client-config-keys)))
    (is (not (some #{:watcher-debounce-ms} config/client-config-keys)))))

;; ============================================================================
;; client-config tests
;; ============================================================================

(deftest client-config-test
  (testing "client-config returns a map"
    (is (map? (config/client-config))))

  (testing "client-config contains only client-safe keys"
    (let [cc (config/client-config)]
      (is (every? #(some #{%} config/client-config-keys) (keys cc)))))

  (testing "client-config excludes server keys"
    (let [cc (config/client-config)]
      (is (not (contains? cc :port)))
      (is (not (contains? cc :watcher-debounce-ms))))))

;; ============================================================================
;; get-config tests
;; ============================================================================

(deftest get-config-test
  (testing "get-config returns a map"
    (is (map? (config/get-config))))

  (testing "get-config includes defaults"
    (let [cfg (config/get-config)]
      (is (contains? cfg :port))
      (is (contains? cfg :large-file-threshold)))))

;; ============================================================================
;; get-value tests
;; ============================================================================

(deftest get-value-test
  (testing "get-value returns known config values"
    (is (number? (config/get-value :port)))
    (is (number? (config/get-value :large-file-threshold))))

  (testing "get-value returns nil for unknown keys"
    (is (nil? (config/get-value :nonexistent-key)))))
