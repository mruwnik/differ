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
