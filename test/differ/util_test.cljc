(ns differ.util-test
  "Tests for utility functions."
  (:require [clojure.test :refer [deftest testing is are]]
            [differ.util :as util]))

;; ============================================================================
;; sha256-hex tests
;; ============================================================================

(deftest sha256-hex-test
  (testing "produces consistent hash for same input"
    (is (= (util/sha256-hex "hello")
           (util/sha256-hex "hello"))))

  (testing "produces different hashes for different inputs"
    (is (not= (util/sha256-hex "hello")
              (util/sha256-hex "world"))))

  (testing "produces valid 64-character hex string"
    (let [hash (util/sha256-hex "test")]
      (is (string? hash))
      (is (= 64 (count hash)))
      (is (re-matches #"[0-9a-f]+" hash))))

  (testing "handles empty string"
    (let [hash (util/sha256-hex "")]
      (is (= 64 (count hash)))))

  (testing "handles nil input"
    (let [hash (util/sha256-hex nil)]
      (is (= 64 (count hash)))
      ;; nil should be treated as empty string
      (is (= (util/sha256-hex nil) (util/sha256-hex ""))))))

;; ============================================================================
;; session-id tests
;; ============================================================================

(deftest session-id-test
  (testing "produces consistent ID for same project+branch"
    (is (= (util/session-id "my-project" "main")
           (util/session-id "my-project" "main"))))

  (testing "produces different IDs for different projects"
    (is (not= (util/session-id "project-a" "main")
              (util/session-id "project-b" "main"))))

  (testing "produces different IDs for different branches"
    (is (not= (util/session-id "project" "main")
              (util/session-id "project" "feature"))))

  (testing "is a valid SHA-256 hash"
    (let [id (util/session-id "project" "branch")]
      (is (= 64 (count id)))
      (is (re-matches #"[0-9a-f]+" id)))))

;; ============================================================================
;; gen-uuid tests
;; ============================================================================

(deftest gen-uuid-test
  (testing "generates a string"
    (is (string? (util/gen-uuid))))

  (testing "generates unique values"
    (let [uuids (repeatedly 100 util/gen-uuid)]
      (is (= 100 (count (set uuids))))))

  (testing "generates valid UUID format"
    (let [uuid (util/gen-uuid)]
      ;; UUID v4 format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
      (is (re-matches #"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" uuid)))))

;; ============================================================================
;; kebab->snake tests
;; ============================================================================

(deftest kebab->snake-test
  (testing "converts single hyphen"
    (is (= "foo_bar" (util/kebab->snake :foo-bar))))

  (testing "converts multiple hyphens"
    (is (= "foo_bar_baz" (util/kebab->snake :foo-bar-baz))))

  (testing "handles no hyphens"
    (is (= "foobar" (util/kebab->snake :foobar))))

  (testing "handles empty keyword"
    (is (= "" (util/kebab->snake (keyword ""))))))

;; ============================================================================
;; snake->kebab tests
;; ============================================================================

(deftest snake->kebab-test
  (testing "converts single underscore"
    (is (= :foo-bar (util/snake->kebab "foo_bar"))))

  (testing "converts multiple underscores"
    (is (= :foo-bar-baz (util/snake->kebab "foo_bar_baz"))))

  (testing "handles no underscores"
    (is (= :foobar (util/snake->kebab "foobar"))))

  (testing "handles empty string"
    (is (= (keyword "") (util/snake->kebab "")))))

;; ============================================================================
;; kebab<->snake roundtrip tests
;; ============================================================================

(deftest case-conversion-roundtrip-test
  (testing "roundtrip preserves value"
    (are [kw] (= kw (util/snake->kebab (util/kebab->snake kw)))
      :foo
      :foo-bar
      :foo-bar-baz
      :my-long-keyword-name)))

;; ============================================================================
;; now-iso tests
;; ============================================================================

(deftest now-iso-test
  (testing "returns a string"
    (is (string? (util/now-iso))))

  (testing "returns ISO-8601 format"
    (let [ts (util/now-iso)]
      ;; Should match ISO format: YYYY-MM-DDTHH:MM:SS.sssZ
      (is (re-matches #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.*" ts))))

  (testing "returns recent timestamp"
    (let [ts (util/now-iso)
          now-year #?(:clj (subs (.toString (java.time.Instant/now)) 0 4)
                      :cljs (subs (.toISOString (js/Date.)) 0 4))]
      (is (= now-year (subs ts 0 4))))))

;; ============================================================================
;; expires-at tests
;; ============================================================================

(deftest expires-at-test
  (testing "returns a string"
    (is (string? (util/expires-at 3600))))

  (testing "returns ISO-8601 format"
    (let [ts (util/expires-at 3600)]
      (is (re-matches #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.*" ts))))

  (testing "is in the future"
    (let [now #?(:clj (.toEpochMilli (java.time.Instant/now))
                 :cljs (.now js/Date))
          expires (util/expires-at 3600)
          expires-ms #?(:clj (.toEpochMilli (java.time.Instant/parse expires))
                        :cljs (.getTime (js/Date. expires)))]
      (is (> expires-ms now)))))

;; ============================================================================
;; expired? tests
;; ============================================================================

(deftest expired?-test
  (testing "returns true for past timestamp"
    (is (util/expired? "2020-01-01T00:00:00.000Z")))

  (testing "returns false for future timestamp"
    (is (not (util/expired? "2099-01-01T00:00:00.000Z"))))

  (testing "correctly handles expires-at output"
    (let [future-ts (util/expires-at 3600)]
      (is (not (util/expired? future-ts))))))

;; ============================================================================
;; gen-token tests
;; ============================================================================

(deftest gen-token-test
  (testing "generates a string"
    (is (string? (util/gen-token))))

  (testing "generates unique tokens"
    (let [tokens (repeatedly 100 util/gen-token)]
      (is (= 100 (count (set tokens))))))

  (testing "includes prefix when provided"
    (let [token (util/gen-token "prefix-")]
      (is (clojure.string/starts-with? token "prefix-"))))

  (testing "prefix appears at start"
    (let [token (util/gen-token "test_")]
      (is (= "test_" (subs token 0 5))))))
