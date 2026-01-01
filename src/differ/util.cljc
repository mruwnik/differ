(ns differ.util
  "Shared utility functions."
  (:require [clojure.string :as str])
  #?(:clj (:import [java.security MessageDigest]
                   [java.time Instant])
     :cljs (:require ["crypto" :as crypto])))

(defn gen-uuid
  "Generate a random UUID string."
  []
  #?(:clj (str (java.util.UUID/randomUUID))
     :cljs (str (random-uuid))))

(defn now-iso
  "Returns current time as ISO-8601 string."
  []
  #?(:clj (.toString (Instant/now))
     :cljs (.toISOString (js/Date.))))

(defn sha256-hex
  "Returns SHA-256 hash of string as hex."
  [s]
  #?(:clj (let [md (MessageDigest/getInstance "SHA-256")
                bytes (.digest md (.getBytes (or s "") "UTF-8"))]
            (apply str (map #(format "%02x" %) bytes)))
     :cljs (-> (crypto/createHash "sha256")
               (.update (or s ""))
               (.digest "hex"))))

(defn session-id
  "Generate deterministic session ID from project and branch."
  [project branch]
  (sha256-hex (str project "|" branch)))

(defn kebab->snake
  "Convert kebab-case keyword to snake_case string."
  [k]
  (-> (name k)
      (str/replace "-" "_")))

(defn snake->kebab
  "Convert snake_case string to kebab-case keyword."
  [s]
  (-> s
      (str/replace "_" "-")
      keyword))

(defn gen-token
  "Generate a secure random token with optional prefix."
  ([prefix]
   #?(:clj (str prefix (java.util.UUID/randomUUID))
      :cljs (str prefix (.toString (crypto/randomBytes 32) "hex"))))
  ([]
   (gen-token "")))

(defn expires-at
  "Calculate expiration time from lifetime in seconds."
  [lifetime-seconds]
  #?(:clj (.toString (.plusSeconds (Instant/now) lifetime-seconds))
     :cljs (.toISOString (js/Date. (+ (.now js/Date) (* lifetime-seconds 1000))))))

(defn expired?
  "Check if an ISO timestamp has expired."
  [iso-timestamp]
  #?(:clj (.isAfter (Instant/now) (Instant/parse iso-timestamp))
     :cljs (> (.now js/Date) (.getTime (js/Date. iso-timestamp)))))
