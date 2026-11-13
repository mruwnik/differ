(ns differ.client.theme-test
  (:require [clojure.test :refer [deftest testing is are]]
            [differ.client.theme :as theme]
            [differ.client.db :as db]))

(defn- fake-storage
  "A minimal Storage stand-in backed by an atom."
  ([] (fake-storage {}))
  ([initial]
   (let [data (atom initial)]
     #js {:getItem (fn [k] (get @data k))
          :setItem (fn [k v] (swap! data assoc k (str v)))
          :data data})))

(defn- throwing-storage []
  #js {:getItem (fn [_] (throw (js/Error. "SecurityError")))
       :setItem (fn [_ _] (throw (js/Error. "QuotaExceededError")))})

(deftest normalize-choice-test
  (are [in out] (= out (theme/normalize-choice in))
    "light"  "light"
    "dark"   "dark"
    "system" "system"
    nil      "system"
    ""       "system"
    "Dark"   "system"
    "blue"   "system"))

(deftest resolve-theme-test
  (testing "an explicit choice wins over the system preference"
    (are [choice system-dark? out] (= out (theme/resolve-theme choice system-dark?))
      "light"  true  "light"
      "light"  false "light"
      "dark"   false "dark"
      "dark"   true  "dark"))
  (testing "system follows the system preference"
    (is (= "dark" (theme/resolve-theme "system" true)))
    (is (= "light" (theme/resolve-theme "system" false))))
  (testing "unknown or missing choices behave like system"
    (is (= "dark" (theme/resolve-theme nil true)))
    (is (= "light" (theme/resolve-theme "junk" false)))))

(deftest storage-test
  (testing "defaults to system when nothing is stored"
    (is (= "system" (theme/load-choice (fake-storage)))))
  (testing "a saved choice is loaded back"
    (let [storage (fake-storage)]
      (theme/save-choice! storage "dark")
      (is (= "dark" (theme/load-choice storage)))
      (theme/save-choice! storage "light")
      (is (= "light" (theme/load-choice storage)))
      (theme/save-choice! storage "system")
      (is (= "system" (theme/load-choice storage)))))
  (testing "uses the key the index.html script reads"
    (let [storage (fake-storage)]
      (theme/save-choice! storage "dark")
      (is (= {"differ-theme" "dark"} @(.-data storage)))))
  (testing "junk in storage loads as system and is not saved"
    (is (= "system" (theme/load-choice (fake-storage {"differ-theme" "neon"}))))
    (let [storage (fake-storage)]
      (theme/save-choice! storage "neon")
      (is (= "system" (get @(.-data storage) "differ-theme")))))
  (testing "missing or failing storage never throws"
    (is (= "system" (theme/load-choice nil)))
    (is (nil? (theme/save-choice! nil "dark")))
    (is (= "system" (theme/load-choice (throwing-storage))))
    (is (nil? (theme/save-choice! (throwing-storage) "dark")))))

(deftest choices-test
  (testing "the control offers light, dark and system, in that order"
    (is (= ["light" "dark" "system"] (mapv first theme/choices))))
  (testing "the app starts on the default choice"
    (is (= theme/default-choice (:theme db/default-db)))))

(deftest select-choice-test
  (testing "picking dark stores it and applies dark, whatever the system says"
    (let [fx (theme/select-choice (assoc db/default-db :system-dark? false) "dark")]
      (is (= "dark" (get-in fx [:db :theme])))
      (is (= "dark" (:save-theme fx)))
      (is (= "dark" (:apply-theme fx)))))
  (testing "picking light on a dark system applies light"
    (let [fx (theme/select-choice (assoc db/default-db :system-dark? true) "light")]
      (is (= "light" (:apply-theme fx)))))
  (testing "picking system applies the system preference"
    (is (= "dark" (:apply-theme (theme/select-choice (assoc db/default-db :system-dark? true) "system"))))
    (is (= "light" (:apply-theme (theme/select-choice (assoc db/default-db :system-dark? false) "system")))))
  (testing "switching back and forth keeps the db and storage in step"
    (let [step (fn [db choice] (:db (theme/select-choice db choice)))
          db (reduce step (assoc db/default-db :system-dark? true) ["dark" "light" "system"])]
      (is (= "system" (:theme db)))
      (is (true? (:system-dark? db)))))
  (testing "an invalid choice falls back to system"
    (let [fx (theme/select-choice (assoc db/default-db :theme "dark" :system-dark? false) "neon")]
      (is (= "system" (get-in fx [:db :theme])))
      (is (= "system" (:save-theme fx)))
      (is (= "light" (:apply-theme fx))))))

(deftest system-changed-test
  (testing "with system chosen, an OS change switches the theme live"
    (let [db (assoc db/default-db :theme "system" :system-dark? false)]
      (is (= "dark" (:apply-theme (theme/system-changed db true))))
      (is (true? (get-in (theme/system-changed db true) [:db :system-dark?])))
      (is (= "light" (:apply-theme (theme/system-changed (assoc db :system-dark? true) false))))))
  (testing "an explicit choice is kept when the OS changes"
    (is (= "light" (:apply-theme (theme/system-changed (assoc db/default-db :theme "light") true))))
    (is (= "dark" (:apply-theme (theme/system-changed (assoc db/default-db :theme "dark") false)))))
  (testing "an OS change never touches the stored choice"
    (is (not (contains? (theme/system-changed db/default-db true) :save-theme)))))

(deftest stored-elsewhere-test
  (testing "a choice stored in another tab is shown, not stored again"
    (let [fx (theme/stored-elsewhere (assoc db/default-db :theme "light" :system-dark? false) "dark")]
      (is (= "dark" (get-in fx [:db :theme])))
      (is (= "dark" (:apply-theme fx)))
      (is (not (contains? fx :save-theme)))))
  (testing "system follows this tab's OS setting; a removed or junk value means system"
    (is (= "dark" (:apply-theme (theme/stored-elsewhere (assoc db/default-db :system-dark? true) "system"))))
    (is (= "system" (get-in (theme/stored-elsewhere db/default-db nil) [:db :theme])))
    (is (= "system" (get-in (theme/stored-elsewhere db/default-db "neon") [:db :theme])))))
