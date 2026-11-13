(ns differ.client.theme
  "Light/dark theme. The user picks light, dark or system; system follows
   the OS `prefers-color-scheme`. The resolved theme is set as the
   `data-theme` attribute on the root element, which style.css keys on.
   index.html applies the stored choice before the app loads, using the
   same storage key and rules as here.")

(def storage-key "differ-theme")

(def default-choice "system")

(def choices
  "Choices in display order, with their labels."
  [["light" "Light"]
   ["dark" "Dark"]
   ["system" "System"]])

(defn normalize-choice
  "A valid choice, or the default for anything else (nil, junk in storage)."
  [choice]
  (if (some #(= choice (first %)) choices)
    choice
    default-choice))

(defn resolve-theme
  "The theme to show, \"light\" or \"dark\": an explicit choice wins,
   otherwise the system preference decides."
  [choice system-dark?]
  (case (normalize-choice choice)
    "light" "light"
    "dark" "dark"
    (if system-dark? "dark" "light")))

(defn load-choice
  "The stored choice from a Storage-like object (nil-safe, never throws)."
  [^js storage]
  (normalize-choice
   (try
     (some-> storage (.getItem storage-key))
     (catch :default _ nil))))

(defn save-choice!
  "Store the choice; storage errors (private mode, quota) are ignored."
  [^js storage choice]
  (try
    (some-> storage (.setItem storage-key (normalize-choice choice)))
    (catch :default _ nil)))

;; Re-frame effect maps, kept here as pure functions so they can be tested.

(defn select-choice
  "Effects for the user picking a choice."
  [db choice]
  (let [choice (normalize-choice choice)]
    {:db (assoc db :theme choice)
     :save-theme choice
     :apply-theme (resolve-theme choice (:system-dark? db))}))

(defn system-changed
  "Effects for the OS color scheme changing."
  [db dark?]
  {:db (assoc db :system-dark? (boolean dark?))
   :apply-theme (resolve-theme (:theme db) dark?)})

;; Browser glue

(defn local-storage
  "window.localStorage, or nil where reading it throws (blocked storage)."
  []
  (try (.-localStorage js/window) (catch :default _ nil)))

(defn- dark-query []
  (when (exists? js/window.matchMedia)
    (.matchMedia js/window "(prefers-color-scheme: dark)")))

(defn system-dark? []
  (boolean (some-> (dark-query) .-matches)))

(defn watch-system!
  "Call `on-change` with true/false whenever the OS color scheme changes."
  [on-change]
  (when-let [query (dark-query)]
    (.addEventListener query "change" #(on-change (.-matches %)))))

(defn apply-theme! [theme]
  (.setAttribute (.-documentElement js/document) "data-theme" theme))
