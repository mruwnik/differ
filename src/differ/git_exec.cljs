(ns differ.git-exec
  "Synchronous git execution, shared by differ.git, differ.backend.local and
   differ.pull-request so the injection rules live in one place.

   git is spawned with an argument array (no shell), so branch names and
   paths are never read by a shell. Nothing here adds --end-of-options:
   callers must put caller-supplied refs after --end-of-options, and paths
   after --, themselves."
  (:require ["child_process" :as cp]
            [clojure.string :as str]))

(defn run-git
  "Run git in repo-path with the argument vector args. Return git's stdout,
   untrimmed, when it exits 0, otherwise nil. Exceptions are logged. With
   :log-failures true, a failed run (non-zero exit or spawn error) is logged
   too, with git's stderr or the error message."
  ([repo-path args] (run-git repo-path args {}))
  ([repo-path args {:keys [log-failures]}]
   (try
     (let [result (cp/spawnSync "git" (clj->js (vec args))
                                #js {:cwd repo-path
                                     :encoding "utf8"
                                     :maxBuffer (* 50 1024 1024)})]  ; 50MB buffer
       (if (zero? (.-status result))
         (.-stdout result)
         (do (when log-failures
               (js/console.warn "[git] Command failed:" (pr-str args)
                                (or (some-> (.-error result) .-message)
                                    (str/trim (or (.-stderr result) "")))))
             nil)))
     (catch :default e
       (js/console.warn "[git] Command failed:" (pr-str args) (.-message e))
       nil))))

(defn exec-git
  "Run git in repo-path and return its trimmed stdout, or nil on a non-zero
   exit (not logged: callers use nil as an answer, e.g. 'not a repo').
   Callers must put caller-supplied refs after --end-of-options."
  [repo-path & args]
  (some-> (run-git repo-path args) str/trim))

(defn exec-git-logged
  "Like exec-git, but also logs a failed run with git's stderr.
   Callers must put caller-supplied refs after --end-of-options."
  [repo-path & args]
  (some-> (run-git repo-path args {:log-failures true}) str/trim))
