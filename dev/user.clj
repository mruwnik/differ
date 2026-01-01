(ns user
  "Dev runner that compiles ClojureScript and starts the server."
  (:require [shadow.cljs.devtools.api :as shadow]
            [shadow.cljs.devtools.server :as server]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn- get-port
  "Get the server port from PORT env var or config.edn."
  []
  (or (some-> (System/getenv "PORT") parse-long)
      (-> (io/resource "config.edn")
          slurp
          edn/read-string
          :port)
      8576))

(defonce ^:private node-process (atom nil))

(defn start-server-process []
  (let [pb (ProcessBuilder. ["node" "target/server.js"])
        _ (.inheritIO pb)
        proc (.start pb)]
    (reset! node-process proc)
    proc))

(defn stop-server-process []
  (when-let [^Process proc @node-process]
    (println "Stopping Node.js server...")
    (.destroy proc)
    (reset! node-process nil)))

(defn go []
  ;; Stop any existing server process
  (stop-server-process)

  ;; Start shadow-cljs server
  (server/start!)

  ;; Start watch first (this also compiles)
  (shadow/watch :server)
  (shadow/watch :ui)

  ;; Give shadow-cljs a moment to set up websocket
  (Thread/sleep 500)

  ;; Start the node server (will connect to shadow-cljs for hot-reload)
  (println "Starting Node.js server...")
  (start-server-process)

  (println (str "Dev environment ready. Server at http://localhost:" (get-port))))

;; Add shutdown hook to clean up node process
(.addShutdownHook (Runtime/getRuntime)
                  (Thread. #(stop-server-process)))

;; Auto-start when loaded
(go)
