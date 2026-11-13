(ns differ.app-test
  "HTTP-level tests for the Express app: CORS headers, request body parsing,
   and request parameters that reach git. Each test starts the app on an
   ephemeral loopback port and stops it afterwards."
  (:require [clojure.test :refer [deftest testing is use-fixtures async]]
            [clojure.string :as str]
            [differ.app :as app]
            [differ.sessions :as sessions]
            [differ.test-helpers :as helpers]
            ["fs" :as fs]
            ["path" :as path]))

;; A regular atom (not a dynamic var): dynamic bindings don't propagate across
;; the async ticks these tests await.
(defonce ^:private state (atom nil))

(use-fixtures :each
  {:before (fn []
             (helpers/init-test-db!)
             (reset! state {:repo (helpers/create-test-repo)
                            :marker-dir (helpers/create-temp-dir "differ-marker")}))
   :after (fn []
            (let [{:keys [repo marker-dir]} @state]
              ((:cleanup repo))
              (helpers/remove-dir marker-dir))
            (helpers/cleanup-test-db!)
            (reset! state nil))})

(defn- repo-path [] (get-in @state [:repo :path]))

(defn- marker-dir [] (:marker-dir @state))

(defn- marker-dir-empty? []
  (empty? (js->clj (fs/readdirSync (marker-dir)))))

;; ============================================================================
;; Server helpers
;; ============================================================================

(defn- start-server []
  (js/Promise.
   (fn [resolve _]
     (let [^js server (.listen (app/create-app) 0 "127.0.0.1")]
       (.on server "listening" #(resolve server))))))

(defn- stop-server [^js server]
  (js/Promise.
   (fn [resolve _]
     (.closeAllConnections server)
     (.close server #(resolve nil)))))

(defn- run-with-server
  "Start the app, run (f base-url), which returns a Promise, then stop the
   app and call done."
  [done f]
  (-> (start-server)
      (.then (fn [^js server]
               (-> (js/Promise.resolve
                    (f (str "http://127.0.0.1:" (.-port (.address server)))))
                   (.catch (fn [err] (is false (str "rejected: " err))))
                   (.then #(stop-server server)))))
      (.then #(done))))

(defn- request
  "fetch url and resolve to {:status :headers :body}; body is parsed JSON
   when the response is JSON."
  [url opts]
  (-> (js/fetch url (clj->js opts))
      (.then (fn [^js res]
               (-> (.text res)
                   (.then (fn [text]
                            {:status (.-status res)
                             :headers (.-headers res)
                             :body (try
                                     (js->clj (js/JSON.parse text) :keywordize-keys true)
                                     (catch :default _ text))})))))))

(defn- post-json [url data]
  (request url {:method "POST"
                :headers {"Content-Type" "application/json"}
                :body (js/JSON.stringify (clj->js data))}))

(defn- session-url [base session-id & parts]
  (str base "/api/sessions/" (js/encodeURIComponent session-id) (str/join parts)))

;; ============================================================================
;; CORS
;; ============================================================================

(deftest no-wildcard-cors-test
  (testing "API, preflight and SSE responses carry no Access-Control-Allow-Origin"
    (async done
           (run-with-server
            done
            (fn [base]
              (let [ctrl (js/AbortController.)
                    origin "http://other.example"]
                (-> (js/Promise.all
                     #js [(js/fetch (str base "/api/sessions")
                                    #js {:headers #js {"Origin" origin}})
                          (js/fetch (str base "/api/sessions")
                                    #js {:method "OPTIONS"
                                         :headers #js {"Origin" origin
                                                       "Access-Control-Request-Method" "POST"
                                                       "Access-Control-Request-Headers" "content-type"}})
                          (js/fetch (str base "/events")
                                    #js {:headers #js {"Origin" origin}
                                         :signal (.-signal ctrl)})])
                    (.then (fn [responses]
                             (is (= 3 (count responses)))
                             (doseq [^js res responses]
                               (is (nil? (.get (.-headers res) "access-control-allow-origin"))
                                   (str "for " (.-url res))))
                             (.abort ctrl))))))))))

;; ============================================================================
;; Body parsing
;; ============================================================================

(deftest form-encoded-bodies-test
  (testing "API routes and /oauth/register ignore form-encoded bodies; token still parses them"
    (async done
           (run-with-server
            done
            (fn [base]
              (let [post-form (fn [route params]
                                (request (str base route)
                                         {:method "POST"
                                          :headers {"Content-Type" "application/x-www-form-urlencoded"}
                                          :body (str (js/URLSearchParams. (clj->js params)))}))]
                (-> (post-form "/api/sessions" {:repo_path (repo-path)})
                    (.then (fn [{:keys [status]}]
                             (is (= 400 status))
                             (is (empty? (sessions/list-sessions)))
                             (post-form "/oauth/token" {:grant_type "client_credentials"})))
                    (.then (fn [{:keys [status body]}]
                             (is (= 400 status))
                             (is (= "Unsupported grant_type: client_credentials"
                                    (:error body)))
                             ;; Registration is JSON-only (RFC 7591).
                             (post-form "/oauth/register" {:client_name "form-client"})))
                    (.then (fn [{:keys [body]}]
                             (is (not= "form-client" (:client_name body))))))))))))

;; ============================================================================
;; Request parameters that reach git
;; ============================================================================

(deftest option-like-branch-rejected-test
  (testing "branch and target branch starting with '-' are rejected"
    (async done
           (run-with-server
            done
            (fn [base]
              (let [bad-ref (str "--output=" (path/join (marker-dir) "out"))]
                (-> (post-json (str base "/api/sessions")
                               {:repo_path (repo-path) :target_branch bad-ref})
                    (.then (fn [{:keys [status body]}]
                             (is (= 400 status))
                             (is (str/includes? (str (:error body)) "must not start with '-'"))
                             (post-json (str base "/api/sessions")
                                        {:repo_path (repo-path) :branch "-x"})))
                    (.then (fn [{:keys [status]}]
                             (is (= 400 status))
                             (is (empty? (sessions/list-sessions)))
                             (post-json (str base "/api/sessions") {:repo_path (repo-path)})))
                    (.then (fn [{:keys [status body]}]
                             (is (= 200 status))
                             (let [session-id (get-in body [:session :id])]
                               (is (string? session-id))
                               (-> (request (session-url base session-id)
                                            {:method "PATCH"
                                             :headers {"Content-Type" "application/json"}
                                             :body (js/JSON.stringify #js {:target_branch bad-ref})})
                                   (.then (fn [{:keys [status]}]
                                            (is (= 400 status))
                                            (request (session-url base session-id "/diff") {})))
                                   (.then (fn [{:keys [status]}]
                                            (is (= 200 status))
                                            (is (= "main" (:target-branch (first (sessions/list-sessions)))))
                                            (is (marker-dir-empty?)))))))))))))))

(deftest shell-metacharacters-in-request-test
  (testing "branch names and paths from requests are not run through a shell"
    (async done
           (run-with-server
            done
            (fn [base]
              (let [marker (path/join (marker-dir) "marker")]
                (-> (post-json (str base "/api/sessions")
                               {:repo_path (repo-path)
                                :branch "feature"
                                :target_branch (str "main;touch " marker ";")})
                    (.then (fn [{:keys [status body]}]
                             (is (= 200 status))
                             (let [session-id (get-in body [:session :id])]
                               (-> (request (session-url base session-id "/diff") {})
                                   (.then (fn [_]
                                            (post-json (session-url base session-id "/stage")
                                                       {:path (str "$(touch " marker ")")})))))))
                    (.then (fn [_]
                             (is (not (fs/existsSync marker))))))))))))
