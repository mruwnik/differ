(ns differ.github-oauth-async-test
  "Async tests for the GitHub OAuth network calls, with `fetch` stubbed.

   Lives in its own ns so the fixture can use map form, which `cljs.test`
   requires for `(async done ...)` tests. Stubbing `fetch` keeps these off
   the real GitHub API and lets us assert on what the promises resolve to."
  (:require [clojure.test :refer [deftest testing is use-fixtures async]]
            [differ.github-oauth :as gh-oauth]))

(use-fixtures :each
  {:before (fn [] nil)
   :after  (fn [] nil)})

(defn fake-response [status body]
  #js {:ok (<= 200 status 299)
       :status status
       :json (fn [] (js/Promise.resolve (clj->js body)))})

(defn run-with-fetch
  "Swap in `fake-fetch` for the global fetch, run `(f)` (returning a promise),
   pass its result to `check`, then restore fetch and call `done`."
  [done fake-fetch f check]
  (let [original js/globalThis.fetch]
    (set! js/globalThis.fetch fake-fetch)
    (-> (js/Promise.resolve nil)
        (.then (fn [_] (f)))
        (.then check (fn [err] (is false (str "Unexpected rejection: " err))))
        (.finally (fn []
                    (set! js/globalThis.fetch original)
                    (done))))))

(defn run-expecting-rejection
  "Like `run-with-fetch`, but `f`'s promise must reject; `check` gets the error."
  [done fake-fetch f check]
  (run-with-fetch done fake-fetch
                  #(-> (f)
                       (.then (fn [v] {:unexpected-success v})
                              (fn [err] {:error err})))
                  (fn [{:keys [unexpected-success error]}]
                    (is (nil? unexpected-success))
                    (check error))))

(deftest exchange-code-for-token-resolves-token-data-test
  (testing "posts the code to GitHub and resolves to the keywordized JSON body"
    (async done
           (let [calls (atom [])]
             (run-with-fetch
              done
              (fn [url opts]
                (swap! calls conj {:url url :body (js->clj (js/JSON.parse (.-body opts)))})
                (js/Promise.resolve (fake-response 200 {:access_token "tok" :scope "repo"})))
              #(gh-oauth/exchange-code-for-token "the-code")
              (fn [result]
                (is (= {:access_token "tok" :scope "repo"} result))
                (is (= "https://github.com/login/oauth/access_token" (:url (first @calls))))
                (is (= "the-code" (get-in (first @calls) [:body "code"])))))))))

(deftest exchange-code-for-token-rejects-on-http-error-test
  (testing "a non-2xx response rejects with the status in ex-data"
    (async done
           (run-expecting-rejection
            done
            (fn [_ _] (js/Promise.resolve (fake-response 500 {})))
            #(gh-oauth/exchange-code-for-token "the-code")
            (fn [err]
              (is (= "Token exchange failed" (ex-message err)))
              (is (= {:status 500} (ex-data err))))))))

(deftest get-user-info-resolves-user-test
  (testing "sends the token as a bearer header and resolves to the user"
    (async done
           (let [headers (atom nil)]
             (run-with-fetch
              done
              (fn [_ opts]
                (reset! headers (js->clj (.-headers opts)))
                (js/Promise.resolve (fake-response 200 {:id 42 :login "octocat"})))
              #(gh-oauth/get-user-info "tok")
              (fn [result]
                (is (= {:id 42 :login "octocat"} result))
                (is (= "Bearer tok" (get @headers "Authorization")))))))))

(deftest validate-token-true-for-valid-token-test
  (testing "resolves true when GitHub accepts the token"
    (async done
           (run-with-fetch
            done
            (fn [_ _] (js/Promise.resolve (fake-response 200 {:id 1 :login "u"})))
            #(gh-oauth/validate-token "good")
            #(is (true? %))))))

(deftest validate-token-false-for-rejected-token-test
  (testing "resolves false (rather than rejecting) when GitHub returns 401"
    (async done
           (run-with-fetch
            done
            (fn [_ _] (js/Promise.resolve (fake-response 401 {:message "Bad credentials"})))
            #(gh-oauth/validate-token "bad")
            #(is (false? %))))))

(deftest complete-oauth-flow-rejects-on-oauth-error-test
  (testing "an OAuth error body rejects with GitHub's error description"
    (async done
           (run-expecting-rejection
            done
            (fn [_ _] (js/Promise.resolve
                       (fake-response 200 {:error "bad_verification_code"
                                           :error_description "The code passed is incorrect or expired."})))
            #(gh-oauth/complete-oauth-flow "stale-code")
            (fn [err]
              (is (= "The code passed is incorrect or expired." (ex-message err)))
              (is (= {:error "bad_verification_code"} (ex-data err))))))))

(deftest validate-pat-valid-token-test
  (testing "resolves {:valid true :user ...} when GitHub accepts the PAT"
    (async done
           (run-with-fetch
            done
            (fn [_ _] (js/Promise.resolve (fake-response 200 {:id 7 :login "pat-user"})))
            #(gh-oauth/validate-pat "ghp_good")
            #(is (= {:valid true :user {:id 7 :login "pat-user"}} %))))))

(deftest validate-pat-rejected-token-test
  (testing "resolves {:valid false :error ...} (rather than rejecting) on 401"
    (async done
           (run-with-fetch
            done
            (fn [_ _] (js/Promise.resolve (fake-response 401 {})))
            #(gh-oauth/validate-pat "ghp_bad")
            #(is (= {:valid false :error "Failed to get user info"} %))))))
