(ns differ.mcp-async-test
  "Async end-to-end tests for MCP's `handle-method \"tools/call\"` path.

   Lives in its own ns so the fixture can use map form (`:before`/`:after`),
   which `cljs.test` requires for `(async done ...)` tests. The main
   `differ.mcp-test` ns uses a single-function fixture that binds dynamic
   vars around a DB-seeded test env — that form is incompatible with async
   tests, so async end-to-end tests that stub DB-facing fns (and therefore
   don't need the full env) go here."
  (:require [clojure.test :refer [deftest testing is use-fixtures async]]
            [differ.mcp :as mcp]
            [differ.boards :as boards]))

;; No shared env needed — each test stubs the boards fns it calls.
(use-fixtures :each
  {:before (fn [] nil)
   :after  (fn [] nil)})

(deftest tools-call-get-upstream-end-to-end-test
  (testing "handle-method \"tools/call\" composes validate-tool-args +
            handle-tool end to end for get_upstream: numeric-string :depth
            is coerced, snake_case fields become kebab-case keywords, and
            the returned Promise resolves to a well-formed MCP tool result
            (content block, no :isError flag).

            This is the regression guard against someone dropping
            validate-tool-args from the tools/call path for these tools —
            the R1 review surfaced that none of the in-band tests
            exercised the Promise-returning dispatcher end to end."
    (async done
           (let [captured (atom nil)
                 done-called? (atom false)
                 finish! (fn [] (when (compare-and-set! done-called? false true) (done)))
                 result (with-redefs [boards/get-upstream (fn [tid opts]
                                                            (reset! captured {:task-id tid :opts opts})
                                                            [{:id "parent-1" :title "Parent" :depth 1}])]
                          (mcp/handle-method
                           "tools/call"
                           {:name "get_upstream"
                            :arguments {:task_id "x"
                                        :depth "2"  ;; numeric string — must be coerced to 2
                                        :fields ["title"]}}))]
             ;; Safety net: if the Promise-returning contract regresses and
             ;; handle-method throws synchronously or returns a non-thenable,
             ;; fire done after 5s with a loud failure rather than hanging
             ;; the test runner.
             (js/setTimeout
              (fn []
                (when-not @done-called?
                  (is false "tools/call handler did not resolve within 5s — possible sync throw or non-Promise return")
                  (finish!)))
              5000)
             (is (instance? js/Promise result))
             ;; Wrap in Promise.resolve so a non-Promise result degrades to a
             ;; rejected .then rather than throwing synchronously here.
             (-> (js/Promise.resolve result)
                 (.then (fn [r]
                          ;; Stub saw coerced args: integer :depth and
                          ;; kebab-case keywords for :fields.
                          (is (= "x" (:task-id @captured)))
                          (is (= 2 (get-in @captured [:opts :depth])))
                          (is (integer? (get-in @captured [:opts :depth])))
                          (is (= [:title] (get-in @captured [:opts :fields])))
                          ;; MCP tool result shape: content vector present,
                          ;; no :isError flag (or explicitly falsy).
                          (is (vector? (:content r)))
                          (is (seq (:content r)))
                          (is (= "text" (:type (first (:content r)))))
                          (is (not (:isError r)))
                          (finish!)))
                 (.catch (fn [err]
                           (is false (str "promise rejected: " err))
                           (finish!))))))))
