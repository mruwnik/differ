(ns differ.test-runner
  "Test runner that requires all test namespaces."
  (:require [cljs.test :refer [run-tests]]
            ;; Require all test namespaces
            [differ.util-test]
            [differ.schema-test]
            [differ.git-test]
            [differ.db-test]
            [differ.sessions-test]
            [differ.comments-test]
            [differ.api-test]
            [differ.mcp-test]
            [differ.client.db-test]
            [differ.client.subs-test]))

(defn main []
  (run-tests 'differ.util-test
             'differ.schema-test
             'differ.git-test
             'differ.db-test
             'differ.sessions-test
             'differ.comments-test
             'differ.api-test
             'differ.mcp-test
             'differ.client.db-test
             'differ.client.subs-test))
