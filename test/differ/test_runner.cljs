(ns differ.test-runner
  "Test runner that requires all test namespaces."
  (:require [cljs.test :refer [run-tests]]
            ;; Require all test namespaces
            ;; Core tests
            [differ.util-test]
            [differ.schema-test]
            [differ.git-test]
            [differ.db-test]
            [differ.sessions-test]
            [differ.comments-test]
            [differ.api-test]
            [differ.mcp-test]
            [differ.config-test]
            [differ.diff-test]
            [differ.watcher-test]
            [differ.push-permissions-test]
            [differ.pull-request-test]
            [differ.sse-test]
            [differ.github-api-test]
            [differ.github-oauth-test]
            [differ.oauth-test]
            ;; Backend tests
            [differ.backend.protocol-test]
            [differ.backend.local-test]
            [differ.backend.github-test]
            ;; Client tests
            [differ.client.db-test]
            [differ.client.subs-test]
            [differ.client.highlight-test]
            [differ.client.events-test]))

(defn main []
  (run-tests
   ;; Core tests
   'differ.util-test
   'differ.schema-test
   'differ.git-test
   'differ.db-test
   'differ.sessions-test
   'differ.comments-test
   'differ.api-test
   'differ.mcp-test
   'differ.config-test
   'differ.diff-test
   'differ.watcher-test
   'differ.push-permissions-test
   'differ.pull-request-test
   'differ.sse-test
   'differ.github-api-test
   'differ.github-oauth-test
   'differ.oauth-test
   ;; Backend tests
   'differ.backend.protocol-test
   'differ.backend.local-test
   'differ.backend.github-test
   ;; Client tests
   'differ.client.db-test
   'differ.client.subs-test
   'differ.client.highlight-test
   'differ.client.events-test))
