(ns differ.test-runner
  "Test runner that requires all test namespaces."
  (:require [cljs.test :refer [run-tests]]
            [differ.test-helpers :as helpers]
            ;; Require all test namespaces
            ;; Core tests
            [differ.util-test]
            [differ.schema-test]
            [differ.git-test]
            [differ.db-test]
            [differ.boards-test]
            [differ.sessions-test]
            [differ.comments-test]
            [differ.api-test]
            [differ.mcp-test]
            [differ.mcp-async-test]
            [differ.config-test]
            [differ.diff-test]
            [differ.watcher-test]
            [differ.push-permissions-test]
            [differ.pull-request-test]
            [differ.sse-test]
            [differ.event-stream-test]
            [differ.github-api-test]
            [differ.github-events-test]
            [differ.github-oauth-test]
            [differ.github-oauth-async-test]
            [differ.session-events-test]
            [differ.oauth-test]
            [differ.gdocs-test]
            ;; Backend tests
            [differ.backend.protocol-test]
            [differ.backend.local-test]
            [differ.backend.local-diff-test]
            [differ.backend.github-test]
            ;; Client tests
            [differ.client.db-test]
            [differ.client.subs-test]
            [differ.client.highlight-test]
            [differ.client.events-test]
            [differ.client.task-filter-test]))

(defn main []
  ;; Never let app code under test open the real ~/.local/share/differ db,
  ;; even from namespaces whose fixtures don't call init-test-db!.
  (helpers/isolate-app-db! (helpers/create-temp-dir "differ-test-app-db"))
  (run-tests
   ;; Core tests
   'differ.util-test
   'differ.schema-test
   'differ.git-test
   'differ.db-test
   'differ.boards-test
   'differ.sessions-test
   'differ.comments-test
   'differ.api-test
   'differ.mcp-test
   'differ.mcp-async-test
   'differ.config-test
   'differ.diff-test
   'differ.watcher-test
   'differ.push-permissions-test
   'differ.pull-request-test
   'differ.sse-test
   'differ.event-stream-test
   'differ.github-api-test
   'differ.github-events-test
   'differ.github-oauth-test
   'differ.github-oauth-async-test
   'differ.session-events-test
   'differ.oauth-test
   'differ.gdocs-test
   ;; Backend tests
   'differ.backend.protocol-test
   'differ.backend.local-test
   'differ.backend.local-diff-test
   'differ.backend.github-test
   ;; Client tests
   'differ.client.db-test
   'differ.client.subs-test
   'differ.client.highlight-test
   'differ.client.events-test
   'differ.client.task-filter-test))
