(ns differ.backend.local-diff-test
  "Async tests for branch-aware diffs on the local backend.

   Lives in its own ns so the fixture can use map form (`:before`/`:after`).
   The classic `(use-fixtures :each (fn [f] ...))` wrapping form is
   incompatible with `cljs.test` async tests (it can't await the Promise), so
   the shared local-backend suite keeps the sync tests and these live here."
  (:require [clojure.test :refer [deftest testing is use-fixtures async]]
            [clojure.string :as str]
            [differ.backend.local :as local]
            [differ.backend.protocol :as proto]
            [differ.test-helpers :as helpers]))

;; A regular atom (not a dynamic var): dynamic bindings don't propagate across
;; the async ticks these tests await, so the repo path must live somewhere flat.
(defonce ^:private test-repo (atom nil))

(use-fixtures :each
  {:before (fn [] (reset! test-repo (:path (helpers/create-test-repo))))
   :after  (fn [] (when-let [p @test-repo] (helpers/remove-dir p) (reset! test-repo nil)))})

(deftest get-diff-three-dot-for-non-head-branch-test
  (testing "diffs target...branch when the session branch is not the checked-out HEAD"
    (async done
           ;; Commit a change on `feature`, then switch the repo back to main so
           ;; the checked-out HEAD differs from the session branch (worktree case).
           (helpers/create-test-branch @test-repo "feature")
           (helpers/add-test-file @test-repo "foo.txt" "hello from feature\n")
           (helpers/commit-test-changes @test-repo "add foo on feature")
           (helpers/checkout-test-branch @test-repo "main")
           (let [backend (local/create-local-backend @test-repo "main" "local:sid" "feature")]
             (-> (proto/get-diff backend)
                 (.then (fn [diff]
                          ;; A plain `git diff main` here (HEAD=main, clean tree)
                          ;; would be empty; three-dot main...feature surfaces the
                          ;; branch commit.
                          (is (some? diff))
                          (is (str/includes? diff "foo.txt"))
                          (is (str/includes? diff "hello from feature"))
                          (done)))
                 (.catch (fn [err] (is false (str "rejected: " err)) (done))))))))

(deftest get-diff-fallback-working-tree-when-branch-is-head-test
  (testing "falls back to a working-tree diff when branch equals the checked-out HEAD"
    (async done
           ;; Uncommitted change on main; branch arg == HEAD (main) must preserve
           ;; the original `git diff <target>` working-tree behavior.
           (helpers/modify-test-file @test-repo "README.md" "# Test Repo\n\nEdited working tree.\n")
           (let [backend (local/create-local-backend @test-repo "main" "local:sid" "main")]
             (-> (proto/get-diff backend)
                 (.then (fn [diff]
                          (is (some? diff))
                          (is (str/includes? diff "README.md"))
                          (is (str/includes? diff "Edited working tree"))
                          (done)))
                 (.catch (fn [err] (is false (str "rejected: " err)) (done))))))))

(deftest get-diff-fallback-working-tree-when-branch-blank-test
  (testing "falls back to a working-tree diff when no branch is provided (back-compat)"
    (async done
           (helpers/modify-test-file @test-repo "README.md" "# Test Repo\n\nAnother edit.\n")
           (let [backend (local/create-local-backend @test-repo "main")]
             (-> (proto/get-diff backend)
                 (.then (fn [diff]
                          (is (some? diff))
                          (is (str/includes? diff "Another edit"))
                          (done)))
                 (.catch (fn [err] (is false (str "rejected: " err)) (done))))))))
