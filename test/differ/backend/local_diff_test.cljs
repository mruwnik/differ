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

(deftest get-file-content-head-reads-non-head-branch-tip-test
  (testing "get-file-content ref=head/nil returns the branch-tip content for a non-checked-out branch"
    (async done
           ;; foo.txt exists only on `feature`; HEAD is back on main where it is
           ;; absent, so a working-tree read would miss it — head/nil must read
           ;; the branch tip via `git show`.
           (helpers/create-test-branch @test-repo "feature")
           (helpers/add-test-file @test-repo "foo.txt" "hello from feature\n")
           (helpers/commit-test-changes @test-repo "add foo on feature")
           (helpers/checkout-test-branch @test-repo "main")
           (let [backend (local/create-local-backend @test-repo "main" "local:sid" "feature")]
             (-> (js/Promise.all
                  #js [(proto/get-file-content backend "head" "foo.txt")
                       (proto/get-file-content backend nil "foo.txt")])
                 (.then (fn [[head-content nil-content]]
                          ;; exec-git trims trailing whitespace off `git show`.
                          (is (= "hello from feature" head-content))
                          (is (= "hello from feature" nil-content))
                          (done)))
                 (.catch (fn [err] (is false (str "rejected: " err)) (done))))))))

(deftest get-changed-files-lists-branch-commit-files-test
  (testing "get-changed-files lists files from the branch commit for a non-checked-out branch"
    (async done
           (helpers/create-test-branch @test-repo "feature")
           (helpers/add-test-file @test-repo "foo.txt" "hello from feature\n")
           (helpers/commit-test-changes @test-repo "add foo on feature")
           (helpers/checkout-test-branch @test-repo "main")
           (let [backend (local/create-local-backend @test-repo "main" "local:sid" "feature")]
             (-> (proto/get-changed-files backend)
                 (.then (fn [files]
                          (let [paths (set (map :path files))]
                            (is (contains? paths "foo.txt"))
                            (is (= :added (:status (first (filter #(= "foo.txt" (:path %)) files))))))
                          (done)))
                 (.catch (fn [err] (is false (str "rejected: " err)) (done))))))))

(deftest get-file-content-base-reads-merge-base-not-target-tip-test
  (testing "get-file-content ref=base returns the merge-base version, not target tip, once target advances"
    (async done
           ;; Build a history where main advances past the point feature was cut:
           ;;   main:    +shared(base) --cut feature--> +shared(advanced target)
           ;;   feature:                 +shared(feature)
           ;; The three-dot base-side is merge-base(main, feature) == the cut
           ;; point (shared = "base version"), which differs from main's tip.
           (helpers/add-test-file @test-repo "shared.txt" "base version\n")
           (helpers/commit-test-changes @test-repo "add shared on main")
           (helpers/create-test-branch @test-repo "feature")
           (helpers/modify-test-file @test-repo "shared.txt" "feature version\n")
           (helpers/commit-test-changes @test-repo "edit shared on feature")
           (helpers/checkout-test-branch @test-repo "main")
           (helpers/modify-test-file @test-repo "shared.txt" "advanced target\n")
           (helpers/commit-test-changes @test-repo "advance shared on main")
           (let [backend (local/create-local-backend @test-repo "main" "local:sid" "feature")]
             (-> (proto/get-file-content backend "base" "shared.txt")
                 (.then (fn [content]
                          ;; exec-git trims trailing whitespace off `git show`.
                          (is (= "base version" content))
                          (is (not= "advanced target" content))
                          (done)))
                 (.catch (fn [err] (is false (str "rejected: " err)) (done))))))))
