(ns differ.backend.local-diff-test
  "Async tests for branch-aware diffs on the local backend.

   Lives in its own ns so the fixture can use map form (`:before`/`:after`).
   The classic `(use-fixtures :each (fn [f] ...))` wrapping form is
   incompatible with `cljs.test` async tests (it can't await the Promise), so
   the shared local-backend suite keeps the sync tests and these live here."
  (:require [clojure.test :refer [deftest testing is use-fixtures async]]
            [clojure.string :as str]
            ["better-sqlite3" :as Database]
            [differ.backend.local :as local]
            [differ.backend.protocol :as proto]
            [differ.db :as db]
            [differ.test-helpers :as helpers]))

;; A regular atom (not a dynamic var): dynamic bindings don't propagate across
;; the async ticks these tests await, so the repo path must live somewhere flat.
(defonce ^:private test-repo (atom nil))

(use-fixtures :each
  {:before (fn [] (reset! test-repo (:path (helpers/create-test-repo))))
   :after  (fn [] (when-let [p @test-repo] (helpers/remove-dir p) (reset! test-repo nil)))})

;; Staleness surfaces only through the DB-backed comment methods, so those tests
;; wire a fully-migrated db into differ.db (init-test-db!'s comments schema is
;; older than migrate-comments-table and lacks the columns create-comment!
;; writes). Each test sets up and tears down its own db in-body.
(defonce ^:private test-db-dir (atom nil))

(defn- fresh-migrated-db! []
  (let [dir (helpers/create-temp-dir "differ-diff-db")
        d (Database (str dir "/test.db"))]
    (reset! test-db-dir dir)
    (.pragma d "journal_mode = WAL")
    (.pragma d "foreign_keys = ON")
    (db/run-migrations! d)
    (reset! db/db-instance d)
    d))

(defn- cleanup-db! [d]
  (.close d)
  (reset! db/db-instance nil)
  (when-let [dir @test-db-dir]
    (helpers/remove-dir dir)
    (reset! test-db-dir nil)))

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

;; ============================================================================
;; Branch-aware staleness (reads the branch tip, not repo-path's working tree)
;; ============================================================================

(deftest staleness-reads-branch-tip-not-working-tree-test
  (testing "get-comments computes staleness against the session branch tip for a non-checked-out branch"
    (async done
           ;; foo.txt lives only on `feature`; HEAD is main, whose working tree
           ;; has no foo.txt. add-comment! hashes line 2 off the branch tip, and
           ;; get-comments must re-read the branch tip to see it as :fresh — a
           ;; working-tree read would hash "" and report :changed.
           (helpers/create-test-branch @test-repo "feature")
           (helpers/add-test-file @test-repo "foo.txt" "line one\nline two\nline three\n")
           (helpers/commit-test-changes @test-repo "add foo on feature")
           (helpers/checkout-test-branch @test-repo "main")
           (let [d (fresh-migrated-db!)]
             (db/create-session! {:id "local:sid" :session-type "local" :project "p"
                                  :branch "feature" :target-branch "main" :repo-path @test-repo})
             (let [backend (local/create-local-backend @test-repo "main" "local:sid" "feature")]
               (-> (proto/add-comment! backend {:file "foo.txt" :line 2 :text "re: line two" :author "rev"})
                   (.then (fn [_] (proto/get-comments backend)))
                   (.then (fn [comments]
                            (let [c (first comments)]
                              (is (= 1 (count comments)))
                              (is (= "foo.txt" (:file c)))
                              (is (= :fresh (:staleness c))))
                            (cleanup-db! d)
                            (done)))
                   (.catch (fn [err] (cleanup-db! d) (is false (str "rejected: " err)) (done)))))))))

(deftest staleness-fallback-reads-working-tree-for-same-branch-test
  (testing "staleness reads the working tree when the session branch is the checked-out HEAD"
    (async done
           ;; Same-branch session (branch == HEAD == main): the working tree is
           ;; the review surface. A comment hashed against an uncommitted
           ;; working-tree edit must read back as :fresh from the working tree.
           (helpers/modify-test-file @test-repo "README.md" "alpha\nbeta\ngamma\n")
           (let [d (fresh-migrated-db!)]
             (db/create-session! {:id "local:sid" :session-type "local" :project "p"
                                  :branch "main" :target-branch "main" :repo-path @test-repo})
             (let [backend (local/create-local-backend @test-repo "main" "local:sid" "main")]
               (-> (proto/add-comment! backend {:file "README.md" :line 2 :text "re: beta" :author "rev"})
                   (.then (fn [_] (proto/get-comments backend)))
                   (.then (fn [comments]
                            (is (= :fresh (:staleness (first comments))))
                            (cleanup-db! d)
                            (done)))
                   (.catch (fn [err] (cleanup-db! d) (is false (str "rejected: " err)) (done)))))))))

;; ============================================================================
;; file-exists? / list-directory: branch-aware + base-side merge-base consistency
;; ============================================================================

(deftest file-exists-head-reads-branch-tip-test
  (testing "file-exists? ref=head/nil resolves to the session branch tip"
    (async done
           ;; foo.txt is on feature only; main's working tree lacks it.
           (helpers/create-test-branch @test-repo "feature")
           (helpers/add-test-file @test-repo "foo.txt" "x\n")
           (helpers/commit-test-changes @test-repo "add foo on feature")
           (helpers/checkout-test-branch @test-repo "main")
           (let [backend (local/create-local-backend @test-repo "main" "local:sid" "feature")]
             (-> (js/Promise.all
                  #js [(proto/file-exists? backend "head" "foo.txt")
                       (proto/file-exists? backend nil "foo.txt")])
                 (.then (fn [[head? nil?]]
                          (is (true? head?))
                          (is (true? nil?))
                          (done)))
                 (.catch (fn [err] (is false (str "rejected: " err)) (done))))))))

(deftest file-exists-base-reads-merge-base-not-target-tip-test
  (testing "file-exists? ref=base resolves to the merge-base, matching get-file-content"
    (async done
           ;; only-on-main-tip.txt is committed to main AFTER feature was cut, so
           ;; it exists at main's tip but NOT at merge-base(main, feature).
           (helpers/add-test-file @test-repo "shared.txt" "base\n")
           (helpers/commit-test-changes @test-repo "shared on main (merge-base)")
           (helpers/create-test-branch @test-repo "feature")
           (helpers/modify-test-file @test-repo "shared.txt" "feature\n")
           (helpers/commit-test-changes @test-repo "edit shared on feature")
           (helpers/checkout-test-branch @test-repo "main")
           (helpers/add-test-file @test-repo "only-on-main-tip.txt" "y\n")
           (helpers/commit-test-changes @test-repo "advance main")
           (let [backend (local/create-local-backend @test-repo "main" "local:sid" "feature")]
             (-> (proto/file-exists? backend "base" "only-on-main-tip.txt")
                 (.then (fn [exists?]
                          ;; false proves base==merge-base; target-tip read would be true.
                          (is (false? exists?))
                          (done)))
                 (.catch (fn [err] (is false (str "rejected: " err)) (done))))))))

(deftest list-directory-head-and-base-are-branch-and-merge-base-consistent-test
  (testing "list-directory resolves head to the branch tip and base to the merge-base"
    (async done
           (helpers/add-test-file @test-repo "shared.txt" "base\n")
           (helpers/commit-test-changes @test-repo "shared on main (merge-base)")
           (helpers/create-test-branch @test-repo "feature")
           (helpers/add-test-file @test-repo "on-feature.txt" "f\n")
           (helpers/commit-test-changes @test-repo "add on-feature")
           (helpers/checkout-test-branch @test-repo "main")
           (helpers/add-test-file @test-repo "only-on-main-tip.txt" "y\n")
           (helpers/commit-test-changes @test-repo "advance main")
           (let [backend (local/create-local-backend @test-repo "main" "local:sid" "feature")]
             (-> (js/Promise.all
                  #js [(proto/list-directory backend "head" "")
                       (proto/list-directory backend "base" "")])
                 (.then (fn [[head-entries base-entries]]
                          (let [head-names (set (map :name head-entries))
                                base-names (set (map :name base-entries))]
                            ;; head == feature tip: has on-feature.txt, not main's later file.
                            (is (contains? head-names "on-feature.txt"))
                            (is (not (contains? head-names "only-on-main-tip.txt")))
                            ;; base == merge-base: has shared.txt, neither branch's later adds.
                            (is (contains? base-names "shared.txt"))
                            (is (not (contains? base-names "on-feature.txt")))
                            (is (not (contains? base-names "only-on-main-tip.txt"))))
                          (done)))
                 (.catch (fn [err] (is false (str "rejected: " err)) (done))))))))
