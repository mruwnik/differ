(ns differ.sessions
  "Session management logic."
  (:require [differ.db :as db]
            [differ.git :as git]
            [differ.util :as util]
            [clojure.set :as set]
            ["fs" :as fs]
            ["path" :as path]))

(defn validate-repo-path
  "Validate that repo-path exists and is a directory.
   Returns {:valid true :path <resolved-path>} or {:valid false :error <message>}"
  [repo-path]
  (if (nil? repo-path)
    {:valid false :error "repo_path is required"}
    (let [resolved (path/resolve repo-path)]
      (cond
        (not (fs/existsSync resolved))
        {:valid false :error (str "Path does not exist: " resolved)}

        (not (.isDirectory (fs/statSync resolved)))
        {:valid false :error (str "Path is not a directory: " resolved)}

        :else
        {:valid true :path resolved}))))

(defn get-or-create-session
  "Get existing session or create new one.
   Returns {:session ... :is-new bool} or {:error ...} if repo-path is invalid."
  [{:keys [repo-path project branch target-branch]}]
  (let [validation (validate-repo-path repo-path)]
    (if-not (:valid validation)
      {:error (:error validation)}
      (let [resolved-path (:path validation)
            project (or project (git/get-project-id resolved-path))
            branch (or branch (git/get-current-branch resolved-path))
            target-branch (or target-branch (git/detect-default-branch resolved-path))
            session-id (util/session-id project branch)]
        (if-let [existing (db/get-session session-id)]
          {:session existing :is-new false}
          (let [session (db/create-session!
                         {:id session-id
                          :project project
                          :branch branch
                          :target-branch target-branch
                          :repo-path resolved-path})]
            {:session session :is-new true}))))))

(defn list-sessions
  "List all sessions, optionally filtered by project.
   Includes unresolved comment count."
  ([] (list-sessions nil))
  ([project]
   (->> (db/list-sessions project)
        (mapv (fn [session]
                (assoc session
                       :unresolved-count
                       (db/count-unresolved-comments (:id session))))))))

(defn get-session
  "Get session by ID with unresolved count."
  [session-id]
  (when-let [session (db/get-session session-id)]
    (assoc session
           :unresolved-count
           (db/count-unresolved-comments session-id))))

(defn compute-file-set
  "Compute the set of files in review for a session.
   Union of: git diff files + registered files + manual additions - manual removals"
  [session repo-path]
  (let [{:keys [target-branch registered-files manual-additions manual-removals]} session
        ;; Get files from git diff
        git-files (->> (git/get-changed-files repo-path target-branch)
                       (map :path)
                       set)
        ;; Get registered file paths (keys of the map)
        registered (set (keys registered-files))
        ;; Compute union
        all-files (set/union git-files registered (set manual-additions))
        ;; Remove manual removals
        final-files (set/difference all-files (set manual-removals))]
    (sort final-files)))

(defn register-files!
  "Register files with a session. Returns list of newly registered paths."
  [session-id paths agent-id]
  (when-let [session (db/get-session session-id)]
    (let [current (:registered-files session)
          ;; Add new registrations (don't overwrite existing)
          new-registrations (reduce
                             (fn [acc path]
                               (if (contains? acc path)
                                 acc
                                 (assoc acc path agent-id)))
                             current
                             paths)
          newly-added (filter #(not (contains? current %)) paths)]
      (db/update-session! session-id {:registered-files new-registrations})
      newly-added)))

(defn unregister-files!
  "Unregister files from a session. Returns list of unregistered paths."
  [session-id paths agent-id]
  (when-let [session (db/get-session session-id)]
    (let [current (:registered-files session)
          ;; Only unregister files registered by this agent (or if agent-id matches)
          to-remove (filter (fn [path]
                              (= agent-id (get current path)))
                            paths)
          new-registrations (apply dissoc current to-remove)]
      (db/update-session! session-id {:registered-files new-registrations})
      to-remove)))

(defn add-manual-file!
  "Manually add a file to the review set."
  [session-id path]
  (when-let [session (db/get-session session-id)]
    (let [current (set (:manual-additions session))
          updated (conj current path)]
      (db/update-session! session-id {:manual-additions (vec updated)})
      path)))

(defn remove-manual-file!
  "Manually remove a file from the review set."
  [session-id path]
  (when-let [session (db/get-session session-id)]
    (let [current (set (:manual-removals session))
          updated (conj current path)]
      (db/update-session! session-id {:manual-removals (vec updated)})
      path)))

(defn get-review-state
  "Get full review state for a session including files and comments."
  [session-id repo-path]
  (when-let [session (get-session session-id)]
    (let [files (compute-file-set session repo-path)
          comments (db/list-comments session-id)]
      {:session-id session-id
       :project (:project session)
       :branch (:branch session)
       :target-branch (:target-branch session)
       :repo-path (:repo-path session)
       :files files
       :excluded-files (vec (:manual-removals session))
       :comments comments
       :unresolved-count (:unresolved-count session)})))

(defn restore-file!
  "Restore an excluded file back to the review set."
  [session-id path]
  (when-let [session (db/get-session session-id)]
    (let [current (set (:manual-removals session))
          updated (disj current path)]
      (db/update-session! session-id {:manual-removals (vec updated)})
      path)))

(defn archive-session!
  "Archive/delete a session and all its data."
  [session-id]
  (db/delete-session! session-id))
