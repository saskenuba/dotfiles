;;; Code

;; buffer-local cache variable globally
(defvar-local my/flycheck-local-cache nil
  "Buffer-local cache for Flycheck checker properties.")

(defun my/flycheck-checker-get (orig-fn checker property)
  "Advice for flycheck-checker-get to use buffer-local cache.
Intercepts calls to ORIG-FN (flycheck-checker-get) for CHECKER and
PROPERTY.  Returns value from `my/flycheck-local-cache` if present,
otherwise calls ORIG-FN."
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
      (funcall orig-fn checker property)))

(advice-add 'flycheck-checker-get :around #'my/flycheck-checker-get)

(defun my-reload-dir-locals-for-current-buffer ()
  "Reload dir locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the current
buffer's dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (my-reload-dir-locals-for-current-buffer))))))

(defun buffer-yank-all ()
  "Yank buffer."
  (interactive)
  (kill-new (buffer-string))
  (message "Buffer content yanked"))

(defun copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer."
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(defun copy-project-relative-file-path-and-line ()
  "Copy the project-relative file path and line number of the
current buffer's point."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer is not visiting a file"))
    (let ((proj (project-current)))
      (unless proj
        (user-error "Not in a project"))
      (let* ((root (project-root proj))
             (rel-path (file-relative-name file root))
             (line (line-number-at-pos (point) t))
             (in-project (file-in-directory-p file root)))
        (unless in-project
          (user-error "File is not within the project"))
        (let ((result (format "%s:%d" rel-path line)))
          (kill-new result)
          (message "Copied: %s" result))))))

(defun my-magit-prune-conceptual-upstream ()
  "Delete local branches configured according to the 'Conceptual Workflow'.

Deletes local branches IF:
1. Their configured upstream (merge target) is the specified main
   integration branch (e.g., 'origin/master').
AND
2. Their corresponding 'push target' branch (e.g., 'origin/branch-name')
   no longer exists on the remote.

Assumes 'git fetch --prune' has been run. Excludes protected
branches ('master', 'main', 'develop') and the current branch."
  (interactive)
  (let* (;; --- Configuration ---
         (protected-branches '("master" "main" "develop"))
         (current-branch (magit-get-current-branch))
         ;; --- User Input for Main Integration Branch ---
         (main-integration-branch
          (let ((branch (magit-read-remote-branch
                         "Main integration branch (e.g., origin/master): " ; Prompt
                         nil ; REQUIRE-UPSTREAM (set to nil)
                         "origin/master"))) ; Default value suggestion
            ;; Validate the input format after reading
            (unless (and branch (not (string-empty-p branch)) (string-match "\\(.+\\)/\\(.+\\)" branch))
              (user-error "Invalid format. Expected 'remote/branch' like 'origin/master', got: '%s'" branch))
            branch))
         ;; Check if branch is nil (user cancelled) before trying to match
         (main-remote (when main-integration-branch (match-string 1 main-integration-branch)))
         (main-branch-name (when main-integration-branch (match-string 2 main-integration-branch)))
         (main-merge-ref (when main-branch-name (concat "refs/heads/" main-branch-name))) ; e.g., "refs/heads/master"

         ;; --- Data Gathering ---
         ;; Need to ensure main-remote is valid before proceeding
         (existing-remotes-output (when main-remote (magit-git-string "branch" "-r" "--list" (concat main-remote "/*") "--no-color")))
         (local-branches (magit-list-local-branches))
         ;; --- Processing Variables ---
         (existing-remotes-hash (make-hash-table :test 'equal)) ; Store *full* remote names, e.g., "origin/feat/X"
         (branches-to-prune '()))

    ;; --- Basic Checks ---
    ;; Added checks for nil values from cancelled input or failed regex
    (unless main-integration-branch (user-error "No main integration branch selected"))
    (unless main-remote (user-error "Could not determine remote from input: %s" main-integration-branch))
    (unless main-branch-name (user-error "Could not determine branch name from input: %s" main-integration-branch))
    (unless local-branches (user-error "Failed to get local branches"))
    (unless existing-remotes-output (user-error "Failed to get remote branches for %s" main-remote))

    ;; --- Step 1: Populate hash table of existing remote branches ---
    (with-temp-buffer
      (insert existing-remotes-output)
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*\\(.+\\)$" nil t)
        (puthash (string-trim (match-string 1)) t existing-remotes-hash)))
    (message "Found %d existing remote branches for '%s'." (hash-table-count existing-remotes-hash) main-remote)

    ;; --- Step 2: Check each local branch's configuration ---
    (message "Checking local branch configurations...")
    (dolist (local-branch-ref local-branches) ; Rename variable to indicate it's a ref
      ;; --- Get the SHORT branch name ---
      (let ((short-local-branch (replace-regexp-in-string "^refs/heads/" "" local-branch-ref)))
        ;; --- Perform checks using the SHORT name ---
        (when (and (not (string= short-local-branch current-branch))
                   (not (member short-local-branch protected-branches)))
          (message "  Checking %s..." short-local-branch)
          ;; --- Query config and build counterpart using the SHORT name ---
          (let* ((config-key-prefix (concat "branch." short-local-branch "."))
                 (configured-remote (ignore-errors (magit-git-string "config" "--get" (concat config-key-prefix "remote"))))
                 (configured-merge (ignore-errors (magit-git-string "config" "--get" (concat config-key-prefix "merge"))))
                 (counterpart-remote-branch (concat main-remote "/" short-local-branch))) ; Use short name

            ;; Condition 1: Does configured upstream match the main integration branch?
            (when (and configured-remote configured-merge ; Ensure config exists
                       (string= configured-remote main-remote)
                       (string= configured-merge main-merge-ref))
              (message "    -> Upstream matches %s." main-integration-branch)
              ;; Condition 2: Does its own counterpart NOT exist anymore?
              (unless (gethash counterpart-remote-branch existing-remotes-hash)
                (message "    -> Counterpart %s is GONE. Adding to prune list." counterpart-remote-branch)
                ;; Push the SHORT name to the prune list
                (push short-local-branch branches-to-prune))
              (when (gethash counterpart-remote-branch existing-remotes-hash)
                (message "    -> Counterpart %s still exists." counterpart-remote-branch)))
            (unless (and configured-remote configured-merge)
              (message "    -> No upstream configuration found.")) ; Removed confusing "or doesn't match" part
            (when (and configured-remote configured-merge
                       (or (not (string= configured-remote main-remote))
                           (not (string= configured-merge main-merge-ref))))
              (message "    -> Upstream (%s %s) does not match target (%s)."
                       configured-remote configured-merge main-integration-branch))
            )))
      )

    ;; --- Step 3: Delete branches if any were found ---
        ;; --- Step 3: Delete branches if any were found ---
    (setq branches-to-prune (nreverse branches-to-prune))

    (if branches-to-prune
        (progn
          (message "Found %d local branches tracking '%s' whose counterparts on '%s' are gone: %s"
                   (length branches-to-prune) main-integration-branch main-remote
                   (mapconcat #'identity branches-to-prune ", "))

          ;; --- Iterate and delete/prompt individually ---
          (let ((deleted-count 0)
                (skipped-count 0)
                (error-count 0))
            (dolist (branch branches-to-prune)
              (condition-case err ; Catch potential errors during deletion attempt
                  (if (magit-branch-merged-p branch current-branch)
                      ;; Branch IS merged into current HEAD, safe delete should work
                      (progn
                        (message "Attempting safe delete (-d) for merged branch '%s'..." branch)
                        ;; Use standard magit-branch-delete for safe delete
                        (magit-branch-delete branch)
                        (message "'%s' deleted." branch)
                        (setq deleted-count (1+ deleted-count)))
                    ;; Branch is NOT merged into current HEAD
                    (if (yes-or-no-p (format "Branch '%s' not fully merged. Force delete (-D)? " branch))
                        ;; User confirmed force delete
                        (progn
                          (message "Attempting force delete (-D) for unmerged branch '%s'..." branch)
                          ;; --- Use magit-run-git directly with -D ---
                          ;; This bypasses magit-branch-delete's force flag logic
                          (magit-run-git "branch" "-D" branch)
                          ;; Note: magit-run-git is async by default but for simple commands like
                          ;; branch -D it should be fine. We might need more complex handling
                          ;; (callbacks, sync versions) if errors aren't caught properly,
                          ;; but let's try this first. Assuming success if no error thrown.
                          (message "'%s' force-deletion attempted." branch)
                          (setq deleted-count (1+ deleted-count)))
                      ;; User declined force delete
                      (progn
                        (message "Skipping unmerged branch '%s'." branch)
                        (setq skipped-count (1+ skipped-count)))))
                ;; --- Error Handling for this branch ---
                (error (message "Error processing branch '%s': %s" branch err)
                       (setq error-count (1+ error-count)))))
            ;; --- Final Summary ---
            (message "Pruning finished. Attempted Deletes: %d, Skipped: %d, Errors during process: %d"
                     deleted-count skipped-count error-count)
            ;; Refresh Magit status buffer after potential changes
            (magit-refresh)))
      (message "No local branches found tracking '%s' whose counterparts on '%s' are gone."
               main-integration-branch main-remote))))

; findlibrary magit-branch RET
;(with-eval-after-load 'magit-branch
;  ;; Redefine magit-branch to add our group.
;  (transient-define-prefix magit-branch (branch)
;    "Add, configure or remove a branch."
;    :man-page "git-branch"
;    [:if (lambda () (and magit-branch-direct-configure (transient-scope)))
;     :description
;     (lambda ()
;       (concat (propertize "Configure " 'face 'transient-heading)
;	       (propertize (transient-scope) 'face 'magit-branch-local)))
;     ("d" magit-branch.<branch>.description)
;     ("u" magit-branch.<branch>.merge/remote)
;     ("r" magit-branch.<branch>.rebase)
;     ("p" magit-branch.<branch>.pushRemote)]
;    [:if-non-nil magit-branch-direct-configure
;     :description "Configure repository defaults"
;     ("R" magit-pull.rebase)
;     ("P" magit-remote.pushDefault)
;     ("B" "Update default branch" magit-update-default-branch
;      :inapt-if-not magit-get-some-remote)]
;    ["Arguments"
;     (7 "-r" "Recurse submodules when checking out an existing branch"
;	"--recurse-submodules")]
;    [["Checkout"
;      ("b" "branch/revision"   magit-checkout)
;      ("l" "local branch"      magit-branch-checkout)
;      (6 "o" "new orphan"      magit-branch-orphan)]
;     [""
;      ("c" "new branch"        magit-branch-and-checkout)
;      ("s" "new spin-off"      magit-branch-spinoff)
;      (5 "w" "new worktree"    magit-worktree-checkout)]
;     ["Create"
;      ("n" "new branch"        magit-branch-create)
;      ("S" "new spin-out"      magit-branch-spinout)
;      (5 "W" "new worktree"    magit-worktree-branch)]
;     ["Do"
;      ("C" "configure..."      magit-branch-configure)
;      ("m" "rename"            magit-branch-rename)
;      ("x" "reset"             magit-branch-reset)
;      ("k" "delete"            magit-branch-delete)
;      ("K" "prune-gone"        my-magit-prune-conceptual-upstream)]
;     [""
;      (7 "h" "shelve"          magit-branch-shelve)
;      (7 "H" "unshelve"        magit-branch-unshelve)]]
;    (interactive (list (magit-get-current-branch)))
;    (transient-setup 'magit-branch nil nil :scope branch))
;  )

(provide 'elisp)

;;; elisp.el ends here
