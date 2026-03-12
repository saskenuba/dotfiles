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

(defun my/copy-file-path--build (absolute-p line-p column-p)
  "Build a file path string and copy to kill ring.
When ABSOLUTE-P is nil, uses project-relative path.
When LINE-P, appends :line.  When COLUMN-P, appends :column."
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer is not visiting a file"))
    (let* ((path (if absolute-p
                     file
                   (if-let ((proj (project-current)))
                       (let ((root (project-root proj)))
                         (unless (file-in-directory-p file root)
                           (user-error "File is not within the project"))
                         (file-relative-name file root))
                     (user-error "Not in a project"))))
           (result (concat path
                           (when line-p
                             (format ":%d" (line-number-at-pos (point) t)))
                           (when column-p
                             (format ":%d" (1+ (current-column)))))))
      (kill-new result)
      (message "Copied: %s" result))))

(defun copy-absolute-path ()
  "Copy absolute file path."
  (interactive)
  (my/copy-file-path--build t nil nil))

(defun copy-absolute-path-and-line ()
  "Copy absolute file path with line number."
  (interactive)
  (my/copy-file-path--build t t nil))

(defun copy-absolute-path-line-and-column ()
  "Copy absolute file path with line and column."
  (interactive)
  (my/copy-file-path--build t t t))

(defun copy-relative-path ()
  "Copy project-relative file path."
  (interactive)
  (my/copy-file-path--build nil nil nil))

(defun copy-relative-path-and-line ()
  "Copy project-relative file path with line number."
  (interactive)
  (my/copy-file-path--build nil t nil))

(defun copy-relative-path-line-and-column ()
  "Copy project-relative file path with line and column."
  (interactive)
  (my/copy-file-path--build nil t t))

(defun copy-project-tree-to-clipboard (&optional max-depth exclude-patterns)
  "Run tree command at project root and copy output to clipboard.
Optional MAX-DEPTH limits directory depth (default: no limit).
Optional EXCLUDE-PATTERNS is a list of patterns to exclude."
  (interactive)
  (let ((proj (project-current)))
    (unless proj
      (user-error "Not in a project"))
    (let* ((root (project-root proj))
           (default-directory root)
           (depth-args (when max-depth (list "-L" (number-to-string max-depth))))
           (exclude-args (when exclude-patterns
                          (mapcan (lambda (pattern) (list "-I" pattern)) exclude-patterns)))
           ;; Build arguments without "tree" prefix since call-process takes command separately
           (tree-args (append depth-args exclude-args '(".")))
           (tree-output (with-temp-buffer
                         (let ((exit-code (apply #'call-process "tree" nil t nil tree-args)))
                           (unless (zerop exit-code)
                             (user-error "tree command failed with exit code %d. Make sure 'tree' is installed" exit-code))
                           (buffer-string)))))
      (kill-new tree-output)
      (message "Project tree copied to clipboard (%d lines)"
               (length (split-string tree-output "\n"))))))

(defun copy-project-tree-with-includes (include-patterns &optional max-depth)
  "Copy project tree showing only specified paths and their full contents.
For directories in INCLUDE-PATTERNS, shows complete tree structure within them."
  (interactive)
  (let ((proj (project-current)))
    (unless proj
      (user-error "Not in a project"))
    (let* ((root (project-root proj))
           (default-directory root)
           ;; Filter patterns to only those that exist
           (existing-paths (seq-filter
                           (lambda (pattern)
                             (or (file-exists-p pattern)
                                 ;; Check if pattern matches any file/dir using glob
                                 (not (null (file-expand-wildcards pattern t)))))
                           include-patterns))
           ;; Build tree command with specific paths
           (depth-args (when max-depth (list "-L" (number-to-string max-depth))))
           ;; If no paths exist, show empty result
           (tree-output (if (null existing-paths)
                           "No matching files or directories found."
                         (with-temp-buffer
                           ;; Run tree with all the paths that should be included
                           (let ((exit-code (apply #'call-process "tree" nil t nil
                                                  (append depth-args existing-paths))))
                             (unless (zerop exit-code)
                               (user-error "tree command failed with exit code %d" exit-code))
                             (buffer-string))))))
      (kill-new tree-output)
      (message "Project tree (filtered) copied to clipboard (%d lines)"
               (length (split-string tree-output "\n"))))))

(defun copy-project-tree-filtered ()
  "Copy project tree with exclusions based on detected Projectile project type."
  (interactive)
  (unless (fboundp 'projectile-project-type)
    (user-error "Projectile is not available. Make sure it's installed and loaded"))

  (let* ((project-type (ignore-errors (projectile-project-type)))
         (common-excludes '(".git" ".DS_Store" "*.elc"))
         (config (cond
                  ;; Clojure projects - show important files only
                  ((memq project-type '(lein-test lein-midje boot-clj clojure-cli))
                   (list :mode 'include
                         :patterns '("deps.edn" "project.clj" "build.boot" "shadow-cljs.edn"
                                   "src" "test" "resources" "dev" "README.md" "CHANGELOG.md")))

                  ;; JavaScript/Node projects
                  ((memq project-type '(npm yarn))
                   (list :mode 'exclude
                         :patterns (append common-excludes
                                         '("node_modules" "dist" "build" ".next" ".nuxt" "coverage"))))

                  ;; Python projects
                  ((memq project-type '(python-pip python-pkg python-tox))
                   (list :mode 'exclude
                         :patterns (append common-excludes
                                         '("__pycache__" "*.pyc" "venv" ".venv" "env" ".env"
                                           "build" "dist" "*.egg-info" ".pytest_cache"))))

                  ;; Rust projects
                  ((eq project-type 'rust-cargo)
                   (list :mode 'exclude
                         :patterns (append common-excludes '("target" "Cargo.lock"))))

                  ;; Java/Maven projects
                  ((eq project-type 'maven)
                   (list :mode 'exclude
                         :patterns (append common-excludes '("target" ".mvn"))))

                  ;; Java/Gradle projects
                  ((eq project-type 'gradle)
                   (list :mode 'exclude
                         :patterns (append common-excludes '("build" ".gradle"))))

                  ;; Go projects
                  ((eq project-type 'go)
                   (list :mode 'exclude
                         :patterns (append common-excludes '("vendor"))))

                  ;; Default fallback
                  (t (list :mode 'exclude
                          :patterns (append common-excludes
                                          '("target" "build" "dist" "node_modules"))))))

         (mode (plist-get config :mode))
         (patterns (plist-get config :patterns)))

    (if (eq mode 'include)
        ;; For include mode, use the special include function
        (copy-project-tree-with-includes patterns)
      ;; For exclude mode, use the standard function with exclude patterns
      (copy-project-tree-to-clipboard nil patterns))

    (message "Copied %s project tree (%s: %s)"
             (if project-type (symbol-name project-type) "unknown")
             (if (eq mode 'include) "included" "excluded")
             (mapconcat 'identity patterns ", "))))

(defun copy-project-tree-shallow ()
  "Copy project tree with depth limit of 3 levels."
  (interactive)
  (copy-project-tree-to-clipboard 3))

(defun magit-prune-orphaned-branches (&optional force)
  "Delete local branches whose push remote branch no longer exists.
This finds branches where the push target (e.g., origin/my-feature)
has been deleted from the remote — typically after a PR was merged
and `git fetch --prune' cleaned up the remote-tracking ref.
Excludes the current branch.
With prefix argument FORCE, also delete unmerged branches (git branch -D)."
  (interactive "P")
  (let* ((default-directory (magit-toplevel))
         (current (magit-get-current-branch))
         (orphaned
          (seq-filter
           (lambda (branch)
             (and (not (string= branch current))
                  (when-let ((push-branch (magit-get-push-branch branch)))
                    (not (magit-rev-verify push-branch)))))
           (magit-list-local-branch-names))))
    (if (null orphaned)
        (message "No orphaned local branches found.")
      (let ((buf (get-buffer-create "*Orphaned Branches*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Orphaned local branches (%d):\n\n" (length orphaned)))
            (dolist (branch orphaned)
              (insert (format "  %s\n" branch)))
            (special-mode)))
        (display-buffer buf))
      (unwind-protect
          (when (yes-or-no-p (format "Delete %d branch(es)%s? "
                                     (length orphaned)
                                     (if force " WITH FORCE" "")))
            (let ((deleted 0)
                  (failed nil)
                  (flag (if force "-D" "-d")))
              (dolist (branch orphaned)
                (if (magit-git-success "branch" flag branch)
                    (cl-incf deleted)
                  (push branch failed)))
              (magit-refresh)
              (message "Deleted %d branch(es)%s."
                       deleted
                       (if failed
                           (format ". Failed (unmerged?): %s"
                                   (string-join (nreverse failed) ", "))
                         ""))))
        (when-let ((buf (get-buffer "*Orphaned Branches*")))
          (kill-buffer buf))))))
