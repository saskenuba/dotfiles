;;; Code

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

(provide 'elisp)

;;; elisp.el ends here
