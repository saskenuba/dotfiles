(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (my-reload-dir-locals-for-current-buffer))))))

(defun dit-start-recording ()
  "Start screencast and F12 to adv presentation."
  (sha/screencast)
  (demo-it-step))

(defun dit-stop-recording ()
  "End screencast and presentation."
  (sha/screencast-handler)
  (demo-it-end))

(defun dit-goto-last-line ()
  "End screencast and presentation."
  (with-current-buffer (current-buffer)
    (evil-goto-line)))

(defun dit-python-repl-eval-buffer ()
  "Open python repl, and send whole buffer to it."
  (if (equal major-mode 'python-mode)
      (with-current-buffer (current-buffer)
        (python-shell-send-buffer))))

(defun dit-python-repl-open
    (&rest
     args)
  "Open python repl, change font scale, and send buffer to it"
  (let ((count-size (plist-get args
                               :size)))
    (if (equal major-mode 'python-mode)
        (with-current-buffer (current-buffer)
          (run-python nil nil 1)
          (other-window 1)
          (text-scale-increase count-size)
          (window-resize nil -10 1)
          (other-window 1)))))

(defun dit-python-eval-and-clear ()
  (dit-python-repl-eval-buffer)
  (dit-clear-buffer))

(defun dit-clear-buffer ()
  "Complete erase current buffer."
  (with-current-buffer (current-buffer)
    (erase-buffer)))

(defun auto-type-to-buffer (filepath)
  "Simulates typing in current buffer using text in other file
     This function is supposedly to ran within a demo-it presentation. "
  (interactive)
  (with-current-buffer (current-buffer)
    (setq-local python-indent-offset 0)
    (demo-it-disable-mode)
    (smartparens-mode -1)
    (evil-goto-line)
    (evil-insert-state 1))
  (let ((proc (start-process "auto-write-process" "main.py" "python"
                             "/home/martin/Documentos/Programming/Python/Projetos/Text-To-Keyboard/main.py"
                             filepath))))
  (set-process-sentinel (get-process "auto-write-process")
                        (lambda (p e)
                          (when (= 0 (process-exit-status p))
                            (message "OK")
                            (with-current-buffer (current-buffer)
                              (smartparens-mode)
                              (demo-it-mode)
                              (demo-it-mode-adv)
                              (evil-normal-state 1))))))

(defun sha/screencast
    (&optional
     output-file)
  (setq activeWindowID (shell-command-to-string "xdotool getactivewindow"))
  (let ((output-file (or output-file
                         (concat "/tmp/" (format "%S" (abs (random))) ".mp4"))))
    (message  "screen-cast started")
    (start-process "screencasting" (get-buffer-create "*screencast-buffer*") "ffcast" "-#"
                   activeWindowID "rec" output-file) output-file))

(defun sha/screencast-handler ()
  (interactive)
  (if (process-status "screencasting")
      (progn (process-send-string "screencasting" "q")
             (message "screen-cast stopped"))
    (sha/screencast)))

(global-set-key (kbd "s--") 'sha/screencast-handler)

;; (load "/home/martin/.emacs.d/private/animations/waves.el")
;; (load "/home/martin/.emacs.d/private/animations/algo.el")
;; (load "/home/martin/.emacs.d/private/animations/end-of-buffer.el")
;; (load "/home/martin/.emacs.d/private/animations/the-stars.el")

(defun manim-render-current-scene (quality)
  (interactive)
  (save-buffer)
  (setq last-point-location (point))
  (re-search-backward "class \\(.*\\)(")
  (setq class-name (match-string 1))
  (setq poetry-path "/home/martin/.poetry/bin/poetry")
  (setq manim-path "/home/martin/Documentos/src/manim/")
  (async-shell-command (mapconcat 'identity (list "cd " manim-path ";" "poetry run " (concat
                                                                                            manim-path
                                                                                            "manim.py ")
                                                  default-directory (buffer-name) " " class-name
                                                  " -p" quality) ""))
  (other-window 1)
  (set-window-point nil last-point-location))

(defun manim-render-low ()
  "Render current scene with low quality."
  (interactive)
  (manim-render-current-scene "l"))

(defun manim-render-medium ()
  "Render current scene with medium quality."
  (interactive)
  (manim-render-current-scene "m"))

(defun manim-render-high ()
  "Render current scene with standard high quality."
  (interactive)
  (manim-render-current-scene ""))

(defun manim-render-last-frame ()
  "Render last frame of current scene."
  (interactive)
  (manim-render-current-scene "s"))

(defun my/org-sidebar (source-buffer)
  "Return an Org QL View buffer showing upcoming items in SOURCE-BUFFER."
  (let ((display-buffer (generate-new-buffer (format "org-sidebar<%s>" (buffer-name
                                                                        source-buffer))))
        (title (concat "Headers and subcategories: " (buffer-name source-buffer))))
    (with-current-buffer display-buffer (setf org-sidebar-source-buffer source-buffer))
    (save-window-excursion
      ;; `org-ql-search' displays the buffer, but we don't want to do that here.
      (org-ql-search source-buffer '(heading ".*")
        :narrow t
        :sort 'date
        :super-groups '((:auto-planning))
        :buffer display-buffer
        :title title)) display-buffer))

(defun modus-themes-toggle ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (progn
        (disable-theme 'modus-operandi)
        (modus-vivendi-theme-load))
    (disable-theme 'modus-vivendi)
    (modus-operandi-theme-load)))

(defun cider-send-reset ()
  (interactive)
  (cider-interactive-eval
   "(do (ns user)
        (reset))"))

(defun cider-send-reset-tests ()
  (interactive)
  (cider-interactive-eval
   "(do (ns user)
        (reset-tests))"))

(defun cider-send-restart ()
  (interactive)
  (cider-interactive-eval
   "(do (ns user)
        (restart))"))

(defun cider-send-go ()
  (interactive)
  (cider-interactive-eval
   "(do (ns user)
        (go))"))

(defun cider-send-halt ()
  (interactive)
  (cider-interactive-eval
   "(do (ns user)
        (halt))"))

(defun th/magit--with-difftastic (buffer command)
  "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
  (let ((process-environment
         (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                       (number-to-string (frame-width)))
               process-environment)))
    ;; Clear the result buffer (we might regenerate a diff, e.g., for
    ;; the current changes in our working directory).
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    ;; Now spawn a process calling the git COMMAND.
    (make-process
     :name (buffer-name buffer)
     :buffer buffer
     :command command
     ;; Don't query for running processes when emacs is quit.
     :noquery t
     ;; Show the result buffer once the process has finished.
     :sentinel (lambda (proc event)
                 (when (eq (process-status proc) 'exit)
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-min))
                     (ansi-color-apply-on-region (point-min) (point-max))
                     (setq buffer-read-only t)
                     (view-mode)
                     (end-of-line)
                     ;; difftastic diffs are usually 2-column side-by-side,
                     ;; so ensure our window is wide enough.
                     (let ((width (current-column)))
                       (while (zerop (forward-line 1))
                         (end-of-line)
                         (setq width (max (current-column) width)))
                       ;; Add column size of fringes
                       (setq width (+ width
                                      (fringe-columns 'left)
                                      (fringe-columns 'right)))
                       (goto-char (point-min))
                       (pop-to-buffer
                        (current-buffer)
                        `(;; If the buffer is that wide that splitting the frame in
                          ;; two side-by-side windows would result in less than
                          ;; 80 columns left, ensure it's shown at the bottom.
                          ,(when (> 80 (- (frame-width) width))
                             #'display-buffer-at-bottom)
                          (window-width
                           . ,(min width (frame-width))))))))))))

(defun th/magit-show-with-difftastic (rev)
  "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If REV is given, just use it.
          (when (boundp 'rev) rev)
          ;; If not invoked with prefix arg, try to guess the REV from
          ;; point's position.
          (and (not current-prefix-arg)
               (or (magit-thing-at-point 'git-revision t)
                   (magit-branch-or-commit-at-point)))
          ;; Otherwise, query the user.
          (magit-read-branch-or-commit "Revision"))))
  (if (not rev)
      (error "No revision specified")
    (th/magit--with-difftastic
     (get-buffer-create (concat "*git show difftastic " rev "*"))
     (list "git" "--no-pager" "show" "--ext-diff" rev))))

(defun th/magit-diff-with-difftastic (arg)
  "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If RANGE is given, just use it.
          (when (boundp 'range) range)
          ;; If prefix arg is given, query the user.
          (and current-prefix-arg
               (magit-diff-read-range-or-commit "Range"))
          ;; Otherwise, auto-guess based on position of point, e.g., based on
          ;; if we are in the Staged or Unstaged section.
          (pcase (magit-diff--dwim)
            ('unmerged (error "unmerged is not yet implemented"))
            ('unstaged nil)
            ('staged "--cached")
            (`(stash . ,value) (error "stash is not yet implemented"))
            (`(commit . ,value) (format "%s^..%s" value value))
            ((and range (pred stringp)) range)
            (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
  (let ((name (concat "*git diff difftastic"
                      (if arg (concat " " arg) "")
                      "*")))
    (th/magit--with-difftastic
     (get-buffer-create name)
     `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

(defun mm/cape-capf-setup-lsp ()
  "Replace the default `lsp-completion-at-point' with its
`cape-capf-buster' version. Also add `cape-file' and
`company-yasnippet' backends."

  ;; (setf (elt (cl-member 'lsp-completion-at-point completion-at-point-functions) 0)
  ;;       (cape-super-capf (cape-capf-buster #'lsp-completion-at-point)
  ;;                        #'cape-dabbrev))

  ;; TODO 2022-02-28: Maybe use `cape-wrap-predicate' to have candidates
  ;; listed when I want?
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  )

(defun mm/cape-capf-setup-clojure ()


  ;; we replace our completions after cider hooks up
  (add-hook 'cider-mode-hook
            (lambda ()
              (setq completion-at-point-functions
                    (list (cape-super-capf ; #_(cape-capf-buster #'lsp-completion-at-point)
                                           #'lsp-completion-at-point
                                           #'cider-complete-at-point
                                           #'cape-dabbrev)
                          #'cape-file)
                    )))
  )


(defun mm/cape-capf-ignore-keywords-elisp (cand)
  "Ignore keywords with forms that begin with \":\" (e.g. :history)."
  (or (not (keywordp cand))
      (eq (char-after (car completion-in-region--data)) ?:)))

(defun mm/cape-capf-setup-elisp ()
  "Replace the default `elisp-completion-at-point'
completion-at-point-function. Doing it this way will prevent
disrupting the addition of other capfs (e.g. merely setting the
variable entirely, or adding to list).

Additionally, add `cape-file' as early as possible to the list."
  (setf (elt (cl-member 'elisp-completion-at-point completion-at-point-functions) 0)
        #'elisp-completion-at-point)

  ;; I prefer this being early/first in the list
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-symbol t)
  )

(defun mm/cape-capf-setup-org ()
  (if (org-roam-file-p)
      (org-roam--register-completion-functions-h)
    (let (result)
      (dolist (element (list
                        (cape-super-capf #'cape-ispell #'cape-dabbrev)
                        (cape-company-to-capf #'company-yasnippet))
                       result)
        (add-to-list 'completion-at-point-functions element)))
    ))

(defun mm/consult-configure()

  ;; TODO this should be generic over any lsp-mode
  (add-hook 'clojure-mode-hook
            (lambda ()
              (consult-customize
               consult-ripgrep
               consult-git-grep
               consult-grep
               consult-lsp-symbols
               :preview-key '(:debounce 0.1 any))
              )
            )

  )
