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
