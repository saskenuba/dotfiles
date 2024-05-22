(defun cider-eval-in-repl-no-focus (form)
  "Insert FORM in the REPL buffer and eval it."
  (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
    (setq form (replace-match "" t t form)))
  (with-current-buffer (cider-current-connection)
    (let ((pt-max (point-max)))
      (goto-char pt-max)
      (insert form)
      (indent-region pt-max (point))
      (cider-repl-return)
      (with-selected-window (get-buffer-window (cider-current-connection))
        (goto-char (point-max))))))


(defun cider-test-run-focused-test ()
  "Run test around point."
  (interactive)
  (cider-load-buffer)
  (cider-test-run-test))

(defun cider-send-ns-form-to-repl ()
  "Send buffer's ns form to REPL and evaluate it without changing
the focus."
  (interactive)
  (cider-eval-in-repl-no-focus (cider-ns-form)))

(defun clojure-set-capf ()
  (add-hook 'cider-mode-hook
	    (lambda ()
	      (setq completion-at-point-functions
		    (list (cape-capf-super #'lsp-completion-at-point
					   #'cider-complete-at-point
					   #'dabbrev-capf))))))
(defun cider-run-flowstorm ()
  (interactive)
  (cider-interactive-eval
   "(do (require '[flow-storm.api :as fs-api])
	(fs-api/local-connect))"))

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



(provide 'clojure)
