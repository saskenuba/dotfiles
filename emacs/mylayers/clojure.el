;;; Code

(defun my/setup-clojure-flycheck-cache ()
  "Set up the buffer-local Flycheck cache for Clojure modes.
This function sets `my/flycheck-local-cache` to chain the appropriate
splint checker after the lsp checker based on the major-mode."
  (setq-local my/flycheck-local-cache
              (pcase major-mode
                ((or 'clojure-mode 'clojure-ts-mode)
                 '((lsp . ((next-checkers . (splint-clj))))))
                ((or 'clojurescript-mode 'clojure-ts-clojurescript-mode)
                 '((lsp . ((next-checkers . (splint-cljs))))))
                ((or 'clojurec-mode 'clojure-ts-clojurec-mode)
                 '((lsp . ((next-checkers . (splint-cljc))))))
                (_ nil))))

(add-hook 'clojure-mode-hook #'my/setup-clojure-flycheck-cache)
(add-hook 'clojurescript-mode-hook #'my/setup-clojure-flycheck-cache)
(add-hook 'clojurec-mode-hook #'my/setup-clojure-flycheck-cache)
;; Add hooks for treesitter modes if needed and they don't inherit
;; (add-hook 'clojure-ts-mode-hook #'my/setup-clojure-flycheck-cache)
;; (add-hook 'clojure-ts-clojurescript-mode-hook #'my/setup-clojure-flycheck-cache)
;; (add-hook 'clojure-ts-clojurec-mode-hook #'my/setup-clojure-flycheck-cache)

(defun clojure/fancify-symbols (mode)
  "Pretty symbols for Clojure's anonymous functions and sets,
   like (λ [a] (+ a 5)), ƒ(+ % 5), and ∈{2 4 6}."
  (font-lock-add-keywords mode
                          `(("(\\(fn\\)[[[:space:]]"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "λ")
                                       nil)))
                            ("(\\(partial\\)[[[:space:]]"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "Ƥ")
                                       nil)))
                            ("(\\(comp\\)[[[:space:]]"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "∘")
                                       nil)))
                            ("\\(#\\)("
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "ƒ")
                                       nil)))
                            ("\\(#\\){"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "∈")
                                       nil))))))

(defun lsp-find-implementation-other-window ()
  "Find the implementation of the symbol at point and open it in another window."
  (interactive)
  (let ((orig-window (selected-window))
	(orig-buffer (current-buffer)))
    (lsp-find-implementation)
    (let ((new-buffer (current-buffer)))
      (switch-to-buffer-other-window new-buffer)
      (select-window orig-window)
      (switch-to-buffer orig-buffer)
      (other-window 1))))

(clojure/fancify-symbols 'clojure-mode)
(clojure/fancify-symbols 'clojurescript-mode)
(clojure/fancify-symbols 'cider-repl-mode)
(clojure/fancify-symbols 'cider-clojure-interaction-mode)

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
		    (list (cape-capf-super #'cider-complete-at-point
					   #'lsp-completion-at-point
					   #'dabbrev-capf))))))
(defun cider-run-flowstorm ()
  (interactive)
  (cider-interactive-eval
   "(do (require '[flow-storm.api :as fs-api])
	(fs-api/local-connect)
        (fs-api/start-recording))"))

(defun cider-send-reset ()
  (interactive)
  (cider-interactive-eval
   "(do (reset))"))

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

(defun json->edn ()
  "Convert the selected region, or entire file, from JSON to EDN."
  (interactive)
  (let ((b (if mark-active (region-beginning) (point-min)))
        (e (if mark-active (region-end) (point-max)))
        (jet (when (executable-find "jet")
               "jet --pretty --keywordize keyword --from json --to edn")))
    (if jet
      (let ((p (point)))
        (shell-command-on-region b e jet (current-buffer) t)
        (goto-char p))
      (user-error "Could not find jet installed"))))


(provide 'clojure)

