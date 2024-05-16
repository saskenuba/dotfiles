; Font settings  -*- lexical-binding: t; -*-

;; This is only needed once, near the top of the file
(eval-when-compile 

  ;; Add MELPA repository
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

  ;; Add MELPA Stable repository
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

  ;; Add Org ELPA repository
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

  (require 'use-package))

; force download and installation of packages
(setq use-package-always-ensure t)

(if init-file-debug
      (setq use-package-verbose t
            use-package-expand-minimally nil
            use-package-compute-statistics t
            debug-on-error t)
    (setq use-package-verbose nil
          use-package-expand-minimally t))

(defvar martmacs/default-font-size 100)
(set-face-attribute 'default nil :font "CommitMono Nerd Font" :height martmacs/default-font-size)

; Disable visible scrollbar
(setq inhibit-startup-message t)

; Disable the toolbar
(scroll-bar-mode -1)

; Saves last location
; (desktop-save-mode 1)

; Disable tooltips
(tool-bar-mode -1)

; Give some breathing room
(tooltip-mode -1)

; Disable the menu bar
(set-fringe-mode 10)

; Set to use y and n simply by pressing the key
(setopt use-short-answers t)

;; Set up the visible bell
(menu-bar-mode -1)

;; Setup support for recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;; Setup bell
(setq visible-bell t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;;; other (refactor later)

;; https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
(defun Fuco1/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.

The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(add-hook 'emacs-lisp-mode-hook
	  (lambda () (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)))

;;; packages 

(use-package general
  :ensure t
  :config
  (general-create-definer global-definer
    :keymaps 'override
    :states '(emacs normal hybrid motion visual operator)
    :prefix "SPC"
    :non-normal-prefix "S-SPC")

  (defmacro +general-global-menu! (name infix-key &rest body)
    "Create a definer named +general-global-NAME wrapping global-definer.
Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
    (declare (indent 2))
    `(progn
       (general-create-definer ,(intern (concat "+general-global-" name))
	 :wrapping global-definer
	 :prefix-map (quote ,(intern (concat "+general-global-" name "-map")))
	 :infix ,infix-key
	 :wk-full-keys nil
	 "" '(:ignore t :which-key ,name))
       (,(intern (concat "+general-global-" name))
	,@body))))

; (use-package iedit)

(use-package org)

(use-package evil-iedit-state
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (setq iedit-current-symbol-default t
	iedit-only-at-symbol-boundaries t
	iedit-toggle-key-default nil))


;;; Enable vertico and extensions
(use-package vertico
  :demand t
  :init
  (vertico-mode)
  (setq vertico-scroll-margin 0)
  (setq vertico-count 13))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook  ((marginalia-mode . all-the-icons-completion-marginalia-setup))
  :init  (all-the-icons-completion-mode))

;; (use-package doom-modeline
;;   :straight t
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   :config (setq inhibit-compacting-font-caches t)
;;   :custom ((doom-modeline-height 15)))

; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init (savehist-mode))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  
  :config
  (evil-mode 1)
  (evil-set-undo-system #'undo-redo)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

; (use-package command-log-mode)

(use-package ef-themes
  :init (load-theme 'ef-cyprus :no-confirm))

(use-package orderless
  :custom
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp)))

(use-package pdf-tools)

(use-package auctex
  :defer t
  :hook (('LaTeX-mode-hook . 'latex/auto-fill-mode)
	 ('LaTeX-mode-hook . 'TeX-source-correlate-mode)
	 ('doc-view-mode-hook . 'auto-revert-mode))
  
  :config
  (setq TeX-parse-self t)
  (setq TeX-auto-save t)
  (setq TeX-engine #'latex-build-engine)
  (setq TeX-command-default #'latex-build-command)

  ; set zathura as default viewer
  (add-to-list 'TeX-view-program-list '("Zathura" ("zathura" " %o")))
  (setcdr (assq 'output-pdf TeX-view-program-selection) '("Zathura")))

(use-package marginalia
  :general
  (:keymaps 'minibuffer-local-map
	    "M-A" 'marginalia-cycle)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Example configuration for Consult
(use-package consult

  ;; Replace bindings. Lazily loaded due by `use-package'.
  :ensure t
  :general
  (global-definer
    "f/" #'consult-fd
    "/"  #'consult-ripgrep)

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook
  (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package corfu
  :ensure t

  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.25)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect-first t)
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s)

  :bind
  (:map corfu-map
	("TAB" . corfu-next)
	([tab] . corfu-next))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (setq completion-styles '(orderless basic))
  (global-corfu-mode)

  :general
  ("C-SPC" #'completion-at-point))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)
   (clojure-mode    . rainbow-delimiters-mode)))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package magit
  :general
  (global-definer
   "gs" #'magit-status-quick))

(use-package magit-delta
  :hook ((magit-mode . magit-delta-mode)))

(use-package forge
  :after magit)

(use-package smartparens
  :init   (smartparens-global-mode)
  :hook
  ((emacs-lisp-mode . (lambda () (smartparens-strict-mode))))
  :config (require 'smartparens-config))

(use-package symex
  :init
  (setq symex--user-evil-keyspec
	'(("j"		.	symex-go-up)
	  ("k"		.	symex-go-down)
	  ("C-j"	.	symex-climb-branch)
	  ("C-k"	.	symex-descend-branch)
	  ("M-j"	.	symex-goto-highest)
	  ("M-k"	.	symex-goto-lowest)
	  ("M-1"	.	symex-cycle-quote)
          ("M-["	.	symex-create-curly)
          ("M-]"	.	symex-wrap-curly)))
  
  :config
  (symex-initialize)
  (global-set-key (kbd "s-;") 'symex-mode-interface)  ; or whatever keybinding you like
  :hook
  ((clojure-mode . (lambda ()
		     (setq symex-quote-prefix-list (list "#'"))
		     (evil-define-key 'normal symex-mode-map (kbd "<escape>") 'symex-mode-interface)
		     (evil-define-key 'insert symex-mode-map (kbd "<escape>") 'symex-mode-interface)))
   (emacs-lisp-mode . (lambda ()
			(setq symex-quote-prefix-list (list "'" "#'"))
			(evil-define-key 'normal symex-mode-map (kbd "<escape>") 'symex-mode-interface)
			(evil-define-key 'insert symex-mode-map (kbd "<escape>") 'symex-mode-interface)))))
(use-package avy)

(use-package markdown-mode
  :init (setq markdown-command "multimarkdown")
  :mode ("README\\.md'" . gfm-mode))

(use-package yasnippet
  :init (yas-global-mode 1))


;(use-package lsp-bridge
;  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;			 :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;			 :build (:not compile))
;  :init (global-lsp-bridge-mode)

  ; :hook ((rust-mode . lsp-bridge-mode))

;  :config (setq lsp-bridge-peek-list-height 10)
;  :general (global-definer "a" #'lsp-bridge-code-action))

(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000)

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((clojure-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package treemacs
  :general
  ("M-S-0" #'treemacs-select-window))

(use-package lsp-treemacs
  :after '(lsp-mode treemacs)
  :commands lsp-treemacs-errors-list)

;(use-package treemacs-all-the-icons
;  :after '(all-the-icons treemacs)
;  :if treemacs-use-all-the-icons-theme
;  :hook ((treemacs-mode . (lambda () (treemacs-load-theme 'all-the-icons)))))

(use-package treemacs-evil
  :after treemacs
  :hook '(treemacs-mode-hook . evil-treemacs-state))

(use-package treemacs-magit
  :defer t
  :after '(treemacs magit))

(use-package org-roam
  :config
  (setq org-roam-directory "~/Dropbox/Pessoal/Notes")
  (org-roam-db-autosync-mode)
  (setq org-M-RET-may-split-line nil)

  :general
  ("M-S-<return>" #'org-insert-item))

(use-package ox-hugo
  :after ox)

(use-package uuidgen)

(use-package cider)

(use-package clojure-mode)

(defun my-clojure-mode-setup ()
  "Custom setup for clojure-mode."
  ; (lsp-bridge-mode 1) ; Activate lsp-bridge-mode
  ; (corfu-mode -1) ; Deactivate corfu-mode
  ; (acm-mode 1) ; Activate acm-mode
  (general-create-definer clojure-definer
    :keymaps 'override
    :states '(emacs normal hybrid motion visual operator)
    :prefix ",")

  (clojure-definer
   "a" #'lsp-bridge-code-action
   "r" #'lsp-bridge-rename
   "e" #'lsp-bridge-diagnostic-list
   
   ;; "=" #'rust-format-buffer
   
   "sj" #'cider-jack-in-clj
   "sc" #'cider-connect-clj
   "sq" #'sesman-quit
   "sa" #'cider-switch-to-repl-buffer
   ; "rs" #'cider-switch-to-repl-buffer
   
   "hh" #'lsp-ui-doc--display
   "gi" #'lsp-find-implementation
   "gI" #'lsp-bridge-find-impl-other-window
   "gr" #'lsp-find-references
   "Gr" #'lsp-ui-peek-find-references
   "gt" #'lsp-bridge-find-type-def
   "gT" #'lsp-bridge-find-type-def-other-window
   "gd" #'lsp-find-definition
   "gD" #'xref-find-definitions-other-window))

(defun my-rust-mode-setup ()
  "Custom setup for rust-mode."
  ; (lsp-bridge-mode 1) ; Activate lsp-bridge-mode
  (corfu-mode -1) ; Deactivate corfu-mode
  (acm-mode 1) ; Activate acm-mode
  (general-create-definer rust-definer
    :keymaps 'override
    :states '(emacs normal hybrid motion visual operator)
    :prefix ",")

  (rust-definer
   "a" #'lsp-bridge-code-action
   "r" #'lsp-bridge-rename
   "e" #'lsp-bridge-diagnostic-list
   "=" #'rust-format-buffer

   "bc" #'rust-run-clippy
   "bb" #'rust-compile
   "bt" #'rust-test
   
   "hh" #'lsp-bridge-show-documentation
   "gi" #'lsp-bridge-find-impl
   "gI" #'lsp-bridge-find-impl-other-window
   "gr" #'lsp-bridge-find-references
   "Gr" #'lsp-bridge-peek
   "gt" #'lsp-bridge-find-type-def
   "gT" #'lsp-bridge-find-type-def-other-window
   "gd" #'lsp-find-definition
   "gG" #'lsp-bridge-find-def-other-window))

(general-evil-setup t)
(add-hook 'rust-mode-hook 'my-rust-mode-setup)
(add-hook 'clojure-mode-hook 'my-clojure-mode-setup)
(add-hook 'lsp-bridge-peek-mode-hook 'evil-normalize-keymaps)
(add-hook 'lsp-bridge-ref-mode-hook 'evil-normalize-keymaps)
(nmap :keymaps 'lsp-bridge-peek-keymap
  "j" 'lsp-bridge-peek-list-next-line
  "k" 'lsp-bridge-peek-list-prev-line
  "J" 'lsp-bridge-peek-tree-next-branch
  "K" 'lsp-bridge-peek-tree-previous-branch
  "RET" 'lsp-bridge-peek-jump

  "q" 'lsp-bridge-peek-abort
  "<escape>" 'lsp-bridge-peek-abort)

(nmap :keymaps 'lsp-bridge-ref-mode-map
  "j" 'lsp-bridge-ref-jump-next-keyword
  "k" 'lsp-bridge-ref-jump-prev-keyword
  "J" 'lsp-bridge-ref-jump-next-file
  "K" 'lsp-bridge-ref-jump-prev-file
  "RET" 'lsp-bridge-ref-open-file-and-stay

  "q" 'lsp-bridge-ref-quit
  "<escape>" 'lsp-bridge-ref-quit)

;; Spacemacs-like buffer menus
(global-definer
  "SPC" '(execute-extended-command :which-key "Execute command")
  "qr"  #'restart-emacs
  "qq"  #'kill-emacs
  "TAB" '((lambda () (interactive) (switch-to-buffer nil))
	  :which-key "Swap to last used buffer")
  
  "ik" '(lambda () (interactive) (evil-insert-newline-above))
  "ij" '(lambda () (interactive) (evil-insert-newline-below))
  "jj" '(avy-goto-char-timer :which-key "Ace jump")
  
  
  "hs" '((lambda () (interactive) (describe-symbol (symbol-at-point)))
	 :which-key "Describe symbol at point"))

(+general-global-menu! "Buffer" "b"
  "b" #'consult-buffer
  "d" '(kill-current-buffer :which-key "Kill this buffer"))

(+general-global-menu! "Window" "w"
  "h"  #'evil-window-left
  "l"  #'evil-window-right
  "s"  #'evil-window-vsplit
  "2"  #'evil-window-vsplit
  "1"  #'delete-other-windows
  "d"  #'delete-window
  "r"  #'evil-window-rotate-downwards)

(+general-global-menu! "File" "f"
  "ei" '((lambda () (interactive) (find-file "~/.emacs.d/init.el"))
	 :which-key "Open init.el")
  "f" '(find-file :which-key)
  "s"  '(save-buffer :which-key "Save buffer"))

(+general-global-menu! "Project" "p"
  "f" #'project-find-file)


;; LSP booster to make json parsing comms faster
;; installation needed: https://github.com/blahgeek/emacs-lsp-booster
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
;; end of lsp booster

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("73c55f5fd22b6fd44f1979b6374ca7cc0a1614ee8ca5d4f1366a0f67da255627" "01aef17f41edea53c665cb57320bd80393761f836be5ab0bd53292afc94bd14d" "a6a979c8b7ccb1d4536f4fa74a6e47674a3ce65feea3fecdf1d9dc448fac47e0" default))
 '(package-selected-packages
   '(treemacs-evil treemacs-all-the-icons treemacs-magit flycheck marginalia rainbow-delimiters smartparens-mode symex smartparens evil-collection evil command-log-mode))
 '(safe-local-variable-values '((TeX-encoding . UTF-8)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
