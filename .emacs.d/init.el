; Font settings  -*- lexical-binding: t; -*-

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;;; Set up the package manager

;; Initialize package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure straight.el to work with use-package
(straight-use-package 'use-package)

;; Configure use-package to use package.el by default
(setq use-package-always-ensure t)  ; Use package.el by default

;; When you want to use straight.el explicitly, use :straight t
;; When you want to use package.el explicitly, use :ensure t (or rely on the default)

;; Optional: Set straight.el to not automatically install packages
;; unless explicitly requested with :straight
(setq straight-use-package-by-default nil)

;; Ensure use-package is installed (via package.el)
(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))
;;; end of package managers

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

; force download and installation of packages
(setq use-package-always-ensure t)
(package-refresh-contents :async)

(if init-file-debug
      (setq use-package-verbose t
            use-package-expand-minimally nil
            use-package-compute-statistics t
            debug-on-error t)
    (setq use-package-verbose nil
          use-package-expand-minimally t))

;; (defvar martmacs/default-font-size 95)
(defvar martmacs/default-font-size 110)

;; Cool fonts:
;; - CommitMono Nerd Font
;; - Cousine Nerd Font Mono
;; - Hasklug Nerd Font
;; - LiterationMono Nerd Font
(set-face-attribute 'default nil
		    :font   "Cousine Nerd Font Mono"
		    :height martmacs/default-font-size
                    :weight 'medium
                    :width  'normal)

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

(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)

;; The built-in `savehist-mode' saves minibuffer histories.  Vertico
;; can then use that information to put recently selected options at
;; the top.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:25765797-27a5-431e-8aa4-cc890a6a913a
(savehist-mode 1)

;; The built-in `recentf-mode' keeps track of recently visited files.
;; You can then access those through the `consult-buffer' interface or
;; with `recentf-open'/`recentf-open-files'.
(recentf-mode 1)

;; Setup bell
(setq visible-bell t)

;; Set to always split vertically
(setq split-width-threshold 100)
(setq split-height-threshold nil)

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
	,@body)))
  
  (general-evil-setup t))

;; automatically cleanup whitespace
(use-package ws-butler
  :init (ws-butler-global-mode))

(use-package sudo-edit)

(use-package expand-region)

(use-package iedit)

(use-package org
  :custom
  (org-babel-clojure-backend 'cider))

(use-package helpful)

(use-package evil-iedit-state
  :after (iedit expand-region)

  :commands (evil-iedit-state
	     evil-iedit-state/iedit-mode)

  :init
  (setq iedit-current-symbol-default t
	iedit-only-at-symbol-boundaries t
	iedit-toggle-key-default nil)

  :general
  (mmap
      "#" #'evil-iedit-state/iedit-mode-from-expand-region
      "*" #'evil-iedit-state/iedit-mode-from-expand-region))

;; (general-define-key
;;    :states '(normal motion)
;;    :keymaps 'evil-iedit-state-map
;;    "#" #'iedit-prev-occurrence
;;    "*" #'iedit-next-occurrence)

;;; Enable vertico and extensions
(use-package vertico
  :demand t
  :init
  (vertico-mode)
  (setq vertico-scroll-margin 0)
  (setq vertico-count 13))

;; Remember to run `all-the-icons-install-fonts'
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

(use-package window-purpose
  :init (purpose-mode)
  :config
  (add-to-list 'purpose-user-mode-purposes '(magit-status-mode . magit))
  (add-to-list 'purpose-user-mode-purposes '(cider-repl-mode . repl))
  ; (purpose-compile-user-configuration)
  )

(use-package writeroom-mode
  :hook '((writeroom-mode . (lambda () (display-line-numbers-mode)))))

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

(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

(use-package evil-visualstar
  :after evil-collection
  :commands (evil-visualstar/begin-search-forward
	     evil-visualstar/begin-search-backward)
  :init
  (global-evil-visualstar-mode)
  :general
  (:keymaps 'evil-visual-state-map
   "*" #'evil-visualstar/begin-search-forward
   "#" #'evil-visualstar/begin-search-backward))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-cleverparens
  :hook ((emacs-lisp-mode . evil-cleverparens-mode)
	 (clojure-mode . evil-cleverparens-mode)
	 (clojurescript-mode . evil-cleverparens-mode)
	 (clojurec-mode . evil-cleverparens-mode)))

(use-package ef-themes)

(use-package modus-themes)

(use-package moe-theme)

(use-package circadian
  :ensure t
  :config
  (setq calendar-latitude -23.484759)
  (setq calendar-longitude -46.622821)
  (setq circadian-themes '((:sunrise . modus-vivendi)
			   (:sunset  . modus-vivendi)))
  (circadian-setup))

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
  ; set zathura as default viewer
  ; (add-to-list 'TeX-view-program-list '("Zathura" ("zathura" " %o")))
  ; (setcdr (assq 'output-pdf TeX-view-program-selection) '("Zathura"))

  :custom
  (TeX-parse-self t)
  (TeX-auto-save t)
  (TeX-engine #'latex-build-engine)
  (TeX-command-default #'latex-build-command))

(use-package marginalia
  :general
  (:keymaps 'minibuffer-local-map
	    "M-A" 'marginalia-cycle)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

;; Example configuration for Consult
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 1))

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

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; smart default action
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings`
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; A few more useful configurations...
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  ;; :bind
  ;; (("C-c p p" . completion-at-point) ;; capf
  ;; 	 ("C-c p t" . complete-tag)        ;; etags
  ;; 	 ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
  ;; 	 ("C-c p h" . cape-history)
  ;; 	 ("C-c p f" . cape-file)
  ;; 	 ("C-c p k" . cape-keyword)
  ;; 	 ("C-c p s" . cape-elisp-symbol)
  ;; 	 ("C-c p e" . cape-elisp-block)
  ;; 	 ("C-c p a" . cape-abbrev)
  ;; 	 ("C-c p l" . cape-line)
  ;; 	 ("C-c p w" . cape-dict)
  ;; 	 ("C-c p :" . cape-emoji)
  ;; 	 ("C-c p \\" . cape-tex)
  ;; 	 ("C-c p _" . cape-tex)
  ;; 	 ("C-c p ^" . cape-tex)
  ;; 	 ("C-c p &" . cape-sgml)
  ;; 	 ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-hook 'completion-at-point-functions #'cape-history)
  ;;(add-hook 'completion-at-point-functions #'cape-keyword)
  ;;(add-hook 'completion-at-point-functions #'cape-tex)
  ;;(add-hook 'completion-at-point-functions #'cape-sgml)
  ;;(add-hook 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-hook 'completion-at-point-functions #'cape-abbrev)
  ;;(add-hook 'completion-at-point-functions #'cape-dict)
  ;;(add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-hook 'completion-at-point-functions #'cape-line)
  )

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

(use-package transient)

(use-package magit
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

  :general
  (global-definer
   "gs" #'magit-status-quick))

(use-package magit-delta
  :hook ((magit-mode . magit-delta-mode)))

(use-package magit-lfs
  :after magit)

(use-package forge
  :after magit)

(use-package projectile)

(use-package consult-projectile)

(use-package smartparens
  :init
  (setq sp-show-pair-from-inside t)
  (setq sp-cancel-autoskip-on-backward-movement nil)
  (setq sp-show-pair-delay 0.2)
  (smartparens-global-mode)

  :hook
  ((emacs-lisp-mode . (lambda () (smartparens-strict-mode))))
  :config (require 'smartparens-config))

(use-package symex-core
  :straight
  (symex-core
   :host github
   :repo "drym-org/symex.el"
   :files ("symex-core/symex*.el")))

(use-package symex
  :after (symex-core)
  :straight
  (symex
   :host github
   :repo "drym-org/symex.el"
   :files ("symex/symex*.el" "symex/doc/*.texi" "symex/doc/figures"))

  :hook
  ((clojure-mode . (lambda ()
		     (setq-local symex-quote-prefix-list (list "#" "'" "@" "#_"))
		     (evil-define-key 'normal symex-mode-map (kbd "<escape>") 'symex-mode-interface)
		     (evil-define-key 'insert symex-mode-map (kbd "<escape>") 'symex-mode-interface)))
   (emacs-lisp-mode . (lambda ()
			(setq symex-quote-prefix-list (list "'" "#'"))
			(evil-define-key 'normal symex-mode-map (kbd "<escape>") 'symex-mode-interface)
			(evil-define-key 'insert symex-mode-map (kbd "<escape>") 'symex-mode-interface))))
  :config
  (symex-mode 1)
  (global-set-key (kbd "s-;") #'symex-mode-interface)
  (lithium-define-keys symex-editing-mode
    (("M-1" symex-cycle-quote)
     ("M-[" symex-create-curly)
     ("M-]" symex-wrap-curly)))

  (evil-define-key 'normal clojure-mode-map
    (kbd "<escape>") (lambda ()
                       (interactive)
                       (rigpa-enter-mode "symex")))
  (evil-define-key 'normal emacs-lisp-mode-map
    (kbd "<escape>") (lambda ()
                       (interactive)
                       (rigpa-enter-mode "symex"))))

(use-package symex-ide
  :after (symex)
  :straight
  (symex-ide
   :host github
   :repo "drym-org/symex.el"
   :files ("symex-ide/symex*.el"))
  :config
  (symex-ide-mode 1))

(use-package symex-evil
  :after (symex evil)
  :straight
  (symex-evil
   :host github
   :repo "drym-org/symex.el"
   :files ("symex-evil/symex*.el"))
  :config
  (symex-evil-mode 1))

(use-package rigpa
  :straight
  (rigpa
   :type git
   :host github
   :repo "countvajhula/rigpa")
  :config
  (rigpa-mode 1))

(use-package symex-rigpa
  :after (symex rigpa symex-evil)
  :straight
  (symex-rigpa
   :host github
   :repo "drym-org/symex.el"
   :files ("symex-rigpa/symex*.el"))
  :config
  (symex-rigpa-mode 1))

(use-package avy
  :straight t)

(use-package markdown-mode
  :init (setq markdown-command "multimarkdown")
  :mode ("README\\.md'" . gfm-mode))

(use-package yasnippet
  :init (yas-global-mode 1)
  :custom
  (yas-snippet-dirs (list "~/.emacs.d/mylayers/snippets")))

(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000)

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :hook '((clojure-mode . lsp)
	  (rust-ts-mode . lsp)
	  (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-completion-provider)

  :config
  (setenv "PATH" (concat "/usr/local/bin" path-separator (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))

  :custom
  (lsp-completion-provider :none)
  (lsp-clojure-server-store-path "/usr/bin/clojure-lsp")
  (lsp-enable-file-watchers t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-log-io nil)
  (lsp-response-timeout 1)

  ;; show function docs on cursor
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-position 'top)
  (lsp-eldoc-render-all nil)
  (lsp-signature-render-documentation t))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

; (use-package treemacs)

(use-package lsp-treemacs
  :after '(lsp-mode treemacs)
  :commands lsp-treemacs-errors-list
  :init (lsp-treemacs-sync-mode 1))

;(use-package treemacs-all-the-icons
;  :after '(all-the-icons treemacs)
;  :if treemacs-use-all-the-icons-theme
;  :hook ((treemacs-mode . (lambda () (treemacs-load-theme 'all-the-icons)))))

(use-package consult-lsp)

(use-package consult-flycheck)

(use-package treemacs-evil
  :hook (treemacs-mode . evil-treemacs-state))

(use-package treemacs-magit
  :defer t
  :after '(treemacs magit))

(use-package org-roam
  :config
  (org-roam-db-autosync-mode)
  :custom
  (org-roam-directory "~/Dropbox/Pessoal/Notes")
  (org-M-RET-may-split-line nil)

  :general
  ("M-S-<return>" #'org-insert-item))

(use-package org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t))


(use-package ox-hugo
  :after ox)

(use-package uuidgen)

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package rustic
  :ensure t)

(use-package cider
  :defer t
  :hook '((clojure-mode . cider-mode)
	  (cider-repl-mode . eldoc-mode)
	  (cider-mode . eldoc-mode))

  :init
  (setq cider-stacktrace-default-filters '(tooling dup)
	cider-repl-pop-to-buffer-on-connect nil
	cider-repl-use-clojure-font-lock t)

  :custom
  (cider-test-fail-fast nil)
  (cider-font-lock-dynamically '(macro core function var))
  (cider-download-java-sources t)
  (cider-prompt-save-file-on-load nil)

  :config
  (cider-enable-flex-completion)
  (add-hook 'cider-mode-hook
	    (lambda ()
	      (setq completion-at-point-functions
		    ;; (list #'cider-complete-at-point
		    ;; 	  #'lsp-completion-at-point
		    ;; 	  #'dabbrev-capf)

		    (list #'cider-complete-at-point
			  #'lsp-completion-at-point
			  #'cape-dabbrev))))

  :general
  (nmap :keymaps 'cider-stacktrace-mode-map
    "C-j" 'cider-stacktrace-next-cause
    "C-k" 'cider-stacktrace-previous-cause
    "TAB" 'cider-stacktrace-cycle-current-cause
    "0"   'cider-stacktrace-cycle-all-causes
    "1"   'cider-stacktrace-cycle-cause-1
    "2"   'cider-stacktrace-cycle-cause-2
    "3"   'cider-stacktrace-cycle-cause-3
    "4"   'cider-stacktrace-cycle-cause-4
    "5"   'cider-stacktrace-cycle-cause-5
    "a"   'cider-stacktrace-toggle-all
    "c"   'cider-stacktrace-toggle-clj
    "d"   'cider-stacktrace-toggle-duplicates
    "J"   'cider-stacktrace-toggle-java
    "r"   'cider-stacktrace-toggle-repl
    "T"   'cider-stacktrace-toggle-tooling)

  (nmap :keymaps 'cider-inspector-mode-map
    "j"   'cider-inspector-next-inspectable-object
    "k"   'cider-inspector-previous-inspectable-object
    "l"   'cider-inspector-next-sibling
    "h"   'cider-inspector-previous-sibling
    "d"   'cider-inspector-def-current-val
    "P"   'cider-inspector-toggle-pretty-print
    "S"   'cider-inspector-toggle-sort-maps
    "<tab>"  'cider-inspector-operate-on-point)
  )

(use-package clojure-mode
  :hook ((clojure-mode . evil-cleverparens-mode)
	 (clojurescript-mode . evil-cleverparens-mode)
	 (clojurec-mode . evil-cleverparens-mode)))

(use-package clj-refactor
  :hook ((clojure-mode . (lambda () (clj-refactor-mode 1))))

  :config
  (setq cljr-add-ns-to-blank-clj-files nil))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "OPENROUTER_API_KEY")
  (exec-path-from-shell-copy-env "GEMINI_API_KEY")
  (exec-path-from-shell-copy-env "ANTHROPIC_API_KEY"))

(use-package kaocha-runner
  :defer t)

(use-package docker
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-ts-mode))

(use-package docker-compose-mode)

(use-package tree-mode)

(use-package json-navigator
  :hook ((hierarchy-tabulated-mode . tree-mode)))

(use-package emojify
  :hook (after-init . global-emojify-mode))

;; load additional layers
(load (expand-file-name "mylayers/flycheck-splint.el" user-emacs-directory))
(load (expand-file-name "mylayers/elisp.el" user-emacs-directory))
(load (expand-file-name "mylayers/clojure.el" user-emacs-directory))

(defun my-clojure-mode-setup ()
  "Custom setup for clojure-mode."
  (general-create-definer clojure-definer
    :keymaps 'override
    :states '(emacs normal hybrid motion visual operator)
    :prefix ",")

  (clojure-definer
    "a" #'lsp-execute-code-action
    "rr" #'lsp-rename
    "rs" #'clojure-sort-ns

    ;; "=" #'rust-format-buffer
    "eb" #'cider-load-buffer
    "evl" #'cider-inspect-last-result

    "if" #'cider-run-flowstorm

    "scj" #'cider-jack-in-clj
    "scJ" #'cider-jack-in-clj&cljs
    "scc" #'cider-connect-clj
    "sq" #'sesman-quit
    "sa" #'cider-switch-to-repl-buffer
    "sb" #'cider-load-buffer
    "sn" #'cider-send-ns-form-to-repl
    "sN" #'cider-repl-set-ns

    "mg" #'cider-send-go
    "mh" #'cider-send-halt
    "mr" #'cider-send-reset
    "mR" #'cider-send-reload
    "mt" #'cider-send-reload-tests

    "tt" #'cider-test-run-focused-test
    "tn" #'cider-test-run-ns-tests
    "tp" #'cider-test-run-project-tests

    "hh" #'cider-doc
    "hc" #'cider-clojuredocs
    "gi" #'lsp-find-implementation
    "gI" #'lsp-find-implementation-other-window
    "gr" #'lsp-find-references
    "Gr" #'lsp-ui-peek-find-references
    "gs" #'consult-lsp-symbols
    "gt" #'lsp-bridge-find-type-def
    "gT" #'lsp-bridge-find-type-def-other-window
    "gd" #'lsp-find-definition
    "gG" #'xref-find-definitions-other-window

    "==" #'lsp-format-buffer
    "=r" #'lsp-format-region))

(defun my-rust-mode-setup ()
  "Custom setup for rust-mode."
  ; (lsp-bridge-mode 1) ; Activate lsp-bridge-mode
  ; (corfu-mode -1) ; Deactivate corfu-mode
  ; (acm-mode 1) ; Activate acm-mode
  (general-create-definer rust-definer
    :keymaps 'override
    :states '(emacs normal hybrid motion visual operator)
    :prefix ",")

  (rust-definer
   "a" #'lsp-execute-code-action
   "r" #'lsp-rename
   "e" #'lsp-bridge-diagnostic-list
   "=" #'rust-format-buffer

   "bc" #'rust-run-clippy
   "bb" #'rust-compile
   "bt" #'rust-test

   "hh" #'lsp-bridge-show-documentation
   "gi" #'lsp-find-implementation
   "gI" #'lsp-bridge-find-impl-other-window
   "gr" #'lsp-bridge-find-references
   "Gr" #'lsp-bridge-peek
   "gt" #'lsp-bridge-find-type-def
   "gT" #'lsp-bridge-find-type-def-other-window
   "gd" #'lsp-find-definition
   "gG" #'lsp-bridge-find-def-other-window))

(add-hook 'rust-mode-hook 'my-rust-mode-setup)
(add-hook 'clojure-mode-hook 'my-clojure-mode-setup)
(add-hook 'clojure-mode-hook (lambda () (set-fill-column 100)))
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
  "d" '(kill-current-buffer :which-key "Kill this buffer")
  "P" '(copy-clipboard-to-whole-buffer :which-key "Paste clipboard to whole buffer")
  "Y" '(buffer-yank-all :which-key "Yank whole buffer"))

(+general-global-menu! "Window" "w"
  "h"  #'evil-window-left
  "l"  #'evil-window-right
  "k"  #'evil-window-up
  "j"  #'evil-window-down
  "s"  #'evil-window-vsplit
  "2"  #'evil-window-vsplit
  "1"  #'delete-other-windows
  "d"  #'delete-window
  "r"  #'evil-window-rotate-downwards
  "="  #'balance-windows)

(+general-global-menu! "File" "f"
  "ei" '((lambda () (interactive) (find-file "~/.emacs.d/init.el"))
	 :which-key "Open init.el")
  "f" '(find-file :which-key)
  "r" '(rename-file :which-key "Rename file")
  "s" '(save-buffer :which-key "Save buffer")
  "S" '(evil-write-all :which-key "Save opened buffers"))

(+general-global-menu! "Project" "p"
  "f" #'consult-projectile-find-file
  "F" #'consult-projectile-find-file-other-window
  "b" #'consult-project-buffer)

(+general-global-menu! "Org Roam" "r"
  "f" #'org-roam-node-find
  "i" #'org-roam-node-insert
  "b" #'org-roam-buffer-toggle
  "u" #'org-roam-ui-open
  "ta" #'org-roam-tag-add
  "tr" #'org-roam-tag-remove)

(+general-global-menu! "Treemacs" "t"
  "t" #'treemacs)

(+general-global-menu! "Text" "x"
  "tj" #'json->edn)

(+general-global-menu! "Checkers" "!"
  "c" '(consult-flycheck :which-key "Consult flycheck"))

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

;; Add a hook to enable emacs-lisp-mode for .dir-locals.el files
(defun enable-emacs-lisp-mode-for-dir-locals ()
  "Enable `emacs-lisp-mode` for .dir-locals.el files."
  (when (and buffer-file-name
             (or (string-equal (file-name-nondirectory buffer-file-name) ".dir-locals.el")
		 (string-equal (file-name-nondirectory buffer-file-name) ".dir-locals-2.el")))
    (emacs-lisp-mode)))

(add-hook 'find-file-hook 'enable-emacs-lisp-mode-for-dir-locals)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (clojure . t)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)

;;; init.el ends here
