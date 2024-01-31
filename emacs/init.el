; Font settings
(defvar martmacs/default-font-size 100)
(set-face-attribute 'default nil :font "CommitMono Nerd Font" :height martmacs/default-font-size)

(setq inhibit-startup-message t)        ; Disable visible scrollbar
(scroll-bar-mode -1)          ; Disable the toolbar

; Saves last location
(desktop-save-mode 1)

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

(setq visible-bell t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Use straight.el instead of package
(setq package-enable-at-startup nil)
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

(straight-use-package 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;;; packages 

;;; packages 

(use-package general
  :config
  (general-create-definer global-definer
    :keymaps 'override
    :states '(insert emacs normal hybrid motion visual operator)
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

(use-package iedit)

;;; Enable vertico and extensions
(use-package vertico
  :demand t
  :straight (vertico :files (:defaults "extensions/*")
		     :includes (vertico-indexed
				vertico-flat
				vertico-grid
				vertico-mouse
				vertico-quick
				vertico-buffer
				vertico-repeat
				vertico-reverse
				vertico-directory
				vertico-multiform
				vertico-unobtrusive))
  :init
  (vertico-mode)
  ; (vertico-multiform-mode)
  ;; Different scroll margin
  (setq vertico-scroll-margin 0)
  (setq vertico-count 13))

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :straight t
  :after (marginalia all-the-icons)
  :hook  (marginalia-mode . all-the-icons-completion-marginalia-setup)
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
  (evil-set-undo-system #'undo-redo)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package command-log-mode)

(use-package ef-themes
  :init (load-theme 'ef-arbutus :no-confirm))

(use-package orderless
  :custom
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp)))

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
    "bb" #'consult-buffer
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
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (clojure-mode    . rainbow-delimiters-mode))

(use-package magit
  :general
  (global-definer
   "gs" #'magit-status-quick))

(use-package magit-delta
  :straight (magit-delta :type git :host github :repo "dandavison/magit-delta")
  :hook (magit-mode . magit-delta-mode))

(use-package forge
  :after magit)

(use-package smartparens
  :ensure t
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
  ((emacs-lisp-mode . (lambda ()
					; (setq symex-quote-prefix-list (list "#" "'" "#_"))
			(evil-define-key 'normal symex-mode-map (kbd "<escape>") 'symex-mode-interface)
			(evil-define-key 'insert symex-mode-map (kbd "<escape>") 'symex-mode-interface)))))

;; Spacemacs-like buffer menus
(global-definer
  "SPC" '(execute-extended-command :which-key "Execute command")
  "qr"  #'restart-emacs
  "qq"  #'kill-emacs
  "TAB" '((lambda () (interactive) (switch-to-buffer nil))
	  :which-key "Swap to last used buffer")
  
  "ik" '(lambda () (interactive) (evil-insert-newline-above))
  "ij" '(lambda () (interactive) (evil-insert-newline-below))
  
  "bd" '(kill-current-buffer :which-key "Kill this buffer")
  "hs" '((lambda () (interactive) (describe-symbol (symbol-at-point)))
	 :which-key "Describe symbol at point"))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("73c55f5fd22b6fd44f1979b6374ca7cc0a1614ee8ca5d4f1366a0f67da255627" "01aef17f41edea53c665cb57320bd80393761f836be5ab0bd53292afc94bd14d" "a6a979c8b7ccb1d4536f4fa74a6e47674a3ce65feea3fecdf1d9dc448fac47e0" default))
 '(package-selected-packages
   '(marginalia rainbow-delimiters smartparens-mode symex smartparens evil-collection evil command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
