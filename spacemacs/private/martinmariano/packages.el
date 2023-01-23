;;; packages.el --- martinmariano layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <martin@Adam>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `martinmariano-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `martinmariano/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `martinmariano/pre-init-PACKAGE' and/or
;;   `martinmariano/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst martinmariano-packages
  '(
    ;; auto complete
    corfu
    cape

    demo-it
    direnv
    ef-themes
    eglot
    elisp-format
    gif-screencast
    kibit-helper
    magit-lfs
    org-sidebar
    symex
    traad
    )
  "Org Roam needs the graphviz package, along with the protocol
   .desktop handler to allow linking from the browser.")

(defun martinmariano/init-corfu()
  (use-package corfu
    :ensure t


    :hook
    ((lsp-completion-mode . mm/cape-capf-setup-lsp))
    ((emacs-lisp-mode . mm/cape-capf-setup-elisp))
    ((clojure-mode . mm/cape-capf-setup-clojure))
    ((org-mode . mm/cape-capf-setup-org))

    :custom
    (corfu-auto t)
    (corfu-auto-delay 0.25)
    (corfu-auto-prefix 2)
    (corfu-cycle t)
    (corfu-preselect-first t)
    (corfu-quit-no-match t)
    (corfu-separator ?\s)

    :bind
    (:map corfu-map
          ("TAB" . corfu-next)
          ([tab] . corfu-next)
          ("S-TAB" . corfu-previous)
          ("SPC" . corfu-insert-separator))

    :init
    (setq completion-styles '(orderless basic))
    (global-corfu-mode)

    ))

(defun martinmariano/init-cape()
  (use-package cape

    :ensure t

    :custom (cape-dabbrev-min-length 3)

    :init

    (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
    (add-to-list 'completion-at-point-functions #'cape-file t)
    ;; (add-to-list 'completion-at-point-functions #'cape-abbrev t)
    ))

(defun martinmariano/init-direnv()
  (use-package direnv
    :config (direnv-mode)))

(defun martinmariano/init-org-sidebar()
  (use-package org-sidebar))

(defun martinmariano/init-ef-themes()
  (use-package ef-themes))

(defun martinmariano/post-init-org-roam()
  (setq org-roam-capture-templates my-org-roam-templates)
  (setq org-roam-capture-ref-templates my-org-roam-ref-templates)

  (require 'org-roam-protocol)
  (add-to-list 'org-modules 'org-roam-protocol)

  (add-hook 'org-mode-hook (lambda ()
                             (smartparens-mode)
                             (imenu-list-minor-mode)
                             ;; (org-sidebar '(my/org-sidebar))
                             ))

  (spacemacs/declare-prefix-for-mode 'org-mode "mr" "org-roam")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "rb" 'org-roam)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "rc" 'org-roam-db-build-cache)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "rf" 'org-roam-find-file)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ri" 'org-roam-insert)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "rg" 'org-roam-graph))

(defun martinmariano/init-company-org-roam()
  :ensure t
  :init (spacemacs|add-company-backends :backends company-org-roam :modes org-mode))

(defun martinmariano/init-magit-lfs()
  (use-package magit-lfs
    :ensure t
    :pin melpa))

(defun martinmariano/init-kibit-helper()
  (use-package kibit-helper))

(defun martinmariano/init-eglot ()
  (use-package eglot))

(defun martinmariano/init-traad ()
  (use-package
    traad
    :init (spacemacs/set-leader-keys-for-major-mode 'python-mode "rr" 'traad-rename)
    :defer t))

(defun martinmariano/init-demo-it ()
  "docstring"
  (use-package demo-it))

(defun martinmariano/init-gif-screencast ()
  "docstring"
  (use-package gif-screencast))

(defun martinmariano/init-elisp-format ()
  (use-package elisp-format))


(defun martinmariano/init-symex ()
  (use-package
    symex

    :init
    (setq symex--user-evil-keyspec
          '(("j" . symex-go-up)
            ("k" . symex-go-down)
            ("C-j" . symex-climb-branch)
            ("C-k" . symex-descend-branch)
            ("M-j" . symex-goto-highest)
            ("M-k" . symex-goto-lowest)
            ("M-1" . symex-cycle-quote)
            ("C-M-l"  . clojure-align)

            ("M-[" . symex-create-curly)
            ("M-]" . symex-wrap-curly)))

    :config
    (symex-initialize)

    :hook
    ((clojure-mode . (lambda ()
                       (setq symex-quote-prefix-list (list "#" "'" "#_"))

                       (evil-define-key 'normal symex-mode-map
                         (kbd "<escape>") 'symex-mode-interface)

                       (evil-define-key 'insert symex-mode-map
                         (kbd "<escape>") 'symex-mode-interface)))

     (clojurescript-mode . (lambda ()
                             (setq symex-quote-prefix-list (list "#" "'" "#_") )
                             (evil-define-key 'normal symex-mode-map
                               (kbd "<escape>") 'symex-mode-interface)

                             (evil-define-key 'insert symex-mode-map
                               (kbd "<escape>") 'symex-mode-interface))))))

(defmacro modus-themes-format-sexp (sexp &rest objects)
  `(eval (read (format ,(format "%S" sexp) ,@objects))))

(dolist (theme '("operandi" "vivendi"))
  (modus-themes-format-sexp
   (defun modus-%1$s-theme-load ()

     ;; Disable first enabled theme to avoid theme stacking
     (disable-theme (car custom-enabled-themes))

     (setq modus-%1$s-theme-slanted-constructs nil
           modus-%1$s-theme-bold-constructs nil
           modus-%1$s-theme-fringes 'subtle ; {nil,'subtle,'intense}
           modus-%1$s-theme-mode-line '3d ; {nil,'3d,'moody}
           modus-%1$s-theme-faint-syntax nil
           modus-%1$s-theme-intense-hl-line nil
           modus-%1$s-theme-intense-paren-match nil
           modus-%1$s-theme-no-link-underline nil
           modus-%1$s-theme-no-mixed-fonts nil
           modus-%1$s-theme-prompts nil ; {nil,'subtle,'intense}
           modus-%1$s-theme-completions 'opinionated ; {nil,'moderate,'opinionated}
           modus-%1$s-theme-diffs nil ; {nil,'desaturated,'fg-only}
           modus-%1$s-theme-org-blocks 'greyscale ; {nil,'greyscale,'rainbow}
           modus-%1$s-theme-headings  ; Read further below in the manual for this one
           '((1 . line)
             (t . rainbow-line-no-bold))
           modus-%1$s-theme-variable-pitch-headings t
           modus-%1$s-theme-scale-headings t
           modus-%1$s-theme-scale-1 1.1
           modus-%1$s-theme-scale-2 1.15
           modus-%1$s-theme-scale-3 1.21
           modus-%1$s-theme-scale-4 1.27
           modus-%1$s-theme-scale-5 1.33)
     (load-theme 'modus-%1$s t))
   theme))

(defconst my-org-roam-templates
  '(("d" "default" plain (function org-roam--capture-get-point) "%?"
     :file-name "%<%d-%m-%Y>-${slug}"
     :target (file+head "#+TITLE: ${title}
#+ROAM_ALIAS:
#+ROAM_TAGS:
#+CREATED: %u")
     :unnarrowed t)))

(defconst my-org-roam-ref-templates
  '(("r" "ref" plain (function org-roam--capture-get-point)
     "%?"
     :file-name "web/${slug}"
     :head
     "#+TITLE: ${title}
#+ROAM_KEY: ${ref}
#+ROAM_ALIAS:
#+ROAM_TAGS:
#+CREATED: %u"
     :unnarrowed t)))
