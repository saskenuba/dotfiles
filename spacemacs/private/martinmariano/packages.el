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
    demo-it
    direnv
    eglot
    elisp-format
    gif-screencast
    kibit-helper
    magit-lfs
    org-roam-ui
    org-sidebar
    traad
    tree-sitter
    tree-sitter-langs)
  "Org Roam needs the graphviz package, along with the protocol
   .desktop handler to allow linking from the browser.")


(defconst my-org-roam-templates
  '(("d" "default" plain (function org-roam--capture-get-point) "%?"
     :file-name "%<%d-%m-%Y>-${slug}"
     :target (file+head "#+TITLE: ${title}
#+ROAM_ALIAS:
#+ROAM_TAGS:
#+CREATED: %u")
     :unnarrowed t))
  )

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

(defun martinmariano/init-direnv()
  (use-package direnv
    :config (direnv-mode)))

(defun martinmariano/init-org-sidebar()
  (use-package org-sidebar))

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

(defun martinmariano/init-org-roam-ui()
  :ensure t
  :after  'org-roam)

(defun martinmariano/init-magit-lfs()
  (use-package magit-lfs
    :ensure t
    :pin melpa))

(defun martinmariano/init-kibit-helper()
  (use-package kibit-helper))

(defun martinmariano/init-tree-sitter-langs ()
  (use-package tree-sitter-langs))

(defun martinmariano/init-tree-sitter ()
  (use-package tree-sitter :init
    (require 'tree-sitter-langs)
    (add-hook 'python-mode-hook #'tree-sitter-mode)))

(defun martinmariano/init-eglot ()
  (use-package eglot))

(defun martinmariano/init-traad ()
  (use-package
    traad
    :init (spacemacs/set-leader-keys-for-major-mode 'python-mode "rr" 'traad-rename)
    :defer t))

;; (defun martinmariano/init-sphinx-doc ()
;;   "docstring"
;;   (use-package sphinx-doc
;;     :defer t
;;     :init
;;     (add-hook 'python-mode-hook 'sphinx-doc-mode)
;;     (spacemacs/set-leader-keys-for-major-mode 'python-mode "id" 'sphinx-doc)))

(defun martinmariano/init-demo-it ()
  "docstring"
  (use-package demo-it))

; (defun martinmariano/init-poet-theme ())

;; Theme
(defun martinmariano/init-doom-themes ()
  :init
  (load-theme 'doom-one t)
  :config
  (custom-set-faces

 ; js2-mode
 '(js2-object-property ((t (:foreground "#fe743f"))))

; org-mode
 '(org-block-begin-line ((t (:foreground "#827591" :background "#373040"))))))

(defun martinmariano/init-gif-screencast ()
  "docstring"
  (use-package gif-screencast))


(defun martinmariano/init-elisp-format ())

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
