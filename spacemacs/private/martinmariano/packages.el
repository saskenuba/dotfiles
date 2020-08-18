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
    doom-themes
    eglot
    elisp-format
    evil-multiedit
    gif-screencast
    (org-roam :location (recipe
                         :fetcher github
                         :repo "jethrokuan/org-roam"))
    org-roam-server
    company-org-roam
    poet-theme
    (poetry :requires transient)
    (sphinx-doc :fetcher github :repo "naiquevin/sphinx-doc.el")
    traad
    tree-sitter
    tree-sitter-langs)
  "Org Roam needs the graphviz package, along with the protocol
   .desktop handler to allow linking from the browser.")


(defconst my-org-roam-templates
  '(("d" "default" plain (function org-roam--capture-get-point) "%?"
     :file-name "%<%d-%m-%Y>-${slug}"
     :head
     "#+TITLE: ${title}
#+ROAM_ALIAS:
#+ROAM_TAGS:
#+CREATED: %u"
     :unnarrowed t)
    ))

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

(defun martinmariano/init-org-roam()
  (use-package
    org-roam
    :after org
    :hook (org-mode . org-roam-mode)
    :custom (org-roam-directory "~/Dropbox/Pessoal/Notes")))

(defun martinmariano/post-init-org-roam()
  (setq org-roam-capture-templates my-org-roam-templates)
  (setq org-roam-capture-ref-templates my-org-roam-ref-templates)

  (require 'org-roam-protocol)
  (add-to-list 'org-modules 'org-roam-protocol)
  (add-hook 'org-mode-hook 'smartparens-mode)

  (spacemacs/declare-prefix-for-mode 'org-mode "mr" "org-roam")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "rb" 'org-roam)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "rc" 'org-roam-db-build-cache)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "rf" 'org-roam-find-file)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ri" 'org-roam-insert)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "rg" 'org-roam-graph))

(defun martinmariano/post-init-org-roam-server()
  (add-hook 'org-mode-hook (lambda ()
                             (org-roam-buffer-activate)
                             (org-roam-server-mode))))

(defun martinmariano/init-org-roam-server()
  (use-package org-roam-server
    :ensure t
    :config
    (setq org-roam-server-host "127.0.0.1"
          org-roam-server-port 9345
          org-roam-server-authenticate nil
          org-roam-server-export-inline-images t
          org-roam-server-serve-files nil
          org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
          org-roam-server-network-poll t
          org-roam-server-network-arrows nil
          org-roam-server-network-label-truncate t
          org-roam-server-network-label-truncate-length 60
          org-roam-server-network-label-wrap-length 20)))


(defun martinmariano/init-company-org-roam()
  :ensure t
  :init (spacemacs|add-company-backends :backends company-org-roam :modes org-mode))

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

(defun martinmariano/init-sphinx-doc ()
  "docstring"
  (use-package sphinx-doc
    :defer t
    :init
    (add-hook 'python-mode-hook 'sphinx-doc-mode)
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "id" 'sphinx-doc)))

(defun martinmariano/init-evil-multiedit ()
  "docstring"
  (use-package evil-multiedit
    :init
    (define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)))

(defun martinmariano/init-demo-it ()
  "docstring"
  (use-package demo-it))


(defun martinmariano/init-poet-theme ())

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


(defun martinmariano/init-poetry ()
  :defer t)

;;; packages.el ends here
