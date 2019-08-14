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
    traad
    (sphinx-doc
     :fetcher github
     :repo "naiquevin/sphinx-doc.el")
    evil-multiedit
    demo-it
    gif-screencast
    elisp-format
    )
  "The list of Lisp packages required by the martinmariano layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun martinmariano/init-traad ()
  (use-package traad
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "rr" 'traad-rename)
    :defer t
    ))

(defun martinmariano/init-sphinx-doc ()
  "docstring"
  (use-package sphinx-doc
    :defer t
    :init
    (add-hook 'python-mode-hook 'sphinx-doc-mode)
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "id" 'sphinx-doc)
    ))

(defun martinmariano/init-evil-multiedit ()
  "docstring"
  (use-package evil-multiedit
    :init
    (define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
    ))

(defun martinmariano/init-demo-it ()
  "docstring"
  (use-package demo-it)
  )

(defun martinmariano/init-gif-screencast ()
  "docstring"
  (use-package gif-screencast)
  )


(defun martinmariano/init-elisp-format ())

;;; packages.el ends here
