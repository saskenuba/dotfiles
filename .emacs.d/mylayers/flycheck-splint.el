;;; flycheck-splint.el --- Add splint linter to flycheck

;; Copyright (C) 2019 Martin Mariano <martin@hotmail.com.br>
;; This code borrows heavily from flycheck-clj-kondo:
;; https://github.com/candid82/flycheck-joker
;;
;; Author: Martin Mariano <martin@hotmail.com.br>
;; Created: 23 April 2025
;; Version: 0.0.1-snapshot
;; Homepage: https://github.com/borkdude/flycheck-clj-kondo
;; Package-Requires: ((emacs "26.1") (flycheck "34"))

;;; Commentary:

;; This package integrates splint with Emacs via flycheck.  To use it,
;; add to your init.el:

;; (require 'flycheck-splint)

;; Make sure the splint binary is on your path.  For installation
;; instructions, see https://github.com/borkdude/clj-kondo.

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'flycheck)

;; force UTF-8 encoding for data sent and received to the splint
;; sub-process.
(add-to-list 'process-coding-system-alist '("splint" . utf-8))

(defvar-local flycheck-splint-lang
  nil
  "Buffer local variable to override the language used to lint the buffer with splint.
Useful if your file extension doesn't match your major-mode.")

(defmacro flycheck-splint--define-checker
    (name lang modes &rest extra-args)
  "Internal macro to define checker.
Argument NAME: the name of the checker.
Argument LANG: language string.
Argument MODES: a major mode symbol or a list thereof in which this checker is activated.
Argument EXTRA-ARGS: passes extra args to the checker."
  `(flycheck-define-checker ,name
     "See https://github.com/saskenuba/flycheck-splint"
     :command ("splint" "-o" "clj-kondo" source ,@extra-args)
     :error-patterns
     ((error line-start (or "<stdin>" (file-name))
             ":" line ":" column ": " (0+ not-newline) (or "error: " "Exception: ") (message) line-end)
      (warning line-start (or "<stdin>" (file-name))
               ":" line ":" column ": " (0+ not-newline) "warning: " (message) line-end)
      (info line-start (or "<stdin>" (file-name))
            ":" line ":" column ": " (0+ not-newline) "info: " (message) line-end))
     :modes ,modes
     :predicate (lambda ()
                  (or
                   ;; We are being told to explicitly lint
                   flycheck-splint-lang
                   ;; If there is an associated file with buffer, use file name extension
                   ;; to infer which language to turn on.
                   (and buffer-file-name
                        (string= ,lang (file-name-extension buffer-file-name)))

                   ;; Else use the mode to infer which language to turn on.
                   (pcase ,lang
                     ("clj"  (member major-mode '(clojure-mode clojure-ts-mode)))
                     ("cljs" (member major-mode '(clojurescript-mode clojure-ts-clojurescript-mode)))
                     ("cljc" (member major-mode '(clojurec-mode clojure-ts-clojurec-mode))))))))

(defmacro flycheck-splint-define-checkers (&rest extra-args)
  "Define all splint checkers.
Argument EXTRA-ARGS: passes extra arguments to the checkers."
  `(progn
     (flycheck-splint--define-checker splint-clj "clj" (clojure-mode clojure-ts-mode) ,@extra-args)
     (flycheck-splint--define-checker splint-cljs "cljs" (clojurescript-mode clojure-ts-clojurescript-mode) ,@extra-args)
     (flycheck-splint--define-checker splint-cljc "cljc" (clojurec-mode clojure-ts-clojurec-mode) ,@extra-args)
     (dolist (element '(splint-clj splint-cljs splint-cljc))
       (add-to-list 'flycheck-checkers element))))

(flycheck-splint-define-checkers)

(provide 'flycheck-splint)
;;; flycheck-splint.el ends here
