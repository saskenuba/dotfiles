;;; early-init.el  -*- lexical-binding: t; -*-
; (setenv "LSP_USE_PLISTS" "true")

;; We manage packages exclusively with straight.el (see init.el).
;; Keep package.el from auto-initializing at startup so we don't end up
;; with two package managers loading the same packages into the session.
(setq package-enable-at-startup nil)

;; No GC during startup; back to the runtime threshold once startup is
;; done (hooked here so an error in init.el can't leave GC off).
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 100000000)))
