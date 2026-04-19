; (setenv "LSP_USE_PLISTS" "true")

;; We manage packages exclusively with straight.el (see init.el).
;; Keep package.el from auto-initializing at startup so we don't end up
;; with two package managers loading the same packages into the session.
(setq package-enable-at-startup nil)
