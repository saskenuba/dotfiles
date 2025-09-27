(setenv "LSP_USE_PLISTS" "true")

;; Don't disable package.el - we want to use both
;; (setq package-enable-at-startup nil)  ; Remove or comment this line

;; Instead, prevent package.el from loading packages automatically
;; but still allow it to be initialized
(setq package-enable-at-startup t)
