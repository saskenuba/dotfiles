;;; patches.el --- Local workarounds for third-party packages -*- lexical-binding: t -*-
;;
;; Keep everything in this file SMALL, SELF-CONTAINED, and REVERSIBLE.
;; Each patch should document:
;;   - Which package + version it targets
;;   - The upstream bug / API mismatch
;;   - How to remove the patch when upstream is fixed
;;
;;; Code:


;;;; rigpa ⇄ magit: `magit-diff-visit-file' calling convention
;;
;; Rigpa's `rigpa--enter-lower-or-pass-through' (rigpa.el, line ~198) calls:
;;
;;     (magit-diff-visit-file (magit-current-file) t)
;;
;; against an older magit whose signature was
;; `(magit-diff-visit-file FILE &optional OTHER-WINDOW)'. Modern magit
;; (>= 20240000 or so) changed it to `(&optional OTHER-WINDOW)' — point
;; location alone decides which blob to visit (red line → old/left,
;; green/context → new/right). That's exactly the behavior we want, so
;; we just need to bridge the ABI mismatch.
;;
;; Strategy: a `:filter-args' advice that drops the leading FILE arg
;; when legacy callers pass 2+ positional args. Normal magit callers
;; (0 or 1 args) are untouched.
;;
;; Upstream tracking: https://github.com/countvajhula/rigpa — no
;; rigpa.el changes on `main' at the time of writing. Remove this
;; advice once rigpa updates its call site.

(defun martmacs/magit-diff-visit-file--drop-legacy-file-arg (args)
  "Drop legacy FILE arg from ARGS when callers use the old magit signature.
Old magit: (magit-diff-visit-file FILE &optional OTHER-WINDOW).
New magit: (magit-diff-visit-file &optional OTHER-WINDOW).
If ARGS has >1 element, assume the first is the stale FILE and strip it."
  (if (> (length args) 1) (cdr args) args))

(with-eval-after-load 'magit-diff
  (advice-add 'magit-diff-visit-file :filter-args
              #'martmacs/magit-diff-visit-file--drop-legacy-file-arg))

;; To remove:
;; (advice-remove 'magit-diff-visit-file
;;                #'martmacs/magit-diff-visit-file--drop-legacy-file-arg)


;;;; window-purpose: *Messages* (and friends) get stuck in their window
;;
;; With `purpose-mode' active, `switch-to-buffer', `pop-to-buffer' and
;; `display-buffer' are advised to route through window-purpose's action
;; function. When the window showing a utility buffer like *Messages*
;; ends up purpose-dedicated (or simply when purpose-mode picks a
;; "matching" window that isn't the one you're in), the usual
;; buffer-switching and kill commands appear to do nothing — the only
;; way to change the buffer in that window is to mouse-click its name
;; in the mode line, because `mouse-buffer-menu' bypasses the advice.
;;
;; Escape hatch provided by window-purpose: any buffer whose name
;; matches a regexp in `purpose-action-function-ignore-buffer-names'
;; is handled by the vanilla Emacs buffer machinery.
;;
;; Upstream tracking: none — this is just a user-config choice about
;; which buffers should never be governed by purposes.

(defvar martmacs/purpose-ignored-buffer-name-regexps
  '("^\\*Messages\\*$"
    "^\\*Backtrace\\*$"
    "^\\*Warnings\\*$"
    "^\\*Compile-Log\\*$")
  "Buffers that should be invisible to `window-purpose'.
Add more regexps here if other utility buffers get stuck in a
purpose-dedicated window.")

(with-eval-after-load 'window-purpose
  (dolist (pat martmacs/purpose-ignored-buffer-name-regexps)
    (add-to-list 'purpose-action-function-ignore-buffer-names pat))
  ;; Undo any already-applied purpose dedication on live windows showing
  ;; these buffers, so the fix takes effect without restarting Emacs.
  (dolist (win (window-list nil 'no-minibuffer))
    (when (and (fboundp 'purpose-set-window-purpose-dedicated-p)
               (buffer-name (window-buffer win))
               (seq-some (lambda (re)
                           (string-match-p re (buffer-name (window-buffer win))))
                         martmacs/purpose-ignored-buffer-name-regexps))
      (purpose-set-window-purpose-dedicated-p win nil))))

;; To remove:
;; (dolist (pat martmacs/purpose-ignored-buffer-name-regexps)
;;   (setq purpose-action-function-ignore-buffer-names
;;         (delete pat purpose-action-function-ignore-buffer-names)))


(provide 'patches)
;;; patches.el ends here
