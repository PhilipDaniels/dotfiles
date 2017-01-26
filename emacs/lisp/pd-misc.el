;;; Miscellaneous settings.
;;; Usage:  (require 'pd-misc)
;;;
;;; Miscellaneous settings, mainly built-in variables that do not
;;; fit into any of the other pd-* packages.

(add-hook 'before-save-hook 'time-stamp)
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq delete-by-moving-to-trash t)
(setq disabled-command-function nil)
(setq echo-keystrokes 0.1)
(setq sentence-end-double-space nil)
(setq use-dialog-box nil)
(setq user-full-name "Philip Daniels")
(setq user-mail-address "philip.daniels1971@gmail.com")
(setq auto-save-default nil)
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq message-log-max 50000)
(setq require-final-newline t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default truncate-lines 1)
(delete-selection-mode 1)


;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; https://github.com/lewang/flx
;; By default Emacs will initiate GC every 0.76 MB allocated (gc-cons-threshold
;; == 800000). Increase it to 20Mb.
(setq gc-cons-threshold 20000000)
(setq garbage-collection-messages t)


(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-file-name-partially
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol)
      )

(pd-log-complete)
(provide 'pd-misc)
