; -*- mode: emacs-lisp -*-
; -*- coding: utf-8 -*-

(provide 'pd-variables)

(setq make-backup-files nil)
(setq delete-by-moving-to-trash t)
(setq inhibit-startup-message t)
(setq scroll-error-top-bottom t)
(setq message-log-max 50000)
(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq-default truncate-lines 1)
(setq require-final-newline t)
(fset 'yes-or-no-p 'y-or-n-p)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(setq-default fill-column 80)
(add-hook 'before-save-hook 'time-stamp)
(setq gdb-many-windows t)
(setq gdb-show-main t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; Teach dired to unzip zip files (use the Z key)
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
		'("\\.zip\\'" ".zip" "unzip")))

;; C/C++ mode configuration
(setq c-default-style "linux"
      c-basic-offset 4)

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
