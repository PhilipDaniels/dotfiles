; -*- mode: emacs-lisp -*-
; -*- coding: utf-8 -*-

(provide 'pd-keys)

;;; ******************* Global Function keys ********************
;;; Function key names must be in lower case, like f7, not F7.
;;; f1 is HELP. Leave it alone.
;;; Make F2 and F3 run the macros stored in the 'q' and 'w' registers
;;; and Shift F2/F3 run the macros until a blank line is encountered.
;;;map <F2> @q
;;;map <F3> @w
;;;map <silent> <S-F2> :call RunMacroToBlankLine('q')<CR>
;;;map <silent> <S-F3> :call RunMacroToBlankLine('w')<CR>

(define-key global-map (kbd "<f2>")   'recentf-open-files)
(define-key global-map (kbd "<S-f2>") 'menu-bar-open)
(define-key global-map (kbd "<C-f2>") 'menu-bar-open)


;;; ******************* Letter keys ********************
(define-key global-map (kbd "M-/") 'hippie-expand)
;; Make a buffer menu in the current window, not an "other" window.
(define-key global-map (kbd "C-x C-b") 'buffer-menu)
(define-key global-map (kbd "C-=") 'fci-mode)

;;; ******************* Arrow keys ********************
(windmove-default-keybindings 'meta)
(define-key global-map (kbd "S-M-<up>") 'shrink-window)
(define-key global-map (kbd "S-M-<down>") 'enlarge-window)
(define-key global-map (kbd "S-M-<left>") 'shrink-window-horizontally)
(define-key global-map (kbd "S-M-<right>") 'enlarge-window-horizontally)


;;; ******************* C/C++ mode keys ********************
(defun pd-setup-vs-keys ()
  "Establishes Visual-Studio compatible local key bindings"
  (interactive)

;     F5 = run/continue with debugging
;   C-F5 = run without debugging
;   S-F5 = stop debugging
;  CS-F5 = restart
;noremap <silent> <special>   <F5> :call Debug(1)<CR>
;noremap <silent> <special> <C-F5> :call CompileAndRun()<CR>
  (local-set-key (kbd "<f6>")   'compile)              ; interactive compile
  (local-set-key (kbd "<S-f6>") 'compile)              ; save and compile
  (local-set-key (kbd "<C-f6>") 'compile)              ; make clean
  (local-set-key (kbd "<f7>")   'ff-find-other-file)   ; View.ToggleDesigner in VS.
  (local-set-key (kbd "<S-f7>") (lambda () (interactive) (ff-find-other-file t))) ; View.ToggleDesigner in VS hsplit
  (local-set-key (kbd "<C-f7>") (lambda () (interactive) (ff-find-other-file t))) ; View.ToggleDesigner in VS vsplit.
  (local-set-key (kbd "<f8>")   'next-error)
  (local-set-key (kbd "<S-f8>") 'previous-error)
;"     F9 = toggle breakpoint (gdb break, clear)
;"  CS-F9 = delete all breakpoints
;"    F10 = step over (gdb next)
;"  C-F10 = run to cursor (gdb advance)
;" CS-F10 = set next statement
;"    F11 = step into (gdb = step)
;"  S-F11 = step out (gdb = finish)
;"  C-Brk = cancel current build
  (local-set-key (kbd "<f12>") 'ff-find-other-file))

(add-hook 'c-mode-common-hook 'pd-setup-vs-keys)

