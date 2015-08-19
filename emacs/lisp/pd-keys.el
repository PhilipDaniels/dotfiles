; -*- mode: emacs-lisp -*-
; -*- coding: utf-8 -*-

(provide 'pd-keys)

;;; ******************* Documentation ********************

;; Emacs understands the following modifiers:
;;    M- (meta)      Alt on my keyboard
;;    C- (control)   Ctrl keys on my keyboard
;;    S- (shift)     Shift keys on my keyboard
;;    s- (super)     Note lower case 's'. On Linux, this is usually bound to
;;                   the Windows key automatically. On Windows, the Windows key
;;                   is used by Windows (see below).
;;    H- (hyper)     Not usually bound to any key by default.
;;    A- (alt)       Not usually bound to any key by default. Note that the
;;                   Alt key sends Meta! Best to avoid A- mappings, use H- and
;;                   s- instead.

;; To find out what the name of a key is, the easiest way is to type the key,
;; then type C-h l (view-lossage), which shows you the last 100 keys pressed.
;; See unbound.el for how to find unbound keys.


;; Keynames for a PC keyboard
;; ==========================
;; Function keys:
;;    [f5] (must be in lower case, f1 is help by default)
;; Cursor arrow keys:
;;    [left], [up], [right], [down]
;; The small pad between qwerty and numeric:
;;    [home], [end], [next (is PgDn)], [prior (is PgUp)], [insert], [delete]
;; Numeric keypad arithmetic operators:
;;    [kp-add], [kp-subtract], [kp-multiply], [kp-divide]
;; Numbers on the numeric pad:
;;    [kp-0], [kp-1] ... [kp-9]
;; The other 2 keys on the numeric pad:
;;    [kp-enter], [kp-decimal]
;; Keypad keys available when NUM LOCK is pressed:
;;    [kp-home], [kp-end], [kp-next], [kp-prior]
;;    [kp-insert], [kp-delete]
;;    [kp-left], [kp-up], [kp-right], [kp-down]
;;
;; "apps" is usually known as the "menu" key, next to Ctrl-right.


;; Key Stealing
;; ============
;; Key stealing is when a key combination is grabbed by the operating system or
;; the window manager before Emacs even sees the keypress, which makes it
;; impossible to bind the key in Emacs. On Linux, it is usually the window
;; manager that is grabbing the key. Use the window manager's tools to free up
;; these keys so that they can be used in Emacs.
;;
;; On Windows, this happens with the two Windows keys: many WinKey+letter
;; combinations are reserved, and the count goes up with each release of
;; Windows. To make these key combinations available a registry hack must be
;; used to disable all the default bindings - use a .reg file with this text:
;;
;; Windows Registry Editor Version 5.00
;;
;;[HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer]
;;"NoWinKeys"=dword:00000001
;;
;; The apps key is also stolen in mintty, though it seems to work ok in GUI
;; Emacs.


;; Recommendations
;; ===============
;; Use super (s-) prefixes by default. This is because they are automatically
;; setup on Linux, so there is no need to do anything special if you also setup
;; a key in Windows to send super.
;;
;; A full size keyboard is CTRL    WIN ALT SPACE ALTGR WIN APPS CTRL
;; My work laptop is       CTRL FN WIN ALT SPACE ALTGR     APPS CTRL
;; So my prefixes          C-          M-                  s-   C-


;; Some standard keybindings
;; =========================
;; C-f, C-b, C-n, C-p : move one character or line
;; C-v, M-v           : forward, backward one screen
;; M-f, M-b           : forward, backward by 1 word
;; C-M-f, C-M-b       : forward/backward by 1 sexp
;; C-x C-x            : exchange-point-and-mark

;; C-a, C-e           : beginning/end of a line
;; M-a, M-e           : beginning/end of a sentence
;; C-M-a, C-M-e       : beginning/end of defun

;; M-<, M->           : beginning/end of buffer
;; M-r                : move-to-window-line-top-bottom
;; C-d, M-d           : kill next character/word
;; Delback, M-Delback : kill prev char/word
;; C-k, M-k           : kill to end of line/sentence

;; C-M-v, C-M-V       : scroll other window down/up
;; C-o                : open-line
;; C-s, C-r           : isearch-forward/backward, M-s/r does regexps.
;; C-t, M-t, C-x C-t  : transpose chars/words/lines
;; C-/                : undo
;; f3, f4             : start, end/run keyboard macro
;; C-x (,  C-x )      : start, end/run keyboard macro
;; C-x `              : next-error

;; M-$, M-%           : ispell-word, query-replace foo
;; M-\, M-^, M-z      : delete horz space, delete-indentation, zap-to-char
;; M-c, M-l, M-u      : capitalize/lower/upper word
;; M-m                : back-to-indentation
;; M-h                : mark-paragraph


;; Techniques
;; ===============
;; How to unset a key globally:
;;   (global-unset-key "\C-x\C-v")
;;
;; Modify a mode-local map:
;;   (define-key lisp-mode-map "\C-xl"; 'make-symbolic-link)
;;
;; Modify a mode-local map after the mode has started (necessary for
;; some modes where the keymap is created after the mode has started):
;;   (add-hook 'texinfo-mode-hook
;;     '(lambda () (define-key texinfo-mode-map "\C-cp"
;;       'backward-paragraph) (define-key texinfo-mode-map "\C-cn"
;;       'forward-paragraph)) (...any command whatsoever here ...))





;;; ******************* Begin customisations ********************

;; Make Win/Apps keys send super/hyper etc.
;; (when (equal window-system 'w32)
;;   (setq w32-pass-lwindow-to-system nil
;; 	w32-lwindow-modifier 'super))

;; (when (equal window-system 'w32)
;;   (setq w32-pass-rwindow-to-system nil
;; 	w32-rwindow-modifier 'super))

(when (equal window-system 'w32)
  (setq w32-pass-apps-to-system nil
	w32-apps-modifier 'super))

;; (when (equal window-system 'w32)
;;   (setq w32-pass-alt-to-system nil
;; 	w32-scroll-lock-modifier nil))










;;; ******************* Global Function keys ********************
;;; Make F2 and F3 run the macros stored in the 'q' and 'w' registers
;;; and Shift F2/F3 run the macros until a blank line is encountered.
;;;map <F2> @q
;;;map <F3> @w
;;;map <silent> <S-F2> :call RunMacroToBlankLine('q')<CR>
;;;map <silent> <S-F3> :call RunMacroToBlankLine('w')<CR>

(define-key global-map (kbd "<f2>")   'recentf-open-files)
(define-key global-map (kbd "<S-f2>") 'menu-bar-open)
(define-key global-map (kbd "<C-f2>") 'menu-bar-open)


;;; ******************* Letter/main section keys ********************
(define-key global-map (kbd "M-/") 'hippie-expand)
(define-key global-map (kbd "C-a") 'pd-back-to-indentation-or-beginning)
;; Make a buffer menu in the current window, not an "other" window.
(define-key global-map (kbd "C-x C-b") 'buffer-menu)
(define-key global-map (kbd "C-=") 'fci-mode)
(define-key global-map (kbd "s-r") 'recentf-open-files)
(define-key global-map (kbd "s-w") 'pd-copy-current-line)
(define-key global-map (kbd "s--") 'text-scale-decrease)
(define-key global-map (kbd "s-=") 'text-scale-increase)

(define-key global-map (kbd "C-x g") 'magit-status)
(define-key global-map (kbd "C-x C-g") 'magit-status)

;;; ******************* Arrow keys ********************
;; Unbind the arrow keys! For hardcore users only.
;; (global-unset-key [left])
;; (global-unset-key [up])
;; (global-unset-key [right])
;; (global-unset-key [down])
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



;; Bindings reliant upon windcycle.el. First, unbind keys in E17.
;; Move between windows.
;; (define-key global-map (kbd "M-<up>") 'windmove-up-cycle)
;; (define-key global-map (kbd "M-<down>") 'windmove-down-cycle)
;; (define-key global-map (kbd "M-<right>") 'windmove-right-cycle)
;; (define-key global-map (kbd "M-<left>") 'windmove-left-cycle)

;; Move buffer between windows.
;;(define-key global-map (kbd "S-s-<up>") 'buffer-up-swap)
;;(define-key global-map (kbd "S-s-<down>") 'buffer-down-swap)
;;(define-key global-map (kbd "S-s-<right>") 'buffer-right-swap)
;;(define-key global-map (kbd "S-s-<left>") 'buffer-left-swap)

;; Bindings reliant upon cycle-buffer.el
;; (define-key global-map (kbd "<f10>") 'other-window)
;; (define-key global-map (kbd "<f11>") 'cycle-buffer-backward)
;; (define-key global-map (kbd "<f12>") 'cycle-buffer)
;; (define-key global-map (kbd "s-<prior>") 'cycle-buffer-backward)
;; (define-key global-map (kbd "s-<next>") 'cycle-buffer)

