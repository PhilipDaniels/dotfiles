;;; -*- mode: emacs-lisp -*-
;;; -*- coding: utf-8 -*-
;;; Local Variables:
;;; eval: (outline-minor-mode 1)
;;; End:

;;; $$ TODO
;;; Something like Ctrl-P (wildfinder) - helm!
;;; super (apps) keybindings.


;;; Determine operating system and window system we are running on.
(message "The system-type variable is %s" system-type)
(message "The window-system variable is %s" window-system)


;;; $$ FUNCTIONS.
(message "FUNCTIONS - BEGIN.")

(defun pd-left-rotate (list)
  "Move the first element to the end of the list."
  (append (cdr list) (list (car list))))

(defun pd-right-rotate (list)
  "Move the last element to the front of the list."
  (append (last list) (butlast list)))

(defun pd-utf8 ()
  "Inserts a utf-8 file coding marker."
  (interactive)
  (insert "-*- coding: utf-8 -*-"))

(defun pd-timestamp()
  "Inserts an Emacs timestamp at point."
  (interactive)
  (insert "Time-stamp: <>"))

(defun pd-copy-current-line ()
  "Copy current line (including the newline character at the end)"
  (interactive)
  (kill-new (buffer-substring (point-at-bol) (+ (point-at-eol) 1))))

(defun pd-back-to-indentation-or-beginning ()
  "Toggle between indentation and true beginning of line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun pd-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(progn
	  (rename-file name new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil))))))


(message "FUNCTIONS - END.")


;;; $$ MAJOR MODES.
;; C/C++ mode.
(message "MAJOR MODES - BEGIN.")

(setq c-default-style "linux"
      c-basic-offset 4)

;; Markdown mode.
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Teach dired to unzip zip files (use the Z key)
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip")))


(message "MAJOR MODES - END.")

;;; $$ MINOR MODES.
; Setup C-tab to toggle outlining in outline-minor-mode.
(add-hook 'outline-mode-hook (lambda () (require 'outline-cycle)))
(add-hook 'outline-minor-mode-hook
	  (lambda ()
	    (require 'outline-magic)
	    (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)))

;; See http://capitaomorte.github.io/yasnippet/
;; Load yasnippet, but only load my snippets (there are many examples under
;; the elpa/yasnippet folder.
(require 'yasnippet)
(setq yas-snippet-dirs '("~/repos/dotfiles/emacs/snippets"))
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)


;;; $$ APPEARANCE.
(message "APPEARANCE - BEGIN.")

(defun pd-font-exists (font &optional frame)
  "Return a font if it exists, nil otherwise. Does not work in daemon mode."
  (find-font (font-spec :name font) frame))

(defvar pd-font-candidates '("Consolas-12" "Cousine-12" "Consolas-14"
			     "Source Code Pro-12"
			     "Liberation Mono-12" "Anonymous Pro-14"
			     "Aurulent Sans Mono-12" "Calibri-12")
  "Defines a list of fonts to be tried in order.")

(defvar pd-font-index nil
  "Specifies the index of the candidate font that is currently selected.")

(defun pd-set-candidate-font (step frame &optional show-msg)
  "Scans forwards (if STEP is 1) or backwards (if STEP is -1) through the list
pd-font-candidates looking for a valid font. If STEP is 0, the current font is
reset to the first font. The first time this function is called it starts the
search at index 0."
  (interactive)
  (let ((num-fonts (length pd-font-candidates))
	(num-seen 0)
	(font-is-valid))
    ;; Loop around the array; num-seen is used to avoid looping forever if
    ;; all elements in pd-font-candidates are invalid.
    (while (and (< num-seen num-fonts) (not font-is-valid))
      (cond ((null pd-font-index) (setq pd-font-index 0))
	    ((= step 0) (setq pd-font-index 0 step 1))
	    ((< (+ pd-font-index step) 0) (setq pd-font-index (- num-fonts 1)))
	    ((>= (+ pd-font-index step) num-fonts) (setq pd-font-index 0))
	    (t (setq pd-font-index (+ pd-font-index step))))

      (setq next-font (nth pd-font-index pd-font-candidates)
	    num-seen (1+ num-seen)
	    font-is-valid (pd-font-exists next-font frame))

      (if font-is-valid
	  (progn
	    (if show-msg
		(message "Font set to %s" next-font))
	    (set-frame-font next-font t (list frame)))
	(message "Font %s does not exist, skipping" next-font)))
    (if (not font-is-valid)
	(message "No valid fonts found in candidates: %s" pd-font-candidates))
  ))


; This ensures the right font is set when running in daemon mode.
(add-hook 'after-make-frame-functions
	  (lambda (frame) (pd-set-candidate-font 0 frame)))

; And this ensures the right font is set when running in non-daemon mode.
(if (display-graphic-p)
    (pd-set-candidate-font 0 (selected-frame)))

; Setting the frame-background-mode before loading the theme stops Solarized
; from initially loading in light mode. The mapc is needed for w32 emacs, or
; else we still come up in light mode, no idea why.
(setq-default frame-background-mode 'dark)
(mapc 'frame-set-background-mode (frame-list))
(load-theme 'solarized t)        ; Package is "color-theme-solarized" on MELPA.
;(load-theme 'solarized-dark t)  ; bbatsov Solarized, no good in terminal

; You can load a different theme for GUI vs Terminal like this.
; Decent terminal themes: manoj-dark, tango-dark, misterioso, tsdh-dark, wheatgrass
;(if (display-graphic-p)
;    (load-theme 'solarized-dark t)
;  (load-theme 'tango-dark t))

; Steal the solarized colors from its palette. Only works for dark mode.
(setq sd-black (nth 1 (assoc 'base02 solarized-colors))
      sd-red (nth 1 (assoc 'red solarized-colors))
      sd-green (nth 1 (assoc 'green solarized-colors))
      sd-yellow (nth 1 (assoc 'yellow solarized-colors))
      sd-blue (nth 1 (assoc 'blue solarized-colors))
      sd-magenta (nth 1 (assoc 'magenta solarized-colors))
      sd-cyan (nth 1 (assoc 'cyan solarized-colors))
      sd-white (nth 1 (assoc 'base2 solarized-colors))
      sd-brblack (nth 1 (assoc 'base03 solarized-colors))
      sd-brred (nth 1 (assoc 'orange solarized-colors))
      sd-brgreen (nth 1 (assoc 'base01 solarized-colors))
      sd-bryellow (nth 1 (assoc 'base00 solarized-colors))
      sd-brblue (nth 1 (assoc 'base0 solarized-colors))
      sd-brmagenta (nth 1 (assoc 'violet solarized-colors))
      sd-brcyan (nth 1 (assoc 'base1 solarized-colors))
      sd-brwhite (nth 1 (assoc 'base3 solarized-colors))
      )

; These colors are from solarized.
(set-face-foreground 'mode-line sd-blue)
(set-face-background 'mode-line sd-white)
(set-face-foreground 'mode-line-inactive sd-white)
(set-face-background 'mode-line-inactive sd-blue)

(message "APPEARANCE - SOLARIZED STUFF DONE.")

;;;(add-to-list 'default-frame-alist '(height . 50))
;;;(add-to-list 'default-frame-alist '(width . 86))
;;;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;;(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq ring-bell-function nil)
(setq visible-bell 1)
(blink-cursor-mode -1)
(show-paren-mode 1)
(global-linum-mode 1)
(setq linum-format "%4d ")
(setq column-number-mode 1)
(setq line-number-mode 1)
(display-time-mode 1)
(size-indication-mode 1)
(set-cursor-color sd-red)

;; fci-mode can cause an increase in the vertical separation of lines,
;; so leave it off by default. It is bound to C-= below, for ease of use.
(require 'fill-column-indicator)
(setq fci-rule-width 2)
(setq fci-rule-color sd-blue)
;(add-hook 'c-mode-common-hook 'fci-mode)
;(add-hook 'emacs-lisp-mode-hook 'fci-mode)
;(add-hook 'shell-script-mode-hook 'fci-mode)

;; Show a red rectangle for trailing whitespace, and color long lines.
(require 'whitespace)
(setq-default show-trailing-whitespace t)
(setq whitespace-line-column 80)
(setq whitespace-style '(face trailing lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(message "APPEARANCE - END.")


;;; $$ GENERAL VARIABLES.
(message "GENERAL VARIABLES - BEGIN.")

; This package provides the command describe-unbound-keys. Try a parameter of 8.
(require 'unbound)

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


(message "GENERAL VARIABLES - END.")



;;; $$ KEYBINDINGS.
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


; Some standard keybindings
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



(message "KEYBINDINGS - BEGIN.")

;; Make Win/Apps keys send super/hyper etc.
;; (when (equal window-system 'w32)
;;   (setq w32-pass-lwindow-to-system nil
;; 	w32-lwindow-modifier 'super))

;; (when (equal window-system 'w32)
;;   (setq w32-pass-rwindow-to-system nil
;; 	w32-rwindow-modifier 'super))

;; This won't work in mintty Emacs until mintty allows APPS to pass through.
(when (or (equal window-system 'w32) (equal system-type 'cygwin))
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
;; f3, f4 = macros.
;; f10 = menu-bar-open

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

;;; ******************* Small pad keys ********************
(define-key global-map (kbd "C-S-<prior>") (lambda () (interactive) (pd-set-candidate-font -1 (selected-frame) t)))
(define-key global-map (kbd "C-S-<next>") (lambda () (interactive) (pd-set-candidate-font 1 (selected-frame) t)))

;;; ******************* Main number keys ********************
;; C-0..9 and M-0..9 are normally bound to digit-argument, which can be used via C-u anyway.
(define-key global-map (kbd "M-1") 'jump-to-register)
(define-key global-map (kbd "M-2") 'window-configuration-to-register)
(define-key global-map (kbd "M-3") 'point-to-register)

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



(message "KEYBINDINGS - END.")

