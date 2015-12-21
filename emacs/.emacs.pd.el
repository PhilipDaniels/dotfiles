;;; -*- mode: emacs-lisp -*-
;;; -*- coding: utf-8 -*-
;;; Local Variables:
;;; End:


;;; $$ TODO
;; W32 proxy settings
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs/683575#683575

;;; $$ USEFUL INFO
;; https://github.com/emacs-tw/awesome-emacs
;; http://pawelbx.github.io/emacs-theme-gallery/
;; https://github.com/pierre-lecocq/emacs4developers
;; https://tuhdo.github.io/c-ide.html
;; http://www.emacswiki.org/
;; http://tuhdo.github.io/helm-intro.html



;;; Determine operating system and window system we are running on.
(message "The system-type variable is %s" system-type)
(message "The window-system variable is %s" window-system)


;;; $$ REQUIRES.
(message "REQUIRES - BEGIN.")

(require 'buffer-move)
(require 'dedicated)
(require 'expand-region)
(require 'fill-column-indicator)
(require 'golden-ratio)
(require 'gud)
(require 'helm)
(require 'helm-config)
(require 'hideshow)
(require 'hlinum)
(require 'shackle)
(require 'speedbar)
(require 'unbound)                ;; This package provides the command describe-unbound-keys. Try a parameter of 8.
;;(require 'which-func)
(require 'whitespace)
(require 'yasnippet)

(message "REQUIRES - END.")


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

(defun pd-delete-file-and-buffer ()
  "Deletes the current buffer and its backing file."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun pd-untabify-buffer ()
  "Runs untabify on the whole buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun pd-sort-paragraph ()
  "Sorts the current paragraph and leaves point after the last line."
  (interactive)
  (let (bounds pos1 pos2)
    (setq bounds (bounds-of-thing-at-point 'paragraph))
    (setq pos1 (car bounds))
    (setq pos2 (cdr bounds))
    (sort-lines nil pos1 pos2)
    (goto-char pos2)))

(defun pd-compile-without-confirmation ()
  "Runs last compilation without asking for confirmation."
  (interactive)
  (save-window-excursion
    (compile compile-command))
  (pop-to-buffer (get-buffer "*compilation*")))

(defun pd-compile-clean-one-shot ()
  "Runs make clean, but restores compile command after it."
  (interactive)
  (let (oldcc)
    (setq oldcc compile-command)
    (save-window-excursion
      (compile "make clean"))
    (pop-to-buffer (get-buffer "*compilation*"))
    (setq compile-command oldcc)))

(defun endless/comment-line-or-region (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))

(defun endless/forward-paragraph (&optional n)
  "Advance just past next blank line."
  (interactive "p")
  (let ((para-commands
         '(endless/forward-paragraph endless/backward-paragraph)))
    ;; Only push mark if it's not active and we're not repeating.
    (or (use-region-p)
        (not (member this-command para-commands))
        (member last-command para-commands)
        (push-mark))
    ;; The actual movement.
    (dotimes (_ (abs n))
      (if (> n 0)
          (skip-chars-forward "\n[:blank:]")
        (skip-chars-backward "\n[:blank:]"))
      (if (search-forward-regexp
           "\n[[:blank:]]*\n[[:blank:]]*" nil t (cl-signum n))
          (goto-char (match-end 0))
        (goto-char (if (> n 0) (point-max) (point-min)))))))

(defun endless/backward-paragraph (&optional n)
  "Go back up to previous blank line."
  (interactive "p")
  (endless/forward-paragraph (- n)))

(message "FUNCTIONS - END.")


;;; $$ MAJOR MODES.
;; C/C++ mode.
(message "MAJOR MODES - BEGIN.")

;; Shells.
;; M-x shell runs a shell as a sub-process, communicating with it via pipes.
;; It is not fully functional.
;; eshell is a shell written in elisp. It is slow and not very complete.
;; term and ansi-term (better) are better choices.
;; There is also multi-term, not sure what that adds though.
;; tl;dr - use ansi-term for starting shells.

(setq c-default-style "linux"
      c-basic-offset 4)

;; Markdown mode.
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Teach dired to unzip zip files (use the Z key).
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip")))

;; Make magit take up the entire screen.
;;(defadvice magit-status (around magit-fullscreen activate)
;;  (window-configuration-to-register :magit-fullscreen)
;;   ad-do-it
;;   (delete-other-windows))

;; (defun magit-quit-session ()
;;   "Restores the previous window configuration and kills the magit buffer"
;;   (interactive)
;;   (kill-buffer)
;;   (jump-to-register :magit-fullscreen))

;; (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; Helm mode.
;; Based on http://tuhdo.github.io/helm-intro.html
(global-set-key (kbd "C-;") 'helm-command-prefix)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(defun pd-helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

(defun pd-make-helm-full-frame ()
  "Expands the helm window so that it takes up the full frame."
  (interactive)
  (with-selected-window (helm-window)
    (delete-other-windows)))

;;(add-to-list 'golden-ratio-inhibit-functions 'pd-helm-alive-p)
(setq-default helm-buffer-max-length nil)
(setq-default helm-ff-newfile-prompt-p nil)

;; Try and sort buffers by mode, LRU, with SPEEDBAR, MESSAGES etc at the bottom.
;;(pd-helm-sort-buffers (buffer-list))
;; (defun pd-sort-buffers (buffers)
;;   "Sorts buffers nicely for display in helm."
;;   (interactive)
;;   buffers
;;   )
;;
;; (defun pd-helm-sort-buffers (orig-fun &rest args)
;;   (pd-sort-buffers (apply orig-fun args)))
;;
;; (advice-add 'helm-buffers-sort-transformer :around #'pd-helm-sort-buffers)

(helm-mode 1)

;; This doesn't seem to work. sm to get rid of char 2603, Snowman, which does not
;; exist in some fonts, such as Consolas.
(shackle-mode 1)
(setq-default shackle-lighter "")
(setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align below :ratio 0.4)))

;; (setq helm-split-window-in-side-p nil)
;; (defun pd-helm-split-window (window)
;;   (if (one-window-p t)
;;       ;; With just window helm does the right thing
;;       (split-window
;;        (selected-window) nil (if (eq helm-split-window-default-side 'other)
;;                                  'below helm-split-window-default-side))
;;     ;; If there are multiple windows, select the bottom-left window
;;     (while (window-in-direction 'left)
;;       (select-window (window-in-direction 'left)))
;;     (while (window-in-direction 'below)
;;       (select-window (window-in-direction 'below)))
;;     (selected-window)))
;;
;; (setq helm-split-window-preferred-function #'pd-helm-split-window)

;; Automatically delete trailing whitespace in modes derived from prog-mode.
;; Based on http://stackoverflow.com/questions/19174302/emacs-only-delete-trailing-whitespace-while-saving-in-programming-mode
;; An alternative, using ws-trim or ws-butler, is mentioned there which
;; can reduce git commit noise.
(defun pd-delete-trailing-whitespace ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(add-hook 'prog-mode-hook #'pd-delete-trailing-whitespace)

(message "MAJOR MODES - END.")


;;; $$ MINOR MODES.
;; See http://capitaomorte.github.io/yasnippet/
;; Load yasnippet, but only load my snippets (there are many examples under
;; the elpa/yasnippet folder.
(message "MINOR MODES - END.")

(setq yas-snippet-dirs '("~/repos/dotfiles/emacs/snippets"))
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(winner-mode 1)

(message "MINOR MODES - END.")


;;; $$ APPEARANCE.
(message "APPEARANCE - BEGIN.")

(defun pd-font-exists (font &optional frame)
  "Return a font if it exists, nil otherwise. Does not work in daemon mode."
  (find-font (font-spec :name font) frame))

;; See https://github.com/chrissimpkins/codeface to get a big zip of ttf and otf
;; fonts. To determine the name that Emacs uses for a font, the easiest way I
;; know is to use the customize system to pick a default font, then save
;; options, the name of the font then appears in the .emacs file.
(defvar pd-font-candidates '("Consolas-11"
                             "Cousine-10"
                             "Courier New-10"
                             "Aurulent Sans Mono-10"
                             "Source Code Pro-12"
                             "DejaVu Sans Mono-12"
                             "Droid Sans Mono-12"
                             "Liberation Mono-12"
                             "Anonymous Pro-12"
                             "Liberation Mono-12"
                             "CPMono_v07 Plain-12"
                             "Calibri-12"
                             )
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


;; This ensures the right font is set when running in daemon mode, but it is
;; no longer required since my preferred method is now to start a normal Emacs
;; window upon login and turn that into a daemon - see the bottom of this file.
;;(add-hook 'after-make-frame-functions
;;        (lambda (frame) (pd-set-candidate-font 0 frame t)))

;; And this ensures the right font is set when running in non-daemon mode.
(if (display-graphic-p)
    (pd-set-candidate-font 0 (selected-frame) t))

;; Setting the frame-background-mode before loading the theme stops Solarized
;; from initially loading in light mode. The mapc is needed for w32 emacs, or
;; else we still come up in light mode, no idea why.
(setq-default frame-background-mode 'dark)
(mapc 'frame-set-background-mode (frame-list))
(load-theme 'solarized t)        ; Package is "color-theme-solarized" on MELPA.

; You can load a different theme for GUI vs Terminal like this.
; Decent terminal themes: manoj-dark, tango-dark, misterioso, tsdh-dark, wheatgrass
;(if (display-graphic-p)
;    (load-theme 'solarized-dark t)
;  (load-theme 'tango-dark t))

;; Steal the solarized colors from its palette. Only works for dark mode.
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

;; These colors are from solarized.
(set-face-foreground 'mode-line sd-blue)
(set-face-background 'mode-line sd-white)
(set-face-foreground 'mode-line-inactive sd-white)
(set-face-background 'mode-line-inactive sd-blue)

(message "APPEARANCE - SOLARIZED STUFF DONE.")

;;;(add-to-list 'default-frame-alist '(height . 50))
;;;(add-to-list 'default-frame-alist '(width . 86))
;;;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(tool-bar-mode -1)
;;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq ring-bell-function nil)
(setq visible-bell 1)
(show-paren-mode 1)
(setq-default show-paren-delay 0)
;;(global-linum-mode 1)           ;; This is very slow with long lines.
;;(setq linum-format "%4d ")      ;; So we don't need this either.
(setq column-number-mode 1)
(setq line-number-mode 1)
;;(display-time-mode 1)
(size-indication-mode 1)

;; Highlighting of the current line. Must do this after theme is loaded.
;; See http://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
(set-face-attribute 'helm-selection nil
                    :background "white"
                    :foreground "red")

(global-hl-line-mode 1)
(set-face-background 'hl-line "black")
(set-face-foreground 'hl-line nil)
(set-face-underline-p 'hl-line nil)
(set-cursor-color "yellow")
(blink-cursor-mode 1)

;; fci-mode can cause an increase in the vertical separation of lines,
;; so leave it off by default. It is bound to C-= below, for ease of use.
(setq fci-rule-width 2)
(setq fci-rule-color sd-blue)
;(add-hook 'c-mode-common-hook 'fci-mode)
;(add-hook 'emacs-lisp-mode-hook 'fci-mode)
;(add-hook 'shell-script-mode-hook 'fci-mode)

;; Show a red rectangle for trailing whitespace, and color long lines.
(setq-default show-trailing-whitespace t)
(setq-default whitespace-line-column 80)
(setq-default whitespace-style '(face trailing lines-tail))

;; Turn off display of trailing whitespace in some modes.
(dolist (hook '(buffer-menu-mode-hook compilation-mode-hook
                                      diff-mode-hook
                                      magit-popup-mode-hook
                                      shell-mode-hook
                                      term-mode-hook))
  (add-hook hook (lambda () (set-variable 'show-trailing-whitespace nil))))

;; We need to turn on whitespace-mode to get the display of the >80 character
;; lines working.
(add-hook 'prog-mode-hook 'whitespace-mode)

;;(hlinum-activate)         ;; Slow
(which-function-mode -1)    ;; Slow and pointless, but seems impossible to disable.

;; This face is used to highlight the selected thing (e.g. function in source
;; file). Box is on by default, which causes a temporary line-height increase
;; which is visually irritating.
(set-face-attribute 'speedbar-highlight-face nil :box nil :background "black")
(setq-default sr-speedbar-right-side t)

(message "APPEARANCE - END.")


;;; $$ GENERAL VARIABLES.
(message "GENERAL VARIABLES - BEGIN.")

(setq user-full-name "Philip Daniels")
(setq user-mail-address "philip.daniels1971@gmail.com")
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq delete-by-moving-to-trash t)
(setq inhibit-startup-message t)
(setq scroll-error-top-bottom t)
(setq message-log-max 50000)
(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq-default truncate-lines 1)
(setq require-final-newline t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(setq-default fill-column 80)
(add-hook 'before-save-hook 'time-stamp)
(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq compilation-ask-about-save nil)
(setq compilation-scroll-output 'first-error)
(setq comment-empty-lines t)
(setq magit-push-always-verify nil)

;; Don't prompt with "Active processes exist, kill them?" when exiting Emacs.
;; http://stackoverflow.com/questions/2706527/make-emacs-stop-asking-active-processes-exist-kill-them-and-exit-anyway
(add-hook 'comint-exec-hook
          (lambda () (set-process-query-on-exit-flag
                      (get-buffer-process (current-buffer)) nil)))
(add-hook 'term-exec-hook
          (lambda () (set-process-query-on-exit-flag
                      (get-buffer-process (current-buffer)) nil)))

;; Don't prompt about killing buffers with live processes attached to them.
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
        kill-buffer-query-functions))

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

(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'csharp-mode-hook 'hs-minor-mode)
(add-hook 'sh-mode-hook 'hs-minor-mode)

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
;; It can also be known as "print" on Cygin, because the sequence ESC [ 2 9 ~
;; is mapped to that in lisp/term/xterm/el.gz.


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
;; BUT DO NOT DO THIS. It makes it hard to work in a business environment with
;; projectors, etc.
;;
;; The apps key used to be stolen in mintty, but the commit
;; 429cb080e6bfee6136227ca5d41ea61494b36c2d on 9 Nov 15 made it possible to pass
;; apps through to the underlying program. See
;; http://emacs.stackexchange.com/questions/18245/making-terminal-emacs-treat-apps-aka-menu-key-as-super-modifier


;; Recommendations
;; ===============
;; * Restrict repeatable keys (those that you might want to press several times
;;   quickly in succession) to C- and M-.
;; * Do not use the Windows keys.
;; * Make the apps/menu key send the "<apps>" leader.
;; * Do not use the super s- prefix.
;;
;; A full size keyboard is CTRL    WIN ALT SPACE ALTGR WIN APPS   CTRL
;; My work laptop is       CTRL FN WIN ALT SPACE ALTGR     APPS   CTRL
;; So my prefixes/leaders  C-          M-                  <apps> C-


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



(message "KEYBINDINGS - BEGIN.")

;; Make Win/Apps keys send super/hyper etc.
;; (when (equal window-system 'w32)
;;   (setq w32-pass-lwindow-to-system nil
;;      w32-lwindow-modifier 'super))

;; (when (equal window-system 'w32)
;;   (setq w32-pass-rwindow-to-system nil
;;      w32-rwindow-modifier 'super))

;; Mintty has been hacked by the maintainer to allow the APPS/MENU key to be
;; passed through to the underlying program. Adding the line
;; "Key_Menu=29" to the .minttyrc file causes terminal Emacs to see it as the
;; "<print"> key. The hack was on 2015-11-19 in commit
;; 429cb080e6bfee6136227ca5d41ea61494b36c2d.
;; Given this hack, we can make APPS send the s- (super) prefix like this in
;; both W32 and terminal Emacs. Unfortunately, APPS still does not work like
;; the Alt or Control keys, if you hold it down by itself you get lots of input
;; in the output buffer.
;; (if (equal system-type 'cygwin)
;;     (if (equal window-system 'w32)
;;      (setq w32-pass-apps-to-system nil
;;            w32-apps-modifier 'super)
;;       (define-key local-function-key-map (kbd "<print>") 'event-apply-super-modifier)))

;; (define-key global-map (kbd "s-h") (lambda () (interactive) (message "hello from menu key via s- prefix")))


;; Alternatively, we can turn it into a leader key like this.
;; See http://ergoemacs.org/emacs/emacs_menu_app_keys.html
(if (equal system-type 'cygwin)
    (if (equal window-system 'w32)
        (setq w32-pass-apps-to-system nil
              w32-apps-modifier nil)
      ;; force all alternatives to <apps> so we can write one set of keybindings.
      (define-key key-translation-map (kbd "<print>") (kbd "<apps>"))
      (define-key key-translation-map (kbd "<menu>") (kbd "<apps>"))))

;;(define-key global-map (kbd "<apps> h")
;;   (lambda () (interactive) (message "hello from menu key via <apps> leader key")))


;; ******************* Global Function keys ********************
(define-key global-map (kbd "<f2>") (lambda () (interactive) (find-file "~/repos/dotfiles/emacs/.emacs.pd.el")))
(define-key helm-map (kbd "<f11>") 'pd-make-helm-full-frame)
(define-key global-map (kbd "<f12>") 'sr-speedbar-toggle)
(define-key global-map (kbd "S-<f12>") 'sr-speedbar-select-window)

;(define-key global-map (kbd "<S-f2>") 'menu-bar-open)
;(define-key global-map (kbd "<C-f2>") 'menu-bar-open)
;; f3, f4 = macros start and end.
;; f5 - f8 = undefined (taken over by pd-vs-minor-mode-map)
;; f9 = undefined
;; f10 = menu-bar-open
;; f11 = full-screen
;; f12 = undefined

;; ******************* Arrow keys ********************
;; Unbind the arrow keys! For hardcore users only.
;; (global-unset-key [left])
;; (global-unset-key [up])
;; (global-unset-key [right])
;; (global-unset-key [down])

;; This takes all M-<arrow> bindings.
(windmove-default-keybindings 'meta)
(define-key global-map (kbd "S-M-<up>") 'enlarge-window)
(define-key global-map (kbd "S-M-<down>") 'shrink-window)
(define-key global-map (kbd "S-M-<left>") 'shrink-window-horizontally)
(define-key global-map (kbd "S-M-<right>") 'enlarge-window-horizontally)

(define-key global-map (kbd "C-<up>") 'endless/backward-paragraph)     ;; Replace standard bindings for bp and fp with better versions.
(define-key global-map (kbd "C-<down>") 'endless/forward-paragraph)
(define-key global-map (kbd "C-<left>") 'backward-word)
(define-key global-map (kbd "C-<right>") 'forward-word)

(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

(define-key global-map (kbd "C-M-<left>") 'beginning-of-defun)     ;; beg/end of defun is C-M-a or e, which is too hard to type.
(define-key global-map (kbd "C-M-<right>") 'end-of-defun)


;; ******************* Small pad keys ********************
(define-key global-map (kbd "C-S-<prior>") (lambda () (interactive) (pd-set-candidate-font -1 (selected-frame) t)))
(define-key global-map (kbd "C-S-<next>") (lambda () (interactive) (pd-set-candidate-font 1 (selected-frame) t)))
(define-key global-map (kbd "M-S-<prior>") 'text-scale-decrease)
(define-key global-map (kbd "M-S-<next>") 'text-scale-increase)

;; ******************* Main number keys ********************
;; C-0..9 and M-0..9 are normally bound to digit-argument, which can be used via C-u anyway.
(define-key global-map (kbd "M-1") (lambda () (interactive) (jump-to-register ?z)))
(define-key global-map (kbd "M-2") (lambda () (interactive) (window-configuration-to-register ?z) (message "Window configuration saved")))
(define-key global-map (kbd "M-3") (lambda () (interactive) (point-to-register ?z) (message "Point saved")))
(define-key global-map (kbd "M-9") 'backward-sexp)
(define-key global-map (kbd "M-0") 'forward-sexp)

;; ******************* Letter/main section keys ********************
(define-key global-map (kbd "M-/") 'hippie-expand)
(define-key global-map (kbd "M-;") 'endless/comment-line-or-region)
(define-key global-map (kbd "M-j") (lambda () (interactive) (join-line -1)))

(define-key global-map (kbd "C-a") 'pd-back-to-indentation-or-beginning)
(define-key global-map (kbd "C-=") 'fci-mode)

(define-key global-map (kbd "C-'") 'er/expand-region)
(define-key global-map (kbd "C-@") (lambda () (interactive) (er/expand-region -1)))
(define-key global-map (kbd "M-'") 'mark-defun)

;; The keys C-` , . ' ; ? are all available.
(define-key global-map (kbd "C-\\") 'hs-toggle-hiding)
(define-key global-map (kbd "C-|") 'hs-show-all)

(define-key global-map (kbd "<apps> dw") 'delete-trailing-whitespace)
(define-key global-map (kbd "<apps> g") 'magit-status)

(define-key global-map (kbd "<apps> rb") (lambda () (interactive) (revert-buffer nil t)))
(define-key global-map (kbd "<apps> rj") 'jump-to-register)
(define-key global-map (kbd "<apps> rp") 'point-to-register)
(define-key global-map (kbd "<apps> rw") 'window-configuration-to-register)

(define-key global-map (kbd "<apps> sp") 'pd-sort-paragraph)
(define-key global-map (kbd "<apps> w") 'pd-copy-current-line)

(define-key global-map (kbd "C-x g") 'magit-status)
(define-key global-map (kbd "C-x C-g") 'magit-status)

;; ******************* C/C++ mode keys ********************
;; Create a keymap with Visual Studio compatible keymappings.
;; See http://ergoemacs.org/emacs/elisp_menu_for_major_mode.html
;; for what is going on here.
(defvar pd-vs-minor-mode-map nil "Keymap for Visual Studio compatibility.")

;; TODO: We should really setup the gud commands only in a gud-mode-hook.

(when (not pd-vs-minor-mode-map)
  (setq pd-vs-minor-mode-map (make-sparse-keymap))
;  (define-key pd-vs-minor-mode-map (kbd "<f5>") 'gud-run) ; continue (gdb command = continue)
;   C-F5 = run without debugging
;   S-F5 = stop debugging
;  CS-F5 = restart
  (define-key pd-vs-minor-mode-map (kbd "<f6>") 'pd-compile-without-confirmation)
  (define-key pd-vs-minor-mode-map (kbd "<S-f6>") 'pd-compile-clean-one-shot)
  (define-key pd-vs-minor-mode-map (kbd "<C-f6>") 'compile) ; make -k, the original compile command.
  (define-key pd-vs-minor-mode-map (kbd "<f7>") 'ff-find-other-file) ; View.ToggleDesigner in VS.
  (define-key pd-vs-minor-mode-map (kbd "<S-f7>") (lambda () (interactive) (ff-find-other-file t))) ; in a split window.
  (define-key pd-vs-minor-mode-map (kbd "<f8>") 'next-error)
  (define-key pd-vs-minor-mode-map (kbd "<S-f8>") 'previous-error)
  ;;(define-key pd-vs-minor-mode-map (kbd "<f9>") 'gud-break) ; toggle breakpoint (gdb command = break)
  ;  CS-F9 = delete all breakpoints = typing d.
  ;; (define-key pd-vs-minor-mode-map (kbd "<f10>") 'gud-next)  ; step over (gdb command = next)
  ;; (define-key pd-vs-minor-mode-map (kbd "<C-f10>") 'gud-until)  ; run to cursor (gdb command = advance)
  ;; (define-key pd-vs-minor-mode-map (kbd "<CS-f10>") 'gud-jump)  ; set next statement (gdb command = jump)
  ;; (define-key pd-vs-minor-mode-map (kbd "<f11>") 'gud-step)  ; step in (gdb command = step)
  ;; (define-key pd-vs-minor-mode-map (kbd "<S-f11>") 'gud-finish)  ; step out (gdb command = finish)
;;    F12 = go to definition
;"  C-Brk = cancel current build
  )

(define-minor-mode pd-vs-minor-mode
   "A minor mode to establish Visual Studio compatible key mappings."
   nil " vs" 'pd-vs-minor-mode-map)

(add-hook 'c-mode-common-hook (lambda () (pd-vs-minor-mode 1)))
(add-hook 'compilation-mode-hook (lambda () (pd-vs-minor-mode 1)))
(add-hook 'gdb-mode-hook (lambda () (tool-bar-mode 1)))

(message "KEYBINDINGS - END.")


;;; $$ Server (aka daemon) mode.
;; There are two ways to start Emacs in daemon mode.
;;   1. Run 'emacs-w32 --daemon' in .bashrc or other startup script.
;;      This creates an Emacs process which runs in the background and is not
;;      attached to any tty.
;;   2. Start Emacs normally after logging in, then call the 'server-start'
;;      function, either manually or at the end of your .emacs file.
;;
;; 1 may seem simpler, but in fact when Emacs is started this way some (hard to
;; determine) functions, variables and settings do not work because there is no
;; frame or window system defined. This can make configuration difficult. Cursor
;; colors, fonts etc. do not get set when you expect them to be.
;; Therefore, I recommend method 2. This .emacs file has been designed with it
;; in mind. So, after I logon I just do "we &" to create a new graphical Win32
;; Emacs with an initial frame - which, typically, I never close. The call to
;; server-start turns this initial Emacs into a server.
;;
;; Other info: daemonp - predicate can be used to detect daemon mode.

(message "SERVER MODE - START.")
(load "server")
(unless (server-running-p)
  (server-start))
(message "SERVER MODE - END.")
