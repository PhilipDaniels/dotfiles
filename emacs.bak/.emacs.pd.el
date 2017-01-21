;;; -*- mode: emacs-lisp -*-
;;; -*- coding: utf-8 -*-
;;; Local Variables:
;;; End:

;; You can use this technique to exit the file early.
;;(with-current-buffer " *load*"
;;  (goto-char (point-max)))

;; TODO: Move into separate files?
;; make daemon mode work.
;; check still works on Linux
;; Windows compatible keybindings for copy/paste/save/find and ctrl-arrow for word movement
;; underline function

(if (functionp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (functionp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;;; $$ REQUIRES.
(message "REQUIRES - BEGIN.")

;; Just a few of my own libraries and packages that are not available on MELPA.
(add-to-list 'load-path "~/repos/dotfiles/emacs/lisp")

(setq bm-restore-repository-on-load t)
;;(require 'bm)
(require 'bookmark+)
(require 'buffer-move)
(require 'cycle-buffer)
(require 'dash)
(require 'dedicated)
(require 'expand-region)
(require 'fill-column-indicator)
(require 'golden-ratio)
(require 'gud)
(require 'helm)
(require 'helm-config)
(require 'hideshow)
(require 'hlinum)
(require 'mic-paren)
(require 'org)
(require 'pd)
(require 'pd-cpp)
(require 'recentf-ext)
(require 's)
(require 'shackle)
(require 'smartparens-config)
(require 'speedbar)
(require 'term)
;; Unbound provides the command describe-unbound-keys. Try a parameter of 8.
(require 'unbound)
(require 'which-func)
(require 'whitespace)
(require 'windmove)
(require 'yasnippet)

(message "REQUIRES - END.")


;; $$ DO THIS EARLY.
(defvar pd-at-home t "t if this Emacs is being run at home, nil if at work.")

(if (s-starts-with-p "rd" system-name t)
    (setq pd-at-home nil))

(message "Host = %s, pd-at-home = %s, system-type=%s, window-system=%s"
         system-name pd-at-home system-type window-system)


;;; $$ FUNCTIONS.
(message "FUNCTIONS - BEGIN.")

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defvar rectangle-mark-mode)
(defun hydra-ex-point-mark ()
  "Exchange point and mark."
  (interactive)
  (if rectangle-mark-mode
      (exchange-point-and-mark)
    (let ((mk (mark)))
      (rectangle-mark-mode 1)
      (goto-char mk))))

(defun pd-get-or-create-ansi-term (bufname)
  "Switch to an ansi-term buffer named BUFNAME if it exists, or
create one if no such buffer does not exist."
  (interactive)
  (let ((dbn (concat "*" bufname "*")))
    (if (get-buffer dbn)
        (switch-to-buffer dbn)
      (ansi-term "/bin/bash" bufname))
  (get-buffer-process dbn)))

(defun pd-ansi-term ()
  "Create or switch to an ansi-term buffer with an initial
working directory that is the same as the current buffer's
DEFAULT-DIRECTORY. This is a quick way to bring up a terminal in
the same directory as the buffer.

The new ansi-term buffer will have a name that matches the
DEFAULT-DIRECTORY. If such a buffer already exists, it is
switched to rather than created.

If the current buffer is a remote buffer then an 'ssh' command
followed by a 'cd' command will be sent to the new ansi-term to
leave you in the correct directory.

Based on http://oremacs.com/2015/01/10/dired-ansi-term/

TODO
1) Make ansi-term mode update the buffer name when you change
directory. At the moment the buffer keeps its original name,
which means there is a chance this function will give you a
buffer that is not actually in that directory.

2) If called when already in an ansi-term, generate another term
with a unique buffer name. See
https://ftp.gnu.org/old-gnu/Manuals/elisp-manual-21-2.8/html_chapter/elisp_27.html
"
  (interactive)
  (if (not (file-remote-p default-directory))
      (pd-get-or-create-ansi-term default-directory)
    ;;(message "Remote file %s" default-directory)
    (let* ((parts (tramp-dissect-file-name default-directory t))
           (uid (aref parts 1))
           (host (aref parts 2))
           (dir (aref parts 3))
           (cmd (format "ssh %s@%s\ncd '%s'\n" uid host dir))
           (bname (format "ansi-term:%s" default-directory))
          )
      (term-send-string
       (pd-get-or-create-ansi-term bname)
       cmd)
     )))

(message "FUNCTIONS - END.")


;;; $$ MODES.
(message "MODES - BEGIN.")

(persistent-scratch-setup-default)
(winner-mode 1)
(semantic-mode 1)
(delete-selection-mode 1)
;; (smartparens-global-mode 1)
;; (show-smartparens-global-mode 1)
;; (setq sp-show-pair-delay 0)

;; The style I want to use in c++ mode.
(c-add-style "pd-style"
             '("k&r"
               (indent-tabs-mode . nil)        ; use spaces rather than tabs
               (c-basic-offset . 4)            ; indent by four spaces
               (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (brace-list-open . 0)
                                   (statement-case-open . 0)
                                   (arglist-intro . 0)
                                   ))))

(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "pd-style")
            (pd-cpp-auto-mode)
            ))

(add-hook 'before-save-hook 'pd-cpp-cleanup-buffer-maybe)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

(when (eq system-type 'cygwin)
  (setq powershell-location-of-exe
        (s-trim (shell-command-to-string "which powershell.exe"))))

;; Dired.
;; Stop dired from opening lots of new buffers when you press RET to edit a file.
;; (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
;; http://oremacs.com/2015/01/13/dired-options/
;; http://oremacs.com/2015/01/10/dired-ansi-term/
(setq dired-listing-switches "-laGh1v --group-directories-first")
(setq dired-dwim-target t)

;; Markdown mode.
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

;; Teach dired to unzip zip files (use the Z key).
;(eval-after-load "dired-aux"
;  '(add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip")))


;; Helm mode. Based on http://tuhdo.github.io/helm-intro.html
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
(setq helm-semantic-fuzzy-match nil)
(setq helm-imenu-fuzzy-match nil)
(setq helm-M-x-fuzzy-match nil)
(setq helm-apropos-fuzzy-match nil)

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
(helm-mode 1)

;; Use shackle to make helm always appear as a window at the bottom spanning
;; the full width of the screen. Also get rid of the shackle lighter, as it
;; used to use a UTF char 2603, Snowman, which does not appear in many fonts
;; and causes the modeline to grow in height. Actually, later versions of
;; shackle mode do not have a lighter, plus I actually use rich-minority
;; to turn off all minor mode lighters.
(shackle-mode 1)
(setq-default shackle-lighter "")
(setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align below :ratio 0.4)))

;; See http://capitaomorte.github.io/yasnippet/. Load yasnippet, but only load
;; my snippets (there are many examples under the elpa/yasnippet folder which we
;; do not want to load).
(setq yas-snippet-dirs '("~/repos/dotfiles/emacs/snippets"))
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'markdown-mode-hook #'yas-minor-mode)


;; Automatically cleanup files before save in programming modes. Based on
;; http://stackoverflow.com/questions/19174302. An alternative, using ws-trim or
;; ws-butler, is mentioned there which can reduce git commit noise, but since
;; I only use Emacs for my own repos at the moment it doesn't matter.
;; (add-hook 'before-save-hook (lambda()
;;                               (when (derived-mode-p 'prog-mode)
;;                                 (pd-cleanup-programming-buffer))))

(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'csharp-mode-hook 'hs-minor-mode)
(add-hook 'sh-mode-hook 'hs-minor-mode)

;; Don't prompt with "Active processes exist, kill them?" when exiting Emacs.
;; http://stackoverflow.com/questions/2706527/make-emacs-stop-asking-active-processes-exist-kill-them-and-exit-anyway
(add-hook 'comint-exec-hook
          (lambda () (set-process-query-on-exit-flag
                      (get-buffer-process (current-buffer)) nil)))
(add-hook 'term-exec-hook
          (lambda () (set-process-query-on-exit-flag
                      (get-buffer-process (current-buffer)) nil)))

;; Make exiting ansi-term kill the buffer.
(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'oleh-term-exec-hook)

;; Don't prompt about killing buffers with live processes attached to them.
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Make ansi-term stop prompting and always use bash.
;; From http://echosa.github.io/blog/2012/06/06/improving-ansi-term/
(defvar pd-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list pd-term-shell)))
(ad-activate 'ansi-term)

;; Tramp mode.
(setq tramp-default-method "sshx")
(setq tramp-default-user "phil")


;; Org mode.
(setq org-directory "~/repos/org")
(setq org-log-done t)
(setq org-hierarchical-todo-statistics nil)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(add-hook 'org-mode-hook (lambda () (local-unset-key (kbd "C-#"))))

;; smartparens interferes with the entry of links in org-mode. Turn it off.
;; From https://github.com/Fuco1/smartparens/wiki/Permissions
;;(sp-local-pair 'org-mode "[" nil :actions nil)

;; Turn on rainbow delimiters in all programming modes.
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; ;; Visible Bookmarks (package bm, https://github.com/joodland/bm)
;; (setq bm-cycle-all-buffers t)
;; (setq-default bm-buffer-persistence t)
;; (setq bm-repository-file "~/.emacs.d/bm-repository")
;; (add-hook' after-init-hook #'bm-repository-load)
;; (add-hook 'find-file-hooks #'bm-buffer-restore)
;; (add-hook 'kill-buffer-hook #'bm-buffer-save)
;; (add-hook 'kill-emacs-hook #'(lambda nil
;;                               (bm-buffer-save-all)
;;                               (bm-repository-save)))
;; (add-hook 'after-save-hook #'bm-buffer-save)
;; (add-hook 'after-revert-hook #'bm-buffer-restore)
;; (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

;; Bookmark+.
(setq bookmark-version-control t)
(setq bookmark-save-flag 1)

;; Shells.
;; M-x shell runs a shell as a sub-process, communicating with it via pipes.
;; It is not fully functional.
;; eshell is a shell written in elisp. It is slow and not very complete.
;; term and ansi-term (better) are better choices.
;; There is also multi-term, not sure what that adds though.
;; tl;dr - use ansi-term for starting shells.

;; Make commits done from the command line also use the Magit COMMIT_MSG mode.
(global-git-commit-mode)

(message "MODES - END.")


;;; $$ APPEARANCE.
(message "APPEARANCE - BEGIN.")

(defun pd-font-exists (font &optional frame)
  "Return a font if it exists, nil otherwise. Does not work in daemon mode."
  (find-font (font-spec :name font) frame))

;; When emacs starts in daemon mode there is no frame and hence some GUI related
;; functions such as find-font do not work. There is however a special
;; non-visible daemon frame.
;; See https://github.com/syl20bnr/spacemacs/issues/6197 at 7 Jun 2016
;; http://emacs.stackexchange.com/questions/12351/when-to-call-find-font-if-launching-emacs-in-daemon-mode
;; and https://github.com/syl20bnr/spacemacs/blob/master/core/core-display-init.el
;; and http://emacs.1067599.n8.nabble.com/bug-23689-Daemon-mode-on-Windows-quot-w32-initialized-quot-is-set-too-early-td399455.html

;; See https://github.com/chrissimpkins/codeface to get a big zip of ttf and otf
;; fonts. To determine the name that Emacs uses for a font, the easiest way I
;; know is to use the customize system to pick a default font, then save
;; options, the name of the font then appears in the .emacs file.
(defvar pd-font-candidates '("Consolas-10"
                             "Consolas-12"
                             "Consolas-11"
                             "Cousine-10"
                             "Source Code Pro-12"
                             "DejaVu Sans Mono-12"
                             "Droid Sans Mono-12"
                             "Courier New-10"
                             "Aurulent Sans Mono-10"
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

(setq custom-theme-directory "~/repos/dotfiles/emacs/themes")

(setq x-gtk-use-system-tooltips nil)
(setq ring-bell-function nil)
(setq visible-bell 1)
(setq column-number-mode 1)
(setq line-number-mode 1)
(size-indication-mode 1)
(blink-cursor-mode 0)
(setq blink-cursor-blinks 0)
;; http://stackoverflow.com/questions/13625080/looking-forward-a-way-to-make-cursor-blinks-like-a-heartbeat-in-emacs
;;(setq-default blink-cursor-alist '((t . (hbar . 5))))
;;(setq-default blink-cursor-alist '((t . nil)))
(global-hl-line-mode 0)
(setq blink-matching-paren nil)  ;; We use the mic-paren package instead.
(which-function-mode -1)         ;; Slow and pointless and some modes have a nasty habit of enabling it,
(setq which-func-modes nil)      ;; such as Powershell mode. Together, these two lines disable it.
(setq rm-blacklist ".*")         ;; List of lighter strings or simply ".*"
(rich-minority-mode 1)
;; Some example frame titles. See Emacs wiki.
;; (setq frame-title-format (list "\u25b6 %f \u25c0 " user-login-name "@" system-name))
;; (setq frame-title-format (list "\u27a4 %f    \u27a4 " user-login-name "@" system-name))
;; (setq frame-title-format (list "\u2b24 %f    \u2b24 " user-login-name "@" system-name))
(setq frame-title-format (list user-login-name "@" system-name ":%f"))

;; fci-mode can cause an increase in the vertical separation of lines, so leave
;; it off by default. It is bound to C-= below, for ease of use.
(setq fci-rule-width 2)
;;(setq fci-rule-color "blue")


;; Show a red rectangle for trailing whitespace, and color long lines.
(setq-default show-trailing-whitespace t)
(setq-default whitespace-line-column 80)
(setq-default whitespace-style '(face trailing lines-tail))
;; Turn off display of trailing whitespace in some modes.
(dolist (hook '(buffer-menu-mode-hook compilation-mode-hook
                                      diff-mode-hook
                                      magit-popup-mode-hook
                                      shell-mode-hook
                                      term-mode-hook
                                      calendar-mode-hook))
  (add-hook hook (lambda () (set-variable 'show-trailing-whitespace nil))))
;; We need to turn on whitespace-mode to get the display of the >80 character lines working.
(add-hook 'prog-mode-hook 'whitespace-mode)

;; This face is used to highlight the selected thing (e.g. function in source
;; file). Box is on by default, which causes a temporary line-height increase
;; which is visually irritating.
;; (set-face-attribute 'speedbar-highlight-face nil :box nil :background "black")
(setq-default sr-speedbar-right-side t)

;; Ensure that ^M characters never appear in text modes.
(add-hook 'text-mode-hook 'pd-hide-dos-eol)
(add-hook 'magit-diff-mode-hook 'pd-hide-dos-eol)
(add-hook 'git-timemachine-mode-hook 'pd-hide-dos-eol)

;; Setup for mic-paren mode.
;; Use list-faces-display to pick a suitable face.
(paren-activate)
(setq paren-match-face 'mode-line)

;;(add-to-list 'default-frame-alist '(height . 50))
;;(add-to-list 'default-frame-alist '(width . 86))
;;(show-paren-mode 1)             ;; Superceded by smartparens mode.
;;(setq-default show-paren-delay 0)
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;;(global-linum-mode 1)           ;; This is very slow with long lines.
;;(setq linum-format "%4d ")      ;; So we don't need this either.
;;(display-time-mode 1)
;;(hlinum-activate)         ;; Slow
;;(set-face-background 'hl-line "black")
;;(set-face-foreground 'hl-line nil)
;;(set-face-underline-p 'hl-line nil)

;;Smart mode line, see https://github.com/Malabarba/smart-mode-line
;;(sml/setup)
;;(setq sml/theme 'dark)
;;(add-to-list 'sml/replacer-regexp-list '("^~/repos/" "RP:") t)
;;(add-to-list 'sml/replacer-regexp-list '("^/c/work/bitbucket/" "BB:") t)
;;(sml/setup)

(message "APPEARANCE - END.")


;;; $$ GENERAL VARIABLES.
(message "GENERAL VARIABLES - BEGIN.")

(add-hook 'before-save-hook 'time-stamp)
(add-to-list 'helm-grep-ignored-files "*.dll")
(add-to-list 'helm-grep-ignored-files "*.exe")
(add-to-list 'helm-grep-ignored-files "*.obj")
(add-to-list 'helm-grep-ignored-files "*.pdb")
(fset 'yes-or-no-p 'y-or-n-p)
(recentf-mode 1)
(setq auto-save-default nil)
(setq backup-inhibited t)
(setq comment-empty-lines t)
(setq compilation-ask-about-save nil)
(setq compilation-scroll-output 'first-error)
(setq confirm-nonexistent-file-or-buffer nil)
(setq delete-by-moving-to-trash t)
(setq disabled-command-function nil)
(setq echo-keystrokes 0.1)
(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq inhibit-startup-message t)
(setq initial-major-mode 'emacs-lisp-mode)
(setq initial-scratch-message nil)
(setq magit-push-always-verify nil)
(setq make-backup-files nil)
(setq message-log-max 50000)
(setq recentf-max-menu-items 60)
(setq recentf-max-saved-items 500)
(setq require-final-newline t)
(setq scroll-margin 3)
(setq scroll-conservatively 10000)
(setq scroll-error-top-bottom t)
(setq scroll-preserve-screen-position t)
(setq sentence-end-double-space nil)
(setq use-dialog-box nil)
(setq user-full-name "Philip Daniels")
(setq user-mail-address "philip.daniels1971@gmail.com")
(setq vc-follow-symlinks t)
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default truncate-lines 1)
(setq windmove-wrap-around t)

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

;; Don't do this - it stops you from invoking Emacs on the command line
;; for a single file, and also changes the cwd which is very annoying.
;; (if pd-at-home
;;     (setq initial-buffer-choice "~/repos/org/home.org")
;;   (setq initial-buffer-choice "/c/work/work.org"))

(message "GENERAL VARIABLES - END.")


;;; $$ Hydras
(message "HYDRAS - BEGIN.")

(defhydra hydra-windows ()
  "M-arrow = switch, S-arrow = size, C-arrow = move"
  ("M-<left>" windmove-left nil)
  ("M-<right>" windmove-right nil)
  ("M-<up>" windmove-up nil)
  ("M-<down>" windmove-down nil)
  ("S-<left>" hydra-move-splitter-left nil)
  ("S-<right>" hydra-move-splitter-right  nil)
  ("S-<up>" hydra-move-splitter-up nil)
  ("S-<down>" hydra-move-splitter-down nil)
  ("C-<left>" buf-move-left nil)
  ("C-<right>" buf-move-right nil)
  ("C-<up>" buf-move-up nil)
  ("C-<down>" buf-move-down nil)
  ("p" previous-buffer "prev-buf")
  ("n" next-buffer "next-buf")
  ("1" delete-other-windows "1")
  ("d" delete-window "del")
  ("k" kill-buffer "kill")
  ("s" save-buffer "save")
  ("u" (progn (winner-undo) (setq this-command 'winner-undo)) "undo")
  ("r" winner-redo "redo")
  ("b" helm-mini "helm-mini" :exit t)
  ("f" helm-find-files "helm-find" :exit t)
  ("|" (lambda () (interactive) (split-window-right) (windmove-right)))
  ("_" (lambda () (interactive) (split-window-below) (windmove-down)))
  ("q" nil "cancel")
  )

(defhydra hydra-fonts ()
  "Adjust font size and face"
  ("+" text-scale-increase "larger")
  ("-" text-scale-decrease "smaller")
  ("n" (lambda () (interactive) (pd-set-candidate-font 1 (selected-frame) t)) "next")
  ("p" (lambda () (interactive) (pd-set-candidate-font -1 (selected-frame) t)) "prev")
  ("q" nil "cancel"))

;; Create a hydra to switch themes. We use the Emacs 24 theme engine (aka
;; deftheme) only, not the old color-theme.el engine.
;; A strange and non-obvious thing - calling load-theme applies the new
;; theme on top of the previous one - themes accumulate! Therefore, in
;; order to get a reasonable implementation we must disable all existing
;; themes before loading the new one, or alternatively keep track of the
;; current theme and disable it before loading a new one, which can be done
;; by writing our own wrapper around load-theme.
;; See Drew's answer at http://emacs.stackexchange.com/questions/3112/how-to-reset-color-theme?rq=1
;; I choose to use my own function so that I have flexibility to enable
;; more than one theme simultaneously if I want to.
;;
;; Applying extra theming
;; For example, you want to tweak cursor colors, modeline colors etc.
;; This is problematic, because your changes are not "part of" the theme
;; and hence cannot be reliably undone (see Drew's bug report at the above
;; SO link). There appear to be several possible solutions, none of which
;; are great:
;;
;; 1. For themes you really care about, customize the theme with your
;;    changes and use your customized version instead. This involves
;;    tweaking (setq custom-theme-directory "your-directory")
;;    and (add-to-list 'custom-theme-load-path "your-directory")
;;    and appears to be a reasonable solution.
;;    Pro: nicely self-contained.
;;    Cons: you have to tweak each theme.
;; 2. Write a custom theme which just contains your changes, then apply it
;;    whenever you change master themes (taking advantage of the fact that
;;    themes "accumulate").
;;    Pro: easy to apply your changes on top of several themes.
;;    Cons: requires all theme loading be done with your own custom function.
;; 3. Write a fn, which may be arbitrarily smart, which simply modifies
;;    faces and colours as appropriate and run it after switching themes.

(defvar pd-current-theme nil "The theme that was last loaded by pd-load-theme.")

(defun pd-apply-theme-overrides ()
  "Apply my theme overrides."
  ;; Post-theme customizations that I apply to everything.
  ;; Some things *should* be prominent.
  ;; In terminal Emacs, these color names appear to map to the corresponding
  ;; solarized colors automatically (see list-colors-display).
  (set-cursor-color "red")
  (set-face-attribute 'helm-selection         nil :background "red"   :foreground "white" :inverse-video nil)
  (set-face-attribute 'menu                   nil :background "black" :foreground "white")
  (set-face-attribute 'tty-menu-enabled-face  nil :background "black" :foreground "white")
  (set-face-attribute 'tty-menu-selected-face nil :background "white" :foreground "black")
  (set-face-attribute 'tty-menu-disabled-face nil :background "black" :foreground "red")
  ;; Provide a sort of "on-off" modeline whereby the current buffer has a nice
  ;; bright blue background, and all the others are in cream.
  (when (eq theme 'solarized)
    (set-face-attribute 'mode-line nil          :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
    (set-face-attribute 'mode-line-inactive nil :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil))
  )

(defun pd-load-theme (theme &optional bg-mode)
  "Load a theme, disabling the current custom theme first.
Normally themes 'accumulate' as you load them which gives a very
confusing experience. This function prevents strange display and
performance problems by disabling the current active custom
theme (if any) before loading the new theme.

This function can only do its job properly if you do all theme
loading using it.

BG-MODE is used to set the variable `frame-background-mode'.
Valid values are nil, 'dark and 'light."
  (if pd-current-theme (disable-theme pd-current-theme))
  (setq pd-current-theme theme)
  ;; Solarized (the only one I am sure about) uses the frame-background-mode to
  ;; determine how to display itself. The default for this variable is nil,
  ;; which most themes seem happy with.
  (setq frame-background-mode bg-mode)
  (mapc 'frame-set-background-mode (frame-list))
  (load-theme theme t)
  (message "Theme set to %s" theme)
  (pd-apply-theme-overrides))

;; All these themes are available on MELPA.
(defhydra hydra-themes (:hint nil)
  "
Favourite : _sd_ Sol Dark       _sl_ Sol Light       _zb_ Zenburn        _ob_ Obsidian        _ty_ TTY Dark
Dark      : _gd_ Gruber Darker  _cp_ Cyberpunk       _gb_ Gruvbox        _bb_ BusyBee         _uw_ Underwater
            _md_ Minimal Dark    _mn_ Monokai        _ml_ Molokai        _cf_ Calm Forest
Light     : _lv_ Leuven         _hl_ Hemisu-Light    _mi_ Minimal Light  _ao_ Aalto Light
Grey      : _ma_ Material       _az_ Anti-Zenburn    _fu_ Flat UI        _sm_ Soft Morning    _tt_ TangoTango
            _je_ JEdit Grey     _cb_ Charcoal Black
Blue      : _rs_ Resolve        _bs_ Blue Sea        _rp_ Raspopovic     _ad_ Aalto Dark      _pr_ Parus
Mono      : _ro_ Retro Orange   _mo_ Monochrome      _rg_ Retro Green    _gp_ Green Phosphor
Consider  :
Rejects   : _ab_ Alect Black _al_ Alect Light _hd_ Hemisu Dark _gr_ Goldenrod
"
  ("ab" (pd-load-theme 'alect-black))
  ("ad" (pd-load-theme 'aalto-dark))
  ("al" (pd-load-theme 'alect-light))
  ("ao" (pd-load-theme 'aalto-light))
  ("az" (pd-load-theme 'anti-zenburn))
  ("bb" (pd-load-theme 'busybee))
  ("bs" (pd-load-theme 'blue-sea))
  ("cb" (pd-load-theme 'charcoal-black))
  ("cf" (pd-load-theme 'calm-forest))
  ("cp" (pd-load-theme 'cyberpunk))
  ("fu" (pd-load-theme 'flatui))
  ("gb" (pd-load-theme 'gruvbox))
  ("gd" (pd-load-theme 'gruber-darker))
  ("gp" (pd-load-theme 'green-phosphor))
  ("gr" (pd-load-theme 'goldenrod))
  ("hd" (pd-load-theme 'hemisu-dark))
  ("hl" (pd-load-theme 'hemisu-light))
  ("je" (pd-load-theme 'jedit-grey))
  ("lv" (pd-load-theme 'leuven))
  ("ma" (pd-load-theme 'material))
  ("md" (pd-load-theme 'minimal))
  ("mi" (pd-load-theme 'minimal-light))
  ("ml" (pd-load-theme 'molokai))
  ("mn" (pd-load-theme 'monokai))
  ("mo" (pd-load-theme 'monochrome))
  ("ob" (pd-load-theme 'obsidian))
  ("pr" (pd-load-theme 'parus))
  ("rg" (pd-load-theme 'retro-green))
  ("ro" (pd-load-theme 'retro-orange))
  ("rp" (pd-load-theme 'raspopovic))
  ("rs" (pd-load-theme 'resolve))
  ("sd" (pd-load-theme 'solarized 'dark))
  ("sl" (pd-load-theme 'solarized 'light))
  ("sm" (pd-load-theme 'soft-morning))
  ("tt" (pd-load-theme 'tangotango))
  ("ty" (pd-load-theme 'tty-dark))
  ("uw" (pd-load-theme 'underwater))
  ("zb" (pd-load-theme 'zenburn))
  ("q"  nil)
  )

;;(pd-load-theme 'gruber-darker)

;; This is package "solarized-theme" from https://github.com/bbatsov/solarized-emacs
;; Problems: increases size of the characters '(' and ')' which causes a jarring effect.
;; Avoid all font-size changes
(setq solarized-height-minus-1 1)
(setq solarized-height-plus-1 1)
(setq solarized-height-plus-2 1)
(setq solarized-height-plus-3 1)
(setq solarized-height-plus-4 1)
;;(pd-load-theme 'solarized-dark)

;; This is package "color-theme-solarized" from https://github.com/sellout/emacs-color-theme-solarized
;; Problems: has wrong background color on Emacs 25.
(pd-load-theme 'solarized 'dark)

(message "HYDRAS - END.")


;;; $$ KEYBINDINGS.
(message "KEYBINDINGS - BEGIN.")

;; Emacs understands the following modifiers:
;;    M- (meta)      Alt on my keyboard
;;    C- (control)   Ctrl keys on my keyboard
;;    S- (shift)     Shift keys on my keyboard
;;    s- (super)     Note lower case 's'. On Linux, this is usually bound to
;;                   the Windows key automatically. On Windows, the Windows key
;;                   is monopolized by Windows (see below).
;;    H- (hyper)     Not usually bound to any key by default.
;;    A- (alt)       Not usually bound to any key by default. Note that the
;;                   Alt key sends Meta! Best to avoid A- mappings, use H- and
;;                   s- instead.
;;
;; To find out what the name of a key is, the easiest way is to type the key,
;; then type C-h l (view-lossage), which shows you the last 100 keys pressed.
;; See unbound.el for how to find unbound keys.
;;
;; Keynames for a PC keyboard
;; ==========================
;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Function-Keys.html#Function-Keys
;;
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
;;
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
;; Windows. So these keys are unavailable *even if* you use the
;; w32-pass-lwindow-to-system technique described below. To make these key
;; combinations available a registry hack must be used to disable all the
;; default bindings - use a .reg file with this text:
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
;;
;; Recommendations
;; ===============
;; * Restrict repeatable keys (those that you might want to press several times
;;   quickly in succession) to C- and M-.
;; * Do not use the Windows keys.
;; * Make the apps/menu key send the "<apps>" leader.
;; * Do not use the super s- prefix.
;;
;; A full size keyboard is CTRL    WIN ALT SPACE ALTGR WIN APPS  CTRL
;; My work laptop is       CTRL FN WIN ALT SPACE ALTGR           CTRL
;; My home laptop is       CTRL FN WIN ALT SPACE ALTGR           CTRL
;; MK Disco is             CTRL    WIN ALT SPACE ALT   WIN FN    CTRL (FN is really APPS) This is standard US layout.
;; So the common set is    CTRL    WIN ALT SPACE ALTGR           CTRL
;;
;; Reserving WIN for Windows, we are left with
;;
;;                         C-          M-                        C-
;;
;; WIN is usually super in Linux, so super is a good modifier to bind keys to,
;; assuming we can make a proper super key in Windows. In Linux, it should be
;; possible to do this with xkb quite easily, see http://www.charvolant.org/%7Edoug/xkb/html/node5.html
;;
;; Some standard keybindings
;; =========================
;; C-c <letter>       ; always reserved for your own bindings
;; C-f, C-b, C-n, C-p : move one character or line
;; C-v, M-v           : forward, backward one screen
;; M-f, M-b           : forward, backward by 1 word
;; C-M-f, C-M-b       : forward/backward by 1 sexp
;; C-SPC              : set mark and activate region
;; C-SPC C-SPC        : just set the mark
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
;; C-_                : undo
;; f3, f4             : start, end/run keyboard macro
;; C-x (,  C-x )      : start, end/run keyboard macro
;; C-x `              : next-error
;; C-x <left>/<right> : previous-buffer and next-buffer
;; C-x 8 RET          : insert characters by name
;; M-$, M-%           : ispell-word, query-replace foo
;; M-\, M-^, M-z      : delete horz space, delete-indentation, zap-to-char
;; M-c, M-l, M-u      : capitalize/lower/upper word
;; M-m                : back-to-indentation
;; M-h                : mark-paragraph
;;
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
;;
;; Make Win/Apps keys send super/hyper etc.
;; (when (equal window-system 'w32)
;;   (setq w32-pass-lwindow-to-system nil
;;      w32-lwindow-modifier 'super))
;; (when (equal window-system 'w32)
;;   (setq w32-pass-rwindow-to-system nil
;;      w32-rwindow-modifier 'super))
;;
;; Mintty has been hacked by the maintainer to allow the APPS/MENU key to be
;; passed through to the underlying program. Adding the line "Key_Menu=29" to
;; the .minttyrc file causes terminal Emacs to see it as the "<print"> key. The
;; hack was on 2015-11-19 in commit 429cb080e6bfee6136227ca5d41ea61494b36c2d.
;; Given this hack, we can make APPS send the s- (super) prefix like this in
;; both W32 and terminal Emacs. Unfortunately, APPS still does not work like the
;; Alt or Control keys in the terminal, if you hold it down by itself you get
;; lots of input in the output buffer. It does work ok in GUI Emacs though.
(if (equal system-type 'cygwin)
    (if (equal window-system 'w32)
     (setq w32-pass-apps-to-system nil
           w32-apps-modifier 'hyper)
      (define-key local-function-key-map (kbd "<print>") 'event-apply-hyper-modifier)))
;; (define-key global-map (kbd "s-h") (lambda () (interactive) (message "hello from menu key via s- prefix")))
;;
;; Alternatively, we can turn it into a leader key like this.
;; See http://ergoemacs.org/emacs/emacs_menu_app_keys.html
;; (if (equal system-type 'cygwin)
;;     (if (equal window-system 'w32)
;;         (setq w32-pass-apps-to-system nil
;;               w32-apps-modifier nil)
;;       ;; force all alternatives to <apps> so we can write one set of keybindings.
;;       (define-key key-translation-map (kbd "<print>") (kbd "<apps>"))
;;       (define-key key-translation-map (kbd "<menu>") (kbd "<apps>"))))
;;
;; (if (equal system-type 'gnu/linux)
;;     (define-key key-translation-map (kbd "<menu>") (kbd "<apps>")))
;;
;; ;; On X, e.g. in Mint, make the left Windows key, which is normally super,
;; ;; send hyper instead.
;; (setq x-super-keysym 'hyper)
;;                                        ;
;; Over ssh to Linux, window-system is 'x and system-type is 'gnu/linux.
;;
;; Helpful links
;; =============
;; http://emacs.stackexchange.com/questions/1020/problems-with-keybindings-when-using-terminal
;; http://unix.stackexchange.com/questions/116629/how-do-keyboard-input-and-text-output-work/116630#116630
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Translation-Keymaps.html#Translation-Keymaps
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Modifier-Keys.html
;; http://stackoverflow.com/questions/10730775/emacs-create-key-modifier   (for faking using event-apply-hyper-modifier etc.)
;; http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
;;
;; and especially
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Keyboard-Events.html#Keyboard-Events



;; ******************* Global Function keys ********************
(define-key global-map (kbd "<f1>")      (lambda () (interactive) (find-file "~/repos/dotfiles/emacs/emacs_keys.txt")))
(define-key global-map (kbd "<f2>")      'bmkp-next-bookmark)
(define-key global-map (kbd "S-<f2>")    'bmkp-previous-bookmark)
(define-key global-map (kbd "C-<f2>")    'bmkp-toggle-autonamed-bookmark-set/delete)
(define-key global-map (kbd "M-<f2>")    'helm-filtered-bookmarks)
(define-key global-map (kbd "<f9>")      'cycle-buffer-backward)
(define-key global-map (kbd "<f10>")     'cycle-buffer)
(define-key global-map (kbd "S-<f9>")    'cycle-buffer-backward-permissive)
(define-key global-map (kbd "S-<f10>")   'cycle-buffer-permissive)
(define-key global-map (kbd "C-<f9>")    (lambda () (interactive) (kill-buffer nil)))
(define-key global-map (kbd "C-<f10>")   'bury-buffer)
(define-key global-map (kbd "<f11>")     'menu-bar-open)
(define-key global-map (kbd "S-<f11>")   'menu-bar-open)
(define-key global-map (kbd "C-<f11>")   'menu-bar-open)
(define-key global-map (kbd "<f12>")     'sr-speedbar-toggle)
(define-key global-map (kbd "S-<f12>")   'sr-speedbar-select-window)
(define-key global-map (kbd "C-<f12> w") 'hydra-windows/body)
(define-key global-map (kbd "C-<f12> f") 'hydra-fonts/body)
(define-key global-map (kbd "C-<f12> t") 'hydra-themes/body)
(message "KEYBINDINGS - FUNCTION KEYS DONE.")


;; f3, f4 = macros start and end.
;; f5 - f8 = undefined (taken over by pd-vs-minor-mode-map)
;; f9 = undefined
;; f10 = menu-bar-open
;; f11 = full-screen
;; f12 = undefined

;; ******************* Arrow keys ********************
;; Hopefully this will have better compatibility with org-mode
;; (org-mode uses M-arrow to change the order of items).
;; S-<arrow> is shown as available by describe-unbound-keys, but
;; it is actually bound - it selects a region.
(define-key global-map (kbd "C-<up>")      'windmove-up)
(define-key global-map (kbd "C-<down>")    'windmove-down)
(define-key global-map (kbd "C-<left>")    'windmove-left)
(define-key global-map (kbd "C-<right>")   'windmove-right)

(define-key global-map (kbd "C-M-<left>")  'beginning-of-defun)     ;; beg/end of defun is C-M-a or e, which is too hard to type.
(define-key global-map (kbd "C-M-<right>") 'end-of-defun)
(message "KEYBINDINGS - ARROWS DONE.")


;; ******************* Small pad keys ********************

;; CUA compatibility.
(define-key global-map (kbd "C-z")       'undo)   ;; Emacs default = suspend-emacs


;; https://en.wikipedia.org/wiki/Table_of_keyboard_shortcuts
;;(define-key global-map (kbd "C-y")       'redo last operation)
;;(define-key global-map (kbd "C-x")       'cut and place in clipboard) Emacs = prefix key
;;(define-key global-map (kbd "C-c")       'copy to clipboard)  Emacs = prefix key
;;(define-key global-map (kbd "C-v")       'paste clipboard)  Emacs = scroll-up-command
;;(define-key global-map (kbd "C-a")       'select all)  Emacs = pd-back-to-indentation-or-beginning
;; o = open, s = save, n = new, p = print, f = find/search
;; w = save as, r/h = replace, C-S-s = save all, g = goto line

;; ******************* Main number keys ********************
;; C-0..9 and M-0..9 are normally bound to digit-argument, which can be used via
;; C-u anyway, so feel free to grab them for other uses. C-n are available in
;; the terminal, so they should be used in preference.
(define-key global-map (kbd "M-1") (lambda () (interactive) (jump-to-register ?z)))
(define-key global-map (kbd "M-2") (lambda () (interactive) (window-configuration-to-register ?z) (message "Window configuration saved")))
(define-key global-map (kbd "M-3") (lambda () (interactive) (point-to-register ?z) (message "Point saved")))

;; ******************* Letter/main section keys ********************
(define-key global-map (kbd "C-'")       'er/expand-region)
(define-key global-map (kbd "C-; SPC")   'helm-all-mark-rings)
(define-key global-map (kbd "C-; a")     'helm-apropos)
(define-key global-map (kbd "C-; c")     'helm-colors)
(define-key global-map (kbd "C-; f")     'helm-find)
(define-key global-map (kbd "C-; i")     'insert-char)
(define-key global-map (kbd "C-; m")     'helm-man-woman)
(define-key global-map (kbd "C-; o")     'helm-occur)
(define-key global-map (kbd "C-; r")     'helm-all-mark-rings)
(define-key global-map (kbd "C-; s")     'helm-semantic-or-imenu)
(define-key global-map (kbd "C-=")       'fci-mode)
(define-key global-map (kbd "M-'")       (lambda () (interactive) (er/expand-region -1)))
(define-key global-map (kbd "M-#")       'hydra-windows/body)
(define-key global-map (kbd "C-\\")      'hs-toggle-hiding)
(define-key global-map (kbd "C-|")       'hs-show-all)
(define-key global-map (kbd "M-/")       'hippie-expand)
(define-key global-map (kbd "M-;")       'endless/comment-line-or-region)
;; (define-key global-map (kbd "M-[")       'backward-sexp)  Screws up in terminal Emacs
;; fine-key global-map (kbd "M-]")       'forward-sexp)
(define-key global-map (kbd "M-{")       'endless/backward-paragraph)     ;; Replace standard bindings for bp and fp with better versions.
(define-key global-map (kbd "M-}")       'endless/forward-paragraph)
(message "KEYBINDINGS - MAIN KEYS 1 DONE.")


(define-key global-map (kbd "C-S-o")     'pd-duplicate-line-or-region)
(define-key global-map (kbd "C-S-w")     'pd-copy-current-line)
(define-key global-map (kbd "C-a")       'pd-back-to-indentation-or-beginning)
(define-key global-map (kbd "C-x C-b")   'helm-mini)
(define-key global-map (kbd "C-x C-f")   'helm-find-files)
(define-key global-map (kbd "C-x C-g")   'magit-status)
(define-key global-map (kbd "C-x b")     'helm-mini)
(define-key global-map (kbd "C-x g")     'magit-status)
(define-key global-map (kbd "C-x C-t")   'pd-ansi-term)
(define-key global-map (kbd "C-x z")     'undo)
;;(define-key global-map (kbd "C-z")       'pd-replace-all-in-buffer)
(define-key global-map (kbd "C-%")       'pd-replace-all-in-buffer)
(define-key global-map (kbd "M-j")       'pd-join-line)
(define-key global-map (kbd "M-x")       'helm-M-x)
(define-key global-map (kbd "M-y")       'helm-show-kill-ring)
(define-key global-map (kbd "M-SPC")     'pd-no-space)
(define-key global-map (kbd "H-SPC")     'pd-no-space)
(message "KEYBINDINGS - MAIN KEYS 2 DONE.")

(defun pd-bind-key (keyseq func)
  "Helper function to bind keys to both <apps> and H-."
  (let ((e1 (concat "<apps> " keyseq))
        (e2 (format "H-%s%s" (substring keyseq 0 1)
                    (if (equal 1 (length keyseq))
                        ""
                      (concat " " (substring keyseq 1 2)))))
        )
    ;;(define-key global-map (kbd e1) func)
    (define-key global-map (kbd e2) func)))


;; Consider arrow keys too.
;; Want for bookmarks, macros, ggtags, rectangles?
(pd-bind-key "b"  'cycle-buffer-backward-permissive)
(pd-bind-key "d"  'dired-jump)
(pd-bind-key "f"  'cycle-buffer-permissive)
(pd-bind-key "g"  'magit-status)
(pd-bind-key "o"  'pd-duplicate-line-or-region)
(pd-bind-key "t"  'pd-ansi-term)
(pd-bind-key "w"  'pd-copy-current-line)





;; (pd-bind-key "cc" 'pd-cleanup-programming-buffer)
;; (pd-bind-key "dl" 'pd-duplicate-line-or-region)
;; (pd-bind-key "dw" 'delete-trailing-whitespace)
;; (pd-bind-key "g"  'magit-status)
;; (pd-bind-key "rb" 'pd-revert-buffer)
;; (pd-bind-key "rj" 'jump-to-register)
;; (pd-bind-key "rp" 'point-to-register)
;; (pd-bind-key "rw" 'window-configuration-to-register)
;; (pd-bind-key "sp" 'pd-sort-paragraph-dwim)
;; (pd-bind-key "w"  'pd-copy-current-line)

(define-key global-map (kbd "C-# au") 'pd-cpp-add-using)
(define-key global-map (kbd "C-# cl") 'pd-copy-current-line)
(define-key global-map (kbd "C-# cp") 'pd-cleanup-programming-buffer)
(define-key global-map (kbd "C-# df") 'pd-delete-file-and-buffer)
(define-key global-map (kbd "C-# dl") 'pd-duplicate-line-or-region)
(define-key global-map (kbd "C-# dw") 'delete-trailing-whitespace)
(define-key global-map (kbd "C-# ib") 'pd-indent-buffer)
(define-key global-map (kbd "C-# ir") 'indent-region)
(define-key global-map (kbd "C-# jr") 'jump-to-register)
(define-key global-map (kbd "C-# mb") 'mark-whole-buffer)
(define-key global-map (kbd "C-# mf") 'mark-defun)
(define-key global-map (kbd "C-# mp") 'mark-paragraph)
(define-key global-map (kbd "C-# pr") 'point-to-register)
(define-key global-map (kbd "C-# rb") 'pd-revert-buffer)
(define-key global-map (kbd "C-# rf") 'pd-rename-file-and-buffer)
(define-key global-map (kbd "C-# sp") 'pd-sort-paragraph-dwim)
(define-key global-map (kbd "C-# ut") 'pd-untabify-buffer)
(define-key global-map (kbd "C-# wr") 'window-configuration-to-register)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i")   'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")   'helm-select-action) ; list actions using C-z
(message "KEYBINDINGS - PD-BIND-KEY DONE.")


;; ******************* C/C++ mode keys ********************
;; Create a keymap with Visual Studio compatible keymappings.
;; See http://ergoemacs.org/emacs/elisp_menu_for_major_mode.html
;; for what is going on here.
(defvar pd-vs-minor-mode-map nil "Keymap for Visual Studio compatibility.")

;; TODO: We should really setup the gud commands only in a gud-mode-hook.

(when (not pd-vs-minor-mode-map)
  (setq pd-vs-minor-mode-map (make-sparse-keymap))
  ;;  (define-key pd-vs-minor-mode-map (kbd "<f5>") 'gud-run) ; continue (gdb command = continue)
  ;;   C-F5 = run without debugging
  ;;   S-F5 = stop debugging
  ;;  CS-F5 = restart
  (define-key pd-vs-minor-mode-map (kbd "<f6>") 'pd-compile-without-confirmation)
  (define-key pd-vs-minor-mode-map (kbd "<S-f6>") 'pd-compile-clean-one-shot)
  (define-key pd-vs-minor-mode-map (kbd "<C-f6>") 'compile) ; make -k, the original compile command.
  (define-key pd-vs-minor-mode-map (kbd "<f7>") 'ff-find-other-file) ; View.ToggleDesigner in VS.
  (define-key pd-vs-minor-mode-map (kbd "<S-f7>") (lambda () (interactive) (ff-find-other-file t))) ; in a split window.
  (define-key pd-vs-minor-mode-map (kbd "<f8>") 'next-error)
  (define-key pd-vs-minor-mode-map (kbd "<S-f8>") 'previous-error)
  ;;(define-key pd-vs-minor-mode-map (kbd "<f9>") 'gud-break) ; toggle breakpoint (gdb command = break)
  ;;  CS-F9 = delete all breakpoints = typing d.
  ;; (define-key pd-vs-minor-mode-map (kbd "<f10>") 'gud-next)  ; step over (gdb command = next)
  ;; (define-key pd-vs-minor-mode-map (kbd "<C-f10>") 'gud-until)  ; run to cursor (gdb command = advance)
  ;; (define-key pd-vs-minor-mode-map (kbd "<CS-f10>") 'gud-jump)  ; set next statement (gdb command = jump)
  ;; (define-key pd-vs-minor-mode-map (kbd "<f11>") 'gud-step)  ; step in (gdb command = step)
  ;; (define-key pd-vs-minor-mode-map (kbd "<S-f11>") 'gud-finish)  ; step out (gdb command = finish)
  ;;    F12 = go to definition
  ;; "  C-Brk = cancel current build
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
;; 1) may seem simpler, but in fact when Emacs is started this way some (hard to
;; determine) functions, variables and settings do not work because there is no
;; frame or window system defined. This can make configuration difficult. Cursor
;; colors, fonts etc. do not get set when you expect them to be.
;; Therefore, I recommend method 2. This .emacs file has been designed with it
;; in mind. So, after I logon I just do "we &" to create a new graphical Win32
;; Emacs with an initial frame - which, typically, I never close. The call to
;; server-start turns this initial Emacs into a server.
;;
;; Other info: daemonp - predicate can be used to detect daemon mode.
(load "server")
(unless (server-running-p)
  (server-start))


(message ".EMACS.PD.EL - ALL DONE!")
