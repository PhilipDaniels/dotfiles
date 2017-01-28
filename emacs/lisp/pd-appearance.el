;;; Customizations related to appearance.
;;; Usage: (require 'pd-appearance)

(require-package 'rich-minority)

(require 'whitespace)
(require 'windmove)
(require 'pd)
(require 'pd-theme)
(pd-log-requires-complete)

(setq ring-bell-function nil)
(setq visible-bell 1)
(setq column-number-mode 1)
(setq line-number-mode 1)
(setq blink-cursor-blinks 0)
(setq windmove-wrap-around t)
(size-indication-mode 1)
(blink-cursor-mode 0)

;; This controls all frames that will be created.
;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Frame-Parameters.html
(setq default-frame-alist
      '((top . 0)
        (left . 0)
        (width . 110)
        (height . 68)
        (auto-raise . t)
        (cursor-color . "red")
        ))

;; This ensures the right font is set when running in daemon mode, but it is
;; no longer required since my preferred method is now to start a normal Emacs
;; window upon login and turn that into a daemon - see the bottom of this file.
;;(add-hook 'after-make-frame-functions
;;        (lambda (frame) (pd-set-candidate-font 0 frame t)))
;; And this ensures the right font is set when running in non-daemon mode.
;; (if (display-graphic-p)
;;    (pd-set-candidate-font 0 (selected-frame) t))


;; Some example frame titles. See Emacs wiki.
;; (setq frame-title-format (list "\u25b6 %f \u25c0 " user-login-name "@" system-name))
;; (setq frame-title-format (list "\u27a4 %f    \u27a4 " user-login-name "@" system-name))
;; (setq frame-title-format (list "\u2b24 %f    \u2b24 " user-login-name "@" system-name))
(setq frame-title-format (list user-login-name "@" system-name ":%f"))


;; Show a red rectangle for trailing whitespace, and color long lines.
(setq-default show-trailing-whitespace t)
(setq-default whitespace-line-column 80)
(setq-default whitespace-style '(face trailing lines-tail))

;; Turn off display of trailing whitespace in some modes.
(dolist (hook '(buffer-menu-mode-hook compilation-mode-hook
                diff-mode-hook shell-mode-hook term-mode-hook
                calendar-mode-hook))
  (add-hook hook 'pd-turn-off-trailing-whitespace-display))

;; We need to turn on whitespace-mode to get the display of the >80 character lines working.
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Ensure that ^M characters never appear in text modes.
(add-hook 'text-mode-hook 'pd-hide-dos-eol)

;; Use richminority-mode to eliminate all lighter strings from the modeline,
;; to save space.
(setq rm-blacklist ".*")         ;; List of lighter strings or simply ".*"
(rich-minority-mode 1)

(setq scroll-margin 3)
(setq scroll-conservatively 10000)
(setq scroll-error-top-bottom t)
(setq scroll-preserve-screen-position t)
(setq inhibit-startup-message t)
(setq-default fill-column 80)

;; Setup for mic-paren mode.
;; Use list-faces-display to pick a suitable face.
(require-package 'mic-paren)
(require 'mic-paren)
(setq blink-matching-paren nil)
(paren-activate)
(setq paren-match-face 'mode-line)

(pd-log-loading-complete)
(provide 'pd-appearance)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; (setq x-gtk-use-system-tooltips nil)
;; (setq custom-theme-directory "~/repos/dotfiles/emacs/themes")

;; http://stackoverflow.com/questions/13625080/looking-forward-a-way-to-make-cursor-blinks-like-a-heartbeat-in-emacs
;;(setq-default blink-cursor-alist '((t . (hbar . 5))))
;;(setq-default blink-cursor-alist '((t . nil)))
;; (global-hl-line-mode 0)

;; This face is used to highlight the selected thing (e.g. function in source
;; file). Box is on by default, which causes a temporary line-height increase
;; which is visually irritating.
;; (set-face-attribute 'speedbar-highlight-face nil :box nil :background "black")
;; (setq-default sr-speedbar-right-side t)
