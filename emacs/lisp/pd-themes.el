;;; The collection of themes I like to install.
;;; These are used in pd-hydras, there is a hydra to switch themes.
;;; Usage:  (require 'pd-themes)

(require-package 'afternoon-theme)
(require-package 'alect-themes)
(require-package 'ample-theme)
(require-package 'ample-zen-theme)
(require-package 'anti-zenburn-theme)
;; (require-package 'badger-theme) Compilation error.
(require-package 'bubbleberry-theme)
(require-package 'busybee-theme)
(require-package 'cherry-blossom-theme)
(require-package 'clues-theme)
(require-package 'color-theme-modern)
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'color-theme-solarized)  ;; https://github.com/sellout/emacs-color-theme-solarized
(require-package 'cyberpunk-theme)
(require-package 'dakrone-theme)
(require-package 'darkburn-theme)
(require-package 'distinguished-theme)
(require-package 'espresso-theme)
(require-package 'firebelly-theme)
(require-package 'flatland-theme)
(require-package 'flatui-theme)
(require-package 'grandshell-theme)
(require-package 'green-phosphor-theme)
(require-package 'gruber-darker-theme)
(require-package 'gruvbox-theme)
(require-package 'hc-zenburn-theme)
(require-package 'hemisu-theme)
(require-package 'heroku-theme)
(require-package 'leuven-theme)
(require-package 'material-theme)
(require-package 'minimal-theme)
;; (require-package 'moe-theme)  Bad! Just requiring it changes Emacs.
(require-package 'molokai-theme)
(require-package 'monokai-theme)
(require-package 'monochrome-theme)
(require-package 'obsidian-theme)
(require-package 'occidental-theme)
(require-package 'soft-morning-theme)
(require-package 'solarized-theme)       ;; https://github.com/bbatsov/solarized-emacs
(require-package 'soothe-theme)
(require-package 'tango-plus-theme)
(require-package 'tangotango-theme)
(require-package 'underwater-theme)
(require-package 'zenburn-theme)


(defvar pd-theme-current nil "The theme that was last loaded by pd-theme-load.")

(defun pd-theme-apply-overrides ()
  "Apply my theme overrides."
  ;; Post-theme customizations that I apply to everything.
  ;; Some things *should* be prominent.
  ;; In terminal Emacs, these color names appear to map to the corresponding
  ;; solarized colors automatically (see list-colors-display).
  (set-cursor-color "red")
  (if (facep 'helm-selection)
      (set-face-attribute 'helm-selection         nil :background "red"   :foreground "white" :inverse-video nil))
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

(defun pd-theme-load (theme &optional bg-mode)
  "Load a theme, disabling the current custom theme first.
Normally themes 'accumulate' as you load them which gives a very
confusing experience. This function prevents strange display and
performance problems by disabling the current active custom
theme (if any) before loading the new theme.

This function can only do its job properly if you do all theme
loading using it.

BG-MODE is used to set the variable `frame-background-mode'.
Valid values are nil, 'dark and 'light."
  (if pd-theme-current (disable-theme pd-theme-current))
  (setq pd-theme-current theme)
  ;; Solarized (the only one I am sure about) uses the frame-background-mode to
  ;; determine how to display itself. The default for this variable is nil,
  ;; which most themes seem happy with.
  (setq frame-background-mode bg-mode)
  (mapc 'frame-set-background-mode (frame-list))
  (load-theme theme t)
  (message "Theme set to %s" theme)
  (pd-theme-apply-overrides))

;; This is package "color-theme-solarized" from https://github.com/sellout/emacs-color-theme-solarized
(pd-theme-load 'solarized 'dark)
;; (pd-theme-load 'gruvbox)

(provide 'pd-themes)
