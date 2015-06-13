; -*- mode: emacs-lisp -*-
; -*- coding: utf-8 -*-

(provide 'pd-appearance)

(add-to-list 'load-path "~/repos/dotfiles/emacs/lisp/themes/emacs-color-theme-solarized")
(require 'color-theme)
(require 'color-theme-solarized)
(color-theme-solarized)

;(load-theme 'tango-dark)


;(add-to-list 'default-frame-alist '(height . 50))
;(add-to-list 'default-frame-alist '(width . 86))
;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq ring-bell-function nil)
(setq visible-bell 1)
(blink-cursor-mode -1)
(show-paren-mode 1)
(setq-default show-trailing-whitespace t)
(global-linum-mode 1)
(setq linum-format "%4d ")
(setq column-number-mode 1)
(setq line-number-mode 1)
(display-time-mode 1)
(size-indication-mode 1)

(require 'fill-column-indicator)
(setq fci-rule-width 2)
(setq fci-rule-color "DodgerBlue1")
(add-hook 'c-mode-common-hook 'fci-mode)
(add-hook 'emacs-lisp-mode-hook 'fci-mode)

