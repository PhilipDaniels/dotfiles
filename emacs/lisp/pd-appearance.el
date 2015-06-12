; -*- mode: emacs-lisp -*-
; -*- coding: utf-8 -*-

(provide 'pd-appearance)

(load-theme 'tango-dark)
;(add-to-list 'default-frame-alist '(height . 50))
;(add-to-list 'default-frame-alist '(width . 86))
(setq ring-bell-function nil)
(setq visible-bell 1)
(blink-cursor-mode -1)
(global-linum-mode 1)
(setq column-number-mode 1)
(setq line-number-mode 1)
;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

