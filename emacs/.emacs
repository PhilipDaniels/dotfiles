; -*- mode: emacs-lisp -*-
; -*- coding: utf-8 -*-

; See http://www.emacswiki.org/emacs/ELPA
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/"))
;			 ("marmalade" . "https://marmalade-repo.org/packages/")
;			 ("melpa" . "http://melpa.org/packages/")))

(add-to-list 'load-path "~/repos/dotfiles/emacs/lisp")

(load-library "pd-appearance")
(load-library "pd-variables")
(load-library "pd-keys")

; Something like Ctrl-P (wildfinder)
; Window keybindings
; Themes - solarized, dark theme, light theme.
; mode line customisation - change color. OVER indicator
; Trailing whitespace in some filetypes only
; 80 column marker

