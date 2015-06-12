; -*- mode: emacs-lisp -*-
; -*- coding: utf-8 -*-

; See http://www.emacswiki.org/emacs/ELPA
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

; The above won't work from work due to proxy issues. Suggested method of getting
; Emacs to use the IE proxy settings is below.
(eval-after-load "url"
  '(progn
     (require 'w32-registry)
     (defadvice url-retrieve (before
			      w32-set-proxy-dynamically
			      activate)
       "Before retrieving a URL, query the IE Proxy settings, and use them."
       (let ((proxy (w32reg-get-ie-proxy-config)))
	 (setq url-using-proxy proxy url-proxy-services proxy)))))


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
