; -*- mode: emacs-lisp -*-
; -*- coding: utf-8 -*-

;;; TODO
;;; Something like Ctrl-P (wildfinder)
;;; Window keybindings
;;; Themes - solarized, dark theme, light theme.
;;; mode line customisation - change color. OVER indicator
;;; Trailing whitespace in some filetypes only


;;; See http://www.emacswiki.org/emacs/ELPA
;;; But package management does not seem to work in Cygwin.
;;;(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;;			 ("marmalade" . "https://marmalade-repo.org/packages/")
;;;			 ("melpa" . "http://melpa.org/packages/")))

;;; The above won't work from work due to proxy issues. Suggested method of
;;; getting Emacs to use the IE proxy settings is below.
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

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;; Do my stuff last. This is particularly important with respect to the themes,
;;; because we must call custom-safe-themes before loading my themes, otherwise
;;; we get prompted every time.
(load-library "pd-appearance")
(load-library "pd-variables")
(load-library "pd-keys")


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight normal :height 106 :width normal)))))
