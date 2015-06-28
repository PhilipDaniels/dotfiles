; -*- mode: emacs-lisp -*-
; -*- coding: utf-8 -*-

(defconst running-ms-windows
  (string-match "windows" (prin1-to-string system-type)))
(defconst running-gnu-linux
  (string-match "linux" (prin1-to-string system-type)))
(defconst running-cygwin
  (string-match "cygwin" (prin1-to-string system-type)))
(defconst running-w32
  (string-match "w32" (prin1-to-string window-system)))

;;; TODO
;;; Something like Ctrl-P (wildfinder)
;;; Window keybindings
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


;;; Do my stuff last. This is particularly important with respect to the themes,
;;; because we must call custom-safe-themes before loading my themes, otherwise
;;; we get prompted every time.
(add-to-list 'load-path "~/repos/dotfiles/emacs/lisp")
(load-library "pd-lisputils")
(load-library "pd-functions")
(load-library "pd-appearance")
(load-library "pd-variables")
(load-library "pd-mode-customizations")
(load-library "pd-keys")

