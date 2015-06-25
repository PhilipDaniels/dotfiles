; -*- mode: emacs-lisp -*-
; -*- coding: utf-8 -*-

(provide 'pd-appearance) 

(require 'cl)

(defun pd-font-exists (font)
  "Return a font if it exists, nil otherwise. Does not work in daemon mode."
  (find-font (font-spec :name font)))


(setq x-font-candidates '("Consolas-14" "blah" "Cousine-12"))
;  "Defines a list of fonts to be tried in order.")

(defun pd-set-next-font ()
  "Selects the next available font from the list of candidates."
  ;; We try the head of the list the next time we are called.
  ;; num-seen is used to avoid looping forever if there are no valid candidates.
  (let (
	(num-fonts (length x-font-candidates))
	(num-seen 0)
	(next-font-is-valid nil)
	)
    (while (and (< num-seen num-fonts) (not next-font-is-valid))
      ; Get head of list and rotate the list
      (setq x-next-font (pop x-font-candidates))
      (setq x-font-candidates (append x-font-candidates (list x-next-font)))
      (setq num-seen (1+ num-seen))
      ; Is it valid?
      (setq next-font-is-valid (pd-font-exists x-next-font))
      (if next-font-is-valid
	  (message "setting font to %s" x-next-font)
	(message "font %s does not exist, skipping" x-next-font)))
    (if (not next-font-is-valid)
	(message "No valid fonts found in candidates: %s" x-font-candidates))))



(defvar pd-font-candidates '("Consolas-14" "Cousine-12")
  "Defines a list of fonts to be tried in order.")

;; Make font setup work in daemon mode.
;;(create-fontset-from-fontset-spec standard-fontset-spec)
;;(dolist (font (reverse pd-font-candidates))
;;  (set-fontset-font "fontset-standard" 'unicode font nil 'prepend))

(defun pd-get-first-existing-font (&rest fonts)
  "Return the first font from FONTS which actually exists on this system."
  (loop for font in fonts when (find-font (font-spec :name font)) return font))

(let
    ((chosen-font (apply 'pd-get-first-existing-font pd-font-candidates)))
  (message "chosen-font = %s" chosen-font)
  (message "pd-fonts = %s" pd-font-candidates)
  (add-to-list 'initial-frame-alist `(font . ,chosen-font))
  (add-to-list 'default-frame-alist `(font . ,chosen-font)))



;;; There are two ways of loading themes in Emacs. The "built-in" way uses the
;;; load-theme function, and the other way uses the color-theme package. Prefer
;;; to use the built-in way. Many of my themes came from
;;; https://github.com/owainlewis/emacs-color-themes which has previews.
(add-to-list 'custom-theme-load-path "~/repos/dotfiles/emacs/themes")
(add-to-list 'custom-theme-load-path "~/repos/dotfiles/emacs/themes/emacs-color-theme-solarized")

;; Setting the frame-background-mode before loading the theme stops Solarized
;; from initially loading in light mode. The mapc is needed for w32 emacs, or
;; else we still come up in light mode, no idea why.
(setq-default frame-background-mode 'dark)
(mapc 'frame-set-background-mode (frame-list))
(load-theme 'solarized t)       ; sellout Solarized
;;(load-theme 'solarized-dark t)  ; bbatsov Solarized, no good in terminal

;; You can load a different theme for GUI vs Terminal like this.
;; Decent terminal themes: manoj-dark, tango-dark, misterioso, tsdh-dark, wheatgrass
;;(if (display-graphic-p)
;;    (load-theme 'solarized-dark t)
;;  (load-theme 'tango-dark t))


;; These colors are from solarized.
(set-face-foreground 'mode-line "#268bd2")
(set-face-background 'mode-line "#eee8d5")
(set-face-foreground 'mode-line-inactive "#eee8d5")
(set-face-background 'mode-line-inactive "#268bd2")

;;;(add-to-list 'default-frame-alist '(height . 50))
;;;(add-to-list 'default-frame-alist '(width . 86))
;;;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;;(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq ring-bell-function nil)
(setq visible-bell 1)
(blink-cursor-mode -1)
(show-paren-mode 1)
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
(add-hook 'shell-script-mode-hook 'fci-mode)

;; The show-trailing-whitespace mode is incompatible with FCI. This workaround
;; is from the FCI documentation.
;;(whitespace-mode nil)
;;(setq-default show-trailing-whitespace t)
;;(setq whitespace-style '(face trailing))

