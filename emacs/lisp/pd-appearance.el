; -*- mode: emacs-lisp -*-
; -*- coding: utf-8 -*-

(provide 'pd-appearance) 

;;; Set default font. Based on http://ergoemacs.org/emacs/emacs_list_and_set_font.html
;;; and http://emacswiki.org/emacs/SetFonts
;(defvar pd-font-candidates '("Consolas-14" "Cousine-12")
;  "Defines a list of fonts to be tried in order."
;  )

;(require 'cl)
;(defun pd-get-first-existing-font (&rest fonts)
;  "Return the first font from FONTS which actually exists on this system."
;   (loop for font in fonts when (find-font (font-spec :name font)) return font))

;(apply 'pd-get-first-existing-font pd-font-candidates)


;(cond
; ((string-equal window-system "w32")
;  (when (member "Consolas" (font-family-list))
;    (add-to-list 'initial-frame-alist '(font . "Consolas-14"))
;    (add-to-list 'default-frame-alist '(font . "Consolas-14"))
;    )))

	

;;; There are two ways of loading themes in Emacs. The "built-in" way uses the
;;; load-theme function, and the other way uses the color-theme package. Prefer
;;; to use the built-in way. Many of my themes came from https://github.com/owainlewis/emacs-color-themes
;;; which has previews.
(add-to-list 'custom-theme-load-path "~/repos/dotfiles/emacs/themes")
(add-to-list 'custom-theme-load-path "~/repos/dotfiles/emacs/themes/emacs-color-theme-solarized")

(message "still running")
;; Setting the frame-background-mode before loading the theme stops Solarized
;; from initially loading in light mode.
(setq-default frame-background-mode 'dark)
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

