; -*- mode: emacs-lisp -*-
; -*- coding: utf-8 -*-

(provide 'pd-appearance)

(require 'cl)

(defun pd-font-exists (font &optional frame)
  "Return a font if it exists, nil otherwise. Does not work in daemon mode."
  (find-font (font-spec :name font) frame))

(defvar pd-font-candidates '("Cousine-12" "Consolas-14" "Source Code Pro-12"
			     "Liberation Mono-12" "Anonymous Pro-14"
			     "Aurulent Sans Mono-12" "Calibri-12")
  "Defines a list of fonts to be tried in order.")

(defvar pd-font-index nil
  "Specifies the index of the candidate font that is currently selected.")

(defun pd-set-candidate-font (step &optional frame)
  "Scans forwards (if STEP is 1) or backwards (if STEP is -1) through the list
pd-font-candidates looking for a valid font. If STEP is 0, the current font is
reset to the first font. The first time this function is called it starts the
search at index 0."
  (interactive)
  (let ((num-fonts (length pd-font-candidates))
	(num-seen 0)
	(font-is-valid))
    ;; Loop around the array; num-seen is used to avoid looping forever if
    ;; all elements in pd-font-candidates are invalid.
    (while (and (< num-seen num-fonts) (not font-is-valid))
      (cond ((null pd-font-index) (setq pd-font-index 0))
	    ((= step 0) (setq pd-font-index 0 step 1))
	    ((< (+ pd-font-index step) 0) (setq pd-font-index (- num-fonts 1)))
	    ((>= (+ pd-font-index step) num-fonts) (setq pd-font-index 0))
	    (t (setq pd-font-index (+ pd-font-index step))))

      (setq next-font (nth pd-font-index pd-font-candidates)
	    num-seen (1+ num-seen)
	    font-is-valid (pd-font-exists next-font frame))

      (if font-is-valid
	  (progn
	    (message "Font set to %s" next-font)
	    (set-frame-font next-font t (list frame)))
	(message "Font %s does not exist, skipping" next-font)))
    (if (not font-is-valid)
	(message "No valid fonts found in candidates: %s" pd-font-candidates))
  ))

(define-key global-map (kbd "C-S-<prior>") (lambda () (interactive) (pd-set-candidate-font -1)))
(define-key global-map (kbd "C-S-<next>") (lambda () (interactive) (pd-set-candidate-font 1)))

(add-hook 'after-make-frame-functions (lambda (frame)
					(message "frame is %s" frame)
					(pd-set-candidate-font 0 frame)))


;; (let
;;     ((chosen-font (apply 'pd-get-first-existing-font pd-font-candidates)))
;;   (message "chosen-font = %s" chosen-font)
;;   (message "pd-fonts = %s" pd-font-candidates)
;;   (add-to-list 'initial-frame-alist `(font . ,chosen-font))
;;   (add-to-list 'default-frame-alist `(font . ,chosen-font)))



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
