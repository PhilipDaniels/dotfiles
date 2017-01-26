;;; Customization related to the Helm package.
;;; Usage:  (require 'pd-helm)
;;;
;;; This should be loaded early because pd-theme likes to have the face
;;; available for the initial load of my chosen theme.

(require-package 'helm)
(require-package 'shackle)

(require 'helm)
(require 'helm-config)
(require 'helm-grep)
(require 'shackle)
(require 'helm-misc)


;; Helm mode. Based on http://tuhdo.github.io/helm-intro.html
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;; This works for helm-grep non-recursive.
(add-to-list 'helm-grep-ignored-files "*.dll")
(add-to-list 'helm-grep-ignored-files "*.exe")
(add-to-list 'helm-grep-ignored-files "*.obj")
(add-to-list 'helm-grep-ignored-files "*.pdb")
(add-to-list 'helm-grep-ignored-files "*.suo")
;; helm-grep-ignored-directories
;; And we have to repeat ourselves for recursive grep.
(require 'grep)
(add-to-list 'grep-find-ignored-files "*.dll")
(add-to-list 'grep-find-ignored-files "*.exe")
(add-to-list 'grep-find-ignored-files "*.obj")
(add-to-list 'grep-find-ignored-files "*.pdb")
(add-to-list 'grep-find-ignored-files "*.suo")
;; grep-find-ignored-directories

;; Add helm-source-files-in-current-dir as the lowest priority source.
(add-to-list 'helm-mini-default-sources 'helm-source-files-in-current-dir 'append)

(setq helm-semantic-fuzzy-match nil)
(setq helm-imenu-fuzzy-match nil)
(setq helm-M-x-fuzzy-match nil)
(setq helm-apropos-fuzzy-match nil)
(setq-default helm-buffer-max-length nil)
(setq-default helm-ff-newfile-prompt-p nil)

(defun pd-helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

(defun pd-helm-make-full-frame ()
  "Expands the helm window so that it takes up the full frame."
  (interactive)
  (with-selected-window (helm-window)
    (delete-other-windows)))

(helm-mode 1)



;; Use shackle to make helm always appear as a window at the bottom spanning
;; the full width of the screen. Also get rid of the shackle lighter, as it
;; used to use a UTF char 2603, Snowman, which does not appear in many fonts
;; and causes the modeline to grow in height. Actually, later versions of
;; shackle mode do not have a lighter, plus I actually use rich-minority
;; to turn off all minor mode lighters.
;; https://github.com/wasamasa/shackle
(shackle-mode 1)
(setq-default shackle-lighter "")
(setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align below :ratio 0.4)))


(pd-log-complete)
(provide 'pd-helm)
