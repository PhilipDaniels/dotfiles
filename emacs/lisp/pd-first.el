;;; File to be executed first when initializing Emacs.
;;; Usage:  (require 'pd-first)
;;;
;;; This file contains code that should be run early in the initialization
;;; process for Emacs.

(require-package 's) ;; string utility functions
(require 's)


;; Turn these off early so that there is no visual flashing.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(defvar pd-at-home t "t if this Emacs is being run at home, nil if at work.")

(if (s-starts-with-p "rd" system-name t)
    (setq pd-at-home nil))

(message "Host = %s, pd-at-home = %s, system-type=%s, window-system=%s"
         system-name pd-at-home system-type window-system)


(provide 'pd-first)
