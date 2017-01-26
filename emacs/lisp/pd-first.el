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

;; I actually like to have a menu bar. Modes like gg-tags make good use of it.
;; (when (fboundp 'menu-bar-mode)
;;  (menu-bar-mode -1))

(defvar pd-at-home t "t if this Emacs is being run at home, nil if at work.")

(if (s-starts-with-p "rd" system-name t)
    (setq pd-at-home nil))

(message "Host = %s, pd-at-home = %s, system-type=%s, window-system=%s"
         system-name pd-at-home system-type window-system)

(defvar pd-last-log-time 0 "The last time pd-log was called.")

(defun pd-log (message)
  "Logs a message with incremental time since last message and
source file name as a prefix."
  (interactive)
  (if (equal pd-last-log-time 0)
      (setq pd-last-log-time (float-time)))
  (let ((secs (- (float-time) pd-last-log-time)))
    (message "%8.5f secs : %s : %s" secs (file-name-nondirectory load-file-name) message)
        )
  (setq pd-last-log-time (float-time))
  )

(defun pd-log-complete ()
  "Logs a 'loading complete' message for a file."
  (interactive)
  (pd-log "Loading complete."))

(pd-log-complete)
(provide 'pd-first)
