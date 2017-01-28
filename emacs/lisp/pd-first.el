;;; File to be executed first when initializing Emacs.
;;; Usage:  (require 'pd-first)
;;;
;;; This file contains code that should be run early in the initialization
;;; process for Emacs.

(require-package 's) ;; string utility functions
(require 's)

(defvar pd-first-log-time 0 "The first time pd-log was called.")
(defvar pd-last-log-time 0 "The last time pd-log was called.")

(defun pd-log (message)
  "Logs a message with incremental time since last message and
source file name as a prefix."
  (interactive)
  (when (equal pd-first-log-time 0)
    (setq pd-last-log-time (float-time))
    (setq pd-first-log-time pd-last-log-time))
  (let ((cum-secs (- (float-time) pd-first-log-time))
        (inc-secs (- (float-time) pd-last-log-time)))
    (message "%9.5fs : %8.5fs inc. : %s : %s" cum-secs inc-secs (file-name-nondirectory load-file-name) message))
  (setq pd-last-log-time (float-time)))

(defun pd-log-requires-complete ()
  "Logs a 'Requires complete' message for a file."
  (interactive)
  (pd-log "Requires complete."))

(defun pd-log-loading-complete ()
  "Logs a 'Loading complete' message for a file."
  (interactive)
  (pd-log "Loading complete."))



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

(pd-log (format "Host = %s, pd-at-home = %s, system-type=%s, window-system=%s"
         system-name pd-at-home system-type window-system))


(pd-log-loading-complete)
(provide 'pd-first)
