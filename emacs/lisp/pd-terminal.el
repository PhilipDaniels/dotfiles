;;; Ansi-term configuration.
;;; Usage:  (require 'pd-terminal)
;;;
;;; Notes on the different shells.
;;; M-x shell runs a shell as a sub-process, communicating with it via pipes.
;;; It is not fully functional.
;;; eshell is a shell written in elisp. It is slow and not very complete.
;;; term and ansi-term (better) are better choices.
;;; There is also multi-term, not sure what that adds though.
;;; tl;dr - use ansi-term for starting shells.


(defun pd-get-or-create-ansi-term (bufname)
  "Switch to an ansi-term buffer named BUFNAME if it exists, or
create one if no such buffer does not exist."
  (interactive)
  (let ((dbn (concat "*" bufname "*")))
    (if (get-buffer dbn)
        (switch-to-buffer dbn)
      (ansi-term "/bin/bash" bufname))
  (get-buffer-process dbn)))

(defun pd-ansi-term ()
  "Create or switch to an ansi-term buffer with an initial
working directory that is the same as the current buffer's
DEFAULT-DIRECTORY. This is a quick way to bring up a terminal in
the same directory as the buffer.

The new ansi-term buffer will have a name that matches the
DEFAULT-DIRECTORY. If such a buffer already exists, it is
switched to rather than created.

If the current buffer is a remote buffer then an 'ssh' command
followed by a 'cd' command will be sent to the new ansi-term to
leave you in the correct directory.

Based on http://oremacs.com/2015/01/10/dired-ansi-term/

TODO
1) Make ansi-term mode update the buffer name when you change
directory. At the moment the buffer keeps its original name,
which means there is a chance this function will give you a
buffer that is not actually in that directory.

2) If called when already in an ansi-term, generate another term
with a unique buffer name. See
https://ftp.gnu.org/old-gnu/Manuals/elisp-manual-21-2.8/html_chapter/elisp_27.html
"
  (interactive)
  (if (not (file-remote-p default-directory))
      (pd-get-or-create-ansi-term default-directory)
    ;;(message "Remote file %s" default-directory)
    (let* ((parts (tramp-dissect-file-name default-directory t))
           (uid (aref parts 1))
           (host (aref parts 2))
           (dir (aref parts 3))
           (cmd (format "ssh %s@%s\ncd '%s'\n" uid host dir))
           (bname (format "ansi-term:%s" default-directory))
          )
      (term-send-string
       (pd-get-or-create-ansi-term bname)
       cmd)
     )))

(defun pd-term-dont-prompt-on-exit ()
  "Stop Emacs prompting with 'Active processes exist, kill them?' when exiting Emacs.
From http://stackoverflow.com/questions/2706527/make-emacs-stop-asking-active-processes-exist-kill-them-and-exit-anyway"
  (set-process-query-on-exit-flag
   (get-buffer-process (current-buffer)) nil))
  
(add-hook 'comint-exec-hook 'pd-term-dont-prompt-on-exit)
(add-hook 'term-exec-hook 'pd-term-dont-prompt-on-exit)

(defun pd-term-exec-hook ()
  "Make exiting ansi-term kill the buffer.
Originally called oleh-term-exec-hook"
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'pd-term-exec-hook)

;; Don't prompt about killing buffers with live processes attached to them.
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Make ansi-term stop prompting and always use bash.
;; From http://echosa.github.io/blog/2012/06/06/improving-ansi-term/
(defvar pd-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list pd-term-shell)))
(ad-activate 'ansi-term)


(provide 'pd-terminal)
