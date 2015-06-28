; -*- mode: emacs-lisp -*-
; -*- coding: utf-8 -*-

(provide 'pd-functions)

(defun pd-utf8 ()
  "Inserts a utf-8 file coding marker."
  (interactive)
  (insert "-*- coding: utf-8 -*-")
  )

(defun pd-timestamp()
  "Inserts an Emacs timestamp at point."
  (interactive)
  (insert "Time-stamp: <>")
  )

(defun pd-copy-current-line ()
  "Copy current line (including the newline character at the end)"
  (interactive)
  (kill-new (buffer-substring (point-at-bol) (+ (point-at-eol) 1))))

(defun pd-back-to-indentation-or-beginning ()
  "Toggle between indentation and true beginning of line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun pd-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(progn
	  (rename-file name new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil))))))

