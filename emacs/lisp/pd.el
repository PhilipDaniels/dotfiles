;;; My personal library of useful lisp functions.
;;; Usage: (require 'pd)

(defun pd-left-rotate (list)
  "Move the first element to the end of the list."
  (append (cdr list) (list (car list))))

(defun pd-right-rotate (list)
  "Move the last element to the front of the list."
  (append (last list) (butlast list)))

(defun pd-sort-lines (reverse beg end &optional cmp)
  "Sort lines in the region using the specified comparison function.

Pass non-nil for REVERSE to reverse the order, BEG and END are the two points
that specify the region to sort. CMP is a binary comparison predicate to be used
for ordering the lines, it will be passed two strings. You may pass nil, in
which case the function STRING< is used."
  (or cmp (setq cmp 'string<))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (sort-subr nil 'forward-line 'end-of-line nil nil
                 ;; We get passed a pair such as (700 . 800) for r1 and another for r2
                 (lambda (r1 r2)
                   (let (
                         (s1 (buffer-substring-no-properties (car r1) (cdr r1)))
                         (s2 (buffer-substring-no-properties (car r2) (cdr r2))))
                     ;; (message "Got s1=%s and s2=%s" s1 s2)
                     (funcall cmp s1 s2)))))))


(defun pd-utf8 ()
  "Inserts a utf-8 file coding marker."
  (interactive)
  (insert "-*- coding: utf-8 -*-"))

(defun pd-timestamp()
  "Inserts an Emacs timestamp at point."
  (interactive)
  (insert "Time-stamp: <>"))

(defun pd-copy-current-line ()
  "Copy the current line (including the newline character at the end)"
  (interactive)
  (kill-new (buffer-substring (point-at-bol) (+ (point-at-eol) 1))))

(defun pd-duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value.
From http://stackoverflow.com/questions/88399"
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(defun pd-back-to-indentation-or-beginning ()
  "Toggle between indentation and true beginning of line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun pd-rename-file-and-buffer (new-name)
  "Rename the current buffer and the file it's visiting to NEW-NAME."
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

(defun pd-delete-file-and-buffer ()
  "Delete the current buffer and its backing file."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun pd-replace-all-in-buffer ()
  "Replace all occurrences of a string in the buffer."
  (interactive)
  (save-excursion
    (if (equal mark-active nil) (mark-word))
    (let* ((curr-word (buffer-substring-no-properties (mark) (point)))
           (old-string (read-string "OLD string: " curr-word))
           (new-string (read-string "NEW string: " old-string)))
      (replace-string old-string new-string nil (point-min) (point-max)))))

(defun pd-join-line ()
  "Joins a line with the next line, leaving no space."
  (interactive)
  (join-line -1))

(defun pd-no-space ()
  "A version of just-one-space that leaves no spaces. If at the end
of a line, calls pd-join-line."
  (interactive)
  (if (eolp)
      (pd-join-line)
    (just-one-space 0)))

(defun pd-untabify-buffer ()
  "Run untabify on the entire buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun pd-indent-buffer ()
  "Run indent on the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun pd-group-number (num &optional size char)
  "Format NUM as string grouped to SIZE with CHAR."
  ;; Based on code for `math-group-float' in calc-ext.el
  (let* ((size (or size 3))
         (char (or char ","))
         (str (if (stringp num)
                  num
                (number-to-string num)))
         ;; omitting any trailing non-digit chars
         ;; NOTE: Calc supports BASE up to 36 (26 letters and 10 digits ;)
         (pt (or (string-match "[^0-9a-zA-Z]" str) (length str))))
    (while (> pt size)
      (setq str (concat (substring str 0 (- pt size))
                        char
                        (substring str (- pt size)))
            pt (- pt size)))
    str))

(defun pd-sort-paragraph-dwim (&optional special-c-sort)
  "Sorts the current paragraph and leaves point after the last line.

If a prefix argument is supplied, and if the paragraph starts
with '#include', then it is sorted specially to ensure that
system library includes such as #include <...> appear before
#include \"...\"."
  (interactive "P")
  (let* ((bounds (bounds-of-thing-at-point 'paragraph))
         (beg (car bounds))
         (end (cdr bounds)))
    (when special-c-sort
      (setq start (buffer-substring-no-properties (+ 1 beg) (+ beg 9)))
      (unless (string= start "#include")
        (setq special-c-sort nil)))
    (if special-c-sort
        (pd-sort-lines nil beg end 'pd-c-cmp-includes)
      (sort-lines nil beg end))
    (goto-char end)))

(defun pd-find-first-paragraph-starting (s)
  "Returns the (BEG . END) point of the first paragraph, if any, that starts
with the specified string S.

For some reason this function tends to return leading whitespace. I consider
this to be a bug."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((p (re-search-forward s nil t)))
      (when p
        (bounds-of-thing-at-point 'paragraph)))))

(defun pd-cleanup-programming-buffer ()
  "Reindent, untabify, delete trailing whitespace, sort c includes and C++
usings; recommended for programming modes only.

Also not recommended when working with other people's code because it will
re-indent the entire buffer."
  (interactive)
  (pd-indent-buffer)
  (pd-untabify-buffer)
  (delete-trailing-whitespace)
  (when (or (derived-mode-p 'c-mode) (derived-mode-p 'c++-mode))
    (pd-c-sort-includes))
  (when (derived-mode-p 'c++-mode)
    (pd-cpp-sort-usings))
  (message "Reindent, untabify, delete trailing whitespace."))

(defun pd-compile-without-confirmation ()
  "Runs last compilation without asking for confirmation."
  (interactive)
  (save-window-excursion
    (compile compile-command))
  (pop-to-buffer (get-buffer "*compilation*")))

(defun pd-compile-clean-one-shot ()
  "Runs make clean, but restores compile command after it."
  (interactive)
  (let (oldcc compile-command)
    (save-window-excursion
      (compile "make clean"))
    (pop-to-buffer (get-buffer "*compilation*"))
    (setq compile-command oldcc)))

(defun endless/comment-line-or-region (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))

(defun endless/forward-paragraph (&optional n)
  "Advance just past next blank line."
  (interactive "p")
  (let ((para-commands
         '(endless/forward-paragraph endless/backward-paragraph)))
    ;; Only push mark if it's not active and we're not repeating.
    (or (use-region-p)
        (not (member this-command para-commands))
        (member last-command para-commands)
        (push-mark))
    ;; The actual movement.
    (dotimes (_ (abs n))
      (if (> n 0)
          (skip-chars-forward "\n[:blank:]")
        (skip-chars-backward "\n[:blank:]"))
      (if (search-forward-regexp
           "\n[[:blank:]]*\n[[:blank:]]*" nil t (cl-signum n))
          (goto-char (match-end 0))
        (goto-char (if (> n 0) (point-max) (point-min)))))))

(defun endless/backward-paragraph (&optional n)
  "Go back up to previous blank line."
  (interactive "p")
  (endless/forward-paragraph (- n)))

(defun pd-revert-buffer ()
  "Reverts a buffer back to its last saved state."
  (interactive)
  (revert-buffer nil t))

(defun pd-hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  ;; From http://stackoverflow.com/questions/730751/hiding-m-in-emacs
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun pd-turn-off-trailing-whitespace-display ()
  "Turns off the buffer-local variable show-trailing-whitespace."
  (interactive)
  (set-variable 'show-trailing-whitespace nil))

(defun pd-get-full-path (relative-path)
  "Return the full path of relative-path, relative to caller's file location.

Example: If you have this line
 (pd-get-full-path \"../xyz.el\")
in the file at
 /home/jane/emacs/emacs_lib.el
then the return value is
 /home/jane/xyz.el
Regardless how or where emacs_lib.el is called.

A call (pd-get-full-path \"\") will get the directory of the
executing lisp file.

This function solves 2 problems.

If you have file A, that calls the `load' on a file at B, and B
calls `load' on file C using a relative path, then Emacs will
complain about unable to find C. Because, emacs does not switch
current directory with `load'.

To solve this problem, when your code only knows the relative
path of another file C, you can use the variable `load-file-name'
to get the current file's full path, then use that with the
relative path to get a full path of the file you are interested.

To know the current file's full path, emacs has 2 ways:
`load-file-name' and `buffer-file-name'. If the file is loaded by
`load', then `load-file-name' works but `buffer-file-name'
doesn't. If the file is called by `eval-buffer', then
`load-file-name' is nil. You want to be able to get the current
file's full path regardless the file is run by `load' or
interactively by `eval-buffer'."
  (expand-file-name "" (concat (file-name-directory (or load-file-name buffer-file-name)) relative-path)))

(pd-log-complete)
(provide 'pd)
