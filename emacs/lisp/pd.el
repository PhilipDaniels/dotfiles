;;; pd.el --- My personal library of useful lisp functions.

;; Author:     Philip Daniels <philip.daniels1971@gmail.com>
;; Maintainer: same
;; Created: 09 Feb 2016
;; Keywords: C++, using, #insert, STL

;;; Commentary:

;; Description:
;; ------------

;; pd-cpp provides functions that enhance the experience of coding C++. The main
;; feature is the ability to automatically look up the standard header file that
;; a symbol resides in and add a #include<...> statement at the top of the file.
;; Furthermore, a 'using' statement will be added, but only in .cpp files
;; because it is bad practice to do so in header files.

;; Installation:
;; -------------
;; Add these lines in your .emacs:
;;   (require 'pd-cpp)

;; TODO
;; ----

;; ChangeLog
;; ---------

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

(provide 'pd)

;;; end of pd.el
