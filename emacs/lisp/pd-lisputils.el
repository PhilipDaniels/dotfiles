; -*- mode: emacs-lisp -*-
; -*- coding: utf-8 -*-

(provide 'pd-lisputils)

(defun pd-left-rotate (list)
  "Move the first element to the end of the list."
  (append (cdr list) (list (car list))))

(defun pd-right-rotate (list)
  "Move the last element to the front of the list."
  (append (last list) (butlast list)))


