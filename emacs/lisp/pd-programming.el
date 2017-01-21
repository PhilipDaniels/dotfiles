;;; Customizations related to programming modes.
;;; Usage:  (require 'pd-programming)

(require-package 'csharp-mode)
(require-package 'fsharp-mode)
(require-package 'ggtags)
(require-package 'haskell-mode)
(require-package 'jade-mode)
(require-package 'json-mode)
(require-package 'lua-mode)
(require-package 'powershell)          ;; Run powershell as an inferior process.
(require-package 'sass-mode)
(require-package 'yaml-mode)

(require 'pd-cpp)

;; The style I want to use in c++ mode.
(c-add-style "pd-style"
             '("k&r"
               (indent-tabs-mode . nil)        ; use spaces rather than tabs
               (c-basic-offset . 4)            ; indent by four spaces
               (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (brace-list-open . 0)
                                   (statement-case-open . 0)
                                   (arglist-intro . 0)
                                   ))))

(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "pd-style")
            (pd-cpp-auto-mode)
            ))

(add-hook 'before-save-hook 'pd-cpp-cleanup-buffer-maybe)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

(when (eq system-type 'cygwin)
  (setq powershell-location-of-exe
        (s-trim (shell-command-to-string "which powershell.exe"))))


;; Automatically cleanup files before save in programming modes. Based on
;; http://stackoverflow.com/questions/19174302. An alternative, using ws-trim or
;; ws-butler, is mentioned there which can reduce git commit noise, but since
;; I only use Emacs for my own repos at the moment it doesn't matter.
;; (add-hook 'before-save-hook (lambda()
;;                               (when (derived-mode-p 'prog-mode)
;;                                 (pd-cleanup-programming-buffer))))

(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'csharp-mode-hook 'hs-minor-mode)
(add-hook 'sh-mode-hook 'hs-minor-mode)


;; Turn on rainbow delimiters in all programming modes.
(require-package 'rainbow-delimiters)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


(setq comment-empty-lines t)
(setq compilation-ask-about-save nil)
(setq compilation-scroll-output 'first-error)
(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq vc-follow-symlinks t)

;; (semantic-mode 1)


(provide 'pd-programming)
