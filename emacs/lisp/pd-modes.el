;;; Customizations related to modes.
;;; Usage: (require 'pd-modes)

(require-package 'markdown-mode)
(require-package 'unbound)          ;; Unbound provides the command describe-unbound-keys. Try a parameter of 8.
(require-package 'recentf-ext)
(require-package 'persistent-scratch)
(require 'which-func)
(require 'winner)
(require 'recentf-ext)
(pd-log-requires-complete)


;; Tramp mode.
(setq tramp-default-method "sshx")
(setq tramp-default-user "phil")

;; Markdown mode.
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

;; Dired mode.
(setq dired-listing-switches "-laGh1v --group-directories-first")
(setq dired-dwim-target t)
;; Stop dired from opening lots of new buffers when you press RET to edit a file.
;; (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
;; http://oremacs.com/2015/01/13/dired-options/
;; http://oremacs.com/2015/01/10/dired-ansi-term/

;; Which-function mode.
;; Slow and pointless and some modes have a nasty habit of enabling it,
;; such as Powershell mode. Together, these two lines disable it.
(which-function-mode -1)
(setq which-func-modes nil)

;; Recentf mode. We also use the package recentf-ext which makes recentf mode
;; store directories too.
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 100)

;; Turn off the scratch message, but leave *scratch* as the initial buffer.
;; And finally make the *scratch* buffer persistent.
(setq initial-major-mode 'emacs-lisp-mode)
(setq initial-scratch-message nil)
(persistent-scratch-setup-default)

(winner-mode 1)


(pd-log-loading-complete)
(provide 'pd-modes)
