;;; -*- mode: emacs-lisp -*-
;;; -*- coding: utf-8 -*-
;;; Local Variables:
;;; End:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes t)
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; This file is just a shim which can be installed as ~/.emacs and allows the
;; Emacs customization system to write whatever it wants into this file without
;; screwing up the reset of my customizations, which I keep in a repo.

;; Initialize the package management system, then, after that is done,
;; load my personal customizations.
(load "~/repos/dotfiles/emacs/.emacs.packages.el")
