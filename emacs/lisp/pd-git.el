;;; Customization related to Git and Magit.
;;; Usage:  (require 'pd-git)


(require-package 'git-timemachine)
(require-package 'gitattributes-mode)
(require-package 'gitignore-mode)
(require-package 'magit)

(require 'pd)

;; Make commits done from the command line also use the Magit COMMIT_MSG mode.
(global-git-commit-mode)
(setq magit-push-always-verify nil)

(add-hook 'magit-diff-mode-hook 'pd-hide-dos-eol)
(add-hook 'git-timemachine-mode-hook 'pd-hide-dos-eol)
(add-hook 'magit-mode-popup-hook 'pd-turn-off-trailing-whitespace-display)

(provide 'pd-git)
