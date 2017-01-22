;;; Customization related to Git and Magit.
;;; Usage:  (require 'pd-git)

(require-package 'git-timemachine)
(require-package 'gitattributes-mode)
(require-package 'gitignore-mode)
(require-package 'magit)
(require-package 'ssh-agency)

(require 'pd)

;; Make commits done from the command line also use the Magit COMMIT_MSG mode.
(global-git-commit-mode)
(setq magit-push-always-verify nil)

(add-hook 'magit-diff-mode-hook 'pd-hide-dos-eol)
(add-hook 'git-timemachine-mode-hook 'pd-hide-dos-eol)
(add-hook 'magit-mode-popup-hook 'pd-turn-off-trailing-whitespace-display)

;; Configure Magit so that it will either use my existing ssh-agent, or
;; prompt for passwords if none is yet known. In Cygwin, this will require
;; X to be started.
;; From https://github.com/magit/magit/wiki/Pushing-with-Magit-from-Windows#openssh-passphrase-caching-via-ssh-agent
(when (eq system-type 'cygwin)
  (setq ssh-agency-add-executable "/bin/ssh-add.exe")
  (setq ssh-agency-agent-executable "/bin/ssh-agent.exe"))

;; Prompt for HTTPS passwords if not cached.
(setenv "GIT_ASKPASS" "git-gui--askpass")

;; Make ssh-ahency prompt for my ssh passphrase using a graphical prompt.
;; This should in fact not be necessary, because ssh-agent should be
;; started from my cygwin login script.
(setenv "SSH_ASKPASS" "git-gui--askpass")


(provide 'pd-git)
