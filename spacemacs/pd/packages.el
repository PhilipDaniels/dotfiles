;;; See https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org#anatomy-of-a-layer

;;; It contains this list of packages of the layer and the actual configuration
;;; for the packages included in the layer. This file is loaded after layers.el.
;;; It must define a variable called <layer>-packages, which should be a list of
;;; all the packages that this layer needs.


;;; packages.el --- pd layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Philip Daniels <phil@homelaptop>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `pd-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `pd/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `pd/pre-init-PACKAGE' and/or
;;   `pd/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst pd-packages
  '(helm
    iy-go-to-char
    jump-char
    magit
    ssh-agency
    persistent-scratch
    recentf-ext
    (buffer-move :location local)
    (cycle-buffer :location local)
    mic-paren
    (pd-hydra :location local)
    )
  "The list of Lisp packages required by the pd layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun pd/post-init-helm ()
  "My helm customizations."
  (use-package helm
    :config
    (progn
      (add-to-list 'helm-mini-default-sources 'helm-source-files-in-current-dir 'append)
      (setq-default helm-buffer-max-length nil)
      (setq-default helm-ff-newfile-prompt-p nil)
      )))

(defun pd/init-jump-char ()
  (use-package jump-char))

(defun pd/init-iy-go-to-char ()
  (use-package iy-go-to-char))

(defun pd/post-init-magit ()
  "My git customizations."
  (use-package magit
    :config
    (progn
      ;; Make commits done from the command line also use the Magit COMMIT_MSG mode.
      (global-git-commit-mode)
      (setq magit-push-always-verify nil))))
    ;; (add-hook 'magit-diff-mode-hook 'pd-hide-dos-eol)
    ;; (add-hook 'git-timemachine-mode-hook 'pd-hide-dos-eol)
    ;; (add-hook 'magit-mode-popup-hook 'pd-turn-off-trailing-whitespace-display)

(defun pd/init-ssh-agency ()
  "My ssh-agency customizations."
  (use-package ssh-agency
    :config
    (progn
      ;; Configure Magit so that it will either use my existing ssh-agent, or
      ;; prompt for passwords if none is yet known. In Cygwin, this will require
      ;; X to be started. From
      ;; https://github.com/magit/magit/wiki/Pushing-with-Magit-from-Windows#openssh-passphrase-caching-via-ssh-agent

      ;; Prompt for HTTPS passwords if not cached.
      (setenv "GIT_ASKPASS" "git-gui--askpass")

      ;; Make ssh-agency prompt for my ssh passphrase using a graphical prompt
      ;; if not already cached. This should in fact not be necessary, because
      ;; ssh-agent should be started from my cygwin login script.
      (setenv "SSH_ASKPASS" "git-gui--askpass")
      ;; And when on cygwin, it needs the .exe extension to work.
      (when (eq system-type 'cygwin)
        (setq ssh-agency-add-executable "/bin/ssh-add.exe")
        (setq ssh-agency-agent-executable "/bin/ssh-agent.exe")))))

(defun pd/init-persistent-scratch ()
  "My persistent-scratch customizations.."
  (use-package persistent-scratch
    :config
    (persistent-scratch-setup-default)))

(defun pd/init-recentf-ext ()
  "My recentf-ext customizations. This makes recentf store
directories too. There is no customization per se."
  (use-package recentf-ext))

(defun pd/init-buffer-move ()
  (use-package buffer-move))

(defun pd/init-cycle-buffer ()
  (use-package cycle-buffer))

(defun pd/init-mic-paren ()
  "Customize mic-paren. The standard package highlight-parentheses is banned
in my .spacemacs file, I much prefer this."
  (use-package mic-paren
    :config
    (setq blink-matching-paren nil)
    (paren-activate)
    (setq paren-match-face 'mode-line)
  ))

(defun pd/init-pd-hydra ()
  (use-package pd-hydra))
