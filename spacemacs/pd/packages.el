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
  '(helm magit)
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

(setq pd-dir (pd-get-full-path ""))


(defun pd/post-init-helm ()
  "My helm customizations."
  (eval-after-load 'helm
    ;; Make the selection line really prominent.
    '(if (facep 'helm-selection)
      (set-face-attribute 'helm-selection nil :background "red" :foreground "white" :inverse-video nil)))
  )

(defun pd/post-init-magit ()
  "My git customizations."
  (eval-after-load 'magit
    ;; Make commits done from the command line also use the Magit COMMIT_MSG mode.
    '(progn (
             (global-git-commit-mode t)
             (setq magit-push-always-verify nil)))
    ;; (add-hook 'magit-diff-mode-hook 'pd-hide-dos-eol)
    ;; (add-hook 'git-timemachine-mode-hook 'pd-hide-dos-eol)
    ;; (add-hook 'magit-mode-popup-hook 'pd-turn-off-trailing-whitespace-display)
    )
  )


;; (defun pd/init-ssh-agency ()
;;   "My ssh-agency customizations."
;;   ;(use-package ssh-agency
;;   ;  :defer t
;;   ;  )

;;   (eval-after-load 'ssh-agency
;;     ;; Configure Magit so that it will either use my existing ssh-agent, or
;;     ;; prompt for passwords if none is yet known. In Cygwin, this will require
;;     ;; X to be started.
;;     ;; From https://github.com/magit/magit/wiki/Pushing-with-Magit-from-Windows#openssh-passphrase-caching-via-ssh-agent
;;     ;; Prompt for HTTPS passwords if not cached.
;;     (setenv "GIT_ASKPASS" "git gui--askpass")
;;     ;; Make ssh-ahency prompt for my ssh passphrase using a graphical prompt.
;;     ;; This should in fact not be necessary, because ssh-agent should be started
;;     ;; from my cygwin login script.
;;     (setenv "SSH_ASKPASS" "git gui--askpass")
;;     (when (eq system-type 'cygwin)
;;       (setq ssh-agency-add-executable "/bin/ssh-add.exe")
;;       (setq ssh-agency-agent-executable "/bin/ssh-agent.exe"))
;;     )
;;   )

(defun pd-snippets-initialize ()
  "My yasnippet customizations. Adds my personal snippet dir to
the head of the list, so that they have priority."
  ;; See http://capitaomorte.github.io/yasnippet/
  (let ((snip-dir (expand-file-name "snippets" pd-dir)))
    (add-to-list 'yas-snippet-dirs snip-dir)
    (yas-load-directory snip-dir)))

(with-eval-after-load 'yasnippet (pd-snippets-initialize))
