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
  '(helm)
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

(setq pd-dir (spacemacs//get-package-directory 'pd))

(defun pd/post-init-helm ()
  "My helm customizations."
  (eval-after-load 'helm
    ;; Make the selection line really prominent.
    '(if (facep 'helm-selection)
      (set-face-attribute 'helm-selection nil :background "red" :foreground "white" :inverse-video nil)))
  )

(defun pd-snippets-initialize ()
  "My yasnippet customizations. Adds my personal snippet dir to
the head of the list, so that they have priority."
  ;; See http://capitaomorte.github.io/yasnippet/
  (let ((snip-dir (expand-file-name "snippets" pd-dir)))
    (add-to-list 'yas-snippet-dirs snip-dir)
    (yas-load-directory snip-dir)))

(with-eval-after-load 'yasnippet (pd-snippets-initialize))