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

;;; Configure the package management system.
(setq package-list '(
		     csharp-mode
		     company                ;; Modular in-buffer completion framework
		     company-irony          ;; Completion back-end for irony-mode
		     expand-region
		     f                      ;; file system manipulation
		     fill-column-indicator
		     fsharp-mode
		     ggtags                 ;; interface to GNU Global tag system
		     golden-ratio
		     gitattributes-mode
		     gitignore-mode
		     git-timemachine
		     haskell-mode
		     helm
		     helm-gtags             ;; Helm interface to GNU Global tag system
		     helm-projectile
		     hlinum
		     hydra
		     irony                  ;; A C/C++ minor mode powered by libclang
		     jade-mode
		     json-mode
		     key-chord
		     lua-mode
		     magit
		     markdown-mode
		     multi-term
		     outline-magic
		     powershell             ;; Run Powershell as an inferior process.
		     projectile
		     rainbow-delimiters
		     rainbow-mode
		     s                      ;; string utility functions
		     sass-mode
		     sr-speedbar            ;; in-frame directory tree
		     sunrise-commander
		     sunrise-x-buttons
		     unbound
		     yaml-mode
		     yasnippet
		     ;; Themes.
		     afternoon-theme
		     alect-themes
		     ample-theme
		     ample-zen-theme
		     anti-zenburn-theme
		     badger-theme
		     bubbleberry-theme
		     busybee-theme
		     cherry-blossom-theme
		     clues-theme
		     color-theme-monokai
		     color-theme-sanityinc-tomorrow
		     color-theme-solarized
		     color-theme-wombat
		     cyberpunk-theme
		     dakrone-theme
		     darkburn-theme
		     distinguished-theme
		     espresso-theme
		     firebelly-theme
		     flatland-theme
		     flatui-theme
		     grandshell-theme
		     gruber-darker-theme
		     gruvbox-theme
		     hc-zenburn-theme
		     hemisu-theme
		     heroku-theme
		     leuven-theme
		     moe-theme
		     occidental-theme
		     soothe-theme
		     tango-plus-theme
		     tangotango-theme
		     zenburn-theme
		     ))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
                         ("sc" . "http://joseito.republika.pl/sunrise-commander/")
			 ))

(package-initialize)

; Fetch the list of packages available.
(unless package-archive-contents
  (package-refresh-contents))

; Install the missing packages.
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


; Do my real initialization, safe in the knowledge that everything is loaded.
(setq enable-local-variables t)
(setq enable-local-eval t)
(add-hook 'after-init-hook
	  (lambda () (load "~/repos/dotfiles/emacs/emacs.pd.el")))
