;;; -*- mode: emacs-lisp -*-
;;; -*- coding: utf-8 -*-
;;; Local Variables:
;;; End:

;;; Specify list of packages we want to install.
(setq package-list '(
		     csharp-mode
		     company                ;; Modular in-buffer completion framework
		     company-irony          ;; Completion back-end for irony-mode
		     dedicated              ;; To stop Emacs munging windows.
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
		     powerline              ;; Powerline (modeline) package. Not sure which variant.
		     powershell             ;; Run Powershell as an inferior process.
		     projectile
		     rainbow-delimiters
		     rainbow-mode
		     recentf-ext            ;; To make recentf mode understand directories.
		     rich-minority          ;; For wrangling minor modes in the modeline
		     s                      ;; string utility functions
		     sass-mode
		     shackle                ;; Window enforcement for popups https://github.com/wasamasa/shackle
		     smart-mode-line        ;; https://github.com/Malabarba/smart-mode-line
		     smart-mode-line-powerline-theme
		     sr-speedbar            ;; in-frame directory tree
		     ;;sunrise-commander
		     ;;sunrise-x-buttons
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
		     color-theme-modern
		     color-theme-sanityinc-tomorrow
		     color-theme-solarized
		     cyberpunk-theme
		     dakrone-theme
		     darkburn-theme
		     distinguished-theme
		     espresso-theme
		     firebelly-theme
		     flatland-theme
		     flatui-theme
		     grandshell-theme
		     green-phosphor-theme
		     gruber-darker-theme
		     gruvbox-theme
		     hc-zenburn-theme
		     hemisu-theme
		     heroku-theme
		     leuven-theme
		     material-theme
		     minimal-theme
		     moe-theme
		     molokai-theme
		     monokai-theme
		     monochrome-theme
		     obsidian-theme
		     occidental-theme
		     paper-theme
		     soft-morning-theme
		     soothe-theme
		     tango-plus-theme
		     tangotango-theme
		     underwater-theme
		     zenburn-theme
		     ))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
                         ("sc" . "http://joseito.republika.pl/sunrise-commander/")
			 ))

(package-initialize)

;; Fetch the list of packages available.
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages.
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq enable-local-variables t)
(setq enable-local-eval t)

;; Do my real initialization, safe in the knowledge that everything is loaded.
(add-hook 'after-init-hook
	  (lambda () (load "~/repos/dotfiles/emacs/.emacs.pd.el")))
