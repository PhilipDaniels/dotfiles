;;; -*- mode: emacs-lisp -*-
;;; -*- coding: utf-8 -*-
;;; Local Variables:
;;; End:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "/home/phil/.emacs.d/bookmarks")
 '(custom-safe-themes t)
 '(package-selected-packages
   (quote
    (bookmark+ zenburn-theme yasnippet yaml-mode underwater-theme unbound tangotango-theme tango-plus-theme sunrise-x-buttons sr-speedbar soothe-theme soft-morning-theme smartparens smart-mode-line-powerline-theme shackle sass-mode recentf-ext rainbow-mode rainbow-delimiters powershell persistent-scratch outline-magic occidental-theme obsidian-theme multi-term monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme mic-paren material-theme markdown-mode magit lua-mode leuven-theme key-chord json-mode jade-mode hydra hlinum heroku-theme hemisu-theme helm-projectile helm-gtags hc-zenburn-theme haskell-mode gruvbox-theme gruber-darker-theme green-phosphor-theme grandshell-theme golden-ratio gitignore-mode gitattributes-mode git-timemachine ggtags fsharp-mode flatui-theme flatland-theme firebelly-theme fill-column-indicator f expand-region espresso-theme distinguished-theme dedicated darkburn-theme dakrone-theme cyberpunk-theme csharp-mode company-irony color-theme-solarized color-theme-sanityinc-tomorrow color-theme-modern clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme bm badger-theme autopair anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme))))

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
