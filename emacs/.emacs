;;; -*- mode: emacs-lisp -*-
;;; -*- coding: utf-8 -*-
;;; Local Variables:
;;; eval: (outline-minor-mode)
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
(setq package-list '(alect-themes
		     ample-theme
		     anti-zenburn-theme
		     color-theme-solarized
		     cyberpunk-theme
		     dakrone-theme
		     fill-column-indicator
		     flatland-theme
		     flatui-theme
		     git-timemachine
		     gruber-darker-theme
		     gruvbox-theme
		     helm
		     hemisu-theme
		     heroku-theme
		     leuven-theme
		     magit
		     markdown-mode
		     monokai-theme
		     moe-theme
		     outline-magic
		     rainbow-mode
		     s
		     tango-plus-theme
		     unbound
		     yasnippet
		     zenburn-theme
		     ))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

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
(add-hook 'after-init-hook (lambda () (load "~/repos/dotfiles/emacs/emacs.pd.el")))
