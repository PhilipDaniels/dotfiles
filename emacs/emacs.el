;;; -*- mode: emacs-lisp -*-
;;; -*- coding: utf-8 -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (expand-region persistent-scratch recentf-ext unbound markdown-mode git-timemachine gitignore-mode gitattributes-mode zenburn-theme underwater-theme tangotango-theme tango-plus-theme soothe-theme solarized-theme soft-morning-theme occidental-theme obsidian-theme monochrome-theme monokai-theme molokai-theme minimal-theme material-theme leuven-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme green-phosphor-theme grandshell-theme flatui-theme flatland-theme firebelly-theme espresso-theme distinguished-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-solarized color-theme-sanityinc-tomorrow color-theme-modern clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme hydra s fill-column-indicator dash))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; Local Variables:
;;; End:

(setq enable-local-variables t)
(setq enable-local-eval t)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")  ;; official
                         ("melpa" . "http://melpa.org/packages/")   ;; user contrib, more up to date
                         ("sc" . "http://joseito.republika.pl/sunrise-commander/")))

(package-initialize)


;; From http://stackoverflow.com/questions/25430029/whats-the-best-way-to-package-my-emacs-installation-packages-and-config-so-tha?noredirect=1&lq=1
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
(if (package-installed-p package min-version)
    t
  (if (or (assoc package package-archive-contents) no-refresh)
      (package-install package)
    (progn
      (package-refresh-contents)
      (require-package package min-version t)))))


;; My .emacs is kept as a separate set of (local) packages that
;; I simply 'require' - n.b. NOT 'require-package'!
(add-to-list 'load-path "~/repos/dotfiles/emacs/lisp")
(require 'pd-first)
(require 'pd)
(require 'pd-cpp)
(require 'pd-modes)
(require 'pd-misc)
(require 'pd-yasnippet)
(require 'pd-helm)
(require 'pd-bookmarks)
(require 'pd-git)
(require 'pd-programming)
(require 'pd-terminal)
(require 'pd-fonts)
(require 'pd-themes)
(require 'pd-appearance)
(require 'pd-hydras)
(require 'pd-keys)


;;; $$ Server (aka daemon) mode.
;; There are two ways to start Emacs in daemon mode.
;;   1. Run 'emacs-w32 --daemon' in .bashrc or other startup script.
;;      This creates an Emacs process which runs in the background and is not
;;      attached to any tty.
;;   2. Start Emacs normally after logging in, then call the 'server-start'
;;      function, either manually or at the end of your .emacs file.
;;
;; 1) may seem simpler, but in fact when Emacs is started this way some (hard to
;; determine) functions, variables and settings do not work because there is no
;; frame or window system defined. This can make configuration difficult. Cursor
;; colors, fonts etc. do not get set when you expect them to be.
;; Therefore, I recommend method 2. This .emacs file has been designed with it
;; in mind. So, after I logon I just do "we &" to create a new graphical Win32
;; Emacs with an initial frame - which, typically, I never close. The call to
;; server-start turns this initial Emacs into a server.
;;
;; Other info: daemonp - predicate can be used to detect daemon mode.
;;(load "server")
;;(unless (server-running-p)
;;  (server-start))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
;; [ ] Get daemon mode working correctly and start Emacs as a daemon rather
;;     than in emacs.el.
;; [ ] Get Emacs client working properly for git. Maybe just start a new Emacs
;;     with a very minimal setup, by using an environment variable and invoking
;;     Emacs like this:    CLI_GIT=true emacs-w32.
;;     In elisp, we can use   (getenv "CLI_GIT")


;; [ ] Move to use-package - this looks awesome.
;; [ ] Log bug about solarized dark messing up rainbow delimiters.
;; [ ] Requires in my files. Nominal order is require-package, require built-ins,
;;     require pd-*, however, should we use eval-when-compile, as seen in pd-cpp?
;; [ ] Write a shell script to byte-compile my /lisp directory.
;; [ ] Setup auto-compile for my /lisp directory so that a local edit of any
;;     file in there is automatically byte-compiled.
;; [ ] Review names of my packages - removal of plural names.
;; [ ] Review names of all functions in my packages - ensure they are prefixed
;;     with the package name.
;; [ ] Should we use setq or setq-default?
;; [ ] Should we use ' or #' ?
;; [ ] Windows compatible keybindings for copy/paste/save/find and ctrl-arrow
;;     for word movement
;; [ ] Write an underline function.
;; [ ] pd-helm: Get helm ignores working. This will speed up greps a lot.
;; [ ] pd-terminal: Make ansi-term update a variable which contains the cwd
;;     so that it can be smarter about creating new windows. See comment in
;;     the file.
;; [ ] pd-helm: Use ripgrep or ag?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tricks
;;
;; You can use this technique to exit a lisp file early.
;;(with-current-buffer " *load*"
;;  (goto-char (point-max)))
;;
;;
;; If you want to try alternate customizations using a clean .emacs.d folder,
;; you can invoke Emacs like this:  HOME=elsewhere emacs
;;
;;
;; Do something once only, the first time a package is loaded:
;; use-package is not built-in, however:
;; https://github.com/jwiegley/use-package
;; (use-package jedi
;;   :ensure t
;;   :config
;;   (unless (package-installed-p 'jedi)
;;     (jedi:install-server))
;;   )
