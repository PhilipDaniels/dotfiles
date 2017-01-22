;;; Customization relating to bookmarks.
;;; Usage:  (require 'pd-bookmark)

;;(setq bm-restore-repository-on-load t)
;;(require-package 'bm)           ;; https://github.com/joodland/bm
;;(require-package 'bookmark+)    ;; https://www.emacswiki.org/emacs/BookmarkPlus

;; ;; Visible Bookmarks (package bm, https://github.com/joodland/bm)
;; (setq bm-cycle-all-buffers t)
;; (setq-default bm-buffer-persistence t)
;; (setq bm-repository-file "~/.emacs.d/bm-repository")
;; (add-hook' after-init-hook #'bm-repository-load)
;; (add-hook 'find-file-hooks #'bm-buffer-restore)
;; (add-hook 'kill-buffer-hook #'bm-buffer-save)
;; (add-hook 'kill-emacs-hook #'(lambda nil
;;                               (bm-buffer-save-all)
;;                               (bm-repository-save)))
;; (add-hook 'after-save-hook #'bm-buffer-save)
;; (add-hook 'after-revert-hook #'bm-buffer-restore)
;; (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

;; Bookmark+.
;; (setq bookmark-version-control t)
;; (setq bookmark-save-flag 1)

(provide 'pd-bookmark)
