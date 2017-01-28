;;; Customizations related to yasnippet.
;;; See http://capitaomorte.github.io/yasnippet/
;;; Usage:  (require 'pd-yasnippet)

(require-package 'yasnippet)
(require 'yasnippet)
(require 'pd)
(pd-log-requires-complete)

;; Load yasnippet, but only load my snippets (there are many examples
;; under the elpa/yasnippet folder which we do not want to load).

(setq yas-snippet-dirs (pd-get-full-path "../snippets"))
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'markdown-mode-hook #'yas-minor-mode)


(pd-log-loading-complete)
(provide 'pd-yasnippet)
