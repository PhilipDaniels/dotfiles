;;; My collection of hydras.
;;; Usage:  (require 'pd-hydra)

;; (require 'buffer-move)
;; (require 'cycle-buffer)
;; (require 'winner)
;; (require 'windmove)

(defun pd-hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun pd-hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun pd-hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun pd-hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defvar rectangle-mark-mode)

(defun pd-hydra-ex-point-mark ()
  "Exchange point and mark."
  (interactive)
  (if rectangle-mark-mode
      (exchange-point-and-mark)
    (let ((mk (mark)))
      (rectangle-mark-mode 1)
      (goto-char mk))))

(defhydra hydra-windows ()
  "M-arrow = select,  M-S-arrow = move, S-arrow = size"
  ("M-<up>" windmove-up nil)
  ("M-<down>" windmove-down nil)
  ("M-<left>" windmove-left nil)
  ("M-<right>" windmove-right nil)
  ("M-S-<left>" buf-move-left nil)
  ("M-S-<right>" buf-move-right nil)
  ("M-S-<up>" buf-move-up nil)
  ("M-S-<down>" buf-move-down nil)
  ("S-<left>" pd-hydra-move-splitter-left nil)
  ("S-<right>" pd-hydra-move-splitter-right  nil)
  ("S-<up>" pd-hydra-move-splitter-up nil)
  ("S-<down>" pd-hydra-move-splitter-down nil)
  ("p" previous-buffer "prev-buf")  ;; nis
  ("n" next-buffer "next-buf")      ;; nis
  ("1" delete-other-windows "1")    ;; nis
  ("d" delete-window "del")         ;; nis
  ("k" kill-buffer "kill")          ;; nis
  ("s" save-buffer "save")          ;; nis
  ("u" (progn (winner-undo) (setq this-command 'winner-undo)) "undo")
  ("r" winner-redo "restore")
  ("b" helm-mini "helm-mini" :exit t)        ;; nis
  ("f" helm-find-files "helm-find" :exit t)  ;; nis
  ("|" (lambda () (interactive) (split-window-right) (windmove-right)) "split-h")
  ("_" (lambda () (interactive) (split-window-below) (windmove-down)) "split-v")
  ("q" nil "cancel")
  )

;; (defhydra hydra-fonts ()
;;   "Adjust font size and face"
;;   ("<up>" text-scale-increase "larger")
;;   ("<down>" text-scale-decrease "smaller")
;;   ("<right>" (lambda () (interactive) (pd-font-set-candidate-font 1 (selected-frame) t)) "next")
;;   ("<left>" (lambda () (interactive) (pd-font-set-candidate-font -1 (selected-frame) t)) "prev")
;;   ("q" nil "cancel"))

;; Create a hydra to switch themes. We use the Emacs 24 theme engine (aka
;; deftheme) only, not the old color-theme.el engine.
;; A strange and non-obvious thing - calling load-theme applies the new
;; theme on top of the previous one - themes accumulate! Therefore, in
;; order to get a reasonable implementation we must disable all existing
;; themes before loading the new one, or alternatively keep track of the
;; current theme and disable it before loading a new one, which can be done
;; by writing our own wrapper around load-theme.
;; See Drew's answer at http://emacs.stackexchange.com/questions/3112/how-to-reset-color-theme?rq=1
;; I choose to use my own function so that I have flexibility to enable
;; more than one theme simultaneously if I want to.
;;
;; Applying extra theming
;; For example, you want to tweak cursor colors, modeline colors etc.
;; This is problematic, because your changes are not "part of" the theme
;; and hence cannot be reliably undone (see Drew's bug report at the above
;; SO link). There appear to be several possible solutions, none of which
;; are great:
;;
;; 1. For themes you really care about, customize the theme with your
;;    changes and use your customized version instead. This involves
;;    tweaking (setq custom-theme-directory "your-directory")
;;    and (add-to-list 'custom-theme-load-path "your-directory")
;;    and appears to be a reasonable solution.
;;    Pro: nicely self-contained.
;;    Cons: you have to tweak each theme.
;; 2. Write a custom theme which just contains your changes, then apply it
;;    whenever you change master themes (taking advantage of the fact that
;;    themes "accumulate").
;;    Pro: easy to apply your changes on top of several themes.
;;    Cons: requires all theme loading be done with your own custom function.
;; 3. Write a fn, which may be arbitrarily smart, which simply modifies
;;    faces and colours as appropriate and run it after switching themes.

;; (defhydra hydra-themes (:hint nil)
;;   "
;; Favourite : _sd_ Sol Dark       _sl_ Sol Light       _zb_ Zenburn        _ob_ Obsidian        _ty_ TTY Dark
;; Dark      : _gd_ Gruber Darker  _cp_ Cyberpunk       _gb_ Gruvbox        _bb_ BusyBee         _uw_ Underwater
;;             _md_ Minimal Dark   _mn_ Monokai         _ml_ Molokai        _cf_ Calm Forest
;; Light     : _lv_ Leuven         _hl_ Hemisu-Light    _mi_ Minimal Light  _ao_ Aalto Light
;; Grey      : _ma_ Material       _az_ Anti-Zenburn    _fu_ Flat UI        _sm_ Soft Morning    _tt_ TangoTango
;;             _je_ JEdit Grey     _cb_ Charcoal Black
;; Blue      : _rs_ Resolve        _bs_ Blue Sea        _rp_ Raspopovic     _ad_ Aalto Dark      _pr_ Parus
;; Mono      : _ro_ Retro Orange   _mo_ Monochrome      _rg_ Retro Green    _gp_ Green Phosphor
;; Consider  :
;; Rejects   : _ab_ Alect Black _al_ Alect Light _hd_ Hemisu Dark _gr_ Goldenrod
;; "
;;   ("ab" (pd-theme-load 'alect-black))
;;   ("ad" (pd-theme-load 'aalto-dark))
;;   ("al" (pd-theme-load 'alect-light))
;;   ("ao" (pd-theme-load 'aalto-light))
;;   ("az" (pd-theme-load 'anti-zenburn))
;;   ("bb" (pd-theme-load 'busybee))
;;   ("bs" (pd-theme-load 'blue-sea))
;;   ("cb" (pd-theme-load 'charcoal-black))
;;   ("cf" (pd-theme-load 'calm-forest))
;;   ("cp" (pd-theme-load 'cyberpunk))
;;   ("fu" (pd-theme-load 'flatui))
;;   ("gb" (pd-theme-load 'gruvbox))
;;   ("gd" (pd-theme-load 'gruber-darker))
;;   ("gp" (pd-theme-load 'green-phosphor))
;;   ("gr" (pd-theme-load 'goldenrod))
;;   ("hd" (pd-theme-load 'hemisu-dark))
;;   ("hl" (pd-theme-load 'hemisu-light))
;;   ("je" (pd-theme-load 'jedit-grey))
;;   ("lv" (pd-theme-load 'leuven))
;;   ("ma" (pd-theme-load 'material))
;;   ("md" (pd-theme-load 'minimal))
;;   ("mi" (pd-theme-load 'minimal-light))
;;   ("ml" (pd-theme-load 'molokai))
;;   ("mn" (pd-theme-load 'monokai))
;;   ("mo" (pd-theme-load 'monochrome))
;;   ("ob" (pd-theme-load 'obsidian))
;;   ("pr" (pd-theme-load 'parus))
;;   ("rg" (pd-theme-load 'retro-green))
;;   ("ro" (pd-theme-load 'retro-orange))
;;   ("rp" (pd-theme-load 'raspopovic))
;;   ("rs" (pd-theme-load 'resolve))
;;   ("sd" (pd-theme-load 'solarized 'dark))
;;   ("sl" (pd-theme-load 'solarized 'light))
;;   ("sm" (pd-theme-load 'soft-morning))
;;   ("tt" (pd-theme-load 'tangotango))
;;   ("ty" (pd-theme-load 'tty-dark))
;;   ("uw" (pd-theme-load 'underwater))
;;   ("zb" (pd-theme-load 'zenburn))
;;   ("q"  nil)
;;   )


;;(pd-log-loading-complete)
(provide 'pd-hydra)
