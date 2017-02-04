;;; See https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org#anatomy-of-a-layer

;;; This file configure the layer like declaring layer variables default values
;;; and setup some other variables related to the layer. This file is loaded
;;; after funcs.el.

(defvar pd-first-log-time 0 "The first time pd-log was called.")
(defvar pd-last-log-time 0 "The last time pd-log was called.")

;; When running in daemon mode the window system is not fully initialized, and
;; hence fonts are not loaded, until the first frame is created. This variable
;; and supporting function allow us to execute a list of functions when the
;; first frame is created. This allows us to setup fonts properly. See
;; pd-font.el and
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html#Startup-Summary
(defvar pd-focus-in-hook nil
  "List of functions to run (once only) when the FOCUS-IN-HOOK runs.")

(add-hook 'focus-in-hook 'pd-focus-in-hook-execute)

;; Some colors from solarized.
(defconst pd-sol-yellow "#b58900")
(defconst pd-sol-blue "#2075c7")
(defconst pd-sol-cream "#e9e2cb")
