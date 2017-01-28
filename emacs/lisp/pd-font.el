;;; Customizations related to fonts.
;;; Usage:  (require 'pd-font)

(require 'cl-lib)
(require 'pd)
(require 'pd-theme)
(pd-log-requires-complete)

;; See https://github.com/chrissimpkins/codeface to get a big zip of ttf and otf
;; fonts. To determine the name that Emacs uses for a font, the easiest way I
;; know is to use the customize system to pick a default font, then save
;; options, the name of the font then appears in the .emacs file.
(defvar pd-font-candidates '("Consolas-14"
                             "Cousine-10"
                             "Source Code Pro-12"
                             "DejaVu Sans Mono-12"
                             "Droid Sans Mono-12"
                             "Courier New-10"
                             "Aurulent Sans Mono-10"
                             "Liberation Mono-12"
                             "Anonymous Pro-12"
                             "Liberation Mono-12"
                             "CPMono_v07 Plain-12"
                             "Calibri-12"
                             )
  "Defines a list of fonts that I like. The fonts are in priority order.")

(defun pd-font-existsp (font &optional frame)
  "Return a font if it exists, nil otherwise. Will fail if run in
daemon mode before initialization is complete."
  (find-font (font-spec :name font) frame))

(defun pd-font-remove-non-existent-candidates ()
  "Removes font elements from PD-FONT-CANDIDATES that do not
exist on this system."
  (setq pd-font-candidates (cl-remove-if-not 'pd-font-existsp pd-font-candidates)))

(defun pd-font-first-candidate ()
  "Returns the first valid candidate font."
  (find-if (lambda (f) (pd-font-existsp f)) pd-font-candidates))

(defvar pd-font-index nil
  "Specifies the index of the candidate font that is currently selected.")

(defun pd-font-set-candidate-font (step frame &optional show-msg)
  "Scans forwards (if STEP is 1) or backwards (if STEP is -1) through the list
pd-font-candidates looking for a valid font. If STEP is 0, the current font is
reset to the first font. The first time this function is called it starts the
search at index 0."
  (interactive)
  (let ((num-fonts (length pd-font-candidates))
        (num-seen 0)
        (font-is-valid))
    ;; Loop around the array; num-seen is used to avoid looping forever if
    ;; all elements in pd-font-candidates are invalid.
    (while (and (< num-seen num-fonts) (not font-is-valid))
      (cond ((null pd-font-index) (setq pd-font-index 0))
            ((= step 0) (setq pd-font-index 0 step 1))
            ((< (+ pd-font-index step) 0) (setq pd-font-index (- num-fonts 1)))
            ((>= (+ pd-font-index step) num-fonts) (setq pd-font-index 0))
            (t (setq pd-font-index (+ pd-font-index step))))

      (setq next-font (nth pd-font-index pd-font-candidates)
            num-seen (1+ num-seen)
            font-is-valid (pd-font-existsp next-font frame))

      (if font-is-valid
          (progn
            (if show-msg
                (message "Font set to %s" next-font))
            (set-frame-font next-font t (list frame)))
        (message "Font %s does not exist, skipping" next-font)))
    (if (not font-is-valid)
        (message "No valid fonts found in candidates: %s" pd-font-candidates))
    ))

(defun pd-font-select-first-valid-candidate ()
  "Selects the first valid candidate font."
  (pd-font-remove-non-existent-candidates)
  (pd-font-set-candidate-font 0 (selected-frame) t))


;;; Note that many of these functions will fail if run in daemon mode before
;;; initialization is complete, because there won't be a GUI frame for them to
;;; use and/or the window system will not be fully initialized. find-font is the
;;; main culprit. There is however a special non-visible daemon frame. See
;;; https://github.com/syl20bnr/spacemacs/issues/6197 at 7 Jun 2016
;;; http://emacs.stackexchange.com/questions/12351/when-to-call-find-font-if-launching-emacs-in-daemon-mode
;;; https://github.com/syl20bnr/spacemacs/blob/master/core/core-display-init.el
;;; http://emacs.1067599.n8.nabble.com/bug-23689-Daemon-mode-on-Windows-quot-w32-initialized-quot-is-set-too-early-td399455.html

;;; Therefore, in my startup logic, the functions are invoked using deferred
;;; execution: this will ensure that this function is run when the first frame
;;; is created.
(add-hook 'pd-focus-in-hook 'pd-font-select-first-valid-candidate)


(pd-log-loading-complete)
(provide 'pd-font)
