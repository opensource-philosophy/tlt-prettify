(require 'org)
(require 'tlt-prettify-TeX)

(defcustom tlt-prettify-org-lighter " ∀"
  "String to show in the modeline when
`tlt-prettify-org-mode' ist active. To
deactivate, set to nil.")

(defface tlt-prettify-org-inline-keywords
  '((t :foreground "turquoise2"))
  "Face for inline LaTeX or beamer code.
See `tlt-prettify-org-inline-keywords'."
  :group 'tlt-faces)

(defvar tlt-prettify-org-keywords nil)

(defvar tlt-prettify-org-inline-keywords
  `(("\\(@@\\(?:latex:\\|beamer:\\)\\)\\([^@]+\\)\\(@@\\)"
     ;; "@@latex:FOO@@" regexp:
     ;; the 1st group is "@@" followed by "beamer:" or "latex:"
     ;; the 2nd group is "FOO".
     ;; the 3rd group is "@@".
     (1 '(face nil invisible t))                  ; invisibility property for group 1
     (2 'tlt-prettify-org-inline-keywords t) ; tp-example-latex property for group 2
     (3 '(face nil invisible t))))                ; invisibility property for group 3
  "How to display LaTeX or beamer inline code,
i.e. expressions of the form '@@latex:foo@@' or 
'@@beamer:foo@@'. In both cases, only shows 'foo' 
highlighted with `tlt-prettify-org-inline-keywords'.")

(defun tlt-prettify-org-put (str)
  "Display the matching region as STR."
  (let* ((beg (match-beginning 0))
         (end (match-end 0))
         (org-type (save-excursion
                     (goto-char beg)
                     (org-element-type (org-element-context)))))
    (when (eq org-type 'latex-fragment)
      (compose-region beg end str)
      (add-text-properties
       beg end
       (list 'tlt-prettify-org-beg beg
             'tlt-prettify-org-end end))))
  ;; Return nil because we are not adding any face property.
  nil)


(defun tlt-prettify-org-pretty-keywords ()
  "Generate the list of keywords to prettify
responsible for prettifying LaTeX commands."
  (mapcar (lambda (cons)
            `(,(car cons) 0
              (tlt-prettify-org-put ,(cdr cons))))
          (reverse tlt-prettify-TeX-symbol-regexps)))

(defun tlt-prettify-org-keywords ()
  "Generate the final list of keywords to be added to
`font-lock-keywords' when `tlt-prettify-org-mode'
is activated."
  (or tlt-prettify-org-keywords                            ; take tlt-prettify-org-keywords if not empty
      (setq tlt-prettify-org-keywords                      ; or define them to 
            (append tlt-prettify-org-inline-keywords       ; consist of tlt-prettify-org-inline-keywords
                    (tlt-prettify-org-pretty-keywords))))) ; and tlt-prettify-org-pretty-keywords

(defun tlt-prettify-org-delete-forward-char (&optional n)
  "Delete the following N characters.
If the next character is a symbol prettified with
`tlt-prettify-org-mode', then remove this symbol.
Otherwise, fallback to `delete-char'."
  (interactive "p")
  ;; The piece to delete region is taken from `delete-backward-char'.
  (if (and (use-region-p)
           delete-active-region)
      (if (eq delete-active-region 'kill)
          (kill-region (region-beginning) (region-end) 'region)
        (funcall region-extract-function 'delete-only))
    (let* ((pos (+ (if (> n 0) 0 -1) (point)))
           (beg (get-text-property pos 'tlt-prettify-org-beg))
           (end (get-text-property pos 'tlt-prettify-org-end)))
      (if beg
          (delete-region beg end)
        (delete-char n)))))

(defun tlt-prettify-org-delete-backward-char (&optional n)
  "Delete the previous N characters.
If the previous character is a symbol prettified with
`tlt-prettify-org-mode', then remove this symbol.
Otherwise, fallback to `delete-char'."
  (interactive "p")
  (tlt-prettify-org-delete-forward-char (- n)))

(defvar tlt-prettify-org-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tlt-prettify-TeX-map)
    (define-key map (kbd "<backspace>") 'tlt-prettify-org-delete-backward-char)
    (define-key map (kbd "<delete>") 'tlt-prettify-org-delete-forward-char)
    map)
  "Keymap for `tlt-prettify-org-mode'.")

(defun tlt-prettify-org-set-lighter ()
  "Correctly set string displayed in the doom modeline. 
To disable this feature, set `tlt-prettify-org-lighter' to nil." 
  (when                                                   ; if 
      (and doom-modeline-mode                             ; doom-modeline is turned on and
           tlt-prettify-org-lighter)                      ; a lighter string should be displayed in the modeline,     
    (setq-local global-mode-string                        ; make `global-mode-string' a buffer-local variable, which in this buffer
                (add-to-list 'global-mode-string          ; has, next to its original elements,
                             tlt-prettify-org-lighter)))) ; also `tlt-prettify-org-lighter' as its element

(defun tlt-prettify-org-remove-lighter-maybe ()
  "Correctly remove string displayed in the doom modeline.
The case for the vanilla modeline is taken care of by the
minor mode itself."
  (when (and tlt-prettify-org-lighter        ; if a string should be displayed and
             doom-modeline-mode)             ; doom-modeline is turned on,
    (setq global-mode-string                 ; set the relevant variable
          (remove tlt-prettify-org-lighter   ; to its current value minus
                  global-mode-string))))     ; that string

(define-minor-mode tlt-prettify-org-mode
  "Prettify some LaTeX names as Unicode symbols.
This mode is intended to be used in an `org-mode' buffer.

When enabled, this mode displays “\\latex-foo” names as the
corresponding Unicode symbols inside latex fragments (text pieces
placed inside “$...$”).

\\{tlt-prettify-org-mode-map}"
  :init-value nil
  :lighter " ∀"
  (let ((keywords (tlt-prettify-org-keywords)))
    (if tlt-prettify-org-mode                               ; if the mode is turned on,
        (progn                                              ; do the following
          (font-lock-add-keywords nil keywords)             
          (modify-syntax-entry ?$ "\"")                     
          (setq-local font-lock-extra-managed-props         
                      (append font-lock-extra-managed-props 
                              '(composition
                                tlt-prettify-org-beg
                                tlt-prettify-org-end)))
          (tlt-prettify-org-set-lighter))     
      (tlt-prettify-org-remove-lighter-maybe)                ; else remove the lighter
      (font-lock-remove-keywords nil keywords))              ; and the keywords
    (jit-lock-refontify)))                                   ; and renew font-locking

(provide 'tlt-prettify-org)

;;; tlt-pretitfy-org.el ends here
