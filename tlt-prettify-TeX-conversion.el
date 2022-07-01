;;; tlt-prettify-TeX-conversion.el --- Convert LaTeX-macros to Unicode-Characters and vice versa -*-

;;; LaTeX ↔ Unicode conversion

(require 'tlt-prettify-TeX)

(defcustom tlt-prettify-TeX-org-msg t "Whether or not LaTeX-to-Unicode-conversion should
automatically happen when sending an e-mail via org-msg."
:group 'tlt-prettify-TeX)

(defcustom tlt-prettify-TeX-export-docx t "Whether or not LaTeX-to-Unicode-conversion should
automatically happen before exporting an .org document to .docx via pandoc."
:group 'tlt-prettify-TeX)

(defun tlt-prettify-TeX-convert-symbol-to-LaTeX (symbol)
  "If possible, convert unicode SYMBOL to respective LaTeX command.
Return a string containing the according LaTeX macro name or nil
if SYMBOL (which should be a one-character string) was not
converted."
  ;; Ignore usual letters, digits, newlines, spaces, etc.
  (unless (string-match-p "[a-zA-Z0-9[:cntrl:][:blank:]]" symbol)
    (let ((key (list symbol)))
      (car (or (rassoc key tlt-prettify-TeX-symbols-no-spaces)
               (rassoc key tlt-prettify-TeX-symbols-left-space)
               (rassoc key tlt-prettify-TeX-symbols-right-space)
               (rassoc key tlt-prettify-TeX-symbols-both-spaces)
               (rassoc key tlt-prettify-TeX-arrow-symbols))))))

(defun tlt-prettify-TeX-convert-text-to-LaTeX (text)
  "Return TEXT with Unicode symbols converted to LaTeX."
  (with-temp-buffer
    (mapc (lambda (char)
            (let* ((symbol (string char))
                   (latex (tlt-prettify-TeX-convert-symbol-to-LaTeX symbol)))
              (insert (if latex
                          (concat " " latex " ")
                        symbol))))
          text)

    ;; Remove extra spaces.
    (goto-char (point-min))
    (while (re-search-forward "  +" nil t) ; 2 spaces or more
      (replace-match " "))
    (goto-char (point-min))
    (while (re-search-forward "^ \\| $" nil t) ; spaces at beg/end of lines
      (replace-match ""))
    (goto-char (point-min))
    ;; Spaces around -, +, =, <, >
    (while (re-search-forward " ?\\([+<=>-]\\) ?" nil t)
      (replace-match "\\1"))
    (goto-char (point-min))
    ;; Spaces after/before opening/closing braces.
    (while (re-search-forward
            (rx (group (or "(" "[" "\\{")) " ")
            nil t)
      (replace-match "\\1"))
    (goto-char (point-min))
    (while (re-search-forward
            (rx " " (group (or (regexp "[]),.!?]") "\\}")))
            nil t)
      (replace-match "\\1"))

    (buffer-substring-no-properties (point-min) (point-max))))

(defun tlt-prettify-TeX-convert-LaTeX-to-unicode (string)
  "Convert STRING containing LaTeX commands to a respective string of unicode symbols."
  (with-temp-buffer
    (insert string)
    (tex-mode)
    ;; If `case-fold-search' is t, then `re-search-forward' makes no
    ;; difference between "\alpha" and "\Alpha".
    (let (case-fold-search)
      (mapc (lambda (entry)
              (let ((re (car entry))
                    (subst (cdr entry)))
                (goto-char (point-min))
                (while (re-search-forward re nil t)
                  (replace-match subst))))
            tlt-prettify-TeX-symbol-regexps))
    (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun tlt-prettify-TeX-copy-region-unicode-to-LaTeX (beg end)
  "Save the selected region as if `copy-region-as-kill' was used, but convert unicode symbols to their respective LaTeX commands."
  (interactive
   (if (region-active-p)
       (let ((m (mark))
             (p (point)))
         (list (min m p) (max m p)))
     (user-error "You need to select some text!")))
  (kill-new (tlt-prettify-TeX-convert-text-to-LaTeX
             (buffer-substring-no-properties beg end)))
  (deactivate-mark))

;;;###autoload
(defun tlt-prettify-TeX-paste-region-unicode-to-LaTeX ()
  "Paste the lastly killed or copied text, but convert unicode symbols to their respective LaTeX commands."
  (interactive)
  (insert (tlt-prettify-TeX-convert-text-to-LaTeX (current-kill 0))))

;;;###autoload
(defun tlt-prettify-TeX-copy-region-LaTeX-to-unicode (beg end)
  "Save the selected region as if `copy-region-as-kill' was used, but convert LaTeX commands to the unicode symbols they would print."
  (interactive
   (if (region-active-p)
       (let ((m (mark))
             (p (point)))
         (list (min m p) (max m p)))
     (user-error "You need to select some text!")))
  (kill-new (tlt-prettify-TeX-convert-LaTeX-to-unicode
             (buffer-substring-no-properties beg end)))
  (deactivate-mark))

;;;###autoload
(defun tlt-prettify-TeX-paste-region-LaTeX-to-unicode ()
  "Paste the lastly killed or copied text, but convert LaTeX commands to the unicode symbols they would print."
  (interactive)
  (insert (tlt-prettify-TeX-convert-LaTeX-to-unicode (current-kill 0))))

(defun tlt-prettify-TeX-buffer-LaTeX-to-unicode ()
  "Replace all LaTeX commands in the current buffer which are wrapped in dollar signs with their Unicode counterpart."
  (interactive)
  (require 'tlt-prettify-TeX) ; require the package to have `tlt-prettify-TeX-convert-LaTeX-to-unicode' available, which we need for conversion
  (let ((re "\\$"))             ; call the string containing the dollar sign "re"
    (save-excursion             ; save the point's position
      (goto-char (point-min))   ; go to the beginning of the buffer
      (while (re-search-forward re nil t) ; search forward until you hit the dollar sign
        (let ((beg (match-beginning 0)))  ; call that position "beg"
          (when (eq (org-element-type (org-element-context)) ; if it is a LaTeX fragment
                    'latex-fragment)
            (let* ((end (or (re-search-forward re nil t) ; call the next occurence of a dollar sign or the end of the buffer "end"
                            (point-max)))
                   (subst (tlt-prettify-TeX-convert-LaTeX-to-unicode ; call the result of applying LaTeX-to-Unicode-conversion
                           (buffer-substring-no-properties (1+ beg) (1- end))))) ; to the string between the dollar signs "subst"
              (delete-region beg end) ; delete the string between the dollar signs plus the dollar signs themselves
              (goto-char beg)         ; then go where the first dollar sign would have been
              (insert subst)))))))   ; and insert the converted string right there
  nil) ; lastly, return nil for `org-ctrl-c-ctrl-c-hook'.


(defun tlt-prettify-TeX-msg-LaTeX-to-unicode () "" (interactive)
       (when
           (and tlt-prettify-TeX-org-msg tlt-prettify-TeX-org-msg
                (eq major-mode 'org-msg-edit-mode) ; make sure we're in mail-mode
                (memq (plist-get (org-export-get-environment) :with-latex)
                      '(t nil)))
         (let ((org-html-with-latex 'verbatim)) ; must be set this way for conversion to work
           (tlt-prettify-TeX-buffer-LaTeX-to-unicode))))       ; actual conversion

(add-hook 'org-ctrl-c-ctrl-c-hook #'tlt-prettify-TeX-msg-LaTeX-to-unicode)

(defun tlt-prettify-TeX-export-to-docx (&optional async subtreep visible-only body-only info)
  "Like `org-pandoc-export-to-docx-and-open' but do LaTeX-Unicode-conversion before."
  (interactive)
  (let ((org-export-before-parsing-hook
         (lambda (_) (tlt-prettify-TeX-buffer-LaTeX-to-unicode))))
    (org-ref-process-buffer 'docx)
    (org-ref-export-to 'docx async subtreep visible-only body-only info)))

(when tlt-prettify-TeX-export-docx
  (advice-add 'org-ref-export-to-docx :override #'tlt-prettify-TeX-export-to-docx))

(provide 'tlt-prettify-TeX-conversion)

;; tlt-prettify-TeX-conversion.el ends here
