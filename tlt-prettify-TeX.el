;;; tlt-prettify-TeX.el --- Enhance LaTeX-mode for semi-WYSIWYG editing  -*- lexical-binding: t -*-

;; Copyright © 2014–2015 zk_phi
;; Copyright © 2021–2022 Vitus Schäfftlein
;; Copyright © 2021–2022 Alex Kost

;; Author: zk_phi
;; URL: https://github.com/opensource-philosophy/tlt-prettify-TeX
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides `tlt-prettify-TeX' minor mode and several
;; commands to convert Unicode symbols to LaTeX command names and back.
;;
;; When you run "M-x tlt-prettify-TeX", in a `latex-mode' buffer, some
;; LaTeX command names will be shown as the Unicode symbols, e.g. "∀"
;; instead of "\forall".  This is only a visual appearance, the
;; underlying LaTeX text is not changed: after disabling the mode (with
;; "M-x tlt-prettify-TeX"), you will get your LaTeX back.

;; This file also provides the following M-x commands:
;;
;; - `tlt-prettify-TeX-copy-region-unicode-to-LaTeX'
;; - `tlt-prettify-TeX-paste-region-unicode-to-LaTeX'
;; - `tlt-prettify-TeX-copy-region-LaTeX-to-unicode'
;; - `tlt-prettify-TeX-paste-region-LaTeX-to-unicode'
;;
;; When you run one of the above commands, the respective "LaTeX to/from
;; Unicode" conversion will occur on the selected region or on the
;; current clipboard contents.  For example, if you select "∀x∃y" and run
;; "M-x tlt-prettify-TeX-copy-region-unicode-to-LaTeX", then "\forall x \exists y"
;; will be copied to the `kill-ring' (Emacs clipboard).

;; To install this package manually, copy "tlt-prettify-TeX.el"
;; somewhere and add the following lines to your Emacs init file:
;;
;;   (add-to-list 'load-path "/path/to/tlt-prettify-TeX-directory")
;;   (autoload 'tlt-prettify-TeX "tlt-prettify-TeX" nil t)
;;
;; This will allow you to run "M-x tlt-prettify-TeX".  If you wish to
;; start `tlt-prettify-TeX' automatically when you open a TeX-file,
;; you also need the following line:
;;
;;   (add-hook 'TeX-update-style-hook 'tlt-prettify-TeX)
;;
;; Finally, you may want to add autoloads for the rest commands, if you
;; need them:
;;
;;   (autoload 'tlt-prettify-TeX-copy-region-unicode-to-LaTeX  "tlt-prettify-TeX" nil t)
;;   (autoload 'tlt-prettify-TeX-paste-region-unicode-to-LaTeX "tlt-prettify-TeX" nil t)
;;   (autoload 'tlt-prettify-TeX-copy-region-LaTeX-to-unicode  "tlt-prettify-TeX" nil t)
;;   (autoload 'tlt-prettify-TeX-paste-region-LaTeX-to-unicode "tlt-prettify-TeX" nil t)

;; For more information, see <https://github.com/opensource-philosophy/tlt-prettify-TeX>.

;;; Code:

(require 'font-lock)
(require 'jit-lock)
(require 'tex-mode)
(require 'iimage)
(require 'cl-lib)


;;; Variables and faces

(defgroup tlt-prettify-TeX-faces nil
  "Faces used by `tlt'."
  :group 'tlt
  :group 'tlt-prettify-TeX-faces)

(defgroup tlt-prettify-TeX nil
  "Treat LaTeX commands as if they were
the unicode symbols they generate when
compiled."
  :group 'tlt)

(defgroup tlt-prettify-TeX-faces nil
  "Treat LaTeX commands as if they were
the unicode symbols they generate when
compiled."
  :group 'tlt-prettify-TeX)

(defcustom tlt-prettify-TeX-ignored-properties
  '(font-lock-comment-face
    font-lock-comment-delimiter-face
    font-lock-constant-face
    tex-verbatim)
  "List of faces which `tlt-prettify-TeX' should ignore."
  :type '(list face)
  :group 'tlt-prettify-TeX)

(defcustom tlt-prettify-TeX-enable-block-highlight t
  "When non-nil, prettify blocks like \"{\\large ...}\"."
  :type 'boolean
  :group 'tlt-prettify-TeX)

(defcustom tlt-prettify-TeX-enable-block-align nil
  "When non-nil, align blocks like \"{\\centering ...}\"."
  :type 'boolean
  :group 'tlt-prettify-TeX)

(defcustom tlt-prettify-TeX-enable-suscript t
  "When non-nil, prettify subscripts and superscripts like
\"a_1\", \"e^{-x}\"."
  :type 'boolean
  :group 'tlt-prettify-TeX)

(defcustom tlt-prettify-TeX-enable-pretty-symbols t
  "When non-nil, prettify symbols with unicode characters and
character composition."
  :type 'boolean
  :group 'tlt-prettify-TeX)

(defcustom tlt-prettify-TeX-enable-inline-image t
  "When non-nil, `iimage-mode' is enabled automatically."
  :type 'boolean
  :group 'tlt-prettify-TeX)

(defcustom tlt-prettify-TeX-enable-minibuffer-echo t
  "When non-nil, actual command is displayed in the modeline."
  :type 'boolean
  :group 'tlt-prettify-TeX)

(defconst tlt-prettify-TeX-syntax-table
  (let ((st (copy-syntax-table tex-mode-syntax-table)))
    (modify-syntax-entry ?$ "\"" st)
    (modify-syntax-entry ?\' "." st)
    st)
  "like `tex-mode-syntax-table' but treat $ as a string quote,
for correct inline-math recognition. Also make the quote ' be considered a delimiter (to correctly detect symbols)")

(defvar-local tlt-prettify-TeX-jit-point nil
  "store the point while font-locking")
(define-advice jit-lock-fontify-now (:around (fn &rest args) tlt-prettify-TeX-ad-jit-lock)
  (let ((tlt-prettify-TeX-jit-point (point)))
    (apply fn args)))

(defface tlt-prettify-TeX-title '((t (:inherit font-lock-function-name-face :height 2.0)))
  "Face used for title command in tlt-prettify-TeX-faces buffers."
  :group 'tlt-prettify-TeX-faces)
(defface tlt-prettify-TeX-chapter '((t (:inherit font-lock-function-name-face :height 1.8)))
  "Face used for chapter command in tlt-prettify-TeX-faces buffers."
  :group 'tlt-prettify-TeX-faces)
(defface tlt-prettify-TeX-section '((t (:inherit font-lock-function-name-face :height 1.6)))
  "Face used for section command in tlt-prettify-TeX-faces buffers."
  :group 'tlt-prettify-TeX-faces)
(defface tlt-prettify-TeX-subsection '((t (:inherit font-lock-function-name-face :height 1.2)))
  "Face used for subsection command in tlt-prettify-TeX-faces buffers."
  :group 'tlt-prettify-TeX-faces)

(defface tlt-prettify-TeX-box '((t (:box t)))
  "Face used for box command in tlt-prettify-TeX-faces buffers."
  :group 'tlt-prettify-TeX-faces)
(defface tlt-prettify-TeX-overline '((t (:overline t)))
  "Face used for overline command in tlt-prettify-TeX-faces buffers."
  :group 'tlt-prettify-TeX-faces)
(defface tlt-prettify-TeX-type '((t (:inherit 'fixed-pitch)))
  "Face used for type command in tlt-prettify-TeX-faces buffers."
  :group 'tlt-prettify-TeX-faces)

(defface tlt-prettify-TeX-tiny '((t (:height 0.7)))
  "Face used for tiny command in tlt-prettify-TeX-faces buffers."
  :group 'tlt-prettify-TeX-faces)
(defface tlt-prettify-TeX-script '((t (:height 0.8)))
  "Face used for script command in tlt-prettify-TeX-faces buffers."
  :group 'tlt-prettify-TeX-faces)
(defface tlt-prettify-TeX-footnote '((t (:height 0.8)))
  "Face used for footnote command in tlt-prettify-TeX-faces buffers."
  :group 'tlt-prettify-TeX-faces)
(defface tlt-prettify-TeX-small '((t (:height 0.9)))
  "Face used for small command in tlt-prettify-TeX-faces buffers."
  :group 'tlt-prettify-TeX-faces)
(defface tlt-prettify-TeX-large '((t (:height 1.1)))
  "Face used for large command in tlt-prettify-TeX-faces buffers."
  :group 'tlt-prettify-TeX-faces)
(defface tlt-prettify-TeX-llarge '((t (:height 1.1)))
  "Face used for Large command in tlt-prettify-TeX-faces buffers."
  :group 'tlt-prettify-TeX-faces)
(defface tlt-prettify-TeX-xlarge '((t (:height 1.1)))
  "Face used for LARGE command in tlt-prettify-TeX-faces buffers."
  :group 'tlt-prettify-TeX-faces)
(defface tlt-prettify-TeX-huge '((t (:height 1.1)))
  "Face used for huge command in tlt-prettify-TeX-faces buffers."
  :group 'tlt-prettify-TeX-faces)
(defface tlt-prettify-TeX-hhuge '((t (:height 1.1)))
  "Face used for Huge command in tlt-prettify-TeX-faces buffers."
  :group 'tlt-prettify-TeX-faces)

(defface tlt-prettify-TeX-lbar '((t (:background "red")))
  "Face used to render vertical line for quote environments in
  tlt-prettify-TeX-faces buffers."
  :group 'tlt-prettify-TeX-faces)


;;; General utilities

(cl-defun tlt-prettify-TeX-quote-latex-macro (name &key head tail)
  (concat (and head " ?")
          (regexp-quote name)
          (and (string-match "[a-zA-Z]\\'" name) "\\>")
          (and tail " ?")))

(cl-defun tlt-prettify-TeX-quote-latex-macros (list &key head tail)
  (mapcar (lambda (list)
            (cons (tlt-prettify-TeX-quote-latex-macro (car list) :head head :tail tail)
                  (cadr list)))
          list))

(defmacro tlt-prettify-TeX-safe-excursion (&rest body)
  "Like `progn' but moves the point only when BODY succeeded."
  `(let ((pos (point)))
     (condition-case err (progn ,@body)
       (error (goto-char pos) (error (error-message-string err))))))

(defun tlt-prettify-TeX-command-regexp-opt (strings)
  "Like `regexp-opt' but for LaTeX command names."
  (concat "\\\\" (regexp-opt strings) "\\>"))

(defun tlt-prettify-TeX-overlay-at (point prop val)
  "Return an overlay at point, whose property PROP is VAL. If
some overlays satisfy the condition, overlay with the highest
priority is returned. If there's no such overlays, return nil."
  (cl-some (lambda (ov) (when (equal (overlay-get ov prop) val) ov))
           (sort (overlays-at point)
                 (lambda (a b) (let ((pa (overlay-get a 'priority))
                                     (pb (overlay-get b 'priority)))
                                 (or (null pb) (and pa (>= pa pb))))))))

(defun tlt-prettify-TeX-column-at-eol ()
  (save-excursion (end-of-line) (current-column)))

(defun tlt-prettify-TeX-column-at-indentation ()
  (save-excursion (back-to-indentation) (current-column)))

(defun tlt-prettify-TeX-skip-comments-and-verbs (&optional backward)
  "Skip forward this comment or verbish environment. Return
non-nil iff the cursor is moved."
  (when (and (not (eobp))             ; return non-nil only when moved
             (memq (get-text-property (point) 'face)
                   tlt-prettify-TeX-ignored-properties)
             (memq (get-text-property (1- (point)) 'face)
                   tlt-prettify-TeX-ignored-properties))
    (let ((pos (if backward
                   (previous-single-property-change (point) 'face)
                 (next-single-property-change (point) 'face))))
      (goto-char (or pos (if backward (point-min) (point-max))))
      (when pos (tlt-prettify-TeX-skip-comments-and-verbs backward))
      t)))

(defun tlt-prettify-TeX-search-regexp (regex &optional bound backward point-safe)
  "Like `search-regexp' but skips escaped chars, comments and
verbish environments. This function raises an error on
failure. When POINT-SAFE is non-nil, the point must not be in the
matching string."
  (tlt-prettify-TeX-safe-excursion
   (let ((case-fold-search nil))
     (if backward
         (search-backward-regexp regex bound)
       (search-forward-regexp regex bound)))
   (or (save-match-data
         (save-excursion
           (and (goto-char (match-beginning 0))
                (not (and point-safe
                          (< (point) tlt-prettify-TeX-jit-point)
                          (< tlt-prettify-TeX-jit-point (match-end 0))))
                (looking-back "\\([^\\\\]\\|^\\)\\(\\\\\\\\\\)*" (point-min))
                (not (tlt-prettify-TeX-skip-comments-and-verbs backward)))))
       (tlt-prettify-TeX-search-regexp regex bound backward point-safe))))

(defun tlt-prettify-TeX-skip-blocks (n &optional exclusive backward brace-only)
  "Skip blocks forward until the point reaches n-level upwards.
examples:
 [n=1]
   inclusive (a b (|c d (e f) g) h) -> (a b (c d (e f) g)| h)
   exclusive (a b (|c d (e f) g) h) -> (a b (c d (e f) g|) h)
 [n=0]
   inclusive (a b (|c d (e f) g) h) -> (a b (c d (e f)| g) h)
   exclusive (a b (|c d (e f) g) h) -> (a b (c d (e f|) g) h)"
  (tlt-prettify-TeX-safe-excursion
   (save-match-data
     (condition-case nil
         (tlt-prettify-TeX-search-regexp
          (if brace-only
              "\\({\\)\\|\\(}\\)"
            "\\(\\\\begin\\>\\|{\\|\\[\\)\\|\\(\\\\end\\>\\|}\\|]\\)")
          nil backward)
       (error (error "unmatched blocks")))
     (setq n (if backward
                 (+ n
                    (if (match-beginning 1) -1 0)
                    (if (match-beginning 2) 1 0))
               (+ n
                  (if (match-beginning 1) 1 0)
                  (if (match-beginning 2) -1 0))))
     (cond ((< n 0)
            (error "unexpected end-of-block"))
           ((> n 0)
            (tlt-prettify-TeX-skip-blocks n exclusive backward brace-only))
           (exclusive
            (if backward
                (goto-char (match-end 0))
              (goto-char (match-beginning 0))))
           (t
            t)))))

(defun tlt-prettify-TeX-read-args (&optional option args)
  "Look forward something like \"[OPT]{ARG0}...{ARGn}}\" and
set (match-string k) to K-th ARG. this function does not move
the point."
  (while (and option (looking-at " *\\["))
    (tlt-prettify-TeX-skip-blocks 0))
  (when args
    (let (res)
      (dotimes (_ args)
        (unless (looking-at " *{")
          (error "too few arguments"))
        (push (match-end 0) res)
        (tlt-prettify-TeX-skip-blocks 0 nil nil t)
        (push (1- (point)) res))
      (setq res (reverse res))
      (set-match-data res)
      res)))

(defun tlt-prettify-TeX-command-matcher (regex &optional option args point-safe)
  "Generate a forward search function that matches something like
\"REGEX[OPT]{ARG1}...{ARGn}\" and moves the cursor just after the
NAME. (match-string 0) will be NAME and (match-string k) will be
K-th ARG if succeeded."
  `(lambda (&optional limit)
     (ignore-errors
       (tlt-prettify-TeX-search-command ,regex ,option ,args ,point-safe limit))))

(defun tlt-prettify-TeX-search-command (regex &optional option args point-safe limit)
  "(Internal function for `tlt-prettify-TeX-command-matcher')"
  (tlt-prettify-TeX-safe-excursion
   (tlt-prettify-TeX-search-regexp regex limit nil point-safe)
   (let ((beg (match-beginning 0))
         (end (match-end 0)))
     (condition-case nil
         (let* ((args (save-excursion (tlt-prettify-TeX-read-args option args)))
                (res (cons beg (cons end args))))
           (set-match-data res)
           res)
       (error (tlt-prettify-TeX-search-command regex option args point-safe limit))))))


;;; Color Latex commands

(defcustom tlt-prettify-TeX-enable-colors t
  "If non-nil, prettify “\\COLOR{TEXT}” and
“\\textcolor{COLOR}{TEXT}” to “TEXT” in COLOR.
COLORs are specified in `tlt-prettify-TeX-color-commands'."
  :type 'boolean
  :group 'tlt-prettify-TeX)

(defcustom tlt-prettify-TeX-color-commands
  '(
        ;; most important predefined colors by `xcolor' ;;
    ("white"       "255,255,255")
    ("yellow"      "255,255,0")
    ("red"         "255,0,0")
    ("magenta"     "255,0,255")
    ("blue"        "0,0,255")
    ("cyan"        "0,255,255")
    ("green"       "0,255,0")
    ("black"       "0,0,0")
    ("gray"        "128,128,128")
    ("darkgray"    "169,169,169")
    ("lightgray"   "211,211,211")
    ("brown"       "150,75,0")
    ("purple"      "160,32,240")
    )
  "List of colors to highlight LaTeX text.
Each element of the list should have (NAME VALUE) form, where
NAME is the name of a color, and VALUE is the “RED,GREEN,BLUE”
string of integers ranging from 0 to 255."
  :type '(repeat (list string string))
  :group 'tlt-prettify-TeX)

(defvar tlt-prettify-TeX-colors-generated nil
  "Non-nil means faces are already generated from `tlt-prettify-TeX-color-commands'.
Set this variable to nil if you wish to regenerate the faces;
e.g. after adding a new color to `tlt-prettify-TeX-color-commands'.")

(defun tlt-prettify-TeX-color-face-name (name)
  "Return NAME face."
  (intern (concat "tlt-prettify-TeX-" name "-command")))

(defmacro tlt-prettify-TeX-make-face (name color)
  "Create NAME face with foreground COLOR."
  (let ((face (tlt-prettify-TeX-color-face-name name)))
    `(defface ,face '((t (:foreground ,color)))
       ,(concat "Face for \\'" name "{}' LaTeX commands.")
       :group 'tlt-prettify-TeX)))

(defun tlt-prettify-TeX-color-dec-to-hex (str)
  "Convert decimal color STR to hexadecimal."
  (apply #'format "#%02x%02x%02x"
         (mapcar #'string-to-number
                 (split-string str ","))))

(defun tlt-prettify-TeX-generate-color-faces ()
  "Generate faces from `tlt-prettify-TeX-color-commands'."
  (unless tlt-prettify-TeX-colors-generated
    (mapc (lambda (entry)
            (let ((name (car entry))
                  (color (tlt-prettify-TeX-color-dec-to-hex (cadr entry))))
              (eval `(tlt-prettify-TeX-make-face ,name ,color))))
          tlt-prettify-TeX-color-commands)
    (setq tlt-prettify-TeX-colors-generated t)))

(defun tlt-prettify-TeX-color-put-invisible ()
  "Put invisible property to the current keyword match."
  (let ((beg      (match-beginning 0))
        (end      (match-end 0))
        (key-beg  (match-beginning 1))
        (key-end  (match-end 1)))
    (with-silent-modifications
      (put-text-property beg key-beg 'invisible t)
      (put-text-property key-end end 'invisible t))))

(defun tlt-prettify-TeX-color-keywords ()
  "Return keywords for `font-lock-keywords'."
  (cl-mapcan
   (lambda (entry)
     (let ((name (car entry)))
       `((,(concat "\\\\" name "{\\(.+?[^\\]\\)}") 0
          (progn (tlt-prettify-TeX-color-put-invisible)
                 ',(tlt-prettify-TeX-color-face-name name))
          prepend)
         (,(concat "\\\\textcolor{" name "}{\\(.+?[^\\]\\)}") 0
          (progn (tlt-prettify-TeX-color-put-invisible)
                 ',(tlt-prettify-TeX-color-face-name name))
          prepend))))
   tlt-prettify-TeX-color-commands))


;;; Block highlighters

(defun tlt-prettify-TeX-block-matcher (regex &optional option args point-safe)
  "Generate a forward search function that matches something like
\"REGEX[OPT]{ARG1}...{ARGn} ... BODY ... \\end{env}\" and moves
the cursor just after the NAME. (match-string 0) will be
NAME, (match-string 1) will be BODY, and (match-string (1+ k))
will be K-th ARG if succeeded."
  `(lambda (&optional limit)
     (ignore-errors
       (tlt-prettify-TeX-search-block ,regex ,option ,args ,point-safe limit))))

(defun tlt-prettify-TeX-search-block (regex &optional option args point-safe limit)
  "(Internal function for `tlt-prettify-TeX-block-matcher')"
  (tlt-prettify-TeX-safe-excursion
   (tlt-prettify-TeX-search-regexp regex limit nil point-safe)
   (let ((command-beg (match-beginning 0))
         (command-end (match-end 0)))
     (condition-case nil
         (save-excursion
           (let* ((res (tlt-prettify-TeX-read-args option args))
                  (content-beg (point))
                  (content-end (condition-case nil
                                   (progn (tlt-prettify-TeX-skip-blocks 1 t) (point))
                                 (error (1+ (buffer-size))))))
             (setq res (cons command-beg
                             (cons command-end
                                   (cons content-beg
                                         (cons content-end res)))))
             (set-match-data res)
             res))
       (error (tlt-prettify-TeX-search-block regex option args point-safe limit))))))

(defvar tlt-prettify-TeX-block-commands
  (let ((tiny (tlt-prettify-TeX-block-matcher "\\\\tiny\\>" nil nil))
        (script (tlt-prettify-TeX-block-matcher "\\\\scriptsize\\>" nil nil))
        (footnote (tlt-prettify-TeX-block-matcher "\\\\footnotesize\\>" nil nil))
        (small (tlt-prettify-TeX-block-matcher "\\\\small\\>" nil nil))
        (large (tlt-prettify-TeX-block-matcher "\\\\large\\>" nil nil))
        (llarge (tlt-prettify-TeX-block-matcher "\\\\Large\\>" nil nil))
        (xlarge (tlt-prettify-TeX-block-matcher "\\\\LARGE\\>" nil nil))
        (huge (tlt-prettify-TeX-block-matcher "\\\\huge\\>" nil nil))
        (hhuge (tlt-prettify-TeX-block-matcher "\\\\Huge\\>" nil nil))
        (type (tlt-prettify-TeX-block-matcher "\\\\tt\\>" nil nil))
        (italic (tlt-prettify-TeX-block-matcher "\\\\\\(?:em\\|it\\|sl\\)\\>" nil nil))
        (bold (tlt-prettify-TeX-block-matcher "\\\\bf\\(?:series\\)?\\>" nil nil)))
    `((,tiny . 'tlt-prettify-TeX-tiny)
      (,script . 'tlt-prettify-TeX-script)
      (,footnote . 'tlt-prettify-TeX-footnote)
      (,small . 'tlt-prettify-TeX-small)
      (,large . 'tlt-prettify-TeX-large)
      (,llarge . 'tlt-prettify-TeX-llarge)
      (,xlarge . 'tlt-prettify-TeX-xlarge)
      (,huge . 'tlt-prettify-TeX-huge)
      (,hhuge . 'tlt-prettify-TeX-hhuge)
      (,type . 'tlt-prettify-TeX-type)
      (,italic . 'italic)
      (,bold . 'bold)))
  "An alist of (MATCHER . FACE). MATCHER is a function that takes
an argument, limit of the search, and does a forward search like
`search-forward-regexp' then sets match-data as needed. FACE is
*a sexp* which evaluates to a face. (match-string 1) will be
propertized with the face.")

(defun tlt-prettify-TeX-make-block-overlay (command-beg command-end content-beg content-end &rest props)
  "Make a pair of overlays, a content overlay and a command
overlay. The command overlay will have `partner' property, that
points the content overlay which the command is associated
with. The content overlay will have PROPS as its properties."
  (let* ((ov1 (make-overlay command-beg command-end))
         (ov2 (make-overlay content-beg content-end)))
    (overlay-put ov1 'category 'tlt-prettify-TeX-ov-block)
    (overlay-put ov1 'partner ov2)
    (while props
      (overlay-put ov2 (car props) (cadr props))
      (setq props (cddr props)))
    ov2))

(defun tlt-prettify-TeX-remove-block-overlays (beg end)
  "Remove all command overlays and their content overlays from
BEG END."
  (dolist (ov (overlays-in beg end))
    (when (eq (overlay-get ov 'category) 'tlt-prettify-TeX-ov-block)
      (delete-overlay (overlay-get ov 'partner))
      (delete-overlay ov))))

(defun tlt-prettify-TeX-jit-block-highlighter (_ end)
  (when tlt-prettify-TeX-enable-block-highlight
    (condition-case nil
        (progn (tlt-prettify-TeX-skip-blocks 1 nil t) (point))
      (error (goto-char 1)))
    (tlt-prettify-TeX-remove-block-overlays (point) end)
    (dolist (command tlt-prettify-TeX-block-commands)
      (save-excursion
        (while (funcall (car command) end)
          (tlt-prettify-TeX-make-block-overlay (match-beginning 0) (match-end 0)
                                 (match-beginning 1) (match-end 1)
                                 'face (eval (cdr command))))))))


;;; Block aligners

(defvar tlt-prettify-TeX-align-commands
  `((,(tlt-prettify-TeX-block-matcher "\\\\\\(?:centering\\>\\|begin{center}\\)" nil nil) . center)
    (,(tlt-prettify-TeX-block-matcher "\\\\\\(?:raggedleft\\>\\|begin{flushleft}\\)" nil nil) . left)
    (,(tlt-prettify-TeX-block-matcher "\\\\\\(?:raggedright\\>\\|begin{flushright}\\)" nil nil) . right)
    (,(tlt-prettify-TeX-block-matcher "\\\\begin{\\(?:quot\\(?:e\\|ation\\)\\|leftbar\\)}" nil nil) . quote))
  "An alist of (MATCHER . POSITION). MATCHER is a function that
takes an argument, limit of the search, and does a forward search
like `search-forward-regexp' then sets match-data as
needed. POSITION can be one of 'center 'right 'left 'quote.")

(defun tlt-prettify-TeX-make-align-overlay (command-beg command-end content-beg content-end position)
  "Make a command overlay and alignment overlay(s) like
`tlt-prettify-TeX-make-block-overlay'. The command overlay will have `partners'
property, which is bound to the list of all alignment
overlay(s)."
  (save-excursion
    (goto-char content-end)
    (end-of-line 0)
    (setq content-end (point))
    (when (< content-beg content-end)
      (remove-overlays content-beg content-end 'category 'tlt-prettify-TeX-ov-align-alignment)
      (let ((ov (make-overlay command-beg command-end))
            ovs)
        (goto-char content-beg)
        (forward-line 1)
        (while (<= (point-at-eol) content-end)
          (cond ((ignore-errors
                   (tlt-prettify-TeX-search-regexp "{\\|\\\\begin\\_>" (min content-end (point-at-eol))))
                 (let* ((block-end (condition-case nil
                                       (save-excursion (tlt-prettify-TeX-skip-blocks 1) (point))
                                     (error (point-max))))
                        (bols (list (point-at-bol)))
                        (width (tlt-prettify-TeX-column-at-eol))
                        (indentation (tlt-prettify-TeX-column-at-indentation)))
                   (while (progn
                            (forward-line 1)
                            (< (point) block-end))
                     (push (point) bols)
                     (setq width       (max width (tlt-prettify-TeX-column-at-eol))
                           indentation (min indentation (tlt-prettify-TeX-column-at-indentation))))
                   (setq ovs (nconc (mapcar
                                     (lambda (p)
                                       (tlt-prettify-TeX-make-align-overlay-1 p width indentation position))
                                     bols)
                                    ovs))))
                (t
                 (push (tlt-prettify-TeX-make-align-overlay-1
                        (point) (tlt-prettify-TeX-column-at-eol) (tlt-prettify-TeX-column-at-indentation) position)
                       ovs)
                 (forward-line 1))))
        (overlay-put ov 'category 'tlt-prettify-TeX-ov-align)
        (overlay-put ov 'partners ovs)))))

(defun tlt-prettify-TeX-make-align-overlay-1 (pos width indentation position)
  "*internal function for `tlt-prettify-TeX-make-align-overlay'*"
  (let ((prop (cl-case position
                ((left) `((space :align-to left)))
                ((center) `((space :align-to (- center ,(/ width 2) ,(/ indentation 2)))))
                ((right) `((space :align-to (- right ,width))))
                ((quote) (propertize " " 'face 'tlt-prettify-TeX-lbar))))
        (ov (make-overlay pos pos)))
    (overlay-put ov 'before-string (propertize " " 'display prop))
    (overlay-put ov 'category 'tlt-prettify-TeX-ov-align-alignment)
    ov))

(defun tlt-prettify-TeX-remove-align-overlays (beg end)
  "Remove all command overlays and their alignment overlays
between BEG and END."
  (dolist (ov (overlays-in beg end))
    (when (eq (overlay-get ov 'category) 'tlt-prettify-TeX-ov-align)
      (mapc 'delete-overlay (overlay-get ov 'partners))
      (delete-overlay ov))))

(defun tlt-prettify-TeX-jit-block-aligner (_ end)
  (when tlt-prettify-TeX-enable-block-align
    (condition-case nil
        (progn (tlt-prettify-TeX-skip-blocks 1 nil t) (point))
      (error (goto-char 1)))
    (tlt-prettify-TeX-remove-align-overlays (point) end)
    (dolist (command tlt-prettify-TeX-align-commands)
      (save-excursion
        (while (funcall (car command) end)
          (tlt-prettify-TeX-make-align-overlay (match-beginning 0) (match-end 0)
                                 (match-beginning 1) (match-end 1)
                                 (cdr command)))))))


;;; Prettified symbols

(defvar tlt-prettify-TeX-symbols-no-spaces
  '(
    ("\\begin"                "▽")
    ("\\end"                  "△")
    ("=>"                     "⇒")
    ("\"`"                    "„")
    ("\"'"                    "“")
    ("\\mp"                   "∓")
    ("\\pm"                   "±")
    ("\\\\"                   "⏎")
    ("\\newline"              "⏎")

    ;; Propositional Logic ;;
    ("\\bot"                  "⊥")
    ("\\top"                  "⊤")
    ("\\to"                   "→")
    ("\\land"                 "∧")
    ("\\lor"                  "∨")
    ("\\nabla"                "∇")
    ("\\leftrightarrow"       "↔")
    ("\\equiv"                "≡")
    ("\\therefore"            "∴")
    ("\\because"              "∵")

    ;; Meta Language ;;
    ("\\eqdef"                "≝")
    ("\\Rightarrow"           "⇒")
    ("\\nRightarrow"          "⇏")
    ("\\Leftarrow"            "⇐")
    ("\\Leftrightarrow"       "⇔")
    ("\\nLeftrightarrow"      "⇎")
    ("\\models"               "⊧")
    ("\\vDash"                "⊨")
    ("\\vdash"                "⊢")
    ("\\dashv"                "⊣")
    ("\\sststile"             "⊢")
    ("\\centernot{\\sststile" "⊬")
    ("\\sdtstile"             "⊨")
    ("\\centernot{\\sdtstile" "⊭")
    ("\\implies"              "⟹")

    ;; Set Theory ;;
    ("\\emptyset"             "∅")
    ("\\infty"                "∞")
    ("\\ninfty"               "⧞")
    ("\\aleph"                "ℵ")

    ;; Misc ;;
    ("\\mid"                  "|")
    ("\\squigarrowright"      "⇝")
    ("\\cdot"                 "⋅")
    ("\\cdots"                "⋯")
    ("\\dots"                 "…")
    ("\\ldots"                "…")
    ("\\vdots"                "⋮")
    ("\\ddots"                "⋱")
    ("\\prod"                 "∏")
    ("\\coprod"               "∐")
    ("\\sum"                  "∑")
    ("\\odot"                 "⊙")
    ("\\oslash"               "⊘")
    ("\\ominus"               "⊖")
    ("\\oplus"                "⊕")
    ("\\otimes"               "⊗")
    ("\\coloneq"              "≔")
    ("\\doteq"                "≐")
    ("\\simeq"                "≃")
    ("\\approx"               "≈")
    ("\\ll"                   "≪")
    ("\\gg"                   "≫")
    ("\\colon"                ":")
    ("\\propto"               "∝")
    ("\\wedge"                "∧")
    ("\\bigwedge"             "⋀")
    ("\\vee"                  "∨")
    ("\\bigvee"               "⋁")
    ("\\ast"                  "∗")

    ;; Quoting Symbols ;;
    ("\\glq"                  "‚")
    ("\\grq"                  "‘")
    ("\\flq"                  "‹")
    ("\\frq"                  "›")
    ("\\flqq"                 "«")
    ("\\frqq"                 "»")

    ;; Sets of Numbers ;;
    ("\\mathbb{N}"            "ℕ")
    ("\\mathbb{P}"            "ℙ")
    ("\\mathbb{Z}"            "ℤ")
    ("\\mathbb{Q}"            "ℚ")
    ("\\mathbb{Rangle}"       "ℝ")
    ("\\mathbb{C}"            "ℂ")
    ("\\Bbb{N}"               "ℕ")
    ("\\Bbb{P}"               "ℙ")
    ("\\Bbb{Z}"               "ℤ")
    ("\\Bbb{Q}"               "ℚ")
    ("\\Bbb{Rangle}"          "ℝ")
    ("\\Bbb{C}"               "ℂ")

    ;; Fraktur Uppercase ;;
    ("\\mathfrak{A}"          "𝔄")
    ("\\mathfrak{B}"          "𝔅")
    ("\\mathfrak{C}"          "ℭ")
    ("\\mathfrak{D}"          "𝔇")
    ("\\mathfrak{E}"          "𝔈")
    ("\\mathfrak{F}"          "𝔉")
    ("\\mathfrak{G}"          "𝔊")
    ("\\mathfrak{H}"          "ℌ")
    ("\\mathfrak{I}"          "ℑ")
    ("\\mathfrak{J}"          "𝔍")
    ("\\mathfrak{K}"          "𝔎")
    ("\\mathfrak{L}"          "𝔏")
    ("\\mathfrak{M}"          "𝔐")
    ("\\mathfrak{N}"          "𝔑")
    ("\\mathfrak{O}"          "𝔒")
    ("\\mathfrak{P}"          "𝔓")
    ("\\mathfrak{Q}"          "𝔔")
    ("\\mathfrak{R}"          "ℜ")
    ("\\mathfrak{S}"          "𝔖")
    ("\\mathfrak{T}"          "𝔗")
    ("\\mathfrak{U}"          "𝔘")
    ("\\mathfrak{V}"          "𝔙")
    ("\\mathfrak{W}"          "𝔚")
    ("\\mathfrak{X}"          "𝔛")
    ("\\mathfrak{Y}"          "𝔜")
    ("\\mathfrak{Z}"          "ℨ")

    ;; Fraktur Lowercase ;;
    ("\\mathfrak{a}"          "𝔞")
    ("\\mathfrak{b}"          "𝔟")
    ("\\mathfrak{c}"          "𝔠")
    ("\\mathfrak{d}"          "𝔡")
    ("\\mathfrak{e}"          "𝔢")
    ("\\mathfrak{f}"          "𝔣")
    ("\\mathfrak{g}"          "𝔤")
    ("\\mathfrak{h}"          "𝔥")
    ("\\mathfrak{i}"          "𝔦")
    ("\\mathfrak{j}"          "𝔧")
    ("\\mathfrak{k}"          "𝔨")
    ("\\mathfrak{l}"          "𝔩")
    ("\\mathfrak{m}"          "𝔪")
    ("\\mathfrak{n}"          "𝔫")
    ("\\mathfrak{o}"          "𝔬")
    ("\\mathfrak{p}"          "𝔭")
    ("\\mathfrak{q}"          "𝔮")
    ("\\mathfrak{r}"          "𝔯")
    ("\\mathfrak{s}"          "𝔰")
    ("\\mathfrak{t}"          "𝔱")
    ("\\mathfrak{u}"          "𝔲")
    ("\\mathfrak{v}"          "𝔳")
    ("\\mathfrak{w}"          "𝔴")
    ("\\mathfrak{x}"          "𝔵")
    ("\\mathfrak{y}"          "𝔶")
    ("\\mathfrak{z}"          "𝔷")

    ;; Greek Uppercase ;;
    ("\\Alpha"                "Α")
    ("\\Beta"                 "Β")
    ("\\Gamma"                "Γ")
    ("\\Delta"                "Δ")
    ("\\Epsilon"              "Ε")
    ("\\Zeta"                 "Ζ")
    ("\\Eta"                  "Η")
    ("\\Theta"                "Θ")
    ("\\Iota"                 "Ι")
    ("\\Kappa"                "Κ")
    ("\\Lambda"               "Λ")
    ("\\Mu"                   "Μ")
    ("\\Nu"                   "Ν")
    ("\\Xi"                   "Ξ")
    ("\\Omicron"              "Ο")
    ("\\Pi"                   "Π")
    ("\\Rho"                  "Ρ")
    ("\\Sigma"                "Σ")
    ("\\Tau"                  "Τ")
    ("\\Upsilon"              "Υ")
    ("\\Phi"                  "Φ")
    ("\\Chi"                  "Χ")
    ("\\Psi"                  "Ψ")
    ("\\Omega"                "Ω")

    ;; Greek Lowercase ;;
    ("\\alpha"                "α")
    ("\\beta"                 "β")
    ("\\gamma"                "γ")
    ("\\delta"                "δ")
    ("\\epsilon"              "ε")
    ("\\zeta"                 "ζ")
    ("\\eta"                  "η")
    ("\\theta"                "θ")
    ("\\vartheta"             "ϑ")
    ("\\iota"                 "ι")
    ("\\kappa"                "κ")
    ("\\lambda"               "λ")
    ("\\mu"                   "μ")
    ("\\nu"                   "ν")
    ("\\xi"                   "ξ")
    ("\\omicron"              "ο")
    ("\\pi"                   "π")
    ("\\rho"                  "ρ")
    ("\\varrho"               "ϱ")
    ("\\sigma"                "σ")
    ("\\tau"                  "τ")
    ("\\upsilon"              "υ")
    ("\\phi"                  "ϕ")
    ("\\varphi"               "φ")
    ("\\chi"                  "χ")
    ("\\psi"                  "ψ")
    ("\\omega"                "ω")

    ;; Analysis ;;
    ("\\Re"                   "ℜ")
    ("\\Im"                   "ℑ")
    ("\\angle"                "∠")
    ("\\surd"                 "√")
    ("\\sqrt"                 "√")
    ("\\partial"              "∂")
    ("\\int"                  "∫")
    ("\\iint"                 "∬")
    ("\\iiint"                "∭")
    ("\\iiiint"               "⨌")
    ("\\oint"                 "∮")
    ("\\oiint"                "∯")
    ("\\oiiint"               "∰")
    ("\\ointclockwise"        "∲")
    ("\\ointctrclockwise"     "∳")

    ;; Misc ;;
    ("\\par"                  "¶")
    ("\\P"                    "¶")
    ("\\S"                    "§")
    ("\\%"                    "％")
    ("\\#"                    "＃")
    ("\\_"                    "＿")
    ("\\wp"                   "≀")
    ("\\bullet"               "•")
    ("\\circ"                 "∘")
    ("\\bigcirc"              "◯")
    ("\\star"                 "★")
    ("\\triangle"             "△")
    ("\\triangleleft"         "◁")
    ("\\triangleright"        "▷")
    ("\\trianglelefteq"       "⊴")
    ("\\trianglerighteq"      "⊵")
    ("\\bigtriangleup"        "△")
    ("\\bigtriangledown"      "▽")
    ("\\o"                    "ø")
    ("\\O"                    "Ø")
    ("\\ae"                   "æ")
    ("\\oe"                   "œ")
    ("\\AE"                   "Æ")
    ("\\OE"                   "Œ")
    ("\\hbar"                 "ℏ")
    ("\\ell"                  "ℓ")
    ("\\wp"                   "℘")
    ("\\l"                    "ł")
    ("\\L"                    "Ł")
    ("\\pounds"               "£")
    )
  "Symbols without left/right spaces.")

(defvar tlt-prettify-TeX-symbols-left-space
  '(
    ;; Right Brackets ;;
    ("\\rrbracket"            "⟧")
    ("\\urcorner"             "⌝")
    ("\\rangle"               "⟩")
    ("\\}"                    "❵")
    ("\\}"                    "}")
    )
  "Symbols with a space to the left.")

(defvar tlt-prettify-TeX-symbols-right-space
  '(
    ;; Propositional Logic ;;
    ("\\til"                  "~")
    ("\\lnot"                 "¬")
    ("\\neg"                  "¬")

    ;; Predicate Logic ;;
    ("\\exists"               "∃")
    ("\\forall"               "∀")
    ("\\turnediota"           "℩")

    ;; Modal Logic ;;
    ("\\Box"                  "◻")
    ("\\Diamond"              "◇")
    ("\\Blackdiamond"         "◆")
    ("\\downarrow"            "↓")

    ;; Left Brackets ;;
    ("\\llbracket"            "⟦")
    ("\\ulcorner"             "⌜")
    ("\\langle"               "⟨")
    ("\\{"                    "❴")
    ("\\{"                    "{")
    )
  "Symbols with a space to the right.")

(defvar tlt-prettify-TeX-symbols-both-spaces
  '(
    ("\\times"                "×")
    ("\\div"                  "÷")

    ;; Predicate Logic ;;
    ("\\neq"                  "≠")

    ;; Plural Logic ;;
    ("\\preccurlyeq"          "≼")
    ("\\not\\preccurlyeq"     "⋠")
    ("\\prec"                 "≺")
    ("\\not\\prec"            "⊀")
    ("\\succcurlyeq"          "≽")
    ("\\not\\succcurlyeq"     "⋡")
    ("\\succ"                 "≻")
    ("\\not\\succ"            "⊁")

    ;; Order Theory ;;
    ("\\leq"                  "≤")
    ("\\nleq"                 "≰")
    ("\\nless"                "≮")
    ("\\geq"                  "≥")
    ("\\ngeq"                 "≱")
    ("\\ngeq"                 "≯")

    ;; Set Theory ;;
    ("\\in"                   "∈")
    ("\\nel"                  "∉")
    ("\\notin"                "∉")
    ("\\ni"                   "∋")
    ("\\nni"                  "∌")
    ("\\subseteq"             "⊆")
    ("\\nsubseteq"            "⊈")
    ("\\subsetneq"            "⊊")
    ("\\subset"               "⊂")
    ("\\nsubset"              "⊄")
    ("\\nteilm"               "⊄")
    ("\\supseteq"             "⊇")
    ("\\supsetneq"            "⊋")
    ("\\nsupseteq"            "⊉")
    ("\\supset"               "⊃")
    ("\\nsupset"              "⊅")
    ("\\cap"                  "∩")
    ("\\bigcap"               "⋂")
    ("\\cup"                  "∪")
    ("\\bigcup"               "⋃")
    ("\\cong"                 "≅")
    ("\\simneqq"              "≆")
    ("\\ncong"                "≇")
    ("\\sqsubseteq"           "⊑")
    ("\\sqsupseteq"           "⊒")
    ("\\setminus"             "＼")
    ("\\sqcup"                "⊔")
    ("\\bigsqcup"             "∐")
    ("\\sqcap"                "⊓")
    ("\\bigsqcap"             "∏")
    ("\\uplus"                "⊎")
    ("\\biguplus"             "⨄")
    ("\\amalg"                "⨿")
    ("\\mapsto"               "⟼")
    )
  "Symbols with spaces on both sides.")

(defvar tlt-prettify-TeX-arrow-symbols
  '(
    ;; Harpoons ;;
    ("\\leftharpoonup"        "↼")
    ("\\rightharpoonup"       "⇀")
    ("\\leftharpoondown"      "↽")
    ("\\rightharpoondown"     "⇁")
    ("\\leftrightharpoons"    "⇋")
    ("\\rightleftharpoons"    "⇌")
    ("\\upharpoonleft"        "↿")
    ("\\upharpoonright"       "↾")
    ("\\downharpoonleft"      "⇃")
    ("\\downharpoonright"     "⇂")

    ;; Long Arrows ;;;
    ("\\longrightarrow"       "⟶")
    ("\\longleftarrow"        "⟵")
    ("\\Longrightarrow"       "⟹")
    ("\\Longleftarrow"        "⟸")
    ("\\longmapsto"           "⟼")
    ("\\longleftrightarrow"   "⟷")
    ("\\Longleftrightarrow"   "⟺")

    ;; 45 Degrees Arrows ;;
    ("\\nearrow"              "↗")
    ("\\searrow"              "↘")
    ("\\nwarrow"              "↖")
    ("\\swarrow"              "↙")

    ;; 90 Degrees Arrows ;;
    ("\\uparrow"              "↑")
    ("\\updownarrow"          "↕")
    ("\\upuparrows"           "⇈")
    ("\\downdownarrows"       "⇊")
    ("\\Uparrow"              "⇑")
    ("\\Downarrow"            "⇓")
    ("\\Updownarrow"          "⇕")

    ;; Misc Arrows ;;
    ("\\hookleftarrow"        "↩")
    ("\\hookrightarrow"       "↪")
    ("\\twoheadleftarrow"     "↞")
    ("\\twoheadrightarrow"    "↠")
    ("\\looparrowleft"        "↫")
    ("\\looparrowright"       "↬")
    ("\\rightsquigarrow"      "⇝")
    ("\\leftrightsquigarrow"  "↭")
    ("\\leftleftarrows"       "⇇")
    ("\\rightrightarrows"     "⇉")
    ("\\leftrightarrows"      "⇆")
    ("\\rightleftarrows"      "⇄")
    ("\\Lleftarrow"           "⇚")
    ("\\Rrightarrow"          "⇛")
    ))


(defvar tlt-prettify-TeX-multichar-symbols
  '(
    ("\\multicolumn"          "|↔|")
    ))

(defvar tlt-prettify-TeX-accents
  `(("\\\\\\(?:mathbb\\){\\([^}]\\)}"
     ;; . (let ((ch (string-to-char (match-string 1)))) (compose-chars ch '(cc cl -85 0) ch))
     . (let ((ch (string-to-char (match-string 1)))) (compose-chars ch '(cc cl -96 0) ch))
     )
    ("\\\\\\(?:vec\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 60) ?→))
    ("\\\\\\(?:tilde\\|~\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 60) ?~))
    ("\\\\\\(?:bar\\|=\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 60) ?-))
    ("\\\\\\(?:dot\\|\\.\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 60) ?・))
    ("\\\\\\(?:hat\\|\\^\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 45) ?^))
    ("\\\\\\(?:acute\\|'\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 45) ?'))
    ("\\\\\\(?:\"\\|H\\|ddot\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 45) ?\"))
    ("\\\\\\(?:grave\\|`\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 30) ?`))
    ("\\\\\\(?:check\\|`\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 60) ?v))
    ("\\\\\\(?:breve\\|`\\){\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 60) ?⌣))
    ("\\\\r{\\([^}]\\)}"
     . (compose-chars (string-to-char (match-string 1)) '(cc Bc 0 60) ?o))
    ))

(defvar tlt-prettify-TeX-symbol-regexps
  (append (tlt-prettify-TeX-quote-latex-macros tlt-prettify-TeX-symbols-no-spaces)
          (tlt-prettify-TeX-quote-latex-macros tlt-prettify-TeX-symbols-left-space :head t)
          (tlt-prettify-TeX-quote-latex-macros tlt-prettify-TeX-symbols-right-space :tail t)
          (tlt-prettify-TeX-quote-latex-macros tlt-prettify-TeX-symbols-both-spaces :head t :tail t)
          (tlt-prettify-TeX-quote-latex-macros tlt-prettify-TeX-arrow-symbols)
          (tlt-prettify-TeX-quote-latex-macros tlt-prettify-TeX-multichar-symbols))
  "Alist of (REGEXP . EXPR).
It is used by `tlt-prettify-TeX' and by LaTeX to unicode conversion.")

(defun tlt-prettify-TeX-symbols ()
  "Return alist of (REGEXP . EXPR). REGEXP is a regular expression
that matches to a command that will be prettified, and EXPR is *a
sexp* which evaluates to a display string for the command. You
can assume that the expressions are evaluated immediately after
regex search, so that you can use match data in the
expressions."
  (append (mapcar (lambda (pattern)
                    (cons (concat "\\\\not[ \t\n]*" (car pattern))
                          (compose-string (concat "／" (cdr pattern)))))
                  (tlt-prettify-TeX-quote-latex-macros tlt-prettify-TeX-arrow-symbols))
          tlt-prettify-TeX-symbol-regexps
          tlt-prettify-TeX-accents))

(defun tlt-prettify-TeX-make-pretty-overlay (from to &rest props)
  "Make an overlay from FROM to TO, that has PROPS as its
properties. The overlay is removed as soon as the text between
FROM and TO is modified."
  (let* ((ov (make-overlay from to))
         (hooks (list `(lambda (&rest _) (delete-overlay ,ov)))))
    (overlay-put ov 'category 'tlt-prettify-TeX-ov-pretty)
    (overlay-put ov 'modification-hooks hooks)
    (overlay-put ov 'insert-in-front-hooks hooks)
    (while props
      (overlay-put ov (car props) (cadr props))
      (setq props (cddr props)))
    ov))

(defun tlt-prettify-TeX-remove-pretty-overlays (beg end)
  "Remove all overlays created with `tlt-prettify-TeX-make-pretty-overlay'
between BEG and END."
  (remove-overlays beg end 'category 'tlt-prettify-TeX-ov-pretty))

(defun tlt-prettify-TeX-search-suscript (point-safe limit)
  "Search forward something like \"^{BODY}\" or \"_{BODY}\" and
set (match-string 0) to \"_\" or \"^\", (match-string 1) to
\"{BODY}\". when POINT-SAFE is non-nil, the point must not be in
the command name."
  (tlt-prettify-TeX-safe-excursion
   (tlt-prettify-TeX-search-regexp
    ;; 1( _^ delims ) 2(  character   |   \  (       command-name          )  | 3( { )  )
    "\\([_^][ \t]*\\)\\([^ \t\n\\{}]\\|\\\\\\(?:[a-zA-Z@]+\\**\\|[^ \t\n]\\)\\|\\({\\)\\)"
    limit nil point-safe)
   (let ((delim-beg (match-beginning 1))
         (delim-end (match-end 1))
         (body-beg (match-beginning 2))
         (body-end (match-end 2))
         (brace-beg (match-beginning 3)))
     (condition-case nil
         (let* ((brace-end (when brace-beg
                             (tlt-prettify-TeX-skip-blocks 1 nil nil t)
                             (point)))
                (res (list delim-beg delim-end body-beg
                           (or brace-end body-end))))
           (goto-char delim-end)
           (set-match-data res)
           res)
       (error (tlt-prettify-TeX-search-suscript point-safe limit))))))

(defun tlt-prettify-TeX-jit-prettifier (beg end)
  (goto-char beg)
  (tlt-prettify-TeX-remove-pretty-overlays beg end)
  ;; prettify suscripts
  (when tlt-prettify-TeX-enable-suscript
    (save-excursion
      (while (ignore-errors (tlt-prettify-TeX-search-suscript t end))
        (let* ((body-beg (match-beginning 1))
               (body-end (match-end 1))
               (delim-beg (match-beginning 0))
               (delim-end (match-end 0))
               ;; the point can be already prettified in a recursive
               ;; suscript like "a_{b_c}".
               (oldov (tlt-prettify-TeX-overlay-at body-beg 'category 'tlt-prettify-TeX-ov-pretty))
               (oldprop (and oldov (overlay-get oldov 'display)))
               (priority-base (and oldov (or (overlay-get oldov 'priority) 0)))
               (raise-base (or (cadr (assoc 'raise oldprop)) 0.0))
               (height-base (or (cadr (assoc 'height oldprop)) 1.0))
               (_ (tlt-prettify-TeX-make-pretty-overlay delim-beg delim-end 'invisible t))
               ;; new overlay must have higher priority than the old
               ;; one.
               (ov (tlt-prettify-TeX-make-pretty-overlay
                    body-beg body-end 'priority (when oldov (1+ priority-base)))))
          (cl-case (string-to-char (match-string 0))
            ((?_) (overlay-put
                   ov 'display
                   `((raise ,(- raise-base 0.2)) (height ,(* height-base 0.8)))))
            ((?^) (overlay-put
                   ov 'display
                   `((raise ,(+ raise-base 0.2)) (height ,(* height-base 0.8))))))))))
  ;; prettify symbols
  (when tlt-prettify-TeX-enable-pretty-symbols
    (dolist (symbol (tlt-prettify-TeX-symbols))
      (save-excursion
        (let ((regex (car symbol)))
          (while (ignore-errors (tlt-prettify-TeX-search-regexp regex end nil t))
            (let* ((oldov (tlt-prettify-TeX-overlay-at (match-beginning 0) 'category 'tlt-prettify-TeX-ov-pretty))
                   (priority-base (and oldov (or (overlay-get oldov 'priority) 1)))
                   (oldprop (and oldov (overlay-get oldov 'display))))
              (unless (stringp oldprop)
                (tlt-prettify-TeX-make-pretty-overlay
                 (match-beginning 0) (match-end 0)
                 'priority (when oldov (1+ priority-base))
                 'display (propertize (eval (cdr symbol)) 'display oldprop))))))))))

(defun tlt-prettify-TeX-post-command-function ()
  (let ((ov (tlt-prettify-TeX-overlay-at (point) 'category 'tlt-prettify-TeX-ov-pretty))
        (message-log-max nil)) ; do not to insert to *Messages* buffer
    (when (and ov tlt-prettify-TeX-enable-minibuffer-echo)
      (message (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))))


;;; Minor mode and its commands

(defcustom tlt-prettify-TeX-enable-delete-LaTeX-macros t
  "Control the behavior of delete-char commands.
If nil, `tlt-prettify-TeX-delete-forward-char' and
`tlt-prettify-TeX-delete-backward-char' always delete a single character.
If non-nil, these commands delete a LaTeX macro if it is
displayed as a Unicode character."
  :type 'boolean
  :group 'tlt-prettify-TeX)

(defvar tlt-prettify-TeX-macro-chars "A-Za-z@*"
  "Characters used in a TeX macro name.
This string should be compatible with `skip-chars-forward'.")

(defun tlt-prettify-TeX-TeX-escaped-p (&optional pos)
  "Return t if the character at position POS is escaped."
  ;; This is the same as `TeX-escaped-p' function from "Auctex" package
  ;; (which we do not require).
  (save-excursion
    (when pos (goto-char pos))
    (not (zerop (mod (skip-chars-backward "\\\\") 2)))))

(defun tlt-prettify-TeX-find-macro-boundaries ()
  "Return a cons cell containing the start and end of a macro.
This function is similar to `TeX-find-macro-boundaries' but it does
not consider arguments enclosed in brackets or braces part of the
macro."
  (let ((orig-point (point))
        start-point)
    (save-excursion
      ;; Point is located directly at the start of a macro. (-!-\foo{bar})
      (when (and (eq (char-after) ?\\)
                 (not (tlt-prettify-TeX-TeX-escaped-p)))
        (setq start-point (point)))
      ;; Point is located on a macro. (\fo-!-o{bar})
      (unless start-point
        (skip-chars-backward tlt-prettify-TeX-macro-chars)
        (when (and (eq (char-before) ?\\)
                   (not (tlt-prettify-TeX-TeX-escaped-p (1- (point)))))
          (setq start-point (1- (point)))))
      ;; Search forward for the end of the macro.
      (when start-point
        (goto-char start-point)
        (forward-char)
        (skip-chars-forward tlt-prettify-TeX-macro-chars)
        (and (< orig-point (point))
             (cons start-point (point)))))))

(defun tlt-prettify-TeX-delete-forward-char ()
  "Delete the following character.
If the next character is a symbol prettified with
`tlt-prettify-TeX', then remove this symbol.  Otherwise,
fallback to `delete-char'."
  (interactive)
  (cond
   ((and (use-region-p)
	 delete-active-region)
    (if (eq delete-active-region 'kill)
        (kill-region (region-beginning) (region-end) 'region)
      (funcall region-extract-function 'delete-only)))
   (tlt-prettify-TeX-enable-delete-LaTeX-macros
    (let ((lbound- 0) (rbound+ 0)
          bounds next-char)
      (if (and (bound-and-true-p tlt-prettify-TeX)
               (tlt-prettify-TeX-overlay-at (point) 'category 'tlt-prettify-TeX-ov-pretty)
               (save-excursion
                 (setq next-char (char-after))
                 (when (eq ?\s next-char)
                   (forward-char)
                   (setq lbound- 1))
                 (setq bounds (tlt-prettify-TeX-find-macro-boundaries))
                 (and bounds
                      (progn
                        (goto-char (cdr bounds))
                        (eq ?\s (char-after)))
                      (tlt-prettify-TeX-overlay-at (point) 'category 'tlt-prettify-TeX-ov-pretty)
                      (setq rbound+ 1))
                 bounds))
          (delete-region (- (car bounds) lbound-)
                         (+ (cdr bounds) rbound+))
        (delete-char 1))))
   (t (delete-char 1))))

(defun tlt-prettify-TeX-delete-backward-char ()
  "Delete the previous character.
If the previous character is a symbol prettified with
`tlt-prettify-TeX', then remove this symbol.  Otherwise,
fallback to `delete-char'."
  (interactive)
  ;; The piece to delete region is taken from `delete-backward-char'.
  (cond
   ((and (use-region-p)
	 delete-active-region)
    (if (eq delete-active-region 'kill)
        (kill-region (region-beginning) (region-end) 'region)
      (funcall region-extract-function 'delete-only)))
   (tlt-prettify-TeX-enable-delete-LaTeX-macros
    (let ((lbound- 0) (rbound+ 0)
          bounds prev-char)
      (if (and (bound-and-true-p tlt-prettify-TeX)
               (tlt-prettify-TeX-overlay-at (1- (point)) 'category 'tlt-prettify-TeX-ov-pretty)
               (save-excursion
                 (setq prev-char (char-before))
                 (backward-char)
                 (when (eq ?\s prev-char)
                   (backward-char)
                   (setq rbound+ 1))
                 (setq bounds (tlt-prettify-TeX-find-macro-boundaries))
                 (and bounds
                      (progn
                        (goto-char (car bounds))
                        (eq ?\s (char-before)))
                      (tlt-prettify-TeX-overlay-at (1- (point)) 'category 'tlt-prettify-TeX-ov-pretty)
                      (setq lbound- 1))
                 bounds))
          (delete-region (- (car bounds) lbound-)
                         (+ (cdr bounds) rbound+))
        (delete-char -1))))
   (t (delete-char -1))))

(defun tlt-prettify-TeX-delete-macro (n)
  "Delete LaTeX macro.
If N is equal to 1, delete the following macro.
If N is equal to -1, delete the previous macro."
  (let (bounds)
    (if (and (= (abs n) 1)
             (save-excursion
               (when (= n -1) (backward-char))
               (setq bounds (tlt-prettify-TeX-find-macro-boundaries))))
        (delete-region (car bounds) (cdr bounds))
      (delete-char n))))

(defun tlt-prettify-TeX-delete-next-macro-or-char ()
  "Delete the following LaTeX macro or character."
  (interactive)
  (tlt-prettify-TeX-delete-macro 1))

(defun tlt-prettify-TeX-delete-previous-macro-or-char ()
  "Delete the previous LaTeX macro or character."
  (interactive)
  (tlt-prettify-TeX-delete-macro -1))

(defvar tlt-prettify-TeX-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backspace>")   'tlt-prettify-TeX-delete-backward-char)
    (define-key map (kbd "<delete>")      'tlt-prettify-TeX-delete-forward-char)
    (define-key map (kbd "<S-backspace>") 'tlt-prettify-TeX-delete-previous-macro-or-char)
    (define-key map (kbd "<S-delete>")    'tlt-prettify-TeX-delete-next-macro-or-char)
    map)
  "Keymap for `tlt-prettify-TeX'.")

;;;###autoload
(define-minor-mode tlt-prettify-TeX-mode
  "Minor mode to make logical/mathematical LaTeX documents more readable. Transforms LaTeX symbol commands (e.g. “\\exists”) to the respective Unicode symbols they would print (in this case, “∃”), and overlays both textcolor and custom color commands (e.g., both “\\textcolor{green}{foo}” and “\\green{foo}” become “foo” highlighted in green).

Key bindings provided by `tlt-prettify-TeX-mode':
\\{tlt-prettify-TeX-map}
"
  :init-value nil
  :global nil
  :lighter " tlt"
  (if tlt-prettify-TeX-mode
      (progn
        (add-hook 'post-command-hook 'tlt-prettify-TeX-post-command-function nil t)
        (jit-lock-mode 1)
        (setq-local font-lock-multiline t)
        (set-syntax-table tlt-prettify-TeX-syntax-table)
        (tlt-prettify-TeX-generate-color-faces)
        (when tlt-prettify-TeX-enable-colors
          (font-lock-add-keywords nil (tlt-prettify-TeX-color-keywords)))
        (jit-lock-register 'tlt-prettify-TeX-jit-prettifier)
        (jit-lock-register 'tlt-prettify-TeX-jit-block-highlighter)
        (jit-lock-register 'tlt-prettify-TeX-jit-block-aligner)
        ;; our prettifiers assume that the region is already fontified
        ;; (to recognize verbatim environments, constants and
        ;; comments), thus we need to push `font-lock-fontify-region'
        ;; before our prettifiers.
        (jit-lock-register 'font-lock-fontify-region)
        (set (make-local-variable 'iimage-mode-image-regex-alist)
             `((,(concat "\\\\includegraphics[\s\t]*\\(?:\\[[^]]*\\]\\)?[\s\t]*"
                         "{\\(" iimage-mode-image-filename-regex "\\)}") . 1)))
        (when tlt-prettify-TeX-enable-inline-image (iimage-mode 1)))
    (remove-hook 'post-command-hook 'tlt-prettify-TeX-post-command-function t)
    (set-syntax-table tex-mode-syntax-table)
    (font-lock-remove-keywords nil (tlt-prettify-TeX-color-keywords))
    (jit-lock-unregister 'tlt-prettify-TeX-jit-prettifier)
    (jit-lock-unregister 'tlt-prettify-TeX-jit-block-highlighter)
    (jit-lock-unregister 'tlt-prettify-TeX-jit-block-aligner)
    (jit-lock-unregister 'font-lock-fontify-region)
    (tlt-prettify-TeX-remove-block-overlays (point-min) (point-max))
    (tlt-prettify-TeX-remove-pretty-overlays (point-min) (point-max))
    (tlt-prettify-TeX-remove-align-overlays (point-min) (point-max))
    (font-lock-refresh-defaults)
    (iimage-mode -1)))

(provide 'tlt-prettify-TeX)

;; tlt-prettify-TeX.el ends here
