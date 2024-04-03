;;; org-obsolete9.0.el --- Obsolete Org mode functions and variables -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, wp
;; URL: https://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains obsolete Org mode code that will be removed in
;; future releases.

;;; Code:

;;;; Obsolete link types

(eval-after-load 'ol
  '(progn
     (org-link-set-parameters "file+emacs") ;since Org 9.0
     (org-link-set-parameters "file+sys"))) ;since Org 9.0

;;;; Obsolete aliases

;;;; XEmacs compatibility, now removed.
(define-obsolete-function-alias 'org-activate-mark 'activate-mark "9.0")
(define-obsolete-function-alias 'org-add-hook 'add-hook "9.0")
(define-obsolete-function-alias 'org-bound-and-true-p 'bound-and-true-p "9.0")
(define-obsolete-function-alias 'org-decompose-region 'decompose-region "9.0")
(define-obsolete-function-alias 'org-defvaralias 'defvaralias "9.0")
(define-obsolete-function-alias 'org-detach-overlay 'delete-overlay "9.0")
(define-obsolete-function-alias 'org-file-equal-p 'file-equal-p "9.0")
(define-obsolete-function-alias 'org-float-time 'float-time "9.0")
(define-obsolete-function-alias 'org-indent-line-to 'indent-line-to "9.0")
(define-obsolete-function-alias 'org-indent-to-column 'indent-to-column "9.0")
(define-obsolete-function-alias 'org-looking-at-p 'looking-at-p "9.0")
(define-obsolete-function-alias 'org-looking-back 'looking-back "9.0")
(define-obsolete-function-alias 'org-match-string-no-properties 'match-string-no-properties "9.0")
(define-obsolete-function-alias 'org-propertize 'propertize "9.0")
(define-obsolete-function-alias 'org-select-frame-set-input-focus 'select-frame-set-input-focus "9.0")

;;;; Functions from cl-lib that Org used to have its own implementation of.
(define-obsolete-function-alias 'org-count 'cl-count "9.0")
(define-obsolete-function-alias 'org-every 'cl-every "9.0")
(define-obsolete-function-alias 'org-find-if 'cl-find-if "9.0")
(define-obsolete-function-alias 'org-reduce 'cl-reduce "9.0")
(define-obsolete-function-alias 'org-remove-if 'cl-remove-if "9.0")
(define-obsolete-function-alias 'org-remove-if-not 'cl-remove-if-not "9.0")
(define-obsolete-function-alias 'org-some 'cl-some "9.0")
(define-obsolete-function-alias 'org-floor* 'cl-floor "9.0")

;;;; Functions available since Emacs 24.3
(define-obsolete-function-alias 'org-buffer-narrowed-p 'buffer-narrowed-p "9.0")
(define-obsolete-function-alias 'org-called-interactively-p 'called-interactively-p "9.0")
(define-obsolete-function-alias 'org-char-to-string 'char-to-string "9.0")
(define-obsolete-function-alias 'org-delete-directory 'delete-directory "9.0")
(define-obsolete-function-alias 'org-format-seconds 'format-seconds "9.0")
(define-obsolete-function-alias 'org-link-escape-browser 'url-encode-url "9.0")
(define-obsolete-function-alias 'org-no-warnings 'with-no-warnings "9.0")
(define-obsolete-function-alias 'org-number-sequence 'number-sequence "9.0")
(define-obsolete-function-alias 'org-pop-to-buffer-same-window 'pop-to-buffer-same-window "9.0")
(define-obsolete-function-alias 'org-string-match-p 'string-match-p "9.0")

(define-obsolete-function-alias 'org-element-remove-indentation
  'org-remove-indentation "9.0")
(define-obsolete-variable-alias 'org-latex-create-formula-image-program
  'org-preview-latex-default-process "9.0")
(define-obsolete-variable-alias 'org-latex-preview-ltxpng-directory
  'org-preview-latex-image-directory "9.0")
(define-obsolete-function-alias 'org-table-p 'org-at-table-p "9.0")
(define-obsolete-function-alias 'org-on-heading-p 'org-at-heading-p "9.0")

(define-obsolete-function-alias 'org-image-file-name-regexp
  'image-file-name-regexp "9.0")
(define-obsolete-function-alias 'org-completing-read-no-i
  'completing-read "9.0")
(define-obsolete-function-alias 'org-icompleting-read
  'completing-read "9.0")
(define-obsolete-function-alias 'org-iread-file-name 'read-file-name "9.0")

(define-obsolete-variable-alias 'org-agenda-ignore-drawer-properties
  'org-agenda-ignore-properties "9.0")

(define-obsolete-function-alias 'org-export-get-genealogy
  'org-element-lineage "9.0")
(define-obsolete-variable-alias 'org-latex-with-hyperref
  'org-latex-hyperref-template "9.0")
(define-obsolete-variable-alias 'hfy-optimisations 'hfy-optimizations "9.0")

(define-obsolete-function-alias 'org-list-parse-list 'org-list-to-lisp "9.0")
(define-obsolete-function-alias 'org-agenda-todayp
  'org-agenda-today-p "9.0")
(define-obsolete-function-alias 'org-babel-examplize-region
  'org-babel-examplify-region "9.0")

(define-obsolete-function-alias 'org-babel-trim 'org-trim "9.0")

(define-obsolete-function-alias 'org-insert-columns-dblock
  'org-columns-insert-dblock "9.0")
(define-obsolete-function-alias 'org-activate-bracket-links
  'org-activate-links "9.0")
(define-obsolete-function-alias 'org-activate-plain-links 'ignore "9.0")
(define-obsolete-function-alias 'org-activate-angle-links 'ignore "9.0")
(define-obsolete-function-alias 'org-remove-double-quotes 'org-strip-quotes "9.0")

(define-obsolete-function-alias 'org-babel-number-p
  'org-babel--string-to-number "9.0")

;;;; Obsolete variables


;;;; Obsolete functions and macros

(defmacro org-re (s)
  "Replace posix classes in regular expression S."
  (declare (debug (form))
           (obsolete "you can safely remove it." "9.0"))
  s)

(defun org-sublist (list start end)
  "Return a section of LIST, from START to END.
Counting starts at 1."
  (cl-subseq list (1- start) end))
(make-obsolete 'org-sublist
               "use cl-subseq (note the 0-based counting)."
               "9.0")

(defun org-in-fixed-width-region-p ()
  "Non-nil if point in a fixed-width region."
  (save-match-data
    (org-element-type-p (org-element-at-point) 'fixed-width)))
(make-obsolete 'org-in-fixed-width-region-p
               "use `org-element' library"
               "9.0")

(defun org-compatible-face (inherits specs)
  "Make a compatible face specification.
If INHERITS is an existing face and if the Emacs version supports
it, just inherit the face.  If INHERITS is not given and SPECS
is, use SPECS to define the face."
  (declare (indent 1))
  (if (facep inherits)
      (list (list t :inherit inherits))
    specs))
(make-obsolete 'org-compatible-face "you can remove it." "9.0")

(defun org-add-link-type (type &optional follow export)
  "Add a new TYPE link.
FOLLOW and EXPORT are two functions.

FOLLOW should take the link path as the single argument and do whatever
is necessary to follow the link, for example find a file or display
a mail message.

EXPORT should format the link path for export to one of the export formats.
It should be a function accepting three arguments:

  path    the path of the link, the text after the prefix (like \"http:\")
  desc    the description of the link, if any
  format  the export format, a symbol like `html' or `latex' or `ascii'.

The function may use the FORMAT information to return different values
depending on the format.  The return value will be put literally into
the exported file.  If the return value is nil, this means Org should
do what it normally does with links which do not have EXPORT defined.

Org mode has a built-in default for exporting links.  If you are happy with
this default, there is no need to define an export function for the link
type.  For a simple example of an export function, see `org-bbdb.el'.

If TYPE already exists, update it with the arguments.
See `org-link-parameters' for documentation on the other parameters."
  (org-link-set-parameters type :follow follow :export export)
  (message "Created %s link." type))
(make-obsolete 'org-add-link-type "use `org-link-set-parameters' instead." "9.0")

(defun org-table-recognize-table.el ()
  "If there is a table.el table nearby, recognize it and move into it."
  (when (org-at-table.el-p)
    (forward-line 0)
    (unless (or (looking-at org-table-dataline-regexp)
                (not (looking-at org-table1-hline-regexp)))
      (forward-line)
      (when (looking-at org-table-any-border-regexp)
        (forward-line -2)))
    (if (re-search-forward "|" (org-table-end t) t)
        (progn
          (require 'table)
          (if (table--at-cell-p (point)) t
            (message "recognizing table.el table...")
            (table-recognize-table)
            (message "recognizing table.el table...done")))
      (error "This should not happen"))))

;; Not used since commit 6d1e3082, Feb 2010.
(make-obsolete 'org-table-recognize-table.el
               "please notify Org mailing list if you use this function."
               "9.0")

(defun org-remove-angle-brackets (s)
  (org-unbracket-string "<" ">" s))
(make-obsolete 'org-remove-angle-brackets 'org-unbracket-string "9.0")

;; The function was made obsolete by commit 65399674d5 of 2013-02-22.
;; This make-obsolete call was added 2016-09-01.
(make-obsolete 'org-capture-import-remember-templates
	       "use the `org-capture-templates' variable instead."
	       "9.0")

(provide 'org-obsolete9.0)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-obsolete9.0.el ends here
