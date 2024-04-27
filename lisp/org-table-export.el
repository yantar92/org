;;; org-table-export.el --- Org table exporter        -*- lexical-binding: t; -*-

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

;; This file contains export functionality for Org mode tables.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ox)

(defcustom org-table-export-default-format "orgtbl-to-tsv"
  "Default export parameters for `org-table-export'.
These can be overridden for a specific table by setting the
TABLE_EXPORT_FORMAT property.  See the manual section on orgtbl
radio tables for the different export transformations and
available parameters."
  :group 'org-table-import-export
  :type 'string)

;;;###autoload
(defun org-table-export (&optional file format)
  "Export table to a file, with configurable format.
Such a file can be imported into usual spreadsheet programs.

FILE can be the output file name.  If not given, it will be taken
from a TABLE_EXPORT_FILE property in the current entry or higher
up in the hierarchy, or the user will be prompted for a file
name.  FORMAT can be an export format, of the same kind as it
used when `-mode' sends a table in a different format.

The command suggests a format depending on TABLE_EXPORT_FORMAT,
whether it is set locally or up in the hierarchy, then on the
extension of the given file name, and finally on the variable
`org-table-export-default-format'."
  (interactive)
  (unless (org-at-table-p) (user-error "No table at point"))
  (org-table-align)	       ; Make sure we have everything we need.
  (let ((file (or file (org-entry-get (point) "TABLE_EXPORT_FILE" t))))
    (unless file
      (setq file (read-file-name "Export table to: "))
      (unless (or (not (file-exists-p file))
		  (y-or-n-p (format "Overwrite file %s? " file)))
	(user-error "File not written")))
    (when (file-directory-p file)
      (user-error "This is a directory path, not a file"))
    (when (and (buffer-file-name (buffer-base-buffer))
	       (file-equal-p
		(file-truename file)
		(file-truename (buffer-file-name (buffer-base-buffer)))))
      (user-error "Please specify a file name that is different from current"))
    (let ((fileext (concat (file-name-extension file) "$"))
	  (format (or format (org-entry-get (point) "TABLE_EXPORT_FORMAT" t))))
      (unless format
	(let* ((formats '("orgtbl-to-tsv" "orgtbl-to-csv" "orgtbl-to-latex"
			  "orgtbl-to-html" "orgtbl-to-generic"
			  "orgtbl-to-texinfo" "orgtbl-to-orgtbl"
			  "orgtbl-to-unicode"))
	       (deffmt-readable
		(replace-regexp-in-string
		 "\t" "\\t"
		 (replace-regexp-in-string
		  "\n" "\\n"
		  (or (car (delq nil
				 (mapcar
				  (lambda (f)
				    (and (string-match-p fileext f) f))
				  formats)))
		      org-table-export-default-format)
		  t t)
		 t t)))
	  (setq format
		(org-completing-read
		 "Format: " formats nil nil deffmt-readable))))
      (if (string-match "\\([^ \t\r\n]+\\)\\( +.*\\)?" format)
	  (let ((transform (intern (match-string 1 format)))
		(params (and (match-end 2)
			     (read (concat "(" (match-string 2 format) ")"))))
		(table (org-table-to-lisp)))
	    (unless (fboundp transform)
	      (user-error "No such transformation function %s" transform))
            (with-temp-file file
              (insert (funcall transform table params) "\n"))
	    (message "Export done."))
	(user-error "TABLE_EXPORT_FORMAT invalid")))))

(defun orgtbl--skip (ast _ info)
  "Extract first X table rows from AST.
X is taken from :skip property in INFO plist.
Return the modified AST."
  (when-let ((skip (plist-get info :skip)))
    (unless (wholenump skip) (user-error "Wrong :skip value"))
    (let ((n 0))
      (org-element-map ast 'table-row
        (lambda (row)
	  (if (>= n skip) t
	    (org-element-extract row)
	    (cl-incf n)
	    nil))
        nil t)))
  ast)

(defun orgtbl--skipcols (ast _ info)
  "Extract first X table columns from AST.
X is taken from :skipcols property in INFO plist.
Special columns are always ignored.
Return the modified AST."
  (when-let ((skipcols (plist-get info :skipcols)))
    (unless (consp skipcols) (user-error "Wrong :skipcols value"))
    (org-element-map ast 'table
      (lambda (table)
	(let ((specialp (org-export-table-has-special-column-p table)))
	  (dolist (row (org-element-contents table))
	    (when (eq (org-element-property :type row) 'standard)
	      (let ((c 1))
		(dolist (cell (nthcdr (if specialp 1 0)
				      (org-element-contents row)))
		  (when (memq c skipcols)
		    (org-element-extract cell))
		  (cl-incf c)))))))))
  ast)

;;;###autoload
(defun orgtbl-to-generic (table params)
  "Convert the `orgtbl-mode' TABLE to some other format.

This generic routine can be used for many standard cases.

TABLE is a list, each entry either the symbol `hline' for
a horizontal separator line, or a list of fields for that
line.  PARAMS is a property list of parameters that can
influence the conversion.

Valid parameters are all the export options understood by the export
backend and also:

:backend, :raw

  Export backend used as a basis to transcode elements of the
  table, when no specific parameter applies to it.  It is also
  used to translate cells contents.  You can prevent this by
  setting :raw property to a non-nil value.

:splice

  When non-nil, only convert rows, not the table itself.  This is
  equivalent to setting to the empty string both :tstart
  and :tend, which see.

:skip

  When set to an integer N, skip the first N lines of the table.
  Horizontal separation lines do count for this parameter!

:skipcols

  List of columns that should be skipped.  If the table has
  a column with calculation marks, that column is automatically
  discarded beforehand.

:hline

  String to be inserted on horizontal separation lines.  May be
  nil to ignore these lines altogether.

:sep

  Separator between two fields, as a string.

Each in the following group may be either a string or a function
of no arguments returning a string:

:tstart, :tend

  Strings to start and end the table.  Ignored when :splice is t.

:lstart, :lend

  Strings to start and end a new table line.

:llstart, :llend

  Strings to start and end the last table line.  Default,
  respectively, to :lstart and :lend.

Each in the following group may be a string or a function of one
argument (either the cells in the current row, as a list of
strings, or the current cell) returning a string:

:lfmt

  Format string for an entire row, with enough %s to capture all
  fields.  When non-nil, :lstart, :lend, and :sep are ignored.

:llfmt

  Format for the entire last line, defaults to :lfmt.

:fmt

  A format to be used to wrap the field, should contain %s for
  the original field value.  For example, to wrap everything in
  dollars, you could use :fmt \"$%s$\".  This may also be
  a property list with column numbers and format strings, or
  functions, e.g.,

    (:fmt (2 \"$%s$\" 4 (lambda (c) (format \"$%s$\" c))))

  The format is ignored for empty fields.  Use :raw t with non-nil
  :backend option to force formatting empty fields.

:hlstart :hllstart :hlend :hllend :hsep :hlfmt :hllfmt :hfmt

 Same as above, specific for the header lines in the table.
 All lines before the first hline are treated as header.  If
 any of these is not present, the data line value is used.

This may be either a string or a function of two arguments:

:efmt

  Use this format to print numbers with exponential.  The format
  should have %s twice for inserting mantissa and exponent, for
  example \"%s\\\\times10^{%s}\".  This may also be a property
  list with column numbers and format strings or functions.
  :fmt will still be applied after :efmt."
  ;; Make sure `org-export-create-backend' is available.
  (require 'ox)
  (let* ((backend (plist-get params :backend))
	 (custom-backend
	  ;; Build a custom backend according to PARAMS.  Before
	  ;; defining a translator, check if there is anything to do.
	  ;; When there isn't, let BACKEND handle the element.
	  (org-export-create-backend
	   :parent (or backend 'org)
	   :transcoders
	   `((table . ,(org-table--to-generic-table params))
	     (table-row . ,(org-table--to-generic-row params))
	     (table-cell . ,(org-table--to-generic-cell params))
	     ;; Macros are not going to be expanded.  However, no
	     ;; regular backend has a transcoder for them.  We
	     ;; provide one so they are not ignored, but displayed
	     ;; as-is instead.
	     (macro . (lambda (m c i) (org-element-macro-interpreter m nil)))
             ;; Only export the actual table.  Do nothing with the
             ;; containing section regardless what backend think about
             ;; it.  (It is somewhat like BODY-ONLY argument in
             ;; `org-export-as', but skips not only transcoding the
             ;; full document, but also section containing the table.
             (section . (lambda (_ contents _) contents))))))
    ;; Store TABLE as Org syntax in DATA.  Tolerate non-string cells.
    ;; Initialize communication channel in INFO.
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
	(dolist (e table)
	  (cond ((eq e 'hline) (princ "|--\n"))
		((consp e)
		 (princ "| ") (dolist (c e) (princ c) (princ " |"))
		 (princ "\n")))))
      (let ((org-inhibit-startup t)) (org-mode))
      (defvar org-export-before-processing-functions) ; ox.el
      (defvar org-export-process-citations) ; ox.el
      (defvar org-export-expand-links) ; ox.el
      (defvar org-export-filter-parse-tree-functions) ; ox.el
      (defvar org-export-filters-alist) ; ox.el
      (defvar org-export-replace-macros) ; ox.el
      (declare-function
       org-export-as "ox"
       (backend &optional subtreep visible-only body-only ext-plist))
      ;; We disable the usual pre-processing and post-processing,
      ;; i.e., hooks, Babel code evaluation, and macro expansion.
      ;; Only backend specific filters are retained.
      ;; We _do not_ disable `org-export-filter-parse-tree-functions'
      ;; (historically).
      (let ((org-export-before-processing-functions nil)
            (org-export-replace-macros nil)
            (org-export-use-babel nil)
            (org-export-before-parsing-functions nil)
            (org-export-process-citations nil)
            (org-export-expand-links nil)
            (org-export-filter-parse-tree-functions
             (append
              '(orgtbl--skip orgtbl--skipcols)
              org-export-filter-parse-tree-functions))
            (org-export-filters-alist
             '((:filter-parse-tree . org-export-filter-parse-tree-functions))))
        (when (or (not backend) (plist-get params :raw)) (require 'ox-org))
        (when (and backend (symbolp backend) (not (org-export-get-backend backend)))
          (user-error "Unknown :backend value: %S" backend))
        (let ((output (org-export-as custom-backend nil nil 'body-only params)))
          ;; Remove final newline.
          (if (org-string-nw-p output) (substring-no-properties output 0 -1) ""))))))

(defun org-table--generic-apply (value name &optional with-cons &rest args)
  (cond ((null value) nil)
        ((functionp value) `(funcall ',value ,@args))
        ((stringp value)
	 (cond ((consp (car args)) `(apply #'format ,value ,@args))
	       (args `(format ,value ,@args))
	       (t value)))
	((and with-cons (consp value))
	 `(let ((val (cadr (memq column ',value))))
	    (cond ((null val) contents)
		  ((stringp val) (format val ,@args))
		  ((functionp val) (funcall val ,@args))
		  (t (user-error "Wrong %s value" ,name)))))
        (t (user-error "Wrong %s value" name))))

(defun org-table--to-generic-table (params)
  "Return custom table transcoder according to PARAMS.
PARAMS is a plist.  See `orgtbl-to-generic' for more
information."
  (let ((backend (plist-get params :backend))
	(splice (plist-get params :splice))
	(tstart (plist-get params :tstart))
	(tend (plist-get params :tend)))
    `(lambda (table contents info)
       (concat
	,(and tstart (not splice)
	      `(concat ,(org-table--generic-apply tstart ":tstart") "\n"))
	,(if (or (not backend) tstart tend splice) 'contents
	   `(org-export-with-backend ',backend table contents info))
	,(org-table--generic-apply (and (not splice) tend) ":tend")))))

(defun org-table--to-generic-row (params)
  "Return custom table row transcoder according to PARAMS.
PARAMS is a plist.  See `orgtbl-to-generic' for more
information."
  (let* ((backend (plist-get params :backend))
	 (lstart (plist-get params :lstart))
	 (llstart (plist-get params :llstart))
	 (hlstart (plist-get params :hlstart))
	 (hllstart (plist-get params :hllstart))
	 (lend (plist-get params :lend))
	 (llend (plist-get params :llend))
	 (hlend (plist-get params :hlend))
	 (hllend (plist-get params :hllend))
	 (lfmt (plist-get params :lfmt))
	 (llfmt (plist-get params :llfmt))
	 (hlfmt (plist-get params :hlfmt))
	 (hllfmt (plist-get params :hllfmt)))
    `(lambda (row contents info)
       (if (eq (org-element-property :type row) 'rule)
	   ,(cond
	     ((plist-member params :hline)
	      (org-table--generic-apply (plist-get params :hline) ":hline"))
	     (backend `(org-export-with-backend ',backend row nil info)))
	 (let ((headerp ,(and (or hlfmt hlstart hlend)
			      '(org-export-table-row-in-header-p row info)))
	       (last-header-p
		,(and (or hllfmt hllstart hllend)
		      '(org-export-table-row-ends-header-p row info)))
	       (lastp (not (org-export-get-next-element row info))))
	   (when contents
	     ;; Check if we can apply `:lfmt', `:llfmt', `:hlfmt', or
	     ;; `:hllfmt' to CONTENTS.  Otherwise, fallback on
	     ;; `:lstart', `:lend' and their relatives.
	     ,(let ((cells
		     '(org-element-map row 'table-cell
			(lambda (cell)
			  ;; Export all cells, without separators.
			  ;;
			  ;; Use `org-export-data-with-backend'
			  ;; instead of `org-export-data' to eschew
			  ;; cached values, which
			  ;; ignore :orgtbl-ignore-sep parameter.
			  (org-export-data-with-backend
			   cell
			   (plist-get info :back-end)
			   (org-combine-plists info '(:orgtbl-ignore-sep t))))
			info)))
		`(cond
		  ,(and hllfmt
			`(last-header-p ,(org-table--generic-apply
					  hllfmt ":hllfmt" nil cells)))
		  ,(and hlfmt
			`(headerp ,(org-table--generic-apply
				    hlfmt ":hlfmt" nil cells)))
		  ,(and llfmt
			`(lastp ,(org-table--generic-apply
				  llfmt ":llfmt" nil cells)))
		  (t
		   ,(if lfmt (org-table--generic-apply lfmt ":lfmt" nil cells)
		      `(concat
			(cond
			 ,(and
			   (or hllstart hllend)
			   `(last-header-p
			     (concat
			      ,(org-table--generic-apply hllstart ":hllstart")
			      contents
			      ,(org-table--generic-apply hllend ":hllend"))))
			 ,(and
			   (or hlstart hlend)
			   `(headerp
			     (concat
			      ,(org-table--generic-apply hlstart ":hlstart")
			      contents
			      ,(org-table--generic-apply hlend ":hlend"))))
			 ,(and
			   (or llstart llend)
			   `(lastp
			     (concat
			      ,(org-table--generic-apply llstart ":llstart")
			      contents
			      ,(org-table--generic-apply llend ":llend"))))
			 (t
			  ,(cond
			    ((or lstart lend)
			     `(concat
			       ,(org-table--generic-apply lstart ":lstart")
			       contents
			       ,(org-table--generic-apply lend ":lend")))
			    (backend
			     `(org-export-with-backend
			       ',backend row contents info))
			    (t 'contents)))))))))))))))

(defun org-table--to-generic-cell (params)
  "Return custom table cell transcoder according to PARAMS.
PARAMS is a plist.  See `orgtbl-to-generic' for more
information."
  (let* ((backend (plist-get params :backend))
	 (efmt (plist-get params :efmt))
	 (fmt (plist-get params :fmt))
	 (hfmt (plist-get params :hfmt))
	 (sep (plist-get params :sep))
	 (hsep (plist-get params :hsep)))
    `(lambda (cell contents info)
       ;; Make sure that contents are exported as Org data when :raw
       ;; parameter is non-nil.
       ,(when (and backend (plist-get params :raw))
	  `(setq contents
		 ;; Since we don't know what are the pseudo object
		 ;; types defined in backend, we cannot pass them to
		 ;; `org-element-interpret-data'.  As a consequence,
		 ;; they will be treated as pseudo elements, and will
		 ;; have newlines appended instead of spaces.
		 ;; Therefore, we must make sure :post-blank value is
		 ;; really turned into spaces.
		 (replace-regexp-in-string
		  "\n" " "
		  (org-trim
		   (org-element-interpret-data
		    (org-element-contents cell))))))

       (let ((headerp ,(and (or hfmt hsep)
			    '(org-export-table-row-in-header-p
			      (org-element-parent-element cell) info)))
	     (column
	      ;; Call costly `org-export-table-cell-address' only if
	      ;; absolutely necessary, i.e., if one
	      ;; of :fmt :efmt :hfmt has a "plist type" value.
	      ,(and (cl-some (lambda (v) (integerp (car-safe v)))
			     (list efmt hfmt fmt))
		    '(1+ (cdr (org-export-table-cell-address cell info))))))
	 (when contents
	   ;; Check if we can apply `:efmt' on CONTENTS.
	   ,(when efmt
	      `(when (string-match orgtbl-exp-regexp contents)
		 (let ((mantissa (match-string 1 contents))
		       (exponent (match-string 2 contents)))
		   (setq contents ,(org-table--generic-apply
				    efmt ":efmt" t 'mantissa 'exponent)))))
	   ;; Check if we can apply FMT (or HFMT) on CONTENTS.
	   (cond
	    ,(and hfmt `(headerp (setq contents ,(org-table--generic-apply
						  hfmt ":hfmt" t 'contents))))
	    ,(and fmt `(t (setq contents ,(org-table--generic-apply
					   fmt ":fmt" t 'contents))))))
	 ;; If a separator is provided, use it instead of BACKEND's.
	 ;; Separators are ignored when LFMT (or equivalent) is
	 ;; provided.
	 ,(cond
	   ((or hsep sep)
	    `(if (or ,(and (not sep) '(not headerp))
		     (plist-get info :orgtbl-ignore-sep)
		     (not (org-export-get-next-element cell info)))
		 ,(if (not backend) 'contents
		    `(org-export-with-backend ',backend cell contents info))
	       (concat contents
		       ,(if (and sep hsep) `(if headerp ,hsep ,sep)
			  (or hsep sep)))))
	   (backend `(org-export-with-backend ',backend cell contents info))
	   (t 'contents))))))

;;;###autoload
(defun orgtbl-to-tsv (table params)
  "Convert the `orgtbl-mode' TABLE to TAB separated material."
  (orgtbl-to-generic table (org-combine-plists '(:sep "\t") params)))

(defun org-quote-csv-field (s)
  "Quote field S for inclusion in CSV material."
  (if (string-match "[\",]" s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")
    s))

;;;###autoload
(defun orgtbl-to-csv (table params)
  "Convert the `orgtbl-mode' TABLE to CSV material.
This does take care of the proper quoting of fields with comma or quotes."
  (orgtbl-to-generic table
		     (org-combine-plists '(:sep "," :fmt org-quote-csv-field)
					 params)))

;;;###autoload
(defun orgtbl-to-latex (table params)
  "Convert the `orgtbl-mode' TABLE to LaTeX.

TABLE is a list, each entry either the symbol `hline' for
a horizontal separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the
conversion.  All parameters from `orgtbl-to-generic' are
supported.  It is also possible to use the following ones:

:booktabs

  When non-nil, use formal \"booktabs\" style.

:environment

  Specify environment to use, as a string.  If you use
  \"longtable\", you may also want to specify :language property,
  as a string, to get proper continuation strings."
  (require 'ox-latex)
  (orgtbl-to-generic
   table
   (org-combine-plists
    ;; Provide sane default values.
    (list :backend 'latex
	  :latex-default-table-mode 'table
	  :latex-tables-centered nil
	  :latex-tables-booktabs (plist-get params :booktabs)
	  :latex-table-scientific-notation nil
	  :latex-default-table-environment
	  (or (plist-get params :environment) "tabular"))
    params)))

;;;###autoload
(defun orgtbl-to-html (table params)
  "Convert the `orgtbl-mode' TABLE to HTML.

TABLE is a list, each entry either the symbol `hline' for
a horizontal separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the
conversion.  All parameters from `orgtbl-to-generic' are
supported.  It is also possible to use the following one:

:attributes

  Attributes and values, as a plist, which will be used in
  <table> tag."
  (require 'ox-html)
  (orgtbl-to-generic
   table
   (org-combine-plists
    ;; Provide sane default values.
    (list :backend 'html
	  :html-table-data-tags '("<td%s>" . "</td>")
	  :html-table-use-header-tags-for-first-column nil
	  :html-table-align-individual-fields t
	  :html-table-row-tags '("<tr>" . "</tr>")
	  :html-table-attributes
	  (if (plist-member params :attributes)
	      (plist-get params :attributes)
	    '(:border "2" :cellspacing "0" :cellpadding "6" :rules "groups"
		      :frame "hsides")))
    params)))

;;;###autoload
(defun orgtbl-to-texinfo (table params)
  "Convert the `orgtbl-mode' TABLE to Texinfo.

TABLE is a list, each entry either the symbol `hline' for
a horizontal separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the
conversion.  All parameters from `orgtbl-to-generic' are
supported.  It is also possible to use the following one:

:columns

  Column widths, as a string.  When providing column fractions,
  \"@columnfractions\" command can be omitted."
  (require 'ox-texinfo)
  (let ((output
	 (orgtbl-to-generic
	  table
	  (org-combine-plists
	   (list :backend 'texinfo
		 :texinfo-tables-verbatim nil
		 :texinfo-table-scientific-notation nil)
	   params)))
	(columns (let ((w (plist-get params :columns)))
		   (cond ((not w) nil)
			 ((string-match-p "{\\|@columnfractions " w) w)
			 (t (concat "@columnfractions " w))))))
    (if (not columns) output
      (replace-regexp-in-string
       "@multitable \\(.*\\)" columns output t nil 1))))

;;;###autoload
(defun orgtbl-to-orgtbl (table params)
  "Convert the `orgtbl-mode' TABLE into another `orgtbl-mode' table.

TABLE is a list, each entry either the symbol `hline' for
a horizontal separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the
conversion.  All parameters from `orgtbl-to-generic' are
supported.

Useful when slicing one table into many.  The :hline, :sep,
:lstart, and :lend provide orgtbl framing.  :tstart and :tend can
be set to provide ORGTBL directives for the generated table."
  (require 'ox-org)
  (orgtbl-to-generic table (org-combine-plists params (list :backend 'org))))

(defun orgtbl-to-table.el (table params)
  "Convert the `orgtbl-mode' TABLE into a table.el table.
TABLE is a list, each entry either the symbol `hline' for
a horizontal separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the
conversion.  All parameters from `orgtbl-to-generic' are
supported."
  (with-temp-buffer
    (insert (orgtbl-to-orgtbl table params))
    (org-table-align)
    (goto-char (point-min))
    (while (search-forward "-|" nil t)
      (replace-match "-+"))
    (goto-char (point-min))
    (while (search-forward "|-" nil t)
      (replace-match "+-"))
    (buffer-string)))

(defun orgtbl-to-unicode (table params)
  "Convert the `orgtbl-mode' TABLE into a table with unicode characters.

TABLE is a list, each entry either the symbol `hline' for
a horizontal separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the
conversion.  All parameters from `orgtbl-to-generic' are
supported.  It is also possible to use the following ones:

:ascii-art

  When non-nil, use \"ascii-art-to-unicode\" package to translate
  the table.  You can download it here:
  https://gnuvola.org/software/j/aa2u/ascii-art-to-unicode.el.

:narrow

  When non-nil, narrow columns width than provided width cookie,
  using \"=>\" as an ellipsis, just like in an Org mode buffer."
  (require 'ox-ascii)
  (orgtbl-to-generic
   table
   (org-combine-plists
    (list :backend 'ascii
	  :ascii-charset 'utf-8
	  :ascii-table-widen-columns (not (plist-get params :narrow))
	  :ascii-table-use-ascii-art (plist-get params :ascii-art))
    params)))


(provide 'org-table-export)

;;; org-table-export.el ends here
