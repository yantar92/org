;;; org-table-core.el --- Org table syntax API        -*- lexical-binding: t; -*-

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

;; This file implements API to retrieve information about Org mode
;; tables.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element)
(require 'org-element-context)

(defgroup org-table nil
  "Options concerning tables in Org mode."
  :tag "Org Table"
  :group 'org)


;;; Regexps Constants

(defconst org-table-any-line-regexp "^[ \t]*\\(|\\|\\+-[-+]\\)"
  "Detect an org-type or table-type table.")

(defconst org-table-line-regexp "^[ \t]*|"
  "Detect an org-type table line.")

(defconst org-table-dataline-regexp "^[ \t]*|[^-]"
  "Detect an org-type table line.")

(defconst org-table1-hline-regexp "^[ \t]*\\+-[-+]"
  "Detect a table-type table hline.")

(defconst org-table-any-border-regexp "^[ \t]*[^|+ \t]"
  "Detect the first line outside a table when searching from within it.
This works for both table types.")

(defvar org-table-TBLFM-begin-regexp "^[ \t]*|.*\n[ \t]*#\\+TBLFM: ")

(defconst org-table-auto-recalculate-regexp "^[ \t]*| *# *\\(|\\|$\\)"
  "Regexp matching a line marked for automatic recalculation.")

(defconst org-table-recalculate-regexp "^[ \t]*| *[#*] *\\(|\\|$\\)"
  "Regexp matching a line marked for recalculation.")

(defconst org-table-calculate-mark-regexp "^[ \t]*| *[!$^_#*] *\\(|\\|$\\)"
  "Regexp matching a line marked for calculation.")

(defconst org-table-border-regexp "^[ \t]*[^| \t]"
  "Regexp matching any line outside an Org table.")

(defconst org-table-range-regexp
  "@\\([-+]?I*[-+]?[0-9]*\\)\\(\\$[-+]?[0-9]+\\)?\\(\\.\\.@?\\([-+]?I*[-+]?[0-9]*\\)\\(\\$[-+]?[0-9]+\\)?\\)?"
  ;;   1                        2                    3          4                        5
  "Regular expression for matching ranges in formulas.")

(defconst org-table-range-regexp2
  (concat
   "\\(" "@[-0-9I$&]+" "\\|" "[a-zA-Z]\\{1,2\\}\\([0-9]+\\|&\\)" "\\|" "\\$[a-zA-Z0-9]+" "\\)"
   "\\.\\."
   "\\(" "@?[-0-9I$&]+" "\\|" "[a-zA-Z]\\{1,2\\}\\([0-9]+\\|&\\)" "\\|" "\\$[a-zA-Z0-9]+" "\\)")
  "Match a range for reference display.")

(defconst org-table-translate-regexp
  (concat "\\(" "@[-0-9I$]+" "\\|" "[a-zA-Z]\\{1,2\\}\\([0-9]+\\|&\\)" "\\)")
  "Match a reference that needs translation, for reference display.")


;;; Internal Variables

(defvar org-pos nil)

(defvar-local org-table-formula-constants-local nil
  "Local version of `org-table-formula-constants'.")

(defvar org-table-column-names nil
  "Alist with column names, derived from the `!' line.
This variable is initialized with `org-table-analyze'.")

(defvar org-table-column-name-regexp nil
  "Regular expression matching the current column names.
This variable is initialized with `org-table-analyze'.")

(defvar org-table-local-parameters nil
  "Alist with parameter names, derived from the `$' line.
This variable is initialized with `org-table-analyze'.")

(defvar org-table-named-field-locations nil
  "Alist with locations of named fields.
Associations follow the pattern (NAME LINE COLUMN) where
  NAME is the name of the field as a string,
  LINE is the number of lines from the beginning of the table,
  COLUMN is the column of the field, as an integer.
This variable is initialized with `org-table-analyze'.")

(defvar org-table-current-line-types nil
  "Table row types in current table.
This variable is initialized with `org-table-analyze'.")

(defvar org-table-current-begin-pos nil
  "Current table begin position, as a marker.
This variable is initialized with `org-table-analyze'.")

(defvar org-table-current-ncol nil
  "Number of columns in current table.
This variable is initialized with `org-table-analyze'.")

(defvar org-table-dlines nil
  "Vector of data line line numbers in the current table.
Line numbers are counted from the beginning of the table.  This
variable is initialized with `org-table-analyze'.")

(defvar org-table-hlines nil
  "Vector of hline line numbers in the current table.
Line numbers are counted from the beginning of the table.  This
variable is initialized with `org-table-analyze'.")


;;; Macros and Inlined Functions

;;;###autoload
(defun org-table-goto-column (n &optional on-delim force)
  "Move the cursor to the Nth column in the current table line.
With optional argument ON-DELIM, stop with point before the left delimiter
of the field.
If there are less than N fields, just go to after the last delimiter.
However, when FORCE is non-nil, create new columns if necessary."
  (interactive "p")
  (forward-line 0)
  (when (> n 0)
    (while (and (> (setq n (1- n)) -1)
                (or (search-forward "|" (line-end-position) t)
		    (and force
			 (progn (end-of-line 1)
				(skip-chars-backward "^|")
				(insert " | ")
				t)))))
    (when (and force (not (looking-at ".*|")))
      (save-excursion (end-of-line 1) (insert " | ")))
    (if on-delim
	(backward-char 1)
      (if (looking-at " ") (forward-char 1)))))

(defmacro org-table-save-field (&rest body)
  "Save current field; execute BODY; restore field.
Field is restored even in case of abnormal exit."
  (declare (debug (body)))
  (org-with-gensyms (line column)
    `(let ((,line (copy-marker (line-beginning-position)))
	   (,column (org-table-current-column)))
       (unwind-protect
	   (progn ,@body)
	 (goto-char ,line)
	 (org-table-goto-column ,column)
	 (set-marker ,line nil)))))


;;; Navigation and Structure Editing

;;;###autoload
(defun org-table-begin (&optional table-type)
  "Find the beginning of the table and return its position.
With a non-nil optional argument TABLE-TYPE, return the beginning
of a table.el-type table.  This function assumes point is on
a table."
  (cond (table-type
	 (org-element-post-affiliated (org-element-at-point)))
	((save-excursion
	   (and (re-search-backward org-table-border-regexp nil t)
		(line-beginning-position 2))))
	(t (point-min))))

;;;###autoload
(defun org-table-end (&optional table-type)
  "Find the end of the table and return its position.
With a non-nil optional argument TABLE-TYPE, return the end of
a table.el-type table.  This function assumes point is on
a table."
  (save-excursion
    (cond (table-type
	   (goto-char (org-element-end (org-element-at-point)))
	   (skip-chars-backward " \t\n")
	   (line-beginning-position 2))
	  ((re-search-forward org-table-border-regexp nil t)
	   (match-beginning 0))
	  ;; When the line right after the table is the last line in
	  ;; the buffer with trailing spaces but no final newline
	  ;; character, be sure to catch the correct ending at its
	  ;; beginning.  In any other case, ending is expected to be
	  ;; at point max.
	  (t (goto-char (point-max))
	     (skip-chars-backward " \t")
	     (if (bolp) (point) (line-end-position))))))

(defun org-table-current-line ()
  "Return the index of the current data line."
  (let ((pos (point)) (end (org-table-end)) (cnt 0))
    (save-excursion
      (goto-char (org-table-begin))
      (while (and (re-search-forward org-table-dataline-regexp end t)
		  (setq cnt (1+ cnt))
                  (< (line-end-position) pos))))
    cnt))

(defun org-table-current-column ()
  "Return current column number."
  (interactive)
  (save-excursion
    (let ((pos (point)))
      (forward-line 0)
      (if (not (search-forward "|" pos t)) 0
	(let ((column 1)
	      (separator (if (org-at-table-hline-p) "[+|]" "|")))
	  (while (re-search-forward separator pos t) (cl-incf column))
	  column)))))

(defun org-table-current-dline ()
  "Find out what table data line we are in.
Only data lines count for this."
  (save-excursion
    (let ((c 0)
	  (pos (line-beginning-position)))
      (goto-char (org-table-begin))
      (while (<= (point) pos)
	(when (looking-at org-table-dataline-regexp) (cl-incf c))
	(forward-line))
      c)))

(defun org-table-TBLFM-begin ()
  "Find the beginning of the TBLFM lines and return its position.
Return nil when the beginning of TBLFM line was not found."
  (save-excursion
    (when (progn (forward-line 1)
		 (re-search-backward org-table-TBLFM-begin-regexp nil t))
      (line-beginning-position 2))))


;;; Generic Tools

;;;###autoload
(defun org-table-map-tables (f &optional quietly)
  "Apply function F to the start of all tables in the buffer."
  (org-with-point-at 1
    (while (re-search-forward org-table-line-regexp nil t)
      (let ((table (org-element-lineage (org-element-at-point) 'table t)))
	(when table
	  (unless quietly
	    (message "Mapping tables: %d%%"
		     (floor (* 100.0 (point)) (buffer-size))))
	  (goto-char (org-element-post-affiliated table))
	  (let ((end (copy-marker (org-element-end table))))
	    (unwind-protect
		(progn (funcall f) (goto-char end))
	      (set-marker end nil)))))))
  (unless quietly (message "Mapping tables: done")))

;;;###autoload
(defun org-table-analyze ()
  "Analyze table at point and store results.

This function sets up the following dynamically scoped variables:

 `org-table-column-name-regexp',
 `org-table-column-names',
 `org-table-current-begin-pos',
 `org-table-current-line-types',
 `org-table-current-ncol',
 `org-table-dlines',
 `org-table-hlines',
 `org-table-local-parameters',
 `org-table-named-field-locations'."
  (let ((beg (org-table-begin))
	(end (org-table-end)))
    (save-excursion
      (goto-char beg)
      ;; Extract column names.
      (setq org-table-column-names nil)
      (when (save-excursion
	      (re-search-forward "^[ \t]*| *! *\\(|.*\\)" end t))
	(let ((c 1))
	  (dolist (name (org-split-string (match-string 1) " *| *"))
	    (cl-incf c)
	    (when (string-match "\\`[a-zA-Z][_a-zA-Z0-9]*\\'" name)
	      (push (cons name (number-to-string c)) org-table-column-names)))))
      (setq org-table-column-names (nreverse org-table-column-names))
      (setq org-table-column-name-regexp
	    (format "\\$\\(%s\\)\\>"
		    (regexp-opt (mapcar #'car org-table-column-names) t)))
      ;; Extract local parameters.
      (setq org-table-local-parameters nil)
      (save-excursion
	(while (re-search-forward "^[ \t]*| *\\$ *\\(|.*\\)" end t)
	  (dolist (field (org-split-string (match-string 1) " *| *"))
	    (when (string-match
		   "\\`\\([a-zA-Z][_a-zA-Z0-9]*\\|%\\) *= *\\(.*\\)" field)
	      (push (cons (match-string 1 field) (match-string 2 field))
		    org-table-local-parameters)))))
      ;; Update named fields locations.  We minimize `count-lines'
      ;; processing by storing last known number of lines in LAST.
      (setq org-table-named-field-locations nil)
      (save-excursion
	(let ((last (cons (point) 0)))
	  (while (re-search-forward "^[ \t]*| *\\([_^]\\) *\\(|.*\\)" end t)
	    (let ((c (match-string 1))
		  (fields (org-split-string (match-string 2) " *| *")))
	      (save-excursion
		(forward-line (if (equal c "_") 1 -1))
		(let ((fields1
		       (and (looking-at "^[ \t]*|[^|]*\\(|.*\\)")
			    (org-split-string (match-string 1) " *| *")))
		      (line (cl-incf (cdr last) (count-lines (car last) (point))))
		      (col 1))
		  (setcar last (point))	; Update last known position.
		  (while (and fields fields1)
		    (let ((field (pop fields))
			  (v (pop fields1)))
		      (cl-incf col)
		      (when (and (stringp field)
				 (stringp v)
				 (string-match "\\`[a-zA-Z][_a-zA-Z0-9]*\\'"
					       field))
			(push (cons field v) org-table-local-parameters)
			(push (list field line col)
			      org-table-named-field-locations))))))))))
      ;; Reuse existing markers when possible.
      (if (markerp org-table-current-begin-pos)
	  (move-marker org-table-current-begin-pos (point))
	(setq org-table-current-begin-pos (point-marker)))
      ;; Analyze the line types.
      (let ((l 0) hlines dlines types)
	(while (looking-at "[ \t]*|\\(-\\)?")
	  (push (if (match-end 1) 'hline 'dline) types)
	  (if (match-end 1) (push l hlines) (push l dlines))
	  (forward-line)
	  (cl-incf l))
	(push 'hline types) ; Add an imaginary extra hline to the end.
	(setq org-table-current-line-types (apply #'vector (nreverse types)))
	(setq org-table-dlines (apply #'vector (cons nil (nreverse dlines))))
	(setq org-table-hlines (apply #'vector (cons nil (nreverse hlines)))))
      ;; Get the number of columns from the first data line in table.
      (goto-char beg)
      (forward-line (aref org-table-dlines 1))
      (setq org-table-current-ncol
	    (length (org-split-string
		     (buffer-substring (line-beginning-position) (line-end-position))
		     "[ \t]*|[ \t]*"))))))

;;;###autoload
(defun org-table-to-lisp (&optional txt)
  "Convert the table at point to a Lisp structure.

The structure will be a list.  Each item is either the symbol `hline'
for a horizontal separator line, or a list of field values as strings.
The table is taken from the parameter TXT, or from the buffer at point."
  (if txt
      (with-temp-buffer
	(buffer-disable-undo)
        (insert txt)
        (goto-char (point-min))
        (org-table-to-lisp))
    (save-excursion
      (goto-char (org-table-begin))
      (let (table)
        (while (progn (skip-chars-forward " \t")
                      (eq (following-char) ?|))
	  (forward-char)
	  (push
	   (if (eq (following-char) ?-)
	       'hline
	     (let (row)
	       (while (progn
                        (skip-chars-forward " \t")
                        (not (eolp)))
                 (let ((q (point)))
                   (skip-chars-forward "^|\n")
                   (goto-char
                    (prog1
                        (let ((p (point)))
                          (unless (eolp) (setq p (1+ p)))
                          p)
	              (skip-chars-backward " \t" q)
                      ;; Preserve text properties.  They are used when
                      ;; calculating cell width.
	              (push (buffer-substring q (point)) row)))))
	       (nreverse row)))
	   table)
	  (forward-line))
	(nreverse table)))))

(provide 'org-table-core)

;;; org-table-core.el ends here
