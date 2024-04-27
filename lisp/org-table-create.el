;;; org-table-create.el --- Import and create Org tables         -*- lexical-binding: t; -*-

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

;; This file contains functions and commands to create/import new Org
;; tables.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ox)

(defgroup org-table-settings nil
  "Settings for tables in Org mode."
  :tag "Org Table Settings"
  :group 'org-table)

(defcustom org-table-default-size "5x2"
  "The default size for newly created tables, Columns x Rows."
  :group 'org-table-settings
  :type 'string)

(defgroup org-table-import-export nil
  "Options concerning table import and export in Org mode."
  :tag "Org Table Import Export"
  :group 'org-table)

(defcustom org-table-convert-region-max-lines 999
  "Max lines that `org-table-convert-region' will attempt to process.

The function can be slow on larger regions; this safety feature
prevents it from hanging Emacs."
  :group 'org-table-import-export
  :type 'integer
  :package-version '(Org . "8.3"))

;;;###autoload
(defun org-table-create-with-table.el ()
  "Use the table.el package to insert a new table.
If there is already a table at point, convert between Org tables
and table.el tables."
  (interactive)
  (require 'table)
  (cond
   ((and (org-at-table.el-p)
	 (y-or-n-p "Convert table to Org table? "))
    (org-table-convert))
   ((and (org-at-table-p)
	 (y-or-n-p "Convert table to table.el table? "))
    (org-table-align)
    (org-table-convert))
   (t (call-interactively 'table-insert))))

;;;###autoload
(defun org-table-create-or-convert-from-region (arg)
  "Convert region to table, or create an empty table.
If there is an active region, convert it to a table, using the function
`org-table-convert-region'.  See the documentation of that function
to learn how the prefix argument is interpreted to determine the field
separator.
If there is no such region, create an empty table with `org-table-create'."
  (interactive "P")
  (if (use-region-p)
      (org-table-convert-region (region-beginning) (region-end) arg)
    (org-table-create arg)))

;;;###autoload
(defun org-table-create (&optional size)
  "Query for a size and insert a table skeleton.
SIZE is a string Columns x Rows like for example \"3x2\"."
  (interactive "P")
  (unless size
    (setq size (read-string
		(concat "Table size Columns x Rows [e.g. "
			org-table-default-size "]: ")
		"" nil org-table-default-size)))

  (let* ((pos (point))
	 (indent (make-string (current-column) ?\ ))
	 (split (org-split-string size " *x *"))
	 (rows (string-to-number (nth 1 split)))
	 (columns (string-to-number (car split)))
	 (line (concat (apply 'concat indent "|" (make-list columns "  |"))
		       "\n")))
    (if (string-match "^[ \t]*$" (buffer-substring-no-properties
                                  (line-beginning-position) (point)))
	(forward-line 0)
      (newline))
    ;; (mapcar (lambda (x) (insert line)) (make-list rows t))
    (dotimes (_ rows) (insert line))
    (goto-char pos)
    (when (> rows 1)
      ;; Insert a hline after the first row.
      (end-of-line 1)
      (insert "\n|-")
      (goto-char pos))
    (org-table-align)))

;;;###autoload
(defun org-table-convert-region (beg0 end0 &optional separator)
  "Convert region to a table.

The region goes from BEG0 to END0, but these borders will be moved
slightly, to make sure a beginning of line in the first line is
included.

Throw an error when the region has more than
`org-table-convert-region-max-lines' lines.

SEPARATOR specifies the field separator in the lines.  It can have the
following values:

(4)     Use the comma as a field separator
(16)    Use a TAB as field separator
(64)    Prompt for a regular expression as field separator
integer  When a number, use that many spaces, or a TAB, as field separator
regexp   When a regular expression, use it to match the separator
nil      When nil, the command tries to be smart and figure out the
         separator in the following way:
         - when each line contains a TAB, assume TAB-separated material
         - when each line contains a comma, assume CSV material
         - else, assume one or more SPACE characters as separator.
`babel-auto'
       Use the same rules as nil, but do not try any separator when
       the region contains a single line and has no commas or tabs."
  (interactive "r\nP")
  (let* ((beg (min beg0 end0))
	 (end (max beg0 end0))
	 re)
    (when (> (count-lines beg end) org-table-convert-region-max-lines)
      (user-error "Region is longer than `org-table-convert-region-max-lines' (%s) lines; not converting"
		  org-table-convert-region-max-lines))
    (when (equal separator '(64))
      (setq separator (read-regexp "Regexp for field separator")))
    (goto-char beg)
    (forward-line 0)
    (setq beg (point-marker))
    (goto-char end)
    (if (bolp) (backward-char 1) (end-of-line 1))
    (setq end (point-marker))
    ;; Get the right field separator
    (when (or (not separator) (eq separator 'babel-auto))
      (goto-char beg)
      (setq separator
	    (cond
	     ((not (save-excursion (re-search-forward "^[^\n\t]+$" end t))) '(16))
	     ((not (save-excursion (re-search-forward "^[^\n,]+$" end t))) '(4))
             ((and (eq separator 'babel-auto)
                   (= 1 (count-lines beg end)))
              (rx unmatchable))
	     (t 1))))
    (goto-char beg)
    (if (equal separator '(4))
	(while (< (point) end)
	  ;; parse the csv stuff
	  (cond
	   ((looking-at "^") (insert "| "))
	   ((looking-at "[ \t]*$") (replace-match " |") (forward-line 1))
	   ((looking-at "[ \t]*\"\\([^\"\n]*\\)\"")
	    (replace-match "\\1")
	    (if (looking-at "\"") (insert "\"")))
	   ((looking-at "[^,\n]+") (goto-char (match-end 0)))
	   ((looking-at "[ \t]*,") (replace-match " | "))
	   (t (forward-line 1))))
      (setq re (cond
		((equal separator '(4)) "^\\|\"?[ \t]*,[ \t]*\"?")
		((equal separator '(16)) "^\\|\t")
		((integerp separator)
		 (if (< separator 1)
		     (user-error "Number of spaces in separator must be >= 1")
		   (format "^ *\\| *\t *\\| \\{%d,\\}" separator)))
		((stringp separator)
		 (format "^ *\\|%s" separator))
		(t (error "This should not happen"))))
      (while (re-search-forward re end t)
	(replace-match "| " t t)))
    (goto-char beg)
    (org-table-align)))

;;;###autoload
(defun org-table-import (file separator)
  "Import FILE as a table.

The command tries to be smart and figure out the separator in the
following way:

- when each line contains a TAB, assume TAB-separated material;
- when each line contains a comma, assume CSV material;
- else, assume one or more SPACE characters as separator.

When non-nil, SEPARATOR specifies the field separator in the
lines.  It can have the following values:

- (4)     Use the comma as a field separator.
- (16)    Use a TAB as field separator.
- (64)    Prompt for a regular expression as field separator.
- integer When a number, use that many spaces, or a TAB, as field separator.
- regexp  When a regular expression, use it to match the separator."
  (interactive "f\nP")
  (when (and (called-interactively-p 'any)
	     (not (string-match-p (rx "." (or "txt" "tsv" "csv") eos) file))
             (not (yes-or-no-p "The file's extension is not .txt, .tsv or .csv.  Import? ")))
    (user-error "Cannot import such file"))
  (unless (bolp) (insert "\n"))
  (let ((beg (point))
	(pm (point-max)))
    (insert-file-contents file)
    (org-table-convert-region beg (+ (point) (- (point-max) pm)) separator)))

(defun org-table-convert ()
  "Convert from Org table to table.el and back.
Obviously, this only works within limits.  When an Org table is converted
to table.el, all horizontal separator lines get lost, because table.el uses
these as cell boundaries and has no notion of horizontal lines.  A table.el
table can be converted to an Org table only if it does not do row or column
spanning.  Multiline cells will become multiple cells.  Beware, Org mode
does not test if the table can be successfully converted - it blindly
applies a recipe that works for simple tables."
  (interactive)
  (require 'table)
  (if (org-at-table.el-p)
      ;; convert to Org table
      (let ((beg (copy-marker (org-table-begin t)))
	    (end (copy-marker (org-table-end t))))
	(table-unrecognize-region beg end)
	(goto-char beg)
	(while (re-search-forward "^\\([ \t]*\\)\\+-.*\n" end t)
	  (replace-match ""))
	(goto-char beg))
    (if (org-at-table-p)
	;; convert to table.el table
	(let ((beg (copy-marker (org-table-begin)))
	      (end (copy-marker (org-table-end))))
	  ;; first, get rid of all horizontal lines
	  (goto-char beg)
	  (while (re-search-forward "^\\([ \t]*\\)|-.*\n" end t)
	    (replace-match ""))
	  ;; insert a hline before first
	  (goto-char beg)
	  (org-table-insert-hline 'above)
	  (forward-line -2)
	  ;; insert a hline after each line
	  (while (progn (forward-line 2) (< (point) end))
	    (org-table-insert-hline))
	  (goto-char beg)
	  (setq end (move-marker end (org-table-end)))
	  ;; replace "+" at beginning and ending of hlines
	  (while (re-search-forward "^\\([ \t]*\\)|-" end t)
	    (replace-match "\\1+-"))
	  (goto-char beg)
	  (while (re-search-forward "-|[ \t]*$" end t)
	    (replace-match "-+"))
	  (goto-char beg)))))

(provide 'org-table-create)

;;; org-table-create.el ends here
