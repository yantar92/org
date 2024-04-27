;;; org-table-move.el --- Moving around Org tables        -*- lexical-binding: t; -*-

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

;; This file implements functions and commands to move around Org
;; tables.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-table-core)
(require 'org-table-align)

(defcustom org-table-tab-jumps-over-hlines t
  "Non-nil means tab in the last column of a table with jump over a hline.
If a horizontal separator line is following the current line,
`org-table-next-field' can either create a new row before that line, or jump
over the line.  When this option is nil, a new line will be created before
this line."
  :group 'org-table-editing
  :type 'boolean)

(declare-function org-table-maybe-eval-formula "org-table-formula" ())
(declare-function org-table-maybe-recalculate-line "org-table-formula" ())
(declare-function org-table-insert-row "org-table-edit" (&optional arg))
;;;###autoload
(defun org-table-next-field ()
  "Go to the next field in the current table, creating new lines as needed.
Before doing so, re-align the table if necessary."
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (when (and org-table-automatic-realign
	     org-table-may-need-update)
    (org-table-align))
  (let ((end (org-table-end)))
    (if (org-at-table-hline-p)
	(end-of-line 1))
    (condition-case nil
	(progn
	  (re-search-forward "|" end)
	  (if (looking-at "[ \t]*$")
	      (re-search-forward "|" end))
	  (if (and (looking-at "-")
		   org-table-tab-jumps-over-hlines
		   (re-search-forward "^[ \t]*|\\([^-]\\)" end t))
	      (goto-char (match-beginning 1)))
	  (if (looking-at "-")
	      (progn
		(forward-line -1)
		(org-table-insert-row 'below))
	    (if (looking-at " ") (forward-char 1))))
      (error
       (org-table-insert-row 'below)))))

;;;###autoload
(defun org-table-previous-field ()
  "Go to the previous field in the table.
Before doing so, re-align the table if necessary."
  (interactive)
  (org-table-justify-field-maybe)
  (org-table-maybe-recalculate-line)
  (when (and org-table-automatic-realign
	     org-table-may-need-update)
    (org-table-align))
  (when (org-at-table-hline-p)
    (end-of-line))
  (let ((start (org-table-begin))
	(origin (point)))
    (condition-case nil
	(progn
	  (search-backward "|" start nil 2)
	  (while (looking-at-p "|\\(?:-\\|[ \t]*$\\)")
	    (search-backward "|" start)))
      (error
       (goto-char origin)
       (user-error "Cannot move to previous table field"))))
  (when (looking-at "| ?")
    (goto-char (match-end 0))))

(defun org-table-beginning-of-field (&optional n)
  "Move to the beginning of the current table field.
If already at or before the beginning, move to the beginning of the
previous field.
With numeric argument N, move N-1 fields backward first."
  (interactive "p")
  (let ((pos (point)))
    (while (> n 1)
      (setq n (1- n))
      (org-table-previous-field))
    (if (not (re-search-backward "|" (line-beginning-position 0) t))
	(user-error "No more table fields before the current")
      (goto-char (match-end 0))
      (and (looking-at " ") (forward-char 1)))
    (when (>= (point) pos) (org-table-beginning-of-field 2))))

(defun org-table-end-of-field (&optional n)
  "Move to the end of the current table field.
If already at or after the end, move to the end of the next table field.
With numeric argument N, move N-1 fields forward first."
  (interactive "p")
  (let ((pos (point)))
    (while (> n 1)
      (setq n (1- n))
      (org-table-next-field))
    (when (re-search-forward "|" (line-end-position 1) t)
      (backward-char 1)
      (skip-chars-backward " ")
      (when (and (equal (char-before (point)) ?|) (equal (char-after (point)) ?\s))
	(forward-char 1)))
    (when (<= (point) pos) (org-table-end-of-field 2))))

;;;###autoload
(defun org-table-next-row ()
  "Go to the next row (same column) in the current table.
When next row is an hline or outside the table, create a new empty
row.  Before doing so, re-align the table if necessary."
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (if (and org-table-automatic-realign
	   org-table-may-need-update)
      (org-table-align))
  (let ((col (org-table-current-column)))
    (forward-line 1)
    (unless (bolp) (insert "\n"))	;missing newline at eob
    (when (or (not (org-at-table-p))
	      (org-at-table-hline-p))
      (forward-line -1)
      (org-table-insert-row 'below))
    (org-table-goto-column col)
    (skip-chars-backward "^|\n\r")
    (when (looking-at " ") (forward-char))))

(defun org-table-goto-line (N)
  "Go to the Nth data line in the current table.
Return t when the line exists, nil if it does not exist."
  (goto-char (org-table-begin))
  (let ((end (org-table-end)) (cnt 0))
    (while (and (re-search-forward org-table-dataline-regexp end t)
		(< (setq cnt (1+ cnt)) N)))
    (= cnt N)))

(defun org-table-goto-field (ref &optional create-column-p)
  "Move point to a specific field in the current table.

REF is either the name of a field its absolute reference, as
a string.  No column is created unless CREATE-COLUMN-P is
non-nil.  If it is a function, it is called with the column
number as its argument as is used as a predicate to know if the
column can be created.

This function assumes the table is already analyzed (i.e., using
`org-table-analyze')."
  (let* ((coordinates
	  (cond
	   ((cdr (assoc ref org-table-named-field-locations)))
	   ((string-match "\\`@\\([1-9][0-9]*\\)\\$\\([1-9][0-9]*\\)\\'" ref)
	    (list (condition-case nil
		      (aref org-table-dlines
			    (string-to-number (match-string 1 ref)))
		    (error (user-error "Invalid row number in %s" ref)))
		  (string-to-number (match-string 2 ref))))
	   (t (user-error "Unknown field: %s" ref))))
	 (line (car coordinates))
	 (column (nth 1 coordinates))
	 (create-new-column (if (functionp create-column-p)
				(funcall create-column-p column)
			      create-column-p)))
    (when coordinates
      (goto-char org-table-current-begin-pos)
      (forward-line line)
      (org-table-goto-column column nil create-new-column))))

(defun org-table-find-dataline ()
  "Find a data line in the current table, which is needed for column commands.
This function assumes point is in a table.  Raise an error when
there is no data row below."
  (or (not (org-at-table-hline-p))
      (let ((col (current-column))
	    (end (org-table-end)))
	(forward-line)
	(while (and (< (point) end) (org-at-table-hline-p))
	  (forward-line))
	(when (>= (point) end)
	  (user-error "Cannot find data row for column operation"))
	(org-move-to-column col)
	t)))

(provide 'org-table-move)

;;; org-table-move.el ends here
