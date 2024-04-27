;;; org-table-align.el --- Aligning Org tables        -*- lexical-binding: t; -*-

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

;; This file implements functions and commands to align Org tables.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-table-core)
(require 'org-table-fold)

(defcustom org-table-number-regexp
  "^\\([<>]?[-+^.0-9]*[0-9][-+^.0-9eEdDx()%:]*\\|[<>]?[-+]?0[xX][[:xdigit:].]+\\|[<>]?[-+]?[0-9]+#[0-9a-zA-Z.]+\\|nan\\|[-+u]?inf\\)$"
  "Regular expression for recognizing numbers in table columns.
If a table column contains mostly numbers, it will be aligned to the
right.  If not, it will be aligned to the left.

The default value of this option is a regular expression which allows
anything which looks remotely like a number as used in scientific
context.  For example, all of the following will be considered a
number:
    12    12.2    2.4e-08    2x10^12    4.034+-0.02    2.7(10)  >3.5

Other options offered by the customize interface are more restrictive."
  :group 'org-table-settings
  :type '(choice
	  (const :tag "Positive Integers"
		 "^[0-9]+$")
	  (const :tag "Integers"
		 "^[-+]?[0-9]+$")
	  (const :tag "Floating Point Numbers"
		 "^[-+]?\\([0-9]*\\.[0-9]+\\|[0-9]+\\.[0-9]*\\)$")
	  (const :tag "Floating Point Number or Integer"
		 "^[-+]?\\([0-9]*\\.[0-9]+\\|[0-9]+\\.?[0-9]*\\)$")
	  (const :tag "Exponential, Floating point, Integer"
		 "^[-+]?[0-9.]+\\([eEdD][-+0-9]+\\)?$")
	  (const :tag "Very General Number-Like, including hex and Calc radix"
		 "^\\([<>]?[-+^.0-9]*[0-9][-+^.0-9eEdDx()%]*\\|[<>]?[-+]?0[xX][[:xdigit:].]+\\|[<>]?[-+]?[0-9]+#[0-9a-zA-Z.]+\\|nan\\|[-+u]?inf\\)$")
	  (const :tag "Very General Number-Like, including hex and Calc radix, allows comma as decimal mark"
		 "^\\([<>]?[-+^.,0-9]*[0-9][-+^.0-9eEdDx()%]*\\|[<>]?[-+]?0[xX][[:xdigit:].]+\\|[<>]?[-+]?[0-9]+#[0-9a-zA-Z.]+\\|nan\\|[-+u]?inf\\)$")
	  (regexp :tag "Regexp:")))

(defcustom org-table-number-fraction 0.5
  "Fraction of numbers in a column required to make the column align right.
In a column all non-white fields are considered.  If at least
this fraction of fields is matched by `org-table-number-regexp',
alignment to the right border applies."
  :group 'org-table-settings
  :type 'number)

(defcustom org-table-automatic-realign t
  "Non-nil means automatically re-align table when pressing TAB or RETURN.
When nil, aligning is only done with `\\[org-table-align]', or after column
removal/insertion."
  :group 'org-table-editing
  :type 'boolean)

(defvar org-table-aligned-begin-marker (make-marker)
  "Marker at the beginning of the table last aligned.
Used to check if cursor still is in that table, to minimize realignment.")

(defvar org-table-aligned-end-marker (make-marker)
  "Marker at the end of the table last aligned.
Used to check if cursor still is in that table, to minimize realignment.")

(defvar org-table-last-alignment nil
  "List of flags for flushright alignment, from the last re-alignment.
This is being used to correctly align a single field after TAB or RET.")

(defvar org-table-last-column-widths nil
  "List of max width of fields in each column.
This is being used to correctly align a single field after TAB or RET.")

;;;###autoload
(defun org-table--align-field (field width align)
  "Format FIELD according to column WIDTH and alignment ALIGN.
FIELD is a string.  WIDTH is a number.  ALIGN is either \"c\",
\"l\" or\"r\"."
  (let* ((spaces (- width (org-string-width field nil 'org-table)))
	 (prefix (pcase align
		   ("l" "")
		   ("r" (make-string spaces ?\s))
		   ("c" (make-string (/ spaces 2) ?\s))))
	 (suffix (make-string (- spaces (length prefix)) ?\s)))
    (concat org-table--separator-space-pre
	    prefix
	    field
	    suffix
	    org-table--separator-space-post)))

(declare-function org-table-overlay-coordinates "org-table-formula-edit" ())
;;;###autoload
(defun org-table-align ()
  "Align the table at point by aligning all vertical bars."
  (interactive)
  (let ((beg (org-table-begin))
	(end (copy-marker (org-table-end))))
    (org-table-save-field
     ;; Make sure invisible characters in the table are at the right
     ;; place since column widths take them into account.
     (font-lock-ensure beg end)
     (move-marker org-table-aligned-begin-marker beg)
     (move-marker org-table-aligned-end-marker end)
     (goto-char beg)
     (org-table-with-shrunk-columns
      (let* ((table (org-table-to-lisp))
             (rows (remq 'hline table))
	     (widths nil)
	     (alignments nil)
	     (columns-number 1))
	(if (null rows)
	    ;; Table contains only horizontal rules.  Compute the
	    ;; number of columns anyway, and choose an arbitrary width
	    ;; and alignment.
	    (let ((end (line-end-position)))
	      (save-excursion
		(while (search-forward "+" end t)
		  (cl-incf columns-number)))
	      (setq widths (make-list columns-number 1))
	      (setq alignments (make-list columns-number "l")))
	  ;; Compute alignment and width for each column.
	  (setq columns-number (apply #'max (mapcar #'length rows)))
	  (dotimes (i columns-number)
	    (let ((max-width 1)
		  (fixed-align? nil)
		  (numbers 0)
		  (non-empty 0))
	      (dolist (row rows)
		(let ((cell (or (nth i row) "")))
		  (setq max-width (max max-width (org-string-width cell nil 'org-table)))
		  (cond (fixed-align? nil)
			((equal cell "") nil)
			((string-match "\\`<\\([lrc]\\)[0-9]*>\\'" cell)
			 (setq fixed-align? (match-string 1 cell)))
			(t
			 (cl-incf non-empty)
			 (when (string-match-p org-table-number-regexp cell)
			   (cl-incf numbers))))))
	      (push max-width widths)
	      (push (cond
		     (fixed-align?)
		     ((>= numbers (* org-table-number-fraction non-empty)) "r")
		     (t "l"))
		    alignments)))
	  (setq widths (nreverse widths))
	  (setq alignments (nreverse alignments)))
	;; Store alignment of this table, for later editing of single
	;; fields.
	(setq org-table-last-alignment alignments)
	(setq org-table-last-column-widths widths)
	;; Build new table rows.  Only replace rows that actually
	;; changed.
	(let ((rule (and (memq 'hline table)
			 (mapconcat (lambda (w) (make-string (+ 2 w) ?-))
				    widths
				    "+")))
              (indent (progn (looking-at "[ \t]*|") (match-string 0))))
	  (dolist (row table)
	    (let ((previous (buffer-substring (point) (line-end-position)))
		  (new
                   (concat indent
		           (if (eq row 'hline) rule
		             (let* ((offset (- columns-number (length row)))
			            (fields (if (= 0 offset) row
                                              ;; Add missing fields.
				              (append row
						      (make-list offset "")))))
			       (mapconcat #'identity
				          (cl-mapcar #'org-table--align-field
					             fields
					             widths
					             alignments)
				          "|")))
		           "|")))
	      (if (equal new previous)
		  (forward-line)
		(insert new "\n")
		(delete-region (point) (line-beginning-position 2))))))
	(set-marker end nil)
	(when (bound-and-true-p org-table-overlay-coordinates)
          (require 'org-table-formula-edit)
          (org-table-overlay-coordinates))
	(setq org-table-may-need-update nil))))))

;;;###autoload
(defun org-table-justify-field-maybe (&optional new)
  "Justify the current field, text to left, number to right.
Optional argument NEW may specify text to replace the current field content."
  ;; FIXME: Prevent newlines inside field.  They are currently not
  ;; supported.
  (when (and (stringp new) (string-match-p "\n" new))
    (message "Removing newlines from formula result: %S" new)
    (setq new (replace-regexp-in-string
               "\n" " "
               (replace-regexp-in-string "\\(^\n+\\)\\|\\(\n+$\\)" "" new))))
  (cond
   ((and (not new) org-table-may-need-update)) ; Realignment will happen anyway
   ((org-at-table-hline-p))
   (t
    (when (or (not (eq (marker-buffer org-table-aligned-begin-marker)
		       (current-buffer)))
	      (< (point) org-table-aligned-begin-marker)
	      (>= (point) org-table-aligned-end-marker))
      ;; This is not the same table, force a full re-align.
      (setq org-table-may-need-update t
            org-table-last-alignment nil
            org-table-last-column-widths nil))
    (when new
      ;; Realign the current field, based on previous full realign.
      (let ((pos (point))
	    (col (org-table-current-column)))
        (when (> col 0)
	  (skip-chars-backward "^|")
	  (if (not (looking-at " *\\(?:\\([^|\n]*?\\) *\\(|\\)\\|\\([^|\n]+?\\) *\\($\\)\\)"))
	      (setq org-table-may-need-update t)
	    (let* ((align (nth (1- col) org-table-last-alignment))
		   (width (nth (1- col) org-table-last-column-widths))
		   (cell (match-string 0))
		   (field (match-string 1))
		   (properly-closed? (/= (match-beginning 2) (match-end 2)))
		   (new-cell
		    (save-match-data
		      (cond (org-table-may-need-update
			     (format " %s |" (or new field)))
			    ((not properly-closed?)
			     (setq org-table-may-need-update t)
			     (format " %s |" (or new field)))
			    ((not new)
			     (concat (org-table--align-field field width align)
				     "|"))
			    ((and width (<= (org-string-width new nil 'org-table) width))
			     (concat (org-table--align-field new width align)
				     "|"))
			    (t
			     (setq org-table-may-need-update t)
			     (format " %s |" new))))))
	      (unless (equal new-cell cell)
	        (let (org-table-may-need-update)
		  (replace-match new-cell t t)))
	      (goto-char pos)))))))))

(provide 'org-table-align)

;;; org-table-align.el ends here
