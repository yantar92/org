;;; org-table-edit.el --- Editing Org tables        -*- lexical-binding: t; -*-

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

;; This file implements functions and commands to edit Org tables.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-edit-structure-common)
(require 'org-duration)
(require 'org-time)
(require 'org-table-core)
(require 'org-table-fold)
(require 'org-table-align)
(require 'org-table-move)

(defgroup org-table-editing nil
  "Behavior of tables during editing in Org mode."
  :tag "Org Table Editing"
  :group 'org-table)

(defcustom org-table-auto-blank-field t
  "Non-nil means automatically blank table field when starting to type into it.
This only happens when typing immediately after a field motion
command (TAB, S-TAB or RET)."
  :group 'org-table-editing
  :type 'boolean)

(defcustom org-table-fix-formulas-confirm nil
  "Whether the user should confirm when Org fixes formulas."
  :group 'org-table-editing
  :version "24.1"
  :type '(choice
	  (const :tag "with yes-or-no" yes-or-no-p)
	  (const :tag "with y-or-n" y-or-n-p)
	  (const :tag "no confirmation" nil)))

(defcustom org-table-use-standard-references 'from
  "Non-nil means using table references like B3 instead of @3$2.
Possible values are:
nil     never use them
from    accept as input, do not present for editing
t       accept as input and present for editing"
  :group 'org-table-calculation
  :type '(choice
	  (const :tag "Never, don't even check user input for them" nil)
	  (const :tag "Always, both as user input, and when editing" t)
	  (const :tag "Convert user input, don't offer during editing" from)))

(defcustom org-table-copy-increment t
  "Non-nil means increment when copying current field with \
`\\[org-table-copy-down]'."
  :group 'org-table-calculation
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(choice
	  (const :tag "Use the difference between the current and the above fields" t)
	  (integer :tag "Use a number" 1)
	  (const :tag "Don't increment the value when copying a field" nil)))

(defvar org-field-marker nil)
(defvar org-show-positions nil)

(defvar org-table-clip nil
  "Clipboard for table regions.")

(defun org-table-get (line column)
  "Get the field in table line LINE, column COLUMN.
If LINE is larger than the number of data lines in the table, the function
returns nil.  However, if COLUMN is too large, we will simply return an
empty string.
If LINE is nil, use the current line.
If COLUMN is nil, use the current column."
  (setq column (or column (org-table-current-column)))
  (save-excursion
    (and (or (not line) (org-table-goto-line line))
	 (org-trim (org-table-get-field column)))))

(defun org-table-put (line column value &optional align)
  "Put VALUE into line LINE, column COLUMN.
When ALIGN is set, also realign the table."
  (setq column (or column (org-table-current-column)))
  (prog1 (save-excursion
	   (and (or (not line) (org-table-goto-line line))
		(progn (org-table-goto-column column nil 'force) t)
		(org-table-get-field column value)))
    (and align (org-table-align))))

;;;###autoload
(defun org-table-blank-field ()
  "Blank the current table field or active region."
  (interactive)
  (org-table-check-inside-data-field)
  (if (and (called-interactively-p 'any) (use-region-p))
      (let (org-table-clip)
	(org-table-cut-region (region-beginning) (region-end)))
    (skip-chars-backward "^|")
    (backward-char 1)
    (if (looking-at "|[^|\n]+")
	(let* ((pos (match-beginning 0))
	       (match (match-string 0))
	       (len (save-match-data (org-string-width match nil 'org-table))))
	  (replace-match (concat "|" (make-string (1- len) ?\ )))
	  (goto-char (+ 2 pos))
	  (substring match 1)))))

(defun org-table-get-field (&optional n replace)
  "Return the value of the field in column N of current row.
N defaults to current column.  If REPLACE is a string, replace
field with this value.  The return value is always the old
value."
  (when n (org-table-goto-column n))
  (skip-chars-backward "^|\n")
  (if (or (bolp) (looking-at-p "[ \t]*$"))
      ;; Before first column or after last one.
      ""
    (looking-at "[^|\r\n]*")
    (let* ((pos (match-beginning 0))
	   (val (buffer-substring pos (match-end 0))))
      (when replace
	(org-table-with-shrunk-field
	 (replace-match (if (equal replace "") " " replace) t t)))
      (goto-char (min (line-end-position) (1+ pos)))
      val)))

(defun org-table-fix-formulas (key replace &optional limit delta remove)
  "Modify the equations after the table structure has been edited.
KEY is \"@\" or \"$\".  REPLACE is an alist of numbers to replace.
For all numbers larger than LIMIT, shift them by DELTA."
  (save-excursion
    (goto-char (org-table-end))
    (while (let ((case-fold-search t)) (looking-at "[ \t]*#\\+tblfm:"))
      (let ((re (concat key "\\([0-9]+\\)"))
	    (re2
	     (when remove
	       (if (equal key "$")
		   (format "\\(@[0-9]+\\)?%s%d=.*?\\(::\\|$\\)"
			   (regexp-quote key) remove)
		 (format "@%d\\$[0-9]+=.*?\\(::\\|$\\)" remove))))
	    s n a)
	(when remove
          (save-excursion
            (while (re-search-forward re2 (line-end-position) t)
	      (unless (save-match-data (org-in-regexp "remote([^)]+?)"))
	        (if (equal (char-before (match-beginning 0)) ?.)
		    (user-error
		     "Change makes TBLFM term %s invalid, use undo to recover"
		     (match-string 0))
		  (replace-match ""))))))
        (while (re-search-forward re (line-end-position) t)
	  (unless (save-match-data (org-in-regexp "remote([^)]+?)"))
	    (setq s (match-string 1) n (string-to-number s))
	    (cond
	     ((setq a (assoc s replace))
	      (replace-match (concat key (cdr a)) t t))
	     ((and limit (> n limit))
	      (replace-match (concat key (number-to-string (+ n delta))) t t)))))
	(message "The formulas in #+TBLFM have been updated"))
      (forward-line))))

;;;###autoload
(defun org-table-insert-column ()
  "Insert a new column into the table."
  (interactive)
  (unless (org-at-table-p) (user-error "Not at a table"))
  (when (eobp) (save-excursion (insert "\n")))
  (unless (string-match-p "|[ \t]*$" (org-current-line-string))
    (org-table-align))
  (org-table-find-dataline)
  (let ((col (max 1 (org-table-current-column)))
	(beg (org-table-begin))
	(end (copy-marker (org-table-end)))
	(shrunk-columns (org-table--list-shrunk-columns)))
    (org-table-expand beg end)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
	(unless (org-at-table-hline-p)
	  (org-table-goto-column col t)
	  (insert "|"))
	(forward-line)))
    (org-table-goto-column col)
    (org-table-align)
    ;; Shift appropriately stored shrunk column numbers, then hide the
    ;; columns again.
    (org-table--shrink-columns (mapcar (lambda (c) (if (< c col) c (1+ c)))
				       shrunk-columns)
			       beg end)
    (set-marker end nil)
    ;; Fix TBLFM formulas, if desirable.
    (when (or (not org-table-fix-formulas-confirm)
	      (funcall org-table-fix-formulas-confirm "Fix formulas? "))
      (org-table-fix-formulas "$" nil (1- col) 1))))

(defun org-table-line-to-dline (line &optional above)
  "Turn a buffer line number into a data line number.

If there is no data line in this line, return nil.

If there is no matching dline (most likely the reference was
a hline), the first dline below it is used.  When ABOVE is
non-nil, the one above is used."
  (let ((min 1)
	(max (1- (length org-table-dlines))))
    (cond ((or (> (aref org-table-dlines min) line)
	       (< (aref org-table-dlines max) line))
	   nil)
	  ((= line (aref org-table-dlines max)) max)
	  (t (catch 'exit
	       (while (> (- max min) 1)
		 (let* ((mean (/ (+ max min) 2))
			(v (aref org-table-dlines mean)))
		   (cond ((= v line) (throw 'exit mean))
			 ((> v line) (setq max mean))
			 (t (setq min mean)))))
	       (cond ((= line (aref org-table-dlines max)) max)
		     ((= line (aref org-table-dlines min)) min)
		     (above min)
		     (t max)))))))

(defun org-table--swap-cells (row1 col1 row2 col2)
  "Swap two cells indicated by the coordinates provided.
ROW1, COL1, ROW2, COL2 are integers indicating the row/column
position of the two cells that will be swapped in the table."
  (let ((content1 (org-table-get row1 col1))
	(content2 (org-table-get row2 col2)))
    (org-table-put row1 col1 content2)
    (org-table-put row2 col2 content1)))

(defun org-table--move-cell (direction)
  "Move the current cell in a cardinal direction.
DIRECTION is a symbol among `up', `down', `left', and `right'.
The contents the current cell are swapped with cell in the
indicated direction.  Raise an error if the move cannot be done."
  (let ((row-shift (pcase direction (`up -1) (`down 1) (_ 0)))
	(column-shift (pcase direction (`left -1) (`right 1) (_ 0))))
    (when (and (= 0 row-shift) (= 0 column-shift))
      (error "Invalid direction: %S" direction))
    ;; Initialize `org-table-current-ncol' and `org-table-dlines'.
    (org-table-analyze)
    (let* ((row (org-table-current-line))
	   (column (org-table-current-column))
	   (target-row (+ row row-shift))
	   (target-column (+ column column-shift))
	   (org-table-current-nrow (1- (length org-table-dlines))))
      (when (or (< target-column 1)
		(< target-row 1)
		(> target-column org-table-current-ncol)
		(> target-row org-table-current-nrow))
	(user-error "Cannot move cell further"))
      (org-table--swap-cells row column target-row target-column)
      (org-table-goto-line target-row)
      (org-table-goto-column target-column))))

;;;###autoload
(defun org-table-move-cell-up ()
  "Move a single cell up in a table.
Swap with anything in target cell."
  (interactive)
  (unless (org-table-check-inside-data-field)
    (error "No table at point"))
  (org-table--move-cell 'up)
  (org-table-align))

;;;###autoload
(defun org-table-move-cell-down ()
  "Move a single cell down in a table.
Swap with anything in target cell."
  (interactive)
  (unless (org-table-check-inside-data-field)
    (error "No table at point"))
  (org-table--move-cell 'down)
  (org-table-align))

;;;###autoload
(defun org-table-move-cell-left ()
  "Move a single cell left in a table.
Swap with anything in target cell."
  (interactive)
  (unless (org-table-check-inside-data-field)
    (error "No table at point"))
  (org-table--move-cell 'left)
  (org-table-align))

;;;###autoload
(defun org-table-move-cell-right ()
  "Move a single cell right in a table.
Swap with anything in target cell."
  (interactive)
  (unless (org-table-check-inside-data-field)
    (error "No table at point"))
  (org-table--move-cell 'right)
  (org-table-align))

;;;###autoload
(defun org-table-delete-column ()
  "Delete a column from the table."
  (interactive)
  (unless (org-at-table-p) (user-error "Not at a table"))
  (org-table-find-dataline)
  (when (save-excursion (skip-chars-forward " \t") (eolp))
    (search-backward "|"))		;snap into last column
  (org-table-check-inside-data-field nil t)
  (let* ((col (org-table-current-column))
	 (beg (org-table-begin))
	 (end (copy-marker (org-table-end)))
	 (shrunk-columns (remq col (org-table--list-shrunk-columns))))
    (org-table-expand beg end)
    (org-table-save-field
     (goto-char beg)
     (while (< (point) end)
       (if (org-at-table-hline-p)
	   nil
	 (org-table-goto-column col t)
	 (and (looking-at "|[^|\n]+|")
	      (replace-match "|")))
       (forward-line)))
    (org-table-align)
    ;; Shift appropriately stored shrunk column numbers, then hide the
    ;; columns again.
    (org-table--shrink-columns (mapcar (lambda (c) (if (< c col) c (1- c)))
				       shrunk-columns)
			       beg end)
    (set-marker end nil)
    ;; Fix TBLFM formulas, if desirable.
    (when (or (not org-table-fix-formulas-confirm)
	      (funcall org-table-fix-formulas-confirm "Fix formulas? "))
      (org-table-fix-formulas
       "$" (list (cons (number-to-string col) "INVALID")) col -1 col))))

;;;###autoload
(defun org-table-move-column-right ()
  "Move column to the right."
  (interactive)
  (org-table-move-column nil))

;;;###autoload
(defun org-table-move-column-left ()
  "Move column to the left."
  (interactive)
  (org-table-move-column 'left))

;;;###autoload
(defun org-table-move-column (&optional left)
  "Move the current column to the right.  With arg LEFT, move to the left."
  (interactive "P")
  (unless (org-at-table-p) (user-error "Not at a table"))
  (org-table-find-dataline)
  (org-table-check-inside-data-field nil t)
  (let* ((col (org-table-current-column))
	 (col1 (if left (1- col) col))
	 (colpos (if left (1- col) (1+ col)))
	 (beg (org-table-begin))
	 (end (copy-marker (org-table-end))))
    (when (and left (= col 1))
      (user-error "Cannot move column further left"))
    (when (and (not left) (looking-at "[^|\n]*|[^|\n]*$"))
      (user-error "Cannot move column further right"))
    (let ((shrunk-columns (org-table--list-shrunk-columns)))
      (org-table-expand beg end)
      (org-table-save-field
       (goto-char beg)
       (while (< (point) end)
	 (unless (org-at-table-hline-p)
	   (org-table-goto-column col1 t)
	   (when (looking-at "|\\([^|\n]+\\)|\\([^|\n]+\\)|")
	     (transpose-regions
	      (match-beginning 1) (match-end 1)
	      (match-beginning 2) (match-end 2))))
	 (forward-line)))
      (org-table-goto-column colpos)
      (org-table-align)
      ;; Shift appropriately stored shrunk column numbers, then shrink
      ;; the columns again.
      (org-table--shrink-columns
       (mapcar (lambda (c)
		 (cond ((and (= col c) left) (1- c))
		       ((= col c) (1+ c))
		       ((and (= col (1+ c)) left) (1+ c))
		       ((and (= col (1- c)) (not left) (1- c)))
		       (t c)))
	       shrunk-columns)
       beg end)
      (set-marker end nil)
      ;; Fix TBLFM formulas, if desirable.
      (when (or (not org-table-fix-formulas-confirm)
		(funcall org-table-fix-formulas-confirm "Fix formulas? "))
	(org-table-fix-formulas
	 "$" (list (cons (number-to-string col) (number-to-string colpos))
		   (cons (number-to-string colpos) (number-to-string col))))))))

;;;###autoload
(defun org-table-move-row-down ()
  "Move table row down."
  (interactive)
  (org-table-move-row nil))

;;;###autoload
(defun org-table-move-row-up ()
  "Move table row up."
  (interactive)
  (org-table-move-row 'up))

;;;###autoload
(defun org-table-move-row (&optional up)
  "Move the current table line down.  With arg UP, move it up."
  (interactive "P")
  (let* ((col (current-column))
	 (pos (point))
	 (hline1p (save-excursion (forward-line 0)
				  (looking-at org-table-hline-regexp)))
	 (dline1 (org-table-current-dline))
	 (dline2 (+ dline1 (if up -1 1)))
	 (tonew (if up -1 1))
	 hline2p)
    (when (and up (= (point-min) (line-beginning-position)))
      (user-error "Cannot move row further"))
    (forward-line tonew)
    (when (or (and (not up) (eobp)) (not (org-at-table-p)))
      (goto-char pos)
      (user-error "Cannot move row further"))
    (org-table-with-shrunk-columns
     (setq hline2p (looking-at org-table-hline-regexp))
     (goto-char pos)
     (let ((row (delete-and-extract-region (line-beginning-position)
					   (line-beginning-position 2))))
       (forward-line tonew)
       (unless (bolp) (insert "\n"))	;at eob without a newline
       (insert row)
       (unless (bolp) (insert "\n"))	;missing final newline in ROW
       (forward-line -1)
       (org-move-to-column col)
       (unless (or hline1p hline2p
		   (not (or (not org-table-fix-formulas-confirm)
			    (funcall org-table-fix-formulas-confirm
				     "Fix formulas? "))))
	 (org-table-fix-formulas
	  "@" (list
	       (cons (number-to-string dline1) (number-to-string dline2))
	       (cons (number-to-string dline2) (number-to-string dline1)))))))))

;;;###autoload
(defun org-table-insert-row (&optional arg)
  "Insert a new row above the current line into the table.
With prefix ARG, insert below the current line."
  (interactive "P")
  (unless (org-at-table-p) (user-error "Not at a table"))
  (when (eobp) (save-excursion (insert "\n")))
  (unless (string-match-p "|[ \t]*$" (org-current-line-string))
    (org-table-align))
  (org-table-with-shrunk-columns
   (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
	  (new (org-table-clean-line line)))
     ;; Fix the first field if necessary
     (when (string-match "^[ \t]*| *[#*$] *|" line)
       (setq new (replace-match (match-string 0 line) t t new)))
     (forward-line (if arg 1 0))
     ;; Buffer may not end of a newline character, so ensure
     ;; (forward-line 1) moves point to a new line.
     (unless (bolp) (insert "\n"))
     (let (org-table-may-need-update) (insert-before-markers new "\n"))
     (forward-line -1)
     (re-search-forward "| ?" (line-end-position) t)
     (when (or org-table-may-need-update (bound-and-true-p org-table-overlay-coordinates))
       (org-table-align))
     (when (or (not org-table-fix-formulas-confirm)
	       (funcall org-table-fix-formulas-confirm "Fix formulas? "))
       (org-table-fix-formulas "@" nil (1- (org-table-current-dline)) 1)))))

;;;###autoload
(defun org-table-insert-hline (&optional above)
  "Insert a horizontal-line below the current line into the table.
With prefix ABOVE, insert above the current line."
  (interactive "P")
  (unless (org-at-table-p) (user-error "Not at a table"))
  (when (eobp) (save-excursion (insert "\n")))
  (unless (string-match-p "|[ \t]*$" (org-current-line-string))
    (org-table-align))
  (org-table-with-shrunk-columns
   (let ((line (org-table-clean-line
                (buffer-substring (line-beginning-position) (line-end-position))))
	 (col (current-column)))
     (while (string-match "|\\( +\\)|" line)
       (setq line (replace-match
		   (concat "+" (make-string (- (match-end 1) (match-beginning 1))
					    ?-) "|") t t line)))
     (and (string-match "\\+" line) (setq line (replace-match "|" t t line)))
     (forward-line (if above 0 1))
     (insert line "\n")
     (forward-line (if above 0 -2))
     (org-move-to-column col)
     (when (bound-and-true-p org-table-overlay-coordinates) (org-table-align)))))

(declare-function org-table-maybe-eval-formula "org-table-formula" ())
(declare-function org-table-maybe-recalculate-line "org-table-formula" ())
;;;###autoload
(defun org-table-hline-and-move (&optional same-column)
  "Insert a hline and move to the row below that line."
  (interactive "P")
  (let ((col (org-table-current-column)))
    (org-table-maybe-eval-formula)
    (org-table-maybe-recalculate-line)
    (org-table-insert-hline)
    (end-of-line 2)
    (if (looking-at "\n[ \t]*|-")
	(progn (insert "\n|") (org-table-align))
      (org-table-next-field))
    (if same-column (org-table-goto-column col))))

(defun org-table-clean-line (s)
  "Convert a table line S into a string with only \"|\" and space.
In particular, this does handle wide and invisible characters."
  (if (string-match "^[ \t]*|-" s)
      ;; It's a hline, just map the characters
      (setq s (mapconcat (lambda (x) (if (member x '(?| ?+)) "|" " ")) s ""))
    (while (string-match "|\\([ \t]*?[^ \t\r\n|][^\r\n|]*\\)|" s)
      (setq s (replace-match
	       (concat "|"
                       (make-string
                        (save-match-data
                          (org-string-width (match-string 1 s) nil 'org-table))
			?\ )
                       "|")
	       t t s)))
    s))

;;;###autoload
(defun org-table-kill-row ()
  "Delete the current row or horizontal line from the table."
  (interactive)
  (unless (org-at-table-p) (user-error "Not at a table"))
  (let ((col (current-column))
	(dline (and (not (org-match-line org-table-hline-regexp))
		    (org-table-current-dline))))
    (org-table-with-shrunk-columns
     (kill-region (line-beginning-position)
                  (min (1+ (line-end-position)) (point-max)))
     (if (not (org-at-table-p)) (forward-line -1))
     (org-move-to-column col)
     (when (and dline
		(or (not org-table-fix-formulas-confirm)
		    (funcall org-table-fix-formulas-confirm "Fix formulas? ")))
       (org-table-fix-formulas
	"@" (list (cons (number-to-string dline) "INVALID")) dline -1 dline)))))

;;;###autoload
(defun org-table-cut-region (beg end)
  "Copy region in table to the clipboard and blank all relevant fields.
If there is no active region, use just the field at point."
  (interactive (list
		(if () (region-beginning) (point))
		(if (use-region-p) (region-end) (point))))
  (org-table-copy-region beg end 'cut))

(declare-function org-timestamp-up-day "org-timestamp" (&optional arg))
(defun org-table--increment-field (field previous)
  "Increment string FIELD according to PREVIOUS field.

Increment FIELD only if it is a string representing a number, per
Emacs Lisp syntax, a timestamp, or is either prefixed or suffixed
with a number.  In any other case, return FIELD as-is.

If PREVIOUS has the same structure as FIELD, e.g.,
a number-prefixed string with the same pattern, the increment
step is the difference between numbers (or timestamps, measured
in days) in PREVIOUS and FIELD.  Otherwise, it uses
`org-table-copy-increment', if the variable contains a number, or
default to 1.

The function assumes `org-table-copy-increment' is non-nil."
  (let* ((default-step (if (numberp org-table-copy-increment)
			   org-table-copy-increment
			 1))
	 (number-regexp			;Lisp read syntax for numbers
	  (rx (and string-start
		   (opt (any "+-"))
		   (or (and (one-or-more digit) (opt "."))
		       (and (zero-or-more digit) "." (one-or-more digit)))
		   (opt (any "eE") (opt (opt (any "+-")) (one-or-more digit)))
		   string-end)))
	 (number-prefix-regexp (rx (and string-start (one-or-more digit))))
	 (number-suffix-regexp (rx (and (one-or-more digit) string-end)))
	 (analyze
	  (lambda (field)
	    ;; Analyze string FIELD and return information related to
	    ;; increment or nil.  When non-nil, return value has the
	    ;; following scheme: (TYPE VALUE PATTERN) where
	    ;; - TYPE is a symbol among `number', `prefix', `suffix'
	    ;;   and `timestamp',
	    ;; - VALUE is a timestamp if TYPE is `timestamp', or
	    ;;   a number otherwise,
	    ;; - PATTERN is the field without its prefix, or suffix if
	    ;;   TYPE is either `prefix' or `suffix' , or nil
	    ;;   otherwise.
	    (cond ((not (org-string-nw-p field)) nil)
		  ((string-match-p number-regexp field)
		   (list 'number
			 (string-to-number field)
			 nil))
		  ((string-match number-prefix-regexp field)
		   (list 'prefix
			 (string-to-number (match-string 0 field))
			 (substring field (match-end 0))))
		  ((string-match number-suffix-regexp field)
		   (list 'suffix
			 (string-to-number (match-string 0 field))
			 (substring field 0 (match-beginning 0))))
		  ((string-match-p org-ts-regexp3 field)
		   (list 'timestamp field nil))
		  (t nil))))
	 (next-number-string
	  (lambda (n1 &optional n2)
	    ;; Increment number N1 and return it as a string.  If N2
	    ;; is also a number, deduce increment step from the
	    ;; difference between N1 and N2.  Otherwise, increment
	    ;; step is `default-step'.
	    (number-to-string (if n2 (+ n1 (- n1 n2)) (+ n1 default-step)))))
	 (shift-timestamp
	  (lambda (t1 &optional t2)
	    ;; Increment timestamp T1 and return it.  If T2 is also
	    ;; a timestamp, deduce increment step from the difference,
	    ;; in days, between T1 and T2.  Otherwise, increment by
	    ;; `default-step' days.
	    (with-temp-buffer
	      (insert t1)
              (require 'org-timestamp)
	      (org-timestamp-up-day (if (not t2) default-step
				      (- (org-time-string-to-absolute t1)
					 (org-time-string-to-absolute t2))))
	      (buffer-string)))))
    ;; Check if both PREVIOUS and FIELD have the same type.  Also, if
    ;; the case of prefixed or suffixed numbers, make sure their
    ;; pattern, i.e., the part of the string without the prefix or the
    ;; suffix, is the same.
    (pcase (cons (funcall analyze field) (funcall analyze previous))
      (`((number ,n1 ,_) . (number ,n2 ,_))
       (funcall next-number-string n1 n2))
      (`((number ,n ,_) . ,_)
       (funcall next-number-string n))
      (`((prefix ,n1 ,p1) . (prefix ,n2 ,p2))
       (concat (funcall next-number-string n1 (and (equal p1 p2) n2)) p1))
      (`((prefix ,n ,p) . ,_)
       (concat (funcall next-number-string n) p))
      (`((suffix ,n1 ,p1) . (suffix ,n2 ,p2))
       (concat p1 (funcall next-number-string n1 (and (equal p1 p2) n2))))
      (`((suffix ,n ,p) . ,_)
       (concat p (funcall next-number-string n)))
      (`((timestamp ,t1 ,_) . (timestamp ,t2 ,_))
       (funcall shift-timestamp t1 t2))
      (`((timestamp ,t1 ,_) . ,_)
       (funcall shift-timestamp t1))
      (_ field))))

;;;###autoload
(defun org-table-copy-down (n)
  "Copy the value of the current field one row below.

If the field at the cursor is empty, copy the content of the
nearest non-empty field above.  With argument N, use the Nth
non-empty field.

If the current field is not empty, it is copied down to the next
row, and the cursor is moved with it.  Therefore, repeating this
command causes the column to be filled row-by-row.

If the variable `org-table-copy-increment' is non-nil and the
field is a number, a timestamp, or is either prefixed or suffixed
with a number, it will be incremented while copying.  By default,
increment by the difference between the value in the current
field and the one in the field above, if any.  To increment using
a fixed integer, set `org-table-copy-increment' to a number.  In
the case of a timestamp, increment by days.

However, when N is 0, do not increment the field at all."
  (interactive "p")
  (org-table-check-inside-data-field)
  (let* ((beg (org-table-begin))
	 (column (org-table-current-column))
	 (initial-field (save-excursion
			  (let ((f (org-string-nw-p (org-table-get-field))))
			    (and f (org-trim f)))))
	 field field-above next-field)
    (save-excursion
      ;; Get reference field.
      (if initial-field (setq field initial-field)
	(forward-line 0)
	(setq field
	      (catch :exit
		(while (re-search-backward org-table-dataline-regexp beg t)
		  (let ((f (org-string-nw-p (org-table-get-field column))))
		    (cond ((and (> n 1) f) (cl-decf n))
			  (f (throw :exit (org-trim f)))
			  (t nil))
		    (forward-line 0)))
		(user-error "No non-empty field found"))))
      ;; Check if increment is appropriate, and how it should be done.
      (when (and org-table-copy-increment (/= n 0))
	;; If increment step is not explicit, get non-empty field just
	;; above the field being incremented to guess it.
	(unless (numberp org-table-copy-increment)
	  (setq field-above
		(let ((f (unless (= beg (line-beginning-position))
			   (forward-line -1)
			   (not (org-at-table-hline-p))
			   (org-table-get-field column))))
		  (and (org-string-nw-p f)
		       (org-trim f)))))
	;; Compute next field.
	(setq next-field (org-table--increment-field field field-above))))
    ;; Since initial field in not empty, we modify row below instead.
    ;; Skip alignment since we do it at the end of the process anyway.
    (when initial-field
      (let ((org-table-may-need-update nil)) (org-table-next-row))
      (org-table-blank-field))
    ;; Insert the new field.  NEW-FIELD may be nil if
    ;; `org-table-copy-increment' is nil, or N = 0.  In that case,
    ;; copy FIELD.
    (insert (or next-field field))
    (org-table-maybe-recalculate-line)
    (org-table-align)))

;;;###autoload
(defun org-table-copy-region (beg end &optional cut)
  "Copy rectangular region in table to clipboard.
A special clipboard is used which can only be accessed with
`org-table-paste-rectangle'.  Return the region copied, as a list
of lists of fields."
  (interactive (list
		(if (use-region-p) (region-beginning) (point))
		(if (use-region-p) (region-end) (point))
		current-prefix-arg))
  (goto-char (min beg end))
  (org-table-check-inside-data-field)
  (let ((beg (line-beginning-position))
	(c01 (org-table-current-column))
	region)
    (goto-char (max beg end))
    (org-table-check-inside-data-field nil t)
    (let* ((end (copy-marker (line-end-position)))
	   (c02 (org-table-current-column))
	   (column-start (min c01 c02))
	   (column-end (max c01 c02))
	   (column-number (1+ (- column-end column-start)))
	   (rpl (and cut "  ")))
      (goto-char beg)
      (while (< (point) end)
	(unless (org-at-table-hline-p)
	  ;; Collect every cell between COLUMN-START and COLUMN-END.
	  (let (cols)
	    (dotimes (c column-number)
	      (push (org-table-get-field (+ c column-start) rpl) cols))
	    (push (nreverse cols) region)))
	(forward-line))
      (set-marker end nil))
    (when cut (org-table-align))
    (when (called-interactively-p 'any)
      (message (substitute-command-keys "Cells in the region copied, use \
\\[org-table-paste-rectangle] to paste them in a table.")))
    (setq org-table-clip (nreverse region))))

;;;###autoload
(defun org-table-paste-rectangle ()
  "Paste a rectangular region into a table.
The upper right corner ends up in the current field.  All involved fields
will be overwritten.  If the rectangle does not fit into the present table,
the table is enlarged as needed.  The process ignores horizontal separator
lines."
  (interactive)
  (unless (consp org-table-clip)
    (user-error "First cut/copy a region to paste!"))
  (org-table-check-inside-data-field)
  (let* ((column (org-table-current-column))
	 (org-table-automatic-realign nil))
    (org-table-save-field
     (dolist (row org-table-clip)
       (while (org-at-table-hline-p) (forward-line))
       ;; If we left the table, create a new row.
       (when (and (bolp) (not (looking-at "[ \t]*|")))
	 (end-of-line 0)
	 (org-table-next-field))
       (let ((c column))
	 (dolist (field row)
	   (org-table-goto-column c nil 'force)
	   (org-table-get-field nil field)
	   (cl-incf c)))
       (forward-line)))
    (org-table-align)))

;;;###autoload
(defun org-table-sort-lines
    (&optional with-case sorting-type getkey-func compare-func interactive?)
  "Sort table lines according to the column at point.

The position of point indicates the column to be used for
sorting, and the range of lines is the range between the nearest
horizontal separator lines, or the entire table of no such lines
exist.  If point is before the first column, you will be prompted
for the sorting column.  If there is an active region, the mark
specifies the first line and the sorting column, while point
should be in the last line to be included into the sorting.

The command then prompts for the sorting type which can be
alphabetically, numerically, or by time (as given in a time stamp
in the field, or as a HH:MM value).  Sorting in reverse order is
also possible.

With prefix argument WITH-CASE, alphabetic sorting will be case-sensitive
if the locale allows for it.

If SORTING-TYPE is specified when this function is called from a Lisp
program, no prompting will take place.  SORTING-TYPE must be a character,
any of (?a ?A ?n ?N ?t ?T ?f ?F) where the capital letters indicate that
sorting should be done in reverse order.

If the SORTING-TYPE is ?f or ?F, then GETKEY-FUNC specifies
a function to be called to extract the key.  It must return a value
that is compatible with COMPARE-FUNC, the function used to compare
entries.

A non-nil value for INTERACTIVE? is used to signal that this
function is being called interactively."
  (interactive (list current-prefix-arg nil nil nil t))
  (when (use-region-p) (goto-char (region-beginning)))
  ;; Point must be either within a field or before a data line.
  (save-excursion
    (skip-chars-backward " \t")
    (when (bolp) (search-forward "|" (line-end-position) t))
    (org-table-check-inside-data-field))
  ;; Set appropriate case sensitivity and column used for sorting.
  (let ((column (let ((c (org-table-current-column)))
		  (cond ((> c 0) c)
			(interactive?
			 (read-number "Use column N for sorting: "))
			(t 1))))
	(sorting-type
	 (or sorting-type
	     (read-char-exclusive "Sort Table: [a]lphabetic, [n]umeric, \
\[t]ime, [f]unc.  A/N/T/F means reversed: ")))
	(start (org-table-begin))
	(end (org-table-end)))
    (save-restriction
      ;; Narrow buffer to appropriate sorting area.
      (if (use-region-p)
	  (progn (goto-char (region-beginning))
		 (narrow-to-region
		  (point)
		  (save-excursion (goto-char (region-end))
				  (line-beginning-position 2))))
	(narrow-to-region
	 (save-excursion
	   (if (re-search-backward org-table-hline-regexp start t)
	       (line-beginning-position 2)
	     start))
	 (if (save-excursion (re-search-forward org-table-hline-regexp end t))
	     (match-beginning 0)
	   end)))
      ;; Determine arguments for `sort-subr'.  Also record original
      ;; position.  `org-table-save-field' cannot help here since
      ;; sorting is too much destructive.
      (let* ((coordinates
	      (cons (count-lines (point-min) (line-beginning-position))
		    (current-column)))
	     (extract-key-from-field
	      ;; Function to be called on the contents of the field
	      ;; used for sorting in the current row.
	      (cl-case sorting-type
		((?n ?N) #'string-to-number)
		((?a ?A) #'org-sort-remove-invisible)
		((?t ?T)
		 (lambda (f)
		   (cond ((string-match org-ts-regexp-both f)
			  (float-time
			   (org-time-string-to-time (match-string 0 f))))
			 ((org-duration-p f) (org-duration-to-minutes f))
			 ((string-match "\\<[0-9]+:[0-9]\\{2\\}\\>" f)
			  (org-duration-to-minutes (match-string 0 f)))
			 (t 0))))
		((?f ?F)
		 (or getkey-func
		     (and interactive?
			  (org-read-function "Function for extracting keys: "))
		     (error "Missing key extractor to sort rows")))
		(t (user-error "Invalid sorting type `%c'" sorting-type))))
	     (predicate
	      (cl-case sorting-type
		((?n ?N ?t ?T) #'<)
		((?a ?A) (if with-case #'org-string<
			   (lambda (s1 s2) (org-string< s1 s2 nil t))))
		((?f ?F)
		 (or compare-func
		     (and interactive?
			  (org-read-function
			   "Function for comparing keys (empty for default \
`sort-subr' predicate): "
			   'allow-empty))))))
	     (shrunk-columns (remq column (org-table--list-shrunk-columns))))
	(goto-char (point-min))
	(sort-subr (memq sorting-type '(?A ?N ?T ?F))
		   (lambda ()
		     (forward-line)
		     (while (and (not (eobp))
				 (not (looking-at org-table-dataline-regexp)))
		       (forward-line)))
		   #'end-of-line
		   (lambda ()
		     (funcall extract-key-from-field
			      (org-trim (org-table-get-field column))))
		   nil
		   predicate)
	;; Hide all columns but the one being sorted.
	(org-table--shrink-columns shrunk-columns start end)
	;; Move back to initial field.
	(forward-line (car coordinates))
	(move-to-column (cdr coordinates))))))

(defun org-table-transpose-table-at-point ()
  "Transpose Org table at point and eliminate hlines.
So a table like

| 1 | 2 | 4 | 5 |
|---+---+---+---|
| a | b | c | d |
| e | f | g | h |

will be transposed as

| 1 | a | e |
| 2 | b | f |
| 4 | c | g |
| 5 | d | h |

Note that horizontal lines disappear."
  (interactive)
  (let* ((table (delete 'hline (org-table-to-lisp)))
	 (dline_old (org-table-current-line))
	 (col_old (org-table-current-column))
	 (contents (mapcar (lambda (_)
			     (let ((tp table))
			       (mapcar
				(lambda (_)
				  (prog1
				      (pop (car tp))
				    (setq tp (cdr tp))))
				table)))
			   (car table))))
    (goto-char (org-table-begin))
    (re-search-forward "|")
    (backward-char)
    (delete-region (point) (org-table-end))
    (insert (mapconcat
	     (lambda(x)
	       (concat "| " (mapconcat 'identity x " | " ) "  |\n" ))
	     contents ""))
    (org-table-goto-line col_old)
    (org-table-goto-column dline_old))
  (org-table-align))

(declare-function org-forward-paragraph "org-move" (&optional arg))
;;;###autoload
(defun org-table-wrap-region (arg)
  "Wrap several fields in a column like a paragraph.
This is useful if you'd like to spread the contents of a field over several
lines, in order to keep the table compact.

If there is an active region, and both point and mark are in the same column,
the text in the column is wrapped to minimum width for the given number of
lines.  Generally, this makes the table more compact.  A prefix ARG may be
used to change the number of desired lines.  For example, \
`C-2 \\[org-table-wrap-region]'
formats the selected text to two lines.  If the region was longer than two
lines, the remaining lines remain empty.  A negative prefix argument reduces
the current number of lines by that amount.  The wrapped text is pasted back
into the table.  If you formatted it to more lines than it was before, fields
further down in the table get overwritten - so you might need to make space in
the table first.

If there is no region, the current field is split at the cursor position and
the text fragment to the right of the cursor is prepended to the field one
line down.

If there is no region, but you specify a prefix ARG, the current field gets
blank, and the content is appended to the field above."
  (interactive "P")
  (org-table-check-inside-data-field)
  (if (use-region-p)
      ;; There is a region: fill as a paragraph.
      (let ((start (region-beginning)))
        (save-restriction
          (narrow-to-region
           (save-excursion (goto-char start) (move-beginning-of-line 1))
           (save-excursion (org-forward-paragraph) (point)))
          (org-table-cut-region (region-beginning) (region-end))
	  (when (> (length (car org-table-clip)) 1)
	    (user-error "Region must be limited to single column"))
	  (let ((nlines (cond ((not arg) (length org-table-clip))
			      ((< arg 1) (+ (length org-table-clip) arg))
			      (t arg))))
	    (setq org-table-clip
		  (mapcar #'list
			  (org-wrap (mapconcat #'car org-table-clip " ")
				    nil
				    nlines))))
	  (goto-char start)
	  (org-table-paste-rectangle))
        (org-table-align))
    ;; No region, split the current field at point.
    (unless (org-get-alist-option org-M-RET-may-split-line 'table)
      (skip-chars-forward "^\r\n|"))
    (cond
     (arg				; Combine with field above.
      (let ((s (org-table-blank-field))
	    (col (org-table-current-column)))
	(forward-line -1)
	(while (org-at-table-hline-p) (forward-line -1))
	(org-table-goto-column col)
	(skip-chars-forward "^|")
	(skip-chars-backward " ")
	(insert " " (org-trim s))
	(org-table-align)))
     ((looking-at "\\([^|]+\\)|")	; Split field.
      (let ((s (match-string 1)))
	(replace-match " |")
	(goto-char (match-beginning 0))
	(org-table-next-row)
	(insert (org-trim s) " ")
	(org-table-align)))
     (t (org-table-next-row)))))


;;; Follow Field minor mode

(defcustom org-table-exit-follow-field-mode-when-leaving-table t
  "Non-nil means automatically exit the follow mode.
When nil, the follow mode will stay on and be active in any table
the cursor enters.  Since the table follow filed mode messes with the
window configuration, it is not recommended to set this variable to nil,
except maybe locally in a special file that has mostly tables with long
fields."
  :group 'org-table
  :version "24.1"
  :type 'boolean)

(define-minor-mode org-table-follow-field-mode
  "Minor mode to make the table field editor window follow the cursor.
When this mode is active, the field editor window will always show the
current field.  The mode exits automatically when the cursor leaves the
table (but see `org-table-exit-follow-field-mode-when-leaving-table')."
  :lighter " TblFollow"
  (if org-table-follow-field-mode
      (add-hook 'post-command-hook 'org-table-follow-fields-with-editor
		'append 'local)
    (remove-hook 'post-command-hook 'org-table-follow-fields-with-editor 'local)
    (let* ((buf (get-buffer "*Org Table Edit Field*"))
	   (win (and buf (get-buffer-window buf))))
      (when win (delete-window win))
      (when buf
	(with-current-buffer buf
	  (move-marker org-field-marker nil))
	(kill-buffer buf)))))

;;;###autoload
(defun org-table-edit-field (arg)
  "Edit table field in a different window.
This is mainly useful for fields that contain hidden parts.

When called with a `\\[universal-argument]' prefix, just make the full field
visible so that it can be edited in place.

When called with a `\\[universal-argument] \\[universal-argument]' prefix, \
toggle `org-table-follow-field-mode'."
  (interactive "P")
  (unless (org-at-table-p) (user-error "Not at a table"))
  (cond
   ((equal arg '(16))
    (org-table-follow-field-mode (if org-table-follow-field-mode -1 1)))
   (arg
    (let ((b (save-excursion (skip-chars-backward "^|") (point)))
	  (e (save-excursion (skip-chars-forward "^|\r\n") (point))))
      (remove-text-properties b e '(invisible t intangible t))
      (if font-lock-mode
	  (font-lock-fontify-block))))
   (t
    (let ((pos (point-marker))
	  (coord
	   (if (eq org-table-use-standard-references t)
	       (concat (org-number-to-letters (org-table-current-column))
		       (number-to-string (org-table-current-dline)))
	     (concat "@" (number-to-string (org-table-current-dline))
		     "$" (number-to-string (org-table-current-column)))))
	  (field (org-table-get-field))
	  (cw (current-window-configuration))
	  p)
      (goto-char pos)
      (switch-to-buffer-other-window "*Org Table Edit Field*")
      (when (and (local-variable-p 'org-field-marker)
		 (markerp org-field-marker))
	(move-marker org-field-marker nil))
      (erase-buffer)
      (insert "#\n# Edit field " coord " and finish with C-c C-c\n#\n")
      (let ((org-inhibit-startup t)) (org-mode))
      (auto-fill-mode -1)
      (setq truncate-lines nil)
      (setq word-wrap t)
      (goto-char (setq p (point-max)))
      (insert (org-trim field))
      (remove-text-properties p (point-max) '(invisible t intangible t))
      (goto-char p)
      (setq-local org-finish-function 'org-table-finish-edit-field)
      (setq-local org-window-configuration cw)
      (setq-local org-field-marker pos)
      (message "Edit and finish with C-c C-c")))))

(defun org-table-follow-fields-with-editor ()
  (if (and org-table-exit-follow-field-mode-when-leaving-table
	   (not (org-at-table-p)))
      ;; We have left the table, exit the follow mode
      (org-table-follow-field-mode -1)
    (when (org-table-check-inside-data-field 'noerror)
      (let ((win (selected-window)))
	(org-table-edit-field nil)
	(org-fit-window-to-buffer)
	(select-window win)))))

(defun org-table-finish-edit-field ()
  "Finish editing a table data field.
Remove all newline characters, insert the result into the table, realign
the table and kill the editing buffer."
  (let ((pos org-field-marker)
	(cw org-window-configuration)
	(cb (current-buffer))
	text)
    (goto-char (point-min))
    (while (re-search-forward "^#.*\n?" nil t) (replace-match ""))
    (while (re-search-forward "[ \t]*\n[ \t\n]*" nil t)
      (replace-match " "))
    (setq text (org-trim (buffer-string)))
    (set-window-configuration cw)
    (kill-buffer cb)
    (select-window (get-buffer-window (marker-buffer pos)))
    (goto-char pos)
    (move-marker pos nil)
    (org-table-check-inside-data-field)
    (org-table-get-field nil text)
    (org-table-align)
    (message "New field value inserted")))

(provide 'org-table-edit)

;;; org-table-edit.el ends here
