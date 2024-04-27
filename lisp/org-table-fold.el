;;; org-table-fold.el --- Folding Org tables        -*- lexical-binding: t; -*-

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

;; This file implements folding in Org tables.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element-context)
(require 'org-table-core)

(defcustom org-table-shrunk-column-indicator "…"
  "String to be displayed in a shrunk column."
  :group 'org-table-editing
  :type 'string
  :package-version '(Org . "9.2")
  :safe (lambda (v) (and (stringp v) (not (equal v "")))))

(defconst org-table--separator-space-pre
  (propertize " " 'display '(space :relative-width 1))
  "Space used in front of fields when aligning the table.
This space serves as a segment separator for the purposes of the
bidirectional reordering.
Note that `org-table--separator-space-pre' is not `eq' to
`org-table--separator-space-post'.  This is done to prevent Emacs from
visually merging spaces in an empty table cell.  See bug#45915.")

(defconst org-table--separator-space-post
  (propertize " " 'display '(space :relative-width 1.001))
  "Space used after fields when aligning the table.
This space serves as a segment separator for the purposes of the
bidirectional reordering.
Note that `org-table--separator-space-pre' is not `eq' to
`org-table--separator-space-post'.  This is done to prevent Emacs from
visually merging spaces in an empty table cell.  See bug#45915.")


;;; Macros and Inlined Functions

(defmacro org-table-with-shrunk-columns (&rest body)
  "Expand all columns before executing BODY, then shrink them again."
  (declare (debug (body)))
  (org-with-gensyms (shrunk-columns begin end)
    `(let ((,begin (copy-marker (org-table-begin)))
	   (,end (copy-marker (org-table-end) t))
	   (,shrunk-columns (org-table--list-shrunk-columns)))
       (org-with-point-at ,begin (org-table-expand ,begin ,end))
       (unwind-protect
	   (progn ,@body)
	 (org-table--shrink-columns ,shrunk-columns ,begin ,end)
	 (set-marker ,begin nil)
	 (set-marker ,end nil)))))

(defmacro org-table-with-shrunk-field (&rest body)
  "Save field shrunk state, execute BODY and restore state."
  (declare (debug (body)))
  (org-with-gensyms (end shrunk size)
    `(let* ((,shrunk (save-match-data (org-table--shrunk-field)))
	    (,end (and ,shrunk (copy-marker (overlay-end ,shrunk) t)))
	    (,size (and ,shrunk (- ,end (overlay-start ,shrunk)))))
       (when ,shrunk (delete-overlay ,shrunk))
       (unwind-protect (progn ,@body)
	 (when ,shrunk (move-overlay ,shrunk (- ,end ,size) ,end))))))



;;; Columns Shrinking

(defun org-table--shrunk-field ()
  "Non-nil if current field is narrowed.
When non-nil, return the overlay narrowing the field."
  (cl-some (lambda (o)
	     (and (eq 'table-column-hide (overlay-get o 'org-overlay-type))
		  o))
	   (overlays-at (save-excursion
			  (skip-chars-forward (if (org-at-table-hline-p) "^+|"
						"^|")
					      (line-end-position))
			  (1- (point))))))

(defun org-table--list-shrunk-columns ()
  "List currently shrunk columns in table at point."
  (save-excursion
    ;; We really check shrunk columns in current row only.  It could
    ;; be wrong if all rows do not contain the same number of columns
    ;; (i.e. the table is not properly aligned).  As a consequence,
    ;; some columns may not be shrunk again upon aligning the table.
    ;;
    ;; For example, in the following table, cursor is on first row and
    ;; "<>" indicates a shrunk column.
    ;;
    ;; | |
    ;; | | <> |
    ;;
    ;; Aligning table from the first row will not shrink again the
    ;; second row, which was not visible initially.
    ;;
    ;; However, fixing it requires checking every row, which may be
    ;; slow on large tables.  Moreover, the hindrance of this
    ;; pathological case is very limited.
    (forward-line 0)
    (search-forward "|")
    (let ((separator (if (org-at-table-hline-p) "+" "|"))
	  (column 1)
	  (shrunk (and (org-table--shrunk-field) (list 1)))
	  (end (line-end-position)))
      (while (search-forward separator end t)
	(cl-incf column)
	(when (org-table--shrunk-field) (push column shrunk)))
      (nreverse shrunk))))

(defun org-table--make-shrinking-overlay (start end display field &optional pre)
  "Create an overlay to shrink text between START and END.

Use string DISPLAY instead of the real text between the two
buffer positions.  FIELD is the real contents of the field, as
a string, or nil.  It is meant to be displayed upon moving the
mouse onto the overlay.

When optional argument PRE is non-nil, assume the overlay is
located at the beginning of the field, and prepend
`org-table--separator-space-pre' to it.  Otherwise, concatenate
`org-table-shrunk-column-indicator' at its end.

Return the overlay."
  (let ((show-before-edit
	 (lambda (o &rest _)
	   ;; Removing one overlay removes all other overlays in the
	   ;; same column.
	   (mapc #'delete-overlay
		 (cdr (overlay-get o 'org-table-column-overlays)))))
	(o (make-overlay start end)))
    (overlay-put o 'insert-behind-hooks (list show-before-edit))
    (overlay-put o 'insert-in-front-hooks (list show-before-edit))
    (overlay-put o 'modification-hooks (list show-before-edit))
    (overlay-put o 'org-overlay-type 'table-column-hide)
    (when (stringp field) (overlay-put o 'help-echo field))
    ;; Make sure overlays stays on top of table coordinates overlays.
    ;; See `org-table-overlay-coordinates'.
    (overlay-put o 'priority 1)
    (let ((d (if pre (concat org-table--separator-space-pre display)
	       (concat display org-table-shrunk-column-indicator))))
      (org-overlay-display o d 'org-table t))
    o))

(defun org-table--shrink-field (width align start end contents)
  "Shrink a table field to a specified width.

WIDTH is an integer representing the number of characters to
display, in addition to `org-table-shrunk-column-indicator'.
ALIGN is the alignment of the current column, as either \"l\",
\"c\" or \"r\".  START and END are, respectively, the beginning
and ending positions of the field.  CONTENTS is its trimmed
contents, as a string, or `hline' for table rules.

Real field is hidden under one or two overlays.  They have the
following properties:

  `org-overlay-type'

    Set to `table-column-hide'.  Used to identify overlays
    responsible for shrinking columns in a table.

  `org-table-column-overlays'

    It is a list with the pattern (siblings . COLUMN-OVERLAYS)
    where COLUMN-OVERLAYS is the list of all overlays hiding the
    same column.

Whenever the text behind or next to the overlay is modified, all
the overlays in the column are deleted, effectively displaying
the column again.

Return a list of overlays hiding the field, or nil if field is
already hidden."
  (cond
   ((= start end) nil)			;no field to narrow
   ((org-table--shrunk-field) nil)	;already shrunk
   ((= 0 width)				;shrink to one character
    (list (org-table--make-shrinking-overlay
	   start end "" (if (eq 'hline contents) "" contents))))
   ((eq contents 'hline)
    (list (org-table--make-shrinking-overlay
	   start end (make-string (1+ width) ?-) "")))
   ((equal contents "")			;no contents to hide
    (list
     (let ((w (org-string-width (buffer-substring start end) nil 'org-table))
	   ;; We really want WIDTH + 2 whitespace, to include blanks
	   ;; around fields.
	   (full (+ 2 width)))
       (if (<= w full)
	   (org-table--make-shrinking-overlay
	    (1- end) end (make-string (- full w) ?\s) "")
	 (org-table--make-shrinking-overlay (- end (- w full) 1) end "" "")))))
   (t
    ;; If the field is not empty, display exactly WIDTH characters.
    ;; It can mean to partly hide the field, or extend it with virtual
    ;; blanks.  To that effect, we use one or two overlays.  The
    ;; first, optional, one may add or hide white spaces before the
    ;; contents of the field.  The other, mandatory, one cuts the
    ;; field or displays white spaces at the end of the field.  It
    ;; also always displays `org-table-shrunk-column-indicator'.
    (let* ((lead (org-with-point-at start (skip-chars-forward " ")))
	   (trail (org-with-point-at end (abs (skip-chars-backward " "))))
	   (contents-width (org-string-width
			    (buffer-substring (+ start lead) (- end trail))
                            nil 'org-table)))
      (cond
       ;; Contents are too large to fit in WIDTH character.  Limit, if
       ;; possible, blanks at the beginning of the field to a single
       ;; white space, and cut the field at an appropriate location.
       ((<= width contents-width)
	(let ((pre
	       (and (> lead 0)
		    (org-table--make-shrinking-overlay
		     start (+ start lead) "" contents t)))
	      (post
	       (org-table--make-shrinking-overlay
		;; Find cut location so that WIDTH characters are
		;; visible using dichotomy.
		(let* ((begin (+ start lead))
		       (lower begin)
		       (upper (1- end))
		       ;; Compensate the absence of leading space,
		       ;; thus preserving alignment.
		       (width (if (= lead 0) (1+ width) width)))
		  (catch :exit
		    (while (> (- upper lower) 1)
		      (let ((mean (+ (ash lower -1)
				     (ash upper -1)
				     (logand lower upper 1))))
			(pcase (org-string-width (buffer-substring begin mean) nil 'org-table)
			  ((pred (= width)) (throw :exit mean))
			  ((pred (< width)) (setq upper mean))
			  (_ (setq lower mean)))))
		    upper))
		end "" contents)))
	  (if pre (list pre post) (list post))))
       ;; Contents fit it WIDTH characters.  First compute number of
       ;; white spaces needed on each side of contents, then expand or
       ;; compact blanks on each side of the field in order to
       ;; preserve width and obey to alignment constraints.
       (t
	(let* ((required (- width contents-width))
	       (before
		(pcase align
		  ;; Compensate the absence of leading space, thus
		  ;; preserving alignment.
		  ((guard (= lead 0)) -1)
		  ("l" 0)
		  ("r" required)
		  ("c" (/ required 2))))
	       (after (- required before))
	       (pre
		(pcase (1- lead)
		  ((or (guard (= lead 0)) (pred (= before))) nil)
		  ((pred (< before))
		   (org-table--make-shrinking-overlay
		    start (+ start (- lead before)) "" contents t))
		  (_
		   (org-table--make-shrinking-overlay
		    start (1+ start)
		    (make-string (- before (1- lead)) ?\s)
		    contents t))))
	       (post
		(pcase (1- trail)
		  ((pred (= after))
		   (org-table--make-shrinking-overlay (1- end) end "" contents))
		  ((pred (< after))
		   (org-table--make-shrinking-overlay
		    (+ after (- end trail)) end "" contents))
		  (_
		   (org-table--make-shrinking-overlay
		    (1- end) end
		    (make-string (- after (1- trail)) ?\s)
		    contents)))))
	  (if pre (list pre post) (list post)))))))))

(defun org-table--read-column-selection (select max)
  "Read column selection select as a list of numbers.

SELECT is a string containing column ranges, separated by white
space characters, see `org-table-toggle-column-width' for details.
MAX is the maximum column number.

Return value is a sorted list of numbers.  Ignore any number
outside of the [1;MAX] range."
  (catch :all
    (sort
     (delete-dups
      (cl-mapcan
       (lambda (s)
	 (cond
	  ((member s '("-" "1-")) (throw :all (number-sequence 1 max)))
	  ((string-match-p "\\`[0-9]+\\'" s)
	   (let ((n (string-to-number s)))
	     (and (> n 0) (<= n max) (list n))))
	  ((string-match "\\`\\([0-9]+\\)?-\\([0-9]+\\)?\\'" s)
	   (let ((n (match-string 1 s))
		 (m (match-string 2 s)))
	     (number-sequence (if n (max 1 (string-to-number n))
				1)
			      (if m (min max (string-to-number m))
				max))))
	  (t nil)))			;invalid specification
       (split-string select)))
     #'<)))

(defun org-table--shrink-columns (columns beg end)
  "Shrink COLUMNS in a table.
COLUMNS is a sorted list of column numbers.  BEG and END are,
respectively, the beginning position and the end position of the
table."
  (org-with-wide-buffer
   (font-lock-ensure beg end)
   (dolist (c columns)
     (goto-char beg)
     (let ((align nil)
	   (width nil)
	   (fields nil))
       (while (< (point) end)
	 (catch :continue
	   (let* ((hline? (org-at-table-hline-p))
		  (separator (if hline? "+" "|")))
	     ;; Move to COLUMN.
	     (search-forward "|")
	     (or (= c 1)		;already there
		 (search-forward separator (line-end-position) t (1- c))
		 (throw :continue nil)) ;skip invalid columns
	     ;; Extract boundaries and contents from current field.
	     ;; Also set the column's width if we encounter a width
	     ;; cookie for the first time.
	     (let* ((start (point))
		    (end (progn
			   (skip-chars-forward (concat "^|" separator)
					       (line-end-position))
			   (point)))
		    (contents (if hline? 'hline
				(org-trim (buffer-substring start end)))))
	       (push (list start end contents) fields)
	       (when (and (not hline?)
			  (string-match "\\`<\\([lrc]\\)?\\([0-9]+\\)>\\'"
					contents))
		 (unless align (setq align (match-string 1 contents)))
		 (unless width
		   (setq width (string-to-number (match-string 2 contents))))))))
	 (forward-line))
       ;; Link overlays for current field to the other overlays in the
       ;; same column.
       (let ((chain (list 'siblings)))
	 (dolist (field fields)
	   (dolist (new (apply #'org-table--shrink-field
			       (or width 0) (or align "l") field))
	     (push new (cdr chain))
	     (overlay-put new 'org-table-column-overlays chain))))))))

;;;###autoload
(defun org-table-toggle-column-width (&optional arg)
  "Shrink or expand current column in an Org table.

If a width cookie specifies a width W for the column, the first
W visible characters are displayed.  Otherwise, the column is
shrunk to a single character.

When point is before the first column or after the last one, ask
for the columns to shrink or expand, as a list of ranges.
A column range can be one of the following patterns:

  N    column N only
  N-M  every column between N and M (both inclusive)
  N-   every column between N (inclusive) and the last column
  -M   every column between the first one and M (inclusive)
  -    every column

When optional argument ARG is a string, use it as white space
separated list of column ranges.

When called with `\\[universal-argument]' prefix, call \
`org-table-shrink', i.e.,
shrink columns with a width cookie and expand the others.

When called with `\\[universal-argument] \\[universal-argument]' \
prefix, expand all columns."
  (interactive "P")
  (unless (org-at-table-p) (user-error "Not in a table"))
  (let* ((begin (org-table-begin))
	 (end (org-table-end))
	 ;; Compute an upper bound for the number of columns.
	 ;; Nonexistent columns are ignored anyway.
	 (max-columns (/ (- (line-end-position) (line-beginning-position)) 2))
	 (shrunk (org-table--list-shrunk-columns))
	 (columns
	  (pcase arg
	    (`nil
	     (if (save-excursion
		   (skip-chars-backward "^|" (line-beginning-position))
		   (or (bolp) (looking-at-p "[ \t]*$")))
		 ;; Point is either before first column or past last
		 ;; one.  Ask for columns to operate on.
		 (org-table--read-column-selection
		  (read-string "Column ranges (e.g. 2-4 6-): ")
		  max-columns)
	       (list (org-table-current-column))))
	    ((pred stringp) (org-table--read-column-selection arg max-columns))
	    ((or `(4) `(16)) nil)
	    (_ (user-error "Invalid argument: %S" arg)))))
    (pcase arg
      (`(4) (org-table-shrink begin end))
      (`(16) (org-table-expand begin end))
      (_
       (org-table-expand begin end)
       (org-table--shrink-columns
	(cl-set-exclusive-or columns shrunk) begin end)))))

;;;###autoload
(defun org-table-shrink (&optional begin end)
  "Shrink all columns with a width cookie in the table at point.

Columns without a width cookie are expanded.

Optional arguments BEGIN and END, when non-nil, specify the
beginning and end position of the current table."
  (interactive)
  (unless (or begin (org-at-table-p)) (user-error "Not at a table"))
  (org-with-wide-buffer
   (let ((begin (or begin (org-table-begin)))
	 (end (or end (org-table-end)))
	 (regexp "|[ \t]*<[lrc]?[0-9]+>[ \t]*\\(|\\|$\\)")
	 (columns))
     (goto-char begin)
     (while (re-search-forward regexp end t)
       (goto-char (match-beginning 1))
       (cl-pushnew (org-table-current-column) columns))
     (org-table-expand begin end)
     ;; Make sure invisible characters in the table are at the right
     ;; place since column widths take them into account.
     (font-lock-ensure begin end)
     (org-table--shrink-columns (sort columns #'<) begin end))))

;;;###autoload
(defun org-table-expand (&optional begin end)
  "Expand all columns in the table at point.
Optional arguments BEGIN and END, when non-nil, specify the
beginning and end position of the current table."
  (interactive)
  (unless (or begin (org-at-table-p)) (user-error "Not at a table"))
  (org-with-wide-buffer
   (let ((begin (or begin (org-table-begin)))
	 (end (or end (org-table-end))))
     (remove-overlays begin end 'org-overlay-type 'table-column-hide))))

(provide 'org-table-fold)

;;; org-table-fold.el ends here
