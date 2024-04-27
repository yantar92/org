;;; org-table-formula.el --- Org table spreadsheet        -*- lexical-binding: t; -*-

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

;; This file implements spreadsheet functionality in Org tables.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-table-core)
(require 'org-table-align)
(require 'org-table-move)
(require 'org-element-timestamp)
(require 'org-table-edit)

(defcustom org-table-duration-hour-zero-padding t
  "Non-nil means hours in table duration computations should be zero-padded.
So this is about 08:32:34 versus 8:33:34."
  :group 'org-table-calculation
  :version "26.1"
  :package-version '(Org . "9.1")
  :type 'boolean
  :safe #'booleanp)

(defcustom org-table-formula-field-format "%s"
  "Format for fields which contain the result of a formula.
For example, using \"~%s~\" will display the result within tilde
characters.  Beware that modifying the display can prevent the
field from being used in another formula."
  :group 'org-table-settings
  :version "24.1"
  :type 'string)

(defgroup org-table-calculation nil
  "Options concerning tables in Org mode."
  :tag "Org Table Calculation"
  :group 'org-table)

(defcustom org-calc-default-modes
  '(calc-internal-prec 12
		       calc-float-format  (float 8)
		       calc-angle-mode    deg
		       calc-prefer-frac   nil
		       calc-symbolic-mode nil
		       calc-date-format (YYYY "-" MM "-" DD " " Www (" " hh ":" mm))
		       calc-display-working-message t)
  "List with Calc mode settings for use in `calc-eval' for table formulas.
The list must contain alternating symbols (Calc modes variables and values).
Don't remove any of the default settings, just change the values.  Org mode
relies on the variables to be present in the list."
  :group 'org-table-calculation
  :type 'plist)

(defcustom org-table-duration-custom-format 'hours
  "Format for the output of calc computations like $1+$2;t.
The default value is `hours', and will output the results as a
number of hours.  Other allowed values are `seconds', `minutes' and
`days', and the output will be a fraction of seconds, minutes or
days.  `hh:mm' selects to use hours and minutes, ignoring seconds.
The `U' flag in a table formula will select this specific format for
a single formula."
  :group 'org-table-calculation
  :version "24.1"
  :type '(choice (symbol :tag "Seconds" 'seconds)
		 (symbol :tag "Minutes" 'minutes)
		 (symbol :tag "Hours  " 'hours)
		 (symbol :tag "Days   " 'days)
		 (symbol :tag "HH:MM  " 'hh:mm)))

(defcustom org-table-formula-evaluate-inline t
  "Non-nil means TAB and RET evaluate a formula in current table field.
If the current field starts with an equal sign, it is assumed to be a formula
which should be evaluated as described in the manual and in the documentation
string of the command `org-table-eval-formula'.  This feature requires the
Emacs calc package.
When this variable is nil, formula calculation is only available through
the command `\\[org-table-eval-formula]'."
  :group 'org-table-calculation
  :type 'boolean)

(defcustom org-table-formula-use-constants t
  "Non-nil means interpret constants in formulas in tables.
A constant looks like `$c' or `$Grav' and will be replaced before evaluation
by the value given in `org-table-formula-constants', or by a value obtained
from the `constants.el' package."
  :group 'org-table-calculation
  :type 'boolean)

(defcustom org-table-formula-constants nil
  "Alist with constant names and values, for use in table formulas.
The car of each element is a name of a constant, without the `$' before it.
The cdr is the value as a string.  For example, if you'd like to use the
speed of light in a formula, you would configure

  (setq org-table-formula-constants \\='((\"c\" . \"299792458.\")))

and then use it in an equation like `$1*$c'.

Constants can also be defined on a per-file basis using a line like

#+CONSTANTS: c=299792458. pi=3.14 eps=2.4e-6"
  :group 'org-table-calculation
  :type '(repeat
	  (cons (string :tag "name")
		(string :tag "value"))))

(defcustom org-table-allow-automatic-line-recalculation t
  "Non-nil means lines marked with |#| or |*| will be recomputed automatically.
\\<org-mode-map>\
Automatically means when `TAB' or `RET' or `\\[org-ctrl-c-ctrl-c]' \
are pressed in the line."
  :group 'org-table-calculation
  :type 'boolean)

(defcustom org-table-relative-ref-may-cross-hline t
  "Non-nil means relative formula references may cross hlines.
Here are the allowed values:

nil    Relative references may not cross hlines.  They will reference the
       field next to the hline instead.  Coming from below, the reference
       will be to the field below the hline.  Coming from above, it will be
       to the field above.
t      Relative references may cross hlines.
error  An attempt to cross a hline will throw an error.

It is probably good to never set this variable to nil, for the sake of
portability of tables."
  :group 'org-table-calculation
  :type '(choice
          (const :tag "Allow crossing hline" t)
	  (const :tag "Stick to hline" nil)
	  (const :tag "Error on attempt to cross" error)))

(defcustom org-table-formula-create-columns nil
  "Non-nil means evaluation of formula can add new columns.
When non-nil, evaluating an out-of-bounds field can insert as
many columns as needed.  When set to `warn', issue a warning when
doing so.  When set to `prompt', ask user before creating a new
column.  Otherwise, throw an error."
  :group 'org-table-calculation
  :package-version '(Org . "8.3")
  :type '(choice
	  (const :tag "Out-of-bounds field generates an error (default)" nil)
	  (const :tag "Out-of-bounds field silently adds columns as needed" t)
	  (const :tag "Out-of-bounds field adds columns, but issues a warning" warn)
	  (const :tag "Prompt user when setting an out-of-bounds field" prompt)))


(defvar org-table-formula-history nil)
(defvar org-last-recalc-line nil)

(defvar org-recalc-commands nil
  "List of commands triggering the recalculation of a line.
Will be filled automatically during use.")

(defvar org-recalc-marks
  '((" " . "Unmarked: no special line, no automatic recalculation")
    ("#" . "Automatically recalculate this line upon TAB, RET, and C-c C-c in the line")
    ("*" . "Recalculate only when entire table is recalculated with `C-u C-c *'")
    ("!" . "Column name definition line.  Reference in formula as $name.")
    ("$" . "Parameter definition line name=value.  Reference in formula as $name.")
    ("_" . "Names for values in row below this one.")
    ("^" . "Names for values in row above this one.")))

(defvar-local org-table-formula-debug nil
  "Non-nil means debug table formulas.
When nil, simply write \"#ERROR\" in corrupted fields.")

;;;###autoload
(defun org-table-toggle-formula-debugger ()
  "Toggle the formula debugger in tables."
  (interactive)
  (setq org-table-formula-debug (not org-table-formula-debug))
  (message "Formula debugging has been turned %s"
	   (if org-table-formula-debug "on" "off")))


;;; Formulas

(defun org-table-current-field-formula (&optional key noerror)
  "Return the formula active for the current field.

Assumes that table is already analyzed.  If KEY is given, return
the key to this formula.  Otherwise return the formula preceded
with \"=\" or \":=\"."
  (let* ((line (count-lines org-table-current-begin-pos
			    (line-beginning-position)))
	 (row (org-table-line-to-dline line)))
    (cond
     (row
      (let* ((col (org-table-current-column))
	     (name (car (rassoc (list line col)
				org-table-named-field-locations)))
	     (scol (format "$%d" col))
	     (ref (format "@%d$%d" (org-table-current-dline) col))
	     (stored-list (org-table-get-stored-formulas noerror))
	     (ass (or (assoc name stored-list)
		      (assoc ref stored-list)
		      (assoc scol stored-list))))
	(cond (key (car ass))
	      (ass (concat (if (string-match-p "^[0-9]+$" (car ass)) "=" ":=")
			   (cdr ass))))))
     (noerror nil)
     (t (error "No formula active for the current field")))))

(defun org-table-formula-to-user (s)
  "Convert a formula from internal to user representation."
  (if (eq org-table-use-standard-references t)
      (org-table-convert-refs-to-an s)
    s))

(defun org-table-formula-from-user (s)
  "Convert a formula from user to internal representation."
  (if org-table-use-standard-references
      (org-table-convert-refs-to-rc s)
    s))

(defun org-table-get-formula (&optional equation named)
  "Read a formula from the minibuffer, offer stored formula as default.
When NAMED is non-nil, look for a named equation."
  (let* ((stored-list (org-table-get-stored-formulas))
	 (name (car (rassoc (list (count-lines org-table-current-begin-pos
					       (line-beginning-position))
				  (org-table-current-column))
			    org-table-named-field-locations)))
	 (ref (format "@%d$%d"
		      (org-table-current-dline)
		      (org-table-current-column)))
	 (scol (cond
		((not named) (format "$%d" (org-table-current-column)))
		(name)
		(t ref)))
	 (name (or name ref))
	 (org-table-may-need-update nil)
	 (stored (cdr (assoc scol stored-list)))
	 (eq (cond
	      ((and stored equation (string-match-p "^ *=? *$" equation))
	       stored)
	      ((stringp equation) equation)
	      (t
	       (org-table-formula-from-user
		(read-string
		 (org-table-formula-to-user
		  (format "%s formula %s=" (if named "Field" "Column") scol))
		 (if stored (org-table-formula-to-user stored) "")
		 'org-table-formula-history)))))
	 mustsave)
    (unless (org-string-nw-p eq)
      ;; Remove formula.
      (setq stored-list (delq (assoc scol stored-list) stored-list))
      (org-table-store-formulas stored-list)
      (user-error "Formula removed"))
    (when (string-match "^ *=?" eq) (setq eq (replace-match "" t t eq)))
    (when (string-match " *$" eq) (setq eq (replace-match "" t t eq)))
    (when (and name (not named))
      ;; We set the column equation, delete the named one.
      (setq stored-list (delq (assoc name stored-list) stored-list)
	    mustsave t))
    (if stored
	(setcdr (assoc scol stored-list) eq)
      (setq stored-list (cons (cons scol eq) stored-list)))
    (when (or mustsave (not (equal stored eq)))
      (org-table-store-formulas stored-list))
    eq))

(declare-function org-indent-line "org-indent-static" ())
(defun org-table-store-formulas (alist &optional location)
  "Store the list of formulas below the current table.
If optional argument LOCATION is a buffer position, insert it at
LOCATION instead."
  (save-excursion
    (if location
	(progn (goto-char location) (forward-line 0))
      (goto-char (org-table-end)))
    (let ((case-fold-search t))
      (if (looking-at "\\([ \t]*\n\\)*[ \t]*\\(#\\+TBLFM:\\)\\(.*\n?\\)")
	  (progn
	    ;; Don't overwrite TBLFM, we might use text properties to
	    ;; store stuff.
	    (goto-char (match-beginning 3))
	    (delete-region (match-beginning 3) (match-end 0)))
        (require 'org-indent-static)
	(org-indent-line)
	(insert "#+TBLFM:"))
      (insert " "
	      (mapconcat (lambda (x) (concat (car x) "=" (cdr x)))
			 (sort alist #'org-table-formula-less-p)
			 "::")
	      "\n"))))

(defsubst org-table-formula-make-cmp-string (a)
  (when (string-match "\\`\\$[<>]" a)
    (let ((arrow (string-to-char (substring a 1))))
      ;; Fake a high number to make sure this is sorted at the end.
      (setq a (org-table-formula-handle-first/last-rc a))
      (setq a (format "$%d" (+ 10000
			       (if (= arrow ?<) -1000 0)
			       (string-to-number (substring a 1)))))))
  (when (string-match
	 "^\\(@\\([0-9]+\\)\\)?\\(\\$?\\([0-9]+\\)\\)?\\(\\$?[a-zA-Z0-9]+\\)?"
	 a)
    (concat
     (if (match-end 2)
	 (format "@%05d" (string-to-number (match-string 2 a))) "")
     (if (match-end 4)
	 (format "$%05d" (string-to-number (match-string 4 a))) "")
     (if (match-end 5)
	 (concat "@@" (match-string 5 a))))))

(defun org-table-formula-less-p (a b)
  "Compare two formulas for sorting."
  (let ((as (org-table-formula-make-cmp-string (car a)))
	(bs (org-table-formula-make-cmp-string (car b))))
    (and as bs (string< as bs))))

;;;###autoload
(defun org-table-get-stored-formulas (&optional noerror location)
  "Return an alist with the stored formulas directly after current table.
By default, only return active formulas, i.e., formulas located
on the first line after the table.  However, if optional argument
LOCATION is a buffer position, consider the formulas there."
  (save-excursion
    (if location
	(progn (goto-char location) (forward-line 0))
      (goto-char (org-table-end)))
    (let ((case-fold-search t))
      (when (looking-at "\\([ \t]*\n\\)*[ \t]*#\\+TBLFM: *\\(.*\\)")
	(let ((strings (org-split-string (match-string-no-properties 2)
					 " *:: *"))
	      eq-alist seen)
	  (dolist (string strings (nreverse eq-alist))
	    (when (string-match "\\`\\(@[-+I<>0-9.$@]+\\|\\$\\([_a-zA-Z0-9]+\\|\
\[<>]+\\)\\) *= *\\(.*[^ \t]\\)"
				string)
	      (let ((lhs
		     (let ((m (match-string 1 string)))
		       (cond
			((not (match-end 2)) m)
			;; Is it a column reference?
			((string-match-p "\\`\\$\\([0-9]+\\|[<>]+\\)\\'" m) m)
			;; Since named columns are not possible in
			;; LHS, assume this is a named field.
			(t (match-string 2 string)))))
		    (rhs (match-string 3 string)))
		(push (cons lhs rhs) eq-alist)
		(cond
		 ((not (member lhs seen)) (push lhs seen))
		 (noerror
		  (message
		   "Double definition `%s=' in TBLFM line, please fix by hand"
		   lhs)
		  (ding)
		  (sit-for 2))
		 (t
		  (user-error
		   "Double definition `%s=' in TBLFM line, please fix by hand"
		   lhs)))))))))))

;;;###autoload
(defun org-table-maybe-eval-formula ()
  "Check if the current field starts with \"=\" or \":=\".
If yes, store the formula and apply it."
  ;; We already know we are in a table.  Get field will only return a formula
  ;; when appropriate.  It might return a separator line, but no problem.
  (when org-table-formula-evaluate-inline
    (let* ((field (org-trim (or (org-table-get-field) "")))
	   named eq)
      (when (string-match "^:?=\\(.*[^=]\\)$" field)
	(setq named (equal (string-to-char field) ?:)
	      eq (match-string 1 field))
	(org-table-eval-formula (and named '(4))
				(org-table-formula-from-user eq))))))

;;;###autoload
(defun org-table-rotate-recalc-marks (&optional newchar)
  "Rotate the recalculation mark in the first column.
If in any row, the first field is not consistent with a mark,
insert a new column for the markers.
When there is an active region, change all the lines in the region,
after prompting for the marking character.
After each change, a message will be displayed indicating the meaning
of the new mark."
  (interactive)
  (unless (org-at-table-p) (user-error "Not at a table"))
  (let* ((region (use-region-p))
	 (l1 (and region
		  (save-excursion (goto-char (region-beginning))
				  (copy-marker (line-beginning-position)))))
	 (l2 (and region
		  (save-excursion (goto-char (region-end))
				  (copy-marker (line-beginning-position)))))
	 (l (copy-marker (line-beginning-position)))
	 (col (org-table-current-column))
	 (newchar (if region
		      (char-to-string
		       (read-char-exclusive
			"Change region to what mark?  Type # * ! $ or SPC: "))
		    newchar))
	 (no-special-column
	  (save-excursion
	    (goto-char (org-table-begin))
	    (re-search-forward
	     "^[ \t]*|[^-|][^|]*[^#!$*_^| \t][^|]*|" (org-table-end) t))))
    (when (and newchar (not (assoc newchar org-recalc-marks)))
      (user-error "Invalid character `%s' in `org-table-rotate-recalc-marks'"
		  newchar))
    (when l1 (goto-char l1))
    (save-excursion
      (forward-line 0)
      (unless (looking-at org-table-dataline-regexp)
	(user-error "Not at a table data line")))
    (when no-special-column
      (org-table-goto-column 1)
      (org-table-insert-column))
    (let ((previous-line-end (line-end-position))
	  (newchar
	   (save-excursion
	     (forward-line 0)
	     (cond ((not (looking-at "^[ \t]*| *\\([#!$*^_ ]\\) *|")) "#")
		   (newchar)
		   (t (cadr (member (match-string 1)
				    (append (mapcar #'car org-recalc-marks)
					    '(" ")))))))))
      ;; Rotate mark in first row.
      (org-table-get-field 1 (format " %s " newchar))
      ;; Rotate marks in additional rows if a region is active.
      (when region
	(save-excursion
	  (forward-line)
	  (while (<= (point) l2)
	    (when (looking-at org-table-dataline-regexp)
	      (org-table-get-field 1 (format " %s " newchar)))
	    (forward-line))))
      ;; Only align if rotation actually changed lines' length.
      (when (/= previous-line-end (line-end-position)) (org-table-align)))
    (goto-char l)
    (org-table-goto-column (if no-special-column (1+ col) col))
    (when l1 (set-marker l1 nil))
    (when l2 (set-marker l2 nil))
    (set-marker l nil)
    (when (called-interactively-p 'interactive)
      (message "%s" (cdr (assoc newchar org-recalc-marks))))))

;;;###autoload
(defun org-table-maybe-recalculate-line ()
  "Recompute the current line if marked for it, and if we haven't just done it."
  (interactive)
  (and org-table-allow-automatic-line-recalculation
       (not (and (memq last-command org-recalc-commands)
	       (eq org-last-recalc-line (line-beginning-position))))
       (save-excursion (forward-line 0)
		       (looking-at org-table-auto-recalculate-regexp))
       (org-table-recalculate) t))

(defun org-table-time-seconds-to-string (secs &optional output-format)
  "Convert a number of seconds to a time string.
If OUTPUT-FORMAT is non-nil, return a number of days, hours,
minutes or seconds."
  (let* ((secs0 (abs secs))
	 (res
	  (cond ((eq output-format 'days)
		 (format "%.3f" (/ (float secs0) 86400)))
		((eq output-format 'hours)
		 (format "%.2f" (/ (float secs0) 3600)))
		((eq output-format 'minutes)
		 (format "%.1f" (/ (float secs0) 60)))
		((eq output-format 'seconds)
		 (format "%d" secs0))
		((eq output-format 'hh:mm)
		 ;; Ignore seconds
		 (substring (format-seconds
			     (if org-table-duration-hour-zero-padding
				 "%.2h:%.2m:%.2s" "%h:%.2m:%.2s")
			     secs0)
			    0 -3))
		(t (format-seconds
		    (if org-table-duration-hour-zero-padding
			"%.2h:%.2m:%.2s" "%h:%.2m:%.2s")
		    secs0)))))
    (if (< secs 0) (concat "-" res) res)))

(defun org-table-time-string-to-seconds (s)
  "Convert a time string into numerical duration in seconds.
S can be a string matching either -?HH:MM:SS or -?HH:MM.
If S is a string representing a number, keep this number."
  (if (equal s "")
      s
    (let (hour minus min sec res)
      (cond
       ((and (string-match "\\(-?\\)\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" s))
	(setq minus (< 0 (length (match-string 1 s)))
	      hour (string-to-number (match-string 2 s))
	      min (string-to-number (match-string 3 s))
	      sec (string-to-number (match-string 4 s)))
	(if minus
	    (setq res (- (+ (* hour 3600) (* min 60) sec)))
	  (setq res (+ (* hour 3600) (* min 60) sec))))
       ((and (not (string-match org-ts-regexp-both s))
	     (string-match "\\(-?\\)\\([0-9]+\\):\\([0-9]+\\)" s))
	(setq minus (< 0 (length (match-string 1 s)))
	      hour (string-to-number (match-string 2 s))
	      min (string-to-number (match-string 3 s)))
	(if minus
	    (setq res (- (+ (* hour 3600) (* min 60))))
	  (setq res (+ (* hour 3600) (* min 60)))))
       (t (setq res (string-to-number s))))
      (number-to-string res))))

;;;###autoload
(defun org-table-eval-formula (&optional arg equation
					 suppress-align suppress-const
					 suppress-store suppress-analysis)
  "Replace the table field value at the cursor by the result of a calculation.

In a table, this command replaces the value in the current field with the
result of a formula.  It also installs the formula as the \"current\" column
formula, by storing it in a special line below the table.  When called
with a `\\[universal-argument]' prefix the formula is installed as a \
field formula.

When called with a `\\[universal-argument] \\[universal-argument]' prefix, \
insert the active equation for the field
back into the current field, so that it can be edited there.  This is \
useful
in order to use \\<org-table-fedit-map>`\\[org-table-show-reference]' to \
check the referenced fields.

When called, the command first prompts for a formula, which is read in
the minibuffer.  Previously entered formulas are available through the
history list, and the last used formula is offered as a default.
These stored formulas are adapted correctly when moving, inserting, or
deleting columns with the corresponding commands.

The formula can be any algebraic expression understood by the Calc package.
For details, see the Org mode manual.

This function can also be called from Lisp programs and offers
additional arguments: EQUATION can be the formula to apply.  If this
argument is given, the user will not be prompted.

SUPPRESS-ALIGN is used to speed-up recursive calls by by-passing
unnecessary aligns.

SUPPRESS-CONST suppresses the interpretation of constants in the
formula, assuming that this has been done already outside the
function.

SUPPRESS-STORE means the formula should not be stored, either
because it is already stored, or because it is a modified
equation that should not overwrite the stored one.

SUPPRESS-ANALYSIS prevents analyzing the table and checking
location of point."
  (interactive "P")
  (unless suppress-analysis
    (org-table-check-inside-data-field nil t)
    (org-table-analyze))
  (if (equal arg '(16))
      (let ((eq (org-table-current-field-formula)))
	(org-table-get-field nil eq)
	(org-table-align)
	(setq org-table-may-need-update t))
    (let* (fields
	   (ndown (if (integerp arg) arg 1))
	   (org-table-automatic-realign nil)
	   (case-fold-search nil)
	   (down (> ndown 1))
	   (formula (if (and equation suppress-store)
			equation
		      (org-table-get-formula equation (equal arg '(4)))))
	   (n0 (org-table-current-column))
	   (calc-modes (copy-sequence org-calc-default-modes))
	   (numbers nil)	   ; was a variable, now fixed default
	   (keep-empty nil)
	   form form0 formrpl formrg bw fmt ev orig lispp literal
	   duration duration-output-format)
      ;; Parse the format string.  Since we have a lot of modes, this is
      ;; a lot of work.  However, I think calc still uses most of the time.
      (if (string-match "\\(.*\\);\\(.*\\)" formula)
	  (progn
	    (setq fmt (concat (cdr (assoc "%" org-table-local-parameters))
			      (match-string-no-properties 2 formula)))
	    (setq formula (match-string-no-properties 1 formula))
	    (while (string-match "\\([pnfse]\\)\\(-?[0-9]+\\)" fmt)
	      (let ((c (string-to-char (match-string 1 fmt)))
		    (n (string-to-number (match-string 2 fmt))))
		(cl-case c
		  (?p (setf (cl-getf calc-modes 'calc-internal-prec) n))
		  (?n (setf (cl-getf calc-modes 'calc-float-format) (list 'float n)))
		  (?f (setf (cl-getf calc-modes 'calc-float-format) (list 'fix n)))
		  (?s (setf (cl-getf calc-modes 'calc-float-format) (list 'sci n)))
		  (?e (setf (cl-getf calc-modes 'calc-float-format) (list 'eng n)))))
	      ;; Remove matched flags from the mode string.
	      (setq fmt (replace-match "" t t fmt)))
	    (while (string-match "\\([tTUNLEDRFSu]\\)" fmt)
	      (let ((c (string-to-char (match-string 1 fmt))))
		(cl-case c
		  (?t (setq duration t numbers t
                            duration-output-format org-table-duration-custom-format))
		  (?T (setq duration t numbers t duration-output-format nil))
		  (?U (setq duration t numbers t duration-output-format 'hh:mm))
		  (?N (setq numbers t))
		  (?L (setq literal t))
		  (?E (setq keep-empty t))
		  (?D (setf (cl-getf calc-modes 'calc-angle-mode) 'deg))
		  (?R (setf (cl-getf calc-modes 'calc-angle-mode) 'rad))
		  (?F (setf (cl-getf calc-modes 'calc-prefer-frac) t))
		  (?S (setf (cl-getf calc-modes 'calc-symbolic-mode) t))
		  (?u (setf (cl-getf calc-modes 'calc-simplify-mode) 'units))))
	      ;; Remove matched flags from the mode string.
	      (setq fmt (replace-match "" t t fmt)))
	    (unless (string-match "\\S-" fmt)
	      (setq fmt nil))))
      (when (and (not suppress-const) org-table-formula-use-constants)
	(setq formula (org-table-formula-substitute-names formula)))
      (setq orig (or (get-text-property 1 :orig-formula formula) "?"))
      (setq formula (org-table-formula-handle-first/last-rc formula))
      (while (> ndown 0)
	(setq fields (org-split-string
		      (org-trim
		       (buffer-substring-no-properties
			(line-beginning-position) (line-end-position)))
		      " *| *"))
	;; replace fields with duration values if relevant
	(if duration
	    (setq fields
		  (mapcar (lambda (x) (org-table-time-string-to-seconds x))
			  fields)))
	(if (eq numbers t)
	    (setq fields (mapcar
			  (lambda (x)
			    (if (string-match "\\S-" x)
				(number-to-string (string-to-number x))
			      x))
			  fields)))
	(setq ndown (1- ndown))
	(setq form (copy-sequence formula)
	      lispp (and (> (length form) 2) (equal (substring form 0 2) "'(")))
	(if (and lispp literal) (setq lispp 'literal))

	;; Insert row and column number of formula result field
	(while (string-match "[@$]#" form)
	  (setq form
		(replace-match
		 (format "%d"
			 (save-match-data
			   (if (equal (substring form (match-beginning 0)
						 (1+ (match-beginning 0)))
				      "@")
			       (org-table-current-dline)
			     (org-table-current-column))))
		 t t form)))

	;; Check for old vertical references
	(org-table--error-on-old-row-references form)
	;; Insert remote references
	(setq form (org-table-remote-reference-indirection form))
	(while (string-match "\\<remote([ \t]*\\([^,)]+\\)[ \t]*,[ \t]*\\([^\n)]+\\))" form)
	  (setq form
		(replace-match
		 (save-match-data
		   (org-table-make-reference
		    (let ((rmtrng (org-table-get-remote-range
				   (match-string 1 form) (match-string 2 form))))
		      (if duration
			  (if (listp rmtrng)
			      (mapcar (lambda(x) (org-table-time-string-to-seconds x)) rmtrng)
			    (org-table-time-string-to-seconds rmtrng))
			rmtrng))
		    keep-empty numbers lispp))
		 t t form)))
	;; Insert complex ranges
	(while (and (string-match org-table-range-regexp form)
		    (> (length (match-string 0 form)) 1))
	  (setq formrg
		(save-match-data
		  (org-table-get-range
		   (match-string 0 form) org-table-current-begin-pos n0)))
	  (setq formrpl
		(save-match-data
		  (org-table-make-reference
		   ;; possibly handle durations
		   (if duration
		       (if (listp formrg)
			   (mapcar (lambda(x) (org-table-time-string-to-seconds x)) formrg)
			 (org-table-time-string-to-seconds formrg))
		     formrg)
		   keep-empty numbers lispp)))
	  (if (not (save-match-data
		   (string-match (regexp-quote form) formrpl)))
	      (setq form (replace-match formrpl t t form))
	    (user-error "Spreadsheet error: invalid reference \"%s\"" form)))
	;; Insert simple ranges, i.e. included in the current row.
	(while (string-match
		"\\$\\(\\([-+]\\)?[0-9]+\\)\\.\\.\\$\\(\\([-+]\\)?[0-9]+\\)"
		form)
	  (setq form
		(replace-match
		 (save-match-data
		   (org-table-make-reference
		    (cl-subseq fields
			       (+ (if (match-end 2) n0 0)
				  (string-to-number (match-string 1 form))
				  -1)
			       (+ (if (match-end 4) n0 0)
				  (string-to-number (match-string 3 form))))
		    keep-empty numbers lispp))
		 t t form)))
	(setq form0 form)
	;; Insert the references to fields in same row
	(while (string-match "\\$\\(\\([-+]\\)?[0-9]+\\)" form)
	  (let* ((n (+ (string-to-number (match-string 1 form))
		       (if (match-end 2) n0 0)))
		 (x (nth (1- (if (= n 0) n0 (max n 1))) fields)))
	    (setq formrpl (save-match-data
			    (org-table-make-reference
			     x keep-empty numbers lispp)))
	    (when (or (not x)
		      (save-match-data
			(string-match (regexp-quote formula) formrpl)))
	      (user-error "Invalid field specifier \"%s\""
			  (match-string 0 form))))
	  (setq form (replace-match formrpl t t form)))

	(if lispp
	    (setq ev (condition-case nil
                         ;; FIXME: Arbitrary code evaluation.
			 (eval (eval (read form)))
		       (error "#ERROR"))
		  ev (if (numberp ev) (number-to-string ev) ev)
		  ev (if duration (org-table-time-seconds-to-string
				   (string-to-number ev)
				   duration-output-format)
		       ev))

	  ;; Use <...> timestamps so that Calc can handle them.
	  (setq form
		(replace-regexp-in-string org-ts-regexp-inactive "<\\1>" form))
	  ;; Internationalize local timestamps by setting locale to
	  ;; "C".
	  (setq form
		(replace-regexp-in-string
		 org-ts-regexp
		 (lambda (ts)
		   (let ((system-time-locale "C"))
		     (format-time-string
		      (org-time-stamp-format
		       (string-match-p "[0-9]\\{1,2\\}:[0-9]\\{2\\}" ts))
		      (save-match-data (org-time-string-to-time ts)))))
		 form t t))

	  (setq ev (if (and duration (string-match "^[0-9]+:[0-9]+\\(?::[0-9]+\\)?$" form))
		       form
		     (calc-eval (cons form calc-modes)
				(when (and (not keep-empty) numbers) 'num)))
		ev (if (and duration
                            ;; When the result is an empty string,
                            ;; keep it empty.
                            ;; See https://list.orgmode.org/orgmode/CAF_DUeEFpNU5UXjE80yB1MB9xj5oVLqG=XadnkqCdzWtakWdPg@mail.gmail.com/
                            (not (string-empty-p ev)))
                       (org-table-time-seconds-to-string
			(if (string-match "^[0-9]+:[0-9]+\\(?::[0-9]+\\)?$" ev)
			    (string-to-number (org-table-time-string-to-seconds ev))
			  (string-to-number ev))
			duration-output-format)
		     ev)))

	(when org-table-formula-debug
	  (let ((wcf (current-window-configuration)))
	    (with-output-to-temp-buffer "*Substitution History*"
	      (princ (format "Substitution history of formula
Orig:   %s
$xyz->  %s
@r$c->  %s
$1->    %s\n" orig formula form0 form))
	      (if (consp ev)
		  (princ (format "        %s^\nError:  %s"
				 (make-string (car ev) ?\-) (nth 1 ev)))
		(princ (format "Result: %s\nFormat: %s\nFinal:  %s"
			       ev (or fmt "NONE")
			       (if fmt (format fmt (string-to-number ev)) ev)))))
	    (setq bw (get-buffer-window "*Substitution History*"))
	    (org-fit-window-to-buffer bw)
	    (unless (and (called-interactively-p 'any) (not ndown))
	      (unless (let (inhibit-redisplay)
			(y-or-n-p "Debugging Formula.  Continue to next? "))
		(org-table-align)
		(user-error "Abort"))
	      (delete-window bw)
	      (message "")
	      (set-window-configuration wcf))))
	(when (consp ev) (setq fmt nil ev "#ERROR"))
	(org-table-justify-field-maybe
	 (format org-table-formula-field-format
		 (cond
		  ((not (stringp ev)) ev)
		  (fmt (format fmt (string-to-number ev)))
		  ;; Replace any active time stamp in the result with
		  ;; an inactive one.  Dates in tables are likely
		  ;; piece of regular data, not meant to appear in the
		  ;; agenda.
		  (t (replace-regexp-in-string org-ts-regexp "[\\1]" ev)))))
	(if (and down (> ndown 0) (looking-at ".*\n[ \t]*|[^-]"))
	    (call-interactively 'org-return)
	  (setq ndown 0)))
      (and down (org-table-maybe-recalculate-line))
      (or suppress-align (and org-table-may-need-update
			      (org-table-align))))))

(defun org-table-put-field-property (prop value)
  (save-excursion
    (put-text-property (progn (skip-chars-backward "^|") (point))
		       (progn (skip-chars-forward "^|") (point))
		       prop value)))

(declare-function org-table-highlight-rectangle "org-table-formula-edit"
                  (&optional beg end face))
(defun org-table-get-range (desc &optional tbeg col highlight corners-only)
  "Get a calc vector from a column, according to descriptor DESC.

Optional arguments TBEG and COL can give the beginning of the table and
the current column, to avoid unnecessary parsing.

HIGHLIGHT means just highlight the range.

When CORNERS-ONLY is set, only return the corners of the range as
a list (line1 column1 line2 column2) where line1 and line2 are
line numbers relative to beginning of table, or TBEG, and column1
and column2 are table column numbers."
  (let* ((desc (if (string-match-p "\\`\\$[0-9]+\\.\\.\\$[0-9]+\\'" desc)
		   (replace-regexp-in-string "\\$" "@0$" desc)
		 desc))
	 (col (or col (org-table-current-column)))
	 (tbeg (or tbeg (org-table-begin)))
	 (thisline (count-lines tbeg (line-beginning-position))))
    (unless (string-match org-table-range-regexp desc)
      (user-error "Invalid table range specifier `%s'" desc))
    (let ((rangep (match-end 3))
	  (r1 (let ((r (and (match-end 1) (match-string 1 desc))))
		(or (save-match-data
		      (and (org-string-nw-p r)
			   (org-table--descriptor-line r thisline)))
		    thisline)))
	  (r2 (let ((r (and (match-end 4) (match-string 4 desc))))
		(or (save-match-data
		      (and (org-string-nw-p r)
			   (org-table--descriptor-line r thisline)))
		    thisline)))
	  (c1 (let ((c (and (match-end 2) (substring (match-string 2 desc) 1))))
		(if (or (not c) (= (string-to-number c) 0)) col
		  (+ (string-to-number c)
		     (if (memq (string-to-char c) '(?- ?+)) col 0)))))
	  (c2 (let ((c (and (match-end 5) (substring (match-string 5 desc) 1))))
		(if (or (not c) (= (string-to-number c) 0)) col
		  (+ (string-to-number c)
		     (if (memq (string-to-char c) '(?- ?+)) col 0))))))
      (save-excursion
	(if (and (not corners-only)
		 (or (not rangep) (and (= r1 r2) (= c1 c2))))
	    ;; Just one field.
	    (progn
	      (forward-line (- r1 thisline))
	      (while (not (looking-at org-table-dataline-regexp))
		(forward-line))
	      (prog1 (org-trim (org-table-get-field c1))
		(when highlight (org-table-highlight-rectangle))))
	  ;; A range, return a vector.  First sort the numbers to get
	  ;; a regular rectangle.
	  (let ((first-row (min r1 r2))
		(last-row (max r1 r2))
		(first-column (min c1 c2))
		(last-column (max c1 c2)))
	    (if corners-only (list first-row first-column last-row last-column)
	      ;; Copy the range values into a list.
	      (forward-line (- first-row thisline))
	      (while (not (looking-at org-table-dataline-regexp))
		(forward-line)
		(cl-incf first-row))
	      (org-table-goto-column first-column)
	      (let ((beg (point)))
		(forward-line (- last-row first-row))
		(while (not (looking-at org-table-dataline-regexp))
		  (forward-line -1))
		(org-table-goto-column last-column)
		(let ((end (point)))
		  (when highlight
		    (org-table-highlight-rectangle
		     beg (progn (skip-chars-forward "^|\n") (point))))
		  ;; Return string representation of calc vector.
		  (mapcar #'org-trim
			  (apply #'append
				 (org-table-copy-region beg end))))))))))))

(defun org-table-convert-refs-to-an (s)
  "Convert spreadsheet references from to @7$28 to AB7.
Works for single references, but also for entire formulas and even the
full TBLFM line.

Leave the relative references unchanged."
  (while (string-match "@\\([0-9]+\\)\\$\\([0-9]+\\)" s)
    (setq s (replace-match
	     (format "%s%d"
		     (org-number-to-letters
		      (string-to-number (match-string 2 s)))
		     (string-to-number (match-string 1 s)))
	     t t s)))
  (while (string-match "\\(^\\|[^0-9a-zA-Z]\\)\\$\\([1-9][0-9]*\\)" s)
    (setq s (replace-match (concat "\\1"
				   (org-number-to-letters
				    (string-to-number (match-string 2 s))) "&")
			   t nil s)))
  s)

(defun org-table-convert-refs-to-rc (s)
  "Convert spreadsheet references from A7 to @7$28.
Works for single references, but also for entire formulas and even the
full TBLFM line."
  (let ((start 0))
    (while (string-match "\\<\\([a-zA-Z]+\\)\\([0-9]+\\>\\|&\\)\\|\\(;[^\r\n:]+\\|\\<remote([^,)]*)\\)" s start)
      (cond
       ((match-end 3)
	;; format match, just advance
	(setq start (match-end 0)))
       ((and (> (match-beginning 0) 0)
	     (equal ?. (aref s (max (1- (match-beginning 0)) 0)))
	     (not (equal ?. (aref s (max (- (match-beginning 0) 2) 0)))))
	;; 3.e5 or something like this.
	(setq start (match-end 0)))
       ((or (> (- (match-end 1) (match-beginning 1)) 2)
	    ;; (member (match-string 1 s)
	    ;;	    '("arctan" "exp" "expm" "lnp" "log" "stir"))
	    )
	;; function name, just advance
	(setq start (match-end 0)))
       (t
	(setq start (match-beginning 0)
	      s (replace-match
		 (if (equal (match-string 2 s) "&")
		     (format "$%d" (org-letters-to-number (match-string 1 s)))
		   (format "@%d$%d"
			   (string-to-number (match-string 2 s))
			   (org-letters-to-number (match-string 1 s))))
		 t t s)))))
    s))

(declare-function org-id-find "org-id" (id &optional markerp))
(defun org-table-get-remote-range (name-or-id form)
  "Get a field value or a list of values in a range from table at ID.

NAME-OR-ID may be the name of a table in the current file as set
by a \"#+NAME:\" directive.  The first table following this line
will then be used.  Alternatively, it may be an ID referring to
any entry, also in a different file.  In this case, the first
table in that entry will be referenced.
FORM is a field or range descriptor like \"@2$3\" or \"B3\" or
\"@I$2..@II$2\".  All the references must be absolute, not relative.

The return value is either a single string for a single field, or a
list of the fields in the rectangle."
  (save-match-data
    (let ((case-fold-search t) (id-loc nil)
	  ;; Protect a bunch of variables from being overwritten by
	  ;; the context of the remote table.
	  org-table-column-names org-table-column-name-regexp
	  org-table-local-parameters org-table-named-field-locations
	  org-table-current-line-types
	  org-table-current-begin-pos org-table-dlines
	  org-table-current-ncol
	  org-table-hlines
	  org-table-last-column-widths
	  org-table-last-alignment
	  buffer loc)
      (setq form (org-table-convert-refs-to-rc form))
      (org-with-wide-buffer
       (goto-char (point-min))
       (if (re-search-forward
	    (concat "^[ \t]*#\\+\\(tbl\\)?name:[ \t]*"
		    (regexp-quote name-or-id) "[ \t]*$")
	    nil t)
	   (setq buffer (current-buffer) loc (match-beginning 0))
         (require 'org-id)
	 (setq id-loc (org-id-find name-or-id 'marker))
	 (unless (and id-loc (markerp id-loc))
	   (user-error "Can't find remote table \"%s\"" name-or-id))
	 (setq buffer (marker-buffer id-loc)
	       loc (marker-position id-loc))
	 (move-marker id-loc nil))
       (with-current-buffer buffer
	 (org-with-wide-buffer
	  (goto-char loc)
	  (forward-char 1)
	  (unless (and (re-search-forward "^\\(\\*+ \\)\\|^[ \t]*|" nil t)
		       (not (match-beginning 1)))
	    (user-error "Cannot find a table at NAME or ID %s" name-or-id))
	  (org-table-analyze)
	  (setq form (org-table-formula-substitute-names
		      (org-table-formula-handle-first/last-rc form)))
	  (if (and (string-match org-table-range-regexp form)
		   (> (length (match-string 0 form)) 1))
	      (org-table-get-range
	       (match-string 0 form) org-table-current-begin-pos 1)
	    form)))))))

(defun org-table-remote-reference-indirection (form)
  "Return formula with table remote references substituted by indirection.
For example \"remote($1, @>$2)\" => \"remote(year_2013, @>$1)\".
This indirection works only with the format @ROW$COLUMN.  The
format \"B3\" is not supported because it can not be
distinguished from a plain table name or ID."
  (let ((regexp
	 ;; Same as in `org-table-eval-formula'.
	 (concat "\\<remote([ \t]*\\("
		 ;; Allow "$1", "@<", "$-1", "@<<$1" etc.
		 "[@$][^ \t,]+"
		 "\\)[ \t]*,[ \t]*\\([^\n)]+\\))")))
    (replace-regexp-in-string
     regexp
     (lambda (m)
       (save-match-data
	 (let ((eq (org-table-formula-handle-first/last-rc (match-string 1 m))))
	   (org-table-get-range
	    (if (string-match-p "\\`\\$[0-9]+\\'" eq)
		(concat "@0" eq)
	      eq)))))
     form t t 1)))

(defun org-table--descriptor-line (desc cline)
  "Return relative line number corresponding to descriptor DESC.
The cursor is currently in relative line number CLINE."
  (if (string-match "\\`[0-9]+\\'" desc)
      (aref org-table-dlines (string-to-number desc))
    (when (or (not (string-match
		  "^\\(\\([-+]\\)?\\(I+\\)\\)?\\(\\([-+]\\)?\\([0-9]+\\)\\)?"
		  ;;  1  2          3           4  5          6
		  desc))
	      (and (not (match-end 3)) (not (match-end 6)))
	      (and (match-end 3) (match-end 6) (not (match-end 5))))
      (user-error "Invalid row descriptor `%s'" desc))
    (let* ((hn (and (match-end 3) (- (match-end 3) (match-beginning 3))))
	   (hdir (match-string 2 desc))
	   (odir (match-string 5 desc))
	   (on (and (match-end 6) (string-to-number (match-string 6 desc))))
	   (rel (and (match-end 6)
		     (or (and (match-end 1) (not (match-end 3)))
			 (match-end 5)))))
      (when (and hn (not hdir))
	(setq cline 0)
	(setq hdir "+")
	(when (eq (aref org-table-current-line-types 0) 'hline) (cl-decf hn)))
      (when (and (not hn) on (not odir)) (user-error "Should never happen"))
      (when hn
	(setq cline
	      (org-table--row-type 'hline hn cline (equal hdir "-") nil desc)))
      (when on
	(setq cline
	      (org-table--row-type 'dline on cline (equal odir "-") rel desc)))
      cline)))

(defun org-table--row-type (type n i backwards relative desc)
  "Return relative line of Nth row with type TYPE.
Search starts from relative line I.  When BACKWARDS in non-nil,
look before I.  When RELATIVE is non-nil, the reference is
relative.  DESC is the original descriptor that started the
search, as a string."
  (let ((l (length org-table-current-line-types)))
    (catch :exit
      (dotimes (_ n)
	(while (and (cl-incf i (if backwards -1 1))
		    (>= i 0)
		    (< i l)
		    (not (eq (aref org-table-current-line-types i) type))
		    ;; We are going to cross a hline.  Check if this is
		    ;; an authorized move.
		    (cond
		     ((not relative))
		     ((not (eq (aref org-table-current-line-types i) 'hline)))
		     ((eq org-table-relative-ref-may-cross-hline t))
		     ((eq org-table-relative-ref-may-cross-hline 'error)
		      (user-error "Row descriptor %s crosses hline" desc))
		     (t (cl-decf i (if backwards -1 1)) ; Step back.
			(throw :exit nil)))))))
    (cond ((or (< i 0) (>= i l))
	   (user-error "Row descriptor %s leads outside table" desc))
	  ;; The last hline doesn't exist.  Instead, point to last row
	  ;; in table.
	  ((= i (1- l)) (1- i))
	  (t i))))

(defun org-table--error-on-old-row-references (s)
  (when (string-match "&[-+0-9I]" s)
    (user-error "Formula contains old &row reference, please rewrite using @-syntax")))

(defun org-table-make-reference (elements keep-empty numbers lispp)
  "Convert list ELEMENTS to something appropriate to insert into formula.
KEEP-EMPTY indicated to keep empty fields, default is to skip them.
NUMBERS indicates that everything should be converted to numbers.
LISPP non-nil means to return something appropriate for a Lisp
list, `literal' is for the format specifier L."
  ;; Calc nan (not a number) is used for the conversion of the empty
  ;; field to a reference for several reasons: (i) It is accepted in a
  ;; Calc formula (e. g. "" or "()" would result in a Calc error).
  ;; (ii) In a single field (not in range) it can be distinguished
  ;; from "(nan)" which is the reference made from a single field
  ;; containing "nan".
  (if (stringp elements)
      ;; field reference
      (if lispp
	  (if (eq lispp 'literal)
	      elements
            ;; Ignore KEEP-EMPTY here.
            ;; When ELEMENTS="" and NUMBERS=t, (string-to-number "")
            ;; returns 0 - consistent with (0) for Calc branch.
            ;; When ELEMENTS="" and NUMBERS=nil, `prin1-to-string' will
            ;; return "\"\"" - historical behavior that also does not
            ;; leave missing arguments in formulas like (string< $1 $2)
            ;; when $2 cell is empty.
            (prin1-to-string
	     (if numbers (string-to-number elements) elements)))
	(if (string-match "\\S-" elements)
	    (progn
	      (when numbers (setq elements (number-to-string
					    (string-to-number elements))))
	      (concat "(" elements ")"))
	  (if (or (not keep-empty) numbers) "(0)" "nan")))
    ;; range reference
    (unless keep-empty
      (setq elements
	    (delq nil
		  (mapcar (lambda (x) (if (string-match "\\S-" x) x nil))
			  elements))))
    (setq elements (or elements '()))  ; if delq returns nil then we need '()
    (if lispp
	(mapconcat
	 (lambda (x)
	   (if (eq lispp 'literal)
	       x
	     (prin1-to-string (if numbers (string-to-number x) x))))
	 elements " ")
      (concat "[" (mapconcat
		   (lambda (x)
		     (if (string-match "\\S-" x)
			 (if numbers
			     (number-to-string (string-to-number x))
			   x)
		       (if (or (not keep-empty) numbers) "0" "nan")))
		   elements
		   ",") "]"))))

(defun org-table-message-once-per-second (t1 &rest args)
  "If there has been more than one second since T1, display message.
ARGS are passed as arguments to the `message' function.  Returns
current time if a message is printed, otherwise returns T1.  If
T1 is nil, always messages."
  (let ((curtime (current-time)))
    (if (or (not t1) (time-less-p 1 (time-subtract curtime t1)))
	(progn (apply 'message args)
	       curtime)
      t1)))

;;;###autoload
(defun org-table-recalculate (&optional all noalign)
  "Recalculate the current table line by applying all stored formulas.

With prefix arg ALL, do this for all lines in the table.

When called with a `\\[universal-argument] \\[universal-argument]' prefix, or \
if ALL is the symbol `iterate',
recompute the table until it no longer changes.

If NOALIGN is not nil, do not re-align the table after the computations
are done.  This is typically used internally to save time, if it is
known that the table will be realigned a little later anyway."
  (interactive "P")
  (unless (memq this-command org-recalc-commands)
    (push this-command org-recalc-commands))
  (unless (org-at-table-p) (user-error "Not at a table"))
  (if (or (eq all 'iterate) (equal all '(16)))
      (org-table-iterate)
    (org-table-analyze)
    (let* ((eqlist (sort (org-table-get-stored-formulas)
			 (lambda (a b) (string< (car a) (car b)))))
	   (inhibit-redisplay (not debug-on-error))
	   (line-re org-table-dataline-regexp)
	   (log-first-time (current-time))
	   (log-last-time log-first-time)
	   (cnt 0)
	   beg end eqlcol eqlfield)
      ;; Insert constants in all formulas.
      (when eqlist
	(org-table-with-shrunk-columns
	 (org-table-save-field
	  ;; Expand equations, then split the equation list between
	  ;; column formulas and field formulas.
	  (dolist (eq eqlist)
	    (let* ((rhs (org-table-formula-substitute-names
			 (org-table-formula-handle-first/last-rc (cdr eq))))
		   (old-lhs (car eq))
		   (lhs
		    (org-table-formula-handle-first/last-rc
		     (cond
		      ((string-match "\\`@-?I+" old-lhs)
		       (user-error "Can't assign to hline relative reference"))
		      ((string-match "\\`\\$[<>]" old-lhs)
		       (let ((new (org-table-formula-handle-first/last-rc
				   old-lhs)))
			 (when (assoc new eqlist)
			   (user-error "\"%s=\" formula tries to overwrite \
existing formula for column %s"
				       old-lhs
				       new))
			 new))
		      (t old-lhs)))))
	      (if (string-match-p "\\`\\$[0-9]+\\'" lhs)
		  (push (cons lhs rhs) eqlcol)
		(push (cons lhs rhs) eqlfield))))
	  (setq eqlcol (nreverse eqlcol))
	  ;; Expand ranges in lhs of formulas
	  (setq eqlfield (org-table-expand-lhs-ranges (nreverse eqlfield)))
	  ;; Get the correct line range to process.
	  (if all
	      (progn
		(setq end (copy-marker (org-table-end)))
		(goto-char (setq beg org-table-current-begin-pos))
		(cond
		 ((re-search-forward org-table-calculate-mark-regexp end t)
		  ;; This is a table with marked lines, compute selected
		  ;; lines.
		  (setq line-re org-table-recalculate-regexp))
		 ;; Move forward to the first non-header line.
		 ((and (re-search-forward org-table-dataline-regexp end t)
		       (re-search-forward org-table-hline-regexp end t)
		       (re-search-forward org-table-dataline-regexp end t))
		  (setq beg (match-beginning 0)))
		 ;; Just leave BEG at the start of the table.
		 (t nil)))
	    (setq beg (line-beginning-position)
		  end (copy-marker (line-beginning-position 2))))
	  (goto-char beg)
	  ;; Mark named fields untouchable.  Also check if several
	  ;; field/range formulas try to set the same field.
	  (remove-text-properties beg end '(:org-untouchable t))
	  (let ((current-line (count-lines org-table-current-begin-pos
					   (line-beginning-position)))
		seen-fields)
	    (dolist (eq eqlfield)
	      (let* ((name (car eq))
		     (location (assoc name org-table-named-field-locations))
		     (eq-line (or (nth 1 location)
				  (and (string-match "\\`@\\([0-9]+\\)" name)
				       (aref org-table-dlines
					     (string-to-number
					      (match-string 1 name))))))
		     (reference
		      (if location
			  ;; Turn field coordinates associated to NAME
			  ;; into an absolute reference.
			  (format "@%d$%d"
				  (org-table-line-to-dline eq-line)
				  (nth 2 location))
			name)))
		(when (member reference seen-fields)
		  (user-error "Several field/range formulas try to set %s"
			      reference))
		(push reference seen-fields)
		(when (or all (eq eq-line current-line))
		  (org-table-goto-field name)
		  (org-table-put-field-property :org-untouchable t)))))
	  ;; Evaluate the column formulas, but skip fields covered by
	  ;; field formulas.
	  (goto-char beg)
	  (while (re-search-forward line-re end t)
	    (unless (string-match "\\` *[_^!$/] *\\'" (org-table-get-field 1))
	      ;; Unprotected line, recalculate.
	      (cl-incf cnt)
	      (when all
		(setq log-last-time
		      (org-table-message-once-per-second
		       log-last-time
		       "Re-applying formulas to full table...(line %d)" cnt)))
	      (if (markerp org-last-recalc-line)
		  (move-marker org-last-recalc-line (line-beginning-position))
		(setq org-last-recalc-line
		      (copy-marker (line-beginning-position))))
	      (dolist (entry eqlcol)
		(goto-char org-last-recalc-line)
		(org-table-goto-column
		 (string-to-number (substring (car entry) 1)) nil 'force)
		(unless (get-text-property (point) :org-untouchable)
		  (org-table-eval-formula
		   nil (cdr entry) 'noalign 'nocst 'nostore 'noanalysis)))))
	  ;; Evaluate the field formulas.
	  (dolist (eq eqlfield)
	    (let ((reference (car eq))
		  (formula (cdr eq)))
	      (setq log-last-time
		    (org-table-message-once-per-second
		     (and all log-last-time)
		     "Re-applying formula to field: %s" (car eq)))
	      (org-table-goto-field
	       reference
	       ;; Possibly create a new column, as long as
	       ;; `org-table-formula-create-columns' allows it.
	       (let ((column-count (progn (end-of-line)
					  (1- (org-table-current-column)))))
		 (lambda (column)
		   (when (> column 1000)
		     (user-error "Formula column target too large"))
		   (and (> column column-count)
			(or (eq org-table-formula-create-columns t)
			    (and (eq org-table-formula-create-columns 'warn)
				 (progn
				   (org-display-warning
				    "Out-of-bounds formula added columns")
				   t))
			    (and (eq org-table-formula-create-columns 'prompt)
				 (yes-or-no-p
				  "Out-of-bounds formula.  Add columns? "))
			    (user-error
			     "Missing columns in the table.  Aborting"))))))
	      (org-table-eval-formula nil formula t t t t)))
	  ;; Clean up marker.
	  (set-marker end nil)))
	(unless noalign
	  (when org-table-may-need-update (org-table-align))
	  (when all
	    (org-table-message-once-per-second
	     log-first-time "Re-applying formulas to %d lines... done" cnt)))
	(org-table-message-once-per-second
	 (and all log-first-time) "Re-applying formulas... done")))))

;;;###autoload
(defun org-table-iterate (&optional arg)
  "Recalculate the table until it does not change anymore.
The maximum number of iterations is 10, but you can choose a different value
with the prefix ARG."
  (interactive "P")
  (let ((imax (if arg (prefix-numeric-value arg) 10))
	(i 0)
	(lasttbl (buffer-substring (org-table-begin) (org-table-end)))
	thistbl)
    (catch 'exit
      (while (< i imax)
	(setq i (1+ i))
	(org-table-recalculate 'all)
	(setq thistbl (buffer-substring (org-table-begin) (org-table-end)))
	(if (not (string= lasttbl thistbl))
	    (setq lasttbl thistbl)
	  (if (> i 1)
	      (message "Convergence after %d iterations" i)
	    (message "Table was already stable"))
	  (throw 'exit t)))
      (user-error "No convergence after %d iterations" i))))

;;;###autoload
(defun org-table-recalculate-buffer-tables ()
  "Recalculate all tables in the current buffer."
  (interactive)
  (org-with-wide-buffer
   (org-table-map-tables
    (lambda ()
      ;; Reason for separate `org-table-align': When repeating
      ;; (org-table-recalculate t) `org-table-may-need-update' gets in
      ;; the way.
      (org-table-recalculate t t)
      (org-table-align))
    t)))

;;;###autoload
(defun org-table-iterate-buffer-tables ()
  "Iterate all tables in the buffer, to converge inter-table dependencies."
  (interactive)
  (let* ((imax 10)
	 (i imax)
	 (checksum (md5 (buffer-string)))
	 c1)
    (org-with-wide-buffer
     (catch 'exit
       (while (> i 0)
	 (setq i (1- i))
	 (org-table-map-tables (lambda () (org-table-recalculate t t)) t)
	 (if (equal checksum (setq c1 (md5 (buffer-string))))
	     (progn
	       (org-table-map-tables #'org-table-align t)
	       (message "Convergence after %d iterations" (- imax i))
	       (throw 'exit t))
	   (setq checksum c1)))
       (org-table-map-tables #'org-table-align t)
       (user-error "No convergence after %d iterations" imax)))))

(defun org-table-calc-current-TBLFM (&optional arg)
  "Apply the #+TBLFM in the line at point to the table."
  (interactive "P")
  (unless (org-at-TBLFM-p) (user-error "Not at a #+TBLFM line"))
  (let ((formula (buffer-substring
		  (line-beginning-position)
		  (line-end-position))))
    (save-excursion
      ;; Insert a temporary formula at right after the table
      (goto-char (org-table-TBLFM-begin))
      (let ((s (point-marker)))
	(insert formula "\n")
	(let ((e (point-marker)))
	  ;; Recalculate the table.
	  (forward-line -1)		; move to the inserted line
	  (skip-chars-backward " \r\n\t")
	  (unwind-protect
	      (org-call-with-arg #'org-table-recalculate (or arg t))
	    ;; Delete the formula inserted temporarily.
	    (delete-region s e)
	    (set-marker s nil)
	    (set-marker e nil)))))))

(defun org-table-expand-lhs-ranges (equations)
  "Expand list of formulas.
If some of the RHS in the formulas are ranges or a row reference,
expand them to individual field equations for each field.  This
function assumes the table is already analyzed (i.e., using
`org-table-analyze')."
  (let (res)
    (dolist (e equations (nreverse res))
      (let ((lhs (car e))
	    (rhs (cdr e)))
	(cond
	 ((string-match-p "\\`@[-+0-9]+\\$-?[0-9]+\\'" lhs)
	  ;; This just refers to one fixed field.
	  (push e res))
	 ((string-match-p "\\`[a-zA-Z][_a-zA-Z0-9]*\\'" lhs)
	  ;; This just refers to one fixed named field.
	  (push e res))
	 ((string-match-p "\\`\\$[0-9]+\\'" lhs)
	  ;; Column formulas are treated specially and are not
	  ;; expanded.
	  (push e res))
	 ((string-match "\\`@[0-9]+\\'" lhs)
	  (dotimes (ic org-table-current-ncol)
	    (push (cons (propertize (format "%s$%d" lhs (1+ ic)) :orig-eqn e)
			rhs)
		  res)))
	 (t
	  (let* ((range (org-table-get-range
			 lhs org-table-current-begin-pos 1 nil 'corners))
		 (r1 (org-table-line-to-dline (nth 0 range)))
		 (c1 (nth 1 range))
		 (r2 (org-table-line-to-dline (nth 2 range) 'above))
		 (c2 (nth 3 range)))
	    (cl-loop for ir from r1 to r2 do
		     (cl-loop for ic from c1 to c2 do
			      (push (cons (propertize
					   (format "@%d$%d" ir ic) :orig-eqn e)
					  rhs)
				    res))))))))))

(defun org-table-formula-handle-first/last-rc (s)
  "Replace @<, @>, $<, $> with first/last row/column of the table.
So @< and $< will always be replaced with @1 and $1, respectively.
The advantage of these special markers are that structure editing of
the table will not change them, while @1 and $1 will be modified
when a line/row is swapped out of that privileged position.  So for
formulas that use a range of rows or columns, it may often be better
to anchor the formula with \"I\" row markers, or to offset from the
borders of the table using the @< @> $< $> makers."
  (let (n nmax len char (start 0))
    (while (string-match "\\([@$]\\)\\(<+\\|>+\\)\\|\\(remote([^)]+)\\)"
			 s start)
      (if (match-end 3)
	  (setq start (match-end 3))
	(setq nmax (if (equal (match-string 1 s) "@")
		       (1- (length org-table-dlines))
		     org-table-current-ncol)
	      len (- (match-end 2) (match-beginning 2))
	      char (string-to-char (match-string 2 s))
	      n (if (= char ?<)
		    len
		  (- nmax len -1)))
	(if (or (< n 1) (> n nmax))
	    (user-error "Reference \"%s\" in expression \"%s\" points outside table"
			(match-string 0 s) s))
	(setq start (match-beginning 0))
	(setq s (replace-match (format "%s%d" (match-string 1 s) n) t t s)))))
  s)

(defun org-table-formula-substitute-names (f)
  "Replace $const with values in string F."
  (let ((start 0)
	(pp (/= (string-to-char f) ?'))
	(duration (string-match-p ";.*[Tt].*\\'" f))
	(new (replace-regexp-in-string	; Check for column names.
	      org-table-column-name-regexp
	      (lambda (m)
		(concat "$" (cdr (assoc (match-string 1 m)
					org-table-column-names))))
	      f t t)))
    ;; Parameters and constants.
    (while (setq start
		 (string-match
		  "\\$\\([a-zA-Z][_a-zA-Z0-9]*\\)\\|\\(\\<remote([^)]*)\\)"
		  new start))
      (if (match-end 2) (setq start (match-end 2))
	(cl-incf start)
	;; When a duration is expected, convert value on the fly.
	(let ((value
	       (save-match-data
		 (let ((v (org-table-get-constant (match-string 1 new))))
		   (if (and (org-string-nw-p v) duration)
		       (org-table-time-string-to-seconds v)
		     v)))))
	  (when value
	    (setq new (replace-match
		       (concat (and pp "(") value (and pp ")")) t t new))))))
    (if org-table-formula-debug (propertize new :orig-formula f) new)))

(declare-function org-entry-get "org-property" (epom property &optional inherit literal-nil))
(defun org-table-get-constant (const)
  "Find the value for a parameter or constant in a formula.
Parameters get priority."
  (or (cdr (assoc const org-table-local-parameters))
      (cdr (assoc const org-table-formula-constants-local))
      (cdr (assoc const org-table-formula-constants))
      (and (fboundp 'constants-get) (constants-get const))
      (and (string= (substring const 0 (min 5 (length const))) "PROP_")
           (require 'org-property)
	   (org-entry-get nil (substring const 5) 'inherit))
      "#UNDEFINED_NAME"))

(defun org-table--force-dataline ()
  "Move point to the closest data line in a table.
Raise an error if the table contains no data line.  Preserve
column when moving point."
  (unless (org-match-line org-table-dataline-regexp)
    (let* ((re org-table-dataline-regexp)
	   (column (current-column))
	   (p1 (save-excursion (re-search-forward re (org-table-end) t)))
	   (p2 (save-excursion (re-search-backward re (org-table-begin) t))))
      (cond ((and p1 p2)
	     (goto-char (if (< (abs (- p1 (point))) (abs (- p2 (point))))
			    p1
			  p2)))
	    ((or p1 p2) (goto-char (or p1 p2)))
	    (t (user-error "No table data line around here")))
      (org-move-to-column column))))


;;; Lookup functions

(defmacro org-define-lookup-function (mode)
  (let ((mode-str (symbol-name mode))
	(first-p (eq mode 'first))
	(all-p (eq mode 'all)))
    (let ((plural-str (if all-p "s" "")))
      `(defun ,(intern (format "org-lookup-%s" mode-str)) (val s-list r-list &optional predicate)
	 ,(format "Find %s occurrence%s of VAL in S-LIST; return corresponding element%s of R-LIST.
If R-LIST is nil, return matching element%s of S-LIST.
If PREDICATE is not nil, use it instead of `equal' to match VAL.
Matching is done by (PREDICATE VAL S), where S is an element of S-LIST.
This function is generated by a call to the macro `org-define-lookup-function'."
		  mode-str plural-str plural-str plural-str)
	 (let ,(let ((lvars '((p (or predicate 'equal))
			      (sl s-list)
			      (rl (or r-list s-list))
			      (ret nil))))
		 (if first-p (cons '(match-p nil) lvars) lvars))
	   (while ,(if first-p '(and (not match-p) sl) 'sl)
	     (when (funcall p val (car sl))
	       ,(when first-p '(setq match-p t))
	       (let ((rval (car rl)))
		 (setq ret ,(if all-p '(append ret (list rval)) 'rval))))
	     (setq sl (cdr sl) rl (cdr rl)))
	   ret)))))

(org-define-lookup-function first)
(org-define-lookup-function last)
(org-define-lookup-function all)

(provide 'org-table-formula)

;;; org-table-formula.el ends here
