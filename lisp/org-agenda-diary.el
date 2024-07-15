;;; org-agenda-diary.el --- Agenda diary integration         -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

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

;;; Commentary:
;;
;; This library implements diary support for Org agenda.

;;; Code:

(require 'org-macs)
(require 'diary-lib)
(require 'org-agenda-line-format)
(require 'org-agenda-files)
(require 'org-indent-static)
(require 'org-fold)
(require 'org-edit-structure-common)
(require 'org-time)

(defvar org-disable-agenda-to-diary nil)          ;Dynamically-scoped param.
(defvar org-diary-last-run-time nil)

(defcustom org-agenda-diary-file 'diary-file
  "File to which to add new entries with the `i' key in agenda and calendar.
When this is the symbol `diary-file', the functionality in the Emacs
calendar will be used to add entries to the `diary-file'.  But when this
points to a file, `org-agenda-diary-entry' will be used instead."
  :group 'org-agenda
  :type '(choice
	  (const :tag "The standard Emacs diary file" diary-file)
	  (file :tag "Special Org file diary entries")))


(defcustom org-agenda-insert-diary-strategy 'date-tree
  "Where in `org-agenda-diary-file' should new entries be added?
Valid values:

date-tree         in the date tree, as first child of the date
date-tree-last    in the date tree, as last child of the date
top-level         as top-level entries at the end of the file."
  :group 'org-agenda
  :type '(choice
	  (const :tag "first in a date tree" date-tree)
	  (const :tag "last in a date tree" date-tree-last)
	  (const :tag "as top level at end of file" top-level)))

(defcustom org-agenda-insert-diary-extract-time nil
  "Non-nil means extract any time specification from the diary entry."
  :group 'org-agenda
  :version "24.1"
  :type 'boolean)

;;; Querying diary data

(defun org-get-entries-from-diary (date)
  "Get the (Emacs Calendar) diary entries for DATE."
  (let* ((diary-fancy-buffer "*temporary-fancy-diary-buffer*")
	 (diary-display-function #'diary-fancy-display)
	 (pop-up-frames nil)
	 (diary-list-entries-hook
	  (cons 'org-diary-default-entry diary-list-entries-hook))
	 (diary-file-name-prefix nil) ; turn this feature off
	 (diary-modify-entry-list-string-function
	  #'org-modify-diary-entry-string)
	 (diary-time-regexp (concat "^" diary-time-regexp))
	 entries
	 (org-disable-agenda-to-diary t))
    (save-excursion
      (save-window-excursion
        (diary-list-entries date 1)))
    (if (not (get-buffer diary-fancy-buffer))
	(setq entries nil)
      (with-current-buffer diary-fancy-buffer
	(setq buffer-read-only nil)
	(if (zerop (buffer-size))
	    ;; No entries
	    (setq entries nil)
	  ;; Omit the date and other unnecessary stuff
	  (org-agenda-cleanup-fancy-diary)
	  ;; Add prefix to each line and extend the text properties
	  (if (zerop (buffer-size))
	      (setq entries nil)
	    (setq entries (buffer-substring (point-min) (- (point-max) 1)))
	    (setq entries
		  (with-temp-buffer
		    (insert entries) (goto-char (point-min))
		    (while (re-search-forward "\n[ \t]+\\(.+\\)$" nil t)
		      (unless (save-match-data (string-match diary-time-regexp (match-string 1)))
			(replace-match (concat "; " (match-string 1)))))
		    (buffer-string)))))
	(set-buffer-modified-p nil)
	(kill-buffer diary-fancy-buffer)))
    (when entries
      (setq entries (org-split-string entries "\n"))
      (setq entries
	    (mapcar
	     (lambda (x)
	       (setq x (org-agenda-format-line x :category "Diary" :dotime 'auto))
	       ;; Extend the text properties to the beginning of the line
	       (org-add-props x (text-properties-at (1- (length x)) x)
		 'type "diary"
                 'date date
                 'face 'org-agenda-diary))
	     entries)))))

(defvar org-agenda-cleanup-fancy-diary-hook nil
  "Hook run when the fancy diary buffer is cleaned up.")

(defun org-agenda-cleanup-fancy-diary ()
  "Remove unwanted stuff in buffer created by `diary-fancy-display'.
This gets rid of the date, the underline under the date, and the
dummy entry installed by Org mode to ensure non-empty diary for
each date.  It also removes lines that contain only whitespace."
  (goto-char (point-min))
  (if (looking-at ".*?:[ \t]*")
      (progn
	(replace-match "")
	(re-search-forward "\n=+$" nil t)
	(replace-match "")
	(while (re-search-backward "^ +\n?" nil t) (replace-match "")))
    (re-search-forward "\n=+$" nil t)
    (delete-region (point-min) (min (point-max) (1+ (match-end 0)))))
  (goto-char (point-min))
  (while (re-search-forward "^ +\n" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (when (re-search-forward "^Org mode dummy\n?" nil t)
    (replace-match ""))
  (run-hooks 'org-agenda-cleanup-fancy-diary-hook))

(defun org-modify-diary-entry-string (string)
  "Add text properties to string, allowing Org to act on it."
  (org-add-props string nil
    'mouse-face 'highlight
    'help-echo (if buffer-file-name
		   (format "mouse-2 or RET jump to diary file %s"
			   (abbreviate-file-name buffer-file-name))
		 "")
    'org-agenda-diary-link t
    'org-marker (org-agenda-new-marker (line-beginning-position))))

(defvar original-date) ; dynamically scoped, calendar.el does scope this
(defun org-diary-default-entry ()
  "Add a dummy entry to the diary.
Needed to avoid empty dates which mess up holiday display."
  ;; Catch the error if dealing with the new add-to-diary-alist
  (when org-disable-agenda-to-diary
    (diary-add-to-list original-date "Org mode dummy" "")))


(defvar org-agenda-entry-types) ; defined in org-agenda-search.el
(declare-function org-agenda-get-day-entries "org-agenda-search" (file date &rest args))
(declare-function org-set-sorting-strategy "org-agenda-sort" (key))
(declare-function org-agenda-finalize-entries "org-agenda-buffer-format" (list &optional type))
;;;###autoload
(defun org-diary (&rest args)
  "Return diary information from org files.
This function can be used in a \"sexp\" diary entry in the Emacs calendar.
It accesses org files and extracts information from those files to be
listed in the diary.  The function accepts arguments specifying what
items should be listed.  For a list of arguments allowed here, see the
variable `org-agenda-entry-types'.

The call in the diary file should look like this:

   &%%(org-diary) ~/path/to/some/orgfile.org

Use a separate line for each org file to check.  Or, if you omit the file name,
all files listed in `org-agenda-files' will be checked automatically:

   &%%(org-diary)

If you don't give any arguments (as in the example above), the default value
of `org-agenda-entry-types' is used: (:deadline :scheduled :timestamp :sexp).
So the example above may also be written as

   &%%(org-diary :deadline :timestamp :sexp :scheduled)

The function expects the lisp variables `entry' and `date' to be provided
by the caller, because this is how the calendar works.  Don't use this
function from a program - use `org-agenda-get-day-entries' instead."
  (with-no-warnings (defvar date) (defvar entry))
  (require 'org-agenda-search)
  (when (> (- (float-time)
	      org-agenda-last-marker-time)
	   5)
    ;; I am not sure if this works with sticky agendas, because the marker
    ;; list is then no longer a global variable.
    (org-agenda-reset-markers))
  (org-compile-prefix-format 'agenda)
  (require 'org-agenda-search)
  (org-set-sorting-strategy 'agenda)
  (setq args (or args org-agenda-entry-types))
  (let* ((files (if (and entry (stringp entry) (string-match "\\S-" entry))
		    (list entry)
		  (org-agenda-files t)))
	 (time (float-time))
	 file rtn results)
    (when (or (not org-diary-last-run-time)
	      (> (- time
		    org-diary-last-run-time)
		 3))
      (org-agenda-prepare-buffers files))
    (setq org-diary-last-run-time time)
    ;; If this is called during org-agenda, don't return any entries to
    ;; the calendar.  Org Agenda will list these entries itself.
    (when org-disable-agenda-to-diary (setq files nil))
    (while (setq file (pop files))
      (require 'org-agenda-search)
      (setq rtn (apply #'org-agenda-get-day-entries file date args))
      (setq results (append results rtn)))
    (when results
      (setq results
	    (mapcar (lambda (i) (replace-regexp-in-string
			    org-link-bracket-re "\\2" i))
		    results))
      (require 'org-agenda-buffer-format)
      (concat (org-agenda-finalize-entries results) "\n"))))

;;; Editing diary from agenda

(declare-function org-datetree-find-date-create "org-datetree" (d &optional keep-restriction))
(defun org-agenda-diary-entry-in-org-file ()
  "Make a diary entry in the file `org-agenda-diary-file'."
  (let (d1 d2 char (text "") dp1 dp2)
    (if (equal (buffer-name) calendar-buffer)
	(setq d1 (calendar-cursor-to-date t)
	      d2 (car calendar-mark-ring))
      (setq dp1 (get-text-property (line-beginning-position) 'day))
      (unless dp1 (user-error "No date defined in current line"))
      (setq d1 (calendar-gregorian-from-absolute dp1)
	    d2 (and (ignore-errors (mark))
		    (save-excursion
		      (goto-char (mark))
                      (setq dp2 (get-text-property (line-beginning-position) 'day)))
		    (calendar-gregorian-from-absolute dp2))))
    (message "Diary entry: [d]ay [a]nniversary [b]lock [j]ump to date tree")
    (setq char (read-char-exclusive))
    (cond
     ((equal char ?d)
      (setq text (read-string "Day entry: "))
      (org-agenda-add-entry-to-org-agenda-diary-file 'day text d1)
      (and (equal (buffer-name) org-agenda-buffer-name) (org-agenda-redo)))
     ((equal char ?a)
      (setq d1 (list (car d1) (nth 1 d1)
		     (read-number (format "Reference year [%d]: " (nth 2 d1))
				  (nth 2 d1))))
      (setq text (read-string "Anniversary (use %d to show years): "))
      (org-agenda-add-entry-to-org-agenda-diary-file 'anniversary text d1)
      (and (equal (buffer-name) org-agenda-buffer-name) (org-agenda-redo)))
     ((equal char ?b)
      (setq text (read-string "Block entry: "))
      (unless (and d1 d2 (not (equal d1 d2)))
	(user-error "No block of days selected"))
      (org-agenda-add-entry-to-org-agenda-diary-file 'block text d1 d2)
      (and (equal (buffer-name) org-agenda-buffer-name) (org-agenda-redo)))
     ((equal char ?j)
      (switch-to-buffer-other-window
       (find-file-noselect org-agenda-diary-file))
      (org-datetree-find-date-create d1)
      (org-fold-reveal t))
     (t (user-error "Invalid selection character `%c'" char)))))

(declare-function org-insert-timestamp "org-timestamp" (time &optional with-hm inactive pre post extra))
(defun org-agenda-add-entry-to-org-agenda-diary-file (type text &optional d1 d2)
  "Add a diary entry with TYPE to `org-agenda-diary-file'.
If TEXT is not empty, it will become the headline of the new entry, and
the resulting entry will not be shown.  When TEXT is empty, switch to
`org-agenda-diary-file' and let the user finish the entry there."
  (let ((cw (current-window-configuration)))
    (switch-to-buffer-other-window
     (find-file-noselect org-agenda-diary-file))
    (widen)
    (goto-char (point-min))
    (cl-case type
      (anniversary
       (or (re-search-forward "^\\*[ \t]+Anniversaries" nil t)
	   (progn
	     (or (org-at-heading-p)
		 (progn
		   (outline-next-heading)
		   (insert "* Anniversaries\n\n")
		   (forward-line -2)))))
       (outline-next-heading)
       (org-back-over-empty-lines)
       (backward-char 1)
       (insert "\n")
       (insert (format "%%%%(org-anniversary %d %2d %2d) %s"
		       (nth 2 d1) (car d1) (nth 1 d1) text)))
      (day
       (let (time)
	 (when org-agenda-insert-diary-extract-time
           (pcase-let
               ((`(,matched-time ,start-time ,end-time)
                 (org-agenda--find-time-in-string text)))
             ;; Normalize the time(s) to 24 hour.
             (let ((org-agenda-time-leading-zero t))
	       (when start-time (setq start-time (org-get-time-of-day start-time t)))
	       (when end-time (setq end-time (org-get-time-of-day end-time t))))
             (when matched-time
	       (when (string-match (concat (regexp-quote matched-time) " *") text)
	         (setq text (replace-match "" nil nil text)))
               (setq time (if end-time (format " %s-%s" start-time end-time)
                            (format " %s" start-time)))))
	   (if (eq org-agenda-insert-diary-strategy 'top-level)
	       (org-agenda-insert-diary-as-top-level text)
	     (require 'org-datetree)
	     (org-datetree-find-date-create d1)
	     (org-agenda-insert-diary-make-new-entry text))
	   (org-insert-timestamp (org-time-from-absolute
				  (calendar-absolute-from-gregorian d1))
			         nil nil nil nil time)))
       (end-of-line 0))
      ((block) ;; Wrap this in (strictly unnecessary) parens because
       ;; otherwise the indentation gets confused by the
       ;; special meaning of 'block
       (when (> (calendar-absolute-from-gregorian d1)
		(calendar-absolute-from-gregorian d2))
	 (setq d1 (prog1 d2 (setq d2 d1))))
       (if (eq org-agenda-insert-diary-strategy 'top-level)
	   (org-agenda-insert-diary-as-top-level text)
	 (require 'org-datetree)
	 (org-datetree-find-date-create d1)
	 (org-agenda-insert-diary-make-new-entry text))
       (org-insert-timestamp (org-time-from-absolute
			      (calendar-absolute-from-gregorian d1)))
       (insert "--")
       (org-insert-timestamp (org-time-from-absolute
			      (calendar-absolute-from-gregorian d2)))
       (end-of-line 0)))
    (if (string-match "\\S-" text)
	(progn
	  (set-window-configuration cw)
	  (message "%s entry added to %s"
		   (capitalize (symbol-name type))
		   (abbreviate-file-name org-agenda-diary-file)))
      (org-fold-reveal t)
      (message "Please finish entry here"))))

(defun org-agenda-insert-diary-as-top-level (text)
  "Make new entry as a top-level entry at the end of the file.
Add TEXT as headline, and position the cursor in the second line so that
a timestamp can be added there."
  (widen)
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (org-insert-heading nil t t)
  (insert text)
  (org-end-of-meta-data)
  (unless (bolp) (insert "\n"))
  (when org-adapt-indentation (indent-to-column 2)))

(declare-function org-do-demote "org-edit-structure" ())
(declare-function org-insert-heading "org-edit-structure" (&optional arg invisible-ok level))
(defun org-agenda-insert-diary-make-new-entry (text)
  "Make a new entry with TEXT as a child of the current subtree.
Position the point in the heading's first body line so that
a timestamp can be added there."
  (cond
   ((eq org-agenda-insert-diary-strategy 'date-tree-last)
    (end-of-line)
    (org-insert-heading '(4) t)
    (org-do-demote))
   (t
    (outline-next-heading)
    (org-back-over-empty-lines)
    (unless (looking-at "[ \t]*$") (save-excursion (insert "\n")))
    (org-insert-heading nil t)
    (org-do-demote)))
  (let ((col (current-column)))
    (insert text)
    (org-end-of-meta-data)
    ;; Ensure point is left on a blank line, at proper indentation.
    (unless (bolp) (insert "\n"))
    (unless (looking-at-p "^[ \t]*$") (save-excursion (insert "\n")))
    (when org-adapt-indentation (indent-to-column col)))
  (org-fold-show-set-visibility 'lineage))

;;;###autoload
(defun org-agenda-diary-entry ()
  "Make a diary entry, like the `i' command from the calendar.
All the standard commands work: block, weekly etc.
When `org-agenda-diary-file' points to a file,
`org-agenda-diary-entry-in-org-file' is called instead to create
entries in that Org file."
  (interactive)
  (if (not (eq org-agenda-diary-file 'diary-file))
      (org-agenda-diary-entry-in-org-file)
    (require 'diary-lib)
    (let* ((char (read-char-exclusive
		  "Diary entry: [d]ay [w]eekly [m]onthly [y]early\
 [a]nniversary [b]lock [c]yclic"))
	   (cmd (cdr (assoc char
			    '((?d . diary-insert-entry)
			      (?w . diary-insert-weekly-entry)
			      (?m . diary-insert-monthly-entry)
			      (?y . diary-insert-yearly-entry)
			      (?a . diary-insert-anniversary-entry)
			      (?b . diary-insert-block-entry)
			      (?c . diary-insert-cyclic-entry)))))
	   (oldf (symbol-function 'calendar-cursor-to-date))
	   ;; (buf (get-file-buffer (substitute-in-file-name diary-file)))
	   (point (point))
	   (mark (or (mark t) (point))))
      (unless cmd
	(user-error "No command associated with <%c>" char))
      (unless (and (get-text-property point 'day)
		   (or (not (equal ?b char))
		       (get-text-property mark 'day)))
	(user-error "Don't know which date to use for diary entry"))
      ;; We implement this by hacking the `calendar-cursor-to-date' function
      ;; and the `calendar-mark-ring' variable.  Saves a lot of code.
      (let ((calendar-mark-ring
	     (list (calendar-gregorian-from-absolute
		    (or (get-text-property mark 'day)
			(get-text-property point 'day))))))
	(unwind-protect
	    (progn
	      (fset 'calendar-cursor-to-date
		    (lambda (&optional _error _dummy)
		      (calendar-gregorian-from-absolute
		       (get-text-property point 'day))))
	      (call-interactively cmd))
	  (fset 'calendar-cursor-to-date oldf))))))

(provide 'org-agenda-diary)

;;; org-agenda-diary.el ends here
