;;; org-agenda-line-format.el --- Agenda item format         -*- lexical-binding: t; -*-

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
;; This library implements functions rendering the matching agenda
;; items.

;;; Code:

(require 'org-priority-common)
(require 'org-outline)
(require 'org-duration)
(require 'org-agenda-mode)

;;; Customizations

(defgroup org-agenda-line-format nil
  "Options concerning the entry prefix in the Org agenda display."
  :tag "Org Agenda Line Format"
  :group 'org-agenda)

(defcustom org-agenda-prefix-format
  '((agenda  . " %i %-12:c%?-12t% s")
    (todo  . " %i %-12:c")
    (tags  . " %i %-12:c")
    (search . " %i %-12:c"))
  "Format specifications for the prefix of items in the agenda views.

An alist with one entry per agenda type.  The keys of the
sublists are `agenda', `todo', `search' and `tags'.  The values
are format strings.

This format works similar to a printf format, with the following meaning:

  %c   the category of the item, \"Diary\" for entries from the diary,
       or as given by the CATEGORY keyword or derived from the file name
  %e   the effort required by the item
  %l   the level of the item (insert X space(s) if item is of level X)
  %i   the icon category of the item, see `org-agenda-category-icon-alist'
  %T   the last tag of the item (ignore inherited tags, which come first)
  %t   the HH:MM time-of-day specification if one applies to the entry
  %s   Scheduling/Deadline information, a short string
  %b   show breadcrumbs, i.e., the names of the higher levels
  %(expression) Eval EXPRESSION and replace the control string
                by the result

All specifiers work basically like the standard `%s' of printf, but may
contain two additional characters: a question mark just after the `%'
and a whitespace/punctuation character just before the final letter.

If the first character after `%' is a question mark, the entire field
will only be included if the corresponding value applies to the current
entry.  This is useful for fields which should have fixed width when
present, but zero width when absent.  For example, \"%?-12t\" will
result in a 12 character time field if a time of the day is specified,
but will completely disappear in entries which do not contain a time.

If there is punctuation or whitespace character just before the
final format letter, this character will be appended to the field
value if the value is not empty.  For example, the format
\"%-12:c\" leads to \"Diary: \" if the category is \"Diary\".  If
the category is empty, no additional colon is inserted.

The default value for the agenda sublist is \"  %-12:c%?-12t% s\",
which means:

- Indent the line with two space characters
- Give the category a 12 chars wide field, padded with whitespace on
  the right (because of `-').  Append a colon if there is a category
  (because of `:').
- If there is a time-of-day, put it into a 12 chars wide field.  If no
  time, don't put in an empty field, just skip it (because of '?').
- Finally, put the scheduling information.

See also the variables `org-agenda-remove-times-when-in-prefix' and
`org-agenda-remove-tags'.

Custom commands can set this variable in the options section."
  :type '(choice
	  (string :tag "General format")
	  (list :greedy t :tag "View dependent"
		(cons  (const agenda) (string :tag "Format"))
		(cons  (const todo) (string :tag "Format"))
		(cons  (const tags) (string :tag "Format"))
		(cons  (const search) (string :tag "Format"))))
  :group 'org-agenda-line-format
  :version "26.1"
  :package-version '(Org . "9.1"))

(defcustom org-agenda-time-leading-zero nil
  "Non-nil means use leading zero for military times in agenda.
For example, 9:30am would become 09:30 rather than  9:30."
  :group 'org-agenda-daily/weekly
  :group 'org-agenda-line-format
  :version "24.1"
  :type 'boolean)

(defcustom org-agenda-breadcrumbs-separator "->"
  "The separator of breadcrumbs in agenda lines."
  :group 'org-agenda-line-format
  :package-version '(Org . "9.3")
  :type 'string
  :safe #'stringp)

(defcustom org-agenda-timerange-leaders '("" "(%d/%d): ")
  "Text preceding timerange entries in the agenda view.
This is a list with two strings.  The first applies when the range
is entirely on one day.  The second applies if the range spans several days.
The strings may have two \"%d\" format specifiers which will be filled
with the sequence number of the days, and the total number of days in the
range, respectively."
  :group 'org-agenda-line-format
  :type '(list
	  (string :tag "Deadline today   ")
	  (choice :tag "Deadline relative"
		  (string :tag "Format string")
		  (function))))

(defcustom org-agenda-scheduled-leaders '("Scheduled: " "Sched.%2dx: ")
  "Text preceding scheduled items in the agenda view.
This is a list with two strings.  The first applies when the item is
scheduled on the current day.  The second applies when it has been scheduled
previously, it may contain a %d indicating that this is the nth time that
this item is scheduled, due to automatic rescheduling of unfinished items
for the following day.  So this number is one larger than the number of days
that passed since this item was scheduled first."
  :group 'org-agenda-line-format
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(list
	  (string :tag "Scheduled today     ")
	  (string :tag "Scheduled previously")))

(defcustom org-agenda-deadline-leaders '("Deadline:  " "In %3d d.: " "%2d d. ago: ")
  "Text preceding deadline items in the agenda view.
This is a list with three strings.  The first applies when the item has its
deadline on the current day.  The second applies when the deadline is in the
future, the third one when it is in the past.  The strings may contain %d
to capture the number of days."
  :group 'org-agenda-line-format
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(list
	  (string :tag "Deadline today          ")
	  (string :tag "Deadline in the future  ")
	  (string :tag "Deadline in the past    ")))

(defcustom org-agenda-remove-times-when-in-prefix t
  "Non-nil means remove duplicate time specifications in agenda items.
When the format `org-agenda-prefix-format' contains a `%t' specifier, a
time-of-day specification in a headline or diary entry is extracted and
placed into the prefix.  If this option is non-nil, the original specification
\(a timestamp or -range, or just a plain time(range) specification like
11:30-4pm) will be removed for agenda display.  This makes the agenda less
cluttered.
The option can be t or nil.  It may also be the symbol `beg', indicating
that the time should only be removed when it is located at the beginning of
the headline/diary entry."
  :group 'org-agenda-line-format
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (const :tag "When at beginning of entry" beg)))

(defcustom org-agenda-remove-timeranges-from-blocks nil
  "Non-nil means remove time ranges specifications in agenda
items that span on several days."
  :group 'org-agenda-line-format
  :version "24.1"
  :type 'boolean)

(defcustom org-agenda-default-appointment-duration nil
  "Default duration for appointments that only have a starting time.
When nil, no duration is specified in such cases.
When non-nil, this must be the number of minutes, e.g. 60 for one hour."
  :group 'org-agenda-line-format
  :type '(choice
	  (integer :tag "Minutes")
	  (const :tag "No default duration")))

(defvaralias 'org-agenda-remove-tags-when-in-prefix
  'org-agenda-remove-tags)

(defcustom org-agenda-remove-tags nil
  "Non-nil means remove the tags from the headline copy in the agenda.
When this is the symbol `prefix', only remove tags when
`org-agenda-prefix-format' contains a `%T' specifier."
  :group 'org-agenda-line-format
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (const :tag "When prefix format contains %T" prefix)))

(defcustom org-agenda-category-icon-alist nil
  "Alist of category icon to be displayed in agenda views.

The icons are displayed in place of the %i placeholders in
`org-agenda-prefix-format', which see.

Each entry should have the following format:

  (CATEGORY-REGEXP FILE-OR-DATA TYPE DATA-P PROPS)

Where CATEGORY-REGEXP is a regexp matching the categories where
the icon should be displayed.
FILE-OR-DATA either a file path or a string containing image data.

The other fields can be omitted safely if not needed:
TYPE indicates the image type.
DATA-P is a boolean indicating whether the FILE-OR-DATA string is
image data.
PROPS are additional image attributes to assign to the image,
like, e.g. `:ascent center'.

   (\"Org\" \"/path/to/icon.png\" nil nil :ascent center)

If you want to set the display properties yourself, just put a
list as second element:

  (CATEGORY-REGEXP (MY PROPERTY LIST))

For example, to display a 16px horizontal space for Emacs
category, you can use:

  (\"Emacs\" \\='(space . (:width (16))))"
  :group 'org-agenda-line-format
  :version "24.1"
  :type '(alist :key-type (regexp :tag "Regexp matching category")
		:value-type (choice (list :tag "Icon"
					  (string :tag "File or data")
					  (symbol :tag "Type")
					  (boolean :tag "Data?")
					  (repeat :tag "Extra image properties" :inline t sexp))
				    (list :tag "Display properties" sexp))))

(defcustom org-agenda-hide-tags-regexp nil
  "Regular expression used to filter away specific tags in agenda views.
This means that these tags will be present, but not be shown in the agenda
line.  Secondary filtering will still work on the hidden tags.
Nil means don't hide any tags."
  :group 'org-agenda-line-format
  :type '(choice
	  (const  :tag "Hide none" nil)
	  (regexp :tag "Regexp   ")))

(defcustom org-agenda-search-headline-for-time t
  "Non-nil means search headline for a time-of-day.
If the headline contains a time-of-day in one format or another, it will
be used to sort the entry into the time sequence of items for a day.
Timestamps in the headline will be ignored."
  :group 'org-agenda-time-grid
  :type 'boolean)

;;; Customization for time grid

(defgroup org-agenda-time-grid nil
  "Options concerning the time grid in the Org Agenda."
  :tag "Org Agenda Time Grid"
  :group 'org-agenda)

(defcustom org-agenda-use-time-grid t
  "Non-nil means show a time grid in the agenda schedule.
A time grid is a set of lines for specific times (like every two hours between
8:00 and 20:00).  The items scheduled for a day at specific times are
sorted in between these lines.
For details about when the grid will be shown, and what it will look like, see
the variable `org-agenda-time-grid'."
  :group 'org-agenda-time-grid
  :type 'boolean)

(defcustom org-agenda-time-grid
  (let ((graphical (and (display-graphic-p)
                        (char-displayable-p ?┄))))
    `((daily today require-timed)
      (800 1000 1200 1400 1600 1800 2000)
      ,(if graphical " ┄┄┄┄┄ " "......")
      ,(if graphical "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄" "----------------")))
  "The settings for time grid for agenda display.
This is a list of four items.  The first item is again a list.  It contains
symbols specifying conditions when the grid should be displayed:

 daily         if the agenda shows a single day
 weekly        if the agenda shows an entire week
 today         show grid on current date, independent of daily/weekly display
 require-timed show grid only if at least one item has a time specification
 remove-match  skip grid times already present in an entry

The second item is a list of integers, indicating the times that
should have a grid line.

The third item is a string which will be placed right after the
times that have a grid line.

The fourth item is a string placed after the grid times.  This
will align with agenda items."
  :group 'org-agenda-time-grid
  :package-version '(Org . "9.6")
  :type
  '(list
    (set :greedy t :tag "Grid Display Options"
	 (const :tag "Show grid in single day agenda display" daily)
	 (const :tag "Show grid in weekly agenda display" weekly)
	 (const :tag "Always show grid for today" today)
	 (const :tag "Show grid only if any timed entries are present"
		require-timed)
	 (const :tag "Skip grid times already present in an entry"
		remove-match))
    (repeat :tag "Grid Times" (integer :tag "Time"))
    (string :tag "Grid String (after agenda times)")
    (string :tag "Grid String (aligns with agenda items)")))

(defcustom org-agenda-show-current-time-in-grid t
  "Non-nil means show the current time in the time grid."
  :group 'org-agenda-time-grid
  :version "24.1"
  :type 'boolean)

(defcustom org-agenda-current-time-string
  (if (and (display-graphic-p)
           (char-displayable-p ?←)
           (char-displayable-p ?─))
      "← now ───────────────────────────────────────────────"
    "now - - - - - - - - - - - - - - - - - - - - - - - - -")
  "The string for the current time marker in the agenda."
  :group 'org-agenda-time-grid
  :package-version '(Org . "9.6")
  :type 'string)

(defcustom org-agenda-timegrid-use-ampm nil
  "When set, show AM/PM style timestamps on the timegrid."
  :group 'org-agenda-time-grid
  :group 'org-agenda
  :version "24.1"
  :type 'boolean)

;;; Functions and internal variables

(defun org-agenda-add-time-grid-maybe (list ndays todayp)
  "Add a time-grid for agenda items which need it.

LIST is the list of agenda items formatted by `org-agenda-list'.
NDAYS is the span of the current agenda view.
TODAYP is t when the current agenda view is on today."
  (catch 'exit
    (cond ((not org-agenda-use-time-grid) (throw 'exit list))
	  ((and todayp (member 'today (car org-agenda-time-grid))))
	  ((and (= ndays 1) (member 'daily (car org-agenda-time-grid))))
	  ((member 'weekly (car org-agenda-time-grid)))
	  (t (throw 'exit list)))
    (let* ((have (delq nil (mapcar
			  (lambda (x) (get-text-property 1 'time-of-day x))
			  list)))
	   (string (nth 3 org-agenda-time-grid))
	   (gridtimes (nth 1 org-agenda-time-grid))
	   (req (car org-agenda-time-grid))
	   (remove (member 'remove-match req))
	   new time
           ;; We abuse `org-agenda-format-item' to format grid lines
           ;; here.  Prevent it from adding default duration, if any
           ;; to the grid lines.
           (org-agenda-default-appointment-duration nil))
      (when (and (member 'require-timed req) (not have))
	;; don't show empty grid
	(throw 'exit list))
      (while (setq time (pop gridtimes))
	(unless (and remove (member time have))
	  (setq time (replace-regexp-in-string " " "0" (format "%04s" time)))
	  (push (org-agenda-format-item
		 nil string nil "" nil
		 (concat (substring time 0 -2) ":" (substring time -2)))
		new)
	  (put-text-property
	   2 (length (car new)) 'face 'org-time-grid (car new))))
      (when (and todayp org-agenda-show-current-time-in-grid)
	(push (org-agenda-format-item
	       nil org-agenda-current-time-string nil "" nil
	       (format-time-string "%H:%M "))
	      new)
	(put-text-property
	 2 (length (car new)) 'face 'org-agenda-current-time (car new)))

      (if (member 'time-up org-agenda-sorting-strategy-selected)
	  (append new list)
	(append list new)))))

(defvar org-prefix-has-time nil
  "A flag, set by `org-compile-prefix-format'.
The flag is set if the currently compiled format contains a `%t'.")
(defvar org-prefix-has-tag nil
  "A flag, set by `org-compile-prefix-format'.
The flag is set if the currently compiled format contains a `%T'.")
(defvar org-prefix-has-effort nil
  "A flag, set by `org-compile-prefix-format'.
The flag is set if the currently compiled format contains a `%e'.")
(defvar org-prefix-has-breadcrumbs nil
  "A flag, set by `org-compile-prefix-format'.
The flag is set if the currently compiled format contains a `%b'.")
(defvar org-prefix-category-length nil
  "Used by `org-compile-prefix-format' to remember the category field width.")
(defvar org-prefix-category-max-length nil
  "Used by `org-compile-prefix-format' to remember the category field width.")
(defvar org-prefix-format-compiled nil
  "The compiled prefix format and associated variables.
This is a list where first element is a list of variable bindings, and second
element is the compiled format expression.  See the variable
`org-agenda-prefix-format'.")

(defun org-compile-prefix-format (key)
  "Compile the prefix format into a Lisp form that can be evaluated.
KEY is the agenda type (see `org-agenda-prefix-format').
The resulting form and associated variable bindings is returned
and stored in the variable `org-prefix-format-compiled'."
  (setq org-prefix-has-time nil
	org-prefix-has-tag nil
	org-prefix-category-length nil
	org-prefix-has-effort nil
	org-prefix-has-breadcrumbs nil)
  (let ((s (cond
	    ((stringp org-agenda-prefix-format)
	     org-agenda-prefix-format)
	    ((assq key org-agenda-prefix-format)
	     (cdr (assq key org-agenda-prefix-format)))
	    (t "  %-12:c%?-12t% s")))
	(start 0)
	varform vars var c f opt) ;; e
    (while (string-match "%\\(\\?\\)?\\([-+]?[0-9.]*\\)\\([ .;,:!?=|/<>]?\\)\\([cltseib]\\|(.+?)\\)"
			 s start)
      (setq var (or (cdr (assoc (match-string 4 s)
				'(("c" . category) ("t" . time) ("l" . level) ("s" . extra)
				  ("i" . category-icon) ("T" . tag) ("e" . effort) ("b" . breadcrumbs))))
		    'eval)
	    c (or (match-string 3 s) "")
	    opt (match-beginning 1)
	    start (1+ (match-beginning 0)))
      (cl-case var
	(time        (setq org-prefix-has-time        t))
	(tag         (setq org-prefix-has-tag         t))
	(effort      (setq org-prefix-has-effort      t))
	(breadcrumbs (setq org-prefix-has-breadcrumbs t)))
      (setq f (concat "%" (match-string 2 s) "s"))
      (when (eq var 'category)
	(setq org-prefix-category-length
	      (floor (abs (string-to-number (match-string 2 s)))))
	(setq org-prefix-category-max-length
	      (let ((x (match-string 2 s)))
		(save-match-data
		  (and (string-match "\\.[0-9]+" x)
		       (string-to-number (substring (match-string 0 x) 1)))))))
      (if (eq var 'eval)
	  (setq varform `(format ,f (org-eval ,(read (substring s (match-beginning 4))))))
	(if opt
	    (setq varform
		  `(if (member ,var '("" nil))
		       ""
		     (format ,f (concat ,var ,c))))
	  (setq varform
		`(format ,f (if (member ,var '("" nil)) ""
			      (concat ,var ,c (get-text-property 0 'extra-space ,var)))))))
      (if (eq var 'eval)
          (setf (substring s (match-beginning 0)
                           (+ (match-beginning 4)
                              (length (format "%S" (read (substring s (match-beginning 4)))))))
                "%s")
        (setq s (replace-match "%s" t nil s)))
      (push varform vars))
    (setq vars (nreverse vars))
    (with-current-buffer (or org-agenda-buffer (current-buffer))
      (setq org-prefix-format-compiled
	    (list
	     `((org-prefix-has-time ,org-prefix-has-time)
	       (org-prefix-has-tag ,org-prefix-has-tag)
	       (org-prefix-category-length ,org-prefix-category-length)
	       (org-prefix-has-effort ,org-prefix-has-effort)
	       (org-prefix-has-breadcrumbs ,org-prefix-has-breadcrumbs))
	     `(format ,s ,@vars))))))

(defun org-agenda-format-item (extra txt &optional with-level with-category tags dotime
				     remove-re habitp)
  "Format TXT to be inserted into the agenda buffer.
In particular, add the prefix and corresponding text properties.

EXTRA must be a string to replace the `%s' specifier in the prefix format.
WITH-LEVEL may be a string to replace the `%l' specifier.
WITH-CATEGORY (a string, a symbol or nil) may be used to overrule the default
category taken from local variable or file name.  It will replace the `%c'
specifier in the format.
DOTIME, when non-nil, indicates that a time-of-day should be extracted from
TXT for sorting of this entry, and for the `%t' specifier in the format.
When DOTIME is a string, this string is searched for a time before TXT is.
TAGS can be the tags of the headline.
Any match of REMOVE-RE will be removed from TXT."
  ;; We keep the org-prefix-* variable values along with a compiled
  ;; formatter, so that multiple agendas existing at the same time do
  ;; not step on each other toes.
  ;;
  ;; It was inconvenient to make these variables buffer local in
  ;; Agenda buffers, because this function expects to be called with
  ;; the buffer where item comes from being current, and not agenda
  ;; buffer
  (let* ((bindings (car org-prefix-format-compiled))
	 (formatter (cadr org-prefix-format-compiled)))
    (cl-loop for (var value) in bindings
	     do (set var value))
    (save-match-data
      ;; Diary entries sometimes have extra whitespace at the beginning
      (setq txt (org-trim txt))

      ;; Fix the tags part in txt
      (setq txt (org-agenda-fix-displayed-tags
		 txt tags
		 org-agenda-show-inherited-tags
		 org-agenda-hide-tags-regexp))

      (with-no-warnings
	;; `time', `tag', `effort' are needed for the eval of the prefix format.
	;; Based on what I see in `org-compile-prefix-format', I added
	;; a few more.
        (defvar breadcrumbs) (defvar category) (defvar category-icon)
        (defvar effort) (defvar extra)
        (defvar level) (defvar tag) (defvar time))
      (let* ((category (or with-category
			   (if buffer-file-name
			       (file-name-sans-extension
				(file-name-nondirectory buffer-file-name))
			     "")))
             (full-category category)
	     (category-icon (org-agenda-get-category-icon category))
	     (category-icon (if category-icon
				(propertize " " 'display category-icon)
			      ""))
	     (effort (and (not (string= txt ""))
			  (get-text-property 1 'effort txt)))
	     (tag (if tags (nth (1- (length tags)) tags) ""))
	     (time-grid-trailing-characters (nth 2 org-agenda-time-grid))
	     (extra (or (and (not habitp) extra) ""))
	     time
	     (string-containing-time
              (when dotime (concat
			    (if (stringp dotime) dotime "")
			    (and org-agenda-search-headline-for-time
                                 ;; Do not search inside
                                 ;; timestamps.  They are handled
                                 ;; separately.
                                 (replace-regexp-in-string
                                  org-ts-regexp-both ""
                                  txt)))))
	     (time-of-day (and dotime (org-get-time-of-day string-containing-time)))
	     timestamp-range? plain-time? date-range-same-day?
             time-string start-time end-time rtn
	     duration breadcrumbs)
	(and (derived-mode-p 'org-mode) buffer-file-name
	     (add-to-list 'org-agenda-contributing-files buffer-file-name))
	(when (and dotime time-of-day)
	  ;; Extract starting and ending time and move them to prefix
	  (when (or (setq timestamp-range?
                          (string-match org-stamp-time-of-day-regexp
                                        string-containing-time))
		    (setq plain-time?
                          (string-match org-plain-time-of-day-regexp
                                        string-containing-time)))
	    (setq time-string (match-string 0 string-containing-time)
		  date-range-same-day? (and timestamp-range? (match-end 3))
		  start-time (match-string (if plain-time? 1 2)
                                           string-containing-time)
		  end-time (match-string (if plain-time? 8
                                           (if date-range-same-day? 4 6))
                                         string-containing-time))

	    ;; If the times are in TXT (not in DOTIMES), and the prefix will list
	    ;; them, we might want to remove them there to avoid duplication.
	    ;; The user can turn this off with a variable.
	    (when (and org-prefix-has-time
		       org-agenda-remove-times-when-in-prefix
                       (or timestamp-range? plain-time?)
		       (string-match (concat (regexp-quote time-string) " *") txt)
		       (not (equal ?\] (string-to-char (substring txt (match-end 0)))))
		       (if (eq org-agenda-remove-times-when-in-prefix 'beg)
			   (= (match-beginning 0) 0)
			 t))
	      (setq txt (replace-match "" nil nil txt))))
          ;; Normalize the time(s) to 24 hour.
	  (when start-time (setq start-time (org-get-time-of-day start-time t)))
	  (when end-time (setq end-time (org-get-time-of-day end-time t)))
	  ;; Try to set s2 if s1 and
	  ;; `org-agenda-default-appointment-duration' are set
	  (when (and start-time (not end-time)
                     org-agenda-default-appointment-duration)
	    (setq end-time
	          (org-duration-from-minutes
	           (+ (org-duration-to-minutes start-time t)
		      org-agenda-default-appointment-duration)
	           nil t)))
	  ;; Compute the duration
	  (when end-time
	    (setq duration (- (org-duration-to-minutes end-time)
			      (org-duration-to-minutes start-time))))
          ;; Format S1 and S2 for display.
	  (when start-time
            (setq start-time (format "%5s" (org-get-time-of-day start-time 'overtime))))
	  (when end-time
            (setq end-time (org-get-time-of-day end-time 'overtime))))
	(when (string-match org-tag-group-re txt)
	  ;; Tags are in the string
	  (if (or (eq org-agenda-remove-tags t)
		  (and org-agenda-remove-tags
		       org-prefix-has-tag))
	      (setq txt (replace-match "" t t txt))
	    (setq txt (replace-match
		       (concat (make-string (max (- 50 (length txt)) 1) ?\ )
			       (match-string 1 txt))
		       t t txt))))

	(when remove-re
	  (while (string-match remove-re txt)
	    (setq txt (replace-match "" t t txt))))

	;; Set org-heading property on `txt' to mark the start of the
	;; heading.
	(add-text-properties 0 (length txt) '(org-heading t) txt)

	;; Prepare the variables needed in the eval of the compiled format
	(when org-prefix-has-breadcrumbs
	  (setq breadcrumbs
                ;; When called from Org buffer, remain in position.
                ;; When called from Agenda buffer, jump to headline position first.
                (org-with-point-at (org-get-at-bol 'org-marker)
		  (let ((s (if (derived-mode-p 'org-mode)
                               (org-format-outline-path (org-get-outline-path)
						        (1- (frame-width))
						        nil org-agenda-breadcrumbs-separator)
                             ;; Not in Org buffer.  This can happen,
                             ;; for example, in
                             ;; `org-agenda-add-time-grid-maybe' where
                             ;; time grid does not correspond to a
                             ;; particular heading.
                             "")))
		    (if (equal "" s) "" (concat s org-agenda-breadcrumbs-separator))))))
	(setq time (cond (end-time
                          (concat
			   (org-agenda-time-of-day-to-ampm-maybe start-time)
			   "-" (org-agenda-time-of-day-to-ampm-maybe end-time)
			   (when org-agenda-timegrid-use-ampm " ")))
			 (start-time
                          (concat
			   (org-agenda-time-of-day-to-ampm-maybe start-time)
			   (if org-agenda-timegrid-use-ampm
                               (concat time-grid-trailing-characters " ")
                             time-grid-trailing-characters)))
			 (t ""))
	      category (if (symbolp category) (symbol-name category) category)
	      level (or with-level ""))
	(if (string-match org-link-bracket-re category)
	    (let ((link-width (string-width (or (match-string 2) (match-string 1)))))
	      (when (< link-width (or org-prefix-category-length 0))
	        (setq category (copy-sequence category))
	        (org-add-props category nil
		  'extra-space (make-string
			        (- org-prefix-category-length link-width 1) ?\ ))))
	  (when (and org-prefix-category-max-length
		     (>= (length category) org-prefix-category-max-length))
	    (setq category (substring category 0 (1- org-prefix-category-max-length)))))
	;; Evaluate the compiled format
	(setq rtn (concat (eval formatter t) txt))

	;; And finally add the text properties
	(remove-text-properties 0 (length rtn) '(line-prefix t wrap-prefix t) rtn)
	(org-add-props rtn nil
          ;; CATEGORY might be truncated.  Store the full category in
          ;; the properties.
	  'org-category full-category
          'tags tags
          'org-priority-highest org-priority-highest
	  'org-priority-lowest org-priority-lowest
	  'time-of-day time-of-day
	  'duration duration
	  'breadcrumbs breadcrumbs
	  'txt txt
	  'level level
	  'time time
	  'extra extra
	  'format org-prefix-format-compiled
	  'dotime dotime)))))

(defun org-agenda-change-all-lines (newhead hdmarker
				            &optional fixface just-this)
  "Change all lines in the agenda buffer which match HDMARKER.
The new content of the line will be NEWHEAD (as modified by
`org-agenda-format-item').  HDMARKER is checked with
`equal' against all `org-hd-marker' text properties in the file.
If FIXFACE is non-nil, the face of each item is modified according to
the new TODO state.
If JUST-THIS is non-nil, change just the current line, not all.
If FORCE-TAGS is non-nil, the car of it returns the new tags."
  (let* ((inhibit-read-only t)
	 (line (org-current-line))
	 (org-agenda-buffer (current-buffer))
	 (thetags (with-current-buffer (marker-buffer hdmarker)
		    (org-get-tags hdmarker)))
	 props m undone-face done-face finish new dotime level cat tags
         effort effort-minutes) ;; pl
    (save-excursion
      (goto-char (point-max))
      (forward-line 0)
      (while (not finish)
	(setq finish (bobp))
	(when (and (setq m (org-get-at-bol 'org-hd-marker))
		   (or (not just-this) (= (org-current-line) line))
		   (equal m hdmarker))
	  (setq props (text-properties-at (point))
		dotime (org-get-at-bol 'dotime)
		cat (org-agenda-get-category)
		level (org-get-at-bol 'level)
		tags thetags
                effort (org-get-at-bol 'effort)
                effort-minutes (org-get-at-bol 'effort-minutes)
		new
		(let ((org-prefix-format-compiled
		       (or (get-text-property (min (1- (point-max)) (point)) 'format)
			   org-prefix-format-compiled))
		      (extra (org-get-at-bol 'extra)))
		  (with-current-buffer (marker-buffer hdmarker)
		    (org-with-wide-buffer
		     (org-agenda-format-item extra
                                             (org-add-props newhead nil
                                               'effort effort
                                               'effort-minutes effort-minutes)
                                             level cat tags dotime))))
                ;; pl (text-property-any (line-beginning-position)
                ;;                       (line-end-position) 'org-heading t)
		undone-face (org-get-at-bol 'undone-face)
		done-face (org-get-at-bol 'done-face))
	  (forward-line 0)
	  (cond
	   ((equal new "") (delete-region (point) (line-beginning-position 2)))
	   ((looking-at ".*")
	    ;; When replacing the whole line, preserve bulk mark
	    ;; overlay, if any.
	    (let ((mark (catch :overlay
			  (dolist (o (overlays-in (point) (+ 2 (point))))
			    (when (eq (overlay-get o 'type)
				      'org-marked-entry-overlay)
			      (throw :overlay o))))))
	      (replace-match new t t)
	      (forward-line 0)
	      (when mark (move-overlay mark (point) (+ 2 (point)))))
            (add-text-properties (line-beginning-position)
                                 (line-end-position) props)
	    (when fixface
              (require 'org-todo)
              (defvar org-last-todo-state-is-todo)
	      (add-text-properties
               (line-beginning-position) (line-end-position)
	       (list 'face
                     ;; FIXME: This assumes that we have just modifed
                     ;; the item and immediately called
                     ;; `org-agenda-change-all-lines' - may or may not
                     ;; be true.
		     (if org-last-todo-state-is-todo
			 undone-face done-face))))
	    (org-agenda-highlight-todo 'line)
	    (forward-line 0))
	   (t (error "Line update did not work")))
	  (save-restriction
            (narrow-to-region (line-beginning-position) (line-end-position))
	    (org-agenda-finalize)))
	(forward-line -1)))))

(defun org-agenda-fix-displayed-tags (txt tags add-inherited hide-re)
  "Remove tags string from TXT, and add a modified list of tags.
The modified list may contain inherited tags, and tags matched by
`org-agenda-hide-tags-regexp' will be removed."
  (when (or add-inherited hide-re)
    (when (string-match org-tag-group-re txt)
      (setq txt (substring txt 0 (match-beginning 0))))
    (setq tags
	  (delq nil
		(mapcar (lambda (tg)
			  (if (or (and hide-re (string-match hide-re tg))
				  (and (not add-inherited)
				       (get-text-property 0 'inherited tg)))
			      nil
			    tg))
			tags)))
    (when tags
      (let ((have-i (get-text-property 0 'inherited (car tags)))
	    i)
	(setq txt (concat txt " :"
			  (mapconcat
			   (lambda (x)
			     (setq i (get-text-property 0 'inherited x))
			     (if (and have-i (not i))
				 (progn
				   (setq have-i nil)
				   (concat ":" x))
			       x))
			   tags ":")
			  (if have-i "::" ":"))))))
  txt)

(defun org-get-time-of-day (s &optional string)
  "Check string S for a time of day.

If found, return it as a military time number between 0 and 2400.
If not found, return nil.

The optional STRING argument forces conversion into a 5 character wide string
HH:MM.  When it is `overtime', any time above 24:00 is turned into \"+H:MM\"
where H:MM is the duration above midnight."
  (let ((case-fold-search t)
        (time-regexp
         (rx word-start
             (group (opt (any "012")) digit)           ;group 1: hours
             (or (and ":" (group (any "012345") digit) ;group 2: minutes
                      (opt (group (or "am" "pm"))))    ;group 3: am/pm
                 ;; Special "HHam/pm" case.
                 (group-n 3 (or "am" "pm")))
             word-end)))
    (save-match-data
      (when (and (string-match time-regexp s)
                 (not (eq 'org-link (get-text-property 1 'face s))))
        (let ((hours
               (let* ((ampm (and (match-end 3) (downcase (match-string 3 s))))
                      (am-p (equal ampm "am")))
                 (pcase (string-to-number (match-string 1 s))
                   ((and (guard (not ampm)) h) h)
                   (12 (if am-p 0 12))
                   (h (+ h (if am-p 0 12))))))
              (minutes
               (if (match-end 2)
                   (string-to-number (match-string 2 s))
                 0)))
          (pcase string
            (`nil (+ minutes (* hours 100)))
            ((and `overtime
                  (guard (or (> hours 24)
                             (and (= hours 24)
                                  (> minutes 0)))))
             (format "+%d:%02d" (- hours 24) minutes))
            ((guard org-agenda-time-leading-zero)
             (format "%02d:%02d" hours minutes))
            (_
             (format "%d:%02d" hours minutes))))))))

(defun org-agenda-time-of-day-to-ampm (time)
  "Convert TIME of a string like \"13:45\" to an AM/PM style time string."
  (let* ((hour-number (string-to-number (substring time 0 -3)))
         (minute (substring time -2))
         (ampm "am"))
    (cond
     ((equal hour-number 12)
      (setq ampm "pm"))
     ((> hour-number 12)
      (setq ampm "pm")
      (setq hour-number (- hour-number 12))))
    (concat
     (if org-agenda-time-leading-zero
	 (format "%02d" hour-number)
       (format "%02s" (number-to-string hour-number)))
     ":" minute ampm)))

(defun org-agenda-time-of-day-to-ampm-maybe (time)
  "Conditionally convert TIME to AM/PM format.
This is based on `org-agenda-timegrid-use-ampm'."
  (if org-agenda-timegrid-use-ampm
      (org-agenda-time-of-day-to-ampm time)
    time))

(defun org-agenda-get-category-icon (category)
  "Return an image for CATEGORY according to `org-agenda-category-icon-alist'."
  (cl-dolist (entry org-agenda-category-icon-alist)
    (when (string-match-p (car entry) category)
      (if (listp (cadr entry))
	  (cl-return (cadr entry))
	(cl-return (apply #'create-image (cdr entry)))))))

(cl-defun org-agenda-insert-block ( block-type insert-body-function
                                    &key suggested-buffer-name
                                    redo-command
                                    block-args
                                    block-header)
  "Insert an agenda block at point using INSERT-BODY-FUNCTION.

INSERT-BODY-FUNCTION is a function called with no arguments to insert
the main block body.

SUGGESTED-BUFFER-NAME is the name (string) to be used when agenda is
sticky (`org-agenda-sticky') and the inserted block is the only block
in the agenda.  The name can also be a cons cell of a form
 (COMMAND-OR-KEY . MATCH).  Then, the sticky buffer name will be set
to either \"*Org Agenda(COMMAND-OR-KEY:MATCH)*\" or \"*Org
Agenda(COMMAND-OR-KEY)*\" (if MATCH is nil or empty string).
When `org-keys' or `org-match' are non-nil non-empty, they will override
the provided COMMAND-OR-KEY and/or MATCH.

BLOCK-TYPE is the block type as a symbol.  This symbol is used to
refer to the block in `org-agenda-prefix-format' and in
`org-agenda-sorting-strategy'.

REDO-COMMAND is an sexp that can be used to re-insert the same block.
BLOCK-ARGS is a set of arguments/settings used to build the block.
These settings should be compatible with
`org-agenda-overriding-arguments'.

BLOCK-HEADER is the suggested title of the block.  It will be used
unless overriding header is defined by the user.  The value is either
a string or a cons cell of title string and text property list to be
applied to the inserted header."
  (catch 'exit
    (setq org-agenda-buffer-name
	  (org-agenda--get-buffer-name
           (if (stringp suggested-buffer-name) suggested-buffer-name
             (when (consp suggested-buffer-name)
               (if (seq-empty-p (or org-match (cdr suggested-buffer-name)))
                   (format "*Org Agenda(%s)*"
                           (or org-keys (car suggested-buffer-name)))
                 (format "*Org Agenda(%s:%s)*"
                         (or org-keys (car suggested-buffer-name))
                         (or org-match (cdr suggested-buffer-name))))))))
    (org-agenda-prepare)
    (org-compile-prefix-format block-type)
    (org-set-sorting-strategy block-type)
    (let ((origin (point))
          (default-header (if (stringp block-header)
                              block-header
                            (or (car-safe block-header) "")))
          (props (cdr-safe block-header)))
      (unless org-agenda-compact-blocks
        (org-agenda--insert-overriding-header default-header))
      (when (> (point) origin) ; we actually inserted something
        (when props (add-text-properties origin (1- (point)) props))
	(org-agenda-mark-header-line origin)))
    (setq org-agenda-redo-command redo-command)
    (funcall insert-body-function)
    (add-text-properties
     (point-min) (point-max)
     `( org-agenda-type ,block-type
        org-last-args ,block-args
        org-redo-cmd ,redo-command
        org-series-cmd ,org-cmd))
    (goto-char (point-min))
    (or org-agenda-multi (org-agenda-fit-window-to-buffer))
    (org-agenda-finalize)
    (setq buffer-read-only t)))

(provide 'org-agenda-line-format)

;;; org-agenda-line-format.el ends here


