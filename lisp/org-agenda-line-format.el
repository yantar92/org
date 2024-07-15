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
(require 'org-property)

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
	   new time)
      (when (and (member 'require-timed req) (not have))
	;; don't show empty grid
	(throw 'exit list))
      (while (setq time (pop gridtimes))
	(unless (and remove (member time have))
	  (setq time (replace-regexp-in-string " " "0" (format "%04s" time)))
	  (push
           (org-agenda-format-line
            string
            :dotime (concat (substring time 0 -2) ":" (substring time -2))
            :default-duration nil)
	   new)
	  (put-text-property
	   2 (length (car new)) 'face 'org-time-grid (car new))))
      (when (and todayp org-agenda-show-current-time-in-grid)
	(push
         (org-agenda-format-line
          org-agenda-current-time-string
          :dotime (format-time-string "%H:%M "))
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

;; FIXME: What if we have more than one item in the selected sorting strategy?
(defun org-agenda-entry-get-agenda-timestamp (&optional epom)
  "Retrieve timestamp information for sorting agenda views.
Given an element, point, or marker EPOM, returns a cons cell of the
timestamp and the timestamp type relevant for the sorting strategy in
`org-agenda-sorting-strategy-selected'."
  (let (ts ts-date-type)
    (save-match-data
      (cond ((org-em 'scheduled-up 'scheduled-down
		     org-agenda-sorting-strategy-selected)
	     (setq ts (org-entry-get epom "SCHEDULED")
		   ts-date-type " scheduled"))
	    ((org-em 'deadline-up 'deadline-down
		     org-agenda-sorting-strategy-selected)
	     (setq ts (org-entry-get epom "DEADLINE")
		   ts-date-type " deadline"))
	    ((org-em 'ts-up 'ts-down
		     org-agenda-sorting-strategy-selected)
	     (setq ts (org-entry-get epom "TIMESTAMP")
		   ts-date-type " timestamp"))
	    ((org-em 'tsia-up 'tsia-down
		     org-agenda-sorting-strategy-selected)
	     (setq ts (org-entry-get epom "TIMESTAMP_IA")
		   ts-date-type " timestamp_ia"))
	    ((org-em 'timestamp-up 'timestamp-down
		     org-agenda-sorting-strategy-selected)
	     (setq ts (or (org-entry-get epom "SCHEDULED")
			  (org-entry-get epom "DEADLINE")
			  (org-entry-get epom "TIMESTAMP")
			  (org-entry-get epom "TIMESTAMP_IA"))
		   ts-date-type ""))
	    (t (setq ts-date-type "")))
      (cons (when ts (ignore-errors (org-time-string-to-absolute ts)))
	    ts-date-type))))

(cl-defun org-agenda-format-heading
    ( &optional epom
      &key
      scheduling-info remove-re
      dotime (default-duration org-agenda-default-appointment-duration)
      overriding-headline)
  "Format Org heading at point or EPOM for insertion into agenda buffer.
EPOM is an element, point, or marker.

When DOTIME is non-nil, it should define time associated with the
heading.  It can be a timestamp object or a time string containing an
active timestamp or time matching `org-plain-time-of-day-regexp'.  It
can also be another non-nil value - the time will then be derived from
the heading title, but only when `org-agenda-search-headline-for-time'
is non-nil.

When the time associated with heading does not contain a time range,
use DEFAULT-DURATION (equal to
`org-agenda-default-appointment-duration' by default) to compute the
appointment duration.

SCHEDULING-INFO is a string describing scheduling information.  It
will be used to replace %s placeholder in `org-agenda-prefix-format',
which see.

REMOVE-RE, when non-nil, is a regular expression.  All its matches will
be removed from heading and its tags line in the returned string.

OVERRIDING-HEADLINE is the headline to be used instead of actual
headline at EPOM.

Return a string suitable for insertion into agenda, with approprtiate
text properties applied.

The string is constructed according to the headline and
`org-agenda-prefix-format'.

The following text properties are applied to the returned string:
1.  hd-marker       :: Marker pointing to the begining of the headline
2.  marker          :: EPOM, if it is a marker, or hd-marker
3.  org-category    :: CATEGORY property of the heading
4.  effort          :: EFFORT property of the heading
5.  effort-minutes  :: EFFORT duration, as a number of minutes
6.  priority        :: Heading priority, as returned by `org-get-priority'
7.  urgency         :: =priority (may be overridden by the caller)
                       Used to sort agenda items by urgency.  See
                       `org-agenda-sorting-strategy'
8.  todo-state     :: Todo keyword of the heading
9.  tags           :: List of tags for the heading
                      May or may not include inherited tags depending
                      on `org-agenda-show-inherited-tags'
10. warntime       :: APPT_WARNING property of the heading
11. ts-date        :: Timestamp string as returned by
                      `org-agenda-entry-get-agenda-timestamp'
                      Used by agenda to sort items
12. type           :: ts-date type as returned by
                      `org-agenda-entry-get-agenda-timestamp'
                      Used by agenda to sort items
13. done-face      :: =org-agenda-done
                      Face to be used in agneda when entry is
                      considered done
14. undone-face    :: =default
                      Face to be used in agenda when entry is not done
15. face           :: =default
16. mouse-face     :: ='highlight
17. help-echo      :: The default value hints user that RET will jump
                      to the containing Org file
18. Additional properties assigned by `org-agenda-format-line', which
see."
  (org-with-point-at epom
    (let ((marker (when (number-or-marker-p epom) (copy-marker epom)))
          (heading (org-headline-at-point epom))
          (hd-marker (org-agenda-new-marker)))
      (unless heading (error "org-agenda-format-heading: There is no heading at %S" epom))
      ;; Get heading text right from inside the Org buffer to retain fontification.
      (skip-chars-forward "* ")
      (let* ((heading-string
              (or overriding-headline
                  (org-buffer-substring-fontified (point) (line-end-position))))
             (category (org-get-category heading))
             (effort (org-entry-get heading org-effort-property))
             (effort-minutes (when effort (org-duration-to-minutes effort)))
             (agenda-use-inherited?
              (or (eq org-agenda-show-inherited-tags 'always)
	          (and (listp org-agenda-show-inherited-tags)
		       (memq 'todo org-agenda-show-inherited-tags))
	          (and (eq org-agenda-show-inherited-tags t)
		       (or (eq org-agenda-use-tag-inheritance t)
		           (memq 'todo org-agenda-use-tag-inheritance)))))
             (tags (org-get-tags heading (not agenda-use-inherited?)))
             (level-indent (make-string (org-element-property :level heading) ? ))
             (priority (org-get-priority heading-string))
             (urgency priority)
             (todo-state (org-get-todo-state heading))
             (warntime (org-entry-get heading "APPT_WARNTIME" 'selective))
             (ts-date-pair (org-agenda-entry-get-agenda-timestamp heading)))

        ;; Cleanup tags in the heading
        (setq heading-string
              (org-agenda-fix-displayed-tags
	       heading-string tags
	       org-agenda-show-inherited-tags
	       org-agenda-hide-tags-regexp))

        (org-add-props
            (org-agenda-format-line
             heading-string
             :scheduling-info scheduling-info
             :effort effort :level level-indent
             :dotime dotime
             :remove-re remove-re
             :category category
             :default-duration default-duration)
            `( org-marker ,(or marker hd-marker)
               org-hd-marker ,hd-marker
               org-category ,category
               effort ,effort
               effort-minutes ,effort-minutes
               priority ,priority
               urgency ,urgency
               todo-state ,todo-state
               tags ,tags
               warntime ,warntime
               ts-date ,(car ts-date-pair)
               type ,(cdr ts-date-pair)
               ;; Default faces
               face 'default
	       done-face 'org-agenda-done
	       undone-face 'default
	       mouse-face 'highlight
	       help-echo
	       ,(format "mouse-2 or RET jump to Org file %S"
		        (abbreviate-file-name
			 (or (buffer-file-name (buffer-base-buffer))
			     (buffer-name (buffer-base-buffer)))))
               ;; FIXME: Obsolete properties
               org-not-done-regexp ,(org-not-done-regexp epom)
	       org-todo-regexp ,(org-todo-regexp epom)
	       org-complex-heading-regexp ,(org-complex-heading-regexp epom)))))))

(defun org-agenda--format-time (start-time &optional end-time)
  "Format %t placeholder in `org-agenda-prefix-format'.
Return a string to be used in place fo %t for START-TIME..END-TIME
time range.  START-TIME and optional END-TIME are time strings passed
to `org-get-time-of-day'.

The exact format of the %t placeholder is affected by
`org-agenda-time-leading-zero', and `org-agenda-timegrid-use-ampm'.
The start time will be forced to fit within 5 characters.  The times
after midnight will be converted into +H:MM past midnight (see
`overtime' value of STRING argument in `org-get-time-of-day').

The returned string will have the following form:
START-END <Trailing string according to `org-agenda-time-grid'>"
  (when start-time
    (setq start-time (format "%5s" (org-get-time-of-day start-time 'overtime))))
  (when end-time
    (setq end-time (org-get-time-of-day end-time 'overtime)))
  (let ((time-grid-trailing-characters (nth 2 org-agenda-time-grid)))
    (cond (end-time
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
	  (t ""))))

(defun org-agenda--find-time-in-string (string)
  "Extract time range from STRING.
STRING will be searched for the first active timestamp or time
matching `org-plain-time-of-day-regexp'.

Return a list (MATCH START-TIME END-TIME), where MATCH is the matched
time range in STRING, and START-TIME/END-TIME are strings representing
time of day.  Return nil when no time range is found in STRING."
  (let ( timestamp-range? plain-time? date-range-same-day?
         time-string start-time end-time)
    (when (or (setq timestamp-range?
                    (string-match org-stamp-time-of-day-regexp string))
	      (setq plain-time?
                    (string-match org-plain-time-of-day-regexp string)))
      (setq time-string (match-string 0 string)
	    date-range-same-day? (and timestamp-range? (match-end 3))
	    start-time (match-string (if plain-time? 1 2) string)
	    end-time (match-string (if plain-time? 8
                                     (if date-range-same-day? 4 6))
                                   string))
      (list time-string start-time end-time))))

(defun org-agenda--format-breadcrumbs (marker &optional width)
  "Return breadcrumbs string at MARKER.
Use `org-agenda-breadcrumbs-separator', which see.
WIDTH is the maximum width of the outline path (see the docstring of
`org-format-outline-path'.  When WIDTH is nil, use `frame-width'."
  (org-with-point-at marker
    (let ((s (if (derived-mode-p 'org-mode)
                 (org-format-outline-path
                  (org-get-outline-path)
		  (or width (1- (frame-width)))
		  nil org-agenda-breadcrumbs-separator)
               ;; Not in Org buffer.  This can happen,
               ;; for example, in
               ;; `org-agenda-add-time-grid-maybe' where
               ;; time grid does not correspond to a
               ;; particular heading.
               "")))
      (if (equal "" s) "" (concat s org-agenda-breadcrumbs-separator)))))

(defun org-agenda--format-category (category length)
  "Format CATEGORY string for agenda prefix format.
Make sure that the resulting category has at least LENGTH width.
Honor `org-prefix-category-max-length'."
  (if (string-match org-link-bracket-re category)
      (let ((link-width (string-width (or (match-string 2) (match-string 1)))))
	(when (< link-width (or length 0))
	  (setq category (copy-sequence category))
	  (org-add-props category nil
            ;; Used by agenda prefix formatter.  See `org-compile-prefix-format'.
	    'extra-space (make-string
			  (- org-prefix-category-length link-width 1) ?\ ))))
    (when (and org-prefix-category-max-length
	       (>= (length category) org-prefix-category-max-length))
      (setq category (substring category 0 (1- org-prefix-category-max-length))))
    category))

(cl-defun org-agenda-format-line
    (agenda-heading
     &key
     (breadcrumbs "") (effort "") (scheduling-info "") (level "")
     remove-re dotime
     (default-duration org-agenda-default-appointment-duration)
     (category "")
     (prefix-format-compiled org-prefix-format-compiled))
  "Add agenda prefix to AGENDA-HEADING.
The prefix is formatted according to `org-prefix-format-compiled'.
CATEGORY (%c, %i), EFFORT (%e), SCHEDULING-INFO (%s), and LEVEL (%l)
 (all strings) will be used to replace the corresponding placeholders
in the prefix format (see `org-agenda-prefix-format' for more info.

If AGENDA-HEADING contains substring matching `org-tag-group-re', it
will be used to retrieve %T placeholder value in the agenda prefix.

BREADCRUMBS (%b) should be a string used to replace %b placeholder or
a marker in Org buffer to build breadcrumbs string.

When DOTIME is non-nil, it should define time associated with the
heading.  It can be a timestamp object or a time string containing an
active timestamp or time matching `org-plain-time-of-day-regexp'.  It
can also be another non-nil value - the time will then be derived from
AGENDA-HEADING, but only when `org-agenda-search-headline-for-time'
is non-nil.

The %t placeholder in `org-agenda-prefix-format' will be derived
from the time associated with the heading:
1. When no time is associated, it will be empty
2. When the time contains a time range, this time range will be used
   to format %t placeholder, dropping all the date info and leaving
   just the time of the day specification.
3. When the time only defines a single time point (no ending time),
   the %t placeholder will be formatted depending on the value of
   DEFAULT-DURATION (`org-agenda-default-appointment-duration' when
   not set)
   - When DEFAULT-DURATION is a number, the time range will be
     constructed starting from the agenda heading time up to
     +DEFAULT-DURATION minutes in future
   - Otherwise, only starting time will be assigned.

The exact format of the %t placeholder is defined by
`org-agenda--format-time', which see.  The following custom options
affect the format: `org-agenda-time-leading-zero',
`org-agenda-timegrid-use-ampm', `org-agenda-time-grid'.

The return value is a string of the form
<PREFIX><MODIFIED-AGENDA-HEADING><SPACES><TAGS>.

<SPACES><TAGS> together will occupy at least 50 characters.
TAGS will be extracted from the original AGENDA-HEADING.

The return value is constructed as the following:

1. AGENDA-HEADING is trimmed from trailing and leading whitespace.

2. AGENDA-HEADING is modified avoid duplicating entries in <PREFIX>
   and the rest of the line.  The modification is controlled by
   `org-agenda-remove-times-when-in-prefix', and
   `org-agenda-remove-tags', which see.

3. When REMOVE-RE is non-nil, the matching text is removed from
   <AGENDA-HEADING><TAGS> (but not from <PREFIX>).

The return string will have the following properties assigned to the
whole string:
1. org-redo-line-fun  :: =org-agenda-format-line
                          Function name used to build this agenda line.
2. org-redo-line-args :: Arguments passed to `org-agenda-format-line'
3. time-of-day        :: Time of the day associated with this agenda line
4. duration           :: Duration associated with this agenda line

In addition, everything past <PREFIX> will be marked by non-nil
org-heading property."
  ;; We keep the org-prefix-* variable values along with a compiled
  ;; formatter, so that multiple agendas existing at the same time do
  ;; not step on each other toes.
  ;;
  ;; It was inconvenient to make these variables buffer local in
  ;; Agenda buffers, because this function expects to be called with
  ;; the buffer where item comes from being current, and not agenda
  ;; buffer

  ;; FIXME: This does not belong here.
  (and (derived-mode-p 'org-mode) buffer-file-name
       (add-to-list 'org-agenda-contributing-files buffer-file-name))

  (let* ((bindings (car prefix-format-compiled))
	 (formatter (cadr prefix-format-compiled))
         (orig-args (list agenda-heading
                          :breadcrumbs breadcrumbs
                          :effort effort
                          :scheduling-info scheduling-info
                          :level level
                          :remove-re remove-re
                          :dotime dotime
                          :default-duration default-duration
                          :category category
                          :prefix-format-compiled prefix-format-compiled)))
    (cl-loop for (var value) in bindings
	     do (set var value))
    (let* ((breadcrumbs
            (when org-prefix-has-breadcrumbs
              (cond
               ((stringp breadcrumbs) breadcrumbs)
               ((markerp breadcrumbs) (org-agenda--format-breadcrumbs breadcrumbs))
               (t (error "org-agenda-format-line: Invalid breadcrumbs parameter %S"
                         breadcrumbs)))))
           (tags (if (string-match org-tag-group-re agenda-heading)
                     (match-string 1 agenda-heading) ""))
           (agenda-heading (if (not (seq-empty-p tags)) (replace-match "" nil nil agenda-heading)
                             agenda-heading))
           ;; Used by FORMATTER
           (tag (or (org-last (split-string tags ":" t)) ""))
           military-time time duration)

      ;; Diary entries sometimes have extra whitespace at the beginning
      (setq agenda-heading (org-trim agenda-heading))
      (setq tags (org-trim tags))

      ;; Maybe find time range associated with the heading.
      (when dotime
        (pcase-let
            ((`(,matched-time ,start-time ,end-time)
              (org-agenda--find-time-in-string
               (concat
	        (cond
                 ((stringp dotime) dotime)
                 ((org-element-type-p dotime 'timestamp)
                  (org-element-interpret-data dotime)))
	        (and org-agenda-search-headline-for-time
                     ;; Do not search inside timestamps.  They are
                     ;; handled separately.
                     (replace-regexp-in-string
                      org-ts-regexp-both ""
                      agenda-heading))))))
          (when matched-time
            (setq military-time (org-get-time-of-day matched-time))
            ;; If the times are in AGENDA-HEADING (not in DOTIME), and the
	    ;; prefix will list them, we might want to remove them there
	    ;; to avoid duplication.  The user can turn this off with a
	    ;; variable.
	    (when (and org-prefix-has-time
		       org-agenda-remove-times-when-in-prefix
		       (string-match (concat (regexp-quote matched-time) " *")
                                     agenda-heading)
		       (if (eq org-agenda-remove-times-when-in-prefix 'beg)
			   (= (match-beginning 0) 0)
		         t))
	      (setq agenda-heading (replace-match "" nil nil agenda-heading)))
            ;; Normalize the time(s) to 24 hour.
	    (when start-time (setq start-time (org-get-time-of-day start-time t)))
	    (when end-time (setq end-time (org-get-time-of-day end-time t)))
            ;; Try to set end-time if start-time and
	    ;; DEFAULT-DURATION are set
	    (when (and start-time (not end-time) default-duration)
	      (setq end-time
	            (org-duration-from-minutes
	             (+ (org-duration-to-minutes start-time t) default-duration)
	             nil t)))
            ;; Compute the duration
	    (when end-time
	      (setq duration (- (org-duration-to-minutes end-time)
			        (org-duration-to-minutes start-time))))
            ;; Used by FORMATTER
            (setq time (org-agenda--format-time start-time end-time)))))

      ;; Maybe clear tags
      (unless (string-empty-p tags)
        ;; Tags are in the string
        (if (or (eq org-agenda-remove-tags t)
	        (and org-agenda-remove-tags
		     org-prefix-has-tag))
	    (setq tags "")
          ;; If not clearing, prepend spaces
          (setq tags (concat
                      (make-string (max (- 50 (length tags)) 1) ?\ )
                      tags))))

      (let ((suffix (concat agenda-heading tags))
            resulting-line)

        (when remove-re
          (while (string-match remove-re suffix)
	    (setq suffix (replace-match "" t t suffix))))

        ;; FIXME: It this text property really necessary?
        ;; Set org-heading property on `suffix' to mark the start of
	;; the heading.
	(add-text-properties 0 (length suffix) '(org-heading t) suffix)

        ;; Dynamically scoped variables used in `org-compile-prefix-format'.
        (org-dlet
            ((breadcrumbs
              (when org-prefix-has-breadcrumbs
                (cond
                 ((stringp breadcrumbs) breadcrumbs)
                 ((markerp breadcrumbs) (org-agenda--format-breadcrumbs breadcrumbs))
                 (t (error "org-agenda-format-line: Invalid breadcrumbs parameter %S"
                           breadcrumbs)))))
             (full-category
              (if (symbolp category) (symbol-name category) category))
             (category
              (org-agenda--format-category
               (if (symbolp category) (symbol-name category) category)
               ;; Bound in `org-prefix-format-compiled'.
               org-prefix-category-length))
             (category-icon
              (if-let ((icon (org-agenda-get-category-icon category)))
                  (propertize " " 'display icon)
                ""))
             (effort effort)
             (extra scheduling-info)
             (level level)
             (tag tag)
             (time time))
          ;; Evaluate the compiled format
          (setq resulting-line (concat (eval formatter t) suffix))

          ;; And finally add the text properties
          (remove-text-properties
           0 (length resulting-line)
           '(line-prefix t wrap-prefix t)
           resulting-line)

          (org-add-props resulting-line nil
            'org-redo-line-fun 'org-agenda-format-line
            'org-redo-line-args orig-args
	    'time-of-day military-time
	    'duration duration
            ;; FIXME: Obsolete properties
	    'org-category full-category
            'tags (split-string (org-trim tags) ":" t)
            'org-priority-highest org-priority-highest
	    'org-priority-lowest org-priority-lowest
	    'breadcrumbs breadcrumbs
	    'txt suffix
	    'level level
            'time time
            'extra extra
            'dotime dotime
            'format prefix-format-compiled))))))

(defun org-agenda-change-all-lines (newhead hdmarker
				            &optional fixface just-this)
  "Change all lines in the agenda buffer which match HDMARKER.
The new content of the line will be NEWHEAD (as modified by
`org-agenda-format-line').  HDMARKER is checked with
`equal' against all `org-hd-marker' text properties in the file.
If FIXFACE is non-nil, the face of each item is modified according to
the new TODO state.
If JUST-THIS is non-nil, change just the current line, not all.
If FORCE-TAGS is non-nil, the car of it returns the new tags."
  (let* ((inhibit-read-only t)
	 (line (org-current-line))
	 (org-agenda-buffer (current-buffer))
	 m finish new)
    (save-excursion
      (goto-char (point-max))
      (forward-line 0)
      (while (not finish)
	(setq finish (bobp))
	(when (and (setq m (org-get-at-bol 'org-hd-marker))
		   (or (not just-this) (= (org-current-line) line))
		   (equal m hdmarker))
	  (setq new
                (let ((fun (org-get-at-bol 'org-redo-line-fun))
                      (args (org-get-at-bol 'org-redo-line-args)))
                  (org-with-point-at hdmarker
                    (apply fun (cons newhead (cdr args))))))
          (setq new
                (org-add-props new
                    (org-combine-plists ; new properties take precedence.
                     (text-properties-at (point))
                     (text-properties-at 0 new))))
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
			 (org-get-at-bol 'undone-face)
                       (org-get-at-bol 'done-face)))))
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


