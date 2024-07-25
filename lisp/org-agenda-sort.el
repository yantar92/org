;;; org-agenda-sort.el --- Sorting entries in Org agenda  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, text
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

;; This file implements sorting and limiting entries in Org agenda
;; buffers.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-agenda-common)
(require 'org-agenda-files)
(require 'org-mode-common)
(require 'org-agenda-window)

(defvar org-agenda-sorting-strategy-selected nil)

(defgroup org-agenda-sorting nil
  "Options concerning sorting in the Org Agenda."
  :tag "Org Agenda Sorting"
  :group 'org-agenda)

(defcustom org-agenda-sorting-strategy
  '((agenda habit-down time-up urgency-down category-keep)
    (todo   urgency-down category-keep)
    (tags   urgency-down category-keep)
    (search category-keep))
  "Sorting structure for the agenda items of a single day.
This is a list of symbols which will be used in sequence to determine
if an entry should be listed before another entry.  The following
symbols are recognized:

time-up            Put entries with time-of-day indications first, early first.
time-down          Put entries with time-of-day indications first, late first.
timestamp-up       Sort by any timestamp, early first.
timestamp-down     Sort by any timestamp, late first.
scheduled-up       Sort by scheduled timestamp, early first.
scheduled-down     Sort by scheduled timestamp, late first.
deadline-up        Sort by deadline timestamp, early first.
deadline-down      Sort by deadline timestamp, late first.
ts-up              Sort by active timestamp, early first.
ts-down            Sort by active timestamp, late first.
tsia-up            Sort by inactive timestamp, early first.
tsia-down          Sort by inactive timestamp, late first.
category-keep      Keep the default order of categories, corresponding to the
		   sequence in `org-agenda-files'.
category-up        Sort alphabetically by category, A-Z.
category-down      Sort alphabetically by category, Z-A.
tag-up             Sort alphabetically by last tag, A-Z.
tag-down           Sort alphabetically by last tag, Z-A.
priority-up        Sort numerically by priority, high priority last.
priority-down      Sort numerically by priority, high priority first.
urgency-up         Sort numerically by urgency, high urgency last.
                   Urgency is calculated based on item's priority,
                   and proximity to scheduled time and deadline.  See
                   info node `(org)Sorting of agenda items' for
                   details.
urgency-down       Sort numerically by urgency, high urgency first.
todo-state-up      Sort by todo state, tasks that are done last.
todo-state-down    Sort by todo state, tasks that are done first.
effort-up          Sort numerically by estimated effort, high effort last.
effort-down        Sort numerically by estimated effort, high effort first.
user-defined-up    Sort according to `org-agenda-cmp-user-defined', high last.
user-defined-down  Sort according to `org-agenda-cmp-user-defined', high first.
habit-up           Put entries that are habits first.
habit-down         Put entries that are habits last.
alpha-up           Sort headlines alphabetically.
alpha-down         Sort headlines alphabetically, reversed.

The different possibilities will be tried in sequence, and testing stops
if one comparison returns a \"not-equal\".  For example,
  (setq org-agenda-sorting-strategy
        \\='(time-up category-keep priority-down))
means: Pull out all entries having a specified time of day and sort them,
in order to make a time schedule for the current day the first thing in the
agenda listing for the day.  Of the entries without a time indication, keep
the grouped in categories, don't sort the categories, but keep them in
the sequence given in `org-agenda-files'.  Within each category sort by
priority.

Leaving out `category-keep' would mean that items will be sorted across
categories by priority.

Instead of a single list, this can also be a set of list for specific
contents, with a context symbol in the car of the list, any of
`agenda', `todo', `tags', `search' for the corresponding agenda views.

Custom commands can bind this variable in the options section."
  :group 'org-agenda-sorting
  :type `(choice
	  (repeat :tag "General" ,org-sorting-choice)
	  (list :tag "Individually"
		(cons (const :tag "Strategy for Weekly/Daily agenda" agenda)
		      (repeat ,org-sorting-choice))
		(cons (const :tag "Strategy for TODO lists" todo)
		      (repeat ,org-sorting-choice))
		(cons (const :tag "Strategy for Tags matches" tags)
		      (repeat ,org-sorting-choice))
		(cons (const :tag "Strategy for search matches" search)
		      (repeat ,org-sorting-choice))))
  :package-version '(Org . "9.7"))

(defcustom org-agenda-cmp-user-defined nil
  "A function to define the comparison `user-defined'.
This function must receive two arguments, agenda entry a and b.
If a>b, return +1.  If a<b, return -1.  If they are equal as seen by
the user comparison, return nil.
When this is defined, you can make `user-defined-up' and `user-defined-down'
part of an agenda sorting strategy."
  :group 'org-agenda-sorting
  :type 'symbol)

(defcustom org-agenda-sort-noeffort-is-high t
  "Non-nil means items without effort estimate are sorted as high effort.
This also applies when filtering an agenda view with respect to the
< or > effort operator.  Then, tasks with no effort defined will be treated
as tasks with high effort.
When nil, such items are sorted as 0 minutes effort."
  :group 'org-agenda-sorting
  :type 'boolean)

(defcustom org-agenda-sort-notime-is-late t
  "Non-nil means items without time are considered late.
This is only relevant for sorting.  When t, items which have no explicit
time like 15:30 will be considered as 99:01, i.e. later than any items which
do have a time.  When nil, the default time is before 0:00.  You can use this
option to decide if the schedule for today should come before or after timeless
agenda entries."
  :group 'org-agenda-sorting
  :type 'boolean)

(defvar org-agenda-before-sorting-filter-function nil
  "Function to be applied to agenda items prior to sorting.
Prior to sorting also means just before they are inserted into the agenda.

To aid sorting, you may revisit the original entries and add more text
properties which will later be used by the sorting functions.

The function should take a string argument, an agenda line.
It has access to the text properties in that line, which contain among
other things, the property `org-hd-marker' that points to the entry
where the line comes from.  Note that not all lines going into the agenda
have this property, only most.

The function should return the modified string.  It is probably best
to ONLY change text properties.

You can also use this function as a filter, by returning nil for lines
you don't want to have in the agenda at all.  For this application, you
could bind the variable in the options section of a custom command.")

(defcustom org-agenda-max-entries nil
  "Maximum number of entries to display in an agenda.
This can be nil (no limit) or an integer or an alist of agenda
types with an associated number of entries to display in this
type."
  :version "24.4"
  :package-version '(Org . "8.0")
  :group 'org-agenda-custom-commands
  :type '(choice (symbol :tag "No limit" nil)
		 (integer :tag "Max number of entries")
		 (repeat
		  (cons (choice :tag "Agenda type"
				(const agenda)
				(const todo)
				(const tags)
				(const search))
			(integer :tag "Max number of entries")))))

(defcustom org-agenda-max-todos nil
  "Maximum number of TODOs to display in an agenda.
This can be nil (no limit) or an integer or an alist of agenda
types with an associated number of entries to display in this
type."
  :version "24.4"
  :package-version '(Org . "8.0")
  :group 'org-agenda-custom-commands
  :type '(choice (symbol :tag "No limit" nil)
		 (integer :tag "Max number of TODOs")
		 (repeat
		  (cons (choice :tag "Agenda type"
				(const agenda)
				(const todo)
				(const tags)
				(const search))
			(integer :tag "Max number of TODOs")))))

(defcustom org-agenda-max-tags nil
  "Maximum number of tagged entries to display in an agenda.
This can be nil (no limit) or an integer or an alist of agenda
types with an associated number of entries to display in this
type."
  :version "24.4"
  :package-version '(Org . "8.0")
  :group 'org-agenda-custom-commands
  :type '(choice (symbol :tag "No limit" nil)
		 (integer :tag "Max number of tagged entries")
		 (repeat
		  (cons (choice :tag "Agenda type"
				(const agenda)
				(const todo)
				(const tags)
				(const search))
			(integer :tag "Max number of tagged entries")))))

(defcustom org-agenda-max-effort nil
  "Maximum cumulated effort duration for the agenda.
This can be nil (no limit) or a number of minutes (as an integer)
or an alist of agenda types with an associated number of minutes
to limit entries to in this type."
  :version "24.4"
  :package-version '(Org . "8.0")
  :group 'org-agenda-custom-commands
  :type '(choice (symbol :tag "No limit" nil)
		 (integer :tag "Max number of minutes")
		 (repeat
		  (cons (choice :tag "Agenda type"
				(const agenda)
				(const todo)
				(const tags)
				(const search))
			(integer :tag "Max number of minutes")))))

;;; Setting sorting strategy for current agenda

(defun org-set-sorting-strategy (key)
  (setq org-agenda-sorting-strategy-selected
        (if (symbolp (car org-agenda-sorting-strategy))
            ;; the old format
            org-agenda-sorting-strategy
	  (or (cdr (assq key org-agenda-sorting-strategy))
	      (cdr (assq 'agenda org-agenda-sorting-strategy))
	      '(time-up category-keep urgency-down)))))

;;; Limiting and ordering agenda entries

(defun org-agenda-sort-entries (entries)
  "Sort ENTRIES list according.  Return the sorted list."
  (when org-agenda-before-sorting-filter-function
    (setq entries (delq nil (mapcar org-agenda-before-sorting-filter-function entries))))
  (mapcar #'identity (sort entries #'org-entries-lessp)))

(defun org-agenda-apply-limits (entries &optional agenda-type)
  "Limit ENTRIES for the agenda TYPE.  Return filtered out entries.
The ENTRIES are dropped/retained according to `org-agenda-max-todos',
`org-agenda-max-effort', `org-agenda-max-tags', and
`org-agenda-max-entries'."
  (let ((max-effort (cond ((listp org-agenda-max-effort)
			   (cdr (assoc agenda-type org-agenda-max-effort)))
			  (t org-agenda-max-effort)))
	(max-todo (cond ((listp org-agenda-max-todos)
			 (cdr (assoc agenda-type org-agenda-max-todos)))
			(t org-agenda-max-todos)))
	(max-tags (cond ((listp org-agenda-max-tags)
			 (cdr (assoc agenda-type org-agenda-max-tags)))
			(t org-agenda-max-tags)))
	(max-entries (cond ((listp org-agenda-max-entries)
			    (cdr (assoc agenda-type org-agenda-max-entries)))
			   (t org-agenda-max-entries))))
    (when max-effort
      (setq entries
            (org-agenda-limit-entries
	     entries 'effort-minutes max-effort
	     (lambda (e) (or e (if org-agenda-sort-noeffort-is-high
			      32767 -1))))))
    (when max-todo
      (setq entries (org-agenda-limit-entries entries 'todo-state max-todo)))
    (when max-tags
      (setq entries (org-agenda-limit-entries entries 'tags max-tags)))
    (when max-entries
      (setq entries (org-agenda-limit-entries entries 'org-hd-marker max-entries)))
    entries))

(defun org-agenda-limit-entries (list prop limit &optional fn)
  "Limit the number of agenda entries in LIST."
  (let ((include (and limit (< limit 0))))
    (if limit
	(let ((fun (or fn (lambda (p) (when p 1))))
	      (lim 0))
	  (delq nil
		(mapcar
		 (lambda (e)
		   (let ((pval (funcall
				fun (get-text-property (1- (length e))
						       prop e))))
		     (when pval (setq lim (+ lim pval)))
		     (cond ((and pval (<= lim (abs limit))) e)
			   ((and include (not pval)) e))))
		 list)))
      list)))

;;; Pre-defined sorting comparators

(defsubst org-cmp-values (a b property)
  "Compare the numeric value of text PROPERTY for string A and B."
  (let ((pa (or (get-text-property (1- (length a)) property a) 0))
	(pb (or (get-text-property (1- (length b)) property b) 0)))
    (cond ((> pa pb) +1)
	  ((< pa pb) -1))))

(defsubst org-cmp-effort (a b)
  "Compare the effort values of string A and B."
  (let* ((def (if org-agenda-sort-noeffort-is-high 32767 -1))
	 ;; `effort-minutes' property is not directly accessible from
	 ;; the strings, but is stored as a property in `txt'.
	 (ea (or
              (get-text-property 0 'effort-minutes a)
              ;; FIXME: Storing effort data in TXT is obsolete.
              (get-text-property
	       0 'effort-minutes (get-text-property 0 'txt a))
	      def))
	 (eb (or
              (get-text-property 0 'effort-minutes b)
              ;; FIXME: Storing effort data in TXT is obsolete.
              (get-text-property
	       0 'effort-minutes (get-text-property 0 'txt b))
	      def)))
    (cond ((> ea eb) +1)
	  ((< ea eb) -1))))

(defsubst org-cmp-category (a b)
  "Compare the string values of categories of strings A and B."
  (let ((ca (or (get-text-property (1- (length a)) 'org-category a) ""))
	(cb (or (get-text-property (1- (length b)) 'org-category b) "")))
    (cond ((org-string< ca cb) -1)
	  ((org-string< cb ca) +1))))

(defsubst org-cmp-todo-state (a b)
  "Compare the todo states of strings A and B."
  (let* ((ma (or (get-text-property 1 'org-marker a)
		 (get-text-property 1 'org-hd-marker a)))
	 (mb (or (get-text-property 1 'org-marker b)
		 (get-text-property 1 'org-hd-marker b)))
	 (ta (or (get-text-property 1 'todo-state a) ""))
	 (tb (or (get-text-property 1 'todo-state b) ""))
	 (la (- (length (member ta (org-element-all-todo-keywords ma)))))
	 (lb (- (length (member tb (org-element-all-todo-keywords mb)))))
	 (donepa (org-element-keyword-done-p ta ma))
	 (donepb (org-element-keyword-done-p tb mb)))
    (cond ((and donepa (not donepb)) -1)
	  ((and (not donepa) donepb) +1)
	  ((< la lb) -1)
	  ((< lb la) +1))))

(defsubst org-cmp-alpha (a b)
  "Compare the headlines, alphabetically."
  (let* ((pla (text-property-any 0 (length a) 'org-heading t a))
	 (plb (text-property-any 0 (length b) 'org-heading t b))
	 (ta (and pla (substring a pla)))
	 (tb (and plb (substring b plb)))
	 (case-fold-search nil))
    (when pla
      (when (string-match (concat "\\`[ \t]*" (or (get-text-property 0 'org-todo-regexp a) "")
				  "\\([ \t]*\\[[a-zA-Z0-9]\\]\\)? *")
			  ta)
	(setq ta (substring ta (match-end 0))))
      (setq ta (downcase ta)))
    (when plb
      (when (string-match (concat "\\`[ \t]*" (or (get-text-property 0 'org-todo-regexp b) "")
				  "\\([ \t]*\\[[a-zA-Z0-9]\\]\\)? *")
			  tb)
	(setq tb (substring tb (match-end 0))))
      (setq tb (downcase tb)))
    (cond ((not (or ta tb)) nil)
	  ((not ta) +1)
	  ((not tb) -1)
	  ((org-string< ta tb) -1)
	  ((org-string< tb ta) +1))))

(defsubst org-cmp-tag (a b)
  "Compare the string values of the first tags of A and B."
  (require 'org-tags)
  (defvar org-tags-sort-function)
  (let ((ta (car (last (get-text-property 1 'tags a))))
	(tb (car (last (get-text-property 1 'tags b)))))
    (cond ((not (or ta tb)) nil)
	  ((not ta) +1)
	  ((not tb) -1)
	  ((funcall (or org-tags-sort-function #'org-string<) ta tb) -1)
	  ((funcall (or org-tags-sort-function #'org-string<) tb ta) +1))))

(defsubst org-cmp-time (a b)
  "Compare the time-of-day values of strings A and B."
  (let* ((def (if org-agenda-sort-notime-is-late 9901 -1))
	 (ta (or (get-text-property 1 'time-of-day a) def))
	 (tb (or (get-text-property 1 'time-of-day b) def)))
    (cond ((< ta tb) -1)
	  ((< tb ta) +1))))

(defsubst org-cmp-ts (a b type)
  "Compare the timestamps values of entries A and B.
When TYPE is \"scheduled\", \"deadline\", \"timestamp\" or
\"timestamp_ia\", compare within each of these type.  When TYPE
is the empty string, compare all timestamps without respect of
their type."
  (let* ((def (if org-agenda-sort-notime-is-late 99999999 -1))
	 (ta (or (and (string-match type (or (get-text-property 1 'type a) ""))
		      (get-text-property 1 'ts-date a))
		 def))
	 (tb (or (and (string-match type (or (get-text-property 1 'type b) ""))
		      (get-text-property 1 'ts-date b))
		 def)))
    (cond ((if ta (and tb (< ta tb)) tb) -1)
	  ((if tb (and ta (< tb ta)) ta) +1))))

(defsubst org-cmp-habit-p (a b)
  "Compare the todo states of strings A and B."
  (let ((ha (get-text-property 1 'org-habit-p a))
	(hb (get-text-property 1 'org-habit-p b)))
    (cond ((and ha (not hb)) -1)
	  ((and (not ha) hb) +1))))

(defun org-entries-lessp (a b)
  "Predicate for sorting agenda entries."
  ;; The following variables will be used when the form is evaluated.
  ;; So even though the compiler complains, keep them.
  (let ((ss org-agenda-sorting-strategy-selected))
    (org-dlet
	((timestamp-up    (and (org-em 'timestamp-up 'timestamp-down ss)
			       (org-cmp-ts a b "")))
	 (timestamp-down  (if timestamp-up (- timestamp-up) nil))
	 (scheduled-up    (and (org-em 'scheduled-up 'scheduled-down ss)
			       (org-cmp-ts a b "scheduled")))
	 (scheduled-down  (if scheduled-up (- scheduled-up) nil))
	 (deadline-up     (and (org-em 'deadline-up 'deadline-down ss)
			       (org-cmp-ts a b "deadline")))
	 (deadline-down   (if deadline-up (- deadline-up) nil))
	 (tsia-up         (and (org-em 'tsia-up 'tsia-down ss)
			       (org-cmp-ts a b "timestamp_ia")))
	 (tsia-down       (if tsia-up (- tsia-up) nil))
	 (ts-up           (and (org-em 'ts-up 'ts-down ss)
			       (org-cmp-ts a b "timestamp")))
	 (ts-down         (if ts-up (- ts-up) nil))
	 (time-up         (and (org-em 'time-up 'time-down ss)
			       (org-cmp-time a b)))
	 (time-down       (if time-up (- time-up) nil))
	 (stats-up        (and (org-em 'stats-up 'stats-down ss)
			       (org-cmp-values a b 'org-stats)))
	 (stats-down      (if stats-up (- stats-up) nil))
	 (priority-up     (and (org-em 'priority-up 'priority-down ss)
			       (org-cmp-values a b 'priority)))
	 (priority-down   (if priority-up (- priority-up) nil))
	 (urgency-up     (and (org-em 'urgency-up 'urgency-down ss)
			      (org-cmp-values a b 'urgency)))
	 (urgency-down   (if urgency-up (- urgency-up) nil))
	 (effort-up       (and (org-em 'effort-up 'effort-down ss)
			       (org-cmp-effort a b)))
	 (effort-down     (if effort-up (- effort-up) nil))
	 (category-up     (and (or (org-em 'category-up 'category-down ss)
				   (memq 'category-keep ss))
			       (org-cmp-category a b)))
	 (category-down   (if category-up (- category-up) nil))
	 (category-keep   (if category-up +1 nil))
	 (tag-up          (and (org-em 'tag-up 'tag-down ss)
			       (org-cmp-tag a b)))
	 (tag-down        (if tag-up (- tag-up) nil))
	 (todo-state-up   (and (org-em 'todo-state-up 'todo-state-down ss)
			       (org-cmp-todo-state a b)))
	 (todo-state-down (if todo-state-up (- todo-state-up) nil))
	 (habit-up        (and (org-em 'habit-up 'habit-down ss)
			       (org-cmp-habit-p a b)))
	 (habit-down      (if habit-up (- habit-up) nil))
	 (alpha-up        (and (org-em 'alpha-up 'alpha-down ss)
			       (org-cmp-alpha a b)))
	 (alpha-down      (if alpha-up (- alpha-up) nil))
	 (need-user-cmp   (org-em 'user-defined-up 'user-defined-down ss))
	 user-defined-up user-defined-down)
      (when (and need-user-cmp org-agenda-cmp-user-defined
	         (functionp org-agenda-cmp-user-defined))
	(setq user-defined-up
	      (funcall org-agenda-cmp-user-defined a b)
	      user-defined-down (if user-defined-up (- user-defined-up) nil)))
      (cdr (assoc
	    (eval (cons 'or org-agenda-sorting-strategy-selected) t)
	    '((-1 . t) (1 . nil) (nil . nil)))))))

;;; Set limits interactively

(declare-function org-agenda-redo "org-agenda-mode" ())
(defun org-agenda-limit-interactively (remove)
  "In agenda, interactively limit entries to various maximums."
  (interactive "P")
  (if remove
      (progn (setq org-agenda-max-entries nil
		   org-agenda-max-todos nil
		   org-agenda-max-tags nil
		   org-agenda-max-effort nil)
             (require 'org-agenda-mode)
	     (org-agenda-redo))
    (let* ((max (read-char "Number of [e]ntries [t]odos [T]ags [E]ffort? "))
	   (msg (cond ((= max ?E) "How many minutes? ")
		      ((= max ?e) "How many entries? ")
		      ((= max ?t) "How many TODO entries? ")
		      ((= max ?T) "How many tagged entries? ")
		      (t (user-error "Wrong input"))))
	   (num (string-to-number (read-from-minibuffer msg))))
      (cond ((equal max ?e)
	     (let ((org-agenda-max-entries num)) (org-agenda-redo)))
	    ((equal max ?t)
	     (let ((org-agenda-max-todos num)) (org-agenda-redo)))
	    ((equal max ?T)
	     (let ((org-agenda-max-tags num)) (org-agenda-redo)))
	    ((equal max ?E)
	     (let ((org-agenda-max-effort num)) (org-agenda-redo))))))
  (org-agenda-fit-window-to-buffer))

(provide 'org-agenda-sort)

;;; org-agenda-sort.el ends here
