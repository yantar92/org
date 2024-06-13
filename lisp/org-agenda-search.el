;;; org-agenda-search.el --- Searching information for agenda display         -*- lexical-binding: t; -*-

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
;; This library implements functions collecting information from
;; agenda files for agenda display.

;;; Code:

(require 'org-agenda-common)
(require 'org-agenda-files)
(require 'org-agenda-line-format)
(require 'org-element-context)
(require 'org-property)
(require 'org-tags-core)
(require 'org-time)
(require 'org-agenda-sort)

(defgroup org-agenda-todo-list nil
  "Options concerning the global todo list agenda view."
  :tag "Org Agenda Todo List"
  :group 'org-agenda)
(defgroup org-agenda-match-view nil
  "Options concerning the general tags/property/todo match agenda view."
  :tag "Org Agenda Match View"
  :group 'org-agenda)

;;; FIXME:
;;; Customization that really belongs to org-agenda-line-format, but
;;; used here because of implementation details.

(defcustom org-agenda-inactive-leader "["
  "Text preceding item pulled into the agenda by inactive time stamps.
These entries are added to the agenda when pressing \"[\"."
  :group 'org-agenda-line-format
  :version "24.1"
  :type 'string)

(defcustom org-agenda-diary-sexp-prefix nil
  "A regexp that matches part of a diary sexp entry
which should be treated as scheduling/deadline information in
`org-agenda'.

For example, you can use this to extract the `diary-remind-message' from
`diary-remind' entries."
  :group 'org-agenda-line-format
  :type '(choice (const :tag "None" nil) (regexp :tag "Regexp")))

(defcustom org-agenda-log-mode-add-notes t
  "Non-nil means add first line of notes to log entries in agenda views.
If a log item like a state change or a clock entry is associated with
notes, the first line of these notes will be added to the entry in the
agenda display."
  :group 'org-agenda-daily/weekly
  :group 'org-agenda-line-format
  :type 'boolean)

(defun org-agenda-deadline-face (fraction)
  "Return the face to displaying a deadline item.
FRACTION is what fraction of the head-warning time has passed."
  (require 'org-faces)
  (defvar org-agenda-deadline-faces)
  (assoc-default fraction org-agenda-deadline-faces #'<=))

;;; General agenda search customization

(defcustom org-agenda-log-mode-items '(closed clock)
  "List of items that should be shown in agenda log mode.
\\<org-agenda-mode-map>\
This list may contain the following symbols:

  closed    Show entries that have been closed on that day.
  clock     Show entries that have received clocked time on that day.
  state     Show all logged state changes.
Note that instead of changing this variable, you can also press \
`\\[universal-argument] \\[org-agenda-log-mode]' in
the agenda to display all available LOG items temporarily."
  :group 'org-agenda-daily/weekly
  :type '(set :greedy t (const closed) (const clock) (const state)))

(defvar org-agenda-entry-types '(:deadline :scheduled :timestamp :sexp)
  "List of types searched for when creating the daily/weekly agenda.
This variable is a list of symbols that controls the types of
items that appear in the daily/weekly agenda.  Allowed symbols in this
list are

  :timestamp   List items containing a date stamp or date range matching
               the selected date.  This includes sexp entries in angular
               brackets.

  :sexp        List entries resulting from plain diary-like sexps.

  :deadline    List deadline due on that date.  When the date is today,
               also list any deadlines past due, or due within
	       `org-deadline-warning-days'.

  :deadline*   Same as above, but only include the deadline if it has an
               hour specification as [h]h:mm.

  :scheduled   List all items which are scheduled for the given date.
	       The diary for *today* also contains items which were
	       scheduled earlier and are not yet marked DONE.

  :scheduled*  Same as above, but only include the scheduled item if it
               has an hour specification as [h]h:mm.

By default, all four non-starred types are turned on.

When :scheduled* or :deadline* are included, :schedule or :deadline
will be ignored.

Never set this variable globally using `setq', because then it
will apply to all future agenda commands.  Instead, bind it with
`let' to scope it dynamically into the agenda-constructing
command.  A good way to set it is through options in
`org-agenda-custom-commands'.  For a more flexible (though
somewhat less efficient) way of determining what is included in
the daily/weekly agenda, see `org-agenda-skip-function'.")

;;; Skipping agenda items that match multiple dates

(defcustom org-agenda-show-future-repeats t
  "Non-nil shows repeated entries in the future part of the agenda.
When set to the symbol `next' only the first future repeat is shown."
  :group 'org-agenda-daily/weekly
  :type '(choice
	  (const :tag "Show all repeated entries" t)
	  (const :tag "Show next repeated entry" next)
	  (const :tag "Do not show repeated entries" nil))
  :package-version '(Org . "9.1")
  :safe #'symbolp)

(defcustom org-agenda-prefer-last-repeat nil
  "Non-nil sets date for repeated entries to their last repeat.

When nil, display SCHEDULED and DEADLINE dates at their base
date, and in today's agenda, as a reminder.  Display plain
timestamps, on the other hand, at every repeat date in the past
in addition to the base date.

When non-nil, show a repeated entry at its latest repeat date,
possibly being today even if it wasn't marked as done.  This
setting is useful if you do not always mark repeated entries as
done and, yet, consider that reaching repeat date starts the task
anew.

When set to a list of strings, prefer last repeats only for
entries with these TODO keywords."
  :group 'org-agenda-daily/weekly
  :type '(choice
	  (const :tag "Prefer last repeat" t)
	  (const :tag "Prefer base date" nil)
	  (repeat :tag "Prefer last repeat for entries with these TODO keywords"
		  (string :tag "TODO keyword")))
  :version "26.1"
  :package-version '(Org . "9.1")
  :safe (lambda (x) (or (booleanp x) (consp x))))

(defcustom org-scheduled-past-days 10000
  "Number of days to continue listing scheduled items not marked DONE.
When an item is scheduled on a date, it shows up in the agenda on
this day and will be listed until it is marked done or for the
number of days given here."
  :group 'org-agenda-daily/weekly
  :type 'integer
  :safe 'integerp)

(defcustom org-deadline-past-days 10000
  "Number of days to warn about missed deadlines.
When an item has deadline on a date, it shows up in the agenda on
this day and will appear as a reminder until it is marked DONE or
for the number of days given here."
  :group 'org-agenda-daily/weekly
  :type 'integer
  :version "26.1"
  :package-version '(Org . "9.1")
  :safe 'integerp)

(defcustom org-deadline-warning-days 14
  "Number of days before expiration during which a deadline becomes active.
This variable governs the display in sparse trees and in the agenda.
When 0 or negative, it means use this number (the absolute value of it)
even if a deadline has a different individual lead time specified.

Custom commands can set this variable in the options section."
  :group 'org-time
  :group 'org-agenda-daily/weekly
  :type 'integer)

(defcustom org-scheduled-delay-days 0
  "Number of days before a scheduled item becomes active.
This variable governs the display in sparse trees and in the agenda.
The default value (i.e. 0) means: don't delay scheduled item.
When negative, it means use this number (the absolute value of it)
even if a scheduled item has a different individual delay time
specified.

Custom commands can set this variable in the options section."
  :group 'org-time
  :group 'org-agenda-daily/weekly
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'integer)

;;; Specific agenda skip customizations

(defgroup org-agenda-skip nil
  "Options concerning skipping parts of agenda files."
  :tag "Org Agenda Skip"
  :group 'org-agenda)

(defcustom org-agenda-skip-archived-trees t
  "Non-nil means the agenda will skip any items located in archived trees.
An archived tree is a tree marked with the tag ARCHIVE.  The use of this
variable is no longer recommended, you should leave it at the value t.
Instead, use the key `v' to cycle the archives-mode in the agenda."
  :group 'org-archive
  :group 'org-agenda-skip
  :type 'boolean)

(defcustom org-agenda-skip-comment-trees t
  "Non-nil means skip trees that start with the COMMENT keyword.
When nil, these trees are also scanned by agenda commands."
  :group 'org-agenda-skip
  :type 'boolean)

(defcustom org-agenda-todo-ignore-with-date nil
  "Non-nil means don't show entries with a date in the global todo list.
You can use this if you prefer to mark mere appointments with a TODO keyword,
but don't want them to show up in the TODO list.
When this is set, it also covers deadlines and scheduled items, the settings
of `org-agenda-todo-ignore-scheduled' and `org-agenda-todo-ignore-deadlines'
will be ignored.
See also the variable `org-agenda-tags-todo-honor-ignore-options'."
  :group 'org-agenda-skip
  :group 'org-agenda-todo-list
  :type 'boolean)

(defcustom org-agenda-todo-list-sublevels t
  "Non-nil means check also the sublevels of a TODO entry for TODO entries.
When nil, the sublevels of a TODO entry are not checked, resulting in
potentially much shorter TODO lists."
  :group 'org-agenda-skip
  :group 'org-agenda-todo-list
  :type 'boolean)

(defcustom org-agenda-todo-ignore-timestamp nil
  "Non-nil means don't show entries with a timestamp.
This applies when creating the global todo list.
Valid values are:

past     Don't show entries for today or in the past.

future   Don't show entries with a timestamp in the future.
         The idea behind this is that if it has a future
         timestamp, you don't want to think about it until the
         date.

all      Don't show any entries with a timestamp in the global todo list.
         The idea behind this is that by setting a timestamp, you
         have already \"taken care\" of this item.

This variable can also have an integer as a value.  If positive (N),
todos with a timestamp N or more days in the future will be ignored.  If
negative (-N), todos with a timestamp N or more days in the past will be
ignored.  If 0, todos with a timestamp either today or in the future will
be ignored.  For example, a value of -1 will exclude todos with a
timestamp in the past (yesterday or earlier), while a value of 7 will
exclude todos with a timestamp a week or more in the future.

See also `org-agenda-todo-ignore-with-date'.
See also the variable `org-agenda-tags-todo-honor-ignore-options' if you want
to make his option also apply to the tags-todo list."
  :group 'org-agenda-skip
  :group 'org-agenda-todo-list
  :version "24.1"
  :type '(choice
	  (const :tag "Ignore future timestamp todos" future)
	  (const :tag "Ignore past or present timestamp todos" past)
	  (const :tag "Ignore all timestamp todos" all)
	  (const :tag "Show timestamp todos" nil)
	  (integer :tag "Ignore if N or more days in past(-) or future(+).")))

(defcustom org-agenda-todo-ignore-scheduled nil
  "Non-nil means, ignore some scheduled TODO items when making TODO list.
This applies when creating the global todo list.
Valid values are:

past     Don't show entries scheduled today or in the past.

future   Don't show entries scheduled in the future.
         The idea behind this is that by scheduling it, you don't want to
         think about it until the scheduled date.

all      Don't show any scheduled entries in the global todo list.
         The idea behind this is that by scheduling it, you have already
         \"taken care\" of this item.

t        Same as `all', for backward compatibility.

This variable can also have an integer as a value.  See
`org-agenda-todo-ignore-timestamp' for more details.

See also `org-agenda-todo-ignore-with-date'.
See also the variable `org-agenda-tags-todo-honor-ignore-options' if you want
to make his option also apply to the tags-todo list."
  :group 'org-agenda-skip
  :group 'org-agenda-todo-list
  :type '(choice
	  (const :tag "Ignore future-scheduled todos" future)
	  (const :tag "Ignore past- or present-scheduled todos" past)
	  (const :tag "Ignore all scheduled todos" all)
	  (const :tag "Ignore all scheduled todos (compatibility)" t)
	  (const :tag "Show scheduled todos" nil)
	  (integer :tag "Ignore if N or more days in past(-) or future(+).")))

(defcustom org-agenda-todo-ignore-deadlines nil
  "Non-nil means ignore some deadline TODO items when making TODO list.

There are different motivations for using different values, please think
carefully when configuring this variable.

This applies when creating the global TODO list.

Valid values are:

near    Don't show near deadline entries.  A deadline is near when it is
        closer than `org-deadline-warning-days' days.  The idea behind this
        is that such items will appear in the agenda anyway.

far     Don't show TODO entries where a deadline has been defined, but
        is not going to happen anytime soon.  This is useful if you want to use
        the TODO list to figure out what to do now.

past    Don't show entries with a deadline timestamp for today or in the past.

future  Don't show entries with a deadline timestamp in the future, not even
        when they become `near' ones.  Use it with caution.

all     Ignore all TODO entries that do have a deadline.

t       Same as `near', for backward compatibility.

This variable can also have an integer as a value.  See
`org-agenda-todo-ignore-timestamp' for more details.

See also `org-agenda-todo-ignore-with-date'.
See also the variable `org-agenda-tags-todo-honor-ignore-options' if you want
to make his option also apply to the tags-todo list."
  :group 'org-agenda-skip
  :group 'org-agenda-todo-list
  :type '(choice
	  (const :tag "Ignore near deadlines" near)
	  (const :tag "Ignore near deadlines (compatibility)" t)
	  (const :tag "Ignore far deadlines" far)
	  (const :tag "Ignore all TODOs with a deadlines" all)
	  (const :tag "Show all TODOs, even if they have a deadline" nil)
	  (integer :tag "Ignore if N or more days in past(-) or future(+).")))

(defcustom org-agenda-todo-ignore-time-comparison-use-seconds nil
  "Time unit to use when possibly ignoring an agenda item.

See the docstring of various `org-agenda-todo-ignore-*' options.
The default is to compare time stamps using days.  An item is thus
considered to be in the future if it is at least one day after today.
Non-nil means to compare time stamps using seconds.  An item is then
considered future if it has a time value later than current time."
  :group 'org-agenda-skip
  :group 'org-agenda-todo-list
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Compare time with days" nil)
	  (const :tag "Compare time with seconds" t)))

(defcustom org-agenda-tags-todo-honor-ignore-options nil
  "Non-nil means honor todo-list ignores options also in tags-todo search.
The variables
   `org-agenda-todo-ignore-with-date',
   `org-agenda-todo-ignore-timestamp',
   `org-agenda-todo-ignore-scheduled',
   `org-agenda-todo-ignore-deadlines'
make the global TODO list skip entries that have time stamps of certain
kinds.  If this option is set, the same options will also apply for the
tags-todo search, which is the general tags/property matcher
restricted to unfinished TODO entries only."
  :group 'org-agenda-skip
  :group 'org-agenda-todo-list
  :group 'org-agenda-match-view
  :type 'boolean)

(defcustom org-agenda-skip-scheduled-if-done nil
  "Non-nil means don't show scheduled items in agenda when they are done.
This is relevant for the daily/weekly agenda, not for the TODO list.  It
applies only to the actual date of the scheduling.  Warnings about an item
with a past scheduling dates are always turned off when the item is DONE."
  :group 'org-agenda-skip
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-skip-scheduled-if-deadline-is-shown nil
  "Non-nil means skip scheduling line if same entry shows because of deadline.

In the agenda of today, an entry can show up multiple times
because it is both scheduled and has a nearby deadline, and maybe
a plain time stamp as well.

When this variable is nil, the entry will be shown several times.

When set to t, then only the deadline is shown and the fact that
the entry is scheduled today or was scheduled previously is not
shown.

When set to the symbol `not-today', skip scheduled previously,
but not scheduled today."
  :group 'org-agenda-skip
  :group 'org-agenda-daily/weekly
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Always" t)
	  (const :tag "Not when scheduled today" not-today))
  :package-version '(Org . "9.7"))

(defcustom org-agenda-skip-scheduled-repeats-after-deadline nil
  "Non-nil hides scheduled repeated entries past deadline."
  :group 'org-agenda-daily/weekly
  :type 'boolean
  :package-version '(Org . "9.7")
  :safe t)

(defcustom org-agenda-skip-timestamp-if-deadline-is-shown nil
  "Non-nil means skip timestamp line if same entry shows because of deadline.
In the agenda of today, an entry can show up multiple times
because it has both a plain timestamp and has a nearby deadline.
When this variable is t, then only the deadline is shown and the
fact that the entry has a timestamp for or including today is not
shown.  When this variable is nil, the entry will be shown
several times."
  :group 'org-agenda-skip
  :group 'org-agenda-daily/weekly
  :version "24.1"
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Always" t)))

(defcustom org-agenda-skip-deadline-if-done nil
  "Non-nil means don't show deadlines when the corresponding item is done.
When nil, the deadline is still shown and should give you a happy feeling.
This is relevant for the daily/weekly agenda.  It applies only to the
actual date of the deadline.  Warnings about approaching and past-due
deadlines are always turned off when the item is DONE."
  :group 'org-agenda-skip
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-skip-deadline-prewarning-if-scheduled nil
  "Non-nil means skip deadline prewarning when entry is also scheduled.
This will apply on all days where a prewarning for the deadline would
be shown, but not at the day when the entry is actually due.  On that day,
the deadline will be shown anyway.
This variable may be set to nil, t, the symbol `pre-scheduled',
or a number which will then give the number of days before the actual
deadline when the prewarnings should resume.  The symbol `pre-scheduled'
eliminates the deadline prewarning only prior to the scheduled date.
This can be used in a workflow where the first showing of the deadline will
trigger you to schedule it, and then you don't want to be reminded of it
because you will take care of it on the day when scheduled."
  :group 'org-agenda-skip
  :group 'org-agenda-daily/weekly
  :version "24.1"
  :type '(choice
	  (const :tag "Always show prewarning" nil)
	  (const :tag "Remove prewarning prior to scheduled date" pre-scheduled)
	  (const :tag "Remove prewarning if entry is scheduled" t)
	  (integer :tag "Restart prewarning N days before deadline")))

(defcustom org-agenda-skip-scheduled-delay-if-deadline nil
  "Non-nil means skip scheduled delay when entry also has a deadline.
This variable may be set to nil, t, the symbol `post-deadline',
or a number which will then give the number of days after the actual
scheduled date when the delay should expire.  The symbol `post-deadline'
eliminates the schedule delay when the date is posterior to the deadline."
  :group 'org-agenda-skip
  :group 'org-agenda-daily/weekly
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Always honor delay" nil)
	  (const :tag "Ignore delay if posterior to the deadline" post-deadline)
	  (const :tag "Ignore delay if entry has a deadline" t)
	  (integer :tag "Honor delay up until N days after the scheduled date")))

(defcustom org-agenda-skip-additional-timestamps-same-entry nil
  "When nil, multiple same-day timestamps in entry make multiple agenda lines.
When non-nil, after the search for timestamps has matched once in an
entry, the rest of the entry will not be searched."
  :group 'org-agenda-skip
  :type 'boolean)

(defcustom org-agenda-skip-timestamp-if-done nil
  "Non-nil means don't select item by timestamp or -range if it is DONE."
  :group 'org-agenda-skip
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-tags-match-list-sublevels t
  "Non-nil means list also sublevels of headlines matching a search.
This variable applies to tags/property searches, and also to stuck
projects because this search is based on a tags match as well.

When set to the symbol `indented', sublevels are indented with
leading dots.

Because of tag inheritance (see variable `org-use-tag-inheritance'),
the sublevels of a headline matching a tag search often also match
the same search.  Listing all of them can create very long lists.
Setting this variable to nil causes subtrees of a match to be skipped.

This variable is semi-obsolete and probably should always be true.  It
is better to limit inheritance to certain tags using the variables
`org-use-tag-inheritance' and `org-tags-exclude-from-inheritance'."
  :group 'org-tags
  :type '(choice
	  (const :tag "No, don't list them" nil)
	  (const :tag "Yes, do list them" t)
	  (const :tag "List them, indented with leading dots" indented)))

;;; Agenda skip function

(defcustom org-agenda-skip-function-global nil
  "Function to be called at each match during agenda construction.
If this function returns nil, the current match should not be skipped.
If the function decided to skip an agenda match, is must return the
buffer position from which the search should be continued.
This may also be a Lisp form, which will be evaluated.

This variable will be applied to every agenda match, including
tags/property searches and TODO lists.  So try to make the test function
do its checking as efficiently as possible.  To implement a skipping
condition just for specific agenda commands, use the variable
`org-agenda-skip-function' which can be set in the options section
of custom agenda commands."
  :group 'org-agenda-skip
  :type 'sexp)

(defvar org-agenda-skip-function nil
  "Function to be called at each match during agenda construction.
If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued.

This may also be a Lisp form that will be evaluated.  Useful
forms include `org-agenda-skip-entry-if' and
`org-agenda-skip-subtree-if'.  See the Info node `(org) Special
Agenda Views' for more details and examples.

Never set this variable using `setq' or similar, because then it
will apply to all future agenda commands.  If you want a global
skipping condition, use the option `org-agenda-skip-function-global'
instead.

The correct way to use `org-agenda-skip-function' is to bind it with `let'
to scope it dynamically into the agenda-constructing command.
A good way to set it is through options in `org-agenda-custom-commands'.")

(defvar org-agenda-archives-mode)
(defun org-agenda-skip (&optional element)
  "Throw to `:skip' in places that should be skipped.
Also moves point to the end of the skipped region, so that search can
continue from there.

Optional argument ELEMENT contains element at point."
  (save-match-data
    (when (or
           (if element
               (org-element-type-p element 'comment)
	     (save-excursion
               (goto-char (line-beginning-position))
               (looking-at comment-start-skip)))
	   (and org-agenda-skip-archived-trees (not org-agenda-archives-mode)
	        (or (and (save-match-data (org-in-archived-heading-p nil element))
		         (org-end-of-subtree t element))
		    (and (member org-archive-tag
                                 (org-element-property :tags (org-element-org-data)))
		         (goto-char (point-max)))))
	   (and org-agenda-skip-comment-trees
                (org-in-commented-heading-p nil element)
	        (org-end-of-subtree t element))
           (let ((to (or (org-agenda-skip-eval org-agenda-skip-function-global)
		         (org-agenda-skip-eval org-agenda-skip-function))))
             (and to (goto-char to)))
	   (org-in-src-block-p t element))
      (throw :skip t))))

(defun org-agenda-skip-eval (form)
  "If FORM is a function or a list, call (or eval) it and return the result.
`save-excursion' and `save-match-data' are wrapped around the call, so point
and match data are returned to the previous state no matter what these
functions do."
  (let (fp)
    (and form
	 (or (setq fp (functionp form))
	     (consp form))
	 (save-excursion
	   (save-match-data
	     (if fp
		 (funcall form)
	       (eval form t)))))))

(defun org-agenda-skip-entry-if (&rest conditions)
  "Skip entry if any of CONDITIONS is true.
See `org-agenda-skip-if' for details about CONDITIONS.

This function can be put into `org-agenda-skip-function' for the
duration of a command."
  (org-agenda-skip-if nil conditions))

(defun org-agenda-skip-subtree-if (&rest conditions)
  "Skip subtree if any of CONDITIONS is true.
See `org-agenda-skip-if' for details about CONDITIONS.

This function can be put into `org-agenda-skip-function' for the
duration of a command."
  (org-agenda-skip-if t conditions))

(declare-function org-back-to-heading "org-move" org-back-to-heading (&optional invisible-ok))
(defun org-agenda-skip-if (subtree conditions)
  "Check current entity for CONDITIONS.
If SUBTREE is non-nil, the entire subtree is checked.  Otherwise, only
the entry (i.e. the text before the next heading) is checked.

CONDITIONS is a list of symbols, boolean OR is used to combine the results
from different tests.  Valid conditions are:

scheduled     Check if there is a scheduled cookie
notscheduled  Check if there is no scheduled cookie
deadline      Check if there is a deadline
notdeadline   Check if there is no deadline
timestamp     Check if there is a timestamp (also deadline or scheduled)
nottimestamp  Check if there is no timestamp (also deadline or scheduled)
regexp        Check if regexp matches
notregexp     Check if regexp does not match.
todo          Check if TODO keyword matches
nottodo       Check if TODO keyword does not match

The regexp is taken from the conditions list, and must come right
after the `regexp' or `notregexp' element.

`todo' and `nottodo' accept as an argument a list of todo
keywords, which may include \"*\" to match any todo keyword.

    (org-agenda-skip-entry-if \\='todo \\='(\"TODO\" \"WAITING\"))

would skip all entries with \"TODO\" or \"WAITING\" keywords.

Instead of a list, a keyword class may be given.  For example:

    (org-agenda-skip-entry-if \\='nottodo \\='done)

would skip entries that haven't been marked with any of \"DONE\"
keywords.  Possible classes are: `todo', `done', `any'.

If any of these conditions is met, this function returns the end point of
the entity, causing the search to continue from there.  This is a function
that can be put into `org-agenda-skip-function' for the duration of a command."
  (require 'org-move)
  (org-back-to-heading t)
  (let* (;; (beg (point))
	 (end (if subtree (save-excursion (org-end-of-subtree t) (point))
		(org-entry-end-position)))
	 (planning-end (if subtree end (line-end-position 2)))
	 m)
    (and
     (or (and (memq 'scheduled conditions)
	      (re-search-forward org-scheduled-time-regexp planning-end t))
	 (and (memq 'notscheduled conditions)
	      (not
	       (save-excursion
		 (re-search-forward org-scheduled-time-regexp planning-end t))))
	 (and (memq 'deadline conditions)
	      (re-search-forward org-deadline-time-regexp planning-end t))
	 (and (memq 'notdeadline conditions)
	      (not
	       (save-excursion
		 (re-search-forward org-deadline-time-regexp planning-end t))))
	 (and (memq 'timestamp conditions)
	      (re-search-forward org-ts-regexp end t))
	 (and (memq 'nottimestamp conditions)
	      (not (save-excursion (re-search-forward org-ts-regexp end t))))
	 (and (setq m (memq 'regexp conditions))
	      (stringp (nth 1 m))
	      (re-search-forward (nth 1 m) end t))
	 (and (setq m (memq 'notregexp conditions))
	      (stringp (nth 1 m))
	      (not (save-excursion (re-search-forward (nth 1 m) end t))))
	 (and (or
	       (setq m (memq 'nottodo conditions))
	       (setq m (memq 'todo-unblocked conditions))
	       (setq m (memq 'nottodo-unblocked conditions))
	       (setq m (memq 'todo conditions)))
	      (org-agenda-skip-if-todo m end)))
     end)))

(declare-function org-entry-blocked-p "org-property" ())
(defun org-agenda-skip-if-todo (args end)
  "Helper function for `org-agenda-skip-if', do not use it directly.
ARGS is a list with first element either `todo', `nottodo',
`todo-unblocked' or `nottodo-unblocked'.  The remainder is either
a list of TODO keywords, or a state symbol `todo' or `done' or
`any'."
  (let ((todo-re
	 (concat "^\\*+[ \t]+"
		 (regexp-opt
		  (pcase args
		    (`(,_ todo)
		     (org-delete-all org-done-keywords
				     (copy-sequence org-todo-keywords-1)))
		    (`(,_ done) org-done-keywords)
		    (`(,_ any) org-todo-keywords-1)
		    (`(,_ ,(pred atom))
		     (error "Invalid TODO class or type: %S" args))
		    (`(,_ ,(pred (member "*"))) org-todo-keywords-1)
		    (`(,_ ,todo-list) todo-list))
		  'words))))
    (pcase args
      (`(todo . ,_)
       (let (case-fold-search) (re-search-forward todo-re end t)))
      (`(nottodo . ,_)
       (not (let (case-fold-search) (re-search-forward todo-re end t))))
      (`(todo-unblocked . ,_)
       (require 'org-property)
       (catch :unblocked
	 (while (let (case-fold-search) (re-search-forward todo-re end t))
	   (when (org-entry-blocked-p) (throw :unblocked t)))
	 nil))
      (`(nottodo-unblocked . ,_)
       (catch :unblocked
	 (while (let (case-fold-search) (re-search-forward todo-re end t))
	   (when (org-entry-blocked-p) (throw :unblocked nil)))
	 t))
      (`(,type . ,_) (error "Unknown TODO skip type: %S" type)))))

;;; Searching Org agenda files

(defvar org-agenda-current-date nil
  "Active date when building the agenda.")

(defun org-agenda-get-day-entries (file date &rest args)
  "Does the work for `org-diary' and `org-agenda'.
FILE is the path to a file to be checked for entries.  DATE is date like
the one returned by `calendar-current-date'.  ARGS are symbols indicating
which kind of entries should be extracted.  For details about these, see
the documentation of `org-diary'."
  (require 'org-mode)
  (defvar org-startup-folded)
  (defvar org-startup-align-all-tables)
  (let* ((org-startup-folded nil)
	 (org-startup-align-all-tables nil)
	 (buffer (if (file-exists-p file) (org-get-agenda-file-buffer file)
		   (error "No such file %s" file))))
    (if (not buffer)
	;; If file does not exist, signal it in diary nonetheless.
	(list (format "ORG-AGENDA-ERROR: No such org-file %s" file))
      (with-current-buffer buffer
	(unless (derived-mode-p 'org-mode)
	  (error "Agenda file %s is not in Org mode" file))
	(setq org-agenda-buffer (or org-agenda-buffer buffer))
	(setf org-agenda-current-date date)
	(save-excursion
	  (save-restriction
	    (if (eq buffer org-agenda-restrict)
		(narrow-to-region org-agenda-restrict-begin
				  org-agenda-restrict-end)
	      (widen))
	    ;; Rationalize ARGS.  Also make sure `:deadline' comes
	    ;; first in order to populate DEADLINES before passing it.
	    ;;
	    ;; We use `delq' since `org-uniquify' duplicates ARGS,
	    ;; guarding us from modifying `org-agenda-entry-types'.
	    (setf args (org-uniquify (or args org-agenda-entry-types)))
	    (when (and (memq :scheduled args) (memq :scheduled* args))
	      (setf args (delq :scheduled* args)))
	    (cond
	     ((memq :deadline args)
	      (setf args (cons :deadline
			       (delq :deadline (delq :deadline* args)))))
	     ((memq :deadline* args)
	      (setf args (cons :deadline* (delq :deadline* args)))))
	    ;; Collect list of headlines.  Return them flattened.
	    (let ((case-fold-search nil) results deadlines)
              (org-dlet
                  ((date date))
	        (dolist (arg args (apply #'nconc (nreverse results)))
		  (pcase arg
		    ((and :todo (guard (org-agenda-today-p date)))
		     (push (org-agenda-get-todos) results))
		    (:timestamp
		     (push (org-agenda-get-blocks) results)
		     (push (org-agenda-get-timestamps deadlines) results))
		    (:sexp
		     (push (org-agenda-get-sexps) results))
		    (:scheduled
		     (push (org-agenda-get-scheduled deadlines) results))
		    (:scheduled*
		     (push (org-agenda-get-scheduled deadlines t) results))
		    (:closed
		     (push (org-agenda-get-progress
                            (if (eq org-agenda-show-log 'clockcheck) '(clock)
                              org-agenda-show-log))
                           results))
		    (:deadline
		     (setf deadlines (org-agenda-get-deadlines))
		     (push deadlines results))
		    (:deadline*
		     (setf deadlines (org-agenda-get-deadlines t))
		     (push deadlines results))))))))))))

(defun org-agenda-todo-custom-ignore-p (time n)
  "Check whether timestamp is farther away than n number of days.
This function is invoked if `org-agenda-todo-ignore-deadlines',
`org-agenda-todo-ignore-scheduled' or
`org-agenda-todo-ignore-timestamp' is set to an integer."
  (let ((days (org-timestamp-to-now
	       time org-agenda-todo-ignore-time-comparison-use-seconds)))
    (if (>= n 0)
	(>= days n)
      (<= days n))))

;;;###autoload
(defun org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item
    (&optional end)
  "Do we have a reason to ignore this TODO entry because it has a time stamp?"
  (when (or org-agenda-todo-ignore-with-date
	    org-agenda-todo-ignore-scheduled
	    org-agenda-todo-ignore-deadlines
	    org-agenda-todo-ignore-timestamp)
    (setq end (or end (save-excursion (outline-next-heading) (point))))
    (save-excursion
      (or (and org-agenda-todo-ignore-with-date
	       (re-search-forward org-ts-regexp end t))
	  (and org-agenda-todo-ignore-scheduled
	       (re-search-forward org-scheduled-time-regexp end t)
	       (cond
		((eq org-agenda-todo-ignore-scheduled 'future)
		 (> (org-timestamp-to-now
		     (match-string 1) org-agenda-todo-ignore-time-comparison-use-seconds)
		    0))
		((eq org-agenda-todo-ignore-scheduled 'past)
		 (<= (org-timestamp-to-now
		     (match-string 1) org-agenda-todo-ignore-time-comparison-use-seconds)
		    0))
		((numberp org-agenda-todo-ignore-scheduled)
		 (org-agenda-todo-custom-ignore-p
		  (match-string 1) org-agenda-todo-ignore-scheduled))
		(t)))
	  (and org-agenda-todo-ignore-deadlines
	       (re-search-forward org-deadline-time-regexp end t)
	       (cond
		((eq org-agenda-todo-ignore-deadlines 'all) t)
		((eq org-agenda-todo-ignore-deadlines 'far)
		 (not (org-deadline-close-p (match-string 1))))
		((eq org-agenda-todo-ignore-deadlines 'future)
		 (> (org-timestamp-to-now
		     (match-string 1) org-agenda-todo-ignore-time-comparison-use-seconds)
		    0))
		((eq org-agenda-todo-ignore-deadlines 'past)
		 (<= (org-timestamp-to-now
		     (match-string 1) org-agenda-todo-ignore-time-comparison-use-seconds)
		    0))
		((numberp org-agenda-todo-ignore-deadlines)
		 (org-agenda-todo-custom-ignore-p
		  (match-string 1) org-agenda-todo-ignore-deadlines))
		(t (org-deadline-close-p (match-string 1)))))
	  (and org-agenda-todo-ignore-timestamp
	       (let ((buffer (current-buffer))
		     (regexp
		      (concat
		       org-scheduled-time-regexp "\\|" org-deadline-time-regexp))
		     (start (point)))
		 ;; Copy current buffer into a temporary one
		 (with-temp-buffer
		   (insert-buffer-substring buffer start end)
		   (goto-char (point-min))
		   ;; Delete SCHEDULED and DEADLINE items
		   (while (re-search-forward regexp end t)
		     (delete-region (match-beginning 0) (match-end 0)))
		   (goto-char (point-min))
		   ;; No search for timestamp left
		   (when (re-search-forward org-ts-regexp nil t)
		     (cond
		      ((eq org-agenda-todo-ignore-timestamp 'future)
		       (> (org-timestamp-to-now
			   (match-string 1) org-agenda-todo-ignore-time-comparison-use-seconds)
			  0))
		      ((eq org-agenda-todo-ignore-timestamp 'past)
		       (<= (org-timestamp-to-now
			   (match-string 1) org-agenda-todo-ignore-time-comparison-use-seconds)
			  0))
		      ((numberp org-agenda-todo-ignore-timestamp)
		       (org-agenda-todo-custom-ignore-p
			(match-string 1) org-agenda-todo-ignore-timestamp))
		      (t))))))))))

(defun org-agenda-entry-get-agenda-timestamp (epom)
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

(defvar org-select-this-todo-keyword nil
  "Keyword selector for todo agenda.
Should either be a keyword, \"*\", or \"|\"-separated list of todo
keywords.")
(defun org-agenda-get-todos ()
  "Return the TODO information for agenda display."
  (let* ((props (list 'face nil
		      'done-face 'org-agenda-done
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'mouse-face 'highlight
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (case-fold-search nil)
	 (regexp (format org-heading-keyword-regexp-format
			 (cond
			  ((and org-select-this-todo-keyword
				(equal org-select-this-todo-keyword "*"))
			   org-todo-regexp)
			  (org-select-this-todo-keyword
			   (concat "\\("
				   (mapconcat #'regexp-quote
				              (org-split-string
				               org-select-this-todo-keyword
				               "|")
				              "\\|")
				   "\\)"))
			  (t org-not-done-regexp))))
	 marker priority urgency category level tags todo-state
	 ts-date ts-date-type ts-date-pair
	 ee txt beg end inherited-tags todo-state-end-pos
         effort effort-minutes)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(save-match-data
	  (forward-line 0)
	  (org-agenda-skip)
	  (setq beg (point) end (save-excursion (outline-next-heading) (point)))
	  (unless (and (setq todo-state (org-get-todo-state))
		       (setq todo-state-end-pos (match-end 2)))
	    (goto-char end)
	    (throw :skip nil))
	  (when (org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item end)
	    (goto-char (1+ beg))
	    (or org-agenda-todo-list-sublevels (org-end-of-subtree 'invisible))
	    (throw :skip nil)))
	(goto-char (match-beginning 2))
	(setq marker (org-agenda-new-marker (match-beginning 0))
	      category (save-match-data (org-get-category))
              effort (save-match-data (or (get-text-property (point) 'effort)
                                          (org-entry-get (point) org-effort-property)))
              effort-minutes (when effort (save-match-data (org-duration-to-minutes effort)))
	      ts-date-pair (org-agenda-entry-get-agenda-timestamp (point))
	      ts-date (car ts-date-pair)
	      ts-date-type (cdr ts-date-pair)
	      txt (org-trim (buffer-substring (match-beginning 2) (match-end 0)))
	      inherited-tags
	      (or (eq org-agenda-show-inherited-tags 'always)
		  (and (listp org-agenda-show-inherited-tags)
		       (memq 'todo org-agenda-show-inherited-tags))
		  (and (eq org-agenda-show-inherited-tags t)
		       (or (eq org-agenda-use-tag-inheritance t)
			   (memq 'todo org-agenda-use-tag-inheritance))))
	      tags (org-get-tags nil (not inherited-tags))
	      level (make-string (org-reduced-level (org-outline-level)) ? )
	      txt (org-agenda-format-item
                   ""
                   (org-add-props txt nil
                     'effort effort
                     'effort-minutes effort-minutes)
                   level category tags t)
              urgency (1+ (org-get-priority txt))
	      priority (org-get-priority txt))
	(org-add-props txt props
	  'org-marker marker 'org-hd-marker marker
	  'priority priority
          'urgency urgency
          'effort effort 'effort-minutes effort-minutes
	  'level level
	  'ts-date ts-date
	  'type (concat "todo" ts-date-type) 'todo-state todo-state)
	(push txt ee)
	(if org-agenda-todo-list-sublevels
	    (goto-char todo-state-end-pos)
	  (org-end-of-subtree 'invisible))))
    (nreverse ee)))

(defun org-agenda--timestamp-to-absolute (&rest args)
  "Call `org-time-string-to-absolute' with ARGS.
However, throw `:skip' whenever an error is raised."
  (condition-case e
      (apply #'org-time-string-to-absolute args)
    (org-diary-sexp-no-match (throw :skip nil))
    (error
     (message "%s; Skipping entry" (error-message-string e))
     (throw :skip nil))))

(defvar org-agenda-include-inactive-timestamps nil
  "Non-nil means include inactive time stamps in agenda.
Dynamically scoped.")
(defun org-agenda-get-timestamps (&optional deadlines)
  "Return the date stamp information for agenda display.
Optional argument DEADLINES is a list of deadline items to be
displayed in agenda view."
  (with-no-warnings (defvar date))
  (let* ((props (list 'face 'org-agenda-calendar-event
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'mouse-face 'highlight
		      'help-echo
		      (format "mouse-2 or RET jump to Org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (current (calendar-absolute-from-gregorian date))
	 (today (org-today))
	 (deadline-position-alist
	  (mapcar (lambda (d)
		    (let ((m (get-text-property 0 'org-hd-marker d)))
		      (and m (marker-position m))))
		  deadlines))
	 ;; Match timestamps set to current date, timestamps with
	 ;; a repeater, and S-exp timestamps.
	 (regexp
	  (concat
	   (if org-agenda-include-inactive-timestamps "[[<]" "<")
	   (regexp-quote
	    (format-time-string
             "%Y-%m-%d" ; We do not use `org-time-stamp-format' to not demand day name in timestamps.
	     (org-encode-time	; DATE bound by calendar
	      0 0 0 (nth 1 date) (car date) (nth 2 date))))
	   "\\|\\(<[0-9]+-[0-9]+-[0-9]+[^>\n]+?\\+[0-9]+[hdwmy]>\\)"
	   "\\|\\(<%%\\(([^>\n]+)\\)\\([^\n>]*\\)>\\)"))
	 timestamp-items)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      ;; Skip date ranges, scheduled and deadlines, which are handled
      ;; specially.  Also skip timestamps before first headline as
      ;; there would be no entry to add to the agenda.  Eventually,
      ;; ignore clock entries.
      (catch :skip
	(save-match-data
	  (when (or (org-at-date-range-p t)
		    (org-at-planning-p)
		    (org-before-first-heading-p)
		    (and org-agenda-include-inactive-timestamps
			 (org-at-clock-log-p))
                    (not (org-at-timestamp-p 'agenda)))
	    (throw :skip nil))
	  (org-agenda-skip (org-element-at-point)))
	(let* ((pos (match-beginning 0))
	       (repeat (match-string 1))
	       (sexp-entry (match-string 3))
	       (timestamp (if (or repeat sexp-entry) (match-string 0)
			    (save-excursion
			      (goto-char pos)
			      (looking-at org-ts-regexp-both)
			      (match-string 0))))
	       (todo-state (org-get-todo-state))
	       (warntime (org-entry-get (point) "APPT_WARNTIME" 'selective))
	       (done? (member todo-state org-done-keywords)))
	  ;; Possibly skip done tasks.
	  (when (and done? org-agenda-skip-timestamp-if-done)
	    (throw :skip t))
	  ;; S-exp entry doesn't match current day: skip it.
	  (when (and sexp-entry (not (org-diary-sexp-entry sexp-entry "" date)))
	    (throw :skip nil))
	  (when repeat
	    (let* ((past
		    ;; A repeating time stamp is shown at its base
		    ;; date and every repeated date up to TODAY.  If
		    ;; `org-agenda-prefer-last-repeat' is non-nil,
		    ;; however, only the last repeat before today
		    ;; (inclusive) is shown.
		    (org-agenda--timestamp-to-absolute
		     repeat
		     (if (or (> current today)
			     (eq org-agenda-prefer-last-repeat t)
			     (member todo-state org-agenda-prefer-last-repeat))
			 today
		       current)
		     'past (current-buffer) pos))
		   (future
		    ;;  Display every repeated date past TODAY
		    ;;  (exclusive) unless
		    ;;  `org-agenda-show-future-repeats' is nil.  If
		    ;;  this variable is set to `next', only display
		    ;;  the first repeated date after TODAY
		    ;;  (exclusive).
		    (cond
		     ((<= current today) past)
		     ((not org-agenda-show-future-repeats) past)
		     (t
		      (let ((base (if (eq org-agenda-show-future-repeats 'next)
				      (1+ today)
				    current)))
			(org-agenda--timestamp-to-absolute
			 repeat base 'future (current-buffer) pos))))))
	      (when (and (/= current past) (/= current future))
		(throw :skip nil))))
	  (save-excursion
	    (re-search-backward org-outline-regexp-bol nil t)
	    ;; Possibly skip timestamp when a deadline is set.
	    (when (and org-agenda-skip-timestamp-if-deadline-is-shown
		       (assq (point) deadline-position-alist))
	      (throw :skip nil))
	    (let* ((category (org-get-category pos))
                   (effort (org-entry-get pos org-effort-property))
                   (effort-minutes (when effort (save-match-data (org-duration-to-minutes effort))))
		   (inherited-tags
		    (or (eq org-agenda-show-inherited-tags 'always)
			(and (consp org-agenda-show-inherited-tags)
			     (memq 'agenda org-agenda-show-inherited-tags))
			(and (eq org-agenda-show-inherited-tags t)
			     (or (eq org-agenda-use-tag-inheritance t)
				 (memq 'agenda
				       org-agenda-use-tag-inheritance)))))
		   (tags (org-get-tags nil (not inherited-tags)))
		   (level (make-string (org-reduced-level (org-outline-level))
				       ?\s))
		   (head (and (looking-at "\\*+[ \t]+\\(.*\\)")
			      (match-string 1)))
		   (inactive? (= (char-after pos) ?\[))
		   (habit? (and (fboundp 'org-is-habit-p) (org-is-habit-p)))
		   (item
		    (org-agenda-format-item
		     (and inactive? org-agenda-inactive-leader)
                     (org-add-props head nil
                       'effort effort
                       'effort-minutes effort-minutes)
                     level category tags timestamp org-ts-regexp habit?)))
	      (org-add-props item props
		'urgency (if habit?
 			     (org-habit-get-urgency (org-habit-parse-todo))
			   (org-get-priority item))
                'priority (org-get-priority item)
		'org-marker (org-agenda-new-marker pos)
		'org-hd-marker (org-agenda-new-marker)
		'date date
		'level level
                'effort effort 'effort-minutes effort-minutes
		'ts-date (if repeat (org-agenda--timestamp-to-absolute repeat)
			   current)
		'todo-state todo-state
		'warntime warntime
		'type "timestamp")
	      (push item timestamp-items))))
	(when org-agenda-skip-additional-timestamps-same-entry
	  (outline-next-heading))))
    (nreverse timestamp-items)))

(defun org-agenda-get-sexps ()
  "Return the sexp information for agenda display."
  (require 'diary-lib)
  (with-no-warnings (defvar date) (defvar entry))
  (let* ((props (list 'face 'org-agenda-calendar-sexp
		      'mouse-face 'highlight
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp "^&?%%(")
	 ;; FIXME: Is this `entry' binding intended to be dynamic,
         ;; so as to "hide" any current binding for it?
	 marker category extra level ee txt tags entry
	 result beg b sexp sexp-entry todo-state warntime inherited-tags
         effort effort-minutes)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
        ;; We do not run `org-agenda-skip' right away because every single sexp
        ;; in the buffer is matched here, unlike day-specific search
        ;; in ordinary timestamps.  Most of the sexps will not match
        ;; the agenda day and it is quicker to run `org-agenda-skip' only for
        ;; matching sexps later on.
	(setq beg (match-beginning 0))
	(goto-char (1- (match-end 0)))
	(setq b (point))
	(forward-sexp 1)
	(setq sexp (buffer-substring b (point)))
	(setq sexp-entry (if (looking-at "[ \t]*\\(\\S-.*\\)")
                             (buffer-substring
                              (match-beginning 1)
                              (save-excursion
                                (goto-char (match-end 1))
                                (skip-chars-backward "[:blank:]")
                                (point)))
			   ""))
	(setq result (org-diary-sexp-entry sexp sexp-entry date))
	(when result
          ;; Only check if entry should be skipped on matching sexps.
          (org-agenda-skip (org-element-at-point))
	  (setq marker (org-agenda-new-marker beg)
		level (make-string (org-reduced-level (org-outline-level)) ? )
		category (org-get-category beg)
                effort (save-match-data (or (get-text-property (point) 'effort)
                                            (org-entry-get (point) org-effort-property)))
		inherited-tags
		(or (eq org-agenda-show-inherited-tags 'always)
		    (and (listp org-agenda-show-inherited-tags)
			 (memq 'agenda org-agenda-show-inherited-tags))
		    (and (eq org-agenda-show-inherited-tags t)
			 (or (eq org-agenda-use-tag-inheritance t)
			     (memq 'agenda org-agenda-use-tag-inheritance))))
		tags (org-get-tags nil (not inherited-tags))
		todo-state (org-get-todo-state)
		warntime (org-entry-get (point) "APPT_WARNTIME" 'selective)
		extra nil)
          (setq effort-minutes (when effort (save-match-data (org-duration-to-minutes effort))))

	  (dolist (r (if (stringp result)
			 (list result)
		       result)) ;; we expect a list here
	    (when (and org-agenda-diary-sexp-prefix
		       (string-match org-agenda-diary-sexp-prefix r))
	      (setq extra (match-string 0 r)
		    r (replace-match "" nil nil r)))
	    (if (string-match "\\S-" r)
		(setq txt r)
	      (setq txt "SEXP entry returned empty string"))
	    (setq txt (org-agenda-format-item extra
                                              (org-add-props txt nil
                                                'effort effort
                                                'effort-minutes effort-minutes)
                                              level category tags 'time))
	    (org-add-props txt props 'org-marker marker
			   'date date 'todo-state todo-state
                           'effort effort 'effort-minutes effort-minutes
			   'level level 'type "sexp" 'warntime warntime)
	    (push txt ee)))))
    (nreverse ee)))

(defalias 'org-get-closed #'org-agenda-get-progress)
(defun org-agenda-get-progress (&optional entry-types)
  "Return the logged TODO entries for agenda display.
ENTRY-TYPES, when non-nil, limits the types of entries to be selected.
The allowed values are those of `org-agenda-log-mode-items', which
see.  When ENTRY-TYPES is nil, use `org-agenda-log-mode-items'."
  (with-no-warnings (defvar date))
  (let* ((props (list 'mouse-face 'highlight
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (items (if (consp entry-types) entry-types
		  org-agenda-log-mode-items))
	 (parts
	  (delq nil
		(list
		 (when (memq 'closed items) (concat "\\<" org-closed-string))
		 (when (memq 'clock items) (concat "\\<" org-clock-string))
		 (when (memq 'state items)
		   (format "- +State \"%s\".*?" org-todo-regexp)))))
	 (parts-re (if parts (mapconcat #'identity parts "\\|")
		     (error "`org-agenda-log-mode-items' is empty")))
	 (regexp (concat
		  "\\(" parts-re "\\)"
		  " *\\["
		  (regexp-quote
		   (format-time-string
                    "%Y-%m-%d" ; We do not use `org-time-stamp-format' to not demand day name in timestamps.
		    (org-encode-time  ; DATE bound by calendar
		     0 0 0 (nth 1 date) (car date) (nth 2 date))))))
	 (org-agenda-search-headline-for-time nil)
	 marker hdmarker priority category level tags closedp type
	 statep clockp state ee txt extra timestr rest clocked inherited-tags
         effort effort-minutes)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(org-agenda-skip)
	(setq marker (org-agenda-new-marker (match-beginning 0))
	      closedp (equal (match-string 1) org-closed-string)
	      statep (equal (string-to-char (match-string 1)) ?-)
	      clockp (not (or closedp statep))
	      state (and statep (match-string 2))
	      category (save-match-data (org-get-category (match-beginning 0)))
	      timestr (buffer-substring (match-beginning 0) (line-end-position))
              effort (save-match-data (or (get-text-property (point) 'effort)
                                          (org-entry-get (point) org-effort-property))))
        (setq effort-minutes (when effort (save-match-data (org-duration-to-minutes effort))))
	(when (string-match org-ts-regexp-inactive timestr)
	  ;; substring should only run to end of time stamp
	  (setq rest (substring timestr (match-end 0))
		timestr (substring timestr 0 (match-end 0)))
	  (if (and (not closedp) (not statep)
		   (string-match "\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\].*?\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)"
				 rest))
	      (progn (setq timestr (concat (substring timestr 0 -1)
					   "-" (match-string 1 rest) "]"))
		     (setq clocked (match-string 2 rest)))
	    (setq clocked "-")))
	(save-excursion
	  (setq extra
		(cond
		 ((not org-agenda-log-mode-add-notes) nil)
		 (statep
		  (and (looking-at ".*\\\\\n[ \t]*\\([^-\n \t].*?\\)[ \t]*$")
		       (match-string 1)))
		 (clockp
		  (and (looking-at ".*\n[ \t]*-[ \t]+\\([^-\n \t].*?\\)[ \t]*$")
		       (match-string 1)))))
	  (if (not (re-search-backward org-outline-regexp-bol nil t))
	      (throw :skip nil)
	    (goto-char (match-beginning 0))
	    (setq hdmarker (org-agenda-new-marker)
		  inherited-tags
		  (or (eq org-agenda-show-inherited-tags 'always)
		      (and (listp org-agenda-show-inherited-tags)
			   (memq 'todo org-agenda-show-inherited-tags))
		      (and (eq org-agenda-show-inherited-tags t)
			   (or (eq org-agenda-use-tag-inheritance t)
			       (memq 'todo org-agenda-use-tag-inheritance))))
		  tags (org-get-tags nil (not inherited-tags))
		  level (make-string (org-reduced-level (org-outline-level)) ? ))
	    (looking-at "\\*+[ \t]+\\([^\r\n]+\\)")
	    (setq txt (match-string 1))
	    (when extra
	      (if (string-match "\\([ \t]+\\)\\(:[^ \n\t]*?:\\)[ \t]*$" txt)
		  (setq txt (concat (substring txt 0 (match-beginning 1))
				    " - " extra " " (match-string 2 txt)))
		(setq txt (concat txt " - " extra))))
	    (setq txt (org-agenda-format-item
		       (cond
			(closedp "Closed:    ")
			(statep (concat "State:     (" state ")"))
			(t (concat "Clocked:   (" clocked  ")")))
                       (org-add-props txt nil
                         'effort effort
                         'effort-minutes effort-minutes)
		       level category tags timestr)))
	  (setq type (cond (closedp "closed")
			   (statep "state")
			   (t "clock")))
	  (setq priority 100000)
	  (org-add-props txt props
	    'org-marker marker 'org-hd-marker hdmarker 'face 'org-agenda-done
	    'urgency priority 'priority priority 'level level
            'effort effort 'effort-minutes effort-minutes
	    'type type 'date date
	    'undone-face 'org-warning 'done-face 'org-agenda-done)
	  (push txt ee))
        (goto-char (line-end-position))))
    (nreverse ee)))

(defun org-deadline-close-p (timestamp-string &optional ndays)
  "Is the time in TIMESTAMP-STRING close to the current date?"
  (setq ndays (or ndays (org-get-wdays timestamp-string)))
  (and (<= (org-timestamp-to-now timestamp-string) ndays)
       (not (org-entry-is-done-p))))

(defun org-get-wdays (ts &optional delay zero-delay)
  "Get the deadline lead time appropriate for timestring TS.
When DELAY is non-nil, get the delay time for scheduled items
instead of the deadline lead time.  When ZERO-DELAY is non-nil
and `org-scheduled-delay-days' is 0, enforce 0 as the delay,
don't try to find the delay cookie in the scheduled timestamp."
  (let ((tv (if delay org-scheduled-delay-days
	      org-deadline-warning-days)))
    (cond
     ((or (and delay (< tv 0))
	  (and delay zero-delay (<= tv 0))
	  (and (not delay) (<= tv 0)))
      ;; Enforce this value no matter what
      (- tv))
     ((string-match "-\\([0-9]+\\)\\([hdwmy]\\)\\(\\'\\|>\\| \\)" ts)
      ;; lead time is specified.
      (floor (* (string-to-number (match-string 1 ts))
		(cdr (assoc (match-string 2 ts)
			    '(("d" . 1)    ("w" . 7)
			      ("m" . 30.4) ("y" . 365.25)
			      ("h" . 0.041667)))))))
     ;; go for the default.
     (t tv))))

(defun org-agenda-get-deadlines (&optional with-hour)
  "Return the deadline information for agenda display.
When WITH-HOUR is non-nil, only return deadlines with an hour
specification like [h]h:mm."
  (with-no-warnings (defvar date))
  (let* ((props (list 'mouse-face 'highlight
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp (if with-hour
		     org-deadline-time-hour-regexp
		   org-deadline-time-regexp))
	 (today (org-today))
	 (today? (org-agenda-today-p date)) ; DATE bound by calendar.
	 (current (calendar-absolute-from-gregorian date))
         deadline-items)
    (org-element-cache-map
     (lambda (el)
       (when (and (org-element-property :deadline el)
                  ;; Only consider active timestamp values.
                  (memq (org-element-property
                         :type
                         (org-element-property :deadline el))
                        '(diary active active-range))
                  (or (not with-hour)
                      (org-element-property
                       :hour-start
                       (org-element-property :deadline el))
                      (org-element-property
                       :hour-end
                       (org-element-property :deadline el))))
         (goto-char (org-element-contents-begin el))
         (catch :skip
	   (org-agenda-skip el)
	   (let* ((s (substring (org-element-property
                                 :raw-value
                                 (org-element-property :deadline el))
                                1 -1))
	          (pos (save-excursion
                         (goto-char (org-element-contents-begin el))
                         ;; We intentionally leave NOERROR
                         ;; argument in `re-search-forward' nil.  If
                         ;; the search fails here, something went
                         ;; wrong and we are looking at
                         ;; non-matching headline.
                         (re-search-forward regexp (line-end-position))
                         (1- (match-beginning 1))))
	          (todo-state (org-element-property :todo-keyword el))
	          (done? (eq 'done (org-element-property :todo-type el)))
                  (sexp? (eq 'diary
                             (org-element-property
                              :type (org-element-property :deadline el))))
	          ;; DEADLINE is the deadline date for the entry.  It is
	          ;; either the base date or the last repeat, according
	          ;; to `org-agenda-prefer-last-repeat'.
	          (deadline
		   (cond
		    (sexp? (org-agenda--timestamp-to-absolute s current))
		    ((or (eq org-agenda-prefer-last-repeat t)
		         (member todo-state org-agenda-prefer-last-repeat))
		     (org-agenda--timestamp-to-absolute
		      s today 'past (current-buffer) pos))
		    (t (org-agenda--timestamp-to-absolute s))))
	          ;; REPEAT is the future repeat closest from CURRENT,
	          ;; according to `org-agenda-show-future-repeats'. If
	          ;; the latter is nil, or if the time stamp has no
	          ;; repeat part, default to DEADLINE.
	          (repeat
		   (cond
		    (sexp? deadline)
		    ((<= current today) deadline)
		    ((not org-agenda-show-future-repeats) deadline)
		    (t
		     (let ((base (if (eq org-agenda-show-future-repeats 'next)
				     (1+ today)
				   current)))
		       (org-agenda--timestamp-to-absolute
		        s base 'future (current-buffer) pos)))))
	          (diff (- deadline current))
	          (max-warning-days
		   (let ((scheduled
		          (and org-agenda-skip-deadline-prewarning-if-scheduled
                               (org-element-property
                                :raw-value
                                (org-element-property :scheduled el)))))
		     (cond
		      ((not scheduled) most-positive-fixnum)
		      ;; The current item has a scheduled date, so
		      ;; evaluate its prewarning lead time.
		      ((integerp org-agenda-skip-deadline-prewarning-if-scheduled)
		       ;; Use global prewarning-restart lead time.
		       org-agenda-skip-deadline-prewarning-if-scheduled)
		      ((eq org-agenda-skip-deadline-prewarning-if-scheduled
			   'pre-scheduled)
		       ;; Set pre-warning to no earlier than SCHEDULED.
		       (min (- deadline
			       (org-agenda--timestamp-to-absolute scheduled))
			    org-deadline-warning-days))
		      ;; Set pre-warning to deadline.
		      (t 0))))
	          (warning-days (min max-warning-days (org-get-wdays s))))
	     (cond
	      ;; Only display deadlines at their base date, at future
	      ;; repeat occurrences or in today agenda.
	      ((= current deadline) nil)
	      ((= current repeat) nil)
	      ((not today?) (throw :skip nil))
	      ;; Upcoming deadline: display within warning period WARNING-DAYS.
	      ((> deadline current) (when (> diff warning-days) (throw :skip nil)))
	      ;; Overdue deadline: warn about it for
	      ;; `org-deadline-past-days' duration.
	      (t (when (< org-deadline-past-days (- diff)) (throw :skip nil))))
	     ;; Possibly skip done tasks.
	     (when (and done?
		        (or org-agenda-skip-deadline-if-done
			    (/= deadline current)))
	       (throw :skip nil))
	     (save-excursion
               (goto-char (org-element-begin el))
	       (let* ((category (org-get-category))
                      (effort (save-match-data (or (get-text-property (point) 'effort)
                                                   (org-element-property (intern (concat ":" (upcase org-effort-property))) el))))
                      (effort-minutes (when effort (save-match-data (org-duration-to-minutes effort))))
		      (level (make-string (org-element-property :level el)
				          ?\s))
		      (head (save-excursion
                              (goto-char (org-element-begin el))
                              (re-search-forward org-outline-regexp-bol)
                              (buffer-substring-no-properties (point) (line-end-position))))
		      (inherited-tags
		       (or (eq org-agenda-show-inherited-tags 'always)
			   (and (listp org-agenda-show-inherited-tags)
			        (memq 'agenda org-agenda-show-inherited-tags))
			   (and (eq org-agenda-show-inherited-tags t)
			        (or (eq org-agenda-use-tag-inheritance t)
				    (memq 'agenda
				          org-agenda-use-tag-inheritance)))))
		      (tags (org-get-tags el (not inherited-tags)))
		      (time
		       (cond
		        ;; No time of day designation if it is only
		        ;; a reminder.
		        ((and (/= current deadline) (/= current repeat)) nil)
		        ((string-match " \\([012]?[0-9]:[0-9][0-9]\\)" s)
		         (concat (substring s (match-beginning 1)) " "))
		        (t 'time)))
		      (item
		       (org-agenda-format-item
		        ;; Insert appropriate suffixes before deadlines.
		        ;; Those only apply to today agenda.
		        (pcase-let ((`(,now ,future ,past)
				     org-agenda-deadline-leaders))
		          (cond
			   ((and today? (< deadline today)) (format past (- diff)))
			   ((and today? (> deadline today)) (format future diff))
			   (t now)))
		        (org-add-props head nil
                          'effort effort
                          'effort-minutes effort-minutes)
                        level category tags time))
		      (face (org-agenda-deadline-face
			     (- 1 (/ (float diff) (max warning-days 1)))))
		      (upcoming? (and today? (> deadline today)))
		      (warntime (org-entry-get (point) "APPT_WARNTIME" 'selective)))
	         (org-add-props item props
		   'org-marker (org-agenda-new-marker pos)
		   'org-hd-marker (org-agenda-new-marker (line-beginning-position))
		   'warntime warntime
		   'level level
                   'effort effort 'effort-minutes effort-minutes
		   'ts-date deadline
		   'urgency
		   ;; Adjust urgency to today reminders about deadlines.
		   ;; Overdue deadlines get the highest urgency
		   ;; increase, then imminent deadlines and eventually
		   ;; more distant deadlines.
		   (let ((adjust (if today? (- diff) 0)))
		     (+ adjust (org-get-priority item)))
                   'priority (org-get-priority item)
		   'todo-state todo-state
		   'type (if upcoming? "upcoming-deadline" "deadline")
		   'date (if upcoming? date deadline)
		   'face (if done? 'org-agenda-done face)
		   'undone-face face
		   'done-face 'org-agenda-done)
	         (push item deadline-items)))))))
     :next-re regexp
     :fail-re regexp
     :narrow t)
    (nreverse deadline-items)))

(defvar org-habit-scheduled-past-days) ; org-habit.el
(declare-function org-habit-parse-todo "org-habit" (&optional pom))
(declare-function org-habit-get-urgency "org-habit" (habit &optional moment))
(defun org-agenda-get-scheduled (&optional deadlines with-hour)
  "Return the scheduled information for agenda display.
Optional argument DEADLINES is a list of deadline items to be
displayed in agenda view.  When WITH-HOUR is non-nil, only return
scheduled items with an hour specification like [h]h:mm."
  (with-no-warnings (defvar date))
  (let* ((props (list 'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'done-face 'org-agenda-done
		      'mouse-face 'highlight
		      'help-echo
		      (format "mouse-2 or RET jump to Org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp (if with-hour
		     org-scheduled-time-hour-regexp
		   org-scheduled-time-regexp))
	 (today (org-today))
	 (todayp (org-agenda-today-p date)) ; DATE bound by calendar.
	 (current (calendar-absolute-from-gregorian date))
	 (deadline-pos
	  (mapcar (lambda (d)
		    (let ((m (get-text-property 0 'org-hd-marker d)))
		      (and m (marker-position m))))
		  deadlines))
         scheduled-items)
    (org-element-cache-map
     (lambda (el)
       (when (and (org-element-property :scheduled el)
                  ;; Only consider active timestamp values.
                  (memq (org-element-property
                         :type
                         (org-element-property :scheduled el))
                        '(diary active active-range))
                  (or (not with-hour)
                      (org-element-property
                       :hour-start
                       (org-element-property :scheduled el))
                      (org-element-property
                       :hour-end
                       (org-element-property :scheduled el))))
         (goto-char (org-element-contents-begin el))
         (catch :skip
           (org-agenda-skip el)
           (let* ((s (substring (org-element-property
                                 :raw-value
                                 (org-element-property :scheduled el))
                                1 -1))
                  (pos (save-excursion
                         (goto-char (org-element-contents-begin el))
                         ;; We intentionally leave NOERROR
                         ;; argument in `re-search-forward' nil.  If
                         ;; the search fails here, something went
                         ;; wrong and we are looking at
                         ;; non-matching headline.
                         (re-search-forward regexp (line-end-position))
                         (1- (match-beginning 1))))
                  (todo-state (org-element-property :todo-keyword el))
	          (donep (eq 'done (org-element-property :todo-type el)))
	          (sexp? (eq 'diary
                             (org-element-property
                              :type (org-element-property :scheduled el))))
	          ;; SCHEDULE is the scheduled date for the entry.  It is
	          ;; either the bare date or the last repeat, according
	          ;; to `org-agenda-prefer-last-repeat'.
	          (schedule
		   (cond
		    (sexp? (org-agenda--timestamp-to-absolute s current))
		    ((or (eq org-agenda-prefer-last-repeat t)
		         (member todo-state org-agenda-prefer-last-repeat))
		     (org-agenda--timestamp-to-absolute
		      s today 'past (current-buffer) pos))
		    (t (org-agenda--timestamp-to-absolute s))))
	          ;; REPEAT is the future repeat closest from CURRENT,
	          ;; according to `org-agenda-show-future-repeats'. If
	          ;; the latter is nil, or if the time stamp has no
	          ;; repeat part, default to SCHEDULE.
	          (repeat
		   (cond
		    (sexp? schedule)
		    ((<= current today) schedule)
		    ((not org-agenda-show-future-repeats) schedule)
		    (t
		     (let ((base (if (eq org-agenda-show-future-repeats 'next)
				     (1+ today)
				   current)))
		       (org-agenda--timestamp-to-absolute
		        s base 'future (current-buffer) pos)))))
	          (diff (- current schedule))
	          (warntime (org-entry-get (point) "APPT_WARNTIME" 'selective))
	          (pastschedp (< schedule today))
	          (futureschedp (> schedule today))
	          (habitp (and (fboundp 'org-is-habit-p)
                               (string= "habit" (org-element-property :STYLE el))))
	          (max-delay-days
		   (let ((deadline (and org-agenda-skip-scheduled-delay-if-deadline
                                        (org-element-property
                                         :raw-value
                                         (org-element-property :deadline el)))))
		     (cond
		      ((not deadline) most-positive-fixnum)
		      ;; The current item has a deadline date, so
		      ;; evaluate its delay time.
		      ((integerp org-agenda-skip-scheduled-delay-if-deadline)
		       ;; Use global delay time.
		       (- org-agenda-skip-scheduled-delay-if-deadline))
		      ((eq org-agenda-skip-scheduled-delay-if-deadline
			   'post-deadline)
		       ;; Set delay to no later than DEADLINE.
		       (min (- schedule
			       (org-agenda--timestamp-to-absolute deadline))
			    org-scheduled-delay-days))
		      (t 0))))
	          (delay-days
		   (cond
		    ;; Nullify delay when a repeater triggered already
		    ;; and the delay is of the form --Xd.
		    ((and (string-match-p "--[0-9]+[hdwmy]" s)
		          (> schedule (org-agenda--timestamp-to-absolute s)))
		     0)
		    (t (min max-delay-days (org-get-wdays s t))))))
	     ;; Display scheduled items at base date (SCHEDULE), today if
	     ;; scheduled before the current date, and at any repeat past
	     ;; today.  However, skip delayed items and items that have
	     ;; been displayed for more than `org-scheduled-past-days'.
	     (unless (and todayp
		          habitp
		          (bound-and-true-p org-habit-show-all-today))
	       (when (or (and (> delay-days 0) (< diff delay-days))
		         (> diff (or (and habitp org-habit-scheduled-past-days)
				     org-scheduled-past-days))
		         (> schedule current)
		         (and (/= current schedule)
			      (/= current today)
			      (/= current repeat)))
	         (throw :skip nil)))
	     ;; Possibly skip done tasks.
	     (when (and donep
		        (or org-agenda-skip-scheduled-if-done
			    (/= schedule current)))
	       (throw :skip nil))
	     ;; Skip entry if it already appears as a deadline, per
	     ;; `org-agenda-skip-scheduled-if-deadline-is-shown'.  This
	     ;; doesn't apply to habits.
             (when (or org-agenda-skip-scheduled-repeats-after-deadline
                       ;; FIXME: Backwards-compatibility.
                       (eq org-agenda-skip-scheduled-if-deadline-is-shown
                           'repeated-after-deadline))
               (let ((deadline
                      (time-to-days
                       (when (org-element-property :deadline el)
                         (org-time-string-to-time
                          (org-element-interpret-data
                           (org-element-property :deadline el)))))))
		 (when (and (or (<= (org-agenda--timestamp-to-absolute s) deadline)
                                (not (= schedule current)))
                            (> current deadline))
                   (throw :skip nil))))
	     (when (pcase org-agenda-skip-scheduled-if-deadline-is-shown
		     ((guard
		       (or (not (memq (line-beginning-position 0) deadline-pos))
			   habitp))
		      nil)
		     (`not-today pastschedp)
		     (`t t)
		     (_ nil))
	       (throw :skip nil))
	     ;; Skip habits if `org-habit-show-habits' is nil, or if we
	     ;; only show them for today.  Also skip done habits.
	     (when (and habitp
		        (or donep
			    (not (bound-and-true-p org-habit-show-habits))
			    (and (not todayp)
			         (bound-and-true-p
			          org-habit-show-habits-only-for-today))))
	       (throw :skip nil))
	     (save-excursion
               (goto-char (org-element-begin el))
	       (let* ((category (org-get-category))
                      (effort (save-match-data
                                (or (get-text-property (point) 'effort)
                                    (org-element-property (intern (concat ":" (upcase org-effort-property))) el))))
                      (effort-minutes (when effort (save-match-data (org-duration-to-minutes effort))))
		      (inherited-tags
		       (or (eq org-agenda-show-inherited-tags 'always)
			   (and (listp org-agenda-show-inherited-tags)
			        (memq 'agenda org-agenda-show-inherited-tags))
			   (and (eq org-agenda-show-inherited-tags t)
			        (or (eq org-agenda-use-tag-inheritance t)
				    (memq 'agenda
				          org-agenda-use-tag-inheritance)))))
		      (tags (org-get-tags el (not inherited-tags)))
		      (level (make-string (org-element-property :level el)
				          ?\s))
		      (head (save-excursion
                              (goto-char (org-element-begin el))
                              (re-search-forward org-outline-regexp-bol)
                              (buffer-substring (point) (line-end-position))))
		      (time
		       (cond
		        ;; No time of day designation if it is only a
		        ;; reminder, except for habits, which always show
		        ;; the time of day.  Habits are an exception
		        ;; because if there is a time of day, that is
		        ;; interpreted to mean they should usually happen
		        ;; then, even if doing the habit was missed.
		        ((and
		          (not habitp)
		          (/= current schedule)
		          (/= current repeat))
		         nil)
		        ((string-match " \\([012]?[0-9]:[0-9][0-9]\\)" s)
		         (concat (substring s (match-beginning 1)) " "))
		        (t 'time)))
		      (item
		       (org-agenda-format-item
		        (pcase-let ((`(,first ,past) org-agenda-scheduled-leaders))
		          ;; Show a reminder of a past scheduled today.
		          (if (and todayp pastschedp)
			      (format past diff)
			    first))
		        (org-add-props head nil
                          'effort effort
                          'effort-minutes effort-minutes)
                        level category tags time nil habitp))
		      (face (cond ((and (not habitp) pastschedp)
				   'org-scheduled-previously)
			          ((and habitp futureschedp)
				   'org-agenda-done)
			          (todayp 'org-scheduled-today)
			          (t 'org-scheduled)))
		      (habitp (and habitp (org-habit-parse-todo (org-element-begin el)))))
	         (org-add-props item props
		   'undone-face face
		   'face (if donep 'org-agenda-done face)
		   'org-marker (org-agenda-new-marker pos)
		   'org-hd-marker (org-agenda-new-marker (line-beginning-position))
		   'type (if pastschedp "past-scheduled" "scheduled")
		   'date (if pastschedp schedule date)
		   'ts-date schedule
		   'warntime warntime
		   'level level
                   'effort effort 'effort-minutes effort-minutes
		   'urgency (if habitp (org-habit-get-urgency habitp)
			      (+ 99 diff (org-get-priority item)))
                   'priority (org-get-priority item)
		   'org-habit-p habitp
		   'todo-state todo-state)
	         (push item scheduled-items)))))))
     :next-re regexp
     :fail-re regexp
     :narrow t)
    (nreverse scheduled-items)))

(defun org-agenda-get-blocks ()
  "Return the date-range information for agenda display."
  (with-no-warnings (defvar date))
  (let* ((props (list 'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'mouse-face 'highlight
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
         ;; Group 1: starting date timestamp without braces
         ;; Group 2: ending date timestamp without braces
	 (regexp (if org-agenda-include-inactive-timestamps
                     org-tr-regexp-both org-tr-regexp))
	 (agenda-today (calendar-absolute-from-gregorian date))
         face marker hdmarker block-list txt start-day end-day
         category level
	 todo-state tags pos head donep inherited-tags effort
	 effort-minutes inactive?)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(org-agenda-skip)
	(setq pos (point))
        (setq inactive? (eq ?\[ (char-after (match-beginning 0))))
	(let ((start-time (match-string 1))
	      (end-time (match-string 2)))
	  (setq start-day (time-to-days
		           (condition-case err
			       (org-time-string-to-time start-time)
		             (error
		              (error
			       "Bad timestamp %S at %d in buffer %S\nError was: %s"
			       start-time
			       pos
			       (current-buffer)
			       (error-message-string err)))))
		end-day (time-to-days
		         (condition-case err
			     (org-time-string-to-time end-time)
		           (error
		            (error
			     "Bad timestamp %S at %d in buffer %S\nError was: %s"
			     end-time
			     pos
                             (current-buffer)
			     (error-message-string err))))))
	  (when (and (> (- agenda-today start-day) -1)
                     (> (- end-day agenda-today) -1))
            ;; Only allow days between the limits, because the normal
	    ;; date stamps will catch the limits.
	    (save-excursion
	      (setq todo-state (org-get-todo-state))
	      (setq donep (member todo-state org-done-keywords))
	      (when (and donep org-agenda-skip-timestamp-if-done)
		(throw :skip t))
              (setq face (if (= start-day end-day)
                             'org-agenda-calendar-event
                           'org-agenda-calendar-daterange))
	      (setq marker (org-agenda-new-marker (point))
		    category (org-get-category))
              (setq effort (save-match-data (or (get-text-property (point) 'effort)
                                                (org-entry-get (point) org-effort-property))))
              (setq effort-minutes (when effort (save-match-data (org-duration-to-minutes effort))))
	      (if (not (re-search-backward org-outline-regexp-bol nil t))
		  (throw :skip nil)
		(goto-char (match-beginning 0))
		(setq hdmarker (org-agenda-new-marker (point))
		      inherited-tags
		      (or (eq org-agenda-show-inherited-tags 'always)
			  (and (listp org-agenda-show-inherited-tags)
			       (memq 'agenda org-agenda-show-inherited-tags))
			  (and (eq org-agenda-show-inherited-tags t)
			       (or (eq org-agenda-use-tag-inheritance t)
				   (memq 'agenda org-agenda-use-tag-inheritance))))
		      tags (org-get-tags nil (not inherited-tags)))
		(setq level (make-string (org-reduced-level (org-outline-level)) ? ))
		(looking-at "\\*+[ \t]+\\(.*\\)")
		(setq head (match-string 1))
		(let ((remove-re
		       (if org-agenda-remove-timeranges-from-blocks
			   (concat
			    "<" (regexp-quote start-time) ".*?>"
			    "--"
			    "<" (regexp-quote end-time) ".*?>")
			 nil)))
                  ;; `org-agenda-format-item' automatically creates a
                  ;; time range when
                  ;; `org-agenda-default-appointment-duration' is
                  ;; non-nil and only start/end time is given.
                  ;; We do not want it here, when the range spans
                  ;; multiple days.
                  (let ((org-agenda-default-appointment-duration nil))
		    (setq txt (org-agenda-format-item
                               (concat
                                (when inactive? org-agenda-inactive-leader)
			        (format
			         (nth (if (= start-day end-day) 0 1)
				      org-agenda-timerange-leaders)
			         (1+ (- agenda-today start-day)) (1+ (- end-day start-day))))
			       (org-add-props head nil
                                 'effort effort
                                 'effort-minutes effort-minutes)
                               level category tags
			       (cond
                                ((and (= start-day agenda-today) (= end-day agenda-today))
			         (concat "<" start-time ">--<" end-time ">"))
                                ((= start-day agenda-today)
			         (concat "<" start-time ">"))
			        ((= end-day agenda-today)
			         (concat "<" end-time ">")))
			       remove-re)))))
	      (org-add-props txt props
                'face face
		'org-marker marker 'org-hd-marker hdmarker
		'type "block" 'date date
		'level level
                'effort effort 'effort-minutes effort-minutes
		'todo-state todo-state
                'urgency (org-get-priority txt)
		'priority (org-get-priority txt))
	      (push txt block-list))))
	(goto-char pos)))
    ;; Sort the entries by expiration date.
    (nreverse block-list)))

(defvar org-search-syntax-table nil
  "Special syntax table for Org search.
In this table, we have single quotes not as word constituents, to
that when \"+Ameli\" is searched as a work, it will also match \"Ameli's\"")

(defun org-search-syntax-table ()
  (unless org-search-syntax-table
    (setq org-search-syntax-table (copy-syntax-table org-mode-syntax-table))
    (modify-syntax-entry ?' "." org-search-syntax-table)
    (modify-syntax-entry ?` "." org-search-syntax-table))
  org-search-syntax-table)

(defun org-agenda-get-regexps ( regexps+ regexps-
                                &optional headline-only todo-only max-outline-level)
  "Return REGEXPS+ matches for agenda display omitting REGEXPS- matches.
REGEXPS+ and REGEXPS- are lists of regexps to match/skip.

When HEADLINE-ONLY is non-nil, match only against headlines.
When TODO-ONLY is non-nil, match only against todo items.
When MAX-OUTLINE-LEVEL is non-nil and not 0, limit matching against
headlines of up to that level."
  (catch 'nextfile
    (let ( last-search-end beg beg1 end str regexp ee category level
           tags txt marker inherited-tags)
      (if (not regexps+)
	  (setq regexp org-outline-regexp-bol)
        (setq regexp (pop regexps+))
        (when headline-only
          (setq regexp (concat org-outline-regexp-bol ".*?"
			       regexp))))
      (with-syntax-table (org-search-syntax-table)
        (let ((case-fold-search t))
	  (goto-char (point-min))
	  (unless (or (org-at-heading-p)
		      (outline-next-heading))
	    (throw 'nextfile nil))
	  (goto-char (max (point-min) (1- (point))))
	  (while (re-search-forward regexp nil t)
            (setq last-search-end (point))
	    (org-back-to-heading t)
	    (while (and (not (zerop max-outline-level))
		        (> (org-reduced-level (org-outline-level))
			   max-outline-level)
		        (forward-line -1)
		        (org-back-to-heading t)))
	    (skip-chars-forward "* ")
            (setq beg (line-beginning-position)
		  beg1 (point)
		  end (progn
		        (outline-next-heading)
		        (while (and (not (zerop max-outline-level))
				    (> (org-reduced-level (org-outline-level))
				       max-outline-level)
				    (forward-line 1)
				    (outline-next-heading)))
		        (point)))
	    (catch :skip
	      (goto-char beg)
	      (org-agenda-skip)
	      (setq str (buffer-substring-no-properties
                         (line-beginning-position)
                         (if headline-only (line-end-position) end)))
	      (mapc (lambda (wr) (when (string-match wr str)
			      (goto-char (1- end))
			      (throw :skip t)))
		    regexps-)
	      (mapc (lambda (wr) (unless (string-match wr str)
			      (goto-char (1- end))
			      (throw :skip t)))
		    (if todo-only
		        (cons (concat "^\\*+[ \t]+"
                                      org-not-done-regexp)
			      regexps+)
		      regexps+))
	      (goto-char beg)
	      (setq marker (org-agenda-new-marker (point))
		    category (org-get-category)
		    level (make-string (org-reduced-level (org-outline-level)) ? )
		    inherited-tags
		    (or (eq org-agenda-show-inherited-tags 'always)
		        (and (listp org-agenda-show-inherited-tags)
			     (memq 'todo org-agenda-show-inherited-tags))
		        (and (eq org-agenda-show-inherited-tags t)
			     (or (eq org-agenda-use-tag-inheritance t)
			         (memq 'todo org-agenda-use-tag-inheritance))))
		    tags (org-get-tags nil (not inherited-tags))
		    txt (org-agenda-format-item
		         ""
		         (buffer-substring-no-properties
                          beg1 (line-end-position))
		         level category tags t))
	      (org-add-props txt nil
                'face nil
		'done-face 'org-agenda-done
		'org-not-done-regexp org-not-done-regexp
		'org-todo-regexp org-todo-regexp
		'org-complex-heading-regexp org-complex-heading-regexp
		'mouse-face 'highlight
		'help-echo "mouse-2 or RET jump to location"
	        'org-marker marker 'org-hd-marker marker
	        'org-todo-regexp org-todo-regexp
	        'level level
	        'org-complex-heading-regexp org-complex-heading-regexp
                'urgency 1000
	        'priority 1000
	        'type "search")
	      (push txt ee)
	      (goto-char (max (1- end) last-search-end))))))
      (nreverse ee))))

(defun org-agenda-get-tags (matcher &optional todo-only)
  "Return entries matching tag MATCHER for agenda display.
MATCHER and TODO-ONLY are passed to `org-scan-tags'."
  (let ((props
         (list 'face 'default
	       'done-face 'org-agenda-done
	       'undone-face 'default
	       'mouse-face 'highlight
	       'org-not-done-regexp org-not-done-regexp
	       'org-todo-regexp org-todo-regexp
	       'org-complex-heading-regexp org-complex-heading-regexp
	       'help-echo
	       (format "mouse-2 or RET jump to Org file %S"
		       (abbreviate-file-name
			(or (buffer-file-name (buffer-base-buffer))
			    (buffer-name (buffer-base-buffer)))))))
        ts-date-pair ts-date ts-date-type txt priority marker
        effort effort-minutes todo category level)
    (org-scan-tags
     (lambda ()
       (goto-char (org-element-begin org-scanner-element))
       (setq ts-date-pair (org-agenda-entry-get-agenda-timestamp org-scanner-element)
	     ts-date (car ts-date-pair)
	     ts-date-type (cdr ts-date-pair)
             effort (org-entry-get org-scanner-element org-effort-property)
             effort-minutes (when effort (save-match-data (org-duration-to-minutes effort)))
             todo (org-element-property :todo-keyword org-scanner-element)
             level (org-element-property :level org-scanner-element)
             category (org-entry-get-with-inheritance "CATEGORY" nil org-scanner-element))
       (setq txt (org-agenda-format-item
		  ""
                  ;; Add `effort' and `effort-minutes'
                  ;; properties for prefix format.
                  (org-add-props
                      (concat
		       (if (eq org-tags-match-list-sublevels 'indented)
			   (make-string (1- level) ?.) "")
		       (org-get-heading))
                      nil
                    'effort effort
                    'effort-minutes effort-minutes)
		  (make-string level ?\s)
		  category
		  org-scanner-tags)
	     priority (org-get-priority txt))
       ;; Now add `effort' and `effort-minutes' to
       ;; full agenda line.
       (setq txt (org-add-props txt nil
                   'effort effort
                   'effort-minutes effort-minutes))
       (setq marker (org-agenda-new-marker))
       (org-add-props txt props
	 'org-marker marker 'org-hd-marker marker 'org-category category
	 'todo-state todo
         'ts-date ts-date
	 'priority priority
         'type (concat "tagsmatch" ts-date-type))
       ;; Return TXT.
       txt)
     matcher todo-only)))

(defvar org-map-continue-from nil
  "Position from where mapping should continue.
Can be set by the action argument to `org-scan-tags' and `org-map-entries'.")

(declare-function org-match-sparse-tree "org-sparse-tree" (&optional todo-only match start-level))
(defun org-scan-tags (action matcher todo-only &optional start-level no-agenda-skip)
  "Scan headline tags with inheritance and produce output ACTION.

ACTION can be `sparse-tree' to produce a sparse tree in the current buffer,
or `agenda' to produce an entry list for an agenda view.  It can also be
a Lisp form or a function that should be called at each matched headline, in
this case the return value is a list of all return values from these calls.

MATCHER is a function accepting three arguments, returning
a non-nil value whenever a given set of tags qualifies a headline
for inclusion.  See `org-make-tags-matcher' for more information.
As a special case, it can also be set to t (respectively nil) in
order to match all (respectively none) headline.

When TODO-ONLY is non-nil, only lines with a TODO keyword are
included in the output.

START-LEVEL can be a string with asterisks, reducing the scope to
headlines matching this string.

When NO-AGENDA-SKIP is non-nil, do not skip entries skipped by
`org-agenda-skip'."
  (cond
   ((eq action 'sparse-tree)
    (require 'org-sparse-tree)
    (org-match-sparse-tree todo-only matcher start-level))
   ((eq action 'agenda)
    (org-agenda-get-tags matcher todo-only))
   (t
    (let* ((heading-re
            (concat ;;FIXME: use cache
             "^"
             (if start-level
	         ;; Get the correct level to match
	         (concat "\\*\\{" (number-to-string start-level) "\\} ")
	       org-outline-regexp)))
	   (org-map-continue-from nil)
           tags-list rtn rtn1 level todo)
      (unless (functionp action) (setq action (list 'lambda nil action)))
      (save-excursion
        (goto-char (point-min))
        (org-element-cache-map
         (lambda (el)
           (goto-char (org-element-begin el))
           (setq todo (org-element-property :todo-keyword el)
                 level (org-element-property :level el)
                 tags-list (org-get-tags el)
                 org-scanner-tags tags-list
                 org-scanner-element el)
           (catch :skip
             (when (and

		    ;; eval matcher only when the todo condition is OK
		    (and (or (not todo-only) (member todo org-todo-keywords-1))
		         (if (functionp matcher)
			     (let ((case-fold-search t) (org-trust-scanner-tags t))
			       (funcall matcher todo tags-list level))
			   matcher))

		    ;; Call the skipper, but return t if it does not
		    ;; skip, so that the `and' form continues evaluating.
		    (progn
		      (unless no-agenda-skip (org-agenda-skip el))
		      t)

		    ;; Check if timestamps are deselecting this entry
		    (or (not todo-only)
		        (and (member todo org-todo-keywords-1)
			     (or (not org-agenda-tags-todo-honor-ignore-options)
			         (not (org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item))))))

	       ;; select this headline
	       (cond
	        ((functionp action)
	         (setq org-map-continue-from nil)
	         (save-excursion
		   (setq rtn1 (funcall action))
		   (push rtn1 rtn)))
	        (t (user-error "Invalid action")))

	       ;; if we are to skip sublevels, jump to end of subtree
	       (unless org-tags-match-list-sublevels
	         (goto-char (1- (org-element-end el))))))
           ;; Get the correct position from where to continue
	   (when org-map-continue-from
             (setq org-element-cache-map-continue-from org-map-continue-from)
	     (goto-char org-map-continue-from))
           ;; Return nil.
           nil)
         :next-re heading-re
         :fail-re heading-re
         :narrow t))
      (nreverse rtn)))))

(declare-function org-add-archive-files "org-archive" (files))
(declare-function org-narrow-to-subtree "org-narrow" (&optional element))
(defun org-map-entries (func &optional match scope &rest skip)
  "Call FUNC at each headline selected by MATCH in SCOPE.

FUNC is a function or a Lisp form.  The function will be called without
arguments, with the cursor positioned at the beginning of the headline.
The return values of all calls to the function will be collected and
returned as a list.

The call to FUNC will be wrapped into a `save-excursion' form, so FUNC
does not need to preserve point.  After evaluation, the cursor will be
moved to the end of the line (presumably of the headline of the
processed entry) and search continues from there.  Under some
circumstances, this may not produce the wanted results.  For example,
if you have removed (e.g. archived) the current (sub)tree it could
mean that the next entry will be skipped entirely.  In such cases, you
can specify the position from where search should continue by making
FUNC set the variable `org-map-continue-from' to the desired buffer
position.

MATCH is a tags/property/todo match as it is used in the agenda tags view.
Only headlines that are matched by this query will be considered during
the iteration.  When MATCH is nil or t, all headlines will be
visited by the iteration.

SCOPE determines the scope of this command.  It can be any of:

nil     The current buffer, respecting the restriction if any
tree    The subtree started with the entry at point
region  The entries within the active region, if any
region-start-level
        The entries within the active region, but only those at
        the same level than the first one.
file    The current buffer, without restriction
file-with-archives
        The current buffer, and any archives associated with it
agenda  All agenda files
agenda-with-archives
        All agenda files with any archive files associated with them
\(file1 file2 ...)
        If this is a list, all files in the list will be scanned

The remaining args are treated as settings for the skipping facilities of
the scanner.  The following items can be given here:

  archive    skip trees with the archive tag
  comment    skip trees with the COMMENT keyword
  function or Emacs Lisp form:
             will be used as value for `org-agenda-skip-function', so
             whenever the function returns a position, FUNC will not be
             called for that entry and search will continue from the
             position returned

If your function needs to retrieve the tags including inherited tags
at the *current* entry, you can use the value of the variable
`org-scanner-tags' which will be much faster than getting the value
with `org-get-tags'.  If your function gets properties with
`org-entry-properties' at the *current* entry, bind `org-trust-scanner-tags'
to t around the call to `org-entry-properties' to get the same speedup.
Note that if your function moves around to retrieve tags and properties at
a *different* entry, you cannot use these techniques."
  (unless (and (or (eq scope 'region) (eq scope 'region-start-level))
	       (not (use-region-p)))
    (let* ((org-agenda-archives-mode nil) ; just to make sure
	   (org-agenda-skip-archived-trees (memq 'archive skip))
	   (org-agenda-skip-comment-trees (memq 'comment skip))
	   (org-agenda-skip-function
	    (car (org-delete-all '(comment archive) skip)))
	   (org-tags-match-list-sublevels t)
	   (start-level (eq scope 'region-start-level))
	   matcher res
	   org-todo-keywords-for-agenda
	   org-done-keywords-for-agenda
	   org-todo-keyword-alist-for-agenda
	   org-tag-alist-for-agenda
	   org--matcher-tags-todo-only)

      (cond
       ((eq match t)   (setq matcher t))
       ((eq match nil) (setq matcher t))
       (t (setq matcher (if match (cdr (org-make-tags-matcher match)) t))))

      (save-excursion
	(save-restriction
	  (cond ((eq scope 'tree)
		 (org-back-to-heading t)
                 (require 'org-narrow)
		 (org-narrow-to-subtree)
		 (setq scope nil))
		((and (or (eq scope 'region) (eq scope 'region-start-level))
		      (use-region-p))
		 ;; If needed, set start-level to a string like "2"
		 (when start-level
		   (save-excursion
		     (goto-char (region-beginning))
		     (unless (org-at-heading-p) (outline-next-heading))
		     (setq start-level (org-current-level))))
		 (narrow-to-region (region-beginning)
				   (save-excursion
				     (goto-char (region-end))
				     (unless (and (bolp) (org-at-heading-p))
				       (outline-next-heading))
				     (point)))
		 (setq scope nil)))

	  (if (not scope)
	      (progn
                ;; Agenda expects a file buffer.  Skip over refreshing
                ;; agenda cache for non-file buffers.
                (when buffer-file-name
		  (org-agenda-prepare-buffers
		   (and buffer-file-name (list (current-buffer)))))
		(setq res
		      (org-scan-tags
		       func matcher org--matcher-tags-todo-only start-level)))
	    ;; Get the right scope
	    (cond
	     ((and scope (listp scope) (symbolp (car scope)))
	      (setq scope (eval scope t)))
	     ((eq scope 'agenda)
	      (setq scope (org-agenda-files t)))
	     ((eq scope 'agenda-with-archives)
	      (setq scope (org-agenda-files t))
              (require 'org-archive)
	      (setq scope (org-add-archive-files scope)))
	     ((eq scope 'file)
	      (setq scope (and buffer-file-name (list buffer-file-name))))
	     ((eq scope 'file-with-archives)
	      (setq scope (org-add-archive-files (list (buffer-file-name))))))
	    (org-agenda-prepare-buffers scope)
	    (dolist (file scope)
	      (with-current-buffer (org-find-base-buffer-visiting file)
		(org-with-wide-buffer
		 (goto-char (point-min))
		 (setq res
		       (append
			res
			(org-scan-tags
			 func matcher org--matcher-tags-todo-only)))))))))
      res)))

(provide 'org-agenda-search)

;;; org-agenda-search.el ends here


