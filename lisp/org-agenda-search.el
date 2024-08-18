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
(require 'org-map)

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
(defun org-agenda-skip (&optional element disable-user-skip)
  "Throw to `:skip' in places that should be skipped.
Also moves point to the end of the skipped region, so that search can
continue from there.

Optional argument ELEMENT contains element at point.

When DISABLE-USER-SKIP is non-nil, ignore `org-agenda-skip-function' and
`org-agenda-skip-function-global."
  (save-match-data
    (when (or
           (if element
               (org-element-type-p element 'comment)
	     (save-excursion
               (goto-char (line-beginning-position))
               (looking-at comment-start-skip)))
	   (and org-agenda-skip-archived-trees (not org-agenda-archives-mode)
	        (or (and (org-in-archived-heading-p nil element)
		         (org-end-of-subtree t element))
		    (and (member org-archive-tag
                                 (org-element-property :tags (org-element-org-data)))
		         (goto-char (point-max)))))
	   (and org-agenda-skip-comment-trees
                (org-in-commented-heading-p nil element)
	        (org-end-of-subtree t element))
           (org-in-src-block-p t element)
           (and (not disable-user-skip)
                (when-let
                    ((to (save-excursion
                           (or (org-eval-form org-agenda-skip-function-global)
		               (org-eval-form org-agenda-skip-function)))))
                  (goto-char to))))
      (throw :skip t))))

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
		    (`(,_ todo) (org-element-not-done-keywords))
		    (`(,_ done) (org-element-done-keywords))
		    (`(,_ any) (org-element-all-todo-keywords))
		    (`(,_ ,(pred atom))
		     (error "Invalid TODO class or type: %S" args))
		    (`(,_ ,(pred (member "*"))) (org-element-all-todo-keywords))
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

(cl-defun org-agenda-map-regexp (regexp action &optional delay-skip-function)
  "Map ACTION over elements containing REGEXP in current buffer.
Collect ACTION non-nil return values into the result.

Honor restriction.

ACTION is a function or sexp that will be called with point at the end
of the match.  When function, it will be called with a single argument
- node at point.  Otherwise, node at point will be dynamically bound
to `node' variable.  ACTION can move point to further location to
continue searching REGEXP from.

All the places that should not appear in agenda views according to
`org-agenda-skip' will be unconditionally ignored.  See
`org-agenda-skip-archived-trees', `org-agenda-archives-mode',
`org-agenda-skip-comment-trees', `org-agenda-skip-function-global',
and `org-agenda-skip-function'.  In addition, all REGEXP matches
inside comments and src blocks will be skipped.

When DELAY-SKIP-FUNCTION is non-nil, `org-agenda-skip-function' and
`org-agenda-skip-function-global' will only run after FUNC, when FUNC
returns non-nil.  If they return non-nil, FUNC result will be discarded."
  (org-map-regexp
   regexp
   (lambda (current-node)
     (let (result)
       (catch :skip
         (unless delay-skip-function (org-agenda-skip current-node))
         (setq result (org-eval-form action '(node) current-node))
         (when (and result delay-skip-function)
           (let ((res result))
             ;; Assign result to be nil in case if we need to skip the
             ;; match.
             (setq result nil)
             (org-agenda-skip current-node)
             ;; Not skipping, keep the result.
             (setq result res))))
       result))))

(defun org-agenda-get-day-entries-1 (date &rest args)
  "Does the work for `org-diary' and `org-agenda' in current buffer.
DATE is date like the one returned by `calendar-current-date'.  ARGS
are symbols indicating which kind of entries should be extracted.  For
details about these, see the documentation of
`org-agenda-entry-types'."
  (setq org-agenda-buffer (or org-agenda-buffer (current-buffer)))
  ;; FIXME `org-agenda-current-date' is obsolete.
  (with-no-warnings
    (setf org-agenda-current-date date))
  ;; Rationalize ARGS.
  ;;
  ;; We use `delq' since `org-uniquify' duplicates ARGS,
  ;; guarding us from modifying `org-agenda-entry-types'.
  (setf args (org-uniquify (or args org-agenda-entry-types)))
  ;; When both :scheduled/deadline and :scheduled*/deadline*
  ;; are present, ignore non-starred versions, as we promise
  ;; in `org-agenda-entry-types' docstring.
  (when (and (memq :scheduled args) (memq :scheduled* args))
    (setf args (delq :scheduled args)))
  (when (and (memq :deadline args) (memq :deadline* args))
    (setf args (delq :deadline args)))
  ;; Make sure `:deadline' comes first in order to populate
  ;; DEADLINES before passing it.
  (cond
   ((memq :deadline args)
    (setf args (cons :deadline (delq :deadline args))))
   ((memq :deadline* args)
    (setf args (cons :deadline* (delq :deadline* args)))))
  ;; Collect list of headlines.  Return them flattened.
  (let ((case-fold-search nil) results deadlines)
    (org-dlet
        ((date date))
      (dolist (arg args (apply #'nconc (nreverse results)))
	(pcase arg
          ;; FIXME: :todo ARG is not documented anywhere and is no
          ;; longer used.
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
	   (push deadlines results)))))))

(defun org-agenda-get-day-entries (file date &rest args)
  "Does the work for `org-diary' and `org-agenda'.
FILE is the path to a file to be checked for entries.  DATE is date like
the one returned by `calendar-current-date'.  ARGS are symbols indicating
which kind of entries should be extracted.  For details about these, see
the documentation of `org-agenda-entry-types'."
  (org-agenda-mapcan-files
   (lambda () (apply #'org-agenda-get-day-entries-1 date args))
   :files-or-buffers file
   :restriction 'agenda-restriction))

(defun org-agenda-todo-custom-ignore-p (time n)
  "Check whether TIME string is farther away than N number of days.
This function is invoked if `org-agenda-todo-ignore-deadlines',
`org-agenda-todo-ignore-scheduled' or
`org-agenda-todo-ignore-timestamp' is set to an integer.
N can also be symbol `future' or `past'."
  (let ((days (org-timestamp-to-now
	       time org-agenda-todo-ignore-time-comparison-use-seconds)))
    (pcase n
      (`future (> days 0))
      (`past (<= days 0))
      (_ (if (>= n 0) (>= days n) (<= days n))))))

(defun org-agenda--skip-todo-due-to-timestamp (&optional epom)
  "Return non-nil when we should skip todo heading at EPOM.
This functions performs checks according to
`org-agenda-todo-ignore-with-date',
`org-agenda-todo-ignore-scheduled',
`org-agenda-todo-ignore-deadlines', `
`org-agenda-todo-ignore-scheduled', and
`org-agenda-todo-ignore-timestamp'."
  (setq epom (org-headline-at-point epom))
  (when (or org-agenda-todo-ignore-with-date
	    org-agenda-todo-ignore-scheduled
	    org-agenda-todo-ignore-deadlines
	    org-agenda-todo-ignore-timestamp)
    (or (and org-agenda-todo-ignore-with-date
             (or (org-entry-get epom "SCHEDULED")
                 (org-entry-get epom "DEADLINE")
                 (org-entry-get epom "TIMESTAMP")))
        (and org-agenda-todo-ignore-scheduled
             (when-let ((scheduled-ts (org-entry-get epom "SCHEDULED")))
               (pcase org-agenda-todo-ignore-scheduled
                 ((and (or `future `past (pred numberp)) n)
                  (org-agenda-todo-custom-ignore-p scheduled-ts n))
                 (_ t))))
        (and org-agenda-todo-ignore-deadlines
             (when-let ((deadline-ts (org-entry-get epom "DEADLINE")))
               (pcase org-agenda-todo-ignore-deadlines
                 (`all t)
                 (`far (not (org-deadline-close-p deadline-ts nil epom)))
                 ((and (or `future `past (pred numberp)) n)
                  (org-agenda-todo-custom-ignore-p deadline-ts n))
                 (_ (org-deadline-close-p deadline-ts nil epom)))))
        (and org-agenda-todo-ignore-timestamp
             (when-let ((ts (org-entry-get epom "TIMESTAMP")))
               (pcase org-agenda-todo-ignore-timestamp
                 ((and (or `future `past (pred numberp)) n)
                  (org-agenda-todo-custom-ignore-p ts n))
                 (_ t)))))))

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

(defun org-agenda-select-todos (&optional selector)
  "Return the list of TODO heading nodes in current buffer for agenda.
When SELECTOR is nil return all the not-done TODO keywords.
When non-nil, it should be a string definining which keywords to choose:
1. A single keyword name
2. A string \"*\", to select all the keywords, including done.
3. \"KWD1|KWD2|KWD3|...\" to select multiple keywords."
  (with-no-warnings
    ;; FIXME: `org-select-this-todo-keyword' is obsolete.
    (unless selector (setq selector org-select-this-todo-keyword)))
  (let* ((case-fold-search nil)
         (selected-kwds (and (stringp selector)
                             (org-split-string selector "|")))
         (regexp (format org-heading-keyword-regexp-format
		         (cond
		          ((and selector (equal selector "*"))
		           (org-todo-regexp))
		          (selector
		           (concat "\\("
			           (mapconcat #'regexp-quote
				              (org-split-string
				               selector "|")
				              "\\|")
			           "\\)"))
		          (t (org-not-done-regexp))))))
    (org-agenda-map-regexp
     regexp
     (lambda (el)
       (setq el (org-headline-at-point el))
       (prog1
           ;; Should we use this heading?
           (and
            (pcase selector
              (`nil (eq 'todo (org-element-property :todo-type el)))
              ("*" (org-element-property :todo-type el))
              (_ (member (org-element-property :todo-keyword el) selected-kwds)))
            (not (org-agenda--skip-todo-due-to-timestamp el))
            el)
         ;; Continue searching from next heading/subtree.
         (goto-char
          (or (and org-agenda-todo-list-sublevels
                   (org-element-end (org-element-current-section el)))
              (org-element-end el)))))
     'delay-user-skip)))

(defun org-agenda-get-todos (&optional selector)
  "Return the TODO information for agenda display.
When SELECTOR is nil return all the not-done TODO keywords.
When non-nil, it should be a string definining which keywords to choose:
1. A single keyword name
2. A string \"*\", to select all the keywords, including done.
3. \"KWD1|KWD2|KWD3|...\" to select multiple keywords."
  (let (txt)
    (mapcar
     (lambda (el)
       (setq txt (org-agenda-format-heading el :dotime 'auto))
       (org-add-props txt
           nil
         'urgency (1+ (get-text-property 0 'priority txt))
         'type (concat "todo" (get-text-property 0 'type txt))))
     (org-agenda-select-todos selector))))

(defun org-agenda--timestamp-to-absolute (timestamp &optional daynr prefer buffer pos)
  "Call `org-time-string-to-absolute' on TIMESTAMP.
The optional arguments DAYNR, PREFER, BUFFER, and POS are passed as is.
TIMESTAMP can be a string or timestamp object.
However, throw `:skip' whenever an error is raised."
  (when (org-element-type-p timestamp 'timestamp)
    (setq timestamp (substring (org-element-property :raw-value timestamp) 1 -1)))
  (condition-case e
      (funcall #'org-time-string-to-absolute timestamp daynr prefer buffer pos)
    (org-diary-sexp-no-match (throw :skip nil))
    (error
     (message "%s; Skipping entry" (error-message-string e))
     (throw :skip nil))))

(defun org-agenda--get-past-repeat (timestamp agenda-day today)
  "Return the past repeat of TIMESTAMP relative to AGENDA-DAY/TODAY.
The relative absolute day is etiher AGENDA-DAY to TODAY depending on
`org-agenda-prefer-last-repeat'.
Return absolute day number."
  (with-no-warnings (defvar date)) ; required for diary timestamp resolution
  ;; A repeating time stamp is shown at its base
  ;; date and every repeated date up to TODAY.  If
  ;; `org-agenda-prefer-last-repeat' is non-nil,
  ;; however, only the last repeat before today
  ;; (inclusive) is shown.
  (catch :skip ; return nil when no match for diary
    (let ((date (calendar-gregorian-from-absolute agenda-day)))
      (org-agenda--timestamp-to-absolute
       timestamp
       (if (or (> agenda-day today)
	       (eq org-agenda-prefer-last-repeat t)
	       (member
                (org-element-property
                 :todo-keyword
                 (org-headline-at-point timestamp))
                org-agenda-prefer-last-repeat))
	   today
         agenda-day)
       'past
       (org-element-property :buffer timestamp)
       (org-element-begin timestamp)))))

(defun org-agenda--get-future-repeat (timestamp agenda-day today)
  "Return the future repeat of TIMESTAMP relative to AGENDA-DAY/TODAY.
The relative absolute day is etiher AGENDA-DAY to TODAY depending on
`org-agenda-show-future-repeats'.
Return absolute day number."
  (with-no-warnings (defvar date)) ; required for diary timestamp resolution
  ;;  Display every repeated date past TODAY
  ;;  (exclusive) unless
  ;;  `org-agenda-show-future-repeats' is nil.  If
  ;;  this variable is set to `next', only display
  ;;  the first repeated date after TODAY
  ;;  (exclusive).
  (let ((date (calendar-gregorian-from-absolute agenda-day))
        (past (org-agenda--get-past-repeat timestamp agenda-day today)))
    (cond
     ((<= agenda-day today) past)
     ((not org-agenda-show-future-repeats) past)
     (t
      (let ((base (if (eq org-agenda-show-future-repeats 'next)
		      (1+ today)
		    agenda-day)))
        (catch :skip ; return nil when no match for diary
	  (org-agenda--timestamp-to-absolute
	   timestamp
           base
           'future
           (org-element-property :buffer timestamp)
           (org-element-begin timestamp))))))))

(defun org-agenda--skip-timestamp-for-date (timestamp agenda-day today)
  "Return non-nil when TIMESTAMP should not be shown on DATE in agenda.
TIMESTAMP is a timestamp object.  AGENDA-DAY is an absolute day in
agenda.  TODAY is the today's absolute day number.

Whether TIMESTAMP should be skipped depends whether TIMESTAMP or its
repetitions around AGENDA-DAY/TODAY matches AGENDA-DAY.  Selection of
repetition center is affected by `org-agenda-prefer-last-repeat' and
`org-agenda-show-future-repeats', which see."
  (let* ((past (org-agenda--get-past-repeat timestamp agenda-day today))
	 (future (org-agenda--get-future-repeat timestamp agenda-day today)))
    ;; Skip non-matching.
    (and (not (equal agenda-day past)) (not (equal agenda-day future)))))

(defvar org-agenda-include-inactive-timestamps nil
  "Non-nil means include inactive time stamps in agenda.
Dynamically scoped.")
(defun org-agenda-select-timestamps (date &optional excluded-headings)
  "Return timestamps matching DATE in current buffer.
DATE is calendar date: list in the form (month day year).

Optional argument EXCLUDED-HEADINGS is a list of :begin positions of
headings to be ignored, even if they contain timestamps.

By default, only consider active timestamps.  When
`org-agenda-include-inactive-timestamps' is non-nil, also include
headings with inactive timestamps.

For repeating timestamps, only consider repeats relative to
`org-today' date.  See `org-agenda-prefer-last-repeat',
`org-agenda-show-future-repeats', and
`org-agenda--skip-timestamp-for-date'.

When `org-agenda-skip-additional-timestamps-same-entry' is non-nil, skip
any additional timestamps under the same heading, except the first
matching."
  (let* ((agenda-day (calendar-absolute-from-gregorian date))
         (today (org-today))
	 ;; Match timestamps set to current agenda date, timestamps
	 ;; with a repeater, and S-exp timestamps.
	 (regexp
	  (concat
	   (if org-agenda-include-inactive-timestamps "[[<]" "<")
	   (regexp-quote
	    (format-time-string
             "%Y-%m-%d" ; We do not use `org-time-stamp-format' to not demand day name in timestamps.
	     (org-encode-time	; DATE bound by calendar
	      0 0 0 (nth 1 date) (car date) (nth 2 date))))
	   "\\|\\(<[0-9]+-[0-9]+-[0-9]+[^>\n]+?\\+[0-9]+[hdwmy]>\\)"
	   "\\|\\(<%%\\(([^>\n]+)\\)\\([^\n>]*\\)>\\)")))
    (org-agenda-map-regexp
     regexp
     (lambda (element)
       (let ((heading (org-headline-at-point element))
             timestamp)
         (when
             (and
              heading ; not before first heading
              ;; Not a planning line
              (not (org-element-type-p element 'planning))
              (not
               (and org-agenda-skip-timestamp-if-done
                    (eq 'done (org-element-property :todo-type heading))))
              ;; Current heading is not one of the provided.
              ;; We compare by :begin because provided headline nodes
              ;; may or may not be copies from cache.
              (not (cl-member (org-element-begin heading) excluded-headings
                            ;; Allow marker/point comparison
                            :test #'=)))
           (setq
            timestamp
            (let ((context (org-element-context element)))
              (if (org-element-type-p context 'timestamp)
                  context
                ;; Directly call the parser since we may match
                ;; timestamps inside property drawers, which are not
                ;; normally recognized.  See `org-at-timestamp-p'.
                (save-excursion
                  (goto-char (match-beginning 0))
                  (org-element-timestamp-parser)))))
           ;; Make sure that TIMESTAMP has reference to the AST
           ;; context.  This is needed for `org-agenda-format-heading'
           ;; to accept the returned timestamp object.
           (when (and timestamp (not (org-element-parent timestamp)))
             (setf (org-element-parent timestamp) element))
           (when
               (and
                timestamp
                ;; Timestamp must be diary, non-range, or time range.
                (or
                 (memq (org-element-property :type timestamp)
                       (if org-agenda-include-inactive-timestamps
                           '(active inactive diary)
                         '(active diary)))
                 (and
                  (eq (org-element-property :range-type timestamp) 'timerange)
                  (memq (org-element-property :type timestamp)
                        (if org-agenda-include-inactive-timestamps
                            '(active-range inactive-range)
                          '(active-range)))))
                (not (org-element-type-p element 'clock))
                (org-at-timestamp-p 'agenda)
                (not (org-agenda--skip-timestamp-for-date
                    timestamp agenda-day today)))
             (prog1 timestamp ; Return value.
               ;; Maybe skip other timestamps under the same heading.
               (when org-agenda-skip-additional-timestamps-same-entry
	         (goto-char
                  (or (org-element-end
                       (org-element-current-section heading))
                      (org-element-end heading))))))))))))

(defun org-agenda-get-timestamps (&optional deadlines)
  "Return the date stamp information for agenda display.
Optional argument DEADLINES is a list of deadline items to be
displayed in agenda view.

See `org-agenda-select-timestamps' for more details on how timestamps
are selected."
  (with-no-warnings (defvar date))
  (let (item habit?)
    (mapcar
     (lambda (timestamp)
       (setq habit? (and (fboundp 'org-is-habit-p)
                         (org-is-habit-p timestamp)))
       (setq item
             (org-agenda-format-heading
              timestamp
              :scheduling-info
              (and (eq (org-element-property :type timestamp) 'inactive)
                   (not habit?)
                   org-agenda-inactive-leader)
              :dotime (org-element-property :raw-value timestamp)
              :remove-re org-ts-regexp))
       (org-add-props item nil
         'urgency (if habit?
 		      (org-habit-get-urgency
                       (org-habit-parse-todo
                        (org-element-begin timestamp)))
		    (org-get-priority item))
         'face 'org-agenda-calendar-event
         'date date
         'ts-date (calendar-absolute-from-gregorian date)
         'type "timestamp"))
     (org-agenda-select-timestamps
      date
      (mapcar (lambda (item) (get-text-property 0 'org-hd-marker item))
              deadlines)))))

(defun org-agenda-select-sexps (date)
  "Return a list of diary sexps matching DATE in current buffer.
Each sexp node will have its :diary-sexp-entry value set to its
resolved value, as returned by `org-diary-sexp-entry'."
  (require 'diary-lib)
  (with-no-warnings (defvar date) (defvar entry))
  (let (b sexp sexp-entry result)
    (org-agenda-map-regexp
     "^&?%%("
     (lambda (sexp-element)
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
         ;; Copy element to avoid overwriting cached data.
         (setq sexp-element (org-element-put-property
                             (org-element-copy sexp-element)
                             :parent
                             (org-element-parent sexp-element)))
         (org-element-put-property sexp-element :diary-sexp-entry result)))
     ;; We do not run `org-agenda-skip' right away because every single sexp
     ;; in the buffer is matched here, unlike day-specific search
     ;; in ordinary timestamps.  Most of the sexps will not match
     ;; the agenda day and it is quicker to run `org-agenda-skip' only for
     ;; matching sexps later on.
     'delay-user-skip)))

(defun org-agenda-get-sexps ()
  "Return the sexp information for agenda display."
  (require 'diary-lib)
  (with-no-warnings (defvar date))
  (let (scheduling-info diary-sexp-entry agenda-line)
    (mapcan
     (lambda (sexp-element)
       (setq diary-sexp-entry (org-element-property :diary-sexp-entry sexp-element))
       (setq scheduling-info nil)
       ;; A single sexp may define multiple agenda lines.
       (mapcar
        (lambda (sexp-record)
          ;; Maybe extract `org-agenda-diary-sexp-prefix'
	  (when (and org-agenda-diary-sexp-prefix
		     (string-match org-agenda-diary-sexp-prefix sexp-record))
	    (setq scheduling-info (match-string 0 sexp-record)
		  sexp-record (replace-match "" nil nil sexp-record)))
          ;; Mark non-empty records
	  (if (string-match "\\S-" sexp-record)
	      (setq agenda-line sexp-record)
	    (setq agenda-line "SEXP entry returned empty string"))
	  (setq agenda-line
                (org-agenda-format-heading
                 sexp-element
                 :scheduling-info scheduling-info
                 :headline-format agenda-line
                 :dotime 'auto))
	  (org-add-props agenda-line
              nil
            'face 'org-agenda-calendar-sexp
	    'date date
            'type "sexp"))
        (if (stringp diary-sexp-entry) (list diary-sexp-entry)
	  diary-sexp-entry)))
     (org-agenda-select-sexps date))))

(defun org-agenda-select-closed (date)
  "Return a list of closed planning nodes matching DATE in current buffer."
  (org-agenda-map-regexp
   (concat "\\<" org-closed-string
           " *\\["
	   (regexp-quote
	    (format-time-string
             "%Y-%m-%d" ; We do not use `org-time-stamp-format' to not demand day name in timestamps.
	     (org-encode-time  ; DATE bound by calendar
	      0 0 0 (nth 1 date) (car date) (nth 2 date)))))
   (lambda (planning)
     (when (and (org-element-type-p planning 'planning)
                (org-element-property :closed planning))
       planning))))

(defun org-agenda-select-clock (date)
  "Return a list of clocks matching DATE in current buffer.
Each clock will have its `:clock-note' property set to the first
paragraph of the following list entry.
Ignore clocks before the first heading."
  (let (result-clock next-element next-paragraph)
    (org-agenda-map-regexp
     (concat "\\<" org-clock-string
             " *\\["
	     (regexp-quote
	      (format-time-string
               "%Y-%m-%d" ; We do not use `org-time-stamp-format' to not demand day name in timestamps.
	       (org-encode-time  ; DATE bound by calendar
	        0 0 0 (nth 1 date) (car date) (nth 2 date)))))
     (lambda (clock)
       (when (and (org-element-type-p clock 'clock)
                  (org-element-lineage clock 'headline))
         (setq result-clock
               (org-element-put-property
                (org-element-copy clock)
                :parent (org-element-parent clock))
               next-element (org-element-at-point (org-element-end clock)))
         (when (and (org-element-type-p next-element 'plain-list)
                    (org-element-type-p
                     (setq next-paragraph
                           (org-element-at-point (+ 2 (org-element-begin next-element))))
                     'paragraph))
           (org-element-put-property
            result-clock :clock-note
            (buffer-substring
             (org-element-contents-begin next-paragraph)
             (org-element-contents-end next-paragraph))))
         result-clock)))))

(defun org-agenda-select-state (date)
  "Return a list of state change notes matching DATE in current buffer.
The return values are paragraph elements containing the notes with
`:new-todo-state' property set to the new state listed in the note.
Ignore state notes before the first heading."
  (require 'org-log-note)
  (declare-function org-state-note-re "org-log-note" (&optional date))
  (let (element-copy)
    (org-agenda-map-regexp
     (org-state-note-re date)
     (lambda (element)
       (when (and (org-element-lineage element 'item)
                  (org-element-lineage element 'headline))
         (setq element-copy (org-element-copy element))
         (org-element-put-property
          element-copy
          :parent (org-element-parent element))
         (org-element-put-property
          element-copy
          :new-todo-state (match-string 1))
         element)))))

(defalias 'org-get-closed #'org-agenda-get-progress)
(defun org-agenda-get-progress (&optional entry-types)
  "Return the logged TODO entries for agenda display.
ENTRY-TYPES, when non-nil, limits the types of entries to be selected.
The allowed values are those of `org-agenda-log-mode-items', which
see.  When ENTRY-TYPES is nil, use `org-agenda-log-mode-items'."
  (with-no-warnings (defvar date))
  (let* ((items (if (consp entry-types) entry-types
		  org-agenda-log-mode-items))
         (org-agenda-search-headline-for-time nil))
    (sort ; Sort by item position in the buffer
     (nconc
      ;; Closed entries
      (mapcar
       (lambda (planning)
         (org-add-props
             (org-agenda-format-heading
              planning
              :scheduling-info "Closed:    "
              :dotime (org-element-property :closed planning))
             nil
           'face 'org-agenda-done
	   'type "closed"
           'date date
	   'undone-face 'org-warning))
       (when (memq 'closed items) (org-agenda-select-closed date)))
      ;; Clock entries
      (mapcar
       (lambda (clock)
         (org-add-props
             (org-agenda-format-heading
              clock
              :scheduling-info
              (format
               "Clocked:   (%s)"
               (or (org-element-property :duration clock)
                   "-"))
              :headline-format
              (if-let ((extra
                        (and org-agenda-log-mode-add-notes
                             (car (string-lines (org-element-property
                                                 :clock-note clock))))))
                  `(headline " - " ,extra tags)
                `(headline tags))
              :dotime (org-element-property :value clock))
             nil
           'face 'org-agenda-done
	   'type "clock"
           'date date
	   'undone-face 'org-warning))
       (when (memq 'clock items) (org-agenda-select-clock date)))
      ;; State change notes
      (let (note-lines)
        (mapcar
         (lambda (paragraph)
           (setq note-lines
                 (string-lines
                  (buffer-substring
                   (org-element-contents-begin paragraph)
                   (org-element-contents-end paragraph))))
           (org-add-props
               (org-agenda-format-heading
                paragraph
                :scheduling-info
                (format
                 "State:     (%s)"
                 (or (org-element-property :new-todo-state paragraph) ""))
                :headline-format
                (if-let ((extra
                          (and org-agenda-log-mode-add-notes
                               ;; Line 2
                               (cadr note-lines))))
                    `(headline " - " ,extra tags)
                  `(headline tags))
                :dotime (car note-lines))
               nil
             'face 'org-agenda-done
	     'type "state"
             'date date
	     'undone-face 'org-warning))
         (when (memq 'state items) (org-agenda-select-state date)))))
     ;; Sort function.
     (lambda (line) (get-text-property 0 'org-agenda-marker line)))))

(defun org-deadline-close-p (timestamp-string &optional ndays epom)
  "Is the time in TIMESTAMP-STRING close to the current date?
EPOM is an element inside the containing heading."
  (setq ndays (or ndays (org-get-wdays timestamp-string)))
  (and (<= (org-timestamp-to-now timestamp-string) ndays)
       (not (org-entry-is-done-p epom))))

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
     ((and (org-element-type-p ts 'timestamp)
           (org-element-property :warning-unit ts))
      ;; lead time is specified.
      (floor (* (org-element-property :warning-value ts)
                (pcase (org-element-property :warning-unit ts)
                  (`hour 0.041667) (`day 1) (`week 7)
                  (`month 30.4) (`year 365.25)))))
     ((and (stringp ts) (string-match "-\\([0-9]+\\)\\([hdwmy]\\)\\(\\'\\|>\\| \\)" ts))
      ;; lead time is specified.
      (floor (* (string-to-number (match-string 1 ts))
		(cdr (assoc (match-string 2 ts)
			    '(("d" . 1)    ("w" . 7)
			      ("m" . 30.4) ("y" . 365.25)
			      ("h" . 0.041667)))))))
     ;; go for the default.
     (t tv))))

(defun org-agenda-select-deadlines (date &optional with-hour)
  "Return a list of planning nodes with deadline for DATE.
Each node will have its properties :past-repeat and
:future-repeat set to the absolute day of past/future deadline repeat,
according to `org-agenda-prefer-last-repeat' and
`org-agenda-show-future-repeats'.

The deadlines are selected according to
`org-agenda-skip-deadline-prewarning-if-scheduled',
`org-deadline-warning-days', and `org-agenda-skip-deadline-if-done',
which see.

When WITH-HOUR is non-nil, only consider deadlines with an hour
specification like [h]h:mm."
  (let* ((today (org-today))
	 (today? (org-agenda-today-p date))
	 (agenda-day (calendar-absolute-from-gregorian date))
         deadline scheduled)
    (org-agenda-map-regexp
     (if with-hour org-deadline-time-hour-regexp org-deadline-time-regexp)
     (lambda (planning)
       (setq deadline (org-element-property :deadline planning))
       (when (and deadline
                  ;; Only consider active timestamp values.
                  (memq (org-element-property :type deadline)
                        '(diary active active-range))
                  (or (not with-hour)
                      (org-element-property :hour-start deadline)
                      (org-element-property :hour-end deadline)))
         (setq scheduled (org-element-property :scheduled planning))
         (when-let*
             (;; PAST-REPEAT is the deadline date for the entry.  It is
	      ;; either the base date or the last repeat, according
	      ;; to `org-agenda-prefer-last-repeat'.
              (past-repeat (org-agenda--get-past-repeat
                            deadline agenda-day today))
	      ;; FUTURE-REPEAT is the future repeat closest from
	      ;; AGENDA-DAY, according to
	      ;; `org-agenda-show-future-repeats'. If the latter is
	      ;; nil, or if the time stamp has no repeat part,
	      ;; default to DEADLINE.
              (future-repeat (org-agenda--get-future-repeat
                              deadline agenda-day today))
              (max-warning-days
	       (cond
		((or (not scheduled)
                     (not org-agenda-skip-deadline-prewarning-if-scheduled))
                 most-positive-fixnum)
		;; The current item has a scheduled date, so
		;; evaluate its prewarning lead time.
		((integerp org-agenda-skip-deadline-prewarning-if-scheduled)
		 ;; Use global prewarning-restart lead time.
		 org-agenda-skip-deadline-prewarning-if-scheduled)
		((eq org-agenda-skip-deadline-prewarning-if-scheduled
		     'pre-scheduled)
		 ;; Set pre-warning to no earlier than SCHEDULED.
		 (min (- past-repeat
			 (org-agenda--timestamp-to-absolute scheduled))
		      org-deadline-warning-days))
		;; Set pre-warning to deadline.
		(t 0)))
              (warning-days (min max-warning-days (org-get-wdays deadline))))
           ;; Copy element to avoid overwriting cached data.
           (setq planning (org-element-put-property
                           (org-element-copy planning)
                           :parent
                           (org-element-parent planning)))
           ;; Save properties.
           (org-element-put-property planning :past-repeat past-repeat)
           (org-element-put-property planning :future-repeat future-repeat)
           (org-element-put-property planning :warning-days warning-days)
           (cond
	    ;; Possibly skip done tasks.
            ((and (or org-agenda-skip-deadline-if-done
		      (/= past-repeat agenda-day))
                  (eq 'done (org-element-property
                             :todo-type
                             (org-headline-at-point planning))))
             nil)
	    ;; Only display deadlines at their base date, at future
	    ;; repeat occurrences or in today agenda.
	    ((= agenda-day past-repeat) planning)
	    ((equal agenda-day future-repeat) planning)
	    ((not today?) nil)
	    ;; Upcoming deadline: display within warning period WARNING-DAYS.
	    ((> past-repeat agenda-day)
             (unless (> (- past-repeat agenda-day)
                        warning-days)
               planning))
	    ;; Overdue deadline: warn about it for
	    ;; `org-deadline-past-days' duration.
	    (t (unless
                   (< org-deadline-past-days
                      (- agenda-day past-repeat))
                 planning)))))))))

(defun org-agenda-get-deadlines (&optional with-hour)
  "Return the deadline information for agenda display.
When WITH-HOUR is non-nil, only return deadlines with an hour
specification like [h]h:mm."
  (with-no-warnings (defvar date))
  (let ((agenda-day (calendar-absolute-from-gregorian date))
        (today (org-today))
	(today? (org-agenda-today-p date)))
    (mapcar
     (lambda (planning)
       (let* ((past-repeat (org-element-property :past-repeat planning))
              (future-repeat (org-element-property :future-repeat planning))
              (warning-days (org-element-property :warning-days planning))
              (diff (- past-repeat agenda-day))
              (time
	       (cond
	        ;; No time of day designation if it is only
	        ;; a reminder.
	        ((and (/= agenda-day past-repeat)
                      (not (equal agenda-day future-repeat)))
                 nil)
	        ((let ((raw (org-element-property
                             :raw-value
                             (org-element-property :deadline planning))))
                   (and
                    (string-match " \\([012]?[0-9]:[0-9][0-9]\\)" raw)
	            (concat (substring raw (match-beginning 1)) " "))))
	        (t 'time)))
	      (item
               (org-agenda-format-heading
                planning
                :scheduling-info
                ;; Insert appropriate suffixes before deadlines.
	        ;; Those only apply to today agenda.
	        (pcase-let ((`(,now ,future ,past)
			     org-agenda-deadline-leaders))
	          (cond
	           ((and today? (< past-repeat today)) (format past (- diff)))
	           ((and today? (> past-repeat today)) (format future diff))
	           (t now)))
                :dotime time))
	      (face (org-agenda-deadline-face
		     (- 1 (/ (float diff) (max warning-days 1)))))
	      (upcoming? (and today? (> past-repeat today))))
         (org-add-props item
             nil
	   'ts-date past-repeat
	   'urgency
	   ;; Adjust urgency to today reminders about deadlines.
	   ;; Overdue deadlines get the highest urgency
	   ;; increase, then imminent deadlines and eventually
	   ;; more distant deadlines.
	   (let ((adjust (if today? (- diff) 0)))
	     (+ adjust (org-get-priority item)))
	   'type (if upcoming? "upcoming-deadline" "deadline")
	   'date (if upcoming? date past-repeat)
	   'face (if (eq 'done
                         (org-element-property
                          :todo-type (org-headline-at-point planning)))
                     'org-agenda-done
                   face)
	   'undone-face face)))
     (org-agenda-select-deadlines date with-hour))))

(defvar org-habit-scheduled-past-days) ; org-habit.el
(declare-function org-habit-parse-todo "org-habit" (&optional pom))
(declare-function org-habit-get-urgency "org-habit" (habit &optional moment))
(defun org-agenda-select-scheduled (date &optional with-hour displayed-deadlines)
  "Return a list of planning nodes scheduled for DATE.
Each node will have its properties :past-repeat and
:future-repeat set to the absolute day of past/future schedule repeat,
according to `org-agenda-prefer-last-repeat' and
`org-agenda-show-future-repeats'.

The scheduled items are selected according to habit status,
`org-agenda-skip-scheduled-delay-if-deadline',
`org-scheduled-delay-days', `org-habit-show-all-today',
`org-habit-show-habits-only-for-today', `org-habit-show-habits',
`org-habit-scheduled-past-days', `org-agenda-skip-scheduled-if-done',
`org-agenda-skip-scheduled-repeats-after-deadline', and
`org-agenda-skip-scheduled-if-deadline-is-shown', which see.

When WITH-HOUR is non-nil, only consider scheduled with an hour
specification like [h]h:mm.

DISPLAYED-DEADLINE is a list of :begin properties of headlines where
deadlines will be displayed.  They are used to honor
`org-agenda-skip-scheduled-if-deadline-is-shown'."
  (let* ((today (org-today))
	 (today? (org-agenda-today-p date))
	 (agenda-day (calendar-absolute-from-gregorian date))
         deadline scheduled headline)
    (org-agenda-map-regexp
     (if with-hour org-scheduled-time-hour-regexp org-scheduled-time-regexp)
     (lambda (planning)
       (setq scheduled (org-element-property :scheduled planning))
       (setq headline (org-headline-at-point planning))
       (when (and scheduled
                  ;; Only consider active timestamp values.
                  (memq (org-element-property :type scheduled)
                        '(diary active active-range))
                  (or (not with-hour)
                      (org-element-property :hour-start scheduled)
                      (org-element-property :hour-end scheduled)))
         (setq deadline (org-element-property :deadline planning))
         (let*
             (;; PAST-REPEAT is the scheduled date for the entry.  It is
	      ;; either the base date or the last repeat, according
	      ;; to `org-agenda-prefer-last-repeat'.
              (past-repeat (org-agenda--get-past-repeat
                            scheduled agenda-day today))
	      ;; FUTURE-REPEAT is the future repeat closest from
	      ;; AGENDA-DAY, according to
	      ;; `org-agenda-show-future-repeats'. If the latter is
	      ;; nil, or if the time stamp has no repeat part,
	      ;; default to SCHEDULED.
              (future-repeat (org-agenda--get-future-repeat
                              scheduled agenda-day today))
              (habitp (and (fboundp 'org-is-habit-p)
                           (string= "habit" (org-element-property :STYLE headline))))
              (max-delay-days
	       (cond
		((or (not deadline) (not past-repeat)
                     (not org-agenda-skip-scheduled-delay-if-deadline))
                 most-positive-fixnum)
		;; The current item has a deadline date, so
		;; evaluate its delay time.
		((integerp org-agenda-skip-scheduled-delay-if-deadline)
                 ;; Use global delay time.
		 (- org-agenda-skip-scheduled-delay-if-deadline))
		((eq org-agenda-skip-scheduled-delay-if-deadline
		     'post-deadline)
		 ;; Set pre-warning to no earlier than SCHEDULED.
		 (min (- past-repeat
			 (org-agenda--timestamp-to-absolute deadline))
		      org-scheduled-delay-days))
		;; Set pre-warning to deadline.
		(t 0)))
              (delay-days (min max-delay-days (org-get-wdays scheduled t))))
           (when past-repeat
             ;; Copy element to avoid overwriting cached data.
             (setq planning (org-element-put-property
                             (org-element-copy planning)
                             :parent
                             (org-element-parent planning)))
             ;; Save properties.
             (org-element-put-property planning :past-repeat past-repeat)
             (org-element-put-property planning :future-repeat future-repeat)
             (catch :skip
	       ;; Display scheduled items at base date (PAST-REPEAT),
	       ;; today if scheduled before the agenda date, and at any
	       ;; repeat past today.  However, skip delayed items and
	       ;; items that have been displayed for more than
	       ;; `org-scheduled-past-days'.
               (unless (and today? habitp
		            (bound-and-true-p org-habit-show-all-today))
	         (when (or (and (> delay-days 0)
                                (< (- agenda-day past-repeat) delay-days))
		           (> (- agenda-day past-repeat)
                              (or (and habitp org-habit-scheduled-past-days)
				  org-scheduled-past-days))
		           (> past-repeat agenda-day)
		           (and (/= agenda-day past-repeat)
			        (/= agenda-day today)
			        (not (equal agenda-day future-repeat))))
	           (throw :skip nil)))
               ;; Possibly skip done tasks.
	       (when (and (eq 'done (org-element-property :todo-type headline))
		          (or org-agenda-skip-scheduled-if-done
			      (/= past-repeat agenda-day)))
	         (throw :skip nil))
               ;; Skip entry if it already appears as a deadline, per
	       ;; `org-agenda-skip-scheduled-if-deadline-is-shown'.  This
	       ;; doesn't apply to habits.
               (when (or org-agenda-skip-scheduled-repeats-after-deadline
                         ;; FIXME: Backwards-compatibility.
                         (eq org-agenda-skip-scheduled-if-deadline-is-shown
                             'repeated-after-deadline))
                 (let ((deadline-day (org-agenda--timestamp-to-absolute deadline)))
	           (when (and (or (<= past-repeat deadline-day)
                                  (not (= past-repeat agenda-day)))
                              (> agenda-day deadline-day))
                     (throw :skip nil))))
               (unless (or habitp
                           (not (cl-member
                               (org-element-begin headline)
                               displayed-deadlines
                               ;; Allow marker/point comparison
                               :test #'=)))
	         (when (pcase org-agenda-skip-scheduled-if-deadline-is-shown
		         (`not-today (< past-repeat today))
		         (`t t)
		         (_ nil))
	           (throw :skip nil)))
               ;; Skip habits if `org-habit-show-habits' is nil, or if we
	       ;; only show them for today.  Also skip done habits.
	       (when (and habitp
		          (or (eq 'done (org-element-property :todo-type headline))
			      (not (bound-and-true-p org-habit-show-habits))
			      (and (not today?)
			           (bound-and-true-p
			            org-habit-show-habits-only-for-today))))
	         (throw :skip nil))
               ;; No reason to skip anything.  Return planning.
               planning))))))))

(defun org-agenda-get-scheduled (&optional deadlines with-hour)
  "Return the scheduled information for agenda display.
Optional argument DEADLINES is a list of deadline items to be
displayed in agenda view.  When WITH-HOUR is non-nil, only return
scheduled items with an hour specification like [h]h:mm."
  (with-no-warnings (defvar date))
  (let ((agenda-day (calendar-absolute-from-gregorian date))
        (today? (org-agenda-today-p date)))
    (mapcar
     (lambda (planning)
       (let* ((past-repeat (org-element-property :past-repeat planning))
              (future-repeat (org-element-property :future-repeat planning))
              (pastschedp (< past-repeat agenda-day))
              (diff (- agenda-day past-repeat))
              (habitp (and (fboundp 'org-is-habit-p)
                           (string= "habit"
                                    (org-element-property
                                     :STYLE
                                     (org-headline-at-point planning)))))
              (time
	       (cond
		;; No time of day designation if it is only a
		;; reminder, except for habits, which always show
		;; the time of day.  Habits are an exception
		;; because if there is a time of day, that is
		;; interpreted to mean they should usually happen
		;; then, even if doing the habit was missed.
	        ((and (not habitp)
                      (/= agenda-day past-repeat)
                      (not (equal agenda-day future-repeat)))
                 nil)
	        ((let ((raw (org-element-property
                             :raw-value
                             (org-element-property :scheduled planning))))
                   (and
                    (string-match " \\([012]?[0-9]:[0-9][0-9]\\)" raw)
	            (concat (substring raw (match-beginning 1)) " "))))
	        (t 'time)))
	      (item
               (org-agenda-format-heading
                planning
                :scheduling-info
                (unless habitp
                  (pcase-let ((`(,first ,past) org-agenda-scheduled-leaders))
		    ;; Show a reminder of a past scheduled today.
		    (if (and today? pastschedp)
			(format past diff)
		      first)))
                :dotime time))
              (face (cond ((and (not habitp) pastschedp)
			   'org-scheduled-previously)
			  ((and habitp (> past-repeat agenda-day))
			   'org-agenda-done)
			  (today? 'org-scheduled-today)
			  (t 'org-scheduled)))
              (habit-obj (and habitp (org-habit-parse-todo (org-element-begin planning)))))
         (org-add-props item
             nil
	   'ts-date past-repeat
	   'urgency
           (if habit-obj (org-habit-get-urgency habit-obj)
	     (+ 99 diff (org-get-priority item)))
	   'type (if pastschedp "past-scheduled" "scheduled")
	   'date (if pastschedp past-repeat date)
	   'face (if (eq 'done
                         (org-element-property
                          :todo-type (org-headline-at-point planning)))
                     'org-agenda-done
                   face)
	   'undone-face face)))
     (org-agenda-select-scheduled
      date
      with-hour
      (mapcar
       (lambda (item)
         (get-text-property 0 'org-hd-marker item))
       deadlines)))))

(defun org-agenda-select-blocks (date)
  "Return a list of range timestamps for agenda display at DATE."
  (let ((agenda-day (calendar-absolute-from-gregorian date))
        start-day end-day)
    (org-agenda-map-regexp
     (if org-agenda-include-inactive-timestamps
         org-tr-regexp-both org-tr-regexp)
     (lambda (element)
       (let ((heading (org-headline-at-point element))
             timestamp)
         (when (and heading ; not before first heading
                    (not (and org-agenda-skip-timestamp-if-done
                            (eq 'done (org-element-property
                                       :todo-type heading)))))
           (setq
            timestamp
            (let ((context (org-element-context element)))
              (if (org-element-type-p context 'timestamp)
                  context
                ;; Directly call the parser since we may match
                ;; timestamps inside property drawers, which are not
                ;; normally recognized.  See `org-at-timestamp-p'.
                (save-excursion
                  (goto-char (match-beginning 0))
                  (org-element-timestamp-parser)))))
           ;; Make sure that TIMESTAMP has reference to the AST
           ;; context.  This is needed for `org-agenda-format-heading'
           ;; to accept the returned timestamp object.
           (when (and timestamp (not (org-element-parent timestamp)))
             (setf (org-element-parent timestamp) element))
           (when (and timestamp
                      (memq (org-element-property :type timestamp)
                            (if org-agenda-include-inactive-timestamps
                                '(active-range inactive-range)
                              '(active-range))))
             (setq start-day (time-to-days (org-timestamp-to-time timestamp))
                   end-day (time-to-days (org-timestamp-to-time timestamp 'end)))
             (when (and (> (- agenda-day start-day) -1)
                        (> (- end-day agenda-day) -1))
               timestamp))))))))

(defun org-agenda-get-blocks ()
  "Return the date-range information for agenda display."
  (with-no-warnings (defvar date))
  (let ((agenda-day (calendar-absolute-from-gregorian date))
        start-day end-day)
    (mapcar
     (lambda (timestamp)
       (setq start-day (time-to-days (org-timestamp-to-time timestamp))
             end-day (time-to-days (org-timestamp-to-time timestamp 'end)))
       (org-add-props
           (org-agenda-format-heading
            timestamp
            :scheduling-info
            (concat
             (when (eq (org-element-property :type timestamp) 'inactive-range)
               org-agenda-inactive-leader)
	     (format
	      (nth (if (= start-day end-day) 0 1)
		   org-agenda-timerange-leaders)
	      (1+ (- agenda-day start-day)) (1+ (- end-day start-day))))
            :dotime
            (cond
             ((and (= start-day agenda-day) (= end-day agenda-day))
	      (org-element-property :raw-value timestamp))
             ((= start-day agenda-day)
              (org-element-property
               :raw-value
               (org-timestamp-split-range timestamp)))
	     ((= end-day agenda-day)
	      (org-element-property
               :raw-value
               (org-timestamp-split-range timestamp 'end))))
            :default-duration nil
            :remove-re
            (regexp-quote (org-element-property
                           :raw-value timestamp)))
           nil
         'face (if (= start-day end-day)
                   'org-agenda-calendar-event
                 'org-agenda-calendar-daterange)
	 'type "block"
         'date date))
     (org-agenda-select-blocks date))))

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

(defun org-agenda-select-regexps
    ( regexps+ regexps-
      &optional headline-only todo-only max-outline-level)
  "Return headlines containing REGEXP+ matches but not REGEXP- matches.
REGEXPS+ and REGEXPS- are lists of regexps to match/skip.

When HEADLINE-ONLY is non-nil, match only against headline titles.
When TODO-ONLY is non-nil, match only against todo items.
When MAX-OUTLINE-LEVEL is non-nil and not 0, limit matching against
headlines of up to that level."
  (unless max-outline-level (setq max-outline-level 0))
  (with-syntax-table (org-search-syntax-table)
    (let ((case-fold-search t)
          (headline-re
           (org-with-limited-levels
            (if (zerop max-outline-level)
                org-outline-regexp-bol
              (org-headline-re
               (org-reduced-level
                (org-get-valid-level max-outline-level))))))
          headline limit)
      (org-agenda-map-regexp
       (if (and headline-only regexps+)
           (concat headline-re ".*?" (pop regexps+))
         (or (pop regexps+) headline-re))
       (lambda (element)
         (setq headline (org-headline-at-point element))
         (when (and headline
                    (or (zerop max-outline-level)
                        (<= (org-element-property :level headline)
                           max-outline-level))
                    (or (not todo-only)
                        (eq 'todo (org-element-property :todo-type headline))))
           (goto-char (org-element-begin headline))
           (skip-chars-forward "* ")
           (and (re-search-forward headline-re nil 'move)
                (goto-char (match-beginning 0)))
           (setq limit
                 (if (or headline-only todo-only)
                     (or (org-element-contents-begin headline)
                         (org-element-end headline))
                   (point)))
           (catch :skip
             (save-excursion
               (dolist (regexp- regexps-)
                 (goto-char (org-element-begin headline))
                 (when (re-search-forward regexp- limit t)
                   (throw :skip t)))
               (dolist (regexp+ regexps+)
                 (goto-char (org-element-begin headline))
                 (unless (re-search-forward regexp+ limit t)
                   (throw :skip t)))
               headline))))))))

(defun org-agenda-get-regexps ( regexps+ regexps-
                                &optional headline-only todo-only max-outline-level)
  "Return REGEXPS+ matches for agenda display omitting REGEXPS- matches.
REGEXPS+ and REGEXPS- are lists of regexps to match/skip.

When HEADLINE-ONLY is non-nil, match only against headlines.
When TODO-ONLY is non-nil, match only against todo items.
When MAX-OUTLINE-LEVEL is non-nil and not 0, limit matching against
headlines of up to that level."
  (mapcar
   (lambda (headline)
     (org-add-props
         (org-agenda-format-heading headline :dotime 'auto)
         nil
       'face nil
       'urgency 1000
       'priority 1000
       'type "search"))
   (org-agenda-select-regexps
    regexps+ regexps-
    headline-only todo-only max-outline-level)))

(defun org-agenda-get-tags (matcher &optional todo-only)
  "Return entries matching tag MATCHER for agenda display.
MATCHER and TODO-ONLY are passed to `org-scan-tags'."
  (let (agenda-line)
    (org-scan-tags
     (lambda ()
       (setq agenda-line (org-agenda-format-heading org-scanner-element))
       (org-add-props agenda-line
           nil
         'type (concat "tagsmatch" (get-text-property 0 'type agenda-line))))
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
		    (and (or (not todo-only) (org-element-todo-keyword-p todo))
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
		        (and (org-element-todo-keyword-p todo)
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
    ;; FIXME: `org-todo-keyword-alist-for-agenda' and
    ;; `org-tag-alist-for-agenda' are obsolete.
    (with-no-warnings
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
        res))))

(provide 'org-agenda-search)

;;; org-agenda-search.el ends here


