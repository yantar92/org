;;; org-timestamp.el --- Timestamps API              -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>

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

;; This file implements tools to operate on, or create, timestamps.

;;; Code:

(require 'calendar)
(require 'org-macs)
(require 'org-fold)


(declare-function calendar-check-holidays "holidays" (date))
(declare-function org-element-timestamp-parser "org-element" ())
(declare-function org-element-create "org-element-ast" (type &optional props &rest children))
(declare-function org-element-property "org-element-ast" (property node))
(declare-function org-element-copy "org-element-ast" (datum))
(declare-function org-element-put-property "org-element-ast" (node property value))
(declare-function org-element-interpret-data "org-element" (data))
(declare-function calendar-iso-to-absolute "cal-iso" (date))
(declare-function org-restart-font-lock "org" ())
(declare-function org-clock-update-time-maybe "org-clock" ())
(declare-function org-at-table-p "org-table" ())
(declare-function org-table-align "org-table" ())
(declare-function org-element-type "org-element-ast" (node &optional anonymous))
(declare-function org-element-context "org-element" (&optional element))

(defvar org-tr-regexp-both)
(defvar org-tr-regexp)
(defvar org-ts-regexp-both)
(defvar org-ts-regexp)
(defvar org-ts-regexp-inactive)
(defvar org-scheduled-time-regexp)
(defvar org-deadline-time-regexp)
(defvar org-closed-time-regexp)
(defvar org-deadline-string)
(defvar org-scheduled-string)
(defvar org-ts-regexp3)
(defvar org-element--timestamp-regexp)
(defvar org-ts-regexp2)
(defvar org-ts-regexp0)
(defvar org-clock-string)
(defvar org-timestamp-formats)

(defvar org-time-was-given nil) ; dynamically scoped parameter
(defvar org-end-time-was-given nil) ; dynamically scoped parameter

(defvaralias 'org-time-stamp-custom-formats 'org-timestamp-custom-formats)
(defcustom org-timestamp-custom-formats
  '("%m/%d/%y %a" . "%m/%d/%y %a %H:%M") ; american
  "Custom formats for time stamps.

See `format-time-string' for the syntax.

These are overlaid over the default ISO format if the variable
`org-display-custom-times' is set.  Time like %H:%M should be at the
end of the second format.  The custom formats are also honored by export
commands, if custom time display is turned on at the time of export.

This variable also affects how timestamps are exported.

Leading \"<\" and trailing \">\" pair will be stripped from the format
strings."
  :group 'org-time
  :package-version '(Org . "9.6")
  :type '(cons string string))

(defcustom org-display-custom-times nil
  "Non-nil means overlay custom formats over all time stamps.
The formats are defined through the variable `org-timestamp-custom-formats'.
To turn this on on a per-file basis, insert anywhere in the file:
   #+STARTUP: customtime"
  :group 'org-time
  :type 'sexp)
(make-variable-buffer-local 'org-display-custom-times)

(defvaralias 'org-time-stamp-rounding-minutes 'org-timestamp-rounding-minutes)
(defcustom org-timestamp-rounding-minutes '(0 5)
  "Number of minutes to round time stamps to.
\\<org-mode-map>\
These are two values, the first applies when first creating a time stamp.
The second applies when changing it with the commands `S-up' and `S-down'.
When changing the time stamp, this means that it will change in steps
of N minutes, as given by the second value.

When a setting is 0 or 1, insert the time unmodified.  Useful rounding
numbers should be factors of 60, so for example 5, 10, 15.

When this is larger than 1, you can still force an exact time stamp by using
a double prefix argument to a time stamp command like \
`\\[org-timestamp]' or `\\[org-timestamp-inactive],
and by using a prefix arg to `S-up/down' to specify the exact number
of minutes to shift."
  :group 'org-time
  :get (lambda (var) ; Make sure both elements are there
	 (if (integerp (default-value var))
	     (list (default-value var) 5)
	   (default-value var)))
  :type '(list
	  (integer :tag "when inserting times")
	  (integer :tag "when modifying times")))

;; Normalize old customizations of this variable.
(when (integerp org-timestamp-rounding-minutes)
  (setq org-timestamp-rounding-minutes
	(list org-timestamp-rounding-minutes
	      org-timestamp-rounding-minutes)))

(defcustom org-extend-today-until 0
  "The hour when your day really ends.  Must be an integer.
This has influence for the following applications:
- When switching the agenda to \"today\".  If it is still earlier than
  the time given here, the day recognized as TODAY is actually yesterday.
- When a date is read from the user and it is still before the time given
  here, the current date and time will be assumed to be yesterday, 23:59.
  Also, timestamps inserted in capture templates follow this rule.

IMPORTANT:  This is a feature whose implementation is and likely will
remain incomplete.  Really, it is only here because past midnight seems to
be the favorite working time of John Wiegley :-)"
  :group 'org-time
  :type 'integer)

(defcustom org-use-effective-time nil
  "If non-nil, consider `org-extend-today-until' when creating timestamps.
For example, if `org-extend-today-until' is 8, and it's 4am, then the
\"effective time\" of any timestamps between midnight and 8am will be
23:59 of the previous day."
  :group 'org-time
  :version "24.1"
  :type 'boolean)

(defcustom org-use-last-clock-out-time-as-effective-time nil
  "When non-nil, use the last clock out time for `org-todo'.
Note that this option has precedence over the combined use of
`org-use-effective-time' and `org-extend-today-until'."
  :group 'org-time
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-read-date-force-compatible-dates t
  "Should date/time prompt force dates that are guaranteed to work in Emacs?

Depending on the system Emacs is running on, certain dates cannot
be represented with the type used internally to represent time.
Dates between 1970-1-1 and 2038-1-1 can always be represented
correctly.  Some systems allow for earlier dates, some for later,
some for both.  One way to find out is to insert any date into an
Org buffer, putting the cursor on the year and hitting S-up and
S-down to test the range.

When this variable is set to t, the date/time prompt will not let
you specify dates outside the 1970-2037 range, so it is certain that
these dates will work in whatever version of Emacs you are
running, and also that you can move a file from one Emacs implementation
to another.  Whenever Org is forcing the year for you, it will display
a message and beep.

When this variable is nil, Org will check if the date is
representable in the specific Emacs implementation you are using.
If not, it will force a year, usually the current year, and beep
to remind you.  Currently this setting is not recommended because
the likelihood that you will open your Org files in an Emacs that
has limited date range is not negligible.

A workaround for this problem is to use diary sexp dates for time
stamps outside of this range."
  :group 'org-time
  :version "24.1"
  :type 'boolean)

(defcustom org-read-date-display-live t
  "Non-nil means display current interpretation of date prompt live.
This display will be in an overlay, in the minibuffer.  Note that
live display is only active when `org-read-date-popup-calendar'
is non-nil."
  :group 'org-time
  :type 'boolean)

(defvaralias 'org-popup-calendar-for-date-prompt
  'org-read-date-popup-calendar)

(defcustom org-read-date-popup-calendar t
  "Non-nil means pop up a calendar when prompting for a date.
In the calendar, the date can be selected with mouse-1.  However, the
minibuffer will also be active, and you can simply enter the date as well.
When nil, only the minibuffer will be available."
  :group 'org-time
  :type 'boolean)

(defcustom org-edit-timestamp-down-means-later nil
  "Non-nil means S-down will increase the time in a time stamp.
When nil, S-up will increase."
  :group 'org-time
  :type 'boolean)

(defcustom org-calendar-follow-timestamp-change t
  "Non-nil means make the calendar window follow timestamp changes.
When a timestamp is modified and the calendar window is visible, it will be
moved to the new date."
  :group 'org-time
  :type 'boolean)

(defcustom org-read-date-prefer-future t
  "Non-nil means assume future for incomplete date input from user.
This affects the following situations:
1. The user gives a month but not a year.
   For example, if it is April and you enter \"feb 2\", this will be read
   as Feb 2, *next* year.  \"May 5\", however, will be this year.
2. The user gives a day, but no month.
   For example, if today is the 15th, and you enter \"3\", Org will read
   this as the third of *next* month.  However, if you enter \"17\",
   it will be considered as *this* month.

If you set this variable to the symbol `time', then also the following
will work:

3. If the user gives a time.
   If the time is before now, it will be interpreted as tomorrow.

Currently none of this works for ISO week specifications.

When this option is nil, the current day, month and year will always be
used as defaults.

See also `org-agenda-jump-prefer-future'."
  :group 'org-time
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Check month and day" t)
	  (const :tag "Check month, day, and time" time)))

;;;; Timestamp API

;; This section contains tools to operate on, or create, timestamp
;; objects, as returned by, e.g. `org-element-context'.

(declare-function org-clock-get-last-clock-out-time "org-clock" ())

(defun org-current-effective-time ()
  "Return current time adjusted for `org-extend-today-until' variable."
  (let* ((ct (org-current-time))
	 (dct (decode-time ct))
	 (ct1
	  (cond
	   (org-use-last-clock-out-time-as-effective-time
	    (or (org-clock-get-last-clock-out-time) ct))
	   ((and org-use-effective-time (< (nth 2 dct) org-extend-today-until))
	    (org-encode-time 0 59 23 (1- (nth 3 dct)) (nth 4 dct) (nth 5 dct)))
	   (t ct))))
    ct1))

(defun org-timestamp-from-string (s)
  "Convert Org timestamp S, as a string, into a timestamp object.
Return nil if S is not a valid timestamp string."
  (when (org-string-nw-p s)
    (with-temp-buffer
      (save-excursion (insert s))
      (org-element-timestamp-parser))))

(defun org-timestamp-from-time (time &optional with-time inactive)
  "Convert a time value into a timestamp object.

TIME is an Emacs internal time representation, as returned, e.g.,
by `current-time'.

When optional argument WITH-TIME is non-nil, return a timestamp
object with a time part, i.e., with hours and minutes.

Return an inactive timestamp if INACTIVE is non-nil.  Otherwise,
return an active timestamp."
  (pcase-let ((`(,_ ,minute ,hour ,day ,month ,year . ,_) (decode-time time)))
    (org-element-create 'timestamp
			(list :type (if inactive 'inactive 'active)
			      :year-start year
			      :month-start month
			      :day-start day
			      :hour-start (and with-time hour)
			      :minute-start (and with-time minute)))))

(defun org-timestamp-to-time (timestamp &optional end)
  "Convert TIMESTAMP object into an Emacs internal time value.
Use end of date range or time range when END is non-nil.
Otherwise, use its start."
  (org-encode-time
   (append '(0)
           (mapcar
            (lambda (prop) (or (org-element-property prop timestamp) 0))
            (if end '(:minute-end :hour-end :day-end :month-end :year-end)
              '(:minute-start :hour-start :day-start :month-start
                              :year-start)))
           '(nil -1 nil))))

(defun org-timestamp-has-time-p (timestamp)
  "Non-nil when TIMESTAMP has a time specified."
  (org-element-property :hour-start timestamp))

(defun org-format-timestamp (timestamp format &optional end utc)
  "Format a TIMESTAMP object into a string.

FORMAT is a format specifier to be passed to
`format-time-string'.

When optional argument END is non-nil, use end of date-range or
time-range, if possible.

When optional argument UTC is non-nil, time is be expressed as
Universal Time."
  (format-time-string format (org-timestamp-to-time timestamp end)
		      (and utc t)))

(defun org-timestamp-split-range (timestamp &optional end)
  "Extract a TIMESTAMP object from a date or time range.

END, when non-nil, means extract the end of the range.
Otherwise, extract its start.

Return a new timestamp object."
  (let ((type (org-element-property :type timestamp)))
    (if (memq type '(active inactive diary)) timestamp
      (let ((split-ts (org-element-copy timestamp)))
	;; Set new type.
	(org-element-put-property
	 split-ts :type (if (eq type 'active-range) 'active 'inactive))
        (org-element-put-property split-ts :range-type nil)
	;; Copy start properties over end properties if END is
	;; non-nil.  Otherwise, copy end properties over `start' ones.
	(let ((p-alist '((:minute-start . :minute-end)
			 (:hour-start . :hour-end)
			 (:day-start . :day-end)
			 (:month-start . :month-end)
			 (:year-start . :year-end))))
	  (dolist (p-cell p-alist)
	    (org-element-put-property
	     split-ts
	     (funcall (if end #'car #'cdr) p-cell)
	     (org-element-property
	      (funcall (if end #'cdr #'car) p-cell) split-ts)))
	  ;; Eventually refresh `:raw-value'.
	  (org-element-put-property split-ts :raw-value nil)
	  (org-element-put-property
	   split-ts :raw-value (org-element-interpret-data split-ts)))))))

(defun org-timestamp-translate (timestamp &optional boundary)
  "Translate TIMESTAMP object to custom format.

Format string is defined in `org-timestamp-custom-formats',
which see.

When optional argument BOUNDARY is non-nil, it is either the
symbol `start' or `end'.  In this case, only translate the
starting or ending part of TIMESTAMP if it is a date or time
range.  Otherwise, translate both parts.

Return timestamp as-is if `org-display-custom-times' is nil or if
it has a `diary' type."
  (let ((type (org-element-property :type timestamp)))
    (if (or (not org-display-custom-times) (eq type 'diary))
	(org-element-interpret-data timestamp)
      (let ((fmt (org-time-stamp-format
                  (org-timestamp-has-time-p timestamp) nil 'custom)))
	(if (and (not boundary) (memq type '(active-range inactive-range)))
	    (concat (org-format-timestamp timestamp fmt)
		    "--"
		    (org-format-timestamp timestamp fmt t))
	  (org-format-timestamp timestamp fmt (eq boundary 'end)))))))

;;;; Timestamps

(defun org-at-date-range-p (&optional inactive-ok)
  "Non-nil if point is inside a date range.

When optional argument INACTIVE-OK is non-nil, also consider
inactive time ranges.

When this function returns a non-nil value, match data is set
according to `org-tr-regexp-both' or `org-tr-regexp', depending
on INACTIVE-OK."
  (save-excursion
    (catch 'exit
      (let ((pos (point)))
	(skip-chars-backward "^[<\r\n")
	(skip-chars-backward "<[")
	(and (looking-at (if inactive-ok org-tr-regexp-both org-tr-regexp))
	     (>= (match-end 0) pos)
	     (throw 'exit t))
	(skip-chars-backward "^<[\r\n")
	(skip-chars-backward "<[")
	(and (looking-at (if inactive-ok org-tr-regexp-both org-tr-regexp))
	     (>= (match-end 0) pos)
	     (throw 'exit t)))
      nil)))

(defvar org-last-changed-timestamp nil)
(defvar org-last-inserted-timestamp nil
  "The last time stamp inserted with `org-insert-timestamp'.")

(defalias 'org-time-stamp #'org-timestamp)
(defun org-timestamp (arg &optional inactive)
  "Prompt for a date/time and insert a time stamp.

If the user specifies a time like HH:MM or if this command is
called with at least one prefix argument, the time stamp contains
the date and the time.  Otherwise, only the date is included.

All parts of a date not specified by the user are filled in from
the timestamp at point, if any, or the current date/time
otherwise.

If there is already a timestamp at the cursor, it is replaced.

With two universal prefix arguments, insert an active timestamp
with the current time without prompting the user.

When called from Lisp, the timestamp is inactive if INACTIVE is
non-nil."
  (interactive "P")
  (let* ((ts (cond
	      ((org-at-date-range-p t)
	       (match-string (if (< (point) (- (match-beginning 2) 2)) 1 2)))
	      ((org-at-timestamp-p 'lax) (match-string 0))))
	 ;; Default time is either the timestamp at point or today.
	 ;; When entering a range, only the range start is considered.
         (default-time (and ts (org-time-string-to-time ts)))
         (default-input (and ts (org-get-compact-tod ts)))
         (repeater (and ts
			(string-match "\\([.+-]+[0-9]+[hdwmy] ?\\)+" ts)
			(match-string 0 ts)))
	 org-time-was-given
	 org-end-time-was-given
	 (time
	  (if (equal arg '(16)) (current-time)
	    ;; Preserve `this-command' and `last-command'.
	    (let ((this-command this-command)
		  (last-command last-command))
	      (org-read-date
	       arg 'totime nil nil default-time default-input
	       inactive)))))
    (cond
     ((and ts
           (memq last-command '( org-time-stamp org-time-stamp-inactive
                                 org-timestamp org-timestamp-inactive))
           (memq this-command '( org-time-stamp org-time-stamp-inactive
                                 org-timestamp org-timestamp-inactive)))
      (insert "--")
      (org-insert-timestamp time (or org-time-was-given arg) inactive))
     (ts
      ;; Make sure we're on a timestamp.  When in the middle of a date
      ;; range, move arbitrarily to range end.
      (unless (org-at-timestamp-p 'lax)
	(skip-chars-forward "-")
	(org-at-timestamp-p 'lax))
      (replace-match "")
      (setq org-last-changed-timestamp
	    (org-insert-timestamp
	     time (or org-time-was-given arg)
	     inactive nil nil (list org-end-time-was-given)))
      (when repeater
	(backward-char)
	(insert " " repeater)
	(setq org-last-changed-timestamp
	      (concat (substring org-last-inserted-timestamp 0 -1)
		      " " repeater ">")))
      (message "Timestamp updated"))
     ((equal arg '(16)) (org-insert-timestamp time t inactive))
     (t (org-insert-timestamp
	 time (or org-time-was-given arg) inactive nil nil
	 (list org-end-time-was-given))))))

;; FIXME: can we use this for something else, like computing time differences?
(defun org-get-compact-tod (s)
  (when (string-match "\\(\\([012]?[0-9]\\):\\([0-5][0-9]\\)\\)\\(-\\(\\([012]?[0-9]\\):\\([0-5][0-9]\\)\\)\\)?" s)
    (let* ((t1 (match-string 1 s))
	   (h1 (string-to-number (match-string 2 s)))
	   (m1 (string-to-number (match-string 3 s)))
	   (t2 (and (match-end 4) (match-string 5 s)))
	   (h2 (and t2 (string-to-number (match-string 6 s))))
	   (m2 (and t2 (string-to-number (match-string 7 s))))
	   dh dm)
      (if (not t2)
	  t1
	(setq dh (- h2 h1) dm (- m2 m1))
	(when (< dm 0) (setq dm (+ dm 60) dh (1- dh)))
	(concat t1 "+" (number-to-string dh)
		(and (/= 0 dm) (format ":%02d" dm)))))))

(defalias 'org-time-stamp-inactive #'org-timestamp-inactive)
(defun org-timestamp-inactive (&optional arg)
  "Insert an inactive time stamp.

An inactive time stamp is enclosed in square brackets instead of
angle brackets.  It is inactive in the sense that it does not
trigger agenda entries.  So these are more for recording a
certain time/date.

If the user specifies a time like HH:MM or if this command is called with
at least one prefix argument, the time stamp contains the date and the time.
Otherwise, only the date is included.

When called with two universal prefix arguments, insert an inactive time stamp
with the current time without prompting the user."
  (interactive "P")
  (org-timestamp arg 'inactive))

(defvar org-date-ovl (make-overlay 1 1))
(overlay-put org-date-ovl 'face 'org-date-selected)
(delete-overlay org-date-ovl)

(defvar org-ans1) ; dynamically scoped parameter
(defvar org-ans2) ; dynamically scoped parameter

(defvar org-plain-time-of-day-regexp) ; defined below

(defvar org-overriding-default-time nil) ; dynamically scoped
(defvar org-read-date-overlay nil)
(defvar org-read-date-history nil)
(defvar org-read-date-final-answer nil)
(defvar org-read-date-analyze-futurep nil)
(defvar org-read-date-analyze-forced-year nil)
(defvar org-read-date-inactive)
(defvar org-def)
(defvar org-defdecode)
(defvar org-with-time)

(defvar org-read-date-minibuffer-local-map)

(declare-function org-defkey "org-keys" (keymap key def))

(defvar calendar-setup)			; Dynamically scoped.
(defun org-read-date (&optional with-time to-time from-string prompt
				default-time default-input inactive)
  "Read a date, possibly a time, and make things smooth for the user.
The prompt will suggest to enter an ISO date, but you can also enter anything
which will at least partially be understood by `parse-time-string'.
Unrecognized parts of the date will default to the current day, month, year,
hour and minute.  If this command is called to replace a timestamp at point,
or to enter the second timestamp of a range, the default time is taken
from the existing stamp.  Furthermore, the command prefers the future,
so if you are giving a date where the year is not given, and the day-month
combination is already past in the current year, it will assume you
mean next year.  For details, see the manual.  A few examples:

  3-2-5         --> 2003-02-05
  feb 15        --> currentyear-02-15
  2/15          --> currentyear-02-15
  sep 12 9      --> 2009-09-12
  12:45         --> today 12:45
  22 sept 0:34  --> currentyear-09-22 0:34
  12            --> currentyear-currentmonth-12
  Fri           --> nearest Friday after today
  -Tue          --> last Tuesday
  etc.

Furthermore you can specify a relative date by giving, as the *first* thing
in the input:  a plus/minus sign, a number and a letter [hdwmy] to indicate
change in days weeks, months, years.
With a single plus or minus, the date is relative to today.  With a double
plus or minus, it is relative to the date in DEFAULT-TIME.  E.g.
  +4d           --> four days from today
  +4            --> same as above
  +2w           --> two weeks from today
  ++5           --> five days from default date

The function understands only English month and weekday abbreviations.

While prompting, a calendar is popped up - you can also select the
date with the mouse (button 1).  The calendar shows a period of three
months.  To scroll it to other months, use the keys `>' and `<'.
There are many other calendar navigation commands available, see
Info node `(org) The date/time prompt' for a full list.

If you don't like the calendar, turn it off with
       (setq org-read-date-popup-calendar nil)

With optional argument TO-TIME, the date will immediately be converted
to an internal time.
With an optional argument WITH-TIME, the prompt will suggest to
also insert a time.  Note that when WITH-TIME is not set, you can
still enter a time, and this function will inform the calling routine
about this change.  The calling routine may then choose to change the
format used to insert the time stamp into the buffer to include the time.
With optional argument FROM-STRING, read from this string instead from
the user.  PROMPT can overwrite the default prompt.  DEFAULT-TIME is
the time/date that is used for everything that is not specified by the
user."
  (require 'parse-time)
  (let* ((org-with-time with-time)
	 (org-timestamp-rounding-minutes
	  (if (equal org-with-time '(16))
	      '(0 0)
	    org-timestamp-rounding-minutes))
	 (ct (org-current-time))
	 (org-def (or org-overriding-default-time default-time ct))
	 (org-defdecode (decode-time org-def))
         (cur-frame (selected-frame))
	 (mouse-autoselect-window nil)	; Don't let the mouse jump
	 (calendar-setup
	  (and (eq calendar-setup 'calendar-only) 'calendar-only))
	 (calendar-move-hook nil)
	 (calendar-view-diary-initially-flag nil)
	 (calendar-view-holidays-initially-flag nil)
	 ans (org-ans0 "") org-ans1 org-ans2 final cal-frame)
    ;; Rationalize `org-def' and `org-defdecode', if required.
    ;; Only consider `org-extend-today-until' when explicit reference
    ;; time is not given.
    (when (and (not default-time)
               (not org-overriding-default-time)
               (< (nth 2 org-defdecode) org-extend-today-until))
      (setf (nth 2 org-defdecode) -1)
      (setf (nth 1 org-defdecode) 59)
      (setq org-def (org-encode-time org-defdecode))
      (setq org-defdecode (decode-time org-def)))
    (let* ((timestr (format-time-string
		     (if org-with-time "%Y-%m-%d %H:%M" "%Y-%m-%d")
		     org-def))
	   (prompt (concat (if prompt (concat prompt " ") "")
			   (format "Date+time [%s]: " timestr))))
      (cond
       (from-string (setq ans from-string))
       (org-read-date-popup-calendar
	(save-excursion
	  (save-window-excursion
	    (calendar)
	    (when (eq calendar-setup 'calendar-only)
	      (setq cal-frame
		    (window-frame (get-buffer-window calendar-buffer 'visible)))
	      (select-frame cal-frame))
	    (org-eval-in-calendar '(setq cursor-type nil) t)
	    (unwind-protect
		(progn
		  (calendar-forward-day (- (time-to-days org-def)
					   (calendar-absolute-from-gregorian
					    (calendar-current-date))))
		  (org-eval-in-calendar nil t)
		  (let* ((old-map (current-local-map))
			 (map (copy-keymap calendar-mode-map))
			 (minibuffer-local-map
			  (copy-keymap org-read-date-minibuffer-local-map)))
		    (org-defkey map (kbd "RET") 'org-calendar-select)
		    (org-defkey map [mouse-1] 'org-calendar-select-mouse)
		    (org-defkey map [mouse-2] 'org-calendar-select-mouse)
		    (unwind-protect
			(progn
			  (use-local-map map)
			  (setq org-read-date-inactive inactive)
			  (add-hook 'post-command-hook 'org-read-date-display)
			  (setq org-ans0
				(read-string prompt
					     default-input
					     'org-read-date-history
					     nil))
			  ;; org-ans0: from prompt
			  ;; org-ans1: from mouse click
			  ;; org-ans2: from calendar motion
			  (setq ans
				(concat org-ans0 " " (or org-ans1 org-ans2))))
		      (remove-hook 'post-command-hook 'org-read-date-display)
		      (use-local-map old-map)
		      (when org-read-date-overlay
			(delete-overlay org-read-date-overlay)
			(setq org-read-date-overlay nil)))))
	      (bury-buffer calendar-buffer)
	      (when cal-frame
		(delete-frame cal-frame)
		(select-frame-set-input-focus cur-frame))))))

       (t				; Naked prompt only
	(unwind-protect
	    (setq ans (read-string prompt default-input
				   'org-read-date-history timestr))
	  (when org-read-date-overlay
	    (delete-overlay org-read-date-overlay)
	    (setq org-read-date-overlay nil))))))

    (setq final (org-read-date-analyze ans org-def org-defdecode))

    (when org-read-date-analyze-forced-year
      (message "Year was forced into %s"
	       (if org-read-date-force-compatible-dates
		   "compatible range (1970-2037)"
		 "range representable on this machine"))
      (ding))

    (setq final (org-encode-time final))

    (setq org-read-date-final-answer ans)

    (if to-time
	final
      ;; This round-trip gets rid of 34th of August and stuff like that....
      (setq final (decode-time final))
      (if (and (boundp 'org-time-was-given) org-time-was-given)
	  (format "%04d-%02d-%02d %02d:%02d"
		  (nth 5 final) (nth 4 final) (nth 3 final)
		  (nth 2 final) (nth 1 final))
	(format "%04d-%02d-%02d" (nth 5 final) (nth 4 final) (nth 3 final))))))

(defun org-read-date-display ()
  "Display the current date prompt interpretation in the minibuffer."
  (when org-read-date-display-live
    (when org-read-date-overlay
      (delete-overlay org-read-date-overlay))
    (when (minibufferp (current-buffer))
      (save-excursion
	(end-of-line 1)
	(while (not (equal (buffer-substring
			    (max (point-min) (- (point) 4)) (point))
			   "    "))
	  (insert " ")))
      (let* ((ans (concat (buffer-substring (line-beginning-position)
                                            (point-max))
			  " " (or org-ans1 org-ans2)))
	     (org-end-time-was-given nil)
	     (f (org-read-date-analyze ans org-def org-defdecode))
	     (fmt (org-time-stamp-format
                   (or org-with-time
                       (and (boundp 'org-time-was-given) org-time-was-given))
                   org-read-date-inactive
                   org-display-custom-times))
	     (txt (format-time-string fmt (org-encode-time f)))
	     (txt (concat "=> " txt)))
	(when (and org-end-time-was-given
		   (string-match org-plain-time-of-day-regexp txt))
	  (setq txt (concat (substring txt 0 (match-end 0)) "-"
			    org-end-time-was-given
			    (substring txt (match-end 0)))))
	(when org-read-date-analyze-futurep
	  (setq txt (concat txt " (=>F)")))
	(setq org-read-date-overlay
              (make-overlay (1- (line-end-position)) (line-end-position)))
	(org-overlay-display org-read-date-overlay txt 'secondary-selection)))))

(defun org-read-date-analyze (ans def defdecode)
  "Analyze the combined answer of the date prompt."
  ;; FIXME: cleanup and comment
  (let ((org-def def)
	(org-defdecode defdecode)
	(nowdecode (decode-time))
	delta deltan deltaw deltadef year month day
	hour minute second wday pm h2 m2 tl wday1
	iso-year iso-weekday iso-week iso-date futurep kill-year)
    (setq org-read-date-analyze-futurep nil
	  org-read-date-analyze-forced-year nil)
    (when (string-match "\\`[ \t]*\\.[ \t]*\\'" ans)
      (setq ans "+0"))

    (when (setq delta (org-read-date-get-relative ans nil org-def))
      (setq ans (replace-match "" t t ans)
	    deltan (car delta)
	    deltaw (nth 1 delta)
	    deltadef (nth 2 delta)))

    ;; Check if there is an iso week date in there.  If yes, store the
    ;; info and postpone interpreting it until the rest of the parsing
    ;; is done.
    (when (string-match "\\<\\(?:\\([0-9]+\\)-\\)?[wW]\\([0-9]\\{1,2\\}\\)\\(?:-\\([0-6]\\)\\)?\\([ \t]\\|$\\)" ans)
      (setq iso-year (when (match-end 1)
		       (org-small-year-to-year
			(string-to-number (match-string 1 ans))))
	    iso-weekday (when (match-end 3)
			  (string-to-number (match-string 3 ans)))
	    iso-week (string-to-number (match-string 2 ans)))
      (setq ans (replace-match "" t t ans)))

    ;; Help matching ISO dates with single digit month or day, like 2006-8-11.
    (when (string-match
	   "^ *\\(\\([0-9]+\\)-\\)?\\([0-1]?[0-9]\\)-\\([0-3]?[0-9]\\)\\([^-0-9]\\|$\\)" ans)
      (setq year (if (match-end 2)
		     (string-to-number (match-string 2 ans))
		   (progn (setq kill-year t)
			  (string-to-number (format-time-string "%Y"))))
	    month (string-to-number (match-string 3 ans))
	    day (string-to-number (match-string 4 ans)))
      (setq year (org-small-year-to-year year))
      (setq ans (replace-match (format "%04d-%02d-%02d\\5" year month day)
			       t nil ans)))

    ;; Help matching dotted european dates
    (when (string-match
	   "^ *\\(3[01]\\|0?[1-9]\\|[12][0-9]\\)\\. ?\\(0?[1-9]\\|1[012]\\)\\.\\( ?[1-9][0-9]\\{3\\}\\)?" ans)
      (setq year (if (match-end 3) (string-to-number (match-string 3 ans))
		   (setq kill-year t)
		   (string-to-number (format-time-string "%Y")))
	    day (string-to-number (match-string 1 ans))
	    month (string-to-number (match-string 2 ans))
	    ans (replace-match (format "%04d-%02d-%02d" year month day)
			       t nil ans)))

    ;; Help matching american dates, like 5/30 or 5/30/7
    (when (string-match
	   "^ *\\(0?[1-9]\\|1[012]\\)/\\(0?[1-9]\\|[12][0-9]\\|3[01]\\)\\(/\\([0-9]+\\)\\)?\\([^/0-9]\\|$\\)" ans)
      (setq year (if (match-end 4)
		     (string-to-number (match-string 4 ans))
		   (progn (setq kill-year t)
			  (string-to-number (format-time-string "%Y"))))
	    month (string-to-number (match-string 1 ans))
	    day (string-to-number (match-string 2 ans)))
      (setq year (org-small-year-to-year year))
      (setq ans (replace-match (format "%04d-%02d-%02d\\5" year month day)
			       t nil ans)))
    ;; Help matching am/pm times, because `parse-time-string' does not do that.
    ;; If there is a time with am/pm, and *no* time without it, we convert
    ;; so that matching will be successful.
    (cl-loop for i from 1 to 2 do	; twice, for end time as well
	     (when (and (not (string-match "\\(\\`\\|[^+]\\)[012]?[0-9]:[0-9][0-9]\\([ \t\n]\\|$\\)" ans))
			(string-match "\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)?\\(am\\|AM\\|pm\\|PM\\)\\>" ans))
	       (setq hour (string-to-number (match-string 1 ans))
		     minute (if (match-end 3)
				(string-to-number (match-string 3 ans))
			      0)
		     pm (equal ?p
			       (string-to-char (downcase (match-string 4 ans)))))
	       (if (and (= hour 12) (not pm))
		   (setq hour 0)
		 (when (and pm (< hour 12)) (setq hour (+ 12 hour))))
	       (setq ans (replace-match (format "%02d:%02d" hour minute)
					t t ans))))

    ;; Help matching HHhMM times, similarly as for am/pm times.
    (cl-loop for i from 1 to 2 do	; twice, for end time as well
	     (when (and (not (string-match "\\(\\`\\|[^+]\\)[012]?[0-9]:[0-9][0-9]\\([ \t\n]\\|$\\)" ans))
			(string-match "\\(?:\\(?1:[012]?[0-9]\\)?h\\(?2:[0-5][0-9]\\)\\)\\|\\(?:\\(?1:[012]?[0-9]\\)h\\(?2:[0-5][0-9]\\)?\\)\\>" ans))
	       (setq hour (if (match-end 1)
			      (string-to-number (match-string 1 ans))
			    0)
		     minute (if (match-end 2)
				(string-to-number (match-string 2 ans))
			      0))
	       (setq ans (replace-match (format "%02d:%02d" hour minute)
					t t ans))))

    ;; Check if a time range is given as a duration
    (when (string-match "\\([012]?[0-9]\\):\\([0-6][0-9]\\)\\+\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)?" ans)
      (setq hour (string-to-number (match-string 1 ans))
	    h2 (+ hour (string-to-number (match-string 3 ans)))
	    minute (string-to-number (match-string 2 ans))
	    m2 (+ minute (if (match-end 5) (string-to-number
					    (match-string 5 ans))0)))
      (when (>= m2 60) (setq h2 (1+ h2) m2 (- m2 60)))
      (setq ans (replace-match (format "%02d:%02d-%02d:%02d" hour minute h2 m2)
			       t t ans)))

    ;; Check if there is a time range
    (when (boundp 'org-end-time-was-given)
      (setq org-time-was-given nil)
      (when (and (string-match org-plain-time-of-day-regexp ans)
		 (match-end 8))
	(setq org-end-time-was-given (match-string 8 ans))
	(setq ans (concat (substring ans 0 (match-beginning 7))
			  (substring ans (match-end 7))))))

    (setq tl (parse-time-string ans)
	  day (or (nth 3 tl) (nth 3 org-defdecode))
	  month
	  (cond ((nth 4 tl))
		((not org-read-date-prefer-future) (nth 4 org-defdecode))
		;; Day was specified.  Make sure DAY+MONTH
		;; combination happens in the future.
		((nth 3 tl)
		 (setq futurep t)
		 (if (< day (nth 3 nowdecode)) (1+ (nth 4 nowdecode))
		   (nth 4 nowdecode)))
		(t (nth 4 org-defdecode)))
	  year
	  (cond ((and (not kill-year) (nth 5 tl)))
		((not org-read-date-prefer-future) (nth 5 org-defdecode))
		;; Month was guessed in the future and is at least
		;; equal to NOWDECODE's.  Fix year accordingly.
		(futurep
		 (if (or (> month (nth 4 nowdecode))
			 (>= day (nth 3 nowdecode)))
		     (nth 5 nowdecode)
		   (1+ (nth 5 nowdecode))))
		;; Month was specified.  Make sure MONTH+YEAR
		;; combination happens in the future.
		((nth 4 tl)
		 (setq futurep t)
		 (cond ((> month (nth 4 nowdecode)) (nth 5 nowdecode))
		       ((< month (nth 4 nowdecode)) (1+ (nth 5 nowdecode)))
		       ((< day (nth 3 nowdecode)) (1+ (nth 5 nowdecode)))
		       (t (nth 5 nowdecode))))
		(t (nth 5 org-defdecode)))
	  hour (or (nth 2 tl) (nth 2 org-defdecode))
	  minute (or (nth 1 tl) (nth 1 org-defdecode))
	  second (or (nth 0 tl) 0)
	  wday (nth 6 tl))

    (when (and (eq org-read-date-prefer-future 'time)
	       (not (nth 3 tl)) (not (nth 4 tl)) (not (nth 5 tl))
	       (equal day (nth 3 nowdecode))
	       (equal month (nth 4 nowdecode))
	       (equal year (nth 5 nowdecode))
	       (nth 2 tl)
	       (or (< (nth 2 tl) (nth 2 nowdecode))
		   (and (= (nth 2 tl) (nth 2 nowdecode))
			(nth 1 tl)
			(< (nth 1 tl) (nth 1 nowdecode)))))
      (setq day (1+ day)
	    futurep t))

    ;; Special date definitions below
    (cond
     (iso-week
      ;; There was an iso week
      (require 'cal-iso)
      (setq futurep nil)
      (setq year (or iso-year year)
	    day (or iso-weekday wday 1)
	    wday nil ; to make sure that the trigger below does not match
	    iso-date (calendar-gregorian-from-absolute
		      (calendar-iso-to-absolute
		       (list iso-week day year))))
					; FIXME:  Should we also push ISO weeks into the future?
					;      (when (and org-read-date-prefer-future
					;		 (not iso-year)
					;		 (< (calendar-absolute-from-gregorian iso-date)
					;		    (time-to-days nil)))
					;	(setq year (1+ year)
					;	      iso-date (calendar-gregorian-from-absolute
					;			(calendar-iso-to-absolute
					;			 (list iso-week day year)))))
      (setq month (car iso-date)
	    year (nth 2 iso-date)
	    day (nth 1 iso-date)))
     (deltan
      (setq futurep nil)
      (unless deltadef
	(let ((now (decode-time)))
	  (setq day (nth 3 now) month (nth 4 now) year (nth 5 now))))
      (cond ((member deltaw '("h" ""))
             (when (boundp 'org-time-was-given)
               (setq org-time-was-given t))
             (setq hour (+ hour deltan)))
            ((member deltaw '("d" "")) (setq day (+ day deltan)))
            ((equal deltaw "w") (setq day (+ day (* 7 deltan))))
            ((equal deltaw "m") (setq month (+ month deltan)))
            ((equal deltaw "y") (setq year (+ year deltan)))))
     ((and wday (not (nth 3 tl)))
      ;; Weekday was given, but no day, so pick that day in the week
      ;; on or after the derived date.
      (setq wday1 (nth 6 (decode-time (org-encode-time 0 0 0 day month year))))
      (unless (equal wday wday1)
	(setq day (+ day (% (- wday wday1 -7) 7))))))
    (when (and (boundp 'org-time-was-given)
	       (nth 2 tl))
      (setq org-time-was-given t))
    (when (< year 100) (setq year (+ 2000 year)))
    ;; Check of the date is representable
    (if org-read-date-force-compatible-dates
	(progn
	  (when (< year 1970)
	    (setq year 1970 org-read-date-analyze-forced-year t))
	  (when (> year 2037)
	    (setq year 2037 org-read-date-analyze-forced-year t)))
      (condition-case nil
	  (ignore (org-encode-time second minute hour day month year))
	(error
	 (setq year (nth 5 org-defdecode))
	 (setq org-read-date-analyze-forced-year t))))
    (setq org-read-date-analyze-futurep futurep)
    (list second minute hour day month year nil -1 nil)))

(defvar parse-time-weekdays)
(defun org-read-date-get-relative (s today default)
  "Check string S for special relative date string.
TODAY and DEFAULT are internal times, for today and for a default.
Return shift list (N what def-flag)
WHAT       is \"d\", \"w\", \"m\", or \"y\" for day, week, month, year.
N          is the number of WHATs to shift.
DEF-FLAG   is t when a double ++ or -- indicates shift relative to
           the DEFAULT date rather than TODAY."
  (require 'parse-time)
  (when (and
         ;; Force case-insensitive.
         (let ((case-fold-search t))
	   (string-match
	    (concat
	     "\\`[ \t]*\\([-+]\\{0,2\\}\\)"
	     "\\([0-9]+\\)?"
	     "\\([hdwmy]\\|\\(" (mapconcat 'car parse-time-weekdays "\\|") "\\)\\)?"
	     "\\([ \t]\\|$\\)") s))
	 (or (> (match-end 1) (match-beginning 1)) (match-end 4)))
    (let* ((dir (if (> (match-end 1) (match-beginning 1))
		    (string-to-char (substring (match-string 1 s) -1))
		  ?+))
	   (rel (and (match-end 1) (= 2 (- (match-end 1) (match-beginning 1)))))
	   (n (if (match-end 2) (string-to-number (match-string 2 s)) 1))
	   (what (if (match-end 3) (match-string 3 s) "d"))
	   (wday1 (cdr (assoc (downcase what) parse-time-weekdays)))
	   (date (if rel default today))
	   (wday (nth 6 (decode-time date)))
	   delta)
      (if wday1
	  (progn
	    (setq delta (mod (+ 7 (- wday1 wday)) 7))
	    (when (= delta 0) (setq delta 7))
	    (when (= dir ?-)
	      (setq delta (- delta 7))
	      (when (= delta 0) (setq delta -7)))
	    (when (> n 1) (setq delta (+ delta (* (1- n) (if (= dir ?-) -7 7)))))
	    (list delta "d" rel))
	(list (* n (if (= dir ?-) -1 1)) what rel)))))

(defun org-order-calendar-date-args (arg1 arg2 arg3)
  "Turn a user-specified date into the internal representation.
The internal representation needed by the calendar is (month day year).
This is a wrapper to handle the brain-dead convention in calendar that
user function argument order change dependent on argument order."
  (pcase calendar-date-style
    (`american (list arg1 arg2 arg3))
    (`european (list arg2 arg1 arg3))
    (`iso (list arg2 arg3 arg1))))

(defun org-eval-in-calendar (form &optional keepdate)
  "Eval FORM in the calendar window and return to current window.
Unless KEEPDATE is non-nil, update `org-ans2' to the cursor date."
  (let ((sf (selected-frame))
	(sw (selected-window)))
    (select-window (get-buffer-window calendar-buffer t))
    (eval form t)
    (when (and (not keepdate) (calendar-cursor-to-date))
      (let* ((date (calendar-cursor-to-date))
	     (time (org-encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
	(setq org-ans2 (format-time-string "%Y-%m-%d" time))))
    (move-overlay org-date-ovl (1- (point)) (1+ (point)) (current-buffer))
    (select-window sw)
    (select-frame-set-input-focus sf)))

(defun org-calendar-select ()
  "Return to `org-read-date' with the date currently selected.
This is used by `org-read-date' in a temporary keymap for the calendar buffer."
  (interactive)
  (when (calendar-cursor-to-date)
    (let* ((date (calendar-cursor-to-date))
	   (time (org-encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
      (setq org-ans1 (format-time-string "%Y-%m-%d" time)))
    (when (active-minibuffer-window) (exit-minibuffer))))

(defalias 'org-insert-time-stamp #'org-insert-timestamp)
(defun org-insert-timestamp (time &optional with-hm inactive pre post extra)
  "Insert a date stamp for the date given by the internal TIME.
See `format-time-string' for the format of TIME.
WITH-HM means use the stamp format that includes the time of the day.
INACTIVE means use square brackets instead of angular ones, so that the
stamp will not contribute to the agenda.
PRE and POST are optional strings to be inserted before and after the
stamp.
The command returns the inserted time stamp."
  (org-fold-core-ignore-modifications
    (let ((fmt (org-time-stamp-format with-hm inactive))
	  stamp)
      (insert-before-markers-and-inherit (or pre ""))
      (when (listp extra)
        (setq extra (car extra))
        (if (and (stringp extra)
	         (string-match "\\([0-9]+\\):\\([0-9]+\\)" extra))
	    (setq extra (format "-%02d:%02d"
			        (string-to-number (match-string 1 extra))
			        (string-to-number (match-string 2 extra))))
	  (setq extra nil)))
      (when extra
        (setq fmt (concat (substring fmt 0 -1) extra (substring fmt -1))))
      (insert-before-markers-and-inherit (setq stamp (format-time-string fmt time)))
      (insert-before-markers-and-inherit (or post ""))
      (setq org-last-inserted-timestamp stamp))))

(defvar org-table-may-need-update)

(defalias 'org-toggle-time-stamp-overlays #'org-toggle-timestamp-overlays)
(defun org-toggle-timestamp-overlays ()
  "Toggle the use of custom time stamp formats."
  (interactive)
  (setq org-display-custom-times (not org-display-custom-times))
  (unless org-display-custom-times
    (let ((p (point-min)) (bmp (buffer-modified-p)))
      (while (setq p (next-single-property-change p 'display))
	(when (and (get-text-property p 'display)
		   (eq (get-text-property p 'face) 'org-date))
	  (remove-text-properties
	   p (setq p (next-single-property-change p 'display))
	   '(display t))))
      (set-buffer-modified-p bmp)))
  (org-restart-font-lock)
  (setq org-table-may-need-update t)
  (if org-display-custom-times
      (message "Time stamps are overlaid with custom format")
    (message "Time stamp overlays removed")))

(defun org-display-custom-time (beg end)
  "Overlay modified time stamp format over timestamp between BEG and END."
  (let* ((ts (buffer-substring beg end))
	 t1 with-hm tf time str (off 0))
    (save-match-data
      (setq t1 (org-parse-time-string ts t))
      (when (string-match "\\(-[0-9]+:[0-9]+\\)?\\( [.+]?\\+[0-9]+[hdwmy]\\(/[0-9]+[hdwmy]\\)?\\)?\\'" ts)
	(setq off (- (match-end 0) (match-beginning 0)))))
    (setq end (- end off))
    (setq with-hm (and (nth 1 t1) (nth 2 t1))
	  tf (org-time-stamp-format with-hm 'no-brackets 'custom)
	  time (org-fix-decoded-time t1)
	  str (org-add-props
		  (format-time-string tf (org-encode-time time))
		  nil 'mouse-face 'highlight))
    (put-text-property beg end 'display str)))

(defun org-fix-decoded-time (time)
  "Set 0 instead of nil for the first 6 elements of time.
Don't touch the rest."
  (let ((n 0))
    (mapcar (lambda (x) (if (< (setq n (1+ n)) 7) (or x 0) x)) time)))

(defalias 'org-time-stamp-to-now #'org-timestamp-to-now)
(defun org-timestamp-to-now (timestamp-string &optional seconds)
  "Difference between TIMESTAMP-STRING and now in days.
If SECONDS is non-nil, return the difference in seconds."
  (let ((fdiff (if seconds #'float-time #'time-to-days)))
    (- (funcall fdiff (org-time-string-to-time timestamp-string))
       (funcall fdiff nil))))

(declare-function org-entry-is-done-p "org" ())

(defun org-calendar-select-mouse (ev)
  "Return to `org-read-date' with the date currently selected.
This is used by `org-read-date' in a temporary keymap for the calendar buffer."
  (interactive "e")
  (mouse-set-point ev)
  (when (calendar-cursor-to-date)
    (let* ((date (calendar-cursor-to-date))
	   (time (org-encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
      (setq org-ans1 (format-time-string "%Y-%m-%d" time)))
    (when (active-minibuffer-window) (exit-minibuffer))))

(defsubst org-re-timestamp (type)
  "Return a regexp for timestamp TYPE.
Allowed values for TYPE are:

        all: all timestamps
     active: only active timestamps (<...>)
   inactive: only inactive timestamps ([...])
  scheduled: only scheduled timestamps
   deadline: only deadline timestamps
     closed: only closed timestamps

When TYPE is nil, fall back on returning a regexp that matches
both scheduled and deadline timestamps."
  (cl-case type
    (all org-ts-regexp-both)
    (active org-ts-regexp)
    (inactive org-ts-regexp-inactive)
    (scheduled org-scheduled-time-regexp)
    (deadline org-deadline-time-regexp)
    (closed org-closed-time-regexp)
    (otherwise
     (concat "\\<"
	     (regexp-opt (list org-deadline-string org-scheduled-string))
	     " *<\\([^>]+\\)>"))))

(defun org-evaluate-time-range (&optional to-buffer)
  "Evaluate a time range by computing the difference between start and end.
Normally the result is just printed in the echo area, but with prefix arg
TO-BUFFER, the result is inserted just after the date stamp into the buffer.
If the time range is actually in a table, the result is inserted into the
next column.
For time difference computation, a year is assumed to be exactly 365
days in order to avoid rounding problems."
  (interactive "P")
  (or
   (org-clock-update-time-maybe)
   (save-excursion
     (unless (org-at-date-range-p t)
       (goto-char (line-beginning-position))
       (re-search-forward org-tr-regexp-both (line-end-position) t))
     (unless (org-at-date-range-p t)
       (user-error "Not at a timestamp range, and none found in current line")))
   (let* ((ts1 (match-string 1))
	  (ts2 (match-string 2))
	  (havetime (or (> (length ts1) 15) (> (length ts2) 15)))
	  (match-end (match-end 0))
	  (time1 (org-time-string-to-time ts1))
	  (time2 (org-time-string-to-time ts2))
	  (diff (abs (float-time (time-subtract time2 time1))))
	  (negative (time-less-p time2 time1))
	  ;; (ys (floor (* 365 24 60 60)))
	  (ds (* 24 60 60))
	  (hs (* 60 60))
	  (fy "%dy %dd %02d:%02d")
	  (fy1 "%dy %dd")
	  (fd "%dd %02d:%02d")
	  (fd1 "%dd")
	  (fh "%02d:%02d")
	  y d h m align)
     (if havetime
	 (setq ; y (floor diff ys)  diff (mod diff ys)
	  y 0
	  d (floor diff ds)  diff (mod diff ds)
	  h (floor diff hs)  diff (mod diff hs)
	  m (floor diff 60))
       (setq ; y (floor diff ys)  diff (mod diff ys)
	y 0
	d (round diff ds)
	h 0 m 0))
     (if (not to-buffer)
	 (message "%s" (org-make-tdiff-string y d h m))
       (if (org-at-table-p)
	   (progn
	     (goto-char match-end)
	     (setq align t)
	     (and (looking-at " *|") (goto-char (match-end 0))))
	 (goto-char match-end))
       (when (looking-at
	      "\\( *-? *[0-9]+y\\)?\\( *[0-9]+d\\)? *[0-9][0-9]:[0-9][0-9]")
	 (replace-match ""))
       (when negative (insert " -"))
       (if (> y 0) (insert " " (format (if havetime fy fy1) y d h m))
	 (if (> d 0) (insert " " (format (if havetime fd fd1) d h m))
	   (insert " " (format fh h m))))
       (when align (org-table-align))
       (message "Time difference inserted")))))

(defun org-make-tdiff-string (y d h m)
  (let ((fmt "")
	(l nil))
    (when (> y 0)
      (setq fmt (concat fmt "%d year" (if (> y 1) "s" "") " "))
      (push y l))
    (when (> d 0)
      (setq fmt (concat fmt "%d day"  (if (> d 1) "s" "") " "))
      (push d l))
    (when (> h 0)
      (setq fmt (concat fmt "%d hour" (if (> h 1) "s" "") " "))
      (push h l))
    (when (> m 0)
      (setq fmt (concat fmt "%d minute" (if (> m 1) "s" "") " "))
      (push m l))
    (apply 'format fmt (nreverse l))))

(defun org-time-string-to-time (s)
  "Convert timestamp string S into internal time."
  (org-encode-time (org-parse-time-string s)))

(defun org-time-string-to-seconds (s)
  "Convert a timestamp string S into a number of seconds."
  (float-time (org-time-string-to-time s)))

(define-error 'org-diary-sexp-no-match "Unable to match diary sexp")

(defun org-time-string-to-absolute (s &optional daynr prefer buffer pos)
  "Convert time stamp S to an absolute day number.

If DAYNR in non-nil, and there is a specifier for a cyclic time
stamp, get the closest date to DAYNR.  If PREFER is
`past' (respectively `future') return a date past (respectively
after) or equal to DAYNR.

POS is the location of time stamp S, as a buffer position in
BUFFER.

Diary sexp timestamps are matched against DAYNR, when non-nil.
If matching fails or DAYNR is nil, `org-diary-sexp-no-match' is
signaled."
  (cond
   ((string-match "\\`%%\\((.*)\\)" s)
    ;; Sexp timestamp: try to match DAYNR, if available, since we're
    ;; only able to match individual dates.  If it fails, raise an
    ;; error.
    (if (and daynr
	     (org-diary-sexp-entry
	      (match-string 1 s) "" (calendar-gregorian-from-absolute daynr)))
	daynr
      (signal 'org-diary-sexp-no-match (list s))))
   (daynr (org-closest-date s daynr prefer))
   (t (time-to-days
       (condition-case errdata
	   (org-time-string-to-time s)
	 (error (error "Bad timestamp `%s'%s\nError was: %s"
		       s
		       (if (not (and buffer pos)) ""
			 (format-message " at %d in buffer `%s'" pos buffer))
		       (cdr errdata))))))))

(defun org-days-to-iso-week (days)
  "Return the ISO week number."
  (require 'cal-iso)
  (car (calendar-iso-from-absolute days)))

(defun org-small-year-to-year (year)
  "Convert 2-digit years into 4-digit years.
YEAR is expanded into one of the 30 next years, if possible, or
into a past one.  Any year larger than 99 is returned unchanged."
  (if (>= year 100) year
    (let* ((current (string-to-number (format-time-string "%Y")))
	   (century (/ current 100))
	   (offset (- year (% current 100))))
      (cond ((> offset 30) (+ (* (1- century) 100) year))
	    ((> offset -70) (+ (* century 100) year))
	    (t (+ (* (1+ century) 100) year))))))

(defun org-time-from-absolute (d)
  "Return the time corresponding to date D.
D may be an absolute day number, or a calendar-type list (month day year)."
  (when (numberp d) (setq d (calendar-gregorian-from-absolute d)))
  (org-encode-time 0 0 0 (nth 1 d) (car d) (nth 2 d)))

(defvar org-agenda-current-date)
(defun org-calendar-holiday ()
  "List of holidays, for Diary display in Org mode."
  (require 'holidays)
  (let ((hl (calendar-check-holidays org-agenda-current-date)))
    (and hl (mapconcat #'identity hl "; "))))

(defvar org--diary-sexp-entry-cache (make-hash-table :test #'equal)
  "Hash table holding return values of `org-diary-sexp-entry'.")
(defun org-diary-sexp-entry (sexp entry d)
  "Process a SEXP diary ENTRY for date D."
  (require 'diary-lib)
  ;; `org-anniversary' and alike expect ENTRY and DATE to be bound
  ;; dynamically.
  (let ((cached (gethash (list sexp entry d) org--diary-sexp-entry-cache 'none)))
    (if (not (eq 'none cached)) cached
      (puthash (list sexp entry d)
               (let* ((sexp `(let ((entry ,entry)
		                   (date ',d))
		               ,(car (read-from-string sexp))))
                      ;; FIXME: Do not use (eval ... t) in the following sexp as
                      ;; diary vars are still using dynamic scope.
	              (result (if calendar-debug-sexp (eval sexp)
		                (condition-case nil
		                    (eval sexp)
		                  (error
		                   (beep)
		                   (message "Bad sexp at line %d in %s: %s"
			                    (org-current-line)
			                    (buffer-file-name) sexp)
		                   (sleep-for 2))))))
                 (cond ((stringp result) (split-string result "; "))
	               ((and (consp result)
		             (not (consp (cdr result)))
		             (stringp (cdr result)))
	                (cdr result))
	               ((and (consp result)
		             (stringp (car result)))
	                result)
	               (result entry)))
               org--diary-sexp-entry-cache))))

(defun org-diary-to-ical-string (frombuf)
  "Get iCalendar entries from diary entries in buffer FROMBUF.
This uses the icalendar.el library."
  (let* ((tmpdir temporary-file-directory)
	 (tmpfile (make-temp-name
		   (expand-file-name "orgics" tmpdir)))
	 buf rtn b e)
    (unwind-protect
        (with-current-buffer frombuf
          (icalendar-export-region (point-min) (point-max) tmpfile)
          (setq buf (find-buffer-visiting tmpfile))
          (set-buffer buf)
          (goto-char (point-min))
          (when (re-search-forward "^BEGIN:VEVENT" nil t)
	    (setq b (match-beginning 0)))
          (goto-char (point-max))
          (when (re-search-backward "^END:VEVENT" nil t)
	    (setq e (match-end 0)))
          (setq rtn (if (and b e) (concat (buffer-substring b e) "\n") "")))
      (when (and buf (buffer-live-p buf)) (kill-buffer buf))
      (delete-file tmpfile))
    rtn))

(defun org-closest-date (start current prefer)
  "Return closest date to CURRENT starting from START.

CURRENT and START are both time stamps.

When PREFER is `past', return a date that is either CURRENT or
past.  When PREFER is `future', return a date that is either
CURRENT or future.

Only time stamps with a repeater are modified.  Any other time
stamp stay unchanged.  In any case, return value is an absolute
day number."
  (if (not (string-match "\\+\\([0-9]+\\)\\([hdwmy]\\)" start))
      ;; No repeater.  Do not shift time stamp.
      (time-to-days (org-time-string-to-time start))
    (let ((value (string-to-number (match-string 1 start)))
	  (type (match-string 2 start)))
      (if (= 0 value)
	  ;; Repeater with a 0-value is considered as void.
	  (time-to-days (org-time-string-to-time start))
	(let* ((base (org-date-to-gregorian start))
	       (target (org-date-to-gregorian current))
	       (sday (calendar-absolute-from-gregorian base))
	       (cday (calendar-absolute-from-gregorian target))
	       n1 n2)
	  ;; If START is already past CURRENT, just return START.
	  (if (<= cday sday) sday
	    ;; Compute closest date before (N1) and closest date past
	    ;; (N2) CURRENT.
	    (pcase type
	      ("h"
	       (let ((missing-hours
		      (mod (+ (- (* 24 (- cday sday))
				 (nth 2 (org-parse-time-string start)))
			      org-extend-today-until)
			   value)))
		 (setf n1 (if (= missing-hours 0) cday
			    (- cday (1+ (/ missing-hours 24)))))
		 (setf n2 (+ cday (/ (- value missing-hours) 24)))))
	      ((or "d" "w")
	       (let ((value (if (equal type "w") (* 7 value) value)))
		 (setf n1 (+ sday (* value (/ (- cday sday) value))))
		 (setf n2 (+ n1 value))))
	      ("m"
	       (let* ((add-months
		       (lambda (d n)
			 ;; Add N months to gregorian date D, i.e.,
			 ;; a list (MONTH DAY YEAR).  Return a valid
			 ;; gregorian date.
			 (let ((m (+ (nth 0 d) n)))
			   (list (mod m 12)
				 (nth 1 d)
				 (+ (/ m 12) (nth 2 d))))))
		      (months		; Complete months to TARGET.
		       (* (/ (+ (* 12 (- (nth 2 target) (nth 2 base)))
				(- (nth 0 target) (nth 0 base))
				;; If START's day is greater than
				;; TARGET's, remove incomplete month.
				(if (> (nth 1 target) (nth 1 base)) 0 -1))
			     value)
			  value))
		      (before (funcall add-months base months)))
		 (setf n1 (calendar-absolute-from-gregorian before))
		 (setf n2
		       (calendar-absolute-from-gregorian
			(funcall add-months before value)))))
	      (_
	       (let* ((d (nth 1 base))
		      (m (nth 0 base))
		      (y (nth 2 base))
		      (years		; Complete years to TARGET.
		       (* (/ (- (nth 2 target)
				y
				;; If START's month and day are
				;; greater than TARGET's, remove
				;; incomplete year.
				(if (or (> (nth 0 target) m)
					(and (= (nth 0 target) m)
					     (> (nth 1 target) d)))
				    0
				  1))
			     value)
			  value))
		      (before (list m d (+ y years))))
		 (setf n1 (calendar-absolute-from-gregorian before))
		 (setf n2 (calendar-absolute-from-gregorian
			   (list m d (+ (nth 2 before) value)))))))
	    ;; Handle PREFER parameter, if any.
	    (cond
	     ((eq prefer 'past)   (if (= cday n2) n2 n1))
	     ((eq prefer 'future) (if (= cday n1) n1 n2))
	     (t (if (> (abs (- cday n1)) (abs (- cday n2))) n2 n1)))))))))

(defun org-date-to-gregorian (d)
  "Turn any specification of date D into a Gregorian date for the calendar."
  (cond ((integerp d) (calendar-gregorian-from-absolute d))
	((and (listp d) (= (length d) 3)) d)
	((stringp d)
	 (let ((d (org-parse-time-string d)))
	   (list (nth 4 d) (nth 3 d) (nth 5 d))))
	((listp d) (list (nth 4 d) (nth 3 d) (nth 5 d)))))

(defun org-timestamp-up (&optional arg)
  "Increase the date item at the cursor by one.
If the cursor is on the year, change the year.  If it is on the month,
the day or the time, change that.  If the cursor is on the enclosing
bracket, change the timestamp type.
With prefix ARG, change by that many units."
  (interactive "p")
  (org-timestamp-change (prefix-numeric-value arg) nil 'updown))

(defun org-timestamp-down (&optional arg)
  "Decrease the date item at the cursor by one.
If the cursor is on the year, change the year.  If it is on the month,
the day or the time, change that.  If the cursor is on the enclosing
bracket, change the timestamp type.
With prefix ARG, change by that many units."
  (interactive "p")
  (org-timestamp-change (- (prefix-numeric-value arg)) nil 'updown))

(declare-function org-todo "org" (&optional arg))
(declare-function org-at-heading-p "org-heading" (&optional invisible-not-ok))
(defun org-timestamp-up-day (&optional arg)
  "Increase the date in the time stamp by one day.
With prefix ARG, change that many days."
  (interactive "p")
  (if (and (not (org-at-timestamp-p 'lax))
	   (progn (require 'org-heading) (org-at-heading-p)))
      (org-todo 'up)
    (org-timestamp-change (prefix-numeric-value arg) 'day 'updown)))

(defun org-timestamp-down-day (&optional arg)
  "Decrease the date in the time stamp by one day.
With prefix ARG, change that many days."
  (interactive "p")
  (if (and (not (org-at-timestamp-p 'lax))
	   (org-at-heading-p))
      (org-todo 'down)
    (org-timestamp-change (- (prefix-numeric-value arg)) 'day) 'updown))

(declare-function org-at-planning-p "org" ())
(declare-function org-at-property-p "org" ())

(declare-function org-at-clock-log-p "org-clock" ())

(defun org-at-timestamp-p (&optional extended)
  "Non-nil if point is inside a timestamp.

By default, the function only consider syntactically valid active
timestamps.  However, the caller may have a broader definition
for timestamps.  As a consequence, optional argument EXTENDED can
be set to the following values

  `inactive'

    Include also syntactically valid inactive timestamps.

  `agenda'

    Include timestamps allowed in Agenda, i.e., those in
    properties drawers, planning lines and clock lines.

  `lax'

    Ignore context.  The function matches any part of the
    document looking like a timestamp.  This includes comments,
    example blocks...

For backward-compatibility with Org 9.0, every other non-nil
value is equivalent to `inactive'.

When at a timestamp, return the position of the point as a symbol
among `bracket', `after', `year', `month', `hour', `minute',
`day' or a number of character from the last know part of the
time stamp.  If diary sexp timestamps, any point inside the timestamp
is considered `day' (i.e. only `bracket', `day', and `after' return
values are possible).

When matching, the match groups are the following:
  group 2: year, if any
  group 3: month, if any
  group 4: day number, if any
  group 5: day name, if any
  group 7: hours, if any
  group 8: minutes, if any"
  (let* ((regexp
          (if extended
              (if (eq extended 'agenda)
                  (rx-to-string
                   `(or (regexp ,org-ts-regexp3)
                        (regexp ,org-element--timestamp-regexp)))
		org-ts-regexp3)
            org-ts-regexp2))
	 (pos (point))
	 (match?
	  (let ((boundaries (org-in-regexp regexp)))
	    (save-match-data
	      (cond ((null boundaries) nil)
		    ((eq extended 'lax) t)
		    (t
		     (or (and (eq extended 'agenda)
			      (or (org-at-planning-p)
				  (org-at-property-p)
				  (and (bound-and-true-p
					org-agenda-include-inactive-timestamps)
				       (org-at-clock-log-p))))
			 (eq 'timestamp
			     (save-excursion
			       (when (= pos (cdr boundaries)) (forward-char -1))
			       (org-element-type (org-element-context)))))))))))
    (cond
     ((not match?)                        nil)
     ((= pos (match-beginning 0))         'bracket)
     ;; Distinguish location right before the closing bracket from
     ;; right after it.
     ((= pos (1- (match-end 0)))          'bracket)
     ((= pos (match-end 0))               'after)
     ((org-pos-in-match-range pos 2)      'year)
     ((org-pos-in-match-range pos 3)      'month)
     ((org-pos-in-match-range pos 7)      'hour)
     ((org-pos-in-match-range pos 8)      'minute)
     ((or (org-pos-in-match-range pos 4)
	  (org-pos-in-match-range pos 5)) 'day)
     ((and (or (match-end 8) (match-end 5))
           (> pos (or (match-end 8) (match-end 5)))
	   (< pos (match-end 0)))
      (- pos (or (match-end 8) (match-end 5))))
     (t                                   'day))))

(defun org-toggle-timestamp-type ()
  "Toggle the type (<active> or [inactive]) of a time stamp."
  (interactive)
  (when (org-at-timestamp-p 'lax)
    (let ((beg (match-beginning 0)) (end (match-end 0))
	  (map '((?\[ . "<") (?\] . ">") (?< . "[") (?> . "]"))))
      (save-excursion
	(goto-char beg)
	(while (re-search-forward "[][<>]" end t)
	  (replace-match (cdr (assoc (char-after (match-beginning 0)) map))
			 t t)))
      (message "Timestamp is now %sactive"
	       (if (equal (char-after beg) ?<) "" "in")))))

(declare-function org-goto-marker-or-bmk "org" (marker &optional bookmark))
(declare-function org-get-heading "org" (&optional no-tags no-todo no-priority no-comment))
(declare-function org-back-to-heading "org-heading" (&optional invisible-ok))

(defvar org-clock-history)                     ; defined in org-clock.el
(defvar org-clock-adjust-closest nil)          ; defined in org-clock.el
(defun org-timestamp-change (n &optional what updown suppress-tmp-delay)
  "Change the date in the time stamp at point.

The date is changed by N times WHAT.  WHAT can be `day', `month',
`year', `hour', or `minute'.  If WHAT is not given, the cursor
position in the timestamp determines what is changed.

When optional argument UPDOWN is non-nil, minutes are rounded
according to `org-timestamp-rounding-minutes'.

When SUPPRESS-TMP-DELAY is non-nil, suppress delays like
\"--2d\"."
  (let ((origin (point))
	(timestamp? (org-at-timestamp-p 'lax))
	origin-cat
	with-hm inactive
	(dm (max (nth 1 org-timestamp-rounding-minutes) 1))
	extra rem
	ts time time0 fixnext clrgx)
    (unless timestamp? (user-error "Not at a timestamp"))
    (if (and (not what) (eq timestamp? 'bracket))
	(org-toggle-timestamp-type)
      ;; Point isn't on brackets.  Remember the part of the timestamp
      ;; the point was in.  Indeed, size of timestamps may change,
      ;; but point must be kept in the same category nonetheless.
      (setq origin-cat timestamp?)
      (when (and (not what) (not (eq timestamp? 'day))
		 org-display-custom-times
		 (get-text-property (point) 'display)
		 (not (get-text-property (1- (point)) 'display)))
	(setq timestamp? 'day))
      (setq timestamp? (or what timestamp?)
	    inactive (= (char-after (match-beginning 0)) ?\[)
	    ts (match-string 0))
      ;; FIXME: Instead of deleting everything and then inserting
      ;; later, we should make use of `replace-match', which preserves
      ;; markers.  The current implementation suffers from
      ;; `save-excursion' not preserving point inside the timestamp
      ;; once we delete the timestamp here.  The point moves to the
      ;; updated timestamp end.
      (replace-match "")
      (when (string-match
	     "\\(\\(-[012][0-9]:[0-5][0-9]\\)?\\( +[.+]?-?[-+][0-9]+[hdwmy]\\(/[0-9]+[hdwmy]\\)?\\)*\\)[]>]"
	     ts)
	(setq extra (match-string 1 ts))
	(when suppress-tmp-delay
	  (setq extra (replace-regexp-in-string " --[0-9]+[hdwmy]" "" extra))))
      (when (string-match "^.\\{10\\}.*?[0-9]+:[0-9][0-9]" ts)
	(setq with-hm t))
      (setq time0 (org-parse-time-string ts))
      (let ((increment n))
        (if (and updown
	         (eq timestamp? 'minute)
	         (not current-prefix-arg))
	    ;; This looks like s-up and s-down.  Change by one rounding step.
            (progn
	      (setq increment (* dm (cond ((> n 0) 1) ((< n 0) -1) (t 0))))
	      (unless (= 0 (setq rem (% (nth 1 time0) dm)))
	        (setcar (cdr time0) (+ (nth 1 time0)
				       (if (> n 0) (- rem) (- dm rem))))))
          ;; Do not round anything in `org-modify-ts-extra' when prefix
          ;; argument is supplied - just use whatever is provided by the
          ;; prefix argument.
          (setq dm 1))
        (setq time
	      (org-encode-time
               (apply #'list
                      (or (car time0) 0)
                      (+ (if (eq timestamp? 'minute) increment 0) (nth 1 time0))
                      (+ (if (eq timestamp? 'hour) increment 0)   (nth 2 time0))
                      (+ (if (eq timestamp? 'day) increment 0)    (nth 3 time0))
                      (+ (if (eq timestamp? 'month) increment 0)  (nth 4 time0))
                      (+ (if (eq timestamp? 'year) increment 0)   (nth 5 time0))
                      (nthcdr 6 time0)))))
      (when (and (memq timestamp? '(hour minute))
		 extra
		 (string-match "-\\([012][0-9]\\):\\([0-5][0-9]\\)" extra))
        ;; When modifying the start time in HH:MM-HH:MM range, update
        ;; end time as well.
	(setq extra (org-modify-ts-extra
		     extra ;; -HH:MM ...
                     ;; Fake position in EXTRA to force changing hours
                     ;; or minutes as needed.
		     (if (eq timestamp? 'hour)
                         2 ;; -H<H>:MM
                       5) ;; -HH:M<M>
		     n dm)))
      (when (integerp timestamp?)
	(setq extra (org-modify-ts-extra extra timestamp? n dm)))
      (when (eq what 'calendar)
	(let ((cal-date (org-get-date-from-calendar)))
	  (setcar (nthcdr 4 time0) (nth 0 cal-date)) ; month
	  (setcar (nthcdr 3 time0) (nth 1 cal-date)) ; day
	  (setcar (nthcdr 5 time0) (nth 2 cal-date)) ; year
	  (setcar time0 (or (car time0) 0))
	  (setcar (nthcdr 1 time0) (or (nth 1 time0) 0))
	  (setcar (nthcdr 2 time0) (or (nth 2 time0) 0))
	  (setq time (org-encode-time time0))))
      ;; Insert the new timestamp, and ensure point stays in the same
      ;; category as before (i.e. not after the last position in that
      ;; category).
      (let ((pos (point)))
	;; Stay before inserted string. `save-excursion' is of no use.
	(setq org-last-changed-timestamp
	      (org-insert-timestamp time with-hm inactive nil nil extra))
	(goto-char pos))
      (save-match-data
	(looking-at org-ts-regexp3)
	(goto-char
	 (pcase origin-cat
	   ;; `day' category ends before `hour' if any, or at the end
	   ;; of the day name.
	   (`day (min (or (match-beginning 7) (1- (match-end 5))) origin))
	   (`hour (min (match-end 7) origin))
	   (`minute (min (1- (match-end 8)) origin))
	   ((pred integerp) (min (1- (match-end 0)) origin))
	   ;; Point was right after the timestamp.  However, the
	   ;; timestamp length might have changed, so refer to
	   ;; (match-end 0) instead.
	   (`after (match-end 0))
	   ;; `year' and `month' have both fixed size: point couldn't
	   ;; have moved into another part.
	   (_ origin))))
      ;; Update clock if on a CLOCK line.
      (org-clock-update-time-maybe)
      ;; Maybe adjust the closest clock in `org-clock-history'
      (when org-clock-adjust-closest
	(if (not (and (org-at-clock-log-p)
		      (< 1 (length (delq nil (mapcar 'marker-position
						     org-clock-history))))))
	    (message "No clock to adjust")
	  (cond ((save-excursion	; fix previous clock?
		   (re-search-backward org-ts-regexp0 nil t)
		   (looking-back (concat org-clock-string " \\[")
				 (line-beginning-position)))
		 (setq fixnext 1 clrgx (concat org-ts-regexp0 "\\] =>.*$")))
		((save-excursion	; fix next clock?
		   (re-search-backward org-ts-regexp0 nil t)
		   (looking-at (concat org-ts-regexp0 "\\] =>")))
		 (setq fixnext -1 clrgx (concat org-clock-string " \\[" org-ts-regexp0))))
	  (save-window-excursion
	    ;; Find closest clock to point, adjust the previous/next one in history
	    (let* ((p (save-excursion (org-back-to-heading t)))
		   (cl (mapcar (lambda(c) (abs (- (marker-position c) p))) org-clock-history))
		   (clfixnth
		    (+ fixnext (- (length cl) (or (length (member (apply 'min cl) cl)) 100))))
		   (clfixpos (unless (> 0 clfixnth) (nth clfixnth org-clock-history))))
	      (if (not clfixpos)
		  (message "No clock to adjust")
		(save-excursion
		  (org-goto-marker-or-bmk clfixpos)
		  (org-fold-show-subtree)
		  (when (re-search-forward clrgx nil t)
		    (goto-char (match-beginning 1))
		    (let (org-clock-adjust-closest)
		      (org-timestamp-change n timestamp? updown))
		    (message "Clock adjusted in %s for heading: %s"
			     (file-name-nondirectory (buffer-file-name))
			     (org-get-heading t t)))))))))
      ;; Try to recenter the calendar window, if any.
      (when (and org-calendar-follow-timestamp-change
		 (get-buffer-window calendar-buffer t)
		 (memq timestamp? '(day month year)))
	(org-recenter-calendar (time-to-days time))))))

(defun org-modify-ts-extra (ts-string pos nincrements increment-step)
  "Change the lead-time/repeat fields at POS in timestamp string TS-STRING.
POS is the position in the timestamp string to be changed.
NINCREMENTS is the number of incremenets/decrements.

INCREMENT-STEP is step used for a single increment when POS in on
minutes.  Before incrementing minutes, they are rounded to
INCREMENT-STEP divisor."
  (let (;; increment order for dwmy: d-1=d; d+1=w; w+1=m; m+1=y; y+1=y.
        (idx '(("d" . 0) ("w" . 1) ("m" . 2) ("y" . 3) ("d" . -1) ("y" . 4)))
	pos-match-group hour minute new rem)
    (when (string-match "\\(-\\([012][0-9]\\):\\([0-5][0-9]\\)\\)?\\( +\\+\\([0-9]+\\)\\([dmwy]\\)\\)?\\( +-\\([0-9]+\\)\\([dmwy]\\)\\)?" ts-string)
      (cond
       ((or (org-pos-in-match-range pos 2) ;; POS in end hours
	    (org-pos-in-match-range pos 3)) ;; POS in end minutes
	(setq minute (string-to-number (match-string 3 ts-string))
	      hour (string-to-number (match-string 2 ts-string)))
	(if (org-pos-in-match-range pos 2) ;; POS in end hours
            ;; INCREMENT-STEP is only applicable to MINUTE.
	    (setq hour (+ hour nincrements))
	  (setq nincrements (* increment-step nincrements))
	  (unless (= 0 (setq rem (% minute increment-step)))
            ;; Round the MINUTE to INCREMENT-STEP.
	    (setq minute (+ minute (if (> nincrements 0) (- rem) (- increment-step rem)))))
	  (setq minute (+ minute nincrements)))
	(when (< minute 0) (setq minute (+ minute 60) hour (1- hour)))
	(when (> minute 59) (setq minute (- minute 60) hour (1+ hour)))
	(setq hour (mod hour 24))
	(setq pos-match-group 1
              new (format "-%02d:%02d" hour minute)))
       
       ((org-pos-in-match-range pos 6) ;; POS on "dmwy" repeater char.
	(setq pos-match-group 6
              new (car (rassoc (+ nincrements (cdr (assoc (match-string 6 ts-string) idx))) idx))))
       
       ((org-pos-in-match-range pos 5) ;; POS on X in "Xd" repeater.
	(setq pos-match-group 5
              ;; Never drop below X=1.
              new (format "%d" (max 1 (+ nincrements (string-to-number (match-string 5 ts-string)))))))
       
       ((org-pos-in-match-range pos 9) ;; POS on "dmwy" repeater in warning interval.
	(setq pos-match-group 9
              new (car (rassoc (+ nincrements (cdr (assoc (match-string 9 ts-string) idx))) idx))))
       
       ((org-pos-in-match-range pos 8) ;; POS on X in "Xd" in warning interval.
	(setq pos-match-group 8
              ;; Never drop below X=0.
              new (format "%d" (max 0 (+ nincrements (string-to-number (match-string 8 ts-string))))))))

      (when pos-match-group
	(setq ts-string (concat
		         (substring ts-string 0 (match-beginning pos-match-group))
		         new
		         (substring ts-string (match-end pos-match-group))))))
    ts-string))

(defun org-recenter-calendar (d)
  "If the calendar is visible, recenter it to date D."
  (let ((cwin (get-buffer-window calendar-buffer t)))
    (when cwin
      (let ((calendar-move-hook nil))
	(with-selected-window cwin
	  (calendar-goto-date
	   (if (listp d) d (calendar-gregorian-from-absolute d))))))))

(defun org-goto-calendar (&optional arg)
  "Go to the Emacs calendar at the current date.
If there is a time stamp in the current line, go to that date.
A prefix ARG can be used to force the current date."
  (interactive "P")
  (let ((calendar-move-hook nil)
	(calendar-view-holidays-initially-flag nil)
	(calendar-view-diary-initially-flag nil)
	diff)
    (when (or (org-at-timestamp-p 'lax)
	      (org-match-line (concat ".*" org-ts-regexp)))
      (let ((d1 (time-to-days nil))
	    (d2 (time-to-days (org-time-string-to-time (match-string 1)))))
	(setq diff (- d2 d1))))
    (calendar)
    (calendar-goto-today)
    (when (and diff (not arg)) (calendar-forward-day diff))))

(defun org-get-date-from-calendar ()
  "Return a list (month day year) of date at point in calendar."
  (with-current-buffer calendar-buffer
    (save-match-data
      (calendar-cursor-to-date))))

(defun org-date-from-calendar ()
  "Insert time stamp corresponding to cursor date in *Calendar* buffer.
If there is already a time stamp at the cursor position, update it."
  (interactive)
  (if (org-at-timestamp-p 'lax)
      (org-timestamp-change 0 'calendar)
    (let ((cal-date (org-get-date-from-calendar)))
      (org-insert-timestamp
       (org-encode-time 0 0 0 (nth 1 cal-date) (car cal-date) (nth 2 cal-date))))))

(defun org-current-time (&optional rounding-minutes past)
  "Current time, possibly rounded to ROUNDING-MINUTES.
When ROUNDING-MINUTES is not an integer, fall back on the car of
`org-timestamp-rounding-minutes'.  When PAST is non-nil, ensure
the rounding returns a past time."
  (let ((r (or (and (integerp rounding-minutes) rounding-minutes)
	       (car org-timestamp-rounding-minutes)))
	(now (current-time)))
    (if (< r 1)
	now
      (let* ((time (decode-time now))
	     (res (org-encode-time
                   (apply #'list
                          0 (* r (round (nth 1 time) r))
                          (nthcdr 2 time)))))
	(if (or (not past) (time-less-p res now))
	    res
	  (time-subtract res (* r 60)))))))

(defun org-today ()
  "Return today date, considering `org-extend-today-until'."
  (time-to-days
   (time-since (* 3600 org-extend-today-until))))

(defun org-time-stamp-format (&optional with-time inactive custom)
  "Get timestamp format for a time string.

The format is based on `org-timestamp-formats' (if CUSTOM is nil) or or
`org-timestamp-custom-formats' (if CUSTOM if non-nil).

When optional argument WITH-TIME is non-nil, the timestamp will contain
time.

When optional argument INACTIVE is nil, format active timestamp.
When `no-brackets', strip timestamp brackets.
Otherwise, format inactive timestamp."
  (let ((format (funcall
                 (if with-time #'cdr #'car)
                 (if custom
                     org-timestamp-custom-formats
                   org-timestamp-formats))))
    ;; Strip brackets, if any.
    (when (or (and (string-prefix-p "<" format)
                   (string-suffix-p ">" format))
              (and (string-prefix-p "[" format)
                   (string-suffix-p "]" format)))
      (setq format (substring format 1 -1)))
    (pcase inactive
      (`no-brackets format)
      (`nil (concat "<" format ">"))
      (_ (concat "[" format "]")))))

(provide 'org-timestamp)
;;; org-timestamp.el ends here
