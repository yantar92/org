;;; org-read-date.el --- Date and time selection dialogue         -*- lexical-binding: t; -*-

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
;; This library implements Org mode's `org-read-date' command - a
;; handy UI to select date and time.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-time)
(require 'org-element-timestamp)
(require 'org-keys)
(require 'org-regexps)
(require 'org-timestamp-common)
(require 'parse-time)
(require 'calendar)

;;; Custom options

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
In the calendar, the date can be selected with \\`mouse-1'.  However, the
minibuffer will also be active, and you can simply enter the date as well.
When nil, only the minibuffer will be available."
  :group 'org-time
  :type 'boolean)

;;; Internal variables

(defvar org-date-ovl (make-overlay 1 1)
  "An overlay highlighting date at point in calendar buffer.
The overlay is used to make selected date visible in the calendar when
the calendar window is not selected, while user is in the
`org-read-date' prompt.")
(overlay-put org-date-ovl 'face 'org-date-selected)
(delete-overlay org-date-ovl)

(defvar org-read-date-overlay nil
  "Overlay displaying the selected date in the `org-read-date'
minibuffer prompt.")

(defvar org-read-date-history nil
  "Prompt history for `org-read-date'.")

(defvar org-read-date-minibuffer-local-map
  (let* ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (org-defkey map (kbd ".") #'org-calendar-goto-today-or-insert-dot)
    (org-defkey map (kbd "C-.") #'org-calendar-goto-today)
    (org-defkey map (kbd "M-S-<left>") #'org-calendar-backward-month)
    (org-defkey map (kbd "ESC S-<left>") #'org-calendar-backward-month)
    (org-defkey map (kbd "M-S-<right>") #'org-calendar-forward-month)
    (org-defkey map (kbd "ESC S-<right>") #'org-calendar-forward-month)
    (org-defkey map (kbd "M-S-<up>") #'org-calendar-backward-year)
    (org-defkey map (kbd "ESC S-<up>") #'org-calendar-backward-year)
    (org-defkey map (kbd "M-S-<down>") #'org-calendar-forward-year)
    (org-defkey map (kbd "ESC S-<down>") #'org-calendar-forward-year)
    (org-defkey map (kbd "S-<up>") #'org-calendar-backward-week)
    (org-defkey map (kbd "S-<down>") #'org-calendar-forward-week)
    (org-defkey map (kbd "S-<left>") #'org-calendar-backward-day)
    (org-defkey map (kbd "S-<right>") #'org-calendar-forward-day)
    (org-defkey map (kbd "!") #'org-calendar-view-entries)
    (org-defkey map (kbd ">") #'org-calendar-scroll-month-left)
    (org-defkey map (kbd "<") #'org-calendar-scroll-month-right)
    (org-defkey map (kbd "C-v") #'org-calendar-scroll-three-months-left)
    (org-defkey map (kbd "M-v") #'org-calendar-scroll-three-months-right)
    map)
  "Keymap for minibuffer commands when using `org-read-date'.")

;;; Internal `org-read-date' state for the duration of prompt.

;; This state is global because user input is gathered from multiple
;; sources:
;;
;; 1. Minibuffer prompt
;; 2. Popup calendar selection
;; 3. Click evens in the popup calendar
;;
;; The functions coordinating the various forms of user input
;; gather `org-read-date' arguments, last selected date, and user
;; input via these global state variables.

(defvar org-read-date--calendar-selected-date nil
  "The last calendar date selected by clicking in calendar.
This variable is set by `org-calendar-select-mouse', which is bound to
mouse click event in the popup calendar buffer while `org-read-date'
prompt is active.  It is also set by `org-calendar-select', bound to
<RET>.")

(defvar org-read-date--calendar-keyboard-date nil
  "The last calendar date selected by moving from `org-read-date' prompt.
This variable is set by `org-eval-in-calendar' and its users while
`org-read-date' prompt is active.")

(defvar org-overriding-default-time nil) ; dynamically scoped

(defvar org-time-was-given)
(defvar org-end-time-was-given)
(defvar org-read-date-inactive)
(defvar org-def)
(defvar org-defdecode)
(defvar org-with-time)

(defvar org-read-date-analyze-forced-year nil
  "Set by `org-read-date-analyze' when return adjusted to fit limits.
See `org-read-date-force-compatible-dates' for more details.")

(defvar org-read-date-final-answer nil
  "Variable holding the actual prompt input in `org-read-date'.
`org-read-date' sets this variable upon completion.")

(defvar org-read-date-analyze-futurep nil
  "Set by `org-read-date-analyze' when entered date is in future.")

;;; Functions

(defun org-get-compact-tod (s)
  "Transform time range S into compact form suitable for `org-read-date'.
For example, 12:00-13:00 will be transformed to 12:00+1.
Return nil when S is not a time range."
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

;;;###autoload
(defun org-read-date (&optional with-time to-time from-string prompt
				default-time default-input inactive)
  "Read a date, possibly a time, and make things smooth for the user.
Return selected date as a string.

Also, set `org-read-date-final-answer' to the actual prompt text
entered by the user.

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
	 ans (prompt-input "") final cal-frame)
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
	    ;; FIXME: Not sure we need `with-current-buffer' but I couldn't
            ;; convince myself that we're always in `calendar-buffer' after
            ;; the call to `calendar'.
	    (with-current-buffer calendar-buffer (setq cursor-type nil))
	    (unwind-protect
		(let ((days (- (time-to-days org-def)
			       (calendar-absolute-from-gregorian
				(calendar-current-date)))))
		  (org-funcall-in-calendar #'calendar-forward-day t days)
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
                          ;; May be set by clicking in popup calendar
                          ;; or by calendar motion.
                          (setq org-read-date--calendar-selected-date nil
                                org-read-date--calendar-keyboard-date nil)
			  (setq prompt-input
				(read-string prompt
                                             (and default-input
                                                  (or (org-get-compact-tod default-input)
					              default-input))
					     'org-read-date-history
					     nil))
			  ;; prompt-input: from prompt
			  ;; org-read-date--calendar-selected-date: from mouse click/RET
			  ;; org-read-date--calendar-keyboard-date: from calendar motion
			  (setq ans
				(concat
                                 prompt-input
                                 " "
                                 (or org-read-date--calendar-selected-date
                                     org-read-date--calendar-keyboard-date))))
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

(defun org-read-date-analyze (ans def defdecode)
  "Analyze the combined answer of the date prompt.
Return entered time (as in return value of `decode-time').

Set `org-read-date-analyze-futurep' to non-nil when the entered time is
in future relative to the reference time passed to `org-read-date'.

When `org-read-date-force-compatible-dates' is non-nil, force entered
time to be within system time bounds and set
`org-read-date-analyze-forced-year' to non-nil."
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
      (declare-function calendar-iso-to-absolute "cal-iso" (date))
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
      ;; FIXME: Duplicated value in ‘cond’: ""
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

(defun org-read-date-display ()
  "Display the current date prompt interpretation in the minibuffer."
  (when org-read-date-display-live
    (when org-read-date-overlay
      (delete-overlay org-read-date-overlay)
      (setq org-read-date-overlay nil))
    (when (minibufferp (current-buffer))
      (save-excursion
	(end-of-line 1)
	(while (not (equal (buffer-substring
			  (max (point-min) (- (point) 4)) (point))
			 "    "))
	  (insert " ")))
      (let* ((ans (concat
                   ;; Prompt input
                   (buffer-substring
                    (line-beginning-position)
                    (point-max))
		   " "
                   (or
                    org-read-date--calendar-selected-date
                    org-read-date--calendar-keyboard-date)))
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
        ;; Avoid priority race with overlay used by calendar.el.
        ;; See bug#69271.
        (overlay-put org-read-date-overlay 'priority 1)
	(org-overlay-display org-read-date-overlay txt 'secondary-selection)))))

(defun org-read-date-get-relative (s today default)
  "Check string S for special relative date string.
TODAY and DEFAULT are internal times, for today and for a default.
Return shift list (N what def-flag)
WHAT       is \"d\", \"w\", \"m\", or \"y\" for day, week, month, year.
N          is the number of WHATs to shift.
DEF-FLAG   is t when a double ++ or -- indicates shift relative to
           the DEFAULT date rather than TODAY."
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

;; FIXME: Unused
(defun org-order-calendar-date-args (arg1 arg2 arg3)
  "Turn a user-specified date into the internal representation.
The internal representation needed by the calendar is (month day year).
This is a wrapper to handle the brain-dead convention in calendar that
user function argument order change dependent on argument order."
  (pcase calendar-date-style
    (`american (list arg1 arg2 arg3))
    (`european (list arg2 arg1 arg3))
    (`iso (list arg2 arg3 arg1))))

(defun org-funcall-in-calendar (func &optional keepdate &rest args)
  "Call FUNC in the calendar window and return to current window.
Unless KEEPDATE is non-nil, set `org-read-date--calendar-keyboard-date'
to the cursor date, as a string."
  (with-selected-window (get-buffer-window calendar-buffer t)
    (apply func args)
    (when (and (not keepdate) (calendar-cursor-to-date))
      (let* ((date (calendar-cursor-to-date))
	     (time (org-encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
	(setq org-read-date--calendar-keyboard-date (format-time-string "%Y-%m-%d" time))))
    (move-overlay org-date-ovl (1- (point)) (1+ (point)) (current-buffer))))

(defun org-eval-in-calendar (form &optional keepdate)
  (declare (obsolete org-funcall-in-calendar "2024"))
  (org-funcall-in-calendar (lambda () (eval form t)) keepdate))

(defun org-calendar-goto-today-or-insert-dot ()
  "Go to the current date, or insert a dot.

If at the beginning of the prompt, behave as `org-calendar-goto-today' else
insert \".\"."
  (interactive)
  ;; Are we at the beginning of the prompt?
  (if (looking-back "^[^:]+: "
		    (let ((inhibit-field-text-motion t))
		      (line-beginning-position)))
      (org-funcall-in-calendar #'calendar-goto-today)
    (insert ".")))

(defun org-calendar-goto-today ()
  "Reposition the calendar window so the current date is visible."
  (interactive)
  (org-funcall-in-calendar #'calendar-goto-today))

(defun org-calendar-backward-month ()
  "Move the cursor backward by one month."
  (interactive)
  (org-funcall-in-calendar #'calendar-backward-month nil 1))

(defun org-calendar-forward-month ()
  "Move the cursor forward by one month."
  (interactive)
  (org-funcall-in-calendar #'calendar-forward-month nil 1))

(defun org-calendar-backward-year ()
  "Move the cursor backward by one year."
  (interactive)
  (org-funcall-in-calendar #'calendar-backward-year nil 1))

(defun org-calendar-forward-year ()
  "Move the cursor forward by one year."
  (interactive)
  (org-funcall-in-calendar #'calendar-forward-year nil 1))

(defun org-calendar-backward-week ()
  "Move the cursor backward by one week."
  (interactive)
  (org-funcall-in-calendar #'calendar-backward-week nil 1))

(defun org-calendar-forward-week ()
  "Move the cursor forward by one week."
  (interactive)
  (org-funcall-in-calendar #'calendar-forward-week nil 1))

(defun org-calendar-backward-day ()
  "Move the cursor backward by one day."
  (interactive)
  (org-funcall-in-calendar #'calendar-backward-day nil 1))

(defun org-calendar-forward-day ()
  "Move the cursor forward by one day."
  (interactive)
  (org-funcall-in-calendar #'calendar-forward-day nil 1))

(defun org-calendar-view-entries ()
  "Prepare and display a buffer with diary entries."
  (interactive)
  (org-funcall-in-calendar #'diary-view-entries)
  (message ""))

(defun org-calendar-scroll-month-left ()
  "Scroll the displayed calendar left by one month."
  (interactive)
  (org-funcall-in-calendar #'calendar-scroll-left nil 1))

(defun org-calendar-scroll-month-right ()
  "Scroll the displayed calendar right by one month."
  (interactive)
  (org-funcall-in-calendar #'calendar-scroll-right nil 1))

(defun org-calendar-scroll-three-months-left ()
  "Scroll the displayed calendar left by three months."
  (interactive)
  (org-funcall-in-calendar
   #'calendar-scroll-left-three-months nil 1))

(defun org-calendar-scroll-three-months-right ()
  "Scroll the displayed calendar right by three months."
  (interactive)
  (org-funcall-in-calendar
   #'calendar-scroll-right-three-months nil 1))

(defun org-calendar-select ()
  "Return to `org-read-date' with the date currently selected.
This is used by `org-read-date' in a temporary keymap for the calendar buffer."
  (interactive)
  (when (calendar-cursor-to-date)
    (let* ((date (calendar-cursor-to-date))
	   (time (org-encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
      (setq org-read-date--calendar-selected-date (format-time-string "%Y-%m-%d" time)))
    (when (active-minibuffer-window) (exit-minibuffer))))

(defun org-calendar-select-mouse (ev)
  "Return to `org-read-date' with the date currently selected.
This is used by `org-read-date' in a temporary keymap for the calendar buffer."
  (interactive "e")
  (mouse-set-point ev)
  (when (calendar-cursor-to-date)
    (let* ((date (calendar-cursor-to-date))
	   (time (org-encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
      (setq org-read-date--calendar-selected-date (format-time-string "%Y-%m-%d" time)))
    (when (active-minibuffer-window) (exit-minibuffer))))

(provide 'org-read-date)

;;; org-read-date.el ends here


