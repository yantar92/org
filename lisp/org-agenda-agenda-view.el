;;; org-agenda-agenda-view.el --- Org agenda view  -*- lexical-binding: t; -*-

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

;; This file implements agenda view listing entries for specific date
;; span.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-agenda-mode)
(require 'org-agenda-search)

(defvar org-agenda-start-day nil  ; dynamically scoped parameter
  "Start day for the agenda view.
Custom commands can set this variable in the options section.
This is usually a string like \"2007-11-01\", \"+2d\" or any other
input allowed when reading a date through the Org calendar.
See the docstring of `org-read-date' for details.

This variable has no effect when `org-agenda-start-on-weekday' is set
and agenda spans 7 or 14 days.")

(defvar org-starting-day nil) ; local variable in the agenda buffer
(defvar org-arg-loc nil) ; local variable

(defvar org-agenda-current-span nil
  "The current span used in the agenda view.") ; local variable in the agenda buffer

;;; Custom options

(defgroup org-agenda-daily/weekly nil
  "Options concerning the daily/weekly agenda."
  :tag "Org Agenda Daily/Weekly"
  :group 'org-agenda)

(defcustom org-agenda-include-diary nil
  "If non-nil, include in the agenda entries from the Emacs Calendar's diary.
Custom commands can set this variable in the options section."
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-include-deadlines t
  "If non-nil, include entries within their deadline warning period.
Custom commands can set this variable in the options section."
  :group 'org-agenda-daily/weekly
  :version "24.1"
  :type 'boolean)

(defcustom org-agenda-span 'week
  "Number of days to include in overview display.
Can be day, week, month, year, or any number of days.
Custom commands can set this variable in the options section."
  :group 'org-agenda-daily/weekly
  :type '(choice (const :tag "Day" day)
		 (const :tag "Week" week)
		 (const :tag "Fortnight" fortnight)
		 (const :tag "Month" month)
		 (const :tag "Year" year)
		 (integer :tag "Custom")))

(defcustom org-agenda-start-on-weekday 1
  "Non-nil means start the overview always on the specified weekday.
0 denotes Sunday, 1 denotes Monday, etc.
When nil, always start on the current day.
Custom commands can set this variable in the options section.

This variable only applies when agenda spans either 7 or 14 days."
  :group 'org-agenda-daily/weekly
  :type '(choice (const :tag "Today" nil)
		 (integer :tag "Weekday No.")))

(defcustom org-agenda-show-all-dates t
  "Non-nil means `org-agenda' shows every day in the selected range.
When nil, only the days which actually have entries are shown."
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-day-face-function nil
  "Function called to determine what face should be used to display a day.
The only argument passed to that function is the day.  It should
returns a face, or nil if does not want to specify a face and let
the normal rules apply."
  :group 'org-agenda-line-format
  :version "24.1"
  :type '(choice (const nil) (function)))

(defcustom org-agenda-weekend-days '(6 0)
  "Which days are weekend?
These days get the special face `org-agenda-date-weekend' in the agenda."
  :group 'org-agenda-daily/weekly
  :type '(set :greedy t
	      (const :tag "Monday" 1)
	      (const :tag "Tuesday" 2)
	      (const :tag "Wednesday" 3)
	      (const :tag "Thursday" 4)
	      (const :tag "Friday" 5)
	      (const :tag "Saturday" 6)
	      (const :tag "Sunday" 0)))

(defcustom org-agenda-format-date 'org-agenda-format-date-aligned
  "Format string for displaying dates in the agenda.
Used by the daily/weekly agenda.  This should be a format string
understood by `format-time-string', or a function returning the
formatted date as a string.  The function must take a single
argument, a calendar-style date list like (month day year)."
  :group 'org-agenda-daily/weekly
  :type '(choice
	  (string :tag "Format string")
	  (function :tag "Function")))

;;; Custom agenda components

;;;###autoload
(defun org-agenda-toggle-diary ()
  "Toggle diary inclusion in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-agenda-include-diary (not org-agenda-include-diary))
  (org-agenda-redo)
  (org-agenda-set-mode-name)
  (message "Diary inclusion turned %s"
	   (if org-agenda-include-diary "on" "off")))

;;;###autoload
(defun org-agenda-log-mode (&optional special)
  "Toggle log mode in an agenda buffer.

With argument SPECIAL, show all possible log items, not only the ones
configured in `org-agenda-log-mode-items'.

With a `\\[universal-argument] \\[universal-argument]' prefix, show *only* \
log items, nothing else."
  (interactive "P")
  (org-agenda-check-type t 'agenda)
  (setq org-agenda-show-log
	(cond
	 ((equal special '(16)) 'only)
	 ((eq special 'clockcheck)
	  (if (eq org-agenda-show-log 'clockcheck)
	      nil 'clockcheck))
	 (special '(closed clock state))
	 (t (not org-agenda-show-log))))
  (org-agenda-set-mode-name)
  (org-agenda-redo)
  (message "Log mode is %s" (if org-agenda-show-log "on" "off")))

;;;###autoload
(defun org-agenda-toggle-deadlines ()
  "Toggle inclusion of entries with a deadline in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-agenda-include-deadlines (not org-agenda-include-deadlines))
  (org-agenda-redo)
  (org-agenda-set-mode-name)
  (message "Deadlines inclusion turned %s"
	   (if org-agenda-include-deadlines "on" "off")))

;;;###autoload
(defun org-agenda-toggle-time-grid ()
  "Toggle time grid in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-agenda-use-time-grid (not org-agenda-use-time-grid))
  (org-agenda-redo)
  (org-agenda-set-mode-name)
  (message "Time-grid turned %s"
	   (if org-agenda-use-time-grid "on" "off")))

;;; Clock report in agenda blocks

(defcustom org-agenda-clock-report-header nil
  "Header inserted before the table in Org agenda clock report mode.

See Info node `(org) Agenda Commands' for more details."
  :group 'org-agenda
  :type '(choice
          (string :tag "Header")
          (const :tag "No header" nil))
  :safe #'stringp
  :package-version '(Org . "9.6"))

(defcustom org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2)
  "Property list with parameters for the clocktable in clockreport mode.
This is the display mode that shows a clock table in the daily/weekly
agenda, the properties for this dynamic block can be set here.
The usual clocktable parameters are allowed here, but you cannot set
the properties :name, :tstart, :tend, :block, and :scope - these will
be overwritten to make sure the content accurately reflects the
current display in the agenda."
  :group 'org-agenda-daily/weekly
  :type 'plist)

(defcustom org-agenda-clock-consistency-checks
  '(:max-duration "10:00" :min-duration 0 :max-gap "0:05"
		  :gap-ok-around ("4:00")
		  :default-face ((:background "DarkRed") (:foreground "white"))
		  :overlap-face nil :gap-face nil :no-end-time-face nil
		  :long-face nil :short-face nil)
  "This is a property list, with the following keys:

:max-duration    Mark clocking chunks that are longer than this time.
                 This is a time string like \"HH:MM\", or the number
                 of minutes as an integer.

:min-duration    Mark clocking chunks that are shorter that this.
                 This is a time string like \"HH:MM\", or the number
                 of minutes as an integer.

:max-gap         Mark gaps between clocking chunks that are longer than
                 this duration.  A number of minutes, or a string
                 like \"HH:MM\".

:gap-ok-around   List of times during the day which are usually not working
                 times.  When a gap is detected, but the gap contains any
                 of these times, the gap is *not* reported.  For example,
                 if this is (\"4:00\" \"13:00\") then gaps that contain
                 4:00 in the morning (i.e. the night) and 13:00
                 (i.e. a typical lunch time) do not cause a warning.
                 You should have at least one time during the night in this
                 list, or otherwise the first task each morning will trigger
                 a warning because it follows a long gap.

Furthermore, the following properties can be used to define faces for
issue display.

:default-face         the default face, if the specific face is undefined
:overlap-face         face for overlapping clocks
:gap-face             face for gaps between clocks
:no-end-time-face     face for incomplete clocks
:long-face            face for clock intervals that are too long
:short-face           face for clock intervals that are too short"
  :group 'org-agenda-daily/weekly
  :group 'org-clock
  :version "24.1"
  :type 'plist)

;;;###autoload
(defun org-agenda-clockreport-mode ()
  "Toggle clocktable mode in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-agenda-clockreport-mode (not org-agenda-clockreport-mode))
  (org-agenda-set-mode-name)
  (org-agenda-redo)
  (message "Clocktable mode is %s"
	   (if org-agenda-clockreport-mode "on" "off")))

;;; Agenda block

(declare-function org-clock-get-clocktable "org-clocktable" (&rest props))
(declare-function org-get-entries-from-diary "org-agenda-diary" (date))
(declare-function org-read-date "org-read-date"
                  (&optional with-time to-time from-string prompt
			     default-time default-input inactive))
;;;###autoload
(defun org-agenda-list (&optional arg start-day span with-hour)
  "Produce a daily/weekly view from all files in variable `org-agenda-files'.
The view will be for the current day or week, but from the overview buffer
you will be able to go to other days/weeks.

With a numeric prefix argument in an interactive call, the agenda will
span ARG days.  Lisp programs should instead specify SPAN to change
the number of days.  SPAN defaults to `org-agenda-span'.

START-DAY defaults to TODAY, or to the most recent match for the weekday
given in `org-agenda-start-on-weekday'.

When WITH-HOUR is non-nil, only include scheduled and deadline
items if they have an hour specification like [h]h:mm."
  (interactive "P")
  (when org-agenda-overriding-arguments
    (setq arg (car org-agenda-overriding-arguments)
	  start-day (nth 1 org-agenda-overriding-arguments)
	  span (nth 2 org-agenda-overriding-arguments)))
  (when (and (integerp arg) (> arg 0))
    (setq span arg arg nil))
  (when (numberp span)
    (unless (< 0 span)
      (user-error "Agenda creation impossible for this span(=%d days)" span)))
  (catch 'exit
    (setq org-agenda-buffer-name
	  (org-agenda--get-buffer-name
	   (and org-agenda-sticky
		(cond ((and org-keys (stringp org-match))
		       (format "*Org Agenda(%s:%s)*" org-keys org-match))
		      (org-keys
		       (format "*Org Agenda(%s)*" org-keys))
		      (t "*Org Agenda(a)*")))))
    (org-agenda-prepare "Day/Week")
    (setq start-day (or start-day org-agenda-start-day))
    (when (stringp start-day)
      ;; Convert to an absolute day number
      (setq start-day (time-to-days (org-read-date nil t start-day))))
    (org-compile-prefix-format 'agenda)
    (org-set-sorting-strategy 'agenda)
    (let* ((span (org-agenda-ndays-to-span (or span org-agenda-span)))
	   (today (org-today))
	   (sd (or start-day today))
	   (ndays (org-agenda-span-to-ndays span sd))
	   (org-agenda-start-on-weekday
	    (and (or (eq ndays 7) (eq ndays 14))
		 org-agenda-start-on-weekday))
	   (thefiles (org-agenda-files nil 'ifmode))
	   (files thefiles)
	   (start (if (or (null org-agenda-start-on-weekday)
			  (< ndays 7))
		      sd
		    (let* ((nt (calendar-day-of-week
				(calendar-gregorian-from-absolute sd)))
			   (n1 org-agenda-start-on-weekday)
			   (d (- nt n1)))
		      (- sd (+ (if (< d 0) 7 0) d)))))
	   (day-numbers (list start))
	   (day-cnt 0)
           ;; FIXME: This may cause confusion when users are trying to
           ;; debug agenda.  The debugger will not trigger without
           ;; redisplay.
	   (inhibit-redisplay (not debug-on-error))
	   s rtn rtnall file date d start-pos end-pos todayp ;; e
	   clocktable-start clocktable-end) ;; filter
      (setq org-agenda-redo-command
	    (list 'org-agenda-list (list 'quote arg) start-day (list 'quote span) with-hour))
      (dotimes (_ (1- ndays))
	(push (1+ (car day-numbers)) day-numbers))
      (setq day-numbers (nreverse day-numbers))
      (setq clocktable-start (car day-numbers)
	    clocktable-end (1+ (or (org-last day-numbers) 0)))
      (setq-local org-starting-day (car day-numbers))
      (setq-local org-arg-loc arg)
      (setq-local org-agenda-current-span (org-agenda-ndays-to-span span))
      (unless org-agenda-compact-blocks
	(let* ((d1 (car day-numbers))
	       (d2 (org-last day-numbers))
	       (w1 (org-days-to-iso-week d1))
	       (w2 (org-days-to-iso-week d2)))
	  (setq s (point))
	  (org-agenda--insert-overriding-header
	    (concat (org-agenda-span-name span)
		    "-agenda"
		    (cond ((<= 350 (- d2 d1)) "")
                          ((= w1 w2) (format " (W%02d)" w1))
                          (t (format " (W%02d-W%02d)" w1 w2)))
		    ":\n")))
	;; Add properties if we actually inserted a header.
	(when (> (point) s)
	  (add-text-properties s (1- (point))
			       (list 'face 'org-agenda-structure
				     'org-date-line t))
	  (org-agenda-mark-header-line s)))
      (while (setq d (pop day-numbers))
	(setq date (calendar-gregorian-from-absolute d)
	      s (point))
	(if (or (setq todayp (= d today))
		(and (not start-pos) (= d sd)))
	    (setq start-pos (point))
	  (when (and start-pos (not end-pos))
	    (setq end-pos (point))))
	(setq files thefiles
	      rtnall nil)
	(while (setq file (pop files))
	  (catch 'nextfile
	    (org-check-agenda-file file)
	    (let ((org-agenda-entry-types org-agenda-entry-types))
	      ;; Starred types override non-starred equivalents
	      (when (member :deadline* org-agenda-entry-types)
		(setq org-agenda-entry-types
		      (delq :deadline org-agenda-entry-types)))
	      (when (member :scheduled* org-agenda-entry-types)
		(setq org-agenda-entry-types
		      (delq :scheduled org-agenda-entry-types)))
	      ;; Honor with-hour
	      (when with-hour
		(when (member :deadline org-agenda-entry-types)
		  (setq org-agenda-entry-types
			(delq :deadline org-agenda-entry-types))
		  (push :deadline* org-agenda-entry-types))
		(when (member :scheduled org-agenda-entry-types)
		  (setq org-agenda-entry-types
			(delq :scheduled org-agenda-entry-types))
		  (push :scheduled* org-agenda-entry-types)))
	      (unless org-agenda-include-deadlines
		(setq org-agenda-entry-types
		      (delq :deadline* (delq :deadline org-agenda-entry-types))))
	      (cond
	       ((memq org-agenda-show-log '(only clockcheck))
		(setq rtn (org-agenda-get-day-entries
			   file date :closed)))
	       (org-agenda-show-log
		(setq rtn (apply #'org-agenda-get-day-entries
				 file date
				 (append '(:closed) org-agenda-entry-types))))
	       (t
		(setq rtn (apply #'org-agenda-get-day-entries
				 file date
				 org-agenda-entry-types)))))
	    (setq rtnall (append rtnall rtn)))) ;; all entries
	(when org-agenda-include-diary
	  (let ((org-agenda-search-headline-for-time t))
	    (require 'org-agenda-diary)
	    (setq rtn (org-get-entries-from-diary date))
	    (setq rtnall (append rtnall rtn))))
	(when (or rtnall org-agenda-show-all-dates)
	  (setq day-cnt (1+ day-cnt))
	  (insert
	   (if (stringp org-agenda-format-date)
	       (format-time-string org-agenda-format-date
				   (org-time-from-absolute date))
	     (funcall org-agenda-format-date date))
	   "\n")
	  (put-text-property s (1- (point)) 'face
			     (org-agenda-get-day-face date))
	  (put-text-property s (1- (point)) 'org-date-line t)
	  (put-text-property s (1- (point)) 'org-agenda-date-header t)
	  (put-text-property s (1- (point)) 'org-day-cnt day-cnt)
	  (when todayp
	    (put-text-property s (1- (point)) 'org-today t))
	  (setq rtnall
		(org-agenda-add-time-grid-maybe rtnall ndays todayp))
	  (when rtnall (insert ;; all entries
			(org-agenda-finalize-entries rtnall 'agenda)
			"\n"))
	  (put-text-property s (1- (point)) 'day d)
	  (put-text-property s (1- (point)) 'org-day-cnt day-cnt)))
      (when (and org-agenda-clockreport-mode clocktable-start)
	(let ((org-agenda-files (org-agenda-files nil 'ifmode))
	      ;; the above line is to ensure the restricted range!
	      (p (copy-sequence org-agenda-clockreport-parameter-plist))
	      tbl)
	  (setq p (org-plist-delete p :block))
	  (setq p (plist-put p :tstart clocktable-start))
	  (setq p (plist-put p :tend clocktable-end))
	  (setq p (plist-put p :scope 'agenda))
          (require 'org-clocktable)
	  (setq tbl (apply #'org-clock-get-clocktable p))
          (when org-agenda-clock-report-header
            (insert (propertize org-agenda-clock-report-header 'face 'org-agenda-structure))
            (unless (string-suffix-p "\n" org-agenda-clock-report-header)
              (insert "\n")))
	  (insert tbl)))
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (unless (or (not (get-buffer-window org-agenda-buffer-name))
		  (and (pos-visible-in-window-p (point-min))
		       (pos-visible-in-window-p (point-max))))
	(goto-char (1- (point-max)))
	(recenter -1)
	(when (not (pos-visible-in-window-p (or start-pos 1)))
	  (goto-char (or start-pos 1))
	  (recenter 1)))
      (goto-char (or start-pos 1))
      (add-text-properties (point-min) (point-max)
			   `(org-agenda-type agenda
					     org-last-args (,arg ,start-day ,span)
					     org-redo-cmd ,org-agenda-redo-command
					     org-series-cmd ,org-cmd))
      (when (eq org-agenda-show-log 'clockcheck)
	(org-agenda-show-clocking-issues))
      (org-agenda-finalize)
      (setq buffer-read-only t)
      (message ""))))

(defun org-agenda-ndays-to-span (n)
  "Return a span symbol for a span of N days, or N if none matches."
  (cond ((symbolp n) n)
	((= n 1) 'day)
	((= n 7) 'week)
	((= n 14) 'fortnight)
	(t n)))

(defun org-agenda-span-to-ndays (span &optional start-day)
  "Return ndays from SPAN, possibly starting at START-DAY.
START-DAY is an absolute time value."
  (cond ((numberp span) span)
	((eq span 'day) 1)
	((eq span 'week) 7)
	((eq span 'fortnight) 14)
	((eq span 'month)
	 (let ((date (calendar-gregorian-from-absolute start-day)))
	   (calendar-last-day-of-month (car date) (cl-caddr date))))
	((eq span 'year)
	 (let ((date (calendar-gregorian-from-absolute start-day)))
	   (if (calendar-leap-year-p (cl-caddr date)) 366 365)))))

(defun org-agenda-span-name (span)
  "Return a SPAN name."
  (if (null span)
      ""
    (if (symbolp span)
	(capitalize (symbol-name span))
      (format "%d days" span))))

(defun org-agenda-get-day-face (date)
  "Return the face DATE should be displayed with."
  (cond ((and (functionp org-agenda-day-face-function)
	      (funcall org-agenda-day-face-function date)))
	((and (org-agenda-today-p date)
              (memq (calendar-day-of-week date) org-agenda-weekend-days))
         'org-agenda-date-weekend-today)
	((org-agenda-today-p date) 'org-agenda-date-today)
	((memq (calendar-day-of-week date) org-agenda-weekend-days)
	 'org-agenda-date-weekend)
	(t 'org-agenda-date)))

(defun org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date))
	 (day (cadr date))
	 (day-of-week (calendar-day-of-week date))
	 (month (car date))
	 (monthname (calendar-month-name month))
	 (year (nth 2 date))
	 (iso-week (org-days-to-iso-week
		    (calendar-absolute-from-gregorian date)))
	 ;; (weekyear (cond ((and (= month 1) (>= iso-week 52))
	 ;;        	  (1- year))
	 ;;        	 ((and (= month 12) (<= iso-week 1))
	 ;;        	  (1+ year))
	 ;;        	 (t year)))
	 (weekstring (if (= day-of-week 1)
			 (format " W%02d" iso-week)
		       "")))
    (format "%-10s %2d %s %4d%s"
	    dayname day monthname year weekstring)))

;;; Clock check

(defun org-agenda-show-clocking-issues ()
  "Add overlays, showing issues with clocking.
See also the user option `org-agenda-clock-consistency-checks'."
  (interactive)
  (let* ((pl org-agenda-clock-consistency-checks)
	 (re (concat "^[ \t]*"
		     org-clock-string
		     "[ \t]+"
		     "\\(\\[.*?\\]\\)"	; group 1 is first stamp
		     "\\(-\\{1,3\\}\\(\\[.*?\\]\\)\\)?")) ; group 3 is second
	 (tlstart 0.)
	 (tlend 0.)
	 (maxtime (org-duration-to-minutes
		   (or (plist-get pl :max-duration) "24:00")))
	 (mintime (org-duration-to-minutes
		   (or (plist-get pl :min-duration) 0)))
	 (maxgap  (org-duration-to-minutes
		   ;; default 30:00 means never complain
		   (or (plist-get pl :max-gap) "30:00")))
	 (gapok (mapcar #'org-duration-to-minutes
			(plist-get pl :gap-ok-around)))
	 (def-face (or (plist-get pl :default-face)
		       '((:background "DarkRed") (:foreground "white"))))
	 issue face m te ts dt ov)
    (goto-char (point-min))
    (while (re-search-forward " Clocked: +(\\(?:-\\|\\([0-9]+:[0-9]+\\)\\))" nil t)
      (setq issue nil face def-face)
      (catch 'next
	(setq m (org-get-at-bol 'org-marker)
	      te nil ts nil)
	(unless (and m (markerp m))
	  (setq issue "No valid clock line") (throw 'next t))
	(org-with-point-at m
	  (save-excursion
            (goto-char (line-beginning-position))
	    (unless (looking-at re)
	      (error "No valid Clock line")
	      (throw 'next t))
	    (unless (match-end 3)
	      (setq issue
		    (format
		     "No end time: (%s)"
		     (org-duration-from-minutes
		      (floor
		       (- (float-time (org-current-time))
			  (float-time (org-time-string-to-time (match-string 1))))
		       60)))
		    face (or (plist-get pl :no-end-time-face) face))
	      (throw 'next t))
	    (setq ts (match-string 1)
		  te (match-string 3)
		  ts (float-time (org-time-string-to-time ts))
		  te (float-time (org-time-string-to-time te))
		  dt (- te ts))))
	(cond
	 ((> dt (* 60 maxtime))
	  ;; a very long clocking chunk
	  (setq issue (format "Clocking interval is very long: %s"
			      (org-duration-from-minutes (floor dt 60)))
		face (or (plist-get pl :long-face) face)))
	 ((< dt (* 60 mintime))
	  ;; a very short clocking chunk
	  (setq issue (format "Clocking interval is very short: %s"
			      (org-duration-from-minutes (floor dt 60)))
		face (or (plist-get pl :short-face) face)))
	 ((and (> tlend 0) (< ts tlend))
	  ;; Two clock entries are overlapping
	  (setq issue (format "Clocking overlap: %d minutes"
			      (/ (- tlend ts) 60))
		face (or (plist-get pl :overlap-face) face)))
	 ((and (> tlend 0) (> ts (+ tlend (* 60 maxgap))))
	  ;; There is a gap, lets see if we need to report it
	  (unless (org-agenda-check-clock-gap tlend ts gapok)
	    (setq issue (format "Clocking gap: %d minutes"
				(/ (- ts tlend) 60))
		  face (or (plist-get pl :gap-face) face))))
	 (t nil)))
      (setq tlend (or te tlend) tlstart (or ts tlstart))
      (when issue
	;; OK, there was some issue, add an overlay to show the issue
        (setq ov (make-overlay (line-beginning-position) (line-end-position)))
	(overlay-put ov 'before-string
		     (concat
		      (org-add-props
			  (format "%-43s" (concat " " issue))
			  nil
			'face face)
		      "\n"))
	(overlay-put ov 'evaporate t)))))

(defun org-agenda-check-clock-gap (t1 t2 ok-list)
  "Check if gap T1 -> T2 contains one of the OK-LIST time-of-day values."
  (catch 'exit
    (unless ok-list
      ;; there are no OK times for gaps...
      (throw 'exit nil))
    (when (> (- (/ t2 36000) (/ t1 36000)) 24)
      ;; This is more than 24 hours, so it is OK.
      ;; because we have at least one OK time, that must be in the
      ;; 24 hour interval.
      (throw 'exit t))
    ;; We have a shorter gap.
    ;; Now we have to get the minute of the day when these times are
    (let* ((t1dec (decode-time t1))
	   (t2dec (decode-time t2))
	   ;; compute the minute on the day
	   (min1 (+ (nth 1 t1dec) (* 60 (nth 2 t1dec))))
	   (min2 (+ (nth 1 t2dec) (* 60 (nth 2 t2dec)))))
      (when (< min2 min1)
	;; if min2 is smaller than min1, this means it is on the next day.
	;; Wrap it to after midnight.
	(setq min2 (+ min2 1440)))
      ;; Now check if any of the OK times is in the gap
      (mapc (lambda (x)
	      ;; Wrap the time to after midnight if necessary
	      (when (< x min1) (setq x (+ x 1440)))
	      ;; Check if in interval
	      (and (<= min1 x) (>= min2 x) (throw 'exit t)))
	    ok-list)
      ;; Nope, this gap is not OK
      nil)))

(provide 'org-agenda-agenda-view)

;;; org-agenda-agenda-view.el ends here
