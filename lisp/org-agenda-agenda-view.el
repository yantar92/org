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

(defun org-agenda--agenda-block-header (span day-numbers)
  "Produce block header for agenda block displaying SPAN.
DAY-NUMBERS is the list of displayed days."
  (let* ((d1 (car day-numbers))
	 (d2 (org-last day-numbers))
	 (w1 (org-days-to-iso-week d1))
	 (w2 (org-days-to-iso-week d2)))
    (concat (org-agenda-span-name span)
	    "-agenda"
	    (cond ((<= 350 (- d2 d1)) "")
                  ((= w1 w2) (format " (W%02d)" w1))
                  (t (format " (W%02d-W%02d)" w1 w2)))
	    ":\n")))

(defun org-agenda--insert-clock-report (tstart tend)
  "Insert clock report at point in agenda for TSTART..TEND period."
  (let ((params (copy-sequence org-agenda-clockreport-parameter-plist))
	tbl)
    (setq params (plist-put params :tstart tstart))
    (setq params (org-plist-delete params :block))
    (setq params (plist-put params :tend tend))
    ;; Default 'agenda scope for clock table ignores agenda restriction.
    ;; So, we need to list contributing files explicitly.
    (setq params (plist-put params :scope (org-agenda-files nil 'ifmode)))
    (require 'org-clocktable)
    (setq tbl (apply #'org-clock-get-clocktable params))
    (when org-agenda-clock-report-header
      (insert (propertize org-agenda-clock-report-header 'face 'org-agenda-structure))
      (unless (string-suffix-p "\n" org-agenda-clock-report-header)
        (insert "\n")))
    (insert tbl)))

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

  (let ((today (org-today))
        (entry-types (copy-sequence org-agenda-entry-types)))
    
    (when (and (integerp arg) (> arg 0))
      (setq span arg
            arg nil))
    (when (numberp span)
      (unless (< 0 span)
        (user-error "Agenda creation impossible for this span(=%d days)" span)))
    (setq span (org-agenda-ndays-to-span (or span org-agenda-span)))
    
    (setq start-day (or start-day org-agenda-start-day today))
    (when (stringp start-day)
      ;; Convert to an absolute day number
      (setq start-day (time-to-days (org-read-date nil t start-day))))

    ;; Honor with-hour
    (when with-hour
      (when (member :deadline entry-types)
	(setq entry-types (delq :deadline entry-types))
	(push :deadline* entry-types))
      (when (member :scheduled entry-types)
	(setq entry-types (delq :scheduled entry-types))
	(push :scheduled* entry-types)))

    ;; Honor `org-agenda-include-deadlines'.
    (unless org-agenda-include-deadlines
      (setq entry-types (delq :deadline* (delq :deadline entry-types))))
    
    (let* ((ndays (org-agenda-span-to-ndays span start-day))
           (start (if (or (null org-agenda-start-on-weekday)
			  (< ndays 7))
		      start-day
		    (let* ((nt (calendar-day-of-week
			        (calendar-gregorian-from-absolute start-day)))
			   (n1
                            (and (or (eq ndays 7) (eq ndays 14))
	                         org-agenda-start-on-weekday))
			   (d (- nt n1)))
		      (- start-day (+ (if (< d 0) 7 0) d)))))
           (day-numbers (list start)))
      
      (dotimes (_ (1- ndays))
        (push (1+ (car day-numbers)) day-numbers))
      (setq day-numbers (nreverse day-numbers))

      (setq-local org-starting-day (car day-numbers))
      (setq-local org-agenda-current-span (org-agenda-ndays-to-span span))

      (org-agenda-insert-block
       'agenda
       (lambda ()
         (let* ((day-cnt 0) origin date todayp)
           ;; Insert list of entries for each day in the agenda span.
           (dolist (current-day-number day-numbers)
	     (setq date (calendar-gregorian-from-absolute current-day-number)
                   todayp (= current-day-number today)
                   day-cnt (1+ day-cnt)
	           origin (point))
             ;; Insert entries for DATE.
             (let ((day-entries nil))
               (setq day-entries
                     (org-agenda-mapcan-files
                      (lambda ()
                        (apply
                         #'org-agenda-get-day-entries-1
                         date
                         (cond
                          ((memq org-agenda-show-log '(only clockcheck))
                           '(:closed))
                          (org-agenda-show-log
                           (append '(:closed) entry-types))
                          (t entry-types))))
                      :files-or-buffers 'current-agenda
                      :restriction 'agenda-restriction))
	       
	       (when org-agenda-include-diary
	         (let ((org-agenda-search-headline-for-time t))
	           (require 'org-agenda-diary)
	           (setq day-entries
                         (append day-entries
                                 (org-get-entries-from-diary date)))))

               ;; Only insert date header when there are actual enties
               ;; on that date, or when user requests to show all
               ;; dates by force.
	       (when (or day-entries org-agenda-show-all-dates)
                 ;; Date header
	         (insert
	          (if (stringp org-agenda-format-date)
	              (format-time-string org-agenda-format-date
				          (org-time-from-absolute date))
	            (funcall org-agenda-format-date date))
	          "\n")

                 (add-text-properties
                  origin (1- (point))
                  `( face ,(org-agenda-get-day-face date)
                     org-date-line t
                     org-agenda-date-header t
                     org-day-cnt ,day-cnt))
	         (when todayp
	           (put-text-property origin (1- (point)) 'org-today t))
                 
	         (setq day-entries
		       (org-agenda-add-time-grid-maybe day-entries ndays todayp))
	         (when day-entries
                   (insert (org-agenda-finalize-entries day-entries 'agenda) "\n"))
                 
                 (add-text-properties
                  origin (1- (point))
                  `( org-day-cnt ,day-cnt
                     day ,current-day-number)))))
           ;; Append clock report after all the day listings as needed.
           (when org-agenda-clockreport-mode
             (org-agenda--insert-clock-report
              (car day-numbers)
              (1+ (or (org-last day-numbers) 0))))
           ;; Display clockcheck overlays.
           (when (eq org-agenda-show-log 'clockcheck)
	     (org-agenda-show-clocking-issues))))
       :suggested-buffer-name (cons "agenda" nil)
       :block-header
       (cons (org-agenda--agenda-block-header span day-numbers)
             '( face org-agenda-structure
                org-date-line t))
       :redo-command
       `(org-agenda-list (quote ,arg) ,start-day (quote ,span) ,with-hour)
       :block-args (list arg start-day span with-hour)))))

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
