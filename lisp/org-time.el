;;; org-time.el --- Org helpers to work with time values                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
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

;; This library provides additional tooling to work with Emacs time
;; objects and their string representation.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'calendar)
(require 'org-timestamp-common)

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

(defmacro org-encode-time (&rest time)
  "Compatibility and convenience helper for `encode-time'.
TIME may be a 9 components list (SECONDS ... YEAR IGNORED DST ZONE)
as the recommended way since Emacs-27 or 6 or 9 separate arguments
similar to the only possible variant for Emacs-26 and earlier.
6 elements list as the only argument causes wrong type argument till
Emacs-29.

Warning: use -1 for DST to guess the actual value, nil means no
daylight saving time and may be wrong at particular time.

DST value is ignored prior to Emacs-27.  Since Emacs-27 DST value matters
even when multiple arguments is passed to this macro and such
behavior is different from `encode-time'.  See
Info node `(elisp)Time Conversion' for details and caveats,
preferably the latest version."
  (if (version< emacs-version "27.1")
      (if (cdr time)
          `(encode-time ,@time)
        `(apply #'encode-time ,@time))
    (if (ignore-errors (with-no-warnings (encode-time '(0 0 0 1 1 1971))))
        (pcase (length time) ; Emacs-29 since d75e2c12eb
          (1 `(encode-time ,@time))
          ((or 6 9) `(encode-time (list ,@time)))
          (_ (error "`org-encode-time' may be called with 1, 6, or 9 arguments but %d given"
                    (length time))))
      (pcase (length time)
        (1 `(encode-time ,@time))
        (6 `(encode-time (list ,@time nil -1 nil)))
        (9 `(encode-time (list ,@time)))
        (_ (error "`org-encode-time' may be called with 1, 6, or 9 arguments but %d given"
                  (length time)))))))

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

(defun org-2ft (s)
  "Convert S to a floating point time.
If S is already a number, just return it.  If it is a string,
parse it as a time string and apply `float-time' to it.  If S is
nil, just return 0."
  (cond
   ((numberp s) s)
   ((stringp s)
    (condition-case nil
	(org-time-string-to-seconds s)
      (error 0)))
   (t 0)))

(defun org-parse-time-string (s &optional nodefault)
  "Parse Org time string S.

If time is not given, defaults to 0:00.  However, with optional
NODEFAULT, hour and minute fields are nil if not given.

Throw an error if S does not contain a valid Org time string.
Note that the first match for YYYY-MM-DD will be used (e.g.,
\"-52000-02-03\" will be taken as \"2000-02-03\").

This should be a lot faster than the `parse-time-string'."
  (unless (string-match org-ts-regexp0 s)
    (error "Not an Org time string: %s" s))
  (list 0
	(cond ((match-beginning 8) (string-to-number (match-string 8 s)))
	      (nodefault nil)
	      (t 0))
	(cond ((match-beginning 7) (string-to-number (match-string 7 s)))
	      (nodefault nil)
	      (t 0))
	(string-to-number (match-string 4 s))
	(string-to-number (match-string 3 s))
	(string-to-number (match-string 2 s))
	nil -1 nil))

(defun org-matcher-time (s)
  "Interpret a time comparison value S as a floating point time.

S can be an Org time stamp, a modifier, e.g., \"<+2d>\", or the
following special strings: \"<now>\", \"<today>\",
\"<tomorrow>\", and \"<yesterday>\".

Return 0. if S is not recognized as a valid value."
  (let ((today (float-time (org-encode-time
                            (append '(0 0 0) (nthcdr 3 (decode-time)))))))
    (save-match-data
      (cond
       ((string= s "<now>") (float-time))
       ((string= s "<today>") today)
       ((string= s "<tomorrow>") (+ 86400.0 today))
       ((string= s "<yesterday>") (- today 86400.0))
       ((string-match "\\`<\\([-+][0-9]+\\)\\([hdwmy]\\)>\\'" s)
	(+ (if (string= (match-string 2 s) "h") (float-time) today)
	   (* (string-to-number (match-string 1 s))
	      (cdr (assoc (match-string 2 s)
			  '(("h" . 3600.0)
			    ("d" . 86400.0)   ("w" . 604800.0)
			    ("m" . 2678400.0) ("y" . 31557600.0)))))))
       ((string-match org-ts-regexp0 s) (org-2ft s))
       (t 0.)))))

(defalias 'org-time-stamp-to-now #'org-timestamp-to-now)
(defun org-timestamp-to-now (timestamp-string &optional seconds)
  "Difference between TIMESTAMP-STRING and now in days.
If SECONDS is non-nil, return the difference in seconds."
  (let ((fdiff (if seconds #'float-time #'time-to-days)))
    (- (funcall fdiff (org-time-string-to-time timestamp-string))
       (funcall fdiff nil))))

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

(defun org-day-of-week (day month year)
  "Return the day of the week as an integer."
  (nth 6
       (decode-time
	(date-to-time
	 (format "%d-%02d-%02dT00:00:00" year month day)))))

(defun org-get-cursor-date (&optional with-time)
  "Return the date at cursor in as a time.
This works in the calendar and in the agenda, anywhere else it just
returns the current time.
If WITH-TIME is non-nil, returns the time of the event at point (in
the agenda) or the current time of the day; otherwise returns the
earliest time on the cursor date that Org treats as that date
\\(bearing in mind `org-extend-today-until')."
  (require 'calendar)
  (let (date day defd tp hod mod)
    (when with-time
      (setq tp (get-text-property (point) 'time))
      (when (and tp (string-match "\\([0-2]?[0-9]\\):\\([0-5][0-9]\\)" tp))
	(setq hod (string-to-number (match-string 1 tp))
	      mod (string-to-number (match-string 2 tp))))
      (or tp (let ((now (decode-time)))
	       (setq hod (nth 2 now)
		     mod (nth 1 now)))))
    (cond
     ((eq major-mode 'calendar-mode)
      (setq date (calendar-cursor-to-date)
	    defd (org-encode-time 0 (or mod 0) (or hod org-extend-today-until)
                                  (nth 1 date) (nth 0 date) (nth 2 date))))
     ((eq major-mode 'org-agenda-mode)
      (setq day (get-text-property (point) 'day))
      (when day
	(setq date (calendar-gregorian-from-absolute day)
	      defd (org-encode-time 0 (or mod 0) (or hod org-extend-today-until)
                                    (nth 1 date) (nth 0 date) (nth 2 date))))))
    (or defd (current-time))))

(declare-function org-clock-get-last-clock-out-time "org-clock-core" ())
(defun org-current-effective-time ()
  "Return current time adjusted for `org-extend-today-until' variable."
  (let* ((ct (org-current-time))
	 (dct (decode-time ct))
	 (ct1
	  (cond
	   (org-use-last-clock-out-time-as-effective-time
            (require 'org-clock-core)
	    (or (org-clock-get-last-clock-out-time) ct))
	   ((and org-use-effective-time (< (nth 2 dct) org-extend-today-until))
	    (org-encode-time 0 59 23 (1- (nth 3 dct)) (nth 4 dct) (nth 5 dct)))
	   (t ct))))
    ct1))

(defun org-today ()
  "Return today date, considering `org-extend-today-until'."
  (time-to-days
   (time-since (* 3600 org-extend-today-until))))

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

(defun org-fix-decoded-time (time)
  "Set 0 instead of nil for the first 6 elements of time.
Don't touch the rest."
  (let ((n 0))
    (mapcar (lambda (x) (if (< (setq n (1+ n)) 7) (or x 0) x)) time)))

(defun org-time= (a b)
  (let ((a (org-2ft a))
	(b (org-2ft b)))
    (and (> a 0) (> b 0) (= a b))))

(defun org-time< (a b)
  (let ((a (org-2ft a))
	(b (org-2ft b)))
    (and (> a 0) (> b 0) (< a b))))

(defun org-time<= (a b)
  (let ((a (org-2ft a))
	(b (org-2ft b)))
    (and (> a 0) (> b 0) (<= a b))))

(defun org-time> (a b)
  (let ((a (org-2ft a))
	(b (org-2ft b)))
    (and (> a 0) (> b 0) (> a b))))

(defun org-time>= (a b)
  (let ((a (org-2ft a))
	(b (org-2ft b)))
    (and (> a 0) (> b 0) (>= a b))))

(defun org-time<> (a b)
  (let ((a (org-2ft a))
	(b (org-2ft b)))
    (and (> a 0) (> b 0) (\= a b))))

(provide 'org-time)

;;; org-time.el ends here
