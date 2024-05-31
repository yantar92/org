;;; org-clock-sum.el --- Calculating clocking stats -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, wp
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

;; This file contains API to retrieve clock statistics.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element)
(require 'org-time)
(require 'org-clock-common)

(defcustom org-clock-report-include-clocking-task nil
  "When non-nil, include the current clocking task time in clock reports."
  :group 'org-clock
  :version "24.1"
  :type 'boolean)

(defvar-local org-clock-file-total-minutes nil
  "Holds the file total time in minutes, after the last call to `org-clock-sum'.")

;;; Helpers

(defun org-count-quarter (n)
  (cond
   ((= n 1) "1st")
   ((= n 2) "2nd")
   ((= n 3) "3rd")
   ((= n 4) "4th")))

(defun org-quarter-to-date (quarter year)
  "Get the date (week day year) of the first day of a given quarter."
  (let (startday)
    (cond
     ((= quarter 1)
      (setq startday (org-day-of-week 1 1 year))
      (cond
       ((= startday 0)
	(list 52 7 (- year 1)))
       ((= startday 6)
	(list 52 6 (- year 1)))
       ((<= startday 4)
	(list 1 startday year))
       ((> startday 4)
	(list 53 startday (- year 1)))
       )
      )
     ((= quarter 2)
      (setq startday (org-day-of-week 1 4 year))
      (cond
       ((= startday 0)
	(list 13 startday year))
       ((< startday 4)
	(list 14 startday year))
       ((>= startday 4)
	(list 13 startday year))
       )
      )
     ((= quarter 3)
      (setq startday (org-day-of-week 1 7 year))
      (cond
       ((= startday 0)
	(list 26 startday year))
       ((< startday 4)
	(list 27 startday year))
       ((>= startday 4)
	(list 26 startday year))
       )
      )
     ((= quarter 4)
      (setq startday (org-day-of-week 1 10 year))
      (cond
       ((= startday 0)
	(list 39 startday year))
       ((<= startday 4)
	(list 40 startday year))
       ((> startday 4)
	(list 39 startday year)))))))

(declare-function calendar-iso-to-absolute "cal-iso" (date))
(declare-function org-read-date "org-read-date"
                  (&optional with-time to-time from-string prompt
			     default-time default-input inactive))
(declare-function org-time-stamp-format "org-element-timestamp"
                  (&optional with-time inactive custom))
(defun org-clock-special-range (key &optional time as-strings wstart mstart)
  "Return two times bordering a special time range.

KEY is a symbol specifying the range and can be one of `today',
`yesterday', `thisweek', `lastweek', `thismonth', `lastmonth',
`thisyear', `lastyear' or `untilnow'.  If set to `interactive',
user is prompted for range boundaries.  It can be a string or an
integer.

By default, a week starts Monday 0:00 and ends Sunday 24:00.  The
range is determined relative to TIME, which defaults to current
time.

The return value is a list containing two internal times, one for
the beginning of the range and one for its end, like the ones
returned by `current-time' or `encode-time' and a string used to
display information.  If AS-STRINGS is non-nil, the returned
times will be formatted strings.  Note that the first element is
always nil when KEY is `untilnow'.

If WSTART is non-nil, use this number to specify the starting day
of a week (monday is 1).  If MSTART is non-nil, use this number
to specify the starting day of a month (1 is the first day of the
month).  If you can combine both, the month starting day will
have priority."
  (let* ((tm (decode-time time))
	 (m (nth 1 tm))
	 (h (nth 2 tm))
	 (d (nth 3 tm))
	 (month (nth 4 tm))
	 (y (nth 5 tm))
	 (dow (nth 6 tm))
	 (skey (format "%s" key))
	 (shift 0)
	 (q (cond ((>= month 10) 4)
		  ((>= month 7) 3)
		  ((>= month 4) 2)
		  (t 1)))
	 h1 d1 month1 y1 shiftedy shiftedm shiftedq) ;; m1
    (cond
     ((string-match "\\`[0-9]+\\'" skey)
      (setq y (string-to-number skey) month 1 d 1 key 'year))
     ((string-match "\\`\\([0-9]+\\)-\\([0-9]\\{1,2\\}\\)\\'" skey)
      (setq y (string-to-number (match-string 1 skey))
	    month (string-to-number (match-string 2 skey))
	    d 1
	    key 'month))
     ((string-match "\\`\\([0-9]+\\)-[wW]\\([0-9]\\{1,2\\}\\)\\'" skey)
      (require 'cal-iso)
      (let ((date (calendar-gregorian-from-absolute
		   (calendar-iso-to-absolute
		    (list (string-to-number (match-string 2 skey))
			  1
			  (string-to-number (match-string 1 skey)))))))
	(setq d (nth 1 date)
	      month (car date)
	      y (nth 2 date)
	      dow 1
	      key 'week)))
     ((string-match "\\`\\([0-9]+\\)-[qQ]\\([1-4]\\)\\'" skey)
      (require 'cal-iso)
      (setq q (string-to-number (match-string 2 skey)))
      (let ((date (calendar-gregorian-from-absolute
		   (calendar-iso-to-absolute
		    (org-quarter-to-date
		     q (string-to-number (match-string 1 skey)))))))
	(setq d (nth 1 date)
	      month (car date)
	      y (nth 2 date)
	      dow 1
	      key 'quarter)))
     ((string-match
       "\\`\\([0-9]+\\)-\\([0-9]\\{1,2\\}\\)-\\([0-9]\\{1,2\\}\\)\\'"
       skey)
      (setq y (string-to-number (match-string 1 skey))
	    month (string-to-number (match-string 2 skey))
	    d (string-to-number (match-string 3 skey))
	    key 'day))
     ((string-match "\\([-+][0-9]+\\)\\'" skey)
      (setq shift (string-to-number (match-string 1 skey))
	    key (intern (substring skey 0 (match-beginning 1))))
      (when (and (memq key '(quarter thisq)) (> shift 0))
	(error "Looking forward with quarters isn't implemented"))))
    (when (= shift 0)
      (pcase key
	(`yesterday (setq key 'today   shift -1))
	(`lastweek  (setq key 'week    shift -1))
	(`lastmonth (setq key 'month   shift -1))
	(`lastyear  (setq key 'year    shift -1))
	(`lastq     (setq key 'quarter shift -1))))
    ;; Prepare start and end times depending on KEY's type.
    (pcase key
      ((or `day `today) (setq m 0
                              h org-extend-today-until
                              h1 (+ 24 org-extend-today-until)
                              d (+ d shift)))
      ((or `week `thisweek)
       (let* ((ws (or wstart 1))
	      (diff (+ (* -7 shift) (mod (+ dow 7 (- ws)) 7))))
	 (setq m 0 h org-extend-today-until d (- d diff) d1 (+ 7 d))))
      ((or `month `thismonth)
       (setq h org-extend-today-until m 0 d (or mstart 1)
             month (+ month shift) month1 (1+ month)))
      ((or `quarter `thisq)
       ;; Compute if this shift remains in this year.  If not, compute
       ;; how many years and quarters we have to shift (via floor*) and
       ;; compute the shifted years, months and quarters.
       (cond
	((< (+ (- q 1) shift) 0)	; Shift not in this year.
	 (let* ((interval (* -1 (+ (- q 1) shift)))
		;; Set tmp to ((years to shift) (quarters to shift)).
		(tmp (cl-floor interval 4)))
	   ;; Due to the use of floor, 0 quarters actually means 4.
	   (if (= 0 (nth 1 tmp))
	       (setq shiftedy (- y (nth 0 tmp))
		     shiftedm 1
		     shiftedq 1)
	     (setq shiftedy (- y (+ 1 (nth 0 tmp)))
		   shiftedm (- 13 (* 3 (nth 1 tmp)))
		   shiftedq (- 5 (nth 1 tmp)))))
	 (setq m 0 h org-extend-today-until d 1
               month shiftedm month1 (+ 3 shiftedm) y shiftedy))
	((> (+ q shift) 0)		; Shift is within this year.
	 (setq shiftedq (+ q shift))
	 (setq shiftedy y)
	 (let ((qshift (* 3 (1- (+ q shift)))))
	   (setq m 0 h org-extend-today-until d 1
                 month (+ 1 qshift) month1 (+ 4 qshift))))))
      ((or `year `thisyear)
       (setq m 0 h org-extend-today-until d 1 month 1 y (+ y shift) y1 (1+ y)))
      ((or `interactive `untilnow))	; Special cases, ignore them.
      (_ (user-error "No such time block %s" key)))
    ;; Format start and end times according to AS-STRINGS.
    (let* ((start (pcase key
		    (`interactive
                     (require 'org-read-date)
                     (org-read-date nil t nil "Range start? "))
		    (`untilnow nil)
		    (_ (org-encode-time 0 m h d month y))))
	   (end (pcase key
		  (`interactive (org-read-date nil t nil "Range end? "))
		  (`untilnow (current-time))
		  (_ (org-encode-time 0
                                      m ;; (or m1 m)
                                      (or h1 h)
                                      (or d1 d)
                                      (or month1 month)
                                      (or y1 y)))))
	   (text
	    (pcase key
	      ((or `day `today) (format-time-string "%A, %B %d, %Y" start))
	      ((or `week `thisweek) (format-time-string "week %G-W%V" start))
	      ((or `month `thismonth) (format-time-string "%B %Y" start))
	      ((or `year `thisyear) (format-time-string "the year %Y" start))
	      ((or `quarter `thisq)
	       (concat (org-count-quarter shiftedq)
		       " quarter of " (number-to-string shiftedy)))
	      (`interactive "(Range interactively set)")
	      (`untilnow "now"))))
      (if (not as-strings) (list start end text)
        (require 'org-element-timestamp)
	(let ((f (org-time-stamp-format 'with-time)))
	  (list (and start (format-time-string f start))
		(format-time-string f end)
		text))))))

;;; Calculating clock sum

(declare-function org-up-heading-safe "org-move" ())
;;;###autoload
(defun org-clock-sum (&optional tstart tend headline-filter propname)
  "Sum the times for each subtree.
Puts the resulting times in minutes as a text property on each headline.
TSTART and TEND can mark a time range to be considered.
HEADLINE-FILTER is a zero-arg function that, if specified, is called for
each headline in the time range with point at the headline.  Headlines for
which HEADLINE-FILTER returns nil are excluded from the clock summation.
PROPNAME lets you set a custom text property instead of :org-clock-minutes."
  (with-silent-modifications
    (let* ((re (concat "^\\(\\*+\\)[ \t]\\|^[ \t]*"
		       org-clock-string
		       "[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)"))
	   (lmax 30)
	   (ltimes (make-vector lmax 0))
	   (level 0)
	   (tstart (cond ((stringp tstart) (org-time-string-to-seconds tstart))
			 ((consp tstart) (float-time tstart))
			 (t tstart)))
	   (tend (cond ((stringp tend) (org-time-string-to-seconds tend))
		       ((consp tend) (float-time tend))
		       (t tend)))
	   (t1 0)
	   time)
      (remove-text-properties (point-min) (point-max)
			      `(,(or propname :org-clock-minutes) t
				:org-clock-force-headline-inclusion t))
      (save-excursion
	(goto-char (point-max))
	(while (re-search-backward re nil t)
          (let* ((element (save-match-data (org-element-at-point)))
                 (element-type (org-element-type element)))
	    (cond
	     ((and (eq element-type 'clock) (match-end 2))
	      ;; Two time stamps.
              (condition-case nil
	          (let* ((timestamp (org-element-property :value element))
		         (ts (float-time
                              (org-encode-time
                               (list 0
                                     (org-element-property :minute-start timestamp)
                                     (org-element-property :hour-start timestamp)
                                     (org-element-property :day-start timestamp)
                                     (org-element-property :month-start timestamp)
                                     (org-element-property :year-start timestamp)
                                     nil -1 nil))))
		         (te (float-time
                              (org-encode-time
                               (list 0
                                     (org-element-property :minute-end timestamp)
                                     (org-element-property :hour-end timestamp)
                                     (org-element-property :day-end timestamp)
                                     (org-element-property :month-end timestamp)
                                     (org-element-property :year-end timestamp)
                                     nil -1 nil))))
		         (dt (- (if tend (min te tend) te)
			        (if tstart (max ts tstart) ts))))
	            (when (> dt 0) (cl-incf t1 (floor dt 60))))
                (error
                 (org-display-warning (format "org-clock-sum: Ignoring invalid %s" (org-current-line-string))))))
	     ((match-end 4)
	      ;; A naked time.
	      (setq t1 (+ t1 (string-to-number (match-string 5))
			  (* 60 (string-to-number (match-string 4))))))
	     ((memq element-type '(headline inlinetask)) ;A headline
	      ;; Add the currently clocking item time to the total.
	      (when (and org-clock-report-include-clocking-task
		         (eq (org-clocking-buffer) (current-buffer))
		         (eq (marker-position org-clock-hd-marker) (point))
		         tstart
		         tend
		         (>= (float-time org-clock-start-time) tstart)
		         (<= (float-time org-clock-start-time) tend))
	        (let ((time (floor (org-time-convert-to-integer
				    (time-since org-clock-start-time))
				   60)))
		  (setq t1 (+ t1 time))))
	      (let* ((headline-forced
		      (get-text-property (point)
				         :org-clock-force-headline-inclusion))
		     (headline-included
		      (or (null headline-filter)
			  (save-excursion
			    (save-match-data (funcall headline-filter))))))
	        (setq level (- (match-end 1) (match-beginning 1)))
	        (when (>= level lmax)
		  (setq ltimes (vconcat ltimes (make-vector lmax 0)) lmax (* 2 lmax)))
	        (when (or (> t1 0) (> (aref ltimes level) 0))
		  (when (or headline-included headline-forced)
		    (if headline-included
		        (cl-loop for l from 0 to level do
			         (aset ltimes l (+ (aref ltimes l) t1))))
		    (setq time (aref ltimes level))
		    (goto-char (match-beginning 0))
                    (put-text-property (point) (line-end-position)
				       (or propname :org-clock-minutes) time)
		    (when headline-filter
                      (require 'org-move)
		      (save-excursion
		        (save-match-data
			  (while (org-up-heading-safe)
			    (put-text-property
			     (point) (line-end-position)
			     :org-clock-force-headline-inclusion t))))))
		  (setq t1 0)
		  (cl-loop for l from level to (1- lmax) do
			   (aset ltimes l 0))))))))
	(setq org-clock-file-total-minutes (aref ltimes 0))))))

(declare-function org-narrow-to-subtree "org-narrow" (&optional element))
(defun org-clock-sum-current-item (&optional tstart)
  "Return time, clocked on current item in total."
  (save-excursion
    (save-restriction
      (if (and (featurep 'org-inlinetask)
               (fboundp 'org-inlinetask-at-task-p)
               (fboundp 'org-inlinetask-in-task-p)
               (fboundp 'org-inlinetask-goto-beginning)
               (fboundp 'org-inlinetask-goto-end)
	       (or (org-inlinetask-at-task-p)
		   (org-inlinetask-in-task-p)))
	  (narrow-to-region (save-excursion (org-inlinetask-goto-beginning) (point))
			    (save-excursion (org-inlinetask-goto-end) (point)))
        (require 'org-narrow)
	(org-narrow-to-subtree))
      (org-clock-sum tstart)
      org-clock-file-total-minutes)))

;;;###autoload
(defun org-clock-sum-today (&optional headline-filter)
  "Sum the times for each subtree for today."
  (let ((range (org-clock-special-range 'today)))
    (org-clock-sum (car range) (cadr range)
		   headline-filter :org-clock-minutes-today)))

(defun org-clock-sum-custom (&optional headline-filter range propname)
  "Sum the times for each subtree for today."
  (let ((r (or (and (symbolp range) (org-clock-special-range range))
	       (org-clock-special-range
		(intern (completing-read
			 "Range: "
			 '("today" "yesterday" "thisweek" "lastweek"
			   "thismonth" "lastmonth" "thisyear" "lastyear"
			   "interactive")
			 nil t))))))
    (org-clock-sum (car r) (cadr r)
		   headline-filter (or propname :org-clock-minutes-custom))))

(provide 'org-clock-sum)

;;; org-clock-sum.el ends here
