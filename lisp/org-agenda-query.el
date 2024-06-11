;;; org-agenda-query.el --- Commands to manipulate Org agenda query  -*- lexical-binding: t; -*-

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

;; This file implements commands that alter current agenda query and
;; refresh results.

;;; Code:

(require 'org-agenda-mode)
(require 'org-agenda-agenda-view)
(require 'org-agenda-commands)
(require 'org-agenda-search-view)

(defcustom org-agenda-query-register ?o
  "The register holding the current query string.
The purpose of this is that if you construct a query string interactively,
you can then use it to define a custom command."
  :group 'org-agenda-custom-commands
  :type 'character)

;;; Manipulate agenda day

;;;###autoload
(defun org-agenda-goto-date (date)
  "Jump to DATE in the agenda buffer.

When called interactively, prompt for the date.
When called from Lisp, DATE should be a date as returned by
`org-read-date'.

See also:
 `org-agenda-earlier'    (\\[org-agenda-earlier])
 `org-agenda-later'      (\\[org-agenda-later])
 `org-agenda-goto-today' (\\[org-agenda-goto-today])"
  (interactive
   (list
    (let ((org-read-date-prefer-future org-agenda-jump-prefer-future))
      (org-read-date))))
  (org-agenda-check-type t 'agenda)
  (let* ((day (time-to-days (org-time-string-to-time date)))
         (org-agenda-sticky-orig org-agenda-sticky)
         (org-agenda-buffer-tmp-name (buffer-name))
         (args (get-text-property (min (1- (point-max)) (point))
                                  'org-last-args))
         (0-arg (or current-prefix-arg (car args)))
         (2-arg (nth 2 args))
         (with-hour-p (nth 4 org-agenda-redo-command))
         (newcmd (list 'org-agenda-list 0-arg date
                       (org-agenda-span-to-ndays
                        2-arg (org-time-string-to-absolute date))
                       with-hour-p))
         (newargs (cdr newcmd))
         (inhibit-read-only t)
         org-agenda-sticky)
    (add-text-properties (point-min) (point-max)
                         `(org-redo-cmd ,newcmd org-last-args ,newargs))
    (org-agenda-redo)
    (goto-char (point-min))
    (while (not (or (= (or (get-text-property (point) 'day) 0) day)
                  (save-excursion (move-beginning-of-line 2) (eobp))))
      (move-beginning-of-line 2))
    (setq org-agenda-sticky org-agenda-sticky-orig
          org-agenda-this-buffer-is-sticky org-agenda-sticky)))

;;;###autoload
(defun org-agenda-goto-today ()
  "Go to today's date in the agenda buffer.

See also:
 `org-agenda-later'     (\\[org-agenda-later])
 `org-agenda-earlier'   (\\[org-agenda-earlier])
 `org-agenda-goto-date' (\\[org-agenda-goto-date])"
  (interactive)
  (org-agenda-check-type t 'agenda)
  (let* ((args (get-text-property (min (1- (point-max)) (point)) 'org-last-args))
	 (curspan (nth 2 args))
	 (tdpos (text-property-any (point-min) (point-max) 'org-today t)))
    (cond
     (tdpos (goto-char tdpos))
     ((eq org-agenda-type 'agenda)
      (let* ((sd (org-agenda-compute-starting-span
		  (org-today) (or curspan org-agenda-span)))
	     (org-agenda-overriding-arguments args))
	(setf (nth 1 org-agenda-overriding-arguments) sd)
	(org-agenda-redo)
	(org-agenda-find-same-or-today-or-agenda)))
     (t (error "Cannot find today")))))

(defun org-agenda-find-same-or-today-or-agenda (&optional cnt)
  (goto-char
   (or (and cnt (text-property-any (point-min) (point-max) 'org-day-cnt cnt))
       (text-property-any (point-min) (point-max) 'org-today t)
       (text-property-any (point-min) (point-max) 'org-agenda-type 'agenda)
       (and (get-text-property (min (1- (point-max)) (point)) 'org-series)
	    (org-agenda-backward-block))
       (point-min))))

;;;###autoload
(defun org-agenda-later (arg)
  "Go forward in time by the current span in the agenda buffer.
With prefix ARG, go forward that many times the current span.

See also:
 `org-agenda-earlier'    (\\[org-agenda-earlier])
 `org-agenda-goto-today' (\\[org-agenda-goto-today])
 `org-agenda-goto-date'  (\\[org-agenda-goto-date])"
  (interactive "p")
  (org-agenda-check-type t 'agenda)
  (let* ((wstart (window-start))
         (args (get-text-property (min (1- (point-max)) (point)) 'org-last-args))
	 (span (or (nth 2 args) org-agenda-current-span))
	 (sd (or (nth 1 args) (org-get-at-bol 'day) org-starting-day))
	 (greg (calendar-gregorian-from-absolute sd))
	 (cnt (org-get-at-bol 'org-day-cnt))
	 greg2)
    (cond
     ((numberp span)
      (setq sd (+ (* span arg) sd)))
     ((eq span 'day)
      (setq sd (+ arg sd)))
     ((eq span 'week)
      (setq sd (+ (* 7 arg) sd)))
     ((eq span 'fortnight)
      (setq sd (+ (* 14 arg) sd)))
     ((eq span 'month)
      (setq greg2 (list (+ (car greg) arg) (nth 1 greg) (nth 2 greg))
	    sd (calendar-absolute-from-gregorian greg2))
      (setcar greg2 (1+ (car greg2))))
     ((eq span 'year)
      (setq greg2 (list (car greg) (nth 1 greg) (+ arg (nth 2 greg)))
	    sd (calendar-absolute-from-gregorian greg2))
      (setcar (nthcdr 2 greg2) (1+ (nth 2 greg2))))
     (t
      (setq sd (+ (* span arg) sd))))
    (let ((org-agenda-overriding-cmd
	   ;; `cmd' may have been set by `org-agenda-run-series' which
	   ;; uses `org-agenda-overriding-cmd' to decide whether
	   ;; overriding is allowed for `cmd'
	   (get-text-property (min (1- (point-max)) (point)) 'org-series-cmd))
	  (org-agenda-overriding-arguments
	   (list (car args) sd span)))
      (org-agenda-redo)
      (org-agenda-find-same-or-today-or-agenda cnt))
    (set-window-start nil wstart)))

;;;###autoload
(defun org-agenda-earlier (arg)
  "Go backward in time by the current span in the agenda buffer.
With prefix ARG, go backward that many times the current span.

See also:
 `org-agenda-later'      (\\[org-agenda-later])
 `org-agenda-goto-today' (\\[org-agenda-goto-today])
 `org-agenda-goto-date'  (\\[org-agenda-goto-date])"
  (interactive "p")
  (org-agenda-later (- arg)))

;;; Manipulate agenda span

;;;###autoload
(defun org-agenda-reset-view ()
  "Switch to default view for agenda."
  (interactive)
  (org-agenda-change-time-span org-agenda-span))

;;;###autoload
(defun org-agenda-day-view (&optional day-of-month)
  "Switch to daily view for agenda.
With argument DAY-OF-MONTH, switch to that day of the month."
  (interactive "P")
  (org-agenda-change-time-span 'day day-of-month))

;;;###autoload
(defun org-agenda-week-view (&optional iso-week)
  "Switch to weekly view for agenda.
With argument ISO-WEEK, switch to the corresponding ISO week.
If ISO-WEEK has more then 2 digits, only the last two encode
the week.  Any digits before this encode a year.  So 200712
means week 12 of year 2007.  Years ranging from 70 years ago
to 30 years in the future can also be written as 2-digit years."
  (interactive "P")
  (org-agenda-change-time-span 'week iso-week))

;;;###autoload
(defun org-agenda-fortnight-view (&optional iso-week)
  "Switch to fortnightly view for agenda.
With argument ISO-WEEK, switch to the corresponding ISO week.
If ISO-WEEK has more then 2 digits, only the last two encode
the week.  Any digits before this encode a year.  So 200712
means week 12 of year 2007.  Years ranging from 70 years ago
to 30 years in the future can also be written as 2-digit years."
  (interactive "P")
  (org-agenda-change-time-span 'fortnight iso-week))

;;;###autoload
(defun org-agenda-month-view (&optional month)
  "Switch to monthly view for agenda.
With argument MONTH, switch to that month.  If MONTH has more
then 2 digits, only the last two encode the month.  Any digits
before this encode a year.  So 200712 means December year 2007.
Years ranging from 70 years ago to 30 years in the future can
also be written as 2-digit years."
  (interactive "P")
  (org-agenda-change-time-span 'month month))

;;;###autoload
(defun org-agenda-year-view (&optional year)
  "Switch to yearly view for agenda.
With argument YEAR, switch to that year.  Years ranging from 70
years ago to 30 years in the future can also be written as
2-digit years."
  (interactive "P")
  (when year
    (setq year (org-small-year-to-year year)))
  (if (y-or-n-p "Are you sure you want to compute the agenda for an entire year? ")
      (org-agenda-change-time-span 'year year)
    (error "Abort")))

(defun org-agenda-change-time-span (span &optional n)
  "Change the agenda view to SPAN.
SPAN may be `day', `week', `fortnight', `month', `year'."
  (org-agenda-check-type t 'agenda)
  (let* ((args (get-text-property (min (1- (point-max)) (point)) 'org-last-args))
	 (curspan (nth 2 args)))
    (when (and (not n) (equal curspan span))
      (error "Viewing span is already \"%s\"" span))
    (let* ((sd (or (org-get-at-bol 'day)
		   (nth 1 args)
		   org-starting-day))
	   (sd (org-agenda-compute-starting-span sd span n))
	   (org-agenda-overriding-cmd
	    (get-text-property (min (1- (point-max)) (point)) 'org-series-cmd))
	   (org-agenda-overriding-arguments
	    (list (car args) sd span)))
      (org-agenda-redo)
      (org-agenda-find-same-or-today-or-agenda))
    (org-agenda-set-mode-name)
    (message "Switched to %s view" span)))

(declare-function calendar-iso-to-absolute "cal-iso" (date))
(defun org-agenda-compute-starting-span (sd span &optional n)
  "Compute starting date for agenda.
SPAN may be `day', `week', `fortnight', `month', `year'.  The return value
is a cons cell with the starting date and the number of days,
so that the date SD will be in that range."
  (let* ((greg (calendar-gregorian-from-absolute sd))
	 ;; (dg (nth 1 greg))
	 (mg (car greg))
	 (yg (nth 2 greg)))
    (cond
     ((eq span 'day)
      (when n
	(setq sd (+ (calendar-absolute-from-gregorian
		     (list mg 1 yg))
		    n -1))))
     ((or (eq span 'week) (eq span 'fortnight))
      (let* ((nt (calendar-day-of-week
		  (calendar-gregorian-from-absolute sd)))
	     (d (if org-agenda-start-on-weekday
		    (- nt org-agenda-start-on-weekday)
		  0))
	     y1)
	(setq sd (- sd (+ (if (< d 0) 7 0) d)))
	(when n
	  (require 'cal-iso)
	  (when (> n 99)
	    (setq y1 (org-small-year-to-year (/ n 100))
		  n (mod n 100)))
	  (setq sd
		(calendar-iso-to-absolute
		 (list n 1
		       (or y1 (nth 2 (calendar-iso-from-absolute sd)))))))))
     ((eq span 'month)
      (let (y1)
	(when (and n (> n 99))
	  (setq y1 (org-small-year-to-year (/ n 100))
		n (mod n 100)))
	(setq sd (calendar-absolute-from-gregorian
		  (list (or n mg) 1 (or y1 yg))))))
     ((eq span 'year)
      (setq sd (calendar-absolute-from-gregorian
		(list 1 1 (or n yg))))))
    sd))

;;; Manipulate agenda query

;;;###autoload
(defun org-agenda-manipulate-query-add ()
  "Manipulate the query by adding a search term with positive selection.
Positive selection means the term must be matched for selection of an entry."
  (interactive)
  (org-agenda-manipulate-query ?\[))
;;;###autoload
(defun org-agenda-manipulate-query-subtract ()
  "Manipulate the query by adding a search term with negative selection.
Negative selection means term must not be matched for selection of an entry."
  (interactive)
  (org-agenda-manipulate-query ?\]))
;;;###autoload
(defun org-agenda-manipulate-query-add-re ()
  "Manipulate the query by adding a search regexp with positive selection.
Positive selection means the regexp must match for selection of an entry."
  (interactive)
  (org-agenda-manipulate-query ?\{))
;;;###autoload
(defun org-agenda-manipulate-query-subtract-re ()
  "Manipulate the query by adding a search regexp with negative selection.
Negative selection means regexp must not match for selection of an entry."
  (interactive)
  (org-agenda-manipulate-query ?\}))
(defun org-agenda-manipulate-query (char)
  (cond
   ((eq org-agenda-type 'agenda)
    (let ((org-agenda-include-inactive-timestamps t))
      (org-agenda-redo))
    (message "Display now includes inactive timestamps as well"))
   ((eq org-agenda-type 'search)
    ;; Previous calls to `org-agenda-manipulate-query' could already
    ;; add trailing text to the query.  Prevent duplicating it.
    ;; Trim the trailing spaces and +/.
    (setq org-agenda-query-string
          (replace-regexp-in-string
           (rx (or (1+ " ") (seq (1+ " ") (any "+-") (opt "{}"))) eos)
           ""
           org-agenda-query-string))
    (org-add-to-string
     'org-agenda-query-string
     (if org-agenda-last-search-view-search-was-boolean
	 (cdr (assoc char '((?\[ . " +") (?\] . " -")
			    (?\{ . " +{}") (?\} . " -{}"))))
       " "))
    (setq org-agenda-redo-command
	  (list 'org-search-view
		(car (get-text-property (min (1- (point-max)) (point))
					'org-last-args))
		org-agenda-query-string
		(+ (length org-agenda-query-string)
		   (if (member char '(?\{ ?\})) 0 1))))
    (set-register org-agenda-query-register org-agenda-query-string)
    (let ((org-agenda-overriding-arguments
	   (cdr org-agenda-redo-command)))
      (org-agenda-redo)))
   (t (error "Cannot manipulate query for %s-type agenda buffers"
	     org-agenda-type))))

(defun org-add-to-string (var string)
  (set var (concat (symbol-value var) string)))

(provide 'org-agenda-query)

;;; org-agenda-query.el ends here
