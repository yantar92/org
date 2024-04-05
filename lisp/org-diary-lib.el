;;; org-diary-lib.el --- Org mode diary integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

;;; This library Org mode extras to work with diaries.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'diary-lib)

(declare-function calendar-check-holidays "holidays" (date))
(defun org-calendar-holiday ()
  "List of holidays, for Diary display in Org mode."
  (require 'holidays)
  (with-no-warnings (defvar date))
  (let ((hl (calendar-check-holidays date)))
    (and hl (mapconcat #'identity hl "; "))))

;; Calendar sanity: define some functions that are independent of
;; `calendar-date-style'.
(defun org-anniversary (year month day &optional mark)
  "Like `diary-anniversary', but with fixed (ISO) order of arguments."
  (with-no-warnings
    (let ((calendar-date-style 'iso))
      (diary-anniversary year month day mark))))

(defun org-cyclic (N year month day &optional mark)
  "Like `diary-cyclic', but with fixed (ISO) order of arguments."
  (with-no-warnings
    (let ((calendar-date-style 'iso))
      (diary-cyclic N year month day mark))))

(defun org-block (Y1 M1 D1 Y2 M2 D2 &optional mark)
  "Like `diary-block', but with fixed (ISO) order of arguments."
  (with-no-warnings
    (let ((calendar-date-style 'iso))
      (diary-block Y1 M1 D1 Y2 M2 D2 mark))))

(defun org-date (year month day &optional mark)
  "Like `diary-date', but with fixed (ISO) order of arguments."
  (with-no-warnings
    (let ((calendar-date-style 'iso))
      (diary-date year month day mark))))

;; Define the `org-class' function
(defun org-class (y1 m1 d1 y2 m2 d2 dayname &rest skip-weeks)
  "Entry applies if date is between dates on DAYNAME, but skips SKIP-WEEKS.
DAYNAME is a number between 0 (Sunday) and 6 (Saturday).
SKIP-WEEKS is any number of ISO weeks in the block period for which the
item should be skipped.  If any of the SKIP-WEEKS arguments is the symbol
`holidays', then any date that is known by the Emacs calendar to be a
holiday will also be skipped.  If SKIP-WEEKS arguments are holiday strings,
then those holidays will be skipped."
  (with-no-warnings (defvar date) (defvar entry))
  (let* ((date1 (calendar-absolute-from-gregorian (list m1 d1 y1)))
	 (date2 (calendar-absolute-from-gregorian (list m2 d2 y2)))
	 (d (calendar-absolute-from-gregorian date))
	 (h (when skip-weeks (calendar-check-holidays date))))
    (and
     (<= date1 d)
     (<= d date2)
     (= (calendar-day-of-week date) dayname)
     (or (not skip-weeks)
	 (progn
	   (require 'cal-iso)
	   (not (member (car (calendar-iso-from-absolute d)) skip-weeks))))
     (not (or (and h (memq 'holidays skip-weeks))
	    (delq nil (mapcar (lambda(g) (member g skip-weeks)) h))))
     entry)))

(provide 'org-diary-lib)
;;; org-diary-lib.el ends here
