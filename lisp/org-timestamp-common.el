;;; org-timestamp-common.el --- Common timestamp customizations         -*- lexical-binding: t; -*-

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
;; This library implements common timestamp-related customizations for
;; Org mode.

;;; Code:

(defconst org-ts-regexp0
  "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\( +[^]+0-9>\r\n -]+\\)?\\( +\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)"
  "Regular expression matching time strings for analysis.
This one does not require the space after the date, so it can be used
on a string that terminates immediately after the date.")

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

(provide 'org-timestamp-common)

;;; org-timestamp-common.el ends here


