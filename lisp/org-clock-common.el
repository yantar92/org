;;; org-clock-common.el --- Current Org clock status -*- lexical-binding: t; -*-

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

;; This file contains variables defining current Org clock status.

;;; Code:

(require 'org-macs)
(org-assert-version)

;; Keep track of some markers for cut and paste.
(require 'org-track-markers)
(org-track-markers-register
 'org-clock-marker 'org-clock-hd-marker
 'org-clock-default-task 'org-clock-interrupted-task
 'org-clock-history)

(defvar org-clock-has-been-used nil
  "Has the clock been used during the current Emacs session?")

(defvar org-clock-marker (make-marker)
  "Marker recording the last clock-in.")

(defvar org-clock-hd-marker (make-marker)
  "Marker recording the last clock-in, but the headline position.")

(defvar org-clock-start-time nil
  "Start time of the currently running clock, as returned by `encode-time'.")

(defvar org-clock-effort ""
  "Effort estimate of the currently clocking task.")

(defvar org-clock-total-time nil
  "Holds total time, spent previously on currently clocked item.
This does not include the time in the currently running clock.")

(defvar org-clock-current-task nil
  "Title of the task currently clocked in.")

(defvar org-clock-heading ""
  "The heading of the current clock entry.
Unlike `org-clock-current-task', it is set according to
`org-clock-heading-function'.")

(defvar org-clock-out-time nil
  "Last clock out time, as returned by `encode-time'.")


(defvar org-clock-history nil
  "List of marker pointing to recent clocked tasks.")

(defvar org-clock-interrupted-task (make-marker)
  "Marker pointing to the task that has been interrupted by the current clock.")

(defvar org-clock-default-task (make-marker)
  "Marker pointing to the default task that should clock time.
The clock can be made to switch to this task after clocking out
of a different task.")

(defun org-clocking-p ()
  "Return t when clocking a task."
  (not (equal (org-clocking-buffer) nil)))

(defalias 'org-clock-is-active #'org-clocking-buffer)
(defun org-clocking-buffer ()
  "Return the buffer where the clock is currently running.
Return nil if no clock is running."
  (marker-buffer org-clock-marker))

(provide 'org-clock-common)

;;; org-clock-common.el ends here
