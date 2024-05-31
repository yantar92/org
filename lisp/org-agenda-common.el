;;; org-agenda-common.el --- Global agenda definitions         -*- lexical-binding: t; -*-

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
;; This library defines global agenda state and helper to manipulate
;; this state.

;;; Code:

(defvar org-agenda-buffer nil
  "Agenda buffer currently being generated.")
(defvar org-agenda-contributing-files nil)
(defvar org-agenda-sorting-strategy-selected nil)

(defsubst org-em (x y list)
  "Is X or Y a member of LIST?"
  (or (memq x list) (memq y list)))

(declare-function org-today "org-time" ())
(declare-function calendar-absolute-from-gregorian "calendar" (date))
(defun org-agenda-today-p (date)
  "Non-nil when DATE means today.
DATE is either a list of the form (month day year) or a number of
days as returned by `calendar-absolute-from-gregorian' or
`org-today'.  This function considers `org-extend-today-until'
when defining today."
  (require 'calendar)
  (require 'org-time)
  (eq (org-today)
      (if (consp date) (calendar-absolute-from-gregorian date) date)))

;;; Agenda markers

(defvar org-agenda-markers nil
  "List of all currently active markers created by `org-agenda'.")
(defvar org-agenda-last-marker-time (float-time)
  "Creation time of the last agenda marker.")

(defun org-agenda-new-marker (&optional pos)
  "Return a new agenda marker.
Marker is at point, or at POS if non-nil.  Org mode keeps a list
of these markers and resets them when they are no longer in use."
  (let ((m (copy-marker (or pos (point)) t)))
    (setq org-agenda-last-marker-time (float-time))
    (if (and org-agenda-buffer (buffer-live-p org-agenda-buffer))
        (with-current-buffer org-agenda-buffer
	  (push m org-agenda-markers))
      (push m org-agenda-markers))
    m))

(defun org-agenda-reset-markers ()
  "Reset markers created by `org-agenda'."
  (while org-agenda-markers
    (move-marker (pop org-agenda-markers) nil)))

;; Keep track of some markers for cut and paste.
(require 'org-track-markers)
(defun org-agenda-markers-for-cut-and-paste (_beg _end)
  "List markers to be tracked in BEG..END region.
To be used with `org-track-markers-register'."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'org-agenda-mode)
        org-agenda-markers))))
(org-track-markers-register #'org-agenda-markers-for-cut-and-paste)

;;; Customization affecting multiple aspects of agenda

(defcustom org-agenda-show-inherited-tags t
  "Non-nil means show inherited tags in each agenda line.

When this option is set to `always', it takes precedence over
`org-agenda-use-tag-inheritance' and inherited tags are shown
in every agenda.

When this option is set to t (the default), inherited tags are
shown when they are available, i.e. when the value of
`org-agenda-use-tag-inheritance' enables tag inheritance for the
given agenda type.

This can be set to a list of agenda types in which the agenda
must display the inherited tags.  Available types are `todo',
`agenda' and `search'.

When set to nil, never show inherited tags in agenda lines."
  :group 'org-agenda-line-format
  :group 'org-agenda
  :version "24.3"
  :type '(choice
	  (const :tag "Show inherited tags when available" t)
	  (const :tag "Always show inherited tags" always)
	  (repeat :tag "Show inherited tags only in selected agenda types"
		  (symbol :tag "Agenda type"))))

(defvar org-scanner-tags nil
  "The current tag list while the tags scanner is running.")

(defvar org-scanner-element nil
  "The current element while the tags scanner is running.")

(defvar org-trust-scanner-tags nil
  "Should `org-get-tags' use the tags for the scanner.
This is for internal dynamical scoping only.
When this is non-nil, the function `org-get-tags' will return the value
of `org-scanner-tags' instead of building the list by itself.  This
can lead to large speed-ups when the tags scanner is used in a file with
many entries, and when the list of tags is retrieved, for example to
obtain a list of properties.  Building the tags list for each entry in such
a file becomes an N^2 operation - but with this variable set, it scales
as N.")

(provide 'org-agenda-common)

;;; org-agenda-common.el ends here


