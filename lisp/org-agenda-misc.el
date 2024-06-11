;;; org-agenda-misc.el --- Additional Org agenda commands  -*- lexical-binding: t; -*-

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

;; This file implements extra user commands for `org-agenda-mode'.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-agenda-mode)
(require 'calendar)
(require 'org-time)

;;;###autoload
(defun org-agenda-show-tags ()
  "Show the tags applicable to the current item."
  (interactive)
  (let* ((tags (org-get-at-bol 'tags)))
    (if tags
	(message "Tags are :%s:"
		 (org-no-properties (mapconcat #'identity tags ":")))
      (message "No tags associated with this line"))))

;;;###autoload
(defun org-agenda-execute (arg)
  "Execute another agenda command, keeping same window.
So this is just a shortcut for \\<global-map>`\\[org-agenda]', available
in the agenda."
  (interactive "P")
  (let ((org-agenda-window-setup 'current-window))
    (org-agenda arg)))

(defun org-agenda-show-1 (&optional more)
  "Display the Org file which contains the item at point.
The prefix arg selects the amount of information to display:

0   hide the subtree
1   just show the entry according to defaults.
2   show the children view
3   show the subtree view
4   show the entire subtree and any drawers
With prefix argument FULL-ENTRY, make the entire entry visible
if it was hidden in the outline."
  (interactive "p")
  (let ((win (selected-window)))
    (org-agenda-goto t)
    (org-back-to-heading)
    (set-window-start (selected-window) (line-beginning-position))
    (cond
     ((= more 0)
      (org-fold-subtree t)
      (save-excursion
	(org-back-to-heading)
	(run-hook-with-args 'org-cycle-hook 'folded))
      (message "Remote: FOLDED"))
     ((and (called-interactively-p 'any) (= more 1))
      (message "Remote: show with default settings"))
     ((= more 2)
      (org-fold-show-entry 'hide-drawers)
      (org-fold-show-children)
      (save-excursion
	(org-back-to-heading)
	(run-hook-with-args 'org-cycle-hook 'children))
      (message "Remote: CHILDREN"))
     ((= more 3)
      (org-fold-show-subtree)
      (save-excursion
	(org-back-to-heading)
	(run-hook-with-args 'org-cycle-hook 'subtree))
      (message "Remote: SUBTREE"))
     ((> more 3)
      (org-fold-show-subtree)
      (message "Remote: SUBTREE AND ALL DRAWERS")))
    (select-window win)))

(defvar org-agenda-cycle-counter nil)
;;;###autoload
(defun org-agenda-cycle-show (&optional n)
  "Show the current entry in another window, with default settings.

Default settings are taken from `org-show-context-detail'.  When
use repeatedly in immediate succession, the remote entry will
cycle through visibility

  children -> subtree -> folded

When called with a numeric prefix arg, that arg will be passed through to
`org-agenda-show-1'.  For the interpretation of that argument, see the
docstring of `org-agenda-show-1'."
  (interactive "P")
  (if (integerp n)
      (setq org-agenda-cycle-counter n)
    (if (not (eq last-command this-command))
	(setq org-agenda-cycle-counter 1)
      (if (equal org-agenda-cycle-counter 0)
	  (setq org-agenda-cycle-counter 2)
	(setq org-agenda-cycle-counter (1+ org-agenda-cycle-counter))
	(when (> org-agenda-cycle-counter 3)
	  (setq org-agenda-cycle-counter 0)))))
  (org-agenda-show-1 org-agenda-cycle-counter))

(defun org-agenda-execute-calendar-command (cmd)
  "Execute a calendar command from the agenda with date from cursor."
  (org-agenda-check-type t 'agenda)
  (require 'diary-lib)
  (unless (get-text-property (min (1- (point-max)) (point)) 'day)
    (user-error "Don't know which date to use for the calendar command"))
  (let* ((oldf (symbol-function 'calendar-cursor-to-date))
	 (point (point))
	 (date (calendar-gregorian-from-absolute
		(get-text-property point 'day))))
    ;; the following 2 vars are needed in the calendar
    (org-dlet
	((displayed-month (car date))
	 (displayed-year (nth 2 date)))
      (unwind-protect
	  (progn
	    (fset 'calendar-cursor-to-date
		  (lambda (&optional _error _dummy)
		    (calendar-gregorian-from-absolute
		     (get-text-property point 'day))))
	    (call-interactively cmd))
	(fset 'calendar-cursor-to-date oldf)))))

;;;###autoload
(defun org-agenda-phases-of-moon ()
  "Display the phases of the moon for the 3 months around the cursor date."
  (interactive)
  (org-agenda-execute-calendar-command 'calendar-lunar-phases))

;;;###autoload
(defun org-agenda-holidays ()
  "Display the holidays for the 3 months around the cursor date."
  (interactive)
  (org-agenda-execute-calendar-command 'calendar-list-holidays))

(defvar calendar-longitude)      ; defined in calendar.el
(defvar calendar-latitude)       ; defined in calendar.el
(defvar calendar-location-name)  ; defined in calendar.el

;;;###autoload
(defun org-agenda-sunrise-sunset (arg)
  "Display sunrise and sunset for the cursor date.
Latitude and longitude can be specified with the variables
`calendar-latitude' and `calendar-longitude'.  When called with prefix
argument, latitude and longitude will be prompted for."
  (interactive "P")
  (require 'solar)
  (let ((calendar-longitude (if arg nil calendar-longitude))
	(calendar-latitude  (if arg nil calendar-latitude))
	(calendar-location-name
	 (if arg "the given coordinates" calendar-location-name)))
    (org-agenda-execute-calendar-command 'calendar-sunrise-sunset)))

;;;###autoload
(defun org-agenda-goto-calendar ()
  "Open the Emacs calendar with the date at the cursor."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (let* ((day (or (get-text-property (min (1- (point-max)) (point)) 'day)
		  (user-error "Don't know which date to open in calendar")))
	 (date (calendar-gregorian-from-absolute day))
	 (calendar-move-hook nil)
	 (calendar-view-holidays-initially-flag nil)
	 (calendar-view-diary-initially-flag nil))
    (calendar)
    (calendar-goto-date date)))

;;;###autoload
(defun org-agenda-convert-date ()
  (interactive)
  (org-agenda-check-type t 'agenda)
  (let ((day (get-text-property (min (1- (point-max)) (point)) 'day))
	date s)
    (unless day
      (user-error "Don't know which date to convert"))
    (setq date (calendar-gregorian-from-absolute day))
    (setq s (concat
	     "Gregorian:  " (calendar-date-string date) "\n"
	     "ISO:        " (calendar-iso-date-string date) "\n"
	     "Day of Yr:  " (calendar-day-of-year-string date) "\n"
	     "Julian:     " (calendar-julian-date-string date) "\n"
	     "Astron. JD: " (calendar-astro-date-string date)
	     " (Julian date number at noon UTC)\n"
	     "Hebrew:     " (calendar-hebrew-date-string date) " (until sunset)\n"
	     "Islamic:    " (calendar-islamic-date-string date) " (until sunset)\n"
	     "French:     " (calendar-french-date-string date) "\n"
	     "Bahá’í:     " (calendar-bahai-date-string date) " (until sunset)\n"
	     "Mayan:      " (calendar-mayan-date-string date) "\n"
	     "Coptic:     " (calendar-coptic-date-string date) "\n"
	     "Ethiopic:   " (calendar-ethiopic-date-string date) "\n"
	     "Persian:    " (calendar-persian-date-string date) "\n"
	     "Chinese:    " (calendar-chinese-date-string date) "\n"))
    (with-output-to-temp-buffer "*Dates*"
      (princ s))
    (org-fit-window-to-buffer (get-buffer-window "*Dates*"))))

;;; Flagging notes

(declare-function org-entry-get "org-property" (epom property &optional inherit literal-nil))
;;;###autoload
(defun org-agenda-show-the-flagging-note ()
  "Display the flagging note in the other window.
When called a second time in direct sequence, offer to remove the FLAGGING
tag and (if present) the flagging note."
  (interactive)
  (let ((hdmarker (org-get-at-bol 'org-hd-marker))
	(win (selected-window))
	note) ;; heading newhead
    (unless hdmarker
      (user-error "No linked entry at point"))
    (if (and (eq this-command last-command)
	     (y-or-n-p "Unflag and remove any flagging note? "))
	(progn
	  (org-agenda-remove-flag hdmarker)
	  (let ((win (get-buffer-window "*Flagging Note*")))
	    (and win (delete-window win)))
	  (message "Entry unflagged"))
      (setq note (org-entry-get hdmarker "THEFLAGGINGNOTE"))
      (unless note
	(user-error "No flagging note"))
      (org-kill-new note)
      (switch-to-buffer-other-window "*Flagging Note*")
      (erase-buffer)
      (insert note)
      (goto-char (point-min))
      (while (re-search-forward "\\\\n" nil t)
	(replace-match "\n" t t))
      (goto-char (point-min))
      (select-window win)
      (message "%s" (substitute-command-keys "Flagging note pushed to \
kill ring.  Press `\\[org-agenda-show-the-flagging-note]' again to remove \
tag and note")))))

(declare-function org-entry-delete "org-property-set" (epom property))
(declare-function org-agenda-change-all-lines "org-agenda-line-format"
                  (newhead hdmarker &optional fixface just-this))
(declare-function org-toggle-tag "org-tags" (tag &optional onoff))
(defun org-agenda-remove-flag (marker)
  "Remove the FLAGGED tag and any flagging note in the entry."
  (let ((newhead
         (org-with-point-at marker
           (org-toggle-tag "FLAGGED" 'off)
           (require 'org-property-set)
           (org-entry-delete nil "THEFLAGGINGNOTE")
           (org-get-heading))))
    (require 'org-agenda-line-format)
    (org-agenda-change-all-lines newhead marker)
    (message "Entry unflagged")))

;;; Appointment reminders

(defvar appt-time-msg-list) ; defined in appt.el

(declare-function org-agenda-get-day-entries "org-agenda-search" (file date &rest args))
;;;###autoload
(defun org-agenda-to-appt (&optional refresh filter &rest args)
  "Activate appointments found in `org-agenda-files'.

With a `\\[universal-argument]' prefix, refresh the list of \
appointments.

If FILTER is t, interactively prompt the user for a regular
expression, and filter out entries that don't match it.

If FILTER is a string, use this string as a regular expression
for filtering entries out.

If FILTER is a function, filter out entries against which
calling the function returns nil.  This function takes one
argument: an entry from `org-agenda-get-day-entries'.

FILTER can also be an alist with the car of each cell being
either `headline' or `category'.  For example:

   ((headline \"IMPORTANT\")
    (category \"Work\"))

will only add headlines containing IMPORTANT or headlines
belonging to the \"Work\" category.

ARGS are symbols indicating what kind of entries to consider.
By default `org-agenda-to-appt' will use :deadline*, :scheduled*
\(i.e., deadlines and scheduled items with a hh:mm specification)
and :timestamp entries.  See the docstring of `org-diary' for
details and examples.

If an entry has a APPT_WARNTIME property, its value will be used
to override `appt-message-warning-time'."
  (interactive "P")
  (when refresh (setq appt-time-msg-list nil))
  (when (eq filter t)
    (setq filter (read-from-minibuffer "Regexp filter: ")))
  (require 'org-agenda-search)
  (defvar org-deadline-warning-days)
  (let* ((cnt 0)                        ; count added events
         (scope (or args '(:deadline* :scheduled* :timestamp)))
         (org-agenda-new-buffers nil)
         (org-deadline-warning-days 0)
         ;; Do not use `org-today' here because appt only takes
         ;; time and without date as argument, so it may pass wrong
         ;; information otherwise
         (today (org-date-to-gregorian
                 (time-to-days nil)))
         (org-agenda-restrict nil)
         (files (org-agenda-files 'unrestricted)) entries file
         (org-agenda-buffer nil))
    ;; Get all entries which may contain an appt
    (org-agenda-prepare-buffers files)
    (while (setq file (pop files))
      (setq entries
            (delq nil
                  (append entries
                          (apply #'org-agenda-get-day-entries
                                 file today scope)))))
    ;; Map through entries and find if we should filter them out
    (mapc
     (lambda (x)
       (let* ((evt (org-trim
                    (replace-regexp-in-string
                     org-link-bracket-re "\\2"
                     (or (get-text-property 1 'txt x) ""))))
              (cat (get-text-property (1- (length x)) 'org-category x))
              (tod (get-text-property 1 'time-of-day x))
              (ok (or (null filter)
                      (and (stringp filter) (string-match filter evt))
                      (and (functionp filter) (funcall filter x))
                      (and (listp filter)
                           (let ((cat-filter (cadr (assq 'category filter)))
                                 (evt-filter (cadr (assq 'headline filter))))
                             (or (and (stringp cat-filter)
                                      (string-match cat-filter cat))
                                 (and (stringp evt-filter)
                                      (string-match evt-filter evt)))))))
              (wrn (get-text-property 1 'warntime x))
              (todo-regexp (get-text-property 1 'org-todo-regexp x))
              (not-done-regexp (get-text-property 1 'org-not-done-regexp x)))
         ;; FIXME: Shall we remove text-properties for the appt text?
         ;; (setq evt (set-text-properties 0 (length evt) nil evt))
         (when (and ok tod
                    ;; Exclude done items unconditionally.
                    (or (not (and todo-regexp (string-match-p todo-regexp evt))) ; no todo keyword
                        (and not-done-regexp (string-match-p not-done-regexp evt)) ; or not done
                        ))
           (setq tod (concat "00" (number-to-string tod)))
           (setq tod (when (string-match
                            "\\([0-9]\\{1,2\\}\\)\\([0-9]\\{2\\}\\)\\'" tod)
                       (concat (match-string 1 tod) ":"
                               (match-string 2 tod))))
           (when (appt-add tod evt wrn)
             (setq cnt (1+ cnt))))))
     entries)
    (org-release-buffers org-agenda-new-buffers)
    (if (eq cnt 0)
        (message "No event to add")
      (message "Added %d event%s for today" cnt (if (> cnt 1) "s" "")))))

(provide 'org-agenda-misc)

;;; org-agenda-misc.el ends here
