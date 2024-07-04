;;; org-clock-commands.el --- Misc user commands to work with Org clock -*- lexical-binding: t; -*-

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

;; This file implements additional user commands to query and act on
;; Org clocks.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-clock-core)

;;; Misc clock in commands

;;;###autoload
(defun org-clock-in-last (&optional arg)
  "Clock in the last closed clocked item.
When already clocking in, send a warning.
With a universal prefix argument, select the task you want to
clock in from the last clocked in tasks.
With two universal prefix arguments, start clocking using the
last clock-out time, if any.
With three universal prefix arguments, interactively prompt
for a todo state to switch to, overriding the existing value
`org-clock-in-switch-to-state'."
  (interactive "P")
  (if (equal arg '(4)) (org-clock-in arg)
    (let ((start-time (if (or org-clock-continuously (equal arg '(16)))
			  (or org-clock-out-time
			      (org-current-time org-clock-rounding-minutes t))
			(org-current-time org-clock-rounding-minutes t))))
      (if (null org-clock-history)
	  (message "No last clock")
	(let ((org-clock-in-switch-to-state
	       (if (and (not org-clock-current-task) (equal arg '(64)))
		   (completing-read "Switch to state: "
				    (and org-clock-history
                                         (org-element-all-todo-keywords
                                          (car org-clock-history))))
		 org-clock-in-switch-to-state))
	      (already-clocking org-clock-current-task))
	  (org-clock-clock-in (list (car org-clock-history)) nil start-time)
	  (or already-clocking
	      ;; Don't display a message if we are already clocking in
	      (message "Clocking back: %s (in %s)"
		       org-clock-current-task
		       (buffer-name (marker-buffer org-clock-marker)))))))))

;;; Goto a clock

(defcustom org-clock-goto-may-find-recent-task t
  "Non-nil means `org-clock-goto' can go to recent task if no active clock."
  :group 'org-clock
  :type 'boolean)

(defcustom org-clock-goto-before-context 2
  "Number of lines of context to display before currently clocked-in entry.
This applies when using `org-clock-goto'."
  :group 'org-clock
  :type 'integer)

(defvar org-clock-goto-hook nil
  "Hook run when selecting the currently clocked-in entry.")

;;;###autoload
(defun org-clock-goto (&optional select)
  "Go to the currently clocked-in entry, or to the most recently clocked one.
With prefix arg SELECT, offer recently clocked tasks for selection."
  (interactive "@P")
  (let* ((recent nil)
	 (m (cond
	     (select
	      (or (org-clock-select-task "Select task to go to: ")
		  (user-error "No task selected")))
	     ((org-clocking-p) org-clock-marker)
	     ((and org-clock-goto-may-find-recent-task
		   (car org-clock-history)
		   (marker-buffer (car org-clock-history)))
	      (setq recent t)
	      (car org-clock-history))
	     (t (user-error "No active or recent clock task")))))
    (pop-to-buffer-same-window (marker-buffer m))
    (if (or (< m (point-min)) (> m (point-max))) (widen))
    (goto-char m)
    (org-fold-show-entry)
    (org-back-to-heading t)
    (recenter org-clock-goto-before-context)
    (org-fold-reveal)
    (if recent
	(message "No running clock, this is the most recently clocked task"))
    (run-hooks 'org-clock-goto-hook)))

;;; Quickly change effort for currently running clock

(declare-function org-entry-put "org-property-set" (epom property value))
;;;###autoload
(defun org-clock-modify-effort-estimate (&optional value)
  "Add to or set the effort estimate of the item currently being clocked.
VALUE can be a number of minutes, or a string with format hh:mm or mm.
When the string starts with a + or a - sign, the current value of the effort
property will be changed by that amount.  If the effort value is expressed
as an unit defined in `org-duration-units' (e.g. \"3h\"), the modified
value will be converted to a hh:mm duration.

This command will update the \"Effort\" property of the currently
clocked item, and the value displayed in the mode line."
  (interactive)
  (if (org-clock-is-active)
      (let ((current org-clock-effort) sign)
	(unless value
	  ;; Prompt user for a value or a change
	  (setq value
		(read-string
		 (format "Set effort (hh:mm or mm%s): "
			 (if current
			     (format ", prefix + to add to %s" org-clock-effort)
			   "")))))
	(when (stringp value)
	  ;; A string.  See if it is a delta
	  (setq sign (string-to-char value))
	  (if (member sign '(?- ?+))
	      (setq current (org-duration-to-minutes current)
		    value (substring value 1))
	    (setq current 0))
	  (setq value (org-duration-to-minutes value))
	  (if (equal ?- sign)
	      (setq value (- current value))
	    (if (equal ?+ sign) (setq value (+ current value)))))
	(setq value (max 0 value)
	      org-clock-effort (org-duration-from-minutes value))
        (require 'org-property-set)
	(org-entry-put org-clock-marker "Effort" org-clock-effort)
	(org-clock-update-clock-status)
	(message "Effort is now %s" org-clock-effort))
    (message "Clock is not currently active")))

;;; Update running clock when its clock line at point is modified by hand

(declare-function org-timestamp-change "org-timestamp" (n &optional what updown suppress-tmp-delay))
;;;###autoload
(defun org-clock-update-time-maybe ()
  "If this is a CLOCK line, update it and return t.
Otherwise, return nil."
  (interactive)
  (let ((origin (point))) ;; `save-excursion' may not work when deleting.
    (prog1
        (save-excursion
          (forward-line 0)
          (skip-chars-forward " \t")
          (when (looking-at org-clock-string)
            (let ((re (concat "[ \t]*" org-clock-string
		              " *[[<]\\([^]>]+\\)[]>]\\(-+[[<]\\([^]>]+\\)[]>]"
		              "\\([ \t]*=>.*\\)?\\)?"))
	          ts te h m s neg)
              (cond
	       ((not (looking-at re))
	        nil)
	       ((not (match-end 2))
	        (when (and (equal (marker-buffer org-clock-marker) (current-buffer))
		           (> org-clock-marker (point))
                           (<= org-clock-marker (line-end-position)))
	          ;; The clock is running here
	          (setq org-clock-start-time
		        (org-time-string-to-time (match-string 1)))
	          (org-clock-update-clock-status)))
	       (t
                ;; Prevent recursive call from `org-timestamp-change'.
                (cl-letf (((symbol-function 'org-clock-update-time-maybe) #'ignore))
                  ;; Update timestamps.
                  (save-excursion
                    (goto-char (match-beginning 1)) ; opening timestamp
                    (save-match-data (org-timestamp-change 0 'day)))
                  ;; Refresh match data.
                  (looking-at re)
                  (save-excursion
                    (goto-char (match-beginning 3)) ; closing timestamp
                    (save-match-data (org-timestamp-change 0 'day))))
                ;; Refresh match data.
                (looking-at re)
                (and (match-end 4) (delete-region (match-beginning 4) (match-end 4)))
                (end-of-line 1)
                (setq ts (match-string 1)
                      te (match-string 3))
                (setq s (- (org-time-string-to-seconds te)
		           (org-time-string-to-seconds ts))
                      neg (< s 0)
                      s (abs s)
                      h (floor (/ s 3600))
                      s (- s (* 3600 h))
                      m (floor (/ s 60))
                      s (- s (* 60 s)))
	        (insert " => " (format (if neg "-%d:%02d" "%2d:%02d") h m))
	        t)))))
      ;; Move back to initial position, but never beyond updated
      ;; clock.
      (unless (< (point) origin)
        (goto-char origin)))))

;;; Display clock sums in overlays

(defcustom org-clock-display-default-range 'thisyear
  "Default range when displaying clocks with `org-clock-display'.
Valid values are: `today', `yesterday', `thisweek', `lastweek',
`thismonth', `lastmonth', `thisyear', `lastyear' and `untilnow'."
  :group 'org-clock
  :type '(choice (const today)
		 (const yesterday)
		 (const thisweek)
		 (const lastweek)
		 (const thismonth)
		 (const lastmonth)
		 (const thisyear)
		 (const lastyear)
		 (const untilnow)
		 (const :tag "Select range interactively" interactive))
  :safe #'symbolp)

;;;###autoload
(defun org-clock-display (&optional arg)
  "Show subtree times in the entire buffer.

By default, show the total time for the range defined in
`org-clock-display-default-range'.  With `\\[universal-argument]' \
prefix, show
the total time for today instead.

With `\\[universal-argument] \\[universal-argument]' prefix, \
use a custom range, entered at prompt.

With `\\[universal-argument] \ \\[universal-argument] \
\\[universal-argument]' prefix, display the total time in the
echo area.

Use `\\[org-clock-remove-overlays]' to remove the subtree times."
  (interactive "P")
  (org-clock-remove-overlays)
  (let* ((todayp (equal arg '(4)))
	 (customp (member arg '((16) today yesterday
				thisweek lastweek thismonth
				lastmonth thisyear lastyear
				untilnow interactive)))
	 (prop (cond ((not arg) :org-clock-minutes-default)
		     (todayp :org-clock-minutes-today)
		     (customp :org-clock-minutes-custom)
		     (t :org-clock-minutes))))
    (cond ((not arg) (org-clock-sum-custom
		    nil org-clock-display-default-range prop))
	  (todayp (org-clock-sum-today))
	  (customp (org-clock-sum-custom nil arg))
	  (t (org-clock-sum)))
    (unless (equal arg '(64))
      (save-excursion
	(goto-char (point-min))
	(let ((p nil))
	  (while (or (and (equal (setq p (point)) (point-min))
			  (get-text-property p prop))
		     (setq p (next-single-property-change (point) prop)))
	    (goto-char p)
	    (let ((time (get-text-property p prop)))
	      (when time (org-clock-put-overlay time)))))
	;; Arrange to remove the overlays upon next change.
	(when org-remove-highlights-with-change
	  (add-hook 'before-change-functions #'org-clock-remove-overlays
		    nil 'local))))
    (let* ((h (/ org-clock-file-total-minutes 60))
	   (m (- org-clock-file-total-minutes (* 60 h))))
      (message (cond
		(todayp
		 "Total file time for today: %s (%d hours and %d minutes)")
		(customp
		 "Total file time (custom): %s (%d hours and %d minutes)")
		(t
		 "Total file time: %s (%d hours and %d minutes)"))
	       (org-duration-from-minutes org-clock-file-total-minutes)
	       h m))))

(defvar-local org-clock-overlays nil)
(put 'org-clock-overlays 'permanent-local t)

(defun org-clock-put-overlay (time)
  "Put an overlay on the headline at point, displaying TIME.
Create a new overlay and store it in `org-clock-overlays', so
that it will be easy to remove.  This function assumes point is
on a headline."
  (org-match-line (org-complex-heading-regexp))
  (goto-char (match-beginning 4))
  (let* ((headline (match-string 4))
	 (text (concat headline
		       (org-add-props
			   (make-string
			    (max (- (- 60 (current-column))
				    (org-string-width headline)
				    (length (org-get-at-bol 'line-prefix)))
				 0)
			    ?\·)
			   '(face shadow))
		       (org-add-props
			   (format " %9s " (org-duration-from-minutes time))
			   '(face org-clock-overlay))))
	 (o (make-overlay (point) (line-end-position))))
    (org-overlay-display o text)
    (push o org-clock-overlays)))

;;;###autoload
(defun org-clock-remove-overlays (&optional _beg _end noremove)
  "Remove the occur highlights from the buffer.
If NOREMOVE is nil, remove this function from the
`before-change-functions' in the current buffer."
  (interactive)
  (unless org-inhibit-highlight-removal
    (mapc #'delete-overlay org-clock-overlays)
    (setq org-clock-overlays nil)
    (unless noremove
      (remove-hook 'before-change-functions
		   #'org-clock-remove-overlays 'local))))

;;; Working with timestamps inside CLOCK: line

;;;###autoload
(defun org-clock-timestamps-up (&optional n)
  "Increase CLOCK timestamps at cursor.
Optional argument N tells to change by that many units."
  (interactive "P")
  (org-clock-timestamps-change 'up n))

;;;###autoload
(defun org-clock-timestamps-down (&optional n)
  "Decrease CLOCK timestamps at cursor.
Optional argument N tells to change by that many units."
  (interactive "P")
  (org-clock-timestamps-change 'down n))

(defun org-clock-timestamps-change (updown &optional n)
  "Change CLOCK timestamps synchronously at cursor.
UPDOWN tells whether to change `up' or `down'.
Optional argument N tells to change by that many units."
  (let ((tschange (if (eq updown 'up) 'org-timestamp-up
		    'org-timestamp-down))
	(timestamp? (org-at-timestamp-p 'lax))
	ts1 begts1 ts2 begts2 updatets1 tdiff)
    (when timestamp?
      (save-excursion
	(move-beginning-of-line 1)
	(re-search-forward org-ts-regexp3 nil t)
	(setq ts1 (match-string 0) begts1 (match-beginning 0))
	(when (re-search-forward org-ts-regexp3 nil t)
	  (setq ts2 (match-string 0) begts2 (match-beginning 0))))
      ;; Are we on the second timestamp?
      (if (<= begts2 (point)) (setq updatets1 t))
      (if (not ts2)
	  ;; fall back on org-timestamp-up if there is only one
	  (funcall tschange n)
	(funcall tschange n)
	(let ((ts (if updatets1 ts2 ts1))
	      (begts (if updatets1 begts1 begts2)))
	  (setq tdiff
		(time-subtract
		 (org-time-string-to-time
                  (save-excursion
                    (goto-char (if updatets1 begts2 begts1))
                    (looking-at org-ts-regexp3)
                    (match-string 0)))
		 (org-time-string-to-time ts)))
          ;; `save-excursion' won't work because
          ;; `org-timestamp-change' deletes and re-inserts the
          ;; timestamp.
	  (let ((origin (point)))
            (save-excursion
	      (goto-char begts)
	      (org-timestamp-change
	       (round (/ (float-time tdiff)
		         (pcase timestamp?
			   (`minute 60)
			   (`hour 3600)
			   (`day (* 24 3600))
			   (`month (* 24 3600 31))
			   (`year (* 24 3600 365.2)))))
	       timestamp? 'updown))
            ;; Move back to initial position, but never beyond updated
            ;; clock.
            (unless (< (point) origin)
              (goto-char origin))))))))

(provide 'org-clock-commands)

;;; org-clock-commands.el ends here
