;;; org-agenda-bulk-edit.el --- Acting on multiple agenda entries  -*- lexical-binding: t; -*-

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

;; This file implements bulk editing of agenda entries.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-agenda-mode)
(require 'org-mode-common)
(require 'org-time)

(defcustom org-agenda-persistent-marks nil
  "Non-nil means marked items will stay marked after a bulk action.
You can toggle this interactively by typing `p' when prompted for a
bulk action."
  :group 'org-agenda
  :version "24.1"
  :type 'boolean)

(defcustom org-agenda-bulk-mark-char ">"
  "A single-character string to be used as the bulk mark."
  :group 'org-agenda
  :version "24.1"
  :type 'string)

(defcustom org-agenda-bulk-custom-functions nil
  "Alist of characters and custom functions for bulk actions.
For example, this makes those two functions available:

  (setq org-agenda-bulk-custom-functions
        \\='((?R set-category)
          (?C bulk-cut)))

With selected entries in an agenda buffer, `B R' will call
the custom function `set-category' on the selected entries.
Note that functions in this alist don't need to be quoted.

You can also specify a function which collects arguments to be
used for each call to your bulk custom function.  The argument
collecting function will be run once and should return a list of
arguments to pass to the bulk function.  For example:

  (setq org-agenda-bulk-custom-functions
        \\='((?R set-category get-category)))

Now, `B R' will call the custom `get-category' which would prompt
the user once for a category.  That category is then passed as an
argument to `set-category' for each entry it's called against."
  :type
  '(alist :key-type character
	  :value-type
          (group (function :tag "Bulk Custom Function")
		 (choice (function :tag "Bulk Custom Argument Function")
		         (const :tag "No Bulk Custom Argument Function" nil))))
  :package-version '(Org . "9.5")
  :group 'org-agenda)

(defun org-agenda-bulk-marked-p ()
  "Non-nil when current entry is marked for bulk action."
  (eq (get-char-property (line-beginning-position) 'type)
      'org-marked-entry-overlay))

;;;###autoload
(defun org-agenda-bulk-mark (&optional arg)
  "Mark entries for future bulk action.

When ARG is nil or one and region is not active then mark the
entry at point.

When ARG is nil or one and region is active then mark the entries
in the region.

When ARG is greater than one mark ARG lines."
  (interactive "p")
  (when (and (or (not arg) (= arg 1)) (use-region-p))
    (setq arg (count-lines (region-beginning) (region-end)))
    (goto-char (region-beginning))
    (deactivate-mark))
  (dotimes (_ (or arg 1))
    (unless (org-get-at-bol 'org-agenda-diary-link)
      (let* ((m (org-get-at-bol 'org-hd-marker))
	     ov)
	(unless (org-agenda-bulk-marked-p)
	  (unless m (user-error "Nothing to mark at point"))
	  (push m org-agenda-bulk-marked-entries)
          (setq ov (make-overlay (line-beginning-position)
                                 (+ 2 (line-beginning-position))))
          ;; Display using 'before-string to make the overlay
          ;; compatible with column view in agenda that uses an
          ;; overlay with higher priority.
          (overlay-put ov 'before-string
                       (propertize org-agenda-bulk-mark-char
                                   'face (org-get-todo-face "TODO")))
          ;; We cannot completely hide the overlay to make point
          ;; adjustment not move point out of overlay (to previous
          ;; line) when moving lines with n/p.
	  (org-overlay-display ov " " nil 'evaporate)
	  (overlay-put ov 'type 'org-marked-entry-overlay))
	(end-of-line 1)
	(or (ignore-errors
	      (goto-char (next-single-property-change (point) 'org-hd-marker)))
	    (forward-line 1))
	(while (and (get-char-property (point) 'invisible) (not (eobp)))
	  (forward-line 1)))))
  (message "%d entries marked for bulk action"
	   (length org-agenda-bulk-marked-entries)))

;;;###autoload
(defun org-agenda-bulk-mark-all ()
  "Mark all entries for future agenda bulk action."
  (interactive)
  (org-agenda-bulk-mark-regexp "."))

;;;###autoload
(defun org-agenda-bulk-mark-regexp (regexp)
  "Mark entries matching REGEXP for future agenda bulk action."
  (interactive "sMark entries matching regexp: ")
  (let ((entries-marked 0) txt-at-point)
    (save-excursion
      (goto-char (point-min))
      (goto-char (next-single-property-change (point) 'org-hd-marker))
      (while (and (re-search-forward regexp nil t)
		  (setq txt-at-point
			(get-text-property (match-beginning 0) 'txt)))
	(if (get-char-property (point) 'invisible)
	    (forward-line 1)
	  (when (string-match-p regexp txt-at-point)
	    (setq entries-marked (1+ entries-marked))
	    (call-interactively #'org-agenda-bulk-mark)))))
    (unless entries-marked
      (message "No entry matching this regexp."))))

;;;###autoload
(defun org-agenda-bulk-unmark (&optional arg)
  "Unmark the entry at point for future bulk action."
  (interactive "P")
  (if arg
      (org-agenda-bulk-unmark-all)
    (cond ((org-agenda-bulk-marked-p)
	   (org-agenda-bulk-remove-overlays
            (line-beginning-position) (+ 2 (line-beginning-position)))
	   (setq org-agenda-bulk-marked-entries
		 (delete (org-get-at-bol 'org-hd-marker)
			 org-agenda-bulk-marked-entries))
	   (end-of-line 1)
	   (or (ignore-errors
		 (goto-char (next-single-property-change (point) 'txt)))
	       (forward-line 1))
	   (while (and (get-char-property (point) 'invisible) (not (eobp)))
	     (forward-line 1))
	   (message "%d entries left marked for bulk action"
		    (length org-agenda-bulk-marked-entries)))
	  (t (message "No entry to unmark here")))))

;;;###autoload
(defun org-agenda-bulk-toggle-all ()
  "Toggle all marks for bulk action."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (ignore-errors
	     (goto-char (next-single-property-change (point) 'org-hd-marker)))
      (org-agenda-bulk-toggle))))

;;;###autoload
(defun org-agenda-bulk-toggle ()
  "Toggle the mark at point for bulk action."
  (interactive)
  (if (org-agenda-bulk-marked-p)
      (org-agenda-bulk-unmark)
    (org-agenda-bulk-mark)))

(defun org-agenda-bulk-remove-overlays (&optional beg end)
  "Remove the mark overlays between BEG and END in the agenda buffer.
BEG and END default to the buffer limits.

This only removes the overlays, it does not remove the markers
from the list in `org-agenda-bulk-marked-entries'."
  (interactive)
  (mapc (lambda (ov)
	  (and (eq (overlay-get ov 'type) 'org-marked-entry-overlay)
	       (delete-overlay ov)))
	(overlays-in (or beg (point-min)) (or end (point-max)))))

;;;###autoload
(defun org-agenda-bulk-unmark-all ()
  "Remove all marks in the agenda buffer.
This will remove the markers and the overlays."
  (interactive)
  (if (null org-agenda-bulk-marked-entries)
      (message "No entry to unmark")
    (setq org-agenda-bulk-marked-entries nil)
    (org-agenda-bulk-remove-overlays (point-min) (point-max))))

(declare-function org-agenda-todo "org-agenda-commands-proxy" (&optional arg))
(declare-function org-read-date "org-read-date"
                  (&optional with-time to-time from-string prompt
			     default-time default-input inactive))
(declare-function org-add-log-note "org-log-note" (&optional _purpose))
(declare-function org-agenda-deadline "org-agenda-commands-proxy" org-agenda-deadline (arg &optional time))
(declare-function org-agenda-schedule "org-agenda-commands-proxy" org-agenda-deadline (arg &optional time))
(declare-function org-agenda-set-tags "org-agenda-commands-proxy" org-agenda-deadline (&optional tag onoff))
(declare-function org-agenda-refile "org-agenda-commands-proxy" org-agenda-deadline (&optional goto rfloc no-update))
(declare-function org-agenda-archive-to-archive-sibling "org-agenda-commands-proxy" org-agenda-deadline ())
(declare-function org-agenda-archive "org-agenda-commands-proxy" org-agenda-deadline ())
(declare-function org-refile-get-location "org-refile" (&optional prompt default-buffer new-nodes))
;;;###autoload
(defun org-agenda-bulk-action (&optional arg)
  "Execute an remote-editing action on all marked entries.
The prefix arg is passed through to the command if possible."
  (interactive "P")
  ;; When there is no mark, act on the agenda entry at point.
  (if (not org-agenda-bulk-marked-entries)
      (save-excursion (org-agenda-bulk-mark)))
  (dolist (m org-agenda-bulk-marked-entries)
    (unless (and (markerp m)
		 (marker-buffer m)
		 (buffer-live-p (marker-buffer m))
		 (marker-position m))
      (user-error "Marker %s for bulk command is invalid" m)))

  ;; Prompt for the bulk command.
  (org-unlogged-message
   (concat "Bulk (" (if org-agenda-persistent-marks "" "don't ") "[p]ersist marks): "
	   "[$]arch [A]rch->sib [t]odo [+/-]tag [s]chd [d]eadline [r]efile "
	   "[S]catter [f]unction    "
	   (and org-agenda-bulk-custom-functions
		(format " Custom: [%s]"
			(mapconcat (lambda (f) (char-to-string (car f)))
				   org-agenda-bulk-custom-functions
				   "")))))
  (catch 'exit
    (defvar org-log-refile) ; defined in org-refile.el
    (let* ((org-log-refile (if org-log-refile 'time nil))
	   (entries (reverse org-agenda-bulk-marked-entries))
	   (default-time
	    (and (get-text-property (point) 'org-agenda-date-header)
		 (org-get-cursor-date)))
	   redo-at-end
	   cmd)
      (pcase (read-char-exclusive)
	(?p
	 (let ((org-agenda-persistent-marks
		(not org-agenda-persistent-marks)))
	   (org-agenda-bulk-action)
	   (throw 'exit nil)))

	(?$
	 (setq cmd #'org-agenda-archive))

	(?A
	 (setq cmd #'org-agenda-archive-to-archive-sibling))

	((or ?r ?w)
         (require 'org-refile)
         (defvar org-refile-allow-creating-parent-nodes)
	 (let ((refile-location
		(org-refile-get-location
		 "Refile to"
		 (marker-buffer (car entries))
		 org-refile-allow-creating-parent-nodes)))
	   (when (nth 3 refile-location)
	     (setcar (nthcdr 3 refile-location)
		     (move-marker
		      (make-marker)
		      (nth 3 refile-location)
		      (or (get-file-buffer (nth 1 refile-location))
			  (find-buffer-visiting (nth 1 refile-location))
			  (error "This should not happen")))))

	   (setq cmd (lambda () (org-agenda-refile nil refile-location t)))
	   (setq redo-at-end t)))

	(?t
	 (let ((state (completing-read
		       "Todo state: "
                       (mapcar #'list (org-element-all-todo-keywords
                                       (car entries))))))
           (require 'org-todo)
           (defvar org-inhibit-blocking) ; org-todo.el
           (defvar org-inhibit-logging)
	   (setq cmd (lambda ()
		       (let ((org-inhibit-blocking t)
			     (org-inhibit-logging 'note))
			 (org-agenda-todo state))))))

	((and (or ?- ?+) action)
         (require 'org-tags)
         (declare-function org-local-tags-completion-table "org-tags" ())
	 (let ((tag (completing-read
		     (format "Tag to %s: " (if (eq action ?+) "add" "remove"))
		     (with-current-buffer (marker-buffer (car entries))
		       (delq nil
			     (mapcar (lambda (x) (and (stringp (car x)) x))
				     (org-local-tags-completion-table)))))))
	   (setq cmd
		 (lambda ()
		   (org-agenda-set-tags tag
					(if (eq action ?+) 'on 'off))))))

	((and (or ?s ?d) c)
	 (let* ((schedule? (eq c ?s))
		(prompt (if schedule? "(Re)Schedule to" "(Re)Set Deadline to"))
		(time
		 (and (not arg)
                      (progn
                        (require 'org-read-date)
                        (defvar org-read-date-final-answer)
		        (let ((new (org-read-date
				    nil nil nil prompt default-time)))
			  ;; A "double plus" answer applies to every
			  ;; scheduled time.  Do not turn it into
			  ;; a fixed date yet.
			  (if (string-match-p "\\`[ \t]*\\+\\+"
					      org-read-date-final-answer)
			      org-read-date-final-answer
			    new))))))
	   ;; Make sure to not prompt for a note when bulk
	   ;; rescheduling/resetting deadline as Org cannot cope with
	   ;; simultaneous notes.  Besides, it could be annoying
	   ;; depending on the number of marked items.
           (require 'org-planning)
           (defvar org-log-reschedule)
           (defvar org-log-redeadline)
	   (setq cmd
		 (if schedule?
		     (lambda ()
		       (let ((org-log-reschedule
			      (and org-log-reschedule 'time)))
			 (org-agenda-schedule arg time)))
		   (lambda ()
		     (let ((org-log-redeadline (and org-log-redeadline 'time)))
		       (org-agenda-deadline arg time)))))))

	(?S
	 (unless (org-agenda-check-type nil 'agenda 'todo)
	   (user-error "Can't scatter tasks in \"%s\" agenda view" org-agenda-type))
	 (let ((days (read-number
		      (format "Scatter tasks across how many %sdays: "
			      (if arg "week" ""))
		      7)))
           (require 'org-agenda-agenda-view)
           (defvar org-agenda-weekend-days)
	   (setq cmd
		 (lambda ()
		   (let ((distance (1+ (random days))))
		     (when arg
		       (let ((dist distance)
			     (day-of-week
			      (calendar-day-of-week
			       (calendar-gregorian-from-absolute (org-today)))))
			 (dotimes (_ (1+ dist))
			   (while (member day-of-week org-agenda-weekend-days)
			     (cl-incf distance)
			     (cl-incf day-of-week)
			     (when (= day-of-week 7)
			       (setq day-of-week 0)))
			   (cl-incf day-of-week)
			   (when (= day-of-week 7)
			     (setq day-of-week 0)))))
		     ;; Silently fail when try to replan a sexp entry.
		     (ignore-errors
		       (let* ((date (calendar-gregorian-from-absolute
				     (+ (org-today) distance)))
			      (time (org-encode-time
                                     0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
			 (org-agenda-schedule nil time))))))))

	(?f
	 (setq cmd
	       (intern
		(completing-read "Function: " obarray #'fboundp t nil nil))))

	(action
         (setq cmd
               (pcase (assoc action org-agenda-bulk-custom-functions)
                 (`(,_ ,fn)
                  fn)
                 (`(,_ ,fn ,arg-fn)
                  (apply #'apply-partially fn (funcall arg-fn)))
                 (_
                  (user-error "Invalid bulk action: %c" action))))
         (setq redo-at-end t)))
      ;; Sort the markers, to make sure that parents are handled
      ;; before children.
      (setq entries (sort entries
			  (lambda (a b)
			    (cond
			     ((eq (marker-buffer a) (marker-buffer b))
			      (< (marker-position a) (marker-position b)))
			     (t
			      (string< (buffer-name (marker-buffer a))
				       (buffer-name (marker-buffer b))))))))

      ;; Now loop over all markers and apply CMD.
      (let ((processed 0)
	    (skipped 0))
	(dolist (e entries)
	  (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
	    (if (not pos)
		(progn (message "Skipping removed entry at %s" e)
		       (cl-incf skipped))
	      (goto-char pos)
	      (let (org-loop-over-headlines-in-active-region) (funcall cmd))
	      ;; `post-command-hook' is not run yet.  We make sure any
	      ;; pending log note is processed.
	      (when (bound-and-true-p org-log-setup)
                (org-add-log-note))
	      (cl-incf processed))))
	(when redo-at-end (org-agenda-redo))
	(unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
	(message "Acted on %d entries%s%s"
		 processed
		 (if (= skipped 0)
		     ""
		   (format ", skipped %d (disappeared before their turn)"
			   skipped))
		 (if (not org-agenda-persistent-marks) "" " (kept marked)"))))))

(provide 'org-agenda-bulk-edit)

;;; org-agenda-bulk-edit.el ends here
