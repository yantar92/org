;;; org-planning.el --- Org planning commands         -*- lexical-binding: t; -*-

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
;; This library implements functions and commands to set planning
;; info: scheduling, deadlines, and closed keywords.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-move)
(require 'org-log-note)
(require 'org-mode-common)
(require 'org-read-date)
(require 'org-timestamp)
(require 'org-indent-static)

(defgroup org-progress nil
  "Options concerning Progress logging in Org mode."
  :tag "Org Progress"
  :group 'org-time)

(defcustom org-log-reschedule nil
  "Information to record when the scheduling date of a task is modified.

Possible values are:

nil     Don't add anything, just change the date
time    Add a time stamp to the task
note    Prompt for a note and add it with template `org-log-note-headings'

This option can also be set with on a per-file-basis with

   #+STARTUP: nologreschedule
   #+STARTUP: logreschedule
   #+STARTUP: lognotereschedule

You can have local logging settings for a subtree by setting the LOGGING
property to one or more of these keywords.

This variable has an effect when calling `org-schedule' or
`org-agenda-schedule' only."
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "No logging" nil)
	  (const :tag "Record timestamp" time)
	  (const :tag "Record timestamp with note" note)))

(defcustom org-log-redeadline nil
  "Information to record when the deadline date of a task is modified.

Possible values are:

nil     Don't add anything, just change the date
time    Add a time stamp to the task
note    Prompt for a note and add it with template `org-log-note-headings'

This option can also be set with on a per-file-basis with

   #+STARTUP: nologredeadline
   #+STARTUP: logredeadline
   #+STARTUP: lognoteredeadline

You can have local logging settings for a subtree by setting the LOGGING
property to one or more of these keywords.

This variable has an effect when calling `org-deadline' or
`org-agenda-deadline' only."
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "No logging" nil)
	  (const :tag "Record timestamp" time)
	  (const :tag "Record timestamp with note." note)))

(defcustom org-log-done-with-time t
  "Non-nil means the CLOSED time stamp will contain date and time.
When nil, only the date will be recorded."
  :group 'org-progress
  :type 'boolean)

(defalias 'org-cancel-repeater #'org-cancel-repeaters)
(defun org-cancel-repeaters ()
  "Cancel all the repeaters in entry by setting their numeric value to zero."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((bound1 (point))
	  (bound0 (save-excursion (outline-next-heading) (point))))
      (while (re-search-forward
	      (concat "\\(" org-scheduled-time-regexp "\\)\\|\\("
		      org-deadline-time-regexp "\\)\\|\\("
		      org-ts-regexp "\\)")
	      bound0 t)
        (when (save-excursion
	        (re-search-backward "[ \t]+\\(?:[.+]\\)?\\+\\([0-9]+\\)[hdwmy]"
			            bound1 t))
	  (replace-match "0" t nil nil 1))))))

(defun org--deadline-or-schedule (arg type time)
  "Insert DEADLINE or SCHEDULE information in current entry.
TYPE is either `deadline' or `scheduled'.  See `org-deadline' or
`org-schedule' for information about ARG and TIME arguments."
  (org-fold-core-ignore-modifications
    (let* ((deadline? (eq type 'deadline))
	   (keyword (if deadline? org-deadline-string org-scheduled-string))
	   (log (if deadline? org-log-redeadline org-log-reschedule))
	   (old-date
            (thread-last
              (org-headline-at-point)
              (org-element-property (if deadline? :deadline :scheduled))
              (org-element-property :raw-value)))
	   (old-date-time (and old-date (org-time-string-to-time old-date)))
	   ;; Save repeater cookie from either TIME or current scheduled
	   ;; time stamp.  We are going to insert it back at the end of
	   ;; the process.
	   (repeater (or (and (org-string-nw-p time)
			      ;; We use `org-ts-regexp-both' because we
			      ;; need to tell the difference between a
			      ;; real repeater and a time delta, e.g.
			      ;; "+2d".
                              (string-match-p org-ts-regexp-both time)
                              (string-match "\\([.+-]+[0-9]+[hdwmy]\
\\(?:[/ ][-+]?[0-9]+[hdwmy]\\)?\\)"
					    time)
			      (match-string 1 time))
		         (and (org-string-nw-p old-date)
			      (string-match "\\([.+-]+[0-9]+[hdwmy]\
\\(?:[/ ][-+]?[0-9]+[hdwmy]\\)?\\)"
					    old-date)
			      (match-string 1 old-date)))))
      (pcase arg
        (`(4)
         (if (not old-date)
	     (message (if deadline? "Entry had no deadline to remove"
		        "Entry was not scheduled"))
	   (when (and old-date log)
	     (org-add-log-setup (if deadline? 'deldeadline 'delschedule)
			        nil old-date log))
	   (org-remove-timestamp-with-keyword keyword)
	   (message (if deadline? "Entry no longer has a deadline."
		      "Entry is no longer scheduled."))))
        (`(16)
         (save-excursion
	   (org-back-to-heading t)
	   (let ((regexp (if deadline? org-deadline-time-regexp
			   org-scheduled-time-regexp)))
	     (if (not (re-search-forward regexp (line-end-position 2) t))
	         (user-error (if deadline? "No deadline information to update"
			       "No scheduled information to update"))
	       (let* ((rpl0 (match-string 1))
		      (rpl (replace-regexp-in-string " -[0-9]+[hdwmy]" "" rpl0))
		      (msg (if deadline? "Warn starting from" "Delay until")))
	         (replace-match
		  (concat keyword
			  " <" rpl
			  (format " -%dd"
				  (abs (- (time-to-days
					   (save-match-data
					     (org-read-date
					      nil t nil msg old-date-time)))
					  (time-to-days old-date-time))))
			  ">") t t))))))
        (_
         (org-add-planning-info type time 'closed)
         (when (and old-date
		    log
		    (not (equal old-date org-last-inserted-timestamp)))
	   (org-add-log-setup (if deadline? 'redeadline 'reschedule)
			      org-last-inserted-timestamp
			      old-date
			      log))
         (when repeater
	   (save-excursion
	     (org-back-to-heading t)
	     (when (re-search-forward
		    (concat keyword " " org-last-inserted-timestamp)
		    (line-end-position 2)
		    t)
	       (goto-char (1- (match-end 0)))
	       (insert-and-inherit " " repeater)
	       (setq org-last-inserted-timestamp
		     (concat (substring org-last-inserted-timestamp 0 -1)
			     " " repeater
			     (substring org-last-inserted-timestamp -1))))))
         (message (if deadline? "Deadline on %s" "Scheduled to %s")
		  org-last-inserted-timestamp))))))

(declare-function org-map-entries "org-map"
                  (func &optional match scope &rest skip))
;;;###autoload
(defun org-deadline (arg &optional time)
  "Insert a \"DEADLINE:\" string with a timestamp to make a deadline.

When called interactively, this command pops up the Emacs calendar to let
the user select a date.

With one universal prefix argument, remove any deadline from the item.
With two universal prefix arguments, prompt for a warning delay.
With argument TIME, set the deadline at the corresponding date.  TIME
can either be an Org date like \"2011-07-24\" or a delta like \"+2d\"."
  (interactive "P")
  (if (and (use-region-p) org-loop-over-headlines-in-active-region)
      (progn
        (require 'org-map)
        (org-map-entries
         (lambda () (org--deadline-or-schedule arg 'deadline time))
         nil
         (if (eq org-loop-over-headlines-in-active-region 'start-level)
	     'region-start-level
           'region)
         (lambda () (when (org-invisible-p) (org-end-of-subtree nil t)))))
    (org--deadline-or-schedule arg 'deadline time)))

;;;###autoload
(defun org-schedule (arg &optional time)
  "Insert a \"SCHEDULED:\" string with a timestamp to schedule an item.

When called interactively, this command pops up the Emacs calendar to let
the user select a date.

With one universal prefix argument, remove any scheduling date from the item.
With two universal prefix arguments, prompt for a delay cookie.
With argument TIME, scheduled at the corresponding date.  TIME can
either be an Org date like \"2011-07-24\" or a delta like \"+2d\"."
  (interactive "P")
  (if (and (use-region-p) org-loop-over-headlines-in-active-region)
      (org-map-entries
       (lambda () (org--deadline-or-schedule arg 'scheduled time))
       nil
       (if (eq org-loop-over-headlines-in-active-region 'start-level)
	   'region-start-level
	 'region)
       (lambda () (when (org-invisible-p) (org-end-of-subtree nil t))))
    (org--deadline-or-schedule arg 'scheduled time)))

(defun org-remove-timestamp-with-keyword (keyword)
  "Remove all time stamps with KEYWORD in the current entry."
  (let ((re (concat "\\<" (regexp-quote keyword) " +<[^>\n]+>[ \t]*"))
	beg)
    (save-excursion
      (org-back-to-heading t)
      (setq beg (point))
      (outline-next-heading)
      (while (re-search-backward re beg t)
	(replace-match "")
        (if (and (string-match "\\S-" (buffer-substring (line-beginning-position) (point)))
		 (equal (char-before) ?\ ))
	    (delete-char -1)
	  (when (string-match "^[ \t]*$" (buffer-substring
                                          (line-beginning-position) (line-end-position)))
            (delete-region (line-beginning-position)
                           (min (point-max) (1+ (line-end-position))))))))))

(defvar org-time-was-given) ; dynamically scoped parameter
(defvar org-end-time-was-given) ; dynamically scoped parameter

(defun org-add-planning-info (what &optional time &rest remove)
  "Insert new timestamp with keyword in the planning line.
WHAT indicates what kind of time stamp to add.  It is a symbol
among `closed', `deadline', `scheduled' and nil.  TIME indicates
the time to use.  If none is given, the user is prompted for
a date.  REMOVE indicates what kind of entries to remove.  An old
WHAT entry will also be removed."
  (org-fold-core-ignore-modifications
    (let (org-time-was-given org-end-time-was-given default-time default-input)
      (when (and (memq what '(scheduled deadline))
	         (or (not time)
		     (and (stringp time)
			  (string-match "^[-+]+[0-9]" time))))
        ;; Try to get a default date/time from existing timestamp
        (save-excursion
	  (org-back-to-heading t)
	  (let ((end (save-excursion (outline-next-heading) (point))) ts)
	    (when (re-search-forward (if (eq what 'scheduled)
				         org-scheduled-time-regexp
				       org-deadline-time-regexp)
				     end t)
	      (setq ts (match-string 1)
		    default-time (org-time-string-to-time ts)
		    default-input (and ts (org-get-compact-tod ts)))))))
      (when what
        (setq time
	      (if (stringp time)
		  ;; This is a string (relative or absolute), set
		  ;; proper date.
		  (org-encode-time
		   (org-read-date-analyze
		    time default-time (decode-time default-time)))
	        ;; If necessary, get the time from the user
	        (or time (org-read-date nil 'to-time nil
				        (cl-case what
				          (deadline "DEADLINE")
				          (scheduled "SCHEDULED")
				          (otherwise nil))
				        default-time default-input)))))
      (org-with-wide-buffer
       (org-back-to-heading t)
       (let ((planning? (save-excursion
			  (forward-line)
			  (looking-at-p org-planning-line-re))))
         (cond
	  (planning?
	   (forward-line)
	   ;; Move to current indentation.
	   (skip-chars-forward " \t")
	   ;; Check if we have to remove something.
	   (dolist (type (if what (cons what remove) remove))
	     (save-excursion
	       (when (re-search-forward
		      (cl-case type
		        (closed org-closed-time-regexp)
		        (deadline org-deadline-time-regexp)
		        (scheduled org-scheduled-time-regexp)
		        (otherwise (error "Invalid planning type: %s" type)))
		      (line-end-position)
		      t)
	         ;; Delete until next keyword or end of line.
	         (delete-region
		  (match-beginning 0)
		  (if (re-search-forward org-keyword-time-not-clock-regexp
				         (line-end-position)
				         t)
		      (match-beginning 0)
		    (line-end-position))))))
	   ;; If there is nothing more to add and no more keyword is
	   ;; left, remove the line completely.
	   (if (and (looking-at-p "[ \t]*$") (not what))
	       (delete-region (line-end-position 0)
			      (line-end-position))
	     ;; If we removed last keyword, do not leave trailing white
	     ;; space at the end of line.
	     (let ((p (point)))
	       (save-excursion
	         (end-of-line)
	         (unless (= (skip-chars-backward " \t" p) 0)
		   (delete-region (point) (line-end-position)))))))
	  (what
	   (end-of-line)
	   (insert-and-inherit "\n")
	   (when org-adapt-indentation
	     (indent-to-column (1+ (org-outline-level)))))
	  (t nil)))
       (when what
         ;; Insert planning keyword.
         (insert-and-inherit (cl-case what
		               (closed org-closed-string)
		               (deadline org-deadline-string)
		               (scheduled org-scheduled-string)
		               (otherwise (error "Invalid planning type: %s" what)))
	                     " ")
         ;; Insert associated timestamp.
         (let ((ts (org-insert-timestamp
		    time
		    (or org-time-was-given
		        (and (eq what 'closed) org-log-done-with-time))
		    (eq what 'closed)
		    nil nil (list org-end-time-was-given))))
	   (unless (eolp) (insert " "))
	   ts))))))

(provide 'org-planning)

;;; org-planning.el ends here


