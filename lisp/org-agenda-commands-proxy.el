;;; org-agenda-commands-proxy.el --- Agenda wrappers for Org commands  -*- lexical-binding: t; -*-

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

;; This file implements user commands for `org-agenda-mode' that
;; wrap Org editing commands to work from agenda buffer.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-agenda-mode)
(require 'org-time)
(require 'org-agenda-line-format)
(require 'org-agenda-commands)

;;; Helpers

;;;###autoload
(defcustom org-agenda-loop-over-headlines-in-active-region t
  "Shall some commands act upon headlines in the active region?

When set to t, some commands will be performed in all headlines
within the active region.

When set to `start-level', some commands will be performed in all
headlines within the active region, provided that these headlines
are of the same level than the first one.

When set to a regular expression, those commands will be
performed on the matching headlines within the active region.

The list of commands is: `org-agenda-schedule',
`org-agenda-deadline', `org-agenda-date-prompt',
`org-agenda-todo', `org-agenda-archive*', `org-agenda-kill'.

See `org-loop-over-headlines-in-active-region' for the equivalent
option for Org buffers."
  :type '(choice (const :tag "Don't loop" nil)
		 (const :tag "All headlines in active region" t)
		 (const :tag "In active region, headlines at the same level than the first one" start-level)
		 (regexp :tag "Regular expression matcher"))
  :version "27.1"
  :package-version '(Org . "9.4")
  :group 'org-agenda)

(defun org-agenda-do-in-region (beg end cmd &optional arg force-arg delete)
  "Between region BEG and END, call agenda command CMD.
When optional argument ARG is non-nil or FORCE-ARG is t, pass
ARG to CMD.  When optional argument DELETE is non-nil, assume CMD
deletes the agenda entry and don't move to the next entry."
  (save-excursion
    (goto-char beg)
    (let ((mend (move-marker (make-marker) end))
	  (all (eq org-agenda-loop-over-headlines-in-active-region t))
	  (match (and (stringp org-agenda-loop-over-headlines-in-active-region)
		      org-agenda-loop-over-headlines-in-active-region))
	  (level (and (eq org-agenda-loop-over-headlines-in-active-region 'start-level)
		      (org-get-at-bol 'level))))
      (while (< (point) mend)
        (let ((ov (make-overlay (point) (line-end-position))))
	  (if (not (or all
		     (and match (looking-at-p match))
		     (eq level (org-get-at-bol 'level))))
	      (org-agenda-next-item 1)
	    (overlay-put ov 'face 'region)
	    (if (or arg force-arg) (funcall cmd arg) (funcall cmd))
	    (when (not delete) (org-agenda-next-item 1))
	    (delete-overlay ov)))))))

;; org-agenda-[schedule,deadline,date-prompt,todo,[toggle]archive*,
;; kill,set-property,set-effort] commands may loop over agenda
;; entries.  Commands `org-agenda-set-tags' and `org-agenda-bulk-mark'
;; use their own mechanisms on active regions.
(defmacro org-agenda-maybe-loop (cmd arg force-arg delete &rest body)
  "Maybe loop over agenda entries and perform CMD.
Pass ARG, FORCE-ARG, DELETE and BODY to `org-agenda-do-in-region'."
  (declare (debug t))
  `(if (and (called-interactively-p 'any)
	    org-agenda-loop-over-headlines-in-active-region
	    (use-region-p))
       (org-agenda-do-in-region
	(region-beginning) (region-end) ,cmd ,arg ,force-arg ,delete)
     ,@body))

;;; Kill subtree

;;;###autoload
(defcustom org-agenda-confirm-kill 1
  "When set, remote killing from the agenda buffer needs confirmation.
When t, a confirmation is always needed.  When a number N, confirmation is
only needed when the text to be killed contains more than N non-white lines."
  :group 'org-agenda
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Always" t)
	  (integer :tag "When more than N lines")))

;;;###autoload
(defun org-agenda-kill ()
  "Kill the entry or subtree belonging to the current agenda entry."
  (interactive)
  (or (eq major-mode 'org-agenda-mode) (user-error "Not in agenda"))
  (org-agenda-maybe-loop
   #'org-agenda-kill nil nil t
   (let* ((bufname-orig (buffer-name))
	  (marker (or (org-get-at-bol 'org-marker)
		      (org-agenda-error)))
	  (buffer (marker-buffer marker))
	  (type (org-get-at-bol 'type))
	  dbeg dend (n 0))
     (org-with-remote-undo buffer
       (org-with-point-at marker
	 (if (and (derived-mode-p 'org-mode) (not (member type '("sexp"))))
	     (setq dbeg (progn (org-back-to-heading t) (point))
		   dend (org-end-of-subtree t t))
           (setq dbeg (line-beginning-position)
                 dend (min (point-max) (1+ (line-end-position)))))
	 (goto-char dbeg)
	 (while (re-search-forward "^[ \t]*\\S-" dend t) (setq n (1+ n))))
       (when (or (eq t org-agenda-confirm-kill)
		 (and (numberp org-agenda-confirm-kill)
		      (> n org-agenda-confirm-kill)))
	 (let ((win-conf (current-window-configuration)))
           (require 'org-indirect-buffer)
           (defvar org-last-indirect-buffer)
	   (unwind-protect
	       (and
		(prog2
		    (org-agenda-tree-to-indirect-buffer nil)
		    (not (y-or-n-p
			(format "Delete entry with %d lines in buffer \"%s\"? "
				n (buffer-name buffer))))
		  (kill-buffer org-last-indirect-buffer))
		(error "Abort"))
	     (set-window-configuration win-conf))))
       (let ((org-agenda-buffer-name bufname-orig))
	 (org-remove-subtree-entries-from-agenda buffer dbeg dend))
       (org-with-point-at marker (delete-region dbeg dend))
       (message "Agenda item and source killed")))))

;;; Refile

(declare-function org-refile-cache-clear "org-refile" ())
(declare-function org-refile-goto-last-stored "org-refile" ())
(declare-function org-refile-get-location "org-refile" (&optional prompt default-buffer new-nodes))
(declare-function org-refile "org-refile" (&optional arg default-buffer rfloc msg))
;;;###autoload
(defun org-agenda-refile (&optional goto rfloc no-update)
  "Refile the item at point.

When called with `\\[universal-argument] \\[universal-argument]', \
go to the location of the last
refiled item.

When called with `\\[universal-argument] \\[universal-argument] \
\\[universal-argument]' prefix or when GOTO is 0, clear
the refile cache.

RFLOC can be a refile location obtained in a different way.

When NO-UPDATE is non-nil, don't redo the agenda buffer."
  (interactive "P")
  (require 'org-refile)
  (defvar org-refile-allow-creating-parent-nodes)
  (cond
   ((member goto '(0 (64)))
    (org-refile-cache-clear))
   ((equal goto '(16))
    (org-refile-goto-last-stored))
   (t
    (let* ((buffer-orig (buffer-name))
	   (marker (or (org-get-at-bol 'org-hd-marker)
		       (org-agenda-error)))
	   (buffer (marker-buffer marker))
	   ;; (pos (marker-position marker))
	   (rfloc (or rfloc
		      (org-refile-get-location
		       (if goto "Goto" "Refile to") buffer
		       org-refile-allow-creating-parent-nodes))))
      (with-current-buffer buffer
	(org-with-wide-buffer
	 (goto-char marker)
	 (let ((org-agenda-buffer-name buffer-orig))
	   (org-remove-subtree-entries-from-agenda))
	 (org-refile goto buffer rfloc))))
    (unless no-update (org-agenda-redo)))))

;;; Change todo state

;;;###autoload
(defun org-agenda-todo-yesterday (&optional arg)
  "Like `org-agenda-todo' but the time of change will be 23:59 of yesterday."
  (interactive "P")
  (let* ((org-use-effective-time t)
	 (hour (nth 2 (decode-time (org-current-time))))
         (org-extend-today-until (1+ hour)))
    (org-agenda-todo arg)))

;;;###autoload
(defun org-agenda-todo-nextset ()
  "Switch TODO entry to next sequence."
  (interactive)
  (org-agenda-todo 'nextset))

;;;###autoload
(defun org-agenda-todo-previousset ()
  "Switch TODO entry to previous sequence."
  (interactive)
  (org-agenda-todo 'previousset))

;; FIXME: unused
(defvar org-last-heading-marker (make-marker)
  "Marker pointing to the headline that last changed its TODO state
by a remote command from the agenda.")

(defvar org-agenda-headline-snapshot-before-repeat) ; Set for us by `org-todo'
(declare-function org-add-log-note "org-log-note" (&optional _purpose))
(declare-function org-todo "org-todo" (&optional arg))
;;;###autoload
(defun org-agenda-todo (&optional arg)
  "Cycle TODO state of line at point, also in Org file.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org file."
  (interactive "P")
  (org-agenda-check-no-diary)
  (org-agenda-maybe-loop
   #'org-agenda-todo arg nil nil
   (let* ((col (current-column))
	  (marker (or (org-get-at-bol 'org-marker)
		      (org-agenda-error)))
	  (buffer (marker-buffer marker))
	  (pos (marker-position marker))
	  (hdmarker (org-get-at-bol 'org-hd-marker))
	  (todayp (org-agenda-today-p (org-get-at-bol 'day)))
	  (inhibit-read-only t)
	  org-loop-over-headlines-in-active-region
	  org-agenda-headline-snapshot-before-repeat newhead just-one)
     (org-with-remote-undo buffer
       (with-current-buffer buffer
	 (widen)
	 (goto-char pos)
	 (org-fold-show-context 'agenda)
	 (let ((current-prefix-arg arg))
	   (call-interactively #'org-todo)
           ;; Make sure that log is recorded in current undo.
           (require 'org-log-note)
           (defvar org-log-setup)
           (defvar org-log-note-how)
           (when (and org-log-setup
                      (not (eq org-log-note-how 'note)))
             (org-add-log-note)))
	 (and (bolp) (forward-char 1))
	 (setq newhead (org-get-heading))
	 (when (and org-agenda-headline-snapshot-before-repeat
		    (not (equal org-agenda-headline-snapshot-before-repeat
			      newhead))
		    todayp)
	   (setq newhead org-agenda-headline-snapshot-before-repeat
		 just-one t))
	 (save-excursion
	   (org-back-to-heading)
	   (move-marker org-last-heading-marker (point))))
       (forward-line 0)
       (save-window-excursion
	 (org-agenda-change-all-lines newhead hdmarker 'fixface just-one))
       (when (bound-and-true-p org-clock-out-when-done)
	 (string-match (concat "^" (regexp-opt org-done-keywords-for-agenda))
		       newhead)
	 (org-agenda-unmark-clocking-task))
       (org-move-to-column col)
       (org-agenda-mark-clocking-task)))))

;;; Edit tags

(declare-function org-set-tags-command "org-tags" (&optional arg))
(declare-function org-toggle-tag "org-tags" (tag &optional onoff))
(declare-function org-change-tag-in-region "org-tags" (beg end tag off))
;; FIXME: should fix the tags property of the agenda line.
;;;###autoload
(defun org-agenda-set-tags (&optional tag onoff)
  "Set tags for the current headline."
  (interactive)
  (org-agenda-check-no-diary)
  (if (and (use-region-p) (called-interactively-p 'any))
      (call-interactively #'org-change-tag-in-region)
    (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
			 (org-agenda-error)))
	   (buffer (marker-buffer hdmarker))
	   (pos (marker-position hdmarker))
	   (inhibit-read-only t)
	   newhead)
      (org-with-remote-undo buffer
	(with-current-buffer buffer
	  (widen)
	  (goto-char pos)
	  (org-fold-show-context 'agenda)
	  (if tag
	      (org-toggle-tag tag onoff)
	    (call-interactively #'org-set-tags-command))
	  (end-of-line 1)
	  (setq newhead (org-get-heading)))
	(org-agenda-change-all-lines newhead hdmarker)
	(forward-line 0)))))

;;;###autoload
(defun org-agenda-ctrl-c-ctrl-c ()
  "Set tags in agenda buffer."
  (interactive)
  (org-agenda-set-tags))

;;; Edit priority

;;;###autoload
(defun org-agenda-priority-up ()
  "Increase the priority of line at point, also in Org file."
  (interactive)
  (org-agenda-priority 'up))

;;;###autoload
(defun org-agenda-priority-down ()
  "Decrease the priority of line at point, also in Org file."
  (interactive)
  (org-agenda-priority 'down))

(declare-function org-priority "org-priority" (&optional action show))
;;;###autoload
(defun org-agenda-priority (&optional force-direction)
  "Set the priority of line at point, also in Org file.
This changes the line at point, all other lines in the agenda
referring to the same tree node, and the headline of the tree
node in the Org file.

Called with one universal prefix arg, show the priority instead
of setting it.

When called programmatically, FORCE-DIRECTION can be `set', `up',
`down', or a character."
  (interactive "P")
  (unless org-priority-enable-commands
    (user-error "Priority commands are disabled"))
  (org-agenda-check-no-diary)
  (let* ((col (current-column))
	 (hdmarker (org-get-at-bol 'org-hd-marker))
	 (buffer (marker-buffer hdmarker))
	 (pos (marker-position hdmarker))
	 (inhibit-read-only t)
	 newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(org-fold-show-context 'agenda)
	(org-priority force-direction)
	(end-of-line 1)
	(setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker)
      (org-move-to-column col))))

;;; Set properties

(declare-function org-set-property "org-property-set" (property value))
;;;###autoload
(defun org-agenda-set-property ()
  "Set a property for the current headline."
  (interactive)
  (org-agenda-check-no-diary)
  (org-agenda-maybe-loop
   #'org-agenda-set-property nil nil nil
   (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
			(org-agenda-error)))
	  (buffer (marker-buffer hdmarker))
	  (pos (marker-position hdmarker))
	  (inhibit-read-only t)
	  ) ;; newhead
     (org-with-remote-undo buffer
       (with-current-buffer buffer
	 (widen)
	 (goto-char pos)
	 (org-fold-show-context 'agenda)
	 (call-interactively #'org-set-property))))))

(declare-function org-set-effort "org-property-set" (&optional increment value))
;;;###autoload
(defun org-agenda-set-effort ()
  "Set the effort property for the current headline."
  (interactive)
  (org-agenda-check-no-diary)
  (org-agenda-maybe-loop
   #'org-agenda-set-effort nil nil nil
   (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
			(org-agenda-error)))
	  (buffer (marker-buffer hdmarker))
	  (pos (marker-position hdmarker))
	  (inhibit-read-only t)
	  newhead)
     (org-with-remote-undo buffer
       (with-current-buffer buffer
	 (widen)
	 (goto-char pos)
	 (org-fold-show-context 'agenda)
	 (call-interactively #'org-set-effort)
	 (end-of-line 1)
	 (setq newhead (org-get-heading)))
       (org-agenda-change-all-lines newhead hdmarker)))))

;;; Take notes

(declare-function org-add-note "org-log-note" ())
;;;###autoload
(defun org-agenda-add-note (&optional _arg)
  "Add a time-stamped note to the entry at point."
  (interactive) ;; "P"
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (_hdmarker (org-get-at-bol 'org-hd-marker))
	 (inhibit-read-only t))
    (with-current-buffer buffer
      (widen)
      (goto-char pos)
      (org-fold-show-context 'agenda)
      (org-add-note))))

;;; Set schedule/deadline/timestamp

(declare-function org-timestamp "org-timestamp" (arg &optional inactive))
;;;###autoload
(defun org-agenda-date-prompt (arg)
  "Change the date of this item.  Date is prompted for, with default today.
The prefix ARG is passed to the `org-timestamp' command and can therefore
be used to request time specification in the time stamp."
  (interactive "P")
  (org-agenda-check-type t 'agenda)
  (org-agenda-check-no-diary)
  (org-agenda-maybe-loop
   #'org-agenda-date-prompt arg t nil
   (let* ((marker (or (org-get-at-bol 'org-marker)
		      (org-agenda-error)))
	  (buffer (marker-buffer marker))
	  (pos (marker-position marker)))
     (require 'org-timestamp)
     (defvar org-last-changed-timestamp) ; set by `org-timestamp'
     (org-with-remote-undo buffer
       (with-current-buffer buffer
	 (widen)
	 (goto-char pos)
	 (unless (org-at-timestamp-p 'lax) (error "Cannot find time stamp"))
	 (org-timestamp arg (equal (char-after (match-beginning 0)) ?\[)))
       (org-agenda-show-new-time marker org-last-changed-timestamp))
     (message "Time stamp changed to %s" org-last-changed-timestamp))))

(declare-function org-schedule "org-planning" (arg &optional time))
;;;###autoload
(defun org-agenda-schedule (arg &optional time)
  "Schedule the item at point.
ARG is passed through to `org-schedule'."
  (interactive "P")
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (org-agenda-maybe-loop
   #'org-agenda-schedule arg t nil
   (let* ((marker (or (org-get-at-bol 'org-marker)
		      (org-agenda-error)))
	  ;; (type (marker-insertion-type marker))
	  (buffer (marker-buffer marker))
	  (pos (marker-position marker))
	  ts)
     (set-marker-insertion-type marker t)
     (org-with-remote-undo buffer
       (with-current-buffer buffer
	 (widen)
	 (goto-char pos)
	 (setq ts (org-schedule arg time)))
       (org-agenda-show-new-time marker ts " S"))
     (message "%s" ts))))

(declare-function org-deadline "org-planning" (arg &optional time))
;;;###autoload
(defun org-agenda-deadline (arg &optional time)
  "Schedule the item at point.
ARG is passed through to `org-deadline'."
  (interactive "P")
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (org-agenda-maybe-loop
   #'org-agenda-deadline arg t nil
   (let* ((marker (or (org-get-at-bol 'org-marker)
		      (org-agenda-error)))
	  (buffer (marker-buffer marker))
	  (pos (marker-position marker))
	  ts)
     (org-with-remote-undo buffer
       (with-current-buffer buffer
	 (widen)
	 (goto-char pos)
	 (setq ts (org-deadline arg time)))
       (org-agenda-show-new-time marker ts " D"))
     (message "%s" ts))))

;;; Clocking

(declare-function org-clock-in "org-clock-core"
                  (&optional select start-time))
;;;###autoload
(defun org-agenda-clock-in (&optional arg)
  "Start the clock on the currently selected item."
  (interactive "P")
  (org-agenda-check-no-diary)
  (require 'org-clock-core)
  (if (equal arg '(4))
      (org-clock-in arg)
    (let* ((marker (or (org-get-at-bol 'org-marker)
		       (org-agenda-error)))
	   (hdmarker (or (org-get-at-bol 'org-hd-marker) marker))
	   (pos (marker-position marker))
	   (col (current-column))
	   newhead)
      (org-with-remote-undo (marker-buffer marker)
        (with-current-buffer (marker-buffer marker)
	  (widen)
	  (goto-char pos)
	  (org-fold-show-context 'agenda)
	  (org-clock-in arg)
	  (setq newhead (org-get-heading)))
	(org-agenda-change-all-lines newhead hdmarker))
      (org-move-to-column col))))

(declare-function org-clock-out "org-clock-core"
                  (&optional switch-to-state fail-quietly at-time))
;;;###autoload
(defun org-agenda-clock-out ()
  "Stop the currently running clock."
  (interactive)
  (unless (marker-buffer org-clock-marker)
    (user-error "No running clock"))
  (require 'org-clock-core)
  (let ((marker (make-marker)) (col (current-column)) newhead)
    (org-with-remote-undo (marker-buffer org-clock-marker)
      (with-current-buffer (marker-buffer org-clock-marker)
	(org-with-wide-buffer
	 (goto-char org-clock-marker)
	 (org-back-to-heading t)
	 (move-marker marker (point))
	 (org-clock-out)
	 (setq newhead (org-get-heading)))))
    (org-agenda-change-all-lines newhead marker)
    (move-marker marker nil)
    (org-move-to-column col)
    (org-agenda-unmark-clocking-task)))

(declare-function org-clock-cancel "org-clock-core" ())
;;;###autoload
(defun org-agenda-clock-cancel (&optional _arg)
  "Cancel the currently running clock."
  (interactive) ;; "P"
  (require 'org-clock-core)
  (unless (marker-buffer org-clock-marker)
    (user-error "No running clock"))
  (org-with-remote-undo (marker-buffer org-clock-marker)
    (org-clock-cancel))
  (org-agenda-unmark-clocking-task))

(declare-function org-clock-goto "org-clock-commands" (&optional select))
;;;###autoload
(defun org-agenda-clock-goto ()
  "Jump to the currently clocked in task within the agenda.
If the currently clocked in task is not listed in the agenda
buffer, display it in another window."
  (interactive)
  (let (pos)
    (mapc (lambda (o)
	    (when (eq (overlay-get o 'type) 'org-agenda-clocking)
	      (setq pos (overlay-start o))))
	  (overlays-in (point-min) (point-max)))
    (cond (pos (goto-char pos))
	  ;; If the currently clocked entry is not in the agenda
	  ;; buffer, we visit it in another window:
	  ((bound-and-true-p org-clock-current-task)
	   (switch-to-buffer-other-window (org-clock-goto)))
	  (t (message "No running clock, use `C-c C-x C-j' to jump to the most recent one")))))

;;; Archiving

(declare-function org-toggle-archive-tag "org-archive" (&optional find-done))
;;;###autoload
(defun org-agenda-toggle-archive-tag ()
  "Toggle the archive tag for the current entry."
  (interactive)
  (org-agenda-check-no-diary)
  (org-agenda-maybe-loop
   #'org-agenda-toggle-archive-tag nil nil nil
   (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
			(org-agenda-error)))
	  (buffer (marker-buffer hdmarker))
	  (pos (marker-position hdmarker))
	  (inhibit-read-only t)
	  newhead)
     (org-with-remote-undo buffer
       (with-current-buffer buffer
	 (widen)
	 (goto-char pos)
	 (org-fold-show-context 'agenda)
	 (call-interactively #'org-toggle-archive-tag)
	 (end-of-line 1)
	 (setq newhead (org-get-heading)))
       (org-agenda-change-all-lines newhead hdmarker)
       (forward-line 0)))))

(defvar org-archive-default-command) ; defined in org-archive.el
;;;###autoload
(defun org-agenda-archive-default ()
  "Archive the entry or subtree belonging to the current agenda entry."
  (interactive)
  (require 'org-archive)
  (funcall-interactively
   #'org-agenda-archive-with org-archive-default-command))

;;;###autoload
(defun org-agenda-archive-default-with-confirmation ()
  "Archive the entry or subtree belonging to the current agenda entry."
  (interactive)
  (require 'org-archive)
  (funcall-interactively
   #'org-agenda-archive-with org-archive-default-command 'confirm))

;;;###autoload
(defun org-agenda-archive ()
  "Archive the entry or subtree belonging to the current agenda entry."
  (interactive)
  (funcall-interactively
   #'org-agenda-archive-with 'org-archive-subtree))

;;;###autoload
(defun org-agenda-archive-to-archive-sibling ()
  "Move the entry to the archive sibling."
  (interactive)
  (funcall-interactively
   #'org-agenda-archive-with 'org-archive-to-archive-sibling))

(defvar org-archive-from-agenda)
;;;###autoload
(defun org-agenda-archive-with (cmd &optional confirm)
  "Move the entry to the archive sibling."
  (interactive)
  (or (eq major-mode 'org-agenda-mode) (user-error "Not in agenda"))
  (org-agenda-maybe-loop
   #'org-agenda-archive-with cmd nil t
   (let* ((bufname-orig (buffer-name))
	  (marker (or (org-get-at-bol 'org-marker)
		      (org-agenda-error)))
	  (buffer (marker-buffer marker))
	  (pos (marker-position marker)))
     (org-with-remote-undo buffer
       (with-current-buffer buffer
	 (if (derived-mode-p 'org-mode)
	     (if (and confirm
		      (not (y-or-n-p "Archive this subtree or entry? ")))
		 (error "Abort")
	       (save-window-excursion
		 (goto-char pos)
		 (let ((org-agenda-buffer-name bufname-orig))
		   (org-remove-subtree-entries-from-agenda))
		 (org-back-to-heading t)
		 (let ((org-archive-from-agenda t))
		   (funcall cmd))))
	   (error "Archiving works only in Org files")))))))

(defun org-remove-subtree-entries-from-agenda (&optional buf beg end)
  "Remove all lines in the agenda that correspond to a given subtree.
The subtree is the one in buffer BUF, starting at BEG and ending at END.
If this information is not given, the function uses the tree at point."
  (let ((buf (or buf (current-buffer))) m p)
    (org-with-wide-buffer
     (unless (and beg end)
       (org-back-to-heading t)
       (setq beg (point))
       (org-end-of-subtree t)
       (setq end (point)))
     (set-buffer (get-buffer org-agenda-buffer-name))
     (save-excursion
       (goto-char (point-max))
       (forward-line 0)
       (while (not (bobp))
	 (when (and (setq m (org-get-at-bol 'org-marker))
		    (equal buf (marker-buffer m))
		    (setq p (marker-position m))
		    (>= p beg)
		    (< p end))
	   (let ((inhibit-read-only t))
             (delete-region (line-beginning-position)
                            (1+ (line-end-position)))))
	 (forward-line -1))))))

;;; Capture

;;;###autoload
(defun org-agenda-capture (&optional with-time)
  "Call `org-capture' with the date at point.
With a `C-1' prefix, use the HH:MM value at point (if any) or the
current HH:MM time."
  (interactive "P")
  (if (not (eq major-mode 'org-agenda-mode))
      (user-error "You cannot do this outside of agenda buffers")
    ;; defined and used by org-read-date.el
    ;; Used by org-read-date and org-capture
    (defvar org-overriding-default-time)
    (let ((org-overriding-default-time
	   (org-get-cursor-date (equal with-time 1))))
      (call-interactively #'org-capture))))

(provide 'org-agenda-commands-proxy)

;;; org-agenda-commands-proxy.el ends here
