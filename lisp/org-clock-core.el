;;; org-clock-core.el --- The time clocking code for Org mode -*- lexical-binding: t; -*-

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

;; This file contains the time clocking code for Org mode

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-regexps)
(require 'org-mode-common)
(require 'org-time)
(require 'org-fold)
(require 'org-log-note)
(require 'org-element-timestamp)
(require 'org-timestamp)
(require 'org-edit-structure-common)
(require 'org-indent-static)
(require 'org-agenda-files)
(require 'org-duration)
(require 'org-property)
(require 'org-idle)

(defvar org-frame-title-format-backup nil)
(require 'org-clock-common)
(require 'org-clock-sum)
(require 'org-clock-notify)

(defvar org-state) ; dynamically scoped in org-todo.el

(defgroup org-clock nil
  "Options concerning clocking working time in Org mode."
  :tag "Org Clock"
  :group 'org-progress)

(defcustom org-clock-into-drawer t
  "Non-nil when clocking info should be wrapped into a drawer.

When non-nil, clocking info will be inserted into the same drawer
as log notes (see variable `org-log-into-drawer'), if it exists,
or \"LOGBOOK\" otherwise.  If necessary, the drawer will be
created.

When an integer, the drawer is created only when the number of
clocking entries in an item reaches or exceeds this value.

When a string, it becomes the name of the drawer, ignoring the
log notes drawer altogether.

Do not check directly this variable in a Lisp program.  Call
function `org-clock-into-drawer' instead."
  :group 'org-todo
  :group 'org-clock
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Only when drawer exists" nil)
	  (integer :tag "When at least N clock entries")
	  (const :tag "Into LOGBOOK drawer" "LOGBOOK")
	  (string :tag "Into Drawer named...")))

(defun org-check-running-clock ()
  "Check if the current buffer contains the running clock.
If yes, offer to stop it and to save the buffer with the changes."
  (when (and (equal (marker-buffer org-clock-marker) (current-buffer))
	     (y-or-n-p (format "Clock-out in buffer %s before killing it? "
			       (buffer-name))))
    (org-clock-out)
    (when (y-or-n-p "Save changed buffer?")
      (save-buffer))))

;;;###autoload
(defun org-clock-auto-clockout-insinuate ()
  "Set up hook for auto clocking out when Emacs is idle.
See `org-clock-auto-clockout-timer'.

This function is meant to be added to the user configuration."
  (add-hook 'org-clock-in-hook #'org-clock-auto-clockout t))

(defun org-clock-into-drawer ()
  "Value of `org-clock-into-drawer', but let properties overrule.

If the current entry has or inherits a CLOCK_INTO_DRAWER
property, it will be used instead of the default value.

Return value is either a string, an integer, or nil."
  (let ((p (org-entry-get nil "CLOCK_INTO_DRAWER" 'inherit t)))
    (cond ((equal p "nil") nil)
	  ((equal p "t") (or (org-log-into-drawer) "LOGBOOK"))
          ((org-string-nw-p p)
	   (if (string-match-p "\\`[0-9]+\\'" p) (string-to-number p) p))
	  ((org-string-nw-p org-clock-into-drawer))
	  ((integerp org-clock-into-drawer) org-clock-into-drawer)
	  ((not org-clock-into-drawer) nil)
	  ((org-log-into-drawer))
	  (t "LOGBOOK"))))

(defcustom org-clock-out-when-done t
  "When non-nil, clock will be stopped when the clocked entry is marked DONE.
\\<org-mode-map>\
DONE here means any DONE-like state.
A nil value means clock will keep running until stopped explicitly with
`\\[org-clock-out]', or until the clock is started in a different item.
Instead of t, this can also be a list of TODO states that should trigger
clocking out."
  :group 'org-clock
  :type '(choice
	  (const :tag "No" nil)
	  (const :tag "Yes, when done" t)
	  (repeat :tag "State list"
		  (string :tag "TODO keyword"))))

(defcustom org-clock-rounding-minutes 0
  "Rounding minutes when clocking in or out.
The default value is 0 so that no rounding is done.
When set to a non-integer value, use the car of
`org-timestamp-rounding-minutes', like for setting a timestamp.

E.g. if `org-clock-rounding-minutes' is set to 5, time is 14:47
and you clock in: then the clock starts at 14:45.  If you clock
out within the next 5 minutes, the clock line will be removed;
if you clock out 8 minutes after your clocked in, the clock
out time will be 14:50."
  :group 'org-clock
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (integer :tag "Minutes (0 for no rounding)")
	  (symbol  :tag "Use `org-time-stamp-rounding-minutes'" 'same-as-time-stamp)))

(defcustom org-clock-out-remove-zero-time-clocks nil
  "Non-nil means remove the clock line when the resulting time is zero."
  :group 'org-clock
  :type 'boolean)

(defcustom org-clock-in-switch-to-state nil
  "Set task to a special todo state while clocking it.
The value should be the state to which the entry should be
switched.  If the value is a function, it must take one
parameter (the current TODO state of the item) and return the
state to switch it to."
  :group 'org-clock
  :group 'org-todo
  :type '(choice
	  (const :tag "Don't force a state" nil)
	  (string :tag "State")
	  (symbol :tag "Function")))

(defcustom org-clock-out-switch-to-state nil
  "Set task to a special todo state after clocking out.
The value should be the state to which the entry should be
switched.  If the value is a function, it must take one
parameter (the current TODO state of the item) and return the
state to switch it to."
  :group 'org-clock
  :group 'org-todo
  :type '(choice
	  (const :tag "Don't force a state" nil)
	  (string :tag "State")
	  (symbol :tag "Function")))

(defcustom org-clock-history-length 5
  "Number of clock tasks to remember in history.
Clocking in using history works best if this is at most 35, in
which case all digits and capital letters are used up by the
*Clock Task Select* buffer."
  :group 'org-clock
  :type 'integer)

(defcustom org-clock-goto-may-find-recent-task t
  "Non-nil means `org-clock-goto' can go to recent task if no active clock."
  :group 'org-clock
  :type 'boolean)

(defcustom org-clock-heading-function nil
  "When non-nil, should be a function to create `org-clock-heading'.
This is the string shown in the mode line when a clock is running.
The function is called with point at the beginning of the headline."
  :group 'org-clock
  :type '(choice (const nil) (function)))

(defcustom org-clock-string-limit 0
  "Maximum length of clock strings in the mode line.  0 means no limit."
  :group 'org-clock
  :type 'integer)

(defcustom org-clock-in-resume nil
  "If non-nil, resume clock when clocking into task with open clock.
When clocking into a task with a clock entry which has not been closed,
the clock can be resumed from that point."
  :group 'org-clock
  :type 'boolean)

(defcustom org-clock-mode-line-total 'auto
  "Default setting for the time included for the mode line clock.
This can be overruled locally using the CLOCK_MODELINE_TOTAL property.
Allowed values are:

current  Only the time in the current instance of the clock
today    All time clocked into this task today
repeat   All time clocked into this task since last repeat
all      All time ever recorded for this task
auto     Automatically, either `all', or `repeat' for repeating tasks"
  :group 'org-clock
  :type '(choice
	  (const :tag "Current clock" current)
	  (const :tag "Today's task time" today)
	  (const :tag "Since last repeat" repeat)
	  (const :tag "All task time" all)
	  (const :tag "Automatically, `all' or since `repeat'" auto)))

(defvaralias 'org-task-overrun-text 'org-clock-task-overrun-text)
(defcustom org-clock-task-overrun-text nil
  "Extra mode line text to indicate that the clock is overrun.
The can be nil to indicate that instead of adding text, the clock time
should get a different face (`org-mode-line-clock-overrun').
When this is a string, it is prepended to the clock string as an indication,
also using the face `org-mode-line-clock-overrun'."
  :group 'org-clock
  :version "24.1"
  :type '(choice
	  (const :tag "Just mark the time string" nil)
	  (string :tag "Text to prepend")))

(defcustom org-clock-idle-time nil
  "When non-nil, resolve open clocks if the user is idle more than X minutes."
  :group 'org-clock
  :type '(choice
	  (const :tag "Never" nil)
	  (integer :tag "After N minutes")))

(defcustom org-clock-auto-clock-resolution 'when-no-clock-is-running
  "When to automatically resolve open clocks found in Org buffers."
  :group 'org-clock
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Always" t)
	  (const :tag "When no clock is running" when-no-clock-is-running)))

(defcustom org-clock-resolve-expert nil
  "Non-nil means do not show the splash buffer with the clock resolver."
  :group 'org-clock
  :version "24.1"
  :type 'boolean)

(defcustom org-clock-continuously nil
  "Non-nil means to start clocking from the last clock-out time, if any."
  :type 'boolean
  :version "24.1"
  :group 'org-clock)

(defcustom org-clock-total-time-cell-format "*%s*"
  "Format string for the total time cells."
  :group 'org-clock
  :version "24.1"
  :type 'string)

(defcustom org-clock-file-time-cell-format "*%s*"
  "Format string for the file time cells."
  :group 'org-clock
  :version "24.1"
  :type 'string)

(defcustom org-clock-clocked-in-display 'mode-line
  "Where to display clocked in task and accumulated time when clocked in.

Allowed values are:

both         displays in both mode line and frame title
mode-line    displays only in mode line (default)
frame-title  displays only in frame title
nil          current clock is not displayed"
  :group 'org-clock
  :type '(choice
	  (const :tag "Mode line" mode-line)
	  (const :tag "Frame title" frame-title)
	  (const :tag "Both" both)
	  (const :tag "None" nil)))

(defcustom org-clock-frame-title-format '(t org-mode-line-string)
  "The value for `frame-title-format' when clocking in.

When `org-clock-clocked-in-display' is set to `frame-title'
or `both', clocking in will replace `frame-title-format' with
this value.  Clocking out will restore `frame-title-format'.

This uses the same format as `frame-title-format', which see."
  :version "24.1"
  :group 'org-clock
  :type 'sexp)

(defcustom org-clock-goto-before-context 2
  "Number of lines of context to display before currently clocked-in entry.
This applies when using `org-clock-goto'."
  :group 'org-clock
  :type 'integer)

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

(defcustom org-clock-auto-clockout-timer nil
  "Timer for auto clocking out when Emacs is idle.
When set to a number, auto clock out the currently clocked in
task after this number of seconds of idle time.

This is only effective when `org-clock-auto-clockout-insinuate'
is added to the user configuration."
  :group 'org-clock
  :package-version '(Org . "9.4")
  :type '(choice
	  (integer :tag "Clock out after Emacs is idle for X seconds")
	  (const :tag "Never auto clock out" nil)))

(defcustom org-clock-ask-before-exiting t
  "If non-nil, ask if the user wants to clock out before exiting Emacs.
This variable only has effect if set with \\[customize]."
  :set (lambda (symbol value)
         (if value
             (add-hook 'kill-emacs-query-functions #'org-clock-kill-emacs-query)
           (remove-hook 'kill-emacs-query-functions #'org-clock-kill-emacs-query))
         (set-default-toplevel-value symbol value))
  :type 'boolean
  :package-version '(Org . "9.5"))


(defcustom org-log-note-clock-out nil
  "Non-nil means record a note when clocking out of an item.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: lognoteclock-out
   #+STARTUP: nolognoteclock-out"
  :group 'org-todo
  :group 'org-progress
  :type 'boolean)

(defvar org-clock-in-prepare-hook nil
  "Hook run when preparing the clock.
This hook is run before anything happens to the task that
you want to clock in.  For example, you can use this hook
to add an effort property.")
(defvar org-clock-in-hook nil
  "Hook run when starting the clock.")
(defvar org-clock-out-hook nil
  "Hook run when stopping the current clock.
The point is at the current clock line when the hook is executed.

The hook functions can access `org-clock-out-removed-last-clock' to
check whether the latest CLOCK line has been cleared.")

(defvar org-clock-cancel-hook nil
  "Hook run when canceling the current clock.")

;;; The clock for measuring work time.

(defvar org-mode-line-string "")
(put 'org-mode-line-string 'risky-local-variable t)

(defvar org-clock-mode-line-timer nil)
(defvar org-clock-idle-timer nil)

(defvar org-clock-leftover-time nil
  "If non-nil, user canceled a clock; this is when leftover time started.")

(defvar org-clock-mode-line-map (make-sparse-keymap))
(declare-function org-clock-goto "org-clock-commands" (&optional select))
(define-key org-clock-mode-line-map [mode-line mouse-2] #'org-clock-goto)
(define-key org-clock-mode-line-map [mode-line mouse-1] #'org-clock-menu)

(defun org-clock--mode-line-heading ()
  "Return currently clocked heading, formatted for mode line."
  (cond ((functionp org-clock-heading-function)
	 (funcall org-clock-heading-function))
	((org-before-first-heading-p) "???")
	(t (org-link-display-format
	    (org-no-properties (org-get-heading t t t t))))))

(defun org-clock-menu ()
  "Pop up org-clock menu."
  (interactive)
  (popup-menu
   '("Clock"
     ["Clock out" org-clock-out t]
     ["Change effort estimate" org-clock-modify-effort-estimate t]
     ["Go to clock entry" org-clock-goto t]
     ["Switch task" (lambda () (interactive) (org-clock-in '(4))) :active t :keys "C-u C-c C-x C-i"])))

(defun org-clock-history-push (&optional pos buffer)
  "Push point marker to the clock history.
When POS is provided, use it as marker point.
When BUFFER and POS are provided, use marker at POS in base buffer of
BUFFER."
  ;; When buffer is provided, POS must be provided.
  (cl-assert (or (not buffer) pos))
  (setq org-clock-history-length (max 1 org-clock-history-length))
  (let ((m (move-marker (make-marker)
			(or pos (point)) (org-base-buffer
					  (or buffer (current-buffer)))))
	n l)
    (while (setq n (member m org-clock-history))
      (move-marker (car n) nil))
    (setq org-clock-history
	  (delq nil
		(mapcar (lambda (x) (if (marker-buffer x) x nil))
			org-clock-history)))
    (when (>= (setq l (length org-clock-history)) org-clock-history-length)
      (setq org-clock-history
	    (nreverse
	     (nthcdr (- l org-clock-history-length -1)
		     (nreverse org-clock-history)))))
    (push m org-clock-history)))

(defun org-clock-drawer-name ()
  "Return clock drawer's name for current entry, or nil."
  (let ((drawer (org-clock-into-drawer)))
    (cond ((integerp drawer)
	   (let ((log-drawer (org-log-into-drawer)))
	     (if (stringp log-drawer) log-drawer "LOGBOOK")))
	  ((stringp drawer) drawer)
	  (t nil))))

(defvar org-clock-before-select-task-hook nil
  "Hook called in task selection just before prompting the user.")

(defun org-clock-select-task (&optional prompt)
  "Select a task that was recently associated with clocking.
PROMPT is the prompt text to be used, as a string.
Return marker position of the selected task.  Raise an error if
there is no recent clock to choose from."
  (let (och chl sel-list rpl (i 0) s)
    ;; Remove successive dups from the clock history to consider
    (dolist (c org-clock-history)
      (unless (equal c (car och)) (push c och)))
    (setq och (reverse och) chl (length och))
    (if (zerop chl)
	(user-error "No recent clock")
      (save-window-excursion
	(switch-to-buffer-other-window
	 (get-buffer-create "*Clock Task Select*"))
	(erase-buffer)
	(when (marker-buffer org-clock-default-task)
	  (insert (org-add-props "Default Task\n" nil 'face 'bold))
	  (setq s (org-clock-insert-selection-line ?d org-clock-default-task))
	  (push s sel-list))
	(when (marker-buffer org-clock-interrupted-task)
	  (insert (org-add-props "The task interrupted by starting the last one\n" nil 'face 'bold))
	  (setq s (org-clock-insert-selection-line ?i org-clock-interrupted-task))
	  (push s sel-list))
	(when (org-clocking-p)
	  (insert (org-add-props "Current Clocking Task\n" nil 'face 'bold))
	  (setq s (org-clock-insert-selection-line ?c org-clock-marker))
	  (push s sel-list))
	(insert (org-add-props "Recent Tasks\n" nil 'face 'bold))
	(dolist (m och)
	  (when (marker-buffer m)
	    (setq i (1+ i)
		  s (org-clock-insert-selection-line
		     (if (< i 10)
			 (+ i ?0)
		       (+ i (- ?A 10))) m))
	    (push s sel-list)))
	(run-hooks 'org-clock-before-select-task-hook)
	(goto-char (point-min))
	;; Set min-height relatively to circumvent a possible but in
	;; `fit-window-to-buffer'
	(fit-window-to-buffer nil nil (if (< chl 10) chl (+ 5 chl)))
	(message (or prompt "Select task for clocking:"))
	(unwind-protect (setq cursor-type nil rpl (read-char-exclusive))
          (when-let ((window (get-buffer-window "*Clock Task Select*" t)))
            (quit-window 'kill window))
	  (when (get-buffer "*Clock Task Select*")
            (kill-buffer "*Clock Task Select*")))
	(cond
	 ((eq rpl ?q) nil)
	 ((eq rpl ?x) nil)
	 ((assoc rpl sel-list) (cdr (assoc rpl sel-list)))
	 (t (user-error "Invalid task choice %c" rpl)))))))

(declare-function org-fontify-like-in-org-mode "org-font-lock"
                  (s &optional odd-levels))
(defun org-clock-insert-selection-line (i marker)
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the marker
pointing to it."
  (when (marker-buffer marker)
    (require 'org-font-lock)
    (let (cat task heading prefix)
      (with-current-buffer (org-base-buffer (marker-buffer marker))
	(org-with-wide-buffer
	 (ignore-errors
	   (goto-char marker)
	   (setq cat (org-get-category)
		 heading (org-get-heading 'notags)
		 prefix (save-excursion
			  (org-back-to-heading t)
			  (looking-at org-outline-regexp)
			  (match-string 0))
		 task (substring
		       (org-fontify-like-in-org-mode
			(concat prefix heading)
			org-odd-levels-only)
		       (length prefix))))))
      (when (and cat task)
        (if (string-match-p "[[:print:]]" (make-string 1 i))
	    (insert (format "[%c] %-12s  %s\n" i cat task))
          ;; Avoid non-printable characters.
          (insert (format "[N/A] %-12s  %s\n" cat task)))
	(cons i marker)))))

(defvar org-clock-task-overrun nil
  "Internal flag indicating if the clock has overrun the planned time.")
(defvar org-clock-update-period 60
  "Number of seconds between mode line clock string updates.")

(defun org-clock-get-clock-string ()
  "Form a clock-string, that will be shown in the mode line.
If an effort estimate was defined for the current item, use
01:30/01:50 format (clocked/estimated).
If not, show simply the clocked time like 01:50."
  (let ((clocked-time (org-clock-get-clocked-time)))
    (if org-clock-effort
	(let* ((effort-in-minutes (org-duration-to-minutes org-clock-effort))
	       (work-done-str
		(propertize (org-duration-from-minutes clocked-time)
			    'face
			    (if (and org-clock-task-overrun
				     (not org-clock-task-overrun-text))
				'org-mode-line-clock-overrun
			      'org-mode-line-clock)))
	       (effort-str (org-duration-from-minutes effort-in-minutes)))
	  (format (propertize "[%s/%s] (%s) " 'face 'org-mode-line-clock)
		  work-done-str effort-str org-clock-heading))
      (format (propertize "[%s] (%s) " 'face 'org-mode-line-clock)
	      (org-duration-from-minutes clocked-time)
	      org-clock-heading))))

(defun org-clock-get-last-clock-out-time ()
  "Get the last clock-out time for the current subtree."
  (save-excursion
    (let ((end (save-excursion (org-end-of-subtree))))
      (when (re-search-forward (concat org-clock-string
				       ".*\\]--\\(\\[[^]]+\\]\\)")
			       end t)
	(org-time-string-to-time (match-string 1))))))

(defun org-clock-restore-global-mode-string ()
  "Remove `org-mode-line-string' from `global-mode-string'."
  (setq global-mode-string
	(delq 'org-mode-line-string global-mode-string)))

(defun org-clock-set-global-mode-string-maybe ()
  "Add `'org-mode-line-string' to `global-mode-string'.
Do nothing when `org-clock-clocked-in-display' does not specify mode
line."
  (when (or (eq org-clock-clocked-in-display 'mode-line)
	    (eq org-clock-clocked-in-display 'both))
    (or global-mode-string (setq global-mode-string '("")))
    (or (memq 'org-mode-line-string global-mode-string)
	(setq global-mode-string
	      (append global-mode-string '(org-mode-line-string))))))

(defvar org-clock-notification-was-shown nil
  "Shows if we have shown notification already.")

(defalias 'org-clock-update-clock-status #'org-clock-update-mode-line)
(defun org-clock-update-mode-line (&optional refresh)
  "Update mode line/frame title with clock information.
When optional argument REFRESH is non-nil, refresh cached heading.
When REFRESH is symbol `restore', restore mode line/frame title."
  (if org-clock-effort
      (org-clock-notify-once-if-expired)
    (setq org-clock-task-overrun nil))
  (when (and refresh (not (eq refresh 'restore)))
    (setq org-clock-heading (org-clock--mode-line-heading))
    ;; Reset `org-clock-notify-once-if-expired'.
    (setq org-clock-notification-was-shown nil)
    ;; Set mode line and frame title.
    (org-clock-set-frame-title-format-maybe)
    (org-clock-set-global-mode-string-maybe)
    ;; Reset timer.
    (when org-clock-mode-line-timer
      (cancel-timer org-clock-mode-line-timer)
      (setq org-clock-mode-line-timer nil))
    (when org-clock-clocked-in-display
      (setq org-clock-mode-line-timer
	    (run-with-timer org-clock-update-period
			    org-clock-update-period
			    #'org-clock-update-clock-status))))
  (when (eq refresh 'restore)
    (org-clock-restore-frame-title-format)
    (org-clock-restore-global-mode-string)
    ;; Cancel timer.
    (when org-clock-mode-line-timer
      (cancel-timer org-clock-mode-line-timer)
      (setq org-clock-mode-line-timer nil)))
  
  (setq org-mode-line-string
	(propertize
	 (let ((clock-string (org-clock-get-clock-string))
	       (help-text "Org mode clock is running.\nmouse-1 shows a \
menu\nmouse-2 will jump to task"))
	   (if (and (> org-clock-string-limit 0)
		    (> (length clock-string) org-clock-string-limit))
	       (propertize
		(substring clock-string 0 org-clock-string-limit)
		'help-echo (concat help-text ": " org-clock-heading))
	     (propertize clock-string 'help-echo help-text)))
	 'local-map org-clock-mode-line-map
	 'mouse-face 'mode-line-highlight))
  (if (and org-clock-task-overrun org-clock-task-overrun-text)
      (setq org-mode-line-string
	    (concat (propertize
		     org-clock-task-overrun-text
		     'face 'org-mode-line-clock-overrun)
		    org-mode-line-string)))
  (force-mode-line-update))

(defun org-clock-get-clocked-time ()
  "Get the clocked time for the current item in minutes.
The time returned includes the time spent on this task in
previous clocking intervals."
  (let ((currently-clocked-time
	 (floor (org-time-convert-to-integer
		 (time-since org-clock-start-time))
		60)))
    (+ currently-clocked-time (or org-clock-total-time 0))))

(defun org-clock-notify-once-if-expired ()
  "Show notification if we spent more time than we estimated before.
Notification is shown only once."
  (when (org-clocking-p)
    (let ((effort-in-minutes (org-duration-to-minutes org-clock-effort))
	  (clocked-time (org-clock-get-clocked-time)))
      (if (setq org-clock-task-overrun
		(if (or (null effort-in-minutes) (zerop effort-in-minutes))
		    nil
		  (>= clocked-time effort-in-minutes)))
	  (unless org-clock-notification-was-shown
	    (setq org-clock-notification-was-shown t)
	    (org-notify
	     (format-message "Task `%s' should be finished by now. (%s)"
                             org-clock-heading org-clock-effort)
             org-clock-sound))
	(setq org-clock-notification-was-shown nil)))))

(defvar org-clock-mode-line-entry nil
  "Information for the mode line about the running clock.")

(defun org-find-open-clocks (file)
  "Search through the given file and find all open clocks."
  (let ((buf (or (get-file-buffer file)
		 (find-file-noselect file)))
	(org-clock-re (concat org-clock-string " \\(\\[.*?\\]\\)$"))
	clocks)
    (with-current-buffer buf
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward org-clock-re nil t)
          (when (save-match-data
                  (org-element-type-p (org-element-at-point) 'clock))
	    (push (cons (copy-marker (match-end 1) t)
		        (org-time-string-to-time (match-string 1)))
		  clocks)))))
    clocks))

(defsubst org-is-active-clock (clock)
  "Return t if CLOCK is the currently active clock."
  (and (org-clock-is-active)
       (= org-clock-marker (car clock))))

(defmacro org-with-clock-position (clock &rest forms)
  "Evaluate FORMS with CLOCK as the current active clock."
  (declare (indent 1) (debug t))
  `(with-current-buffer (marker-buffer (car ,clock))
     (org-with-wide-buffer
      (goto-char (car ,clock))
      (forward-line 0)
      ,@forms)))

(defmacro org-with-clock (clock &rest forms)
  "Evaluate FORMS with CLOCK as the current active clock.
This macro also protects the current active clock from being altered."
  (declare (indent 1) (debug t))
  `(org-with-clock-position ,clock
     (let ((org-clock-start-time (cdr ,clock))
	   (org-clock-total-time)
	   (org-clock-history)
	   (org-clock-effort)
	   (org-clock-marker (car ,clock))
	   (org-clock-hd-marker (save-excursion
				  (org-back-to-heading t)
				  (point-marker))))
       ,@forms)))

(defsubst org-clock-clock-in (clock &optional resume start-time)
  "Clock in to the clock located by CLOCK.
If necessary, clock-out of the currently active clock."
  (org-with-clock-position clock
    (let ((org-clock-in-resume (or resume org-clock-in-resume)))
      (org-clock-in nil start-time))))

(defsubst org-clock-clock-out (clock &optional fail-quietly at-time)
  "Clock out of the clock located by CLOCK."
  (let ((temp (copy-marker (car clock)
			   (marker-insertion-type (car clock)))))
    (if (org-is-active-clock clock)
	(org-clock-out nil fail-quietly at-time)
      (org-with-clock clock
	(org-clock-out nil fail-quietly at-time)))
    (setcar clock temp)))

(defsubst org-clock-clock-cancel (clock)
  "Cancel the clock located by CLOCK."
  (let ((temp (copy-marker (car clock)
			   (marker-insertion-type (car clock)))))
    (if (org-is-active-clock clock)
	(org-clock-cancel)
      (org-with-clock clock
	(org-clock-cancel)))
    (setcar clock temp)))

(defvar org-clock-clocking-in nil)
(defvar org-clock-resolving-clocks nil)
(defvar org-clock-resolving-clocks-due-to-idleness nil)

(defun org-clock-resolve-clock
    (clock resolve-to clock-out-time close restart fail-quietly)
  "Resolve CLOCK given the time RESOLVE-TO, and the present.
CLOCK is a cons cell of the form (MARKER START-TIME)."
  (let ((org-clock-resolving-clocks t)
	;; If the clocked entry contained only a clock and possibly
	;; the associated drawer, and we either cancel it or clock it
	;; out, `org-clock-out-remove-zero-time-clocks' may clear all
	;; contents, and leave point on the /next/ headline.  We store
	;; the current entry location to be able to get back here when
	;; we need to clock in again the previously clocked task.
	(heading (org-with-point-at (car clock)
		   (org-back-to-heading t)
		   (point-marker))))
    (pcase resolve-to
      (`nil
       (org-clock-clock-cancel clock)
       (when (and restart (not org-clock-clocking-in))
	 (org-with-point-at heading (org-clock-in))))
      (`now
       (cond
	(restart (error "RESTART is not valid here"))
	((or close org-clock-clocking-in)
	 (org-clock-clock-out clock fail-quietly))
	((org-is-active-clock clock) nil)
	(t (org-clock-clock-in clock t))))
      ((pred (time-less-p nil))
       (error "RESOLVE-TO must refer to a time in the past"))
      (_
       (when restart (error "RESTART is not valid here"))
       (org-clock-clock-out clock fail-quietly (or clock-out-time resolve-to))
       (cond
	(org-clock-clocking-in nil)
	(close
	 (setq org-clock-leftover-time (and (null clock-out-time) resolve-to)))
	(t
	 (org-with-point-at heading
	   (org-clock-in nil (and clock-out-time resolve-to)))))))))

(defun org-clock-jump-to-current-clock (&optional effective-clock)
  "When an Org clock is running, jump to it."
  (let ((drawer (org-clock-into-drawer))
	(clock (or effective-clock (cons org-clock-marker
					 org-clock-start-time))))
    (unless (marker-buffer (car clock))
      (user-error "No Org clock is currently running"))
    (org-with-clock clock (org-clock-goto))
    (with-current-buffer (marker-buffer (car clock))
      (goto-char (car clock))
      (when drawer
	(org-with-wide-buffer
	 (let ((drawer-re (format "^[ \t]*:%s:[ \t]*$"
				  (regexp-quote (if (stringp drawer) drawer "LOGBOOK"))))
	       (beg (save-excursion (org-back-to-heading t) (point))))
	   (catch 'exit
	     (while (re-search-backward drawer-re beg t)
	       (let ((element (org-element-at-point)))
		 (when (org-element-type-p element 'drawer)
		   (when (> (org-element-end element) (car clock))
		     (org-fold-hide-drawer-toggle 'off nil element))
		   (throw 'exit nil)))))))))))

(declare-function org-read-date "org-read-date"
                  (&optional with-time to-time from-string prompt
			     default-time default-input inactive))
(defun org-clock-resolve (clock &optional prompt-fn last-valid fail-quietly)
  "Resolve an open Org clock.
An open clock was found, with `dangling' possibly being non-nil.
If this function was invoked with a prefix argument, non-dangling
open clocks are ignored.  The given clock requires some sort of
user intervention to resolve it, either because a clock was left
dangling or due to an idle timeout.  The clock resolution can
either be:

  (a) deleted, the user doesn't care about the clock
  (b) restarted from the current time (if no other clock is open)
  (c) closed, giving the clock X minutes
  (d) closed and then restarted
  (e) resumed, as if the user had never left

The format of clock is (CONS MARKER START-TIME), where MARKER
identifies the buffer and position the clock is open at (and
thus, the heading it's under), and START-TIME is when the clock
was started."
  (cl-assert clock)
  (let* ((ch
	  (save-window-excursion
	    (save-excursion
	      (unless org-clock-resolving-clocks-due-to-idleness
		(org-clock-jump-to-current-clock clock))
	      (unless org-clock-resolve-expert
		(with-output-to-temp-buffer "*Org Clock*"
		  (princ (format-message "Select a Clock Resolution Command:

i/q      Ignore this question; the same as keeping all the idle time.

k/K      Keep X minutes of the idle time (default is all).  If this
         amount is less than the default, you will be clocked out
         that many minutes after the time that idling began, and then
         clocked back in at the present time.

t/T      Like `k', but will ask you to specify a time (when you got
         distracted away), instead of a number of minutes.

g/G      Indicate that you \"got back\" X minutes ago.  This is quite
         different from `k': it clocks you out from the beginning of
         the idle period and clock you back in X minutes ago.

s/S      Subtract the idle time from the current clock.  This is the
         same as keeping 0 minutes.

C        Cancel the open timer altogether.  It will be as though you
         never clocked in.

j/J      Jump to the current clock, to make manual adjustments.

For all these options, using uppercase makes your final state
to be CLOCKED OUT."))))
	      (org-fit-window-to-buffer (get-buffer-window "*Org Clock*"))
	      (let (char-pressed)
		(while (or (null char-pressed)
			   (and (not (memq char-pressed
					   '(?k ?K ?g ?G ?s ?S ?C
					        ?j ?J ?i ?q ?t ?T)))
				(or (ding) t)))
		  (setq char-pressed
			(read-char-exclusive (concat (funcall prompt-fn clock)
					             " [jkKtTgGSscCiq]? ")
				             nil 45)))
		(and (not (memq char-pressed '(?i ?q))) char-pressed)))))
	 (default
	  (floor (org-time-convert-to-integer (time-since last-valid))
		 60))
	 (keep
	  (or (and (memq ch '(?k ?K))
		   (read-number "Keep how many minutes: " default))
	      (and (memq ch '(?t ?T))
                   (require 'org-read-date)
		   (floor
		    (/ (float-time
			(time-subtract (org-read-date t t) last-valid))
		       60)))))
	 (gotback
	  (and (memq ch '(?g ?G))
	       (read-number "Got back how many minutes ago: " default)))
	 (subtractp (memq ch '(?s ?S)))
	 (barely-started-p (time-less-p
			    (time-subtract last-valid (cdr clock))
			    45))
	 (start-over (and subtractp barely-started-p)))
    (cond
     ((memq ch '(?j ?J))
      (if (eq ch ?J)
	  (org-clock-resolve-clock clock 'now nil t nil fail-quietly))
      (org-clock-jump-to-current-clock clock))
     ((or (null ch)
	  (not (memq ch '(?k ?K ?g ?G ?s ?S ?C ?t ?T))))
      (message ""))
     (t
      (org-clock-resolve-clock
       clock (cond
	      ((or (eq ch ?C)
		   ;; If the time on the clock was less than a minute before
		   ;; the user went away, and they've ask to subtract all the
		   ;; time...
		   start-over)
	       nil)
	      ((or subtractp
		   (and gotback (= gotback 0)))
	       last-valid)
	      ((or (and keep (= keep default))
		   (and gotback (= gotback default)))
	       'now)
	      (keep
	       (time-add last-valid (* 60 keep)))
	      (gotback
	       (time-since (* 60 gotback)))
	      (t
	       (error "Unexpected, please report this as a bug")))
       (and gotback last-valid)
       (memq ch '(?K ?G ?S ?T))
       (and start-over
	    (not (memq ch '(?K ?G ?S ?C))))
       fail-quietly)))))

;;;###autoload
(defun org-resolve-clocks (&optional only-dangling-p prompt-fn last-valid)
  "Resolve all currently open Org clocks.
If `only-dangling-p' is non-nil, only ask to resolve dangling
\(i.e., not currently open and valid) clocks."
  (interactive "P")
  (unless org-clock-resolving-clocks
    (let ((org-clock-resolving-clocks t))
      (dolist (file (org-files-list))
	(let ((clocks (org-find-open-clocks file)))
	  (dolist (clock clocks)
	    (let ((dangling (or (not (org-clock-is-active))
				(/= (car clock) org-clock-marker))))
	      (if (or (not only-dangling-p) dangling)
		  (org-clock-resolve
		   clock
		   (or prompt-fn
		       (lambda (clock)
			 (format
			  "Dangling clock started %d mins ago"
			  (floor (org-time-convert-to-integer
				  (time-since (cdr clock)))
				 60))))
		   (or last-valid
		       (cdr clock)))))))))))

(defvar org-clock-user-idle-seconds)

(defun org-resolve-clocks-if-idle ()
  "Resolve all currently open Org clocks.
This is performed after `org-clock-idle-time' minutes, to check
if the user really wants to stay clocked in after being idle for
so long."
  (when (and org-clock-idle-time (not org-clock-resolving-clocks)
	     org-clock-marker (marker-buffer org-clock-marker))
    (let* ((org-clock-user-idle-seconds (org-user-idle-seconds))
	   (org-clock-user-idle-start
	    (time-since org-clock-user-idle-seconds))
	   (org-clock-resolving-clocks-due-to-idleness t))
      (when (> org-clock-user-idle-seconds (* 60 org-clock-idle-time))
        (org-clock-idle-update 'cancel)
	(org-clock-resolve
	 (cons org-clock-marker
	       org-clock-start-time)
	 (lambda (_)
	   (format "Clocked in & idle for %.1f mins"
		   (/ (float-time
		       (time-since org-clock-user-idle-start))
		      60)))
	 org-clock-user-idle-start)
        (when (org-clocking-p) (org-clock-idle-update 'start))))))

(defun org-clock-resolve-clock-maybe ()
  "Resolve unclosed clocks if appropriate.
Clock resolution is done according to
`org-clock-auto-clock-resolution'."
  (when (and org-clock-auto-clock-resolution
	     (or (not (org-clocking-p))
                 org-clock-resolving-clocks-due-to-idleness
	         (eq t org-clock-auto-clock-resolution))
	     (not org-clock-clocking-in)
	     (not org-clock-resolving-clocks))
    (setq org-clock-leftover-time nil)
    (let ((org-clock-clocking-in t))
      (org-resolve-clocks))))

(defun org-clock-idle-update (status)
  "Refresh idle tracking of Org clock.
STATUS may be symbol `start' or `cancel' - to start, or cancel idle
time tracking."
  (pcase status
    (`start
     (org-clock-idle-update 'cancel)
     (setq org-clock-idle-timer
	   (run-with-timer 60 60 #'org-resolve-clocks-if-idle)))
    (`cancel
     (when org-clock-idle-timer
       (cancel-timer org-clock-idle-timer)
       (setq org-clock-idle-timer nil)))
    (_ (error "Unknown argument: %S" status))))

(defvar org--msg-extra)

(declare-function org-in-item-p "org-list-core" ())
(declare-function org-todo "org-todo" (&optional arg))
;;;###autoload
(defun org-clock-in (&optional select start-time)
  "Start the clock on the current item.

If necessary, clock-out of the currently active clock.

With a `\\[universal-argument]' prefix argument SELECT, offer a list of \
recently clocked
tasks to clock into.

When SELECT is `\\[universal-argument] \ \\[universal-argument]', \
clock into the current task and mark it as
the default task, a special task that will always be offered in the
clocking selection, associated with the letter `d'.

When SELECT is `\\[universal-argument] \\[universal-argument] \
\\[universal-argument]', clock in by using the last clock-out
time as the start time.  See `org-clock-continuously' to make this
the default behavior."
  (interactive "P")
  (catch 'abort

    (org-clock-resolve-clock-maybe); check if any clocks are dangling

    (let ((interrupting (and (not org-clock-resolving-clocks-due-to-idleness)
			     (org-clocking-p)))
	  ts selected-task target-pos (org--msg-extra "")
	  (leftover (and (not org-clock-resolving-clocks)
			 org-clock-leftover-time)))

      (when (equal select '(64))
	;; Set start-time to `org-clock-out-time'
	(let ((org-clock-continuously t))
	  (org-clock-in nil org-clock-out-time)
	  (throw 'abort nil)))

      (when (equal select '(4))
	(pcase (org-clock-select-task "Clock-in on task: ")
	  (`nil (error "Abort"))
	  (task (setq selected-task (copy-marker task)))))

      (when (equal select '(16))
	;; Mark as default clocking task
	(org-clock-mark-default-task))

      (when interrupting
	;; We are interrupting the clocking of a different task.  Save
	;; a marker to this task, so that we can go back.  First check
	;; if we are trying to clock into the same task!
	(when (or selected-task (derived-mode-p 'org-mode))
	  (org-with-point-at selected-task
	    (unless selected-task (org-back-to-heading t))
	    (when (and (eq (marker-buffer org-clock-hd-marker)
			   (org-base-buffer (current-buffer)))
		       (= (point) (marker-position org-clock-hd-marker))
		       (equal org-clock-current-task (org-get-heading t t t t)))
	      (message "Clock continues in %S" org-clock-heading)
	      (throw 'abort nil))))
	(move-marker org-clock-interrupted-task
		     (marker-position org-clock-marker)
		     (marker-buffer org-clock-marker))
	(let ((org-clock-clocking-in t))
	  (org-clock-out nil t)))

      ;; Clock in at which position?
      (setq target-pos
	    (if (and (eobp) (not (org-at-heading-p)))
		(org-with-wide-buffer (line-beginning-position 0))
	      (point)))
      (save-excursion
	(when (and selected-task (marker-buffer selected-task))
	  ;; There is a selected task, move to the correct buffer
	  ;; and set the new target position.
	  (set-buffer (org-base-buffer (marker-buffer selected-task)))
	  (setq target-pos (marker-position selected-task))
	  (move-marker selected-task nil))
	(org-with-wide-buffer
	 (goto-char target-pos)
	 (org-back-to-heading t)
	 (or interrupting (move-marker org-clock-interrupted-task nil))
	 (run-hooks 'org-clock-in-prepare-hook)
	 (org-clock-history-push)
	 (setq org-clock-current-task (org-get-heading t t t t))
	 (cond ((functionp org-clock-in-switch-to-state)
		(let ((case-fold-search nil))
		  (looking-at org-complex-heading-regexp))
		(let ((newstate (funcall org-clock-in-switch-to-state
					 (match-string 2))))
		  (when newstate
                    (require 'org-todo)
                    (org-todo newstate))))
	       ((and org-clock-in-switch-to-state
		     (not (looking-at (concat org-outline-regexp "[ \t]*"
					    org-clock-in-switch-to-state
					    "\\(?:[ \t]\\|$\\)"))))
		(org-todo org-clock-in-switch-to-state)))
	 (org-clock-update-clock-status)
	 (org-clock-find-position org-clock-in-resume)
	 (cond
	  ((and org-clock-in-resume
		(looking-at org-clock-running-re))
	   (message "Matched %s" (match-string 1))
	   (setq ts (concat "[" (match-string 1) "]"))
	   (goto-char (match-end 1))
	   (setq org-clock-start-time
		 (org-time-string-to-time (match-string 1)))
	   (setq org-clock-effort (org-entry-get (point) org-effort-property))
	   (setq org-clock-total-time (org-clock-sum-current-item
				       (org-clock-get-sum-start))))
	  ((eq org-clock-in-resume 'auto-restart)
	   ;; called from org-clock-load during startup,
	   ;; do not interrupt, but warn!
	   (message "Cannot restart clock because task does not contain unfinished clock")
	   (ding)
	   (sit-for 2)
	   (throw 'abort nil))
	  (t
           ;; Make sure that point moves after clock line upon
           ;; inserting it.  Then, users can continue typing even if
           ;; point was right where the clock is inserted.
	   (insert-before-markers-and-inherit "\n")
	   (backward-char 1)
           (require 'org-list-core)
	   (when (and (save-excursion
			(end-of-line 0)
			(org-in-item-p)))
	     (forward-line 0)
	     (indent-line-to (max 0 (- (current-indentation) 2))))
	   (insert-and-inherit org-clock-string " ")
	   (setq org-clock-effort (org-entry-get (point) org-effort-property))
	   (setq org-clock-total-time (org-clock-sum-current-item
				       (org-clock-get-sum-start)))
	   (setq org-clock-start-time
		 (or (and org-clock-continuously org-clock-out-time)
		     (and leftover
			  (y-or-n-p
			   (format
			    "You stopped another clock %d mins ago; start this one from then? "
			    (/ (org-time-convert-to-integer
				(time-subtract
				 (org-current-time org-clock-rounding-minutes t)
				 leftover))
			       60)))
			  leftover)
		     start-time
		     (org-current-time org-clock-rounding-minutes t)))
	   (setq ts (org-insert-timestamp org-clock-start-time
					  'with-hm 'inactive))
	   (org-indent-line)))
	 (move-marker org-clock-marker (point) (buffer-base-buffer))
	 (move-marker org-clock-hd-marker
		      (save-excursion (org-back-to-heading t) (point))
		      (buffer-base-buffer))
	 (setq org-clock-has-been-used t)
	 ;; add to frame title/mode line
	 (org-clock-update-clock-status t)
         (org-clock-idle-update 'start)
	 (message "Clock starts at %s - %s" ts org--msg-extra)
	 (run-hooks 'org-clock-in-hook))))))

(defvar org-clock--auto-clockout-timer-obj nil
  "Timer object holding the existing clockout timer.")
(defun org-clock--auto-clockout-maybe ()
  "Clock out the currently clocked in task when idle.
See `org-clock-auto-clockout-timer' to set the idle time span.

This function is to be called by a timer."
  (when (and (numberp org-clock-auto-clockout-timer)
	     org-clock-current-task)
    (let ((user-idle-seconds (org-user-idle-seconds)))
      (cond
       ;; Already idle.  Clock out.
       ((>= user-idle-seconds org-clock-auto-clockout-timer)
        (setq org-clock--auto-clockout-timer-obj nil)
        (org-clock-out))
       ;; Emacs is idle but system is not.  Retry assuming that system will remain idle.
       ((>= (org-emacs-idle-seconds) org-clock-auto-clockout-timer)
        (setq org-clock--auto-clockout-timer-obj
              (run-with-timer
               (- org-clock-auto-clockout-timer user-idle-seconds)
               nil #'org-clock--auto-clockout-maybe)))
       ;; Emacs is not idle.  Check again next time we are idle.
       (t
        (setq org-clock--auto-clockout-timer-obj
              (run-with-idle-timer
               org-clock-auto-clockout-timer nil #'org-clock--auto-clockout-maybe)))))))

(defun org-clock-auto-clockout ()
  "Clock out the currently clocked in task if Emacs is idle.
See `org-clock-auto-clockout-timer' to set the idle time span.

This is only effective when `org-clock-auto-clockout-insinuate'
is present in the user configuration."
  (when (and (numberp org-clock-auto-clockout-timer)
	     org-clock-current-task
             (not (timerp org-clock--auto-clockout-timer-obj)))
    (setq org-clock--auto-clockout-timer-obj
          (run-with-idle-timer
           org-clock-auto-clockout-timer nil #'org-clock--auto-clockout-maybe))))

;;;###autoload
(defun org-clock-toggle-auto-clockout ()
  (interactive)
  (if (memq 'org-clock-auto-clockout org-clock-in-hook)
      (progn (remove-hook 'org-clock-in-hook #'org-clock-auto-clockout)
	     (message "Auto clock-out after idle time turned off"))
    (add-hook 'org-clock-in-hook #'org-clock-auto-clockout t)
    (message "Auto clock-out after idle time turned on")))

(defun org-clock-mark-default-task ()
  "Mark current task as default task."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (move-marker org-clock-default-task (point))))

(defun org-clock-get-sum-start ()
  "Return the time from which clock times should be counted.

This is for the currently running clock as it is displayed in the
mode line.  This function looks at the properties LAST_REPEAT and
in particular CLOCK_MODELINE_TOTAL and the corresponding variable
`org-clock-mode-line-total' and then decides which time to use.

The time is always returned as UTC."
  (let ((cmt (or (org-entry-get nil "CLOCK_MODELINE_TOTAL" 'selective)
		 (symbol-name org-clock-mode-line-total)))
	(lr (org-entry-get nil "LAST_REPEAT")))
    (cond
     ((equal cmt "current")
      (setq org--msg-extra "showing time in current clock instance")
      (current-time))
     ((equal cmt "today")
      (setq org--msg-extra "showing today's task time.")
      (let* ((dt (decode-time))
	     (hour (nth 2 dt))
	     (day (nth 3 dt)))
	(if (< hour org-extend-today-until) (setf (nth 3 dt) (1- day)))
	(setf (nth 2 dt) org-extend-today-until)
	(org-encode-time (apply #'list 0 0 (nthcdr 2 dt)))))
     ((or (equal cmt "all")
	  (and (or (not cmt) (equal cmt "auto"))
	       (not lr)))
      (setq org--msg-extra "showing entire task time.")
      nil)
     ((or (equal cmt "repeat")
	  (and (or (not cmt) (equal cmt "auto"))
	       lr))
      (setq org--msg-extra "showing task time since last repeat.")
      (and lr (org-time-string-to-time lr)))
     (t nil))))

(defun org-clock-find-position (find-unclosed)
  "Find the location where the next clock line should be inserted.
When FIND-UNCLOSED is non-nil, first check if there is an unclosed clock
line and position cursor in that line."
  (org-back-to-heading t)
  (catch 'exit
    (let* ((beg (line-beginning-position))
	   (end (save-excursion (outline-next-heading) (point)))
	   (org-clock-into-drawer (org-clock-into-drawer))
	   (drawer (org-clock-drawer-name)))
      ;; Look for a running clock if FIND-UNCLOSED in non-nil.
      (when find-unclosed
	(while (re-search-forward org-clock-running-re end t)
	  (let ((element (org-element-at-point)))
	    (when (and (org-element-type-p element 'clock)
		       (eq (org-element-property :status element) 'running))
	      (forward-line 0)
	      (throw 'exit t)))))
      ;; Look for an existing clock drawer.
      (when drawer
	(goto-char beg)
	(let ((drawer-re (concat "^[ \t]*:" (regexp-quote drawer) ":[ \t]*$")))
	  (while (re-search-forward drawer-re end t)
	    (let ((element (org-element-at-point)))
	      (when (org-element-type-p element 'drawer)
		(let ((cend (org-element-contents-end element)))
		  (if (and (not org-log-states-order-reversed) cend)
		      (goto-char cend)
		    (forward-line))
		  (throw 'exit t)))))))
      (goto-char beg)
      (let ((count 0) positions)
	;; Count the CLOCK lines and store their positions.
	(save-excursion
	  (while (re-search-forward org-clock-line-re end t)
	    (let ((element (org-element-at-point)))
	      (when (org-element-type-p element 'clock)
		(setq positions (cons (line-beginning-position) positions)
		      count (1+ count))))))
	(cond
	 ((null positions)
          (org-fold-core-ignore-modifications
	    ;; Skip planning line and property drawer, if any.
	    (org-end-of-meta-data)
	    (unless (bolp) (insert-before-markers-and-inherit "\n"))
	    ;; Create a new drawer if necessary.
	    (when (and org-clock-into-drawer
		       (or (not (wholenump org-clock-into-drawer))
			   (< org-clock-into-drawer 2)))
	      (let ((beg (point)))
                ;; Make sure that point moves after drawer upon
                ;; inserting it.  Then, users can continue typing even
                ;; if point was right where the clock is inserted.
	        (insert-before-markers-and-inherit ":" drawer ":\n:END:\n")
	        (org-indent-region beg (point))
                (org-fold-region (line-end-position -1) (1- (point)) t 'drawer)
	        (forward-line -1)))))
	 ;; When a clock drawer needs to be created because of the
	 ;; number of clock items or simply if it is missing, collect
	 ;; all clocks in the section and wrap them within the drawer.
	 ((if (wholenump org-clock-into-drawer)
	      (>= (1+ count) org-clock-into-drawer)
	    drawer)
	  ;; Skip planning line and property drawer, if any.
	  (org-end-of-meta-data)
          (org-fold-core-ignore-modifications
	    (let ((beg (point)))
	      (insert-and-inherit
	       (mapconcat
	        (lambda (p)
		  (save-excursion
		    (goto-char p)
		    (org-trim (delete-and-extract-region
			       (save-excursion (skip-chars-backward " \r\t\n")
					       (line-beginning-position 2))
			       (line-beginning-position 2)))))
	        positions "\n")
	       "\n:END:\n")
	      (let ((end (point-marker)))
	        (goto-char beg)
	        (save-excursion (insert-before-markers-and-inherit ":" drawer ":\n"))
	        (org-fold-region (line-end-position) (1- end) t 'outline)
	        (org-indent-region (point) end)
	        (forward-line)
	        (unless org-log-states-order-reversed
		  (goto-char end)
		  (forward-line -2))
	        (set-marker end nil)))))
	 (org-log-states-order-reversed (goto-char (car (last positions))))
	 (t (goto-char (car positions))))))))

(defun org-clock-restore-frame-title-format ()
  "Restore `frame-title-format' from `org-frame-title-format-backup'.
`frame-title-format' is restored if `org-frame-title-format-backup' is not nil
and current `frame-title-format' is equal to `org-clock-frame-title-format'."
  (when (and org-frame-title-format-backup
	     (equal frame-title-format org-clock-frame-title-format))
    (setq frame-title-format org-frame-title-format-backup)))

(defun org-clock-set-frame-title-format-maybe ()
  "Set `frame-title-format' to `org-clock-frame-title-format'.
Do nothing when `org-clock-clocked-in-display' does not specify frame
title."
  (when (or (eq org-clock-clocked-in-display 'frame-title)
	    (eq org-clock-clocked-in-display 'both))
    (setq org-frame-title-format-backup frame-title-format)
    (setq frame-title-format org-clock-frame-title-format)))

(defvar org-clock-out-removed-last-clock nil
  "When non-nil, the last `org-clock-out' removed the clock line.
This can happen when `org-clock-out-remove-zero-time-clocks' is set to
non-nil and the latest clock took 0 minutes.")

;;;###autoload
(defun org-clock-out (&optional switch-to-state fail-quietly at-time)
  "Stop the currently running clock.
Throw an error if there is no running clock and FAIL-QUIETLY is nil.
With a universal prefix, prompt for a state to switch the clocked out task
to, overriding the existing value of `org-clock-out-switch-to-state'."
  (interactive "P")
  (catch 'exit
    (when (not (org-clocking-p))
      (org-clock-update-clock-status 'restore)
      (if fail-quietly (throw 'exit t) (user-error "No active clock")))
    (let ((org-clock-out-switch-to-state
	   (if switch-to-state
	       (completing-read "Switch to state: "
				(with-current-buffer
				    (marker-buffer org-clock-marker)
				  org-todo-keywords-1)
				nil t "DONE")
	     org-clock-out-switch-to-state))
	  (now (org-current-time org-clock-rounding-minutes))
	  ts te s h m remove)
      (setq org-clock-out-time (or at-time now))
      (save-excursion ; Do not replace this with `with-current-buffer'.
	(with-no-warnings (set-buffer (org-clocking-buffer)))
	(save-restriction
	  (widen)
	  (goto-char org-clock-marker)
	  (forward-line 0)
	  (if (and (looking-at (concat "[ \t]*" org-keyword-time-regexp))
		   (equal (match-string 1) org-clock-string))
	      (setq ts (match-string 2))
	    (if fail-quietly (throw 'exit nil) (error "Clock start time is gone")))
	  (goto-char (match-end 0))
	  (delete-region (point) (line-end-position))
          (org-fold-core-ignore-modifications
            (insert-and-inherit "--")
            (setq te (org-insert-timestamp (or at-time now) 'with-hm 'inactive))
            (setq s (org-time-convert-to-integer
	             (time-subtract
	              (org-time-string-to-time te)
	              (org-time-string-to-time ts)))
	          h (floor s 3600)
	          m (floor (mod s 3600) 60))
            (insert-and-inherit " => " (format "%2d:%02d" h m))
            (move-marker org-clock-marker nil)
            (move-marker org-clock-hd-marker nil)
            ;; Possibly remove zero time clocks.
            (when (and org-clock-out-remove-zero-time-clocks
	               (= 0 h m))
              (setq remove t)
              (delete-region (line-beginning-position)
		             (line-beginning-position 2)))
            (org-clock-remove-empty-clock-drawer))
          (org-clock-idle-update 'cancel)
          (org-clock-update-clock-status 'restore)
	  (when org-clock-out-switch-to-state
	    (save-excursion
	      (org-back-to-heading t)
	      (let ((org-clock-out-when-done nil))
		(cond
		 ((functionp org-clock-out-switch-to-state)
		  (let ((case-fold-search nil))
		    (looking-at org-complex-heading-regexp))
		  (let ((newstate (funcall org-clock-out-switch-to-state
					   (match-string 2))))
		    (when newstate (org-todo newstate))))
		 ((and org-clock-out-switch-to-state
		       (not (looking-at
                             (concat
                              org-outline-regexp "[ \t]*"
			      org-clock-out-switch-to-state
			      "\\(?:[ \t]\\|$\\)"))))
		  (org-todo org-clock-out-switch-to-state))))))
	  (force-mode-line-update)
	  (message (if remove
		       "Clock stopped at %s after %s => LINE REMOVED"
		     "Clock stopped at %s after %s")
		   te (org-duration-from-minutes (+ (* 60 h) m)))
          (unless (org-clocking-p)
	    (setq org-clock-current-task nil))
          (setq org-clock-out-removed-last-clock remove)
          (run-hooks 'org-clock-out-hook)
          ;; Add a note, but only if we didn't remove the clock line.
          (when (and org-log-note-clock-out (not remove))
            (org-add-log-setup
	     'clock-out nil nil nil
	     (concat "# Task: " (org-get-heading t) "\n\n"))))))))

(defun org-clock-remove-empty-clock-drawer ()
  "Remove empty clock drawers in current subtree."
  (save-excursion
    (org-back-to-heading t)
    (org-map-tree
     (lambda ()
       (let ((drawer (org-clock-drawer-name))
	     (case-fold-search t))
	 (when drawer
	   (let ((re (format "^[ \t]*:%s:[ \t]*$" (regexp-quote drawer)))
		 (end (save-excursion (outline-next-heading))))
	     (while (re-search-forward re end t)
	       (org-remove-empty-drawer-at (point))))))))))

(defun org-clock-timestamps-up (&optional n)
  "Increase CLOCK timestamps at cursor.
Optional argument N tells to change by that many units."
  (interactive "P")
  (org-clock-timestamps-change 'up n))

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

;;;###autoload
(defun org-clock-cancel ()
  "Cancel the running clock by removing the start timestamp."
  (interactive)
  (when (not (org-clocking-p))
    (org-clock-update-clock-status 'restore)
    (user-error "No active clock"))
  (save-excursion    ; Do not replace this with `with-current-buffer'.
    (with-no-warnings (set-buffer (org-clocking-buffer)))
    (goto-char org-clock-marker)
    (if (org-element-type-p (org-element-at-point) 'clock)
        (progn (delete-region (1- (line-beginning-position)) (line-end-position))
	       (org-remove-empty-drawer-at (point)))
      (message "Clock gone, cancel the timer anyway")
      (sit-for 2)))
  (move-marker org-clock-marker nil)
  (move-marker org-clock-hd-marker nil)
  (setq org-clock-current-task nil)
  (org-clock-update-clock-status 'restore)
  (message "Clock canceled")
  (run-hooks 'org-clock-cancel-hook))

;;;###autoload
(defun org-clock-out-if-current ()
  "Clock out if the current entry contains the running clock.
This is used to stop the clock after a TODO entry is marked DONE,
and is only done if the variable `org-clock-out-when-done' is not nil."
  (when (and (org-clocking-p)
	     org-clock-out-when-done
	     (marker-buffer org-clock-marker)
	     (or (and (eq t org-clock-out-when-done)
		      (member org-state org-done-keywords))
		 (and (listp org-clock-out-when-done)
		      (member org-state org-clock-out-when-done)))
	     (equal (or (buffer-base-buffer (org-clocking-buffer))
			(org-clocking-buffer))
		    (or (buffer-base-buffer (current-buffer))
			(current-buffer)))
	     (< (point) org-clock-marker)
	     (> (org-with-wide-buffer (org-entry-end-position))
		org-clock-marker))
    ;; Clock out, but don't accept a logging message for this.
    (let ((org-log-note-clock-out nil)
	  (org-clock-out-switch-to-state nil))
      (org-clock-out))))


;; Saving and loading the clock

(defun org-clock-kill-emacs-query ()
  "Query user when killing Emacs.
This function is added to `kill-emacs-query-functions'."
  (let ((buf (org-clocking-buffer)))
    (when (and buf (yes-or-no-p "Clock out and save? "))
      (with-current-buffer buf
        (org-clock-out)
        (save-buffer))))
  ;; Unconditionally return t for `kill-emacs-query-functions'.
  t)

(provide 'org-clock-core)

;;; org-clock-core.el ends here
