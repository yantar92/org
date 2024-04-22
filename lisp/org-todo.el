;;; org-todo.el --- Org todo commands                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
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

;; This library implements commands used to set and change todo
;; states.

;;; Code:

(require 'org-macs)
(org-assert-version)

(declare-function org-agenda-todo-yesterday "org-agenda")
(require 'org-time)
(require 'org-compat)
(defvar org-loop-over-headlines-in-active-region)
(require 'org-map)
(defvar org-todo-kwd-alist)
(defvar org-todo-keywords-1)
(defvar org-done-keywords)
(defvar org-todo-heads)
(defvar org-todo-key-trigger)
(defvar org-todo-sets)
(defvar org-closed-keep-when-no-todo)
(require 'org-log-note)
(declare-function org-clock-out-if-current "org-clock")
(declare-function org-entry-end-position "org")
(defvar org-todo-key-alist)
(require 'org-time)
(require 'org-planning)

(defvar org-inhibit-blocking nil)       ; Dynamically-scoped param.
(defvar org-inhibit-logging nil)        ; Dynamically-scoped param.
(defvar org-last-state)

(defgroup org-todo nil
  "Options concerning TODO items in Org mode."
  :tag "Org TODO"
  :group 'org)

(defcustom org-use-fast-todo-selection 'auto
  "\\<org-mode-map>\
Non-nil means use the fast todo selection scheme with `\\[org-todo]'.
This variable describes if and under what circumstances the cycling
mechanism for TODO keywords will be replaced by a single-key, direct
selection scheme, where the choices are displayed in a little window.

When nil, fast selection is never used.  This means that the command
will always switch to the next state.

When it is the symbol `auto', fast selection is whenever selection
keys have been defined.

`expert' is like `auto', but no special window with the keyword
will be shown, choices will only be listed in the prompt.

In all cases, the special interface is only used if access keys have
actually been assigned by the user, i.e. if keywords in the configuration
are followed by a letter in parenthesis, like TODO(t)."
  :group 'org-todo
  :set (lambda (var val)
	 (cond
	  ((eq var t) (set-default-toplevel-value var 'auto))
	  ((eq var 'prefix) (set-default-toplevel-value var nil))
	  (t (set-default-toplevel-value var val))))
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Automatically, when key letter have been defined" auto)
	  (const :tag "Automatically, but don't show the selection window" expert)))

(defcustom org-provide-todo-statistics t
  "Non-nil means update todo statistics after insert and toggle.
ALL-HEADLINES means update todo statistics by including headlines
with no TODO keyword as well, counting them as not done.
A list of TODO keywords means the same, but skip keywords that are
not in this list.
When set to a list of two lists, the first list contains keywords
to consider as TODO keywords, the second list contains keywords
to consider as DONE keywords.

When this is set, todo statistics is updated in the parent of the
current entry each time a todo state is changed."
  :group 'org-todo
  :type '(choice
	  (const :tag "Yes, only for TODO entries" t)
	  (const :tag "Yes, including all entries" all-headlines)
	  (repeat :tag "Yes, for TODOs in this list"
		  (string :tag "TODO keyword"))
	  (list :tag "Yes, for TODOs and DONEs in these lists"
		(repeat (string :tag "TODO keyword"))
		(repeat (string :tag "DONE keyword")))
	  (other :tag "No TODO statistics" nil)))

(defcustom org-hierarchical-todo-statistics t
  "Non-nil means TODO statistics covers just direct children.
When nil, all entries in the subtree are considered.
This has only an effect if `org-provide-todo-statistics' is set.
To set this to nil for only a single subtree, use a COOKIE_DATA
property and include the word \"recursive\" into the value."
  :group 'org-todo
  :type 'boolean)

(defcustom org-closed-keep-when-no-todo nil
  "Remove CLOSED: timestamp when switching back to a non-todo state?"
  :group 'org-todo
  :group 'org-keywords
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-after-todo-state-change-hook nil
  "Hook which is run after the state of a TODO item was changed.
The new state (a string with a TODO keyword, or nil) is available in the
Lisp variable `org-state'."
  :group 'org-todo
  :type 'hook)

(defvar org-blocker-hook nil
  "Hook for functions that are allowed to block a state change.

Functions in this hook should not modify the buffer.
Each function gets as its single argument a property list,
see `org-trigger-hook' for more information about this list.

If any of the functions in this hook returns nil, the state change
is blocked.")

(defvar org-trigger-hook nil
  "Hook for functions that are triggered by a state change.

Each function gets as its single argument a property list with at
least the following elements:

 (:type type-of-change :position pos-at-entry-start
  :from old-state :to new-state)

Depending on the type, more properties may be present.

This mechanism is currently implemented for:

TODO state changes
------------------
:type  todo-state-change
:from  previous state (keyword as a string), or nil, or a symbol
       `todo' or `done', to indicate the general type of state.
:to    new state, like in :from")

(defcustom org-enforce-todo-dependencies nil
  "Non-nil means undone TODO entries will block switching the parent to DONE.
Also, if a parent has an :ORDERED: property, switching an entry to DONE will
be blocked if any prior sibling is not yet done.
Finally, if the parent is blocked because of ordered siblings of its own,
the child will also be blocked."
  :set (lambda (var val)
	 (set-default-toplevel-value var val)
	 (if val
	     (add-hook 'org-blocker-hook
		       'org-block-todo-from-children-or-siblings-or-parent)
	   (remove-hook 'org-blocker-hook
			'org-block-todo-from-children-or-siblings-or-parent)))
  :group 'org-todo
  :type 'boolean)

(defcustom org-enforce-todo-checkbox-dependencies nil
  "Non-nil means unchecked boxes will block switching the parent to DONE.
When this is nil, checkboxes have no influence on switching TODO states.
When non-nil, you first need to check off all check boxes before the TODO
entry can be switched to DONE.
This variable needs to be set before org.el is loaded, and you need to
restart Emacs after a change to make the change effective.  The only way
to change it while Emacs is running is through the customize interface."
  :set (lambda (var val)
	 (set-default-toplevel-value var val)
	 (if val
	     (add-hook 'org-blocker-hook
		       'org-block-todo-from-checkboxes)
	   (remove-hook 'org-blocker-hook
			'org-block-todo-from-checkboxes)))
  :group 'org-todo
  :type 'boolean)

(defcustom org-todo-state-tags-triggers nil
  "Tag changes that should be triggered by TODO state changes.
This is a list.  Each entry is

  (state-change (tag . flag) .......)

State-change can be a string with a state, and empty string to indicate the
state that has no TODO keyword, or it can be one of the symbols `todo'
or `done', meaning any not-done or done state, respectively."
  :group 'org-todo
  :group 'org-tags
  :type '(repeat
	  (cons (choice :tag "When changing to"
			(const :tag "Not-done state" todo)
			(const :tag "Done state" done)
			(string :tag "State"))
		(repeat
		 (cons :tag "Tag action"
		       (string :tag "Tag")
		       (choice (const :tag "Add" t) (const :tag "Remove" nil)))))))

(defcustom org-log-done nil
  "Information to record when a task moves to the DONE state.

Possible values are:

nil     Don't add anything, just change the keyword
time    Add a time stamp to the task
note    Prompt for a note and add it with template `org-log-note-headings'

This option can also be set with on a per-file-basis with

   #+STARTUP: nologdone
   #+STARTUP: logdone
   #+STARTUP: lognotedone

You can have local logging settings for a subtree by setting the LOGGING
property to one or more of these keywords."
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "No logging" nil)
	  (const :tag "Record CLOSED timestamp" time)
	  (const :tag "Record CLOSED timestamp with note." note)))

(defcustom org-log-repeat 'time
  "Non-nil means record moving through the DONE state when triggering repeat.
An auto-repeating task is immediately switched back to TODO when
marked DONE.  If you are not logging state changes (by adding \"@\"
or \"!\" to the TODO keyword definition), or set `org-log-done' to
record a closing note, there will be no record of the task moving
through DONE.  This variable forces taking a note anyway.

nil     Don't force a record
time    Record a time stamp
note    Prompt for a note and add it with template `org-log-note-headings'

This option can also be set with on a per-file-basis with

   #+STARTUP: nologrepeat
   #+STARTUP: logrepeat
   #+STARTUP: lognoterepeat

You can have local logging settings for a subtree by setting the LOGGING
property to one or more of these keywords."
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "Don't force a record" nil)
	  (const :tag "Force recording the DONE state" time)
	  (const :tag "Force recording a note with the DONE state" note)))

(defcustom org-todo-repeat-to-state nil
  "The TODO state to which a repeater should return the repeating task.
By default this is the first task of a TODO sequence or the
previous state of a TYPE_TODO set.  But you can specify to use
the previous state in a TODO sequence or a string.

Alternatively, you can set the :REPEAT_TO_STATE: property of the
entry, which has precedence over this option."
  :group 'org-todo
  :version "24.1"
  :type '(choice (const :tag "Use the previous TODO state" t)
		 (const :tag "Use the head of the TODO sequence" nil)
		 (string :tag "Use a specific TODO state")))

(defcustom org-todo-repeat-hook nil
  "Hook that is run after a task has been repeated."
  :package-version '(Org . "9.2")
  :group 'org-todo
  :type 'hook)

;; Normalize old uses of org-log-done.
(cond
 ((eq org-log-done t) (setq org-log-done 'time))
 ((and (listp org-log-done) (memq 'done org-log-done))
  (setq org-log-done 'note)))

(defvar org-last-todo-state-is-todo nil
  "This is non-nil when the last TODO state change led to a TODO state.
If the last change removed the TODO tag or switched to DONE, then
this is nil.")

(defvar org-todo-setup-filter-hook nil
  "Hook for functions that pre-filter todo specs.
Each function takes a todo spec and returns either nil or the spec
transformed into canonical form." )

(defvar org-todo-get-default-hook nil
  "Hook for functions that get a default item for todo.
Each function takes arguments (NEW-MARK OLD-MARK) and returns either
nil or a string to be used for the todo mark." )

(defvar org-agenda-headline-snapshot-before-repeat)

(defun org-todo-yesterday (&optional arg)
  "Like `org-todo' but the time of change will be 23:59 of yesterday."
  (interactive "P")
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-todo-yesterday arg)
    (let* ((org-use-effective-time t)
	   (hour (nth 2 (decode-time (org-current-time))))
	   (org-extend-today-until (1+ hour)))
      (org-todo arg))))

(defvar org-block-entry-blocking ""
  "First entry preventing the TODO state change.")

(defun org-auto-repeat-maybe (done-word)
  "Check if the current headline contains a repeated timestamp.

If yes, set TODO state back to what it was and change the base date
of repeating deadline/scheduled time stamps to new date.

This function is run automatically after each state change to a DONE state."
  (let* ((repeat (org-get-repeat))
	 (aa (assoc org-last-state org-todo-kwd-alist))
	 (interpret (nth 1 aa))
	 (head (nth 2 aa))
	 (whata '(("h" . hour) ("d" . day) ("m" . month) ("y" . year)))
	 (msg "Entry repeats: ")
	 (org-log-done nil)
	 (org-todo-log-states nil)
	 (end (copy-marker (org-entry-end-position))))
    (when (and repeat (not (= 0 (string-to-number (substring repeat 1)))))
      (when (eq org-log-repeat t) (setq org-log-repeat 'state))
      (let ((to-state
             (or (org-entry-get nil "REPEAT_TO_STATE" 'selective)
		 (and (stringp org-todo-repeat-to-state)
		      org-todo-repeat-to-state)
		 (and org-todo-repeat-to-state org-last-state))))
	(org-todo (cond ((and to-state (member to-state org-todo-keywords-1))
			 to-state)
			((eq interpret 'type) org-last-state)
			(head)
			(t 'none))))
      (org-back-to-heading t)
      (org-add-planning-info nil nil 'closed)
      ;; When `org-log-repeat' is non-nil or entry contains
      ;; a clock, set LAST_REPEAT property.
      (when (or org-log-repeat
		(catch :clock
		  (save-excursion
		    (while (re-search-forward org-clock-line-re end t)
		      (when (org-at-clock-log-p) (throw :clock t))))))
	(org-entry-put nil "LAST_REPEAT" (format-time-string
					(org-time-stamp-format t t)
                                        (org-current-effective-time))))
      (when org-log-repeat
	(if org-log-setup
	    ;; We are already setup for some record.
	    (when (eq org-log-repeat 'note)
	      ;; Make sure we take a note, not only a time stamp.
	      (setq org-log-note-how 'note))
	  ;; Set up for taking a record.
	  (org-add-log-setup 'state
			     (or done-word (car org-done-keywords))
			     org-last-state
			     org-log-repeat)))
      ;; Timestamps without a repeater are usually skipped.  However,
      ;; a SCHEDULED timestamp without one is removed, as they are no
      ;; longer relevant.
      (save-excursion
	(let ((scheduled (org-entry-get (point) "SCHEDULED")))
	  (when (and scheduled (not (string-match-p org-repeat-re scheduled)))
	    (org-remove-timestamp-with-keyword org-scheduled-string))))
      ;; Update every timestamp with a repeater in the entry.
      (let ((planning-re (regexp-opt
			  (list org-scheduled-string org-deadline-string))))
	(while (re-search-forward org-repeat-re end t)
	  (let* ((ts (match-string 0))
		 (type (if (not (org-at-planning-p)) "Plain:"
			 (save-excursion
			   (re-search-backward
			    planning-re (line-beginning-position) t)
			   (match-string 0)))))
	    (when (and (org-at-timestamp-p 'agenda)
		       (string-match "\\([.+]\\)?\\(\\+[0-9]+\\)\\([hdwmy]\\)" ts))
	      (let ((n (string-to-number (match-string 2 ts)))
		    (what (match-string 3 ts)))
		(when (equal what "w") (setq n (* n 7) what "d"))
		(when (and (equal what "h")
			   (not (string-match-p "[0-9]\\{1,2\\}:[0-9]\\{2\\}"
						ts)))
		  (user-error
		   "Cannot repeat in %d hour(s) because no hour has been set"
		   n))
		;; Preparation, see if we need to modify the start
		;; date for the change.
		(when (match-end 1)
		  (let ((time (save-match-data (org-time-string-to-time ts)))
			(repeater-type (match-string 1 ts)))
		    (cond
		     ((equal "." repeater-type)
		      ;; Shift starting date to today, or now if
		      ;; repeater is by hours.
		      (if (equal what "h")
			  (org-timestamp-change
			   (floor (- (org-timestamp-to-now ts t)) 60) 'minute)
			(org-timestamp-change
			 (- (org-today) (time-to-days time)) 'day)))
		     ((equal "+" repeater-type)
		      (let ((nshiftmax 10)
			    (nshift 0))
			(while (or (= nshift 0)
				   (if (equal what "h")
				       (not (time-less-p nil time))
				     (>= (org-today)
					 (time-to-days time))))
			  (when (= nshiftmax (cl-incf nshift))
			    (or (y-or-n-p
				 (format "%d repeater intervals were not \
enough to shift date past today.  Continue? "
					 nshift))
				(user-error "Abort")))
			  (org-timestamp-change n (cdr (assoc what whata)))
			  (org-in-regexp org-ts-regexp3)
			  (setq ts (match-string 1))
			  (setq time
				(save-match-data
				  (org-time-string-to-time ts)))))
		      (org-timestamp-change (- n) (cdr (assoc what whata)))
		      ;; Rematch, so that we have everything in place
		      ;; for the real shift.
		      (org-in-regexp org-ts-regexp3)
		      (setq ts (match-string 1))
		      (string-match "\\([.+]\\)?\\(\\+[0-9]+\\)\\([hdwmy]\\)"
				    ts)))))
		(save-excursion
		  (org-timestamp-change n (cdr (assoc what whata)) nil t))
		(setq msg
		      (concat msg type " " org-last-changed-timestamp " ")))))))
      (run-hooks 'org-todo-repeat-hook)
      (setq org-log-post-message msg)
      (message msg))))

(defvar org-state)
;; FIXME: We should refactor this and similar dynamically scoped blocker flags.
(defvar org-blocked-by-checkboxes nil) ; dynamically scoped
;;;###autoload
(defun org-todo (&optional arg)
  "Change the TODO state of an item.

The state of an item is given by a keyword at the start of the heading,
like
     *** TODO Write paper
     *** DONE Call mom

The different keywords are specified in the variable `org-todo-keywords'.
By default the available states are \"TODO\" and \"DONE\".  So, for this
example: when the item starts with TODO, it is changed to DONE.
When it starts with DONE, the DONE is removed.  And when neither TODO nor
DONE are present, add TODO at the beginning of the heading.
You can set up single-character keys to fast-select the new state.  See the
`org-todo-keywords' and `org-use-fast-todo-selection' for details.

With `\\[universal-argument]' prefix ARG, force logging the state change \
and take a
logging note.
With a `\\[universal-argument] \\[universal-argument]' prefix, switch to the \
next set of TODO \
keywords (nextset).
Another way to achieve this is `S-C-<right>'.
With a `\\[universal-argument] \\[universal-argument] \\[universal-argument]' \
prefix, circumvent any state blocking.
With numeric prefix arg, switch to the Nth state.

With a numeric prefix arg of 0, inhibit note taking for the change.
With a numeric prefix arg of -1, cancel repeater to allow marking as DONE.

When called through ELisp, arg is also interpreted in the following way:
`none'        -> empty state
\"\"            -> switch to empty state
`done'        -> switch to DONE
`nextset'     -> switch to the next set of keywords
`previousset' -> switch to the previous set of keywords
\"WAITING\"     -> switch to the specified keyword, but only if it
                 really is a member of `org-todo-keywords'."
  (interactive "P")
  (if (and (use-region-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
		    'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 (lambda () (org-todo arg))
	 nil cl
	 (when (org-invisible-p) (org-end-of-subtree nil t))))
    (when (equal arg '(16)) (setq arg 'nextset))
    (when (equal (prefix-numeric-value arg) -1) (org-cancel-repeater) (setq arg nil))
    (when (< (prefix-numeric-value arg) -1) (user-error "Prefix argument %d not supported" arg))
    (let ((org-blocker-hook org-blocker-hook)
	  commentp
	  case-fold-search)
      (when (equal arg '(64))
	(setq arg nil org-blocker-hook nil))
      (when (and org-blocker-hook
		 (or org-inhibit-blocking
		     (org-entry-get nil "NOBLOCKING")))
	(setq org-blocker-hook nil))
      (save-excursion
	(catch 'exit
	  (org-back-to-heading t)
	  (when (org-in-commented-heading-p t)
	    (org-toggle-comment)
	    (setq commentp t))
	  (when (looking-at org-outline-regexp) (goto-char (1- (match-end 0))))
	  (or (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
	      (looking-at "\\(?: *\\|[ \t]*$\\)"))
	  (let* ((match-data (match-data))
		 (startpos (copy-marker (line-beginning-position)))
		 (force-log (and  (equal arg '(4)) (prog1 t (setq arg nil))))
		 (logging (save-match-data (org-entry-get nil "LOGGING" t t)))
		 (org-log-done org-log-done)
		 (org-log-repeat org-log-repeat)
		 (org-todo-log-states org-todo-log-states)
		 (org-inhibit-logging
		  (if (equal arg 0)
		      (progn (setq arg nil) 'note) org-inhibit-logging))
		 (this (match-string 1))
		 (hl-pos (match-beginning 0))
		 (head (org-get-todo-sequence-head this))
		 (ass (assoc head org-todo-kwd-alist))
		 (interpret (nth 1 ass))
		 (done-word (nth 3 ass))
		 (final-done-word (nth 4 ass))
		 (org-last-state (or this ""))
		 (completion-ignore-case t)
		 (member (member this org-todo-keywords-1))
		 (tail (cdr member))
		 (org-state (cond
			     ((eq arg 'right)
			      ;; Next state
			      (if this
			          (if tail (car tail) nil)
			        (car org-todo-keywords-1)))
			     ((eq arg 'left)
			      ;; Previous state
			      (unless (equal member org-todo-keywords-1)
			        (if this
				    (nth (- (length org-todo-keywords-1)
					    (length tail) 2)
				         org-todo-keywords-1)
			          (org-last org-todo-keywords-1))))
			     (arg
			      ;; User or caller requests a specific state.
			      (cond
			       ((equal arg "") nil)
			       ((eq arg 'none) nil)
			       ((eq arg 'done) (or done-word (car org-done-keywords)))
			       ((eq arg 'nextset)
			        (or (car (cdr (member head org-todo-heads)))
				    (car org-todo-heads)))
			       ((eq arg 'previousset)
			        (let ((org-todo-heads (reverse org-todo-heads)))
			          (or (car (cdr (member head org-todo-heads)))
				      (car org-todo-heads))))
			       ((car (member arg org-todo-keywords-1)))
			       ((stringp arg)
			        (user-error "State `%s' not valid in this file" arg))
			       ((nth (1- (prefix-numeric-value arg))
				     org-todo-keywords-1))))
			     ((and org-todo-key-trigger org-use-fast-todo-selection)
			      ;; Use fast selection.
			      (org-fast-todo-selection this))
			     ((null member) (or head (car org-todo-keywords-1)))
			     ((equal this final-done-word) nil) ;-> make empty
			     ((null tail) nil) ;-> first entry
			     ((memq interpret '(type priority))
			      (if (eq this-command last-command)
			          (car tail)
			        (if (> (length tail) 0)
				    (or done-word (car org-done-keywords))
			          nil)))
			     (t
			      (car tail))))
		 (org-state (or
			     (run-hook-with-args-until-success
			      'org-todo-get-default-hook org-state org-last-state)
			     org-state))
		 (next (if (org-string-nw-p org-state) (concat " " org-state " ") " "))
		 (change-plist (list :type 'todo-state-change :from this :to org-state
				     :position startpos))
		 dolog now-done-p)
	    (when org-blocker-hook
	      (let (org-blocked-by-checkboxes block-reason)
		(setq org-last-todo-state-is-todo
		      (not (member this org-done-keywords)))
		(unless (save-excursion
			  (save-match-data
			    (org-with-wide-buffer
			     (run-hook-with-args-until-failure
			      'org-blocker-hook change-plist))))
		  (setq block-reason (if org-blocked-by-checkboxes
					 "contained checkboxes"
				       (format "\"%s\"" org-block-entry-blocking)))
		  (if (called-interactively-p 'interactive)
		      (user-error "TODO state change from %s to %s blocked (by %s)"
				  this org-state block-reason)
		    ;; Fail silently.
		    (message "TODO state change from %s to %s blocked (by %s)"
			     this org-state block-reason)
		    (throw 'exit nil)))))
	    (store-match-data match-data)
            (org-fold-core-ignore-modifications
              (goto-char (match-beginning 0))
              (replace-match "")
              ;; We need to use `insert-before-markers-and-inherit'
              ;; because: (1) We want to preserve the folding state
              ;; text properties; (2) We do not want to make point
              ;; move before new todo state when inserting a new todo
              ;; into an empty heading.  In (2), the above
              ;; `save-excursion' is relying on markers saved before.
              (insert-before-markers-and-inherit next)
              (unless (org-invisible-p (line-beginning-position))
                (org-fold-region (line-beginning-position)
                                 (line-end-position)
                                 nil 'outline)))
	    (cond ((and org-state (equal this org-state))
		   (message "TODO state was already %s" (org-trim next)))
		  ((not (pos-visible-in-window-p hl-pos))
		   (message "TODO state changed to %s" (org-trim next))))
	    (unless head
	      (setq head (org-get-todo-sequence-head org-state)
		    ass (assoc head org-todo-kwd-alist)
		    interpret (nth 1 ass)
		    done-word (nth 3 ass)
		    final-done-word (nth 4 ass)))
	    (when (memq arg '(nextset previousset))
	      (message "Keyword-Set %d/%d: %s"
		       (- (length org-todo-sets) -1
			  (length (memq (assoc org-state org-todo-sets) org-todo-sets)))
		       (length org-todo-sets)
		       (mapconcat 'identity (assoc org-state org-todo-sets) " ")))
	    (setq org-last-todo-state-is-todo
		  (not (member org-state org-done-keywords)))
	    (setq now-done-p (and (member org-state org-done-keywords)
				  (not (member this org-done-keywords))))
	    (and logging (org-local-logging logging))
	    (when (or (and (or org-todo-log-states org-log-done)
			   (not (eq org-inhibit-logging t))
			   (not (memq arg '(nextset previousset))))
		      force-log)
	      ;; We need to look at recording a time and note.
	      (setq dolog (or (if force-log 'note)
			      (nth 1 (assoc org-state org-todo-log-states))
			      (nth 2 (assoc this org-todo-log-states))))
	      (when (and (eq dolog 'note) (eq org-inhibit-logging 'note))
		(setq dolog 'time))
	      (when (or (and (not org-state) (not org-closed-keep-when-no-todo))
			(and org-state
			     (member org-state org-not-done-keywords)
			     (not (member this org-not-done-keywords))))
		;; This is now a todo state and was not one before
		;; If there was a CLOSED time stamp, get rid of it.
		(org-add-planning-info nil nil 'closed))
	      (when (and now-done-p org-log-done)
		;; It is now done, and it was not done before.
		(org-add-planning-info 'closed (org-current-effective-time))
		(when (and (not dolog) (eq 'note org-log-done))
		  (org-add-log-setup 'done org-state this 'note)))
	      (when (and org-state dolog)
		;; This is a non-nil state, and we need to log it.
		(org-add-log-setup 'state org-state this dolog)))
	    ;; Fixup tag positioning.
	    (org-todo-trigger-tag-changes org-state)
	    (when org-auto-align-tags (org-align-tags))
	    (when org-provide-todo-statistics
	      (org-update-parent-todo-statistics))
	    (when (bound-and-true-p org-clock-out-when-done)
	      (org-clock-out-if-current))
	    (run-hooks 'org-after-todo-state-change-hook)
	    (when (and arg (not (member org-state org-done-keywords)))
	      (setq head (org-get-todo-sequence-head org-state)))
            (put-text-property (line-beginning-position)
                               (line-end-position) 'org-todo-head head)
	    ;; Do we need to trigger a repeat?
	    (when now-done-p
	      (when (boundp 'org-agenda-headline-snapshot-before-repeat)
		;; This is for the agenda, take a snapshot of the headline.
		(save-match-data
		  (setq org-agenda-headline-snapshot-before-repeat
			(org-get-heading))))
	      (org-auto-repeat-maybe org-state))
	    ;; Fixup cursor location if close to the keyword.
	    (when (and (org-at-heading-p t)
		       (not (bolp))
		       (save-excursion
                         (forward-line 0)
			 (looking-at org-todo-line-regexp))
		       (< (point) (+ 2 (or (match-end 2) (match-end 1)))))
	      (goto-char (or (match-end 2) (match-end 1)))
	      (and (looking-at " ")
		   (not (looking-at " *:"))
		   (just-one-space)))
	    (when org-trigger-hook
	      (save-excursion
		(run-hook-with-args 'org-trigger-hook change-plist)))
	    (when commentp (org-toggle-comment))))))))

(defun org-block-todo-from-children-or-siblings-or-parent (change-plist)
  "Block turning an entry into a TODO, using the hierarchy.
This checks whether the current task should be blocked from state
changes.  Such blocking occurs when:

  1. The task has children which are not all in a completed state.

  2. A task has a parent with the property :ORDERED:, and there
     are siblings prior to the current task with incomplete
     status.

  3. The parent of the task is blocked because it has siblings that should
     be done first, or is child of a block grandparent TODO entry."

  (if (not org-enforce-todo-dependencies)
      t ; if locally turned off don't block
    (catch 'dont-block
      ;; If this is not a todo state change, or if this entry is already DONE,
      ;; do not block
      (when (or (not (eq (plist-get change-plist :type) 'todo-state-change))
		(member (plist-get change-plist :from)
			(cons 'done org-done-keywords))
		(member (plist-get change-plist :to)
			(cons 'todo org-not-done-keywords))
		(not (plist-get change-plist :to)))
	(throw 'dont-block t))
      ;; If this task has children, and any are undone, it's blocked
      (save-excursion
	(org-back-to-heading t)
	(let ((this-level (funcall outline-level)))
	  (outline-next-heading)
	  (let ((child-level (funcall outline-level)))
	    (while (and (not (eobp))
			(> child-level this-level))
	      ;; this todo has children, check whether they are all
	      ;; completed
	      (when (and (not (org-entry-is-done-p))
			 (org-entry-is-todo-p))
		(setq org-block-entry-blocking (org-get-heading))
		(throw 'dont-block nil))
	      (outline-next-heading)
	      (setq child-level (funcall outline-level))))))
      ;; Otherwise, if the task's parent has the :ORDERED: property, and
      ;; any previous siblings are undone, it's blocked
      (save-excursion
	(org-back-to-heading t)
	(let* ((pos (point))
	       (parent-pos (and (org-up-heading-safe) (point)))
	       (case-fold-search nil))
	  (unless parent-pos (throw 'dont-block t)) ; no parent
	  (when (and (org-not-nil (org-entry-get (point) "ORDERED"))
		     (forward-line 1)
		     (re-search-forward org-not-done-heading-regexp pos t))
	    (setq org-block-entry-blocking (match-string 0))
	    (throw 'dont-block nil))  ; block, there is an older sibling not done.
	  ;; Search further up the hierarchy, to see if an ancestor is blocked
	  (while t
	    (goto-char parent-pos)
	    (unless (looking-at org-not-done-heading-regexp)
	      (throw 'dont-block t))	; do not block, parent is not a TODO
	    (setq pos (point))
	    (setq parent-pos (and (org-up-heading-safe) (point)))
	    (unless parent-pos (throw 'dont-block t)) ; no parent
	    (when (and (org-not-nil (org-entry-get (point) "ORDERED"))
		       (forward-line 1)
		       (re-search-forward org-not-done-heading-regexp pos t)
		       (setq org-block-entry-blocking (org-get-heading)))
	      (throw 'dont-block nil)))))))) ; block, older sibling not done.

(defcustom org-track-ordered-property-with-tag nil
  "Should the ORDERED property also be shown as a tag?
The ORDERED property decides if an entry should require subtasks to be
completed in sequence.  Since a property is not very visible, setting
this option means that toggling the ORDERED property with the command
`org-toggle-ordered-property' will also toggle a tag ORDERED.  That tag is
not relevant for the behavior, but it makes things more visible.

Note that toggling the tag with tags commands will not change the property
and therefore not influence behavior!

This can be t, meaning the tag ORDERED should be used.  It can also be a
string to select a different tag for this task."
  :group 'org-todo
  :type '(choice
	  (const :tag "No tracking" nil)
	  (const :tag "Track with ORDERED tag" t)
	  (string :tag "Use other tag")))

;;;###autoload
(defun org-toggle-ordered-property ()
  "Toggle the ORDERED property of the current entry.
For better visibility, you can track the value of this property with a tag.
See variable `org-track-ordered-property-with-tag'."
  (interactive)
  (let* ((t1 org-track-ordered-property-with-tag)
	 (tag (and t1 (if (stringp t1) t1 "ORDERED"))))
    (save-excursion
      (org-back-to-heading)
      (if (org-entry-get nil "ORDERED")
	  (progn
	    (org-delete-property "ORDERED")
	    (and tag (org-toggle-tag tag 'off))
	    (message "Subtasks can be completed in arbitrary order"))
	(org-entry-put nil "ORDERED" "t")
	(and tag (org-toggle-tag tag 'on))
	(message "Subtasks must be completed in sequence")))))

(defun org-block-todo-from-checkboxes (change-plist)
  "Block turning an entry into a TODO, using checkboxes.
This checks whether the current task should be blocked from state
changes because there are unchecked boxes in this entry."
  (if (not org-enforce-todo-checkbox-dependencies)
      t ; if locally turned off don't block
    (catch 'dont-block
      ;; If this is not a todo state change, or if this entry is already DONE,
      ;; do not block
      (when (or (not (eq (plist-get change-plist :type) 'todo-state-change))
		(member (plist-get change-plist :from)
			(cons 'done org-done-keywords))
		(member (plist-get change-plist :to)
			(cons 'todo org-not-done-keywords))
		(not (plist-get change-plist :to)))
	(throw 'dont-block t))
      ;; If this task has checkboxes that are not checked, it's blocked
      (save-excursion
	(org-back-to-heading t)
	(let ((beg (point)) end)
	  (outline-next-heading)
	  (setq end (point))
	  (goto-char beg)
	  (when (org-list-search-forward
		 (concat (org-item-beginning-re)
			 "\\(?:\\[@\\(?:start:\\)?\\([0-9]+\\|[A-Za-z]\\)\\][ \t]*\\)?"
			 "\\[[- ]\\]")
		 end t)
	    (when (boundp 'org-blocked-by-checkboxes)
	      (setq org-blocked-by-checkboxes t))
	    (throw 'dont-block nil))))
      t))) ; do not block

(defun org-entry-blocked-p ()
  "Non-nil if entry at point is blocked."
  (and (not (org-entry-get nil "NOBLOCKING"))
       (member (org-entry-get nil "TODO") org-not-done-keywords)
       (not (run-hook-with-args-until-failure
	     'org-blocker-hook
	     (list :type 'todo-state-change
		   :position (point)
		   :from 'todo
		   :to 'done)))))

;;;###autoload
(defun org-update-statistics-cookies (all)
  "Update the statistics cookie, either from TODO or from checkboxes.
This should be called with the cursor in a line with a statistics
cookie.  When called with a \\[universal-argument] prefix, update
all statistics cookies in the buffer."
  (interactive "P")
  (if all
      (progn
	(org-update-checkbox-count 'all)
	(org-map-region 'org-update-parent-todo-statistics
                        (point-min) (point-max)))
    (if (not (org-at-heading-p))
	(org-update-checkbox-count)
      (let ((pos (point-marker))
	    end l1 l2)
	(ignore-errors (org-back-to-heading t))
	(if (not (org-at-heading-p))
	    (org-update-checkbox-count)
	  (setq l1 (org-outline-level))
	  (setq end
                (save-excursion
		  (outline-next-heading)
		  (when (org-at-heading-p) (setq l2 (org-outline-level)))
		  (point)))
	  (if (and (save-excursion
		     (re-search-forward
		      "^[ \t]*\\([-+*]\\|[0-9]+[.)]\\) \\[[- X]\\]" end t))
	           (not (save-excursion
                          (re-search-forward
			   ":COOKIE_DATA:.*\\<todo\\>" end t))))
	      (org-update-checkbox-count)
	    (if (and l2 (> l2 l1))
		(progn
		  (goto-char end)
		  (org-update-parent-todo-statistics))
	      (goto-char pos)
	      (forward-line 0)
	      (while (re-search-forward
		      "\\(\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)\\)"
                      (line-end-position) t)
		(replace-match (if (match-end 2) "[100%]" "[0/0]") t t)))))
	(goto-char pos)
	(move-marker pos nil)))))

(defvar org-entry-property-inherited-from) ;; defined below
(defun org-update-parent-todo-statistics ()
  "Update any statistics cookie in the parent of the current headline.
When `org-hierarchical-todo-statistics' is nil, statistics will cover
the entire subtree and this will travel up the hierarchy and update
statistics everywhere."
  (let* ((prop (save-excursion
                 (org-up-heading-safe)
		 (org-entry-get nil "COOKIE_DATA" 'inherit)))
	 (recursive (or (not org-hierarchical-todo-statistics)
			(and prop (string-match "\\<recursive\\>" prop))))
	 (lim (or (and prop (marker-position org-entry-property-inherited-from))
		  0))
	 (first t)
	 (box-re "\\(\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)\\)")
	 level ltoggle l1 new ndel
	 (cnt-all 0) (cnt-done 0) is-percent kwd
	 checkbox-beg cookie-present)
    (catch 'exit
      (save-excursion
	(forward-line 0)
	(setq ltoggle (funcall outline-level))
	;; Three situations are to consider:

	;; 1. if `org-hierarchical-todo-statistics' is nil, repeat up
	;;    to the top-level ancestor on the headline;

	;; 2. If parent has "recursive" property, repeat up to the
	;;    headline setting that property, taking inheritance into
	;;    account;

	;; 3. Else, move up to direct parent and proceed only once.
	(while (and (setq level (org-up-heading-safe))
		    (or recursive first)
		    (>= (point) lim))
	  (setq first nil cookie-present nil)
	  (unless (and level
		       (not (string-match
			     "\\<checkbox\\>"
			     (downcase (or (org-entry-get nil "COOKIE_DATA")
					   "")))))
	    (throw 'exit nil))
          (while (re-search-forward box-re (line-end-position) t)
	    (setq cnt-all 0 cnt-done 0 cookie-present t)
	    (setq is-percent (match-end 2) checkbox-beg (match-beginning 0))
            (when (org-element-type-p
                   (save-excursion
                     (goto-char checkbox-beg)
                     (save-match-data (org-element-context)))
                   '(statistics-cookie
                     ;; Special case - statistics cookie inside properties.
                     keyword))
	      (save-match-data
	        (unless (outline-next-heading) (throw 'exit nil))
	        (while (and (looking-at org-complex-heading-regexp)
	    		    (> (setq l1 (length (match-string 1))) level))
	    	  (setq kwd (and (or recursive (= l1 ltoggle))
	    		         (match-string 2)))
	    	  (if (or (eq org-provide-todo-statistics 'all-headlines)
			  (and (eq org-provide-todo-statistics t)
			       (or (member kwd org-done-keywords)))
	    		  (and (listp org-provide-todo-statistics)
			       (stringp (car org-provide-todo-statistics))
	    		       (or (member kwd org-provide-todo-statistics)
				   (member kwd org-done-keywords)))
			  (and (listp org-provide-todo-statistics)
			       (listp (car org-provide-todo-statistics))
			       (or (member kwd (car org-provide-todo-statistics))
				   (and (member kwd org-done-keywords)
				        (member kwd (cadr org-provide-todo-statistics))))))
	    	      (setq cnt-all (1+ cnt-all))
		    (and (eq org-provide-todo-statistics t)
		         kwd
		         (setq cnt-all (1+ cnt-all))))
		  (when (or (and (member org-provide-todo-statistics '(t all-headlines))
			         (member kwd org-done-keywords))
			    (and (listp org-provide-todo-statistics)
			         (listp (car org-provide-todo-statistics))
			         (member kwd org-done-keywords)
			         (member kwd (cadr org-provide-todo-statistics)))
			    (and (listp org-provide-todo-statistics)
			         (stringp (car org-provide-todo-statistics))
			         (member kwd org-done-keywords)))
		    (setq cnt-done (1+ cnt-done)))
	    	  (outline-next-heading)))
	      (setq new
	    	    (if is-percent
		        (format "[%d%%]" (floor (* 100.0 cnt-done)
					        (max 1 cnt-all)))
	    	      (format "[%d/%d]" cnt-done cnt-all))
	    	    ndel (- (match-end 0) checkbox-beg))
              (goto-char (match-end 0))
              (unless (string-equal new (buffer-substring checkbox-beg (match-end 0)))
	        (goto-char checkbox-beg)
	        (insert new)
	        (delete-region (point) (+ (point) ndel))
	        (when org-auto-align-tags (org-fix-tags-on-the-fly)))))
	  (when cookie-present
	    (run-hook-with-args 'org-after-todo-statistics-hook
				cnt-done (- cnt-all cnt-done))))))
    (run-hooks 'org-todo-statistics-hook)))

(defvar org-after-todo-statistics-hook nil
  "Hook that is called after a TODO statistics cookie has been updated.
Each function is called with two arguments: the number of not-done entries
and the number of done entries.

For example, the following function, when added to this hook, will switch
an entry to DONE when all children are done, and back to TODO when new
entries are set to a TODO status.  Note that this hook is only called
when there is a statistics cookie in the headline!

 (defun org-summary-todo (n-done n-not-done)
   \"Switch entry to DONE when all subentries are done, to TODO otherwise.\"
   (let (org-log-done org-todo-log-states)   ; turn off logging
     (org-todo (if (= n-not-done 0) \"DONE\" \"TODO\"))))")

(defvar org-todo-statistics-hook nil
  "Hook that is run whenever Org thinks TODO statistics should be updated.
This hook runs even if there is no statistics cookie present, in which case
`org-after-todo-statistics-hook' would not run.")

(defun org-todo-trigger-tag-changes (state)
  "Apply the changes defined in `org-todo-state-tags-triggers'."
  (let ((l org-todo-state-tags-triggers)
	changes)
    (when (or (not state) (equal state ""))
      (setq changes (append changes (cdr (assoc "" l)))))
    (when (and (stringp state) (> (length state) 0))
      (setq changes (append changes (cdr (assoc state l)))))
    (when (member state org-not-done-keywords)
      (setq changes (append changes (cdr (assq 'todo l)))))
    (when (member state org-done-keywords)
      (setq changes (append changes (cdr (assq 'done l)))))
    (dolist (c changes)
      (org-toggle-tag (car c) (if (cdr c) 'on 'off)))))

(defun org-local-logging (value)
  "Get logging settings from a property VALUE."
  ;; Directly set the variables, they are already local.
  (setq org-log-done nil
        org-log-repeat nil
        org-todo-log-states nil)
  (dolist (w (split-string value))
    (let (a)
      (cond
       ((setq a (assoc w org-startup-options))
        (and (member (nth 1 a) '(org-log-done org-log-repeat))
             (set (nth 1 a) (nth 2 a))))
       ((setq a (org-extract-log-state-settings w))
        (and (member (car a) org-todo-keywords-1)
             (push a org-todo-log-states)))))))

(defun org-get-todo-sequence-head (kwd)
  "Return the head of the TODO sequence to which KWD belongs.
If KWD is not set, check if there is a text property remembering the
right sequence."
  (let (p)
    (cond
     ((not kwd)
      (or (get-text-property (line-beginning-position) 'org-todo-head)
	  (progn
            (setq p (next-single-property-change (line-beginning-position)
                                                 'org-todo-head
                                                 nil (line-end-position)))
	    (get-text-property p 'org-todo-head))))
     ((not (member kwd org-todo-keywords-1))
      (car org-todo-keywords-1))
     (t (nth 2 (assoc kwd org-todo-kwd-alist))))))

(defun org-fast-todo-selection (&optional current-todo-keyword)
  "Fast TODO keyword selection with single keys.
Returns the new TODO keyword, or nil if no state change should occur.

When CURRENT-TODO-KEYWORD is given and selection letters are not
unique globally, prefer a state in the current todo keyword sequence
where CURRENT-TODO-KEYWORD belongs over on in another sequence."
  (let* ((todo-alist org-todo-key-alist) ; copy from the original Org buffer.
         (todo-alist-tail todo-alist)
         ;; TODO keyword sequence that takes priority in case if there is binding collision.
	 (preferred-sequence-head (org-get-todo-sequence-head current-todo-keyword))
         in-preferred-sequence preferred-todo-alist
	 (done-keywords org-done-keywords) ;; needed for the faces when calling `org-get-todo-face'.
	 (expert-interface (equal org-use-fast-todo-selection 'expert))
	 (prompt "") ; Additional expert prompt, listing todo keyword bindings.
         ;; Max width occupied by a single todo record in the completion buffer.
         (field-width
          (+ 3 ; keep space for "[c]" binding.
             1 ; ensure that there is at least one space between adjacent todo fields.
             3 ; FIXME: likely coped from `org-fast-tag-selection'
             ;; The longest todo keyword.
             (apply 'max (mapcar
			  (lambda (x)
			    (if (stringp (car x)) (string-width (car x)) 0))
			  org-todo-key-alist))))
         field-number ; current todo keyword column in the completion buffer.
         todo-binding-spec todo-keyword todo-char input-char)
    ;; Display todo selection dialog, read the user input, and return.
    (save-excursion
      (save-window-excursion
        ;; Select todo keyword list buffer, and display it unless EXPERT-INTERFACE.
	(if expert-interface
	    (set-buffer (get-buffer-create " *Org todo*"))
          (pop-to-buffer
           (get-buffer-create (get-buffer-create " *Org todo*"))
           '(org-display-buffer-split (direction . down))))
        ;; Fill text in *Org todo* buffer.
	(erase-buffer)
        ;; Copy `org-done-keywords' from the original Org buffer to be
        ;; used by `org-get-todo-face'.
	(setq-local org-done-keywords done-keywords)
        ;; Show todo keyword sequences and bindings in a grid.
        ;; Each todo keyword in the grid occupies FIELD-WIDTH characters.
        ;; The keywords are filled up to `window-width'.
	(setq field-number 0)
	(while (setq todo-binding-spec (pop todo-alist-tail))
	  (pcase todo-binding-spec
            ;; Group keywords as { KWD1 KWD2 ... }
	    (`(:startgroup)
	     (unless (= field-number 0)
	       (setq field-number 0)
	       (insert "\n"))
	     (setq prompt (concat prompt "{"))
	     (insert "{ "))
	    (`(:endgroup)
	     (setq field-number 0
                   ;; End of a group.  Reset flag indicating preferred keyword sequence.
                   in-preferred-sequence nil)
	     (setq prompt (concat prompt "}"))
	     (insert "}\n"))
	    (`(:newline)
	     (unless (= field-number 0)
	       (insert "\n")
	       (setq field-number 0)
	       (setq todo-binding-spec (car todo-alist-tail))
	       (while (equal (car todo-alist-tail) '(:newline))
		 (insert "\n")
		 (pop todo-alist-tail))))
	    (_
	     (setq todo-keyword (car todo-binding-spec)
                   todo-char (cdr todo-binding-spec))
             ;; For the first keyword in a preferred sequence, set flag.
	     (if (equal todo-keyword preferred-sequence-head)
                 (setq in-preferred-sequence t))
             ;; Store the preferred todo keyword sequence.
	     (when in-preferred-sequence (push todo-binding-spec preferred-todo-alist))
             ;; Assign face to the todo keyword.
	     (setq todo-keyword
                   (org-add-props
                       todo-keyword nil
                     'face (org-get-todo-face todo-keyword)))
	     (when (= field-number 0) (insert "  "))
	     (setq prompt (concat prompt "[" (char-to-string todo-char) "] " todo-keyword " "))
	     (insert "[" todo-char "] " todo-keyword
                     ;; Fill spaces up to FIELD-WIDTH.
                     (make-string
		      (- field-width 4 (length todo-keyword)) ?\ ))
             ;; Last column in the row.
	     (when (and (= (setq field-number (1+ field-number))
                           (/ (- (window-width) 4) field-width))
		        ;; Avoid lines with just a closing delimiter.
		        (not (equal (car todo-alist-tail) '(:endgroup))))
	       (insert "\n")
	       (setq field-number 0)))))
	(insert "\n")
	(goto-char (point-min))
	(unless expert-interface (org-fit-window-to-buffer))
	(message (concat "[a-z..]:Set [SPC]:clear"
			 (if expert-interface (concat "\n" prompt) "")))
        ;; Read the todo keyword input and exit.
	(setq input-char
              (let ((inhibit-quit t)) ; intercept C-g.
                (read-char-exclusive)))
        ;; Restore the original keyword order.  Previously, it was reversed using `push'.
	(setq preferred-todo-alist (nreverse preferred-todo-alist))
	(cond
	 ((equal input-char ?\s) nil)
         ((or (= input-char ?\C-g)
	      (and (= input-char ?q) (not (rassoc input-char todo-alist))))
          (signal 'quit nil))
	 ((setq todo-binding-spec (or
                                   ;; Prefer bindings from todo sequence containing CURRENT-TODO-KEYWORD.
                                   (rassoc input-char preferred-todo-alist)
                                   (rassoc input-char todo-alist))
	        todo-keyword (car todo-binding-spec))
	  todo-keyword)
         (t (signal 'quit nil)))))))

(provide 'org-todo)

;;; org-todo.el ends here
