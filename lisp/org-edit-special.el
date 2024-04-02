;;; org-edit-special.el --- Special Org editing commands                      -*- lexical-binding: t; -*-

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

;; This library implements special context-sensitive Org mode
;; commands.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-table)
(require 'org-element)
(require 'org-cycle)
(defvar org-support-shift-select)
(defvar org-clock-adjust-closest)
(require 'org-regexps)
(declare-function org-inlinetask-in-task-p "org-inlinetask")
(require 'org-edit)
(declare-function org-clocktable-shift "org-clock")
(require 'org-todo)
(declare-function org-clock-timestamps-up "org-clock")
(declare-function org-clock-timestamps-down "org-clock")
(declare-function org-columns-quit "org-colview")
(defvar org-occur-highlights)
(declare-function org-clock-remove-overlays "org-clock")
(declare-function org-remove-occur-highlights "org-sparse-tree")
(declare-function org-babel-lob-get-info "ob-lob")
(declare-function org-clock-update-time-maybe "org-clock")
(declare-function org-update-dblock "org-dblock")
(declare-function org-plot/gnuplot "org-plot")
(declare-function org-indent-mode "org-indent")
(declare-function org-mark-element "org")
(defvar org-done-keywords)

(defcustom org-treat-S-cursor-todo-selection-as-state-change t
  "Non-nil means switching TODO states with S-cursor counts as state change.
This is the default behavior.  However, setting this to nil allows a
convenient way to select a TODO state and bypass any logging associated
with that."
  :group 'org-todo
  :type 'boolean)

(defvar org-ctrl-c-ctrl-c-hook nil
  "Hook for functions attaching themselves to \\`C-c C-c'.

This can be used to add additional functionality to the \\`C-c C-c'
key which executes context-dependent commands.  This hook is run
before any other test, while `org-ctrl-c-ctrl-c-final-hook' is
run after the last test.

Each function will be called with no arguments.  The function
must check if the context is appropriate for it to act.  If yes,
it should do its thing and then return a non-nil value.  If the
context is wrong, just do nothing and return nil.")

(defvar org-ctrl-c-ctrl-c-final-hook nil
  "Hook for functions attaching themselves to \\`C-c C-c'.

This can be used to add additional functionality to the \\`C-c C-c'
key which executes context-dependent commands.  This hook is run
after any other test, while `org-ctrl-c-ctrl-c-hook' is run
before the first test.

Each function will be called with no arguments.  The function
must check if the context is appropriate for it to act.  If yes,
it should do its thing and then return a non-nil value.  If the
context is wrong, just do nothing and return nil.")

(defvar org-tab-after-check-for-table-hook nil
  "Hook for functions to attach themselves to TAB.
See `org-ctrl-c-ctrl-c-hook' for more information.
This hook runs after it has been established that the cursor is not in a
table, but before checking if the cursor is in a headline or if global cycling
should be done.
If any function in this hook returns t, not other actions like visibility
cycling will be done.")

(defvar org-tab-after-check-for-cycling-hook nil
  "Hook for functions to attach themselves to TAB.
See `org-ctrl-c-ctrl-c-hook' for more information.
This hook runs after it has been established that not table field motion and
not visibility should be done because of current context.  This is probably
the place where a package like yasnippets can hook in.")

(defvar org-tab-before-tab-emulation-hook nil
  "Hook for functions to attach themselves to TAB.
See `org-ctrl-c-ctrl-c-hook' for more information.
This hook runs after every other options for TAB have been exhausted, but
before indentation and \t insertion takes place.")

(defvar org-metaleft-hook nil
  "Hook for functions attaching themselves to `M-left'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metaleft-final-hook nil
  "Hook for functions attaching themselves to `M-left'.
This one runs after all options have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metaright-hook nil
  "Hook for functions attaching themselves to `M-right'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metaright-final-hook nil
  "Hook for functions attaching themselves to `M-right'.
This one runs after all options have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metaup-hook nil
  "Hook for functions attaching themselves to `M-up'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metaup-final-hook nil
  "Hook for functions attaching themselves to `M-up'.
This one runs after all other options except
`org-drag-element-backward' have been excluded.  See
`org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metadown-hook nil
  "Hook for functions attaching themselves to `M-down'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metadown-final-hook nil
  "Hook for functions attaching themselves to `M-down'.
This one runs after all other options except
`org-drag-element-forward' have been excluded.  See
`org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetaleft-hook nil
  "Hook for functions attaching themselves to `M-S-left'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetaleft-final-hook nil
  "Hook for functions attaching themselves to `M-S-left'.
This one runs after all other options have been excluded.  See
`org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetaright-hook nil
  "Hook for functions attaching themselves to `M-S-right'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetaright-final-hook nil
  "Hook for functions attaching themselves to `M-S-right'.
This one runs after all other options have been excluded.  See
`org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetaup-hook nil
  "Hook for functions attaching themselves to `M-S-up'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetaup-final-hook nil
  "Hook for functions attaching themselves to `M-S-up'.
This one runs after all other options except
`org-drag-line-backward' have been excluded.  See
`org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetadown-hook nil
  "Hook for functions attaching themselves to `M-S-down'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetadown-final-hook nil
  "Hook for functions attaching themselves to `M-S-down'.
This one runs after all other options except
`org-drag-line-forward' have been excluded.  See
`org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metareturn-hook nil
  "Hook for functions attaching themselves to `M-RET'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftup-hook nil
  "Hook for functions attaching themselves to `S-up'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftup-final-hook nil
  "Hook for functions attaching themselves to `S-up'.
This one runs after all other options except shift-select have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftdown-hook nil
  "Hook for functions attaching themselves to `S-down'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftdown-final-hook nil
  "Hook for functions attaching themselves to `S-down'.
This one runs after all other options except shift-select have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftleft-hook nil
  "Hook for functions attaching themselves to `S-left'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftleft-final-hook nil
  "Hook for functions attaching themselves to `S-left'.
This one runs after all other options except shift-select have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftright-hook nil
  "Hook for functions attaching themselves to `S-right'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftright-final-hook nil
  "Hook for functions attaching themselves to `S-right'.
This one runs after all other options except shift-select have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")

(defun org-modifier-cursor-error ()
  "Throw an error, a modified cursor command was applied in wrong context."
  (user-error "This command is active in special context like tables, headlines or items"))

(defun org-shiftselect-error ()
  "Throw an error because Shift-Cursor command was applied in wrong context."
  (if (and (boundp 'shift-select-mode) shift-select-mode)
      (user-error "To use shift-selection with Org mode, customize `org-support-shift-select'")
    (user-error "This command works only in special context like headlines or timestamps")))

(defun org-call-for-shift-select (cmd)
  (let ((this-command-keys-shift-translated t))
    (call-interactively cmd)))

(defun org-shifttab (&optional arg)
  "Global visibility cycling or move to previous table field.
Call `org-table-previous-field' within a table.
When ARG is nil, cycle globally through visibility states.
When ARG is a numeric prefix, show contents of this level."
  (interactive "P")
  (cond
   ((org-at-table-p) (call-interactively 'org-table-previous-field))
   ((integerp arg)
    (let ((arg2 (if org-odd-levels-only (1- (* 2 arg)) arg)))
      (message "Content view to level: %d" arg)
      (org-cycle-content (prefix-numeric-value arg2))
      (org-cycle-show-empty-lines t)
      (setq org-cycle-global-status 'overview)
      (run-hook-with-args 'org-cycle-hook 'overview)))
   (t (call-interactively 'org-cycle-global))))

(defun org-shiftmetaleft ()
  "Promote subtree or delete table column.
Calls `org-promote-subtree', `org-outdent-item-tree', or
`org-table-delete-column', depending on context.  See the
individual commands for more information.

This function runs the functions in `org-shiftmetaleft-hook' one
by one as a first step, and exits immediately if a function from
the hook returns non-nil.  In the absence of a specific context,
the function also runs `org-shiftmetaleft-final-hook' using the
same logic."
  (interactive)
  (cond
   ((and (eq system-type 'darwin)
         (or (eq org-support-shift-select 'always)
             (and org-support-shift-select (org-region-active-p))))
    (org-call-for-shift-select 'backward-char))
   ((run-hook-with-args-until-success 'org-shiftmetaleft-hook))
   ((org-at-table-p) (call-interactively 'org-table-delete-column))
   ((org-at-heading-p) (call-interactively 'org-promote-subtree))
   ((if (not (org-region-active-p)) (org-at-item-p)
      (save-excursion (goto-char (region-beginning))
		      (org-at-item-p)))
    (call-interactively 'org-outdent-item-tree))
   ((run-hook-with-args-until-success 'org-shiftmetaleft-final-hook))
   (t (org-modifier-cursor-error))))

(defun org-shiftmetaright ()
  "Demote subtree or insert table column.
Calls `org-demote-subtree', `org-indent-item-tree', or
`org-table-insert-column', depending on context.  See the
individual commands for more information.

This function runs the functions in `org-shiftmetaright-hook' one
by one as a first step, and exits immediately if a function from
the hook returns non-nil.  In the absence of a specific context,
the function also runs `org-shiftmetaright-final-hook' using the
same logic."
  (interactive)
  (cond
   ((and (eq system-type 'darwin)
         (or (eq org-support-shift-select 'always)
             (and org-support-shift-select (org-region-active-p))))
    (org-call-for-shift-select 'forward-char))
   ((run-hook-with-args-until-success 'org-shiftmetaright-hook))
   ((org-at-table-p) (call-interactively 'org-table-insert-column))
   ((org-at-heading-p) (call-interactively 'org-demote-subtree))
   ((if (not (org-region-active-p)) (org-at-item-p)
      (save-excursion (goto-char (region-beginning))
		      (org-at-item-p)))
    (call-interactively 'org-indent-item-tree))
   ((run-hook-with-args-until-success 'org-shiftmetaright-final-hook))
   (t (org-modifier-cursor-error))))

(defun org-shiftmetaup (&optional _arg)
  "Drag the line at point up.
In a table, kill the current row.
On a clock timestamp, update the value of the timestamp like `S-<up>'
but also adjust the previous clocked item in the clock history.
Everywhere else, drag the line at point up.

This function runs the functions in `org-shiftmetaup-hook' one by
one as a first step, and exits immediately if a function from the
hook returns non-nil.  In the absence of a specific context, the
function also runs `org-shiftmetaup-final-hook' using the same
logic."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftmetaup-hook))
   ((org-at-table-p) (call-interactively 'org-table-kill-row))
   ((org-at-clock-log-p) (let ((org-clock-adjust-closest t))
			   (call-interactively 'org-timestamp-up)))
   ((run-hook-with-args-until-success 'org-shiftmetaup-final-hook))
   (t (call-interactively 'org-drag-line-backward))))

(defun org-shiftmetadown (&optional _arg)
  "Drag the line at point down.
In a table, insert an empty row at the current line.
On a clock timestamp, update the value of the timestamp like `S-<down>'
but also adjust the previous clocked item in the clock history.
Everywhere else, drag the line at point down.

This function runs the functions in `org-shiftmetadown-hook' one
by one as a first step, and exits immediately if a function from
the hook returns non-nil.  In the absence of a specific context,
the function also runs `org-shiftmetadown-final-hook' using the
same logic."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftmetadown-hook))
   ((org-at-table-p) (call-interactively 'org-table-insert-row))
   ((org-at-clock-log-p) (let ((org-clock-adjust-closest t))
			   (call-interactively 'org-timestamp-down)))
   ((run-hook-with-args-until-success 'org-shiftmetadown-final-hook))
   (t (call-interactively 'org-drag-line-forward))))

(defsubst org-hidden-tree-error ()
  (user-error
   "Hidden subtree, open with TAB or use subtree command M-S-<left>/<right>"))

(defun org-metaleft (&optional _arg)
  "Promote heading, list item at point or move table column left.

Calls `org-do-promote', `org-outdent-item' or `org-table-move-column',
depending on context.  With no specific context, calls the Emacs
default `backward-word'.  See the individual commands for more
information.

This function runs the functions in `org-metaleft-hook' one by
one as a first step, and exits immediately if a function from the
hook returns non-nil.  In the absence of a specific context, the
function runs `org-metaleft-final-hook' using the same logic."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-metaleft-hook))
   ((org-at-table-p) (org-call-with-arg 'org-table-move-column 'left))
   ((org-with-limited-levels
     (or (org-at-heading-p)
	 (and (org-region-active-p)
	      (save-excursion
		(goto-char (region-beginning))
		(org-at-heading-p)))))
    (when (org-check-for-hidden 'headlines) (org-hidden-tree-error))
    (call-interactively 'org-do-promote))
   ;; At an inline task.
   ((org-at-heading-p)
    (call-interactively 'org-inlinetask-promote))
   ((or (org-at-item-p)
	(and (org-region-active-p)
	     (save-excursion
	       (goto-char (region-beginning))
	       (org-at-item-p))))
    (when (org-check-for-hidden 'items) (org-hidden-tree-error))
    (call-interactively 'org-outdent-item))
   ((run-hook-with-args-until-success 'org-metaleft-final-hook))
   (t (call-interactively 'backward-word))))

(defun org-metaright (&optional _arg)
  "Demote heading, list item at point or move table column right.

In front of a drawer or a block keyword, indent it correctly.

Calls `org-do-demote', `org-indent-item', `org-table-move-column',
`org-indent-drawer' or `org-indent-block' depending on context.
With no specific context, calls the Emacs default `forward-word'.
See the individual commands for more information.

This function runs the functions in `org-metaright-hook' one by
one as a first step, and exits immediately if a function from the
hook returns non-nil.  In the absence of a specific context, the
function runs `org-metaright-final-hook' using the same logic."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-metaright-hook))
   ((org-at-table-p) (call-interactively 'org-table-move-column))
   ((org-at-drawer-p) (call-interactively 'org-indent-drawer))
   ((org-at-block-p) (call-interactively 'org-indent-block))
   ((org-with-limited-levels
     (or (org-at-heading-p)
	 (and (org-region-active-p)
	      (save-excursion
		(goto-char (region-beginning))
		(org-at-heading-p)))))
    (when (org-check-for-hidden 'headlines) (org-hidden-tree-error))
    (call-interactively 'org-do-demote))
   ;; At an inline task.
   ((org-at-heading-p)
    (call-interactively 'org-inlinetask-demote))
   ((or (org-at-item-p)
	(and (org-region-active-p)
	     (save-excursion
	       (goto-char (region-beginning))
	       (org-at-item-p))))
    (when (org-check-for-hidden 'items) (org-hidden-tree-error))
    (call-interactively 'org-indent-item))
   ((run-hook-with-args-until-success 'org-metaright-final-hook))
   (t (call-interactively 'forward-word))))

(defun org-check-for-hidden (what)
  "Check if there are hidden headlines/items in the current visual line.
WHAT can be either `headlines' or `items'.  If the current line is
an outline or item heading and it has a folded subtree below it,
this function returns t, nil otherwise."
  (let ((re (cond
	     ((eq what 'headlines) org-outline-regexp-bol)
	     ((eq what 'items) (org-item-beginning-re))
	     (t (error "This should not happen"))))
	beg end)
    (save-excursion
      (catch 'exit
	(unless (org-region-active-p)
          (setq beg (line-beginning-position))
	  (forward-line 1)
	  (while (and (not (eobp)) ;; this is like `next-line'
		      (org-invisible-p (1- (point))))
	    (forward-line 1))
	  (setq end (point))
	  (goto-char beg)
          (goto-char (line-end-position))
	  (setq end (max end (point)))
	  (while (re-search-forward re end t)
	    (when (org-invisible-p (match-beginning 0))
	      (throw 'exit t))))
	nil))))

(defun org-metaup (&optional _arg)
  "Move subtree up or move table row up.
Calls `org-move-subtree-up' or `org-table-move-row' or
`org-move-item-up', depending on context.  Everywhere else, move
backward the element at point.  See the individual commands for
more information.

This function runs the functions in `org-metaup-hook' one by one
as a first step, and exits immediately if a function from the
hook returns non-nil.  In the absence of a specific context, the
function runs `org-metaup-final-hook' using the same logic."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-metaup-hook))
   ((and (org-region-active-p)
         (org-with-limited-levels
          (save-excursion
            (goto-char (region-beginning))
            (org-at-heading-p))))
    (when (org-check-for-hidden 'headlines) (org-hidden-tree-error))
    (let ((beg (region-beginning))
          (end (region-end)))
      (save-excursion
        ;; Go a little earlier because `org-move-subtree-down' will
        ;; insert before markers and we may overshoot in some cases.
        (goto-char (max beg (1- end)))
        (setq end (point-marker))
        (goto-char beg)
        (let ((level (org-current-level)))
          (when (or (and (> level 1) (re-search-forward (format "^\\*\\{1,%s\\} " (1- level)) end t))
                    ;; Search previous subtree.
                    (progn
                      (goto-char beg)
                      (forward-line 0)
                      (not (re-search-backward (format "^\\*\\{%s\\} " level) nil t))))
            (user-error "Cannot move past superior level or buffer limit"))
          ;; Drag first subtree above below the selected.
          (while (< (point) end)
            (let ((deactivate-mark nil))
              (call-interactively 'org-move-subtree-down)))))))
   ((org-region-active-p)
    (let* ((a (save-excursion
                (goto-char (region-beginning))
                (line-beginning-position)))
           (b (save-excursion
                (goto-char (region-end))
                (if (bolp) (1- (point)) (line-end-position))))
           (c (save-excursion
                (goto-char a)
                (move-beginning-of-line 0)
                (point)))
           (d (save-excursion
                (goto-char a)
                (move-end-of-line 0)
                (point)))
           (deactivate-mark nil)
           (swap? (< (point) (mark))))
      (transpose-regions a b c d)
      (set-mark c)
      (goto-char (+ c (- b a)))
      (when swap? (exchange-point-and-mark))))
   ((org-at-table-p) (org-call-with-arg 'org-table-move-row 'up))
   ((and (featurep 'org-inlinetask)
         (org-inlinetask-in-task-p))
    (org-drag-element-backward))
   ((org-at-heading-p) (call-interactively 'org-move-subtree-up))
   ((org-at-item-p) (call-interactively 'org-move-item-up))
   ((run-hook-with-args-until-success 'org-metaup-final-hook))
   (t (org-drag-element-backward))))

(defun org-metadown (&optional _arg)
  "Move subtree down or move table row down.
Calls `org-move-subtree-down' or `org-table-move-row' or
`org-move-item-down', depending on context.  Everywhere else,
move forward the element at point.  See the individual commands
for more information.

This function runs the functions in `org-metadown-hook' one by
one as a first step, and exits immediately if a function from the
hook returns non-nil.  In the absence of a specific context, the
function runs `org-metadown-final-hook' using the same logic."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-metadown-hook))
   ((and (org-region-active-p)
         (org-with-limited-levels
          (save-excursion
            (goto-char (region-beginning))
            (org-at-heading-p))))
    (when (org-check-for-hidden 'headlines) (org-hidden-tree-error))
    (let ((beg (region-beginning))
          (end (region-end)))
      (save-excursion
        (goto-char beg)
        (setq beg (point-marker))
        (let ((level (org-current-level)))
          (when (or (and (> level 1) (re-search-forward (format "^\\*\\{1,%s\\} " (1- level)) end t))
                    ;; Search next subtree.
                    (progn
                      (goto-char end)
                      (not (re-search-forward (format "^\\*\\{%s\\} " level) nil t))))
            (user-error "Cannot move past superior level or buffer limit"))
          ;; Drag first subtree below above the selected.
          (while (> (point) beg)
            (let ((deactivate-mark nil))
              (call-interactively 'org-move-subtree-up)))))))
   ((org-region-active-p)
    (let* ((a (save-excursion
                (goto-char (region-beginning))
                (line-beginning-position)))
	   (b (save-excursion
                (goto-char (region-end))
                (if (bolp) (1- (point)) (line-end-position))))
	   (c (save-excursion
                (goto-char b)
                (move-beginning-of-line (if (bolp) 1 2))
                (point)))
	   (d (save-excursion
                (goto-char b)
                (move-end-of-line (if (bolp) 1 2))
                (point)))
           (deactivate-mark nil)
           (swap? (< (point) (mark))))
      (transpose-regions a b c d)
      (set-mark (+ 1 a (- d c)))
      (goto-char (+ 1 a (- d c) (- b a)))
      (when swap? (exchange-point-and-mark))))
   ((org-at-table-p) (call-interactively 'org-table-move-row))
   ((and (featurep 'org-inlinetask)
         (org-inlinetask-in-task-p))
    (org-drag-element-forward))
   ((org-at-heading-p) (call-interactively 'org-move-subtree-down))
   ((org-at-item-p) (call-interactively 'org-move-item-down))
   ((run-hook-with-args-until-success 'org-metadown-final-hook))
   (t (org-drag-element-forward))))

(defun org-clocktable-try-shift (dir n)
  "Check if this line starts a clock table, if yes, shift the time block."
  (when (org-match-line "^[ \t]*#\\+BEGIN:[ \t]+clocktable\\>")
    (org-clocktable-shift dir n)))

(defun org-shiftup (&optional arg)
  "Act on current element according to context.
Call `org-timestamp-up' or `org-priority-up', or
`org-previous-item', or `org-table-move-cell-up'.  See the
individual commands for more information.

This function runs the functions in `org-shiftup-hook' one by one
as a first step, and exits immediately if a function from the
hook returns non-nil.  In the absence of a specific context, the
function also runs `org-shiftup-final-hook' using the same logic.

If none of the previous steps succeed and
`org-support-shift-select' is non-nil, the function runs
`shift-select-mode' associated command.  See that variable for
more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftup-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'previous-line))
   ((org-at-timestamp-p 'lax)
    (call-interactively (if org-edit-timestamp-down-means-later
			    'org-timestamp-down 'org-timestamp-up)))
   ((and (not (eq org-support-shift-select 'always))
	 org-priority-enable-commands
	 (org-at-heading-p))
    (call-interactively 'org-priority-up))
   ((and (not org-support-shift-select) (org-at-item-p))
    (call-interactively 'org-previous-item))
   ((org-clocktable-try-shift 'up arg))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-table-p))
    (org-table-move-cell-up))
   ((run-hook-with-args-until-success 'org-shiftup-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'previous-line))
   (t (org-shiftselect-error))))

(defun org-shiftdown (&optional arg)
  "Act on current element according to context.
Call `org-timestamp-down' or `org-priority-down', or
`org-next-item', or `org-table-move-cell-down'.  See the
individual commands for more information.

This function runs the functions in `org-shiftdown-hook' one by
one as a first step, and exits immediately if a function from the
hook returns non-nil.  In the absence of a specific context, the
function also runs `org-shiftdown-final-hook' using the same
logic.

If none of the previous steps succeed and
`org-support-shift-select' is non-nil, the function runs
`shift-select-mode' associated command.  See that variable for
more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftdown-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'next-line))
   ((org-at-timestamp-p 'lax)
    (call-interactively (if org-edit-timestamp-down-means-later
			    'org-timestamp-up 'org-timestamp-down)))
   ((and (not (eq org-support-shift-select 'always))
	 org-priority-enable-commands
	 (org-at-heading-p))
    (call-interactively 'org-priority-down))
   ((and (not org-support-shift-select) (org-at-item-p))
    (call-interactively 'org-next-item))
   ((org-clocktable-try-shift 'down arg))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-table-p))
    (org-table-move-cell-down))
   ((run-hook-with-args-until-success 'org-shiftdown-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'next-line))
   (t (org-shiftselect-error))))

(defun org-shiftright (&optional arg)
  "Act on the current element according to context.
This does one of the following:

- switch a timestamp at point one day into the future
- on a headline, switch to the next TODO keyword
- on an item, switch entire list to the next bullet type
- on a property line, switch to the next allowed value
- on a clocktable definition line, move time block into the future
- in a table, move a single cell right

This function runs the functions in `org-shiftright-hook' one by
one as a first step, and exits immediately if a function from the
hook returns non-nil.  In the absence of a specific context, the
function runs `org-shiftright-final-hook' using the same logic.

If none of the above succeeds and `org-support-shift-select' is
non-nil, runs `shift-select-mode' specific command.  See that
variable for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftright-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'forward-char))
   ((org-at-timestamp-p 'lax) (call-interactively 'org-timestamp-up-day))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-heading-p))
    (let ((org-inhibit-logging
	   (not org-treat-S-cursor-todo-selection-as-state-change))
	  (org-inhibit-blocking
	   (not org-treat-S-cursor-todo-selection-as-state-change)))
      (org-call-with-arg 'org-todo 'right)))
   ((or (and org-support-shift-select
	     (not (eq org-support-shift-select 'always))
	     (org-at-item-bullet-p))
	(and (not org-support-shift-select) (org-at-item-p)))
    (org-call-with-arg 'org-cycle-list-bullet nil))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-property-p))
    (call-interactively 'org-property-next-allowed-value))
   ((org-clocktable-try-shift 'right arg))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-table-p))
    (org-table-move-cell-right))
   ((run-hook-with-args-until-success 'org-shiftright-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'forward-char))
   (t (org-shiftselect-error))))

(defun org-shiftleft (&optional arg)
  "Act on current element according to context.
This does one of the following:

- switch a timestamp at point one day into the past
- on a headline, switch to the previous TODO keyword.
- on an item, switch entire list to the previous bullet type
- on a property line, switch to the previous allowed value
- on a clocktable definition line, move time block into the past
- in a table, move a single cell left

This function runs the functions in `org-shiftleft-hook' one by
one as a first step, and exits immediately if a function from the
hook returns non-nil.  In the absence of a specific context, the
function runs `org-shiftleft-final-hook' using the same logic.

If none of the above succeeds and `org-support-shift-select' is
non-nil, runs `shift-select-mode' specific command.  See that
variable for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftleft-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'backward-char))
   ((org-at-timestamp-p 'lax) (call-interactively 'org-timestamp-down-day))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-heading-p))
    (let ((org-inhibit-logging
	   (not org-treat-S-cursor-todo-selection-as-state-change))
	  (org-inhibit-blocking
	   (not org-treat-S-cursor-todo-selection-as-state-change)))
      (org-call-with-arg 'org-todo 'left)))
   ((or (and org-support-shift-select
	     (not (eq org-support-shift-select 'always))
	     (org-at-item-bullet-p))
	(and (not org-support-shift-select) (org-at-item-p)))
    (org-call-with-arg 'org-cycle-list-bullet 'previous))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-property-p))
    (call-interactively 'org-property-previous-allowed-value))
   ((org-clocktable-try-shift 'left arg))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-table-p))
    (org-table-move-cell-left))
   ((run-hook-with-args-until-success 'org-shiftleft-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'backward-char))
   (t (org-shiftselect-error))))

(defun org-shiftcontrolright ()
  "Switch to next TODO set."
  (interactive)
  (cond
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'forward-word))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-heading-p))
    (org-call-with-arg 'org-todo 'nextset))
   (org-support-shift-select
    (org-call-for-shift-select 'forward-word))
   (t (org-shiftselect-error))))

(defun org-shiftcontrolleft ()
  "Switch to previous TODO set."
  (interactive)
  (cond
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'backward-word))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-heading-p))
    (org-call-with-arg 'org-todo 'previousset))
   (org-support-shift-select
    (org-call-for-shift-select 'backward-word))
   (t (org-shiftselect-error))))

(defun org-shiftcontrolup (&optional n)
  "Change timestamps synchronously up in CLOCK log lines.
Optional argument N tells to change by that many units."
  (interactive "P")
  (if (and (org-at-clock-log-p) (org-at-timestamp-p 'lax))
      (let (org-support-shift-select)
	(org-clock-timestamps-up n))
    (user-error "Not at a clock log")))

(defun org-shiftcontroldown (&optional n)
  "Change timestamps synchronously down in CLOCK log lines.
Optional argument N tells to change by that many units."
  (interactive "P")
  (if (and (org-at-clock-log-p) (org-at-timestamp-p 'lax))
      (let (org-support-shift-select)
	(org-clock-timestamps-down n))
    (user-error "Not at a clock log")))

(defun org-increase-number-at-point (&optional inc)
  "Increment the number at point.
With an optional prefix numeric argument INC, increment using
this numeric value."
  (interactive "p")
  (if (not (number-at-point))
      (user-error "Not on a number")
    (unless inc (setq inc 1))
    (let ((pos (point))
	  (beg (skip-chars-backward "-+^/*0-9eE."))
	  (end (skip-chars-forward "-+^/*0-9eE.")) nap)
      (setq nap (buffer-substring-no-properties
		 (+ pos beg) (+ pos beg end)))
      (delete-region (+ pos beg) (+ pos beg end))
      (insert (calc-eval (concat (number-to-string inc) "+" nap))))
    (when (org-at-table-p)
      (org-table-align)
      (org-table-end-of-field 1))))

(defun org-decrease-number-at-point (&optional inc)
  "Decrement the number at point.
With an optional prefix numeric argument INC, decrement using
this numeric value."
  (interactive "p")
  (org-increase-number-at-point (- (or inc 1))))

(defun org-ctrl-c-ret ()
  "Call `org-table-hline-and-move' or `org-insert-heading'."
  (interactive)
  (cond
   ((org-at-table-p) (call-interactively 'org-table-hline-and-move))
   (t (call-interactively 'org-insert-heading))))

(defun org-edit-special (&optional arg)
  "Call a special editor for the element at point.
When at a table, call the formula editor with `org-table-edit-formulas'.
When at table.el table, edit it in dedicated buffer.
When in a source code block, call `org-edit-src-code'; with prefix
  argument, switch to session buffer.
When in an example block, call `org-edit-src-code'.
When in an inline code block, call `org-edit-inline-src-code'.
When in a fixed-width region, call `org-edit-fixed-width-region'.
When in an export block, call `org-edit-export-block'.
When in a comment block, call `org-edit-comment-block'.
When in a LaTeX environment, call `org-edit-latex-environment'.
When at an INCLUDE, SETUPFILE or BIBLIOGRAPHY keyword, visit the included file.
When at a footnote reference, call `org-edit-footnote-reference'.
When at a planning line call, `org-deadline' and/or `org-schedule'.
When at an active timestamp, call `org-timestamp'.
When at an inactive timestamp, call `org-timestamp-inactive'.
On a link, call `ffap' to visit the link at point.
Otherwise, return a user error."
  (interactive "P")
  (let ((element (org-element-at-point)))
    (barf-if-buffer-read-only)
    (pcase (org-element-type element)
      (`src-block
       (if (not arg) (org-edit-src-code)
	 (let* ((info (org-babel-get-src-block-info))
		(lang (nth 0 info))
		(params (nth 2 info))
		(session (cdr (assq :session params))))
	   (if (not session) (org-edit-src-code)
	     ;; At a source block with a session and function called
	     ;; with an ARG: switch to the buffer related to the
	     ;; inferior process.
	     (switch-to-buffer
	      (funcall (intern (concat "org-babel-prep-session:" lang))
		       session params))))))
      (`keyword
       (unless (member (org-element-property :key element)
		       '("BIBLIOGRAPHY" "INCLUDE" "SETUPFILE"))
	 (user-error "No special environment to edit here"))
       (let ((value (org-element-property :value element)))
	 (unless (org-string-nw-p value) (user-error "No file to edit"))
	 (let ((file (and (string-match "\\`\"\\(.*?\\)\"\\|\\S-+" value)
			  (or (match-string 1 value)
			      (match-string 0 value)))))
	   (when (org-url-p file)
	     (user-error "Files located with a URL cannot be edited"))
	   (org-link-open-from-string
	    (format "[[%s]]" (expand-file-name file))))))
      (`table
       (if (eq (org-element-property :type element) 'table.el)
           (org-edit-table.el)
         (call-interactively 'org-table-edit-formulas)))
      ;; Only Org tables contain `table-row' type elements.
      (`table-row (call-interactively 'org-table-edit-formulas))
      (`example-block (org-edit-src-code))
      (`export-block (org-edit-export-block))
      (`comment-block (org-edit-comment-block))
      (`fixed-width (org-edit-fixed-width-region))
      (`latex-environment (org-edit-latex-environment))
      (`planning
       (let ((proplist (cadr element)))
         (mapc #'call-interactively
               (remq nil
                     (list
                      (when (plist-get proplist :deadline) #'org-deadline)
                      (when (plist-get proplist :scheduled) #'org-schedule))))))
      (_
       ;; No notable element at point.  Though, we may be at a link or
       ;; a footnote reference, which are objects.  Thus, scan deeper.
       (let ((context (org-element-context element)))
	 (pcase (org-element-type context)
	   (`footnote-reference (org-edit-footnote-reference))
	   (`inline-src-block (org-edit-inline-src-code))
	   (`latex-fragment (org-edit-latex-fragment))
	   (`timestamp (if (eq 'inactive (org-element-property :type context))
			   (call-interactively #'org-timestamp-inactive)
			 (call-interactively #'org-timestamp)))
	   (`link (call-interactively #'ffap))
	   (_ (user-error "No special environment to edit here"))))))))

(defun org-ctrl-c-ctrl-c (&optional arg)
  "Set tags in headline, or update according to changed information at point.

This command does many different things, depending on context:

- If column view is active, in agenda or org buffers, quit it.

- If there are highlights, remove them.

- If a function in `org-ctrl-c-ctrl-c-hook' recognizes this location,
  this is what we do.

- If the cursor is on a statistics cookie, update it.

- If the cursor is in a headline, in an agenda or an org buffer,
  prompt for tags and insert them into the current line, aligned
  to `org-tags-column'.  When called with prefix arg, realign all
  tags in the current buffer.

- If the cursor is in one of the special #+KEYWORD lines, this
  triggers scanning the buffer for these lines and updating the
  information.

- If the cursor is inside a table, realign the table.  This command
  works even if the automatic table editor has been turned off.

- If the cursor is on a #+TBLFM line, re-apply the formulas to
  the entire table.

- If the cursor is at a footnote reference or definition, jump to
  the corresponding definition or references, respectively.

- If the cursor is a the beginning of a dynamic block, update it.

- If the current buffer is a capture buffer, close note and file it.

- If the cursor is on a <<<target>>>, update radio targets and
  corresponding links in this buffer.

- If the cursor is on a numbered item in a plain list, renumber the
  ordered list.

- If the cursor is on a checkbox, toggle it.

- If the cursor is on a code block, evaluate it.  The variable
  `org-confirm-babel-evaluate' can be used to control prompting
  before code block evaluation, by default every code block
  evaluation requires confirmation.  Code block evaluation can be
  inhibited by setting `org-babel-no-eval-on-ctrl-c-ctrl-c'."
  (interactive "P")
  (cond
   ((bound-and-true-p org-columns-overlays) (org-columns-quit))
   ((or (bound-and-true-p org-clock-overlays) org-occur-highlights)
    (when (boundp 'org-clock-overlays) (org-clock-remove-overlays))
    (org-remove-occur-highlights)
    (message "Temporary highlights/overlays removed from current buffer"))
   ((and (local-variable-p 'org-finish-function)
	 (fboundp org-finish-function))
    (funcall org-finish-function))
   ((org-babel-hash-at-point))
   ((run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-hook))
   (t
    (let* ((context
	    (org-element-lineage
	     (org-element-context)
	     ;; Limit to supported contexts.
	     '(babel-call clock dynamic-block footnote-definition
			  footnote-reference inline-babel-call inline-src-block
			  inlinetask item keyword node-property paragraph
			  plain-list planning property-drawer radio-target
			  src-block statistics-cookie table table-cell table-row
			  timestamp)
	     t))
	   (radio-list-p (org-at-radio-list-p))
	   (type (org-element-type context)))
      ;; For convenience: at the first line of a paragraph on the same
      ;; line as an item, apply function on that item instead.
      (when (eq type 'paragraph)
	(let ((parent (org-element-parent context)))
	  (when (and (org-element-type-p parent 'item)
		     (= (line-beginning-position)
			(org-element-begin parent)))
	    (setq context parent)
	    (setq type 'item))))
      ;; Act according to type of element or object at point.
      ;;
      ;; Do nothing on a blank line, except if it is contained in
      ;; a source block.  Hence, we first check if point is in such
      ;; a block and then if it is at a blank line.
      (pcase type
	((or `inline-src-block `src-block)
	 (unless org-babel-no-eval-on-ctrl-c-ctrl-c
	   (org-babel-eval-wipe-error-buffer)
	   (org-babel-execute-src-block
	    current-prefix-arg (org-babel-get-src-block-info nil context))))
	((guard (org-match-line "[ \t]*$"))
	 (or (run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-final-hook)
	     (user-error
	      (substitute-command-keys
	       "`\\[org-ctrl-c-ctrl-c]' can do nothing useful here"))))
	((or `babel-call `inline-babel-call)
	 (let ((info (org-babel-lob-get-info context)))
	   (when info (org-babel-execute-src-block nil info nil type))))
	(`clock
         (if (org-at-timestamp-p 'lax)
             ;; Update the timestamp as well.  `org-timestamp-change'
             ;; will call `org-clock-update-time-maybe'.
             (org-timestamp-change 0 'day)
           (org-clock-update-time-maybe)))
	(`dynamic-block
	 (save-excursion
	   (goto-char (org-element-post-affiliated context))
	   (org-update-dblock)))
	(`footnote-definition
	 (goto-char (org-element-post-affiliated context))
	 (call-interactively 'org-footnote-action))
	(`footnote-reference (call-interactively #'org-footnote-action))
	((or `headline `inlinetask)
	 (save-excursion (goto-char (org-element-begin context))
			 (call-interactively #'org-set-tags-command)))
	(`item
	 ;; At an item: `C-u C-u' sets checkbox to "[-]"
	 ;; unconditionally, whereas `C-u' will toggle its presence.
	 ;; Without a universal argument, if the item has a checkbox,
	 ;; toggle it.  Otherwise repair the list.
	 (if (or radio-list-p
		 (and (boundp org-list-checkbox-radio-mode)
		      org-list-checkbox-radio-mode))
	     (org-toggle-radio-button arg)
	   (let* ((box (org-element-property :checkbox context))
		  (struct (org-element-property :structure context))
		  (old-struct (copy-tree struct))
		  (parents (org-list-parents-alist struct))
		  (prevs (org-list-prevs-alist struct))
		  (orderedp (org-not-nil (org-entry-get nil "ORDERED"))))
	     (org-list-set-checkbox
	      (org-element-begin context) struct
	      (cond ((equal arg '(16)) "[-]")
		    ((and (not box) (equal arg '(4))) "[ ]")
		    ((or (not box) (equal arg '(4))) nil)
		    ((eq box 'on) "[ ]")
		    (t "[X]")))
	     ;; Mimic `org-list-write-struct' but with grabbing a return
	     ;; value from `org-list-struct-fix-box'.
	     (org-list-struct-fix-ind struct parents 2)
	     (org-list-struct-fix-item-end struct)
	     (org-list-struct-fix-bul struct prevs)
	     (org-list-struct-fix-ind struct parents)
	     (let ((block-item
		    (org-list-struct-fix-box struct parents prevs orderedp)))
	       (if (and box (equal struct old-struct))
		   (if (equal arg '(16))
		       (message "Checkboxes already reset")
		     (user-error "Cannot toggle this checkbox: %s"
				 (if (eq box 'on)
				     "all subitems checked"
				   "unchecked subitems")))
		 (org-list-struct-apply-struct struct old-struct)
		 (org-update-checkbox-count-maybe))
	       (when block-item
		 (message "Checkboxes were removed due to empty box at line %d"
			  (org-current-line block-item)))))))
	(`plain-list
	 ;; At a plain list, with a double C-u argument, set
	 ;; checkboxes of each item to "[-]", whereas a single one
	 ;; will toggle their presence according to the state of the
	 ;; first item in the list.  Without an argument, repair the
	 ;; list.
	 (if (or radio-list-p
		 (and (boundp org-list-checkbox-radio-mode)
		      org-list-checkbox-radio-mode))
	     (org-toggle-radio-button arg)
	   (let* ((begin (org-element-contents-begin context))
		  (struct (org-element-property :structure context))
		  (old-struct (copy-tree struct))
		  (first-box (save-excursion
			       (goto-char begin)
			       (looking-at org-list-full-item-re)
			       (match-string-no-properties 3)))
		  (new-box (cond ((equal arg '(16)) "[-]")
				 ((equal arg '(4)) (unless first-box "[ ]"))
				 ((equal first-box "[X]") "[ ]")
				 (t "[X]"))))
	     (cond
	      (arg
	       (dolist (pos
			(org-list-get-all-items
			 begin struct (org-list-prevs-alist struct)))
		 (org-list-set-checkbox pos struct new-box)))
	      ((and first-box (eq (point) begin))
	       ;; For convenience, when point is at bol on the first
	       ;; item of the list and no argument is provided, simply
	       ;; toggle checkbox of that item, if any.
	       (org-list-set-checkbox begin struct new-box)))
	     (when (equal
		    (org-list-write-struct
		     struct (org-list-parents-alist struct) old-struct)
		    old-struct)
	       (message "Cannot update this checkbox"))
	     (org-update-checkbox-count-maybe))))
	(`keyword
	 (let ((org-inhibit-startup-visibility-stuff t)
	       (org-startup-align-all-tables nil))
	   (when (boundp 'org-table-coordinate-overlays)
	     (mapc #'delete-overlay org-table-coordinate-overlays)
	     (setq org-table-coordinate-overlays nil))
	   (org-save-outline-visibility 'use-markers (org-mode-restart)))
	 (message "Local setup has been refreshed"))
	((or `property-drawer `node-property)
	 (call-interactively #'org-property-action))
	(`radio-target
	 (call-interactively #'org-update-radio-target-regexp))
	(`statistics-cookie
	 (call-interactively #'org-update-statistics-cookies))
	((or `table `table-cell `table-row)
	 ;; At a table, generate a plot if on the #+plot line,
         ;; recalculate every field and align it otherwise.  Also
	 ;; send the table if necessary.
         (cond
          ((and (org-match-line "[ \t]*#\\+plot:")
                (< (point) (org-element-post-affiliated context)))
           (org-plot/gnuplot))
          ;; If the table has a `table.el' type, just give up.
          ((eq (org-element-property :type context) 'table.el)
           (message "%s" (substitute-command-keys "\\<org-mode-map>\
Use `\\[org-edit-special]' to edit table.el tables")))
          ;; At a table row or cell, maybe recalculate line but always
	  ;; align table.
          ((or (eq type 'table)
               ;; Check if point is at a TBLFM line.
               (and (eq type 'table-row)
                    (= (point) (org-element-end context))))
           (save-excursion
             (if (org-at-TBLFM-p)
                 (progn (require 'org-table)
                        (org-table-calc-current-TBLFM))
               (goto-char (org-element-contents-begin context))
               (org-call-with-arg 'org-table-recalculate (or arg t))
               (orgtbl-send-table 'maybe))))
          (t
           (org-table-maybe-eval-formula)
           (cond (arg (call-interactively #'org-table-recalculate))
                 ((org-table-maybe-recalculate-line))
                 (t (org-table-align))))))
	((or `timestamp (and `planning (guard (org-at-timestamp-p 'lax))))
	 (org-timestamp-change 0 'day))
	((and `nil (guard (org-at-heading-p)))
	 ;; When point is on an unsupported object type, we can miss
	 ;; the fact that it also is at a heading.  Handle it here.
	 (call-interactively #'org-set-tags-command))
	((guard
	  (run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-final-hook)))
	(_
	 (user-error
	  (substitute-command-keys
	   "`\\[org-ctrl-c-ctrl-c]' can do nothing useful here"))))))))

(defvar org-finish-function nil
  "Function to be called when \\`C-c C-c' is used.
This is for getting out of special buffers like capture.")

(defun org-kill-note-or-show-branches ()
  "Abort storing current note, or show just branches."
  (interactive)
  (cond (org-finish-function
	 (let ((org-note-abort t)) (funcall org-finish-function)))
	((org-before-first-heading-p)
	 (org-fold-show-branches-buffer)
	 (org-fold-hide-archived-subtrees (point-min) (point-max)))
	(t
	 (let ((beg (progn (org-back-to-heading) (point)))
	       (end (save-excursion (org-end-of-subtree t t) (point))))
	   (org-fold-hide-subtree)
	   (org-fold-show-branches)
	   (org-fold-hide-archived-subtrees beg end)))))

(defun org-ctrl-c-tab (&optional arg)
  "Toggle columns width in a table, or show children.
Call `org-table-toggle-column-width' if point is in a table.
Otherwise provide a compact view of the children.  ARG is the
level to hide."
  (interactive "p")
  (cond
   ((org-at-table-p)
    (call-interactively #'org-table-toggle-column-width))
   ((org-before-first-heading-p)
    (save-excursion
      (org-fold-flag-above-first-heading)
      (org-fold-hide-sublevels (or arg 1))))
   (t
    (org-fold-hide-subtree)
    (org-fold-show-children arg))))

(defun org-ctrl-c-star ()
  "Compute table, or change heading status of lines.
Calls `org-table-recalculate' or `org-toggle-heading',
depending on context."
  (interactive)
  (cond
   ((org-at-table-p)
    (call-interactively 'org-table-recalculate))
   (t
    ;; Convert all lines in region to list items
    (call-interactively 'org-toggle-heading))))

(defun org-ctrl-c-minus ()
  "Insert separator line in table or modify bullet status of line.
Also turns a plain line or a region of lines into list items.
Calls `org-table-insert-hline', `org-toggle-item', or
`org-cycle-list-bullet', depending on context."
  (interactive)
  (cond
   ((org-at-table-p)
    (call-interactively 'org-table-insert-hline))
   ((org-region-active-p)
    (call-interactively 'org-toggle-item))
   ((org-in-item-p)
    (call-interactively 'org-cycle-list-bullet))
   (t
    (call-interactively 'org-toggle-item))))

(defun org-toggle-heading (&optional nstars)
  "Convert headings to normal text, or items or text to headings.
If there is no active region, only convert the current line.

With a `\\[universal-argument]' prefix, convert the whole list at
point into heading.

In a region:

- If the first non blank line is a headline, remove the stars
  from all headlines in the region.

- If it is a normal line, turn each and every normal line (i.e.,
  not an heading or an item) in the region into headings.  If you
  want to convert only the first line of this region, use one
  universal prefix argument.

- If it is a plain list item, turn all plain list items into headings.
  The checkboxes are converted to appropriate TODO or DONE keywords
  (using `car' or `org-done-keywords' and `org-not-done-keywords' when
  available).

When converting a line into a heading, the number of stars is chosen
such that the lines become children of the current entry.  However,
when a numeric prefix argument is given, its value determines the
number of stars to add."
  (interactive "P")
  (let ((skip-blanks
	 ;; Return beginning of first non-blank line, starting from
	 ;; line at POS.
	 (lambda (pos)
	   (save-excursion
	     (goto-char pos)
	     (while (org-at-comment-p) (forward-line))
	     (skip-chars-forward " \r\t\n")
             (line-beginning-position))))
	beg end toggled)
    ;; Determine boundaries of changes.  If a universal prefix has
    ;; been given, put the list in a region.  If region ends at a bol,
    ;; do not consider the last line to be in the region.

    (when (and current-prefix-arg (org-at-item-p))
      (when (listp current-prefix-arg) (setq current-prefix-arg 1))
      (org-mark-element))

    (if (org-region-active-p)
	(setq beg (funcall skip-blanks (region-beginning))
	      end (copy-marker (save-excursion
				 (goto-char (region-end))
                                 (if (bolp) (point) (line-end-position)))))
      (setq beg (funcall skip-blanks (line-beginning-position))
            end (copy-marker (line-end-position))))
    ;; Ensure inline tasks don't count as headings.
    (org-with-limited-levels
     (save-excursion
       (goto-char beg)
       (cond
	;; Case 1. Started at an heading: de-star headings.
	((org-at-heading-p)
	 (while (< (point) end)
	   (when (org-at-heading-p)
	     (looking-at org-outline-regexp) (replace-match "")
	     (setq toggled t))
	   (forward-line)))
	;; Case 2. Started at an item: change items into headlines.
	;;         One star will be added by `org-list-to-subtree'.
	((org-at-item-p)
	 (while (< (point) end)
	   (when (org-at-item-p)
	     ;; Pay attention to cases when region ends before list.
	     (let* ((struct (org-list-struct))
		    (list-end
		     (min (org-list-get-bottom-point struct) (1+ end))))
	       (save-restriction
		 (narrow-to-region (point) list-end)
		 (insert (org-list-to-subtree
			  (org-list-to-lisp t)
			  (pcase (org-current-level)
			    (`nil 1)
			    (l (1+ (org-reduced-level l))))
                          ;; Keywords to replace checkboxes.
                          (list
                           ;; [X]
                           :cbon (concat (or (car org-done-keywords) "DONE") " ")
                           ;; [ ]
                           :cboff (concat (or (car org-not-done-keywords) "TODO") " ")
                           ;; [-]
                           :cbtrans (concat (or (car org-not-done-keywords) "TODO") " ")))
			 "\n")))
	     (setq toggled t))
	   (forward-line)))
	;; Case 3. Started at normal text: make every line an heading,
	;;         skipping headlines and items.
	(t (let* ((stars
		   (make-string
		    (if (numberp nstars) nstars (or (org-current-level) 0)) ?*))
		  (add-stars
		   (cond (nstars "")	; stars from prefix only
			 ((equal stars "") "*")	; before first heading
			 (org-odd-levels-only "**") ; inside heading, odd
			 (t "*")))	; inside heading, oddeven
		  (rpl (concat stars add-stars " "))
		  (lend (when (listp nstars) (save-excursion (end-of-line) (point)))))
	     (while (< (point) (if (equal nstars '(4)) lend end))
	       (when (and (not (or (org-at-heading-p) (org-at-item-p) (org-at-comment-p)))
			  (looking-at "\\([ \t]*\\)\\(\\S-\\)"))
		 (replace-match (concat rpl (match-string 2))) (setq toggled t))
	       (forward-line)))))))
    (unless toggled (message "Cannot toggle heading from here"))))

(defun org-meta-return (&optional arg)
  "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading', `org-insert-item' or
`org-table-wrap-region', depending on context.  When called with
an argument, unconditionally call `org-insert-heading'."
  (interactive "P")
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (call-interactively (cond (arg #'org-insert-heading)
				((org-at-table-p) #'org-table-wrap-region)
				((org-in-item-p) #'org-insert-item)
				(t #'org-insert-heading)))))

(provide 'org-edit-special)

;;; org-edit-special.el ends here