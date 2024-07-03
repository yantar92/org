;;; org-mode-common.el --- Common definitions for Org major mode                      -*- lexical-binding: t; -*-

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

;; This library contains common variables defined inside Org mode
;; buffers.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'outline)

(defvar org-inhibit-startup nil
  "When non-nil, `org-mode' will skip \"startup\" settings.
These settings include initial visibility/folding, auto-alignment,
auto-previews, and minor modes triggered by \"startup\" options: line
truncation, `org-num-mode', `org-indent-mode', and `org-beamer-mode'.")

(defvar org-inhibit-startup-visibility-stuff nil
  "When non-nil, `org-mode' will not setup initial visibility.")

(defvar org-mode-loading nil
  "Non-nil during Org mode initialization.")

(defvar org-window-configuration nil
  "Used in various places to store a window configuration.")

;; FIXME: We should separate alignment of table text and visual
;; alignment dependent on the buffer display.  The text alignment is
;; useful when reading Org files as plain text, while visual alignment
;; only makes sense in Emacs and should ideally be handled by
;; font-lock.  Once separated, we can limit usage of this variable to
;; org-table-* internals.
(defvar org-table-may-need-update t
  "Indicates that a table might need an update.
This variable is set by `org-before-change-function'.
`org-table-align' sets it back to nil.")

(defcustom org-remove-highlights-with-change t
  "Non-nil means any change to the buffer will remove temporary highlights.
\\<org-mode-map>\
Such highlights are created by `org-occur' and `org-clock-display'.
When nil, `\\[org-ctrl-c-ctrl-c]' needs to be used \
to get rid of the highlights.
The highlights created by `org-latex-preview' always need
`\\[org-latex-preview]' to be removed."
  :group 'org-sparse-trees
  :group 'org-time
  :type 'boolean)

(defvar org-inhibit-highlight-removal nil
  "When non-nil, do not remove overlay highlights from Org buffers.
This affects table overlays when editing formulas, sparse tree
highlights, and clock overlays.")

(defvar-local org-todo-log-states nil
  "Logging settings for todo keywords.
The value is a list with each element representing logging settings
for a given todo keyword:

 (KWD LOG-WHEN-SETTING-TO-KWD LOG-WHEN-SETTING-FROM-KWD)
 
LOG-WHEN-SETTING-FROM/TO-KWD is either nil, symbol `note', or symbol
`time' - do not log, log with interactive note, or log time
respectively.

This variable is set according to the todo keyword sequence settings
provided in `org-todo-keywords', #+TODO, #+SEQ_TODO, or #+TYP_TODO
in-buffer keywords (see `org-set-regexps-and-options').
This variable is ignored when a given heading has LOGGING property.")

(defvar-local org-link-abbrev-alist-local nil
  "Buffer-local version of `org-link-abbrev-alist', which see.
The value of this is taken from the LINK keywords.")

;; FIXME: Org parser depends on the syntax table implicitly.  We may
;; consider moving this table to Org parser and even defining it
;; without inheritance to make Org syntax more deterministic.
(defvar org-mode-syntax-table
  (let ((st (make-syntax-table outline-mode-syntax-table)))
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "_" st)
    (modify-syntax-entry ?~ "_" st)
    (modify-syntax-entry ?< "(>" st)
    (modify-syntax-entry ?> ")<" st)
    st)
  "Standard syntax table for Org mode buffers.")

(defvar org-mode-tags-syntax-table
  (let ((st (make-syntax-table org-mode-syntax-table)))
    (modify-syntax-entry ?@ "w" st)
    (modify-syntax-entry ?_ "w" st)
    st)
  "Syntax table including \"@\" and \"_\" as word constituents.")

(defvar org-display-table nil
  "The display table for Org mode, in case `org-ellipsis' is non-nil.")

(defvar org-finish-function nil
  "Function to be called when \\`C-c C-c' is used.
This is for getting out of special buffers like capture.")

;;; Variables for pre-computed regular expressions, all buffer local

(defvar-local org-category nil
  "Variable used by Org files to set a category for agenda display.
There are multiple ways to set the category.  One way is to set
it in the document property drawer.  For example:

:PROPERTIES:
:CATEGORY: ELisp
:END:

Other ways to define it is as an Emacs file variable, for example

#   -*- mode: org; org-category: \"ELisp\"

or for the file to contain a special line:

#+CATEGORY: ELisp

If the file does not specify a category, then file's base name
is used instead.")
(put 'org-category 'safe-local-variable (lambda (x) (or (symbolp x) (stringp x))))

(defvar-local org-todo-regexp nil
  "Matches any of the TODO state keywords.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")

(defvar-local org-not-done-regexp nil
  "Matches any of the TODO state keywords except the last one.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")

(defvar-local org-not-done-heading-regexp nil
  "Matches a TODO headline that is not done.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")

(defvar-local org-todo-line-regexp nil
  "Matches a headline and puts TODO state into group 2 if present.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")

(defvar-local org-complex-heading-regexp nil
  "Matches a headline and puts everything into groups:

group 1: Stars
group 2: The TODO keyword, maybe
group 3: Priority cookie
group 4: True headline
group 5: Tags

Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")

(defvar-local org-complex-heading-regexp-format nil
  "Printf format to make regexp to match an exact headline.
This regexp will match the headline of any node which has the
exact headline text that is put into the format, but may have any
TODO state, priority, tags, statistics cookies (at the beginning
or end of the headline title), or COMMENT keyword.")

(defvar-local org-todo-line-tags-regexp nil
  "Matches a headline and puts TODO state into group 2 if present.
Also put tags into group 4 if tags are present.")

(defcustom org-loop-over-headlines-in-active-region t
  "Shall some commands act upon headlines in the active region?

When set to t, some commands will be performed in all headlines
within the active region.

When set to `start-level', some commands will be performed in all
headlines within the active region, provided that these headlines
are of the same level than the first one.

When set to a string, those commands will be performed on the
matching headlines within the active region.  Such string must be
a tags/property/todo match as it is used in the agenda tags view.

The list of commands is: `org-schedule', `org-deadline',
`org-todo', `org-set-tags-command', `org-archive-subtree',
`org-archive-set-tag', `org-toggle-archive-tag' and
`org-archive-to-archive-sibling'.  The archiving commands skip
already archived entries.

See `org-agenda-loop-over-headlines-in-active-region' for the
equivalent option for agenda views."
  :type '(choice (const :tag "Don't loop" nil)
		 (const :tag "All headlines in active region" t)
		 (const :tag "In active region, headlines at the same level than the first one" start-level)
		 (string :tag "Tags/Property/Todo matcher"))
  :package-version '(Org . "9.4")
  :group 'org-todo
  :group 'org-archive)

(defun org--update-property-plist (key val props)
  "Associate KEY to VAL in alist PROPS.
Modifications are made by side-effect.  Return new alist."
  (let* ((appending (string= (substring key -1) "+"))
	 (key (if appending (substring key 0 -1) key))
	 (old (assoc-string key props t)))
    (if (not old) (cons (cons key val) props)
      (setcdr old (if appending (concat (cdr old) " " val) val))
      props)))

(defun org-tag-alist-to-groups (alist)
  "Return group alist from tag ALIST.
ALIST is an alist, as defined in `org-tag-alist' or
`org-tag-persistent-alist', or produced with
`org-tag-string-to-alist'.  Return value is an alist following
the pattern (GROUP-TAG TAGS) where GROUP-TAG is the tag, as
a string, summarizing TAGS, as a list of strings."
  (let (groups group-status current-group)
    (dolist (token alist (nreverse groups))
      (pcase token
	(`(,(or :startgroup :startgrouptag)) (setq group-status t))
	(`(,(or :endgroup :endgrouptag))
	 (when (eq group-status 'append)
	   (push (nreverse current-group) groups))
	 (setq group-status nil current-group nil))
	(`(:grouptags) (setq group-status 'append))
	((and `(,tag . ,_) (guard group-status))
	 (if (eq group-status 'append) (push tag current-group)
	   (setq current-group (list tag))))
	(_ nil)))))

(defvar org--fast-selection-keys
  (string-to-list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~")
  "List of chars to be used as bindings by `org-fast-tag-selection'.")

(defun org-assign-fast-keys (alist)
  "Assign fast keys to a keyword-key alist.
Respect keys that are already there."
  (let (new e (candidate-key-list org--fast-selection-keys))
    (while (setq e (pop alist))
      (if (or (memq (car e) '(:newline :grouptags :endgroup :startgroup))
	      (cdr e)) ;; Key already assigned.
	  (push e new)
	(let ((clist (string-to-list (downcase (car e))))
	      (used (append new alist)))
	  (when (= (car clist) ?@)
	    (pop clist))
	  (while (and clist (rassoc (car clist) used))
	    (pop clist))
	  (unless clist
	    (while (and candidate-key-list
                        (rassoc (car candidate-key-list) used))
	      (setq candidate-key-list (cdr candidate-key-list))))
	  (push (cons (car e) (or (car clist) (car candidate-key-list)))
                new))))
    (nreverse new)))

(defun org--settings-add-to-alist (alist1 alist2)
  "Merge tags/todo keyword settings from ALIST1 into ALIST2.

Duplicates tags/keywords outside a group are removed.  Keywords and
order are preserved.

The function assumes ALIST1 and ALIST2 are proper tag/todo keyword
alists.  See `org-tag-alist' for their structure."
  (cond
   ((null alist2) alist1)
   ((null alist1) alist2)
   (t
    (let ((to-add nil)
	  (group-flag nil))
      (dolist (tag-pair alist1)
	(pcase tag-pair
	  (`(,(or :startgrouptag :startgroup))
	   (setq group-flag t)
	   (push tag-pair to-add))
	  (`(,(or :endgrouptag :endgroup))
	   (setq group-flag nil)
	   (push tag-pair to-add))
	  (`(,(or :grouptags :newline))
	   (push tag-pair to-add))
	  (`(,tag . ,_)
	   ;; Remove duplicates from ALIST1, unless they are in
	   ;; a group.  Indeed, it makes sense to have a tag appear in
	   ;; multiple groups.
	   (when (or group-flag (not (assoc tag alist2)))
	     (push tag-pair to-add)))
	  (_ (error "Invalid association in tag alist: %S" tag-pair))))
      ;; Preserve order of ALIST1.
      (append (nreverse to-add) alist2)))))

(defun org-restart-font-lock ()
  "Restart `font-lock-mode', to force refontification."
  (when font-lock-mode
    (font-lock-mode -1)
    (font-lock-mode 1)))

(provide 'org-mode-common)

;;; org-mode-common.el ends here
