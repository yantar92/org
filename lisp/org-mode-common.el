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

(defvar org-inhibit-startup nil)        ; Dynamically-scoped param.
(defvar org-inhibit-startup-visibility-stuff nil) ; Dynamically-scoped param.

(defvar org-mode-loading nil
  "Non-nil during Org mode initialization.")

(defvar org-window-configuration nil
  "Used in various places to store a window configuration.")

(defvar-local org-done-keywords nil)
(defvar-local org-todo-heads nil)
(defvar-local org-todo-sets nil)
(defvar-local org-todo-kwd-alist nil)
(defvar-local org-todo-key-alist nil)
(defvar-local org-todo-key-trigger nil)
(defvar-local org-todo-log-states nil)
(defvar-local org-todo-keywords-1 nil
  "All TODO and DONE keywords active in a buffer.")

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


(defvar-local org-current-tag-alist nil
  "Alist of all tag groups in current buffer.
This variable takes into consideration `org-tag-alist',
`org-tag-persistent-alist' and TAGS keywords in the buffer.")

(defvar-local org-not-done-keywords nil)
(defvar-local org-tag-groups-alist nil)

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

(provide 'org-mode-common)

;;; org-mode-common.el ends here
