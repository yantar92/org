;;; org-agenda-common.el --- Global agenda definitions         -*- lexical-binding: t; -*-

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
;; This library defines global agenda state and helper to manipulate
;; this state.

;;; Code:

(defvar org-agenda-buffer nil
  "Agenda buffer currently being generated.")
(defvar org-agenda-contributing-files nil)

(defcustom org-agenda-sticky nil
  "Non-nil means agenda q key will bury agenda buffers.
Agenda commands will then show existing buffer instead of generating new ones.
When nil, `q' will kill the single agenda buffer."
  :group 'org-agenda
  :version "24.3"
  :type 'boolean)

(defconst org-agenda-local-vars
  '(org-agenda-this-buffer-name
    org-agenda-undo-list
    org-agenda-pending-undo-list
    org-agenda-follow-mode
    org-agenda-entry-text-mode
    org-agenda-clockreport-mode
    org-agenda-show-log
    org-agenda-redo-command
    org-agenda-query-string
    org-agenda-type
    org-agenda-bulk-marked-entries
    org-agenda-undo-has-started-in
    org-agenda-info
    org-agenda-pre-window-conf
    org-agenda-columns-active
    org-agenda-tag-filter
    org-agenda-category-filter
    org-agenda-top-headline-filter
    org-agenda-regexp-filter
    org-agenda-effort-filter
    org-agenda-filters-preset
    org-agenda-markers
    org-agenda-last-search-view-search-was-boolean
    org-agenda-last-indirect-buffer
    org-agenda-filtered-by-category
    org-agenda-filter-form
    org-agenda-cycle-counter
    org-agenda-last-prefix-arg)
  "Variables that must be local in agenda buffers to allow multiple buffers.")

(defvar org-keys nil) ; set by org-agenda-dispatch
(defvar org-match nil) ; set by org-agenda-dispatch

(defvar org-agenda-type nil)
(defvar org-agenda-redo-command nil)
(defvar org-agenda-query-string nil)

(defvar org-agenda-bulk-marked-entries nil
  "List of markers that refer to marked entries in the agenda.")

(defvar org-agenda-this-buffer-is-sticky nil)
(defvar org-agenda-last-prefix-arg nil)

(defvar org-agenda-this-buffer-name nil)


;; Variables bound when building agenda views
(defvar org-agenda-buffer-tmp-name nil)
(defvar org-agenda-doing-sticky-redo nil)
(defvar org-agenda-buffer-name "*Org Agenda*")
(defun org-agenda--get-buffer-name (sticky-name)
  (or org-agenda-buffer-tmp-name
      (and org-agenda-doing-sticky-redo org-agenda-buffer-name)
      sticky-name
      "*Org Agenda*"))

(defvar org-agenda-keep-modes nil)
(defvar org-agenda-multi nil)
(defvar org-agenda-name nil) ; FIXME: Unused
(defvar org-agenda-overriding-cmd nil)
(defvar org-agenda-overriding-arguments nil)
(defvar org-agenda-overriding-cmd-arguments nil)
(defvar org-cmd nil)


(defvar org-agenda-columns-active nil)
(defvar org-agenda-follow-mode nil)
(defvar org-agenda-entry-text-mode nil)
(defvar org-agenda-clockreport-mode nil)
(defvar org-agenda-archives-mode nil
  "Non-nil means the agenda will include archived items.
If this is the symbol `trees', trees in the selected agenda scope
that are marked with the ARCHIVE tag will be included anyway.  When this is
t, also all archive files associated with the current selection of agenda
files will be included.")
(defvar org-agenda-show-log nil
  "When non-nil, show the log in the agenda.
Do not set this directly; instead use
`org-agenda-start-with-log-mode', which see.")

(defvar org-agenda-represented-categories nil
  "Cache for the list of all categories in the agenda.")
(defvar org-agenda-represented-tags nil
  "Cache for the list of all categories in the agenda.")

;; Keep custom values for `org-agenda-filter-preset' compatible with
;; the new variable `org-agenda-tag-filter-preset'.
(defvaralias 'org-agenda-filter-preset 'org-agenda-tag-filter-preset)
(defvaralias 'org-agenda-filter 'org-agenda-tag-filter)

(defvar org-agenda-tag-filter nil)
(defvar org-agenda-category-filter nil)
(defvar org-agenda-regexp-filter nil)
(defvar org-agenda-effort-filter nil)
(defvar org-agenda-top-headline-filter nil)
(defvar org-agenda-filters-preset nil
  "Alist of filter types and associated preset of filters.
This variable is local in `org-agenda' buffers.  See `org-agenda-local-vars'.")

(defvar org-agenda-tag-filter-preset nil
  "A preset of the tags filter used for secondary agenda filtering.
This must be a list of strings, each string must be a single tag preceded
by \"+\" or \"-\".
This variable should not be set directly, but agenda custom commands can
bind it in the options section.  The preset filter is a global property of
the entire agenda view.  In a block agenda, it will not work reliably to
define a filter for one of the individual blocks.  You need to set it in
the global options and expect it to be applied to the entire view.")

(defvar org-agenda-category-filter-preset nil
  "A preset of the category filter used for secondary agenda filtering.
This must be a list of strings, each string must be a single category
preceded by \"+\" or \"-\".
This variable should not be set directly, but agenda custom commands can
bind it in the options section.  The preset filter is a global property of
the entire agenda view.  In a block agenda, it will not work reliably to
define a filter for one of the individual blocks.  You need to set it in
the global options and expect it to be applied to the entire view.")

(defvar org-agenda-regexp-filter-preset nil
  "A preset of the regexp filter used for secondary agenda filtering.
This must be a list of strings, each string must be a single regexp
preceded by \"+\" or \"-\".
This variable should not be set directly, but agenda custom commands can
bind it in the options section.  The preset filter is a global property of
the entire agenda view.  In a block agenda, it will not work reliably to
define a filter for one of the individual blocks.  You need to set it in
the global options and expect it to be applied to the entire view.")

(defvar org-agenda-effort-filter-preset nil
  "A preset of the effort condition used for secondary agenda filtering.
This must be a list of strings, each string must be a single regexp
preceded by \"+\" or \"-\".
This variable should not be set directly, but agenda custom commands can
bind it in the options section.  The preset filter is a global property of
the entire agenda view.  In a block agenda, it will not work reliably to
define a filter for one of the individual blocks.  You need to set it in
the global options and expect it to be applied to the entire view.")

(defconst org-sorting-choice
  '(choice
    (const time-up) (const time-down)
    (const timestamp-up) (const timestamp-down)
    (const scheduled-up) (const scheduled-down)
    (const deadline-up)  (const deadline-down)
    (const ts-up) (const ts-down)
    (const tsia-up) (const tsia-down)
    (const category-keep) (const category-up) (const category-down)
    (const tag-down) (const tag-up)
    (const priority-up) (const priority-down)
    (const urgency-up) (const urgency-down)
    (const todo-state-up) (const todo-state-down)
    (const effort-up) (const effort-down)
    (const habit-up) (const habit-down)
    (const alpha-up) (const alpha-down)
    (const user-defined-up) (const user-defined-down))
  "Sorting choices.")

(defsubst org-em (x y list)
  "Is X or Y a member of LIST?"
  (or (memq x list) (memq y list)))

(declare-function org-today "org-time" ())
(declare-function calendar-absolute-from-gregorian "calendar" (date))
(defun org-agenda-today-p (date)
  "Non-nil when DATE means today.
DATE is either a list of the form (month day year) or a number of
days as returned by `calendar-absolute-from-gregorian' or
`org-today'.  This function considers `org-extend-today-until'
when defining today."
  (require 'calendar)
  (require 'org-time)
  (eq (org-today)
      (if (consp date) (calendar-absolute-from-gregorian date) date)))

;;; Agenda markers

(defvar org-agenda-markers nil
  "List of all currently active markers created by `org-agenda'.")
(defvar org-agenda-last-marker-time (float-time)
  "Creation time of the last agenda marker.")

(defun org-agenda-new-marker (&optional pos)
  "Return a new agenda marker.
Marker is at point, or at POS if non-nil.  Org mode keeps a list
of these markers and resets them when they are no longer in use."
  (let ((m (copy-marker (or pos (point)) t)))
    (setq org-agenda-last-marker-time (float-time))
    (if (and org-agenda-buffer (buffer-live-p org-agenda-buffer))
        (with-current-buffer org-agenda-buffer
	  (push m org-agenda-markers))
      (push m org-agenda-markers))
    m))

(defun org-agenda-reset-markers ()
  "Reset markers created by `org-agenda'."
  (while org-agenda-markers
    (move-marker (pop org-agenda-markers) nil)))

;; Keep track of some markers for cut and paste.
(require 'org-track-markers)
(defun org-agenda-markers-for-cut-and-paste (_beg _end)
  "List markers to be tracked in BEG..END region.
To be used with `org-track-markers-register'."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'org-agenda-mode)
        org-agenda-markers))))
(org-track-markers-register #'org-agenda-markers-for-cut-and-paste)

;;; Customization affecting multiple aspects of agenda

(defcustom org-agenda-show-inherited-tags t
  "Non-nil means show inherited tags in each agenda line.

When this option is set to `always', it takes precedence over
`org-agenda-use-tag-inheritance' and inherited tags are shown
in every agenda.

When this option is set to t (the default), inherited tags are
shown when they are available, i.e. when the value of
`org-agenda-use-tag-inheritance' enables tag inheritance for the
given agenda type.

This can be set to a list of agenda types in which the agenda
must display the inherited tags.  Available types are `todo',
`agenda' and `search'.

When set to nil, never show inherited tags in agenda lines."
  :group 'org-agenda-line-format
  :group 'org-agenda
  :version "24.3"
  :type '(choice
	  (const :tag "Show inherited tags when available" t)
	  (const :tag "Always show inherited tags" always)
	  (repeat :tag "Show inherited tags only in selected agenda types"
		  (symbol :tag "Agenda type"))))

(defcustom org-agenda-use-tag-inheritance '(todo search agenda)
  "List of agenda view types where to use tag inheritance.

In tags/tags-todo/tags-tree agenda views, tag inheritance is
controlled by `org-use-tag-inheritance'.  In other agenda types,
`org-use-tag-inheritance' is not used for the selection of the
agenda entries.  Still, you may want the agenda to be aware of
the inherited tags anyway, e.g. for later tag filtering.

Allowed value are `todo', `search' and `agenda'.

This variable has no effect if `org-agenda-show-inherited-tags'
is set to `always'.  In that case, the agenda is aware of those
tags.

The default value sets tags in every agenda type.  Setting this
option to nil will speed up non-tags agenda view a lot."
  :group 'org-agenda
  :version "26.1"
  :package-version '(Org . "9.1")
  :type '(choice
	  (const :tag "Use tag inheritance in all agenda types" t)
	  (repeat :tag "Use tag inheritance in selected agenda types"
		  (symbol :tag "Agenda type"))))

(defvar org-scanner-tags nil
  "The current tag list while the tags scanner is running.")

(defvar org-scanner-element nil
  "The current element while the tags scanner is running.")

(defvar org-trust-scanner-tags nil
  "Should `org-get-tags' use the tags for the scanner.
This is for internal dynamical scoping only.
When this is non-nil, the function `org-get-tags' will return the value
of `org-scanner-tags' instead of building the list by itself.  This
can lead to large speed-ups when the tags scanner is used in a file with
many entries, and when the list of tags is retrieved, for example to
obtain a list of properties.  Building the tags list for each entry in such
a file becomes an N^2 operation - but with this variable set, it scales
as N.")

(provide 'org-agenda-common)

;;; org-agenda-common.el ends here


