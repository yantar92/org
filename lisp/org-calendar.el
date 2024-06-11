;;; org-calendar.el --- Org mode calendar.el integration                      -*- lexical-binding: t; -*-

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

;; This library implements Org mode integration with calendar.el.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'calendar)

(defcustom org-calendar-to-agenda-key 'default
  "Key to be installed in `calendar-mode-map' for switching to the agenda.

The command `org-calendar-goto-agenda' will be bound to this key.

When set to `default', bind the function to `c', but only if it is
available in the Calendar keymap.  This is the default choice because
`c' can then be used to switch back and forth between agenda and calendar.

When nil, `org-calendar-goto-agenda' is not bound to any key."
  :group 'org-agenda
  :type '(choice
	  (const :tag "Bind to `c' if available" default)
	  (key-sequence :tag "Other binding")
	  (const :tag "No binding" nil))
  :safe (lambda (v) (or (symbolp v) (stringp v)))
  :package-version '(Org . "9.2"))

(defcustom org-calendar-insert-diary-entry-key [?i]
  "The key to be installed in `calendar-mode-map' for adding diary entries.
This option is irrelevant until `org-agenda-diary-file' has been configured
to point to an Org file.  When that is the case, the command
`org-agenda-diary-entry' will be bound to the key given here, by default
`i'.  In the calendar, `i' normally adds entries to `diary-file'.  So
if you want to continue doing this, you need to change this to a different
key."
  :group 'org-agenda
  :type 'sexp)

(declare-function org-agenda-diary-entry "org-agenda" ())
(defun org--setup-calendar-bindings ()
  "Bind Org functions in Calendar keymap."
  (pcase org-calendar-to-agenda-key
    (`nil nil)
    ((and key (pred stringp))
     (local-set-key (kbd key) #'org-calendar-goto-agenda))
    ((guard (not (lookup-key calendar-mode-map "c")))
     (local-set-key "c" #'org-calendar-goto-agenda))
    (_ nil))
  (when (and (boundp 'org-agenda-diary-file)
	     (not (eq org-agenda-diary-file 'diary-file)))
    (local-set-key org-calendar-insert-diary-entry-key
		   #'org-agenda-diary-entry)))

(declare-function org-agenda-list "org-agenda-agenda-view" (&optional arg start-day span with-hour))
;;;###autoload
(defun org-calendar-goto-agenda ()
  "Compute the Org agenda for the calendar date displayed at the cursor.
This is a command that has to be installed in `calendar-mode-map'."
  (interactive)
  (require 'org-agenda-mode)
  (defvar org-agenda-buffer-tmp-name)
  (defvar org-agenda-sticky)
  ;; Temporarily disable sticky agenda since user clearly wants to
  ;; refresh view anyway.
  (let ((org-agenda-buffer-tmp-name "*Org Agenda(a)*")
	(org-agenda-sticky nil))
    (org-agenda-list nil (calendar-absolute-from-gregorian
			(calendar-cursor-to-date))
		     nil)))

(eval-after-load 'calendar
  '(add-hook 'calendar-mode-hook #'org--setup-calendar-bindings))

(provide 'org-calendar)

;;; org-calendar.el ends here
