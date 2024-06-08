;;; org-edit-structure-common.el --- Common definitions for Org structure editing                      -*- lexical-binding: t; -*-

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

;; This library implements common variables and helpers to edit Org
;; structure.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element)

(defgroup org-edit-structure nil
  "Options concerning structure editing in Org mode."
  :tag "Org Edit Structure"
  :group 'org-structure)


(defcustom org-M-RET-may-split-line '((default . t))
  "Non-nil means M-RET will split the line at the cursor position.
When nil, it will go to the end of the line before making a
new line.
You may also set this option in a different way for different
contexts.  Valid contexts are:

headline  when creating a new headline
item      when creating a new item
table     in a table field
default   the value to be used for all contexts not explicitly
          customized"
  :group 'org-structure
  :group 'org-table
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (repeat :greedy t :tag "Individual contexts"
		  (cons
		   (choice :tag "Context"
			   (const headline)
			   (const item)
			   (const table)
			   (const default))
		   (boolean)))))

(defcustom org-insert-heading-respect-content nil
  "Non-nil means insert new headings after the current subtree.
\\<org-mode-map>
When nil, the new heading is created directly after the current line.
The commands `\\[org-insert-heading-respect-content]' and \
`\\[org-insert-todo-heading-respect-content]' turn this variable on
for the duration of the command."
  :group 'org-structure
  :type 'boolean)

(defcustom org-blank-before-new-entry '((heading . auto)
					(plain-list-item . auto))
  "Should `org-insert-heading' leave a blank line before new heading/item?
The value is an alist, with `heading' and `plain-list-item' as CAR,
and a boolean flag as CDR.  The cdr may also be the symbol `auto', in
which case Org will look at the surrounding headings/items and try to
make an intelligent decision whether to insert a blank line or not."
  :group 'org-edit-structure
  :type '(list
	  (cons (const heading)
		(choice (const :tag "Never" nil)
			(const :tag "Always" t)
			(const :tag "Auto" auto)))
	  (cons (const plain-list-item)
		(choice (const :tag "Never" nil)
			(const :tag "Always" t)
			(const :tag "Auto" auto)))))

(defun org-back-over-empty-lines ()
  "Move backwards over whitespace, to the beginning of the first empty line.
Returns the number of empty lines passed."
  (let ((pos (point)))
    (if (cdr (assq 'heading org-blank-before-new-entry))
	(org-skip-whitespace 'back)
      (unless (eobp)
	(forward-line -1)))
    (forward-line 1)
    (goto-char (min (point) pos))
    (count-lines (point) pos)))

(defun org-kill-is-subtree-p (&optional txt)
  "Check if the current kill is an outline subtree, or a set of trees.
Returns nil if kill does not start with a headline, or if the first
headline level is not the largest headline level in the tree.
So this will actually accept several entries of equal levels as well,
which is OK for `org-paste-subtree'.
If optional TXT is given, check this string instead of the current kill."
  (let* ((kill (or txt (ignore-errors (current-kill 0))))
	 (re (org-get-limited-outline-regexp))
	 (^re (concat "^" re))
	 (start-level (and kill
			   (string-match
			    (concat "\\`\\([ \t\n\r]*?\n\\)?\\(" re "\\)")
			    kill)
			   (- (match-end 2) (match-beginning 2) 1)))
	 (start (1+ (or (match-beginning 2) -1))))
    (if (not start-level)
	(progn
	  nil)  ;; does not even start with a heading
      (catch 'exit
	(while (setq start (string-match ^re kill (1+ start)))
	  (when (< (- (match-end 0) (match-beginning 0) 1) start-level)
	    (throw 'exit nil)))
	t))))

(defun org-remove-empty-drawer-at (pos)
  "Remove an empty drawer at position POS.
POS may also be a marker."
  (with-current-buffer (if (markerp pos) (marker-buffer pos) (current-buffer))
    (org-with-wide-buffer
     (goto-char pos)
     (let ((drawer (org-element-at-point)))
       (when (and (org-element-type-p drawer '(drawer property-drawer))
		  (not (org-element-contents-begin drawer)))
	 (delete-region
          (org-element-begin drawer)
          (org-element-pos-before-blank drawer)))))))

(provide 'org-edit-structure-common)

;;; org-edit-structure-common.el ends here
