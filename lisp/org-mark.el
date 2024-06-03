;;; org-mark.el --- Org mark region commands                      -*- lexical-binding: t; -*-

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

;; This library implements Org commands to mark Org syntax elements.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element)
(require 'org-element-context)
(require 'org-move)

;;;###autoload
(defun org-mark-element ()
  "Put point at beginning of this element, mark at end.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next element after the
ones already marked."
  (interactive)
  (let (deactivate-mark)
    (if (and (called-interactively-p 'any)
	     (or (and (eq last-command this-command) (mark t))
		 (and transient-mark-mode mark-active)))
	(set-mark
	 (save-excursion
	   (goto-char (mark))
	   (goto-char (org-element-end (org-element-at-point)))
	   (point)))
      (let ((element (org-element-at-point)))
	(end-of-line)
	(push-mark (min (point-max) (org-element-end element)) t t)
	(goto-char (org-element-begin element))))))

;;;###autoload
(defun org-mark-subtree (&optional up)
  "Mark the current subtree.
This puts point at the start of the current subtree, and mark at
the end.  If a numeric prefix UP is given, move up into the
hierarchy of headlines by UP levels before marking the subtree."
  (interactive "P")
  (org-with-limited-levels
   (cond ((org-at-heading-p) (forward-line 0))
	 ((org-before-first-heading-p) (user-error "Not in a subtree"))
	 (t (outline-previous-visible-heading 1))))
  (when up (while (and (> up 0) (org-up-heading-safe)) (cl-decf up)))
  (if (called-interactively-p 'any)
      (call-interactively #'org-mark-element)
    (org-mark-element)))

(provide 'org-mark)

;;; org-mark.el ends here
