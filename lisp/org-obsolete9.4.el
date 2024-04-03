;;; org-obsolete9.4.el --- Obsolete Org mode functions and variables -*- lexical-binding: t; -*-

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

;; This file contains obsolete Org mode code that will be removed in
;; future releases.

;;; Code:

;;;; Obsolete aliases

(define-obsolete-function-alias
  'org-at-property-block-p 'org-at-property-drawer-p "9.4")

(define-obsolete-variable-alias 'org-sort-agenda-notime-is-late
  'org-agenda-sort-notime-is-late "9.4")

(define-obsolete-variable-alias 'org-sort-agenda-noeffort-is-high
  'org-agenda-sort-noeffort-is-high "9.4")

(define-obsolete-function-alias 'org-copy 'org-refile-copy "9.4")

(define-obsolete-function-alias 'org-get-last-sibling 'org-get-previous-sibling "9.4")

;;;; Obsolete variables

(defconst org-maybe-keyword-time-regexp
  (concat "\\(\\<\\(\\(?:CLO\\(?:CK\\|SED\\)\\|DEADLINE\\|SCHEDULED\\):\\)\\)?"
	  " *\\([[<][0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [^]\r\n>]*[]>]"
	  "\\|"
	  "<%%([^\r\n>]*>\\)")
  "Matches a timestamp, possibly preceded by a keyword.")
(make-obsolete-variable
 'org-maybe-keyword-time-regexp
 "use `org-planning-line-re', followed by `org-ts-regexp-both' instead."
 "9.4")

;;;; Obsolete functions and macros

(make-obsolete 'org-attach-store-link "No longer used" "9.4")
(make-obsolete 'org-attach-expand-link "No longer used" "9.4")

(declare-function org-fold-region "org-fold")
(defvar org-fold-core-style) ; org-fold-core.el
(declare-function org-element-at-point "org-element")
(declare-function org-element-type-p "org-element-ast")
(declare-function org-element-post-affiliated "org-element")
(declare-function org-element-end "org-element")
(defun org-flag-drawer (flag &optional element beg end)
  "When FLAG is non-nil, hide the drawer we are at.
Otherwise make it visible.

When optional argument ELEMENT is a parsed drawer, as returned by
`org-element-at-point', hide or show that drawer instead.

When buffer positions BEG and END are provided, hide or show that
region as a drawer without further ado."
  (declare (obsolete "use `org-hide-drawer-toggle' instead." "9.4"))
  (require 'org-fold)
  (require 'org-element-ast)
  (require 'org-element)
  (if (and beg end) (org-fold-region beg end flag 'drawer)
    (let ((drawer
	   (or element
	       (and (save-excursion
		      (forward-line 0)
		      (looking-at-p "^[ \t]*:\\(\\(?:\\w\\|[-_]\\)+\\):[ \t]*$"))
		    (org-element-at-point)))))
      (when (org-element-type-p drawer '(drawer property-drawer))
	(let ((post (org-element-post-affiliated drawer)))
	  (org-fold-region
	   (save-excursion (goto-char post) (line-end-position))
	   (save-excursion (goto-char (org-element-end drawer))
			   (skip-chars-backward " \t\n")
			   (line-end-position))
	   flag 'drawer)
	  ;; When the drawer is hidden away, make sure point lies in
	  ;; a visible part of the buffer.
	  (when (invisible-p (max (1- (point)) (point-min)))
	    (goto-char post)))))))

(declare-function org-fold-hide-block-toggle "org-fold")
(defun org-hide-block-toggle-maybe ()
  "Toggle visibility of block at point.
Unlike to `org-hide-block-toggle', this function does not throw
an error.  Return a non-nil value when toggling is successful."
  (declare (obsolete "use `org-hide-block-toggle' instead." "9.4"))
  (interactive)
  (require 'org-fold)
  (org-fold-hide-block-toggle nil t))

(defun org-hide-block-toggle-all ()
  "Toggle the visibility of all blocks in the current buffer."
  (declare (obsolete "please notify Org mailing list if you use this function."
		     "9.4"))
  (let ((start (point-min))
        (end (point-max)))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
		  (re-search-forward "^[ \t]*#\\+begin_?\
\\([^ \n]+\\)\\(\\([^\n]+\\)\\)?\n\\(\\(?:.\\|\n\\)+?\\)#\\+end_?\\1[ \t]*$" end t))
	(save-excursion
	  (save-match-data
            (goto-char (match-beginning 0))
            (org-fold-hide-block-toggle)))))))

(declare-function org-return "org-edit")
(defun org-return-indent ()
  "Goto next table row or insert a newline and indent.
Calls `org-table-next-row' or `newline-and-indent', depending on
context.  See the individual commands for more information."
  (declare (obsolete "use `org-return' with INDENT set to t instead."
		     "9.4"))
  (interactive)
  (require 'org-edit)
  (org-return t))

(provide 'org-obsolete9.4)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-obsolete9.4.el ends here
