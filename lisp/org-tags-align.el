;;; org-tags-align.el --- Org tag alignment                      -*- lexical-binding: t; -*-

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

;; This library implements tag alignment in headings.

;;; Code:

(require 'org-regexps)
(require 'org-element-context)
(require 'org-move)

(defcustom org-auto-align-tags t
  "Non-nil keeps tags aligned when modifying headlines.
Some operations (i.e. demoting) change the length of a headline and
therefore shift the tags around.  With this option turned on, after
each such operation the tags are again aligned to `org-tags-column'."
  :group 'org-tags
  :type 'boolean)

(defun org--align-tags-here (to-col)
  "Align tags on the current headline to TO-COL.
Assume point is on a headline.  Preserve point when aligning
tags."
  (when (org-match-line org-tag-line-re)
    (let* ((tags-start (match-beginning 1))
	   (blank-start (save-excursion
			  (goto-char tags-start)
			  (skip-chars-backward " \t")
			  (point)))
	   (new (max (if (>= to-col 0) to-col
		       (- (abs to-col) (string-width (match-string 1))))
		     ;; Introduce at least one space after the heading
		     ;; or the stars.
		     (save-excursion
		       (goto-char blank-start)
		       (1+ (current-column)))))
	   (current
	    (save-excursion (goto-char tags-start) (current-column)))
	   (origin (point-marker))
	   (column (current-column))
	   (in-blank? (and (> origin blank-start) (<= origin tags-start))))
      (when (/= new current)
	(delete-region blank-start tags-start)
	(goto-char blank-start)
	(let ((indent-tabs-mode nil)) (indent-to new))
	;; Try to move back to original position.  If point was in the
	;; blanks before the tags, ORIGIN marker is of no use because
	;; it now points to BLANK-START.  Use COLUMN instead.
	(if in-blank? (org-move-to-column column) (goto-char origin))))))

;;;###autoload
(defun org-align-tags (&optional all)
  "Align tags in current entry.
When optional argument ALL is non-nil, align all tags in the
visible part of the buffer."
  (let ((get-indent-column
	 (lambda ()
	   (let ((offset (if (bound-and-true-p org-indent-mode)
                             (save-excursion
                               (org-back-to-heading-or-point-min)
                               (length
                                (get-text-property
                                 (line-end-position)
                                 'line-prefix)))
			   0)))
	     (+ org-tags-column
		(if (> org-tags-column 0) (- offset) offset))))))
    (if (and (not all) (org-at-heading-p))
	(org--align-tags-here (funcall get-indent-column))
      (save-excursion
	(if all
	    (progn
	      (goto-char (point-min))
	      (while (re-search-forward org-tag-line-re nil t)
		(org--align-tags-here (funcall get-indent-column))))
	  (org-back-to-heading t)
	  (org--align-tags-here (funcall get-indent-column)))))))

(defun org-fix-tags-on-the-fly ()
  "Align tags in headline at point.
Unlike `org-align-tags', this function does nothing if point is
either not currently on a tagged headline or on a tag."
  (when (and (org-match-line org-tag-line-re)
	     (< (point) (match-beginning 1)))
    (org-align-tags)))

(provide 'org-tags-align)

;;; org-tags-align.el ends here
