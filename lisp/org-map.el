;;; org-map.el --- Org mapping API                      -*- lexical-binding: t; -*-

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

;; This library implements Org mapping API to act upon multiple Org
;; elements.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-macs)
(require 'org-move)
(require 'org-element)
(require 'org-mode)
(require 'org-agenda-files)
(require 'org-agenda-search)
(require 'org-agenda-line-format)
(require 'org-tags-common)
(require 'org-outline)
(require 'org-property)
(require 'org-priority)
(require 'org-narrow)
(require 'org-duration)
(require 'org-sparse-tree)
(require 'org-archive-core)

(defvar org-agenda-archives-mode)

(defun org-map-tree (fun)
  "Call FUN for every heading underneath the current one."
  (org-back-to-heading t)
  (let ((level (funcall outline-level)))
    (save-excursion
      (funcall fun)
      (while (and (progn
		    (outline-next-heading)
		    (> (funcall outline-level) level))
		  (not (eobp)))
	(funcall fun)))))

(defun org-map-region (fun beg end)
  "Call FUN for every heading between BEG and END."
  (let (;; Force commands ignore region, if any region is active.
        (transient-mark-mode nil))
    (save-excursion
      (setq end (copy-marker end))
      (goto-char beg)
      (when (and (re-search-forward org-outline-regexp-bol nil t)
		 (< (point) end))
	(funcall fun))
      (while (and (progn
		    (outline-next-heading)
		    (< (point) end))
		  (not (eobp)))
	(funcall fun)))))

(defun org-block-map (function &optional start end)
  "Call FUNCTION at the head of all source blocks in the current buffer.
Optional arguments START and END can be used to limit the range."
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end) (re-search-forward "^[ \t]*#\\+begin" end t))
	(save-excursion
	  (save-match-data
            (goto-char (match-beginning 0))
            (when (org-at-block-p)
              (funcall function))))))))


(defun org-map-entries (func &optional match scope &rest skip)
  "Call FUNC at each headline selected by MATCH in SCOPE.

FUNC is a function or a Lisp form.  The function will be called without
arguments, with the cursor positioned at the beginning of the headline.
The return values of all calls to the function will be collected and
returned as a list.

The call to FUNC will be wrapped into a `save-excursion' form, so FUNC
does not need to preserve point.  After evaluation, the cursor will be
moved to the end of the line (presumably of the headline of the
processed entry) and search continues from there.  Under some
circumstances, this may not produce the wanted results.  For example,
if you have removed (e.g. archived) the current (sub)tree it could
mean that the next entry will be skipped entirely.  In such cases, you
can specify the position from where search should continue by making
FUNC set the variable `org-map-continue-from' to the desired buffer
position.

MATCH is a tags/property/todo match as it is used in the agenda tags view.
Only headlines that are matched by this query will be considered during
the iteration.  When MATCH is nil or t, all headlines will be
visited by the iteration.

SCOPE determines the scope of this command.  It can be any of:

nil     The current buffer, respecting the restriction if any
tree    The subtree started with the entry at point
region  The entries within the active region, if any
region-start-level
        The entries within the active region, but only those at
        the same level than the first one.
file    The current buffer, without restriction
file-with-archives
        The current buffer, and any archives associated with it
agenda  All agenda files
agenda-with-archives
        All agenda files with any archive files associated with them
\(file1 file2 ...)
        If this is a list, all files in the list will be scanned

The remaining args are treated as settings for the skipping facilities of
the scanner.  The following items can be given here:

  archive    skip trees with the archive tag
  comment    skip trees with the COMMENT keyword
  function or Emacs Lisp form:
             will be used as value for `org-agenda-skip-function', so
             whenever the function returns a position, FUNC will not be
             called for that entry and search will continue from the
             position returned

If your function needs to retrieve the tags including inherited tags
at the *current* entry, you can use the value of the variable
`org-scanner-tags' which will be much faster than getting the value
with `org-get-tags'.  If your function gets properties with
`org-entry-properties' at the *current* entry, bind `org-trust-scanner-tags'
to t around the call to `org-entry-properties' to get the same speedup.
Note that if your function moves around to retrieve tags and properties at
a *different* entry, you cannot use these techniques."
  (unless (and (or (eq scope 'region) (eq scope 'region-start-level))
	       (not (use-region-p)))
    (let* ((org-agenda-archives-mode nil) ; just to make sure
	   (org-agenda-skip-archived-trees (memq 'archive skip))
	   (org-agenda-skip-comment-trees (memq 'comment skip))
	   (org-agenda-skip-function
	    (car (org-delete-all '(comment archive) skip)))
	   (org-tags-match-list-sublevels t)
	   (start-level (eq scope 'region-start-level))
	   matcher res
	   org-todo-keywords-for-agenda
	   org-done-keywords-for-agenda
	   org-todo-keyword-alist-for-agenda
	   org-tag-alist-for-agenda
	   org--matcher-tags-todo-only)

      (cond
       ((eq match t)   (setq matcher t))
       ((eq match nil) (setq matcher t))
       (t (setq matcher (if match (cdr (org-make-tags-matcher match)) t))))

      (save-excursion
	(save-restriction
	  (cond ((eq scope 'tree)
		 (org-back-to-heading t)
		 (org-narrow-to-subtree)
		 (setq scope nil))
		((and (or (eq scope 'region) (eq scope 'region-start-level))
		      (use-region-p))
		 ;; If needed, set start-level to a string like "2"
		 (when start-level
		   (save-excursion
		     (goto-char (region-beginning))
		     (unless (org-at-heading-p) (outline-next-heading))
		     (setq start-level (org-current-level))))
		 (narrow-to-region (region-beginning)
				   (save-excursion
				     (goto-char (region-end))
				     (unless (and (bolp) (org-at-heading-p))
				       (outline-next-heading))
				     (point)))
		 (setq scope nil)))

	  (if (not scope)
	      (progn
                ;; Agenda expects a file buffer.  Skip over refreshing
                ;; agenda cache for non-file buffers.
                (when buffer-file-name
		  (org-agenda-prepare-buffers
		   (and buffer-file-name (list (current-buffer)))))
		(setq res
		      (org-scan-tags
		       func matcher org--matcher-tags-todo-only start-level)))
	    ;; Get the right scope
	    (cond
	     ((and scope (listp scope) (symbolp (car scope)))
	      (setq scope (eval scope t)))
	     ((eq scope 'agenda)
	      (setq scope (org-agenda-files t)))
	     ((eq scope 'agenda-with-archives)
	      (setq scope (org-agenda-files t))
              (require 'org-archive)
	      (setq scope (org-add-archive-files scope)))
	     ((eq scope 'file)
	      (setq scope (and buffer-file-name (list buffer-file-name))))
	     ((eq scope 'file-with-archives)
	      (setq scope (org-add-archive-files (list (buffer-file-name))))))
	    (org-agenda-prepare-buffers scope)
	    (dolist (file scope)
	      (with-current-buffer (org-find-base-buffer-visiting file)
		(org-with-wide-buffer
		 (goto-char (point-min))
		 (setq res
		       (append
			res
			(org-scan-tags
			 func matcher org--matcher-tags-todo-only)))))))))
      res)))

(provide 'org-map)

;;; org-map.el ends here
