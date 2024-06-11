;;; org-agenda-stuck-view.el --- Listing stuck projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, text
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

;; This file implements Org agenda view listing stuck projects.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-agenda-mode)
(require 'org-agenda-search)

(defcustom org-stuck-projects
  '("+LEVEL=2/-DONE" ("TODO" "NEXT" "NEXTACTION") nil "")
  "How to identify stuck projects.
This is a list of four items:

1. A tags/todo/property matcher string that is used to identify a project.
   See Info node `(org) Matching tags and properties' for a
   description of tag and property searches.  The entire tree
   below a headline matched by this is considered one project.

2. A list of TODO keywords identifying non-stuck projects.
   If the project subtree contains any headline with one of these todo
   keywords, the project is considered to be not stuck.  If you specify
   \"*\" as a keyword, any TODO keyword will mark the project unstuck.

3. A list of tags identifying non-stuck projects.
   If the project subtree contains any headline with one of these tags,
   the project is considered to be not stuck.  If you specify \"*\" as
   a tag, any tag will mark the project unstuck.  Note that this is about
   the explicit presence of a tag somewhere in the subtree, inherited
   tags do not count here.  If inherited tags make a project not stuck,
   use \"-TAG\" in the tags part of the matcher under (1.) above.

4. An arbitrary regular expression matching non-stuck projects.

If the project turns out to be not stuck, search continues also in the
subtree to see if any of the subtasks have project status.

See also the variable `org-tags-match-list-sublevels' which applies
to projects matched by this search as well.

After defining this variable, you may use `org-agenda-list-stuck-projects'
\(bound to `\\[org-agenda] #') to produce the list."
  :group 'org-agenda-custom-commands
  :type '(list
	  (string :tag "Tags/TODO match to identify a project")
	  (repeat :tag "Projects are *not* stuck if they have an entry with \
TODO keyword any of" (string))
	  (repeat :tag "Projects are *not* stuck if they have an entry with \
TAG being any of" (string))
	  (regexp :tag "Projects are *not* stuck if this regexp matches inside \
the subtree")))

;;;###autoload
(defun org-agenda-list-stuck-projects (&rest _ignore)
  "Create agenda view for projects that are stuck.
Stuck projects are project that have no next actions.  For the definitions
of what a project is and how to check if it stuck, customize the variable
`org-stuck-projects'."
  (interactive)
  (let* ((org-agenda-overriding-header
	  (or org-agenda-overriding-header "List of stuck projects: "))
	 (matcher (nth 0 org-stuck-projects))
	 (todo (nth 1 org-stuck-projects))
	 (tags (nth 2 org-stuck-projects))
	 (gen-re (org-string-nw-p (nth 3 org-stuck-projects)))
	 (todo-wds
	  (if (not (member "*" todo)) todo
	    (org-agenda-prepare-buffers (org-agenda-files nil 'ifmode))
	    (org-delete-all org-done-keywords-for-agenda
			    (copy-sequence org-todo-keywords-for-agenda))))
	 (todo-re (and todo
		       (format "^\\*+[ \t]+\\(%s\\)\\(?:[ \t]\\|$\\)"
			       (mapconcat #'regexp-quote todo-wds "\\|"))))
	 (tags-re (cond ((null tags) nil)
			((member "*" tags) org-tag-line-re)
			(tags
			 (let ((other-tags (format "\\(?:%s:\\)*" org-tag-re)))
			   (concat org-outline-regexp-bol
				   ".*?[ \t]:"
				   other-tags
				   (regexp-opt tags t)
				   ":" other-tags "[ \t]*$")))
			(t nil)))
	 (re-list (delq nil (list todo-re tags-re gen-re)))
	 (skip-re
	  (if (null re-list)
	      (error "Missing information to identify unstuck projects")
	    (mapconcat #'identity re-list "\\|")))
	 (org-agenda-skip-function
	  ;; Skip entry if `org-agenda-skip-regexp' matches anywhere
	  ;; in the subtree.
	  (lambda ()
	    (and (save-excursion
		   (let ((case-fold-search nil))
		     (re-search-forward
		      skip-re (save-excursion (org-end-of-subtree t)) t)))
		 (progn (outline-next-heading) (point))))))
    (org-tags-view nil matcher)
    (setq org-agenda-buffer-name (buffer-name))
    (with-current-buffer org-agenda-buffer-name
      (setq org-agenda-redo-command
	    `(org-agenda-list-stuck-projects ,current-prefix-arg))
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         `(org-redo-cmd ,org-agenda-redo-command))))))

(provide 'org-agenda-stuck-view)

;;; org-agenda-stuck-view.el ends here
