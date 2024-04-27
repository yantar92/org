;;; org-table.el --- The Table Editor for Org        -*- lexical-binding: t; -*-

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

;; This file contains the table editor and spreadsheet for Org mode.

;; Watch out:  Here we are talking about two different kind of tables.
;; Most of the code is for the tables created with the Org mode table editor.
;; Sometimes, we talk about tables created and edited with the table.el
;; Emacs package.  We call the former org-type tables, and the latter
;; table.el-type tables.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-table-core)
(require 'org-table-fold)
(require 'org-table-align)
(require 'org-table-move)
(require 'org-table-edit)
(require 'org-table-formula)
(require 'org-table-formula-edit)
(require 'orgtbl-mode)
(require 'org-table-create)
(require 'org-table-export)
(require 'org-table-header-line-mode)


;;; Generic Tools

(defun org-table-collapse-header (table &optional separator max-header-lines)
  "Collapse the lines before `hline' into a single header.

The given TABLE is a list of lists as returned by `org-table-to-lisp'.
The leading lines before the first `hline' symbol are considered
forming the table header.  This function collapses all leading header
lines into a single header line, followed by the `hline' symbol, and
the rest of the TABLE.  Header cells are glued together with a space,
or the given SEPARATOR."
  (while (eq (car table) 'hline) (pop table))
  (let* ((separator (or separator " "))
	 (max-header-lines (or max-header-lines 4))
	 (trailer table)
	 (header-lines (cl-loop for line in table
				until (eq 'hline line)
				collect (pop trailer))))
    (if (and trailer (<= (length header-lines) max-header-lines))
	(cons (apply #'cl-mapcar
		     (lambda (&rest x)
		       (org-trim
			(mapconcat #'identity x separator)))
		     header-lines)
	      trailer)
      table)))

(defun org-remove-by-index (list indices &optional i0)
  "Remove the elements in LIST with indices in INDICES.
First element has index 0, or I0 if given."
  (if (not indices)
      list
    (if (integerp indices) (setq indices (list indices)))
    (setq i0 (1- (or i0 0)))
    (delq :rm (mapcar (lambda (x)
			(setq i0 (1+ i0))
			(if (memq i0 indices) :rm x))
		      list))))

(provide 'org-table)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-table.el ends here
