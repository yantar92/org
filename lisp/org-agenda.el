;;; org-agenda.el --- Dynamic task and appointment lists for Org  -*- lexical-binding: t; -*-

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

;; This file contains the code for creating and using the Agenda for Org.
;;
;; The functions `org-batch-agenda', `org-batch-agenda-csv', and
;; `org-batch-store-agenda-views' are implemented as macros to provide
;; a convenient way for extracting agenda information from the command
;; line.  The Lisp does not evaluate parameters of a macro call; thus
;; it is not necessary to quote the parameters passed to one of those
;; functions.  E.g. you can write:
;;
;;   emacs -batch -l ~/.emacs -eval '(org-batch-agenda "a" org-agenda-span 7)'
;;
;; To export an agenda spanning 7 days.  If `org-batch-agenda' would
;; have been implemented as a regular function you'd have to quote the
;; symbol org-agenda-span.  Moreover: To use a symbol as parameter
;; value you would have to double quote the symbol.
;;
;; This is a hack, but it works even when running Org byte-compiled.
;;

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-agenda-files)
(require 'org-agenda-common)
(require 'org-agenda-search)
(require 'org-agenda-line-format)
(require 'org-agenda-buffer-format)
(require 'org-agenda-highlight)
(require 'org-agenda-diary)
(require 'org-agenda-mode)
(require 'org-agenda-entry-text-mode)
(require 'org-agenda-bulk-edit)
(require 'org-agenda-query)
(require 'org-agenda-search)
(require 'org-agenda-sort)
(require 'org-agenda-undo)

(require 'org-agenda-agenda-view)
(require 'org-agenda-search-view)
(require 'org-agenda-todo-view)
(require 'org-agenda-tags-view)
(require 'org-agenda-multi-view)

(require 'org-agenda-commands)
(require 'org-agenda-commands-proxy)
(require 'org-agenda-filter-commands)
(require 'org-agenda-misc)

(require 'org-agenda-dispatch)
(require 'org-agenda-export)
(require 'org-colview-agenda)

;; FIXME: Unused
(defmacro org-agenda-with-point-at-orig-entry (string &rest body)
  "Execute BODY with point at location given by `org-hd-marker' property.
If STRING is non-nil, the text property will be fetched from position 0
in that string.  If STRING is nil, it will be fetched from the beginning
of the current line."
  (declare (debug t) (indent 1))
  (org-with-gensyms (marker)
    `(let ((,marker (get-text-property (if ,string 0 (line-beginning-position))
				       'org-hd-marker ,string)))
       (with-current-buffer (marker-buffer ,marker)
	 (save-excursion
	   (goto-char ,marker)
	   ,@body)))))

(provide 'org-agenda)

;;; org-agenda.el ends here
