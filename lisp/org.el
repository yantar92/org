;;; org.el --- Outline-based notes management and organizer -*- lexical-binding: t; -*-

;; Carstens outline-mode for keeping track of everything.
;; Copyright (C) 2004-2024 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: outlines, hypermedia, calendar, text
;; URL: https://orgmode.org
;; Package-Requires: ((emacs "26.1"))

;; Version: 9.8-pre

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
;;
;;; Commentary:
;;
;; Org is a mode for keeping notes, maintaining ToDo lists, and doing
;; project planning with a fast and effective plain-text system.
;;
;; Org mode develops organizational tasks around NOTES files that
;; contain information about projects as plain text.  Org mode is
;; implemented on top of outline-mode, which makes it possible to keep
;; the content of large files well structured.  Visibility cycling and
;; structure editing help to work with the tree.  Tables are easily
;; created with a built-in table editor.  Org mode supports ToDo
;; items, deadlines, time stamps, and scheduling.  It dynamically
;; compiles entries into an agenda that utilizes and smoothly
;; integrates much of the Emacs calendar and diary.  Plain text
;; URL-like links connect to websites, emails, Usenet messages, BBDB
;; entries, and any files related to the projects.  For printing and
;; sharing of notes, an Org file can be exported as a structured ASCII
;; file, as HTML, or (todo and agenda items only) as an iCalendar
;; file.  It can also serve as a publishing tool for a set of linked
;; webpages.
;;
;; Installation and Activation
;; ---------------------------
;; See the corresponding sections in the manual at
;;
;;   https://orgmode.org/org.html#Installation
;;
;; Documentation
;; -------------
;; The documentation of Org mode can be found in the TeXInfo file.  The
;; distribution also contains a PDF version of it.  At the Org mode website,
;; you can read the same text online as HTML.  There is also an excellent
;; reference card made by Philip Rooke.  This card can be found in the
;; doc/ directory.
;;
;; A list of recent changes can be found at
;; https://orgmode.org/Changes.html
;;
;;; Code:

(require 'org-compat)
(org-assert-version)

(require 'org-load)
(require 'org-mode)

(defgroup org nil
  "Outline-based notes management and organizer."
  :tag "Org"
  :group 'outlines
  :group 'calendar)

;; Update `customize-package-emacs-version-alist'
(add-to-list 'customize-package-emacs-version-alist
	     '(Org ("8.0" . "24.4")
		   ("8.1" . "24.4")
		   ("8.2" . "24.4")
		   ("8.2.7" . "24.4")
		   ("8.3" . "26.1")
		   ("9.0" . "26.1")
		   ("9.1" . "26.1")
		   ("9.2" . "27.1")
		   ("9.3" . "27.1")
		   ("9.4" . "27.2")
		   ("9.5" . "28.1")
		   ("9.6" . "29.1")
                   ("9.7" . "30.1")))

(defvar org--warnings nil
  "List of warnings to be added to the bug reports.")

;;;; Require other packages

(require 'cl-lib)
(eval-when-compile (require 'gnus-sum))
(require 'calendar)
(require 'find-func)
(require 'format-spec)
(require 'outline)
(require 'time-date)

;;;; Require other Org libraries.

;; These libraries are loaded to keep third-party code relying upon
;; (require 'org) operational.
(eval-and-compile (require 'org-macs))
(require 'org-compat)
(require 'org-keys)
(require 'ol)
(require 'oc)
(require 'org-table)
(require 'org-fold)
(require 'org-element)
(require 'org-regexps)
(require 'org-element-context)
(require 'org-dnd)
(require 'org-cdlatex)
(require 'org-indent-static)
(require 'org-fill)
(require 'org-comment)
(require 'org-time)
(require 'org-element-timestamp)
(require 'org-read-date)
(require 'org-timestamp)
(require 'org-tags)
(require 'org-font-lock)
(require 'org-dblock)
(require 'org-property)
(require 'org-priority)
(require 'org-log-note)
(require 'org-agenda-files)
(require 'org-map)
(require 'org-sparse-tree)
(require 'org-open-file)
(require 'org-mark-ring)
(require 'org-edit)
(require 'org-todo)
(require 'org-planning)
(require 'org-edit-special)
(require 'org-mark)
(require 'org-narrow)
(require 'org-preview-latex)
(require 'org-preview-image)
(require 'org-indirect-buffer)
(require 'org-edit-markup)
(require 'org-open-at-point)
(require 'org-cycle)
(require 'org-entities)
(require 'org-faces)
(require 'org-list)
(require 'org-pcomplete)
(require 'org-src)
(require 'org-footnote)
(require 'org-macro)
(require 'ob)
(require 'org-move)

(provide 'org)

;;; org.el ends here
