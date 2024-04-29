;;; org-archive-core.el --- Common definitions for org-archive.el             -*- lexical-binding: t; -*-

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

;; This file contains common definitions for org-archive.el

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-agenda-files)
(require 'org-property-core)

(defgroup org-archive nil
  "Options concerning archiving in Org mode."
  :tag "Org Archive"
  :group 'org-structure)

(defcustom org-archive-location "%s_archive::"
  "The location where subtrees should be archived.

The value of this variable is a string, consisting of two parts,
separated by a double-colon.  The first part is a filename and
the second part is a headline.

When the filename is omitted, archiving happens in the same file.
%s in the filename will be replaced by the current file
name (without the directory part).  Archiving to a different file
is useful to keep archived entries from contributing to the
Org Agenda.

The archived entries will be filed as subtrees of the specified
headline.  When the headline is omitted, the subtrees are simply
filed away at the end of the file, as top-level entries.  Also in
the heading you can use %s to represent the file name, this can be
useful when using the same archive for a number of different files.

Here are a few examples:
\"%s_archive::\"
	If the current file is Projects.org, archive in file
	Projects.org_archive, as top-level trees.  This is the default.

\"::* Archived Tasks\"
	Archive in the current file, under the top-level headline
	\"* Archived Tasks\".

\"~/org/archive.org::\"
	Archive in file ~/org/archive.org (absolute path), as top-level trees.

\"~/org/archive.org::* From %s\"
	Archive in file ~/org/archive.org (absolute path), under headlines
        \"From FILENAME\" where file name is the current file name.

\"~/org/datetree.org::datetree/* Finished Tasks\"
        The \"datetree/\" string is special, signifying to archive
        items to the datetree.  Items are placed in either the CLOSED
        date of the item, or the current date if there is no CLOSED date.
        The heading will be a subentry to the current date.  There doesn't
        need to be a heading, but there always needs to be a slash after
        datetree.  For example, to store archived items directly in the
        datetree, use \"~/org/datetree.org::datetree/\".

\"basement::** Finished Tasks\"
	Archive in file ./basement (relative path), as level 3 trees
	below the level 2 heading \"** Finished Tasks\".

You may define it locally by setting an ARCHIVE property.  If
such a property is found in the file or in an entry, and anywhere
up the hierarchy, it will be used.

You can also set it for the whole file using the keyword-syntax:

#+ARCHIVE: basement::** Finished Tasks"
  :group 'org-archive
  :type 'string)

;;;###autoload
(defun org-add-archive-files (files)
  "Splice the archive FILES into the list of files.
This implies visiting all these files and finding out what the
archive file is."
  (seq-uniq
   (apply
    'append
    (mapcar
     (lambda (f)
       (if (not (file-exists-p f))
	   nil
	 (with-current-buffer (org-get-agenda-file-buffer f)
	   (cons f (org-all-archive-files)))))
     files))
   #'file-equal-p))

(defun org-all-archive-files ()
  "List of all archive files used in the current buffer."
  (let* ((case-fold-search t)
	 (files `(,(car (org-archive--compute-location
                         (org-get-archive-location (org-element-org-data)))))))
    (org-with-point-at 1
      (while (re-search-forward "^[ \t]*:ARCHIVE:" nil t)
	(when (org-at-property-p)
	  (pcase (org-archive--compute-location (match-string 3))
	    (`(,file . ,_)
	     (when (org-string-nw-p file)
	       (cl-pushnew file files :test #'file-equal-p))))))
      (cl-remove-if-not #'file-exists-p (nreverse files)))))

(defun org-get-archive-location (&optional epom)
  "Get archive location applying to EPOM or point.
EPOM is an element, marker, or buffer position."
  (or (org-entry-get-with-inheritance "ARCHIVE" nil epom)
      org-archive-location))

(defun org-archive--compute-location (location)
  "Extract and expand the location from archive LOCATION.
Return a pair (FILE . HEADING) where FILE is the file name and
HEADING the heading of the archive location, as strings.  Raise
an error if LOCATION is not a valid archive location."
  (unless (string-match "::" location)
    (error "Invalid archive location: %S" location))
  (let ((current-file (buffer-file-name (buffer-base-buffer)))
	(file-fmt (substring location 0 (match-beginning 0)))
	(heading-fmt (substring location (match-end 0))))
    (cons
     ;; File part.
     (if (org-string-nw-p file-fmt)
	 (expand-file-name
	  (format file-fmt (file-name-nondirectory current-file)))
       current-file)
     ;; Heading part.
     (format heading-fmt (file-name-nondirectory current-file)))))

(provide 'org-archive-core)

;;; org-archive-core.el ends here
