;;; org-tags-common.el --- Common definitions for org-tags.el                      -*- lexical-binding: t; -*-

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

;; This library provides common functions and definitions for
;; org-tags.el.

;;; Code:

(defcustom org-group-tags t
  "When non-nil (the default), use group tags.
This can be turned on/off through `org-toggle-tags-groups'."
  :group 'org-tags
  :group 'org-startup
  :type 'boolean)

(defcustom org-tag-persistent-alist nil
  "Tags always available in Org files.

The value of this variable is an alist.  Associations either:

  (TAG)
  (TAG . SELECT)
  (SPECIAL)

where TAG is a tag as a string, SELECT is a character, used to
select that tag through the fast tag selection interface, and
SPECIAL is one of the following keywords: `:startgroup',
`:startgrouptag', `:grouptags', `:endgroup', `:endgrouptag' or
`:newline'.  These keywords are used to define a hierarchy of
tags.  See manual for details.

Unlike to `org-tag-alist', tags defined in this variable do not
depend on a local TAGS keyword.  Instead, to disable these tags
on a per-file basis, insert anywhere in the file:

  #+STARTUP: noptag"
  :group 'org-tags
  :type '(repeat
	  (choice
	   (cons :tag "Tag with key"
		 (string    :tag "Tag name")
		 (character :tag "Access char"))
	   (list :tag "Tag" (string :tag "Tag name"))
	   (const :tag "Start radio group" (:startgroup))
	   (const :tag "Start tag group, non distinct" (:startgrouptag))
	   (const :tag "Group tags delimiter" (:grouptags))
	   (const :tag "End radio group" (:endgroup))
	   (const :tag "End tag group, non distinct" (:endgrouptag))
	   (const :tag "New line" (:newline)))))

(defcustom org-tag-alist nil
  "Default tags available in Org files.

The value of this variable is an alist.  Associations either:

  (TAG)
  (TAG . SELECT)
  (SPECIAL)

where TAG is a tag as a string, SELECT is character, used to
select that tag through the fast tag selection interface, and
SPECIAL is one of the following keywords: `:startgroup',
`:startgrouptag', `:grouptags', `:endgroup', `:endgrouptag' or
`:newline'.  These keywords are used to define a hierarchy of
tags.  See manual for details.

When this variable is nil, Org mode bases tag input on what is
already in the buffer.  The value can be overridden locally by
using a TAGS keyword, e.g.,

  #+TAGS: tag1 tag2

See also `org-tag-persistent-alist' to sidestep this behavior."
  :group 'org-tags
  :type '(repeat
	  (choice
	   (cons :tag "Tag with key"
		 (string    :tag "Tag name")
		 (character :tag "Access char"))
	   (list :tag "Tag" (string :tag "Tag name"))
	   (const :tag "Start radio group" (:startgroup))
	   (const :tag "Start tag group, non distinct" (:startgrouptag))
	   (const :tag "Group tags delimiter" (:grouptags))
	   (const :tag "End radio group" (:endgroup))
	   (const :tag "End tag group, non distinct" (:endgrouptag))
	   (const :tag "New line" (:newline)))))

(provide 'org-tags-common)

;;; org-tags-common.el ends here
