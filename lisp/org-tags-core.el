;;; org-tags-core.el --- Org tag retrival                      -*- lexical-binding: t; -*-

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

;; This library provides API to retrieve Org tags.

;;; Code:

(require 'org-element)
(require 'org-agenda-common)

(defcustom org-use-tag-inheritance t
  "Non-nil means tags in levels apply also for sublevels.
When nil, only the tags directly given in a specific line apply there.
This may also be a list of tags that should be inherited, or a regexp that
matches tags that should be inherited.  Additional control is possible
with the variable  `org-tags-exclude-from-inheritance' which gives an
explicit list of tags to be excluded from inheritance, even if the value of
`org-use-tag-inheritance' would select it for inheritance.

If this option is t, a match early-on in a tree can lead to a large
number of matches in the subtree when constructing the agenda or creating
a sparse tree.  If you only want to see the first match in a tree during
a search, check out the variable `org-tags-match-list-sublevels'."
  :group 'org-tags
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Always" t)
	  (repeat :tag "Specific tags" (string :tag "Tag"))
	  (regexp :tag "Tags matched by regexp")))

(defcustom org-tags-exclude-from-inheritance nil
  "List of tags that should never be inherited.
This is a way to exclude a few tags from inheritance.  For way to do
the opposite, to actively allow inheritance for selected tags,
see the variable `org-use-tag-inheritance'."
  :group 'org-tags
  :type '(repeat (string :tag "Tag")))

(defun org-tag-inherit-p (tag)
  "Check if TAG is one that should be inherited."
  (cond
   ((member tag org-tags-exclude-from-inheritance) nil)
   ((eq org-use-tag-inheritance t) t)
   ((not org-use-tag-inheritance) nil)
   ((stringp org-use-tag-inheritance)
    (string-match org-use-tag-inheritance tag))
   ((listp org-use-tag-inheritance)
    (member tag org-use-tag-inheritance))
   (t (error "Invalid setting of `org-use-tag-inheritance'"))))

(defun org-remove-uninherited-tags (tags)
  "Remove all tags that are not inherited from the list TAGS."
  (cond
   ((eq org-use-tag-inheritance t)
    (if org-tags-exclude-from-inheritance
	(org-delete-all org-tags-exclude-from-inheritance tags)
      tags))
   ((not org-use-tag-inheritance) nil)
   ((stringp org-use-tag-inheritance)
    (delq nil (mapcar
	     (lambda (x)
	       (if (and (string-match org-use-tag-inheritance x)
			(not (member x org-tags-exclude-from-inheritance)))
		   x nil))
	     tags)))
   ((listp org-use-tag-inheritance)
    (delq nil (mapcar
	     (lambda (x)
	       (if (member x org-use-tag-inheritance) x nil))
	     tags)))))

(defun org--get-local-tags (&optional epom)
  "Return list of tags for headline at EPOM.
When EPOM is non-nil, it should be a marker, point, or element
representing headline."
  ;; If we do not explicitly copy the result, reference would
  ;; be returned and cache element might be modified directly.
  (mapcar
   #'copy-sequence
   (org-element-property
    :tags
    (org-element-lineage
     (org-element-at-point epom)
     '(headline inlinetask)
     'with-self))))

(defun org-get-tags (&optional epom local)
  "Get the list of tags specified in the headline at EPOM.

When argument EPOM is non-nil, it should be point, marker, or headline
element.

According to `org-use-tag-inheritance', tags may be inherited
from parent headlines, and from the whole document, through
#+FILETAGS keyword.  In this case, the returned list of tags
contains tags in this order: file tags, tags inherited from
parent headlines, local tags.  If a tag appears multiple times,
only the most local tag is returned.

However, when optional argument LOCAL is non-nil, only return
tags specified at the headline.

Inherited tags have the `inherited' text property.

This function may modify the match data."
  (if (and org-trust-scanner-tags
           (or (not epom) (eq epom (point)))
           (not local))
      org-scanner-tags
    (setq epom (org-element-lineage
                (org-element-at-point epom)
                '(headline inlinetask)
                'with-self))
    (let ((ltags (org--get-local-tags epom))
          itags)
      (if (or local (not org-use-tag-inheritance)) ltags
        (setq
         itags
         (mapcar
          #'org-add-prop-inherited
          (org-element-property-inherited :tags epom nil 'acc)))
        (nreverse
	 (delete-dups
	  (nreverse (nconc (org-remove-uninherited-tags itags) ltags))))))))

(defun org-get-buffer-tags ()
  "Get a table of all tags used in the buffer, for completion."
  (let ((hashed (make-hash-table :test #'equal)))
    (org-element-cache-map
     (lambda (el)
       (dolist (tag (org-element-property :tags el))
         ;; Do not carry over the text properties.  They may look
         ;; ugly in the completion.
         (puthash (list (substring-no-properties tag)) t hashed))))
    (dolist (tag (org-element-property :tags (org-element-org-data)))
      (puthash (list tag) t hashed))
    (hash-table-keys hashed)))

(defun org-make-tag-string (tags)
  "Return string associated to TAGS.
TAGS is a list of strings."
  (if (null tags) ""
    (format ":%s:" (mapconcat #'identity tags ":"))))

(provide 'org-tags-core)

;;; org-tags-core.el ends here
