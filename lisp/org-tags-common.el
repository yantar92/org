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

(require 'org-macs)
(org-assert-version)

(require 'org-mode-common)
(require 'org-regexps)

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

(defun org--tags-expand-group (group tag-groups expanded)
  "Recursively expand all tags in GROUP, according to TAG-GROUPS.
TAG-GROUPS is the list of groups used for expansion.  EXPANDED is
an accumulator used in recursive calls."
  (dolist (tag group)
    (unless (member tag expanded)
      (let ((group (assoc tag tag-groups)))
	(push tag expanded)
	(when group
	  (setq expanded
		(org--tags-expand-group (cdr group) tag-groups expanded))))))
  expanded)

(defun org-local-tags-alist (&optional epom)
  "Return an alist of default tags to be used for completion at EPOM.
EPOM is a point, marker, or node.
The returned alist takes into consideration `org-tag-alist',
`org-tag-persistent-alist' and TAGS keywords in the buffer at EPOM."
  (org--settings-add-to-alist
   org-tag-persistent-alist
   (let ((tags (org-element-property :TAGS (org-element-org-data epom))))
     (if tags
	 (org-tag-string-to-alist
	  (mapconcat #'identity tags "\n"))
       org-tag-alist))))

(defun org-tags-expand (match &optional single-as-list)
  "Expand group tags in MATCH.

This replaces every group tag in MATCH with a regexp tag search.
For example, a group tag \"Work\" defined as { Work : Lab Conf }
will be replaced like this:

   Work =>  {\\<\\(?:Work\\|Lab\\|Conf\\)\\>}
  +Work => +{\\<\\(?:Work\\|Lab\\|Conf\\)\\>}
  -Work => -{\\<\\(?:Work\\|Lab\\|Conf\\)\\>}

Replacing by a regexp preserves the structure of the match.
E.g., this expansion

  Work|Home => {\\(?:Work\\|Lab\\|Conf\\}|Home

will match anything tagged with \"Lab\" and \"Home\", or tagged
with \"Conf\" and \"Home\" or tagged with \"Work\" and \"Home\".

A group tag in MATCH can contain regular expressions of its own.
For example, a group tag \"Proj\" defined as { Proj : {P@.+} }
will be replaced like this:

   Proj => {\\<\\(?:Proj\\)\\>\\|P@.+}

When the optional argument SINGLE-AS-LIST is non-nil, MATCH is
assumed to be a single group tag, and the function will return
the list of tags in this group."
  (unless (org-string-nw-p match) (error "Invalid match tag: %S" match))
  (let ((tag-groups (org-tag-alist-to-groups (org-local-tags-alist))))
    (cond
     (single-as-list (org--tags-expand-group (list match) tag-groups nil))
     (org-group-tags
      (let* ((case-fold-search t)
	     (group-keys (mapcar #'car tag-groups))
	     (key-regexp (concat "\\([+-]?\\)" (regexp-opt group-keys 'words)))
	     (return-match match))
	;; Mark regexp-expressions in the match-expression so that we
	;; do not replace them later on.
	(let ((s 0))
	  (while (string-match "{.+?}" return-match s)
	    (setq s (match-end 0))
	    (add-text-properties
	     (match-beginning 0) (match-end 0) '(regexp t) return-match)))
	;; For each tag token found in MATCH, compute a regexp and  it
	(with-syntax-table org-mode-tags-syntax-table
	  (replace-regexp-in-string
	   key-regexp
	   (lambda (m)
	     (if (get-text-property (match-beginning 2) 'regexp m)
		 m			;regexp tag: ignore
	       (let* ((operator (match-string 1 m))
		      (tag-token (let ((tag (match-string 2 m)))
				   (list tag)))
		      regexp-tags regular-tags)
		 ;; Partition tags between regexp and regular tags.
		 ;; Remove curly bracket syntax from regexp tags.
		 (dolist (tag (org--tags-expand-group tag-token tag-groups nil))
		   (save-match-data
		     (if (string-match "{\\(.+?\\)}" tag)
			 (push (match-string 1 tag) regexp-tags)
		       (push tag regular-tags))))
		 ;; Replace tag token by the appropriate regexp.
		 ;; Regular tags need to be regexp-quoted, whereas
		 ;; regexp-tags are inserted as-is.
		 (let ((regular (regexp-opt regular-tags))
		       (regexp (mapconcat #'identity regexp-tags "\\|")))
		   (concat operator
			   (cond
			    ((null regular-tags) (format "{%s}" regexp))
			    ((null regexp-tags) (format "{\\<%s\\>}" regular))
			    (t (format "{\\<%s\\>\\|%s}" regular regexp))))))))
	   return-match
	   t t))))
     (t match))))

(defun org-tag-string-to-alist (s)
  "Return tag alist associated to string S.
S is a value for TAGS keyword or produced with
`org-tag-alist-to-string'.  Return value is an alist suitable for
`org-tag-alist' or `org-tag-persistent-alist'."
  (let ((lines (mapcar #'split-string (split-string s "\n" t)))
	(tag-re (concat "\\`\\(" org-tag-re "\\|{.+?}\\)" ; regular expression
			"\\(?:(\\(.\\))\\)?\\'"))
	alist group-flag)
    (dolist (tokens lines (cdr (nreverse alist)))
      (push '(:newline) alist)
      (while tokens
	(let ((token (pop tokens)))
	  (pcase token
	    ("{"
	     (push '(:startgroup) alist)
	     (when (equal (nth 1 tokens) ":") (setq group-flag t)))
	    ("}"
	     (push '(:endgroup) alist)
	     (setq group-flag nil))
	    ("["
	     (push '(:startgrouptag) alist)
	     (when (equal (nth 1 tokens) ":") (setq group-flag t)))
	    ("]"
	     (push '(:endgrouptag) alist)
	     (setq group-flag nil))
	    (":"
	     (push '(:grouptags) alist))
	    ((guard (string-match tag-re token))
	     (let ((tag (match-string 1 token))
		   (key (and (match-beginning 2)
			     (string-to-char (match-string 2 token)))))
	       ;; Push all tags in groups, no matter if they already
	       ;; appear somewhere else in the list.
	       (when (or group-flag (not (assoc tag alist)))
		 (push (cons tag key) alist))))))))))

(provide 'org-tags-common)

;;; org-tags-common.el ends here
