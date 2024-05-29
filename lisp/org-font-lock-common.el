;;; org-font-lock-common.el --- Common variables and functions for org-font-lock.el -*- lexical-binding: t; -*-

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

;; This library implements Org mode fontification.

;;; Code:

(require 'org-macs)
(org-assert-version)

(defvar org-emph-re nil
  "Regular expression for matching emphasis.
After a match, the match groups contain these elements:
0  The match of the full regular expression, including the characters
   before and after the proper match
1  The character before the proper match, or empty at beginning of line
2  The proper match, including the leading and trailing markers
3  The leading marker like * or /, indicating the type of highlighting
4  The text between the emphasis markers, not including the markers
5  The character after the match, empty at the end of a line")

(defvar org-verbatim-re nil
  "Regular expression for matching verbatim text.")

(defvar org-emphasis-regexp-components) ; defined just below
(defvar org-emphasis-alist) ; defined just below
(defun org-set-emph-re (var val)
  "Set variable and compute the emphasis regular expression."
  (set-default-toplevel-value var val)
  (when (and (boundp 'org-emphasis-alist)
	     (boundp 'org-emphasis-regexp-components)
	     org-emphasis-alist org-emphasis-regexp-components)
    (pcase-let*
	((`(,pre ,post ,border ,body ,nl) org-emphasis-regexp-components)
	 (body (if (<= nl 0) body
		 (format "%s*?\\(?:\n%s*?\\)\\{0,%d\\}" body body nl)))
	 (template
	  (format (concat "\\([%s]\\|^\\)" ;before markers
			  "\\(\\([%%s]\\)\\([^%s]\\|[^%s]%s[^%s]\\)\\3\\)"
			  "\\([%s]\\|$\\)") ;after markers
		  pre border border body border post)))
      (setq org-emph-re (format template "*/_+"))
      (setq org-verbatim-re (format template "=~")))))

;; This used to be a defcustom (Org <8.0) but allowing the users to
;; set this option proved cumbersome.  See this message/thread:
;; https://orgmode.org/list/B72CDC2B-72F6-43A8-AC70-E6E6295766EC@gmail.com
(defvar org-emphasis-regexp-components
  '("-[:space:]('\"{" "-[:space:].,:!?;'\")}\\[" "[:space:]" "." 1)
  "Components used to build the regular expression for emphasis.
This is a list with five entries.  Terminology:  In an emphasis string
like \" *strong word* \", we call the initial space PREMATCH, the final
space POSTMATCH, the stars MARKERS, \"s\" and \"d\" are BORDER characters
and \"trong wor\" is the body.  The different components in this variable
specify what is allowed/forbidden in each part:

pre          Chars allowed as prematch.  Beginning of line will be allowed too.
post         Chars allowed as postmatch.  End of line will be allowed too.
border       The chars *forbidden* as border characters.
body-regexp  A regexp like \".\" to match a body character.  Don't use
             non-shy groups here, and don't allow newline here.
newline      The maximum number of newlines allowed in an emphasis exp.

You need to reload Org or to restart Emacs after setting this.")

(defcustom org-emphasis-alist
  '(("*" bold)
    ("/" italic)
    ("_" underline)
    ("=" org-verbatim verbatim)
    ("~" org-code verbatim)
    ("+" (:strike-through t)))
  "Alist of characters and faces to emphasize text.
Text starting and ending with a special character will be emphasized,
for example *bold*, _underlined_ and /italic/.  This variable sets the
the face to be used by font-lock for highlighting in Org buffers.
Marker characters must be one of */_=~+.

You need to reload Org or to restart Emacs after customizing this."
  :group 'org-appearance
  :set 'org-set-emph-re
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(repeat
	  (list
           (choice
	    (const :tag "Bold" "*")
            (const :tag "Italic" "/")
            (const :tag "Underline" "_")
            (const :tag "Verbatim" "=")
            (const :tag "Code" "~")
            (const :tag "Strike through" "+"))
	   (choice
	    (face :tag "Font-lock-face")
	    (plist :tag "Face property list"))
	   (option (const verbatim)))))

(defcustom org-format-latex-options
  '(:foreground default :background default :scale 1.0
		:html-foreground "Black" :html-background "Transparent"
		:html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
  "Options for creating images from LaTeX fragments.
This is a property list with the following properties:
:foreground  the foreground color for images embedded in Emacs, e.g. \"Black\".
             `default' means use the foreground of the default face.
             `auto' means use the foreground from the text face.
:background  the background color, or \"Transparent\".
             `default' means use the background of the default face.
             `auto' means use the background from the text face.
:scale       a scaling factor for the size of the images, to get more pixels
:html-foreground, :html-background, :html-scale
             the same numbers for HTML export.
:matchers    a list indicating which matchers should be used to
             find LaTeX fragments.  Valid members of this list are:
             \"begin\" find environments
             \"$1\"    find single characters surrounded by $.$
             \"$\"     find math expressions surrounded by $...$
             \"$$\"    find math expressions surrounded by $$....$$
             \"\\(\"    find math expressions surrounded by \\(...\\)
             \"\\=\\[\"    find math expressions surrounded by \\=\\[...\\]"
  :group 'org-latex
  :type 'plist)

(defcustom org-hide-emphasis-markers nil
  "Non-nil means font-lock should hide the emphasis marker characters."
  :group 'org-appearance
  :type 'boolean
  :safe #'booleanp)

(provide 'org-font-lock-common)

;;; org-font-lock-common.el ends here
