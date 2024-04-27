;;; org-priority-common.el --- Common definitions for org-priority.el                      -*- lexical-binding: t; -*-

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

;; This library contains common definitions for org-priority.el.

;;; Code:

(require 'org-regexps)

(defvaralias 'org-enable-priority-commands 'org-priority-enable-commands)
(defcustom org-priority-enable-commands t
  "Non-nil means priority commands are active.
When nil, these commands will be disabled, so that you never accidentally
set a priority."
  :group 'org-priorities
  :type 'boolean)

(defvaralias 'org-highest-priority 'org-priority-highest)
(defcustom org-priority-highest ?A
  "The highest priority of TODO items.

A character like ?A, ?B, etc., or a numeric value like 1, 2, etc.

The default is the character ?A, which is 65 as a numeric value.

If you set `org-priority-highest' to a numeric value inferior to
65, Org assumes you want to use digits for the priority cookie.
If you set it to >=65, Org assumes you want to use alphabetical
characters.

In both cases, the value of `org-priority-highest' must be
smaller than `org-priority-lowest': for example, if \"A\" is the
highest priority, it is smaller than the lowest \"C\" priority:
65 < 67."
  :group 'org-priorities
  :type '(choice
	  (character :tag "Character")
	  (integer :tag "Integer (< 65)")))

(defvaralias 'org-lowest-priority 'org-priority-lowest)
(defcustom org-priority-lowest ?C
  "The lowest priority of TODO items.

A character like ?C, ?B, etc., or a numeric value like 9, 8, etc.

The default is the character ?C, which is 67 as a numeric value.

If you set `org-priority-lowest' to a numeric value inferior to
65, Org assumes you want to use digits for the priority cookie.
If you set it to >=65, Org assumes you want to use alphabetical
characters.

In both cases, the value of `org-priority-lowest' must be greater
than `org-priority-highest': for example, if \"C\" is the lowest
priority, it is greater than the highest \"A\" priority: 67 >
65."
  :group 'org-priorities
  :type '(choice
	  (character :tag "Character")
	  (integer :tag "Integer (< 65)")))

(defvaralias 'org-default-priority 'org-priority-default)
(defcustom org-priority-default ?B
  "The default priority of TODO items.
This is the priority an item gets if no explicit priority is given.
When starting to cycle on an empty priority the first step in the cycle
depends on `org-priority-start-cycle-with-default'.  The resulting first
step priority must not exceed the range from `org-priority-highest' to
`org-priority-lowest' which means that `org-priority-default' has to be
in this range exclusive or inclusive to the range boundaries.  Else the
first step refuses to set the default and the second will fall back on
\(depending on the command used) the highest or lowest priority."
  :group 'org-priorities
  :type '(choice
	  (character :tag "Character")
	  (integer :tag "Integer (< 65)")))

(defvaralias 'org-get-priority-function 'org-priority-get-priority-function)
(defcustom org-priority-get-priority-function nil
  "Function to extract the priority from a string.
The string is normally the headline.  If this is nil, Org
computes the priority from the priority cookie like [#A] in the
headline.  It returns an integer, increasing by 1000 for each
priority level.

The user can set a different function here, which should take a
string as an argument and return the numeric priority."
  :group 'org-priorities
  :version "24.1"
  :type '(choice
	  (const nil)
	  (function)))

(defun org-priority-to-value (s)
  "Convert priority string S to its numeric value."
  (or (save-match-data
	(and (string-match "\\([0-9]+\\)" s)
	     (string-to-number (match-string 1 s))))
      (string-to-char s)))

(defun org-get-priority (s)
  "Find priority cookie and return priority.
S is a string against which you can match `org-priority-regexp'.
If `org-priority-get-priority-function' is set to a custom
function, use it.  Otherwise process S and output the priority
value, an integer."
  (save-match-data
    (if (functionp org-priority-get-priority-function)
	(funcall org-priority-get-priority-function s)
      (if (not (string-match org-priority-regexp s))
	  (* 1000 (- org-priority-lowest org-priority-default))
	(* 1000 (- org-priority-lowest
		   (org-priority-to-value (match-string 2 s))))))))

(provide 'org-priority-common)

;;; org-priority-common.el ends here
