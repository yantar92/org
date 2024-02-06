;;; org-regexps.el --- Auxiliary Org Syntax regexps and format strings         -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Free Software Foundation, Inc.

;; Author: Ihor Radchenko <yantar92 at posteo dot net>
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is part of GNU Emacs.

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

;;; Commentary:
;;
;; This file defines a number of auxiliary regexps and format strings
;; related, but not used by Org Parser and Org Interpreter.

;; FIXME: Many regexps here should not be necessary.  Org element API
;; should be used instead where possible.  Some regexps are also
;; redundant, repeating equivalent regexps (or their parts) in
;; org-element.el.

;;; Code:

(require 'org-element)

;;;; Heading

;; `org-outline-regexp' ought to be a defconst but is let-bound in
;; some places -- e.g. see the macro `org-with-limited-levels'.
(defvar org-outline-regexp (org-headline-re nil t)
  "Regexp to match Org headlines.
This variable may be re-defined inside `org-with-limited-levels'.")

(defvar org-outline-regexp-bol org-element-headline-re
  "Regexp to match Org headlines.
This is similar to `org-outline-regexp' but additionally makes
sure that we are at the beginning of the line.
Unlike `org-element-headline-re', this variable may be re-defined
inside `org-with-limited-levels'.")

;; FIXME: Unused in Org mode, may need to be deprecated.
(defvar org-heading-regexp "^\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[ \t]*$"
  "Matches a headline, putting stars and text into groups.
Stars are put in group 1 and the trimmed body in group 2.")

(defvar org-priority-regexp ".*?\\(\\[#\\([A-Z0-9]+\\)\\] ?\\)"
  "Regular expression matching the priority indicator.
A priority indicator can be e.g. [#A] or [#1].
This regular expression matches these groups:
0 : the whole match, e.g. \"TODO [#A] Hack\"
1 : the priority cookie, e.g. \"[#A]\"
2 : the value of the priority cookie, e.g. \"A\".")

;;;; Block

(defconst org-block-regexp
  "^[ \t]*#\\+begin_?\\([^ \n]+\\)\\(\\([^\n]+\\)\\)?\n\\(\\(?:.\\|\n\\)+?\\)#\\+end_?\\1[ \t]*$"
  "Regular expression for hiding blocks.")

(defconst org-dblock-start-re
  "^[ \t]*#\\+\\(?:BEGIN\\|begin\\):[ \t]+\\(\\S-+\\)\\([ \t]+\\(.*\\)\\)?"
  "Matches the start line of a dynamic block, with parameters.")

(defconst org-dblock-end-re "^[ \t]*#\\+\\(?:END\\|end\\)\\([: \t\r\n]\\|$\\)"
  "Matches the end of a dynamic block.")

;;;; Keyword
(defconst org-keyword-regexp "^[ \t]*#\\+\\(\\S-+?\\):[ \t]*\\(.*\\)$"
  "Regular expression for keyword-lines.")

;;;; Timestamp

(defconst org-tr-regexp (concat org-ts-regexp "--?-?" org-ts-regexp)
  "Regular expression matching a time stamp range.")

(defconst org-tr-regexp-both
  (concat org-ts-regexp-both "--?-?" org-ts-regexp-both)
  "Regular expression matching a time stamp range.")

(defconst org-tsr-regexp (concat org-ts-regexp "\\(--?-?"
				 org-ts-regexp "\\)?")
  "Regular expression matching a time stamp or time stamp range.")

(defconst org-tsr-regexp-both
  (concat org-ts-regexp-both "\\(--?-?"
	  org-ts-regexp-both "\\)?")
  "Regular expression matching a time stamp or time stamp range.
The time stamps may be either active or inactive.")

(defconst org-ts-regexp0
  "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\( +[^]+0-9>\r\n -]+\\)?\\( +\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)"
  "Regular expression matching time strings for analysis.
This one does not require the space after the date, so it can be used
on a string that terminates immediately after the date.")

(defconst org-ts-regexp1 "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\(?: *\\([^]+0-9>\r\n -]+\\)\\)?\\( \\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)"
  "Regular expression matching time strings for analysis.")

(defconst org-ts-regexp2 (concat "<" org-ts-regexp1 "[^>\n]\\{0,16\\}>")
  "Regular expression matching time stamps, with groups.")

(defconst org-ts-regexp3 (concat "[[<]" org-ts-regexp1 "[^]>\n]\\{0,16\\}[]>]")
  "Regular expression matching time stamps (also [..]), with groups.")

(defconst org-repeat-re
  "<[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [^>\n]*?\
\\([.+]?\\+[0-9]+[hdwmy]\\(/[0-9]+[hdwmy]\\)?\\)"
  "Regular expression for specifying repeated events.
After a match, group 1 contains the repeat expression.")

(defvaralias 'org-time-stamp-formats 'org-timestamp-formats)
(defconst org-timestamp-formats (cons org-element-timestamp-format
                                      org-element-timestamp-format-with-time)
  "Formats for `format-time-string' which are used for time stamps.

The value is a cons cell containing two strings.  The `car' and `cdr'
of the cons cell are used to format time stamps that do not and do
contain time, respectively.

Leading \"<\"/\"[\" and trailing \">\"/\"]\" pair will be stripped
from the format strings.

Also, see `org-time-stamp-format'.")

(defconst org-plain-time-of-day-regexp
  (concat
   "\\(\\<[012]?[0-9]"
   "\\(\\(:\\([0-5][0-9]\\([AaPp][Mm]\\)?\\)\\)\\|\\([AaPp][Mm]\\)\\)\\>\\)"
   "\\(--?"
   "\\(\\<[012]?[0-9]"
   "\\(\\(:\\([0-5][0-9]\\([AaPp][Mm]\\)?\\)\\)\\|\\([AaPp][Mm]\\)\\)\\>\\)"
   "\\)?")
  "Regular expression to match a plain time or time range.
Examples:  11:45 or 8am-13:15 or 2:45-2:45pm.  After a match, the following
groups carry important information:
0  the full match
1  the first time, range or not
8  the second time, if it is a range.")

(defconst org-plain-time-extension-regexp
  (concat
   "\\(\\<[012]?[0-9]"
   "\\(\\(:\\([0-5][0-9]\\([AaPp][Mm]\\)?\\)\\)\\|\\([AaPp][Mm]\\)\\)\\>\\)"
   "\\+\\([0-9]+\\)\\(:\\([0-5][0-9]\\)\\)?")
  "Regular expression to match a time range like 13:30+2:10 = 13:30-15:40.
Examples:  11:45 or 8am-13:15 or 2:45-2:45pm.  After a match, the following
groups carry important information:
0  the full match
7  hours of duration
9  minutes of duration")

(defconst org-stamp-time-of-day-regexp
  (concat
   "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} +\\sw+ +\\)"
   "\\([012][0-9]:[0-5][0-9]\\)\\(-\\([012][0-9]:[0-5][0-9]\\)\\)?[^\n\r>]*?>"
   "\\(--?"
   "<\\1\\([012][0-9]:[0-5][0-9]\\)>\\)?")
  "Regular expression to match a timestamp time or time range.
After a match, the following groups carry important information:
0  the full match
1  date plus weekday, for back referencing to make sure
     both times are on the same day
2  the first time, range or not
4  the second time, if it is a range.")

;;;; Clock and Planning

(defvaralias 'org-closed-string 'org-element-closed-keyword)

(defvaralias 'org-deadline-string 'org-element-deadline-keyword)

(defvaralias 'org-scheduled-string 'org-element-scheduled-keyword)

(defconst org-ds-keyword-length
  (+ 2
     (apply #'max
	    (mapcar #'length
		    (list org-deadline-string org-scheduled-string
			  org-clock-string org-closed-string))))
  "Maximum length of the DEADLINE and SCHEDULED keywords.")

(defconst org-planning-line-re
  (concat "^[ \t]*"
	  (regexp-opt
	   (list org-closed-string org-deadline-string org-scheduled-string)
	   t))
  "Matches a line with planning info.
Matched keyword is in group 1.")

(defconst org-deadline-regexp (concat "\\<" org-deadline-string)
  "Matches the DEADLINE keyword.")

(defconst org-deadline-time-regexp
  (concat "\\<" org-deadline-string " *<\\([^>]+\\)>")
  "Matches the DEADLINE keyword together with a time stamp.")

(defconst org-deadline-time-hour-regexp
  (concat "\\<" org-deadline-string
	  " *<\\([^>]+[0-9]\\{1,2\\}:[0-9]\\{2\\}[0-9+:hdwmy/ \t.-]*\\)>")
  "Matches the DEADLINE keyword together with a time-and-hour stamp.")

(defconst org-deadline-line-regexp
  (concat "\\<\\(" org-deadline-string "\\).*")
  "Matches the DEADLINE keyword and the rest of the line.")

(defconst org-scheduled-regexp (concat "\\<" org-scheduled-string)
  "Matches the SCHEDULED keyword.")

(defconst org-scheduled-time-regexp
  (concat "\\<" org-scheduled-string " *<\\([^>]+\\)>")
  "Matches the SCHEDULED keyword together with a time stamp.")

(defconst org-scheduled-time-hour-regexp
  (concat "\\<" org-scheduled-string
	  " *<\\([^>]+[0-9]\\{1,2\\}:[0-9]\\{2\\}[0-9+:hdwmy/ \t.-]*\\)>")
  "Matches the SCHEDULED keyword together with a time-and-hour stamp.")

(defconst org-closed-time-regexp
  (concat "\\<" org-closed-string " *\\[\\([^]]+\\)\\]")
  "Matches the CLOSED keyword together with a time stamp.")

(defconst org-keyword-time-regexp
  (concat "\\<"
	  (regexp-opt
	   (list org-scheduled-string org-deadline-string org-closed-string
		 org-clock-string)
	   t)
	  " *[[<]\\([^]>]+\\)[]>]")
  "Matches any of the 4 keywords, together with the time stamp.")

(defconst org-keyword-time-not-clock-regexp
  (concat
   "\\<"
   (regexp-opt
    (list org-scheduled-string org-deadline-string org-closed-string) t)
   " *[[<]\\([^]>]+\\)[]>]")
  "Matches any of the 3 keywords, together with the time stamp.")

(defconst org-all-time-keywords
  (mapcar (lambda (w) (substring w 0 -1))
	  (list org-scheduled-string org-deadline-string
		org-clock-string org-closed-string))
  "List of time keywords.")

;;;; Drawer

(defvaralias 'org-drawer-regexp 'org-element-drawer-re)

(defconst org-property-start-re "^[ \t]*:PROPERTIES:[ \t]*$"
  "Regular expression matching the first line of a property drawer.")

(defconst org-property-end-re "^[ \t]*:END:[ \t]*$"
  "Regular expression matching the last line of a property drawer.")

(defconst org-clock-drawer-start-re "^[ \t]*:CLOCK:[ \t]*$"
  "Regular expression matching the first line of a clock drawer.")

(defconst org-clock-drawer-end-re "^[ \t]*:END:[ \t]*$"
  "Regular expression matching the last line of a clock drawer.")

(defconst org-logbook-drawer-re
  (rx (seq bol (0+ (any "\t ")) ":LOGBOOK:" (0+ (any "\t ")) "\n"
	   (*? (0+ nonl) "\n")
	   (0+ (any "\t ")) ":END:" (0+ (any "\t ")) eol))
  "Matches an entire LOGBOOK drawer.")

(defconst org-clock-drawer-re
  (concat "\\(" org-clock-drawer-start-re "\\)\\(?:.\\|\n\\)*?\\("
	  org-clock-drawer-end-re "\\)\n?")
  "Matches an entire clock drawer.")

;;;; Headline

(defconst org-heading-keyword-regexp-format
  "^\\(\\*+\\)\\(?: +%s\\)\\(?: +\\(.*?\\)\\)?[ \t]*$"
  "Printf format for a regexp matching a headline with some keyword.
This regexp will match the headline of any node which has the
exact keyword that is put into the format.  The keyword isn't in
any group by default, but the stars and the body are.")

(defconst org-heading-keyword-maybe-regexp-format
  "^\\(\\*+\\)\\(?: +%s\\)?\\(?: +\\(.*?\\)\\)?[ \t]*$"
  "Printf format for a regexp matching a headline, possibly with some keyword.
This regexp can match any headline with the specified keyword, or
without a keyword.  The keyword isn't in any group by default,
but the stars and the body are.")

(defconst org-archive-tag "ARCHIVE"
  "The tag that marks a subtree as archived.
An archived subtree does not open during visibility cycling, and does
not contribute to the agenda listings.")

(defconst org-tag-re "[[:alnum:]_@#%]+"
  "Regexp matching a single tag.")

(defconst org-tag-group-re "[ \t]+\\(:\\([[:alnum:]_@#%:]+\\):\\)[ \t]*$"
  "Regexp matching the tag group at the end of a line, with leading spaces.
Tags are stored in match group 1.  Match group 2 stores the tags
without the enclosing colons.")

(defconst org-tag-line-re
  "^\\*+ \\(?:.*[ \t]\\)?\\(:\\([[:alnum:]_@#%:]+\\):\\)[ \t]*$"
  "Regexp matching tags in a headline.
Tags are stored in match group 1.  Match group 2 stores the tags
without the enclosing colons.")

;;;; LaTeX Environments and Fragments

(defconst org-latex-regexps
  '(("begin" "^[ \t]*\\(\\\\begin{\\([a-zA-Z0-9\\*]+\\)\\(?:.\\|\n\\)+?\\\\end{\\2}\\)" 1 t)
    ;; ("$" "\\([ \t(]\\|^\\)\\(\\(\\([$]\\)\\([^ \t\n,.$].*?\\(\n.*?\\)\\{0,5\\}[^ \t\n,.$]\\)\\4\\)\\)\\([ \t.,?;:'\")]\\|$\\)" 2 nil)
    ("$1" "\\([^$]\\|^\\)\\(\\$[^ \t\r\n,;.$]\\$\\)\\(\\s.\\|\\s-\\|\\s(\\|\\s)\\|\\s\"\\|'\\|$\\)" 2 nil)
    ("$"  "\\([^$]\\|^\\)\\(\\(\\$\\([^ \t\n,;.$][^$\n\r]*?\\(\n[^$\n\r]*?\\)\\{0,2\\}[^ \t\n,.$]\\)\\$\\)\\)\\(\\s.\\|\\s-\\|\\s(\\|\\s)\\|\\s\"\\|'\\|$\\)" 2 nil)
    ("\\(" "\\\\(\\(?:.\\|\n\\)*?\\\\)" 0 nil)
    ("\\[" "\\\\\\[\\(?:.\\|\n\\)*?\\\\\\]" 0 nil)
    ("$$" "\\$\\$\\(?:.\\|\n\\)*?\\$\\$" 0 nil))
  "Regular expressions for matching embedded LaTeX.")

;;;; Node Property

(defconst org-effort-property "Effort"
  "The property that is being used to keep track of effort estimates.
Effort estimates given in this property need to be in the format
defined in org-duration.el.")

;;;; Superscripts and subscripts

(defconst org-match-substring-with-braces-regexp
  (concat
   "\\(\\S-\\)\\([_^]\\)"
   "\\(" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)")
  "The regular expression matching a sub- or superscript, forcing braces.")

(provide 'org-regexps)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-regexps.el ends here


