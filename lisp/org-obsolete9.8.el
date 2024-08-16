;;; org-obsolete9.8.el --- Obsolete Org mode functions and variables -*- lexical-binding: t; -*-

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

;; This file contains obsolete Org mode code that will be removed in
;; future releases.

;;; Code:

(require 'org-macs)

;;;; Groups

;; It was used only for two non-obsolete variables.
(defgroup org-keywords nil
  "Keywords in Org mode."
  :tag "Org Keywords"
  :group 'org)

;;;; Obsolete aliases

;;;; Obsolete variables

(defvar org-clock-mode-line-entry nil
  "Information for the mode line about the running clock.")
(make-obsolete-variable 'org-clock-mode-line-entry "unused" "9.8")

(define-obsolete-variable-alias 'org-ans1 'org-read-date--calendar-selected-date "9.8")
(define-obsolete-variable-alias 'org-ans2 'org-read-date--calendar-keyboard-date "9.8")
(define-obsolete-variable-alias 'org-def 'org-read-date--default-time "9.8")
(define-obsolete-variable-alias 'org-defdecode 'org-read-date--default-decoded-time "9.8")
(define-obsolete-variable-alias 'org-with-time 'org-read-date--with-time "9.8")
(define-obsolete-variable-alias 'org-read-date-inactive 'org-read-date--inactive "9.8")

(define-obsolete-variable-alias 'org-ts-type 'org-sparse-tree--current-date-type "9.8")

(defvar org-add-colon-after-tag-completion nil)  ;; dynamically scoped param
(make-obsolete-variable 'org-add-colon-after-tag-completion "unused" "9.8")

(defvar org-last-tag-selection-key nil)
(make-obsolete-variable 'org-last-tag-selection-key "unused" "9.8")

(make-obsolete-variable 'org-last-set-property-value "unused" "9.8")
(make-obsolete-variable 'org-last-set-property "unused" "9.8")

(defcustom org-self-insert-cluster-for-undo nil
  "Non-nil means cluster self-insert commands for undo when possible.
If this is set, then, like in the Emacs command loop, 20 consecutive
characters will be undone together.
This is configurable, because there is some impact on typing performance."
  :group 'org-table
  :type 'boolean)
(defvar org-self-insert-command-undo-counter 0)
(make-obsolete-variable 'org-self-insert-cluster-for-undo "no longer needed" "9.8")
(make-obsolete-variable 'org-self-insert-command-undo-counter "no longer needed" "9.8")

(defvar-local org-keyword-properties nil
  "List of property/value pairs inherited by any entry.

Valid for the current buffer.  This variable is populated from
PROPERTY keywords.

Note that properties are defined also in property drawers.
Properties defined there take precedence over properties defined
as keywords.")
(make-obsolete-variable 'org-keyword-properties "Use (org-entry-get (org-element-org-data) \"PROPERTY\") instead" "9.8")

(defvar-local org-done-keywords nil)
(make-obsolete-variable 'org-done-keywords "Use (org-element-done-keywords) instead" "9.8")

(defvar-local org-not-done-keywords nil)
(make-obsolete-variable 'org-not-done-keywords "Use (org-element-not-done-keywords) instead" "9.8")

(defvar-local org-todo-keywords-1 nil
  "All TODO and DONE keywords active in a buffer.")
(make-obsolete-variable 'org-todo-keywords-1 "Use (org-element-all-todo-keywords) instead" "9.8")

(defvar-local org-todo-heads nil)
(make-obsolete-variable 'org-todo-heads "Use (org-element-todo-sequences) instead" "9.8")

(defvar-local org-todo-sets nil)
(make-obsolete-variable 'org-todo-sets "Use (org-element-todo-sequences) instead" "9.8")

(defvar-local org-todo-kwd-alist nil)
(make-obsolete-variable 'org-todo-kwd-alist "Use (org-element-todo-sequences) instead" "9.8")

(defvar-local org-todo-key-alist nil)
(make-obsolete-variable 'org-todo-key-alist "Use (org-todo-keyword-binding-alist) instead" "9.8")

(defvar-local org-todo-key-trigger nil)
(make-obsolete-variable 'org-todo-key-trigger "Use (org-todo-keyword-binding-alist nil 'no-auto) instead" "9.8")

(defvar org-todo-keyword-alist-for-agenda nil
  "Combined `org-todo-keyword-alist' from all agenda files.
The agenda files are the files processed by
`org-agenda-prepare-buffers'.")
(make-obsolete-variable 'org-todo-keyword-alist-for-agenda "No longer used" "9.8")

(defvar org-tag-alist-for-agenda nil
  "Alist of all tags from all agenda files.
The agenda files are the files processed by
`org-agenda-prepare-buffers'.")
(make-obsolete-variable 'org-tag-alist-for-agenda "No longer used" "9.8")

(defvar org-tag-groups-alist-for-agenda nil
  "Alist of all groups tags from all current agenda files.
The agenda files are the files processed by
`org-agenda-prepare-buffers'.")
(make-obsolete-variable 'org-tag-groups-alist-for-agenda "No longer used" "9.8")

(defvar-local org-current-tag-alist nil
  "Alist of all tag groups in current buffer.
This variable takes into consideration `org-tag-alist',
`org-tag-persistent-alist' and TAGS keywords in the buffer.")
(make-obsolete-variable 'org-current-tag-alist "Use `org-local-tags-alist' or `org-overriding-tag-alist' instead" "9.8")

(defvar-local org-tag-groups-alist nil)
(make-obsolete-variable 'org-tag-groups-alist "Use (org-tag-alist-to-groups (org-local-tags-alist)) instead" "9.8")

(defvar-local org-link-abbrev-alist-local nil
  "Buffer-local version of `org-link-abbrev-alist', which see.
The value of this is taken from the LINK keywords.")
(make-obsolete-variable 'org-link-abbrev-alist-local "Use (org-element-property :link-abbrevs (org-element-org-data)) instead" "9.8")

(defvar org-file-tags nil
  "List of tags that can be inherited by all entries in the file.
The tags will be inherited if the variable `org-use-tag-inheritance'
says they should be.
This variable is populated from #+FILETAGS lines.")
(make-obsolete-variable 'org-file-tags "Use (org-element-property :tags (org-element-org-data)) instead" "9.8")

(defvar-local org-todo-regexp nil
  "Matches any of the TODO state keywords.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")
(make-obsolete-variable 'org-todo-regexp "Use (org-todo-regexp) instead" "9.8")

(defvar-local org-not-done-regexp nil
  "Matches any of the TODO state keywords except the last one.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")
(make-obsolete-variable 'org-not-done-regexp "Use (org-not-done-regexp) instead" "9.8")

(defvar-local org-not-done-heading-regexp nil
  "Matches a TODO headline that is not done.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")
(make-obsolete-variable 'org-not-done-heading-regexp "Use (format org-heading-keyword-regexp-format (org-not-done-regexp)) instead" "9.8")

(defvar-local org-todo-line-regexp nil
  "Matches a headline and puts TODO state into group 2 if present.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")
(make-obsolete-variable 'org-todo-line-regexp "Use (format org-heading-keyword-maybe-regexp-format (org-todo-regexp)) instead" "9.8")

(defvar-local org-todo-line-tags-regexp nil
  "Matches a headline and puts TODO state into group 2 if present.
Also put tags into group 4 if tags are present.")
(make-obsolete-variable 'org-todo-line-tags-regexp "No longer used" "9.8")

(defvar-local org-complex-heading-regexp nil
  "Matches a headline and puts everything into groups:

group 1: Stars
group 2: The TODO keyword, maybe
group 3: Priority cookie
group 4: True headline
group 5: Tags

Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")
(make-obsolete-variable 'org-complex-heading-regexp "Use (org-complex-heading-regexp) instead" "9.8")

(defvar-local org-complex-heading-regexp-format nil
  "Printf format to make regexp to match an exact headline.
This regexp will match the headline of any node which has the
exact headline text that is put into the format, but may have any
TODO state, priority, tags, statistics cookies (at the beginning
or end of the headline title), or COMMENT keyword.")
(make-obsolete-variable 'org-complex-heading-regexp-format
                        "Use (org-complex-heading-regexp-format) instead"
                        "9.8")

(defvar-local org-table-formula-constants-local nil
  "Local version of `org-table-formula-constants'.")
(make-obsolete-variable 'org-table-formula-constants-local
                        "Use (org-table-formula-constants-local) instead"
                        "9.8")

(defvar org-agenda-name nil)
(make-obsolete-variable 'org-agenda-name "unused" "9.8")

(defvar org-agenda-current-date nil
  "Active date when building the agenda.")
(make-obsolete-variable 'org-agenda-current-date "unused" "9.8")

(defvar org-select-this-todo-keyword nil
  "Keyword selector for todo agenda.
Should either be a keyword, \"*\", or \"|\"-separated list of todo
keywords.")
(make-obsolete-variable 'org-select-this-todo-keyword "pass argument to `org-agenda-get-todos' instead" "9.8")

;;;; Obsolete functions and macros

(declare-function org-check-and-save-marker "org-track-markers"
                  (marker beg end))
(defun org-agenda-save-markers-for-cut-and-paste (beg end)
  "Save relative positions of markers in region.
This check for agenda markers in all agenda buffers currently active."
  (require 'org-agenda-common)
  (defvar org-agenda-markers)
  (require 'org-track-markers)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'org-agenda-mode)
	(mapc (lambda (m) (org-check-and-save-marker m beg end))
	      org-agenda-markers)))))
(make-obsolete 'org-agenda-save-markers-for-cut-and-paste "no longer used" "9.8")

(defun org-clock-save-markers-for-cut-and-paste (beg end)
  "Save relative positions of markers in region BEG..END.
Save `org-clock-marker', `org-clock-hd-marker',
`org-clock-default-task', `org-clock-interrupted-task', and the
markers in `org-clock-history'."
  (require 'org-track-markers)
  (require 'org-clock-core)
  (defvar org-clock-marker)
  (defvar org-clock-hd-marker)
  (defvar org-clock-default-task)
  (defvar org-clock-interrupted-task)
  (defvar org-clock-history)
  (org-check-and-save-marker org-clock-marker beg end)
  (org-check-and-save-marker org-clock-hd-marker beg end)
  (org-check-and-save-marker org-clock-default-task beg end)
  (org-check-and-save-marker org-clock-interrupted-task beg end)
  (dolist (m org-clock-history)
    (org-check-and-save-marker m beg end)))
(make-obsolete 'org-clock-save-markers-for-cut-and-paste "no longer used" "9.8")

(defun org-remove-empty-overlays-at (pos)
  "Remove outline overlays that do not contain non-white stuff."
  (dolist (o (overlays-at pos))
    (and (eq 'outline (overlay-get o 'invisible))
	 (not (string-match-p
             "\\S-" (buffer-substring (overlay-start o)
				     (overlay-end o))))
	 (delete-overlay o))))
(make-obsolete 'org-remove-empty-overlays-at "no longer used" "9.8")

;; Use `with-silent-modifications' to ignore cosmetic changes and
;; `org-unmodified' to ignore real text modifications.
(defmacro org-unmodified (&rest body)
  "Run BODY while preserving the buffer's `buffer-modified-p' state."
  (declare (debug (body)) (obsolete with-silent-modifications "9.8"))
  (org-with-gensyms (was-modified)
    `(let ((,was-modified (buffer-modified-p)))
       (unwind-protect
           (let ((buffer-undo-list t)
		 (inhibit-modification-hooks t))
	     ,@body)
	 (set-buffer-modified-p ,was-modified)))))

(defmacro org-no-read-only (&rest body)
  "Inhibit read-only for BODY."
  (declare (debug (body)) (obsolete "no longer used" "9.8"))
  `(let ((inhibit-read-only t)) ,@body))

(defun org-uniquify-alist (alist)
  "Merge elements of ALIST with the same key.

For example, in this alist:

\(org-uniquify-alist \\='((a 1) (b 2) (a 3)))
  => ((a 1 3) (b 2))

merge (a 1) and (a 3) into (a 1 3).

The function returns the new ALIST."
  (let (rtn)
    (dolist (e alist rtn)
      (let (n)
	(if (not (assoc (car e) rtn))
	    (push e rtn)
	  (setq n (cons (car e) (append (cdr (assoc (car e) rtn)) (cdr e))))
	  (setq rtn (assq-delete-all (car e) rtn))
	  (push n rtn))))))
(make-obsolete 'org-uniquify-alist "no longer used" "9.8")

(defun org-make-parameter-alist (plist)
  "Return alist based on PLIST.
PLIST is a property list with alternating symbol names and values.
The returned alist is a list of lists with the symbol name in `car'
and the value in `cadr'."
  (when plist
    (cons (list (car plist) (cadr plist))
	  (org-make-parameter-alist (cddr plist)))))
(make-obsolete 'org-make-parameter-alist "no longer used" "9.8")

(defun org-get-at-eol (property n)
  "Get text property PROPERTY at the end of line less N characters."
  (get-text-property (- (line-end-position) n) property))
(make-obsolete 'org-get-at-eol "no longer used" "9.8")

(defvar org-last-tags-completion-table nil
  "The last used completion table for tags.")
(make-obsolete 'org-last-tags-completion-table "unused" "9.8")
(defun org-tags-completion-function (string _predicate &optional flag)
  "Complete tag STRING.
FLAG specifies the type of completion operation to perform.  This
function is passed as a collection function to `completing-read',
which see."
  (let ((completion-ignore-case nil)	;tags are case-sensitive
	(confirm (lambda (x) (stringp (car x))))
	(prefix "")
        begin)
    (when (string-match "^\\(.*[-+:&,|]\\)\\([^-+:&,|]*\\)$" string)
      (setq prefix (match-string 1 string))
      (setq begin (match-beginning 2))
      (setq string (match-string 2 string)))
    (defvar org-last-tags-completion-table) ; org-tags.el
    (pcase flag
      (`t (all-completions string org-last-tags-completion-table confirm))
      (`lambda (assoc string org-last-tags-completion-table)) ;exact match?
      (`(boundaries . ,suffix)
       (let ((end (if (string-match "[-+:&,|]" suffix)
                      (match-string 0 suffix)
                    (length suffix))))
         `(boundaries ,(or begin 0) . ,end)))
      (`nil
       (pcase (try-completion string org-last-tags-completion-table confirm)
	 ((and completion (pred stringp))
	  (concat prefix
		  completion
		  (if (and org-add-colon-after-tag-completion
			   (assoc completion org-last-tags-completion-table))
		      ":"
		    "")))
	 (completion completion)))
      (_ nil))))
(make-obsolete 'org-tags-completion-function "no longer used" "9.8")

(defun org-agenda-format-item (extra txt &optional with-level with-category tags dotime
				     remove-re habitp)
  "Format TXT to be inserted into the agenda buffer.
In particular, add the prefix and corresponding text properties.

EXTRA must be a string to replace the `%s' specifier in the prefix format.
WITH-LEVEL may be a string to replace the `%l' specifier.
WITH-CATEGORY (a string, a symbol or nil) may be used to overrule the default
category taken from local variable or file name.  It will replace the `%c'
specifier in the format.
DOTIME, when non-nil, indicates that a time-of-day should be extracted from
TXT for sorting of this entry, and for the `%t' specifier in the format.
When DOTIME is a string, this string is searched for a time before TXT is.
TAGS can be the tags of the headline.
Any match of REMOVE-RE will be removed from TXT."
  ;; We keep the org-prefix-* variable values along with a compiled
  ;; formatter, so that multiple agendas existing at the same time do
  ;; not step on each other toes.
  ;;
  ;; It was inconvenient to make these variables buffer local in
  ;; Agenda buffers, because this function expects to be called with
  ;; the buffer where item comes from being current, and not agenda
  ;; buffer
  (require 'org-agenda-line-format)
  (defvar org-prefix-format-compiled)
  (defvar org-agenda-hide-tags-regexp)
  (defvar org-agenda-time-grid)
  (defvar org-agenda-search-headline-for-time)
  (defvar org-prefix-has-time)
  (defvar org-prefix-has-tag)
  (defvar org-prefix-has-breadcrumbs)
  (defvar org-agenda-remove-times-when-in-prefix)
  (defvar org-agenda-default-appointment-duration)
  (defvar org-agenda-remove-tags)
  (defvar org-agenda-timegrid-use-ampm)
  (defvar org-prefix-category-length)
  (defvar org-prefix-category-max-length)
  (defvar org-agenda-breadcrumbs-separator)
  (declare-function org-agenda-fix-displayed-tags "org-agenda-line-format"
                    (txt tags add-inherited hide-re))
  (declare-function org-agenda-get-category-icon "org-agenda-line-format" (category))
  (declare-function org-get-time-of-day "org-agenda-line-format" (s &optional string))
  (declare-function org-agenda-time-of-day-to-ampm-maybe "org-agenda-line-format" (time))
  (require 'org-agenda-common)
  (defvar org-agenda-show-inherited-tags)
  (defvar org-agenda-contributing-files)
  (require 'org-element)
  (defvar org-ts-regexp-both)
  (defvar org-link-bracket-re)
  (require 'org-regexps)
  (defvar org-stamp-time-of-day-regexp)
  (defvar org-plain-time-of-day-regexp)
  (defvar org-tag-group-re)
  (require 'org-duration)
  (declare-function org-duration-from-minutes "org-duration"
                    (minutes &optional fmt canonical))
  (declare-function org-duration-to-minutes "org-duration" (duration &optional canonical))
  (require 'org-outline)
  (declare-function org-get-outline-path "org-outline" (&optional with-self use-cache))
  (declare-function org-format-outline-path "org-outline" (path &optional width prefix separator))
  (require 'org-priority-common)
  (defvar org-priority-highest)
  (defvar org-priority-lowest)
  (let* ((bindings (car org-prefix-format-compiled))
	 (formatter (cadr org-prefix-format-compiled)))
    (cl-loop for (var value) in bindings
	     do (set var value))
    (save-match-data
      ;; Diary entries sometimes have extra whitespace at the beginning
      (setq txt (org-trim txt))

      ;; Fix the tags part in txt
      (setq txt (org-agenda-fix-displayed-tags
		 txt tags
		 org-agenda-show-inherited-tags
		 org-agenda-hide-tags-regexp))

      (with-no-warnings
	;; `time', `tag', `effort' are needed for the eval of the prefix format.
	;; Based on what I see in `org-compile-prefix-format', I added
	;; a few more.
        (defvar breadcrumbs) (defvar category) (defvar category-icon)
        (defvar effort) (defvar extra)
        (defvar level) (defvar tag) (defvar time))
      (let* ((category (or with-category
			   (if buffer-file-name
			       (file-name-sans-extension
				(file-name-nondirectory buffer-file-name))
			     "")))
             (full-category category)
	     (category-icon (org-agenda-get-category-icon category))
	     (category-icon (if category-icon
				(propertize " " 'display category-icon)
			      ""))
	     (effort (and (not (string= txt ""))
			  (get-text-property 1 'effort txt)))
	     (tag (if tags (nth (1- (length tags)) tags) ""))
	     (time-grid-trailing-characters (nth 2 org-agenda-time-grid))
	     (extra (or (and (not habitp) extra) ""))
	     time
	     (string-containing-time
              (when dotime (concat
			    (if (stringp dotime) dotime "")
			    (and org-agenda-search-headline-for-time
                                 ;; Do not search inside
                                 ;; timestamps.  They are handled
                                 ;; separately.
                                 (replace-regexp-in-string
                                  org-ts-regexp-both ""
                                  txt)))))
	     (time-of-day (and dotime (org-get-time-of-day string-containing-time)))
	     timestamp-range? plain-time? date-range-same-day?
             time-string start-time end-time rtn
	     duration breadcrumbs)
	(and (derived-mode-p 'org-mode) buffer-file-name
	     (add-to-list 'org-agenda-contributing-files buffer-file-name))
	(when (and dotime time-of-day)
	  ;; Extract starting and ending time and move them to prefix
	  (when (or (setq timestamp-range?
                          (string-match org-stamp-time-of-day-regexp
                                        string-containing-time))
		    (setq plain-time?
                          (string-match org-plain-time-of-day-regexp
                                        string-containing-time)))
	    (setq time-string (match-string 0 string-containing-time)
		  date-range-same-day? (and timestamp-range? (match-end 3))
		  start-time (match-string (if plain-time? 1 2)
                                           string-containing-time)
		  end-time (match-string (if plain-time? 8
                                           (if date-range-same-day? 4 6))
                                         string-containing-time))

	    ;; If the times are in TXT (not in DOTIMES), and the prefix will list
	    ;; them, we might want to remove them there to avoid duplication.
	    ;; The user can turn this off with a variable.
	    (when (and org-prefix-has-time
		       org-agenda-remove-times-when-in-prefix
                       (or timestamp-range? plain-time?)
		       (string-match (concat (regexp-quote time-string) " *") txt)
		       (not (equal ?\] (string-to-char (substring txt (match-end 0)))))
		       (if (eq org-agenda-remove-times-when-in-prefix 'beg)
			   (= (match-beginning 0) 0)
			 t))
	      (setq txt (replace-match "" nil nil txt))))
          ;; Normalize the time(s) to 24 hour.
	  (when start-time (setq start-time (org-get-time-of-day start-time t)))
	  (when end-time (setq end-time (org-get-time-of-day end-time t)))
	  ;; Try to set s2 if s1 and
	  ;; `org-agenda-default-appointment-duration' are set
	  (when (and start-time (not end-time)
                     org-agenda-default-appointment-duration)
	    (setq end-time
	          (org-duration-from-minutes
	           (+ (org-duration-to-minutes start-time t)
		      org-agenda-default-appointment-duration)
	           nil t)))
	  ;; Compute the duration
	  (when end-time
	    (setq duration (- (org-duration-to-minutes end-time)
			      (org-duration-to-minutes start-time))))
          ;; Format S1 and S2 for display.
	  (when start-time
            (setq start-time (format "%5s" (org-get-time-of-day start-time 'overtime))))
	  (when end-time
            (setq end-time (org-get-time-of-day end-time 'overtime))))
	(when (string-match org-tag-group-re txt)
	  ;; Tags are in the string
	  (if (or (eq org-agenda-remove-tags t)
		  (and org-agenda-remove-tags
		       org-prefix-has-tag))
	      (setq txt (replace-match "" t t txt))
	    (setq txt (replace-match
		       (concat (make-string (max (- 50 (length txt)) 1) ?\ )
			       (match-string 1 txt))
		       t t txt))))

	(when remove-re
	  (while (string-match remove-re txt)
	    (setq txt (replace-match "" t t txt))))

	;; Set org-heading property on `txt' to mark the start of the
	;; heading.
	(add-text-properties 0 (length txt) '(org-heading t) txt)

	;; Prepare the variables needed in the eval of the compiled format
	(when org-prefix-has-breadcrumbs
	  (setq breadcrumbs
                ;; When called from Org buffer, remain in position.
                ;; When called from Agenda buffer, jump to headline position first.
                (org-with-point-at (org-get-at-bol 'org-marker)
		  (let ((s (if (derived-mode-p 'org-mode)
                               (org-format-outline-path (org-get-outline-path)
						        (1- (frame-width))
						        nil org-agenda-breadcrumbs-separator)
                             ;; Not in Org buffer.  This can happen,
                             ;; for example, in
                             ;; `org-agenda-add-time-grid-maybe' where
                             ;; time grid does not correspond to a
                             ;; particular heading.
                             "")))
		    (if (equal "" s) "" (concat s org-agenda-breadcrumbs-separator))))))
	(setq time (cond (end-time
                          (concat
			   (org-agenda-time-of-day-to-ampm-maybe start-time)
			   "-" (org-agenda-time-of-day-to-ampm-maybe end-time)
			   (when org-agenda-timegrid-use-ampm " ")))
			 (start-time
                          (concat
			   (org-agenda-time-of-day-to-ampm-maybe start-time)
			   (if org-agenda-timegrid-use-ampm
                               (concat time-grid-trailing-characters " ")
                             time-grid-trailing-characters)))
			 (t ""))
	      category (if (symbolp category) (symbol-name category) category)
	      level (or with-level ""))
	(if (string-match org-link-bracket-re category)
	    (let ((link-width (string-width (or (match-string 2) (match-string 1)))))
	      (when (< link-width (or org-prefix-category-length 0))
	        (setq category (copy-sequence category))
	        (org-add-props category nil
		  'extra-space (make-string
			        (- org-prefix-category-length link-width 1) ?\ ))))
	  (when (and org-prefix-category-max-length
		     (>= (length category) org-prefix-category-max-length))
	    (setq category (substring category 0 (1- org-prefix-category-max-length)))))
	;; Evaluate the compiled format
	(setq rtn (concat (eval formatter t) txt))

	;; And finally add the text properties
	(remove-text-properties 0 (length rtn) '(line-prefix t wrap-prefix t) rtn)
	(org-add-props rtn nil
          ;; CATEGORY might be truncated.  Store the full category in
          ;; the properties.
	  'org-category full-category
          'tags tags
          'org-priority-highest org-priority-highest
	  'org-priority-lowest org-priority-lowest
	  'time-of-day time-of-day
	  'duration duration
	  'breadcrumbs breadcrumbs
	  'txt txt
	  'level level
	  'time time
	  'extra extra
	  'format org-prefix-format-compiled
	  'dotime dotime)))))
(make-obsolete
 'org-agenda-format-item
 "Use `org-agenda-format-heading' or `org-agenda-format-line' instead"
 "9.8")

(defun org-agenda-skip-eval (form)
  "If FORM is a function or a list, call (or eval) it and return the result.
`save-excursion' and `save-match-data' are wrapped around the call, so point
and match data are returned to the previous state no matter what these
functions do."
  (let (fp)
    (and form
	 (or (setq fp (functionp form))
	     (consp form))
	 (save-excursion
	   (save-match-data
	     (if fp
		 (funcall form)
	       (eval form t)))))))
(make-obsolete
 'org-agenda-skip-eval
 "Use `org-eval-form'instead"
 "9.8")

;;;; Helpers

(defun org--set-obsolete-regexps-and-options (org-data &optional tags-only)
  "Set obsolete regexp variables in current buffer according to ORG-DATA.
When TAGS-ONLY is non-nil, only set tag-related variables."
  (with-no-warnings
    (setq-local org-file-tags
		(mapcar #'org-add-prop-inherited
                        (org-element-property :tags org-data)))
    ;; Used for tag completion.
    (setq org-current-tag-alist (org-local-tags-alist org-data))
    ;; Used for tag completion.
    (setq org-tag-groups-alist
	  (org-tag-alist-to-groups org-current-tag-alist))
    (unless tags-only
      ;; Constants.
      (let ((store nil))
	(dolist (pair (cl-mapcan #'split-string
				 (org-element-property :CONSTANTS org-data)))
	  (when (string-match "^\\([a-zA-Z0][_a-zA-Z0-9]*\\)=\\(.*\\)" pair)
	    (let* ((name (match-string 1 pair))
		   (value (match-string 2 pair))
		   (old (assoc name store)))
	      (if old (setcdr old value)
		(push (cons name value) store)))))
	(setq org-table-formula-constants-local store))
      ;; FIXME: `org-keyword-properties' is set for backwards
      ;; compatibility.  org-data element properties should be used
      ;; instead.
      (let ((properties nil))
	(dolist (value (org-element-property :PROPERTY org-data))
	  (when (string-match "\\(\\S-+\\)[ \t]+\\(.*\\)" value)
	    (setq properties (org--update-property-plist
			      (match-string-no-properties 1 value)
			      (match-string-no-properties 2 value)
			      properties))))
	(setq-local org-keyword-properties properties))
      ;; Category.
      (let ((category (org-element-property :CATEGORY org-data)))
	(when category
	  (setq-local org-category (intern category))
	  (setq-local org-keyword-properties
		      (org--update-property-plist
		       "CATEGORY" category org-keyword-properties))))
      ;; Link abbreviations.
      (with-no-warnings
        (setq org-link-abbrev-alist-local
              (org-element-property :link-abbrevs org-data)))
      ;; TODO keywords.
      (with-no-warnings
	(setq-local org-todo-heads nil)
	(setq-local org-todo-sets nil)
        (setq-local org-todo-kwd-alist nil)
        (setq-local org-todo-key-alist nil)
        (setq-local org-todo-key-trigger nil))
      
      (let ((sequences (org-element-property :todo-keyword-sequences org-data)))
        (with-no-warnings
          ;; org-todo-keywords-1
          (setq org-todo-keywords-1 (org-element-property :todo-keywords org-data))
          ;; org-done-keywords
          (setq org-done-keywords (org-element-property :done-keywords org-data))
          ;; org-not-done-keywords
          (setq org-not-done-keywords (org-element-property :not-done-keywords org-data)))
        ;; org-todo-heads
        ;; org-todo-sets
        ;; org-todo-key-alist
        ;; org-todo-kwd-alist
        (dolist (sequence sequences)
          (with-no-warnings
            ;; SEQUENCE = (TYPE ((KWD1 . OPT1) (KWD2 . OPT2) ...) ((DONE-KWD1 . OPT1) ...))
            (push (caar (nth 1 sequence)) org-todo-heads)
            (push (mapcar #'car (nth 1 sequence)) org-todo-sets))
          (let ((kwd-tail (list (car sequence) ;; TYPE
                                (with-no-warnings (car org-todo-heads)) ;; First keyword
                                (car (car (nth 2 sequence))) ;; First done keyword
                                (car (org-last (nth 2 sequence))) ;; Last done keyword
                                )))
            (dolist (pair (nth 1 sequence))
              (pcase pair
                (`(,(and (pred stringp) name) .
                   ,setting)
                 (push (cons name kwd-tail) org-todo-kwd-alist))))))
        (with-no-warnings
          (setq org-todo-heads (nreverse org-todo-heads)
                org-todo-sets (nreverse org-todo-sets)
                org-todo-kwd-alist (nreverse org-todo-kwd-alist)
                ;; org-todo-key-trigger
	        org-todo-key-trigger (delq nil (mapcar #'cdr (org-todo-keyword-binding-alist org-data 'no-auto-keys)))
                org-todo-key-alist (org-todo-keyword-binding-alist org-data))))
      
      ;; Compute the regular expressions and other local variables.
      ;; Using `org-outline-regexp-bol' would complicate them much,
      ;; because of the fixed white space at the end of that string.
      (with-no-warnings
	(setq org-todo-regexp (org-element-property :todo-regexp org-data)
	      org-not-done-regexp (org-not-done-regexp org-data)
	      org-not-done-heading-regexp
	      (format org-heading-keyword-regexp-format org-not-done-regexp)
	      org-todo-line-regexp
	      (format org-heading-keyword-maybe-regexp-format org-todo-regexp)
	      org-complex-heading-regexp
	      (concat "^\\(\\*+\\)"
		      "\\(?: +" org-todo-regexp "\\)?"
		      "\\(?: +\\(\\[#.\\]\\)\\)?"
		      "\\(?: +\\(.*?\\)\\)??"
		      "\\(?:[ \t]+\\(:[[:alnum:]_@#%:]+:\\)\\)?"
		      "[ \t]*$")
	      org-complex-heading-regexp-format
	      (concat "^\\(\\*+\\)"
		      "\\(?: +" org-todo-regexp "\\)?"
		      "\\(?: +\\(\\[#.\\]\\)\\)?"
		      "\\(?: +"
                      ;; Headline might be commented
                      "\\(?:" org-comment-string " +\\)?"
		      ;; Stats cookies can be stuck to body.
		      "\\(?:\\[[0-9%%/]+\\] *\\)*"
		      "\\(%s\\)"
		      "\\(?: *\\[[0-9%%/]+\\]\\)*"
		      "\\)"
		      "\\(?:[ \t]+\\(:[[:alnum:]_@#%%:]+:\\)\\)?"
		      "[ \t]*$")
	      org-todo-line-tags-regexp
	      (concat "^\\(\\*+\\)"
		      "\\(?: +" org-todo-regexp "\\)?"
		      "\\(?: +\\(.*?\\)\\)??"
		      "\\(?:[ \t]+\\(:[[:alnum:]:_@#%]+:\\)\\)?"
		      "[ \t]*$"))))))

(provide 'org-obsolete9.8)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-obsolete9.8.el ends here
