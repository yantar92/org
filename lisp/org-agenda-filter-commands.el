;;; org-agenda-filter-commands.el --- Filtering visible entries in agenda  -*- lexical-binding: t; -*-

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

;; This file implements user commands to manipulate agenda filtering.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-agenda-mode)
(require 'org-property-core)

(defcustom org-agenda-filter-hook nil
  "Hook run just after filtering with `org-agenda-filter'."
  :group 'org-agenda-startup
  :package-version '(Org . "9.4")
  :type 'hook)

(defcustom org-agenda-auto-exclude-function nil
  "A function called with a tag to decide if it is filtered on \
\\<org-agenda-mode-map>`\\[org-agenda-filter-by-tag] RET'.
The sole argument to the function, which is called once for each
possible tag, is a string giving the name of the tag.  The
function should return either nil if the tag should be included
as normal, \"-<TAG>\" to exclude the tag, or \"+<TAG>\" to exclude
lines not carrying this tag.
Note that for the purpose of tag filtering, only the lower-case version of
all tags will be considered, so that this function will only ever see
the lower-case version of all tags."
  :group 'org-agenda
  :type '(choice (const nil) (function)))

(defvar org-global-tags-completion-table nil)

;;; Individual filter

;;;###autoload
(defun org-agenda-filter-by-category (strip)
  "Filter lines in the agenda buffer that have a specific category.
The category is that of the current line.
With a `\\[universal-argument]' prefix argument, exclude the lines of that category.
When there is already a category filter in place, this command removes the
filter."
  (interactive "P")
  (if (and org-agenda-filtered-by-category
	   org-agenda-category-filter)
      (org-agenda-filter-show-all-cat)
    (let ((cat (org-no-properties (org-agenda-get-category))))
      (cond
       ((and cat strip)
        (org-agenda-filter-apply
         (push (concat "-" cat) org-agenda-category-filter) 'category))
       (cat
        (org-agenda-filter-apply
         (setq org-agenda-category-filter
	       (list (concat "+" cat)))
	 'category))
       (t (error "No category at point"))))))

;;;###autoload
(defun org-agenda-filter-by-top-headline (strip)
  "Keep only those lines that are descendants from the same top headline.
The top headline is that of the current line.  With prefix arg STRIP, hide
all lines of the category at point."
  (interactive "P")
  (if org-agenda-filtered-by-top-headline
      (progn
        (setq org-agenda-filtered-by-top-headline nil
	      org-agenda-top-headline-filter nil)
        (org-agenda-filter-show-all-top-filter))
    (let ((toph (org-find-top-headline (org-get-at-bol 'org-hd-marker))))
      (if toph (org-agenda-filter-top-headline-apply toph strip)
        (error "No top-level headline at point")))))

(defvar org-agenda-regexp-filter nil)
;;;###autoload
(defun org-agenda-filter-by-regexp (strip-or-accumulate)
  "Filter agenda entries by a regular expressions.
You will be prompted for the regular expression, and the agenda
view will only show entries that are matched by that expression.

With one `\\[universal-argument]' prefix argument, hide entries matching the regexp.
When there is already a regexp filter active, this command removed the
filter.  However, with two `\\[universal-argument]' prefix arguments, add a new condition to
an already existing regexp filter."
  (interactive "P")
  (let* ((strip (equal strip-or-accumulate '(4)))
	 (accumulate (equal strip-or-accumulate '(16))))
    (cond
     ((and org-agenda-regexp-filter (not accumulate))
      (org-agenda-filter-show-all-re)
      (message "Regexp filter removed"))
     (t (let ((flt (concat (if strip "-" "+")
			   (read-from-minibuffer
			    (if strip
				"Hide entries matching regexp: "
			      "Narrow to entries matching regexp: ")))))
	  (push flt org-agenda-regexp-filter)
	  (org-agenda-filter-apply org-agenda-regexp-filter 'regexp))))))

(defvar org-agenda-effort-filter nil)
;;;###autoload
(defun org-agenda-filter-by-effort (strip-or-accumulate)
  "Filter agenda entries by effort.
With no `\\[universal-argument]' prefix argument, keep entries matching the effort condition.
With one `\\[universal-argument]' prefix argument, filter out entries matching the condition.
With two `\\[universal-argument]' prefix arguments, add a second condition to the existing filter.
This last option is in practice not very useful, but it is available for
consistency with the other filter commands."
  (interactive "P")
  (let* ((efforts (split-string
		   (or (cdr (assoc-string (concat org-effort-property "_ALL")
					  org-global-properties
					  t))
		       "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00")))
	 ;; XXX: the following handles only up to 10 different
	 ;; effort values.
	 (allowed-keys (if (null efforts) nil
			 (mapcar (lambda (n) (mod n 10)) ;turn 10 into 0
				 (number-sequence 1 (length efforts)))))
	 (keep (equal strip-or-accumulate '(16)))
	 (negative (equal strip-or-accumulate '(4)))
	 (current org-agenda-effort-filter)
	 (op nil))
    (while (not (memq op '(?< ?> ?= ?_)))
      (setq op (read-char-exclusive
		"Effort operator? (> = or <)     or press `_' again to remove filter")))
    ;; Select appropriate duration.  Ignore non-digit characters.
    (if (eq op ?_)
	(progn
	  (org-agenda-filter-show-all-effort)
	  (message "Effort filter removed"))
      (let ((prompt
	     (apply #'format
		    (concat "Effort %c "
			    (mapconcat (lambda (s) (concat "[%d]" s))
				       efforts
				       " "))
		    op allowed-keys))
	    (eff -1))
	(while (not (memq eff allowed-keys))
	  (message prompt)
	  (setq eff (- (read-char-exclusive) 48)))
	(org-agenda-filter-show-all-effort)
	(setq org-agenda-effort-filter
	      (append
	       (list (concat (if negative "-" "+")
			     (char-to-string op)
			     ;; Numbering is 1 2 3 ... 9 0, but we want
			     ;; 0 1 2 ... 8 9.
			     (nth (mod (1- eff) 10) efforts)))
	       (if keep current nil)))
	(org-agenda-filter-apply org-agenda-effort-filter 'effort)))))

;;;###autoload
(defun org-agenda-filter (&optional strip-or-accumulate)
  "Prompt for a general filter string and apply it to the agenda.

The string may contain filter elements like

+category
+tag
+<effort        > and = are also allowed as effort operators
+/regexp/

Instead of `+', `-' is allowed to strip the agenda of matching entries.
`+' is optional if it is not required to separate two string parts.
Multiple filter elements can be concatenated without spaces, for example

     +work-John<0:10-/plot/

selects entries with category `work' and effort estimates below 10 minutes,
and deselects entries with tag `John' or matching the regexp `plot'.

During entry of the filter, completion for tags, categories and effort
values is offered.  Since the syntax for categories and tags is identical
there should be no overlap between categories and tags.  If there is, tags
get priority.

A single `\\[universal-argument]' prefix arg STRIP-OR-ACCUMULATE will negate the
entire filter, which can be useful in connection with the prompt history.

A double `\\[universal-argument] \\[universal-argument]' prefix arg will add the new filter elements to the
existing ones.  A shortcut for this is to add an additional `+' at the
beginning of the string, like `+-John'.

With a triple prefix argument, execute the computed filtering defined in
the variable `org-agenda-auto-exclude-function'."
  (interactive "P")
  (if (equal strip-or-accumulate '(64))
      ;; Execute the auto-exclude action
      (if (not org-agenda-auto-exclude-function)
	  (user-error "`org-agenda-auto-exclude-function' is undefined")
	(org-agenda-filter-show-all-tag)
	(setq org-agenda-tag-filter nil)
	(dolist (tag (org-agenda-get-represented-tags))
	  (let ((modifier (funcall org-agenda-auto-exclude-function tag)))
	    (when modifier
	      (push modifier org-agenda-tag-filter))))
	(unless (null org-agenda-tag-filter)
	  (org-agenda-filter-apply org-agenda-tag-filter 'tag 'expand)))
    ;; Prompt for a filter and act
    (let* ((tag-list (org-agenda-get-represented-tags))
	   (category-list (org-agenda-get-represented-categories))
	   (negate (equal strip-or-accumulate '(4)))
	   (cf (mapconcat #'identity org-agenda-category-filter ""))
	   (tf (mapconcat #'identity org-agenda-tag-filter ""))
	   ;; (rpl-fn (lambda (c) (replace-regexp-in-string "^\\+" "" (or (car c) ""))))
	   (ef (replace-regexp-in-string "^\\+" "" (or (car org-agenda-effort-filter) "")))
	   (rf (replace-regexp-in-string "^\\+" "" (or (car org-agenda-regexp-filter) "")))
	   (ff (concat cf tf ef (when (not (equal rf "")) (concat "/" rf "/"))))
	   (f-string (completing-read
		      (concat
		       (if negate "Negative filter" "Filter")
		       " [+cat-tag<0:10-/regexp/]: ")
		      #'org-agenda-filter-completion-function
		      nil nil ff))
	   (keep (or (if (string-match "^\\+[+-]" f-string)
			 (progn (setq f-string (substring f-string 1)) t))
		     (equal strip-or-accumulate '(16))))
	   (fc (if keep org-agenda-category-filter))
	   (ft (if keep org-agenda-tag-filter))
	   (fe (if keep org-agenda-effort-filter))
	   (fr (if keep org-agenda-regexp-filter))
	   pm s)
      ;; If the filter contains a double-quoted string, replace a
      ;; single hyphen by the arbitrary and temporary string "~~~"
      ;; to disambiguate such hyphens from syntactic ones.
      (setq f-string (replace-regexp-in-string
		      "\"\\([^\"]*\\)-\\([^\"]*\\)\"" "\"\\1~~~\\2\"" f-string))
      (while (string-match "^[ \t]*\\([-+]\\)?\\(\\([^-+<>=/ \t]+\\)\\|\\([<>=][0-9:]+\\)\\|\\(/\\([^/]+\\)/?\\)\\)" f-string)
	(setq pm (if (match-beginning 1) (match-string 1 f-string) "+"))
	(when negate
	  (setq pm (if (equal pm "+") "-" "+")))
	(cond
	 ((match-beginning 3)
	  ;; category or tag
	  (setq s (replace-regexp-in-string ; Remove the temporary special string.
		   "~~~" "-" (match-string 3 f-string)))
	  (cond
	   ((member s tag-list)
	    (org-pushnew-to-end (concat pm s) ft))
	   ((member s category-list)
	    (org-pushnew-to-end (concat pm ; Remove temporary double quotes.
				        (replace-regexp-in-string "\"\\(.*\\)\"" "\\1" s))
				fc))
	   (t (message
	       "`%s%s' filter ignored because tag/category is not represented"
	       pm s))))
	 ((match-beginning 4)
	  ;; effort
	  (org-pushnew-to-end (concat pm (match-string 4 f-string)) fe))
	 ((match-beginning 5)
	  ;; regexp
	  (org-pushnew-to-end (concat pm (match-string 6 f-string)) fr)))
	(setq f-string (substring f-string (match-end 0))))
      (org-agenda-filter-remove-all)
      (and fc (org-agenda-filter-apply
	       (setq org-agenda-category-filter fc) 'category))
      (and ft (org-agenda-filter-apply
	       (setq org-agenda-tag-filter ft) 'tag 'expand))
      (and fe (org-agenda-filter-apply
	       (setq org-agenda-effort-filter fe) 'effort))
      (and fr (org-agenda-filter-apply
	       (setq org-agenda-regexp-filter fr) 'regexp))
      (run-hooks 'org-agenda-filter-hook))))

(defun org-agenda-filter-completion-function (string _predicate &optional flag)
  "Complete a complex filter string.
FLAG specifies the type of completion operation to perform.  This
function is passed as a collection function to `completing-read',
which see."
  (let ((completion-ignore-case t)	;tags are case-sensitive
	(confirm (lambda (x) (stringp x)))
	(prefix "")
	(operator "")
	table
        begin)
    (when (string-match "^\\(.*\\([-+<>=]\\)\\)\\([^-+<>=]*\\)$" string)
      (setq prefix (match-string 1 string)
	    operator (match-string 2 string)
            begin (match-beginning 3)
	    string (match-string 3 string)))
    (cond
     ((member operator '("+" "-" "" nil))
      (setq table (append (org-agenda-get-represented-categories)
			  (org-agenda-get-represented-tags))))
     ((member operator '("<" ">" "="))
      (setq table (split-string
		   (or (cdr (assoc-string (concat org-effort-property "_ALL")
					  org-global-properties
					  t))
		       "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00")
		   " +")))
     (t (setq table nil)))
    (pcase flag
      (`t (all-completions string table confirm))
      (`lambda (assoc string table)) ;exact match?
      (`(boundaries . ,suffix)
       (let ((end (if (string-match "[-+<>=]" suffix)
                      (match-string 0 suffix)
                    (length suffix))))
         `(boundaries ,(or begin 0) . ,end)))
      (`nil
       (pcase (try-completion string table confirm)
	 ((and completion (pred stringp))
	  (concat prefix completion))
	 (completion completion)))
      (_ nil))))

;;;###autoload
(defun org-agenda-filter-remove-all ()
  "Remove all filters from the current agenda buffer."
  (interactive)
  (when org-agenda-tag-filter
    (org-agenda-filter-show-all-tag))
  (when org-agenda-category-filter
    (org-agenda-filter-show-all-cat))
  (when org-agenda-regexp-filter
    (org-agenda-filter-show-all-re))
  (when org-agenda-top-headline-filter
    (org-agenda-filter-show-all-top-filter))
  (when org-agenda-effort-filter
    (org-agenda-filter-show-all-effort))
  (org-agenda-finalize)
  (when (called-interactively-p 'interactive)
    (message "All agenda filters removed")))

(declare-function org-global-tags-completion-table "org-tags" (&optional files))
;;;###autoload
(defun org-agenda-filter-by-tag (strip-or-accumulate &optional char exclude)
  "Keep only those lines in the agenda buffer that have a specific tag.

The tag is selected with its fast selection letter, as configured.

With a `\\[universal-argument]' prefix, apply the filter negatively, stripping all matches.

With a `\\[universal-argument] \\[universal-argument]' prefix, add the new tag to the existing filter
instead of replacing it.

With a `\\[universal-argument] \\[universal-argument] \\[universal-argument]' prefix, filter the literal tag, \
i.e. don't
filter on all its group members.

A Lisp caller can specify CHAR.  EXCLUDE means that the new tag
should be used to exclude the search - the interactive user can
also press `-' or `+' to switch between filtering and excluding."
  (interactive "P")
  (let* ((alist org-tag-alist-for-agenda)
	 (seen-chars nil)
	 (tag-chars (mapconcat
		     (lambda (x) (if (and (not (symbolp (car x)))
				     (cdr x)
				     (not (member (cdr x) seen-chars)))
				(progn
				  (push (cdr x) seen-chars)
				  (char-to-string (cdr x)))
			      ""))
		     org-tag-alist-for-agenda ""))
	 (valid-char-list (append '(?\t ?\r ?\\ ?. ?\s ?q)
				  (string-to-list tag-chars)))
	 (exclude (or exclude (equal strip-or-accumulate '(4))))
	 (accumulate (equal strip-or-accumulate '(16)))
	 (expand (not (equal strip-or-accumulate '(64))))
	 (inhibit-read-only t)
	 (current org-agenda-tag-filter)
	 a tag) ;; n
    (unless char
      (while (not (memq char valid-char-list))
	(org-unlogged-message
	 "%s by tag%s: [%s ]tag-char [TAB]tag %s[\\]off [q]uit"
	 (if exclude "Exclude[+]" "Filter[-]")
	 (if expand "" " (no grouptag expand)")
	 tag-chars
	 (if org-agenda-auto-exclude-function "[RET] " ""))
	(setq char (read-char-exclusive))
	;; Excluding or filtering down
	(cond ((eq char ?-) (setq exclude t))
	      ((eq char ?+) (setq exclude nil)))))
    (when (eq char ?\t)
      (unless (local-variable-p 'org-global-tags-completion-table)
        (require 'org-tags)
	(setq-local org-global-tags-completion-table
		    (org-global-tags-completion-table)))
      (let ((completion-ignore-case t))
	(setq tag (completing-read
		   "Tag: " org-global-tags-completion-table nil t))))
    (cond
     ((eq char ?\r)
      (org-agenda-filter-show-all-tag)
      (when org-agenda-auto-exclude-function
	(setq org-agenda-tag-filter nil)
	(dolist (tag (org-agenda-get-represented-tags))
	  (let ((modifier (funcall org-agenda-auto-exclude-function tag)))
	    (when modifier
	      (push modifier org-agenda-tag-filter))))
	(unless (null org-agenda-tag-filter)
	  (org-agenda-filter-apply org-agenda-tag-filter 'tag expand))))
     ((eq char ?\\)
      (org-agenda-filter-show-all-tag)
      (when (assoc-default 'tag org-agenda-filters-preset)
	(org-agenda-filter-apply org-agenda-tag-filter 'tag expand)))
     ((eq char ?.)
      (setq org-agenda-tag-filter
	    (mapcar (lambda(tag) (concat "+" tag))
		    (org-get-at-bol 'tags)))
      (org-agenda-filter-apply org-agenda-tag-filter 'tag expand))
     ((eq char ?q)) ;If q, abort (even if there is a q-key for a tag...)
     ((or (eq char ?\s)
	  (setq a (rassoc char alist))
	  (and tag (setq a (cons tag nil))))
      (org-agenda-filter-show-all-tag)
      (setq tag (car a))
      (setq org-agenda-tag-filter
	    (cons (concat (if exclude "-" "+") tag)
		  (if accumulate current nil)))
      (org-agenda-filter-apply org-agenda-tag-filter 'tag expand))
     (t (error "Invalid tag selection character %c" char)))))

(defun org-agenda-get-represented-categories ()
  "Return a list of all categories used in this agenda buffer."
  (or org-agenda-represented-categories
      (when (derived-mode-p 'org-agenda-mode)
	(let ((pos (point-min)) categories)
	  (while (and (< pos (point-max))
		      (setq pos (next-single-property-change
				 pos 'org-category nil (point-max))))
	    (push (get-text-property pos 'org-category) categories))
	  (setq org-agenda-represented-categories
		;; Enclose category names with a hyphen in double
		;; quotes to process them specially in `org-agenda-filter'.
		(mapcar (lambda (s) (if (string-match-p "-" s) (format "\"%s\"" s) s))
			(nreverse (org-uniquify (delq nil categories)))))))))

(defvar org-tag-groups-alist-for-agenda)
(defun org-agenda-get-represented-tags ()
  "Return a list of all tags used in this agenda buffer.
These will be lower-case, for filtering."
  (or org-agenda-represented-tags
      (when (derived-mode-p 'org-agenda-mode)
	(let ((pos (point-min)) tags-lists tt)
	  (while (and (< pos (point-max))
		      (setq pos (next-single-property-change
				 pos 'tags nil (point-max))))
	    (setq tt (get-text-property pos 'tags))
	    (if tt (push tt tags-lists)))
	  (setq tags-lists
		(nreverse (org-uniquify
			   (delq nil (apply #'append tags-lists)))))
	  (dolist (tag tags-lists)
	    (mapc
	     (lambda (group)
	       (when (member tag group)
		 (push (car group) tags-lists)))
	     org-tag-groups-alist-for-agenda))
	  (setq org-agenda-represented-tags tags-lists)))))

(defun org-agenda-compare-effort (op value)
  "Compare the effort of the current line with VALUE, using OP.
If the line does not have an effort defined, return nil."
  ;; `effort-minutes' property cannot be extracted directly from
  ;; current line but is stored as a property in `txt'.
  (let ((effort (get-text-property 0 'effort-minutes (org-get-at-bol 'txt))))
    (funcall op
	     (or effort (if org-agenda-sort-noeffort-is-high 32767 -1))
	     value)))

(defun org-agenda-remove-filter (type)
  "Remove filter of type TYPE from the agenda buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t) pos)
      (while (setq pos (text-property-any (point) (point-max)
					  'org-filter-type type))
	(goto-char pos)
	(remove-text-properties
	 (point) (next-single-property-change (point) 'org-filter-type)
	 `(invisible org-filtered org-filter-type ,type))))
    (set (intern (format "org-agenda-%s-filter" (intern-soft type))) nil)
    (setq org-agenda-filter-form nil)
    (org-agenda-set-mode-name)
    (org-agenda-finalize)))

(defun org-agenda-filter-show-all-tag nil
  (org-agenda-remove-filter 'tag))
(defun org-agenda-filter-show-all-re nil
  (org-agenda-remove-filter 'regexp))
(defun org-agenda-filter-show-all-effort nil
  (org-agenda-remove-filter 'effort))
(defun org-agenda-filter-show-all-cat nil
  (org-agenda-remove-filter 'category))
(defun org-agenda-filter-show-all-top-filter nil
  (org-agenda-remove-filter 'top-headline))

(provide 'org-agenda-filter-commands)

;;; org-agenda-filter-commands.el ends here
