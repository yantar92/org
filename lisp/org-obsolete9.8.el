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
(make-obsolete 'org-clock-mode-line-entry "unused" "9.8")

(define-obsolete-variable-alias 'org-ans1 'org-read-date--calendar-selected-date "9.8")
(define-obsolete-variable-alias 'org-ans2 'org-read-date--calendar-keyboard-date "9.8")
(define-obsolete-variable-alias 'org-def 'org-read-date--default-time "9.8")
(define-obsolete-variable-alias 'org-defdecode 'org-read-date--default-decoded-time "9.8")
(define-obsolete-variable-alias 'org-with-time 'org-read-date--with-time "9.8")
(define-obsolete-variable-alias 'org-read-date-inactive 'org-read-date--inactive "9.8")

(define-obsolete-variable-alias 'org-ts-type 'org-sparse-tree--current-date-type "9.8")

(defvar org-add-colon-after-tag-completion nil)  ;; dynamically scoped param
(make-obsolete 'org-add-colon-after-tag-completion "unused" "9.8")

(defvar org-last-tag-selection-key nil)
(make-obsolete 'org-last-tag-selection-key "unused" "9.8")

(make-obsolete 'org-last-set-property-value "unused" "9.8")
(make-obsolete 'org-last-set-property "unused" "9.8")

(defcustom org-self-insert-cluster-for-undo nil
  "Non-nil means cluster self-insert commands for undo when possible.
If this is set, then, like in the Emacs command loop, 20 consecutive
characters will be undone together.
This is configurable, because there is some impact on typing performance."
  :group 'org-table
  :type 'boolean)
(defvar org-self-insert-command-undo-counter 0)
(make-obsolete 'org-self-insert-cluster-for-undo "no longer needed" "9.8")
(make-obsolete 'org-self-insert-command-undo-counter "no longer needed" "9.8")

(defvar-local org-keyword-properties nil
  "List of property/value pairs inherited by any entry.

Valid for the current buffer.  This variable is populated from
PROPERTY keywords.

Note that properties are defined also in property drawers.
Properties defined there take precedence over properties defined
as keywords.")
(make-obsolete 'org-keyword-properties "Use (org-entry-get (org-element-org-data) \"PROPERTY\") instead" "9.8")

(defvar-local org-done-keywords nil)
(make-obsolete 'org-done-keywords "Use (org-element-done-keywords) instead" "9.8")

(defvar-local org-not-done-keywords nil)
(make-obsolete 'org-not-done-keywords "Use (org-element-not-done-keywords) instead" "9.8")

(defvar-local org-todo-keywords-1 nil
  "All TODO and DONE keywords active in a buffer.")
(make-obsolete 'org-todo-keywords-1 "Use (org-element-all-todo-keywords) instead" "9.8")

(defvar-local org-todo-heads nil)
(make-obsolete 'org-todo-heads "Use (org-element-todo-sequences) instead" "9.8")

(defvar-local org-todo-sets nil)
(make-obsolete 'org-todo-sets "Use (org-element-todo-sequences) instead" "9.8")

(defvar-local org-todo-kwd-alist nil)
(make-obsolete 'org-todo-kwd-alist "Use (org-element-todo-sequences) instead" "9.8")

(defvar-local org-todo-key-alist nil)
(make-obsolete 'org-todo-key-alist "Use (org-todo-keyword-binding-alist) instead" "9.8")

(defvar-local org-todo-key-trigger nil)
(make-obsolete 'org-todo-key-trigger "Use (org-todo-keyword-binding-alist nil 'no-auto) instead" "9.8")

(defvar org-todo-keyword-alist-for-agenda nil
  "Combined `org-todo-keyword-alist' from all agenda files.
The agenda files are the files processed by
`org-agenda-prepare-buffers'.")
(make-obsolete 'org-todo-keyword-alist-for-agenda "No longer used" "9.8")

(defvar org-tag-alist-for-agenda nil
  "Alist of all tags from all agenda files.
The agenda files are the files processed by
`org-agenda-prepare-buffers'.")
(make-obsolete 'org-tag-alist-for-agenda "No longer used" "9.8")

(defvar org-tag-groups-alist-for-agenda nil
  "Alist of all groups tags from all current agenda files.
The agenda files are the files processed by
`org-agenda-prepare-buffers'.")
(make-obsolete 'org-tag-groups-alist-for-agenda "No longer used" "9.8")

(defvar-local org-current-tag-alist nil
  "Alist of all tag groups in current buffer.
This variable takes into consideration `org-tag-alist',
`org-tag-persistent-alist' and TAGS keywords in the buffer.")
(make-obsolete 'org-current-tag-alist "Use `org-local-tags-alist' or `org-overriding-tag-alist' instead" "9.8")

(defvar-local org-tag-groups-alist nil)
(make-obsolete 'org-tag-groups-alist "Use (org-tag-alist-to-groups (org-local-tags-alist)) instead" "9.8")

(defvar-local org-link-abbrev-alist-local nil
  "Buffer-local version of `org-link-abbrev-alist', which see.
The value of this is taken from the LINK keywords.")
(make-obsolete 'org-link-abbrev-alist-local "Use (org-element-property :link-abbrevs (org-element-org-data)) instead" "9.8")

(defvar org-file-tags nil
  "List of tags that can be inherited by all entries in the file.
The tags will be inherited if the variable `org-use-tag-inheritance'
says they should be.
This variable is populated from #+FILETAGS lines.")
(make-obsolete 'org-file-tags "Use (org-element-property :tags (org-element-org-data)) instead" "9.8")

(defvar-local org-todo-regexp nil
  "Matches any of the TODO state keywords.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")
(make-obsolete 'org-todo-regexp "Use (org-todo-regexp) instead" "9.8")

(defvar-local org-not-done-regexp nil
  "Matches any of the TODO state keywords except the last one.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")
(make-obsolete 'org-not-done-regexp "Use (org-not-done-regexp) instead" "9.8")

(defvar-local org-not-done-heading-regexp nil
  "Matches a TODO headline that is not done.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")
(make-obsolete 'org-not-done-heading-regexp "Use (format org-heading-keyword-regexp-format (org-not-done-regexp)) instead" "9.8")

(defvar-local org-todo-line-regexp nil
  "Matches a headline and puts TODO state into group 2 if present.
Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")
(make-obsolete 'org-todo-line-regexp "Use (format org-heading-keyword-maybe-regexp-format (org-todo-regexp)) instead" "9.8")

(defvar-local org-todo-line-tags-regexp nil
  "Matches a headline and puts TODO state into group 2 if present.
Also put tags into group 4 if tags are present.")
(make-obsolete 'org-todo-line-tags-regexp "No longer used" "9.8")

(defvar-local org-complex-heading-regexp nil
  "Matches a headline and puts everything into groups:

group 1: Stars
group 2: The TODO keyword, maybe
group 3: Priority cookie
group 4: True headline
group 5: Tags

Since TODO keywords are case-sensitive, `case-fold-search' is
expected to be bound to nil when matching against this regexp.")
(make-obsolete 'org-complex-heading-regexp "Use (org-complex-heading-regexp) instead" "9.8")

(defvar-local org-complex-heading-regexp-format nil
  "Printf format to make regexp to match an exact headline.
This regexp will match the headline of any node which has the
exact headline text that is put into the format, but may have any
TODO state, priority, tags, statistics cookies (at the beginning
or end of the headline title), or COMMENT keyword.")
(make-obsolete 'org-complex-heading-regexp-format
               "Use (org-complex-heading-regexp-format) instead"
               "9.8")

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
