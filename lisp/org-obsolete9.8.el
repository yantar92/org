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

(provide 'org-obsolete9.8)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-obsolete9.8.el ends here
