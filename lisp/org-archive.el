;;; org-archive.el --- Archiving for Org             -*- lexical-binding: t; -*-

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

;; This file contains the archive functionality for Org.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'cl-lib)
(require 'org-property)
(require 'org-mode-common)
(require 'org-agenda-search)
(require 'org-edit-structure)
(require 'org-narrow)
(require 'org-todo)
(require 'org-element)
(require 'org-time)
(require 'org-archive-core)
(require 'org-tags)

(defcustom org-archive-default-command 'org-archive-subtree
  "The default archiving command."
  :group 'org-archive
  :type '(choice
	  (const org-archive-subtree)
	  (const org-archive-to-archive-sibling)
	  (const org-archive-set-tag)))

(defcustom org-archive-reversed-order nil
  "Non-nil means make the tree first child under the archive heading, not last."
  :group 'org-archive
  :version "24.1"
  :type 'boolean)

(defcustom org-archive-sibling-heading "Archive"
  "Name of the local archive sibling that is used to archive entries locally.
Locally means: in the tree, under a sibling.
See `org-archive-to-archive-sibling' for more information."
  :group 'org-archive
  :type 'string)

(defcustom org-archive-mark-done nil
  "Non-nil means mark entries as DONE when they are moved to the archive file.
This can be a string to set the keyword to use.  When non-nil, Org will
use the first keyword in its list that means done."
  :group 'org-archive
  :type '(choice
	  (const :tag "No" nil)
	  (const :tag "Yes" t)
	  (string :tag "Use this keyword")))

(defcustom org-archive-stamp-time t
  "Non-nil means add a time stamp to entries moved to an archive file.
This variable is obsolete and has no effect anymore, instead add or remove
`time' from the variable `org-archive-save-context-info'."
  :group 'org-archive
  :type 'boolean)

(defcustom org-archive-file-header-format "\nArchived entries from file %s\n\n"
  "The header format string for newly created archive files.
When nil, no header will be inserted.
When a string, a %s formatter will be replaced by the file name."
  :group 'org-archive
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-archive-subtree-add-inherited-tags 'infile
  "Non-nil means append inherited tags when archiving a subtree."
  :group 'org-archive
  :version "24.1"
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "When archiving a subtree to the same file" infile)
	  (const :tag "Always" t)))

(defcustom org-archive-subtree-save-file-p 'from-org
  "Conditionally save the archive file after archiving a subtree.
This variable can be any of the following symbols:

t              saves in all cases.
`from-org'     prevents saving from an agenda-view.
`from-agenda'  saves only when the archive is initiated from an agenda-view.
nil            prevents saving in all cases.

Note that, regardless of this value, the archive buffer is never
saved when archiving into a location in the current buffer."
  :group 'org-archive
  :package-version '(Org . "9.4")
  :type '(choice
	  (const :tag "Save archive buffer" t)
	  (const :tag "Save when archiving from agenda" from-agenda)
	  (const :tag "Save when archiving from an Org buffer" from-org)
	  (const :tag "Do not save")))

(defcustom org-archive-save-context-info '(time file olpath category todo itags)
  "Parts of context info that should be stored as properties when archiving.
When a subtree is moved to an archive file, it loses information given by
context, like inherited tags, the category, and possibly also the TODO
state (depending on the variable `org-archive-mark-done').
This variable can be a list of any of the following symbols:

time       The time of archiving.
file       The file where the entry originates.
ltags      The local tags, in the headline of the subtree.
itags      The tags the subtree inherits from further up the hierarchy.
todo       The pre-archive TODO state.
category   The category, taken from file name or #+CATEGORY lines.
olpath     The outline path to the item.  These are all headlines above
           the current item, separated by /, like a file path.

For each symbol present in the list, a property will be created in
the archived entry, with a prefix \"ARCHIVE_\", to remember this
information."
  :group 'org-archive
  :type '(set :greedy t
	      (const :tag "Time" time)
	      (const :tag "File" file)
	      (const :tag "Category" category)
	      (const :tag "TODO state" todo)
	      (const :tag "Priority" priority)
	      (const :tag "Inherited tags" itags)
	      (const :tag "Outline path" olpath)
	      (const :tag "Local tags" ltags)))

(defvar org-archive-hook nil
  "Hook run after successfully archiving a subtree.
Hook functions are called with point on the subtree in the
original file.  At this stage, the subtree has been added to the
archive location, but not yet deleted from the original file.")

(defun org-archive--mark-done-maybe ()
  "Mark heading at point done if necessary."
  (when (and org-archive-mark-done (org-entry-is-todo-p))
    (let (org-log-done org-todo-log-states)
      (org-todo
       (if (org-element-keyword-done-p org-archive-mark-done)
           org-archive-mark-done
	 (car (org-element-done-keywords)))))))

(defun org-archive--ensure-org-mode ()
  "Activate `org-mode' in current buffer now and for future sesstions."
  ;; Enforce Org mode for the archive buffer.
  (require 'org-mode)
  (defvar org-insert-mode-line-in-empty-file)
  (if (not (derived-mode-p 'org-mode))
      ;; Force the mode for future visits.
      (let ((org-insert-mode-line-in-empty-file t)
	    (org-inhibit-startup t))
	(call-interactively #'org-mode))))

(declare-function org-datetree-find-date-create "org-datetree" (date &optional keep-restriction))
(declare-function org-inlinetask-remove-END-maybe "org-inlinetask" ())
(declare-function org-entry-put "org-property-set" (epom property value))
;;;###autoload
(defun org-archive-subtree (&optional find-done)
  "Move the current subtree to the archive.
The archive can be a certain top-level heading in the current
file, or in a different file.  The tree will be moved to that
location, the subtree heading be marked DONE, and the current
time will be added.

When called with a single prefix argument FIND-DONE, find whole
trees without any open TODO items and archive them (after getting
confirmation from the user).  When called with a double prefix
argument, find whole trees with timestamps before today and
archive them (after getting confirmation from the user).  If the
cursor is not at a headline when these commands are called, try
all level 1 trees.  If the cursor is on a headline, only try the
direct children of this heading."
  (interactive "P")
  (if (and (use-region-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
		    'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 `(progn (setq org-map-continue-from (progn (org-back-to-heading) (point)))
		 (org-archive-subtree ,find-done))
	 org-loop-over-headlines-in-active-region
	 cl (if (org-invisible-p) (org-end-of-subtree nil t))))
    (cond
     ((equal find-done '(4))  (org-archive-all-done))
     ((equal find-done '(16)) (org-archive-all-old))
     (t
      ;; If any kind error occurs in the process, undo all the changes.
      (atomic-change-group
        ;; Suppress on-the-fly headline updates.
        (let ((org-element--cache-avoid-synchronous-headline-re-parsing t))
          (org-back-to-heading t)
          ;; Compute archive location.
	  ;; Get context information that will be lost by moving the
	  ;; tree.  See `org-archive-save-context-info'.
          (let* ((source-buffer (current-buffer))
	         (time (format-time-string
                        (org-time-stamp-format 'with-time 'no-brackets)))
	         (source-file (abbreviate-file-name
		               (or (buffer-file-name (buffer-base-buffer))
			           (error "No file associated to buffer"))))
	         (archive-location
                  (org-archive--compute-location
                   (org-get-archive-location)))
	         (archive-file (car archive-location))
	         (archive-heading (cdr archive-location))
	         (infile-p (equal source-file (abbreviate-file-name (or archive-file ""))))
	         (newfile-p (and (org-string-nw-p archive-file)
			         (not (file-exists-p archive-file))))
	         (archive-buffer (cond ((not (org-string-nw-p archive-file)) source-buffer)
			               ((find-file-noselect archive-file 'nowarn))
			               (t (error "Cannot access file \"%s\"" archive-file))))
	         level datetree-date datetree-subheading-p
                 (all-tags (org-get-tags))
		 (local-tags (org-get-tags nil 'local))
		 (inherited-tags (seq-difference all-tags local-tags))
		 (context
		  `((category . ,(org-get-category))
		    (file . ,source-file)
		    (itags . ,(mapconcat #'identity inherited-tags " "))
		    (ltags . ,(mapconcat #'identity local-tags " "))
		    (olpath . ,(mapconcat #'identity
					  (org-get-outline-path)
					  "/"))
		    (time . ,time)
		    (todo . ,(org-entry-get (point) "TODO")))))
            ;; Find archive-heading.  The archived subtree will go
            ;; under that heading (or to top level, if it is nil).
	    (when (string-match "\\`datetree/\\(\\**\\)" archive-heading)
	      ;; "datetree/" corresponds to 3 levels of headings.
	      (let ((nsub (length (match-string 1 archive-heading))))
	        (setq archive-heading
                      (concat
                       (make-string
			(+ (if (buffer-local-value 'org-odd-levels-only archive-buffer)
                               5 3)
			   (* (org-level-increment) nsub))
			?*)
		       (substring archive-heading (match-end 0))))
	        (setq datetree-subheading-p (> nsub 0)))
	      (setq datetree-date (org-date-to-gregorian
			           (or (org-entry-get nil "CLOSED" t) time))))
	    (if (and (> (length archive-heading) 0)
		     (string-match "^\\*+" archive-heading))
	        (setq level (match-end 0))
	      (setq archive-heading nil level 0))
            ;; Mark heading done using the source buffer settings
            ;; (todo keywords, note settings, etc).
            (org-archive--mark-done-maybe)
            ;; We first only copy, in case something goes wrong;
	    ;; we need to protect `this-command', to avoid
	    ;; `kill-region' setting it, which would lead to
	    ;; duplication of subtrees.
	    (let (this-command) (org-copy-subtree 1 nil t))
            ;; Paste the subtree into archive buffer, adding context
            ;; info as needed.
            (save-excursion ;; If archive-buffer is the same with current, retain point
              (with-current-buffer archive-buffer
                ;; If anything goes wrong in the process, undo all the
                ;; edits.
                (atomic-change-group
                  (org-archive--ensure-org-mode)
	          (when (and newfile-p org-archive-file-header-format)
	            (goto-char (point-max))
	            (insert (format org-archive-file-header-format
			            (buffer-file-name source-buffer))))
                  ;; Move point to the position where we are to insert the
                  ;; archived heading.
	          (when datetree-date
	            (require 'org-datetree)
	            (org-datetree-find-date-create datetree-date)
	            (org-narrow-to-subtree))
	          (goto-char (point-min))
	          (if (and archive-heading (not (and datetree-date (not datetree-subheading-p))))
		      (progn
		        (if (re-search-forward
		             (concat "^" (regexp-quote archive-heading)
			             "\\([ \t]+:\\(" org-tag-re ":\\)+\\)?[ \t]*$")
		             nil t)
		            (goto-char (match-end 0))
		          ;; Heading not found, just insert it at the end
		          (goto-char (point-max))
		          (or (bolp) (insert "\n"))
		          ;; datetrees don't need too much spacing
		          (insert (if datetree-date "" "\n") archive-heading "\n")
		          (end-of-line 0))
		        (if org-archive-reversed-order
		            (progn
			      (org-back-to-heading t)
			      (outline-next-heading))
		          (org-end-of-subtree t))
                        (org-skip-whitespace 'back)
		        (and (looking-at "[ \t\r\n]*")
		             ;; datetree archives don't need so much spacing.
		             (replace-match (if datetree-date "\n" "\n\n"))))
	            ;; No specific heading, just go to end of file, or to the
	            ;; beginning, depending on `org-archive-reversed-order'.
	            (if org-archive-reversed-order
		        (progn
		          (goto-char (point-min))
		          (unless (org-at-heading-p) (outline-next-heading)))
		      (goto-char (point-max))
		      ;; Subtree narrowing can let the buffer end on
		      ;; a headline.  `org-paste-subtree' then deletes it.
		      ;; To prevent this, make sure visible part of buffer
		      ;; always terminates on a new line, while limiting
		      ;; number of blank lines in a date tree.
		      (unless (and datetree-date (bolp)) (insert "\n"))))
	          ;; Paste
	          (org-paste-subtree (org-get-valid-level level (and archive-heading 1)))
	          ;; Shall we append inherited tags?
	          (and inherited-tags
		       (or (and (eq org-archive-subtree-add-inherited-tags 'infile)
			        infile-p)
		           (eq org-archive-subtree-add-inherited-tags t))
		       (org-set-tags all-tags))
	          ;; Add the context info.
	          (dolist (item org-archive-save-context-info)
	            (let ((value (cdr (assq item context))))
		      (when (org-string-nw-p value)
                        (require 'org-property-set)
		        (org-entry-put
		         (point)
		         (concat "ARCHIVE_" (upcase (symbol-name item)))
		         value))))
	          ;; Save the buffer, if it is not the same buffer and
	          ;; depending on `org-archive-subtree-save-file-p'.
	          (unless (eq source-buffer archive-buffer)
	            (when (or (eq org-archive-subtree-save-file-p t)
			      (eq org-archive-subtree-save-file-p
			          (if (boundp 'org-archive-from-agenda)
				      'from-agenda
			            'from-org)))
		      (save-buffer)))
	          (widen))))
	    ;; Here we are back in the original buffer.  Everything seems
	    ;; to have worked.  So now run hooks, cut the tree and finish
	    ;; up.
	    (run-hooks 'org-archive-hook)
	    (let (this-command) (org-cut-subtree))
	    (when (featurep 'org-inlinetask)
	      (org-inlinetask-remove-END-maybe))
	    (setq org-markers-to-move nil)
	    (when org-provide-todo-statistics
	      (save-excursion
	        ;; Go to parent, even if no children exist.
	        (org-up-heading-safe)
	        ;; Update cookie of parent.
	        (org-update-statistics-cookies nil)))
	    (message "Subtree archived %s"
		     (if (eq source-buffer archive-buffer)
		         (concat "under heading: " archive-heading)
		       (concat "in file: " (abbreviate-file-name archive-file)))))))))
    (org-fold-reveal)
    (if (looking-at "^[ \t]*$")
	(org-next-visible-heading 1))))

(declare-function org-cycle-show-empty-lines "org-cycle" (state))
(declare-function org-set-property "org-property-set" (property value))
;;;###autoload
(defun org-archive-to-archive-sibling ()
  "Archive the current heading by moving it under the archive sibling.

The archive sibling is a sibling of the heading with the heading name
`org-archive-sibling-heading' and an `org-archive-tag' tag.  If this
sibling does not exist, it will be created at the end of the subtree.

Archiving time is retained in the ARCHIVE_TIME node property."
  (interactive)
  (if (and (use-region-p) org-loop-over-headlines-in-active-region)
      (let ((cl (when (eq org-loop-over-headlines-in-active-region 'start-level)
		  'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 '(progn (setq org-map-continue-from
		       (progn (org-back-to-heading)
			      (if (looking-at (concat "^.*:" org-archive-tag ":.*$"))
				  (org-end-of-subtree t)
				(point))))
		 (when (org-at-heading-p)
		   (org-archive-to-archive-sibling)))
	 org-loop-over-headlines-in-active-region
	 cl (if (org-invisible-p) (org-end-of-subtree nil t))))
    (save-restriction
      (widen)
      (let (b e pos leader level)
	(org-back-to-heading t)
	(looking-at org-outline-regexp)
	(setq leader (match-string 0)
	      level (funcall outline-level))
	(setq pos (point-marker))
        ;; Advance POS upon insertion in front of it.
        (set-marker-insertion-type pos t)
	(condition-case nil
	    (outline-up-heading 1 t)
	  (error (setq e (point-max)) (goto-char (point-min))))
	(setq b (point))
	(unless e
	  (condition-case nil
	      (org-end-of-subtree t t)
	    (error (goto-char (point-max))))
	  (setq e (point)))
	(goto-char b)
	(unless (re-search-forward
		 (concat "^" (regexp-quote leader)
			 "[ \t]*"
			 org-archive-sibling-heading
			 "[ \t]*:"
			 org-archive-tag ":") e t)
	  (goto-char e)
	  (or (bolp) (newline))
	  (insert leader org-archive-sibling-heading "\n")
	  (forward-line -1)
	  (org-toggle-tag org-archive-tag 'on))
	(forward-line 0)
	(if org-archive-reversed-order
	    (outline-next-heading)
	  (org-end-of-subtree t t))
	(save-excursion
	  (goto-char pos)
	  (let ((this-command this-command)) (org-cut-subtree)))
	(org-paste-subtree (org-get-valid-level level 1))
        (require 'org-property-set)
	(org-set-property
	 "ARCHIVE_TIME"
	 (format-time-string
          (org-time-stamp-format 'with-time 'no-brackets)))
	(outline-up-heading 1 t)
	(org-fold-subtree t)
        (require 'org-cycle)
	(org-cycle-show-empty-lines 'folded)
	(when org-provide-todo-statistics
	  ;; Update TODO statistics of parent.
	  (org-update-parent-todo-statistics))
	(goto-char pos)))
    (org-fold-reveal)
    (if (looking-at "^[ \t]*$")
	(org-next-visible-heading 1))))

(defun org-archive-all-done (&optional tag)
  "Archive sublevels of the current tree without open TODO items.
If the cursor is not on a headline, try all level 1 trees.  If
it is on a headline, try all direct children.
When TAG is non-nil, don't move trees, but mark them with the ARCHIVE tag."
  (org-archive-all-matches
   (lambda (_beg end)
     (let ((case-fold-search nil))
       (unless (re-search-forward org-not-done-heading-regexp end t)
	 "no open TODO items")))
   tag))

(defun org-archive-all-old (&optional tag)
  "Archive sublevels of the current tree with timestamps prior to today.
If the cursor is not on a headline, try all level 1 trees.  If
it is on a headline, try all direct children.
When TAG is non-nil, don't move trees, but mark them with the ARCHIVE tag."
  (org-archive-all-matches
   (lambda (_beg end)
     (let (ts)
       (and (re-search-forward org-ts-regexp end t)
	    (setq ts (match-string 0))
	    (< (org-timestamp-to-now ts) 0)
	    (if (not (looking-at
		    (concat "--\\(" org-ts-regexp "\\)")))
		(concat "old timestamp " ts)
	      (setq ts (concat "old timestamp " ts (match-string 0)))
	      (and (< (org-timestamp-to-now (match-string 1)) 0)
		   ts)))))
   tag))

(defun org-archive-all-matches (predicate &optional tag)
  "Archive sublevels of the current tree that match PREDICATE.

PREDICATE is a function of two arguments, BEG and END, which
specify the beginning and end of the headline being considered.
It is called with point positioned at BEG.  The headline will be
archived if PREDICATE returns non-nil.  If the return value of
PREDICATE is a string, it should describe the reason for
archiving the heading.

If the cursor is not on a headline, try all level 1 trees.  If it
is on a headline, try all direct children.  When TAG is non-nil,
don't move trees, but mark them with the ARCHIVE tag."
  (let ((rea (concat ".*:" org-archive-tag ":")) re1
	(begm (make-marker))
	(endm (make-marker))
	(question (if tag "Set ARCHIVE tag? "
		    "Move subtree to archive? "))
	reason beg end (cntarch 0))
    (if (org-at-heading-p)
	(progn
	  (setq re1 (concat "^" (regexp-quote
				 (make-string
				  (+ (- (match-end 0) (match-beginning 0) 1)
				     (if org-odd-levels-only 2 1))
				  ?*))
			    " "))
	  (move-marker begm (point))
	  (move-marker endm (org-end-of-subtree t)))
      (setq re1 "^* ")
      (move-marker begm (point-min))
      (move-marker endm (point-max)))
    (save-excursion
      (goto-char begm)
      (while (re-search-forward re1 endm t)
	(setq beg (match-beginning 0)
	      end (save-excursion (org-end-of-subtree t) (point)))
	(goto-char beg)
	(if (not (setq reason (funcall predicate beg end)))
	    (goto-char end)
	  (goto-char beg)
	  (if (and (or (not tag) (not (looking-at rea)))
		   (y-or-n-p
		    (if (stringp reason)
			(concat question "(" reason ")")
		      question)))
	      (progn
		(if tag
		    (org-toggle-tag org-archive-tag 'on)
		  (org-archive-subtree))
		(setq cntarch (1+ cntarch)))
	    (goto-char end)))))
    (message "%d trees archived" cntarch)))

;;;###autoload
(defun org-toggle-archive-tag (&optional find-done)
  "Toggle the archive tag for the current headline.
With prefix argument FIND-DONE, check all children of current headline
and offer tagging the children that do not contain any open TODO
items."
  (interactive "P")
  (if (and (use-region-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
		    'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 `(org-toggle-archive-tag ,find-done)
	 org-loop-over-headlines-in-active-region
	 cl (if (org-invisible-p) (org-end-of-subtree nil t))))
    (if find-done
	(org-archive-all-done 'tag)
      (let (set)
	(save-excursion
	  (org-back-to-heading t)
	  (setq set (org-toggle-tag org-archive-tag))
	  (when set (org-fold-subtree t)))
	(and set (forward-line 0))
	(message "Subtree %s" (if set "archived" "unarchived"))))))

(defun org-archive-set-tag ()
  "Set the ARCHIVE tag."
  (interactive)
  (if (and (use-region-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
		    'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 'org-archive-set-tag
	 org-loop-over-headlines-in-active-region
	 cl (if (org-invisible-p) (org-end-of-subtree nil t))))
    (org-toggle-tag org-archive-tag 'on)))

;;;###autoload
(defun org-archive-subtree-default ()
  "Archive the current subtree with the default command.
This command is set with the variable `org-archive-default-command'."
  (interactive)
  (call-interactively org-archive-default-command))

;;;###autoload
(defun org-archive-subtree-default-with-confirmation ()
  "Archive the current subtree with the default command.
This command is set with the variable `org-archive-default-command'."
  (interactive)
  (if (y-or-n-p "Archive this subtree or entry? ")
      (call-interactively org-archive-default-command)
    (error "Abort")))

(provide 'org-archive)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-archive.el ends here
