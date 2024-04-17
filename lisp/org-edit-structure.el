;;; org-edit-structure.el --- Org structure editing commands                      -*- lexical-binding: t; -*-

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

;; This library implements commands used to edit hierarchical
;; structure in Org buffers.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-edit-structure-common)

(require 'org-fold)
(require 'org-element)
(require 'outline)
(require 'org-move)
(defvar org-complex-heading-regexp)
(require 'org-tags)
(require 'org-map)
(defvar org-todo-line-regexp)
(defvar org-done-keywords)
(defvar org-todo-keywords-1)
(declare-function org-todo "org")
(defvar org-provide-todo-statistics)
(declare-function org-update-parent-todo-statistics "org")
(declare-function org-inlinetask-in-task-p "org-inlinetask")
(declare-function org-inlinetask-goto-end "org-inlinetask")
(declare-function org-clock-save-markers-for-cut-and-paste "org-clock")
(declare-function org-agenda-save-markers-for-cut-and-paste "org-agenda")
(defvar org-id-overriding-file-name)
(declare-function org-id-get-create "org-id")
(declare-function org-remove-empty-drawer-at "org")
(declare-function org-clock-sum "org-clock")
(declare-function org-clocking-buffer "org")
(defvar org-clock-marker)
(declare-function org-get-heading "org")

(defcustom org-insert-heading-hook nil
  "Hook being run after inserting a new heading."
  :group 'org-edit-structure
  :type 'hook)

(defcustom org-treat-insert-todo-heading-as-state-change nil
  "Non-nil means inserting a TODO heading is treated as state change.
So when the command `\\[org-insert-todo-heading]' is used, state change
logging will apply if appropriate.  When nil, the new TODO item will
be inserted directly, and no logging will take place."
  :group 'org-todo
  :type 'boolean)

(defcustom org-clone-delete-id nil
  "Remove ID property of clones of a subtree.
When non-nil, clones of a subtree don't inherit the ID property.
Otherwise they inherit the ID property with a new unique
identifier."
  :type 'boolean
  :version "24.1"
  :group 'org-id)


;;; Inserting headlines

(defun org--blank-before-heading-p (&optional parent)
  "Non-nil when an empty line should precede a new heading here.
When optional argument PARENT is non-nil, consider parent
headline instead of current one."
  (pcase (assq 'heading org-blank-before-new-entry)
    (`(heading . auto)
     (save-excursion
       (org-with-limited-levels
        (unless (and (org-before-first-heading-p)
                     (not (outline-next-heading)))
          (org-back-to-heading t)
          (when parent (org-up-heading-safe))
          (cond ((not (bobp))
                 (org-previous-line-empty-p))
		((outline-next-heading)
		 (org-previous-line-empty-p))
		;; Ignore trailing spaces on last buffer line.
		((progn (skip-chars-backward " \t") (bolp))
		 (org-previous-line-empty-p))
		(t nil))))))
    (`(heading . ,value) value)
    (_ nil)))

(defun org-insert-heading (&optional arg invisible-ok level)
  "Insert a new heading or an item with the same depth at point.

If point is at the beginning of a heading, insert a new heading
or a new headline above the current one.  When at the beginning
of a regular line of text, turn it into a heading.

If point is in the middle of a line, split it and create a new
headline with the text in the current line after point (see
`org-M-RET-may-split-line' on how to modify this behavior).  As
a special case, on a headline, splitting can only happen on the
title itself.  E.g., this excludes breaking stars or tags.

With a `\\[universal-argument]' prefix, set \
`org-insert-heading-respect-content' to
a non-nil value for the duration of the command.  This forces the
insertion of a heading after the current subtree, independently
on the location of point.

With a `\\[universal-argument] \\[universal-argument]' prefix, \
insert the heading at the end of the tree
above the current heading.  For example, if point is within a
2nd-level heading, then it will insert a 2nd-level heading at
the end of the 1st-level parent subtree.

When INVISIBLE-OK is set, stop at invisible headlines when going
back.  This is important for non-interactive uses of the
command.

When optional argument LEVEL is a number, insert a heading at
that level.  For backwards compatibility, when LEVEL is non-nil
but not a number, insert a level-1 heading."
  (interactive "P")
  (let* ((blank? (org--blank-before-heading-p (equal arg '(16))))
         (current-level (org-current-level))
         (num-stars (or
                     ;; Backwards compat: if LEVEL non-nil, level is 1
                     (and level (if (wholenump level) level 1))
                     current-level
                     ;; This `1' is for when before first headline
                     1))
         (stars (make-string num-stars ?*))
         (maybe-add-blank-after
          (lambda (blank?)
            "Add a blank line before next heading when BLANK? is non-nil.
Assume that point is on the inserted heading."
            (save-excursion
              (end-of-line)
              (unless (eobp)
                (forward-char)
                (when (and blank? (org-at-heading-p))
                  (insert "\n")))))))
    (cond
     ((or org-insert-heading-respect-content
	  (member arg '((4) (16)))
	  (and (not invisible-ok)
	       (invisible-p (max (1- (point)) (point-min)))))
      ;; Position point at the location of insertion.  Make sure we
      ;; end up on a visible headline if INVISIBLE-OK is nil.
      (org-with-limited-levels
       (if (not current-level) (outline-next-heading) ;before first headline
	 (org-back-to-heading invisible-ok)
	 (when (equal arg '(16)) (org-up-heading-safe))
	 (org-end-of-subtree invisible-ok 'to-heading)))
      ;; At `point-max', if the file does not have ending newline,
      ;; create one, so that we are not appending stars at non-empty
      ;; line.
      (unless (bolp) (insert "\n"))
      (when (and blank? (save-excursion
                          (backward-char)
                          (org-before-first-heading-p)))
        (insert "\n")
        (backward-char))
      (when (and (not current-level) (not (eobp)) (not (bobp)))
        (when (org-at-heading-p) (insert "\n"))
        (backward-char))
      (unless (and blank? (org-previous-line-empty-p))
	(org-N-empty-lines-before-current (if blank? 1 0)))
      (insert stars " " "\n")
      ;; Move point after stars.
      (backward-char)
      ;; Retain blank lines before next heading.
      (funcall maybe-add-blank-after blank?)
      ;; When INVISIBLE-OK is non-nil, ensure newly created headline
      ;; is visible.
      (unless invisible-ok
        (if (eq org-fold-core-style 'text-properties)
	    (cond
	     ((org-fold-folded-p
               (max (point-min)
                    (1- (line-beginning-position)))
               'headline)
	      (org-fold-region (line-end-position 0) (line-end-position) nil 'headline))
	     (t nil))
          (pcase (get-char-property-and-overlay (point) 'invisible)
	    (`(outline . ,o)
	     (move-overlay o (overlay-start o) (line-end-position 0)))
	    (_ nil)))))
     ;; At a headline...
     ((org-at-heading-p)
      (cond ((bolp)
	     (when blank? (save-excursion (insert "\n")))
	     (save-excursion (insert stars " \n"))
	     (unless (and blank? (org-previous-line-empty-p))
	       (org-N-empty-lines-before-current (if blank? 1 0)))
	     (end-of-line))
	    ((and (org-get-alist-option org-M-RET-may-split-line 'headline)
		  (org-match-line org-complex-heading-regexp)
		  (org-pos-in-match-range (point) 4))
	     ;; Grab the text that should moved to the new headline.
	     ;; Preserve tags.
	     (let ((split (delete-and-extract-region (point) (match-end 4))))
	       (if (looking-at "[ \t]*$") (replace-match "")
		 (when org-auto-align-tags (org-align-tags)))
	       (end-of-line)
	       (when blank? (insert "\n"))
	       (insert "\n" stars " ")
               ;; Retain blank lines before next heading.
               (funcall maybe-add-blank-after blank?)
	       (when (org-string-nw-p split) (insert split))))
	    (t
	     (end-of-line)
	     (when blank? (insert "\n"))
	     (insert "\n" stars " ")
             ;; Retain blank lines before next heading.
             (funcall maybe-add-blank-after blank?))))
     ;; On regular text, turn line into a headline or split, if
     ;; appropriate.
     ((bolp)
      (insert stars " ")
      (unless (and blank? (org-previous-line-empty-p))
        (org-N-empty-lines-before-current (if blank? 1 0)))
      ;; Retain blank lines before next heading.
      (funcall maybe-add-blank-after blank?))
     (t
      (unless (org-get-alist-option org-M-RET-may-split-line 'headline)
        (end-of-line))
      (insert "\n" stars " ")
      (unless (and blank? (org-previous-line-empty-p))
        (org-N-empty-lines-before-current (if blank? 1 0)))
      ;; Retain blank lines before next heading.
      (funcall maybe-add-blank-after blank?))))
  (run-hooks 'org-insert-heading-hook))

(defun org-N-empty-lines-before-current (n)
  "Make the number of empty lines before current exactly N.
So this will delete or add empty lines."
  (let ((column (current-column)))
    (forward-line 0)
    (unless (bobp)
      (let ((start (save-excursion
		     (skip-chars-backward " \r\t\n")
		     (line-end-position))))
	(delete-region start (line-end-position 0))))
    (insert (make-string n ?\n))
    (move-to-column column)))

(defun org-edit-headline (&optional heading)
  "Edit the current headline.
Set it to HEADING when provided."
  (interactive)
  (org-with-wide-buffer
   (org-back-to-heading t)
   (let ((case-fold-search nil))
     (when (looking-at org-complex-heading-regexp)
       (let* ((old (match-string-no-properties 4))
	      (new (save-match-data
		     (org-trim (or heading (read-string "Edit: " old))))))
	 (unless (equal old new)
	   (if old (replace-match new t t nil 4)
	     (goto-char (or (match-end 3) (match-end 2) (match-end 1)))
	     (insert " " new))
	   (when org-auto-align-tags (org-align-tags))
	   (when (looking-at "[ \t]*$") (replace-match ""))))))))

(defun org-insert-heading-after-current ()
  "Insert a new heading with same level as current, after current subtree."
  (interactive)
  (org-back-to-heading)
  (org-insert-heading)
  (org-move-subtree-down)
  (end-of-line 1))

(defun org-insert-heading-respect-content (&optional invisible-ok)
  "Insert heading with `org-insert-heading-respect-content' set to t."
  (interactive)
  (org-insert-heading '(4) invisible-ok))

(defun org-insert-todo-heading-respect-content (&optional arg)
  "Call `org-insert-todo-heading', inserting after current subtree.
ARG is passed to `org-insert-todo-heading'.
This command temporarily sets `org-insert-heading-respect-content' to t."
  (interactive "P")
  (let ((org-insert-heading-respect-content t))
    (org-insert-todo-heading arg t)))

(defun org-insert-todo-heading (arg &optional force-heading)
  "Insert a new heading with the same level and TODO state as current heading.

If the heading has no TODO state, or if the state is DONE, use
the first state (TODO by default).  Also with `\\[universal-argument]'
prefix, force first state.  With a `\\[universal-argument]
\\[universal-argument]' prefix, force inserting at the end of the
parent subtree.

When called at a plain list item, insert a new item with an
unchecked check box."
  (interactive "P")
  (when (or force-heading (not (org-insert-item 'checkbox)))
    (org-insert-heading (or (and (equal arg '(16)) '(16))
			    force-heading))
    (save-excursion
      (org-forward-heading-same-level -1)
      (let ((case-fold-search nil)) (looking-at org-todo-line-regexp)))
    (let* ((new-mark-x
	    (if (or (equal arg '(4))
		    (not (match-beginning 2))
		    (member (match-string 2) org-done-keywords))
		(car org-todo-keywords-1)
	      (match-string 2)))
	   (new-mark
	    (or
	     (run-hook-with-args-until-success
	      'org-todo-get-default-hook new-mark-x nil)
	     new-mark-x)))
      (forward-line 0)
      (and (looking-at org-outline-regexp) (goto-char (match-end 0))
	   (if org-treat-insert-todo-heading-as-state-change
	       (org-todo new-mark)
	     (insert new-mark " "))))
    (when org-provide-todo-statistics
      (org-update-parent-todo-statistics))))

(defun org-insert-subheading (arg)
  "Insert a new subheading and demote it.
Works for outline headings and for plain lists alike.
The prefix argument ARG is passed to `org-insert-heading'.
Unlike `org-insert-heading', when point is at the beginning of a
heading, still insert the new sub-heading below."
  (interactive "P")
  (when (and (bolp) (not (eobp)) (not (eolp))) (forward-char))
  (org-insert-heading arg)
  (cond
   ((org-at-heading-p) (org-do-demote))
   ((org-at-item-p) (org-indent-item))))

(defun org-insert-todo-subheading (arg)
  "Insert a new subheading with TODO keyword or checkbox and demote it.
Works for outline headings and for plain lists alike.
The prefix argument ARG is passed to `org-insert-todo-heading'."
  (interactive "P")
  (org-insert-todo-heading arg)
  (cond
   ((org-at-heading-p) (org-do-demote))
   ((org-at-item-p) (org-indent-item))))

;;; Promotion and Demotion

(defvar org-after-demote-entry-hook nil
  "Hook run after an entry has been demoted.
The cursor will be at the beginning of the entry.
When a subtree is being demoted, the hook will be called for each node.")

(defvar org-after-promote-entry-hook nil
  "Hook run after an entry has been promoted.
The cursor will be at the beginning of the entry.
When a subtree is being promoted, the hook will be called for each node.")

(defun org-promote-subtree ()
  "Promote the entire subtree.
See also `org-promote'."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (org-combine-change-calls (point) (save-excursion (org-end-of-subtree t))
      (org-with-limited-levels (org-map-tree 'org-promote))))
  (org-fix-position-after-promote))

(defun org-demote-subtree ()
  "Demote the entire subtree.
See `org-demote' and `org-promote'."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (org-combine-change-calls (point) (save-excursion (org-end-of-subtree t))
      (org-with-limited-levels (org-map-tree 'org-demote))))
  (org-fix-position-after-promote))

(defun org-do-promote ()
  "Promote the current heading higher up the tree.
If the region is active in `transient-mark-mode', promote all
headings in the region."
  (interactive)
  (save-excursion
    (if (use-region-p)
        (let ((deactivate-mark nil))
          (org-map-region 'org-promote (region-beginning) (region-end)))
      (org-promote)))
  (org-fix-position-after-promote))

(defun org-do-demote ()
  "Demote the current heading lower down the tree.
If the region is active in `transient-mark-mode', demote all
headings in the region."
  (interactive)
  (save-excursion
    (if (use-region-p)
        (let ((deactivate-mark nil))
          (org-map-region 'org-demote (region-beginning) (region-end)))
      (org-demote)))
  (org-fix-position-after-promote))

(defun org-fix-position-after-promote ()
  "Fix cursor position and indentation after demoting/promoting."
  (let ((pos (point)))
    (when (save-excursion
	    (forward-line 0)
	    (let ((case-fold-search nil)) (looking-at org-todo-line-regexp))
	    (or (eq pos (match-end 1)) (eq pos (match-end 2))))
      (cond ((eobp) (insert " "))
	    ((eolp) (insert " "))
	    ((equal (char-after) ?\s) (forward-char 1))))))

(defun org-promote ()
  "Promote the current heading higher up the tree."
  (org-with-wide-buffer
   (org-back-to-heading t)
   (let* ((after-change-functions (remq 'flyspell-after-change-function
					after-change-functions))
	  (level (save-match-data (funcall outline-level)))
	  (up-head (concat (make-string (org-get-valid-level level -1) ?*) " "))
	  (diff (abs (- level (length up-head) -1))))
     (cond
      ((and (= level 1) org-allow-promoting-top-level-subtree)
       (replace-match "# " nil t))
      ((= level 1)
       (user-error "Cannot promote to level 0.  UNDO to recover if necessary"))
      (t (replace-match (apply #'propertize up-head (text-properties-at (match-beginning 0))) t)))
     (unless (= level 1)
       (when org-auto-align-tags (org-align-tags))
       (when org-adapt-indentation (org-fixup-indentation (- diff))))
     (run-hooks 'org-after-promote-entry-hook))))

(defun org-demote ()
  "Demote the current heading lower down the tree."
  (org-with-wide-buffer
   (org-back-to-heading t)
   (let* ((after-change-functions (remq 'flyspell-after-change-function
					after-change-functions))
	  (level (save-match-data (funcall outline-level)))
	  (down-head (concat (make-string (org-get-valid-level level 1) ?*) " "))
	  (diff (abs (- level (length down-head) -1))))
     (org-fold-core-ignore-fragility-checks
       (replace-match (apply #'propertize down-head (text-properties-at (match-beginning 0))) t)
       (when org-auto-align-tags (org-align-tags))
       (when org-adapt-indentation (org-fixup-indentation diff)))
     (run-hooks 'org-after-demote-entry-hook))))

(defun org-get-previous-line-level ()
  "Return the outline depth of the last headline before the current line.
Returns 0 for the first headline in the buffer, and nil if before the
first headline."
  (and (org-current-level)
       (or (and (/= (line-beginning-position) (point-min))
		(save-excursion (forward-line -1) (org-current-level)))
	   0)))

(defun org-cycle-level ()
  "Cycle the level of an empty headline through possible states.
This goes first to child, then to parent, level, then up the hierarchy.
After top level, it switches back to sibling level."
  (interactive)
  (let ((org-adapt-indentation nil))
    (when (and (org-point-at-end-of-empty-headline)
               (not (and (featurep 'org-inlinetask)
                         (org-inlinetask-in-task-p))))
      (setq this-command 'org-cycle-level) ; Only needed for caching
      (let ((cur-level (org-current-level))
            (prev-level (org-get-previous-line-level)))
        (cond
         ;; If first headline in file, promote to top-level.
         ((= prev-level 0)
          (cl-loop repeat (/ (- cur-level 1) (org-level-increment))
		   do (org-do-promote)))
         ;; If same level as prev, demote one.
         ((= prev-level cur-level)
          (org-do-demote))
         ;; If parent is top-level, promote to top level if not already.
         ((= prev-level 1)
          (cl-loop repeat (/ (- cur-level 1) (org-level-increment))
		   do (org-do-promote)))
         ;; If top-level, return to prev-level.
         ((= cur-level 1)
          (cl-loop repeat (/ (- prev-level 1) (org-level-increment))
		   do (org-do-demote)))
         ;; If less than prev-level, promote one.
         ((< cur-level prev-level)
          (org-do-promote))
         ;; If deeper than prev-level, promote until higher than
         ;; prev-level.
         ((> cur-level prev-level)
          (cl-loop repeat (+ 1 (/ (- cur-level prev-level) (org-level-increment)))
		   do (org-do-promote))))
        t))))

(defvar org-property-drawer-re)

(defun org-fixup-indentation (diff)
  "Change the indentation in the current entry by DIFF.

DIFF is an integer.  Indentation is done according to the
following rules:

  - Planning information and property drawers are always indented
    according to the new level of the headline;

  - Footnote definitions and their contents are ignored;

  - Inlinetasks' boundaries are not shifted;

  - Empty lines are ignored;

  - Other lines' indentation are shifted by DIFF columns, unless
    it would introduce a structural change in the document, in
    which case no shifting is done at all.

Assume point is at a heading or an inlinetask beginning."
  (org-with-wide-buffer
   (narrow-to-region (line-beginning-position)
		     (save-excursion
		       (if (org-with-limited-levels (org-at-heading-p))
			   (org-with-limited-levels (outline-next-heading))
			 (org-inlinetask-goto-end))
		       (point)))
   (forward-line)
   ;; Indent properly planning info and property drawer.
   (when (looking-at-p org-planning-line-re)
     (org-indent-line)
     (forward-line))
   (when (looking-at org-property-drawer-re)
     (goto-char (match-end 0))
     (forward-line)
     (org-indent-region (match-beginning 0) (match-end 0)))
   (when (looking-at org-logbook-drawer-re)
     (let ((end-marker  (move-marker (make-marker) (match-end 0)))
	   (col (+ (current-indentation) diff)))
       (when (wholenump col)
	 (while (< (point) end-marker)
           (if (natnump diff)
	       (insert (make-string diff 32))
             (delete-char (abs diff)))
	   (forward-line)))))
   (catch 'no-shift
     (when (or (zerop diff) (not (eq org-adapt-indentation t)))
       (throw 'no-shift nil))
     ;; If DIFF is negative, first check if a shift is possible at all
     ;; (e.g., it doesn't break structure).  This can only happen if
     ;; some contents are not properly indented.
     (let ((case-fold-search t))
       (when (< diff 0)
	 (let ((diff (- diff))
	       (forbidden-re (concat org-outline-regexp
				     "\\|"
				     (substring org-footnote-definition-re 1))))
	   (save-excursion
	     (while (not (eobp))
	       (cond
		((looking-at-p "[ \t]*$") (forward-line))
		((and (looking-at-p org-footnote-definition-re)
		      (let ((e (org-element-at-point)))
			(and (org-element-type-p e 'footnote-definition)
			     (goto-char (org-element-end e))))))
		((looking-at-p org-outline-regexp) (forward-line))
		;; Give up if shifting would move before column 0 or
		;; if it would introduce a headline or a footnote
		;; definition.
		(t
		 (skip-chars-forward " \t")
		 (let ((ind (current-column)))
		   (when (or (< ind diff)
			     (and (= ind diff) (looking-at-p forbidden-re)))
		     (throw 'no-shift nil)))
		 ;; Ignore contents of example blocks and source
		 ;; blocks if their indentation is meant to be
		 ;; preserved.  Jump to block's closing line.
		 (forward-line 0)
		 (or (and (looking-at-p "[ \t]*#\\+BEGIN_\\(EXAMPLE\\|SRC\\)")
			  (let ((e (org-element-at-point)))
			    (and (org-src-preserve-indentation-p e)
			         (goto-char (org-element-end e))
			         (progn (skip-chars-backward " \r\t\n")
				        (forward-line 0)
				        t))))
		     (forward-line))))))))
       ;; Shift lines but footnote definitions, inlinetasks boundaries
       ;; by DIFF.  Also skip contents of source or example blocks
       ;; when indentation is meant to be preserved.
       (while (not (eobp))
	 (cond
	  ((and (looking-at-p org-footnote-definition-re)
		(let ((e (org-element-at-point)))
		  (and (org-element-type-p e 'footnote-definition)
		       (goto-char (org-element-end e))))))
	  ((looking-at-p org-outline-regexp) (forward-line))
	  ((looking-at-p "[ \t]*$") (forward-line))
	  (t
	   (indent-line-to (+ (current-indentation) diff))
	   (forward-line 0)
	   (or (and (looking-at-p "[ \t]*#\\+BEGIN_\\(EXAMPLE\\|SRC\\)")
		    (let ((e (org-element-at-point)))
		      (and (org-src-preserve-indentation-p e)
			   (goto-char (org-element-end e))
			   (progn (skip-chars-backward " \r\t\n")
				  (forward-line 0)
				  t))))
	       (forward-line)))))))))

(defun org-convert-to-odd-levels ()
  "Convert an Org file with all levels allowed to one with odd levels.
This will leave level 1 alone, convert level 2 to level 3, level 3 to
level 5 etc."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to globally change levels to odd? ")
    (let ((outline-level 'org-outline-level)
	  (org-odd-levels-only nil) n)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^\\*\\*+ " nil t)
	  (setq n (- (length (match-string 0)) 2))
	  (while (>= (setq n (1- n)) 0)
	    (org-demote))
	  (end-of-line 1))))))

(defun org-convert-to-oddeven-levels ()
  "Convert an Org file with only odd levels to one with odd/even levels.
This promotes level 3 to level 2, level 5 to level 3 etc.  If the
file contains a section with an even level, conversion would
destroy the structure of the file.  An error is signaled in this
case."
  (interactive)
  (goto-char (point-min))
  ;; First check if there are no even levels
  (when (re-search-forward "^\\(\\*\\*\\)+ " nil t)
    (org-fold-show-set-visibility 'canonical)
    (error "Not all levels are odd in this file.  Conversion not possible"))
  (when (yes-or-no-p "Are you sure you want to globally change levels to odd-even? ")
    (let ((outline-regexp org-outline-regexp)
	  (outline-level 'org-outline-level)
	  (org-odd-levels-only nil) n)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^\\*\\*+ " nil t)
	  (setq n (/ (1- (length (match-string 0))) 2))
	  (while (>= (setq n (1- n)) 0)
	    (org-promote))
	  (end-of-line 1))))))


;;; Vertical tree motion, cutting and pasting of subtrees

(defun org-move-subtree-up (&optional arg)
  "Move the current subtree up past ARG headlines of the same level."
  (interactive "p")
  (org-move-subtree-down (- (prefix-numeric-value arg))))

(defun org-clean-visibility-after-subtree-move ()
  "Fix visibility issues after moving a subtree."
  ;; First, find a reasonable region to look at:
  ;; Start two siblings above, end three below
  (let* ((beg (save-excursion
		(and (org-get-previous-sibling)
		     (org-get-previous-sibling))
		(point)))
	 (end (save-excursion
		(and (org-get-next-sibling)
		     (org-get-next-sibling)
		     (org-get-next-sibling))
		(if (org-at-heading-p)
		    (line-end-position)
		  (point))))
	 (level (looking-at "\\*+"))
	 (re (when level (concat "^" (regexp-quote (match-string 0)) " "))))
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(when re
	  ;; Properly fold already folded siblings
	  (goto-char (point-min))
	  (while (re-search-forward re nil t)
	    (when (and (not (org-invisible-p))
		       (org-invisible-p (line-end-position)))
	      (org-fold-heading nil))))
	(org-cycle-hide-drawers 'all)
	(org-cycle-show-empty-lines 'overview)))))

(defun org-move-subtree-down (&optional arg)
  "Move the current subtree down past ARG headlines of the same level."
  (interactive "p")
  (setq arg (prefix-numeric-value arg))
  (org-preserve-local-variables
   (let ((movfunc (if (> arg 0) 'org-get-next-sibling
		    'org-get-previous-sibling))
	 (ins-point (make-marker))
	 (cnt (abs arg))
	 (col (current-column))
	 beg end txt folded)
     ;; Select the tree
     (org-back-to-heading)
     (setq beg (point))
     (save-match-data
       (save-excursion (outline-end-of-heading)
		       (setq folded (org-invisible-p)))
       (progn (org-end-of-subtree nil t)
	      (unless (eobp) (backward-char))))
     (outline-next-heading)
     (setq end (point))
     (goto-char beg)
     ;; Find insertion point, with error handling
     (while (> cnt 0)
       (unless (and (funcall movfunc) (looking-at org-outline-regexp))
	 (goto-char beg)
	 (user-error "Cannot move past superior level or buffer limit"))
       (setq cnt (1- cnt)))
     (when (> arg 0)
       ;; Moving forward - still need to move over subtree
       (org-end-of-subtree t t)
       (save-excursion
	 (org-back-over-empty-lines)
	 (or (bolp) (newline))))
     (move-marker ins-point (point))
     (setq txt (buffer-substring beg end))
     (org-save-markers-in-region beg end)
     (delete-region beg end)
     (when (eq org-fold-core-style 'overlays) (org-remove-empty-overlays-at beg))
     (unless (= beg (point-min)) (org-fold-region (1- beg) beg nil 'outline))
     (unless (bobp) (org-fold-region (1- (point)) (point) nil 'outline))
     (and (not (bolp)) (looking-at "\n") (forward-char 1))
     (let ((bbb (point)))
       (insert-before-markers txt)
       (org-reinstall-markers-in-region bbb)
       (move-marker ins-point bbb))
     (or (bolp) (insert "\n"))
     (goto-char ins-point)
     (org-skip-whitespace)
     (move-marker ins-point nil)
     (if folded
	 (org-fold-subtree t)
       (org-fold-show-entry 'hide-drawers)
       (org-fold-show-children))
     (org-clean-visibility-after-subtree-move)
     ;; move back to the initial column we were at
     (move-to-column col))))

(defvar org-subtree-clip ""
  "Clipboard for cut and paste of subtrees.
This is actually only a copy of the kill, because we use the normal kill
ring.  We need it to check if the kill was created by `org-copy-subtree'.")

(defvar org-subtree-clip-folded nil
  "Was the last copied subtree folded?
This is used to fold the tree back after pasting.")

(defun org-cut-subtree (&optional n)
  "Cut the current subtree into the clipboard.
With prefix arg N, cut this many sequential subtrees.
This is a short-hand for marking the subtree and then cutting it."
  (interactive "p")
  (org-copy-subtree n 'cut))

(defun org-copy-subtree (&optional n cut force-store-markers nosubtrees)
  "Copy the current subtree into the clipboard.
With prefix arg N, copy this many sequential subtrees.
This is a short-hand for marking the subtree and then copying it.
If CUT is non-nil, actually cut the subtree.
If FORCE-STORE-MARKERS is non-nil, store the relative locations
of some markers in the region, even if CUT is non-nil.  This is
useful if the caller implements cut-and-paste as copy-then-paste-then-cut."
  (interactive "p")
  (org-preserve-local-variables
   (let (beg end folded (beg0 (point)))
     (if (called-interactively-p 'any)
	 (org-back-to-heading nil)    ; take what looks like a subtree
       (org-back-to-heading t))	      ; take what is really there
     ;; Do not consider inlinetasks as a subtree.
     (when (org-element-type-p (org-element-at-point) 'inlinetask)
       (org-up-element))
     (setq beg (point))
     (skip-chars-forward " \t\r\n")
     (save-match-data
       (if nosubtrees
	   (outline-next-heading)
	 (save-excursion (outline-end-of-heading)
			 (setq folded (org-invisible-p)))
	 (ignore-errors (org-forward-heading-same-level (1- n) t))
	 (org-end-of-subtree t t)))
     ;; Include the end of an inlinetask
     (when (and (featurep 'org-inlinetask)
		(looking-at-p (concat (org-inlinetask-outline-regexp)
				      "END[ \t]*$")))
       (end-of-line))
     (setq end (point))
     (goto-char beg0)
     (when (> end beg)
       (setq org-subtree-clip-folded folded)
       (when (or cut force-store-markers)
	 (org-save-markers-in-region beg end))
       (if cut (kill-region beg end) (copy-region-as-kill beg end))
       (setq org-subtree-clip (current-kill 0))
       (message "%s: Subtree(s) with %d characters"
		(if cut "Cut" "Copied")
		(length org-subtree-clip))))))

(defun org-paste-subtree (&optional level tree for-yank remove)
  "Paste the clipboard as a subtree, with modification of headline level.

The entire subtree is promoted or demoted in order to match a new headline
level.

If the cursor is at the beginning of a headline, the same level as
that headline is used to paste the tree before current headline.

With `\\[universal-argument]' prefix, force inserting at the same level
as current headline, after subtree at point.

With `\\[universal-argument]' `\\[universal-argument]' prefix, force
inserting as a child headline, as the first child.

If not, the new level is derived from the *visible* headings
before and after the insertion point, and taken to be the inferior headline
level of the two.  So if the previous visible heading is level 3 and the
next is level 4 (or vice versa), level 4 will be used for insertion.
This makes sure that the subtree remains an independent subtree and does
not swallow low level entries.

You can also force a different level, either by using a numeric prefix
argument, or by inserting the heading marker by hand.  For example, if the
cursor is after \"*****\", then the tree will be shifted to level 5.

If optional TREE is given, use this text instead of the kill ring.

When FOR-YANK is set, this is called by `org-yank'.  In this case, do not
move back over whitespace before inserting, and move point to the end of
the inserted text when done.

When REMOVE is non-nil, remove the subtree from the clipboard."
  (interactive "P")
  (setq tree (or tree (current-kill 0)))
  (unless (org-kill-is-subtree-p tree)
    (user-error
     (substitute-command-keys
      "The kill is not a (set of) tree(s).  Use `\\[yank]' to yank anyway")))
  (org-with-limited-levels
   (org-fold-core-ignore-fragility-checks
     (let* ((visp (not (org-invisible-p)))
	    (txt tree)
	    (old-level (if (string-match org-outline-regexp-bol txt)
			   (- (match-end 0) (match-beginning 0) 1)
		         -1))
            level-indicator?
	    (force-level
	     (cond
	      ;; When point is after the stars in an otherwise empty
	      ;; headline, use the number of stars as the forced level.
	      ((and (or (not level) (member level '((4) (16))))
                    (org-match-line "^\\*+[ \t]*$")
		    (not (eq ?* (char-after))))
	       (setq level-indicator? (org-outline-level)))
              ((equal level '(4)) (org-outline-level))
              ((equal level '(16)) nil) ; handle later
	      (level (prefix-numeric-value level))
	      ((looking-at-p org-outline-regexp-bol) (org-outline-level))))
	    (previous-level
	     (save-excursion
	       (unless (org-at-heading-p) (org-previous-visible-heading 1))
	       (if (org-at-heading-p) (org-outline-level) 1)))
	    (next-level
	     (save-excursion
	       (org-next-visible-heading 1)
	       (if (org-at-heading-p) (org-outline-level) 1)))
	    (new-level (or force-level
                           (max
                            ;; C-u C-u forces child.
                            (if (equal level '(16)) (1+ previous-level) 0)
                            previous-level
                            next-level)))
	    (shift (if (or (= old-level -1)
			   (= new-level -1)
			   (= old-level new-level))
		       0
		     (- new-level old-level)))
	    (delta (if (> shift 0) -1 1))
	    (func (if (> shift 0) #'org-demote #'org-promote))
	    (org-odd-levels-only nil)
	    beg end newend)
       ;; Remove the forced level indicator.
       (when level-indicator?
         (delete-region (line-beginning-position) (line-beginning-position 2)))
       ;; Paste before the next visible heading or at end of buffer,
       ;; unless point is at the beginning of a headline.
       (unless (and (bolp) (org-at-heading-p) (not (member level '((4) (16)))))
         (when (equal level '(4)) (org-end-of-subtree t))
         (org-next-visible-heading 1)
         (unless (bolp) (insert "\n")))
       (setq beg (point))
       ;; Avoid re-parsing cache elements when i.e. level 1 heading
       ;; is inserted and then promoted.
       (org-combine-change-calls beg beg
         (when (fboundp 'org-id-paste-tracker) (org-id-paste-tracker txt))
         (insert txt)
         (unless (string-suffix-p "\n" txt) (insert "\n"))
         (setq newend (point))
         (org-reinstall-markers-in-region beg)
         (setq end (point))
         (goto-char beg)
         (skip-chars-forward " \t\n\r")
         (setq beg (point))
         (when (and (org-invisible-p) visp)
           (save-excursion (org-fold-heading nil)))
         ;; Shift if necessary.
         (unless (= shift 0)
           (save-restriction
	     (narrow-to-region beg end)
	     (while (not (= shift 0))
	       (org-map-region func (point-min) (point-max))
	       (setq shift (+ delta shift)))
	     (goto-char (point-min))
	     (setq newend (point-max)))))
       (when (or for-yank (called-interactively-p 'interactive))
         (message "Clipboard pasted as level %d subtree" new-level))
       (when (and (not for-yank) ; in this case, org-yank will decide about folding
		  (equal org-subtree-clip tree)
		  org-subtree-clip-folded)
         ;; The tree was folded before it was killed/copied
         (org-fold-subtree t))
       (when for-yank (goto-char newend))
       (when remove (pop kill-ring))))))

(defvar org-markers-to-move nil
  "Markers that should be moved with a cut-and-paste operation.
Those markers are stored together with their positions relative to
the start of the region.")

(defvar org-log-note-marker) ; defined later
(defun org-save-markers-in-region (beg end)
  "Check markers in region.
If these markers are between BEG and END, record their position relative
to BEG, so that after moving the block of text, we can put the markers back
into place.
This function gets called just before an entry or tree gets cut from the
buffer.  After re-insertion, `org-reinstall-markers-in-region' must be
called immediately, to move the markers with the entries."
  (setq org-markers-to-move nil)
  (org-check-and-save-marker org-log-note-marker beg end)
  (when (featurep 'org-clock)
    (org-clock-save-markers-for-cut-and-paste beg end))
  (when (featurep 'org-agenda)
    (org-agenda-save-markers-for-cut-and-paste beg end)))

(defun org-check-and-save-marker (marker beg end)
  "Check if MARKER is between BEG and END.
If yes, remember the marker and the distance to BEG."
  (when (and (marker-buffer marker)
	     (or (equal (marker-buffer marker) (current-buffer))
                 (equal (marker-buffer marker) (buffer-base-buffer (current-buffer))))
	     (>= marker beg) (< marker end))
    (push (cons marker (- marker beg)) org-markers-to-move)))

(defun org-reinstall-markers-in-region (beg)
  "Move all remembered markers to their position relative to BEG."
  (dolist (x org-markers-to-move)
    (move-marker (car x) (+ beg (cdr x))))
  (setq org-markers-to-move nil))

(defun org-clone-subtree-with-time-shift (n &optional shift)
  "Clone the task (subtree) at point N times.
The clones will be inserted as siblings.

In interactive use, the user will be prompted for the number of
clones to be produced.  If the entry has a timestamp, the user
will also be prompted for a time shift, which may be a repeater
as used in time stamps, for example `+3d'.  To disable this,
you can call the function with a universal prefix argument.

When a valid repeater is given and the entry contains any time
stamps, the clones will become a sequence in time, with time
stamps in the subtree shifted for each clone produced.  If SHIFT
is nil or the empty string, time stamps will be left alone.  The
ID property of the original subtree is removed.

In each clone, all the CLOCK entries will be removed.  This
prevents Org from considering that the clocked times overlap.

If the original subtree did contain time stamps with a repeater,
the following will happen:
- the repeater will be removed in each clone
- an additional clone will be produced, with the current, unshifted
  date(s) in the entry.
- the original entry will be placed *after* all the clones, with
  repeater intact.
- the start days in the repeater in the original entry will be shifted
  to past the last clone.
In this way you can spell out a number of instances of a repeating task,
and still retain the repeater to cover future instances of the task.

As described above, N+1 clones are produced when the original
subtree has a repeater.  Setting N to 0, then, can be used to
remove the repeater from a subtree and create a shifted clone
with the original repeater."
  (interactive "nNumber of clones to produce: ")
  (unless (wholenump n) (user-error "Invalid number of replications %s" n))
  (when (org-before-first-heading-p) (user-error "No subtree to clone"))
  (let* ((beg (save-excursion (org-back-to-heading t) (point)))
	 (end-of-tree (save-excursion (org-end-of-subtree t t) (point)))
	 (shift
	  (or shift
	      (if (and (not (equal current-prefix-arg '(4)))
		       (save-excursion
			 (goto-char beg)
			 (re-search-forward org-ts-regexp-both end-of-tree t)))
		  (read-from-minibuffer
		   "Date shift per clone (e.g. +1w, empty to copy unchanged): ")
		"")))			;No time shift
	 (doshift
	  (and (org-string-nw-p shift)
	       (or (string-match "\\`[ \t]*\\([+-]?[0-9]+\\)\\([hdwmy]\\)[ \t]*\\'"
				 shift)
		   (user-error "Invalid shift specification %s" shift)))))
    (goto-char end-of-tree)
    (unless (bolp) (insert "\n"))
    (let* ((end (point))
	   (template (buffer-substring beg end))
	   (shift-n (and doshift (string-to-number (match-string 1 shift))))
	   (shift-what (pcase (and doshift (match-string 2 shift))
			 (`nil nil)
			 ("h" 'hour)
			 ("d" 'day)
			 ("w" (setq shift-n (* 7 shift-n)) 'day)
			 ("m" 'month)
			 ("y" 'year)
			 (_ (error "Unsupported time unit"))))
	   (nmin 1)
	   (nmax n)
	   (n-no-remove -1)
	   (org-id-overriding-file-name (buffer-file-name (buffer-base-buffer)))
	   (idprop (org-entry-get beg "ID")))
      (when (and doshift
		 (string-match-p "<[^<>\n]+ [.+]?\\+[0-9]+[hdwmy][^<>\n]*>"
				 template))
	(delete-region beg end)
	(setq end beg)
	(setq nmin 0)
	(setq nmax (1+ nmax))
	(setq n-no-remove nmax))
      (goto-char end)
      (cl-loop for n from nmin to nmax do
	       (insert
		;; Prepare clone.
		(with-temp-buffer
		  (insert template)
		  (org-mode)
		  (goto-char (point-min))
		  (org-fold-show-subtree)
		  (and idprop (if org-clone-delete-id
				  (org-entry-delete nil "ID")
				(org-id-get-create t)))
		  (unless (= n 0)
		    (while (re-search-forward org-clock-line-re nil t)
		      (delete-region (line-beginning-position)
				     (line-beginning-position 2)))
		    (goto-char (point-min))
		    (while (re-search-forward org-drawer-regexp nil t)
		      (org-remove-empty-drawer-at (point))))
		  (goto-char (point-min))
		  (when doshift
		    (while (re-search-forward org-ts-regexp-both nil t)
		      (org-timestamp-change (* n shift-n) shift-what))
		    (unless (= n n-no-remove)
		      (goto-char (point-min))
		      (while (re-search-forward org-ts-regexp nil t)
			(save-excursion
			  (goto-char (match-beginning 0))
			  (when (looking-at "<[^<>\n]+\\( +[.+]?\\+[0-9]+[hdwmy]\\)")
			    (delete-region (match-beginning 1) (match-end 1)))))))
		  (buffer-string)))))
    (goto-char beg)))

;;; Outline Sorting

(defun org-sort (&optional with-case)
  "Call `org-sort-entries', `org-table-sort-lines' or `org-sort-list'.
Optional argument WITH-CASE means sort case-sensitively."
  (interactive "P")
  (org-call-with-arg
   (cond ((org-at-table-p) #'org-table-sort-lines)
	 ((org-at-item-p) #'org-sort-list)
	 (t #'org-sort-entries))
   with-case))

(defun org-sort-remove-invisible (s)
  "Remove emphasis markers and any invisible property from string S.
Assume S may contain only objects."
  ;; org-element-interpret-data clears any text property, including
  ;; invisible part.
  (org-element-interpret-data
   (let ((tree (org-element-parse-secondary-string
                s (org-element-restriction 'paragraph))))
     (org-element-map tree '(bold code italic link strike-through underline verbatim)
       (lambda (o)
         (pcase (org-element-type o)
           ;; Terminal object.  Replace it with its value.
           ((or `code `verbatim)
            (let ((new (org-element-property :value o)))
              (org-element-insert-before new o)
              (org-element-put-property
               new :post-blank (org-element-post-blank o))))
           ;; Non-terminal objects.  Splice contents.
           (type
            (let ((contents
                   (or (org-element-contents o)
                       (and (eq type 'link)
                            (list (org-element-property :raw-link o)))))
                  (c nil))
              (while contents
                (setq c (pop contents))
                (org-element-insert-before c o))
              (org-element-put-property
               c :post-blank (org-element-post-blank o)))))
         (org-element-extract o)))
     ;; Return modified tree.
     tree)))

(defvar org-after-sorting-entries-or-items-hook nil
  "Hook that is run after a bunch of entries or items have been sorted.
When children are sorted, the cursor is in the parent line when this
hook gets called.  When a region or a plain list is sorted, the cursor
will be in the first entry of the sorted region/list.")

(defun org-sort-entries
    (&optional with-case sorting-type getkey-func compare-func property
	       interactive?)
  "Sort entries on a certain level of an outline tree.
If there is an active region, the entries in the region are sorted.
Else, if the cursor is before the first entry, sort the top-level items.
Else, the children of the entry at point are sorted.

Sorting can be alphabetically, numerically, by date/time as given by
a time stamp, by a property, by priority order, or by a custom function.

The command prompts for the sorting type unless it has been given to the
function through the SORTING-TYPE argument, which needs to be a character,
\(?n ?N ?a ?A ?t ?T ?s ?S ?d ?D ?p ?P ?o ?O ?r ?R ?f ?F ?k ?K).  Here is
the precise meaning of each character:

a   Alphabetically, ignoring the TODO keyword and the priority, if any.
c   By creation time, which is assumed to be the first inactive time stamp
    at the beginning of a line.
d   By deadline date/time.
k   By clocking time.
n   Numerically, by converting the beginning of the entry/item to a number.
o   By order of TODO keywords.
p   By priority according to the cookie.
r   By the value of a property.
s   By scheduled date/time.
t   By date/time, either the first active time stamp in the entry, or, if
    none exist, by the first inactive one.

Capital letters will reverse the sort order.

If the SORTING-TYPE is ?f or ?F, then GETKEY-FUNC specifies a function to be
called with point at the beginning of the record.  It must return a
value that is compatible with COMPARE-FUNC, the function used to
compare entries.

Comparing entries ignores case by default.  However, with an optional argument
WITH-CASE, the sorting considers case as well.

Sorting is done against the visible part of the headlines, it ignores hidden
links.

When sorting is done, call `org-after-sorting-entries-or-items-hook'.

A non-nil value for INTERACTIVE? is used to signal that this
function is being called interactively."
  (interactive (list current-prefix-arg nil nil nil nil t))
  (let ((case-func (if with-case 'identity 'downcase))
        start beg end stars re re2
        txt what tmp)
    ;; Find beginning and end of region to sort
    (cond
     ((use-region-p)
      (setq start (region-beginning)
            end (region-end))
      ;; we will sort the region
      ;; Limit the region to full headings.
      (goto-char start)
      ;; Move to beginning of heading.
      ;; If we are inside heading, move to next.
      ;; If we are on heading, move to its begin position.
      (if (org-at-heading-p)
          (forward-line 0)
        (outline-next-heading))
      (setq start (point))
      ;; Extend region end beyond the last subtree.
      (goto-char end)
      (org-end-of-subtree nil t)
      (setq end (point)
            what "region")
      (goto-char start))
     ((or (org-at-heading-p)
          (ignore-errors (progn (org-back-to-heading) t)))
      ;; we will sort the children of the current headline
      (org-back-to-heading)
      (setq start (point)
	    end (progn (org-end-of-subtree t t)
		       (or (bolp) (insert "\n"))
		       (when (>= (org-back-over-empty-lines) 1)
			 (forward-line 1))
		       (point))
	    what "children")
      (goto-char start)
      (org-fold-show-subtree)
      (outline-next-heading))
     (t
      ;; we will sort the top-level entries in this file
      (goto-char (point-min))
      (or (org-at-heading-p) (outline-next-heading))
      (setq start (point))
      (goto-char (point-max))
      (forward-line 0)
      (when (looking-at ".*?\\S-")
	;; File ends in a non-white line
	(end-of-line 1)
	(insert "\n"))
      (setq end (point-max))
      (setq what "top-level")
      (goto-char start)
      (org-fold-show-all '(headings drawers blocks))))

    (setq beg (point))
    (when (>= beg end) (goto-char start) (user-error "Nothing to sort"))

    (looking-at "\\(\\*+\\)")
    (setq stars (match-string 1)
	  re (concat "^" (regexp-quote stars) " +")
	  re2 (concat "^" (regexp-quote (substring stars 0 -1)) "[ \t\n]")
	  txt (buffer-substring beg end))
    (unless (equal (substring txt -1) "\n") (setq txt (concat txt "\n")))
    (when (and (not (equal stars "*")) (string-match re2 txt))
      (user-error "Region to sort contains a level above the first entry"))

    (unless sorting-type
      (message
       "Sort %s: [a]lpha  [n]umeric  [p]riority  p[r]operty  todo[o]rder  [f]unc
               [t]ime [s]cheduled  [d]eadline  [c]reated  cloc[k]ing
               A/N/P/R/O/F/T/S/D/C/K means reversed:"
       what)
      (setq sorting-type (read-char-exclusive)))

    (unless getkey-func
      (and (= (downcase sorting-type) ?f)
	   (setq getkey-func
		 (or (and interactive?
			  (org-read-function
			   "Function for extracting keys: "))
		     (error "Missing key extractor")))))

    (and (= (downcase sorting-type) ?r)
	 (not property)
	 (setq property
	       (completing-read "Property: "
				(mapcar #'list (org-buffer-property-keys t))
				nil t)))

    (when (member sorting-type '(?k ?K)) (org-clock-sum))
    (message "Sorting entries...")

    (save-restriction
      (narrow-to-region start end)
      ;; No trailing newline - add one to avoid
      ;; * heading
      ;; text* another heading
      (save-excursion
        (goto-char end)
        (unless (bolp) (insert "\n")))
      (let ((restore-clock?
	     ;; The clock marker is lost when using `sort-subr'; mark
	     ;; the clock with temporary `:org-clock-marker-backup'
	     ;; text property.
	     (when (and (eq (org-clocking-buffer) (current-buffer))
			(<= start (marker-position org-clock-marker))
			(>= end (marker-position org-clock-marker)))
	       (with-silent-modifications
		 (put-text-property (1- org-clock-marker) org-clock-marker
				    :org-clock-marker-backup t))
	       t))
	    (dcst (downcase sorting-type))
	    (case-fold-search nil)
	    (now (current-time)))
        (org-preserve-local-variables
	 (sort-subr
	  (/= dcst sorting-type)
	  ;; This function moves to the beginning character of the
	  ;; "record" to be sorted.
	  (lambda nil
	    (if (re-search-forward re nil t)
		(goto-char (match-beginning 0))
	      (goto-char (point-max))))
	  ;; This function moves to the last character of the "record" being
	  ;; sorted.
	  (lambda nil
	    (save-match-data
	      (condition-case nil
		  (outline-forward-same-level 1)
		(error
		 (goto-char (point-max))))))
	  ;; This function returns the value that gets sorted against.
	  (lambda ()
	    (cond
	     ((= dcst ?n)
	      (string-to-number
	       (org-sort-remove-invisible (org-get-heading t t t t))))
	     ((= dcst ?a)
	      (funcall case-func
		       (org-sort-remove-invisible (org-get-heading t t t t))))
	     ((= dcst ?k)
	      (or (get-text-property (point) :org-clock-minutes) 0))
	     ((= dcst ?t)
	      (let ((end (save-excursion (outline-next-heading) (point))))
		(if (or (re-search-forward org-ts-regexp end t)
			(re-search-forward org-ts-regexp-both end t))
		    (org-time-string-to-seconds (match-string 0))
		  (float-time now))))
	     ((= dcst ?c)
	      (let ((end (save-excursion (outline-next-heading) (point))))
		(if (re-search-forward
		     (concat "^[ \t]*\\[" org-ts-regexp1 "\\]")
		     end t)
		    (org-time-string-to-seconds (match-string 0))
		  (float-time now))))
	     ((= dcst ?s)
	      (let ((end (save-excursion (outline-next-heading) (point))))
		(if (re-search-forward org-scheduled-time-regexp end t)
		    (org-time-string-to-seconds (match-string 1))
		  (float-time now))))
	     ((= dcst ?d)
	      (let ((end (save-excursion (outline-next-heading) (point))))
		(if (re-search-forward org-deadline-time-regexp end t)
		    (org-time-string-to-seconds (match-string 1))
		  (float-time now))))
	     ((= dcst ?p)
              (if (re-search-forward org-priority-regexp (line-end-position) t)
		  (string-to-char (match-string 2))
		org-priority-default))
	     ((= dcst ?r)
	      (or (org-entry-get nil property) ""))
	     ((= dcst ?o)
	      (when (looking-at org-complex-heading-regexp)
		(let* ((m (match-string 2))
		       (s (if (member m org-done-keywords) '- '+)))
		  (- 99 (funcall s (length (member m org-todo-keywords-1)))))))
	     ((= dcst ?f)
	      (if getkey-func
		  (progn
		    (setq tmp (funcall getkey-func))
		    (when (stringp tmp) (setq tmp (funcall case-func tmp)))
		    tmp)
		(error "Invalid key function `%s'" getkey-func)))
	     (t (error "Invalid sorting type `%c'" sorting-type))))
	  nil
	  (cond
	   ((= dcst ?a) #'org-string<)
	   ((= dcst ?f)
	    (or compare-func
		(and interactive?
		     (org-read-function
		      (concat "Function for comparing keys "
			      "(empty for default `sort-subr' predicate): ")
		      'allow-empty))))
	   ((member dcst '(?p ?t ?s ?d ?c ?k)) '<))))
	(org-cycle-hide-drawers 'all)
	(when restore-clock?
	  (move-marker org-clock-marker
		       (1+ (next-single-property-change
			    start :org-clock-marker-backup)))
	  (remove-text-properties (1- org-clock-marker) org-clock-marker
				  '(:org-clock-marker-backup t)))))
    (run-hooks 'org-after-sorting-entries-or-items-hook)
    (message "Sorting entries...done")))

(provide 'org-edit-structure)

;;; org-edit-structure.el ends here
