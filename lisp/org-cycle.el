;;; org-cycle.el --- Visibility cycling of Org entries -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2020 Free Software Foundation, Inc.
;;
;; Author: ??
;; Keywords: folding, visibility cycling, invisible text
;; Homepage: https://orgmode.org
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

;; This file contains code controlling global folding state in buffer
;; and TAB-cycling.

;;; Code:

(require 'org-macs)
(require 'org-fold)

(declare-function org-element-type "org-element" (element))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-lineage "org-element" (datum &optional types with-self))
(declare-function org-inlinetask-at-task-p "org-inlinetask" ())
(declare-function org-inlinetask-toggle-visibility "org-inlinetask" ())

(defvar-local org-cycle-global-status nil)
(put 'org-cycle-global-status 'org-state t)
(defvar-local org-cycle-subtree-status nil)
(put 'org-cycle-subtree-status 'org-state t)

(defun org-cycle-hide-drawers (state)
  "Re-hide all drawers after a visibility state change.
STATE should be one of the symbols listed in the docstring of
`org-cycle-hook'."
  (when (and (derived-mode-p 'org-mode)
	     (not (memq state '(overview folded contents))))
    (let* ((global? (eq state 'all))
	   (beg (if global? (point-min) (line-beginning-position)))
	   (end (cond (global? (point-max))
		      ((eq state 'children) (org-entry-end-position))
		      (t (save-excursion (org-end-of-subtree t t))))))
      (save-excursion
	(goto-char beg)
	(while (re-search-forward org-drawer-regexp end t)
	  (if (org-fold-get-folding-spec (org-fold-get-folding-spec-for-element 'drawer))
	      ;; Do not fold already folded drawers.
              (goto-char (min end (org-fold-next-folding-state-change (org-fold-get-folding-spec-for-element 'drawer))))
	    (let ((drawer (org-element-at-point)))
	      (when (memq (org-element-type drawer) '(drawer property-drawer))
		(org-fold-hide-drawer-toggle t nil drawer)
		;; Make sure to skip drawer entirely or we might flag
		;; it another time when matching its ending line with
		;; `org-drawer-regexp'.
		(goto-char (org-element-property :end drawer))))))))))

;;;###autoload
(defun org-cycle (&optional arg)
  "TAB-action and visibility cycling for Org mode.

This is the command invoked in Org mode by the `TAB' key.  Its main
purpose is outline visibility cycling, but it also invokes other actions
in special contexts.

When this function is called with a `\\[universal-argument]' prefix, rotate \
the entire
buffer through 3 states (global cycling)
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.

With a `\\[universal-argument] \\[universal-argument]' prefix argument, \
switch to the startup visibility,
determined by the variable `org-startup-folded', and by any VISIBILITY
properties in the buffer.

With a `\\[universal-argument] \\[universal-argument] \
\\[universal-argument]' prefix argument, show the entire buffer, including
any drawers.

When inside a table, re-align the table and move to the next field.

When point is at the beginning of a headline, rotate the subtree started
by this line through 3 different states (local cycling)
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.
               From this state, you can move to one of the children
               and zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.
If there is no subtree, switch directly from CHILDREN to FOLDED.

When point is at the beginning of an empty headline and the variable
`org-cycle-level-after-item/entry-creation' is set, cycle the level
of the headline by demoting and promoting it to likely levels.  This
speeds up creation document structure by pressing `TAB' once or several
times right after creating a new headline.

When there is a numeric prefix, go up to a heading with level ARG, do
a `show-subtree' and return to the previous cursor position.  If ARG
is negative, go up that many levels.

When point is not at the beginning of a headline, execute the global
binding for `TAB', which is re-indenting the line.  See the option
`org-cycle-emulate-tab' for details.

As a special case, if point is at the very beginning of the buffer, if
there is no headline there, and if the variable `org-cycle-global-at-bob'
is non-nil, this function acts as if called with prefix argument \
\(`\\[universal-argument] TAB',
same as `S-TAB') also when called without prefix argument."
  (interactive "P")
  (org-load-modules-maybe)
  (unless (or (run-hook-with-args-until-success 'org-tab-first-hook)
	      (and org-cycle-level-after-item/entry-creation
		   (or (org-cycle-level)
		       (org-cycle-item-indentation))))
    (let* ((limit-level
	    (or org-cycle-max-level
		(and (boundp 'org-inlinetask-min-level)
		     org-inlinetask-min-level
		     (1- org-inlinetask-min-level))))
	   (nstars
	    (and limit-level
		 (if org-odd-levels-only
		     (1- (* 2 limit-level))
		   limit-level)))
	   (org-outline-regexp
	    (format "\\*%s " (if nstars (format "\\{1,%d\\}" nstars) "+"))))
      (cond
       ((equal arg '(16))
	(setq last-command 'dummy)
	(org-cycle-set-startup-visibility)
	(org-unlogged-message "Startup visibility, plus VISIBILITY properties"))
       ((equal arg '(64))
	(org-fold-show-all)
	(org-unlogged-message "Entire buffer visible, including drawers"))
       ((equal arg '(4)) (org-cycle-internal-global))
       ;; Show-subtree, ARG levels up from here.
       ((integerp arg)
	(save-excursion
	  (org-back-to-heading)
	  (outline-up-heading (if (< arg 0) (- arg)
				(- (funcall outline-level) arg)))
	  (org-fold-show-subtree)))
       ;; Global cycling at BOB: delegate to `org-cycle-internal-global'.
       ((and org-cycle-global-at-bob
	     (bobp)
	     (not (looking-at org-outline-regexp)))
	(let ((org-cycle-hook
	       (remq 'org-optimize-window-after-visibility-change
		     org-cycle-hook)))
	  (org-cycle-internal-global)))
       ;; Try CDLaTeX TAB completion.
       ((org-try-cdlatex-tab))
       ;; Inline task: delegate to `org-inlinetask-toggle-visibility'.
       ((and (featurep 'org-inlinetask)
	     (org-inlinetask-at-task-p)
	     (or (bolp) (not (eq org-cycle-emulate-tab 'exc-hl-bol))))
	(org-inlinetask-toggle-visibility))
       (t
	(let ((pos (point))
	      (element (org-element-at-point)))
	  (cond
	   ;; Try toggling visibility for block at point.
	   ((org-fold-hide-block-toggle nil t element))
	   ;; Try toggling visibility for drawer at point.
	   ((org-fold-hide-drawer-toggle nil t element))
	   ;; Table: enter it or move to the next field.
	   ((and (org-match-line "[ \t]*[|+]")
		 (org-element-lineage element '(table) t))
	    (if (and (eq 'table (org-element-type element))
		     (eq 'table.el (org-element-property :type element)))
		(message (substitute-command-keys "\\<org-mode-map>\
Use `\\[org-edit-special]' to edit table.el tables"))
	      (org-table-justify-field-maybe)
	      (call-interactively #'org-table-next-field)))
	   ((run-hook-with-args-until-success
	     'org-tab-after-check-for-table-hook))
	   ;; At an item/headline: delegate to `org-cycle-internal-local'.
	   ((and (or (and org-cycle-include-plain-lists
			  (let ((item (org-element-lineage element
							   '(item plain-list)
							   t)))
			    (and item
				 (= (line-beginning-position)
				    (org-element-property :post-affiliated
							  item)))))
		     (org-match-line org-outline-regexp))
		 (or (bolp) (not (eq org-cycle-emulate-tab 'exc-hl-bol))))
	    (org-cycle-internal-local))
	   ;; From there: TAB emulation and template completion.
	   (buffer-read-only (org-back-to-heading))
	   ((run-hook-with-args-until-success
	     'org-tab-after-check-for-cycling-hook))
	   ((run-hook-with-args-until-success
	     'org-tab-before-tab-emulation-hook))
	   ((and (eq org-cycle-emulate-tab 'exc-hl-bol)
		 (or (not (bolp))
		     (not (looking-at org-outline-regexp))))
	    (call-interactively (global-key-binding (kbd "TAB"))))
	   ((or (eq org-cycle-emulate-tab t)
		(and (memq org-cycle-emulate-tab '(white whitestart))
		     (save-excursion (beginning-of-line 1) (looking-at "[ \t]*"))
		     (or (and (eq org-cycle-emulate-tab 'white)
			      (= (match-end 0) (point-at-eol)))
			 (and (eq org-cycle-emulate-tab 'whitestart)
			      (>= (match-end 0) pos)))))
	    (call-interactively (global-key-binding (kbd "TAB"))))
	   (t
	    (save-excursion
	      (org-back-to-heading)
	      (org-cycle))))))))))

(defun org-cycle-internal-global ()
  "Do the global cycling action."
  ;; Hack to avoid display of messages for .org  attachments in Gnus
  (let ((ga (string-match-p "\\*fontification" (buffer-name))))
    (cond
     ((and (eq last-command this-command)
	   (eq org-cycle-global-status 'overview))
      ;; We just created the overview - now do table of contents
      ;; This can be slow in very large buffers, so indicate action
      (run-hook-with-args 'org-pre-cycle-hook 'contents)
      (unless ga (org-unlogged-message "CONTENTS..."))
      (org-content)
      (unless ga (org-unlogged-message "CONTENTS...done"))
      (setq org-cycle-global-status 'contents)
      (run-hook-with-args 'org-cycle-hook 'contents))

     ((and (eq last-command this-command)
	   (eq org-cycle-global-status 'contents))
      ;; We just showed the table of contents - now show everything
      (run-hook-with-args 'org-pre-cycle-hook 'all)
      (org-fold-show-all '(headings blocks))
      (unless ga (org-unlogged-message "SHOW ALL"))
      (setq org-cycle-global-status 'all)
      (run-hook-with-args 'org-cycle-hook 'all))

     (t
      ;; Default action: go to overview
      (run-hook-with-args 'org-pre-cycle-hook 'overview)
      (org-overview)
      (unless ga (org-unlogged-message "OVERVIEW"))
      (setq org-cycle-global-status 'overview)
      (run-hook-with-args 'org-cycle-hook 'overview)))))

(defun org-cycle-internal-local ()
  "Do the local cycling action."
  (let ((goal-column 0) eoh eol eos has-children children-skipped struct)
    ;; First, determine end of headline (EOH), end of subtree or item
    ;; (EOS), and if item or heading has children (HAS-CHILDREN).
    (save-excursion
      (if (org-at-item-p)
	  (progn
	    (beginning-of-line)
	    (setq struct (org-list-struct))
	    (setq eoh (point-at-eol))
	    (setq eos (org-list-get-item-end-before-blank (point) struct))
	    (setq has-children (org-list-has-child-p (point) struct)))
	(org-back-to-heading)
	(setq eoh (save-excursion (outline-end-of-heading) (point)))
	(setq eos (save-excursion
		    (org-end-of-subtree t t)
		    (unless (eobp) (forward-char -1))
		    (point)))
	(setq has-children
	      (or
	       (save-excursion
		 (let ((level (funcall outline-level)))
		   (outline-next-heading)
		   (and (org-at-heading-p t)
			(> (funcall outline-level) level))))
	       (and (eq org-cycle-include-plain-lists 'integrate)
		    (save-excursion
		      (org-list-search-forward (org-item-beginning-re) eos t))))))
      ;; Determine end invisible part of buffer (EOL)
      (beginning-of-line 2)
      (while (and (not (eobp))		;this is like `next-line'
		  (org-fold-get-folding-spec nil (1- (point))))
	(goto-char (org-fold-next-visibility-change (point)))
	(and (eolp) (beginning-of-line 2)))
      (setq eol (point)))
    ;; Find out what to do next and set `this-command'
    (cond
     ((= eos eoh)
      ;; Nothing is hidden behind this heading
      (unless (org-before-first-heading-p)
	(run-hook-with-args 'org-pre-cycle-hook 'empty))
      (org-unlogged-message "EMPTY ENTRY")
      (setq org-cycle-subtree-status nil)
      (save-excursion
	(goto-char eos)
        (org-with-limited-levels
	 (outline-next-heading))
	(when (org-invisible-p) (org-fold-heading nil))))
     ((and (>= eol eos)
	   (or has-children
	       (not (setq children-skipped
			org-cycle-skip-children-state-if-no-children))))
      ;; Entire subtree is hidden in one line: children view
      (unless (org-before-first-heading-p)
	(run-hook-with-args 'org-pre-cycle-hook 'children))
      (if (org-at-item-p)
	  (org-list-set-item-visibility (point-at-bol) struct 'children)
	(org-fold-show-entry)
	(org-with-limited-levels (org-fold-show-children))
	(org-fold-show-set-visibility 'canonical)
	;; Fold every list in subtree to top-level items.
	(when (eq org-cycle-include-plain-lists 'integrate)
	  (save-excursion
	    (org-back-to-heading)
	    (while (org-list-search-forward (org-item-beginning-re) eos t)
	      (beginning-of-line 1)
	      (let* ((struct (org-list-struct))
		     (prevs (org-list-prevs-alist struct))
		     (end (org-list-get-bottom-point struct)))
		(dolist (e (org-list-get-all-items (point) struct prevs))
		  (org-list-set-item-visibility e struct 'folded))
		(goto-char (if (< end eos) end eos)))))))
      (org-unlogged-message "CHILDREN")
      (save-excursion
	(goto-char eos)
        (org-with-limited-levels
	 (outline-next-heading))
	(when (org-invisible-p) (org-fold-heading nil)))
      (setq org-cycle-subtree-status 'children)
      (unless (org-before-first-heading-p)
	(run-hook-with-args 'org-cycle-hook 'children)))
     ((or children-skipped
	  (and (eq last-command this-command)
	       (eq org-cycle-subtree-status 'children)))
      ;; We just showed the children, or no children are there,
      ;; now show everything.
      (unless (org-before-first-heading-p)
	(run-hook-with-args 'org-pre-cycle-hook 'subtree))
      (org-fold-region eoh eos nil (org-fold-get-folding-spec-for-element 'headline))
      (org-unlogged-message
       (if children-skipped "SUBTREE (NO CHILDREN)" "SUBTREE"))
      (setq org-cycle-subtree-status 'subtree)
      (unless (org-before-first-heading-p)
	(run-hook-with-args 'org-cycle-hook 'subtree)))
     (t
      ;; Default action: hide the subtree.
      (run-hook-with-args 'org-pre-cycle-hook 'folded)
      (org-fold-region eoh eos t (org-fold-get-folding-spec-for-element 'headline))
      (org-unlogged-message "FOLDED")
      (setq org-cycle-subtree-status 'folded)
      (unless (org-before-first-heading-p)
	(run-hook-with-args 'org-cycle-hook 'folded))))))

;;;###autoload
(defun org-global-cycle (&optional arg)
  "Cycle the global visibility.  For details see `org-cycle'.
With `\\[universal-argument]' prefix ARG, switch to startup visibility.
With a numeric prefix, show all headlines up to that level."
  (interactive "P")
  (cond
   ((integerp arg)
    (org-content arg)
    (setq org-cycle-global-status 'contents))
   ((equal arg '(4))
    (org-cycle-set-startup-visibility)
    (org-unlogged-message "Startup visibility, plus VISIBILITY properties."))
   (t
    (org-cycle '(4)))))

(defun org-cycle-set-startup-visibility ()
  "Set the visibility required by startup options and properties."
  (cond
   ((eq org-startup-folded t)
    (org-overview))
   ((eq org-startup-folded 'content)
    (org-content))
   ((or (eq org-startup-folded 'showeverything)
	(eq org-startup-folded nil))
    (org-fold-show-all)))
  (unless (eq org-startup-folded 'showeverything)
    (when org-hide-block-startup (org-fold-hide-block-all))
    (org-set-visibility-according-to-property)
    (org-cycle-hide-archived-subtrees 'all)
    (org-cycle-hide-drawers 'all)
    (org-cycle-show-empty-lines t)))

(defun org-set-visibility-according-to-property ()
  "Switch subtree visibility according to VISIBILITY property."
  (interactive)
  (let ((regexp (org-re-property "VISIBILITY")))
    (org-with-point-at 1
      (while (re-search-forward regexp nil t)
	(let ((state (match-string 3)))
	  (if (not (org-at-property-p)) (outline-next-heading)
	    (save-excursion
	      (org-back-to-heading t)
	      (org-fold-subtree t)
	      (org-fold-reveal)
	      (pcase state
		("folded"
		 (org-fold-subtree t))
		("children"
		 (org-fold-show-hidden-entry)
		 (org-fold-show-children))
		("content"
		 (save-excursion
		   (save-restriction
		     (org-narrow-to-subtree)
		     (org-content))))
		((or "all" "showall")
		 (org-fold-show-subtree))
		(_ nil)))
	    (org-end-of-subtree)))))))

(defun org-overview ()
  "Switch to overview mode, showing only top-level headlines."
  (interactive)
  (org-fold-show-all '(headings drawers))
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward org-outline-regexp-bol nil t)
      (let* ((last (line-end-position))
             (level (- (match-end 0) (match-beginning 0) 1))
             (regexp (format "^\\*\\{1,%d\\} " level)))
        (while (re-search-forward regexp nil :move)
          (org-fold-region last (line-end-position 0) t (org-fold-get-folding-spec-for-element 'headline))
          (setq last (line-end-position))
          (setq level (- (match-end 0) (match-beginning 0) 1))
          (setq regexp (format "^\\*\\{1,%d\\} " level)))
        (org-fold-region last (point) t (org-fold-get-folding-spec-for-element 'headline))))))

(defun org-content (&optional arg)
  "Show all headlines in the buffer, like a table of contents.
With numerical argument N, show content up to level N."
  (interactive "p")
  (org-fold-show-all '(headings drawers))
  (save-excursion
    (goto-char (point-max))
    (let ((regexp (if (and (wholenump arg) (> arg 0))
                      (format "^\\*\\{1,%d\\} " arg)
                    "^\\*+ "))
          (last (point)))
      (while (re-search-backward regexp nil t)
        (org-fold-region (line-end-position) last t (org-fold-get-folding-spec-for-element 'headline))
        (setq last (line-end-position 0))))))

(defvar org-scroll-position-to-restore nil
  "Temporarily store scroll position to restore.")
(defun org-optimize-window-after-visibility-change (state)
  "Adjust the window after a change in outline visibility.
This function is the default value of the hook `org-cycle-hook'."
  (when (get-buffer-window (current-buffer))
    (cond
     ((eq state 'content)  nil)
     ((eq state 'all)      nil)
     ((and (eq state 'folded) (eq last-command this-command))
      (set-window-start nil org-scroll-position-to-restore))
     ((eq state 'folded) nil)
     ((eq state 'children)
      (setq org-scroll-position-to-restore (window-start))
      (or (org-subtree-end-visible-p) (recenter 1)))
     ((eq state 'subtree)
      (when (not (eq last-command this-command))
	(setq org-scroll-position-to-restore (window-start)))
      (or (org-subtree-end-visible-p) (recenter 1))))))

(defun org-clean-visibility-after-subtree-move ()
  "Fix visibility issues after moving a subtree."
  ;; First, find a reasonable region to look at:
  ;; Start two siblings above, end three below
  (let* ((beg (save-excursion
		(and (org-get-last-sibling)
		     (org-get-last-sibling))
		(point)))
	 (end (save-excursion
		(and (org-get-next-sibling)
		     (org-get-next-sibling)
		     (org-get-next-sibling))
		(if (org-at-heading-p)
		    (point-at-eol)
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
	      (org-fold-hide-entry))))
	(org-cycle-hide-drawers 'all)
	(org-cycle-show-empty-lines 'overview)))))

(defun org-cycle-show-empty-lines (state)
  "Show empty lines above all visible headlines.
The region to be covered depends on STATE when called through
`org-cycle-hook'.  Lisp program can use t for STATE to get the
entire buffer covered.  Note that an empty line is only shown if there
are at least `org-cycle-separator-lines' empty lines before the headline."
  (when (/= org-cycle-separator-lines 0)
    (save-excursion
      (let* ((n (abs org-cycle-separator-lines))
	     (re (cond
		  ((= n 1) "\\(\n[ \t]*\n\\*+\\) ")
		  ((= n 2) "^[ \t]*\\(\n[ \t]*\n\\*+\\) ")
		  (t (let ((ns (number-to-string (- n 2))))
		       (concat "^\\(?:[ \t]*\n\\)\\{" ns "," ns "\\}"
			       "[ \t]*\\(\n[ \t]*\n\\*+\\) ")))))
	     beg end)
	(cond
	 ((memq state '(overview contents t))
	  (setq beg (point-min) end (point-max)))
	 ((memq state '(children folded))
	  (setq beg (point)
		end (progn (org-end-of-subtree t t)
			   (line-beginning-position 2)))))
	(when beg
	  (goto-char beg)
	  (while (re-search-forward re end t)
	    (unless (org-invisible-p (match-end 1))
	      (let ((e (match-end 1))
		    (b (if (>= org-cycle-separator-lines 0)
			   (match-beginning 1)
			 (save-excursion
			   (goto-char (match-beginning 0))
			   (skip-chars-backward " \t\n")
			   (line-end-position)))))
		(org-fold-region b e nil (org-fold-get-folding-spec-for-element 'headline)))))))))
  ;; Never hide empty lines at the end of the file.
  (save-excursion
    (goto-char (point-max))
    (outline-previous-heading)
    (outline-end-of-heading)
    (when (and (looking-at "[ \t\n]+")
	       (= (match-end 0) (point-max)))
      (org-fold-region (point) (match-end 0) nil (org-fold-get-folding-spec-for-element 'headline)))))

(provide 'org-cycle)

;;; org-cycle.el ends here
