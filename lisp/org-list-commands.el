;;; org-list-commands.el --- Commands to act on plain lists              -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2004-2024 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;;	   Bastien Guerry <bzg@gnu.org>
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains use commands to act on Org mode lists.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-list-core)
(require 'org-list-edit)
(require 'org-move)

(defcustom org-list-use-circular-motion nil
  "Non-nil means commands implying motion in lists should be cyclic.
\\<org-mode-map>
In that case, the item following the last item is the first one,
and the item preceding the first item is the last one.

This affects the behavior of
  `\\[org-move-item-up]',
  `\\[org-move-item-down]',
  `\\[org-next-item]',
  `\\[org-previous-item]'."
  :group 'org-plain-lists
  :version "24.1"
  :type 'boolean)

(defalias 'org-list-get-item-begin 'org-in-item-p)

;;;###autoload
(defun org-beginning-of-item ()
  "Go to the beginning of the current item.
Throw an error when not in a list."
  (interactive)
  (let ((begin (org-in-item-p)))
    (if begin (goto-char begin) (error "Not in an item"))))

;;;###autoload
(defun org-beginning-of-item-list ()
  "Go to the beginning item of the current list or sublist.
Throw an error when not in a list."
  (interactive)
  (let ((begin (org-in-item-p)))
    (if (not begin)
	(error "Not in an item")
      (goto-char begin)
      (let* ((struct (org-list-struct))
	     (prevs (org-list-prevs-alist struct)))
	(goto-char (org-list-get-list-begin begin struct prevs))))))

;;;###autoload
(defun org-end-of-item-list ()
  "Go to the end of the current list or sublist.
Throw an error when not in a list."
  (interactive)
  (let ((begin (org-in-item-p)))
    (if (not begin)
	(error "Not in an item")
      (goto-char begin)
      (let* ((struct (org-list-struct))
	     (prevs (org-list-prevs-alist struct)))
	(goto-char (org-list-get-list-end begin struct prevs))))))

;;;###autoload
(defun org-end-of-item ()
  "Go to the end of the current item.
Throw an error when not in a list."
  (interactive)
  (let ((begin (org-in-item-p)))
    (if (not begin)
	(error "Not in an item")
      (goto-char begin)
      (let ((struct (org-list-struct)))
	(goto-char (org-list-get-item-end begin struct))))))

;;;###autoload
(defun org-previous-item ()
  "Move to the beginning of the previous item.
Throw an error when not in a list.  Also throw an error when at
first item, unless `org-list-use-circular-motion' is non-nil."
  (interactive)
  (let ((item (org-in-item-p)))
    (if (not item)
	(error "Not in an item")
      (goto-char item)
      (let* ((struct (org-list-struct))
	     (prevs (org-list-prevs-alist struct))
	     (prevp (org-list-get-prev-item item struct prevs)))
	(cond
	 (prevp (goto-char prevp))
	 (org-list-use-circular-motion
	  (goto-char (org-list-get-last-item item struct prevs)))
	 (t (error "On first item")))))))

;;;###autoload
(defun org-next-item ()
  "Move to the beginning of the next item.
Throw an error when not in a list.  Also throw an error when at
last item, unless `org-list-use-circular-motion' is non-nil."
  (interactive)
  (let ((item (org-in-item-p)))
    (if (not item)
	(error "Not in an item")
      (goto-char item)
      (let* ((struct (org-list-struct))
	     (prevs (org-list-prevs-alist struct))
	     (prevp (org-list-get-next-item item struct prevs)))
	(cond
	 (prevp (goto-char prevp))
	 (org-list-use-circular-motion
	  (goto-char (org-list-get-first-item item struct prevs)))
	 (t (error "On last item")))))))

;;;###autoload
(defun org-move-item-down ()
  "Move the item at point down, i.e. swap with following item.
Sub-items (items with larger indentation) are considered part of
the item, so this really moves item trees."
  (interactive)
  (unless (org-at-item-p) (error "Not at an item"))
  (let* ((col (current-column))
         (item (line-beginning-position))
	 (struct (org-list-struct))
	 (prevs (org-list-prevs-alist struct))
         (next-item (org-list-get-next-item (line-beginning-position) struct prevs)))
    (unless (or next-item org-list-use-circular-motion)
      (user-error "Cannot move this item further down"))
    (if (not next-item)
	(setq struct (org-list-send-item item 'begin struct))
      (setq struct (org-list-swap-items item next-item struct))
      (goto-char
       (org-list-get-next-item item struct (org-list-prevs-alist struct))))
    (org-list-write-struct struct (org-list-parents-alist struct))
    (org-move-to-column col)))

;;;###autoload
(defun org-move-item-up ()
  "Move the item at point up, i.e. swap with previous item.
Sub-items (items with larger indentation) are considered part of
the item, so this really moves item trees."
  (interactive)
  (unless (org-at-item-p) (error "Not at an item"))
  (let* ((col (current-column))
         (item (line-beginning-position))
	 (struct (org-list-struct))
	 (prevs (org-list-prevs-alist struct))
         (prev-item (org-list-get-prev-item (line-beginning-position) struct prevs)))
    (unless (or prev-item org-list-use-circular-motion)
      (user-error "Cannot move this item further up"))
    (if (not prev-item)
	(setq struct (org-list-send-item item 'end struct))
      (setq struct (org-list-swap-items prev-item item struct)))
    (org-list-write-struct struct (org-list-parents-alist struct))
    (org-move-to-column col)))

(declare-function org-timer-item "org-timer" (&optional arg))
;;;###autoload
(defun org-insert-item (&optional checkbox)
  "Insert a new item at the current level.
If cursor is before first character after bullet of the item, the
new item will be created before the current one.

If CHECKBOX is non-nil, add a checkbox next to the bullet.

Return t when things worked, nil when we are not in an item, or
item is invisible."
  (interactive "P")
  (let ((itemp (org-in-item-p))
	(pos (point)))
    ;; If cursor isn't is a list or if list is invisible, return nil.
    (unless (or (not itemp)
		(save-excursion
		  (goto-char itemp)
		  (org-invisible-p)))
      (if (save-excursion
	    (goto-char itemp)
	    (org-at-item-timer-p))
	  ;; Timer list: delegate to `org-timer-item'.
	  (progn
            (require 'org-timer)
            (org-timer-item)
            t)
	(let* ((struct (save-excursion (goto-char itemp)
				       (org-list-struct)))
	       (prevs (org-list-prevs-alist struct))
	       ;; If we're in a description list, ask for the new term.
	       (desc (when (eq (org-list-get-list-type itemp struct prevs)
			       'descriptive)
		       " :: ")))
	  (setq struct (org-list-insert-item pos struct prevs checkbox desc))
	  (org-list-write-struct struct (org-list-parents-alist struct))
	  (when checkbox (org-update-checkbox-count-maybe))
          (forward-line 0)
	  (looking-at org-list-full-item-re)
	  (goto-char (if (and (match-beginning 4)
			      (save-match-data
				(string-match "[.)]" (match-string 1))))
			 (match-beginning 4)
		       (match-end 0)))
	  (when desc (backward-char 1))
	  t)))))

;;;###autoload
(defun org-cycle-list-bullet (&optional which)
  "Cycle through the different itemize/enumerate bullets.
This cycle the entire list level through the sequence:

   `-'  ->  `+'  ->  `*'  ->  `1.'  ->  `1)'

If WHICH is a valid string, use that as the new bullet.  If WHICH
is an integer, 0 means `-', 1 means `+' etc.  If WHICH is
`previous', cycle backwards."
  (interactive "P")
  (unless (org-at-item-p) (error "Not at an item"))
  (let ((origin (point-marker)))
    (forward-line 0)
    (let* ((struct (org-list-struct))
           (parents (org-list-parents-alist struct))
           (prevs (org-list-prevs-alist struct))
           (list-beg (org-list-get-first-item (point) struct prevs))
           ;; Record relative point position to bullet beginning.
           (origin-offset (- origin
                             (+ (point) (org-list-get-ind (point) struct))))
           ;; Record relative point position to bullet end.
           (origin-offset2 (- origin
                              (+ (point) (org-list-get-ind (point) struct)
                                 (length (org-list-get-bullet (point) struct)))))
           (bullet (org-list-get-bullet list-beg struct))
	   (alpha-p (org-list-use-alpha-bul-p list-beg struct prevs))
	   (case-fold-search nil)
	   (current (cond
		     ((string-match "[a-z]\\." bullet) "a.")
		     ((string-match "[a-z])" bullet) "a)")
		     ((string-match "[A-Z]\\." bullet) "A.")
		     ((string-match "[A-Z])" bullet) "A)")
		     ((string-match "\\." bullet) "1.")
		     ((string-match ")" bullet) "1)")
		     (t (org-trim bullet))))
           ;; Compute list of possible bullets, depending on context.
	   (bullet-list
	    (append '("-" "+" )
		    ;; *-bullets are not allowed at column 0.
		    (unless (looking-at "\\S-") '("*"))
		    ;; Description items cannot be numbered.
		    (unless (or (eq org-plain-list-ordered-item-terminator ?\))
				(org-at-item-description-p))
		      '("1."))
		    (unless (or (eq org-plain-list-ordered-item-terminator ?.)
				(org-at-item-description-p))
		      '("1)"))
		    (unless (or (not alpha-p)
				(eq org-plain-list-ordered-item-terminator ?\))
				(org-at-item-description-p))
		      '("a." "A."))
		    (unless (or (not alpha-p)
				(eq org-plain-list-ordered-item-terminator ?.)
				(org-at-item-description-p))
		      '("a)" "A)"))))
	   (len (length bullet-list))
	   (item-index (- len (length (member current bullet-list))))
	   (get-value (lambda (index) (nth (mod index len) bullet-list)))
	   (new (cond
		 ((member which bullet-list) which)
		 ((numberp which) (funcall get-value which))
		 ((eq 'previous which) (funcall get-value (1- item-index)))
		 (t (funcall get-value (1+ item-index))))))
      ;; Use a short variation of `org-list-write-struct' as there's
      ;; no need to go through all the steps.
      (let ((old-struct (copy-tree struct)))
        (org-list-set-bullet list-beg struct (org-list-bullet-string new))
        (org-list-struct-fix-bul struct prevs)
        (org-list-struct-fix-ind struct parents)
        (org-list-struct-apply-struct struct old-struct))
      (goto-char origin)
      (setq struct (org-list-struct))
      (cond
       ((>= origin-offset2 0)
        (forward-line 0)
        (move-marker origin (+ (point)
                               (org-list-get-ind (point) struct)
                               (length (org-list-get-bullet (point) struct))
                               origin-offset2))
        (goto-char origin))
       ((>= origin-offset 0)
        (forward-line 0)
        (move-marker origin (+ (point)
                               (org-list-get-ind (point) struct)
                               origin-offset))
        (goto-char origin)))
      (move-marker origin nil))))

;;;###autoload
(define-minor-mode org-list-checkbox-radio-mode
  "When turned on, use list checkboxes as radio buttons."
  :lighter " CheckBoxRadio"
  (unless (eq major-mode 'org-mode)
    (user-error "Cannot turn this mode outside org-mode buffers")))

(declare-function org-entry-get "org-property"
                  (epom property &optional inherit literal-nil))
;;;###autoload
(defun org-toggle-radio-button (&optional arg)
  "Toggle off all checkboxes and toggle on the one at point."
  (interactive "P")
  (if (not (org-at-item-p))
      (user-error "Cannot toggle checkbox outside of a list")
    (require 'org-property)
    (let* ((cpos (org-in-item-p))
	   (struct (org-list-struct))
	   (orderedp (org-entry-get nil "ORDERED"))
	   (parents (org-list-parents-alist struct))
	   (old-struct (copy-tree struct))
	   (cbox (org-list-get-checkbox cpos struct))
           (prevs (org-list-prevs-alist struct))
           (start (org-list-get-list-begin (line-beginning-position) struct prevs))
	   (new (unless (and cbox (equal arg '(4)) (equal start cpos))
		  "[ ]")))
      (dolist (pos (org-list-get-all-items
		    start struct (org-list-prevs-alist struct)))
	(org-list-set-checkbox pos struct new))
      (when new
	(org-list-set-checkbox
	 cpos struct
	 (cond ((equal arg '(4)) (unless cbox "[ ]"))
	       ((equal arg '(16)) (unless cbox "[-]"))
	       (t (if (equal cbox "[X]") "[ ]" "[X]")))))
      (org-list-struct-fix-box struct parents prevs orderedp)
      (org-list-struct-apply-struct struct old-struct)
      (org-update-checkbox-count-maybe))))

;;;###autoload
(defun org-toggle-checkbox (&optional toggle-presence)
  "Toggle the checkbox in the current line.

With prefix argument TOGGLE-PRESENCE, add or remove checkboxes.
With a double prefix argument, set the checkbox to \"[-]\".

When there is an active region, toggle status or presence of the
first checkbox there, and make every item inside have the same
status or presence, respectively.

If point is on a headline, apply this to all checkbox items in
the text below the heading, taking as reference the first item in
subtree, ignoring planning line and any drawer following it."
  (interactive "P")
  (if (org-at-radio-list-p)
      (org-toggle-radio-button toggle-presence)
    (save-excursion
      (let* (singlep
	     block-item
	     lim-up
	     lim-down
	     (orderedp (org-entry-get nil "ORDERED"))
	     (_bounds
	      ;; In a region, start at first item in region.
	      (cond
	       ((use-region-p)
		(let ((limit (region-end)))
		  (goto-char (region-beginning))
		  (if (org-list-search-forward (org-item-beginning-re) limit t)
                      (setq lim-up (line-beginning-position))
		    (error "No item in region"))
		  (setq lim-down (copy-marker limit))))
	       ((org-at-heading-p)
		;; On a heading, start at first item after drawers and
		;; timestamps (scheduled, etc.).
		(let ((limit (save-excursion (outline-next-heading) (point))))
		  (org-end-of-meta-data t)
		  (if (org-list-search-forward (org-item-beginning-re) limit t)
                      (setq lim-up (line-beginning-position))
		    (error "No item in subtree"))
		  (setq lim-down (copy-marker limit))))
	       ;; Just one item: set SINGLEP flag.
	       ((org-at-item-p)
		(setq singlep t)
                (setq lim-up (line-beginning-position)
                      lim-down (copy-marker (line-end-position))))
	       (t (error "Not at an item or heading, and no active region"))))
	     ;; Determine the checkbox going to be applied to all items
	     ;; within bounds.
	     (ref-checkbox
	      (progn
		(goto-char lim-up)
		(let ((cbox (and (org-at-item-checkbox-p) (match-string 1))))
		  (cond
		   ((equal toggle-presence '(16)) "[-]")
		   ((equal toggle-presence '(4))
		    (unless cbox "[ ]"))
		   ((equal "[X]" cbox) "[ ]")
		   (t "[X]"))))))
	;; When an item is found within bounds, grab the full list at
	;; point structure, then: (1) set check-box of all its items
	;; within bounds to REF-CHECKBOX, (2) fix check-boxes of the
	;; whole list, (3) move point after the list.
	(goto-char lim-up)
	(while (and (< (point) lim-down)
		    (org-list-search-forward (org-item-beginning-re)
					     lim-down 'move))
	  (let* ((struct (org-list-struct))
		 (struct-copy (copy-tree struct))
		 (parents (org-list-parents-alist struct))
		 (prevs (org-list-prevs-alist struct))
		 (bottom (copy-marker (org-list-get-bottom-point struct)))
		 (items-to-toggle (cl-remove-if
				   (lambda (e) (or (< e lim-up) (> e lim-down)))
				   (mapcar #'car struct))))
	    (dolist (e items-to-toggle)
	      (org-list-set-checkbox
	       e struct
	       ;; If there is no box at item, leave as-is unless
	       ;; function was called with C-u prefix.
	       (let ((cur-box (org-list-get-checkbox e struct)))
		 (if (or cur-box (member toggle-presence '((4) (16))))
		     ref-checkbox
		   cur-box))))
	    (setq block-item (org-list-struct-fix-box
			      struct parents prevs orderedp))
	    ;; Report some problems due to ORDERED status of subtree.
	    ;; If only one box was being checked, throw an error, else,
	    ;; only signal problems.
	    (cond
	     ((and singlep block-item (> lim-up block-item))
	      (error
	       "Checkbox blocked because of unchecked box at line %d"
	       (org-current-line block-item)))
	     (block-item
	      (message
	       "Checkboxes were removed due to unchecked box at line %d"
	       (org-current-line block-item))))
	    (goto-char bottom)
	    (move-marker bottom nil)
	    (org-list-struct-apply-struct struct struct-copy)))
	(move-marker lim-down nil))))
  (org-update-checkbox-count-maybe))

;;;###autoload
(defun org-toggle-checkbox-in-list (&optional toggle-presence)
  "Toggle the checkbox in the current list.
At a plain list, with `\\[universal-argument]' `\\[universal-argument]'
prefix argument TOGGLE-PRESENCE, set checkboxes of each item to
\"[-]\", whereas `\\[universal-argument]' will toggle their presence
according to the state of the first item in the list.  Without an
argument, repair the list."
  (if (or (org-at-radio-list-p)
	  (and (boundp 'org-list-checkbox-radio-mode)
	       org-list-checkbox-radio-mode))
      (org-toggle-radio-button toggle-presence)
    (let* ((context (org-element-lineage (org-element-at-point) 'plain-list t))
           (begin (org-element-contents-begin context))
	   (struct (org-element-property :structure context))
	   (old-struct (copy-tree struct))
	   (first-box (save-excursion
			(goto-char begin)
			(looking-at org-list-full-item-re)
			(match-string-no-properties 3)))
	   (new-box (cond ((equal toggle-presence '(16)) "[-]")
			  ((equal toggle-presence '(4)) (unless first-box "[ ]"))
			  ((equal first-box "[X]") "[ ]")
			  (t "[X]"))))
      (cond
       (toggle-presence
	(dolist (pos
		 (org-list-get-all-items
		  begin struct (org-list-prevs-alist struct)))
	  (org-list-set-checkbox pos struct new-box)))
       ((and first-box (eq (point) begin))
	;; For convenience, when point is at bol on the first
	;; item of the list and no argument is provided, simply
	;; toggle checkbox of that item, if any.
	(org-list-set-checkbox begin struct new-box)))
      (when (equal
	     (org-list-write-struct
	      struct (org-list-parents-alist struct) old-struct)
	     old-struct)
	(message "Cannot update this checkbox"))
      (org-update-checkbox-count-maybe))))

(declare-function org-narrow-to-subtree "org-narrow" (&optional element))
(declare-function org-fold-show-subtree "org-fold" ())
;;;###autoload
(defun org-reset-checkbox-state-subtree ()
  "Reset all checkboxes in an entry subtree."
  (interactive "*")
  (if (org-before-first-heading-p)
      (error "Not inside a tree")
    (save-restriction
      (save-excursion
        (require 'org-narrow)
	(org-narrow-to-subtree)
        (require 'org-fold)
	(org-fold-show-subtree)
	(goto-char (point-min))
	(let ((end (point-max)))
	  (while (< (point) end)
	    (when (org-at-item-checkbox-p)
	      (replace-match "[ ]" t t nil 1))
	    (forward-line 1)))
	(org-update-checkbox-count-maybe 'narrow)))))

(defvar org-last-indent-begin-marker (make-marker))
(defvar org-last-indent-end-marker (make-marker))
(defun org-list-indent-item-generic (arg no-subtree struct)
  "Indent a local list item including its children.
When number ARG is a negative, item will be outdented, otherwise
it will be indented.

If a region is active, all items inside will be moved.

If NO-SUBTREE is non-nil, only indent the item itself, not its
children.

STRUCT is the list structure.

Return t if successful."
  (save-excursion
    (let* ((regionp (use-region-p))
	   (rbeg (and regionp (region-beginning)))
	   (rend (and regionp (region-end)))
	   (top (org-list-get-top-point struct))
	   (parents (org-list-parents-alist struct))
	   (prevs (org-list-prevs-alist struct))
	   ;; Are we going to move the whole list?
	   (specialp
	    (and (not regionp)
                 (= top (line-beginning-position))
		 (cdr (assq 'indent org-list-automatic-rules))
		 (if no-subtree
		     (user-error
		      "At first item: use S-M-<left/right> to move the whole list")
		   t))))
      ;; Determine begin and end points of zone to indent.  If moving
      ;; more than one item, save them for subsequent moves.
      (unless (and (memq last-command '(org-shiftmetaright org-shiftmetaleft))
		   (memq this-command '(org-shiftmetaright org-shiftmetaleft)))
	(if regionp
	    (progn
	      (set-marker org-last-indent-begin-marker rbeg)
	      (set-marker org-last-indent-end-marker rend))
          (set-marker org-last-indent-begin-marker (line-beginning-position))
	  (set-marker org-last-indent-end-marker
		      (cond
		       (specialp (org-list-get-bottom-point struct))
                       (no-subtree (1+ (line-beginning-position)))
                       (t (org-list-get-item-end (line-beginning-position) struct))))))
      (let* ((beg (marker-position org-last-indent-begin-marker))
	     (end (marker-position org-last-indent-end-marker))
             (deactivate-mark nil))
	(cond
	 ;; Special case: moving top-item with indent rule.
	 (specialp
	  (let* ((level-skip (org-level-increment))
		 (offset (if (< arg 0) (- level-skip) level-skip))
		 (top-ind (org-list-get-ind beg struct))
		 (old-struct (copy-tree struct)))
	    (if (< (+ top-ind offset) 0)
		(error "Cannot outdent beyond margin")
	      ;; Change bullet if necessary.
	      (when (and (= (+ top-ind offset) 0)
			 (string-match "\\*"
				       (org-list-get-bullet beg struct)))
		(org-list-set-bullet beg struct
				     (org-list-bullet-string "-")))
	      ;; Shift every item by OFFSET and fix bullets.  Then
	      ;; apply changes to buffer.
	      (pcase-dolist (`(,pos . ,_) struct)
		(let ((ind (org-list-get-ind pos struct)))
		  (org-list-set-ind pos struct (+ ind offset))))
	      (org-list-struct-fix-bul struct prevs)
	      (org-list-struct-apply-struct struct old-struct))))
	 ;; Forbidden move:
	 ((and (< arg 0)
	       ;; If only one item is moved, it mustn't have a child.
	       (or (and no-subtree
			(not regionp)
			(org-list-has-child-p beg struct))
		   ;; If a subtree or region is moved, the last item
		   ;; of the subtree mustn't have a child.
		   (let ((last-item (caar
				     (reverse
				      (cl-remove-if
				       (lambda (e) (>= (car e) end))
				       struct)))))
		     (org-list-has-child-p last-item struct))))
	  (error "Cannot outdent an item without its children"))
	 ;; Normal shifting
	 (t
	  (let* ((old-struct (copy-tree struct))
		 (new-parents
		  (if (< arg 0)
		      (org-list-struct-outdent beg end struct parents)
		    (org-list-struct-indent beg end struct parents prevs))))
	    (org-list-write-struct struct new-parents old-struct))
	  (org-update-checkbox-count-maybe))))))
  t)

;;;###autoload
(defun org-outdent-item ()
  "Outdent a local list item, but not its children.
If a region is active, all items inside will be moved."
  (interactive)
  (let ((regionp (use-region-p)))
    (cond
     ((or (org-at-item-p)
	  (and regionp
	       (save-excursion (goto-char (region-beginning))
			       (org-at-item-p))))
      (let ((struct (if (not regionp) (org-list-struct)
		      (save-excursion (goto-char (region-beginning))
				      (org-list-struct)))))
	(org-list-indent-item-generic -1 t struct)))
     (regionp (error "Region not starting at an item"))
     (t (error "Not at an item")))))

;;;###autoload
(defun org-indent-item ()
  "Indent a local list item, but not its children.
If a region is active, all items inside will be moved."
  (interactive)
  (let ((regionp (use-region-p)))
    (cond
     ((or (org-at-item-p)
	  (and regionp
	       (save-excursion (goto-char (region-beginning))
			       (org-at-item-p))))
      (let ((struct (if (not regionp) (org-list-struct)
		      (save-excursion (goto-char (region-beginning))
				      (org-list-struct)))))
	(org-list-indent-item-generic 1 t struct)))
     (regionp (error "Region not starting at an item"))
     (t (error "Not at an item")))))

;;;###autoload
(defun org-outdent-item-tree ()
  "Outdent a local list item including its children.
If a region is active, all items inside will be moved."
  (interactive)
  (let ((regionp (use-region-p)))
    (cond
     ((or (org-at-item-p)
	  (and regionp
	       (save-excursion (goto-char (region-beginning))
			       (org-at-item-p))))
      (let ((struct (if (not regionp) (org-list-struct)
		      (save-excursion (goto-char (region-beginning))
				      (org-list-struct)))))
	(org-list-indent-item-generic -1 nil struct)))
     (regionp (error "Region not starting at an item"))
     (t (error "Not at an item")))))

;;;###autoload
(defun org-indent-item-tree ()
  "Indent a local list item including its children.
If a region is active, all items inside will be moved."
  (interactive)
  (let ((regionp (use-region-p)))
    (cond
     ((or (org-at-item-p)
	  (and regionp
	       (save-excursion (goto-char (region-beginning))
			       (org-at-item-p))))
      (let ((struct (if (not regionp) (org-list-struct)
		      (save-excursion (goto-char (region-beginning))
				      (org-list-struct)))))
	(org-list-indent-item-generic 1 nil struct)))
     (regionp (error "Region not starting at an item"))
     (t (error "Not at an item")))))

(defvar org-tab-ind-state)
;;;###autoload
(defun org-cycle-item-indentation ()
  "Cycle levels of indentation of an empty item.

The first run indents the item, if applicable.  Subsequent runs
outdent it at meaningful levels in the list.  When done, item is
put back at its original position with its original bullet.

Return t at each successful move."
  (when (org-at-item-p)
    (let* ((struct (org-list-struct))
	   (item (line-beginning-position))
	   (ind (org-list-get-ind item struct)))
      ;; Accept empty items or if cycle has already started.
      (when (or (eq last-command 'org-cycle-item-indentation)
		(and (org-match-line org-list-full-item-re)
		     (>= (match-end 0)
			(save-excursion
			  (goto-char (org-list-get-item-end item struct))
                          (org-skip-whitespace 'back)
			  (point)))))
	(setq this-command 'org-cycle-item-indentation)
	(let ((prevs (org-list-prevs-alist struct))
	      (parents (org-list-parents-alist struct)))
	  (if (eq last-command 'org-cycle-item-indentation)
	      ;; When in the middle of the cycle, try to outdent.  If
	      ;; it fails, move point back to its initial position and
	      ;; reset cycle.
	      (pcase-let ((`(,old-ind . ,old-bul) org-tab-ind-state)
			  (allow-outdent
			   (lambda (struct prevs parents)
			     ;; Non-nil if current item can be
			     ;; outdented.
			     (and (not (org-list-get-next-item item nil prevs))
				  (not (org-list-has-child-p item struct))
				  (org-list-get-parent item struct parents)))))
		(cond
		 ((and (> ind old-ind)
		       (org-list-get-prev-item item nil prevs))
		  (org-list-indent-item-generic 1 t struct))
		 ((and (< ind old-ind)
		       (funcall allow-outdent struct prevs parents))
		  (org-list-indent-item-generic -1 t struct))
		 (t
		  (delete-region (line-beginning-position) (line-end-position))
		  (indent-to-column old-ind)
		  (insert old-bul " ")
		  (let* ((struct (org-list-struct))
			 (parents (org-list-parents-alist struct)))
		    (if (and (> ind old-ind)
			     ;; We were previously indenting item.  It
			     ;; is no longer possible.  Try to outdent
			     ;; from initial position.
			     (funcall allow-outdent
				      struct
				      (org-list-prevs-alist struct)
				      parents))
			(org-list-indent-item-generic -1 t struct)
		      (org-list-write-struct struct parents)
		      ;; Start cycle over.
		      (setq this-command 'identity)
		      t)))))
	    ;; If a cycle is starting, remember initial indentation
	    ;; and bullet, then try to indent.  If it fails, try to
	    ;; outdent.
	    (setq org-tab-ind-state
		  (cons ind (org-trim (org-current-line-string))))
	    (cond
	     ((org-list-get-prev-item item nil prevs)
	      (org-list-indent-item-generic 1 t struct))
	     ((and (not (org-list-get-next-item item nil prevs))
		   (org-list-get-parent item struct parents))
	      (org-list-indent-item-generic -1 t struct))
	     (t
	      ;; This command failed.  So will the following one.
	      ;; There's no point in starting the cycle.
	      (setq this-command 'identity)
	      (user-error "Cannot move item")))))))))

(declare-function org-timer-hms-to-secs "org-timer" (hms))
(declare-function org-time-string-to-seconds "org-time" (s))
;;;###autoload
(defun org-sort-list
    (&optional with-case sorting-type getkey-func compare-func interactive?)
  "Sort list items.
The cursor may be at any item of the list that should be sorted.
Sublists are not sorted.  Checkboxes, if any, are ignored.

Sorting can be alphabetically, numerically, by date/time as given
by a time stamp, by a property or by priority.

Comparing entries ignores case by default.  However, with an
optional argument WITH-CASE, the sorting considers case as well,
if the current locale allows for it.

The command prompts for the sorting type unless it has been given
to the function through the SORTING-TYPE argument, which needs to
be a character, among ?n ?N ?a ?A ?t ?T ?f ?F ?x or ?X.  Here is
the detailed meaning of each character:

n   Numerically, by converting the beginning of the item to a number.
a   Alphabetically.  Only the first line of item is checked.
t   By date/time, either the first active time stamp in the entry, if
    any, or by the first inactive one.  In a timer list, sort the timers.
x   By \"checked\" status of a check list.

Capital letters will reverse the sort order.

If the SORTING-TYPE is ?f or ?F, then GETKEY-FUNC specifies
a function to be called with point at the beginning of the
record.  It must return a value that is compatible with COMPARE-FUNC,
the function used to compare entries.

Sorting is done against the visible part of the headlines, it
ignores hidden links.

A non-nil value for INTERACTIVE? is used to signal that this
function is being called interactively."
  (interactive (list current-prefix-arg nil nil nil t))
  (let* ((case-func (if with-case 'identity 'downcase))
         (struct (org-list-struct))
         (prevs (org-list-prevs-alist struct))
         (start (org-list-get-list-begin (line-beginning-position) struct prevs))
         (end (org-list-get-list-end (line-beginning-position) struct prevs))
	 (sorting-type
	  (or sorting-type
	      (progn
		(message
		 "Sort plain list: [a]lpha  [n]umeric  [t]ime  [f]unc  [x]checked  A/N/T/F/X means reversed:")
		(read-char-exclusive))))
	 (dcst (downcase sorting-type))
	 (getkey-func
	  (and (= dcst ?f)
	       (or getkey-func
		   (and interactive?
			(org-read-function "Function for extracting keys: "))
		   (error "Missing key extractor"))))
	 (sort-func
	  (cond
	   ((= dcst ?a) #'org-string<)
	   ((= dcst ?f)
	    (or compare-func
		(and interactive?
		     (org-read-function
		      (concat "Function for comparing keys "
			      "(empty for default `sort-subr' predicate): ")
		      'allow-empty))))
	   ((= dcst ?t) #'<)
	   ((= dcst ?x) #'string<))))
    (message "Sorting items...")
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let* ((case-fold-search nil)
	     (now (current-time))
	     (next-record (lambda ()
			    (org-skip-whitespace)
			    (or (eobp) (forward-line 0))))
	     (end-record (lambda ()
			   (goto-char (org-list-get-item-end-before-blank
				       (point) struct))))
	     (value-to-sort
	      (lambda ()
		(when (looking-at "[ \t]*[-+*0-9.)]+\\([ \t]+\\[[- X]\\]\\)?[ \t]+")
		  (cond
		   ((= dcst ?n)
		    (string-to-number
		     (org-sort-remove-invisible
                      (buffer-substring (match-end 0) (line-end-position)))))
		   ((= dcst ?a)
		    (funcall case-func
			     (org-sort-remove-invisible
			      (buffer-substring
                               (match-end 0) (line-end-position)))))
		   ((= dcst ?t)
		    (cond
		     ;; If it is a timer list, convert timer to seconds
		     ((org-at-item-timer-p)
                      (require 'org-timer)
		      (org-timer-hms-to-secs (match-string 1)))
		     ((or (save-excursion
                            (re-search-forward org-ts-regexp (line-end-position) t))
			  (save-excursion (re-search-forward org-ts-regexp-both
                                                             (line-end-position) t)))
                      (require 'org-time)
		      (org-time-string-to-seconds (match-string 0)))
		     (t (float-time now))))
		   ((= dcst ?x) (or (and (stringp (match-string 1))
					 (match-string 1))
				    ""))
		   ((= dcst ?f)
		    (if getkey-func
			(let ((value (funcall getkey-func)))
			  (if (stringp value)
			      (funcall case-func value)
			    value))
		      (error "Invalid key function `%s'" getkey-func)))
		   (t (error "Invalid sorting type `%c'" sorting-type)))))))
	(sort-subr (/= dcst sorting-type)
		   next-record
		   end-record
		   value-to-sort
		   nil
		   sort-func)
	;; Read and fix list again, as `sort-subr' probably destroyed
	;; its structure.
	(org-list-repair)
	(run-hooks 'org-after-sorting-entries-or-items-hook)
	(message "Sorting items...done")))))

(declare-function org-set-tags "org-tags" (tags))
(defun org-list--delete-metadata ()
  "Delete metadata from the heading at point.
Metadata are tags, planning information and properties drawers."
  (save-match-data
    (org-with-wide-buffer
     (require 'org-tags)
     (org-set-tags nil)
     (delete-region (line-beginning-position 2)
		    (save-excursion
		      (org-end-of-meta-data)
		      (org-skip-whitespace)
		      (if (eobp) (point) (line-beginning-position)))))))

(defun org-list--to-plain (&optional limit)
  "De-itemize all the lists from point to LIMIT or `point-max'."
  (setq limit (or limit (point-max)))
  (while (< (point) limit)
    (when (org-at-item-p)
      (skip-chars-forward " \t")
      (delete-region (point) (match-end 0)))
    (forward-line)))

(defun org-list--extract-footnote-definitions (&optional limit)
  "Extract footnote definitions between point and LIMIT or `point-max'.
Return the list of strings - the extracted definitions."
  (setq limit (or limit (point-max)))
  (let (definitions element)
    (save-excursion
      (while (re-search-forward org-footnote-definition-re limit t)
        (setq element (org-element-at-point))
        (when (org-element-type-p element 'footnote-definition)
          (push (buffer-substring-no-properties
                 (org-element-begin element)
                 (org-element-end element))
                definitions)
          ;; Ensure at least 2 blank lines after the last
          ;; footnote definition, thus not slurping the
          ;; following element.
          (unless (<= 2 (org-element-post-blank
                        (org-element-at-point)))
            (setf (car definitions)
                  (concat (car definitions)
                          (make-string
                           (- 2 (org-element-post-blank
                                 (org-element-at-point)))
                           ?\n))))
          (delete-region
           (org-element-begin element)
           (org-element-end element))))
      definitions)))

;; FIXME: This probably belongs to org-indent-static.el
(defun org--indent-to (ind &optional limit)
  "Indent text from point to LIMIT or `point-max' to IND.
Leave point at LIMIT."
  (setq limit (or limit (point-max)))
  (let ((min-i 1000) (limit (copy-marker limit)))
    ;; First determine the minimum indentation (MIN-I) of
    ;; the text.
    (save-excursion
      (catch 'exit
	(while (< (point) limit)
	  (let ((i (org-current-text-indentation)))
	    (cond
	     ;; Skip blank lines and inline tasks.
	     ((looking-at "^[ \t]*$"))
	     ((looking-at org-outline-regexp-bol))
	     ;; We can't find less than 0 indentation.
	     ((zerop i) (throw 'exit (setq min-i 0)))
	     ((< i min-i) (setq min-i i))))
	  (forward-line))))
    ;; Then indent each line so that a line indented to
    ;; MIN-I becomes indented to IND.  Ignore blank lines
    ;; and inline tasks in the process.
    (let ((delta (- ind min-i)))
      (while (< (point) limit)
	(unless (or (looking-at "^[ \t]*$")
		    (looking-at org-outline-regexp-bol))
	  (indent-line-to (+ (org-current-text-indentation) delta)))
	(forward-line)))))

(defun org--heading-to-list (&optional limit)
  "Convert headings from point to LIMIT or `point-max' to a list."
  (setq limit (or limit (point-max)))
  ;; Remove metadata
  (let (org-loop-over-headlines-in-active-region)
    (org-list--delete-metadata))
  (require 'org-indent-static)
  (defvar org-adapt-indentation)
  (let* ((bul (org-list-bullet-string "-"))
	 (bul-len (length bul))
	 ;; Indentation of the first heading.  It should be
	 ;; relative to the indentation of its parent, if any.
	 (start-ind (save-excursion
		      (cond
		       ((not org-adapt-indentation) 0)
		       ((not (outline-previous-heading)) 0)
		       (t (length (match-string 0))))))
	 ;; Level of first heading.  Further headings will be
	 ;; compared to it to determine hierarchy in the list.
	 (ref-level (org-reduced-level (org-outline-level)))
         (footnote-definitions
          (org-list--extract-footnote-definitions limit)))
    (require 'org-property)
    (while (< (point) limit)
      (let* ((level (org-reduced-level (org-outline-level)))
	     (delta (max 0 (- level ref-level)))
	     (todo-state (org-get-todo-state)))
	;; If current headline is less indented than the first
	;; one, set it as reference, in order to preserve
	;; subtrees.
	(when (< level ref-level) (setq ref-level level))
	;; Remove metadata
	(let (org-loop-over-headlines-in-active-region)
	  (org-list--delete-metadata))
	;; Remove stars and TODO keyword.
	(let ((case-fold-search nil)) (looking-at org-todo-line-regexp))
	(delete-region (point) (or (match-beginning 3)
				   (line-end-position)))
	(insert bul)
	(indent-line-to (+ start-ind (* delta bul-len)))
	;; Turn TODO keyword into a check box.
	(when todo-state
	  (let* ((struct (org-list-struct))
		 (old (copy-tree struct)))
	    (org-list-set-checkbox
	     (line-beginning-position)
	     struct
	     (if (member todo-state org-done-keywords)
		 "[X]"
	       "[ ]"))
	    (org-list-write-struct struct
				   (org-list-parents-alist struct)
				   old)))
	;; Ensure all text down to END (or SECTION-END) belongs
	;; to the newly created item.
	(let ((section-end (save-excursion
			     (or (outline-next-heading) (point)))))
	  (forward-line)
	  (org--indent-to
	   (+ start-ind (* (1+ delta) bul-len))
	   (min limit section-end)))))
    (when footnote-definitions
      (goto-char limit)
      ;; Insert footnote definitions after the list.
      (unless (bolp) (forward-line 1))
      ;; At (point-max).
      (unless (bolp) (insert "\n"))
      (dolist (def footnote-definitions)
        (insert def)))))

(declare-function org-list-to-subtree "org-list-export" (list &optional start-level params))
(declare-function org-list-to-lisp "org-list-export" (&optional delete))
(defun org--list-to-heading (&optional limit)
  "Convert lists from point to LIMIT or `point-max' to headings.
Return non-nil when at least one list is converted."
  (setq limit (or limit (point-max)))
  (require 'org-list-export)
  (let ((toggled nil))
    (while (< (point) limit)
      (when (org-at-item-p)
        ;; Pay attention to cases when region ends before list.
        (let* ((struct (org-list-struct))
	       (list-end
	        (min (org-list-get-bottom-point struct) (1+ limit))))
	  (save-restriction
	    (narrow-to-region (point) list-end)
	    (insert (org-list-to-subtree
		     (org-list-to-lisp t)
		     (pcase (org-current-level)
		       (`nil 1)
		       (l (1+ (org-reduced-level l))))
                     ;; Keywords to replace checkboxes.
                     (list
                      ;; [X]
                      :cbon (concat (or (car org-done-keywords) "DONE") " ")
                      ;; [ ]
                      :cboff (concat (or (car org-not-done-keywords) "TODO") " ")
                      ;; [-]
                      :cbtrans (concat (or (car org-not-done-keywords) "TODO") " ")))
		    "\n")))
        (setq toggled t))
      (forward-line))
    toggled))

(defun org--line-to-list (&optional limit)
  "Convert line at point to item and the rest to item body.
All the text starting from next line up to LIMIT or `point-max'
becomes the item body."
  (setq limit (or limit (point-max)))
  (let* ((bul (org-list-bullet-string "-"))
	 (bul-len (length bul))
	 (ref-ind (org-current-text-indentation))
         (footnote-definitions
          (org-list--extract-footnote-definitions limit)))
    (skip-chars-forward " \t")
    (insert bul)
    (forward-line)
    (while (< (point) limit)
      ;; Ensure that lines less indented than first one
      ;; still get included in item body.
      (org--indent-to
       (+ ref-ind bul-len)
       (min limit (save-excursion (or (outline-next-heading)
				      (point)))))
      (forward-line))
    (when footnote-definitions
      ;; If the new list is followed by same-level items,
      ;; move past them as well.
      (goto-char (org-element-end
                  (org-element-lineage
                   (org-element-at-point (1- limit))
                   'plain-list t)))
      ;; Insert footnote definitions after the list.
      (unless (bolp) (forward-line 1))
      ;; At (point-max).
      (unless (bolp) (insert "\n"))
      (dolist (def footnote-definitions)
        (insert def)))))

(defun org--lines-to-list (&optional limit)
  "Itemize all the lines from point to LIMIT or `point-max'."
  (setq limit (or limit (point-max)))
  (while (< (point) limit)
    (unless (or (org-at-heading-p) (org-at-item-p))
      (when (looking-at "\\([ \t]*\\)\\(\\S-\\)")
	(replace-match
	 (concat "\\1" (org-list-bullet-string "-") "\\2"))))
    (forward-line)))

(declare-function org-get-todo-state "org-property" ())
;;;###autoload
(defun org-toggle-item (arg)
  "Convert headings or normal lines to items, items to normal lines.
If there is no active region, only the current line is considered.

If the first non blank line in the region is a headline, convert
all headlines to items, shifting text accordingly.

If it is an item, convert all items to normal lines.

If it is normal text, change region into a list of items.
With a prefix argument ARG, change the region in a single item."
  (interactive "P")
  (let (beg end)
    ;; Determine boundaries of changes.
    (if (use-region-p)
	(setq beg (save-excursion
                    (org-skip-blanks-and-comments (region-beginning)))
	      end (copy-marker (region-end)))
      (setq beg (line-beginning-position)
            end (copy-marker (line-end-position))))
    ;; Depending on the starting line, choose an action on the text
    ;; between BEG and END.
    (org-with-limited-levels
     (save-excursion
       (goto-char beg)
       (cond
	;; Case 1. Start at an item: de-itemize.  Note that it only
	;;         happens when a region is active: `org-ctrl-c-minus'
	;;         would call `org-cycle-list-bullet' otherwise.
	((org-at-item-p) (org-list--to-plain end))
	;; Case 2. Start at a heading: convert to items.
	((org-at-heading-p) (org--heading-to-list end))
	;; Case 3. Normal line with ARG: make the first line of region
	;;         an item, and shift indentation of others lines to
	;;         set them as item's body.
	(arg (org--line-to-list end))
	;; Case 4. Normal line without ARG: turn each non-item line
	;;         into an item.
	(t (org--lines-to-list end)))))))

(provide 'org-list-commands)

;;; org-list-commands.el ends here
