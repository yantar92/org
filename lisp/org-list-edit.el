;;; org-list-edit.el --- Extra functions for editing plain lists              -*- lexical-binding: t; -*-
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

;; This file contains additional API to edit Org lists.


;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element)
(require 'org-list-core)
(require 'org-edit-structure-common)

(defcustom org-list-demote-modify-bullet nil
  "Default bullet type installed when demoting an item.
This is an association list, for each bullet type, this alist will point
to the bullet that should be used when this item is demoted.
For example,

 (setq org-list-demote-modify-bullet
       \\='((\"+\" . \"-\") (\"-\" . \"+\") (\"*\" . \"+\")))

will make

  + Movies
    + Silence of the Lambs
    + My Cousin Vinny
  + Books
    + The Hunt for Red October
    + The Road to Omaha

into

  + Movies
    - Silence of the Lambs
    - My Cousin Vinny
  + Books
    - The Hunt for Red October
    - The Road to Omaha"
  :group 'org-plain-lists
  :type '(repeat
	  (cons
	   (choice :tag "If the current bullet is  "
		   (const "-")
		   (const "+")
		   (const "*")
		   (const "1.")
		   (const "1)"))
	   (choice :tag "demotion will change it to"
		   (const "-")
		   (const "+")
		   (const "*")
		   (const "1.")
		   (const "1)")))))

(defun org-list-swap-items (beg-A beg-B struct)
  "Swap item starting at BEG-A with item starting at BEG-B in STRUCT.

Blank lines at the end of items are left in place.  Item
visibility is preserved.  Return the new structure after the
changes.

Assume BEG-A is lesser than BEG-B and that BEG-A and BEG-B belong
to the same sub-list.

This function modifies STRUCT."
  (save-excursion
    (org-fold-core-ignore-modifications
      (let* ((end-A-no-blank (org-list-get-item-end-before-blank beg-A struct))
	     (end-B-no-blank (org-list-get-item-end-before-blank beg-B struct))
	     (end-A (org-list-get-item-end beg-A struct))
	     (end-B (org-list-get-item-end beg-B struct))
	     (size-A (- end-A-no-blank beg-A))
	     (size-B (- end-B-no-blank beg-B))
	     (body-A (buffer-substring beg-A end-A-no-blank))
	     (body-B (buffer-substring beg-B end-B-no-blank))
	     (between-A-no-blank-and-B (buffer-substring end-A-no-blank beg-B))
	     (sub-A (cons beg-A (org-list-get-subtree beg-A struct)))
	     (sub-B (cons beg-B (org-list-get-subtree beg-B struct)))
	     ;; Store inner folds responsible for visibility status.
	     (folds
	      (cons
               (org-fold-core-get-regions :from beg-A :to end-A :relative t)
               (org-fold-core-get-regions :from beg-B :to end-B :relative t))))
        ;; Clear up the folds.
        (org-fold-core-region beg-A end-B-no-blank nil)
        ;; 1. Move effectively items in buffer.
        (goto-char beg-A)
        (delete-region beg-A end-B-no-blank)
        (insert (concat body-B between-A-no-blank-and-B body-A))
        ;; Restore visibility status.
        (org-fold-core-regions (cdr folds) :relative beg-A)
        (org-fold-core-regions
         (car folds)
         :relative (+ beg-A size-B (length between-A-no-blank-and-B)))
        ;; 2. Now modify struct.  No need to re-read the list, the
        ;;    transformation is just a shift of positions.  Some special
        ;;    attention is required for items ending at END-A and END-B
        ;;    as empty spaces are not moved there.  In others words,
        ;;    item BEG-A will end with whitespaces that were at the end
        ;;    of BEG-B and the same applies to BEG-B.
        (dolist (e struct)
	  (let ((pos (car e)))
	    (cond
	     ((< pos beg-A))
	     ((memq pos sub-A)
	      (let ((end-e (nth 6 e)))
	        (setcar e (+ pos (- end-B-no-blank end-A-no-blank)))
	        (setcar (nthcdr 6 e)
		        (+ end-e (- end-B-no-blank end-A-no-blank)))
	        (when (= end-e end-A) (setcar (nthcdr 6 e) end-B))))
	     ((memq pos sub-B)
	      (let ((end-e (nth 6 e)))
	        (setcar e (- (+ pos beg-A) beg-B))
	        (setcar (nthcdr 6 e) (+ end-e (- beg-A beg-B)))
	        (when (= end-e end-B)
		  (setcar (nthcdr 6 e)
			  (+ beg-A size-B (- end-A end-A-no-blank))))))
	     ((< pos beg-B)
	      (let ((end-e (nth 6 e)))
	        (setcar e (+ pos (- size-B size-A)))
	        (setcar (nthcdr 6 e) (+ end-e (- size-B size-A))))))))
        (setq struct (sort struct #'car-less-than-car))
        ;; Return structure.
        struct))))

(defun org-list-separating-blank-lines-number (pos struct prevs)
  "Return number of blank lines that should separate items in list.

POS is the position of point where `org-list-insert-item' was called.

STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'.

Assume point is at item's beginning.  If the item is alone, apply
some heuristics to guess the result."
  (save-excursion
    (let ((item (point))
	  (insert-blank-p
	   (cdr (assq 'plain-list-item org-blank-before-new-entry)))
	  usr-blank
	  (count-blanks
	   (lambda ()
	     ;; Count blank lines above beginning of line.
	     (save-excursion
               (count-lines (goto-char (line-beginning-position))
			    (progn (skip-chars-backward " \r\t\n")
				   (forward-line)
				   (point)))))))
      (cond
       ;; Trivial cases where there should be none.
       ((not insert-blank-p) 0)
       ;; When `org-blank-before-new-entry' says so, it is 1.
       ((eq insert-blank-p t) 1)
       ;; `plain-list-item' is 'auto.  Count blank lines separating
       ;; neighbors' items in list.
       (t (let ((next-p (org-list-get-next-item item struct prevs)))
	    (cond
	     ;; Is there a next item?
	     (next-p (goto-char next-p)
		     (funcall count-blanks))
	     ;; Is there a previous item?
	     ((org-list-get-prev-item item struct prevs)
	      (funcall count-blanks))
	     ;; User inserted blank lines, trust him.
	     ((and (> pos (org-list-get-item-end-before-blank item struct))
		   (> (save-excursion (goto-char pos)
				      (setq usr-blank (funcall count-blanks)))
		      0))
	      usr-blank)
	     ;; Are there blank lines inside the list so far?
	     ((save-excursion
		(goto-char (org-list-get-top-point struct))
		;; Do not use `org-list-search-forward' so blank lines
		;; in blocks can be counted in.
		(re-search-forward
		 "^[ \t]*$" (org-list-get-item-end-before-blank item struct) t))
	      1)
	     ;; Default choice: no blank line.
	     (t 0))))))))

(defun org-list-insert-item (pos struct prevs &optional checkbox after-bullet)
  "Insert a new list item at POS and return the new structure.
If POS is before first character after bullet of the item, the
new item will be created before the current one.

STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'.

Insert a checkbox if CHECKBOX is non-nil, and string AFTER-BULLET
after the bullet.  Cursor will be after this text once the
function ends.

This function modifies STRUCT."
  (let* ((case-fold-search t)
	 ;; Get information about list: ITEM containing POS, position
	 ;; of point with regards to item start (BEFOREP), blank lines
	 ;; number separating items (BLANK-NB), if we're allowed to
	 ;; (SPLIT-LINE-P).
	 (item
	  (catch :exit
	    (let ((i nil))
	      (pcase-dolist (`(,start ,_ ,_ ,_ ,_ ,_ ,end) struct)
		(cond
		 ((> start pos) (throw :exit i))
		 ((< end pos) nil)	;skip sub-lists before point
		 (t (setq i start))))
	      ;; If no suitable item is found, insert a sibling of the
	      ;; last item in buffer.
	      (or i (caar (reverse struct))))))
	 (item-end (org-list-get-item-end item struct))
	 (item-end-no-blank (org-list-get-item-end-before-blank item struct))
	 (beforep
	  (progn
	    (goto-char item)
	    (looking-at org-list-full-item-re)
	    (<= pos
	        (cond
		 ((not (match-beginning 4)) (match-end 0))
		 ;; Ignore tag in a non-descriptive list.
		 ((save-match-data (string-match "[.)]" (match-string 1)))
		  (match-beginning 4))
		 (t (save-excursion
		      (goto-char (match-end 4))
		      (skip-chars-forward " \t")
		      (point)))))))
	 (split-line-p (org-get-alist-option org-M-RET-may-split-line 'item))
	 (blank-nb (org-list-separating-blank-lines-number pos struct prevs))
	 ;; Build the new item to be created.  Concatenate same bullet
	 ;; as item, checkbox, text AFTER-BULLET if provided, and text
	 ;; cut from point to end of item (TEXT-CUT) to form item's
	 ;; BODY.  TEXT-CUT depends on BEFOREP and SPLIT-LINE-P.  The
	 ;; difference of size between what was cut and what was
	 ;; inserted in buffer is stored in SIZE-OFFSET.
	 (ind (org-list-get-ind item struct))
	 (ind-size (if indent-tabs-mode
		       (+ (/ ind tab-width) (mod ind tab-width))
		     ind))
	 (bullet (org-list-bullet-string (org-list-get-bullet item struct)))
	 (box (and checkbox "[ ]"))
	 (text-cut
	  (and (not beforep)
	       split-line-p
	       (progn
		 (goto-char pos)
		 ;; If POS is greater than ITEM-END, then point is in
		 ;; some white lines after the end of the list.  Those
		 ;; must be removed, or they will be left, stacking up
		 ;; after the list.
		 (when (< item-end pos)
                   (delete-region (1- item-end) (line-end-position)))
		 (skip-chars-backward " \r\t\n")
		 ;; Cut position is after any blank on the line.
		 (save-excursion
		   (skip-chars-forward " \t")
		   (setq pos (point)))
		 (delete-and-extract-region (point) item-end-no-blank))))
	 (body
	  (concat bullet
		  (and box (concat box " "))
		  after-bullet
		  (and text-cut
		       (if (string-match "\\`[ \t]+" text-cut)
			   (replace-match "" t t text-cut)
			 text-cut))))
	 (item-sep (make-string  (1+ blank-nb) ?\n))
	 (item-size (+ ind-size (length body) (length item-sep)))
	 (size-offset (- item-size (length text-cut))))
    ;; Insert effectively item into buffer.
    (goto-char item)
    (indent-to-column ind)
    (insert body item-sep)
    ;; Add new item to STRUCT.
    (dolist (e struct)
      (let ((p (car e)) (end (nth 6 e)))
	(cond
	 ;; Before inserted item, positions don't change but an item
	 ;; ending after insertion has its end shifted by SIZE-OFFSET.
	 ((< p item)
	  (when (> end item)
	    (setcar (nthcdr 6 e) (+ end size-offset))))
	 ;; Item where insertion happens may be split in two parts.
	 ;; In this case, move start by ITEM-SIZE and end by
	 ;; SIZE-OFFSET.
	 ((and (= p item) (not beforep) split-line-p)
	  (setcar e (+ p item-size))
	  (setcar (nthcdr 6 e) (+ end size-offset)))
	 ;; Items starting after modified item fall into two
	 ;; categories.
	 ;;
	 ;; If modified item was split, and current sub-item was
	 ;; located after split point, it was moved to the new item:
	 ;; the part between body start and split point (POS) was
	 ;; removed.  So we compute the length of that part and shift
	 ;; item's positions accordingly.
	 ;;
	 ;; Otherwise, the item was simply shifted by SIZE-OFFSET.
	 ((and split-line-p (not beforep) (>= p pos) (<= p item-end-no-blank))
	  (let ((offset (- pos item ind (length bullet) (length after-bullet))))
	    (setcar e (- p offset))
	    (setcar (nthcdr 6 e) (- end offset))))
	 (t
	  (setcar e (+ p size-offset))
	  (setcar (nthcdr 6 e) (+ end size-offset))))))
    (push (list item ind bullet nil box nil (+ item item-size)) struct)
    (setq struct (sort struct #'car-less-than-car))
    ;; If not BEFOREP, new item must appear after ITEM, so exchange
    ;; ITEM with the next item in list.  Position cursor after bullet,
    ;; counter, checkbox, and label.
    (if beforep
	(goto-char item)
      (setq struct (org-list-swap-items item (+ item item-size) struct))
      (goto-char (org-list-get-next-item
		  item struct (org-list-prevs-alist struct))))
    struct))

(defun org-list-delete-item (item struct)
  "Remove ITEM from the list and return the new structure.

STRUCT is the list structure."
  (let* ((end (org-list-get-item-end item struct))
	 (beg (if (= (org-list-get-bottom-point struct) end)
		  ;; If ITEM ends with the list, delete blank lines
		  ;; before it.
		  (save-excursion
		    (goto-char item)
		    (skip-chars-backward " \r\t\n")
                    (min (1+ (line-end-position)) (point-max)))
		item)))
    ;; Remove item from buffer.
    (delete-region beg end)
    ;; Remove item from structure and shift others items accordingly.
    ;; Don't forget to shift also ending position when appropriate.
    (let ((size (- end beg)))
      (delq nil (mapcar (lambda (e)
			  (let ((pos (car e)))
			    (cond
			     ((< pos item)
			      (let ((end-e (nth 6 e)))
			        (cond
			         ((< end-e item) e)
			         ((= end-e item)
				  (append (butlast e) (list beg)))
			         (t
				  (append (butlast e) (list (- end-e size)))))))
			     ((< pos end) nil)
			     (t
			      (cons (- pos size)
				    (append (butlast (cdr e))
					    (list (- (nth 6 e) size))))))))
		        struct)))))

(defun org-list-send-item (item dest struct)
  "Send ITEM to destination DEST.

STRUCT is the list structure.

DEST can have various values.

If DEST is a buffer position, the function will assume it points
to another item in the same list as ITEM, and will move the
latter just before the former.

If DEST is `begin' (respectively `end'), ITEM will be moved at
the beginning (respectively end) of the list it belongs to.

If DEST is a string like \"N\", where N is an integer, ITEM will
be moved at the Nth position in the list.

If DEST is `kill', ITEM will be deleted and its body will be
added to the kill-ring.

If DEST is `delete', ITEM will be deleted.

Visibility of item is preserved.

This function returns, destructively, the new list structure."
  (let* ((prevs (org-list-prevs-alist struct))
	 (item-end (org-list-get-item-end item struct))
	 ;; Grab full item body minus its bullet.
	 (body (org-trim
		(buffer-substring
		 (save-excursion
		   (goto-char item)
		   (looking-at
		    (concat "[ \t]*"
			    (regexp-quote (org-list-get-bullet item struct))))
		   (match-end 0))
		 item-end)))
	 ;; Change DEST into a buffer position.  A trick is needed
	 ;; when ITEM is meant to be sent at the end of the list.
	 ;; Indeed, by setting locally `org-M-RET-may-split-line' to
	 ;; nil and insertion point (INS-POINT) to the first line's
	 ;; end of the last item, we ensure the new item will be
	 ;; inserted after the last item, and not after any of its
	 ;; hypothetical sub-items.
	 (ins-point (cond
		     ((or (eq dest 'kill) (eq dest 'delete)))
		     ((eq dest 'begin)
		      (setq dest (org-list-get-list-begin item struct prevs)))
		     ((eq dest 'end)
		      (setq dest (org-list-get-list-end item struct prevs))
		      (save-excursion
			(goto-char (org-list-get-last-item item struct prevs))
                        (line-end-position)))
		     ((and (stringp dest) (string-match-p "\\`[0-9]+\\'" dest))
		      (let* ((all (org-list-get-all-items item struct prevs))
			     (len (length all))
			     (index (mod (string-to-number dest) len)))
			(if (not (zerop index))
			    (setq dest (nth (1- index) all))
			  ;; Send ITEM at the end of the list.
			  (setq dest (org-list-get-list-end item struct prevs))
			  (save-excursion
			    (goto-char
			     (org-list-get-last-item item struct prevs))
                            (line-end-position)))))
		     (t dest)))
	 (org-M-RET-may-split-line nil)
	 ;; Store inner overlays (to preserve visibility).
	 (overlays (cl-remove-if (lambda (o) (or (< (overlay-start o) item)
						 (> (overlay-end o) item)))
				 (overlays-in item item-end))))
    (cond
     ((eq dest 'delete) (org-list-delete-item item struct))
     ((eq dest 'kill)
      (kill-new body)
      (org-list-delete-item item struct))
     ((and (integerp dest) (/= item ins-point))
      (setq item (copy-marker item))
      (setq struct (org-list-insert-item ins-point struct prevs nil body))
      ;; 1. Structure returned by `org-list-insert-item' may not be
      ;;    accurate, as it cannot see sub-items included in BODY.
      ;;    Thus, first compute the real structure so far.
      (let ((moved-items
	     (cons (marker-position item)
		   (org-list-get-subtree (marker-position item) struct)))
	    (new-end (org-list-get-item-end (point) struct))
	    (old-end (org-list-get-item-end (marker-position item) struct))
	    (new-item (point))
	    (shift (- (point) item)))
	;; 1.1. Remove the item just created in structure.
	(setq struct (delete (assq new-item struct) struct))
	;; 1.2. Copy ITEM and any of its sub-items at NEW-ITEM.
	(setq struct (sort
		      (append
		       struct
		       (mapcar (lambda (e)
				 (let* ((cell (assq e struct))
					(pos (car cell))
					(end (nth 6 cell)))
				   (cons (+ pos shift)
					 (append (butlast (cdr cell))
						 (list (if (= end old-end)
							   new-end
							 (+ end shift)))))))
			       moved-items))
		      #'car-less-than-car)))
      ;; 2. Restore inner overlays.
      (dolist (o overlays)
	(move-overlay o
		      (+ (overlay-start o) (- (point) item))
		      (+ (overlay-end o) (- (point) item))))
      ;; 3. Eventually delete extra copy of the item and clean marker.
      (prog1 (org-list-delete-item (marker-position item) struct)
	(move-marker item nil)))
     (t struct))))

(defun org-list-struct-outdent (start end struct parents)
  "Outdent items between positions START and END.

STRUCT is the list structure.  PARENTS is the alist of items'
parents, as returned by `org-list-parents-alist'.

START is included, END excluded."
  (let* (acc
	 (out (lambda (cell)
		(let* ((item (car cell))
		       (parent (cdr cell)))
		  (cond
		   ;; Item not yet in zone: keep association.
		   ((< item start) cell)
		   ;; Item out of zone: follow associations in ACC.
		   ((>= item end)
		    (let ((convert (and parent (assq parent acc))))
		      (if convert (cons item (cdr convert)) cell)))
		   ;; Item has no parent: error
		   ((not parent)
		    (error "Cannot outdent top-level items"))
		   ;; Parent is outdented: keep association.
		   ((>= parent start)
		    (push (cons parent item) acc) cell)
		   (t
		    ;; Parent isn't outdented: reparent to grand-parent.
		    (let ((grand-parent (org-list-get-parent
					 parent struct parents)))
		      (push (cons parent item) acc)
		      (cons item grand-parent))))))))
    (mapcar out parents)))

(defun org-list-struct-indent (start end struct parents prevs)
  "Indent items between positions START and END.

STRUCT is the list structure.  PARENTS is the alist of parents
and PREVS is the alist of previous items, returned by,
respectively, `org-list-parents-alist' and
`org-list-prevs-alist'.

START is included and END excluded.

STRUCT may be modified if `org-list-demote-modify-bullet' matches
bullets between START and END."
  (let* (acc
	 (set-assoc (lambda (cell) (push cell acc) cell))
	 (ind
	  (lambda (cell)
	    (let* ((item (car cell))
		   (parent (cdr cell)))
	      (cond
	       ;; Item not yet in zone: keep association.
	       ((< item start) cell)
	       ((>= item end)
		;; Item out of zone: follow associations in ACC.
		(let ((convert (assq parent acc)))
		  (if convert (cons item (cdr convert)) cell)))
	       (t
		;; Item is in zone...
		(let ((prev (org-list-get-prev-item item struct prevs)))
		  ;; Check if bullet needs to be changed.
		  (pcase (assoc (let ((b (org-list-get-bullet item struct))
				      (case-fold-search nil))
				  (cond ((string-match "[A-Z]\\." b) "A.")
					((string-match "[A-Z])" b) "A)")
					((string-match "[a-z]\\." b) "a.")
					((string-match "[a-z])" b) "a)")
					((string-match "[0-9]\\." b) "1.")
					((string-match "[0-9])" b) "1)")
					(t (org-trim b))))
				org-list-demote-modify-bullet)
		    (`(,_ . ,bullet)
		     (org-list-set-bullet
		      item struct (org-list-bullet-string bullet)))
		    (_ nil))
		  (cond
		   ;; First item indented but not parent: error
		   ((and (not prev) (or (not parent) (< parent start)))
		    (user-error "Cannot indent the first item of a list"))
		   ;; First item and parent indented: keep same
		   ;; parent.
		   ((not prev) (funcall set-assoc cell))
		   ;; Previous item not indented: reparent to it.
		   ((< prev start) (funcall set-assoc (cons item prev)))
		   ;; Previous item indented: reparent like it.
		   (t
		    (funcall set-assoc
			     (cons item (cdr (assq prev acc)))))))))))))
    (mapcar ind parents)))

(provide 'org-list-edit)

;;; org-list-edit.el ends here
