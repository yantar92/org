;;; org-list-core.el --- Plain lists for Org              -*- lexical-binding: t; -*-
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

;; This file contains the code dealing with plain lists in Org mode.

;; The core concept behind lists is their structure.  A structure is
;; a snapshot of the list, in the shape of a data tree (see
;; `org-list-struct').

;; Once the list structure is stored, it is possible to make changes
;; on it that will be mirrored to the real list or to get information
;; about the list, using accessors and methods provided in the
;; library.  Most of them require the use of one or two helper
;; functions, namely `org-list-parents-alist' and
;; `org-list-prevs-alist'.

;; Structure is eventually applied to the buffer with
;; `org-list-write-struct'.  This function repairs (bullets,
;; indentation, checkboxes) the list in the process.  It should be
;; called near the end of any function working on structures.

;; Thus, a function applying to lists should usually follow this
;; template:

;; 1. Verify point is in a list and grab item beginning (with the same
;;    function `org-in-item-p').  If the function requires the cursor
;;    to be at item's bullet, `org-at-item-p' is more selective.  It
;;    is also possible to move point to the closest item with
;;    `org-list-search-backward', or `org-list-search-forward',
;;    applied to the function `org-item-beginning-re'.

;; 2. Get list structure with `org-list-struct'.

;; 3. Compute one, or both, helper functions,
;;    (`org-list-parents-alist', `org-list-prevs-alist') depending on
;;    needed accessors.

;; 4. Proceed with the modifications, using methods and accessors.

;; 5. Verify and apply structure to buffer, using
;;    `org-list-write-struct'.

;; 6. If changes made to the list might have modified check-boxes,
;;    call `org-update-checkbox-count-maybe'.

;; Computing a structure can be a costly operation on huge lists (a
;; few thousand lines long).  Thus, code should follow the rule:
;; "collect once, use many".  As a corollary, it is usually a bad idea
;; to use directly an interactive function inside the code, as those,
;; being independent entities, read the whole list structure another
;; time.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element-context)
(require 'org-move)


;;; Configuration variables

(defgroup org-plain-lists nil
  "Options concerning plain lists in Org mode."
  :tag "Org Plain lists"
  :group 'org-structure)

(defcustom org-list-automatic-rules '((checkbox . t)
				      (indent . t))
  "Non-nil means apply set of rules when acting on lists.
\\<org-mode-map>
By default, automatic actions are taken when using
  `\\[org-meta-return]',
  `\\[org-metaright]',
  `\\[org-metaleft]',
  `\\[org-shiftmetaright]',
  `\\[org-shiftmetaleft]',
  `\\[org-ctrl-c-minus]',
  `\\[org-toggle-checkbox]',
  `\\[org-insert-todo-heading]'.

You can disable individually these rules by setting them to nil.
Valid rules are:

checkbox  when non-nil, checkbox statistics is updated each time
          you either insert a new checkbox or toggle a checkbox.
indent    when non-nil, indenting or outdenting list top-item
          with its subtree will move the whole list and
          outdenting a list whose bullet is * to column 0 will
          change that bullet to \"-\"."
  :group 'org-plain-lists
  :version "24.1"
  :type '(alist :tag "Sets of rules"
		:key-type
		(choice
		 (const :tag "Checkbox" checkbox)
		 (const :tag "Indent" indent))
		:value-type
		(boolean :tag "Activate" :value t)))

(defvar org-checkbox-statistics-hook nil
  "Hook that is run whenever Org thinks checkbox statistics should be updated.
This hook runs even if checkbox rule in
`org-list-automatic-rules' does not apply, so it can be used to
implement alternative ways of collecting statistics
information.")

(defcustom org-checkbox-hierarchical-statistics t
  "Non-nil means checkbox statistics counts only the state of direct children.
When nil, all boxes below the cookie are counted.
This can be set to nil on a per-node basis using a COOKIE_DATA property
with the word \"recursive\" in the value."
  :group 'org-plain-lists
  :type 'boolean)

(defcustom org-list-indent-offset 0
  "Additional indentation for sub-items in a list.
By setting this to a small number, usually 1 or 2, one can more
clearly distinguish sub-items in a list."
  :group 'org-plain-lists
  :version "24.1"
  :type 'integer)

(defvar org-list-forbidden-blocks '("example" "verse" "src" "export")
  "Names of blocks where lists are not allowed.
Names must be in lower case.")


;;; Predicates and regexps

(defsubst org-item-beginning-re ()
  "Regexp matching the beginning of a plain list item."
  (concat "^" (org-item-re)))

(defun org-list-at-regexp-after-bullet-p (regexp)
  "Is point at a list item with REGEXP after bullet?"
  (and (org-at-item-p)
       (save-excursion
	 (goto-char (match-end 0))
	 (let ((counter-re (concat "\\(?:\\[@\\(?:start:\\)?"
				   (if org-list-allow-alphabetical
				       "\\([0-9]+\\|[A-Za-z]\\)"
				     "[0-9]+")
				   "\\][ \t]*\\)")))
	   ;; Ignore counter if any
	   (when (looking-at counter-re) (goto-char (match-end 0))))
	 (looking-at regexp))))

(defun org-list-in-valid-context-p ()
  "Is point in a context where lists are allowed?"
  (not (org-in-block-p org-list-forbidden-blocks)))

(defun org-in-item-p ()
  "Return item beginning position when in a plain list, nil otherwise."
  (save-excursion
    (forward-line 0)
    (let* ((case-fold-search t)
	   (context (org-list-context))
	   (lim-up (car context))
	   (inlinetask-re (and (featurep 'org-inlinetask)
			       (org-inlinetask-outline-regexp)))
	   (item-re (org-item-re))
	   ;; Indentation isn't meaningful when point starts at an empty
	   ;; line or an inline task.
	   (ind-ref (if (or (looking-at "^[ \t]*$")
			    (and inlinetask-re (looking-at inlinetask-re)))
			10000
		      (org-current-text-indentation))))
      (cond
       ((eq (nth 2 context) 'invalid) nil)
       ((looking-at item-re) (point))
       (t
	;; Detect if cursor in amidst `org-list-end-re'.  First, count
	;; number HL of hard lines it takes, then call `org-in-regexp'
	;; to compute its boundaries END-BOUNDS.  When point is
	;; in-between, move cursor before regexp beginning.
	(let ((hl 0) (i -1) end-bounds)
	  (when (and (progn
		       (while (setq i (string-match
				       "[\r\n]" org-list-end-re (1+ i)))
			 (setq hl (1+ hl)))
		       (setq end-bounds (org-in-regexp org-list-end-re hl)))
		     (>= (point) (car end-bounds))
		     (< (point) (cdr end-bounds)))
	    (goto-char (car end-bounds))
	    (forward-line -1)))
	;; Look for an item, less indented that reference line.
	(catch 'exit
	  (while t
	    (let ((ind (org-current-text-indentation)))
	      (cond
	       ;; This is exactly what we want.
	       ((and (looking-at item-re) (< ind ind-ref))
		(throw 'exit (point)))
	       ;; At upper bound of search or looking at the end of a
	       ;; previous list: search is over.
	       ((<= (point) lim-up) (throw 'exit nil))
	       ((looking-at org-list-end-re) (throw 'exit nil))
	       ;; Skip blocks, drawers, inline-tasks, blank lines
	       ((and (looking-at "^[ \t]*#\\+end_")
		     (re-search-backward "^[ \t]*#\\+begin_" lim-up t)))
	       ((and (looking-at "^[ \t]*:END:")
		     (re-search-backward org-drawer-regexp lim-up t))
		(forward-line 0))
	       ((and inlinetask-re (looking-at inlinetask-re)
                     (fboundp 'org-inlinetask-goto-beginning))
		(org-inlinetask-goto-beginning)
		(forward-line -1))
	       ((looking-at "^[ \t]*$") (forward-line -1))
	       ;; Text at column 0 cannot belong to a list: stop.
	       ((zerop ind) (throw 'exit nil))
	       ;; Normal text less indented than reference line, take
	       ;; it as new reference.
	       ((< ind ind-ref)
		(setq ind-ref ind)
		(forward-line -1))
	       (t (forward-line -1)))))))))))

(defun org-at-item-bullet-p ()
  "Is point at the bullet of a plain list item?"
  (and (org-at-item-p)
       (not (member (char-after) '(?\  ?\t)))
       (< (point) (match-end 0))))

(defun org-at-item-timer-p ()
  "Is point at a line starting a plain list item with a timer?"
  (org-list-at-regexp-after-bullet-p
   "\\([0-9]+:[0-9]+:[0-9]+\\)[ \t]+::[ \t]+"))

(defun org-at-item-description-p ()
  "Is point at a description list item?"
  (org-list-at-regexp-after-bullet-p "\\(\\S-.+\\)[ \t]+::\\([ \t]+\\|$\\)"))

(defun org-at-item-checkbox-p ()
  "Is point at a line starting a plain-list item with a checklet?"
  (org-list-at-regexp-after-bullet-p "\\(\\[[- X]\\]\\)[ \t]+"))

(defun org-at-item-counter-p ()
  "Is point at a line starting a plain-list item with a counter?"
  (and (org-at-item-p)
       (looking-at org-list-full-item-re)
       (match-string 2)))



;;; Structures and helper functions

(defun org-list-context ()
  "Determine context, and its boundaries, around point.

Context will be a cell like (MIN MAX CONTEXT) where MIN and MAX
are boundaries and CONTEXT is a symbol among `drawer', `block',
`invalid', `inlinetask' and nil.

Contexts `block' and `invalid' refer to `org-list-forbidden-blocks'."
  (save-match-data
    (save-excursion
      (org-with-limited-levels
       (forward-line 0)
       (let ((case-fold-search t) (pos (point)) beg end context-type
	     ;; Get positions of surrounding headings.  This is the
	     ;; default context.
	     (lim-up (or (save-excursion
                           (and (ignore-errors (org-back-to-heading t))
				(point)))
			 (point-min)))
	     (lim-down (or (save-excursion (outline-next-heading)) (point-max))))
	 ;; Is point inside a drawer?
	 (let ((end-re "^[ \t]*:END:")
	       (beg-re org-drawer-regexp))
	   (when (save-excursion
		   (and (not (looking-at beg-re))
			(not (looking-at end-re))
			(setq beg (and (re-search-backward beg-re lim-up t)
                                       (1+ (line-end-position))))
			(setq end (or (and (re-search-forward end-re lim-down t)
					   (1- (match-beginning 0)))
				      lim-down))
			(>= end pos)))
	     (setq lim-up beg lim-down end context-type 'drawer)))
	 ;; Is point strictly in a block, and of which type?
	 (let ((block-re "^[ \t]*#\\+\\(begin\\|end\\)_") type)
	   (when (save-excursion
		   (and (not (looking-at block-re))
			(setq beg (and (re-search-backward block-re lim-up t)
                                       (1+ (line-end-position))))
			(looking-at "^[ \t]*#\\+begin_\\(\\S-+\\)")
			(setq type (downcase (match-string 1)))
			(goto-char beg)
			(setq end (or (and (re-search-forward block-re lim-down t)
                                           (1- (line-beginning-position)))
				      lim-down))
			(>= end pos)
			(equal (downcase (match-string 1)) "end")))
	     (setq lim-up beg lim-down end
		   context-type (if (member type org-list-forbidden-blocks)
				    'invalid 'block))))
	 ;; Is point in an inlinetask?
	 (when (and (featurep 'org-inlinetask)
		    (save-excursion
		      (let* ((beg-re (org-inlinetask-outline-regexp))
			     (end-re (concat beg-re "END[ \t]*$")))
			(and (not (looking-at "^\\*+"))
			     (setq beg (and (re-search-backward beg-re lim-up t)
                                            (1+ (line-end-position))))
			     (not (looking-at end-re))
			     (setq end (and (re-search-forward end-re lim-down t)
					    (1- (match-beginning 0))))
			     (> (point) pos)))))
	   (setq lim-up beg lim-down end context-type 'inlinetask))
	 ;; Return context boundaries and type.
	 (list lim-up lim-down context-type))))))

(defun org-list-struct ()
  "Return structure of list at point.

A list structure is an alist where key is point at item, and
values are:
1. indentation,
2. bullet with trailing whitespace,
3. bullet counter, if any,
4. checkbox, if any,
5. description tag, if any,
6. position at item end.

Thus the following list, where numbers in parens are
line-beginning-position:

- [X] first item                             (1)
  1. sub-item 1                              (18)
  5. [@5] sub-item 2                         (34)
  some other text belonging to first item    (55)
- last item                                  (97)
  + tag :: description                       (109)
                                             (131)

will get the following structure:

 ((1 0 \"- \"  nil \"[X]\" nil 97)
  (18 2 \"1. \"  nil nil nil 34)
  (34 2 \"5. \" \"5\" nil nil 55)
  (97 0 \"- \"  nil nil nil 131)
  (109 2 \"+ \" nil nil \"tag\" 131))

Assume point is at an item."
  (save-excursion
    (forward-line 0)
    (let* ((case-fold-search t)
	   (context (org-list-context))
	   (lim-up (car context))
	   (lim-down (nth 1 context))
	   (text-min-ind 10000)
	   (item-re (org-item-re))
	   (inlinetask-re (and (featurep 'org-inlinetask)
			       (org-inlinetask-outline-regexp)))
	   (beg-cell (cons (point) (org-current-text-indentation)))
           itm-lst itm-lst-2 end-lst end-lst-2 struct
	   (assoc-at-point
	    ;; Return association at point.
	    (lambda (ind)
	      (looking-at org-list-full-item-re)
	      (let ((bullet (match-string-no-properties 1)))
		(list (point)
		      ind
		      bullet
		      (match-string-no-properties 2) ; counter
		      (match-string-no-properties 3) ; checkbox
		      ;; Description tag.
		      (and (string-match-p "[-+*]" bullet)
			   (match-string-no-properties 4))))))
	   (end-before-blank
	    ;; Ensure list ends at the first blank line.
	    (lambda ()
              (org-skip-whitespace 'back)
              (min (1+ (line-end-position)) lim-down))))
      ;; 1. Read list from starting item to its beginning, and save
      ;;    top item position and indentation in BEG-CELL.  Also store
      ;;    ending position of items in END-LST.
      (save-excursion
	(catch 'exit
	  (while t
	    (let ((ind (org-current-text-indentation)))
	      (cond
	       ((<= (point) lim-up)
		;; At upward limit: if we ended at an item, store it,
		;; else dismiss useless data recorded above BEG-CELL.
		;; Jump to part 2.
		(throw 'exit
		       (setq itm-lst
			     (if (not (looking-at item-re))
				 (memq (assq (car beg-cell) itm-lst) itm-lst)
			       (setq beg-cell (cons (point) ind))
			       (cons (funcall assoc-at-point ind) itm-lst)))))
	       ;; Looking at a list ending regexp.  Dismiss useless
	       ;; data recorded above BEG-CELL.  Jump to part 2.
	       ((looking-at org-list-end-re)
		(throw 'exit
		       (setq itm-lst
			     (memq (assq (car beg-cell) itm-lst) itm-lst))))
	       ;; Point is at an item.  Add data to ITM-LST. It may
	       ;; also end a previous item: save it in END-LST.  If
	       ;; ind is less or equal than BEG-CELL and there is no
	       ;; end at this ind or lesser, this item becomes the new
	       ;; BEG-CELL.
	       ((looking-at item-re)
		(push (funcall assoc-at-point ind) itm-lst)
		(push (cons ind (point)) end-lst)
		(when (< ind text-min-ind) (setq beg-cell (cons (point) ind)))
		(forward-line -1))
	       ;; Skip blocks, drawers, inline tasks, blank lines.
	       ((and (looking-at "^[ \t]*#\\+end_")
		     (re-search-backward "^[ \t]*#\\+begin_" lim-up t)))
	       ((and (looking-at "^[ \t]*:END:")
		     (re-search-backward org-drawer-regexp lim-up t))
		(forward-line 0))
	       ((and inlinetask-re (looking-at inlinetask-re)
                     (fboundp 'org-inlinetask-goto-beginning))
		(org-inlinetask-goto-beginning)
		(forward-line -1))
	       ((looking-at "^[ \t]*$")
		(forward-line -1))
	       ;; From there, point is not at an item.  Interpret
	       ;; line's indentation:
	       ;; - text at column 0 is necessarily out of any list.
	       ;;   Dismiss data recorded above BEG-CELL.  Jump to
	       ;;   part 2.
	       ;; - any other case may be an ending position for an
	       ;;   hypothetical item above.  Store it and proceed.
	       ((zerop ind)
		(throw 'exit
		       (setq itm-lst
			     (memq (assq (car beg-cell) itm-lst) itm-lst))))
	       (t
		(when (< ind text-min-ind) (setq text-min-ind ind))
		(push (cons ind (point)) end-lst)
		(forward-line -1)))))))
      ;; 2. Read list from starting point to its end, that is until we
      ;;    get out of context, or that a non-item line is less or
      ;;    equally indented than BEG-CELL's cdr.  Also, store ending
      ;;    position of items in END-LST-2.
      (catch 'exit
	(while t
	  (let ((ind (org-current-text-indentation)))
	    (cond
	     ((>= (point) lim-down)
	      ;; At downward limit: this is de facto the end of the
	      ;; list.  Save point as an ending position, and jump to
	      ;; part 3.
	      (throw 'exit
		     (push (cons 0 (funcall end-before-blank)) end-lst-2)))
	     ;; Looking at a list ending regexp.  Save point as an
	     ;; ending position and jump to part 3.
	     ((looking-at org-list-end-re)
	      (throw 'exit (push (cons 0 (point)) end-lst-2)))
	     ((looking-at item-re)
	      ;; Point is at an item.  Add data to ITM-LST-2. It may
	      ;; also end a previous item, so save it in END-LST-2.
	      (push (funcall assoc-at-point ind) itm-lst-2)
	      (push (cons ind (point)) end-lst-2)
	      (forward-line 1))
	     ;; Skip inline tasks and blank lines along the way
	     ((and inlinetask-re (looking-at inlinetask-re)
                   (fboundp 'org-inlinetask-goto-end))
	      (org-inlinetask-goto-end))
	     ((looking-at "^[ \t]*$")
	      (forward-line 1))
	     ;; Ind is lesser or equal than BEG-CELL's.  The list is
	     ;; over: store point as an ending position and jump to
	     ;; part 3.
	     ((<= ind (cdr beg-cell))
	      (throw 'exit
		     (push (cons 0 (funcall end-before-blank)) end-lst-2)))
	     ;; Else, if ind is lesser or equal than previous item's,
	     ;; this is an ending position: store it.  In any case,
	     ;; skip block or drawer at point, and move to next line.
	     (t
	      (when (<= ind (nth 1 (car itm-lst-2)))
		(push (cons ind (point)) end-lst-2))
	      (cond
	       ((and (looking-at "^[ \t]*#\\+begin_")
		     (re-search-forward "^[ \t]*#\\+end_" lim-down t)))
	       ((and (looking-at org-drawer-regexp)
		     (re-search-forward "^[ \t]*:END:" lim-down t))))
	      (forward-line 1))))))
      (setq struct (append itm-lst (cdr (nreverse itm-lst-2)))
	    end-lst (append end-lst (cdr (nreverse end-lst-2))))
      ;; 3. Associate each item to its end position.
      (org-list-struct-assoc-end struct end-lst)
      ;; 4. Return STRUCT
      struct)))

(defun org-list-struct-assoc-end (struct end-list)
  "Associate proper ending point to items in STRUCT.

END-LIST is a pseudo-alist where car is indentation and cdr is
ending position.

This function modifies STRUCT."
  (let ((endings end-list))
    (mapc
     (lambda (elt)
       (let ((pos (car elt))
	     (ind (nth 1 elt)))
	 ;; Remove end candidates behind current item.
	 (while (or (<= (cdar endings) pos))
	   (pop endings))
	 ;; Add end position to item assoc.
	 (let ((old-end (nthcdr 6 elt))
	       (new-end (assoc-default ind endings '<=)))
	   (if old-end
	       (setcar old-end new-end)
	     (setcdr elt (append (cdr elt) (list new-end)))))))
     struct)))

(defun org-list-prevs-alist (struct)
  "Return alist between item and previous item in STRUCT."
  (let ((item-end-alist (mapcar (lambda (e) (cons (car e) (nth 6 e)))
				struct)))
    (mapcar (lambda (e)
	      (let ((prev (car (rassq (car e) item-end-alist))))
		(cons (car e) prev)))
	    struct)))

(defun org-list-parents-alist (struct)
  "Return alist between item and parent in STRUCT."
  (let* ((ind-to-ori (list (list (nth 1 (car struct)))))
	 (top-item (org-list-get-top-point struct))
	 (prev-pos (list top-item)))
    (cons prev-pos
	  (mapcar (lambda (item)
		    (let ((pos (car item))
			  (ind (nth 1 item))
			  (prev-ind (caar ind-to-ori)))
		      (push pos prev-pos)
		      (cond
		       ((> prev-ind ind)
			;; A sub-list is over.  Find the associated
			;; origin in IND-TO-ORI.  If it cannot be
			;; found (ill-formed list), set its parent as
			;; the first item less indented.  If there is
			;; none, make it a top-level item.
			(setq ind-to-ori
			      (or (member (assq ind ind-to-ori) ind-to-ori)
                                  (catch 'exit
                                    (mapc
                                     (lambda (e)
                                       (when (< (car e) ind)
                                         (throw 'exit (member e ind-to-ori))))
                                     ind-to-ori)
                                    (list (list ind)))))
			(cons pos (cdar ind-to-ori)))
                       ;; A sub-list starts.  Every item at IND will
                       ;; have previous item as its parent.
		       ((< prev-ind ind)
			(let ((origin (nth 1 prev-pos)))
			  (push (cons ind origin) ind-to-ori)
			  (cons pos origin)))
                       ;; Another item in the same sub-list: it shares
                       ;; the same parent as the previous item.
		       (t (cons pos (cdar ind-to-ori))))))
		  (cdr struct)))))


;;; Accessors

(defun org-list-get-item-end-before-blank (item struct)
  "Return point at end of ITEM in STRUCT, before any blank line.
Point returned is at end of line."
  (save-excursion
    (goto-char (org-list-get-item-end item struct))
    (org-skip-whitespace 'back)
    (line-end-position)))

(defun org-list-get-parent (item struct parents)
  "Return parent of ITEM or nil.
STRUCT is the list structure.  PARENTS is the alist of parents,
as returned by `org-list-parents-alist'."
  (let ((parents (or parents (org-list-parents-alist struct))))
    (cdr (assq item parents))))

(defun org-list-has-child-p (item struct)
  "Non-nil if ITEM has a child.

STRUCT is the list structure.

Value returned is the position of the first child of ITEM."
  (let ((ind (org-list-get-ind item struct))
	(child-maybe (car (nth 1 (member (assq item struct) struct)))))
    (when (and child-maybe
	       (< ind (org-list-get-ind child-maybe struct)))
      child-maybe)))

(defun org-list-get-next-item (item _struct prevs)
  "Return next item in same sub-list as ITEM, or nil.
STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'."
  (car (rassq item prevs)))

(defun org-list-get-prev-item (item _struct prevs)
  "Return previous item in same sub-list as ITEM, or nil.
STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'."
  (cdr (assq item prevs)))

(defun org-list-get-subtree (item struct)
  "List all items having ITEM as a common ancestor, or nil.
STRUCT is the list structure."
  (let* ((item-end (org-list-get-item-end item struct))
	 (sub-struct (cdr (member (assq item struct) struct)))
	 items)
    (catch :exit
      (pcase-dolist (`(,pos . ,_) sub-struct)
	(if (< pos item-end)
	    (push pos items)
	  (throw :exit nil))))
    (nreverse items)))

(defun org-list-get-all-items (item struct prevs)
  "List all items in the same sub-list as ITEM.
STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'."
  (let ((prev-item item)
	(next-item item)
	before-item after-item)
    (while (setq prev-item (org-list-get-prev-item prev-item struct prevs))
      (push prev-item before-item))
    (while (setq next-item (org-list-get-next-item next-item struct prevs))
      (push next-item after-item))
    (append before-item (list item) (nreverse after-item))))

(defun org-list-get-children (item _struct parents)
  "List all children of ITEM, or nil.
STRUCT is the list structure.  PARENTS is the alist of parents,
as returned by `org-list-parents-alist'."
  (let (all child)
    (while (setq child (car (rassq item parents)))
      (setq parents (cdr (member (assq child parents) parents)))
      (push child all))
    (nreverse all)))

(defun org-list-get-top-point (struct)
  "Return point at beginning of list.
STRUCT is the list structure."
  (caar struct))

(defun org-list-get-bottom-point (struct)
  "Return point at bottom of list.
STRUCT is the list structure."
  (apply #'max
	 (mapcar (lambda (e) (org-list-get-item-end (car e) struct)) struct)))

(defun org-list-get-list-begin (item struct prevs)
  "Return point at beginning of sub-list ITEM belongs.
STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'."
  (let ((first-item item) prev-item)
    (while (setq prev-item (org-list-get-prev-item first-item struct prevs))
      (setq first-item prev-item))
    first-item))

(defalias 'org-list-get-first-item 'org-list-get-list-begin)

(defun org-list-get-last-item (item struct prevs)
  "Return point at last item of sub-list ITEM belongs.
STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'."
  (let ((last-item item) next-item)
    (while (setq next-item (org-list-get-next-item last-item struct prevs))
      (setq last-item next-item))
    last-item))

(defun org-list-get-list-end (item struct prevs)
  "Return point at end of sub-list ITEM belongs.
STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'."
  (org-list-get-item-end (org-list-get-last-item item struct prevs) struct))

(defun org-list-get-list-type (item struct prevs)
  "Return the type of the list containing ITEM, as a symbol.

STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'.

Possible types are `descriptive', `ordered' and `unordered'.  The
type is determined by the first item of the list."
  (let ((first (org-list-get-list-begin item struct prevs)))
    (cond
     ((string-match-p "[[:alnum:]]" (org-list-get-bullet first struct)) 'ordered)
     ((org-list-get-tag first struct) 'descriptive)
     (t 'unordered))))

(defun org-list-get-item-number (item struct prevs parents)
  "Return ITEM's sequence number.

STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'.  PARENTS is the
alist of ancestors, as returned by `org-list-parents-alist'.

Return value is a list of integers.  Counters have an impact on
that value."
  (let ((get-relative-number
	 (lambda (item struct prevs)
	   ;; Return relative sequence number of ITEM in the sub-list
	   ;; it belongs.  STRUCT is the list structure.  PREVS is
	   ;; the alist of previous items.
	   (let ((seq 0) (pos item) counter)
	     (while (and (not (setq counter (org-list-get-counter pos struct)))
			 (setq pos (org-list-get-prev-item pos struct prevs)))
	       (cl-incf seq))
	     (if (not counter) (1+ seq)
	       (cond
		((string-match "[A-Za-z]" counter)
		 (+ (- (string-to-char (upcase (match-string 0 counter))) 64)
		    seq))
		((string-match "[0-9]+" counter)
		 (+ (string-to-number (match-string 0 counter)) seq))
		(t (1+ seq))))))))
    ;; Cons each parent relative number into return value (OUT).
    (let ((out (list (funcall get-relative-number item struct prevs)))
	  (parent item))
      (while (setq parent (org-list-get-parent parent struct parents))
	(push (funcall get-relative-number parent struct prevs) out))
      ;; Return value.
      out)))



;;; Searching

(defun org-list-search-generic (search re bound noerr)
  "Search a string in valid contexts for lists.
Arguments SEARCH, RE, BOUND and NOERR are similar to those used
in `re-search-forward'."
  (catch 'exit
    (let ((origin (point)))
      (while t
	;; 1. No match: return to origin or bound, depending on NOERR.
	(unless (funcall search re bound noerr)
	  (throw 'exit (and (goto-char (if (memq noerr '(t nil)) origin bound))
			    nil)))
	;; 2. Match in valid context: return point.  Else, continue
	;;    searching.
	(when (org-list-in-valid-context-p) (throw 'exit (point)))))))

(defun org-list-search-backward (regexp &optional bound noerror)
  "Like `re-search-backward' but stop only where lists are recognized.
Arguments REGEXP, BOUND and NOERROR are similar to those used in
`re-search-backward'."
  (org-list-search-generic #'re-search-backward
			   regexp (or bound (point-min)) noerror))

(defun org-list-search-forward (regexp &optional bound noerror)
  "Like `re-search-forward' but stop only where lists are recognized.
Arguments REGEXP, BOUND and NOERROR are similar to those used in
`re-search-forward'."
  (org-list-search-generic #'re-search-forward
			   regexp (or bound (point-max)) noerror))



;;; Repairing structures

(defun org-list-use-alpha-bul-p (first struct prevs)
  "Non-nil if list starting at FIRST can have alphabetical bullets.

STRUCT is list structure.  PREVS is the alist of previous items,
as returned by `org-list-prevs-alist'."
  (and org-list-allow-alphabetical
       (catch 'exit
	 (let ((item first) (ascii 64) (case-fold-search nil))
	   ;; Pretend that bullets are uppercase and check if alphabet
	   ;; is sufficient, taking counters into account.
	   (while item
	     (let ((count (org-list-get-counter item struct)))
	       ;; Virtually determine current bullet
	       (if (and count (string-match-p "[a-zA-Z]" count))
		   ;; Counters are not case-sensitive.
		   (setq ascii (string-to-char (upcase count)))
		 (setq ascii (1+ ascii)))
	       ;; Test if bullet would be over z or Z.
	       (if (> ascii 90)
		   (throw 'exit nil)
		 (setq item (org-list-get-next-item item struct prevs)))))
	   ;; All items checked.  All good.
	   t))))

(defun org-list-inc-bullet-maybe (bullet)
  "Increment BULLET if applicable."
  (let ((case-fold-search nil))
    (cond
     ;; Num bullet: increment it.
     ((string-match "[0-9]+" bullet)
      (replace-match
       (number-to-string (1+ (string-to-number (match-string 0 bullet))))
       nil nil bullet))
     ;; Alpha bullet: increment it.
     ((string-match "[A-Za-z]" bullet)
      (replace-match
       (char-to-string (1+ (string-to-char (match-string 0 bullet))))
       nil nil bullet))
     ;; Unordered bullet: leave it.
     (t bullet))))

(defun org-list-struct-fix-bul (struct prevs)
  "Verify and correct bullets in STRUCT.
PREVS is the alist of previous items, as returned by
`org-list-prevs-alist'.

This function modifies STRUCT."
  (let ((case-fold-search nil)
	(fix-bul
	 ;; Set bullet of ITEM in STRUCT, depending on the type of
	 ;; first item of the list, the previous bullet and counter
	 ;; if any.
	 (lambda (item)
	   (let* ((prev (org-list-get-prev-item item struct prevs))
		  (prev-bul (and prev (org-list-get-bullet prev struct)))
		  (counter (org-list-get-counter item struct))
		  (bullet (org-list-get-bullet item struct))
		  (alphap (and (not prev)
			       (org-list-use-alpha-bul-p item struct prevs))))
	     (org-list-set-bullet
	      item struct
	      (org-list-bullet-string
	       (cond
		;; Alpha counter in alpha list: use counter.
		((and prev counter
		      (string-match "[a-zA-Z]" counter)
		      (string-match "[a-zA-Z]" prev-bul))
		 ;; Use cond to be sure `string-match' is used in
		 ;; both cases.
		 (let ((real-count
			(cond
			 ((string-match "[a-z]" prev-bul) (downcase counter))
			 ((string-match "[A-Z]" prev-bul) (upcase counter)))))
		   (replace-match real-count nil nil prev-bul)))
		;; Num counter in a num list: use counter.
		((and prev counter
		      (string-match "[0-9]+" counter)
		      (string-match "[0-9]+" prev-bul))
		 (replace-match counter nil nil prev-bul))
		;; No counter: increase, if needed, previous bullet.
		(prev
		 (org-list-inc-bullet-maybe (org-list-get-bullet prev struct)))
		;; Alpha counter at first item: use counter.
		((and counter (org-list-use-alpha-bul-p item struct prevs)
		      (string-match "[A-Za-z]" counter)
		      (string-match "[A-Za-z]" bullet))
		 (let ((real-count
			(cond
			 ((string-match "[a-z]" bullet) (downcase counter))
			 ((string-match "[A-Z]" bullet) (upcase counter)))))
		   (replace-match real-count nil nil bullet)))
		;; Num counter at first item: use counter.
		((and counter
		      (string-match "[0-9]+" counter)
		      (string-match "[0-9]+" bullet))
		 (replace-match counter nil nil bullet))
		;; First bullet is alpha uppercase: use "A".
		((and alphap (string-match "[A-Z]" bullet))
		 (replace-match "A" nil nil bullet))
		;; First bullet is alpha lowercase: use "a".
		((and alphap (string-match "[a-z]" bullet))
		 (replace-match "a" nil nil bullet))
		;; First bullet is num: use "1".
		((string-match "\\([0-9]+\\|[A-Za-z]\\)" bullet)
		 (replace-match "1" nil nil bullet))
		;; Not an ordered list: keep bullet.
		(t bullet))))))))
    (mapc fix-bul (mapcar #'car struct))))

(defun org-list-struct-fix-ind (struct parents &optional bullet-size)
  "Verify and correct indentation in STRUCT.

PARENTS is the alist of parents, as returned by
`org-list-parents-alist'.

If numeric optional argument BULLET-SIZE is set, assume all
bullets in list have this length to determine new indentation.

This function modifies STRUCT."
  (let* ((ancestor (org-list-get-top-point struct))
         (top-ind (org-list-get-ind ancestor struct))
         (new-ind
          (lambda (item)
            (let ((parent (org-list-get-parent item struct parents)))
              (if parent
                  ;; Indent like parent + length of parent's bullet +
		  ;; sub-list offset.
                  (org-list-set-ind
		   item struct (+ (or bullet-size
				      (length
				       (org-list-get-bullet parent struct)))
				  (org-list-get-ind parent struct)
				  org-list-indent-offset))
                ;; If no parent, indent like top-point.
		(org-list-set-ind item struct top-ind))))))
    (mapc new-ind (mapcar #'car (cdr struct)))))

(defun org-list-struct-fix-box (struct parents prevs &optional ordered)
  "Verify and correct checkboxes in STRUCT.

PARENTS is the alist of parents and PREVS is the alist of
previous items, as returned by, respectively,
`org-list-parents-alist' and `org-list-prevs-alist'.

If ORDERED is non-nil, a checkbox can only be checked when every
checkbox before it is checked too.  If there was an attempt to
break this rule, the function will return the blocking item.  In
all others cases, the return value will be nil.

This function modifies STRUCT."
  (let ((all-items (mapcar #'car struct))
	(set-parent-box
	 (lambda (item)
	   (let* ((box-list
		   (mapcar (lambda (child)
			     (org-list-get-checkbox child struct))
			   (org-list-get-children item struct parents))))
	     (org-list-set-checkbox
	      item struct
	      (cond
	       ((and (member "[ ]" box-list) (member "[X]" box-list)) "[-]")
	       ((member "[-]" box-list) "[-]")
	       ((member "[X]" box-list) "[X]")
	       ((member "[ ]" box-list) "[ ]")
	       ;; Parent has no boxed child: leave box as-is.
	       (t (org-list-get-checkbox item struct)))))))
	parent-list)
    ;; 1. List all parents with a checkbox.
    (mapc
     (lambda (e)
       (let* ((parent (org-list-get-parent e struct parents))
	      (parent-box-p (org-list-get-checkbox parent struct)))
	 (when (and parent-box-p (not (memq parent parent-list)))
	   (push parent parent-list))))
     all-items)
    ;; 2. Sort those parents by decreasing indentation.
    (setq parent-list (sort parent-list
			    (lambda (e1 e2)
			      (> (org-list-get-ind e1 struct)
				 (org-list-get-ind e2 struct)))))
    ;; 3. For each parent, get all children's checkboxes to determine
    ;;    and set its checkbox accordingly.
    (mapc set-parent-box parent-list)
    ;; 4. If ORDERED is set, see if we need to uncheck some boxes.
    (when ordered
      (let* ((box-list
	      (mapcar (lambda (e) (org-list-get-checkbox e struct)) all-items))
	     (after-unchecked (member "[ ]" box-list)))
	;; There are boxes checked after an unchecked one: fix that.
	(when (member "[X]" after-unchecked)
	  (let ((index (- (length struct) (length after-unchecked))))
	    (dolist (e (nthcdr index all-items))
	      (when (org-list-get-checkbox e struct)
		(org-list-set-checkbox e struct "[ ]")))
	    ;; Verify once again the structure, without ORDERED.
	    (org-list-struct-fix-box struct parents prevs nil)
	    ;; Return blocking item.
	    (nth index all-items)))))))

(defun org-list-struct-fix-item-end (struct)
  "Verify and correct each item end position in STRUCT.

This function modifies STRUCT."
  (let (end-list acc-end)
    (pcase-dolist (`(,pos . ,_) struct)
      (let ((ind-pos (org-list-get-ind pos struct))
	    (end-pos (org-list-get-item-end pos struct)))
	(unless (assq end-pos struct)
	  ;; To determine real ind of an ending position that is not
	  ;; at an item, we have to find the item it belongs to: it is
	  ;; the last item (ITEM-UP), whose ending is further than the
	  ;; position we're interested in.
	  (let ((item-up (assoc-default end-pos acc-end #'>)))
	    (push (cons
		   ;; Else part is for the bottom point.
		   (if item-up (+ (org-list-get-ind item-up struct) 2) 0)
		   end-pos)
		  end-list)))
	(push (cons ind-pos pos) end-list)
	(push (cons end-pos pos) acc-end)))
    (setq end-list (sort end-list (lambda (e1 e2) (< (cdr e1) (cdr e2)))))
    (org-list-struct-assoc-end struct end-list)))

(defun org-list-struct-apply-struct (struct old-struct)
  "Apply set difference between STRUCT and OLD-STRUCT to the buffer.

OLD-STRUCT is the structure before any modifications, and STRUCT
the structure to be applied.  The function will only modify parts
of the list which have changed.

Initial position of cursor is restored after the changes."
  (let* ((origin (point-marker))
	 (inlinetask-re (and (featurep 'org-inlinetask)
			     (org-inlinetask-outline-regexp)))
	 (item-re (org-item-re))
	 (shift-body-ind
	  ;; Shift the indentation between END and BEG by DELTA.
	  ;; Start from the line before END.
          ;; Take care not to shift to or before IND, which is the
          ;; containg list item indentation. (otherwise, we are going
          ;; to break the list structure)
	  (lambda (end beg delta ind)
	    (goto-char end)
            (org-skip-whitespace 'back)
	    (forward-line 0)
	    (while (or (> (point) beg)
		       (and (= (point) beg)
			    (not (looking-at item-re))))
	      (cond
	       ;; Skip inline tasks.
	       ((and inlinetask-re (looking-at inlinetask-re)
                     (fboundp 'org-inlinetask-goto-beginning))
		(org-inlinetask-goto-beginning))
	       ;; Shift only non-empty lines.
	       ((looking-at-p "^[ \t]*\\S-")
		(indent-line-to (max (+ (org-current-text-indentation) delta)
                                     (if ind (1+ ind) -1)))))
	      (forward-line -1))))
	 (modify-item
	  ;; Replace ITEM first line elements with new elements from
	  ;; STRUCT, if appropriate.
	  (lambda (item)
	    (goto-char item)
	    (let* ((new-ind (org-list-get-ind item struct))
		   (old-ind (org-current-text-indentation))
		   (new-bul (org-list-bullet-string
			     (org-list-get-bullet item struct)))
		   (old-bul (org-list-get-bullet item old-struct))
		   (new-box (org-list-get-checkbox item struct)))
	      (looking-at org-list-full-item-re)
	      ;; a.  Replace bullet
	      (unless (equal old-bul new-bul)
                (let ((keep-space ""))
                  (save-excursion
                    ;; If origin is inside the bullet, preserve the
                    ;; spaces after origin.
                    (when (<= (match-beginning 1) origin (match-end 1))
                      (org-with-point-at origin
                        (save-match-data
                          (when (looking-at "[ \t]+")
                            (setq keep-space (match-string 0))))))
                    (replace-match "" nil nil nil 1)
                    (goto-char (match-end 1))
                    (insert-before-markers new-bul)
                    (insert keep-space))))
              ;; Refresh potentially shifted match markers.
              (goto-char item)
              (looking-at org-list-full-item-re)
	      ;; b.  Replace checkbox.
	      (cond
	       ((equal (match-string 3) new-box))
	       ((and (match-string 3) new-box)
		(replace-match new-box nil nil nil 3))
	       ((match-string 3)
		(looking-at ".*?\\([ \t]*\\[[ X-]\\]\\)")
		(replace-match "" nil nil nil 1))
	       (t (let ((counterp (match-end 2)))
		    (goto-char (if counterp (1+ counterp) (match-end 1)))
		    (insert (concat new-box (unless counterp " "))))))
	      ;; c.  Indent item to appropriate column.
	      (unless (= new-ind old-ind)
                (delete-region (goto-char (line-beginning-position))
			       (progn (skip-chars-forward " \t") (point)))
		(indent-to new-ind))))))
    ;; 1. First get list of items and position endings.  We maintain
    ;;    two alists: ITM-SHIFT, determining indentation shift needed
    ;;    at item, and END-LIST, a pseudo-alist where key is ending
    ;;    position and value point.
    (let (end-list acc-end itm-shift all-ends sliced-struct)
      (dolist (e old-struct)
	(let* ((pos (car e))
	       (ind-pos (org-list-get-ind pos struct))
	       (ind-old (org-list-get-ind pos old-struct))
	       (bul-pos (org-list-get-bullet pos struct))
	       (bul-old (org-list-get-bullet pos old-struct))
	       (ind-shift (- (+ ind-pos (length bul-pos))
			     (+ ind-old (length bul-old))))
	       (end-pos (org-list-get-item-end pos old-struct)))
	  (push (list pos ind-shift ind-pos) itm-shift)
	  (unless (assq end-pos old-struct)
	    ;; To determine real ind of an ending position that
	    ;; is not at an item, we have to find the item it
	    ;; belongs to: it is the last item (ITEM-UP), whose
	    ;; ending is further than the position we're
	    ;; interested in.
	    (let ((item-up (assoc-default end-pos acc-end #'>)))
	      (push (cons end-pos item-up) end-list)))
	  (push (cons end-pos pos) acc-end)))
      ;; 2. Slice the items into parts that should be shifted by the
      ;;    same amount of indentation.  Each slice follow the pattern
      ;;    (END BEG DELTA).  Slices are returned in reverse order.
      (setq all-ends (sort (append (mapcar #'car itm-shift)
				   (org-uniquify (mapcar #'car end-list)))
			   #'<)
	    acc-end (nreverse acc-end))
      (while (cdr all-ends)
	(let* ((up (pop all-ends))
	       (down (car all-ends))
	       (itemp (assq up struct))
	       (delta
		(if itemp (nth 1 (assq up itm-shift))
		  ;; If we're not at an item, there's a child of the
		  ;; item point belongs to above.  Make sure the less
		  ;; indented line in this slice has the same column
		  ;; as that child.
		  (let* ((child (cdr (assq up acc-end)))
			 (ind (org-list-get-ind child struct))
			 (min-ind most-positive-fixnum))
		    (save-excursion
		      (goto-char up)
		      (while (< (point) down)
			;; Ignore empty lines.  Also ignore blocks and
			;; drawers contents.
			(unless (looking-at-p "[ \t]*$")
			  (setq min-ind (min (org-current-text-indentation) min-ind))
			  (cond
			   ((and (looking-at "#\\+BEGIN\\(:\\|_\\S-+\\)")
				 (re-search-forward
				  (format "^[ \t]*#\\+END%s[ \t]*$"
					  (match-string 1))
				  down t)))
			   ((and (looking-at org-drawer-regexp)
				 (re-search-forward "^[ \t]*:END:[ \t]*$"
						    down t)))))
			(forward-line)))
		    (- ind min-ind)))))
	  (push (list down up delta (when itemp (nth 2 (assq up itm-shift)))) sliced-struct)))
      ;; 3. Shift each slice in buffer, provided delta isn't 0, from
      ;;    end to beginning.  Take a special action when beginning is
      ;;    at item bullet.
      (dolist (e sliced-struct)
	(unless (zerop (nth 2 e)) (apply shift-body-ind e))
	(let* ((beg (nth 1 e))
	       (cell (assq beg struct)))
	  (unless (or (not cell) (equal cell (assq beg old-struct)))
	    (funcall modify-item beg)))))
    ;; 4. Go back to initial position and clean marker.
    (goto-char origin)
    (move-marker origin nil)))

(defun org-list-write-struct (struct parents &optional old-struct)
  "Correct bullets, checkboxes and indentation in list at point.

STRUCT is the list structure.  PARENTS is the alist of parents,
as returned by `org-list-parents-alist'.

When non-nil, optional argument OLD-STRUCT is the reference
structure of the list.  It should be provided whenever STRUCT
doesn't correspond anymore to the real list in buffer."
  ;; Order of functions matters here: checkboxes and endings need
  ;; correct indentation to be set, and indentation needs correct
  ;; bullets.
  ;;
  ;; 0. Save a copy of structure before modifications
  (let ((old-struct (or old-struct (copy-tree struct))))
    ;; 1. Set a temporary, but coherent with PARENTS, indentation in
    ;;    order to get items endings and bullets properly
    (org-list-struct-fix-ind struct parents 2)
    ;; 2. Fix each item end to get correct prevs alist.
    (org-list-struct-fix-item-end struct)
    ;; 3. Get bullets right.
    (let ((prevs (org-list-prevs-alist struct)))
      (org-list-struct-fix-bul struct prevs)
      ;; 4. Now get real indentation.
      (org-list-struct-fix-ind struct parents)
      ;; 5. Eventually fix checkboxes.
      (org-list-struct-fix-box struct parents prevs))
    ;; 6. Apply structure modifications to buffer.
    (org-list-struct-apply-struct struct old-struct))
  ;; 7. Return the updated structure
  struct)



;;; Misc Tools

(defun org-apply-on-list (function init-value &rest args)
  "Call FUNCTION on each item of the list at point.
FUNCTION must be called with at least one argument: INIT-VALUE,
that will contain the value returned by the function at the
previous item, plus ARGS extra arguments.

FUNCTION is applied on items in reverse order.

As an example, \(org-apply-on-list \(lambda \(result) \(1+ result)) 0)
will return the number of items in the current list.

Sublists of the list are skipped.  Cursor is always at the
beginning of the item."
  (let* ((struct (org-list-struct))
	 (prevs (org-list-prevs-alist struct))
         (item (copy-marker (line-beginning-position)))
	 (all (org-list-get-all-items (marker-position item) struct prevs))
	 (value init-value))
    (dolist (e (nreverse all))
      (goto-char e)
      (setq value (apply function value args)))
    (goto-char item)
    (move-marker item nil)
    value))

(defun org-list-set-item-visibility (item struct view)
  "Set visibility of ITEM in STRUCT to VIEW.

Possible values are: `folded', `children' or `subtree'.  See
`org-cycle' for more information."
  (cond
   ((eq view 'folded)
    (let ((item-end (org-list-get-item-end-before-blank item struct)))
      ;; Hide from eol
      (org-fold-core-region (save-excursion (goto-char item) (line-end-position))
		            item-end t 'outline)))
   ((eq view 'children)
    ;; First show everything.
    (org-list-set-item-visibility item struct 'subtree)
    ;; Then fold every child.
    (let* ((parents (org-list-parents-alist struct))
	   (children (org-list-get-children item struct parents)))
      (dolist (child children)
	(org-list-set-item-visibility child struct 'folded))))
   ((eq view 'subtree)
    ;; Show everything
    (let ((item-end (org-list-get-item-end item struct)))
      (org-fold-core-region item item-end nil 'outline)))))

(defun org-list-item-body-column (item)
  "Return column at which body of ITEM should start."
  (save-excursion
    (goto-char item)
    (looking-at "[ \t]*\\(\\S-+\\)")
    (+ (progn (goto-char (match-end 1)) (current-column))
       (if (and org-list-two-spaces-after-bullet-regexp
		(string-match-p org-list-two-spaces-after-bullet-regexp
				(match-string 1)))
	   2
	 1))))

(defun org-list-repair ()
  "Fix indentation, bullets and checkboxes in the list at point."
  (interactive)
  (unless (org-at-item-p) (error "This is not a list"))
  (let* ((struct (org-list-struct))
	 (parents (org-list-parents-alist struct)))
    (org-list-write-struct struct parents)))


(defun org-at-radio-list-p ()
  "Is point at a list item with radio buttons?"
  (when (org-match-line (org-item-re))	;short-circuit
    (let* ((e (save-excursion (forward-line 0) (org-element-at-point))))
      ;; Check we're really on a line with a bullet.
      (when (org-element-type-p e '(item plain-list))
	;; Look for ATTR_ORG attribute in the current plain list.
	(let ((plain-list (org-element-lineage e 'plain-list t)))
	  (org-with-point-at (org-element-post-affiliated plain-list)
	    (let ((case-fold-search t)
		  (regexp "^[ \t]*#\\+attr_org:.* :radio \\(\\S-+\\)")
		  (begin (org-element-begin plain-list)))
	      (and (re-search-backward regexp begin t)
		   (not (string-equal "nil" (match-string 1)))))))))))

(declare-function org-fix-tags-on-the-fly "org-tags-align" ())
(declare-function org-entry-get "org-property"
                  (epom property &optional inherit literal-nil))
(defun org-update-checkbox-count (&optional all)
  "Update the checkbox statistics in the current section.

This will find all statistic cookies like [57%] and [6/12] and
update them with the current numbers.

With optional prefix argument ALL, do this for the whole buffer.
When ALL is symbol `narrow', update statistics only in the accessible
portion of the buffer."
  (interactive "P")
  (save-excursion
    (save-restriction
      (unless (eq all 'narrow) (widen))
      (let* ((cookie-re "\\(\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)\\)")
	     (box-re "^[ \t]*\\([-+*]\\|\\([0-9]+\\|[A-Za-z]\\)[.)]\\)[ \t]+\
\\(?:\\[@\\(?:start:\\)?\\([0-9]+\\|[A-Za-z]\\)\\][ \t]*\\)?\\(\\[[- X]\\]\\)")
             (cookie-data (or (org-entry-get nil "COOKIE_DATA") ""))
	     (recursivep
	      (or (not org-checkbox-hierarchical-statistics)
	          (string-match-p "\\<recursive\\>" cookie-data)))
	     (within-inlinetask (and (not all)
				     (featurep 'org-inlinetask)
                                     (fboundp 'org-inlinetask-in-task-p)
				     (org-inlinetask-in-task-p)))
	     (end (cond (all (point-max))
		        (within-inlinetask
		         (save-excursion (outline-next-heading) (point)))
		        (t (save-excursion
			     (org-with-limited-levels (outline-next-heading))
			     (point)))))
	     (count-boxes
	      (lambda (item structs recursivep)
	        ;; Return number of checked boxes and boxes of all types
	        ;; in all structures in STRUCTS.  If RECURSIVEP is
	        ;; non-nil, also count boxes in sub-lists.  If ITEM is
	        ;; nil, count across the whole structure, else count only
	        ;; across subtree whose ancestor is ITEM.
	        (let ((c-on 0) (c-all 0))
	          (dolist (s structs (list c-on c-all))
		    (let* ((pre (org-list-prevs-alist s))
			   (par (org-list-parents-alist s))
			   (items
			    (cond
			     ((and recursivep item) (org-list-get-subtree item s))
			     (recursivep (mapcar #'car s))
			     (item (org-list-get-children item s par))
			     (t (org-list-get-all-items
			         (org-list-get-top-point s) s pre))))
			   (cookies (delq nil (mapcar
					     (lambda (e)
					       (org-list-get-checkbox e s))
					     items))))
		      (cl-incf c-all (length cookies))
		      (cl-incf c-on (cl-count "[X]" cookies :test #'equal)))))))
	     cookies-list cache)
        ;; Move to start.
        (cond (all (goto-char (point-min)))
	      (within-inlinetask (org-back-to-heading t))
	      (t (org-with-limited-levels (org-back-to-heading-or-point-min t))))
        ;; Build an alist for each cookie found.  The key is the position
        ;; at beginning of cookie and values ending position, format of
        ;; cookie, number of checked boxes to report and total number of
        ;; boxes.
        (while (re-search-forward cookie-re end t)
          (let ((context (save-excursion (backward-char)
				         (save-match-data (org-element-context)))))
	    (when (and (org-element-type-p context 'statistics-cookie)
                       (not (string-match-p "\\<todo\\>" cookie-data)))
	      (push
	       (append
	        (list (match-beginning 1) (match-end 1) (match-end 2))
	        (let* ((container
		        (org-element-lineage
		         context
		         '(drawer center-block dynamic-block inlinetask item
			          quote-block special-block verse-block)))
		       (beg (if container
			        (org-element-contents-begin container)
			      (save-excursion
			        (org-with-limited-levels
			         (outline-previous-heading))
			        (point)))))
	          (or (cdr (assq beg cache))
		      (save-excursion
		        (goto-char beg)
		        (let ((end
			       (if container
				   (org-element-contents-end container)
			         (save-excursion
				   (org-with-limited-levels (outline-next-heading))
				   (point))))
			      structs)
		          (while (re-search-forward box-re end t)
			    (let ((element (org-element-at-point)))
			      (when (org-element-type-p element 'item)
			        (push (org-element-property :structure element)
				      structs)
			        ;; Skip whole list since we have its
			        ;; structure anyway.
			        (while (setq element (org-element-lineage
						      element 'plain-list))
			          (goto-char
				   (min (org-element-end element)
				        end))))))
		          ;; Cache count for cookies applying to the same
		          ;; area.  Then return it.
		          (let ((count
			         (funcall count-boxes
				          (and (org-element-type-p
                                                container 'item)
					       (org-element-property
					        :begin container))
				          structs
				          recursivep)))
			    (push (cons beg count) cache)
			    count))))))
	       cookies-list))))
        ;; Apply alist to buffer.
        (dolist (cookie cookies-list)
          (let* ((beg (car cookie))
	         (end (nth 1 cookie))
	         (percent (nth 2 cookie))
	         (checked (nth 3 cookie))
	         (total (nth 4 cookie)))
	    (goto-char beg)
            (org-fold-core-ignore-modifications
	      (insert-and-inherit
	       (if percent (format "[%d%%]" (floor (* 100.0 checked)
					           (max 1 total)))
	         (format "[%d/%d]" checked total)))
	      (delete-region (point) (+ (point) (- end beg))))
            (require 'org-tags-align)
            (defvar org-auto-align-tags)
	    (when org-auto-align-tags (org-fix-tags-on-the-fly))))))))

(defun org-update-checkbox-count-maybe (&optional all)
  "Update checkbox statistics unless turned off by user.
With an optional argument ALL, update them in the whole buffer.
When ALL is symbol `narrow', update statistics only in the accessible
portion of the buffer."
  (when (cdr (assq 'checkbox org-list-automatic-rules))
    (org-update-checkbox-count all))
  (run-hooks 'org-checkbox-statistics-hook))

(provide 'org-list-core)

;;; org-list-core.el ends here
