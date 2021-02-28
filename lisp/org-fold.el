;;; org-fold.el --- Folding of Org entries -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2020 Free Software Foundation, Inc.
;;
;; Author: Ihor Radchenko <yantar92 at gmail dot com>
;; Keywords: folding, invisible text
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

;; This file contains code handling temporary invisibility (folding
;; and unfolding) of text in org buffers.

;; The file implements the following functionality:
;; - Folding/unfolding text regions and org elements
;; - Searching and examining boundaries of folded text
;; - Revealing text around point
;; - Interactive searching in folded text (via isearch)
;; - Handling edits in folded text
;; - Killing/yanking (copying/pasting) of the folded text

;;; Folding/unfolding text regions and org elements

;; User can temporarily fold/unfold arbitrary regions or text inside
;; headlines, blocks, or drawers.

;; Internally, different types of elements are marked with different
;; folding specs (see `org-fold--spec-priority-list' for the list of available
;; specs).  Overlapping folds marked with the same folding spec are
;; automatically merged, while folds with different folding specs can
;; coexist and be folded/unfolded independently.

;; By default, we define tree types of folding specs:
;; - for headlines
;; - for all kinds of blocks
;; - for drawers (including property drawers)

;; If necessary, one can add extra folding specs using
;; `org-fold-add-folding-spec'.

;; Because of details of implementation of the folding, it is not
;; recommended to set text visibility in org buffer directly by
;; setting 'invisible text property to anything other than t.  While
;; this should usually work just fine, normal org folding can be
;; broken if one sets 'invisible text property to a value not listed
;; in `buffer-invisibility-spec'.

;; It is also possible to use this library if one wants to show some
;; parts of otherwise hidden text.  `org-fold-add-folding-spec' has an option
;; to define a new spec, that does not hide folded text.  This option,
;; for example, is used to hide some parts of org-mode links:

;; Consider the following link:
;; [[file:/path/to/file/file.ext][description]]
;; Only the word "description" is normally visible in this link.
;; 
;; The way this partial visibility is achieved is combining two folding specs:
;; 1. 'org-link, which is a normal spec hiding text
;; 2. 'org-link-description, which is a "visible" folding spec
;; 'org-link-description spec is defined with higher priority (it is
;; added first) in comparison with 'org-link:
;;
;; (org-fold-add-folding-spec 'org-link-description nil t                'no-isearch-open 'append 'visible)
;; (org-fold-add-folding-spec 'org-link             nil 'hide-completely 'no-isearch-open 'append nil)
;;
;; Then, the whole link is folded using 'org-link folding spec, but
;; the visible part is additionally folded using
;; 'org-link-description:
;;
;; <begin fold: 'org-link>[[file:/path/to/file/file.ext][<begin fold: 'org-link-description>description<end fold: 'org-link-description>]]<end fold: 'org-link>
;; 
;; Because 'org-link-description is visible folding spec and has
;; higher priority than 'org-link, it suppresses all the
;; lower-priority specs and thus reveal the description part of the
;; link.
;;
;; Note that the above call to `org-fold-add-folding-spec' used 'append
;; argument, so that default outline/drawer/block folding is ensured
;; to be prioritised over the link description.

;;; Searching and examining boundaries of folded text

;; It is possible to examine folding specs (there may be several) of
;; text at point or search for regions with the same folding spec.

;; If one wants to search invisible text without using functions
;; defined below, it is important to keep in mind that 'invisible text
;; property in org buffers may have multiple possible values (not just nil
;; and t). Hence, (next-single-char-property-change pos 'invisible) is
;; not guarantied to return the boundary of invisible/visible text.

;;; Revealing text around point

;; In addition to unfolding individual org elements, it is possible to
;; unfold meaningful headline structure around point using
;; `org-fold-show-context' and `org-fold-reveal'.  There are several possible variants
;; of structure to be revealed.  See `org-fold-show-context-detail' for the
;; details.

;;; Handling edits inside invisible text

;; Accidental user edits inside invisible text may easily mess up org
;; documents.  Here, we provide a framework to catch such edits and
;; throw error if necessary.  This framework is used, for example, by
;; `org-self-insert-command' and `org-delete-backward-char', See
;; `org-fold-catch-invisible-edits' for available customisation.

;; Some edits inside folded text are not accidental and done by
;; various Org functions.  Setting scheduled time, deadlines,
;; properties, etc often involve adding or changing text insided
;; folded headlines or drawers.  Normally, such edits do not reveal
;; the folded text.  However, the edited text is revealed when
;; document structure is disturbed by edits.  See more details in
;; `org-fold--fix-folded-region'.

;;; Code:

(require 'org-macs)
(require 'org-fold-core)

(defvar org-inlinetask-min-level)

(declare-function isearch-filter-visible "isearch" (beg end))
(declare-function org-element-type "org-element" (element))
(declare-function org-element-property "org-element" (property element))

;;; Customization

(defgroup org-fold-reveal-location nil
  "Options about how to make context of a location visible."
  :tag "Org Reveal Location"
  :group 'org-structure)

(defcustom org-fold-show-context-detail '((agenda . local)
				  (bookmark-jump . lineage)
				  (isearch . lineage)
				  (default . ancestors))
  "Alist between context and visibility span when revealing a location.

\\<org-mode-map>Some actions may move point into invisible
locations.  As a consequence, Org always exposes a neighborhood
around point.  How much is shown depends on the initial action,
or context.  Valid contexts are

  agenda         when exposing an entry from the agenda
  org-goto       when using the command `org-goto' (`\\[org-goto]')
  occur-tree     when using the command `org-occur' (`\\[org-sparse-tree] /')
  tags-tree      when constructing a sparse tree based on tags matches
  link-search    when exposing search matches associated with a link
  mark-goto      when exposing the jump goal of a mark
  bookmark-jump  when exposing a bookmark location
  isearch        when exiting from an incremental search
  default        default for all contexts not set explicitly

Allowed visibility spans are

  minimal        show current headline; if point is not on headline,
                 also show entry

  local          show current headline, entry and next headline

  ancestors      show current headline and its direct ancestors; if
                 point is not on headline, also show entry

  lineage        show current headline, its direct ancestors and all
                 their children; if point is not on headline, also show
                 entry and first child

  tree           show current headline, its direct ancestors and all
                 their children; if point is not on headline, also show
                 entry and all children

  canonical      show current headline, its direct ancestors along with
                 their entries and children; if point is not located on
                 the headline, also show current entry and all children

As special cases, a nil or t value means show all contexts in
`minimal' or `canonical' view, respectively.

Some views can make displayed information very compact, but also
make it harder to edit the location of the match.  In such
a case, use the command `org-fold-reveal' (`\\[org-fold-reveal]') to show
more context."
  :group 'org-fold-reveal-location
  :version "26.1"
  :package-version '(Org . "9.0")
  :type '(choice
	  (const :tag "Canonical" t)
	  (const :tag "Minimal" nil)
	  (repeat :greedy t :tag "Individual contexts"
		  (cons
		   (choice :tag "Context"
			   (const agenda)
			   (const org-goto)
			   (const occur-tree)
			   (const tags-tree)
			   (const link-search)
			   (const mark-goto)
			   (const bookmark-jump)
			   (const isearch)
			   (const default))
		   (choice :tag "Detail level"
			   (const minimal)
			   (const local)
			   (const ancestors)
			   (const lineage)
			   (const tree)
			   (const canonical))))))

(defvar org-fold-reveal-start-hook nil
  "Hook run before revealing a location.")

(defcustom org-fold-catch-invisible-edits 'smart
  "Check if in invisible region before inserting or deleting a character.
Valid values are:

nil              Do not check, so just do invisible edits.
error            Throw an error and do nothing.
show             Make point visible, and do the requested edit.
show-and-error   Make point visible, then throw an error and abort the edit.
smart            Make point visible, and do insertion/deletion if it is
                 adjacent to visible text and the change feels predictable.
                 Never delete a previously invisible character or add in the
                 middle or right after an invisible region.  Basically, this
                 allows insertion and backward-delete right before ellipses.
                 FIXME: maybe in this case we should not even show?"
  :group 'org-edit-structure
  :version "24.1"
  :type '(choice
	  (const :tag "Do not check" nil)
	  (const :tag "Throw error when trying to edit" error)
	  (const :tag "Unhide, but do not do the edit" show-and-error)
	  (const :tag "Show invisible part and do the edit" show)
	  (const :tag "Be smart and do the right thing" smart)))

;;; Core functionality

;;; API

;;;; Modifying folding specs

(defalias org-fold-folding-spec-p #'org-fold-core-folding-spec-p)
(defalias org-fold-add-folding-spec #'org-fold-core-add-folding-spec)
(defalias org-fold-remove-folding-spec #'org-fold-core-remove-folding-spec)

(defun org-fold-initialize ()
  "Setup folding in current Org buffer."
  (setq-local org-fold-core-isearch-open-function #'org-fold--isearch-reveal)
  (setq-local outline-isearch-open-invisible-function #'org-fold--isearch-reveal)
  (setq-local org-fold-core-extend-changed-region-functions (list #'org-fold--extend-changed-region))
  (org-fold-core-initialize '((org-fold-outline
                       (:fragile . #'org-fold--reveal-outline-maybe)
                       (:alias . (headline inlinetask plain-list)))
                      (org-fold-block
                       (:fragile . #'org-fold--reveal-drawer-or-block-maybe)
                       (:alias . ( block center-block comment-block
                                   dynamic-block example-block export-block
                                   quote-block special-block src-block
                                   verse-block)))
                      (org-fold-drawer
                       (:fragile . #'org-fold--reveal-drawer-or-block-maybe)
                       (:alias . (drawer property-drawer))))))

;;;; Searching and examining folded text

(defalias org-fold-folded-p #'org-fold-core-folded-p)
(defalias org-fold-get-folding-spec #'org-fold-core-get-folding-spec)
(defalias org-fold-get-folding-specs-in-region #'org-fold-core-get-folding-specs-in-region)
(defalias org-fold-get-region-at-point #'org-fold-core-get-region-at-point)
(defalias org-fold-next-visibility-change #'org-fold-core-next-visibility-change)
(defalias org-fold-previous-visibility-change #'org-fold-core-previous-visibility-change)
(defalias org-fold-next-folding-state-change #'org-fold-core-next-folding-state-change)
(defalias org-fold-previous-folding-state-change #'org-fold-core-previous-folding-state-change)
(defalias org-fold-search-forward #'org-fold-core-search-forward)

;;;; Changing visibility (regions, blocks, drawers, headlines)

;;;;; Region visibility

(defalias org-fold-region #'org-fold-core-region)

(defun org-fold-show-all (&optional types)
  "Show all contents in the visible part of the buffer.
By default, the function expands headings, blocks and drawers.
When optional argument TYPES is a list of symbols among `blocks',
`drawers' and `headings', to only expand one specific type."
  (interactive)
  (dolist (type (or types '(blocks drawers headings)))
    (org-fold-region (point-min) (point-max) nil
	     (pcase type
	       (`blocks 'block)
	       (`drawers 'drawer)
	       (`headings 'headline)
	       (_ (error "Invalid type: %S" type))))))

(defun org-fold-flag-above-first-heading (&optional arg)
  "Hide from bob up to the first heading.
Move point to the beginning of first heading or end of buffer."
  (goto-char (point-min))
  (unless (org-at-heading-p)
    (outline-next-heading))
  (unless (bobp)
    (org-fold-region 1 (1- (point)) (not arg) 'headline)))

;;;;; Heading visibility

(defun org-fold-heading (flag &optional entry)
  "Fold/unfold the current heading.  FLAG non-nil means make invisible.
When ENTRY is non-nil, show the entire entry."
  (save-excursion
    (org-back-to-heading t)
    ;; Check if we should show the entire entry
    (if (not entry)
	(org-fold-region
	 (line-end-position 0) (line-end-position) flag 'headline)
      (org-fold-show-entry)
      (save-excursion
	;; FIXME: potentially catches inlinetasks
	(and (outline-next-heading)
	     (org-fold-heading nil))))))

(defun org-fold-hide-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (when (org-at-heading-p) (forward-line))
    (unless (eobp) ; Current headline is empty and ends at the end of buffer.
      (org-fold-region
       (line-end-position 0)
       (save-excursion
         (if (re-search-forward
 	      (concat "[\r\n]" (org-get-limited-outline-regexp)) nil t)
             (line-end-position 0)
 	   (point-max)))
       t
       'headline))))

(defun org-fold-subtree (flag)
  (save-excursion
    (org-back-to-heading t)
    (org-fold-region (line-end-position)
	     (progn (org-end-of-subtree t) (point))
	     flag
	     'headline)))

(defun org-fold-hide-subtree ()
  "Hide everything after this heading at deeper levels."
  (interactive)
  (org-fold-subtree t))

(defun org-fold-hide-sublevels (levels)
  "Hide everything but the top LEVELS levels of headers, in whole buffer.
This also unhides the top heading-less body, if any.

Interactively, the prefix argument supplies the value of LEVELS.
When invoked without a prefix argument, LEVELS defaults to the level
of the current heading, or to 1 if the current line is not a heading."
  (interactive (list
		(cond
		 (current-prefix-arg (prefix-numeric-value current-prefix-arg))
		 ((save-excursion (beginning-of-line)
				  (looking-at outline-regexp))
		  (funcall outline-level))
		 (t 1))))
  (if (< levels 1)
      (error "Must keep at least one level of headers"))
  (save-excursion
    (let* ((beg (progn
                  (goto-char (point-min))
                  ;; Skip the prelude, if any.
                  (unless (org-at-heading-p) (outline-next-heading))
                  (point)))
           (end (progn
                  (goto-char (point-max))
                  ;; Keep empty last line, if available.
                  (max (point-min) (if (bolp) (1- (point)) (point))))))
      (if (< end beg)
	  (setq beg (prog1 end (setq end beg))))
      ;; First hide everything.
      (org-fold-region beg end t 'headline)
      ;; Then unhide the top level headers.
      (org-map-region
       (lambda ()
	 (when (<= (funcall outline-level) levels)
           (org-fold-heading nil)))
       beg end)
      ;; Finally unhide any trailing newline.
      (goto-char (point-max))
      (if (and (bolp) (not (bobp)) (outline-invisible-p (1- (point))))
          (org-fold-region (max (point-min) (1- (point))) (point) nil)))))

(defun org-fold-show-entry ()
  "Show the body directly following its heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (org-fold-region
     (line-end-position 0)
     (save-excursion
       (if (re-search-forward
 	    (concat "[\r\n]\\(" (org-get-limited-outline-regexp) "\\)") nil t)
 	   (match-beginning 1)
 	 (point-max)))
     nil
     'headline)
    (org-cycle-hide-drawers 'children)))

;; FIXME: defalias instead?
(defun org-fold-show-hidden-entry ()
  "Show an entry where even the heading is hidden."
  (save-excursion
    (org-fold-show-entry)))

(defun org-fold-show-siblings ()
  "Show all siblings of the current headline."
  (save-excursion
    (while (org-goto-sibling) (org-fold-heading nil)))
  (save-excursion
    (while (org-goto-sibling 'previous)
      (org-fold-heading nil))))

(defun org-fold-show-children (&optional level)
  "Show all direct subheadings of this heading.
Prefix arg LEVEL is how many levels below the current level
should be shown.  Default is enough to cause the following
heading to appear."
  (interactive "p")
   (unless (org-before-first-heading-p)
     (save-excursion
       (org-with-limited-levels (org-back-to-heading t))
       (let* ((current-level (funcall outline-level))
 	     (max-level (org-get-valid-level
 			 current-level
 			 (if level (prefix-numeric-value level) 1)))
 	     (end (save-excursion (org-end-of-subtree t t)))
 	     (regexp-fmt "^\\*\\{%d,%s\\}\\(?: \\|$\\)")
 	     (past-first-child nil)
 	     ;; Make sure to skip inlinetasks.
 	     (re (format regexp-fmt
 			 current-level
 			 (cond
 			  ((not (featurep 'org-inlinetask)) "")
 			  (org-odd-levels-only (- (* 2 org-inlinetask-min-level)
 						  3))
 			  (t (1- org-inlinetask-min-level))))))
 	;; Display parent heading.
 	(org-fold-heading nil)
 	(forward-line)
 	;; Display children.  First child may be deeper than expected
 	;; MAX-LEVEL.  Since we want to display it anyway, adjust
 	;; MAX-LEVEL accordingly.
 	(while (re-search-forward re end t)
 	  (unless past-first-child
 	    (setq re (format regexp-fmt
 			     current-level
 			     (max (funcall outline-level) max-level)))
 	    (setq past-first-child t))
 	  (org-fold-heading nil))))))

(defun org-fold-show-subtree ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (org-fold-region
   (point) (save-excursion (org-end-of-subtree t t)) nil 'headline))

(defun org-fold-show-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (org-fold-show-children 1000))

(defun org-fold-show-branches-buffer ()
  "Show all branches in the buffer."
  (org-fold-flag-above-first-heading)
  (org-fold-hide-sublevels 1)
  (unless (eobp)
    (org-fold-show-branches)
    (while (outline-get-next-sibling)
      (org-fold-show-branches)))
  (goto-char (point-min)))

;;;;; Blocks and drawers visibility

(defun org-fold--hide-wrapper-toggle (element category force no-error)
  "Toggle visibility for ELEMENT.

ELEMENT is a block or drawer type parsed element.  CATEGORY is
either `block' or `drawer'.  When FORCE is `off', show the block
or drawer.  If it is non-nil, hide it unconditionally.  Throw an
error when not at a block or drawer, unless NO-ERROR is non-nil.

Return a non-nil value when toggling is successful."
  (let ((type (org-element-type element)))
    (cond
     ((memq type
	    (pcase category
	      (`drawer '(drawer property-drawer))
	      (`block '(center-block
			comment-block dynamic-block example-block export-block
			quote-block special-block src-block verse-block))
	      (_ (error "Unknown category: %S" category))))
      (let* ((post (org-element-property :post-affiliated element))
	     (start (save-excursion
		      (goto-char post)
		      (line-end-position)))
	     (end (save-excursion
		    (goto-char (org-element-property :end element))
		    (skip-chars-backward " \t\n")
		    (line-end-position))))
	;; Do nothing when not before or at the block opening line or
	;; at the block closing line.
	(unless (let ((eol (line-end-position)))
		  (and (> eol start) (/= eol end)))
	  (let* ((spec (cond ((eq category 'block) (org-fold-get-folding-spec-for-element 'block))
			     ((eq category 'drawer) (org-fold-get-folding-spec-for-element 'drawer))
			     (t (org-fold-get-folding-spec-for-element 'headline))))
		 (flag
		  (cond ((eq force 'off) nil)
			(force t)
			((org-fold-folded-p start) nil)
			(t t))))
	    (org-fold-region start end flag spec))
	  ;; When the block is hidden away, make sure point is left in
	  ;; a visible part of the buffer.
	  (when (invisible-p (max (1- (point)) (point-min)))
	    (goto-char post))
	  ;; Signal success.
	  t)))
     (no-error nil)
     (t
      (user-error (if (eq category 'drawer)
		      "Not at a drawer"
		    "Not at a block"))))))

(defun org-fold-hide-block-toggle (&optional force no-error element)
  "Toggle the visibility of the current block.

When optional argument FORCE is `off', make block visible.  If it
is non-nil, hide it unconditionally.  Throw an error when not at
a block, unless NO-ERROR is non-nil.  When optional argument
ELEMENT is provided, consider it instead of the current block.

Return a non-nil value when toggling is successful."
  (interactive)
  (org-fold--hide-wrapper-toggle
   (or element (org-element-at-point)) 'block force no-error))

(defun org-fold-hide-drawer-toggle (&optional force no-error element)
  "Toggle the visibility of the current drawer.

When optional argument FORCE is `off', make drawer visible.  If
it is non-nil, hide it unconditionally.  Throw an error when not
at a drawer, unless NO-ERROR is non-nil.  When optional argument
ELEMENT is provided, consider it instead of the current drawer.

Return a non-nil value when toggling is successful."
  (interactive)
  (org-fold--hide-wrapper-toggle
   (or element (org-element-at-point)) 'drawer force no-error))

(defun org-fold-hide-block-all ()
  "Fold all blocks in the current buffer."
  (interactive)
  (org-fold-show-all '(blocks))
  (org-block-map 'org-fold-hide-block-toggle))

(defun org-fold-hide-drawer-all ()
  "Fold all visible drawers in the current buffer or narrow."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-drawer-regexp nil t)
      ;; Skip drawers in folded headings
      (when (org-fold-folded-p) (goto-char (org-fold-next-visibility-change nil nil 'ignore-hidden)))
      (let* ((drawer (org-element-at-point))
	     (type (org-element-type drawer)))
	(when (memq type '(drawer property-drawer))
	  ;; We are sure regular drawers are unfolded because of
	  ;; `org-show-all' call above.  However, property drawers may
	  ;; be folded, or in a folded headline.  In that case, do not
	  ;; re-hide it.
	  (unless (and (eq type 'property-drawer)
		       (org-fold-folded-p))
	    (org-fold-hide-drawer-toggle t nil drawer))
	  ;; Make sure to skip drawer entirely or we might flag it
	  ;; another time when matching its ending line with
	  ;; `org-drawer-regexp'.
	  (goto-char (org-element-property :end drawer)))))))

;;;;; Reveal point location

(defun org-fold-show-context (&optional key)
  "Make sure point and context are visible.
Optional argument KEY, when non-nil, is a symbol.  See
`org-fold-show-context-detail' for allowed values and how much is to
be shown."
  (org-fold-show-set-visibility
   (cond ((symbolp org-fold-show-context-detail) org-fold-show-context-detail)
	 ((cdr (assq key org-fold-show-context-detail)))
	 (t (cdr (assq 'default org-fold-show-context-detail))))))

(defun org-fold-show-set-visibility (detail)
  "Set visibility around point according to DETAIL.
DETAIL is either nil, `minimal', `local', `ancestors', `lineage',
`tree', `canonical' or t.  See `org-fold-show-context-detail' for more
information."
  ;; Show current heading and possibly its entry, following headline
  ;; or all children.
  (if (and (org-at-heading-p) (not (eq detail 'local)))
      (org-fold-heading nil)
    (org-fold-show-entry)
    ;; If point is hidden make sure to expose it.
    (when (org-fold-folded-p)
      (let ((region (org-fold-get-region-at-point))
            (spec (org-fold-get-folding-spec)))
        (org-fold-region (car region) (cdr region) nil)
        ;; Reveal emphasis markers.
        (when (member spec '(org-link org-link-description))
          (let (org-hide-emphasis-markers
                org-link-descriptive
                (region (or (org-find-text-property-region (point) 'org-emphasis)
                            region)))
            (when region
              (org-with-point-at (car region)
                (beginning-of-line)
                (let (font-lock-extend-region-functions)
                  (font-lock-fontify-region (1- (car region)) (cdr region)))))))))
    (unless (org-before-first-heading-p)
      (org-with-limited-levels
       (cl-case detail
	 ((tree canonical t) (org-fold-show-children))
	 ((nil minimal ancestors))
	 (t (save-excursion
	      (outline-next-heading)
	      (org-fold-heading nil)))))))
  ;; Show all siblings.
  (when (eq detail 'lineage) (org-fold-show-siblings))
  ;; Show ancestors, possibly with their children.
  (when (memq detail '(ancestors lineage tree canonical t))
    (save-excursion
      (while (org-up-heading-safe)
	(org-fold-heading nil)
	(when (memq detail '(canonical t)) (org-fold-show-entry))
	(when (memq detail '(tree canonical t)) (org-fold-show-children))))))

(defun org-fold-reveal (&optional siblings)
  "Show current entry, hierarchy above it, and the following headline.

This can be used to show a consistent set of context around
locations exposed with `org-fold-show-context'.

With optional argument SIBLINGS, on each level of the hierarchy all
siblings are shown.  This repairs the tree structure to what it would
look like when opened with hierarchical calls to `org-cycle'.

With a \\[universal-argument] \\[universal-argument] prefix, \
go to the parent and show the entire tree."
  (interactive "P")
  (run-hooks 'org-fold-reveal-start-hook)
  (cond ((equal siblings '(4)) (org-fold-show-set-visibility 'canonical))
	((equal siblings '(16))
	 (save-excursion
	   (when (org-up-heading-safe)
	     (org-fold-show-subtree)
	     (run-hook-with-args 'org-cycle-hook 'subtree))))
	(t (org-fold-show-set-visibility 'lineage))))

;;; Make isearch search in some text hidden via text propertoes

(defun org-fold--isearch-reveal (&rest _)
  "Reveal text at POS found by isearch."
  (org-fold-show-set-visibility 'isearch))

;;; Handling changes in folded elements

(defun org-fold--extend-changed-region (from to)
  "Consider folded regions in the next/previous line when fixing
region visibility.
This function is intended to be used as a member of
`org-fold-core-extend-changed-region-functions'."
  ;; If the edit is done in the first line of a folded drawer/block,
  ;; the folded text is only starting from the next line and needs to
  ;; be checked.
  (setq to (save-excursion (goto-char to) (line-beginning-position 2)))
  ;; If the ":END:" line of the drawer is deleted, the folded text is
  ;; only ending at the previous line and needs to be checked.
  (setq from (save-excursion (goto-char from) (line-beginning-position 0)))
  (cons from to))

(defun org-fold--reveal-outline-maybe (region spec)
  "Reveal folded outline (according to SPEC) in REGION when needed.

This function is intended to be used as :fragile prperty of
`org-fold-outline' spec."
  (save-match-data
    (save-excursion
      (goto-char (car region))
      ;; The line before beginning of the fold should be either a
      ;; headline or a list item.
      (backward-char)
      (beginning-of-line)
      (unless (let ((case-fold-search t))
	        (looking-at (rx (or (regex (org-item-re))
			            (regex org-outline-regexp-bol))))) ; the match-data will be used later
	t))))

(defun org-fold--reveal-drawer-or-block-maybe (region spec)
  "Reveal folded drawer/block (according to SPEC) in REGION when needed.

This function is intended to be used as :fragile prperty of
`org-fold-drawer' or `org-fold-block' spec."
  (let ((begin-re (cond
		   ((eq spec (org-fold-core-get-folding-spec-from-alias 'drawer))
		    org-drawer-regexp)
		   ;; Group one below contains the type of the block.
		   ((eq spec (org-fold-core-get-folding-spec-from-alias 'block))
		    (rx bol (zero-or-more (any " " "\t"))
			"#+begin"
			(or ":"
			    (seq "_"
				 (group (one-or-more (not (syntax whitespace))))))))))
        ;; To be determined later. May depend on `begin-re' match (i.e. for blocks).
        end-re)
    (save-match-data ; we should not clobber match-data in after-change-functions
      (let ((fold-begin (car region))
	    (fold-end (cdr region)))
	(let (unfold?)
	  (catch :exit
	    ;; The line before folded text should be beginning of
	    ;; the drawer/block.
	    (save-excursion
	      (goto-char fold-begin)
	      ;; The line before beginning of the fold should be the
	      ;; first line of the drawer/block.
	      (backward-char)
	      (beginning-of-line)
	      (unless (let ((case-fold-search t))
			(looking-at begin-re)) ; the match-data will be used later
		(throw :exit (setq unfold? t))))
            ;; Set `end-re' for the current drawer/block.
            (setq end-re
		  (cond
		   ((eq spec (org-fold-core-get-folding-spec-from-alias 'drawer))
                    org-property-end-re)
		   ((eq spec (org-fold-core-get-folding-spec-from-alias 'block))
		    (let ((block-type (match-string 1))) ; the last match is from `begin-re'
		      (concat (rx bol (zero-or-more (any " " "\t")) "#+end")
			      (if block-type
				  (concat "_"
					  (regexp-quote block-type)
					  (rx (zero-or-more (any " " "\t")) eol))
				(rx (opt ":") (zero-or-more (any " " "\t")) eol)))))))
	    ;; The last line of the folded text should match `end-re'.
	    (save-excursion
	      (goto-char fold-end)
	      (beginning-of-line)
	      (unless (let ((case-fold-search t))
			(looking-at end-re))
		(throw :exit (setq unfold? t))))
	    ;; There should be no `end-re' or
	    ;; `org-outline-regexp-bol' anywhere in the
	    ;; drawer/block body.
	    (save-excursion
	      (goto-char fold-begin)
	      (when (save-excursion
		      (let ((case-fold-search t))
			(re-search-forward (rx (or (regex end-re)
						   (regex org-outline-regexp-bol)))
					   (max (point)
						(1- (save-excursion
						      (goto-char fold-end)
						      (line-beginning-position))))
					   t)))
		(throw :exit (setq unfold? t)))))
          unfold?)))))

(defun org-fold--fix-folded-region (from to _)
  "Process modifications in folded elements within FROM . TO region.
This function intended to be used as one of `after-change-functions'.

This function does nothing if text the only modification was changing
text properties (for the sake of reducing overheads).

If a text was inserted into invisible region, hide the inserted text.
If the beginning/end line of a folded drawer/block was changed, unfold it.
If a valid end line was inserted in the middle of the folded drawer/block, unfold it."
  ;; If no insertions or deletions in buffer, skip all the checks.
  (unless (eq org-fold--last-buffer-chars-modified-tick (buffer-chars-modified-tick))
    (save-match-data
      ;; Store the new buffer modification state.
      (setq org-fold--last-buffer-chars-modified-tick (buffer-chars-modified-tick))
      ;; Re-hide text inserted in the middle of a folded region.
      (unless (equal from to) ; Ignore deletions.
	(dolist (spec org-fold--spec-priority-list)
	  (let ((spec-to (org-fold-get-folding-spec spec (min to (1- (point-max)))))
		(spec-from (org-fold-get-folding-spec spec (max (point-min) (1- from)))))
	    (when (and spec-from spec-to (eq spec-to spec-from))
	      (org-fold-region from to t (or spec-from spec-to))))))
      ;; Re-hide text inserted right in front (but not at the back) of a
      ;; folded region.
      ;; Examples: beginning of a folded drawer, first line of folded
      ;; headline (schedule).  However, do not hide headline text.
      (unless (equal from to)
	(when (or
	       ;; Prepending to folded headline, block, or drawer.
	       (and (not (org-fold-folded-p (max (point-min) (1- from))))
		    (org-fold-folded-p to)
		    (not (org-at-heading-p)))
	       ;; Appending to folded headline. We cannot append to
	       ;; folded block or drawer though.
               (and (org-fold-folded-p (max (point-min) (1- from)) 'headline)
		    (not (org-fold-folded-p to))))
	  (org-fold-region from to t (or
	                      ;; Only headline spec for appended text.
			      (org-fold-get-folding-spec 'headline (max (point-min) (1- from)))
			      (org-fold-get-folding-spec nil to)))))
      ;; Reveal the whole region if inserted in the middle of
      ;; visible text. This is needed, for example, when one is
      ;; trying to copy text from indirect buffer to main buffer. If
      ;; the text is unfolded in the indirect buffer, but folded in
      ;; the main buffer, the text properties responsible for
      ;; folding will be activated as soon as the text is pasted
      ;; into the main buffer. Thus, we need to unfold the inserted
      ;; text to make org-mode behave as expected (the inserted text
      ;; is visible).
      ;; FIXME: this breaks when replacing buffer/region contents - we do not need to unfold in that case
      ;; (unless (equal from to)
      ;;   (when (and (not (org-fold-folded-p (max (point-min) (1- from)))) (not (org-fold-folded-p to)))
      ;;     (org-fold-region from to nil)))
      ;; Process all the folded text between `from' and `to'.
      (org-with-wide-buffer
       ;; If the edit is done in the first line of a folded drawer/block,
       ;; the folded text is only starting from the next line and needs to
       ;; be checked.
       (setq to (save-excursion (goto-char to) (line-beginning-position 2)))
       ;; If the ":END:" line of the drawer is deleted, the folded text is
       ;; only ending at the previous line and needs to be checked.
       (setq from (save-excursion (goto-char from) (line-beginning-position 0)))
       ;; Expand the considered region to include partially present folded
       ;; drawer/block.
       (when (org-fold-get-folding-spec (org-fold-get-folding-spec-for-element 'drawer) from)
	 (setq from (org-fold-previous-folding-state-change (org-fold-get-folding-spec-for-element 'drawer) from)))
       (when (org-fold-get-folding-spec (org-fold-get-folding-spec-for-element 'block) from)
	 (setq from (org-fold-previous-folding-state-change (org-fold-get-folding-spec-for-element 'block) from)))
       (when (org-fold-get-folding-spec (org-fold-get-folding-spec-for-element 'drawer) to)
	 (setq to (org-fold-next-folding-state-change (org-fold-get-folding-spec-for-element 'drawer) to)))
       (when (org-fold-get-folding-spec (org-fold-get-folding-spec-for-element 'block) to)
	 (setq from (org-fold-next-folding-state-change (org-fold-get-folding-spec-for-element 'block) to)))
       ;; Check folded drawers and blocks.
       (dolist (spec (list (org-fold-get-folding-spec-for-element 'drawer) (org-fold-get-folding-spec-for-element 'block)))
	 (let ((pos from)
	       (begin-re (cond
			  ((eq spec (org-fold-get-folding-spec-for-element 'drawer))
			   org-drawer-regexp)
			  ;; Group one below contains the type of the block.
			  ((eq spec (org-fold-get-folding-spec-for-element 'block))
			   (rx bol (zero-or-more (any " " "\t"))
			       "#+begin"
			       (or ":"
				   (seq "_"
					(group (one-or-more (not (syntax whitespace))))))))))
               ;; To be determined later. May depend on `begin-re' match (i.e. for blocks).
               end-re)
	   ;; Move to the first hidden drawer/block.
	   (unless (org-fold-get-folding-spec spec pos)
	     (setq pos (org-fold-next-folding-state-change spec pos to)))
	   ;; Cycle over all the hidden drawers/blocks.
	   (while (< pos to)
	     (save-match-data ; we should not clobber match-data in after-change-functions
	       (let ((fold-begin (and (org-fold-get-folding-spec spec pos)
				      pos))
		     (fold-end (org-fold-next-folding-state-change spec pos to)))
		 (when (and fold-begin fold-end)
		   (let (unfold?)
		     (catch :exit
		       ;; The line before folded text should be beginning of
		       ;; the drawer/block.
		       (save-excursion
			 (goto-char fold-begin)
			 ;; The line before beginning of the fold should be the
			 ;; first line of the drawer/block.
			 (backward-char)
			 (beginning-of-line)
			 (unless (let ((case-fold-search t))
				   (looking-at begin-re)) ; the match-data will be used later
			   (throw :exit (setq unfold? t))))
                       ;; Set `end-re' for the current drawer/block.
                       (setq end-re
			     (cond
			      ((eq spec (org-fold-get-folding-spec-for-element 'drawer))
                               org-property-end-re)
			      ((eq spec (org-fold-get-folding-spec-for-element 'block))
			       (let ((block-type (match-string 1))) ; the last match is from `begin-re'
				 (concat (rx bol (zero-or-more (any " " "\t")) "#+end")
					 (if block-type
					     (concat "_"
						     (regexp-quote block-type)
						     (rx (zero-or-more (any " " "\t")) eol))
					   (rx (opt ":") (zero-or-more (any " " "\t")) eol)))))))
		       ;; The last line of the folded text should match `end-re'.
		       (save-excursion
			 (goto-char fold-end)
			 (beginning-of-line)
			 (unless (let ((case-fold-search t))
				   (looking-at end-re))
			   (throw :exit (setq unfold? t))))
		       ;; there should be no `end-re' or
		       ;; `org-outline-regexp-bol' anywhere in the
		       ;; drawer/block body.
		       (save-excursion
			 (goto-char fold-begin)
			 (when (save-excursion
				 (let ((case-fold-search t))
				   (re-search-forward (rx (or (regex end-re)
							      (regex org-outline-regexp-bol)))
						      (max (point)
							   (1- (save-excursion
								 (goto-char fold-end)
								 (line-beginning-position))))
						      't)))
			   (throw :exit (setq unfold? t)))))
		     (when unfold?
		       (org-fold-region fold-begin fold-end nil spec)))
		   (goto-char fold-end))))
	     ;; Move to next hidden drawer/block.
	     (setq pos
		   (org-fold-next-folding-state-change spec to)))))))))

;; Catching user edits inside invisible text
(defun org-fold-check-before-invisible-edit (kind)
  "Check is editing if kind KIND would be dangerous with invisible text around.
The detailed reaction depends on the user option `org-fold-catch-invisible-edits'."
  ;; First, try to get out of here as quickly as possible, to reduce overhead
  (when (and org-fold-catch-invisible-edits
	     (or (not (boundp 'visible-mode)) (not visible-mode))
	     (or (org-invisible-p)
		 (org-invisible-p (max (point-min) (1- (point))))))
    ;; OK, we need to take a closer look.  Only consider invisibility
    ;; caused by folding.
    (let* ((invisible-at-point (org-fold-folded-p))
	   ;; Assume that point cannot land in the middle of an
	   ;; overlay, or between two overlays.
	   (invisible-before-point
	    (and (not invisible-at-point)
		 (not (bobp))
		 (org-fold-folded-p (1- (point)))))
	   (border-and-ok-direction
	    (or
	     ;; Check if we are acting predictably before invisible
	     ;; text.
	     (and invisible-at-point
		  (memq kind '(insert delete-backward)))
	     ;; Check if we are acting predictably after invisible text
	     ;; This works not well, and I have turned it off.  It seems
	     ;; better to always show and stop after invisible text.
	     (and (not invisible-at-point) invisible-before-point
		  (memq kind '(insert delete)))
	     )))
      (when (or invisible-at-point invisible-before-point)
	(when (eq org-fold-catch-invisible-edits 'error)
	  (user-error "Editing in invisible areas is prohibited, make them visible first"))
	(if (and org-custom-properties-hidden-p
		 (y-or-n-p "Display invisible properties in this buffer? "))
	    (org-toggle-custom-properties-visibility)
	  ;; Make the area visible
          (save-excursion
	    (org-fold-show-set-visibility 'local))
          (when invisible-before-point
            (org-with-point-at (1- (point)) (org-fold-show-set-visibility 'local)))
	  (cond
	   ((eq org-fold-catch-invisible-edits 'show)
	    ;; That's it, we do the edit after showing
	    (message
	     "Unfolding invisible region around point before editing")
	    (sit-for 1))
	   ((and (eq org-fold-catch-invisible-edits 'smart)
		 border-and-ok-direction)
	    (message "Unfolding invisible region around point before editing"))
	   (t
	    ;; Don't do the edit, make the user repeat it in full visibility
	    (user-error "Edit in invisible region aborted, repeat to confirm with text visible"))))))))

(provide 'org-fold)

;;; org-fold.el ends here
