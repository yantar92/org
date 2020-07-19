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

;;; Code:

(require 'org-macs)
(require 'org-element)

;;; Customization

;;; Core functionality

;;; Buffer-local folding specs

(defvar-local org-fold--spec-priority-list '(outline org-hide-drawer org-hide-block)
  "Priority of folding specs.
If a region has multiple folding specs at the same time, only the
first property from this list will be considered.")

(defvar-local org-fold--spec-with-ellipsis '(outline org-hide-drawer org-hide-block)
  "A list of folding specs, which should be indicated by `org-ellipsis'.")

(defun org-fold--property-symbol-get-create (spec &optional buffer return-only)
  "Return unique symbol suitable to be used as folding SPEC in BUFFER.
If the buffer already have buffer-local setup in `char-property-alias-alist'
and the setup appears to be created for different buffer,
copy the old invisibility state into new buffer-local text properties,
unless RETURN-ONLY is non-nil."
  (if (not (member spec org-fold--spec-priority-list))
      (user-error "%s should be a valid invisibility spec" spec)
    (let* ((buf (or buffer (current-buffer))))
      (let ((local-prop (intern (format "org-fold--spec-%s-%S"
					(symbol-name spec)
					;; (sxhash buf) appears to be not constant over time.
					;; Using buffer-name is safe, since the only place where
					;; buffer-local text property actually matters is an indirect
					;; buffer, where the name cannot be same anyway.
					(sxhash (buffer-name buf))))))
        (prog1
            local-prop
          (unless return-only
	    (with-current-buffer buf
	      (unless (member local-prop (alist-get 'invisible char-property-alias-alist))
		;; copy old property
		(dolist (old-prop (alist-get 'invisible char-property-alias-alist))
		  (org-with-wide-buffer
		   (let* ((pos (point-min))
			  (spec (seq-find (lambda (spec)
					    (string-match-p (symbol-name spec)
							    (symbol-name old-prop)))
					  org-fold--spec-priority-list))
			  (new-prop (org-fold--property-symbol-get-create spec nil 'return-only)))
		     (while (< pos (point-max))
		       (when-let (val (get-text-property pos old-prop))
			 (put-text-property pos (next-single-char-property-change pos old-prop) new-prop val))
		       (setq pos (next-single-char-property-change pos old-prop))))))
		(setq-local char-property-alias-alist
			    (cons (cons 'invisible
					(mapcar (lambda (spec)
						  (org--get-buffer-local-invisible-property-symbol spec nil 'return-only))
						org-fold--spec-priority-list))
				  (remove (assq 'invisible char-property-alias-alist)
					  char-property-alias-alist)))))))))))

;;; API

;;; Modifying folding specs

(defun org-fold-add-folding-spec (spec &optional buffer no-ellipsis-p no-isearch-open-p append)
  "Add a new folding SPEC in BUFFER.

SPEC must be a symbol.  BUFFER can be a buffer to set SPEC in, nil to
set SPEC in current buffer, or 'all to set SPEC in all open `org-mode'
buffers and all future org buffers.  Non-nil optional argument
NO-ELLIPSIS-P means that folded text will not be indicated by
`org-ellipsis'.  Non-nil optional argument NO-ISEARCH-OPEN-P means
that folded text cannot be searched by isearch.  By default, the added
SPEC will have highest priority among the previously defined specs.
When optional APPEND argument is non-nil, SPEC will have the lowest
priority instead.  If SPEC was already defined earlier, it will be
redefined according to provided optional arguments."
  (when (eq buffer 'all)
    (mapc (lambda (buf)
	    (org-fold-add-folding-spec spec buf no-ellipsis-p no-isearch-open-p append))
	  (org-buffer-list))
    (setq-default org-fold--spec-priority-list (delq spec org-fold--spec-priority-list))
    (add-to-list 'org-fold--spec-priority-list spec append)
    (setq-default org-fold--spec-priority-list org-fold--spec-priority-list)
    (setq-default org-fold--spec-with-ellipsis (delq spec org-fold--spec-with-ellipsis))
    (unless no-ellipsis-p
      (add-to-list 'org-fold--spec-with-ellipsis spec)
      (setq-default org-fold--spec-with-ellipsis org-fold--spec-with-ellipsis))
    (setq-default org-fold--isearch-specs (delq spec org-fold--isearch-specs))
    (unless no-isearch-open-p
      (add-to-list 'org-fold--isearch-specs spec)
      (setq-default org-fold--isearch-specs org-fold--isearch-specs)))
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (add-to-invisibility-spec (cons spec (not no-ellipsis-p)))
      (setq org-fold--spec-priority-list (delq spec org-fold--spec-priority-list))
      (add-to-list 'org-fold--spec-priority-list spec append)
      (setq-local org-fold--spec-with-ellipsis (delq spec org-fold--spec-with-ellipsis))
      (unless no-ellipsis-p (add-to-list 'org-fold--spec-with-ellipsis spec))
      (setq-local org-fold--isearch-specs (delq spec org-fold--isearch-specs))
      (unless no-isearch-open-p (add-to-list 'org-fold--isearch-specs spec)))))

(defun org-fold-remove-folding-spec (spec &optional buffer)
  "Remove a folding SPEC in BUFFER.

SPEC must be a symbol.
BUFFER can be a buffer to remove SPEC in, nil to remove SPEC in current buffer,
or 'all to remove SPEC in all open org-mode buffers and all future org buffers."
  (org-fold--check-spec spec)
  (when (eq buffer 'all)
    (mapc (lambda (buf)
	    (org-fold-remove-folding-spec spec buf))
	  (org-buffer-list))
    (setq-default org-fold--spec-priority-list (delq spec org-fold--spec-priority-list))
    (setq-default org-fold--spec-with-ellipsis (delq spec org-fold--spec-with-ellipsis))
    (setq-default org-fold--isearch-specs (delq spec org-fold--isearch-specs)))
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (remove-from-invisibility-spec (cons spec t))
      (remove-from-invisibility-spec spec)
      (setq org-fold--spec-priority-list (delq spec org-fold--spec-priority-list))
      (setq-local org-fold--spec-with-ellipsis (delq spec org-fold--spec-with-ellipsis))
      (setq-local org-fold--isearch-specs (delq spec org-fold--isearch-specs)))))

;;; Searching and examining folded text

(defun org-fold-invisible-p (&optional pos)
  "Non-nil if the character after POS is folded.
If POS is nil, use `point' instead."
  (let ((value (get-char-property (or pos (point)) 'invisible)))
    (cond ((not value) nil)
	  ((memq value org-fold--spec-priority-list) value)
	  (t nil))))

(defun org-fold-get-folding-spec (&optional spec pom)
  "Get folding state SPEC at point or POM.
If SPEC is nil, return a folding spec with highest priority among
present at point or POM.
If SPEC is 'all, return the list of all present folding specs.
Return nil if there is no folding at point or POM.
If SPEC is a valid folding spec, return value is SPEC if the point is
within region folded using SPEC or nil otherwise."
  (org-fold--check-spec spec)
  (org-with-point-at (or pom (point))
    (if (and spec (not (eq spec 'all)))
	(get-char-property (point) (org--get-buffer-local-invisible-property-symbol spec nil t))
      (let ((result))
	(dolist (spec org-fold--spec-priority-list)
	  (let ((val (get-char-property (point) (org--get-buffer-local-invisible-property-symbol spec nil t))))
	    (when val
	      (push val result))))
        (if (eq spec 'all)
            result
          (car (last result)))))))

(defun org-fold-region-at-point (&optional spec pom)
  "Return region folded using SPEC at point or POM.
If SPEC is nil, return the largest possible folded region.
The return value is a cons of beginning and the end of the region.
Return nil when no fold is present at point of POM."
  (org-fold--check-spec spec)
  (org-with-point-at (or pom (point))
    (if spec
	(org--find-text-property-region (point) (org-fold--property-symbol-get-create spec nil t))
      (let ((beg (point))
	    (end (point)))
        (while (get-text-property beg 'invisible)
          (setq beg (or (previous-single-property-change beg 'invisible nil (point-min))
			beg)))
        (while (get-text-property end 'invisible)
          (setq end (or (next-single-property-change end 'invisible nil (point-max))
			end)))
        (cons beg end)))))

(defun org-fold-search-forward (spec &optional limit)
  "Search next region folded via folding SPEC up to LIMIT.
Move point right after the end of the region, to LIMIT, or `point-max'.
The match-data will contain the region."
  (org-fold--check-spec spec)
  (let ((prop-symbol (org-fold--property-symbol-get-create spec nil t)))
    (goto-char (or (next-single-char-property-change (point) prop-symbol nil limit) limit (point-max)))
    (when (and (< (point) (or limit (point-max)))
	       (not (org-fold-get-folding-spec spec)))
      (goto-char (next-single-char-property-change (point) prop-symbol nil limit)))
    (when (org-fold-get-folding-spec spec)
      (let ((region (org-fold-region-at-point spec)))
	(when (< (cdr region) (or limit (point-max)))
	  (goto-char (1+ (cdr region)))
          (set-match-data (list (set-marker (make-marker) (car region) (current-buffer))
				(set-marker (make-marker) (cdr region) (current-buffer)))))))))

;;; Changing visibility (regions, blocks, drawers, headlines)

;;;; Regions visibility

(defun org-fold-region (from to flag spec)
  "Hide or show lines from FROM to TO, according to FLAG.
SPEC is the folding spec, as a symbol."
  (with-silent-modifications
    (if flag
	(put-text-property from to (org-fold--property-symbol-get-create spec) spec)
      (remove-text-properties from to (list (org-fold--property-symbol-get-create spec) nil)))))

;;;; Blocks and drawers visibility

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
	  (let* ((spec (cond ((eq category 'block) 'org-hide-block)
			     ((eq category 'drawer) 'org-hide-drawer)
			     (t 'outline)))
		 (flag
		  (cond ((eq force 'off) nil)
			(force t)
			((eq spec (org-fold-get-folding-spec spec start)) nil)
			(t t))))
	    (org-fold-flag-region start end flag spec))
	  ;; When the block is hidden away, make sure point is left in
	  ;; a visible part of the buffer.
	  (when (invisible-p (max (1- (point)) (point-min)))
	    (goto-char post))
	  ;; Signal success.
	  t)))
     (no-error nil)
     (t
      (user-error (concat (if (eq category 'drawer)
			      "Not at a drawer"
			    "Not at a block")
                          " (%s:%s)")
                  (current-buffer)
                  (point))))))

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

;;;; Headlines visibility

(defun org-fold-hide-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (outline-end-of-heading)
    (org-fold-region (point) (progn (outline-next-preface) (point)) t 'outline)))

(defun org-fold-flag-subtree (flag)
  (save-excursion
    (org-back-to-heading t)
    (org-fold-region (line-end-position)
	     (progn (org-end-of-subtree t) (point))
	     flag
	     'outline)))

(defun org-fold-hide-subtree ()
  "Hide everything after this heading at deeper levels."
  (interactive)
  (org-fold-flag-subtree t))

(defun org-fold-hide-sublevels (levels)
  "Hide everything but the top LEVELS levels of headers, in whole buffer.
This also unhides the top heading-less body, if any.

Interactively, the prefix argument supplies the value of LEVELS.
When invoked without a prefix argument, LEVELS defaults to the level
of the current heading, or to 1 if the current line is not a heading."
  (cl-letf (((symbol-function 'outline-flag-region) #'org-fold-region))
    (outline-hide-sublevels levels)))

(defun org-fold-show-entry ()
  "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (require 'org)
  (save-excursion
    (ignore-errors
      (org-back-to-heading t)
      (org-fold-region
       (line-end-position 0)
       (save-excursion
	 (if (re-search-forward
	      (concat "[\r\n]\\(" (org-get-limited-outline-regexp) "\\)") nil t)
	     (match-beginning 1)
	   (point-max)))
       nil
       'outline)
      (org-cycle-hide-drawers 'children))))

(defun org-fold-show-heading ()
  "Show the current heading and move to its end."
  (org-fold-region (- (point)
 	      (if (bobp) 0
 		(if (and outline-blank-line
			 (eq (char-before (1- (point))) ?\n))
 		    2 1)))
	   (progn (outline-end-of-heading) (point))
	   nil
	   'outline))

(defun org-fold-show-children (&optional level)
  "Show all direct subheadings of this heading.
Prefix arg LEVEL is how many levels below the current level
should be shown.  Default is enough to cause the following
heading to appear."
  (interactive "p")
  (save-excursion
    (org-back-to-heading t)
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
      (org-fold-flag-heading nil)
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
	(org-fold-flag-heading nil)))))

(defun org-fold-show-subtree ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (org-fold-region
   (point) (save-excursion (org-end-of-subtree t t)) nil 'outline))

(defun org-fold-show-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (org-fold-show-children 1000))

;;; Internal functions

(defun org-fold--check-spec (spec)
  "Throw an error if SPEC is not present in `org-fold--spec-priority-list'."
  (unless (and spec (memq spec org-fold--spec-priority-list))
    (user-error "%s is not a valid folding spec." spec)))

;;; Make isearch search in some text hidden via text propertoes

(defvar org-fold--isearch-overlays nil
  "List of overlays temporarily created during isearch.
This is used to allow searching in regions hidden via text properties.
As for [2020-05-09 Sat], Isearch only has special handling of hidden overlays.
Any text hidden via text properties is not revealed even if `search-invisible'
is set to 't.")

(defvar org-fold--isearch-specs '(org-hide-block
			  org-hide-drawer
			  outline)
  "List of text invisibility specs to be searched by isearch.
By default ([2020-05-09 Sat]), isearch does not search in hidden text,
which was made invisible using text properties.  Isearch will be forced
to search in hidden text with any of the listed 'invisible property value.")

(defun org-fold--create-isearch-overlays (beg end)
  "Replace text property invisibility spec by overlays between BEG and END.
All the regions with invisibility text property spec from
`org-fold--isearch-specs' will be changed to use overlays instead
of text properties.  The created overlays will be stored in
`org-fold--isearch-overlays'."
  (let ((pos beg))
    (while (< pos end)
      ;; We need loop below to make sure that we clean all invisible
      ;; properties, which may be nested.
      (while (memq (get-text-property pos 'invisible) org-fold--isearch-specs)
	(let* ((spec (get-text-property pos 'invisible))
               (region (org--find-text-property-region pos (org-fold--property-symbol-get-create spec))))
	  ;; Changing text properties is considered buffer modification.
	  ;; We do not want it here.
	  (with-silent-modifications
            (org-fold-region (car region) (cdr region) nil spec)
	    ;; The overlay is modelled after `outline-flag-region'
	    ;; [2020-05-09 Sat] overlay for 'outline blocks.
	    (let ((o (make-overlay (car region) (cdr region) nil 'front-advance)))
	      (overlay-put o 'evaporate t)
	      (overlay-put o 'invisible spec)
	      ;; `delete-overlay' here means that spec information will be lost
	      ;; for the region. The region will remain visible.
	      (overlay-put o 'isearch-open-invisible #'delete-overlay)
	      (push o org-fold--isearch-overlays)))))
      (setq pos (next-single-property-change pos 'invisible nil end)))))

(defun org-fold--isearch-filter-predicate (beg end)
  "Return non-nil if text between BEG and END is deemed visible by Isearch.
This function is intended to be used as `isearch-filter-predicate'.
Unlike `isearch-filter-visible', make text with 'invisible text property
value listed in `org-fold--isearch-specs' visible to Isearch."
  (org-fold--create-isearch-overlays beg end) ;; trick isearch by creating overlays in place of invisible text
  (isearch-filter-visible beg end))

(defun org-fold--clear-isearch-overlay (ov)
  "Convert OV region back into using text properties."
  (let ((spec (overlay-get ov 'invisible))) ;; ignore deleted overlays
    (when spec
      ;; Changing text properties is considered buffer modification.
      ;; We do not want it here.
      (with-silent-modifications
	(org-fold-region (overlay-start ov) (overlay-end ov) t spec))))
  (when (member ov isearch-opened-overlays)
    (setq isearch-opened-overlays (delete ov isearch-opened-overlays)))
  (delete-overlay ov))

(defun org-fold--clear-isearch-overlays ()
  "Convert overlays from `org--isearch-overlays' back into using text properties."
  (when org-fold--isearch-overlays
    (mapc #'org-fold--clear-isearch-overlay org-fold--isearch-overlays)
    (setq org-fold--isearch-overlays nil)))

;;; Handling changes in folded elements

(defun org-fold--fix-folded-region (from to len)
  "Process changes in folded elements.
If a text was inserted into invisible region, hide the inserted text.
If the beginning/end line of a folded drawer/block was changed, unfold it.
If a valid end line was inserted in the middle of the folded drawer/block, unfold it."
  ;; Re-hide text inserted in the middle of a folded region.
  (unless (equal from to)
    (dolist (spec org-fold--spec-priority-list)
      (let ((spec-to (get-text-property to (org-fold--property-symbol-get-create spec)))
	    (spec-from (get-text-property (max (point-min) (1- from)) (org-fold--property-symbol-get-create spec))))
	(when (and spec-from spec-to (eq spec-to spec-from))
	  (org-flag-region from to t spec-to)))))
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
   (when (get-text-property from (org-fold--property-symbol-get-create 'org-hide-drawer))
     (setq from (previous-single-char-property-change from (org-fold--property-symbol-get-create 'org-hide-drawer))))
   (when (get-text-property from (org-fold--property-symbol-get-create 'org-hide-block))
     (setq from (previous-single-char-property-change from (org-fold--property-symbol-get-create 'org-hide-block))))
   ;; Check folded drawers and blocks.
   (dolist (spec '(org-hide-drawer org-hide-block))
     (let ((pos from)
	   (begin-re (pcase spec
		       ('org-hide-drawer org-drawer-regexp)
                       ;; Group one below contains the type of the block.
                       ('org-hide-block (rx bol
					    (zero-or-more (any " " "\t"))
					    "#+begin"
					    (or ":"
						(seq "_"
						     (group (one-or-more (not (syntax whitespace))))))))))
           ;; To be determined later. May depend on `begin-re' match (i.e. for blocks).
           end-re)
       ;; Move to the first hidden drawer/block.
       (unless (get-text-property pos (org-fold--property-symbol-get-create spec))
	 (setq pos (next-single-char-property-change pos
						     (org-fold--property-symbol-get-create spec))))
       ;; Cycle over all the hidden drawers/blocks.
       (while (< pos to)
	 (save-match-data ; we should not clobber match-data in after-change-functions
	   (let ((fold-begin (and (get-text-property pos (org-fold--property-symbol-get-create spec))
				  pos))
		 (fold-end (next-single-char-property-change pos (org-fold--property-symbol-get-create spec))))
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
			 (pcase spec
			   ('org-hide-drawer org-property-end-re)
			   ('org-hide-block
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
		   (org-fold-region fold-begin fold-end nil spec))))))
	 ;; Move to next hidden drawer/block.
	 (setq pos
               (next-single-char-property-change pos
						 (org-fold--property-symbol-get-create spec))))))))

(provide 'org-fold)

;;; org-fold.el ends here
