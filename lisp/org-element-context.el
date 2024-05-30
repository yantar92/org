;;; org-element-context.el --- Tools to query Org syntax around point         -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

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

;;; Commentary:
;;
;; This library contains additional (to `org-element-at-point' an
;; `org-element-context') functions to query things at point in Org
;; buffers.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element)
(require 'org-regexps)
(require 'org-mode-common)

;;; Querying heading at point

(defun org-before-first-heading-p ()
  "Return non-nil when point is before first heading.
Respect narrowing."
  (let ((headline (org-element-lineage (org-element-at-point) 'headline t)))
    (or (not headline)
        (< (org-element-begin headline) (point-min)))))

(defun org-at-heading-p (&optional invisible-not-ok)
  "Return t if point is on a (possibly invisible) heading line.
If INVISIBLE-NOT-OK is non-nil, an invisible heading line is not ok."
  (save-excursion
    (forward-line 0)
    (and (or (not invisible-not-ok) (not (org-fold-core-folded-p)))
         (org-element-type-p
          (org-element-at-point)
          (if org-called-with-limited-levels
              '(headline) '(headline inlinetask)))
         ;; Some Org code relies upon match data being modified here.
	 (looking-at org-element-headline-re))))

(defun org-in-commented-heading-p (&optional no-inheritance element)
  "Non-nil if point is under a commented heading.
This function also checks ancestors of the current headline,
unless optional argument NO-INHERITANCE is non-nil.

Optional argument ELEMENT contains element at point."
  (unless element
    (setq
     element
     (org-element-lineage
      (org-element-at-point)
      '(headline inlinetask) 'with-self)))
  (if no-inheritance
      (org-element-property :commentedp element)
    (org-element-property-inherited :commentedp element 'with-self)))

(defun org-in-archived-heading-p (&optional no-inheritance element)
  "Non-nil if point is under an archived heading.
This function also checks ancestors of the current headline,
unless optional argument NO-INHERITANCE is non-nil.

Optional argument ELEMENT contains element at point."
  (unless element
    (setq
     element
     (org-element-lineage
      (org-element-at-point)
      '(headline inlinetask) 'with-self)))
  (if no-inheritance
      (org-element-property :archivedp element)
    (org-element-property-inherited :archivedp element 'with-self)))


(defun org-outline-level ()
  "Compute the outline level of the heading at point.

If this is called at a normal headline, the level is the number
of stars.  Use `org-reduced-level' to remove the effect of
`org-odd-levels-only'.  Unlike `org-current-level', this function
takes into consideration inlinetasks."
  (org-with-wide-buffer
   (end-of-line)
   (if (re-search-backward org-element-headline-re nil t)
       (1- (- (match-end 0) (match-beginning 0)))
     0)))

(defun org-current-level ()
  "Return the level of the current entry, or nil if before the first headline.
The level is the number of stars at the beginning of the
headline.  Use `org-reduced-level' to remove the effect of
`org-odd-levels-only'.  Unlike `org-outline-level', this function
ignores inlinetasks."
  (let ((level (org-with-limited-levels (org-outline-level))))
    (and (> level 0) level)))

(defun org-point-at-end-of-empty-headline ()
  "Return non-nil when point is at the end of an empty headline.
If the heading only contains a TODO keyword, it is still considered
empty."
  (and (looking-at-p "[ \t]*$")
       (let ((element (org-element-at-point)))
         (and (org-element-type-p element 'headline)
              (not (org-element-property :priority element))
              (not (org-element-property :tags element))
              (not (org-element-property :title element))))))

(defun org-at-heading-or-item-p ()
  "Return non-nil, when point is at heading or item."
  (org-element-type-p
   (org-element-at-point)
   '(plain-list item headline inlinetask)))

(defun org-first-sibling-p ()
  "Is this heading the first child of its parents?
Respect narrowing."
  (let ((heading (org-element-at-point)))
    (unless (and (org-element-type-p heading 'headline)
                 (<= (org-element-begin heading)
                    (point)
                    (org-element-contents-begin heading)))
      (user-error "Not at a heading"))
    (let* ((start-at-bob? (> (org-element-begin heading) (point-min)))
           (previous (and (not start-at-bob?)
                          (org-element-lineage
                           (org-element-at-point
                            (1- (org-element-begin heading)))
                           '(headline) 'with-self))))
      (and previous (< (org-element-property :true-level previous)
                       (org-element-property :true-level heading))))))

;;; Planning and timestamps

(defsubst org-at-planning-p ()
  "Non-nil when point is on a planning info line."
  (org-element-type-p (org-element-at-point) 'planning))

;; FIXME: It is confusing that the default matching is only against
;; active timestamps.  Also, lax matching is in odds with the parser
;; and may need to be somehow integrated into org-element API.
(defun org-at-timestamp-p (&optional extended)
  "Non-nil if point is inside a timestamp.

By default, the function only consider syntactically valid active
timestamps.  However, the caller may have a broader definition
for timestamps.  As a consequence, optional argument EXTENDED can
be set to the following values

  `inactive'

    Include also syntactically valid inactive timestamps.

  `agenda'

    Include timestamps allowed in Agenda, i.e., those in
    properties drawers, planning lines and clock lines.

  `lax'

    Ignore context.  The function matches any part of the
    document looking like a timestamp.  This includes comments,
    example blocks...

For backward-compatibility with Org 9.0, every other non-nil
value is equivalent to `inactive'.

When at a timestamp, return the position of the point as a symbol
among `bracket', `after', `year', `month', `hour', `minute',
`day' or a number of character from the last know part of the
time stamp.  If diary sexp timestamps, any point inside the timestamp
is considered `day' (i.e. only `bracket', `day', and `after' return
values are possible).

When matching, the match groups are the following:
  group 2: year, if any
  group 3: month, if any
  group 4: day number, if any
  group 5: day name, if any
  group 7: hours, if any
  group 8: minutes, if any"
  (let* ((regexp
          (if extended
              (if (eq extended 'agenda)
                  (rx-to-string
                   `(or (regexp ,org-ts-regexp3)
                        (regexp ,org-element--timestamp-regexp)))
		org-ts-regexp3)
            org-ts-regexp2))
	 (pos (point))
	 (match?
	  (let ((boundaries (org-in-regexp regexp)))
	    (save-match-data
	      (cond ((null boundaries) nil)
		    ((eq extended 'lax) t)
		    (t
		     (or (and (eq extended 'agenda)
			      (or (org-at-planning-p)
				  (org-at-property-p)
				  (and (bound-and-true-p
					org-agenda-include-inactive-timestamps)
				       (org-at-clock-log-p))))
			 (eq 'timestamp
			     (save-excursion
			       (when (= pos (cdr boundaries)) (forward-char -1))
			       (org-element-type (org-element-context)))))))))))
    (cond
     ((not match?)                        nil)
     ((= pos (match-beginning 0))         'bracket)
     ;; Distinguish location right before the closing bracket from
     ;; right after it.
     ((= pos (1- (match-end 0)))          'bracket)
     ((= pos (match-end 0))               'after)
     ((org-pos-in-match-range pos 2)      'year)
     ((org-pos-in-match-range pos 3)      'month)
     ((org-pos-in-match-range pos 7)      'hour)
     ((org-pos-in-match-range pos 8)      'minute)
     ((or (org-pos-in-match-range pos 4)
	  (org-pos-in-match-range pos 5)) 'day)
     ((and (or (match-end 8) (match-end 5))
           (> pos (or (match-end 8) (match-end 5)))
	   (< pos (match-end 0)))
      (- pos (or (match-end 8) (match-end 5))))
     (t                                   'day))))

(defun org-at-date-range-p (&optional inactive-ok)
  "Non-nil if point is inside a date range.

When optional argument INACTIVE-OK is non-nil, also consider
inactive time ranges.

When this function returns a non-nil value, match data is set
according to `org-tr-regexp-both' or `org-tr-regexp', depending
on INACTIVE-OK."
  (save-excursion
    (catch 'exit
      (let ((pos (point)))
	(skip-chars-backward "^[<\r\n")
	(skip-chars-backward "<[")
	(and (looking-at (if inactive-ok org-tr-regexp-both org-tr-regexp))
	     (>= (match-end 0) pos)
	     (throw 'exit t))
	(skip-chars-backward "^<[\r\n")
	(skip-chars-backward "<[")
	(and (looking-at (if inactive-ok org-tr-regexp-both org-tr-regexp))
	     (>= (match-end 0) pos)
	     (throw 'exit t)))
      nil)))

;;; Elements

(defsubst org-at-comment-p ()
  "Return t if cursor is in a commented line."
  (save-match-data (org-element-type-p (org-element-at-point) 'comment)))

(defun org-at-keyword-p ()
  "Return t if cursor is at a keyword-line."
  (when-let ((element (org-element-at-point)))
    (or (org-element-type-p element 'keyword)
        (< (point) (org-element-post-affiliated element)))))

(defun org-at-drawer-p ()
  "Return t if cursor is at a drawer keyword."
  (save-excursion
    (forward-line 0)
    (looking-at org-element-drawer-re-nogroup)))

(defalias 'org-in-block-p #'org-at-block-p)
(defun org-at-block-p (&optional names)
  "Return block name, as a string, if cursor is at a block.
When optional argument NAMES is non-nil, it should be a list of block
names as strings."
  (when names
    (setq names
          (mapcar
           (lambda (name) (intern (format "%s-block" name)))
           names)))
  (let ((element (org-element-at-point)))
    (when
        (org-element-type-p
         element
         (or names
             '( center-block comment-block dynamic-block
                example-block export-block quote-block
                special-block src-block verse-block)))
      (string-match "\\([^-]+\\)-block" (symbol-name (org-element-type element)))
      (match-string 1 (symbol-name (org-element-type element))))))

(defun org-in-clocktable-p ()
  "Check if the cursor is in a clocktable."
  (when-let ((element (org-element-lineage (org-element-at-point) '(dynamic-block))))
    (when (equal "clocktable" (org-element-property :block-name element))
      (org-element-post-affiliated element))))

(defalias 'org-at-src-block-p #'org-in-src-block-p)
(defun org-in-src-block-p (&optional inside element)
  "Return t when point is at a source block element.
When INSIDE is non-nil, return t only when point is between #+BEGIN_SRC
and #+END_SRC lines.

Note that affiliated keywords and blank lines after are considered a
part of a source block.

When ELEMENT is provided, it is considered to be element at point."
  (setq element (or element (save-match-data (org-element-at-point))))
  (when (org-element-type-p element 'src-block)
    (or (not inside)
        (not (or (<= (line-beginning-position)
                  (org-element-post-affiliated element))
               (>= (line-end-position)
                  (org-with-point-at (org-element-end element)
                    (skip-chars-backward " \t\n\r")
                    (point))))))))

(defalias 'org-babel-where-is-src-block-head #'org-src-block-head)
(defun org-src-block-head (&optional src-block)
  "Find where the current source block begins.

If optional argument SRC-BLOCK is `src-block' type element, find
its current beginning instead.

Return the point at the beginning of the current source block.
Specifically at the beginning of the #+BEGIN_SRC line.  Also set
`match-data' relatively to `org-babel-src-block-regexp', which see.
If the point is not on a source block or within blank lines after an
src block, then return nil."
  (let ((element (or src-block (org-element-at-point))))
    (when (org-element-type-p element 'src-block)
      (let ((end (org-element-end element)))
	(org-with-wide-buffer
	 ;; Ensure point is not on a blank line after the block.
	 (forward-line 0)
	 (skip-chars-forward " \r\t\n" end)
	 (when (< (point) end)
	   (prog1 (goto-char (org-element-post-affiliated element))
	     (looking-at org-babel-src-block-regexp))))))))

(defun org-at-property-drawer-p ()
  "Non-nil when point is at the first line of a property drawer."
  (let ((pdrawer (org-element-at-point)))
    (and (org-element-type-p pdrawer 'property-drawer)
         (< (point) (org-element-contents-begin pdrawer)))))

(defun org-at-property-p ()
  "Non-nil when point is inside a property drawer.
See `org-property-re' for match data, if applicable."
  (and (org-element-type-p (org-element-at-point) 'node-property)
       (org-match-line org-property-re)))

(defun org-at-clock-log-p ()
  "Non-nil if point is on a clock log line."
  (org-element-type-p (save-match-data (org-element-at-point)) 'clock))


;;; Tables

(defun org-at-TBLFM-p (&optional pos)
  "Non-nil when point (or POS) is in #+TBLFM line."
  (save-excursion
    (goto-char (or pos (point)))
    (forward-line 0)
    (and (let ((case-fold-search t)) (looking-at org-TBLFM-regexp))
	 (org-element-type-p (org-element-at-point) 'table))))

(defun org-at-table-p (&optional table-type)
  "Non-nil if the cursor is inside an Org table.
If TABLE-TYPE is non-nil, also check for table.el-type tables."
  (and (org-match-line (if table-type "[ \t]*[|+]" "[ \t]*|"))
       (or (not (derived-mode-p 'org-mode))
	   (let ((e (org-element-lineage (org-element-at-point) 'table t)))
	     (and e (or table-type
			(eq 'org (org-element-property :type e))))))))

(defun org-at-table.el-p ()
  "Non-nil when point is at a table.el table."
  (and (org-match-line "[ \t]*[|+]")
       (let ((element (org-element-at-point)))
	 (and (org-element-type-p element 'table)
	      (eq (org-element-property :type element) 'table.el)))))

(defun org-at-table-hline-p ()
  "Non-nil when point is inside a hline in a table.
Assume point is already in a table."
  (org-match-line org-table-hline-regexp))

(defun org-table-check-inside-data-field (&optional noerror assume-table)
  "Non-nil when point is inside a table data field.
Raise an error otherwise, unless NOERROR is non-nil.  In that
case, return nil if point is not inside a data field.  When
optional argument ASSUME-TABLE is non-nil, assume point is within
a table."
  (cond ((and (or assume-table (org-at-table-p))
	      (not (save-excursion (skip-chars-backward " \t") (bolp)))
	      (not (org-at-table-hline-p))
	      (not (looking-at-p "[ \t]*$"))))
	(noerror nil)
	(t (user-error "Not in table data field"))))

;;; Objects

(defalias 'org-at-latex-p #'org-inside-LaTeX-fragment-p)
(defsubst org-inside-LaTeX-fragment-p (&optional element)
  "Test if point is inside a LaTeX fragment or environment.

When optional argument ELEMENT is non-nil, it should be element/object
at point."
  (org-element-type-p
   (or element (org-element-context))
   '(latex-fragment latex-environment)))

(defun org-inside-latex-macro-p ()
  "Is point inside a LaTeX macro or its arguments?"
  (save-match-data (org-in-regexp org-element--latex-macro-re)))

(defsubst org-in-verbatim-emphasis ()
  "Return non-nil, when point is at code or verbatim emphasis."
  (org-element-lineage (org-element-context) '(code verbatim)))

(defun org-at-item-p ()
  "Is point in a line starting a hand-formatted item?
Modify match data, matching against `org-item-re'."
  (save-excursion
    (forward-line 0)
    (and
     (org-element-type-p
      (org-element-at-point)
      '(plain-list item))
     ;; Set match data.
     (looking-at (org-item-re)))))

;;; `org-context'

(declare-function org-table-begin "org-table-core" (&optional table-type))
(declare-function org-table-end "org-table-core" (&optional table-type))
(declare-function org-end-of-item "org-list-commands" ())
(declare-function org-at-item-checkbox-p "org-list" ())
(defun org-context ()
  "Return a list of contexts of the current cursor position.
If several contexts apply, all are returned.
Each context entry is a list with a symbol naming the context, and
two positions indicating start and end of the context.  Possible
contexts are:

:headline         anywhere in a headline
:headline-stars   on the leading stars in a headline
:todo-keyword     on a TODO keyword (including DONE) in a headline
:tags             on the TAGS in a headline
:priority         on the priority cookie in a headline
:item             on the first line of a plain list item
:item-bullet      on the bullet/number of a plain list item
:checkbox         on the checkbox in a plain list item
:table            in an Org table
:table-special    on a special filed in a table
:table-table      in a table.el table
:clocktable       in a clocktable
:src-block        in a source block
:link             on a hyperlink
:keyword          on a keyword: SCHEDULED, DEADLINE, CLOSE, COMMENT.
:latex-fragment   on a LaTeX fragment
:latex-preview    on a LaTeX fragment with overlaid preview image

This function expects the position to be visible because it uses font-lock
faces as a help to recognize the following contexts: :table-special, :link,
and :keyword."
  (let* ((f (get-text-property (point) 'face))
	 (faces (if (listp f) f (list f)))
	 (case-fold-search t)
	 (p (point)) clist o)
    ;; First the large context
    (cond
     ((org-at-heading-p)
      (push (list :headline (line-beginning-position)
                  (line-end-position))
            clist)
      (when (progn
	      (forward-line 0)
	      (looking-at org-todo-line-tags-regexp))
	(push (org-point-in-group p 1 :headline-stars) clist)
	(push (org-point-in-group p 2 :todo-keyword) clist)
	(push (org-point-in-group p 4 :tags) clist))
      (goto-char p)
      (skip-chars-backward "^[\n\r \t") (or (bobp) (backward-char 1))
      (when (looking-at "\\[#[A-Z0-9]\\]")
	(push (org-point-in-group p 0 :priority) clist)))

     ((org-at-item-p)
      (require 'org-list-commands)
      (push (org-point-in-group p 2 :item-bullet) clist)
      (push (list :item (line-beginning-position)
		  (save-excursion (org-end-of-item) (point)))
	    clist)
      (and (org-at-item-checkbox-p)
	   (push (org-point-in-group p 0 :checkbox) clist)))

     ((org-at-table-p)
      (require 'org-table-core)
      (push (list :table (org-table-begin) (org-table-end)) clist)
      (when (memq 'org-formula faces)
	(push (list :table-special
		    (previous-single-property-change p 'face)
		    (next-single-property-change p 'face))
	      clist)))
     ((org-at-table-p 'any)
      (push (list :table-table) clist)))
    (goto-char p)

    (let ((case-fold-search t))
      ;; New the "medium" contexts: clocktables, source blocks
      (cond ((org-in-clocktable-p)
	     (push (list :clocktable
			 (and (or (looking-at "[ \t]*\\(#\\+BEGIN: clocktable\\)")
				  (re-search-backward "[ \t]*\\(#+BEGIN: clocktable\\)" nil t))
			      (match-beginning 1))
			 (and (re-search-forward "[ \t]*#\\+END:?" nil t)
			      (match-end 0)))
		   clist))
	    ((org-in-src-block-p)
	     (push (list :src-block
			 (and (or (looking-at "[ \t]*\\(#\\+BEGIN_SRC\\)")
				  (re-search-backward "[ \t]*\\(#+BEGIN_SRC\\)" nil t))
			      (match-beginning 1))
			 (and (search-forward "#+END_SRC" nil t)
			      (match-beginning 0)))
		   clist))))
    (goto-char p)

    ;; Now the small context
    (cond
     ((org-at-timestamp-p)
      (push (org-point-in-group p 0 :timestamp) clist))
     ((memq 'org-link faces)
      (push (list :link
		  (previous-single-property-change p 'face)
		  (next-single-property-change p 'face))
	    clist))
     ((memq 'org-special-keyword faces)
      (push (list :keyword
		  (previous-single-property-change p 'face)
		  (next-single-property-change p 'face))
	    clist))
     ((setq o (cl-some
	       (lambda (o)
		 (and (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
		      o))
	       (overlays-at (point))))
      (push (list :latex-fragment
		  (overlay-start o) (overlay-end o))
	    clist)
      (push (list :latex-preview
		  (overlay-start o) (overlay-end o))
	    clist))
     ((org-inside-LaTeX-fragment-p)
      ;; FIXME: positions wrong.
      (push (list :latex-fragment (point) (point)) clist)))

    (setq clist (nreverse (delq nil clist)))
    clist))

(provide 'org-element-context)

;;; org-element-context.el ends here


