;;; org-heading.el --- Org heading API and commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>

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

;; This file implements API to match and navigate Org headings.

;;; Code:

(require 'org-fold)
(require 'outline)

(declare-function org-element-lineage-map "org-element-ast"
                  (datum fun &optional types with-self first-match))
(declare-function org-element-at-point "org-element" (&optional pom cached-only))
(declare-function org-element-begin "org-element" (node))
(declare-function org-element-end "org-element" (node))
(declare-function org-element-lineage "org-element-ast" (blob &optional types with-self))
(declare-function org-element-property "org-element-ast" (property node))
(declare-function org-element-property-inherited "org-element-ast"
                  (property node &optional with-self accumulate literal-nil include-nil))
(declare-function org-element-type-p "org-element-ast" (node types))
(declare-function org-element-contents-begin "org-element" (node))

(defvar org-todo-line-regexp)
(defvar org-outline-regexp-bol)

(defcustom org-odd-levels-only nil
  "Non-nil means skip even levels and only use odd levels for the outline.
This has the effect that two stars are being added/taken away in
promotion/demotion commands.  It also influences how levels are
handled by the exporters.
Changing it requires restart of `font-lock-mode' to become effective
for fontification also in regions already fontified.
You may also set this on a per-file basis by adding one of the following
lines to the buffer:

   #+STARTUP: odd
   #+STARTUP: oddeven"
  :group 'org-edit-structure
  :group 'org-appearance
  :type 'boolean)

(defun org-current-level ()
  "Return the level of the current entry, or nil if before the first headline.
The level is the number of stars at the beginning of the
headline.  Use `org-reduced-level' to remove the effect of
`org-odd-levels-only'.  Unlike `org-outline-level', this function
ignores inlinetasks."
  (let ((level (org-with-limited-levels (org-outline-level))))
    (and (> level 0) level)))

(defun org-outline-level ()
  "Compute the outline level of the heading at point.

If this is called at a normal headline, the level is the number
of stars.  Use `org-reduced-level' to remove the effect of
`org-odd-levels-only'.  Unlike `org-current-level', this function
takes into consideration inlinetasks."
  (org-with-wide-buffer
   (end-of-line)
   (if (re-search-backward org-outline-regexp-bol nil t)
       (1- (- (match-end 0) (match-beginning 0)))
     0)))

(defun org-get-previous-line-level ()
  "Return the outline depth of the last headline before the current line.
Returns 0 for the first headline in the buffer, and nil if before the
first headline."
  (and (org-current-level)
       (or (and (/= (line-beginning-position) (point-min))
		(save-excursion (forward-line -1) (org-current-level)))
	   0)))

(defun org-reduced-level (nstars)
  "Compute the effective level of a heading with NSTARS stars.
This takes into account the setting of `org-odd-levels-only'."
  (cond
   ((zerop nstars) 0)
   (org-odd-levels-only (1+ (floor (/ nstars 2))))
   (t nstars)))

(defun org-back-to-heading (&optional invisible-ok)
  "Go back to beginning of visible heading or inlinetask.
When INVISIBLE-OK is non-nil, ignore visibility."
  (forward-line 0)
  (or (and (org-at-heading-p (not invisible-ok))
           (not (and (featurep 'org-inlinetask)
                   (fboundp 'org-inlinetask-end-p)
                   (org-inlinetask-end-p))))
      (unless
          (org-element-lineage-map
              (org-element-at-point)
              (lambda (el)
                (goto-char (org-element-begin el))
                (or invisible-ok (not (org-fold-folded-p))))
            '(headline inlinetask)
            'with-self 'first-match)
        (user-error "Before first headline at position %d in buffer %s"
		    (point) (current-buffer))))
  (point))

(defun org-back-to-heading-or-point-min (&optional invisible-ok)
  "Go back to visible heading or first point in buffer.
If point is before first heading go to first point in buffer
instead of back to heading.
When INVISIBLE-OK is non-nil, ignore visibility."
  (if (org-before-first-heading-p)
      (goto-char (point-min))
    (org-back-to-heading invisible-ok)))

(defun org-before-first-heading-p ()
  "Before first heading?
Respect narrowing."
  (let ((cached (org-element-at-point nil 'cached)))
    (if cached
        (let ((cached-headline (org-element-lineage cached 'headline t)))
          (or (not cached-headline)
              (< (org-element-begin cached-headline) (point-min))))
      (org-with-limited-levels
       (save-excursion
         (end-of-line)
         (null (re-search-backward org-outline-regexp-bol nil t)))))))

(defun org-at-heading-p (&optional invisible-not-ok)
  "Return t if point is on a (possibly invisible) heading line.
If INVISIBLE-NOT-OK is non-nil, an invisible heading line is not ok."
  (save-excursion
    (forward-line 0)
    (and (or (not invisible-not-ok) (not (org-fold-folded-p)))
	 (looking-at outline-regexp))))

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

(defun org-point-at-end-of-empty-headline ()
  "If point is at the end of an empty headline, return t, else nil.
If the heading only contains a TODO keyword, it is still considered
empty."
  (let ((case-fold-search nil))
    (and (looking-at "[ \t]*$")
	 org-todo-line-regexp
	 (save-excursion
	   (forward-line 0)
	   (looking-at org-todo-line-regexp)
	   (string= (match-string 3) "")))))

(defun org-up-heading-all (arg)
  "Move to the heading line of which the present line is a subheading.
This function considers both visible and invisible heading lines.
With argument, move up ARG levels."
  (outline-up-heading arg t))

(defun org-up-heading-safe ()
  "Move to the heading line of which the present line is a subheading.
Return the heading level, as number or nil when there is no such heading.

When point is not at heading, go to the parent of the current heading.
When point is at or inside an inlinetask, go to the containing
heading.

This version will not throw an error.  It will return the level of the
headline found, or nil if no higher level is found.

When no higher level is found, the still move point to the containing
heading, if there is any in the accessible portion of the buffer.

When narrowing is in effect, ignore headings starting before the
available portion of the buffer."
  (let* ((current-heading (org-element-lineage
                           (org-element-at-point)
                           '(headline inlinetask)
                           'with-self))
         (parent (org-element-lineage current-heading 'headline)))
    (if (and parent
             (<= (point-min) (org-element-begin parent)))
        (progn
          (goto-char (org-element-begin parent))
          (org-element-property :level parent))
      (when (and current-heading
                 (<= (point-min) (org-element-begin current-heading)))
        (goto-char (org-element-begin current-heading))
        nil))))

(defun org-up-heading-or-point-min ()
  "Move to the heading line of which the present is a subheading, or point-min.
This version is needed to make point-min behave like a virtual
heading of level 0 for property-inheritance.  It will return the
level of the headline found (down to 0) or nil if already at a
point before the first headline or at point-min."
  (when (ignore-errors (org-back-to-heading t))
    (if (< 1 (funcall outline-level))
	(or (org-up-heading-safe)
            ;; The first heading may not be level 1 heading.
            (goto-char (point-min)))
      (unless (= (point) (point-min)) (goto-char (point-min))))))

(defun org-first-sibling-p ()
  "Is this heading the first child of its parents?"
  (let ((re org-outline-regexp-bol)
	level l)
    (unless (org-at-heading-p t)
      (user-error "Not at a heading"))
    (setq level (funcall outline-level))
    (save-excursion
      (if (not (re-search-backward re nil t))
	  t
	(setq l (funcall outline-level))
	(< l level)))))

(defun org-goto-sibling (&optional previous)
  "Goto the next sibling heading, even if it is invisible.
When PREVIOUS is set, go to the previous sibling instead.  Returns t
when a sibling was found.  When none is found, return nil and don't
move point."
  (let ((fun (if previous 're-search-backward 're-search-forward))
	(pos (point))
	(re org-outline-regexp-bol)
	level l)
    (when (ignore-errors (org-back-to-heading t))
      (when (org-element-type-p (org-element-at-point) 'inlinetask)
        (org-up-heading-safe))
      (setq level (funcall outline-level))
      (catch 'exit
	(or previous (forward-char 1))
	(while (funcall fun re nil t)
	  (setq l (funcall outline-level))
	  (when (< l level) (goto-char pos) (throw 'exit nil))
	  (when (= l level) (goto-char (match-beginning 0)) (throw 'exit t)))
	(goto-char pos)
	nil))))

(defun org-goto-first-child (&optional element)
  "Goto the first child of current heading, even if it is invisible.
Return t when a child was found.  Otherwise don't move point and
return nil.
When optional argument ELEMENT is non-nil, go to the first child of
heading containing ELEMENT."
  (let ((heading (org-element-lineage
                  (or element (org-element-at-point))
                  '(headline inlinetask org-data)
                  'with-self)))
    (when heading
      (unless (or (org-element-type-p heading 'inlinetask)
                  (not (org-element-contents-begin heading)))
        (let ((pos (point)))
          (goto-char (org-element-contents-begin heading))
          (if (re-search-forward
               org-outline-regexp-bol
               (org-element-end heading)
               t)
              (progn (goto-char (match-beginning 0)) t)
            (goto-char pos) nil))))))

(defun org-get-next-sibling ()
  "Move to next heading of the same level, and return point.
If there is no such heading, return nil.
This is like outline-next-sibling, but invisible headings are ok."
  (let ((level (funcall outline-level)))
    (outline-next-heading)
    (while (and (not (eobp)) (> (funcall outline-level) level))
      (outline-next-heading))
    (unless (or (eobp) (< (funcall outline-level) level))
      (point))))

(defun org-get-previous-sibling ()
  "Move to previous heading of the same level, and return point.
If there is no such heading, return nil."
  (let ((opoint (point))
	(level (funcall outline-level)))
    (outline-previous-heading)
    (when (and (/= (point) opoint) (outline-on-heading-p t))
      (while (and (> (funcall outline-level) level)
		  (not (bobp)))
	(outline-previous-heading))
      (unless (< (funcall outline-level) level)
        (point)))))

(defun org-end-of-subtree (&optional invisible-ok to-heading element)
  "Goto to the end of a visible subtree at point or ELEMENT and return point.
The subtree is considered at first heading parent containing point or
ELEMENT.

When end of the subtree has blank lines, move point before these blank
lines.

When INVISIBLE-OK is non-nil, ignore visibility.

When before first heading, goto `point-max' minus blank lines.
When TO-HEADING is non-nil, go to the next heading or `point-max'."
  (when element
    (setq element (org-element-lineage
                   element
                   '(headline)
                   'include-self))
    (goto-char (org-element-begin element)))
  (unless (and invisible-ok element)
    (org-back-to-heading-or-point-min invisible-ok)
    (setq element
          (org-element-lineage
           (org-element-at-point)
           '(headline)
           'include-self)))
  (if (org-element-type-p element 'headline)
      (goto-char (org-element-end element))
    (goto-char (point-max)))
  (unless to-heading
    (when (memq (preceding-char) '(?\n ?\^M))
      ;; Go to end of line before heading
      (forward-char -1)
      ;; Skip blank lines
      (skip-chars-backward "\n\r\t ")))
  (point))

(defun org--line-fully-invisible-p ()
  "Return non-nil if the current line is fully invisible."
  (let ((line-beg (line-beginning-position))
	(line-pos (1- (line-end-position)))
	(is-invisible t))
    (while (and (< line-beg line-pos) is-invisible)
      (setq is-invisible (org-invisible-p line-pos))
      (setq line-pos (1- line-pos)))
    is-invisible))

(defun org-forward-heading-same-level (arg &optional invisible-ok)
  "Move forward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading.
Normally this only looks at visible headings, but when INVISIBLE-OK is
non-nil it will also look at invisible ones."
  (interactive "p")
  (let ((backward? (and arg (< arg 0))))
    (if (org-before-first-heading-p)
	(if backward? (goto-char (point-min)) (outline-next-heading))
      (org-back-to-heading invisible-ok)
      (unless backward? (end-of-line))	;do not match current headline
      (let ((level (org-current-level))
	    (f (if backward? #'re-search-backward #'re-search-forward))
	    (count (if arg (abs arg) 1))
	    (result (point)))
	(while (and (> count 0)
		    (funcall f org-outline-regexp-bol nil 'move))
	  (let ((l (- (match-end 0) (match-beginning 0) 1)))
	    (cond ((< l level) (setq count 0))
		  ((and (= l level)
			(or invisible-ok
			    ;; FIXME: See commit a700fadd72 and the
			    ;; related discussion on why using
			    ;; `org--line-fully-invisible-p' is needed
			    ;; here, which is to serve the needs of an
			    ;; external package.  If the change is
			    ;; wrong regarding Org itself, it should
			    ;; be removed.
			    (not (org--line-fully-invisible-p))))
		   (cl-decf count)
		   (when (= l level) (setq result (point)))))))
	(goto-char result))
      (forward-line 0))))

(defun org-backward-heading-same-level (arg &optional invisible-ok)
  "Move backward to the ARG'th visible subheading at same level as this one.
Stop at the first and last subheadings of a superior heading.
When optional argument INVISIBLE-OK is non-nil, ignore visibility."
  (interactive "p")
  (org-forward-heading-same-level (if arg (- arg) -1) invisible-ok))

(defun org-next-visible-heading (arg)
  "Move to the next visible heading line.
With ARG, repeats or can move backward if negative."
  (interactive "p")
  (let ((regexp (concat "^" (org-get-limited-outline-regexp))))
    (if (< arg 0)
	(forward-line 0)
      (end-of-line))
    (while (and (< arg 0) (re-search-backward regexp nil :move))
      (unless (bobp)
	(when (org-fold-folded-p)
	  (goto-char (org-fold-previous-visibility-change))
          (unless (looking-at-p regexp)
            (re-search-backward regexp nil :mode))))
      (cl-incf arg))
    (while (and (> arg 0) (re-search-forward regexp nil :move))
      (when (org-fold-folded-p)
	(goto-char (org-fold-next-visibility-change))
        (skip-chars-forward " \t\n")
	(end-of-line))
      (cl-decf arg))
    (if (> arg 0) (goto-char (point-max)) (forward-line 0))))

(defun org-previous-visible-heading (arg)
  "Move to the previous visible heading.
With ARG, repeats or can move forward if negative."
  (interactive "p")
  (org-next-visible-heading (- arg)))

(defun org-map-tree (fun)
  "Call FUN for every heading underneath the current one."
  (org-back-to-heading t)
  (let ((level (funcall outline-level)))
    (save-excursion
      (funcall fun)
      (while (and (progn
		    (outline-next-heading)
		    (> (funcall outline-level) level))
		  (not (eobp)))
	(funcall fun)))))

(defun org-map-region (fun beg end)
  "Call FUN for every heading between BEG and END."
  (let ((org-ignore-region t))
    (save-excursion
      (setq end (copy-marker end))
      (goto-char beg)
      (when (and (re-search-forward org-outline-regexp-bol nil t)
		 (< (point) end))
	(funcall fun))
      (while (and (progn
		    (outline-next-heading)
		    (< (point) end))
		  (not (eobp)))
	(funcall fun)))))

(provide 'org-heading)
;;; org-heading.el ends here
