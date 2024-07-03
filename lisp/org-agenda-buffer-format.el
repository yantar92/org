;;; org-agenda-buffer-format.el --- Agenda buffer formatting         -*- lexical-binding: t; -*-

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
;; This library implements functions rendering the agenda buffer.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-agenda-common)
(require 'org-agenda-highlight)
(require 'org-agenda-sort)
(require 'org-agenda-entry-text-mode)

(require 'org-mode-common)
(require 'org-agenda-files)
(require 'org-font-lock)
(require 'org-priority-common)

(defvar org-agenda-overriding-header nil
  "When set during agenda, todo and tags searches it replaces the header.
If an empty string, no header will be inserted.  If any other
string, it will be inserted as a header.  If a function, insert
the string returned by the function as a header.  If nil, a
header will be generated automatically according to the command.
This variable should not be set directly, but custom commands can
bind it in the options section.")

(defcustom org-agenda-with-colors t
  "Non-nil means use colors in agenda views."
  :group 'org-agenda-export
  :type 'boolean)

(defcustom org-agenda-todo-keyword-format "%-1s"
  "Format for the TODO keyword in agenda lines.
Set this to something like \"%-12s\" if you want all TODO keywords
to occupy a fixed space in the agenda display."
  :group 'org-agenda-line-format
  :type 'string)

(defvaralias 'org-agenda-align-tags-to-column 'org-agenda-tags-column)

(defcustom org-agenda-tags-column 'auto
  "Shift tags in agenda items to this column.
If set to `auto', tags will be automatically aligned to the right
edge of the window.

If set to a positive number, tags will be left-aligned to that
column.  If set to a negative number, tags will be right-aligned
to that column.  For example, -80 works well for a normal 80
character screen."
  :group 'org-agenda-line-format
  :type '(choice
	  (const :tag "Automatically align to right edge of window" auto)
	  (integer :tag "Specific column" -80))
  :package-version '(Org . "9.1")
  :version "26.1")

(defcustom org-agenda-fontify-priorities 'cookies
  "Non-nil means highlight low and high priorities in agenda.
When t, the highest priority entries are bold, lowest priority italic.
However, settings in `org-priority-faces' will overrule these faces.
When this variable is the symbol `cookies', only fontify the
cookies, not the entire task.
This may also be an association list of priority faces, whose
keys are the character values of `org-priority-highest',
`org-priority-default', and `org-priority-lowest' (the default values
are ?A, ?B, and ?C, respectively).  The face may be a named face, a
color as a string, or a list like `(:background \"Red\")'.
If it is a color, the variable `org-faces-easy-properties'
determines if it is a foreground or a background color."
  :group 'org-agenda-line-format
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Defaults" t)
	  (const :tag "Cookies only" cookies)
	  (repeat :tag "Specify"
		  (list (character :tag "Priority" :value ?A)
			(choice    :tag "Face    "
				   (string :tag "Color")
				   (sexp :tag "Face"))))))

(defcustom org-agenda-dim-blocked-tasks t
  "Non-nil means dim blocked tasks in the agenda display.
This causes some overhead during agenda construction, but if you
have turned on `org-enforce-todo-dependencies',
`org-enforce-todo-checkbox-dependencies', or any other blocking
mechanism, this will create useful feedback in the agenda.

Instead of t, this variable can also have the value `invisible'.
Then blocked tasks will be invisible and only become visible when
they become unblocked.  An exemption to this behavior is when a task is
blocked because of unchecked checkboxes below it.  Since checkboxes do
not show up in the agenda views, making this task invisible you remove any
trace from agenda views that there is something to do.  Therefore, a task
that is blocked because of checkboxes will never be made invisible, it
will only be dimmed."
  :group 'org-agenda-daily/weekly
  :group 'org-agenda-todo-list
  :version "24.3"
  :type '(choice
	  (const :tag "Do not dim" nil)
	  (const :tag "Dim to a gray face" t)
	  (const :tag "Make invisible" invisible)))

(defcustom org-agenda-compact-blocks nil
  "Non-nil means make the block agenda more compact.
This is done globally by leaving out lines like the agenda span
name and week number or the separator lines."
  :group 'org-agenda
  :type 'boolean)

(defcustom org-agenda-block-separator
  (if (and (display-graphic-p)
           (char-displayable-p ?─))
      ?─
    ?=)
  "The separator between blocks in the agenda.
If this is a string, it will be used as the separator, with a newline added.
If it is a character, it will be repeated to fill the window width.
If nil the separator is disabled.  In `org-agenda-custom-commands' this
addresses the separator between the current and the previous block."
  :group 'org-agenda
  :package-version '(Org . "9.6")
  :type '(choice
	  (const :tag "Disabled" nil)
	  (character)
	  (string)))

(defvar org-agenda-title-append nil) ; FIXME: used by org-mobile.el without documentation
(defun org-agenda-mark-header-line (pos)
  "Mark the line at POS as an agenda structure header."
  (save-excursion
    (goto-char pos)
    (put-text-property (line-beginning-position) (line-end-position)
		       'org-agenda-structural-header t)
    (when org-agenda-title-append
      (put-text-property (line-beginning-position) (line-end-position)
			 'org-agenda-title-append org-agenda-title-append))))

(defun org-agenda--insert-block-separator ()
  "Insert block separator at `point-max'."
  (setq buffer-read-only nil)
  (goto-char (point-max))
  (unless (or (bobp) org-agenda-compact-blocks
	      (not org-agenda-block-separator))
    (insert "\n"
	    (if (stringp org-agenda-block-separator)
		org-agenda-block-separator
	      (make-string (window-max-chars-per-line) org-agenda-block-separator))
	    "\n")))

(defmacro org-agenda--insert-overriding-header (default)
  "Insert header into agenda view.
The inserted header depends on `org-agenda-overriding-header'.
If the empty string, don't insert a header.  If any other string,
insert it as a header.  If nil, insert DEFAULT, which should
evaluate to a string.  If a function, call it and insert the
string that it returns."
  (declare (debug (form)) (indent defun))
  `(cond
    ((not org-agenda-overriding-header) (insert ,default))
    ((equal org-agenda-overriding-header "") nil)
    ((stringp org-agenda-overriding-header)
     (insert (propertize org-agenda-overriding-header
			 'face 'org-agenda-structure)
	     "\n"))
    ((functionp org-agenda-overriding-header)
     (insert (funcall org-agenda-overriding-header)))
    (t (user-error "Invalid value for `org-agenda-overriding-header': %S"
		   org-agenda-overriding-header))))

(defun org-agenda-highlight-todo (x)
  (let ((case-fold-search nil)
	re)
    (if (eq x 'line)
	(save-excursion
	  (forward-line 0)
	  (setq re (org-get-at-bol 'org-todo-regexp))
          (goto-char (or (text-property-any (line-beginning-position)
                                            (line-end-position)
                                            'org-heading t)
                         (point)))
	  (when (looking-at (concat "[ \t]*\\.*\\(" re "\\) +"))
	    (add-text-properties
             (match-beginning 0) (match-end 1)
	     (list 'face
                   (org-get-todo-face
                    1
                    (org-element-done-keywords
                     (org-get-at-bol 'org-hd-marker)))))
	    (let ((s (buffer-substring (match-beginning 1) (match-end 1))))
	      (delete-region (match-beginning 1) (1- (match-end 0)))
	      (goto-char (match-beginning 1))
	      (insert (format org-agenda-todo-keyword-format s)))))
      (let ((pl (text-property-any 0 (length x) 'org-heading t x)))
	(setq re (get-text-property 0 'org-todo-regexp x))
	(when (and re
		   ;; Test `pl' because if there's no heading content,
		   ;; there's no point matching to highlight.  Note
		   ;; that if we didn't test `pl' first, and there
		   ;; happened to be no keyword from `org-todo-regexp'
		   ;; on this heading line, then the `equal' comparison
		   ;; afterwards would spuriously succeed in the case
		   ;; where `pl' is nil -- causing an args-out-of-range
		   ;; error when we try to add text properties to text
		   ;; that isn't there.
		   pl
		   (equal (string-match (concat "\\(\\.*\\)" re "\\( +\\)")
					x pl)
			  pl))
	  (add-text-properties
	   (or (match-end 1) (match-end 0)) (match-end 0)
	   (list 'face (org-get-todo-face
                        (match-string 2 x)
                        (org-element-done-keywords
                         (get-text-property 0 'org-hd-marker x))))
	   x)
	  (when (match-end 1)
	    (setq x
		  (concat
		   (substring x 0 (match-end 1))
                   (unless (string= org-agenda-todo-keyword-format "")
                     (format org-agenda-todo-keyword-format
                             (match-string 2 x)))
                   (unless (string= org-agenda-todo-keyword-format "")
                     ;; Remove `display' property as the icon could leak
                     ;; on the white space.
                     (apply #'propertize " " (org-plist-delete (text-properties-at 0 x) 'display)))
                   (substring x (match-end 3)))))))
      x)))

(defun org-agenda-align-tags (&optional line)
  "Align all tags in agenda items to `org-agenda-tags-column'.
When optional argument LINE is non-nil, align tags only on the
current line."
  (let ((inhibit-read-only t)
	(org-agenda-tags-column (if (eq 'auto org-agenda-tags-column)
			            (- (window-max-chars-per-line))
			          org-agenda-tags-column))
	(end (and line (line-end-position)))
	l c)
    (org-fold-core-ignore-modifications
      (save-excursion
        (goto-char (if line (line-beginning-position) (point-min)))
        (while (re-search-forward org-tag-group-re end t)
	  (add-text-properties
	   (match-beginning 1) (match-end 1)
	   (list 'face (delq nil (let ((prop (get-text-property
					    (match-beginning 1) 'face)))
			         (or (listp prop) (setq prop (list prop)))
			         (if (memq 'org-tag prop)
				     prop
				   (cons 'org-tag prop))))))
	  (setq l (string-width (match-string 1))
	        c (if (< org-agenda-tags-column 0)
		      (- (abs org-agenda-tags-column) l)
		    org-agenda-tags-column))
	  (goto-char (match-beginning 1))
	  (delete-region (save-excursion (skip-chars-backward " \t") (point))
		         (point))
	  (insert (org-add-props
		      (make-string (max 1 (- c (current-column))) ?\s)
		      (plist-put (copy-sequence (text-properties-at (point)))
			         'face nil))))
        (goto-char (point-min))
        (org-font-lock-add-tag-faces (point-max))))))

(defun org-agenda-fontify-priorities ()
  "Make highest priority lines bold, and lowest italic."
  (interactive)
  (mapc (lambda (o) (when (eq (overlay-get o 'org-type) 'org-priority)
		 (delete-overlay o)))
	(overlays-in (point-min) (point-max)))
  (save-excursion
    (let (b e p ov h l)
      (goto-char (point-min))
      (while (re-search-forward org-priority-regexp nil t)
	(setq h (or (get-char-property (point) 'org-priority-highest)
		    org-priority-highest)
	      l (or (get-char-property (point) 'org-priority-lowest)
		    org-priority-lowest)
	      p (string-to-char (match-string 2))
	      b (match-beginning 1)
	      e (if (eq org-agenda-fontify-priorities 'cookies)
		    (1+ (match-end 2))
                  (line-end-position))
	      ov (make-overlay b e))
        (require 'org-faces)
        (defvar org-priority-faces)
	(overlay-put
	 ov 'face
	 (let ((special-face
		(cond ((org-face-from-face-or-color
			'priority 'org-priority
			(cdr (assoc p org-priority-faces))))
		      ((and (listp org-agenda-fontify-priorities)
			    (org-face-from-face-or-color
			     'priority 'org-priority
			     (cdr (assoc p org-agenda-fontify-priorities)))))
		      ((equal p l) 'italic)
		      ((equal p h) 'bold))))
	   (if special-face (list special-face 'org-priority) 'org-priority)))
	(overlay-put ov 'org-type 'org-priority)))))

(declare-function org-agenda-filter-hide-line "org-agenda-mode" (type))
(defun org-agenda-dim-blocked-tasks (&optional _invisible)
  "Dim currently blocked TODOs in the agenda display.
When INVISIBLE is non-nil, hide currently blocked TODO instead of
dimming them."                   ;FIXME: The arg isn't used, actually!
  (interactive "P")
  (when (called-interactively-p 'interactive)
    (message "Dim or hide blocked tasks..."))
  (dolist (o (overlays-in (point-min) (point-max)))
    (when (eq (overlay-get o 'face) 'org-agenda-dimmed-todo-face)
      (delete-overlay o)))
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (let ((pos (text-property-not-all
			 (point) (point-max) 'org-todo-blocked nil)))
	       (when pos (goto-char pos)))
	(let* ((invisible
		(eq (org-get-at-bol 'org-todo-blocked) 'invisible))
	       (todo-blocked
		(eq (org-get-at-bol 'org-filter-type) 'todo-blocked))
	       (ov (make-overlay (if invisible
				     (line-end-position 0)
				   (line-beginning-position))
				 (line-end-position))))
	  (when todo-blocked
	    (overlay-put ov 'face 'org-agenda-dimmed-todo-face)
            ;; Override other overlays.
            (overlay-put ov 'priority 50))
	  (when invisible
	    (org-agenda-filter-hide-line 'todo-blocked)))
        (if (= (point-max) (line-end-position))
            (goto-char (point-max))
	  (move-beginning-of-line 2)))))
  (when (called-interactively-p 'interactive)
    (message "Dim or hide blocked tasks...done")))

(declare-function org-get-tags "org-tags-core" (&optional epom local) )
(declare-function org-habit-insert-consistency-graphs "org-habit" (&optional line))
(defun org-agenda-format-buffer ()
  "Format current agenda buffer.
Assume that the buffer is already populated with all the agenda
blocks."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (save-excursion
	(while (org-activate-links (point-max))
	  (goto-char (match-end 0))))
      (unless (and (boundp 'org-agenda-remove-tags)
                   (eq org-agenda-remove-tags t))
	(org-agenda-align-tags))
      (unless org-agenda-with-colors
	(remove-text-properties (point-min) (point-max) '(face nil)))
      (when org-agenda-fontify-priorities
	(org-agenda-fontify-priorities))
      (when (and org-agenda-dim-blocked-tasks
                 (bound-and-true-p org-blocker-hook))
	(org-agenda-dim-blocked-tasks))
      (org-agenda-mark-clocking-task)
      (when org-agenda-entry-text-mode
	(org-agenda-entry-text-hide)
	(org-agenda-entry-text-show))
      (when (and (featurep 'org-habit)
		 (save-excursion (next-single-property-change (point-min) 'org-habit-p)))
	(org-habit-insert-consistency-graphs))
      (unless (or (eq org-agenda-show-inherited-tags 'always)
		  (and (listp org-agenda-show-inherited-tags)
		       (memq org-agenda-type org-agenda-show-inherited-tags))
		  (and (eq org-agenda-show-inherited-tags t)
		       (or (eq org-agenda-use-tag-inheritance t)
			   (and (listp org-agenda-use-tag-inheritance)
				(not (memq org-agenda-type
					 org-agenda-use-tag-inheritance))))))
        (require 'org-tags-core)
	(let (mrk)
	  (save-excursion
	    (goto-char (point-min))
	    (while (equal (forward-line) 0)
	      (when (setq mrk (get-text-property (point) 'org-hd-marker))
                (put-text-property (line-beginning-position) (line-end-position)
				   'tags
				   (org-with-point-at mrk
				     (org-get-tags)))))))))))

(defvar org-depend-tag-blocked) ; Historic support for external org-depend.el
(declare-function org-entry-blocked-p "org-property" ())
(defun org-agenda--mark-blocked-entry (entry)
  "If ENTRY is blocked, mark it for fontification or invisibility.

If the header at `org-hd-marker' is blocked according to
`org-entry-blocked-p', then if `org-agenda-dim-blocked-tasks' is
`invisible' and the header is not blocked by checkboxes, set the
text property `org-todo-blocked' to `invisible', otherwise set it
to t."
  (when (get-text-property 0 'todo-state entry)
    (let ((entry-marker (get-text-property 0 'org-hd-marker entry))
          (org-blocked-by-checkboxes nil)
	  ;; Necessary so that `org-entry-blocked-p' does not change
	  ;; the buffer.
          (org-depend-tag-blocked nil))
      (when entry-marker
	(let ((blocked
	       (with-current-buffer (marker-buffer entry-marker)
		 (save-excursion
		   (goto-char entry-marker)
                   (require 'org-property)
		   (org-entry-blocked-p)))))
	  (when blocked
	    (let ((really-invisible
		   (and (not org-blocked-by-checkboxes)
			(eq org-agenda-dim-blocked-tasks 'invisible))))
	      (put-text-property
	       0 (length entry) 'org-todo-blocked
	       (if really-invisible 'invisible t)
	       entry)
	      (put-text-property
	       0 (length entry) 'org-filter-type 'todo-blocked entry)))))))
  entry)

(defun org-agenda-finalize-entries (list &optional type)
  "Sort, limit and concatenate the LIST of agenda items.
The optional argument TYPE tells the agenda type."
  ;; Sort entries.
  (setq list (org-agenda-sort-entries list))
  ;; Limit the number of entries.
  (setq list (org-agenda-apply-limits list type))
  ;; Fontify entries.
  (setq list (mapcar #'org-agenda-highlight-todo list))
  (when (and org-agenda-dim-blocked-tasks
             (bound-and-true-p org-blocker-hook))
    (setq list (mapcar #'org-agenda--mark-blocked-entry list)))
  (mapconcat #'identity list "\n"))

(provide 'org-agenda-buffer-format)

;;; org-agenda-buffer-format.el ends here
