;;; org-edit.el --- General Org editing commands                      -*- lexical-binding: t; -*-

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

;; This library implements generic commands used to edit Org buffers.

;;; Code:

(require 'org-macs)
(org-assert-version)

(defvar org-speed-command nil)

(require 'org-keys)
(require 'org-tags)
(require 'org-edit-structure)
(require 'org-mode-common)
(require 'org-cycle)
(require 'org-font-lock-common)
(require 'org-table-fold)

(declare-function org-open-at-point "org-open-at-point")

(defcustom org-special-ctrl-k nil
  "Non-nil means that \\<org-mode-map>\\[org-kill-line] \
will behave specially in headlines.

When nil, \\[org-kill-line] will call the default `kill-line' command.
Otherwise, the following will happen when point is in a headline:

- At the beginning of a headline, kill the entire line.
- In the middle of the headline text, kill the text up to the tags.
- After the headline text and before the tags, kill all the tags."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-ctrl-k-protect-subtree nil
  "Non-nil means, do not delete a hidden subtree with `C-k'.
When set to the symbol `error', simply throw an error when `C-k' is
used to kill (part-of) a headline that has hidden text behind it.
Any other non-nil value will result in a query to the user, if it is
OK to kill that hidden subtree.  When nil, kill without remorse."
  :group 'org-edit-structure
  :version "24.1"
  :type '(choice
	  (const :tag "Do not protect hidden subtrees" nil)
	  (const :tag "Protect hidden subtrees with a security query" t)
	  (const :tag "Never kill a hidden subtree with C-k" error)))

(defcustom org-special-ctrl-o t
  "Non-nil means, make `open-line' (\\[open-line]) insert a row in tables."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-yank-folded-subtrees t
  "Non-nil means when yanking subtrees, fold them.
If the kill is a single subtree, or a sequence of subtrees, i.e. if
it starts with a heading and all other headings in it are either children
or siblings, then fold all the subtrees.  However, do this only if no
text after the yank would be swallowed into a folded tree by this action."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-yank-adjusted-subtrees nil
  "Non-nil means when yanking subtrees, adjust the level.
With this setting, `org-paste-subtree' is used to insert the subtree, see
this function for details."
  :group 'org-edit-structure
  :type 'boolean)

;;;###autoload
(defun org-force-self-insert (N)
  "Needed to enforce self-insert under remapping."
  (interactive "p")
  (self-insert-command N))

(defun org--speed-command-p ()
  "Return non-nil when current command is a speed command.
Set `org-speed-command' to the appropriate command as a side effect."
  (and org-use-speed-commands
       (let ((kv (this-command-keys-vector)))
         (setq org-speed-command
               (run-hook-with-args-until-success
                'org-speed-command-hook
                (make-string 1 (aref kv (1- (length kv)))))))))

(declare-function org-table-blank-field "org-table-edit" ())
;;;###autoload
(defun org-self-insert-command (N)
  "Like `self-insert-command', use `overwrite-mode' for whitespace in tables.
If the cursor is in a table looking at whitespace, the whitespace is
overwritten, and the table is not marked as requiring realignment."
  (interactive "p")
  (cond
   ((org--speed-command-p)
    (cond
     ((commandp org-speed-command)
      (setq this-command org-speed-command)
      (call-interactively org-speed-command))
     ((functionp org-speed-command)
      (funcall org-speed-command))
     ((consp org-speed-command)
      (eval org-speed-command t))
     (t (let (org-use-speed-commands)
	  (call-interactively 'org-self-insert-command)))))
   ((and
     (= N 1)
     (not (use-region-p))
     (org-at-table-p)
     (progn
       (require 'org-table-edit)
       (defvar org-table-auto-blank-field)
       ;; Check if we blank the field, and if that triggers align.
       (and org-table-auto-blank-field
	    (memq last-command
	          '(org-cycle org-return org-shifttab org-ctrl-c-ctrl-c))
	    (if (or (eq (char-after) ?\s) (looking-at "[^|\n]*  |"))
	        ;; Got extra space, this field does not determine
	        ;; column width.
	        (let (org-table-may-need-update)
                  (require 'org-table-edit)
                  (org-table-blank-field))
	      ;; No extra space, this field may determine column
	      ;; width.
	      (org-table-blank-field)))
       t)
     (looking-at "[^|\n]*  |"))
    ;; There is room for insertion without re-aligning the table.
    ;; Interactively, point should never be inside invisible regions
    (org-fold-core-suppress-folding-fix
      (self-insert-command N))
    (org-table-with-shrunk-field
     (save-excursion
       (skip-chars-forward "^|")
       ;; Do not delete last space, which is
       ;; `org-table-separator-space', but the regular space before
       ;; it.
       (delete-region (- (point) 2) (1- (point))))))
   (t
    (setq org-table-may-need-update t)
    ;; Interactively, point should never be inside invisible regions
    (org-fold-core-suppress-folding-fix
      (self-insert-command N)
      (when org-auto-align-tags (org-fix-tags-on-the-fly)))
    (when org-self-insert-cluster-for-undo
      (if (not (eq last-command 'org-self-insert-command))
	  (setq org-self-insert-command-undo-counter 1)
	(if (>= org-self-insert-command-undo-counter 20)
	    (setq org-self-insert-command-undo-counter 1)
	  (and (> org-self-insert-command-undo-counter 0)
	       buffer-undo-list (listp buffer-undo-list)
	       (not (cadr buffer-undo-list)) ; remove nil entry
	       (setcdr buffer-undo-list (cddr buffer-undo-list)))
	  (setq org-self-insert-command-undo-counter
		(1+ org-self-insert-command-undo-counter))))))))

;;;###autoload
(defun org-delete-backward-char (N)
  "Like `delete-backward-char', insert whitespace at field end in tables.
When deleting backwards, in tables this function will insert whitespace in
front of the next \"|\" separator, to keep the table aligned.  The table will
still be marked for re-alignment if the field did fill the entire column,
because, in this case the deletion might narrow the column."
  (interactive "p")
  (save-match-data
    (if (and (= N 1)
	     (not overwrite-mode)
	     (not (use-region-p))
	     (not (eq (char-before) ?|))
	     (save-excursion (skip-chars-backward " \t") (not (bolp)))
	     (looking-at-p ".*?|")
	     (org-at-table-p))
	(progn (forward-char -1) (org-delete-char 1))
      (funcall-interactively #'backward-delete-char N)
      (when org-auto-align-tags (org-fix-tags-on-the-fly)))))

;;;###autoload
(defun org-delete-char (N)
  "Like `delete-char', but insert whitespace at field end in tables.
When deleting characters, in tables this function will insert whitespace in
front of the next \"|\" separator, to keep the table aligned.  The table will
still be marked for re-alignment if the field did fill the entire column,
because, in this case the deletion might narrow the column."
  (interactive "p")
  (save-match-data
    (cond
     ((or (/= N 1)
	  (eq (char-after) ?|)
	  (save-excursion (skip-chars-backward " \t") (bolp))
	  (not (org-at-table-p)))
      (delete-char N)
      (when org-auto-align-tags (org-fix-tags-on-the-fly)))
     ((looking-at ".\\(.*?\\)|")
      (let* ((update? org-table-may-need-update)
	     (noalign (looking-at-p ".*?  |")))
	(delete-char 1)
	(org-table-with-shrunk-field
	 (save-excursion
	   ;; Last space is `org-table-separator-space', so insert
	   ;; a regular one before it instead.
	   (goto-char (- (match-end 0) 2))
	   (insert " ")))
	;; If there were two spaces at the end, this field does not
	;; determine the width of the column.
	(when noalign (setq org-table-may-need-update update?))))
     (t
      (delete-char N)))))

;; Make `delete-selection-mode' work with Org mode and Orgtbl mode
(put 'org-self-insert-command 'delete-selection
     (lambda ()
       (unless (org--speed-command-p)
         (not (run-hook-with-args-until-success
             'self-insert-uses-region-functions)))))
(put 'orgtbl-self-insert-command 'delete-selection
     (lambda ()
       (not (run-hook-with-args-until-success
             'self-insert-uses-region-functions))))
(put 'org-delete-char 'delete-selection 'supersede)
(put 'org-delete-backward-char 'delete-selection 'supersede)
(put 'org-yank 'delete-selection 'yank)
(put 'org-return 'delete-selection t)

;; Make `flyspell-mode' delay after some commands
(put 'org-self-insert-command 'flyspell-delayed t)
(put 'orgtbl-self-insert-command 'flyspell-delayed t)
(put 'org-delete-char 'flyspell-delayed t)
(put 'org-delete-backward-char 'flyspell-delayed t)

;; Make pabbrev-mode expand after Org mode commands
(put 'org-self-insert-command 'pabbrev-expand-after-command t)
(put 'orgtbl-self-insert-command 'pabbrev-expand-after-command t)

(defvar org-mode-transpose-word-syntax-table
  (let ((st (make-syntax-table text-mode-syntax-table)))
    (dolist (c org-emphasis-alist st)
      (modify-syntax-entry (string-to-char (car c)) "w p" st))))

;;;###autoload
(defun org-transpose-words ()
  "Transpose words for Org.
This uses the `org-mode-transpose-word-syntax-table' syntax
table, which interprets characters in `org-emphasis-alist' as
word constituents."
  (interactive)
  (with-syntax-table org-mode-transpose-word-syntax-table
    (call-interactively 'transpose-words)))

;;;###autoload
(defun org-delete-indentation (&optional arg beg end)
  "Join current line to previous and fix whitespace at join.

If previous line is a headline add to headline title.  Otherwise
the function calls `delete-indentation'.

If there is a region (BEG END), then join the lines in that region.

With a non-nil prefix ARG, join the line with the following one,
ignoring region."
  (interactive
   (cons current-prefix-arg
         (when (and (not current-prefix-arg) (use-region-p))
           (list (region-beginning) (region-end)))))
  (unless (and beg end)
    ;; No region selected or BEG/END arguments not passed.
    (setq beg (line-beginning-position (if arg 1 0))
          end (line-end-position (if arg 2 1))))
  (if (save-excursion
        (goto-char beg)
        (forward-line 0)
        (and (< (line-end-position) end)
             (let ((case-fold-search nil))
	       (looking-at org-complex-heading-regexp))))
      ;; At headline.
      (let ((tags-column (when (match-beginning 5)
			   (save-excursion (goto-char (match-beginning 5))
					   (current-column))))
	    string)
        (goto-char beg)
        ;; Join all but headline.
        (save-excursion
          (save-match-data
            (if (version<= "27" emacs-version)
                (delete-indentation nil (line-beginning-position 2) end)
              ;; FIXME: Emacs 26.  `delete-indentation' does not yet
              ;; accept BEG/END arguments.
              (save-restriction
                (narrow-to-region beg end)
                (goto-char beg)
                (forward-line 2)
                (while (< (point) (point-max))
                  (delete-indentation)
                  (forward-line 1))))))
        (setq string (org-trim (delete-and-extract-region (line-end-position) (line-end-position 2))))
	(goto-char (or (match-end 4)
		       (match-beginning 5)
		       (match-end 0)))
	(skip-chars-backward " \t")
	(save-excursion (insert " " string))
	;; Adjust alignment of tags.
	(cond
	 ((not tags-column))		;no tags
	 (org-auto-align-tags (org-align-tags))
	 (t (org--align-tags-here tags-column)))) ;preserve tags column
    (if (version<= "27" emacs-version)
        (funcall-interactively #'delete-indentation arg beg end)
      ;; FIXME: Emacs 26.  `delete-indentation' does not yet
      ;; accept BEG/END arguments.
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        (forward-line 1)
        (while (< (point) (point-max))
          (delete-indentation)
          (forward-line 1))))))

(declare-function org-table-insert-row "org-table-edit" (&optional arg))
;;;###autoload
(defun org-open-line (n)
  "Insert a new row in tables, call `open-line' elsewhere.
If `org-special-ctrl-o' is nil, just call `open-line' everywhere.
As a special case, when a document starts with a table, allow
calling `open-line' on the very first character."
  (interactive "*p")
  (if (and org-special-ctrl-o (/= (point) 1) (org-at-table-p))
      (progn
        (require 'org-table-edit)
        (org-table-insert-row))
    (open-line n)))

(defun org--newline (indent arg interactive)
  "Call `newline-and-indent' or just `newline'.
If INDENT is non-nil, call `newline-and-indent' with ARG to
indent unconditionally; otherwise, call `newline' with ARG and
INTERACTIVE, which can trigger indentation if
`electric-indent-mode' is enabled."
  (if indent
      (org-newline-and-indent arg)
    (newline arg interactive)))

(declare-function org-table-next-row "org-table-move" ())
(declare-function org-table-justify-field-maybe "org-table-align" (&optional new))
;;;###autoload
(defun org-return (&optional indent arg interactive)
  "Goto next table row or insert a newline.

Calls `org-table-next-row' or `newline', depending on context.

When optional INDENT argument is non-nil, call
`newline-and-indent' with ARG, otherwise call `newline' with ARG
and INTERACTIVE.

When `org-return-follows-link' is non-nil and point is on
a timestamp, a link or a citation, call `org-open-at-point'.
However, it will not happen if point is in a table or on a \"dead\"
object (e.g., within a comment).  In these case, you need to use
`org-open-at-point' directly."
  (interactive "i\nP\np")
  (let* ((context (if org-return-follows-link (org-element-context)
		    (org-element-at-point)))
         (element-type (org-element-type context)))
    (cond
     ;; In a table, call `org-table-next-row'.  However, before first
     ;; column or after last one, split the table.
     ((or (and (eq 'table element-type)
	       (not (eq 'table.el (org-element-property :type context)))
	       (>= (point) (org-element-contents-begin context))
	       (< (point) (org-element-contents-end context)))
	  (org-element-lineage context '(table-row table-cell) t))
      (if (or (looking-at-p "[ \t]*$")
	      (save-excursion (skip-chars-backward " \t") (bolp)))
	  (insert "\n")
        (require 'org-table-align)
	(org-table-justify-field-maybe)
        (require 'org-table-move)
	(call-interactively #'org-table-next-row)))
     ;; On a link, a timestamp or a citation, call `org-open-at-point'
     ;; if `org-return-follows-link' allows it.  Tolerate fuzzy
     ;; locations, e.g., in a comment, as `org-open-at-point'.
     ((and org-return-follows-link
	   (or (and (eq 'link element-type)
		    ;; Ensure point is not on the white spaces after
		    ;; the link.
		    (let ((origin (point)))
		      (org-with-point-at (org-element-end context)
			(skip-chars-backward " \t")
			(> (point) origin))))
	       (org-in-regexp org-ts-regexp-both nil t)
	       (org-in-regexp org-tsr-regexp-both nil  t)
               (org-element-lineage context '(citation citation-reference) 'include-self)
	       (org-in-regexp org-link-any-re nil t)))
      (call-interactively #'org-open-at-point))
     ;; Insert newline in heading, but preserve tags.
     ((and (not (bolp))
	   (let ((case-fold-search nil))
	     (org-match-line org-complex-heading-regexp)))
      ;; At headline.  Split line.  However, if point is on keyword,
      ;; priority cookie or tags, do not break any of them: add
      ;; a newline after the headline instead.
      (let ((tags-column (and (match-beginning 5)
			      (save-excursion (goto-char (match-beginning 5))
					      (current-column))))
	    (string
	     (when (and (match-end 4) (org-point-in-group (point) 4))
	       (delete-and-extract-region (point) (match-end 4)))))
	;; Adjust tag alignment.
	(cond
	 ((not (and tags-column string)))
	 (org-auto-align-tags (org-align-tags))
	 (t (org--align-tags-here tags-column))) ;preserve tags column
	(end-of-line)
	(org-fold-show-entry 'hide-drawers)
	(org--newline indent arg interactive)
	(when string (save-excursion (insert (org-trim string))))))
     ;; In a list, make sure indenting keeps trailing text within.
     ((and (not (eolp))
	   (org-element-lineage context 'item))
      (let ((trailing-data
	     (delete-and-extract-region (point) (line-end-position))))
	(org--newline indent arg interactive)
	(save-excursion (insert trailing-data))))
     (t
      ;; Do not auto-fill when point is in an Org property drawer.
      (let ((auto-fill-function (and (not (org-at-property-p))
				     auto-fill-function)))
	(org--newline indent arg interactive))))))

;;;###autoload
(defun org-return-and-maybe-indent ()
  "Goto next table row, or insert a newline, maybe indented.
Call `org-table-next-row' or `org-return', depending on context.
See the individual commands for more information.

When inserting a newline, if `org-adapt-indentation' is t:
indent the line if `electric-indent-mode' is disabled, don't
indent it if it is enabled."
  (interactive)
  (org-return (not electric-indent-mode)))

;;;###autoload
(defun org-kill-line (&optional _arg)
  "Kill line, to tags or end of line.

The behavior of this command depends on the user options
`org-special-ctrl-k' and `org-ctrl-k-protect-subtree' (which
see)."
  (interactive)
  (cond
   ((or (not org-special-ctrl-k)
	(bolp)
	(not (org-at-heading-p)))
    (when (and (org-invisible-p (line-end-position))
	       org-ctrl-k-protect-subtree
	       (or (eq org-ctrl-k-protect-subtree 'error)
		   (not (y-or-n-p "Kill hidden subtree along with headline? "))))
      (user-error
       (substitute-command-keys
	"`\\[org-kill-line]' aborted as it would kill a hidden subtree")))
    (call-interactively
     (if (bound-and-true-p visual-line-mode) 'kill-visual-line 'kill-line)))
   ((org-match-line org-tag-line-re)
    (let ((end (save-excursion
		 (goto-char (match-beginning 1))
		 (skip-chars-backward " \t")
		 (point))))
      (if (<= end (point))		;on tags part
	  (kill-region (point) (line-end-position))
	(kill-region (point) end)))
    ;; Only align tags when we are still on a heading:
    (if (and (org-at-heading-p) org-auto-align-tags) (org-align-tags)))
   (t (kill-region (point) (line-end-position)))))

(defun org-drag-element-backward ()
  "Move backward element at point."
  (interactive)
  (let ((elem (or (org-element-at-point)
		  (user-error "No element at point"))))
    (if (org-element-type-p elem 'headline)
	;; Preserve point when moving a whole tree, even if point was
	;; on blank lines below the headline.
	(let ((offset (skip-chars-backward " \t\n")))
	  (unwind-protect (org-move-subtree-up)
	    (forward-char (- offset))))
      (let ((prev-elem
	     (save-excursion
	       (goto-char (org-element-begin elem))
	       (skip-chars-backward " \r\t\n")
	       (unless (bobp)
		 (let* ((beg (org-element-begin elem))
			(prev (org-element-at-point))
			(up prev))
		   (while (and (setq up (org-element-parent up))
			       (<= (org-element-end up) beg))
		     (setq prev up))
		   prev)))))
	;; Error out if no previous element or previous element is
	;; a parent of the current one.
	(if (or (not prev-elem) (org-element-nested-p elem prev-elem))
	    (user-error "Cannot drag element backward")
	  (let ((pos (point)))
	    (org-element-swap-A-B prev-elem elem)
	    (goto-char (+ (org-element-begin prev-elem)
			  (- pos (org-element-begin elem))))))))))

(defun org-drag-element-forward ()
  "Move forward element at point."
  (interactive)
  (let* ((pos (point))
	 (elem (or (org-element-at-point)
		   (user-error "No element at point"))))
    (when (= (point-max) (org-element-end elem))
      (user-error "Cannot drag element forward"))
    (goto-char (org-element-end elem))
    (let ((next-elem (org-element-at-point)))
      (when (or (org-element-nested-p elem next-elem)
		(and (org-element-type-p next-elem 'headline)
		     (not (org-element-type-p elem 'headline))))
	(goto-char pos)
	(user-error "Cannot drag element forward"))
      ;; Compute new position of point: it's shifted by NEXT-ELEM
      ;; body's length (without final blanks) and by the length of
      ;; blanks between ELEM and NEXT-ELEM.
      (let ((size-next (- (save-excursion
			    (goto-char (org-element-end next-elem))
			    (skip-chars-backward " \r\t\n")
			    (forward-line)
			    ;; Small correction if buffer doesn't end
			    ;; with a newline character.
			    (if (and (eolp) (not (bolp))) (1+ (point)) (point)))
			  (org-element-begin next-elem)))
	    (size-blank (- (org-element-end elem)
			   (save-excursion
			     (goto-char (org-element-end elem))
			     (skip-chars-backward " \r\t\n")
			     (forward-line)
			     (point)))))
	(org-element-swap-A-B elem next-elem)
	(goto-char (+ pos size-next size-blank))))))

(defun org-drag-line-forward (arg)
  "Drag the line at point ARG lines forward."
  (interactive "p")
  (dotimes (_ (abs arg))
    (let ((c (current-column)))
      (if (< 0 arg)
	  (progn
	    (forward-line 1)
	    (transpose-lines 1)
	    (forward-line -1))
	(transpose-lines 1)
	(forward-line -2))
      (org-move-to-column c))))

(defun org-drag-line-backward (arg)
  "Drag the line at point ARG lines backward."
  (interactive "p")
  (org-drag-line-forward (- arg)))

;;;###autoload
(defun org-transpose-element ()
  "Transpose current and previous elements, keeping blank lines between.
Point is moved after both elements."
  (interactive)
  (org-skip-whitespace)
  (let ((end (org-element-end (org-element-at-point))))
    (org-drag-element-backward)
    (goto-char end)))

;;;###autoload
(defun org-yank (&optional arg)
  "Yank.  If the kill is a subtree, treat it specially.
This command will look at the current kill and check if is a single
subtree, or a series of subtrees[1].  If it passes the test, and if the
cursor is at the beginning of a line or after the stars of a currently
empty headline, then the yank is handled specially.  How exactly depends
on the value of the following variables.

`org-yank-folded-subtrees'
    By default, this variable is non-nil, which results in
    subtree(s) being folded after insertion, except if doing so
    would swallow text after the yanked text.

`org-yank-adjusted-subtrees'
    When non-nil (the default value is nil), the subtree will be
    promoted or demoted in order to fit into the local outline tree
    structure, which means that the level will be adjusted so that it
    becomes the smaller one of the two *visible* surrounding headings.

Any prefix to this command will cause `yank' to be called directly with
no special treatment.  In particular, a simple `\\[universal-argument]' prefix \
will just
plainly yank the text as it is.

\[1] The test checks if the first non-white line is a heading
    and if there are no other headings with fewer stars."
  (interactive "P")
  (org-yank-generic 'yank arg))

(defun org-yank-generic (command arg)
  "Perform some yank-like command.

This function implements the behavior described in the `org-yank'
documentation.  However, it has been generalized to work for any
interactive command with similar behavior."

  ;; pretend to be command COMMAND
  (setq this-command command)

  (if arg
      (call-interactively command)

    (let ((subtreep ; is kill a subtree, and the yank position appropriate?
	   (and (org-kill-is-subtree-p)
		(or (bolp)
		    (and (looking-at "[ \t]*$")
			 (string-match
			  "\\`\\*+\\'"
                          (buffer-substring (line-beginning-position) (point)))))))
	  swallowp)
      (cond
       ((and subtreep org-yank-folded-subtrees)
	(let ((beg (point))
	      end)
	  (if (and subtreep org-yank-adjusted-subtrees)
	      (org-paste-subtree nil nil 'for-yank)
	    (call-interactively command))

	  (setq end (point))
	  (goto-char beg)
	  (when (and (bolp) subtreep
		     (not (setq swallowp
			        (org-yank-folding-would-swallow-text beg end))))
	    (org-with-limited-levels
	     (or (looking-at org-outline-regexp)
		 (re-search-forward org-outline-regexp-bol end t))
	     (while (and (< (point) end) (looking-at org-outline-regexp))
	       (org-fold-subtree t)
	       (org-cycle-show-empty-lines 'folded)
	       (condition-case nil
		   (outline-forward-same-level 1)
		 (error (goto-char end))))))
	  (when swallowp
	    (message
	     "Inserted text not folded because that would swallow text"))

	  (goto-char end)
	  (skip-chars-forward " \t\n\r")
	  (forward-line 0)
	  (push-mark beg 'nomsg)))
       ((and subtreep org-yank-adjusted-subtrees)
        (let ((beg (line-beginning-position)))
	  (org-paste-subtree nil nil 'for-yank)
	  (push-mark beg 'nomsg)))
       (t
	(call-interactively command))))))

(defun org-yank-folding-would-swallow-text (beg end)
  "Would `hide-subtree' at BEG swallow any text after END?"
  (let (level)
    (org-with-limited-levels
     (save-excursion
       (goto-char beg)
       (when (or (looking-at org-outline-regexp)
		 (re-search-forward org-outline-regexp-bol end t))
	 (setq level (org-outline-level)))
       (goto-char end)
       (skip-chars-forward " \t\r\n\v\f")
       (not (or (eobp)
	      (and (bolp) (looking-at-p org-outline-regexp)
		   (<= (org-outline-level) level))))))))

;;;###autoload
(defun org-copy-visible (beg end)
  "Copy the visible parts of the region."
  (interactive "r")
  (let ((result ""))
    (while (/= beg end)
      (while (org-invisible-p beg)
	(setq beg (org-fold-next-visibility-change beg end)))
      (let ((next (org-fold-next-visibility-change beg end)))
	(setq result (concat result (buffer-substring beg next)))
	(setq beg next)))
    ;; Prevent Emacs from adding full selected text to `kill-ring'
    ;; when `select-enable-primary' is non-nil.  This special value of
    ;; `deactivate-mark' only works since Emacs 29.
    (setq deactivate-mark 'dont-save)
    (kill-new result)
    (message "Visible strings have been copied to the kill ring.")))

(declare-function org-table-copy-region "org-table-edit" (beg end &optional cut))
;;;###autoload
(defun org-copy-special ()
  "Copy region in table or copy current subtree.
Calls `org-table-copy-region' or `org-copy-subtree', depending on
context.  See the individual commands for more information."
  (interactive)
  (call-interactively
   (if (org-at-table-p)
       (progn
         (require 'org-table-edit)
         #'org-table-copy-region)
     #'org-copy-subtree)))

(declare-function org-table-cut-region "org-table-edit" (beg end))
;;;###autoload
(defun org-cut-special ()
  "Cut region in table or cut current subtree.
Calls `org-table-cut-region' or `org-cut-subtree', depending on
context.  See the individual commands for more information."
  (interactive)
  (call-interactively
   (if (org-at-table-p)
       (progn
         (require 'org-table-edit)
         #'org-table-cut-region)
     #'org-cut-subtree)))

(declare-function org-table-paste-rectangle "org-table-edit" ())
;;;###autoload
(defun org-paste-special (arg)
  "Paste rectangular region into table, or past subtree relative to level.
Calls `org-table-paste-rectangle' or `org-paste-subtree', depending on context.
See the individual commands for more information."
  (interactive "P")
  (if (org-at-table-p)
      (progn
        (require 'org-table-edit)
        (org-table-paste-rectangle))
    (org-paste-subtree arg)))



(provide 'org-edit)

;;; org-edit.el ends here
