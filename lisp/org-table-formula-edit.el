;;; org-table-formula-edit.el --- Interactive editing of Org table formulas        -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements a major mode to edit Org table formulas
;; interactively.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-keys)
(require 'org-table-core)
(require 'org-table-formula)
(require 'org-table-move)
(require 'org-mode-common)

(defvar org-selected-window nil
  "Used in various places to store a window configuration.")
(defvar org-table-buffer-is-an nil)
(defvar org-table-rectangle-overlays nil)

(defvar-local org-table-overlay-coordinates nil
  "Overlay coordinates after each align of a table.")

(defvar org-table-fedit-map
  (let ((map (make-sparse-keymap)))
    (org-defkey map "\C-x\C-s"      'org-table-fedit-finish)
    (org-defkey map "\C-c\C-s"      'org-table-fedit-finish)
    (org-defkey map "\C-c\C-c"      'org-table-fedit-finish)
    (org-defkey map "\C-c'"         'org-table-fedit-finish)
    (org-defkey map "\C-c\C-q"      'org-table-fedit-abort)
    (org-defkey map "\C-c?"	    'org-table-show-reference)
    (org-defkey map [(meta shift up)]    'org-table-fedit-line-up)
    (org-defkey map [(meta shift down)]  'org-table-fedit-line-down)
    (org-defkey map [(shift up)]    'org-table-fedit-ref-up)
    (org-defkey map [(shift down)]  'org-table-fedit-ref-down)
    (org-defkey map [(shift left)]  'org-table-fedit-ref-left)
    (org-defkey map [(shift right)] 'org-table-fedit-ref-right)
    (org-defkey map [(meta up)]     'org-table-fedit-scroll-down)
    (org-defkey map [(meta down)]   'org-table-fedit-scroll)
    (org-defkey map [(meta tab)]    'lisp-complete-symbol)
    (org-defkey map "\M-\C-i"       'lisp-complete-symbol)
    (org-defkey map [(tab)]	    'org-table-fedit-lisp-indent)
    (org-defkey map "\C-i"	    'org-table-fedit-lisp-indent)
    (org-defkey map "\C-c\C-r" 'org-table-fedit-toggle-ref-type)
    (org-defkey map "\C-c}"    'org-table-fedit-toggle-coordinates)
    map))

(easy-menu-define org-table-fedit-menu org-table-fedit-map "Org Edit Formulas Menu."
  '("Edit-Formulas"
    ["Finish and Install" org-table-fedit-finish t]
    ["Finish, Install, and Apply" (org-table-fedit-finish t) :keys "C-u C-c C-c"]
    ["Abort" org-table-fedit-abort t]
    "--"
    ["Pretty-Print Lisp Formula" org-table-fedit-lisp-indent t]
    ["Complete Lisp Symbol" lisp-complete-symbol t]
    "--"
    "Shift Reference at Point"
    ["Up" org-table-fedit-ref-up t]
    ["Down" org-table-fedit-ref-down t]
    ["Left" org-table-fedit-ref-left t]
    ["Right" org-table-fedit-ref-right t]
    "-"
    "Change Test Row for Column Formulas"
    ["Up" org-table-fedit-line-up t]
    ["Down" org-table-fedit-line-down t]
    "--"
    ["Scroll Table Window" org-table-fedit-scroll t]
    ["Scroll Table Window down" org-table-fedit-scroll-down t]
    ["Show Table Grid" org-table-fedit-toggle-coordinates
     :style toggle :selected (with-current-buffer (marker-buffer org-pos)
			       org-table-overlay-coordinates)]
    "--"
    ["Standard Refs (B3 instead of @3$2)" org-table-fedit-toggle-ref-type
     :style toggle :selected org-table-buffer-is-an]))

(defvar org-table--fedit-source nil
  "Position of the TBLFM line being edited.")

;;;###autoload
(defun org-table-edit-formulas ()
  "Edit the formulas of the current table in a separate buffer."
  (interactive)
  (let ((at-tblfm (org-at-TBLFM-p)))
    (unless (or at-tblfm (org-at-table-p))
      (user-error "Not at a table"))
    (save-excursion
      ;; Move point within the table before analyzing it.
      (when at-tblfm (re-search-backward "^[ \t]*|"))
      (org-table-analyze))
    (let ((key (org-table-current-field-formula 'key 'noerror))
	  (eql (sort (org-table-get-stored-formulas t (and at-tblfm (point)))
		     #'org-table-formula-less-p))
	  (pos (point-marker))
	  (source (copy-marker (line-beginning-position)))
	  (startline 1)
	  (wc (current-window-configuration))
	  (sel-win (selected-window))
	  (titles '((column . "# Column Formulas\n")
		    (field . "# Field and Range Formulas\n")
		    (named . "# Named Field Formulas\n"))))
      (let ((pop-up-frames nil))
        ;; We explicitly prohibit creating edit buffer in a new frame
        ;; - such configuration is not supported.
        (switch-to-buffer-other-window "*Edit Formulas*"))
      (erase-buffer)
      ;; Keep global-font-lock-mode from turning on font-lock-mode
      (let ((font-lock-global-modes '(not fundamental-mode)))
	(fundamental-mode))
      (setq-local font-lock-global-modes (list 'not major-mode))
      (setq-local org-pos pos)
      (setq-local org-table--fedit-source source)
      (setq-local org-window-configuration wc)
      (setq-local org-selected-window sel-win)
      (use-local-map org-table-fedit-map)
      (add-hook 'post-command-hook #'org-table-fedit-post-command t t)
      (setq startline (org-current-line))
      (dolist (entry eql)
	(let* ((type (cond
		      ((string-match "\\`\\$\\([0-9]+\\|[<>]+\\)\\'"
				     (car entry))
		       'column)
		      ((equal (string-to-char (car entry)) ?@) 'field)
		      (t 'named)))
	       (title (assq type titles)))
	  (when title
	    (unless (bobp) (insert "\n"))
	    (insert
	     (org-add-props (cdr title) nil 'face font-lock-comment-face))
	    (setq titles (remove title titles)))
	  (when (equal key (car entry)) (setq startline (org-current-line)))
	  (let ((s (concat
		    (if (memq (string-to-char (car entry)) '(?@ ?$)) "" "$")
		    (car entry) " = " (cdr entry) "\n")))
	    (remove-text-properties 0 (length s) '(face nil) s)
	    (insert s))))
      (when (eq org-table-use-standard-references t)
	(org-table-fedit-toggle-ref-type))
      (org-goto-line startline)
      (message "%s" (substitute-command-keys "\\<org-mode-map>\
Edit formulas, finish with `\\[org-ctrl-c-ctrl-c]' or `\\[org-edit-special]'.  \
See menu for more commands.")))))

(defun org-table-fedit-post-command ()
  (when (not (memq this-command '(lisp-complete-symbol)))
    (let ((win (selected-window)))
      (save-excursion
	(ignore-errors (org-table-show-reference))
	(select-window win)))))

(defun org-table-fedit-convert-buffer (function)
  "Convert all references in this buffer, using FUNCTION."
  (let ((origin (copy-marker (line-beginning-position))))
    (goto-char (point-min))
    (while (not (eobp))
      (insert (funcall function (buffer-substring (point) (line-end-position))))
      (delete-region (point) (line-end-position))
      (forward-line))
    (goto-char origin)
    (set-marker origin nil)))

(defun org-table-fedit-toggle-ref-type ()
  "Convert all references in the buffer from B3 to @3$2 and back."
  (interactive)
  (setq-local org-table-buffer-is-an (not org-table-buffer-is-an))
  (org-table-fedit-convert-buffer
   (if org-table-buffer-is-an
       'org-table-convert-refs-to-an 'org-table-convert-refs-to-rc))
  (message "Reference type switched to %s"
	   (if org-table-buffer-is-an "A1 etc" "@row$column")))

(defun org-table-fedit-ref-up ()
  "Shift the reference at point one row/hline up."
  (interactive)
  (org-table-fedit-shift-reference 'up))

(defun org-table-fedit-ref-down ()
  "Shift the reference at point one row/hline down."
  (interactive)
  (org-table-fedit-shift-reference 'down))

(defun org-table-fedit-ref-left ()
  "Shift the reference at point one field to the left."
  (interactive)
  (org-table-fedit-shift-reference 'left))

(defun org-table-fedit-ref-right ()
  "Shift the reference at point one field to the right."
  (interactive)
  (org-table-fedit-shift-reference 'right))

(defun org-table--rematch-and-replace (n &optional decr hline)
  "Re-match the group N, and replace it with the shifted reference."
  (or (match-end n) (user-error "Cannot shift reference in this direction"))
  (goto-char (match-beginning n))
  (and (looking-at (regexp-quote (match-string n)))
       (replace-match (org-table-shift-refpart (match-string 0) decr hline)
		      t t)))

(defun org-table-fedit-shift-reference (dir)
  (cond
   ((org-in-regexp "\\(\\<[a-zA-Z]\\)&")
    (if (memq dir '(left right))
	(org-table--rematch-and-replace 1 (eq dir 'left))
      (user-error "Cannot shift reference in this direction")))
   ((org-in-regexp "\\(\\<[a-zA-Z]\\{1,2\\}\\)\\([0-9]+\\)")
    ;; A B3-like reference
    (if (memq dir '(up down))
	(org-table--rematch-and-replace 2 (eq dir 'up))
      (org-table--rematch-and-replace 1 (eq dir 'left))))
   ((org-in-regexp
     "\\(@\\|\\.\\.\\)\\([-+]?\\(I+\\>\\|[0-9]+\\)\\)\\(\\$\\([-+]?[0-9]+\\)\\)?")
    ;; An internal reference
    (if (memq dir '(up down))
	(org-table--rematch-and-replace 2 (eq dir 'up) (match-end 3))
      (org-table--rematch-and-replace 5 (eq dir 'left))))))

(defun org-table-shift-refpart (ref &optional decr hline)
  "Shift a reference part REF.
If DECR is set, decrease the references row/column, else increase.
If HLINE is set, this may be a hline reference, it certainly is not
a translation reference."
  (save-match-data
    (let* ((sign (string-match "^[-+]" ref)) n)

      (if sign (setq sign (substring ref 0 1) ref (substring ref 1)))
      (cond
       ((and hline (string-match "^I+" ref))
	(setq n (string-to-number (concat sign (number-to-string (length ref)))))
	(setq n (+ n (if decr -1 1)))
	(if (= n 0) (setq n (+ n (if decr -1 1))))
	(if sign
	    (setq sign (if (< n 0) "-" "+") n (abs n))
	  (setq n (max 1 n)))
	(concat sign (make-string n ?I)))

       ((string-match "^[0-9]+" ref)
	(setq n (string-to-number (concat sign ref)))
	(setq n (+ n (if decr -1 1)))
	(if sign
	    (concat (if (< n 0) "-" "+") (number-to-string (abs n)))
	  (number-to-string (max 1 n))))

       ((string-match "^[a-zA-Z]+" ref)
	(org-number-to-letters
	 (max 1 (+ (org-letters-to-number ref) (if decr -1 1)))))

       (t (user-error "Cannot shift reference"))))))

(defun org-table-fedit-toggle-coordinates ()
  "Toggle the display of coordinates in the referenced table."
  (interactive)
  (let ((pos (marker-position org-pos)))
    (with-current-buffer (marker-buffer org-pos)
      (save-excursion
	(goto-char pos)
	(org-table-toggle-coordinate-overlays)))))

(defun org-table-fedit-finish (&optional arg)
  "Parse the buffer for formula definitions and install them.
With prefix ARG, apply the new formulas to the table."
  (interactive "P")
  (org-table-remove-rectangle-highlight)
  (when org-table-use-standard-references
    (org-table-fedit-convert-buffer 'org-table-convert-refs-to-rc)
    (setq org-table-buffer-is-an nil))
  (let ((pos org-pos)
	(sel-win org-selected-window)
	(source org-table--fedit-source)
	eql)
    (goto-char (point-min))
    (while (re-search-forward
	    "^\\(@[-+I<>0-9.$@]+\\|@?[0-9]+\\|\\$\\([a-zA-Z0-9]+\\|[<>]+\\)\\) *= *\\(.*\\(\n[ \t]+.*$\\)*\\)"
	    nil t)
      (let ((var (match-string 1))
	    (form (org-trim (match-string 3))))
	(unless (equal form "")
	  (while (string-match "[ \t]*\n[ \t]*" form)
	    (setq form (replace-match " " t t form)))
	  (when (assoc var eql)
	    (user-error "Double formulas for %s" var))
	  (push (cons var form) eql))))
    (set-window-configuration org-window-configuration)
    (select-window sel-win)
    (goto-char source)
    (org-table-store-formulas eql)
    (set-marker pos nil)
    (set-marker source nil)
    (when-let ((window (get-buffer-window "*Edit Formulas*" t)))
      (quit-window 'kill window))
    (when (get-buffer "*Edit Formulas*") (kill-buffer "*Edit Formulas*"))
    (if arg
	(org-table-recalculate 'all)
      (message "New formulas installed - press C-u C-c C-c to apply."))))

(defun org-table-fedit-abort ()
  "Abort editing formulas, without installing the changes."
  (interactive)
  (org-table-remove-rectangle-highlight)
  (let ((pos org-pos) (sel-win org-selected-window))
    (set-window-configuration org-window-configuration)
    (select-window sel-win)
    (goto-char pos)
    (move-marker pos nil)
    (message "Formula editing aborted without installing changes")))

(declare-function org-delete-backward-char "org-edit" (n))
(defun org-table-fedit-lisp-indent ()
  "Pretty-print and re-indent Lisp expressions in the Formula Editor."
  (interactive)
  (let ((pos (point)) beg end ind)
    (forward-line 0)
    (cond
     ((looking-at "[ \t]")
      (goto-char pos)
      (call-interactively #'lisp-indent-line))
     ((looking-at "[$&@0-9a-zA-Z]+ *= *[^ \t\n']") (goto-char pos))
     ((looking-at "[$&@0-9a-zA-Z]+ *= *'(")
      (goto-char (- (match-end 0) 2))
      (setq beg (point))
      (setq ind (make-string (current-column) ?\ ))
      (condition-case nil (forward-sexp 1)
	(error
	 (user-error "Cannot pretty-print Lisp expression: Unbalanced parenthesis")))
      (setq end (point))
      (save-restriction
	(narrow-to-region beg end)
	(if (eq last-command this-command)
	    (progn
	      (goto-char (point-min))
	      (setq this-command nil)
	      (while (re-search-forward "[ \t]*\n[ \t]*" nil t)
		(replace-match " ")))
	  (pp-buffer)
	  (untabify (point-min) (point-max))
	  (goto-char (1+ (point-min)))
	  (while (re-search-forward "^." nil t)
	    (forward-line 0)
	    (insert ind))
	  (goto-char (point-max))
	  (org-delete-backward-char 1)))
      (goto-char beg))
     (t nil))))

(defun org-table-fedit-line-up ()
  "Move cursor one line up in the window showing the table."
  (interactive)
  (org-table-fedit-move 'previous-line))

(defun org-table-fedit-line-down ()
  "Move cursor one line down in the window showing the table."
  (interactive)
  (org-table-fedit-move 'next-line))

(defun org-table-fedit-move (command)
  "Move the cursor in the window showing the table.
Use COMMAND to do the motion, repeat if necessary to end up in a data line."
  (let ((org-table-allow-automatic-line-recalculation nil)
	(pos org-pos) (win (selected-window)) p)
    (select-window (get-buffer-window (marker-buffer org-pos)))
    (setq p (point))
    (call-interactively command)
    (while (and (org-at-table-p)
		(org-at-table-hline-p))
      (call-interactively command))
    (or (org-at-table-p) (goto-char p))
    (move-marker pos (point))
    (select-window win)))

(defun org-table-fedit-scroll (N)
  (interactive "p")
  (let ((other-window-scroll-buffer (marker-buffer org-pos)))
    (scroll-other-window N)))

(defun org-table-fedit-scroll-down (N)
  (interactive "p")
  (org-table-fedit-scroll (- N)))

(defun org-table-add-rectangle-overlay (beg end &optional face)
  "Add a new overlay."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face (or face 'secondary-selection))
    (push ov org-table-rectangle-overlays)))

(defun org-table-highlight-rectangle (&optional beg end face)
  "Highlight rectangular region in a table.
When buffer positions BEG and END are provided, use them to
delimit the region to highlight.  Otherwise, refer to point.  Use
FACE, when non-nil, for the highlight."
  (let* ((beg (or beg (point)))
	 (end (or end (point)))
	 (b (min beg end))
	 (e (max beg end))
	 (start-coordinates
	  (save-excursion
	    (goto-char b)
	    (cons (line-beginning-position) (org-table-current-column))))
	 (end-coordinates
	  (save-excursion
	    (goto-char e)
	    (cons (line-beginning-position) (org-table-current-column)))))
    (when (boundp 'org-show-positions)
      (setq org-show-positions (cons b (cons e org-show-positions))))
    (goto-char (car start-coordinates))
    (let ((column-start (min (cdr start-coordinates) (cdr end-coordinates)))
	  (column-end (max (cdr start-coordinates) (cdr end-coordinates)))
	  (last-row (car end-coordinates)))
      (while (<= (point) last-row)
	(when (looking-at org-table-dataline-regexp)
	  (org-table-goto-column column-start)
	  (skip-chars-backward "^|\n")
	  (let ((p (point)))
	    (org-table-goto-column column-end)
	    (skip-chars-forward "^|\n")
	    (org-table-add-rectangle-overlay p (point) face)))
	(forward-line)))
    (goto-char (car start-coordinates)))
  (add-hook 'before-change-functions #'org-table-remove-rectangle-highlight))

(defun org-table-remove-rectangle-highlight (&rest _ignore)
  "Remove the rectangle overlays."
  (unless org-inhibit-highlight-removal
    (remove-hook 'before-change-functions 'org-table-remove-rectangle-highlight)
    (mapc 'delete-overlay org-table-rectangle-overlays)
    (setq org-table-rectangle-overlays nil)))

(defvar-local org-table-coordinate-overlays nil
  "Collects the coordinate grid overlays, so that they can be removed.")
(put 'org-table-coordinate-overlays 'permanent-local t)

(defun org-table-overlay-coordinates ()
  "Add overlays to the table at point, to show row/column coordinates."
  (interactive)
  (mapc 'delete-overlay org-table-coordinate-overlays)
  (setq org-table-coordinate-overlays nil)
  (save-excursion
    (let ((id 0) (ih 0) hline eol str ov)
      (goto-char (org-table-begin))
      (while (org-at-table-p)
        (setq eol (line-end-position))
        (setq ov (make-overlay (line-beginning-position)
                               (1+ (line-beginning-position))))
	(push ov org-table-coordinate-overlays)
	(setq hline (looking-at org-table-hline-regexp))
	(setq str (if hline (format "I*%-2d" (setq ih (1+ ih)))
		    (format "%4d" (setq id (1+ id)))))
	(org-overlay-before-string ov str 'org-special-keyword 'evaporate)
	(when hline
	  (let ((ic 0))
	    (while (re-search-forward "[+|]\\(-+\\)" eol t)
	      (cl-incf ic)
	      (let* ((beg (1+ (match-beginning 0)))
		     (s1 (format "$%d" ic))
		     (s2 (org-number-to-letters ic))
		     (str (if (eq t org-table-use-standard-references) s2 s1))
		     (ov (make-overlay beg (+ beg (length str)))))
		(push ov org-table-coordinate-overlays)
		(org-overlay-display ov str 'org-special-keyword 'evaporate)))))
	(forward-line)))))

;;;###autoload
(defun org-table-toggle-coordinate-overlays ()
  "Toggle the display of Row/Column numbers in tables."
  (interactive)
  (if (not (org-at-table-p))
      (user-error "Not on a table")
    (setq org-table-overlay-coordinates (not org-table-overlay-coordinates))
    (when (and (org-at-table-p) org-table-overlay-coordinates)
      (org-table-align))
    (unless org-table-overlay-coordinates
      (mapc 'delete-overlay org-table-coordinate-overlays)
      (setq org-table-coordinate-overlays nil))
    (message "Tables Row/Column numbers display turned %s"
	     (if org-table-overlay-coordinates "on" "off"))))

(defun org-table-show-reference (&optional local)
  "Show the location/value of the $ expression at point.
When LOCAL is non-nil, show references for the table at point."
  (interactive)
  (org-table-remove-rectangle-highlight)
  (when local (org-table-analyze))
  (catch 'exit
    (let ((pos (if local (point) org-pos))
	  (face2 'highlight)
	  (org-inhibit-highlight-removal t)
	  (win (selected-window))
	  (org-show-positions nil)
	  var name e what match dest)
      (setq what (cond
		  ((org-in-regexp "^@[0-9]+[ \t=]")
		   (setq match (concat (substring (match-string 0) 0 -1)
				       "$1.."
				       (substring (match-string 0) 0 -1)
				       "$100"))
		   'range)
		  ((or (org-in-regexp org-table-range-regexp2)
		       (org-in-regexp org-table-translate-regexp)
		       (org-in-regexp org-table-range-regexp))
		   (setq match
			 (save-match-data
			   (org-table-convert-refs-to-rc (match-string 0))))
		   'range)
		  ((org-in-regexp "\\$[a-zA-Z][a-zA-Z0-9]*") 'name)
		  ((org-in-regexp "\\$[0-9]+") 'column)
		  ((not local) nil)
		  (t (user-error "No reference at point")))
	    match (and what (or match (match-string 0))))
      (when (and  match (not (equal (match-beginning 0) (line-beginning-position))))
	(org-table-add-rectangle-overlay (match-beginning 0) (match-end 0)
					 'secondary-selection))
      (add-hook 'before-change-functions
		#'org-table-remove-rectangle-highlight)
      (when (eq what 'name) (setq var (substring match 1)))
      (when (eq what 'range)
	(unless (eq (string-to-char match) ?@) (setq match (concat "@" match)))
	(setq match (org-table-formula-substitute-names match)))
      (unless local
	(save-excursion
	  (end-of-line)
	  (re-search-backward "^\\S-" nil t)
	  (forward-line 0)
	  (when (looking-at "\\(\\$[0-9a-zA-Z]+\\|@[0-9]+\\$[0-9]+\\|[a-zA-Z]+\
\\([0-9]+\\|&\\)\\) *=")
	    (setq dest
		  (save-match-data
		    (org-table-convert-refs-to-rc (match-string 1))))
	    (org-table-add-rectangle-overlay
	     (match-beginning 1) (match-end 1) face2))))
      (if (and (markerp pos) (marker-buffer pos))
	  (if (get-buffer-window (marker-buffer pos))
	      (select-window (get-buffer-window (marker-buffer pos)))
	    (switch-to-buffer-other-window (get-buffer-window
					    (marker-buffer pos)))))
      (goto-char pos)
      (org-table--force-dataline)
      (let ((table-start
	     (if local org-table-current-begin-pos (org-table-begin))))
	(when dest
	  (setq name (substring dest 1))
	  (cond
	   ((string-match-p "\\`\\$[a-zA-Z][a-zA-Z0-9]*" dest)
	    (org-table-goto-field dest))
	   ((string-match-p "\\`@\\([1-9][0-9]*\\)\\$\\([1-9][0-9]*\\)\\'"
			    dest)
	    (org-table-goto-field dest))
	   (t (org-table-goto-column (string-to-number name))))
	  (move-marker pos (point))
	  (org-table-highlight-rectangle nil nil face2))
	(cond
	 ((equal dest match))
	 ((not match))
	 ((eq what 'range)
	  (ignore-errors (org-table-get-range match table-start nil 'highlight)))
	 ((setq e (assoc var org-table-named-field-locations))
	  (org-table-goto-field var)
	  (org-table-highlight-rectangle)
	  (message "Named field, column %d of line %d" (nth 2 e) (nth 1 e)))
	 ((setq e (assoc var org-table-column-names))
	  (org-table-goto-column (string-to-number (cdr e)))
	  (org-table-highlight-rectangle)
	  (goto-char table-start)
	  (if (re-search-forward (concat "^[ \t]*| *! *.*?| *\\(" var "\\) *|")
				 (org-table-end) t)
	      (progn
		(goto-char (match-beginning 1))
		(org-table-highlight-rectangle)
		(message "Named column (column %s)" (cdr e)))
	    (user-error "Column name not found")))
	 ((eq what 'column)
	  ;; Column number.
	  (org-table-goto-column (string-to-number (substring match 1)))
	  (org-table-highlight-rectangle)
	  (message "Column %s" (substring match 1)))
	 ((setq e (assoc var org-table-local-parameters))
	  (goto-char table-start)
	  (if (re-search-forward (concat "^[ \t]*| *\\$ *.*?| *\\(" var "=\\)") nil t)
	      (progn
		(goto-char (match-beginning 1))
		(org-table-highlight-rectangle)
		(message "Local parameter."))
	    (user-error "Parameter not found")))
	 ((not var) (user-error "No reference at point"))
	 ((setq e (assoc var (org-table-formula-constants-local)))
	  (message "Local Constant: $%s=%s in #+CONSTANTS line."
		   var (cdr e)))
	 ((setq e (assoc var org-table-formula-constants))
	  (message "Constant: $%s=%s in `org-table-formula-constants'."
		   var (cdr e)))
	 ((setq e (and (fboundp 'constants-get) (constants-get var)))
          (defvar constants-unit-system) ; defined in constants.el
	  (message "Constant: $%s=%s, from `constants.el'%s."
		   var e (format " (%s units)" constants-unit-system)))
	 (t (user-error "Undefined name $%s" var)))
	(goto-char pos)
	(when (and org-show-positions
		   (not (memq this-command '(org-table-fedit-scroll
					   org-table-fedit-scroll-down))))
	  (push pos org-show-positions)
	  (push table-start org-show-positions)
	  (let ((min (apply 'min org-show-positions))
		(max (apply 'max org-show-positions)))
	    (set-window-start (selected-window) min)
	    (goto-char max)
	    (or (pos-visible-in-window-p max)
		(set-window-start (selected-window) max)))))
      (select-window win))))

;;;###autoload
(defun org-table-field-info (_arg)
  "Show info about the current field, and highlight any reference at point."
  (interactive "P")
  (unless (org-at-table-p) (user-error "Not at a table"))
  (org-table-analyze)
  (save-excursion
    (let* ((pos (point))
	   (col (org-table-current-column))
	   (cname (car (rassoc (number-to-string col) org-table-column-names)))
	   (name (car (rassoc (list (count-lines org-table-current-begin-pos
						 (line-beginning-position))
				    col)
			      org-table-named-field-locations)))
	   (eql (org-table-expand-lhs-ranges
		 (mapcar
		  (lambda (e)
		    (cons (org-table-formula-handle-first/last-rc (car e))
			  (cdr e)))
		  (org-table-get-stored-formulas))))
	   (dline (org-table-current-dline))
	   (ref (format "@%d$%d" dline col))
	   (ref1 (org-table-convert-refs-to-an ref))
	   ;; Prioritize field formulas over column formulas.
	   (fequation (or (assoc name eql) (assoc ref eql)))
	   (cequation (assoc (format "$%d" col) eql))
	   (eqn (or fequation cequation)))
      (let ((p (and eqn (get-text-property 0 :orig-eqn (car eqn)))))
	(when p (setq eqn p)))
      (goto-char pos)
      (ignore-errors (org-table-show-reference 'local))
      (message "line @%d, col $%s%s, ref @%d$%d or %s%s%s"
	       dline col
	       (if cname (concat " or $" cname) "")
	       dline col ref1
	       (if name (concat " or $" name) "")
	       ;; FIXME: formula info not correct if special table line
	       (if eqn
		   (concat ", formula: "
			   (org-table-formula-to-user
			    (concat
			     (if (or (string-prefix-p "$" (car eqn))
				     (string-prefix-p "@" (car eqn)))
				 ""
			       "$")
			     (car eqn) "=" (cdr eqn))))
		 "")))))

(provide 'org-table-formula-edit)

;;; org-table-formula-edit.el ends here
