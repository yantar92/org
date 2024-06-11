;;; org-agenda-entry-text-mode.el --- Displaying entry text in agenda buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, text
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

;; This file implement agenda mode to display entry text in agenda
;; buffers.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-agenda-common)
(require 'org-regexps)

(defcustom org-agenda-add-entry-text-maxlines 0
  "Maximum number of entry text lines to be added to agenda.
This is only relevant when `org-agenda-add-entry-text' is part of
`org-agenda-before-write-hook', which is the default.
When this is 0, nothing will happen.  When it is greater than 0, it
specifies the maximum number of lines that will be added for each entry
that is listed in the agenda view.

Note that this variable is not used during display, only when exporting
the agenda.  For agenda display, see the variables `org-agenda-entry-text-mode'
and `org-agenda-entry-text-maxlines'."
  :group 'org-agenda
  :type 'integer)

(defcustom org-agenda-add-entry-text-descriptive-links t
  "Non-nil means export org-links as descriptive links in agenda added text.
This variable applies to the text added to the agenda when
`org-agenda-add-entry-text-maxlines' is larger than 0.
When this variable is nil, the URL will (also) be shown."
  :group 'org-agenda
  :type 'boolean)

(defcustom org-agenda-entry-text-maxlines 5
  "Number of text lines to be added when `E' is pressed in the agenda.

Note that this variable only used during agenda display.  To add entry text
when exporting the agenda, configure the variable
`org-agenda-add-entry-text-maxlines'."
  :group 'org-agenda
  :type 'integer)

(defcustom org-agenda-entry-text-exclude-regexps nil
  "List of regular expressions to clean up entry text.
The complete matches of all regular expressions in this list will be
removed from entry text before it is shown in the agenda."
  :group 'org-agenda
  :type '(repeat (regexp)))

(defcustom org-agenda-entry-text-leaders "    > "
  "Text prepended to the entry text in agenda buffers."
  :version "24.4"
  :package-version '(Org . "8.0")
  :group 'org-agenda
  :type 'string)

(defvar org-agenda-entry-text-cleanup-hook nil
  "Hook that is run after basic cleanup of entry text to be shown in agenda.
This cleanup is done in a temporary buffer, so the function may inspect and
change the entire buffer.
Some default stuff like drawers and scheduling/deadline dates will already
have been removed when this is called, as will any matches for regular
expressions listed in `org-agenda-entry-text-exclude-regexps'.")

(declare-function org-activate-links "org-font-lock" (limit))
(declare-function outline-next-heading "outline" ())
(defun org-agenda-get-some-entry-text (marker n-lines &optional indent
					      &rest keep)
  "Extract entry text from MARKER, at most N-LINES lines.
This will ignore drawers etc, just get the text.
If INDENT is given, prefix every line with this string.  If KEEP is
given, it is a list of symbols, defining stuff that should not be
removed from the entry content.  Currently only `planning' is allowed here."
  (let (txt drawer-re kwd-time-re ind)
    (save-excursion
      (with-current-buffer (marker-buffer marker)
	(if (not (derived-mode-p 'org-mode))
	    (setq txt "")
	  (org-with-wide-buffer
	   (goto-char marker)
	   (end-of-line 1)
	   (setq txt (buffer-substring
		      (min (1+ (point)) (point-max))
		      (progn (outline-next-heading) (point)))
		 drawer-re org-drawer-regexp
		 kwd-time-re (concat "^[ \t]*" org-keyword-time-regexp
				     ".*\n?"))
	   (with-temp-buffer
	     (insert txt)
	     (when org-agenda-add-entry-text-descriptive-links
	       (goto-char (point-min))
               (require 'org-font-lock)
	       (while (org-activate-links (point-max))
		 (goto-char (match-end 0))))
	     (goto-char (point-min))
	     (while (re-search-forward org-link-bracket-re (point-max) t)
	       (set-text-properties (match-beginning 0) (match-end 0)
				    nil))
	     (goto-char (point-min))
	     (while (re-search-forward drawer-re nil t)
	       (delete-region
		(match-beginning 0)
		(progn (re-search-forward
			"^[ \t]*:END:.*\n?" nil 'move)
		       (point))))
	     (unless (member 'planning keep)
	       (goto-char (point-min))
	       (while (re-search-forward kwd-time-re nil t)
		 (replace-match "")))
	     (goto-char (point-min))
	     (when org-agenda-entry-text-exclude-regexps
	       (let ((re-list org-agenda-entry-text-exclude-regexps)	re)
		 (while (setq re (pop re-list))
		   (goto-char (point-min))
		   (while (re-search-forward re nil t)
		     (replace-match "")))))
	     (goto-char (point-max))
	     (skip-chars-backward " \t\n")
	     (when (looking-at "[ \t\n]+\\'") (replace-match ""))

	     ;; find and remove min common indentation
	     (goto-char (point-min))
	     (untabify (point-min) (point-max))
	     (setq ind (org-current-text-indentation))
	     (while (not (eobp))
	       (unless (looking-at "[ \t]*$")
		 (setq ind (min ind (org-current-text-indentation))))
	       (forward-line 1))
	     (goto-char (point-min))
	     (while (not (eobp))
	       (unless (looking-at "[ \t]*$")
		 (move-to-column ind)
                 (delete-region (line-beginning-position) (point)))
	       (forward-line 1))

	     (run-hooks 'org-agenda-entry-text-cleanup-hook)

	     (goto-char (point-min))
	     (when indent
	       (while (and (not (eobp)) (re-search-forward "^" nil t))
		 (replace-match indent t t)))
	     (goto-char (point-min))
	     (while (looking-at "[ \t]*\n") (replace-match ""))
	     (goto-char (point-max))
	     (when (> (org-current-line)
		      n-lines)
	       (org-goto-line (1+ n-lines))
	       (backward-char 1))
	     (setq txt (buffer-substring (point-min) (point))))))))
    txt))

(defun org-agenda-entry-text-show-here ()
  "Add some text from the entry as context to the current line."
  (let (m txt o)
    (setq m (org-get-at-bol 'org-hd-marker))
    (unless (marker-buffer m)
      (error "No marker points to an entry here"))
    (setq txt (concat "\n" (org-no-properties
			    (org-agenda-get-some-entry-text
			     m org-agenda-entry-text-maxlines
			     org-agenda-entry-text-leaders))))
    (when (string-match "\\S-" txt)
      (setq o (make-overlay (line-beginning-position) (line-end-position)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'org-overlay-type 'agenda-entry-content)
      (overlay-put o 'after-string txt))))

(defun org-agenda-entry-text-show ()
  "Add entry context for all agenda lines."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (forward-line 0)
    (while (not (bobp))
      (when (org-get-at-bol 'org-hd-marker)
	(org-agenda-entry-text-show-here))
      (forward-line -1))))

(defun org-agenda-entry-text-hide ()
  "Remove any shown entry context."
  (mapc (lambda (o)
	  (when (eq (overlay-get o 'org-overlay-type)
		    'agenda-entry-content)
	    (delete-overlay o)))
	(overlays-in (point-min) (point-max))))

(declare-function org-agenda-set-mode-name "org-agenda-mode" ())
(defun org-agenda-entry-text-mode (&optional arg)
  "Toggle entry text mode in an agenda buffer."
  (interactive "P")
  (if (or org-agenda-tag-filter
	  org-agenda-category-filter
	  org-agenda-regexp-filter
	  org-agenda-top-headline-filter)
      (user-error "Can't show entry text in filtered views")
    (setq org-agenda-entry-text-mode (or (integerp arg)
					 (not org-agenda-entry-text-mode)))
    (org-agenda-entry-text-hide)
    (and org-agenda-entry-text-mode
	 (let ((org-agenda-entry-text-maxlines
		(if (integerp arg) arg org-agenda-entry-text-maxlines)))
	   (org-agenda-entry-text-show)))
    (require 'org-agenda-mode)
    (org-agenda-set-mode-name)
    (message "Entry text mode is %s%s"
	     (if org-agenda-entry-text-mode "on" "off")
	     (if (not org-agenda-entry-text-mode) ""
	       (format " (maximum number of lines is %d)"
		       (if (integerp arg) arg org-agenda-entry-text-maxlines))))))

(defun org-agenda-add-entry-text ()
  "Add entry text to agenda lines.
This will add a maximum of `org-agenda-add-entry-text-maxlines' lines of the
entry text following headings shown in the agenda.
Drawers will be excluded, also the line with scheduling/deadline info."
  (when (and (> org-agenda-add-entry-text-maxlines 0)
	     (not (bound-and-true-p org-mobile-creating-agendas)))
    (let (m txt)
      (goto-char (point-min))
      (while (not (eobp))
	(if (not (setq m (org-get-at-bol 'org-hd-marker)))
	    (forward-line 1)
	  (setq txt (org-agenda-get-some-entry-text
		     m org-agenda-add-entry-text-maxlines "    > "))
	  (end-of-line 1)
	  (if (string-match "\\S-" txt)
	      (insert "\n" txt)
	    (or (eobp) (forward-char 1))))))))

(provide 'org-agenda-entry-text-mode)

;;; org-agenda-entry-text-mode.el ends here
