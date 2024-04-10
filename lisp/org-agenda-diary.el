;;; org-agenda-diary.el --- Agenda diary integration         -*- lexical-binding: t; -*-

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
;; This library implements diary support for Org agenda.

;;; Code:

(require 'org-macs)
(require 'diary-lib)
(require 'org-agenda-line-format)
(require 'org-agenda-files)

(defvar org-disable-agenda-to-diary nil)          ;Dynamically-scoped param.
(defvar org-diary-last-run-time nil)

(defun org-get-entries-from-diary (date)
  "Get the (Emacs Calendar) diary entries for DATE."
  (let* ((diary-fancy-buffer "*temporary-fancy-diary-buffer*")
	 (diary-display-function #'diary-fancy-display)
	 (pop-up-frames nil)
	 (diary-list-entries-hook
	  (cons 'org-diary-default-entry diary-list-entries-hook))
	 (diary-file-name-prefix nil) ; turn this feature off
	 (diary-modify-entry-list-string-function
	  #'org-modify-diary-entry-string)
	 (diary-time-regexp (concat "^" diary-time-regexp))
	 entries
	 (org-disable-agenda-to-diary t))
    (save-excursion
      (save-window-excursion
        (diary-list-entries date 1)))
    (if (not (get-buffer diary-fancy-buffer))
	(setq entries nil)
      (with-current-buffer diary-fancy-buffer
	(setq buffer-read-only nil)
	(if (zerop (buffer-size))
	    ;; No entries
	    (setq entries nil)
	  ;; Omit the date and other unnecessary stuff
	  (org-agenda-cleanup-fancy-diary)
	  ;; Add prefix to each line and extend the text properties
	  (if (zerop (buffer-size))
	      (setq entries nil)
	    (setq entries (buffer-substring (point-min) (- (point-max) 1)))
	    (setq entries
		  (with-temp-buffer
		    (insert entries) (goto-char (point-min))
		    (while (re-search-forward "\n[ \t]+\\(.+\\)$" nil t)
		      (unless (save-match-data (string-match diary-time-regexp (match-string 1)))
			(replace-match (concat "; " (match-string 1)))))
		    (buffer-string)))))
	(set-buffer-modified-p nil)
	(kill-buffer diary-fancy-buffer)))
    (when entries
      (setq entries (org-split-string entries "\n"))
      (setq entries
	    (mapcar
	     (lambda (x)
	       (setq x (org-agenda-format-item "" x nil "Diary" nil 'time))
	       ;; Extend the text properties to the beginning of the line
	       (org-add-props x (text-properties-at (1- (length x)) x)
		 'type "diary" 'date date 'face 'org-agenda-diary))
	     entries)))))

(defvar org-agenda-cleanup-fancy-diary-hook nil
  "Hook run when the fancy diary buffer is cleaned up.")

(defun org-agenda-cleanup-fancy-diary ()
  "Remove unwanted stuff in buffer created by `diary-fancy-display'.
This gets rid of the date, the underline under the date, and the
dummy entry installed by Org mode to ensure non-empty diary for
each date.  It also removes lines that contain only whitespace."
  (goto-char (point-min))
  (if (looking-at ".*?:[ \t]*")
      (progn
	(replace-match "")
	(re-search-forward "\n=+$" nil t)
	(replace-match "")
	(while (re-search-backward "^ +\n?" nil t) (replace-match "")))
    (re-search-forward "\n=+$" nil t)
    (delete-region (point-min) (min (point-max) (1+ (match-end 0)))))
  (goto-char (point-min))
  (while (re-search-forward "^ +\n" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (when (re-search-forward "^Org mode dummy\n?" nil t)
    (replace-match ""))
  (run-hooks 'org-agenda-cleanup-fancy-diary-hook))

(defun org-modify-diary-entry-string (string)
  "Add text properties to string, allowing Org to act on it."
  (org-add-props string nil
    'mouse-face 'highlight
    'help-echo (if buffer-file-name
		   (format "mouse-2 or RET jump to diary file %s"
			   (abbreviate-file-name buffer-file-name))
		 "")
    'org-agenda-diary-link t
    'org-marker (org-agenda-new-marker (line-beginning-position))))

(defvar original-date) ; dynamically scoped, calendar.el does scope this
(defun org-diary-default-entry ()
  "Add a dummy entry to the diary.
Needed to avoid empty dates which mess up holiday display."
  ;; Catch the error if dealing with the new add-to-diary-alist
  (when org-disable-agenda-to-diary
    (diary-add-to-list original-date "Org mode dummy" "")))


(defvar org-agenda-entry-types) ; defined in org-agenda-search.el
(declare-function org-agenda-get-day-entries "org-agenda-search" (file date &rest args))
(declare-function org-set-sorting-strategy "org-agenda" (key))
(declare-function org-agenda-finalize-entries "org-agenda" (list &optional type))
;;;###autoload
(defun org-diary (&rest args)
  "Return diary information from org files.
This function can be used in a \"sexp\" diary entry in the Emacs calendar.
It accesses org files and extracts information from those files to be
listed in the diary.  The function accepts arguments specifying what
items should be listed.  For a list of arguments allowed here, see the
variable `org-agenda-entry-types'.

The call in the diary file should look like this:

   &%%(org-diary) ~/path/to/some/orgfile.org

Use a separate line for each org file to check.  Or, if you omit the file name,
all files listed in `org-agenda-files' will be checked automatically:

   &%%(org-diary)

If you don't give any arguments (as in the example above), the default value
of `org-agenda-entry-types' is used: (:deadline :scheduled :timestamp :sexp).
So the example above may also be written as

   &%%(org-diary :deadline :timestamp :sexp :scheduled)

The function expects the lisp variables `entry' and `date' to be provided
by the caller, because this is how the calendar works.  Don't use this
function from a program - use `org-agenda-get-day-entries' instead."
  (with-no-warnings (defvar date) (defvar entry))
  (require 'org-agenda)
  (require 'org-agenda-search)
  (when (> (- (float-time)
	      org-agenda-last-marker-time)
	   5)
    ;; I am not sure if this works with sticky agendas, because the marker
    ;; list is then no longer a global variable.
    (org-agenda-reset-markers))
  (org-compile-prefix-format 'agenda)
  (org-set-sorting-strategy 'agenda)
  (setq args (or args org-agenda-entry-types))
  (let* ((files (if (and entry (stringp entry) (string-match "\\S-" entry))
		    (list entry)
		  (org-agenda-files t)))
	 (time (float-time))
	 file rtn results)
    (when (or (not org-diary-last-run-time)
	      (> (- time
		    org-diary-last-run-time)
		 3))
      (org-agenda-prepare-buffers files))
    (setq org-diary-last-run-time time)
    ;; If this is called during org-agenda, don't return any entries to
    ;; the calendar.  Org Agenda will list these entries itself.
    (when org-disable-agenda-to-diary (setq files nil))
    (while (setq file (pop files))
      (require 'org-agenda-search)
      (setq rtn (apply #'org-agenda-get-day-entries file date args))
      (setq results (append results rtn)))
    (when results
      (setq results
	    (mapcar (lambda (i) (replace-regexp-in-string
			    org-link-bracket-re "\\2" i))
		    results))
      (concat (org-agenda-finalize-entries results) "\n"))))

(provide 'org-agenda-diary)

;;; org-agenda-diary.el ends here
