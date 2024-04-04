;;; org-misc.el --- Miscellaneous Org mode commands -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

;;; Code:

;;; This library implements potentially useful, but probably rarely
;;; used commands.  The commands are autoloaded, but the library
;;; itself is not loaded explicitly by the rest of Org mode.

(require 'org-macs)
(org-assert-version)

(require 'org-element)

(declare-function org-require-autoloaded-modules "org")

;;; Bug reporting

;;;###autoload
(defun org-submit-bug-report ()
  "Submit a bug report on Org via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your Org version and configuration."
  (interactive)
  (require 'reporter)
  (defvar reporter-prompt-for-summary-p)
  (org-load-modules-maybe)
  (org-require-autoloaded-modules)
  (let ((reporter-prompt-for-summary-p "Bug report subject: "))
    (reporter-submit-bug-report
     "emacs-orgmode@gnu.org"
     (org-version nil 'full)
     (let (list)
       (save-window-excursion
         (pop-to-buffer
          (get-buffer-create "*Warn about privacy*")
          '(org-display-buffer-full-frame))
	 (erase-buffer)
	 (insert "You are about to submit a bug report to the Org mailing list.

If your report is about Org installation, please read this section:
https://orgmode.org/org.html#Installation

Please read https://orgmode.org/org.html#Feedback on how to make
a good report, it will help Org contributors fixing your problem.

Search https://lists.gnu.org/archive/html/emacs-orgmode/ to see
if the issue you are about to raise has already been dealt with.

We also would like to add your full Org and Outline configuration
to the bug report.  It will help us debugging the issue.

*HOWEVER*, some variables you have customized may contain private
information.  The names of customers, colleagues, or friends, might
appear in the form of file names, tags, todo states or search strings.
If you answer \"yes\" to the prompt, you might want to check and remove
such private information before sending the email.")
	 (add-text-properties (point-min) (point-max) '(face org-warning))
         (when (yes-or-no-p "Include your Org configuration and Org warning log?")
	   (mapatoms
	    (lambda (v)
	      (and (boundp v)
		   (string-match "\\`\\(org-\\|outline-\\)" (symbol-name v))
		   (or (and (symbol-value v)
			    (string-match "\\(-hook\\|-function\\)\\'" (symbol-name v)))
                       (eq v 'org--warnings)
		       (and
			(get v 'custom-type) (get v 'standard-value)
			(not (equal (symbol-value v)
			          (eval (car (get v 'standard-value)) t)))))
		   (push v list)))))
	 (kill-buffer (get-buffer "*Warn about privacy*"))
	 list))
     nil nil
     "Remember to cover the basics, that is, what you expected to happen and
what in fact did happen.  You don't know how to make a good report?  See

     https://orgmode.org/manual/Feedback.html#Feedback

Your bug report will be posted to the Org mailing list.
------------------------------------------------------------------------")
    (save-excursion
      (when (re-search-backward "^\\(Subject: \\)Org mode version \\(.*?\\);[ \t]*\\(.*\\)" nil t)
	(replace-match "\\1[BUG] \\3 [\\2]")))))

;;; Customization

;;;###autoload
(defun org-customize ()
  "Call the customize function with org as argument."
  (interactive)
  (org-load-modules-maybe)
  (org-require-autoloaded-modules)
  (customize-browse 'org))

;;; News

;;;###autoload
(defun org-browse-news ()
  "Browse the news for the latest major release."
  (interactive)
  (browse-url "https://orgmode.org/Changes.html"))

;;; Conveniently switch to Info nodes

;;;###autoload
(defun org-info (&optional node)
  "Read documentation for Org in the info system.
With optional NODE, go directly to that node."
  (interactive)
  (info (format "(org)%s" (or node ""))))

(declare-function Info-goto-node "info" (nodename &optional fork strict-case))
;;;###autoload
(defun org-info-find-node (&optional nodename)
  "Find Info documentation NODENAME or Org documentation according context.
Started from `gnus-info-find-node'."
  (interactive)
  (require 'info)
  (Info-goto-node
   (or nodename
       (let ((default-org-info-node "(org) Top"))
         (cond
          ((eq 'org-agenda-mode major-mode) "(org) Agenda Views")
          ((eq 'org-mode major-mode)
           (let* ((context (org-element-at-point))
                  (element-info-nodes ; compare to `org-element-all-elements'.
                   `((babel-call . "(org) Evaluating Code Blocks")
                     (center-block . "(org) Paragraphs")
                     (clock . ,default-org-info-node)
                     (comment . "(org) Comment Lines")
                     (comment-block . "(org) Comment Lines")
                     (diary-sexp . ,default-org-info-node)
                     (drawer . "(org) Drawers")
                     (dynamic-block . "(org) Dynamic Blocks")
                     (example-block . "(org) Literal Examples")
                     (export-block . "(org) ASCII/Latin-1/UTF-8 export")
                     (fixed-width . ,default-org-info-node)
                     (footnote-definition . "(org) Creating Footnotes")
                     (headline . "(org) Document Structure")
                     (horizontal-rule . "(org) Built-in Table Editor")
                     (inlinetask . ,default-org-info-node)
                     (item . "(org) Plain Lists")
                     (keyword . "(org) Per-file keywords")
                     (latex-environment . "(org) LaTeX Export")
                     (node-property . "(org) Properties and Columns")
                     (paragraph . "(org) Paragraphs")
                     (plain-list . "(org) Plain Lists")
                     (planning . "(org) Deadlines and Scheduling")
                     (property-drawer . "(org) Properties and Columns")
                     (quote-block . "(org) Paragraphs")
                     (section . ,default-org-info-node)
                     (special-block . ,default-org-info-node)
                     (src-block . "(org) Working with Source Code")
                     (table . "(org) Tables")
                     (table-row . "(org) Tables")
                     (verse-block . "(org) Paragraphs"))))
             (or (cdr (assoc (car context) element-info-nodes))
                 default-org-info-node)))
          (t default-org-info-node))))))

;;;; Describe point

(declare-function org-clock-update-time-maybe "org-clock" ())
(declare-function org-time-string-to-time "org-time" (s))
(declare-function org-table-align "org-table" ())
(declare-function org-make-tdiff-string "org-time" (y d h m))
(defvar org-tr-regexp-both) ; org-regexps.el
;;;###autoload
(defun org-evaluate-time-range (&optional to-buffer)
  "Evaluate a time range by computing the difference between start and end.
Normally the result is just printed in the echo area, but with prefix arg
TO-BUFFER, the result is inserted just after the date stamp into the buffer.
If the time range is actually in a table, the result is inserted into the
next column.
For time difference computation, a year is assumed to be exactly 365
days in order to avoid rounding problems."
  (interactive "P")
  (require 'org-clock)
  (require 'org-time)
  (require 'org-regexps)
  (or
   (org-clock-update-time-maybe)
   (save-excursion
     (unless (org-at-date-range-p t)
       (goto-char (line-beginning-position))
       (re-search-forward org-tr-regexp-both (line-end-position) t))
     (unless (org-at-date-range-p t)
       (user-error "Not at a timestamp range, and none found in current line")))
   (let* ((ts1 (match-string 1))
	  (ts2 (match-string 2))
	  (havetime (or (> (length ts1) 15) (> (length ts2) 15)))
	  (match-end (match-end 0))
	  (time1 (org-time-string-to-time ts1))
	  (time2 (org-time-string-to-time ts2))
	  (diff (abs (float-time (time-subtract time2 time1))))
	  (negative (time-less-p time2 time1))
	  ;; (ys (floor (* 365 24 60 60)))
	  (ds (* 24 60 60))
	  (hs (* 60 60))
	  (fy "%dy %dd %02d:%02d")
	  (fy1 "%dy %dd")
	  (fd "%dd %02d:%02d")
	  (fd1 "%dd")
	  (fh "%02d:%02d")
	  y d h m align)
     (if havetime
	 (setq ; y (floor diff ys)  diff (mod diff ys)
	  y 0
	  d (floor diff ds)  diff (mod diff ds)
	  h (floor diff hs)  diff (mod diff hs)
	  m (floor diff 60))
       (setq ; y (floor diff ys)  diff (mod diff ys)
	y 0
	d (round diff ds)
	h 0 m 0))
     (if (not to-buffer)
	 (message "%s" (org-make-tdiff-string y d h m))
       (if (org-at-table-p)
	   (progn
	     (goto-char match-end)
	     (setq align t)
	     (and (looking-at " *|") (goto-char (match-end 0))))
	 (goto-char match-end))
       (when (looking-at
	      "\\( *-? *[0-9]+y\\)?\\( *[0-9]+d\\)? *[0-9][0-9]:[0-9][0-9]")
	 (replace-match ""))
       (when negative (insert " -"))
       (if (> y 0) (insert " " (format (if havetime fy fy1) y d h m))
	 (if (> d 0) (insert " " (format (if havetime fd fd1) d h m))
	   (insert " " (format fh h m))))
       (when align
         (require 'org-table)
         (org-table-align))
       (message "Time difference inserted")))))

(provide 'org-misc)
;;; org-misc.el ends here
