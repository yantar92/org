;;; org-agenda-export.el --- Exporting agenda views  -*- lexical-binding: t; -*-

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

;; This file implements saving agenda views to plain text, HTML, pdf,
;; Org, etc.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-agenda-common)
(require 'org-agenda-dispatch)

(declare-function org-mobile-write-agenda-for-mobile "org-mobile" (file))

(defgroup org-agenda-export nil
  "Options concerning exporting agenda views in Org mode."
  :tag "Org Agenda Export"
  :group 'org-agenda)

(defcustom org-agenda-exporter-settings nil
  ;; FIXME: Do we really want to evaluate those settings and thus force
  ;; the user to use `quote' all the time?
  "Alist of variable/value pairs that should be active during agenda export.
This is a good place to set options for ps-print and for htmlize.
Note that the way this is implemented, the values will be evaluated
before assigned to the variables.  So make sure to quote values you do
*not* want evaluated, for example

   (setq org-agenda-exporter-settings
         \\='((ps-print-color-p \\='black-white)))"
  :group 'org-agenda-export
  :type '(repeat
	  (list
	   (variable)
	   (sexp :tag "Value"))))

(defcustom org-agenda-before-write-hook '(org-agenda-add-entry-text)
  "Hook run in a temporary buffer before writing the agenda to an export file.
A useful function for this hook is `org-agenda-add-entry-text'."
  :group 'org-agenda-export
  :type 'hook
  :options '(org-agenda-add-entry-text))

(defcustom org-agenda-export-html-style nil
  "The style specification for exported HTML Agenda files.
If this variable contains a string, it will replace the default <style>
section as produced by `htmlize'.
Since there are different ways of setting style information, this variable
needs to contain the full HTML structure to provide a style, including the
surrounding HTML tags.  The style specifications should include definitions
the fonts used by the agenda, here is an example:

   <style type=\"text/css\">
       p { font-weight: normal; color: gray; }
       .org-agenda-structure {
          font-size: 110%;
          color: #003399;
          font-weight: 600;
       }
       .org-todo {
          color: #cc6666;
          font-weight: bold;
       }
       .org-agenda-done {
          color: #339933;
       }
       .org-done {
          color: #339933;
       }
       .title { text-align: center; }
       .todo, .deadline { color: red; }
       .done { color: green; }
    </style>

or, if you want to keep the style in a file,

   <link rel=\"stylesheet\" type=\"text/css\" href=\"mystyles.css\">

As the value of this option simply gets inserted into the HTML <head> header,
you can \"misuse\" it to also add other text to the header."
  :group 'org-agenda-export
  :group 'org-export-html
  :type '(choice
	  (const nil)
	  (string)))

(defun org-agenda-remove-marked-text (property &optional value)
  "Delete all text marked with VALUE of PROPERTY.
VALUE defaults to t."
  (let (beg)
    (setq value (or value t))
    (while (setq beg (text-property-any (point-min) (point-max)
					property value))
      (delete-region
       beg (or (next-single-property-change beg property)
	       (point-max))))))

(defvar org-mobile-creating-agendas) ; defined in org-mobile.el
(defvar org-agenda-write-buffer-name "Agenda View")
(declare-function org-paste-subtree "org-edit-structure" (&optional level tree for-yank remove))
(declare-function org-copy-subtree "org-edit-structure" (&optional n cut force-store-markers nosubtrees))
;;;###autoload
(defun org-agenda-write (file &optional open nosettings _)
  "Write the current buffer (an agenda view) as a file.

Depending on the extension of the file name, plain text (.txt),
HTML (.html or .htm), PDF (.pdf) or Postscript (.ps) is produced.
If the extension is .ics, translate visible agenda into iCalendar
format.  If the extension is .org, collect all subtrees
corresponding to the agenda entries and add them in an .org file.

With prefix argument OPEN, open the new file immediately.  If
NOSETTINGS is given, do not scope the settings of
`org-agenda-exporter-settings' into the export commands.  This is
used when the settings have already been scoped and we do not
wish to overrule other, higher priority settings."
  (interactive "FWrite agenda to file: \nP")
  (if (or (not (file-writable-p file))
	  (and (file-exists-p file)
	       (if (called-interactively-p 'any)
		   (not (y-or-n-p (format "Overwrite existing file %s? " file))))))
      (user-error "Cannot write agenda to file %s" file))
  (cl-progv
      (if nosettings nil (mapcar #'car org-agenda-exporter-settings))
      (if nosettings nil (mapcar (lambda (binding) (eval (cadr binding) t))
                               org-agenda-exporter-settings))
    (save-excursion
      (save-window-excursion
	(let ((bs (copy-sequence (buffer-string)))
	      (extension (file-name-extension file))
	      (default-directory (file-name-directory file))
	      ) ;; beg content
	  (with-temp-buffer
	    (rename-buffer org-agenda-write-buffer-name t)
	    (set-buffer-modified-p nil)
	    (insert bs)
	    (org-agenda-remove-marked-text 'invisible 'org-filtered)
	    (run-hooks 'org-agenda-before-write-hook)
	    (cond
	     ((bound-and-true-p org-mobile-creating-agendas)
	      (org-mobile-write-agenda-for-mobile file))
	     ((string= "org" extension)
	      (let (content p m message-log-max)
		(goto-char (point-min))
                (require 'org-edit-structure)
                (defvar org-subtree-clip)
		(while (setq p (next-single-property-change (point) 'org-hd-marker nil))
		  (goto-char p)
		  (setq m (get-text-property (point) 'org-hd-marker))
		  (when m
		    (cl-pushnew (with-current-buffer (marker-buffer m)
			          (goto-char m)
			          (org-copy-subtree 1 nil t t)
			          org-subtree-clip)
			        content
                                :test #'equal)))
		(find-file file)
		(erase-buffer)
		(dolist (s content) (org-paste-subtree 1 s))
		(write-file file)
		(kill-buffer (current-buffer))
		(message "Org file written to %s" file)))
	     ((member extension '("html" "htm"))
              (org-require-package 'htmlize)
	      (declare-function htmlize-buffer "htmlize" (&optional buffer))
	      (set-buffer (htmlize-buffer (current-buffer)))
	      (when org-agenda-export-html-style
		;; replace <style> section with org-agenda-export-html-style
		(goto-char (point-min))
		(kill-region (- (search-forward "<style") 6)
			     (search-forward "</style>"))
		(insert org-agenda-export-html-style))
	      (write-file file)
	      (kill-buffer (current-buffer))
	      (message "HTML written to %s" file))
	     ((string= "ps" extension)
	      (require 'ps-print)
	      (ps-print-buffer-with-faces file)
	      (message "Postscript written to %s" file))
	     ((string= "pdf" extension)
	      (require 'ps-print)
	      (ps-print-buffer-with-faces
	       (concat (file-name-sans-extension file) ".ps"))
	      (call-process "ps2pdf" nil nil nil
			    (expand-file-name
			     (concat (file-name-sans-extension file) ".ps"))
			    (expand-file-name file))
	      (delete-file (concat (file-name-sans-extension file) ".ps"))
	      (message "PDF written to %s" file))
	     ((string= "ics" extension)
	      (require 'ox-icalendar)
	      (declare-function org-icalendar-export-current-agenda
	                        "ox-icalendar" (file))
	      (org-icalendar-export-current-agenda (expand-file-name file)))
	     (t
              (write-region nil nil file)
              (message "Plain text written to %s" file))))))))
  (when open (org-open-file file)))

(defun org-agenda--split-plist (plist)
  ;; We could/should arguably use `map-keys' and `map-values'.
  (let (keys vals)
    (while plist
      (push (pop plist) keys)
      (push (pop plist) vals))
    (cons (nreverse keys) (nreverse vals))))

;;;###autoload
(defmacro org-batch-agenda (cmd-key &rest parameters)
  "Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string it is used as a tags/todo match string.
Parameters are alternating variable names and values that will be bound
before running the agenda command."
  (pcase-let ((`(,vars . ,exps) (org-agenda--split-plist parameters)))
    `(org--batch-agenda ,cmd-key ',vars (list ,@exps))))

(defun org--batch-agenda (cmd-key vars vals)
  ;; `org-batch-agenda' is a macro because every other "parameter" is
  ;; a variable name rather than an expression to evaluate.  Yuck!
  (cl-progv vars vals
    (let (org-agenda-sticky)
      (if (> (length cmd-key) 1)
	  (org-tags-view nil cmd-key)
	(org-agenda nil cmd-key))))
  (set-buffer org-agenda-buffer-name)
  (princ (buffer-string)))

(defvar org-agenda-info nil)

;;;###autoload
(defmacro org-batch-agenda-csv (cmd-key &rest parameters)
  "Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string it is used as a tags/todo match string.
Parameters are alternating variable names and values that will be bound
before running the agenda command.

The output gives a line for each selected agenda item.  Each
item is a list of comma-separated values, like this:

category,head,type,todo,tags,date,time,extra,priority-l,priority-n

category     The category of the item
head         The headline, without TODO kwd, TAGS and PRIORITY
type         The type of the agenda entry, can be
                todo               selected in TODO match
                tagsmatch          selected in tags match
                diary              imported from diary
                deadline           a deadline on given date
                scheduled          scheduled on given date
                timestamp          entry has timestamp on given date
                closed             entry was closed on given date
                upcoming-deadline  warning about deadline
                past-scheduled     forwarded scheduled item
                block              entry has date block including g. date
todo         The todo keyword, if any
tags         All tags including inherited ones, separated by colons
date         The relevant date, like 2007-2-14
time         The time, like 15:00-16:50
extra        String with extra planning info
priority-l   The priority letter if any was given
priority-n   The computed numerical priority
agenda-day   The day in the agenda where this is listed"
  (pcase-let ((`(,vars . ,exps) (org-agenda--split-plist parameters)))
    `(org--batch-agenda-csv ,cmd-key ',vars (list ,@exps))))

(defun org--batch-agenda-csv (cmd-key vars vals)
  ;; `org-batch-agenda-csv' is a macro because every other "parameter" is
  ;; a variable name rather than an expression to evaluate.  Yuck!
  (let ((org-agenda-remove-tags t))
    (cl-progv vars vals
      ;; FIXME: Shouldn't this be 1 (see commit 10173ad6d610b)?
      (if (> (length cmd-key) 2)
	  (org-tags-view nil cmd-key)
	(org-agenda nil cmd-key))))
  (set-buffer org-agenda-buffer-name)
  (let ((lines (org-split-string (buffer-string) "\n")))
    (dolist (line lines)
      (when (get-text-property 0 'org-category line)
	(setq org-agenda-info
	      (org-fix-agenda-info (text-properties-at 0 line)))
	(princ
	 (mapconcat #'org-agenda-export-csv-mapper
		    '(org-category txt type todo tags date time extra
		                   priority-letter priority agenda-day)
		    ","))
	(princ "\n")))))

(defun org-fix-agenda-info (props)
  "Make sure all properties on an agenda item have a canonical form.
This ensures the export commands can easily use it."
  (let (tmp re)
    (when (setq tmp (plist-get props 'tags))
      (setq props (plist-put props 'tags (mapconcat #'identity tmp ":"))))
    (when (setq tmp (plist-get props 'date))
      (when (integerp tmp) (setq tmp (calendar-gregorian-from-absolute tmp)))
      (let ((calendar-date-display-form
             '((format "%s-%.2d-%.2d" year
                       (string-to-number month)
                       (string-to-number day)))))
	(setq tmp (calendar-date-string tmp)))
      (setq props (plist-put props 'date tmp)))
    (when (setq tmp (plist-get props 'day))
      (when (integerp tmp) (setq tmp (calendar-gregorian-from-absolute tmp)))
      (let ((calendar-date-display-form
             '((format "%s-%.2d-%.2d" year
                       (string-to-number month)
                       (string-to-number day)))))
	(setq tmp (calendar-date-string tmp)))
      (setq props (plist-put props 'day tmp))
      (setq props (plist-put props 'agenda-day tmp)))
    (when (setq tmp (plist-get props 'txt))
      (when (string-match "\\[#\\([A-Z0-9]\\)\\] ?" tmp)
	(plist-put props 'priority-letter (match-string 1 tmp))
	(setq tmp (replace-match "" t t tmp)))
      (when (and (setq re (plist-get props 'org-todo-regexp))
		 (setq re (concat "\\`\\.*" re " ?"))
		 (let ((case-fold-search nil)) (string-match re tmp)))
	(plist-put props 'todo (match-string 1 tmp))
	(setq tmp (replace-match "" t t tmp)))
      (plist-put props 'txt tmp)))
  props)

(defun org-agenda-export-csv-mapper (prop)
  (let ((res (plist-get org-agenda-info prop)))
    (setq res
	  (cond
	   ((not res) "")
	   ((stringp res) res)
	   (t (prin1-to-string res))))
    (org-trim (replace-regexp-in-string "," ";" res nil t))))

;;;###autoload
(defun org-store-agenda-views (&rest _parameters)
  "Store agenda views."
  (interactive)
  (org--batch-store-agenda-views nil nil))

;;;###autoload
(defmacro org-batch-store-agenda-views (&rest parameters)
  "Run all custom agenda commands that have a file argument."
  (pcase-let ((`(,vars . ,exps) (org-agenda--split-plist parameters)))
    `(org--batch-store-agenda-views ',vars (list ,@exps))))

(defun org--batch-store-agenda-views (vars vals)
  (let ((cmds (org-agenda-normalize-custom-commands org-agenda-custom-commands))
        (pop-up-frames nil)
        (dir default-directory)
        cmd thiscmdkey thiscmdcmd match files opts cmd-or-set
        seriesp bufname)
    (save-window-excursion
      (while cmds
	(setq cmd (pop cmds)
	      thiscmdkey (car cmd)
	      thiscmdcmd (cdr cmd)
	      match (nth 2 thiscmdcmd)
	      bufname (if org-agenda-sticky
			  (or (and (stringp match)
				   (format "*Org Agenda(%s:%s)*" thiscmdkey match))
			      (format "*Org Agenda(%s)*" thiscmdkey))
			org-agenda-buffer-name)
              ;; series:     (0:key 1:desc 2:(cmd1 cmd2 ...) 3:general-settings 4:files)
              ;; non-series: (0:key 1:desc 2:type 3:match    4:settings         5:files)
	      cmd-or-set (nth 2 cmd)
	      seriesp (not (or (symbolp cmd-or-set) (functionp cmd-or-set)))
	      opts (nth (if seriesp 3 4) cmd)
	      files (nth (if seriesp 4 5) cmd))
	(if (stringp files) (setq files (list files)))
	(when files
	  (let* ((opts (append org-agenda-exporter-settings opts))
	         (vars (append (mapcar #'car opts) vars))
	         (vals (append (mapcar (lambda (binding) (eval (cadr binding) t))
	                               opts)
	                       vals)))
	    (cl-progv vars vals
	      (org-agenda nil thiscmdkey))
	    (set-buffer bufname)
	    (while files
	      (cl-progv vars vals
	        (org-agenda-write (expand-file-name (pop files) dir) nil t))))
	  (and (get-buffer bufname)
	       (kill-buffer bufname)))))))

(defun org-agenda-normalize-custom-commands (cmds)
  "Normalize custom commands CMDS."
  (delq nil
	(mapcar
	 (lambda (x)
	   (cond ((stringp (cdr x)) nil)
		 ((stringp (nth 1 x)) x)
		 ((not (nth 1 x)) (cons (car x) (cons "" (cddr x))))
		 (t (cons (car x) (cons "" (cdr x))))))
	 cmds)))

(provide 'org-agenda-export)

;;; org-agenda-export.el ends here
