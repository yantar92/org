;;; org-agenda-files.el --- Managing Org files and buffers    -*- lexical-binding: t; -*-

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

;; This library provides tools to manage files and buffers what are
;; tracked by Org mode.
;; "Tracked" includes standard Org file locations, files where Org
;; mode searches information for the purposes of following links,
;; searching text, building agendas, etc.

;;; Code:

(require 'org-macs)
(require 'org-mode)

(defvar org-window-configuration)

;;; Customizing files managed by Org mode.

(defcustom org-directory "~/org"
  "Directory with Org files.
This is just a default location to look for Org files.  There is no need
at all to put your files into this directory.  It is used in the
following situations:

1. When a capture template specifies a target file that is not an
   absolute path.  The path will then be interpreted relative to
   `org-directory'
2. When the value of variable `org-agenda-files' is a single file, any
   relative paths in this file will be taken as relative to
   `org-directory'."
  :group 'org-refile
  :group 'org-capture
  :type 'directory)

(defcustom org-default-notes-file (convert-standard-filename "~/.notes")
  "Default target for storing notes.
Used as a fall back file for org-capture.el, for templates that
do not specify a target file."
  :group 'org-refile
  :group 'org-capture
  :type 'file)

(defcustom org-agenda-files nil
  "The files to be used for agenda display.

If an entry is a directory, all files in that directory that are matched
by `org-agenda-file-regexp' will be part of the file list.

If the value of the variable is not a list but a single file name, then
the list of agenda files is actually stored and maintained in that file,
one agenda file per line.  In this file paths can be given relative to
`org-directory'.  Tilde expansion and environment variable substitution
are also made.

Entries may be added to this list with `\\[org-agenda-file-to-front]'
and removed with `\\[org-remove-file]'."
  :group 'org-agenda
  :type '(choice
	  (repeat :tag "List of files and directories" file)
	  (file :tag "Store list in a file\n" :value "~/.agenda_files")))

(defcustom org-agenda-file-regexp "\\`[^.].*\\.org\\'"
  "Regular expression to match files for `org-agenda-files'.
If any element in the list in that variable contains a directory instead
of a normal file, all files in that directory that are matched by this
regular expression will be included."
  :group 'org-agenda
  :type 'regexp)

(defcustom org-agenda-skip-unavailable-files nil
  "Non-nil means to just skip non-reachable files in `org-agenda-files'.
A nil value means to remove them, after a query, from the list."
  :group 'org-agenda
  :type 'boolean)

(defvaralias 'org-agenda-multi-occur-extra-files
  'org-agenda-text-search-extra-files)
(defcustom org-agenda-text-search-extra-files nil
  "List of extra files to be searched by text search commands.
These files will be searched in addition to the agenda files by the
commands `org-search-view' (`\\[org-agenda] s') \
and `org-occur-in-agenda-files'.
Note that these files will only be searched for text search commands,
not for the other agenda views like todo lists, tag searches or the weekly
agenda.  This variable is intended to list notes and possibly archive files
that should also be searched by these two commands.
In fact, if the first element in the list is the symbol `agenda-archives',
then all archive files of all agenda files will be added to the search
scope."
  :group 'org-agenda
  :type '(set :greedy t
	      (const :tag "Agenda Archives" agenda-archives)
	      (repeat :inline t (file))))

;;; Listing and querying Org agenda files

(defun org-read-agenda-file-list (&optional pair-with-expansion)
  "Read the list of agenda files from a file.
If PAIR-WITH-EXPANSION is t return pairs with un-expanded
filenames, used by `org-store-new-agenda-file-list' to write back
un-expanded file names."
  (when (file-directory-p org-agenda-files)
    (error "`org-agenda-files' cannot be a single directory"))
  (when (stringp org-agenda-files)
    (with-temp-buffer
      (insert-file-contents org-agenda-files)
      (mapcar
       (lambda (f)
	 (let ((e (expand-file-name (substitute-in-file-name f)
				    org-directory)))
	   (if pair-with-expansion
	       (cons e f)
	     e)))
       (org-split-string (buffer-string) "[ \t\r\n]*?[\r\n][ \t\r\n]*")))))

(defvar org-agenda-archives-mode)
(declare-function org-add-archive-files "org-archive")
(defun org-agenda-files (&optional unrestricted archives)
  "Get the list of agenda files.
Optional UNRESTRICTED means return the full list even if a restriction
is currently in place.
When ARCHIVES is t, include all archive files that are really being
used by the agenda files.  If ARCHIVE is `ifmode', do this only if
`org-agenda-archives-mode' is t."
  (let ((files
	 (cond
	  ((and (not unrestricted) (get 'org-agenda-files 'org-restrict)))
	  ((stringp org-agenda-files) (org-read-agenda-file-list))
	  ((listp org-agenda-files) org-agenda-files)
	  (t (error "Invalid value of `org-agenda-files'")))))
    (setq files (apply 'append
		       (mapcar (lambda (f)
				 (if (file-directory-p f)
				     (directory-files
				      f t org-agenda-file-regexp)
				   (list (expand-file-name f org-directory))))
			       files)))
    (when org-agenda-skip-unavailable-files
      (setq files (delq nil
			(mapcar (lambda (file)
				  (and (file-readable-p file) file))
				files))))
    (when (or (eq archives t)
	      (and (eq archives 'ifmode)
                   (derived-mode-p 'org-agenda-mode)
                   (eq org-agenda-archives-mode t)))
      (require 'org-archive)
      (setq files (org-add-archive-files files)))
    (delete-dups files)))

(defun org-agenda-file-p (&optional file)
  "Return non-nil, if FILE is an agenda file.
If FILE is omitted, use the file associated with the current
buffer."
  (let ((fname (or file (buffer-file-name))))
    (and fname
         (member (file-truename fname)
                 (mapcar #'file-truename (org-agenda-files t))))))

(defun org-files-list ()
  "Return `org-agenda-files' list, plus all open Org files.
This is useful for operations that need to scan all of a user's
open and agenda-wise Org files."
  (let ((files (mapcar #'expand-file-name (org-agenda-files))))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(when (and (derived-mode-p 'org-mode) (buffer-file-name))
	  (cl-pushnew (expand-file-name (buffer-file-name)) files
		      :test #'equal))))
    files))

(defvar remember-data-file)
(defun org-get-org-file ()
  "Read a filename, with default directory `org-directory'."
  (require 'remember)
  (let ((default (or org-default-notes-file remember-data-file)))
    (read-file-name (format "File name [%s]: " default)
		    (file-name-as-directory org-directory)
		    default)))

;;; Opening Org agenda files, if they are not yet open in Emacs

(defcustom org-agenda-inhibit-startup nil
  "Inhibit startup when preparing agenda buffers.
When this variable is t, the initialization of the Org agenda
buffers is inhibited: e.g. the visibility state is not set, the
tables are not re-aligned, etc."
  :type 'boolean
  :version "24.3"
  :group 'org-agenda)

(defcustom org-agenda-ignore-properties nil
  "Avoid updating text properties when building the agenda.
Properties are used to prepare buffers for effort estimates,
appointments, statistics and subtree-local categories.
If you don't use these in the agenda, you can add them to this
list and agenda building will be a bit faster.
The value is a list, with symbol `stats'."
  :type '(set :greedy t
	      (const stats))
  :package-version '(Org . "9.7")
  :group 'org-agenda)

(defun org-check-agenda-file (file)
  "Make sure FILE exists.  If not, ask user what to do."
  (unless (file-exists-p file)
    (message "Non-existent agenda file %s.  [R]emove from list or [A]bort?"
	     (abbreviate-file-name file))
    (let ((r (downcase (read-char-exclusive))))
      (cond
       ((equal r ?r)
	(org-remove-file file)
	(throw 'nextfile t))
       (t (user-error "Abort"))))))

(defvar org-agenda-new-buffers nil
  "Buffers created to visit agenda files.")

(defun org-get-agenda-file-buffer (file)
  "Get an agenda buffer visiting FILE.
If the buffer needs to be created, add it to the list of buffers
which might be released later."
  (let ((buf (org-find-base-buffer-visiting file)))
    (if buf
	buf ; just return it
      ;; Make a new buffer and remember it
      (setq buf (find-file-noselect file))
      (when buf (push buf org-agenda-new-buffers))
      buf)))

(defun org-release-buffers (blist)
  "Release all buffers in list, asking the user for confirmation when needed.
When a buffer is unmodified, it is just killed.  When modified, it is saved
\(if the user agrees) and then killed."
  (let (file)
    (dolist (buf blist)
      (setq file (buffer-file-name buf))
      (when (and (buffer-modified-p buf)
		 file
		 (y-or-n-p (format "Save file %s? " file)))
	(with-current-buffer buf (save-buffer)))
      (kill-buffer buf))))

(defun org-refresh-stats-properties ()
  "Refresh stats text properties in the buffer."
  (with-silent-modifications
    (org-with-point-at 1
      (let ((regexp (concat org-outline-regexp-bol
			    ".*\\[\\([0-9]*\\)\\(?:%\\|/\\([0-9]*\\)\\)\\]")))
	(while (re-search-forward regexp nil t)
	  (let* ((numerator (string-to-number (match-string 1)))
		 (denominator (and (match-end 2)
				   (string-to-number (match-string 2))))
		 (stats (cond ((not denominator) numerator) ;percent
			      ((= denominator 0) 0)
			      (t (/ (* numerator 100) denominator)))))
	    (put-text-property (point) (progn (org-end-of-subtree t t) (point))
			       'org-stats stats)))))))

(defvar org-todo-keywords-for-agenda nil
  "Combined `org-todo-keywords' from all agenda files.
The agenda files are the files processed by
`org-agenda-prepare-buffers'.")
(defvar org-done-keywords-for-agenda nil
  "Combined `org-done-keywords' from all agenda files.
The agenda files are the files processed by
`org-agenda-prepare-buffers'.")
(defvar org-todo-keyword-alist-for-agenda nil
  "Combined `org-todo-keyword-alist' from all agenda files.
The agenda files are the files processed by
`org-agenda-prepare-buffers'.")
(defvar org-tag-alist-for-agenda nil
  "Alist of all tags from all agenda files.
The agenda files are the files processed by
`org-agenda-prepare-buffers'.")
(defvar org-tag-groups-alist-for-agenda nil
  "Alist of all groups tags from all current agenda files.
The agenda files are the files processed by
`org-agenda-prepare-buffers'.")

(defun org-agenda-prepare-buffers (files)
  "Create buffers for all agenda files, protect archived trees and comments.

1. Open the files in Emacs if they are not open yet;
2. Query user if any file in FILES does not exist;
3. Perform minimal Org mode activation according to
   `org-agenda-inhibit-startup;
4. Run `org-refresh-stats-properties' in each file;
5. Combine todo keyword and tag settings in FILES into
   `org-todo-keywords-for-agenda', `org-done-keywords-for-agenda',
   `org-todo-keyword-alist-for-agenda', `org-tag-alist-for-agenda',
   and `org-tag-groups-alist-for-agenda';
6. Refresh agenda list in the menu.
7. Collect all the agenda files that were opened as a result into
   `org-agenda-new-buffers'"
  (interactive)
  (let ((inhibit-read-only t)
	(org-inhibit-startup org-agenda-inhibit-startup)
        ;; Do not refresh list of agenda files in the menu when
        ;; opening every new file.
        (org-agenda-file-menu-enabled nil))
    (setq org-tag-alist-for-agenda nil
	  org-tag-groups-alist-for-agenda nil)
    (dolist (file files)
      (catch 'nextfile
        (with-current-buffer
            (if (bufferp file)
                file
              (org-check-agenda-file file)
              (org-get-agenda-file-buffer file))
          (org-with-wide-buffer
	   (org-set-regexps-and-options 'tags-only)
	   (or (memq 'stats org-agenda-ignore-properties)
	       (org-refresh-stats-properties))
           (dolist (el org-todo-keywords-1)
             (unless (member el org-todo-keywords-for-agenda)
               (push el org-todo-keywords-for-agenda)))
           (dolist (el org-done-keywords)
             (unless (member el org-done-keywords-for-agenda)
               (push el org-done-keywords-for-agenda)))
	   (setq org-todo-keyword-alist-for-agenda
                 (org--tag-add-to-alist
		  org-todo-key-alist
                  org-todo-keyword-alist-for-agenda))
	   (setq org-tag-alist-for-agenda
		 (org--tag-add-to-alist
		  org-current-tag-alist
                  org-tag-alist-for-agenda))
	   ;; Merge current file's tag groups into global
	   ;; `org-tag-groups-alist-for-agenda'.
	   (when org-group-tags
	     (dolist (alist org-tag-groups-alist)
	       (let ((old (assoc (car alist) org-tag-groups-alist-for-agenda)))
		 (if old
		     (setcdr old (org-uniquify (append (cdr old) (cdr alist))))
		   (push alist org-tag-groups-alist-for-agenda)))))))))
    ;; Refresh the menu once after loading all the agenda buffers.
    (when org-agenda-file-menu-enabled
      (org-install-agenda-files-menu))))

;;; User commands to manage Org agenda files

(defun org-store-new-agenda-file-list (list)
  "Set new value for the agenda file list and save it correctly."
  (if (stringp org-agenda-files)
      (let ((fe (org-read-agenda-file-list t)) b u)
	(while (setq b (find-buffer-visiting org-agenda-files))
	  (kill-buffer b))
	(with-temp-file org-agenda-files
	  (insert
	   (mapconcat
	    (lambda (f) ;; Keep un-expanded entries.
	      (if (setq u (assoc f fe))
		  (cdr u)
		f))
	    list "\n")
	   "\n")))
    (let ((org-mode-hook nil) (org-inhibit-startup t)
	  (org-insert-mode-line-in-empty-file nil))
      (setq org-agenda-files list)
      (customize-save-variable 'org-agenda-files org-agenda-files))))

(defun org-edit-agenda-file-list ()
  "Edit the list of agenda files.
Depending on setup, this either uses customize to edit the variable
`org-agenda-files', or it visits the file that is holding the list.  In the
latter case, the buffer is set up in a way that saving it automatically kills
the buffer and restores the previous window configuration."
  (interactive)
  (if (stringp org-agenda-files)
      (let ((cw (current-window-configuration)))
	(find-file org-agenda-files)
	(setq-local org-window-configuration cw)
	(add-hook 'after-save-hook
		  (lambda ()
		    (set-window-configuration
		     (prog1 org-window-configuration
		       (kill-buffer (current-buffer))))
		    (org-install-agenda-files-menu)
		    (message "New agenda file list installed"))
		  nil 'local)
	(message "%s" (substitute-command-keys
		       "Edit list and finish with \\[save-buffer]")))
    (customize-variable 'org-agenda-files)))

(defun org-agenda-file-to-front (&optional to-end)
  "Move/add the current file to the top of the agenda file list.
If the file is not present in the list, it is added to the front.  If it is
present, it is moved there.  With optional argument TO-END, add/move to the
end of the list."
  (interactive "P")
  (let ((org-agenda-skip-unavailable-files nil)
	(file-alist (mapcar (lambda (x)
			      (cons (file-truename x) x))
			    (org-agenda-files t)))
	(ctf (file-truename
	      (or buffer-file-name
		  (user-error "Please save the current buffer to a file"))))
	x had)
    (setq x (assoc ctf file-alist) had x)

    (unless x (setq x (cons ctf (abbreviate-file-name buffer-file-name))))
    (if to-end
	(setq file-alist (append (delq x file-alist) (list x)))
      (setq file-alist (cons x (delq x file-alist))))
    (org-store-new-agenda-file-list (mapcar 'cdr file-alist))
    (org-install-agenda-files-menu)
    (message "File %s to %s of agenda file list"
	     (if had "moved" "added") (if to-end "end" "front"))))

(defun org-remove-file (&optional file)
  "Remove current file from the list of files in variable `org-agenda-files'.
These are the files which are being checked for agenda entries.
Optional argument FILE means use this file instead of the current."
  (interactive)
  (let* ((org-agenda-skip-unavailable-files nil)
	 (file (or file buffer-file-name
		   (user-error "Current buffer does not visit a file")))
	 (true-file (file-truename file))
	 (afile (abbreviate-file-name file))
	 (files (delq nil (mapcar
			 (lambda (x)
			   (unless (equal true-file
					  (file-truename x))
			     x))
			 (org-agenda-files t)))))
    (if (not (= (length files) (length (org-agenda-files t))))
	(progn
	  (org-store-new-agenda-file-list files)
	  (org-install-agenda-files-menu)
	  (message "Removed from Org Agenda list: %s" afile))
      (message "File was not in list: %s (not removed)" afile))))

;;; User commands to act on Org agenda files

(declare-function org-id-locations-save "org-id")
(defun org-save-all-org-buffers ()
  "Save all Org buffers without user confirmation."
  (interactive)
  (message "Saving all Org buffers...")
  (save-some-buffers t (lambda () (and (derived-mode-p 'org-mode) t)))
  (when (featurep 'org-id) (org-id-locations-save))
  (message "Saving all Org buffers... done"))

(defvar org-id-track-globally)
(declare-function org-id-locations-load "org-id")
(defun org-revert-all-org-buffers ()
  "Revert all Org buffers.
Prompt for confirmation when there are unsaved changes.
Be sure you know what you are doing before letting this function
overwrite your changes.

This function is useful in a setup where one tracks Org files
with a version control system, to revert on one machine after pulling
changes from another.  I believe the procedure must be like this:

1. \\[org-save-all-org-buffers]
2. Pull changes from the other machine, resolve conflicts
3. \\[org-revert-all-org-buffers]"
  (interactive)
  (unless (yes-or-no-p "Revert all Org buffers from their files? ")
    (user-error "Abort"))
  (save-excursion
    (save-window-excursion
      (dolist (b (buffer-list))
	(when (and (with-current-buffer b (derived-mode-p 'org-mode))
		   (with-current-buffer b buffer-file-name))
	  (pop-to-buffer-same-window b)
	  (revert-buffer t 'no-confirm)))
      (when (and (featurep 'org-id) org-id-track-globally)
	(org-id-locations-load)))))

;;;###autoload
(defun org-switchb (&optional arg)
  "Switch between Org buffers.

With `\\[universal-argument]' prefix, restrict available buffers to files.

With `\\[universal-argument] \\[universal-argument]' \
prefix, restrict available buffers to agenda files."
  (interactive "P")
  (let ((blist (org-buffer-list
		(cond ((equal arg '(4))  'files)
		      ((equal arg '(16)) 'agenda)))))
    (pop-to-buffer-same-window
     (completing-read "Org buffer: "
		      (mapcar #'list (mapcar #'buffer-name blist))
		      nil t))))

;;;###autoload
(defun org-cycle-agenda-files ()
  "Cycle through the files in `org-agenda-files'.
If the current buffer visits an agenda file, find the next one in the list.
If the current buffer does not, find the first agenda file."
  (interactive)
  (let* ((fs (or (org-agenda-files t)
		 (user-error "No agenda files")))
	 (files (copy-sequence fs))
	 (tcf (and buffer-file-name (file-truename buffer-file-name)))
	 file)
    (when tcf
      (while (and (setq file (pop files))
		  (not (equal (file-truename file) tcf)))))
    (find-file (car (or files fs)))
    (when (buffer-base-buffer) (pop-to-buffer-same-window (buffer-base-buffer)))))

;; FIXME: This is slightly out of scope - helper function to create
;; menu `find-file' entry.
(defun org-file-menu-entry (file)
  (vector file (list 'find-file file) t))

(provide 'org-agenda-files)

;;; org-agenda-files.el ends here
