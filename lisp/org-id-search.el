;;; org-id-search.el --- Looking up Org IDs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2008-2024 Free Software Foundation, Inc.
;;
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

;; This file implements global lookup for Org IDs.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element)
(require 'org-agenda-files)
(require 'org-property-search)

(defcustom org-id-track-globally t
  "Non-nil means track IDs through files, so that links work globally.
This work by maintaining a hash table for IDs and writing this table
to disk when exiting Emacs.  Because of this, it works best if you use
a single Emacs process, not many.

When nil, IDs are not tracked.  Links to IDs will still work within
a buffer, but not if the entry is located in another file.
IDs can still be used if the entry with the id is in the same file as
the link."
  :group 'org-id
  :type 'boolean)

(defcustom org-id-locations-file (locate-user-emacs-file ".org-id-locations")
  "The file for remembering in which file an ID was defined.
This variable is only relevant when `org-id-track-globally' is set."
  :group 'org-id
  :type 'file)

(defcustom org-id-locations-file-relative nil
  "Determine if `org-id-locations' should be stored as relative links.
Non-nil means that links to locations are stored as links
relative to the location of where `org-id-locations-file' is
stored.

Nil means to store absolute paths to files.

This customization is useful when folders are shared across
systems but mounted at different roots.  Relative path to
`org-id-locations-file' still has to be maintained across
systems."
  :group 'org-id
  :type 'boolean
  :package-version '(Org . "9.3"))

(defvar org-id-locations nil
  "List of files with IDs in those files.")
(defvar org-id--locations-checksum nil
  "Last checksum corresponding to ID files and their modifications.")

(defvar org-id-files nil
  "List of files that contain IDs.")

(defcustom org-id-extra-files 'org-agenda-text-search-extra-files
  "Files to be searched for IDs, besides the agenda files.
When Org reparses files to remake the list of files and IDs it is tracking,
it will normally scan the agenda files, the archives related to agenda files,
any files that are listed as ID containing in the current register, and
any Org file currently visited by Emacs.
You can list additional files here.
This variable is only relevant when `org-id-track-globally' is set."
  :group 'org-id
  :type
  '(choice
    (symbol :tag "Variable")
    (repeat :tag "List of files"
	    (file))))

(defcustom org-id-search-archives t
  "Non-nil means search also the archive files of agenda files for entries.
This is a possibility to reduce overhead, but it means that entries moved
to the archives can no longer be found by ID.
This variable is only relevant when `org-id-track-globally' is set."
  :group 'org-id
  :type 'boolean)

;;;###autoload
(defun org-id-find (id &optional markerp)
  "Return the location of the entry with the id ID.
The return value is a cons cell (file-name . position), or nil
if there is no entry with that ID.
With optional argument MARKERP, return the position as a new marker."
  (cond
   ((symbolp id) (setq id (symbol-name id)))
   ((numberp id) (setq id (number-to-string id))))
  (let ((file (org-id-find-id-file id))
	where)
    (when file
      (setq where (org-id-find-id-in-file id file markerp)))
    (unless where
      (org-id-update-id-locations nil t)
      (setq file (org-id-find-id-file id))
      (when file
	(setq where (org-id-find-id-in-file id file markerp))))
    where))

;; Storing ID locations (files)

;;;###autoload
(defun org-id-update-id-locations (&optional files silent)
  "Scan relevant files for IDs.
Store the relation between files and corresponding IDs.
This will scan all agenda files, all associated archives, all open Org
files, and all files currently mentioned in `org-id-locations'.
When FILES is given, scan also these files.
If SILENT is non-nil, messages are suppressed."
  (interactive)
  (unless org-id-track-globally
    (error "Please turn on `org-id-track-globally' if you want to track IDs"))
  (let* ((files
          (delete-dups
           (mapcar #'file-truename
                   (cl-remove-if-not
		    ;; Default `org-id-extra-files' value contains
		    ;; `agenda-archives' symbol.
		    #'stringp
		    (append
		     ;; Agenda files and all associated archives.
		     (org-agenda-files t org-id-search-archives)
		     ;; Explicit extra files.
		     (if (symbolp org-id-extra-files)
			 (symbol-value org-id-extra-files)
		       org-id-extra-files)
		     ;; All files known to have IDs.
		     org-id-files
                     ;; All Org files open in Emacs.
                     (mapcar #'buffer-file-name (org-buffer-list 'files t))
		     ;; Additional files from function call.
		     files)))))
         (nfiles (length files))
         (id-regexp
	  (rx (seq bol (0+ (any "\t ")) ":ID:" (1+ " ") (not (any " ")))))
         (seen-ids (make-hash-table :test #'equal))
         (ndup 0)
         (i 0)
         (checksum
          (mapcar
           (lambda (f)
             (when (file-exists-p f)
               (list f (file-attribute-modification-time (file-attributes f)))))
           (sort (copy-sequence files) #'string<))))
    (unless (equal checksum org-id--locations-checksum) ; Files have changed since the last update.
      (setq org-id-locations nil)
      (with-temp-buffer
        (delay-mode-hooks
	  (org-mode)
	  (dolist (file files)
	    (when (file-exists-p file)
              (unless silent
                (cl-incf i)
                (message "Finding ID locations (%d/%d files): %s" i nfiles file))
	      (insert-file-contents file nil nil nil 'replace)
              (let ((ids nil)
                    node
		    (case-fold-search t))
                (org-with-point-at 1
		  (while (re-search-forward id-regexp nil t)
                    (setq node (org-element-at-point))
		    (when (org-element-type-p node 'node-property)
                      (push (org-element-property :value node) ids)))
		  (when ids
		    (push (cons (abbreviate-file-name file) ids)
			  org-id-locations)
		    (dolist (id ids)
                      (cond
                       ((not (gethash id seen-ids)) (puthash id t seen-ids))
                       (silent nil)
                       (t
                        (message "Duplicate ID %S" id)
                        (cl-incf ndup)))))))))))
      (setq org-id-files (mapcar #'car org-id-locations))
      (org-id-locations-save)
      ;; Now convert to a hash table.
      (setq org-id-locations (org-id-alist-to-hash org-id-locations))
      (setq org-id--locations-checksum checksum)
      (when (and (not silent) (> ndup 0))
        (warn "WARNING: %d duplicate IDs found, check *Messages* buffer" ndup))
      (message "%d files scanned, %d files contains IDs, and %d IDs found."
               nfiles (length org-id-files) (hash-table-count org-id-locations)))
    org-id-locations))

(defun org-id-locations-save ()
  "Save `org-id-locations' in `org-id-locations-file'."
  (when (and org-id-track-globally org-id-locations)
    (let ((out (if (hash-table-p org-id-locations)
		   (org-id-hash-to-alist org-id-locations)
		 org-id-locations)))
      (when (and org-id-locations-file-relative out)
	(setq out (mapcar
                   (lambda (item)
		     (if (file-name-absolute-p (car item))
		         (cons (file-relative-name
                                (car item) (file-name-directory
					    org-id-locations-file))
                               (cdr item))
                       item))
	           out)))
      (with-temp-file org-id-locations-file
	(let ((print-level nil)
	      (print-length nil))
	  (print out (current-buffer)))))))

(defun org-id-locations-load ()
  "Read the data from `org-id-locations-file'."
  (setq org-id-locations nil)
  (when org-id-track-globally
    (with-temp-buffer
      (condition-case nil
	  (progn
	    (insert-file-contents org-id-locations-file)
	    (setq org-id-locations (read (current-buffer)))
	    (let ((loc (file-name-directory org-id-locations-file)))
	      (mapc (lambda (item)
		      (unless (file-name-absolute-p (car item))
			(setf (car item) (expand-file-name (car item) loc))))
		    org-id-locations)))
	(error
         (message "Could not read `org-id-locations' from %s, setting it to nil"
		  org-id-locations-file))))
    (setq org-id-files (mapcar 'car org-id-locations))
    (setq org-id-locations (org-id-alist-to-hash org-id-locations))))

(defun org-id-add-location (id file)
  "Add the ID with location FILE to the database of ID locations."
  ;; Only if global tracking is on, and when the buffer has a file
  (unless file
    (error "`org-id-get' expects a file-visiting buffer"))
  (let ((afile (abbreviate-file-name file)))
    (when (and org-id-track-globally id)
      (unless org-id-locations (org-id-locations-load))
      (puthash id afile org-id-locations)
      (unless (member afile org-id-files)
	(add-to-list 'org-id-files afile)))))

(unless noninteractive
  (add-hook 'kill-emacs-hook 'org-id-locations-save))

(defun org-id-hash-to-alist (hash)
  "Turn an org-id HASH into an alist.
This is to be able to write it to a file."
  (let (res x)
    (maphash
     (lambda (k v)
       (if (setq x (assoc v res))
	   (setcdr x (cons k (cdr x)))
	 (push (list v k) res)))
     hash)
    res))

(defun org-id-alist-to-hash (list)
  "Turn an org-id location LIST into a hash table."
  (let ((res (make-hash-table
	      :test 'equal
	      :size (apply '+ (mapcar 'length list))))
	f)
    (mapc
     (lambda (x)
       (setq f (car x))
       (mapc (lambda (i) (puthash i f res)) (cdr x)))
     list)
    res))

(defun org-id-paste-tracker (txt &optional buffer-or-file)
  "Update any ids in TXT and assign BUFFER-OR-FILE to them."
  (when org-id-track-globally
    (save-match-data
      (setq buffer-or-file (or buffer-or-file (current-buffer)))
      (when (bufferp buffer-or-file)
	(setq buffer-or-file (or (buffer-base-buffer buffer-or-file)
				 buffer-or-file))
	(setq buffer-or-file (buffer-file-name buffer-or-file)))
      (when buffer-or-file
	(let ((fname (abbreviate-file-name buffer-or-file))
	      (s 0))
	  (while (string-match "^[ \t]*:ID:[ \t]+\\([^ \t\n\r]+\\)" txt s)
	    (setq s (match-end 0))
	    (org-id-add-location (match-string 1 txt) fname)))))))

;; Finding entries with specified id

;;;###autoload
(defun org-id-find-id-file (id)
  "Query the id database for the file in which ID is located."
  (unless org-id-locations (org-id-locations-load))
  (or (and org-id-locations
	   (hash-table-p org-id-locations)
	   (gethash id org-id-locations))
      ;; Fall back on current buffer
      (buffer-file-name (or (buffer-base-buffer (current-buffer))
			    (current-buffer)))))

(defun org-id-find-id-in-file (id file &optional markerp)
  "Return the position of the entry ID in FILE.

If that files does not exist, or if it does not contain this ID,
return nil.

The position is returned as a cons cell (file-name . position).  With
optional argument MARKERP, return the position as a new marker."
  (cond
   ((not file) nil)
   ((not (file-exists-p file)) nil)
   (t
    (let* ((visiting (find-buffer-visiting file))
	   (buffer (or visiting
                       (if markerp (find-file-noselect file)
                         (org-get-buffer-create " *Org ID temp*" t)))))
      (unwind-protect
	  (with-current-buffer buffer
            (unless (derived-mode-p 'org-mode) (org-mode))
            (unless (or visiting markerp)
              (buffer-disable-undo)
              ;; FIXME: In Emacs 27, `insert-file-contents' seemingly
              ;; does not trigger modification hooks in some
              ;; scenarios.  This is manifested in test failures due
              ;; to element cache losing track of the modifications.
              (org-element-cache-reset)
              (insert-file-contents file nil nil nil 'replace))
	    (let ((pos (org-find-entry-with-id id)))
	      (cond
	       ((null pos) nil)
	       (markerp (move-marker (make-marker) pos buffer))
	       (t (cons file pos)))))
	;; Clean temporarily buffer if we don't need to keep it.
	(unless (or visiting markerp)
          (with-current-buffer buffer (erase-buffer))))))))

(provide 'org-id-search)

;;; org-id-search.el ends here
