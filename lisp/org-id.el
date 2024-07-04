;;; org-id.el --- Global identifiers for Org entries -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2008-2024 Free Software Foundation, Inc.
;;
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

;; This file implements globally unique identifiers for Org entries.
;; Identifiers are stored in the entry as an :ID: property.  Functions
;; are provided that create and retrieve such identifiers, and that find
;; entries based on the identifier.

;; Identifiers consist of a prefix (given by the variable
;; `org-id-prefix') and a unique part that can be created by a number
;; of different methods, see the variable `org-id-method'.  Org has a
;; builtin method that uses a compact encoding of the creation time of
;; the ID, with microsecond accuracy.  This virtually guarantees
;; globally unique identifiers, even if several people are creating
;; IDs at the same time in files that will eventually be used
;; together.
;;
;; By default Org uses UUIDs as global unique identifiers.
;;
;; This file defines the following API:
;;
;; org-id-get-create
;;        Create an ID for the entry at point if it does not yet have one.
;;        Returns the ID (old or new).  This function can be used
;;        interactively, with prefix argument the creation of a new ID is
;;        forced, even if there was an old one.
;;
;; org-id-get
;;        Get the ID property of an entry.  Using appropriate arguments
;;        to the function, it can also create the ID for this entry.
;;
;; org-id-goto
;;        Command to go to a specific ID, this command can be used
;;        interactively.
;;
;; org-id-get-with-outline-path-completion
;;        Retrieve the ID of an entry, using outline path completion.
;;        This function can work for multiple files.
;;
;; org-id-get-with-outline-drilling
;;        Retrieve the ID of an entry, using outline path completion.
;;        This function only works for the current file.
;;
;; org-id-find
;;        Find the location of an entry with specific id.
;;

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element-ast)
(require 'org-element)
(require 'org-property)
(require 'org-move)
(require 'org-agenda-files)
(require 'ol)

(require 'org-id-search)

;;; Customization

(defgroup org-id nil
  "Options concerning global entry identifiers in Org mode."
  :tag "Org ID"
  :group 'org)

(defcustom org-id-link-to-org-use-id nil
  "Non-nil means storing a link to an Org file will use entry IDs.
\\<org-mode-map>
The variable can have the following values:

t     Create an ID if needed to make a link to the current entry.

create-if-interactive
      If `org-store-link' is called directly (interactively, as a user
      command), do create an ID to support the link.  But when doing the
      job for capture, only use the ID if it already exists.  The
      purpose of this setting is to avoid proliferation of unwanted
      IDs, just because you happen to be in an Org file when you
      call `org-capture' that automatically and preemptively creates a
      link.  If you do want to get an ID link in a capture template to
      an entry not having an ID, create it first by explicitly creating
      a link to it, using `\\[org-store-link]' first.

create-if-interactive-and-no-custom-id
      Like create-if-interactive, but do not create an ID if there is
      a CUSTOM_ID property defined in the entry.

use-existing
      Use existing ID, do not create one.

nil   Never use an ID to make a link, instead link using a text search for
      the headline text."
  :group 'org-link-store
  :group 'org-id
  :version "24.3"
  :type '(choice
	  (const :tag "Create ID to make link" t)
	  (const :tag "Create if storing link interactively"
		 create-if-interactive)
	  (const :tag "Create if storing link interactively and no CUSTOM_ID is present"
		 create-if-interactive-and-no-custom-id)
	  (const :tag "Only use existing" use-existing)
	  (const :tag "Do not use ID to create link" nil)))

(defcustom org-id-link-consider-parent-id nil
  "Non-nil means storing a link to an Org entry considers inherited IDs.

When this option is non-nil and `org-id-link-use-context' is
enabled, ID properties inherited from parent entries will be
considered when storing an ID link.  If no ID is found in this
way, a new one may be created as normal (see
`org-id-link-to-org-use-id').

For example, given this org file:

* Parent
:PROPERTIES:
:ID: abc
:END:
** Child 1
** Child 2

With `org-id-link-consider-parent-id' and
`org-id-link-use-context' both enabled, storing a link with point
at \"Child 1\" will produce a link \"<id:abc::*Child 1>\".  This
allows linking to uniquely-named sub-entries within a parent
entry with an ID, without requiring every sub-entry to have its
own ID."
  :group 'org-link-store
  :group 'org-id
  :package-version '(Org . "9.7")
  :type 'boolean)

(defcustom org-id-link-use-context t
  "Non-nil means enables search string context in org-id links.

Search strings are added by `org-id-store-link' when both the
general option `org-link-context-for-files' and the org-id option
`org-id-link-use-context' are non-nil."
  :group 'org-link-store
  :group 'org-id
  :package-version '(Org . "9.7")
  :type 'boolean)

(defcustom org-id-uuid-program "uuidgen"
  "The uuidgen program."
  :group 'org-id
  :type 'string)

(defcustom org-id-ts-format "%Y%m%dT%H%M%S.%6N"
  "Timestamp format for IDs generated using `ts' `org-id-method'.
The format should be suitable to pass as an argument to `format-time-string'.

Defaults to ISO8601 timestamps without separators and without
timezone, local time and precision down to 1e-6 seconds."
  :type 'string
  :package-version '(Org . "9.5"))

(defcustom org-id-method 'uuid
  "The method that should be used to create new IDs.

An ID will consist of the optional prefix specified in `org-id-prefix',
and a unique part created by the method this variable specifies.

Allowed values are:

org        Org's own internal method, using an encoding of the current time to
           microsecond accuracy, and optionally the current domain of the
           computer.  See the variable `org-id-include-domain'.

uuid       Create random (version 4) UUIDs.  If the program defined in
           `org-id-uuid-program' is available it is used to create the ID.
           Otherwise an internal functions is used.

ts         Create ID's based on timestamps as specified in `org-id-ts-format'."
  :group 'org-id
  :type '(choice
	  (const :tag "Org's internal method" org)
	  (const :tag "external: uuidgen" uuid)
	  (const :tag "Timestamp with format `org-id-ts-format'" ts)))

(defcustom org-id-prefix nil
  "The prefix for IDs.

This may be a string, or it can be nil to indicate that no prefix is required.
When a string, the string should have no space characters as IDs are expected
to have no space characters in them."
  :group 'org-id
  :type '(choice
	  (const :tag "No prefix")
	  (string :tag "Prefix")))

(defcustom org-id-include-domain nil
  "Non-nil means add the domain name to new IDs.
This ensures global uniqueness of IDs, and is also suggested by
the relevant RFCs.  This is relevant only if `org-id-method' is
`org' or `ts'.  When uuidgen is used, the domain will never be added.

The default is to not use this because we have no really good way to get
the true domain, and Org entries will normally not be shared with enough
people to make this necessary."
  :group 'org-id
  :type 'boolean)

;;; The API functions

(declare-function org-entry-put "org-property-set" (epom property value))
;;;###autoload
(defun org-id-get-create (&optional force)
  "Create an ID for the current entry and return it.
If the entry already has an ID, just return it.
With optional argument FORCE, force the creation of a new ID."
  (interactive "P")
  (when force
    (progn
      (require 'org-property-set)
      (org-entry-put (point) "ID" nil)))
  (org-id-get (point) 'create))

;;;###autoload
(defun org-id-copy ()
  "Copy the ID of the entry at point to the kill ring.
Create an ID if necessary."
  (interactive)
  (org-kill-new (org-id-get nil 'create)))

(defvar org-id-overriding-file-name nil
  "Tell `org-id-get' to use this as the file name when creating an ID.
This is useful when working with contents in a temporary buffer
that will be copied back to the original.")

(declare-function org-entry-put "org-property-set" (epom property value))
;;;###autoload
(defun org-id-get (&optional epom create prefix inherit)
  "Get the ID of the entry at EPOM.

EPOM is an element, marker, or buffer position.  If EPOM is nil,
refer to the entry at point.

If INHERIT is non-nil, ID properties inherited from parent
entries are considered.  Otherwise, only ID properties on the
entry itself are considered.

When CREATE is nil, return the ID of the entry if found,
otherwise nil.  When CREATE is non-nil, create an ID if none has
been found, and return the new ID.  PREFIX will be passed through
to `org-id-new'."
  (let ((id (org-entry-get epom "ID" (and inherit t))))
    (cond
     ((and id (stringp id) (string-match "\\S-" id))
      id)
     (create
      (setq id (org-id-new prefix))
      (require 'org-property-set)
      (org-entry-put epom "ID" id)
      (org-with-point-at epom
        (org-id-add-location id
			     (or org-id-overriding-file-name
			         (buffer-file-name (buffer-base-buffer)))))
      id))))

(defvar org-refile-use-outline-path)
(defvar org-refile-target-verify-function)
(declare-function org-refile-get-location "org-refile"
                  (&optional prompt default-buffer new-nodes))
;;;###autoload
(defun org-id-get-with-outline-path-completion (&optional targets)
  "Use `outline-path-completion' to retrieve the ID of an entry.
TARGETS may be a setting for `org-refile-targets' to define
eligible headlines.  When omitted, all headlines in the current
file are eligible.  This function returns the ID of the entry.
If necessary, the ID is created."
  (require 'org-refile)
  (let* ((org-refile-targets (or targets '((nil . (:maxlevel . 10)))))
	 (org-refile-use-outline-path
	  (if (caar org-refile-targets) 'file t))
	 (org-refile-target-verify-function nil)
	 (spos (org-refile-get-location "Entry"))
	 (pom (and spos (move-marker (make-marker) (or (nth 3 spos) 1)
				     (get-file-buffer (nth 1 spos))))))
    (prog1 (org-id-get pom 'create)
      (move-marker pom nil))))

(declare-function org-goto-location "org-goto" (&optional _buf help))
;;;###autoload
(defun org-id-get-with-outline-drilling ()
  "Use an outline-cycling interface to retrieve the ID of an entry.
This only finds entries in the current buffer, using `org-goto-location'.
It returns the ID of the entry.  If necessary, the ID is created."
  (require 'org-goto)
  (let* ((spos (org-goto-location))
	 (pom (and spos (move-marker (make-marker) (car spos)))))
    (prog1 (org-id-get pom 'create)
      (move-marker pom nil))))

(declare-function org-fold-show-context "org-fold" (&optional key))
;;;###autoload
(defun org-id-goto (id)
  "Switch to the buffer containing the entry with id ID.
Move the cursor to that entry in that buffer."
  (interactive "sID: ")
  (let ((m (org-id-find id 'marker)))
    (unless m
      (error "Cannot find entry with ID \"%s\"" id))
    (pop-to-buffer-same-window (marker-buffer m))
    (goto-char m)
    (move-marker m nil)
    (org-fold-show-context)))

;;; Internal functions

;; Creating new IDs

(declare-function message-make-fqdn "message" ())
;;;###autoload
(defun org-id-new (&optional prefix)
  "Create a new globally unique ID.

An ID consists of two parts separated by a colon:
- a prefix
- a unique part that will be created according to `org-id-method'.

PREFIX can specify the prefix, the default is given by the variable
`org-id-prefix'.  However, if PREFIX is the symbol `none', don't use any
prefix even if `org-id-prefix' specifies one.

So a typical ID could look like \"Org:4nd91V40HI\"."
  (let* ((prefix (if (eq prefix 'none)
		     ""
		   (concat (or prefix org-id-prefix) ":")))
	 unique)
    (if (equal prefix ":") (setq prefix ""))
    (cond
     ((memq org-id-method '(uuidgen uuid))
      (setq unique (org-trim (shell-command-to-string org-id-uuid-program)))
      (unless (org-uuidgen-p unique)
	(setq unique (org-id-uuid))))
     ((eq org-id-method 'org)
      (let* ((etime (org-reverse-string (org-id-time-to-b36)))
	     (postfix (when org-id-include-domain
			(require 'message)
			(concat "@" (message-make-fqdn)))))
	(setq unique (concat etime postfix))))
     ((eq org-id-method 'ts)
      (let ((ts (format-time-string org-id-ts-format))
	    (postfix (when org-id-include-domain
		       (require 'message)
		       (concat "@" (message-make-fqdn)))))
	(setq unique (concat ts postfix))))
     (t (error "Invalid `org-id-method'")))
    (concat prefix unique)))

(defun org-id-int-to-b36-one-digit (integer)
  "Convert INTEGER between 0 and 61 into a single character 0..9, A..Z, a..z."
  (cond
   ((< integer 10) (+ ?0 integer))
   ((< integer 36) (+ ?a integer -10))
   (t (error "Larger that 35"))))

(defun org-id-b36-to-int-one-digit (i)
  "Convert character 0..9, A..Z, a..z into a number 0..61.
The input I may be a character, or a single-letter string."
  (and (stringp i) (setq i (string-to-char i)))
  (cond
   ((and (>= i ?0) (<= i ?9)) (- i ?0))
   ((and (>= i ?a) (<= i ?z)) (+ (- i ?a) 10))
   (t (error "Invalid b36 letter"))))

(defun org-id-int-to-b36 (integer &optional length)
  "Convert an INTEGER to a base-36 number represented as a string.
The returned string is padded with leading zeros to LENGTH if necessary."
  (let ((s "")
        (i integer))
    (while (> i 0)
      (setq s (concat (char-to-string
		       (org-id-int-to-b36-one-digit (mod i 36))) s)
	    i (/ i 36)))
    (setq length (max 1 (or length 1)))
    (if (< (length s) length)
	(setq s (concat (make-string (- length (length s)) ?0) s)))
    s))

(defun org-id-b36-to-int (string)
  "Convert a base-36 STRING into the corresponding integer."
  (let ((r 0))
    (mapc (lambda (i) (setq r (+ (* r 36) (org-id-b36-to-int-one-digit i))))
	  string)
    r))

(defun org-id-time-to-b36 (&optional time)
  "Encode TIME as a 12-digit string.
This string holds the time to micro-second accuracy, and can be decoded
using `org-id-decode'."
  ;; FIXME: If TIME represents N seconds after the epoch, then
  ;; this encoding assumes 0 <= N < 110075314176 = (* (expt 36 4) 65536),
  ;; i.e., that TIME is from 1970-01-01 00:00:00 to 5458-02-23 20:09:36 UTC.
  (setq time (org-time-convert-to-list nil))
  (concat (org-id-int-to-b36 (nth 0 time) 4)
	  (org-id-int-to-b36 (nth 1 time) 4)
	  (org-id-int-to-b36 (nth 2 time) 4)))

(defun org-id-decode (id)
  "Split ID into the prefix and the time value that was used to create it.
The return value is (prefix . time) where PREFIX is nil or a string,
and TIME is a Lisp time value (HI LO USEC)."
  (let (prefix time parts)
    (setq parts (org-split-string id ":"))
    (if (= 2 (length parts))
	(setq prefix (car parts) time (nth 1 parts))
      (setq prefix nil time (nth 0 parts)))
    (setq time (org-reverse-string time))
    (setq time (list (org-id-b36-to-int (substring time 0 4))
		     (org-id-b36-to-int (substring time 4 8))
		     (org-id-b36-to-int (substring time 8 12))))
    (cons prefix time)))

;; id link type

(defun org-id--get-id-to-store-link (&optional create)
  "Get or create the relevant ID for storing a link.

Optional argument CREATE is passed to `org-id-get'.

Inherited IDs are only considered when
`org-id-link-consider-parent-id', `org-id-link-use-context' and
`org-link-context-for-files' are all enabled, since inherited IDs
are confusing without the additional search string context.

Note that this function resets the
`org-entry-property-inherited-from' marker: it will either point
to nil (if the id was not inherited) or to the point it was
inherited from."
  (let* ((inherit-id (and org-id-link-consider-parent-id
                          org-id-link-use-context
                          org-link-context-for-files)))
    (move-marker org-entry-property-inherited-from nil)
    (org-id-get nil create nil inherit-id)))

;;;###autoload
(defun org-id-store-link ()
  "Store a link to the current entry, using its ID.

The link description is based on the heading, or if before the
first heading, the title keyword if available, or else the
filename.

When `org-link-context-for-files' and `org-id-link-use-context'
are non-nil, add a search string to the link.  The link
description is then based on the search string target.

When in addition `org-id-link-consider-parent-id' is non-nil, the
ID can be inherited from a parent entry, with the search string
used to still link to the current location."
  (interactive)
  (when (and (buffer-file-name (buffer-base-buffer))
             (derived-mode-p 'org-mode))
    ;; Get the precise target first, in case looking for an id causes
    ;; a properties drawer to be added at the current location.
    (let* ((precise-target (and org-link-context-for-files
                                org-id-link-use-context
                                (org-link-precise-link-target)))
           (link (concat "id:" (org-id--get-id-to-store-link 'create)))
           (id-location (or (and org-entry-property-inherited-from
                                 (marker-position org-entry-property-inherited-from))
                            (save-excursion (org-back-to-heading-or-point-min t) (point))))
	   (case-fold-search nil)
	   (desc (save-excursion
                   (goto-char id-location)
                   (cond ((org-before-first-heading-p)
                          (or (org-element-property :TITLE (org-element-org-data))
                              (file-name-nondirectory
			       (buffer-file-name (buffer-base-buffer)))))
		         ((looking-at (org-complex-heading-regexp))
			  (if (match-end 4)
			      (match-string 4)
			    (match-string 0)))
                         (t link)))))
      ;; Precise targets should be after id-location to avoid
      ;; duplicating the current headline as a search string
      (when (and precise-target
                 (> (nth 2 precise-target) id-location))
        (setq link (concat link "::" (nth 0 precise-target)))
        (setq desc (nth 1 precise-target)))
      (org-link-store-props :link link :description desc :type "id")
      link)))

;;;###autoload
(defun org-id-store-link-maybe (&optional interactive?)
  "Store a link to the current entry using its ID if enabled.

The value of `org-id-link-to-org-use-id' determines whether an ID
link should be stored, using `org-id-store-link'.

Assume the function is called interactively if INTERACTIVE? is
non-nil."
  (when (and (buffer-file-name (buffer-base-buffer))
             (derived-mode-p 'org-mode)
             (or (eq org-id-link-to-org-use-id t)
                 (and interactive?
                      (or (eq org-id-link-to-org-use-id 'create-if-interactive)
                          (and (eq org-id-link-to-org-use-id
                                   'create-if-interactive-and-no-custom-id)
                               (not (org-entry-get nil "CUSTOM_ID")))))
                 ;; 'use-existing
                 (and org-id-link-to-org-use-id
                      (org-id--get-id-to-store-link))))
    (org-id-store-link)))

(declare-function org-mark-ring-push "org-mark-ring" (&optional pos buffer))
(declare-function org-narrow-to-subtree "org-narrow" (&optional element))
(defun org-id-open (link _)
  "Go to the entry indicated by id link LINK.

The link can include a search string after \"::\", which is
passed to `org-link-search'.

For backwards compatibility with IDs that contain \"::\", if no
match is found for the ID, the full link string including \"::\"
will be tried as an ID."
  (let* ((option (and (string-match "::\\(.*\\)\\'" link)
		      (match-string 1 link)))
	 (id (if (not option) link
               (substring link 0 (match-beginning 0))))
         m cmd)
    (require 'org-mark-ring)
    (org-mark-ring-push)
    (setq m (org-id-find id 'marker))
    (when (and (not m) option)
      ;; Backwards compatibility: if id is not found, try treating
      ;; whole link as an id.
      (setq m (org-id-find link 'marker))
      (when m
        (setq option nil)))
    (unless m
      (error "Cannot find entry with ID \"%s\"" id))
    ;; Use a buffer-switching command in analogy to finding files
    (setq cmd
	  (or
	   (cdr
	    (assq
	     (cdr (assq 'file org-link-frame-setup))
	     '((find-file . switch-to-buffer)
	       (find-file-other-window . switch-to-buffer-other-window)
	       (find-file-other-frame . switch-to-buffer-other-frame))))
	   'switch-to-buffer-other-window))
    (if (not (equal (current-buffer) (marker-buffer m)))
	(funcall cmd (marker-buffer m)))
    (goto-char m)
    (move-marker m nil)
    (when option
      (save-restriction
        (unless (org-before-first-heading-p)
          (require 'org-narrow)
          (org-narrow-to-subtree))
        (org-link-search option nil nil
                         (org-element-lineage (org-element-at-point) 'headline t))))
    (org-fold-show-context)))

(org-link-set-parameters "id"
  :follow #'org-id-open
  :store #'org-id-store-link-maybe)

(provide 'org-id)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-id.el ends here
