;;; org-crypt.el --- Public Key Encryption for Org Entries -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2007-2024 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;;; Commentary:

;; Right now this is just a set of functions to play with.  It depends
;; on the epg library.  Here's how you would use it:
;;
;; 1. To mark an entry for encryption, tag the heading with "crypt".
;;    You can change the tag to any complex tag matching string by
;;    setting the `org-crypt-tag-matcher' variable.
;;
;; 2. Set the encryption key to use in the `org-crypt-key' variable,
;;    or use `M-x org-set-property' to set the property CRYPTKEY to
;;    any address in your public keyring.  The text of the entry (but
;;    not its properties or headline) will be encrypted for this user.
;;    For them to read it, the corresponding secret key must be
;;    located in the secret key ring of the account where you try to
;;    decrypt it.  This makes it possible to leave secure notes that
;;    only the intended recipient can read in a shared-org-mode-files
;;    scenario.
;;    If the key is not set, org-crypt will default to symmetric encryption.
;;
;; 3. To later decrypt an entry, use `org-decrypt-entries' or
;;    `org-decrypt-entry'.  It might be useful to bind this to a key,
;;    like C-c C-/.
;;
;; 4. To automatically encrypt all necessary entries when saving a
;;    file, call `org-crypt-use-before-save-magic' after loading
;;    org-crypt.el.

;;; Thanks:

;; - Carsten Dominik
;; - Vitaly Ostanin

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-macs)
(require 'org-compat)

(declare-function epg-decrypt-string "epg" (context cipher))
(declare-function epg-list-keys "epg" (context &optional name mode))
(declare-function epg-make-context "epg"
		  (&optional protocol armor textmode include-certs
			     cipher-algorithm digest-algorithm
			     compress-algorithm))
(declare-function epg-encrypt-string "epg"
		  (context plain recipients &optional sign always-trust))
(defvar epg-context)

(declare-function org-back-over-empty-lines "org" ())
(declare-function org-current-level "org" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-before-first-heading-p "org" ())
(declare-function org-end-of-meta-data "org" (&optional full))
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading element))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-fold-subtree "org-fold" (flag))
(declare-function org-make-tags-matcher "org" (match &optional only-local-tags))
(declare-function org-previous-visible-heading "org" (arg))
(declare-function org-scan-tags "org" (action matcher todo-only &optional start-level))
(declare-function org-set-property "org" (property value))
(declare-function org-cycle-set-startup-visibility "org-cycle" ())

(defgroup org-crypt nil
  "Org Crypt."
  :tag "Org Crypt"
  :group 'org)

(defcustom org-crypt-tag-matcher "crypt"
  "The tag matcher used to find headings whose contents should be encrypted.

See the \"Match syntax\" section of the org manual for more details."
  :type 'string
  :group 'org-crypt)

(defcustom org-crypt-key ""
  "The default key to use when encrypting the contents of a heading.

If this variable is nil, always use symmetric encryption, unconditionally.

Otherwise, The string is matched against all keys in the key ring.
In particular, the empty string matches no key.  If no key is found,
look for the `epa-file-encrypt-to' local variable.  Ultimately fall back
to symmetric encryption.

This setting can be overridden in the CRYPTKEY property."
  :group 'org-crypt
  :type '(choice
	  (string :tag "Public key(s) matching")
	  (const :tag "Symmetric encryption" nil)))

(defun org-crypt--set-encrypt-on-save (var encrypt?)
  "Set function for `org-crypt-encrypt-on-save'."
  (set-default-toplevel-value var encrypt?)
  (eval-after-load "org-crypt"
    '(org-crypt-use-before-save-magic (not encrypt?))))

(defcustom org-crypt-encrypt-on-save t
  "When non-nil, write encrypted entries to disk.
More specifically, non-nil will arrange encrypting all the entries that
match `org-crypt-tag-matcher' before writing buffer to disk.  Then,
these entries will be decrypted back, so that they state the same
inside Org buffer."
  :group 'org-crypt
  :set #'org-crypt--set-encrypt-on-save
  :type 'boolean
  :package-version '(Org . "9.8"))

(defcustom org-crypt-disable-auto-save 'ask
  "What org-crypt should do if `auto-save-mode' is enabled.

t        : Disable `auto-save-mode' for the current buffer
           prior to decrypting an entry.

nil      : Leave `auto-save-mode' enabled.
           This may cause data to be written to disk unencrypted!

`ask'    : Ask user whether or not to disable `auto-save-mode'
           for the current buffer.

`encrypt': Leave `auto-save-mode' enabled for the current buffer,
           but automatically re-encrypt all decrypted entries
           *before* auto-saving.
           NOTE: This only works for entries which have a tag
           that matches `org-crypt-tag-matcher'."
  :group 'org-crypt
  :version "24.1"
  :type '(choice (const :tag "Always"  t)
                 (const :tag "Never (not recommended)"   nil)
                 (const :tag "Ask"     ask)
                 (const :tag "Encrypt" encrypt)))

(defun org-crypt--encrypted-text (beg end)
  "Return encrypted text in between BEG and END."
  ;; Ignore indentation.
  (replace-regexp-in-string
   "^[ \t]*" ""
   (buffer-substring-no-properties beg end)))

(defun org-at-encrypted-entry-p ()
  "Is the current entry encrypted?
When the entry is encrypted, return a pair (BEG . END) where BEG
and END are buffer positions delimiting the encrypted area."
  (org-with-wide-buffer
   (unless (org-before-first-heading-p)
     (org-back-to-heading t)
     (org-end-of-meta-data 'standard)
     (let ((case-fold-search nil)
	   (banner-start (rx (seq bol
				  (zero-or-more (any "\t "))
				  "-----BEGIN PGP MESSAGE-----"
				  eol))))
       (when (looking-at banner-start)
	 (let ((start (point))
	       (banner-end (rx (seq bol
				    (or (group (zero-or-more (any "\t "))
					       "-----END PGP MESSAGE-----"
					       eol)
					(seq (one-or-more "*") " "))))))
	   (when (and (re-search-forward banner-end nil t) (match-string 1))
	     (cons start (line-beginning-position 2)))))))))

(defun org-crypt-check-auto-save ()
  "Check whether `auto-save-mode' is enabled for the current buffer.

`auto-save-mode' may cause leakage when decrypting entries, so
check whether it's enabled, and decide what to do about it.

See `org-crypt-disable-auto-save'."
  (when buffer-auto-save-file-name
    (let ((org-crypt-disable-auto-save
           (if (and (eq org-crypt-disable-auto-save 'ask)
                    (y-or-n-p "`org-decrypt': auto-save-mode may cause leakage.  Disable it for current buffer? "))
               t org-crypt-disable-auto-save)))
      (pcase org-crypt-disable-auto-save
        (`t
         (message "org-decrypt: Disabling auto-save-mode for %s%s"
                  (or (buffer-file-name) (current-buffer))
                  (if (eq org-crypt-disable-auto-save 'ask)
                      "\nCustomize `org-crypt-disable-auto-save' to set the default answer"
                    ""))
         ;; The argument to auto-save-mode has to be "-1", since
         ;; giving a "nil" argument toggles instead of disabling.
         (auto-save-mode -1))
        (`nil
         (message "org-decrypt: Decrypting entry with auto-save-mode enabled.  This may cause leakage."))
        (`encrypt
         (unless (memq #'org-crypt--encrypt-entries-all-buffers auto-save-hook)
           (message "org-decrypt: Enabling re-encryption on auto-save."))
         (add-hook 'auto-save-hook
                   #'org-crypt--encrypt-entries-all-buffers
		   nil t))
        (other-value (error "Incorrect value of `org-crypt-disable-auto-save': %S" other-value))))))

(defun org-crypt-key-for-heading ()
  "Return the encryption key(s) for the current heading.
Assume `epg-context' is set."
  (and org-crypt-key
       (or (epg-list-keys epg-context
			  (pcase (org-entry-get nil "CRYPTKEY" 'selective 'literal-nil)
                            ("nil" "")
                            (key (or key org-crypt-key ""))))
	   (bound-and-true-p epa-file-encrypt-to)
	   (progn
	     (message "No crypt key set, using symmetric encryption.")
	     nil))))

(defun org-encrypt--set-reveal-callback (beg end context)
  "Set callback that displays BEG..END before it is encrypted/decrypted in CONTEXT.
The BEG..END is a region defined by markers to be displayed and
highlighted."
  (setf (epg-context-pinentry-mode epg-context) 'loopback)
  (epg-context-set-passphrase-callback
   context
   (let ((old-callback (epg-context-passphrase-callback context)))
     (cons
      (lambda (context key-id handback)
        (save-window-excursion
          (with-current-buffer (marker-buffer beg)
            (display-buffer (marker-buffer beg))
            (goto-char (marker-position beg))
            (org-fold-show-set-visibility 'local)
            (let ((ov (make-overlay beg end)))
              (overlay-put ov 'face 'highlight)
              (unwind-protect
                  (funcall (car old-callback)
                           context key-id handback)
                (delete-overlay ov))))))
      (cdr old-callback)))))

;;;###autoload
(defun org-encrypt-entry ()
  "Encrypt the content of the current headline.
Return non-nil when the entry was actually encrypted."
  (interactive)
  (unless (org-at-encrypted-entry-p)
    (require 'epg)
    (setq-local epg-context (epg-make-context nil t t))
    (org-with-wide-buffer
     (org-back-to-heading t)
     (setcdr (epg-context-passphrase-callback epg-context)
             (format "encrypting \"%s\"" (org-get-heading t t t t)))
     (let ((start-heading (point))
	   (crypt-key (org-crypt-key-for-heading))
	   (folded? (org-invisible-p (line-beginning-position))))
       (org-end-of-meta-data 'standard)
       (let ((beg (point))
	     (folded-heading
	      (and folded?
		   (save-excursion
		     (org-previous-visible-heading 1)
		     (point)))))
	 (goto-char start-heading)
	 (org-end-of-subtree t t)
	 (org-back-over-empty-lines)
         ;; Display heading to be encrypted when querying the passphrase.
         (org-encrypt--set-reveal-callback (copy-marker beg) (point-marker) epg-context)
	 (let* ((contents (buffer-substring beg (point)))
		(key (get-text-property 0 'org-crypt-key contents))
		(checksum (get-text-property 0 'org-crypt-checksum contents)))
           (let ((encrypted-text
                  ;; Text and key have to be identical, otherwise we
	          ;; re-crypt.
	          (if (and (equal crypt-key key)
		           (string= checksum (sha1 contents)))
		      (get-text-property 0 'org-crypt-text contents)
                    (condition-case err
		        (epg-encrypt-string epg-context contents crypt-key)
                      (error (error "Org-crypt (%s): %S"
                                    (current-buffer)
                                    (error-message-string err)))))))
             (delete-region beg (point))
	     (insert encrypted-text)
             (backward-char) ; make sure that we are not at the next heading
             (pcase (org-at-encrypted-entry-p)
               (`(,beg . ,end)
                (let ((encrypted-text (org-crypt--encrypted-text beg end)))
                  (add-text-properties
                   beg end
                   `( org-crypt-checksum ,(sha1 encrypted-text)
                      org-crypt-text ,contents)))))))
	 (when folded-heading
	   (goto-char folded-heading)
	   (org-fold-subtree t))
	 t)))))

(defvar org-outline-regexp-bol)
;;;###autoload
(defun org-decrypt-entry ()
  "Decrypt the content of the current headline."
  (interactive)
  (pcase (org-at-encrypted-entry-p)
    (`(,beg . ,end)
     (require 'epg)
     (setq-local epg-context (epg-make-context nil t t))
     (setcdr (epg-context-passphrase-callback epg-context)
             (format "decrypting \"%s\"" (org-get-heading t t t t)))
     (org-with-point-at beg
       (org-crypt-check-auto-save)
       ;; Display heading to be encrypted when querying the passphrase.
       (org-encrypt--set-reveal-callback (copy-marker beg) (copy-marker end) epg-context)
       (let* ((folded-heading
	       (and (org-invisible-p)
		    (save-excursion
		      (org-previous-visible-heading 1)
		      (point))))
              (level (org-current-level))
	      (encrypted-text (org-crypt--encrypted-text beg end))
	      (decrypted-text
	       (decode-coding-string
                (if (equal (sha1 encrypted-text)
                           (get-text-property beg 'org-crypt-checksum))
                    (get-text-property beg 'org-crypt-text)
                  (condition-case err
		      (epg-decrypt-string epg-context encrypted-text)
                    (error (error "Org-crypt (%s): %S"
                                  (current-buffer)
                                  (error-message-string err)))))
		'utf-8))
              origin-marker)
	 ;; Delete region starting just before point, because the
	 ;; outline property starts at the \n of the heading.
	 (delete-region (1- (point)) end)
         (setq origin-marker (point-marker))
         (if (string-match (org-headline-re level) decrypted-text)
             ;; If decrypted text contains other headings with levels
             ;; below LEVEL, adjust the subtree.
             (let ((start 0) (min-level level))
               (while (string-match (org-headline-re level) decrypted-text start)
                 (setq min-level (min min-level (1- (length (match-string 0 decrypted-text))))
                       start (match-end 0)))
               (insert "\n"
                       (replace-regexp-in-string
                        org-outline-regexp-bol
                        (concat (make-string (1+ (- level min-level)) ?*) "\\&")
                        decrypted-text)))
	   ;; Store a checksum of the decrypted and the encrypted text
	   ;; value.  This allows reusing the same encrypted text if the
	   ;; text does not change, and therefore avoid a re-encryption
	   ;; process.
	   (insert "\n"
		   (propertize decrypted-text
			       'org-crypt-checksum (sha1 decrypted-text)
			       'org-crypt-key (org-crypt-key-for-heading)
			       'org-crypt-text encrypted-text)))
         ;; Apply initial visibility.
         (save-restriction
           (narrow-to-region origin-marker (point))
           (set-marker origin-marker nil)
           (org-cycle-set-startup-visibility))
         ;; ... but keep the previous folded state.
	 (when folded-heading
	   (goto-char folded-heading)
	   (org-fold-subtree t))
	 nil)))
    (_ nil)))

(defvar org--matcher-tags-todo-only)

(defun org-encrypt--map-items (func)
  "Run FUNC at every entry matching `org-crypt-tag-matcher'."
  (let* ((org--matcher-tags-todo-only nil))
    (when (or (not (equal org-crypt-tag-matcher "crypt"))
              (org-with-wide-buffer
               (goto-char (point-min))
               (re-search-forward ":crypt:" nil t)))
      (org-scan-tags
       (lambda ()
         (funcall func)
         ;; FIXME: Ad-hoc optimization to speed-up encryption when
         ;; using defaults.  It is difficult to generalize it to
         ;; completely arbitrary `org-crypt-tag-matcher'.
         (when (equal org-crypt-tag-matcher "crypt")
           (re-search-forward ":crypt:" nil 'move)
           (org-back-to-heading t)))
       (cdr (org-make-tags-matcher org-crypt-tag-matcher))
       org--matcher-tags-todo-only))))

;;;###autoload
(defun org-encrypt-entries (&optional hook-function)
  "Encrypt all the entries matching `org-crypt-tag-matcher' in buffer.
When optional argument HOOK-FUNCTION is non-nil, it should be a function
accepting 0 arguments.  The function will be called with point at
encrypted entry if the entry was actually encrypted."
  (interactive)
  (org-encrypt--map-items
   (if hook-function
       (lambda () (when (org-encrypt-entry) (funcall hook-function)))
     #'org-encrypt-entry)))

;;;###autoload
(defun org-decrypt-entries ()
  "Decrypt all entries in the current buffer."
  (interactive)
  (org-encrypt--map-items #'org-decrypt-entry))

(defun org-crypt--encrypt-entries-all-buffers ()
  "Call `org-crypt--encrypt-and-mark-entries' in all Org buffers with auto-save.
Do not throw errors.  Do not mark headings for future auto-decryption."
  (message "org-crypt: Re-encrypting all decrypted entries due to auto-save.")
  (dolist (buf (org-buffer-list))
    (with-current-buffer buf
      (when buffer-auto-save-file-name
        (with-demoted-errors "%S"
          (org-crypt--encrypt-and-mark-entries 'no-mark))))))

(defvar-local org-crypt--point-before-encryption nil
  "Point position before encrypt+mark -> decrypt cycle.
See `org-crypt--encrypt-and-mark-entries',
`org-crypt--decrypt-marked-entries', and
`org-crypt--setup-save-hooks'.")
(defun org-crypt--encrypt-and-mark-entries (&optional no-mark)
  "Encrypt entries and mark them with `org-crypt-auto-encrypted' property.
When optional argument NO-MARK is non-nil, do not mark.
Return nil."
  (let ((modified-flag (buffer-modified-p)))
    ;; Store point position before we start encryption to restore it
    ;; later in case if encryption moves point that is inside entry to
    ;; be encrypted.
    (unless no-mark (setq org-crypt--point-before-encryption (point)))
    (condition-case err
        (unwind-protect
            (org-encrypt-entries
             (lambda ()
               (unless no-mark
                 (pcase (org-at-encrypted-entry-p)
                   (`(,beg . ,end)
                    (put-text-property beg end 'org-crypt-auto-encrypted t))))))
          (set-buffer-modified-p modified-flag))
      (error
       (unless no-mark (setq org-crypt--point-before-encryption nil))
       ;; Disable auto-save.
       (auto-save-mode -1)
       (error "Org-crypt: Encryption failed.  Not saving the buffer.
Use %s to disable auto-encryption in all the Org buffers
Error: %s"
              (substitute-command-keys "\\[universal-argument] \\[org-crypt-use-before-save-magic]")
              (error-message-string err)))))
  ;; Return nil to continue saving as usual.
  nil)
(defun org-crypt--decrypt-marked-entries ()
  "Decrypt all the entries marked with `org-crypt-auto-encrypted' property."
  (let ((modified-flag (buffer-modified-p)))
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (not (eobp))
       (when (get-text-property (point) 'org-crypt-auto-encrypted)
         (org-decrypt-entry))
       (goto-char (next-single-char-property-change (point) 'org-crypt-auto-encrypted))))
    (set-buffer-modified-p modified-flag)
    (when org-crypt--point-before-encryption
      (goto-char org-crypt--point-before-encryption)
      (setq org-crypt--point-before-encryption nil))))

(defun org-crypt--setup-save-hooks ()
  "Setup encryption before saving buffer to disk."
  (add-hook 'write-contents-functions #'org-crypt--encrypt-and-mark-entries nil t)
  (add-hook 'after-save-hook #'org-crypt--decrypt-marked-entries nil t))

;;;###autoload
(defun org-crypt-use-before-save-magic (&optional remove-hook)
  "Add hooks to automatically encrypt entries before a file is saved to disk.
When REMOVE-HOOK is non-nil, remove the hook instead."
  (interactive "P")
  (let ((setup
         (if remove-hook
             (lambda ()
               (remove-hook 'write-contents-functions #'org-crypt--encrypt-and-mark-entries t)
               (remove-hook 'after-save-hook #'org-crypt--decrypt-marked-entries t))
           #'org-crypt--setup-save-hooks)))
    (dolist (buf (org-buffer-list))
      (with-current-buffer buf
        (funcall setup)))
    (add-hook 'org-mode-hook setup)
    (when (called-interactively-p 'any)
      (if remove-hook
          (message "Org-crypt: Automatic encrypion on save OFF.  WARNING: Unencrypted data may leak to disk\n%s"
                   (substitute-command-keys "Use \\[org-crypt-use-before-save-magic] to re-enable auto-encryption"))
        (message "Org-crypt: Automatic encrypion on save ON")))))

(add-hook 'org-fold-reveal-start-hook 'org-decrypt-entry)

(provide 'org-crypt)

;;; org-crypt.el ends here
