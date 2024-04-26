;;; ol-core.el --- Org links API                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, wp

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

;; This library provides Elisp API to define new link types.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element)

(defcustom org-link-from-user-regexp
  (let ((mail (and (org-string-nw-p user-mail-address)
		   (format "\\<%s\\>" (regexp-quote user-mail-address))))
	(name (and (org-string-nw-p user-full-name)
		   (format "\\<%s\\>" (regexp-quote user-full-name)))))
    (if (and mail name) (concat mail "\\|" name) (or mail name)))
  "Regexp matched against the \"From:\" header of an email or Usenet message.
It should match if the message is from the user him/herself."
  :group 'org-link-store
  :type 'regexp
  :safe #'stringp)

(defvar org-store-link-plist nil
  "Plist with info about the most recently link created with `org-store-link'.")

(defvar org-stored-links nil
  "Contains the links stored with `org-store-link'.")

(defun org-link-get-parameter (type key)
  "Get TYPE link property for KEY.
TYPE is a string and KEY is a plist keyword.  See
`org-link-parameters' for supported keywords."
  (plist-get (cdr (assoc type org-link-parameters))
	     key))

(defun org-link-set-parameters (type &rest parameters)
  "Set link TYPE properties to PARAMETERS.
PARAMETERS should be keyword value pairs.  See
`org-link-parameters' for supported keys."
  (when (member type '("coderef" "custom-id" "fuzzy" "radio"))
    (error "Cannot override reserved link type: %S" type))
  (let ((data (assoc type org-link-parameters)))
    (if data (setcdr data (org-combine-plists (cdr data) parameters))
      (push (cons type parameters) org-link-parameters)
      (org-link-make-regexps)
      (when (featurep 'org-element) (org-element-update-syntax)))))

;; This way, one can add multiple functions as, say, :follow parameter.
;; For example,
;; (add-function :before-until (org-link-get-parameter "id" :follow) #'my-function)
;; See https://orgmode.org/list/a123389c-8f86-4836-a4fe-1e3f4281d33b@app.fastmail.com
(gv-define-setter org-link-get-parameter (value type key)
  `(org-link-set-parameters ,type ,key ,value))

(defun org-link-encode (text table)
  "Return percent escaped representation of string TEXT.
TEXT is a string with the text to escape.  TABLE is a list of
characters that should be escaped."
  (mapconcat
   (lambda (c)
     (if (memq c table)
	 (mapconcat (lambda (e) (format "%%%.2X" e))
		    (or (encode-coding-char c 'utf-8)
			(error "Unable to percent escape character: %c" c))
		    "")
       (char-to-string c)))
   text ""))

(defun org-link-decode (s)
  "Decode percent-encoded parts in string S.
E.g. \"%C3%B6\" becomes the German o-Umlaut."
  (replace-regexp-in-string "\\(%[0-9A-Za-z]\\{2\\}\\)+"
			    #'org-link--decode-compound s t t))


(defun org-link--decode-compound (hex)
  "Unhexify Unicode hex-chars HEX.
E.g. \"%C3%B6\" is the German o-Umlaut.  Note: this function also
decodes single byte encodings like \"%E1\" (a-acute) if not
followed by another \"%[A-F0-9]{2}\" group."
  (save-match-data
    (let* ((bytes (cdr (split-string hex "%")))
	   (ret "")
	   (eat 0)
	   (sum 0))
      (while bytes
	(let* ((val (string-to-number (pop bytes) 16))
	       (shift-xor
		(if (= 0 eat)
		    (cond
		     ((>= val 252) (cons 6 252))
		     ((>= val 248) (cons 5 248))
		     ((>= val 240) (cons 4 240))
		     ((>= val 224) (cons 3 224))
		     ((>= val 192) (cons 2 192))
		     (t (cons 0 0)))
		  (cons 6 128))))
	  (when (>= val 192) (setq eat (car shift-xor)))
	  (setq val (logxor val (cdr shift-xor)))
	  (setq sum (+ (ash sum (car shift-xor)) val))
	  (when (> eat 0) (setq eat (- eat 1)))
	  (cond
	   ((= 0 eat)			;multi byte
	    (setq ret (concat ret (char-to-string sum)))
	    (setq sum 0))
	   ((not bytes)			; single byte(s)
	    (setq ret (org-link--decode-single-byte-sequence hex))))))
      ret)))

(defun org-link--decode-single-byte-sequence (hex)
  "Unhexify hex-encoded single byte character sequence HEX."
  (mapconcat (lambda (byte)
	       (char-to-string (string-to-number byte 16)))
	     (cdr (split-string hex "%"))
	     ""))

(defun org-link-make-string (link &optional description)
  "Make a bracket link, consisting of LINK and DESCRIPTION.
LINK is escaped with backslashes for inclusion in buffer."
  (let* ((zero-width-space (string ?\x200B))
	 (description
	  (and (org-string-nw-p description)
	       ;; Description cannot contain two consecutive square
	       ;; brackets, or end with a square bracket.  To prevent
	       ;; this, insert a zero width space character between
	       ;; the brackets, or at the end of the description.
	       (replace-regexp-in-string
		"\\(]\\)\\(]\\)"
		(concat "\\1" zero-width-space "\\2")
		(replace-regexp-in-string "]\\'"
					  (concat "\\&" zero-width-space)
					  (org-trim description))))))
    (if (not (org-string-nw-p link))
        (or description
            (error "Empty link"))
      (format "[[%s]%s]"
	      (org-link-escape link)
	      (if description (format "[%s]" description) "")))))

(declare-function org-time-stamp-format "org-element-timestamp"
                  (&optional with-time inactive custom))
(defun org-link-store-props (&rest plist)
  "Store link properties PLIST.
The properties are pre-processed by extracting names, addresses
and dates."
  (let ((x (plist-get plist :from)))
    (when x
      (let ((adr (mail-extract-address-components x)))
	(setq plist (plist-put plist :fromname (car adr)))
	(setq plist (plist-put plist :fromaddress (nth 1 adr))))))
  (let ((x (plist-get plist :to)))
    (when x
      (let ((adr (mail-extract-address-components x)))
	(setq plist (plist-put plist :toname (car adr)))
	(setq plist (plist-put plist :toaddress (nth 1 adr))))))
  (let ((x (ignore-errors (date-to-time (plist-get plist :date)))))
    (when x
      (require 'org-element-timestamp)
      (setq plist (plist-put plist :date-timestamp
			     (format-time-string
			      (org-time-stamp-format t) x)))
      (setq plist (plist-put plist :date-timestamp-inactive
			     (format-time-string
			      (org-time-stamp-format t t) x)))))
  (let ((from (plist-get plist :from))
	(to (plist-get plist :to)))
    (when (and from to org-link-from-user-regexp)
      (setq plist
	    (plist-put plist :fromto
		       (if (string-match org-link-from-user-regexp from)
			   (concat "to %t")
			 (concat "from %f"))))))
  (setq org-store-link-plist plist))

(defun org-link-add-props (&rest plist)
  "Add these properties to the link property list PLIST."
  (let (key value)
    (while plist
      (setq key (pop plist) value (pop plist))
      (setq org-store-link-plist
	    (plist-put org-store-link-plist key value)))))

(provide 'ol-core)

;;; ol-core.el ends here
