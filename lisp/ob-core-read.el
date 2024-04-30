;;; ob-core-read.el --- Improting text/Org data to Elisp          -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2024 Free Software Foundation, Inc.

;; Authors: Eric Schulte
;;	Dan Davison
;; Keywords: literate programming, reproducible research
;; URL: https://orgmode.org

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

;; This library implements reading Org mode text and plain text data
;; as Elisp to be used as intermediate representation for be
;; transferred between code blocks.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element)
(require 'org-table-create)

;;; Reading plain text

(defun org-babel-read (cell &optional inhibit-lisp-eval)
  "Convert the string value of CELL to a number if appropriate.
Otherwise if CELL looks like Lisp (meaning it starts with a
\"(\", \"\\='\", \"\\=`\" or a \"[\") then read and evaluate it as
lisp, otherwise return it unmodified as a string.  Optional
argument INHIBIT-LISP-EVAL inhibits lisp evaluation for
situations in which is it not appropriate."
  (cond ((not (org-string-nw-p cell)) cell)
	((org-babel--string-to-number cell))
	((and (not inhibit-lisp-eval)
	      (or (memq (string-to-char cell) '(?\( ?' ?` ?\[))
		  (string= cell "*this*")))
         ;; FIXME: Arbitrary code evaluation.
	 (eval (read cell) t))
	((let (read-val)
           (and (string-match-p
                 (rx bos (0+ (any space ?\n))
                     ?\" (0+ anychar) ?\"
                     (0+ (any space ?\n)) eos)
                 cell)
                ;; CELL is a single string
                (with-temp-buffer
                  (insert cell)
                  (goto-char 1)
                  (when (setq read-val
                              (ignore-errors
                                (read (current-buffer))))
                    (skip-chars-forward "[:space:]")
                    (eobp)))
                read-val)))
	(t (org-no-properties cell))))

(defun org-babel--string-to-number (string)
  "If STRING represents a number return its value.
Otherwise return nil."
  (unless (or (string-match-p "\\s-" (org-trim string))
	      (not (string-match-p "^[0-9e.+ -]+$" string)))
    (let ((interned-string (ignore-errors (read string))))
      (when (numberp interned-string)
	interned-string))))

(defun org-babel-import-elisp-from-file (file-name &optional separator)
  "Read the results located at FILE-NAME into an elisp table.
If the table is trivial, then return it as a scalar.
SEPARATOR is passed to `org-table-convert-region', which see."
  (let ((result
	 (with-temp-buffer
	   (condition-case err
	       (progn
		 (insert-file-contents file-name)
		 (delete-file file-name)
		 (let ((pmax (point-max)))
		   ;; If the file was empty, don't bother trying to
		   ;; convert the table.
		   (when (> pmax 1)
		     (org-table-convert-region
                      (point-min) pmax
                      (or separator 'babel-auto))
		     (delq nil
			   (mapcar (lambda (row)
				     (and (not (eq row 'hline))
					  (mapcar #'org-babel-string-read row)))
				   (org-table-to-lisp))))))
	     (error
	      (display-warning 'org-babel
			       (format "Error reading results: %S" err)
			       :error)
	      nil)))))
    (pcase result
      (`((,scalar)) scalar)
      (`((,_ ,_ . ,_)) result)
      (`(,scalar) scalar)
      (_ result))))

(defun org-babel-string-read (cell)
  "Strip nested \"s from around CELL string.
When CELL is not a string, return CELL."
  (org-babel-read (or (and (stringp cell)
                           (string-match "^[[:space:]]*\"\\(.+\\)\"[[:space:]]*$" cell)
                           (match-string 1 cell))
                      cell) t))

(defun org-babel--script-escape-inner (str)
  (let (in-single in-double backslash out)
    (mapc
     (lambda (ch)
       (setq
	out
	(if backslash
	    (progn
	      (setq backslash nil)
	      (cond
	       ((and in-single (eq ch ?'))
		;; Escaped single quote inside single quoted string:
		;; emit just a single quote, since we've changed the
		;; outer quotes to double.
		(cons ch out))
	       ((eq ch ?\")
		;; Escaped double quote
		(if in-single
		    ;; This should be interpreted as backslash+quote,
		    ;; not an escape.  Emit a three backslashes
		    ;; followed by a quote (because one layer of
		    ;; quoting will be stripped by `org-babel-read').
		    (append (list ch ?\\ ?\\ ?\\) out)
		  ;; Otherwise we are in a double-quoted string.  Emit
		  ;; a single escaped quote
		  (append (list ch ?\\) out)))
	       ((eq ch ?\\)
		;; Escaped backslash: emit a single escaped backslash
		(append (list ?\\ ?\\) out))
	       ;; Other: emit a quoted backslash followed by whatever
	       ;; the character was (because one layer of quoting will
	       ;; be stripped by `org-babel-read').
	       (t (append (list ch ?\\ ?\\) out))))
	  (cl-case ch
	    (?\[ (if (or in-double in-single)
		     (cons ?\[ out)
		   (cons ?\( out)))
	    (?\] (if (or in-double in-single)
		     (cons ?\] out)
		   (cons ?\) out)))
	    (?\{ (if (or in-double in-single)
		     (cons ?\{ out)
		   (cons ?\( out)))
	    (?\} (if (or in-double in-single)
		     (cons ?\} out)
		   (cons ?\) out)))
	    (?, (if (or in-double in-single)
		    (cons ?, out) (cons ?\s out)))
	    (?\' (if in-double
		     (cons ?\' out)
		   (setq in-single (not in-single)) (cons ?\" out)))
	    (?\" (if in-single
		     (append (list ?\" ?\\) out)
		   (setq in-double (not in-double)) (cons ?\" out)))
	    (?\\ (unless (or in-single in-double)
		   (error "Can't handle backslash outside string in `org-babel-script-escape'"))
		 (setq backslash t)
		 out)
	    (t  (cons ch out))))))
     (string-to-list str))
    (when (or in-single in-double)
      (error "Unterminated string in `org-babel-script-escape'"))
    (apply #'string (reverse out))))

(defun org-babel-script-escape (str &optional force)
  "Safely convert tables into elisp lists."
  (unless (stringp str)
    (error "`org-babel-script-escape' expects a string"))
  (let ((escaped
	 (cond
	  ((and (>= (length str) 2)
		(or (and (string-equal "[" (substring str 0 1))
			 (string-equal "]" (substring str -1)))
		    (and (string-equal "{" (substring str 0 1))
			 (string-equal "}" (substring str -1)))
		    (and (string-equal "(" (substring str 0 1))
			 (string-equal ")" (substring str -1)))))

	   (concat "'" (org-babel--script-escape-inner str)))
	  ((or force
	       (and (> (length str) 2)
		    (or (and (string-equal "'" (substring str 0 1))
			     (string-equal "'" (substring str -1)))
			;; We need to pass double-quoted strings
			;; through the backslash-twiddling bits, even
			;; though we don't need to change their
			;; delimiters.
			(and (string-equal "\"" (substring str 0 1))
			     (string-equal "\"" (substring str -1))))))
	   (org-babel--script-escape-inner str))
	  (t str))))
    (condition-case nil (org-babel-read escaped) (error escaped))))

;;; Reading Org mode markup

(defun org-babel-read-element (element)
  "Read ELEMENT into emacs-lisp.
Return nil if ELEMENT cannot be read."
  (org-with-wide-buffer
   (goto-char (org-element-post-affiliated element))
   (pcase (org-element-type element)
     (`fixed-width
      (let ((v (org-trim (org-element-property :value element))))
	(or (org-babel--string-to-number v) v)))
     (`table (org-babel-read-table))
     (`plain-list (org-babel-read-list))
     ((or `example-block `src-block)
      (let ((v (org-element-property :value element)))
	(if (org-src-preserve-indentation-p element) v
	  (org-remove-indentation v))))
     (`export-block
      (org-remove-indentation (org-element-property :value element)))
     (`paragraph
      ;; Treat paragraphs containing a single link specially.
      (skip-chars-forward " \t")
      (if (and (looking-at org-link-bracket-re)
	       (save-excursion
		 (goto-char (match-end 0))
		 (skip-chars-forward " \r\t\n")
		 (<= (org-element-end element)
		    (point))))
	  (org-babel-read-link)
	(buffer-substring-no-properties
	 (org-element-contents-begin element)
	 (org-element-contents-end element))))
     ((or `center-block `quote-block `verse-block `special-block)
      (org-remove-indentation
       (buffer-substring-no-properties
	(org-element-contents-begin element)
	(org-element-contents-end element))))
     (_ nil))))

(defun org-babel-read-result ()
  "Read the result at point into emacs-lisp."
  (and (not (save-excursion
	    (forward-line 0)
	    (looking-at-p "[ \t]*$")))
       (org-babel-read-element (org-element-at-point))))

(defun org-babel-read-table ()
  "Read the table at point into emacs-lisp."
  (mapcar (lambda (row)
            (if (and (symbolp row) (equal row 'hline)) row
              (mapcar (lambda (el) (org-babel-read el 'inhibit-lisp-eval)) row)))
          (org-table-to-lisp)))

(defun org-babel-read-list ()
  "Read the list at point into emacs-lisp.

Return the list of strings representing top level items:

   (item1 item2 ...)

Only consider top level items.  See Info node
`(org)Environment of a Code Block'."
  (mapcar (lambda (el) (org-babel-read (car el) 'inhibit-lisp-eval))
	  (cdr (org-list-to-lisp))))

(defvar org-link-types-re)
(defun org-babel-read-link ()
  "Read the link at point into emacs-lisp.
If the path of the link is a file path it is expanded using
`expand-file-name'."
  (let* ((case-fold-search t)
         (raw (and (looking-at org-link-bracket-re)
                   (org-no-properties (match-string 1))))
         (type (and (string-match org-link-types-re raw)
                    (match-string 1 raw))))
    (cond
     ((not type) (expand-file-name raw))
     ((string= type "file")
      (and (string-match "file\\(.*\\):\\(.+\\)" raw)
           (expand-file-name (match-string 2 raw))))
     (t raw))))

(provide 'ob-core-read)

;;; ob-core-read.el ends here
