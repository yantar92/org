;;; org-pcomplete.el --- In-buffer Completion Code -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2024 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;;         John Wiegley <johnw at gnu dot org>
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

;;; Commentary:

;; This library implements completion support in Org mode buffers.

;;; Code:

;;;; Require other packages

(require 'org-macs)
(org-assert-version)

(require 'pcomplete)
(require 'org-mode)
(require 'ox-backend)
(require 'org-move)

(defconst org-options-keywords
  '("ARCHIVE:" "AUTHOR:" "BIBLIOGRAPHY:" "BIND:" "CATEGORY:" "CITE_EXPORT:"
    "COLUMNS:" "CREATOR:" "DATE:" "DESCRIPTION:" "DRAWERS:" "EMAIL:"
    "EXCLUDE_TAGS:" "FILETAGS:" "INCLUDE:" "INDEX:" "KEYWORDS:" "LANGUAGE:"
    "MACRO:" "OPTIONS:" "PROPERTY:" "PRINT_BIBLIOGRAPHY:" "PRIORITIES:"
    "SELECT_TAGS:" "SEQ_TODO:" "SETUPFILE:" "STARTUP:" "TAGS:" "TITLE:" "TODO:"
    "TYP_TODO:" "SELECT_TAGS:" "EXCLUDE_TAGS:" "EXPORT_FILE_NAME:"))


;;; Internal Functions

(defun org-thing-at-point ()
  "Examine the thing at point and let the caller know what it is.
The return value is a string naming the thing at point."
  (let ((line-to-here (org-current-line-string t))
	(case-fold-search t))
    (cond
     ;; Parameters on a clock table opening line.
     ((org-match-line "[ \t]*#\\+BEGIN: clocktable[ \t]")
      (cons "block-option" "clocktable"))
     ;; Flags and parameters on a source block opening line.
     ((org-match-line "[ \t]*#\\+BEGIN_SRC[ \t]")
      (cons "block-option" "src"))
     ;; Value for a known keyword.
     ((org-match-line "[ \t]*#\\+\\(\\S-+\\):")
      (cons "file-option" (match-string-no-properties 1)))
     ;; Keyword name.
     ((and (org-match-line "[ \t]*#\\+[a-zA-Z_]*$")
	   (looking-at-p "[ \t]*$"))
      (cons "file-option" nil))
     ;; Link abbreviation.
     ((save-excursion
	(skip-chars-backward "-A-Za-z0-9_")
	(and (eq ?\[ (char-before))
	     (eq ?\[ (char-before (1- (point))))))
      (cons "link" nil))
     ;; Entities.  Some of them accept numbers, but only at their end.
     ;; So, we first skip numbers, then letters.
     ((eq ?\\ (save-excursion
		(skip-chars-backward "0-9")
		(skip-chars-backward "a-zA-Z")
		(char-before)))
      (cons "tex" nil))
     ;; Tags on a headline.
     ((and (org-match-line
	    (format "\\*+ \\(?:.+? \\)?\\(:\\)\\(\\(?::\\|%s\\)+\\)?[ \t]*$"
		    org-tag-re))
	   (or (org-point-in-group (point) 2)
	       (= (point) (match-end 1))))
      (cons "tag" nil))
     ;; TODO keywords on an empty headline.
     ((and (string-match "^\\*+ +\\S-*$" line-to-here)
	   (looking-at-p "[ \t]*$"))
      (cons "todo" nil))
     ;; Heading after a star for search strings or links.
     ((save-excursion
	(skip-chars-backward "^*" (line-beginning-position))
	(and (eq ?* (char-before))
	     (eq (char-before (1- (point))) '?\[)
	     (eq (char-before (- (point) 2)) '?\[)))
      (cons "searchhead" nil))
     ;; Property or drawer name, depending on point.  If point is at
     ;; a valid location for a node property, offer completion on all
     ;; node properties in the buffer. Otherwise, offer completion on
     ;; all drawer names, including "PROPERTIES".
     ((and (string-match "^[ \t]*:\\S-*$" line-to-here)
	   (looking-at-p "[ \t]*$"))
      (let ((origin (line-beginning-position)))
	(if (org-before-first-heading-p) (cons "drawer" nil)
	  (save-excursion
	    (org-end-of-meta-data)
	    (if (or (= origin (point))
		    (not (org-match-line "[ \t]*:PROPERTIES:[ \t]*$")))
		(cons "drawer" nil)
	      (while (org-match-line org-property-re)
		(forward-line))
	      (if (= origin (point)) (cons "prop" nil)
		(cons "drawer" nil)))))))
     (t nil))))

(defun org-pcomplete-case-double (list)
  "Return list with both upcase and downcase version of all strings in LIST."
  (let (e res)
    (while (setq e (pop list))
      (setq res (cons (downcase e) (cons (upcase e) res))))
    (nreverse res)))


;;; Completion API

;;;###autoload
(defun org-command-at-point ()
  "Return the qualified name of the Org completion entity at point.
When completing for #+STARTUP, for example, this function returns
\"file-option/startup\"."
  (let ((thing (org-thing-at-point)))
    (cond
     ((string= "file-option" (car thing))
      (concat (car thing)
	      (and (cdr thing) (concat "/" (downcase (cdr thing))))))
     ((string= "block-option" (car thing))
      (concat (car thing) "/" (downcase (cdr thing))))
     (t (car thing)))))

;;;###autoload
(defun org-parse-arguments ()
  "Parse whitespace separated arguments in the current region."
  (if (equal (cons "searchhead" nil) (org-thing-at-point))
      ;; [[* foo<point> bar link::search option.
      ;; Arguments are not simply space-separated.
      (save-excursion
        (let ((origin (point)))
          (skip-chars-backward "^*" (line-beginning-position))
          (cons (list (buffer-substring-no-properties (point) origin))
                (list (point)))))
    (let ((begin (line-beginning-position))
	  (end (line-end-position))
	  begins args)
      (save-restriction
        (narrow-to-region begin end)
        (save-excursion
	  (goto-char (point-min))
	  (while (not (eobp))
	    (skip-chars-forward " \t\n[")
	    (setq begins (cons (point) begins))
	    (skip-chars-forward "^ \t\n[")
	    (setq args (cons (buffer-substring-no-properties
			      (car begins) (point))
			     args)))
	  (cons (reverse args) (reverse begins)))))))

(defun org-pcomplete-initial ()
  "Call the right completion function for first argument completions."
  (ignore
   (funcall (or (pcomplete-find-completion-function
		 (car (org-thing-at-point)))
		pcomplete-default-completion-function))))


;;; Completion functions

(defun org-get-export-keywords ()
  "Return a list of all currently understood export keywords.
Export keywords include options, block names, attributes and
keywords relative to each registered export backend."
  (let (keywords)
    (dolist (backend
	     (bound-and-true-p org-export-registered-backends)
	     (delq nil keywords))
      ;; Backend name (for keywords, like #+LATEX:)
      (push (upcase (symbol-name (org-export-backend-name backend))) keywords)
      (dolist (option-entry (org-export-backend-options backend))
	;; Backend options.
	(push (nth 1 option-entry) keywords)))))

(defun pcomplete/org-mode/file-option ()
  "Complete against all valid file options."
  (require 'org-element)
  (pcomplete-here
   (org-pcomplete-case-double
    (append (mapcar (lambda (keyword) (concat keyword " "))
		    org-options-keywords)
	    (mapcar (lambda (keyword) (concat keyword ": "))
		    org-element-affiliated-keywords)
	    (let (block-names)
	      (dolist (name
		       '("CENTER" "COMMENT" "EXAMPLE" "EXPORT" "QUOTE" "SRC"
			 "VERSE")
		       block-names)
		(push (format "END_%s" name) block-names)
		(push (concat "BEGIN_"
			      name
			      ;; Since language is compulsory in
			      ;; export blocks source blocks, add
			      ;; a space.
			      (and (member name '("EXPORT" "SRC")) " "))
		      block-names)
		(push (format "ATTR_%s: " name) block-names)))
	    (mapcar (lambda (keyword) (concat keyword ": "))
		    (org-get-export-keywords))))
   (substring pcomplete-stub 2)))

(defun pcomplete/org-mode/file-option/author ()
  "Complete arguments for the #+AUTHOR file option."
  (pcomplete-here (list user-full-name)))

(declare-function org-time-stamp-format "org-element-timestamp" (&optional with-time inactive custom))
(defun pcomplete/org-mode/file-option/date ()
  "Complete arguments for the #+DATE file option."
  (require 'org-element-timestamp)
  (pcomplete-here (list (format-time-string (org-time-stamp-format)))))

(defun pcomplete/org-mode/file-option/email ()
  "Complete arguments for the #+EMAIL file option."
  (pcomplete-here (list user-mail-address)))

(defun pcomplete/org-mode/file-option/exclude_tags ()
  "Complete arguments for the #+EXCLUDE_TAGS file option."
  (require 'ox)
  (defvar org-export-exclude-tags)
  (pcomplete-here
   (and org-export-exclude-tags
	(list (mapconcat #'identity org-export-exclude-tags " ")))))

(defun pcomplete/org-mode/file-option/filetags ()
  "Complete arguments for the #+FILETAGS file option."
  (pcomplete-here
   (let ((file-tags (org-element-property :tags (org-element-org-data))))
     (and file-tags (mapconcat #'identity file-tags " ")))))

(defun pcomplete/org-mode/file-option/language ()
  "Complete arguments for the #+LANGUAGE file option."
  (require 'ox)
  (defvar org-export-default-language)
  (pcomplete-here
   (pcomplete-uniquify-list
    (list org-export-default-language "en"))))

(defun pcomplete/org-mode/file-option/priorities ()
  "Complete arguments for the #+PRIORITIES file option."
  (pcomplete-here (list (format "%c %c %c"
				org-priority-highest
				org-priority-lowest
				org-priority-default))))

(defun pcomplete/org-mode/file-option/select_tags ()
  "Complete arguments for the #+SELECT_TAGS file option."
  (require 'ox)
  (defvar org-export-select-tags)
  (pcomplete-here
   (and org-export-select-tags
	(list (mapconcat #'identity org-export-select-tags " ")))))

(defun pcomplete/org-mode/file-option/startup ()
  "Complete arguments for the #+STARTUP file option."
  (while (pcomplete-here
	  (let ((opts (pcomplete-uniquify-list
		       (mapcar #'car org-startup-options))))
	    ;; Some options are mutually exclusive, and shouldn't be completed
	    ;; against if certain other options have already been seen.
	    (dolist (arg pcomplete-args)
	      (cond
	       ((string= arg "hidestars")
		(setq opts (delete "showstars" opts)))))
	    opts))))

(declare-function org-tag-alist-to-string "org-tags" (alist &optional skip-key))
(defun pcomplete/org-mode/file-option/tags ()
  "Complete arguments for the #+TAGS file option."
  (require 'org-tags)
  (declare-function org-local-tags-completion-table "org-tags" ())
  (pcomplete-here
   (list (org-tag-alist-to-string (org-local-tags-completion-table)))))

(defun pcomplete/org-mode/file-option/title ()
  "Complete arguments for the #+TITLE file option."
  (pcomplete-here
   (let ((visited-file (buffer-file-name (buffer-base-buffer))))
     (list (or (and visited-file
		    (file-name-sans-extension
		     (file-name-nondirectory visited-file)))
	       (buffer-name (buffer-base-buffer)))))))


(defun pcomplete/org-mode/file-option/options ()
  "Complete arguments for the #+OPTIONS file option."
  (while (pcomplete-here
	  (pcomplete-uniquify-list
	   (append
	    ;; Hard-coded OPTION items always available.
	    '("H:" "\\n:" "num:" "timestamp:" "arch:" "author:" "c:"
	      "creator:" "date:" "d:" "email:" "*:" "e:" "::" "f:"
	      "inline:" "tex:" "p:" "pri:" "':" "-:" "stat:" "^:" "toc:"
	      "|:" "tags:" "tasks:" "<:" "todo:")
	    ;; OPTION items from registered backends.
	    (let (items)
	      (dolist (backend (bound-and-true-p
				org-export-registered-backends))
		(dolist (option (org-export-backend-options backend))
		  (let ((item (nth 2 option)))
		    (when item (push (concat item ":") items)))))
	      items))))))

(defun pcomplete/org-mode/file-option/infojs_opt ()
  "Complete arguments for the #+INFOJS_OPT file option."
  (while (pcomplete-here
	  (pcomplete-uniquify-list
	   (mapcar (lambda (item) (format "%s:" (car item)))
		   (bound-and-true-p org-html-infojs-opts-table))))))

(defun pcomplete/org-mode/file-option/bind ()
  "Complete arguments for the #+BIND file option, which are variable names."
  (let (vars)
    (mapatoms
     (lambda (a) (when (boundp a) (setq vars (cons (symbol-name a) vars)))))
    (pcomplete-here vars)))

(defun pcomplete/org-mode/link ()
  "Complete against defined #+LINK patterns."
  (pcomplete-here
   (pcomplete-uniquify-list
    (copy-sequence
     (mapcar (lambda (e) (concat (car e) ":"))
	     (append (org-element-property :link-abbrevs (org-element-org-data))
		     org-link-abbrev-alist))))))

(defun pcomplete/org-mode/tex ()
  "Complete against TeX-style HTML entity names."
  (require 'org-entities)
  (while (pcomplete-here
	  (pcomplete-uniquify-list
	   (remove nil (mapcar #'car-safe org-entities)))
	  (substring pcomplete-stub 1))))

(defun pcomplete/org-mode/todo ()
  "Complete against known TODO keywords."
  (pcomplete-here (pcomplete-uniquify-list (copy-sequence (org-element-all-todo-keywords)))))

(declare-function org-link-heading-search-string "ol" (&optional string))
(defun pcomplete/org-mode/searchhead ()
  "Complete against all headings.
This needs more work, to handle headings with lots of spaces in them."
  (require 'ol)
  (while (pcomplete-here
	  (save-excursion
	    (goto-char (point-min))
	    (let (tbl)
	      (while (re-search-forward org-outline-regexp nil t)
		;; Remove the leading asterisk from
		;; `org-link-heading-search-string' result.
		(push (substring (org-link-heading-search-string) 1) tbl))
	      (pcomplete-uniquify-list tbl))))))

(declare-function org-get-buffer-tags "org-tags-core" ())
(declare-function org-get-tags "org-tags-core" (&optional epom local))
(defun pcomplete/org-mode/tag ()
  "Complete a tag name.  Omit tags already set."
  (require 'org-tags-core)
  (while (pcomplete-here
	  (mapcar (lambda (x) (concat x ":"))
		  (let ((lst (pcomplete-uniquify-list
			      (or (remq
				   nil
				   (mapcar (lambda (x) (org-string-nw-p (car x)))
					   (org-local-tags-completion-table)))
				  (mapcar #'car (org-get-buffer-tags))))))
		    (dolist (tag (org-get-tags nil t))
		      (setq lst (delete tag lst)))
		    lst))
	  (and (string-match ".*:" pcomplete-stub)
	       (substring pcomplete-stub (match-end 0)))
	  t)))

(defun pcomplete/org-mode/drawer ()
  "Complete a drawer name, including \"PROPERTIES\"."
  (pcomplete-here
   (org-pcomplete-case-double
    (mapcar (lambda (x) (concat x ":"))
	    (let ((names (list "PROPERTIES")))
	      (save-excursion
		(goto-char (point-min))
		(while (re-search-forward org-drawer-regexp nil t)
		  (let ((drawer (org-element-at-point)))
		    (when (org-element-type-p drawer '(drawer property-drawer))
		      (push (org-element-property :drawer-name drawer) names)
		      (goto-char (org-element-end drawer))))))
	      (pcomplete-uniquify-list names))))
   (substring pcomplete-stub 1)))	;remove initial colon

(declare-function org-buffer-property-keys "org-property" (&optional specials defaults columns))
(declare-function org-entry-properties "org-property" (&optional epom which))
(defun pcomplete/org-mode/prop ()
  "Complete a property name.  Omit properties already set."
  (require 'org-property)
  (pcomplete-here
   (org-pcomplete-case-double
    (mapcar (lambda (x)
	      (concat x ": "))
	    (let ((lst (pcomplete-uniquify-list
			(copy-sequence (org-buffer-property-keys nil t t)))))
	      (dolist (prop (org-entry-properties))
		(setq lst (delete (car prop) lst)))
	      lst)))
   (substring pcomplete-stub 1)))

(declare-function org-babel-get-src-block-info "ob-core" (&optional no-eval datum))
(declare-function org-babel-combine-header-arg-lists "ob-core" (original &rest others))
(defun pcomplete/org-mode/block-option/src ()
  "Complete the arguments of a source block.
Complete a language in the first field, the header arguments and
switches."
  (pcomplete-here
   (mapcar
    (lambda(x) (symbol-name (nth 3 x)))
    (cdr (car (cdr (memq :key-type (plist-get
				    (symbol-plist
				     'org-babel-load-languages)
				    'custom-type)))))))
  (require 'ob-core)
  (defvar org-babel-combine-header-arg-lists)
  (defvar org-babel-common-header-args-w-values)
  (let* ((info (org-babel-get-src-block-info 'no-eval))
	 (lang (car info))
	 (lang-headers (intern (concat "org-babel-header-args:" lang)))
	 (headers (org-babel-combine-header-arg-lists
		   org-babel-common-header-args-w-values
		   (and (boundp lang-headers) (eval lang-headers t)))))
    (while (pcomplete-here
	    (append (mapcar
		     (lambda (arg) (format ":%s" (symbol-name (car arg))))
		     headers)
		    '("-n" "-r" "-l"))))))

(defun pcomplete/org-mode/block-option/clocktable ()
  "Complete keywords in a clocktable line."
  (while (pcomplete-here '(":maxlevel" ":scope" ":lang"
			   ":tstart" ":tend" ":block" ":step"
			   ":stepskip0" ":fileskip0"
			   ":emphasize" ":link" ":narrow" ":indent"
			   ":hidefiles" ":tcolumns" ":level" ":compact"
			   ":timestamp" ":formula" ":formatter"
			   ":wstart" ":mstart"))))


;;; Finish up

(provide 'org-pcomplete)

;;; org-pcomplete.el ends here
