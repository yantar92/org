;;; org-obsolete9.7.el --- Obsolete Org mode functions and variables -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2024 Free Software Foundation, Inc.

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

;; This file contains obsolete Org mode code that will be removed in
;; future releases.

;;; Code:

;;;; Obsolete aliases

(define-obsolete-variable-alias 'org-export-ignored-local-variables
  'org-element-ignored-local-variables "Org 9.7")

(define-obsolete-function-alias 'org-habit-get-priority
  'org-habit-get-urgency "Org 9.7")

(define-obsolete-function-alias 'org--math-always-on
  'org--math-p "9.7")

(declare-function ob-clojure-eval-with-cmd "ob-clojure" (cmd expanded))
(define-obsolete-function-alias 'ob-clojure-eval-with-babashka
  #'ob-clojure-eval-with-cmd "9.7")

(define-obsolete-function-alias 'org-export-get-parent
  'org-element-parent "9.7")
(define-obsolete-function-alias 'org-export-get-parent-element
  'org-element-parent-element "9.7")

(define-obsolete-function-alias 'org-print-speed-command
  'org--print-speed-command "9.7"
  "Internal function.  Subject of unannounced changes.")

(define-obsolete-function-alias 'org-advertized-archive-subtree
  'org-archive-subtree "9.7")

;;;; Obsolete variables

(defconst org-latex-line-break-safe "\\\\[0pt]"
  "Linebreak protecting the following [...].

Without \"[0pt]\" it would be interpreted as an optional argument to
the \\\\.

This constant, for example, makes the below code not err:

\\begin{tabular}{c|c}
    [t] & s\\\\[0pt]
    [I] & A\\\\[0pt]
    [m] & kg
\\end{tabular}")
(make-obsolete 'org-latex-line-break-safe
               "should not be used - it is not safe in all the scenarios."
               "9.7")

(defcustom org-capture-bookmark t
  "When non-nil, add bookmark pointing at the last stored position when capturing."
  :group 'org-capture
  :version "24.3"
  :type 'boolean)
(make-obsolete-variable
 'org-capture-bookmark
 "use `org-bookmark-names-plist' instead."
 "9.7")

(defcustom org-todo-interpretation 'sequence
  "Controls how TODO keywords are interpreted.
This variable is in principle obsolete and is only used for
backward compatibility, if the interpretation of todo keywords is
not given already in `org-todo-keywords'.  See that variable for
more information."
  :group 'org-todo
  :group 'org-keywords
  :type '(choice (const sequence)
		 (const type)))
(make-obsolete-variable
 'org-todo-interpretation
 "set interpretation in `org-todo-keywords' instead."
 "9.7")

(defconst org-babel-python-mode 'python
  "Python mode for use in running python interactively.")

(make-obsolete-variable
 'org-babel-python-mode
 "Only the built-in Python mode is supported in ob-python now."
 "9.7")

;;;; Obsolete functions and macros

(defmacro org-no-popups (&rest body)
  "Suppress popup windows and evaluate BODY."
  `(let (pop-up-frames pop-up-windows)
     ,@body))
(make-obsolete 'org-no-popups "no longer used" "9.7")

(defun org-switch-to-buffer-other-window (&rest args)
  "Switch to buffer in a second window on the current frame.
In particular, do not allow pop-up frames.
Returns the newly created buffer."
  (let (pop-up-frames pop-up-windows)
    (apply #'switch-to-buffer-other-window args)))
(make-obsolete 'org-switch-to-buffer-other-window "no longer used" "9.7")

(defvar org-category)
(eval-when-compile (require 'org-macs)) ; `org-with-wide-buffer'
(declare-function org-element-type-p "org-element-ast")
(declare-function org-element-property "org-element-ast")
(declare-function org-element-at-point-no-context "org-element")
(declare-function org-re-property "org-element")
(declare-function org-at-property-p "org-element")
(declare-function org-before-first-heading-p "org-element")
(declare-function org-back-to-heading-or-point-min "org-move")
(declare-function org-end-of-subtree "org-move")
(defun org-refresh-category-properties ()
  "Refresh category text properties in the buffer."
  (require 'org-element-ast)
  (require 'org-element)
  (require 'org-move)
  (let ((case-fold-search t)
	(inhibit-read-only t)
	(default-category
	 (cond ((null org-category)
		(if buffer-file-name
		    (file-name-sans-extension
		     (file-name-nondirectory buffer-file-name))
		  "???"))
	       ((symbolp org-category) (symbol-name org-category))
	       (t org-category))))
    (let ((category (catch 'buffer-category
                      (org-with-wide-buffer
	               (goto-char (point-max))
	               (while (re-search-backward "^[ \t]*#\\+CATEGORY:" (point-min) t)
	                 (let ((element (org-element-at-point-no-context)))
	                   (when (org-element-type-p element 'keyword)
		             (throw 'buffer-category
		                    (org-element-property :value element))))))
	              default-category)))
      (with-silent-modifications
        (org-with-wide-buffer
         ;; Set buffer-wide property from keyword.  Search last #+CATEGORY
         ;; keyword.  If none is found, fall-back to `org-category' or
         ;; buffer file name, or set it by the document property drawer.
         (put-text-property (point-min) (point-max)
                            'org-category category)
         ;; Set categories from the document property drawer or
         ;; property drawers in the outline.  If category is found in
         ;; the property drawer for the whole buffer that value
         ;; overrides the keyword-based value set above.
         (goto-char (point-min))
         (let ((regexp (org-re-property "CATEGORY")))
           (while (re-search-forward regexp nil t)
             (let ((value (match-string-no-properties 3)))
               (when (org-at-property-p)
                 (put-text-property
                  (save-excursion (org-back-to-heading-or-point-min t))
                  (save-excursion (if (org-before-first-heading-p)
                                      (point-max)
                                    (org-end-of-subtree t t)))
                  'org-category
                  value))))))))))
(make-obsolete 'org-refresh-category-properties "no longer used" "9.7")

(declare-function org-refresh-properties "org")
(defvar org-effort-property) ; org-regexps
(defun org-refresh-effort-properties ()
  "Refresh effort properties."
  (require 'org-regexps)
  (org-refresh-properties
   org-effort-property
   '((effort . identity)
     (effort-minutes . org-duration-to-minutes))))
(make-obsolete 'org-refresh-effort-properties "no longer used" "9.7")

(defvar org-cached-props nil)
(defvar org-use-property-inheritance)
(declare-function org-entry-get "org" (epom property &optional inherit literal-nil))
(declare-function org-entry-properties "org" (&optional epom which))
(defun org-cached-entry-get (pom property)
  (if (or (eq t org-use-property-inheritance)
	  (and (stringp org-use-property-inheritance)
	       (let ((case-fold-search t))
		 (string-match-p org-use-property-inheritance property)))
	  (and (listp org-use-property-inheritance)
	       (member-ignore-case property org-use-property-inheritance)))
      ;; Caching is not possible, check it directly.
      (org-entry-get pom property 'inherit)
    ;; Get all properties, so we can do complicated checks easily.
    (cdr (assoc-string property
		       (or org-cached-props
			   (setq org-cached-props (org-entry-properties pom)))
		       t))))

(make-obsolete 'org-cached-entry-get
               "Performs badly.  Instead use `org-entry-get' with the argument INHERIT set to `selective'"
               "9.7")

(provide 'org-obsolete9.7)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-obsolete9.7.el ends here
