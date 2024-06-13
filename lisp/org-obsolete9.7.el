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

;;;; Prevent breakage of third-party packages that rely upon
;;;; outline.el functions working in Org mode.

(declare-function org-fold-show-entry "org-fold" (&optional hide-drawers))
(declare-function org-fold-show-children "org-fold" (&optional level))
(declare-function org-fold-hide-subtree "org-fold" ())
(declare-function org-back-to-heading "org-move" (&optional invisible-ok))
(declare-function org-fold-hide-sublevels "org-fold" (levels))
(declare-function org-fold-folded-p "org-fold" (&optional pos spec-or-alias))
(declare-function org-at-heading-p "org-element-context" (&optional invisible-not-ok))
(declare-function org-next-visible-heading "org-move" (arg))
(declare-function org-fold-region "org-fold" (from to flag &optional spec-or-alias))
;; Folding in outline-mode is not compatible with org-mode folding
;; anymore. Working around to avoid breakage of external packages
;; assuming the compatibility.
(define-advice outline-flag-region (:around (oldfun from to flag &rest extra) fix-for-org-fold)
  "Run `org-fold-region' when in org-mode."
  (if (derived-mode-p 'org-mode)
      (org-fold-region (max from (point-min)) (min to (point-max)) flag 'headline)
    ;; Apply EXTRA to avoid breakages if advised function definition
    ;; changes.
    (apply oldfun from to flag extra)))

(define-advice outline-next-visible-heading (:around (oldfun arg &rest extra) fix-for-org-fold)
  "Run `org-next-visible-heading' when in org-mode."
  (if (derived-mode-p 'org-mode)
      (org-next-visible-heading arg)
    ;; Apply EXTRA to avoid breakages if advised function definition
    ;; changes.
    (apply oldfun arg extra)))

(define-advice outline-back-to-heading (:around (oldfun &optional invisible-ok &rest extra) fix-for-org-fold)
  "Run `org-back-to-heading' when in org-mode."
  (if (derived-mode-p 'org-mode)
      (progn
        (forward-line 0)
        (or (org-at-heading-p (not invisible-ok))
            (let (found)
	      (save-excursion
	        (while (not found)
	          (or (re-search-backward (concat "^\\(?:" outline-regexp "\\)")
				          nil t)
                      (signal 'outline-before-first-heading nil))
	          (setq found (and (or invisible-ok (not (org-fold-folded-p)))
			           (point)))))
	      (goto-char found)
	      found)))
    ;; Apply EXTRA to avoid breakages if advised function definition
    ;; changes.
    (apply oldfun invisible-ok extra)))

(define-advice outline-on-heading-p (:around (oldfun &optional invisible-ok &rest extra) fix-for-org-fold)
  "Run `org-at-heading-p' when in org-mode."
  (if (derived-mode-p 'org-mode)
      (org-at-heading-p (not invisible-ok))
    ;; Apply EXTRA to avoid breakages if advised function definition
    ;; changes.
    (apply oldfun invisible-ok extra)))

(define-advice outline-hide-sublevels (:around (oldfun levels &rest extra) fix-for-org-fold)
  "Run `org-fold-hide-sublevels' when in org-mode."
  (if (derived-mode-p 'org-mode)
      (org-fold-hide-sublevels levels)
    ;; Apply EXTRA to avoid breakages if advised function definition
    ;; changes.
    (apply oldfun levels extra)))

(define-advice outline-toggle-children (:around (oldfun &rest extra) fix-for-org-fold)
  "Run `org-fold-hide-sublevels' when in org-mode."
  (if (derived-mode-p 'org-mode)
      (save-excursion
        (org-back-to-heading)
        (if (not (org-fold-folded-p (line-end-position)))
            (org-fold-hide-subtree)
          (org-fold-show-children)
          (org-fold-show-entry 'hide-drawers)))
    ;; Apply EXTRA to avoid breakages if advised function definition
    ;; changes.
    (apply oldfun extra)))

;;;; Groups

;; It was used only for two non-obsolete variables.
(defgroup org-keywords nil
  "Keywords in Org mode."
  :tag "Org Keywords"
  :group 'org)

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

(defvar org-ignore-region nil
  "Non-nil means temporarily disable the active region.")

(make-obsolete-variable
 'org-ignore-region
 "Temporarily bind `transient-mark-mode' or `mark-active' instead."
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

(defun org-refresh-property (tprop p &optional inherit)
  "Refresh the buffer text property TPROP from the drawer property P.

The refresh happens only for the current entry, or the whole
sub-tree if optional argument INHERIT is non-nil.

If point is before first headline, the function applies to the
part before the first headline.  In that particular case, when
optional argument INHERIT is non-nil, it refreshes properties for
the whole buffer."
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (let ((start (point))
	  (end (save-excursion
		 (cond ((and inherit (org-before-first-heading-p))
			(point-max))
		       (inherit
			(org-end-of-subtree t t))
		       ((outline-next-heading))
		       ((point-max))))))
      (with-silent-modifications
	(if (symbolp tprop)
	    ;; TPROP is a text property symbol.
	    (put-text-property start end tprop p)
	  ;; TPROP is an alist with (property . function) elements.
	  (pcase-dolist (`(,prop . ,f) tprop)
	    (put-text-property start end prop (funcall f p))))))))
(make-obsolete 'org-refresh-property "no longer used" "9.7")

(eval-when-compile (require 'org-macs))
(declare-function org-property-inherit-p "org-property" (property))
(declare-function org--property-global-or-keyword-value "org-property" (property literal-nil))
(declare-function org-entry-get "org-property" (epom property &optional inherit literal-nil))
(defun org-refresh-properties (dprop tprop)
  "Refresh buffer text properties.
DPROP is the drawer property and TPROP is either the
corresponding text property to set, or an alist with each element
being a text property (as a symbol) and a function to apply to
the value of the drawer property."
  (require 'org-property)
  (let* ((case-fold-search t)
	 (inhibit-read-only t)
	 (inherit? (org-property-inherit-p dprop))
	 (property-re (org-re-property (concat (regexp-quote dprop) "\\+?") t))
	 (global-or-keyword (and inherit?
				 (org--property-global-or-keyword-value dprop nil))))
    (with-silent-modifications
      (org-with-point-at 1
	;; Set global and keyword based values to the whole buffer.
	(when global-or-keyword
	  (put-text-property (point-min) (point-max) tprop global-or-keyword))
	;; Set values based on property-drawers throughout the document.
	(while (re-search-forward property-re nil t)
	  (when (org-at-property-p)
            (with-no-warnings
	      (org-refresh-property tprop (org-entry-get (point) dprop) inherit?)))
	  (outline-next-heading))))))
(make-obsolete 'org-refresh-properties "no longer used" "9.7")

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

(defvar org-effort-property) ; org-regexps
(defun org-refresh-effort-properties ()
  "Refresh effort properties."
  (require 'org-regexps)
  (with-no-warnings
    (org-refresh-properties
     org-effort-property
     '((effort . identity)
       (effort-minutes . org-duration-to-minutes)))))
(make-obsolete 'org-refresh-effort-properties "no longer used" "9.7")

(defvar org-cached-props nil)
(defvar org-use-property-inheritance)
(declare-function org-entry-get "org-property" (epom property &optional inherit literal-nil))
(declare-function org-entry-properties "org-property" (&optional epom which))
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

(declare-function outline-previous-heading "outline")
(declare-function outline-next-heading "outline")
(declare-function org-in-regexp "org-macs" (regexp &optional nlines visually))
(defun org-between-regexps-p (start-re end-re &optional lim-up lim-down)
  "Non-nil when point is between matches of START-RE and END-RE.

Also return a non-nil value when point is on one of the matches.

Optional arguments LIM-UP and LIM-DOWN bound the search; they are
buffer positions.  Default values are the positions of headlines
surrounding the point.

The functions returns a cons cell whose car (resp. cdr) is the
position before START-RE (resp. after END-RE)."
  (require 'outline)
  (require 'org-macs)
  (save-match-data
    (let ((pos (point))
	  (limit-up (or lim-up (save-excursion (outline-previous-heading))))
	  (limit-down (or lim-down (save-excursion (outline-next-heading))))
	  beg end)
      (save-excursion
	;; Point is on a block when on START-RE or if START-RE can be
	;; found before it...
	(and (or (org-in-regexp start-re)
		 (re-search-backward start-re limit-up t))
	     (setq beg (match-beginning 0))
	     ;; ... and END-RE after it...
	     (goto-char (match-end 0))
	     (re-search-forward end-re limit-down t)
	     (> (setq end (match-end 0)) pos)
	     ;; ... without another START-RE in-between.
	     (goto-char (match-beginning 0))
	     (not (re-search-backward start-re (1+ beg) t))
	     ;; Return value.
	     (cons beg end))))))
(make-obsolete 'org-between-regexps-p "no longer used" "9.7")

(defvar reftex-docstruct-symbol)
(defvar org--rds)
(defun org-reftex-citation ()
  "Use `reftex-citation' to insert a citation into the buffer.
This looks for a line like

#+BIBLIOGRAPHY: foo plain option:-d

and derives from it that foo.bib is the bibliography file relevant
for this document.  It then installs the necessary environment for RefTeX
to work in this buffer and calls `reftex-citation'  to insert a citation
into the buffer.

Export of such citations to both LaTeX and HTML is handled by the contributed
package ox-bibtex by Taru Karttunen."
  (interactive)
  (let ((reftex-docstruct-symbol 'org--rds)
	org--rds bib)
    (org-with-wide-buffer
     (let ((case-fold-search t)
	   (re "^[ \t]*#\\+BIBLIOGRAPHY:[ \t]+\\([^ \t\n]+\\)"))
       (if (not (save-excursion
		(or (re-search-forward re nil t)
		    (re-search-backward re nil t))))
	   (user-error "No bibliography defined in file")
	 (setq bib (concat (match-string 1) ".bib")
	       org--rds (list (list 'bib bib))))))
    (call-interactively 'reftex-citation)))
(make-obsolete 'org-reftex-citation "predates native citation support" "9.7")

(declare-function org-defkey "org-keys" (keymap key def))
(defvar org-mode-map)
(eval-after-load 'org-keys
  '(with-no-warnings (org-defkey org-mode-map (kbd "C-c C-x [") #'org-reftex-citation)))

(defun org-remove-keyword-keys (list)
  "Remove a pair of parenthesis at the end of each string in LIST."
  (mapcar (lambda (x)
	    (if (string-match "(.*)$" x)
		(substring x 0 (match-beginning 0))
	      x))
	  list))
(make-obsolete 'org-remove-keyword-keys "no longer used" "9.7")

(defvar org-odd-levels-only)
(defun org-tr-level (n)
  "Make N odd if required."
  (require 'org-element)
  (if org-odd-levels-only (1+ (/ n 2)) n))
(make-obsolete 'org-tr-level "no longer used" "9.7")

(defun org-region-active-p ()
  "Non-nil when the region active.
Unlike to `use-region-p', this function also checks
`org-ignore-region'."
  (and (not org-ignore-region) (use-region-p)))
(make-obsolete 'org-region-active-p "no longer used" "9.7")

(defun org-cursor-to-region-beginning ()
  (when (and (with-no-warnings (org-region-active-p))
             (> (point) (region-beginning)))
    (exchange-point-and-mark)))
(make-obsolete 'org-cursor-to-region-beginning "no longer used" "9.7")

(defun org-in-invisibility-spec-p (arg)
  "Is ARG a member of `buffer-invisibility-spec'?"
  (when (consp buffer-invisibility-spec)
    (member arg buffer-invisibility-spec)))
(make-obsolete 'org-in-invisibility-spec-p "no longer used" "9.7")

(defun org-export-get-parent-headline (blob)
  "Return BLOB parent headline or nil.
BLOB is the element or object being considered."
  (require 'org-element-ast)
  (declare-function org-element-lineage "org-element-ast" (datum &optional types with-self))
  (org-element-lineage blob 'headline))
(make-obsolete 'org-export-get-parent-headline "no longer used" "9.7")

(defun org-export-get-parent-table (object)
  "Return OBJECT parent table or nil.
OBJECT is either a `table-cell' or `table-element' type object."
  (org-element-lineage object 'table))
(make-obsolete 'org-export-get-parent-table "no longer used" "9.7")

(provide 'org-obsolete9.7)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-obsolete9.7.el ends here
