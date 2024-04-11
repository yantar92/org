;;; org-map.el --- Org mapping API                      -*- lexical-binding: t; -*-

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

;; This library implements Org mapping API to act upon multiple Org
;; elements.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-macs)
(require 'org-move)
(require 'org-element)
(require 'org-tags)
(require 'org-mode)
(require 'org-agenda-files)
(require 'org-agenda-search)

(defvar org-not-done-regexp)
(defvar org-todo-regexp)
(defvar org-complex-heading-regexp)
(defvar org-todo-keywords-1)
(declare-function org-cycle-overview "org-cycle")
(declare-function org-remove-occur-highlights "org")
(declare-function org-entry-get-with-inheritance "org-property")
(declare-function org-agenda-entry-get-agenda-timestamp "org-agenda")
(declare-function org-agenda-skip "org-agenda")
(defvar org-agenda-tags-todo-honor-ignore-options)
(defvar org-highlight-sparse-tree-matches)
(declare-function org-get-heading "org")
(declare-function org-highlight-new-match "org")
(declare-function org-entry-get "org-property")
(declare-function org-duration-to-minutes "org-duration")
(declare-function org-agenda-format-item "org-agenda")
(declare-function org-get-priority "org-priority")
(declare-function org-agenda-new-marker "org-agenda")
(defvar org-sparse-tree-open-archived-trees)
(defvar org-agenda-skip-archived-trees)
(defvar org-todo-keywords-for-agenda)
(defvar org-done-keywords-for-agenda)
(defvar org-todo-keyword-alist-for-agenda)
(defvar org-tag-alist-for-agenda)
(declare-function org-narrow-to-subtree "org")
(declare-function org-agenda-prepare-buffers "org-agenda")
(declare-function org-add-archive-files "org-archive")

(defun org-map-tree (fun)
  "Call FUN for every heading underneath the current one."
  (org-back-to-heading t)
  (let ((level (funcall outline-level)))
    (save-excursion
      (funcall fun)
      (while (and (progn
		    (outline-next-heading)
		    (> (funcall outline-level) level))
		  (not (eobp)))
	(funcall fun)))))

(defun org-map-region (fun beg end)
  "Call FUN for every heading between BEG and END."
  (let ((org-ignore-region t))
    (save-excursion
      (setq end (copy-marker end))
      (goto-char beg)
      (when (and (re-search-forward org-outline-regexp-bol nil t)
		 (< (point) end))
	(funcall fun))
      (while (and (progn
		    (outline-next-heading)
		    (< (point) end))
		  (not (eobp)))
	(funcall fun)))))

(defun org-block-map (function &optional start end)
  "Call FUNCTION at the head of all source blocks in the current buffer.
Optional arguments START and END can be used to limit the range."
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end) (re-search-forward "^[ \t]*#\\+begin" end t))
	(save-excursion
	  (save-match-data
            (goto-char (match-beginning 0))
            (when (org-at-block-p)
              (funcall function))))))))

(defvar org-agenda-archives-mode)

(defvar org--matcher-tags-todo-only nil)

(defvar org-cached-props nil)
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

(defun org-make-tags-matcher (match &optional only-local-tags)
  "Create the TAGS/TODO matcher form for the selection string MATCH.

Returns a cons of the selection string MATCH and a function
implementing the matcher.

The matcher is to be called at an Org entry, with point on the
headline, and returns non-nil if the entry matches the selection
string MATCH.  It must be called with three arguments: the TODO
keyword at the entry (or nil if none), the list of all tags at
the entry including inherited ones and the reduced level of the
headline.  Additionally, the category of the entry, if any, must
be specified as the text property `org-category' on the headline.

This function sets the variable `org--matcher-tags-todo-only' to
a non-nil value if the matcher restricts matching to TODO
entries, otherwise it is not touched.

When ONLY-LOCAL-TAGS is non-nil, ignore the global tag completion
table, only get buffer tags.

See also `org-scan-tags'."
  (unless match
    ;; Get a new match request, with completion against the global
    ;; tags table and the local tags in current buffer.
    (let ((org-last-tags-completion-table
	   (org--tag-add-to-alist
            (when (derived-mode-p 'org-mode)
	      (org-get-buffer-tags))
	    (unless only-local-tags
	      (org-global-tags-completion-table)))))
      (setq match
	    (completing-read
	     "Match: "
	     'org-tags-completion-function nil nil nil 'org-tags-history))))

  (let* ((match0 match)
         (opre "[<=>]=?\\|[!/]=\\|<>")
         (re (concat
              "^"
              ;; implicit AND operator (OR is done by global splitting)
              "&?"
              ;; exclusion and inclusion (the latter being implicit)
              "\\(?1:[-+:]\\)?"
              ;; query term
              "\\(?2:"
              ;; tag regexp match
              "{[^}]+}\\|"
              ;; property match.  Try to keep this subre generic
              ;; and rather handle special properties like LEVEL
              ;; and CATEGORY further below.  This ensures that
              ;; the same quoting mechanics can be used for all
              ;; property names.
              "\\(?:"
              ;; property name [1]
              "\\(?5:\\(?:[[:alnum:]_]+\\|\\\\[^[:space:]]\\)+\\)"
              ;; operator, optionally starred
              "\\(?6:" opre "\\)\\(?7:\\*\\)?"
              ;; operand (regexp, double-quoted string,
              ;; number)
              "\\(?8:"
              "{[^}]+}\\|"
              "\"[^\"]*\"\\|"
              "-?[.0-9]+\\(?:[eE][-+]?[0-9]+\\)?"
              "\\)"
              "\\)\\|"
              ;; exact tag match
              org-tag-re
              "\\)"))
         (start 0)
         tagsmatch todomatch tagsmatcher todomatcher)

    ;; [1] The history of this particular subre:
    ;; - \\([[:alnum:]_]+\\) [pre-19b0e03]
    ;;   Does not allow for minus characters in property names.
    ;; - "\\(\\(?:[[:alnum:]_]+\\(?:\\\\-\\)*\\)+\\)" [19b0e03]
    ;;   Incomplete fix of above issue, still resulting in, e.g.,
    ;;   https://orgmode.org/list/87jzv67k3p.fsf@localhost.
    ;; - "\\(?5:[[:alnum:]_-]+\\)" [f689eb4]
    ;;   Allows for unquoted minus characters in property names, but
    ;;   conflicts with searches like -TAG-PROP="VALUE".  See
    ;;   https://orgmode.org/list/87h6oq2nu1.fsf@gmail.com.
    ;; - current subre
    ;;   Like second solution, but with proper unquoting and allowing
    ;;   for all possible characters in property names to be quoted.

    ;; Expand group tags.
    (setq match (org-tags-expand match))

    ;; Check if there is a TODO part of this match, which would be the
    ;; part after a "/".  To make sure that this slash is not part of
    ;; a property value to be matched against, we also check that
    ;; there is no / after that slash.  First, find the last slash.
    (let ((s 0))
      (while (string-match "/+" match s)
	(setq start (match-beginning 0))
	(setq s (match-end 0))))
    (if (and (string-match "/+" match start)
	     (not (string-match-p "\"" match start)))
	;; Match contains also a TODO-matching request.
	(progn
	  (setq tagsmatch (substring match 0 (match-beginning 0)))
	  (setq todomatch (substring match (match-end 0)))
	  (when (string-prefix-p "!" todomatch)
	    (setq org--matcher-tags-todo-only t)
	    (setq todomatch (substring todomatch 1)))
	  (when (string-match "\\`\\s-*\\'" todomatch)
	    (setq todomatch nil)))
      ;; Only matching tags.
      (setq tagsmatch match)
      (setq todomatch nil))

    ;; Make the tags matcher.
    (when (org-string-nw-p tagsmatch)
      (let ((orlist nil)
	    (orterms (org-split-string tagsmatch "|"))
	    term)
	(while (setq term (pop orterms))
	  (while (and (equal (substring term -1) "\\") orterms)
	    (setq term (concat term "|" (pop orterms)))) ;repair bad split.
	  (while (string-match re term)
	    (let* ((rest (substring term (match-end 0)))
		   (minus (and (match-end 1)
			       (equal (match-string 1 term) "-")))
		   ;; Bind the whole query term to `tag' and use that
		   ;; variable for a tag regexp match in [2] or as an
		   ;; exact tag match in [3].
		   (tag (match-string 2 term))
		   (regexp (eq (string-to-char tag) ?{))
		   (propp (match-end 5))
		   (mm
		    (cond
		     (regexp			; [2]
                      `(with-syntax-table org-mode-tags-syntax-table
                         (org-match-any-p ,(substring tag 1 -1) tags-list)))
		     (propp
		      (let* (;; Determine property name.
                             (pn (upcase
                                  (save-match-data
                                    (replace-regexp-in-string
                                     "\\\\\\(.\\)" "\\1"
                                     (match-string 5 term)
                                     t nil))))
                             ;; Convert property name to an Elisp
			     ;; accessor for that property (aka. as
			     ;; getter value).  Symbols LEVEL and TODO
			     ;; referenced below get bound by the
			     ;; matcher that this function returns.
			     (gv (pcase pn
				   ("LEVEL"
                                    '(number-to-string level))
				   ("CATEGORY"
				    '(org-get-category (point)))
				   ("TODO" 'todo)
				   (p `(org-entry-get (point) ,p 'selective))))
			     ;; Determine operand (aka. property
			     ;; value).
			     (pv (match-string 8 term))
			     ;; Determine type of operand.  Note that
			     ;; these are not exclusive: Any TIMEP is
			     ;; also STRP.
			     (regexp (eq (string-to-char pv) ?{))
			     (strp (eq (string-to-char pv) ?\"))
			     (timep (string-match-p "^\"[[<]\\(?:[0-9]+\\|now\\|today\\|tomorrow\\|[+-][0-9]+[dmwy]\\).*[]>]\"$" pv))
			     ;; Massage operand.  TIMEP must come
			     ;; before STRP.
			     (pv (cond (regexp (substring pv 1 -1))
				       (timep  (org-matcher-time
						(substring pv 1 -1)))
				       (strp   (substring pv 1 -1))
				       (t      pv)))
			     ;; Convert operator to Elisp.
			     (po (org-op-to-function (match-string 6 term)
						     (if timep 'time strp)))
			     ;; Convert whole property term to Elisp.
			     (pt (cond ((and regexp (eq po '/=))
					`(not (string-match ,pv (or ,gv ""))))
				       (regexp `(string-match ,pv (or ,gv "")))
				       (strp `(,po (or ,gv "") ,pv))
				       (t
					`(,po
					  (string-to-number (or ,gv ""))
					  ,(string-to-number pv)))))
			     ;; Respect the star after the operand.
			     (pt (if (match-end 7) `(and ,gv ,pt) pt)))
			pt))
		     (t `(member ,tag tags-list))))) ; [3]
	      (push (if minus `(not ,mm) mm) tagsmatcher)
	      (setq term rest)))
	  (push `(and ,@tagsmatcher) orlist)
	  (setq tagsmatcher nil))
	(setq tagsmatcher `(or ,@orlist))))

    ;; Make the TODO matcher.
    (when (org-string-nw-p todomatch)
      (let ((orlist nil))
	(dolist (term (org-split-string todomatch "|"))
	  (while (string-match re term)
	    (let* ((minus (and (match-end 1)
			       (equal (match-string 1 term) "-")))
		   (kwd (match-string 2 term))
		   (regexp (eq (string-to-char kwd) ?{))
		   (mm (if regexp `(string-match ,(substring kwd 1 -1) todo)
			 `(equal todo ,kwd))))
	      (push (if minus `(not ,mm) mm) todomatcher))
	    (setq term (substring term (match-end 0))))
	  (push (if (> (length todomatcher) 1)
		    (cons 'and todomatcher)
		  (car todomatcher))
		orlist)
	  (setq todomatcher nil))
	(setq todomatcher (cons 'or orlist))))

    ;; Return the string and function of the matcher.  If no
    ;; tags-specific or todo-specific matcher exists, match
    ;; everything.
    (let ((matcher (if (and tagsmatcher todomatcher)
		       `(and ,tagsmatcher ,todomatcher)
		     (or tagsmatcher todomatcher t))))
      (when org--matcher-tags-todo-only
	(setq matcher `(and (member todo org-not-done-keywords) ,matcher)))
      (cons match0
            (byte-compile
             `(lambda (todo tags-list level)
                ;; Pacify byte-compiler.
                (ignore todo) (ignore tags-list) (ignore level)
                ,matcher))))))

(defun org-op-to-function (op &optional stringp)
  "Turn an operator into the appropriate function."
  (setq op
	(cond
	 ((equal  op   "<"            ) '(<     org-string<  org-time<))
	 ((equal  op   ">"            ) '(>     org-string>  org-time>))
	 ((member op '("<=" "=<"     )) '(<=    org-string<= org-time<=))
	 ((member op '(">=" "=>"     )) '(>=    org-string>= org-time>=))
	 ((member op '("="  "=="     )) '(=     string=      org-time=))
	 ((member op '("<>" "!=" "/=")) '(/=    org-string<> org-time<>))))
  (nth (if (eq stringp 'time) 2 (if stringp 1 0)) op))

(defvar org-agenda-skip-comment-trees)
(defvar org-agenda-skip-function)
(defun org-map-entries (func &optional match scope &rest skip)
  "Call FUNC at each headline selected by MATCH in SCOPE.

FUNC is a function or a Lisp form.  The function will be called without
arguments, with the cursor positioned at the beginning of the headline.
The return values of all calls to the function will be collected and
returned as a list.

The call to FUNC will be wrapped into a `save-excursion' form, so FUNC
does not need to preserve point.  After evaluation, the cursor will be
moved to the end of the line (presumably of the headline of the
processed entry) and search continues from there.  Under some
circumstances, this may not produce the wanted results.  For example,
if you have removed (e.g. archived) the current (sub)tree it could
mean that the next entry will be skipped entirely.  In such cases, you
can specify the position from where search should continue by making
FUNC set the variable `org-map-continue-from' to the desired buffer
position.

MATCH is a tags/property/todo match as it is used in the agenda tags view.
Only headlines that are matched by this query will be considered during
the iteration.  When MATCH is nil or t, all headlines will be
visited by the iteration.

SCOPE determines the scope of this command.  It can be any of:

nil     The current buffer, respecting the restriction if any
tree    The subtree started with the entry at point
region  The entries within the active region, if any
region-start-level
        The entries within the active region, but only those at
        the same level than the first one.
file    The current buffer, without restriction
file-with-archives
        The current buffer, and any archives associated with it
agenda  All agenda files
agenda-with-archives
        All agenda files with any archive files associated with them
\(file1 file2 ...)
        If this is a list, all files in the list will be scanned

The remaining args are treated as settings for the skipping facilities of
the scanner.  The following items can be given here:

  archive    skip trees with the archive tag
  comment    skip trees with the COMMENT keyword
  function or Emacs Lisp form:
             will be used as value for `org-agenda-skip-function', so
             whenever the function returns a position, FUNC will not be
             called for that entry and search will continue from the
             position returned

If your function needs to retrieve the tags including inherited tags
at the *current* entry, you can use the value of the variable
`org-scanner-tags' which will be much faster than getting the value
with `org-get-tags'.  If your function gets properties with
`org-entry-properties' at the *current* entry, bind `org-trust-scanner-tags'
to t around the call to `org-entry-properties' to get the same speedup.
Note that if your function moves around to retrieve tags and properties at
a *different* entry, you cannot use these techniques."
  (unless (and (or (eq scope 'region) (eq scope 'region-start-level))
	       (not (org-region-active-p)))
    (let* ((org-agenda-archives-mode nil) ; just to make sure
	   (org-agenda-skip-archived-trees (memq 'archive skip))
	   (org-agenda-skip-comment-trees (memq 'comment skip))
	   (org-agenda-skip-function
	    (car (org-delete-all '(comment archive) skip)))
	   (org-tags-match-list-sublevels t)
	   (start-level (eq scope 'region-start-level))
	   matcher res
	   org-todo-keywords-for-agenda
	   org-done-keywords-for-agenda
	   org-todo-keyword-alist-for-agenda
	   org-tag-alist-for-agenda
	   org--matcher-tags-todo-only)

      (cond
       ((eq match t)   (setq matcher t))
       ((eq match nil) (setq matcher t))
       (t (setq matcher (if match (cdr (org-make-tags-matcher match)) t))))

      (save-excursion
	(save-restriction
	  (cond ((eq scope 'tree)
		 (org-back-to-heading t)
		 (org-narrow-to-subtree)
		 (setq scope nil))
		((and (or (eq scope 'region) (eq scope 'region-start-level))
		      (org-region-active-p))
		 ;; If needed, set start-level to a string like "2"
		 (when start-level
		   (save-excursion
		     (goto-char (region-beginning))
		     (unless (org-at-heading-p) (outline-next-heading))
		     (setq start-level (org-current-level))))
		 (narrow-to-region (region-beginning)
				   (save-excursion
				     (goto-char (region-end))
				     (unless (and (bolp) (org-at-heading-p))
				       (outline-next-heading))
				     (point)))
		 (setq scope nil)))

	  (if (not scope)
	      (progn
                ;; Agenda expects a file buffer.  Skip over refreshing
                ;; agenda cache for non-file buffers.
                (when buffer-file-name
		  (org-agenda-prepare-buffers
		   (and buffer-file-name (list (current-buffer)))))
		(setq res
		      (org-scan-tags
		       func matcher org--matcher-tags-todo-only start-level)))
	    ;; Get the right scope
	    (cond
	     ((and scope (listp scope) (symbolp (car scope)))
	      (setq scope (eval scope t)))
	     ((eq scope 'agenda)
	      (setq scope (org-agenda-files t)))
	     ((eq scope 'agenda-with-archives)
	      (setq scope (org-agenda-files t))
              (require 'org-archive)
	      (setq scope (org-add-archive-files scope)))
	     ((eq scope 'file)
	      (setq scope (and buffer-file-name (list buffer-file-name))))
	     ((eq scope 'file-with-archives)
	      (setq scope (org-add-archive-files (list (buffer-file-name))))))
	    (org-agenda-prepare-buffers scope)
	    (dolist (file scope)
	      (with-current-buffer (org-find-base-buffer-visiting file)
		(org-with-wide-buffer
		 (goto-char (point-min))
		 (setq res
		       (append
			res
			(org-scan-tags
			 func matcher org--matcher-tags-todo-only)))))))))
      res)))

(provide 'org-map)

;;; org-map.el ends here
