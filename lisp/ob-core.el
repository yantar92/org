;;; ob-core.el --- Working with Code Blocks          -*- lexical-binding: t; -*-

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

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'cl-lib)
(require 'ob-eval)
(require 'org-macs)
(require 'org-fold)
(require 'org-compat)
(require 'org-cycle)
(require 'org-element-context)
(require 'org-element)
(require 'org-move)
(require 'org-property)
(require 'org-map)
(require 'org-src)

(require 'ob-core-read)
(require 'ob-core-result)

(defgroup org-babel nil
  "Code block evaluation and management in `org-mode' documents."
  :tag "Babel"
  :group 'org)

;;; Variables and constants

(defvar org-babel-src-name-regexp
  "^[ \t]*#\\+name:[ \t]*"
  "Regular expression used to match a source name line.")

(defvar org-babel-multi-line-header-regexp
  "^[ \t]*#\\+headers?:[ \t]*\\([^\n]*\\)$"
  "Regular expression used to match multi-line header arguments.")

(defconst org-babel-name-regexp
  (format "^[ \t]*#\\+%s:[ \t]*"
	  ;; FIXME: TBLNAME is for backward compatibility.
	  (regexp-opt '("NAME" "TBLNAME")))
  "Regexp matching a NAME keyword.")

;;; Babel backend API

(defconst org-babel-exeext
  (if (memq system-type '(windows-nt cygwin))
      ".exe"
    nil))

(defvar org-babel-exp-reference-buffer nil
  "Buffer containing original contents of the exported buffer.
This is used by Babel to resolve references in source blocks.
Its value is dynamically bound during export.")

(defun org-babel-expand-body:generic (body params &optional var-lines)
  "Expand BODY with PARAMS.
Expand a block of code with org-babel according to its header
arguments.  This generic implementation of body expansion is
called for languages which have not defined their own specific
org-babel-expand-body:lang function.

VAR-LINES is a list of lines that define variable environment.  These
lines will be added after `:prologue' parameter and before BODY."
  (let ((pro (cdr (assq :prologue params)))
	(epi (cdr (assq :epilogue params))))
    (mapconcat #'identity
	       (append (when pro (list pro))
		       var-lines
		       (list body)
		       (when epi (list epi)))
	       "\n")))

(defun org-babel-session-buffer (&optional info)
  "Return buffer name for session associated with current code block.
Return nil when no such live buffer with process exists.
When INFO is non-nil, it should be a list returned by
`org-babel-get-src-block-info'.
This function uses org-babel-session-buffer:<lang> function to
retrieve backend-specific session buffer name."
  (declare-function org-babel-comint-buffer-livep "ob-comint" (buffer))
  (when-let* ((info (or info (org-babel-get-src-block-info 'no-eval)))
              (lang (nth 0 info))
              (session (cdr (assq :session (nth 2 info))))
              (cmd (intern (concat "org-babel-session-buffer:" lang)))
              (buffer-name
               (if (fboundp cmd)
                   (funcall cmd session info)
                 ;; Use session name as buffer name by default.
                 session)))
    (require 'ob-comint)
    (when (org-babel-comint-buffer-livep buffer-name)
      buffer-name)))

(defun org-babel-pick-name (names selector)
  "Select one out of an alist of row or column names.
SELECTOR can be either a list of names in which case those names
will be returned directly, or an index into the list NAMES in
which case the indexed names will be return."
  (if (listp selector)
      selector
    (when names
      (if (and selector (symbolp selector) (not (equal t selector)))
	  (cdr (assoc selector names))
	(if (integerp selector)
	    (nth (- selector 1) names)
	  (cdr (car (last names))))))))

;; FIXME: This should probably be an internal function.
(defun org-babel-put-colnames (table colnames)
  "Add COLNAMES to TABLE if they exist."
  (if colnames (apply 'list colnames 'hline table) table))

;; FIXME: This should probably be an internal function.
(defun org-babel-put-rownames (table rownames)
  "Add ROWNAMES to TABLE if they exist."
  (if rownames
      (mapcar (lambda (row)
                (if (listp row)
                    (cons (or (pop rownames) "") row)
                  row))
	      table)
    table))

(defun org-babel-reassemble-table (table colnames rownames)
  "Add column and row names to a table.
Given a TABLE and set of COLNAMES and ROWNAMES add the names
to the table for reinsertion to `org-mode'."
  (if (listp table)
      (let ((table (if (and rownames (= (length table) (length rownames)))
                       (org-babel-put-rownames table rownames) table)))
        (if (and colnames (listp (car table)) (= (length (car table))
                                                 (length colnames)))
            (org-babel-put-colnames table colnames) table))
    table))

(defmacro org-babel-result-cond (result-params scalar-form &rest table-forms)
  "Call the code to parse raw string results according to RESULT-PARAMS.
Do nothing with :results discard.
Execute SCALAR-FORM when result should be treated as a string.
Execute TABLE-FORMS when result should be considered sexp and parsed."
  (declare (indent 1) (debug t))
  (org-with-gensyms (params)
    `(let ((,params ,result-params))
       (unless (member "discard" ,params)
         (if (or (member "scalar" ,params)
	         (member "verbatim" ,params)
	         (member "html" ,params)
	         (member "code" ,params)
	         (member "pp" ,params)
	         (member "file" ,params)
	         (and (or (member "output" ,params)
			  (member "raw"    ,params)
			  (member "org"    ,params)
			  (member "drawer" ,params))
		      (not (member "table" ,params))))
	     ,scalar-form
	   ,@table-forms)))))

(defun org-babel-graphical-output-file (params)
  "File where a babel block should send graphical output, per PARAMS.
Return nil if no graphical output is expected.  Raise an error if
the output file is ill-defined."
  (let ((file (cdr (assq :file params))))
    (cond (file (and (member "graphics" (cdr (assq :result-params params)))
		     file))
	  ((assq :file-ext params)
	   (user-error ":file-ext given but no :file generated; did you forget \
to name a block?"))
	  (t (user-error "No :file header argument given; cannot create \
graphical result")))))

(defun org-babel-process-file-name (name &optional no-quote-p)
  "Prepare NAME to be used in an external process.
If NAME specifies a remote location, the remote portion of the
name is removed, since in that case the process will be executing
remotely.  The file name is then processed by `expand-file-name'.
Unless second argument NO-QUOTE-P is non-nil, the file name is
additionally processed by `shell-quote-argument'."
  (let ((f (org-babel-local-file-name (expand-file-name name))))
    (if no-quote-p f (shell-quote-argument f))))

(defun org-babel-make-language-alias (new old)
  "Make source blocks of type NEW aliases for those of type OLD.

NEW and OLD should be strings.  This function should be called
after the babel API for OLD-type source blocks is fully defined.

Callers of this function will probably want to add an entry to
`org-src-lang-modes' as well."
  (dolist (fn '("execute" "expand-body" "prep-session"
		"variable-assignments" "load-session"
		"edit-prep"))
    (let ((sym (intern-soft (concat "org-babel-" fn ":" old))))
      (when (and sym (fboundp sym))
	(defalias (intern (concat "org-babel-" fn ":" new)) sym))))
  ;; Technically we don't need a `dolist' for just one variable, but
  ;; we keep it for symmetry/ease of future expansion.
  (dolist (var '("default-header-args"))
    (let ((sym (intern-soft (concat "org-babel-" var ":" old))))
      (when (and sym (boundp sym))
	(defvaralias (intern (concat "org-babel-" var ":" new)) sym)))))

;;; Helpers

(defmacro org-babel-when-in-src-block (&rest body)
  "Execute BODY if point is in a source block and return t.

Otherwise do nothing and return nil."
  `(if (org-element-type-p (org-element-context) '(inline-src-block src-block))
       (progn
	 ,@body
	 t)
     nil))

(defun org-babel-combine-header-arg-lists (original &rest others)
  "Combine ORIGINAL and OTHERS lists of header argument names and arguments."
  (let ((results (copy-sequence original)))
    (dolist (new-list others)
      (dolist (arg-pair new-list)
	(let ((header (car arg-pair)))
	  (setq results
		(cons arg-pair (cl-remove-if
				(lambda (pair) (equal header (car pair)))
				results))))))
    results))

(defun org-babel-named-src-block-regexp-for-name (&optional name)
  "Generate a regexp used to match a source block named NAME.
If NAME is nil, match any name.  Matched name is then put in
match group 9.  Other match groups are defined in
`org-babel-src-block-regexp'."
  (concat org-babel-src-name-regexp
	  (concat (if name (regexp-quote name) "\\(?9:.*?\\)") "[ \t]*" )
	  "\\(?:\n[ \t]*#\\+\\S-+:.*\\)*?"
	  "\n"
	  (substring org-babel-src-block-regexp 1)))

(defun org-babel-named-data-regexp-for-name (name)
  "Generate a regexp used to match data named NAME."
  (concat org-babel-name-regexp (regexp-quote name) "[ \t]*$"))

(defun org-babel-balanced-split (string alts)
  "Split STRING on instances of ALTS.
ALTS is a character, or cons of two character options where each
option may be either the numeric code of a single character or
a list of character alternatives.  For example, to split on
balanced instances of \"[ \t]:\", set ALTS to ((32 9) . 58)."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((splitp (lambda (past next)
		    ;; Non-nil when there should be a split after NEXT
		    ;; character. PAST is the character before NEXT.
		    (pcase alts
		      (`(,(and first (pred consp)) . ,(and second (pred consp)))
		       (and (memq past first) (memq next second)))
		      (`(,first . ,(and second (pred consp)))
		       (and (eq past first) (memq next second)))
		      (`(,(and first (pred consp)) . ,second)
		       (and (memq past first) (eq next second)))
		      (`(,first . ,second)
		       (and (eq past first) (eq next second)))
		      ((pred (eq next)) t)
		      (_ nil))))
	  (partial nil)
	  (result nil))
      (while (not (eobp))
        (cond
	 ((funcall splitp (char-before) (char-after))
	  ;; There is a split after point.  If ALTS is two-folds,
	  ;; remove last parsed character as it belongs to ALTS.
	  (when (consp alts) (pop partial))
	  ;; Include elements parsed so far in RESULTS and flush
	  ;; partial parsing.
	  (when partial
	    (push (apply #'string (nreverse partial)) result)
	    (setq partial nil))
	  (forward-char))
	 ((memq (char-after) '(?\( ?\[))
	  ;; Include everything between balanced brackets.
	  (let* ((origin (point))
		 (after (char-after))
		 (openings (list after)))
	    (forward-char)
	    (while (and openings (re-search-forward "[]()]" nil t))
	      (pcase (char-before)
		((and match (or ?\[ ?\()) (push match openings))
		(?\] (when (eq ?\[ (car openings)) (pop openings)))
		(_ (when (eq ?\( (car openings)) (pop openings)))))
	    (if (null openings)
		(setq partial
		      (nconc (nreverse (string-to-list
					(buffer-substring origin (point))))
			     partial))
	      ;; Un-balanced bracket.  Backtrack.
	      (push after partial)
	      (goto-char (1+ origin)))))
	 ((and (eq ?\" (char-after)) (not (eq ?\\ (char-before))))
	  ;; Include everything from current double quote to next
	  ;; non-escaped double quote.
	  (let ((origin (point)))
	    (if (re-search-forward "[^\\]\"" nil t)
		(setq partial
		      (nconc (nreverse (string-to-list
					(buffer-substring origin (point))))
			     partial))
	      ;; No closing double quote.  Backtrack.
	      (push ?\" partial)
	      (forward-char))))
	 (t (push (char-after) partial)
	    (forward-char))))
      ;; Add pending parsing and return result.
      (when partial (push (apply #'string (nreverse partial)) result))
      (nreverse result))))

(defun org-babel-join-splits-near-ch (ch list)
  "Join strings in LIST where CH is on either end of the strings.
This function will join list elements like \"a=\" \"2\" into \"a=2\"."
  (let ((last= (lambda (str) (= ch (aref str (1- (length str))))))
	(first= (lambda (str) (= ch (aref str 0)))))
    (reverse
     (cl-reduce (lambda (acc el)
		  (let ((head (car acc)))
		    (if (and head (or (funcall last= head) (funcall first= el)))
			(cons (concat head el) (cdr acc))
		      (cons el acc))))
		list :initial-value nil))))

;; row and column names
(defun org-babel-del-hlines (table)
  "Remove all `hline's from TABLE."
  (remq 'hline table))

(defun org-babel-get-colnames (table)
  "Return the column names of TABLE.
Return a cons cell, the `car' of which contains the TABLE less
colnames, and the `cdr' of which contains a list of the column
names."
  ;; Skip over leading hlines.
  (while (eq 'hline (car table)) (pop table))
  (if (eq 'hline (nth 1 table))
      (cons (cddr table) (car table))
    (cons (cdr table) (car table))))

(defun org-babel-get-rownames (table)
  "Return the row names of TABLE.
Return a cons cell, the `car' of which contains the TABLE less
rownames, and the `cdr' of which contains a list of the rownames.
Note: this function removes any hlines in TABLE."
  (let* ((table (org-babel-del-hlines table))
	 (rownames (funcall (lambda ()
			      (let ((tp table))
				(mapcar
				 (lambda (_row)
				   (prog1
				       (pop (car tp))
				     (setq tp (cdr tp))))
				 table))))))
    (cons table rownames)))

(defun org-babel-update-block-body (new-body)
  "Update the body of the current code block to NEW-BODY."
  (let ((element (org-element-at-point)))
    (unless (org-element-type-p element 'src-block)
      (error "Not in a source block"))
    (goto-char (org-src-block-head element))
    (let* ((ind (org-current-text-indentation))
	   (body-start (line-beginning-position 2))
	   (body (org-element-normalize-string
		  (if (org-src-preserve-indentation-p element) new-body
		    (with-temp-buffer
		      (insert (org-remove-indentation new-body))
		      (indent-rigidly
		       (point-min)
		       (point-max)
		       (+ ind org-edit-src-content-indentation))
		      (buffer-string))))))
      (delete-region body-start
		     (org-with-wide-buffer
		      (goto-char (org-element-end element))
		      (skip-chars-backward " \t\n")
		      (line-beginning-position)))
      (goto-char body-start)
      (insert body))))

(defun org-babel-src-block-names (&optional file)
  "Return the names of source blocks in FILE or the current buffer."
  (with-current-buffer (if file (find-file-noselect file) (current-buffer))
    (org-with-point-at 1
      (let ((regexp "^[ \t]*#\\+begin_src ")
	    (case-fold-search t)
	    (names nil))
	(while (re-search-forward regexp nil t)
	  (let ((element (org-element-at-point)))
	    (when (org-element-type-p element 'src-block)
	      (let ((name (org-element-property :name element)))
		(when name (push name names))))))
	names))))

(defun org-babel-find-named-block (name)
  "Find a named source-code block.
Return the location of the source block identified by source
NAME, or nil if no such block exists.  Set match data according
to `org-babel-named-src-block-regexp'."
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (org-babel-named-src-block-regexp-for-name name)))
      (or (and (looking-at regexp)
	       (progn (goto-char (match-beginning 1))
		      (line-beginning-position)))
	  (ignore-errors (org-next-block 1 nil regexp))))))

;;; Code evaluation safety

(defcustom org-confirm-babel-evaluate t
  "Confirm before evaluation.
\\<org-mode-map>\
Require confirmation before interactively evaluating code
blocks in Org buffers.  The default value of this variable is t,
meaning confirmation is required for any code block evaluation.
This variable can be set to nil to inhibit any future
confirmation requests.  This variable can also be set to a
function which takes two arguments the language of the code block
and the body of the code block.  Such a function should then
return a non-nil value if the user should be prompted for
execution or nil if no prompt is required.

Warning: Disabling confirmation may result in accidental
evaluation of potentially harmful code.  It may be advisable
remove code block execution from `\\[org-ctrl-c-ctrl-c]' \
as further protection
against accidental code block evaluation.  The
`org-babel-no-eval-on-ctrl-c-ctrl-c' variable can be used to
remove code block execution from the `\\[org-ctrl-c-ctrl-c]' keybinding."
  :group 'org-babel
  :version "24.1"
  :type '(choice boolean function))
;; don't allow this variable to be changed through file settings
(put 'org-confirm-babel-evaluate 'safe-local-variable (lambda (x) (eq x t)))

(defcustom org-babel-no-eval-on-ctrl-c-ctrl-c nil
  "\\<org-mode-map>\
Remove code block evaluation from the `\\[org-ctrl-c-ctrl-c]' key binding."
  :group 'org-babel
  :version "24.1"
  :type 'boolean)

(defun org-babel-check-confirm-evaluate (info)
  "Check whether INFO allows code block evaluation.

Returns nil if evaluation is disallowed, t if it is
unconditionally allowed, and the symbol `query' if the user
should be asked whether to allow evaluation."
  (let* ((headers (nth 2 info))
	 (eval (or (cdr  (assq :eval headers))
		   (when (assq :noeval headers) "no")))
	 (eval-no (member eval '("no" "never")))
	 (export org-babel-exp-reference-buffer)
	 (eval-no-export (and export (member eval '("no-export" "never-export"))))
	 (noeval (or eval-no eval-no-export))
	 (query (or (equal eval "query")
		    (and export (equal eval "query-export"))
		    (if (functionp org-confirm-babel-evaluate)
			(funcall org-confirm-babel-evaluate
				 ;; Language, code block body.
				 (nth 0 info)
				 (org-babel--expand-body info))
		      org-confirm-babel-evaluate))))
    (cond
     (noeval nil)
     (query 'query)
     (t t))))

(defun org-babel-check-evaluate (info)
  "Check if code block INFO should be evaluated.
Do not query the user, but do display an informative message if
evaluation is blocked.  Returns non-nil if evaluation is not blocked."
  (let ((confirmed (org-babel-check-confirm-evaluate info)))
    (unless confirmed
      (message "Evaluation of this %s code block%sis disabled."
	       (nth 0 info)
	       (let ((name (nth 4 info)))
		 (if name (format " (%s) " name) " "))))
    confirmed))

;; Dynamically scoped for asynchronous export.
(defvar org-babel-confirm-evaluate-answer-no)

(defun org-babel-confirm-evaluate (info)
  "Confirm evaluation of the code block INFO.

This query can also be suppressed by setting the value of
`org-confirm-babel-evaluate' to nil, in which case all future
interactive code block evaluations will proceed without any
confirmation from the user.

Note disabling confirmation may result in accidental evaluation
of potentially harmful code.

The variable `org-babel-confirm-evaluate-answer-no' is used by
the async export process, which requires a non-interactive
environment, to override this check."
  (let* ((evalp (org-babel-check-confirm-evaluate info))
	 (lang (nth 0 info))
	 (name (nth 4 info))
	 (name-string (if name (format " (%s) " name) " ")))
    (pcase evalp
      (`nil nil)
      (`t t)
      (`query (or
	       (and (not (bound-and-true-p
			org-babel-confirm-evaluate-answer-no))
		    (yes-or-no-p
		     (format "Evaluate this %s code block%son your system? "
			     lang name-string)))
	       (progn
		 (message "Evaluation of this %s code block%sis aborted."
			  lang name-string)
		 nil)))
      (x (error "Unexpected value `%s' from `org-babel-check-confirm-evaluate'" x)))))

;;; Noweb expansion

(defcustom org-babel-noweb-wrap-start "<<"
  "String used to begin a noweb reference in a code block.
See also `org-babel-noweb-wrap-end'."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-noweb-wrap-end ">>"
  "String used to end a noweb reference in a code block.
See also `org-babel-noweb-wrap-start'."
  :group 'org-babel
  :type 'string)

(defun org-babel-noweb-wrap (&optional regexp)
  "Return regexp matching a Noweb reference.

Match any reference, or only those matching REGEXP, if non-nil.

When matching, reference is stored in match group 1."
  (concat (regexp-quote org-babel-noweb-wrap-start)
	  (or regexp "\\([^ \t\n]\\(?:.*?[^ \t\n]\\)?\\)")
	  (regexp-quote org-babel-noweb-wrap-end)))

(defvar org-babel-noweb-error-all-langs nil
  "Raise errors when noweb references don't resolve.
Also see `org-babel-noweb-error-langs' to control noweb errors on
a language by language bases.")

(defvar org-babel-noweb-error-langs nil
  "Languages for which Babel will raise literate programming errors.
List of languages for which errors should be raised when the
source code block satisfying a noweb reference in this language
can not be resolved.  Also see `org-babel-noweb-error-all-langs'
to raise errors for all languages.")

(defun org-babel-noweb-p (params context)
  "Check if PARAMS require expansion in CONTEXT.
CONTEXT may be one of :tangle, :export or :eval."
  (let ((allowed-values (cl-case context
			  (:tangle '("yes" "tangle" "no-export" "strip-export" "strip-tangle"))
			  (:eval   '("yes" "no-export" "strip-export" "eval" "strip-tangle"))
			  (:export '("yes" "strip-tangle")))))
    (cl-some (lambda (v) (member v allowed-values))
	     (split-string (or (cdr (assq :noweb params)) "")))))

(defvar org-babel-library-of-babel) ; defined later
(declare-function org-babel-tangle-comment-links "ob-tangle" (&optional info))
(defvar org-babel-tangle-uncomment-comments) ; defined in ob-tangle.el
(defvar org-babel-expand-noweb-references--cache nil
  "Noweb reference cache used during expansion.")
(defvar org-babel-expand-noweb-references--cache-buffer nil
  "Cons (BUFFER . MODIFIED-TICK) for cached noweb references.
See `org-babel-expand-noweb-references--cache'.")
(defun org-babel-expand-noweb-references (&optional info parent-buffer)
  "Expand Noweb references in the body of the current source code block.

When optional argument INFO is non-nil, use the block defined by INFO
instead.

The block is assumed to be located in PARENT-BUFFER or current buffer
\(when PARENT-BUFFER is nil).

For example the following reference would be replaced with the
body of the source-code block named `example-block'.

<<example-block>>

Note that any text preceding the <<foo>> construct on a line will
be interposed between the lines of the replacement text.  So for
example if <<foo>> is placed behind a comment, then the entire
replacement text will also be commented.

This function must be called from inside of the buffer containing
the source-code block which holds BODY.

In addition the following syntax can be used to insert the
results of evaluating the source-code block named `example-block'.

<<example-block()>>

Any optional arguments can be passed to example-block by placing
the arguments inside the parenthesis following the convention
defined by `org-babel-lob'.  For example

<<example-block(a=9)>>

would set the value of argument \"a\" equal to \"9\".  Note that
these arguments are not evaluated in the current source-code
block but are passed literally to the \"example-block\"."
  (let* ((parent-buffer (or parent-buffer (current-buffer)))
	 (info (or info (org-babel-get-src-block-info 'no-eval)))
         (lang (nth 0 info))
         (body (nth 1 info))
	 (comment (string= "noweb" (cdr (assq :comments (nth 2 info)))))
         (noweb-prefix (let ((v (assq :noweb-prefix (nth 2 info))))
                         (or (not v)
                             (and (org-not-nil (cdr v))
                                  (not (equal (cdr v) "no"))))))
	 (noweb-re (format "\\(.*?\\)\\(%s\\)"
			   (with-current-buffer parent-buffer
			     (org-babel-noweb-wrap)))))
    (unless (equal (cons parent-buffer
                         (with-current-buffer parent-buffer
                           (buffer-chars-modified-tick)))
                   org-babel-expand-noweb-references--cache-buffer)
      (setq org-babel-expand-noweb-references--cache nil
            org-babel-expand-noweb-references--cache-buffer
            (cons parent-buffer
                  (with-current-buffer parent-buffer
                    (buffer-chars-modified-tick)))))
    (cl-macrolet ((c-wrap
	            (s)
	            ;; Comment string S, according to LANG mode.  Return new
	            ;; string.
	            `(unless org-babel-tangle-uncomment-comments
	               (with-temp-buffer
		         (funcall (org-src-get-lang-mode lang))
		         (comment-region (point)
				         (progn (insert ,s) (point)))
		         (org-trim (buffer-string)))))
	          (expand-body
	            (i)
	            ;; Expand body of code represented by block info I.
	            `(let ((b (if (org-babel-noweb-p (nth 2 ,i) :eval)
			          (org-babel-expand-noweb-references ,i)
		                (nth 1 ,i))))
	               (if (not comment) b
                         (require 'ob-tangle)
		         (let ((cs (org-babel-tangle-comment-links ,i)))
		           (concat (c-wrap (car cs)) "\n"
			           b "\n"
			           (c-wrap (cadr cs)) "\n")))))
	          (expand-references
	            (ref)
	            `(pcase (gethash ,ref org-babel-expand-noweb-references--cache)
	               (`(,last . ,previous)
	                ;; Ignore separator for last block.
	                (let ((strings (list (expand-body last))))
		          (dolist (i previous)
		            (let ((parameters (nth 2 i)))
		              ;; Since we're operating in reverse order, first
		              ;; push separator, then body.
		              (push (or (cdr (assq :noweb-sep parameters)) "\n")
			            strings)
		              (push (expand-body i) strings)))
		          (mapconcat #'identity strings "")))
	               ;; Raise an error about missing reference, or return the
	               ;; empty string.
	               ((guard (or org-babel-noweb-error-all-langs
			           (member lang org-babel-noweb-error-langs)))
	                (error "Cannot resolve %s (see `org-babel-noweb-error-langs')"
		               (org-babel-noweb-wrap ,ref)))
	               (_ ""))))
      (replace-regexp-in-string
       noweb-re
       (lambda (m)
         (with-current-buffer parent-buffer
	   (save-match-data
	     (let* ((prefix (match-string 1 m))
		    (id (match-string 3 m))
		    (evaluate (string-match-p "(.*)" id))
		    (expansion
		     (cond
		      (evaluate
                       (prog1
		           (let ((raw (org-babel-ref-resolve id)))
		             (if (stringp raw) raw (format "%S" raw)))
                         ;; Evaluation can potentially modify the buffer
		         ;; and invalidate the cache: reset it.
                         (unless (equal org-babel-expand-noweb-references--cache-buffer
                                        (cons parent-buffer
                                              (buffer-chars-modified-tick)))
		           (setq org-babel-expand-noweb-references--cache nil
                                 org-babel-expand-noweb-references--cache-buffer
                                 (cons parent-buffer
                                       (with-current-buffer parent-buffer
                                         (buffer-chars-modified-tick)))))))
                      ;; Already cached.
                      ((and (hash-table-p org-babel-expand-noweb-references--cache)
                            (gethash id org-babel-expand-noweb-references--cache))
                       (expand-references id))
		      ;; Return the contents of headlines literally.
		      ((org-babel-ref-goto-headline-id id)
		       (org-babel-ref-headline-body))
		      ;; Look for a source block named SOURCE-NAME.  If
		      ;; found, assume it is unique; do not look after
		      ;; `:noweb-ref' header argument.
		      ((org-with-point-at 1
		         (let ((r (org-babel-named-src-block-regexp-for-name id)))
			   (and (re-search-forward r nil t)
			        (not (org-in-commented-heading-p))
                                (let ((info (org-babel-get-src-block-info t)))
                                  (unless (hash-table-p org-babel-expand-noweb-references--cache)
                                    (setq org-babel-expand-noweb-references--cache (make-hash-table :test #'equal)))
                                  (push info (gethash id  org-babel-expand-noweb-references--cache))
			          (expand-body info))))))
		      ;; Retrieve from the Library of Babel.
		      ((nth 2 (assoc-string id org-babel-library-of-babel)))
		      ;; All Noweb references were cached in a previous
		      ;; run.  Yet, ID is not in cache (see the above
		      ;; condition).  Process missing reference in
		      ;; `expand-references'.
		      ((and (hash-table-p org-babel-expand-noweb-references--cache)
                            (gethash 'buffer-processed org-babel-expand-noweb-references--cache))
		       (expand-references id))
		      ;; Though luck.  We go into the long process of
		      ;; checking each source block and expand those
		      ;; with a matching Noweb reference.  Since we're
		      ;; going to visit all source blocks in the
		      ;; document, cache information about them as well.
		      (t
		       (setq org-babel-expand-noweb-references--cache (make-hash-table :test #'equal))
		       (org-with-wide-buffer
		        (org-babel-map-src-blocks nil
			  (if (org-in-commented-heading-p)
			      (org-forward-heading-same-level nil t)
			    (let* ((info (org-babel-get-src-block-info t))
				   (ref (cdr (assq :noweb-ref (nth 2 info)))))
			      (push info (gethash ref org-babel-expand-noweb-references--cache))))))
                       (puthash 'buffer-processed t org-babel-expand-noweb-references--cache)
		       (expand-references id)))))
	       ;; Interpose PREFIX between every line.
               (if noweb-prefix
		   (mapconcat #'identity
			      (split-string expansion "[\n\r]")
			      (concat "\n" prefix))
                 expansion)))))
       body t t 2))))

;;; Library of Babel

(defvar org-babel-library-of-babel nil
  "Library of source-code blocks.
This is an association list.  Populate the library by calling
`org-babel-lob-ingest' on files containing source blocks.")

(defvar org-babel-default-lob-header-args '((:exports . "results"))
  "Default header arguments to use when exporting Babel calls.
By default, a Babel call inherits its arguments from the source
block being called.  Header arguments defined in this variable
take precedence over these.  It is useful for properties that
should not be inherited from a source block.")

(defun org-babel-lob-ingest (&optional file)
  "Add all named source blocks defined in FILE to `org-babel-library-of-babel'."
  (interactive "fFile: ")
  (let ((lob-ingest-count 0))
    (org-babel-map-src-blocks file
      (let* ((info (org-babel-get-src-block-info 'no-eval))
	     (source-name (nth 4 info)))
	(when source-name
	  (setf (nth 1 info)
		(if (org-babel-noweb-p (nth 2 info) :eval)
		    (org-babel-expand-noweb-references info)
		  (nth 1 info)))
	  (let ((source (intern source-name)))
	    (setq org-babel-library-of-babel
		  (cons (cons source info)
			(assq-delete-all source org-babel-library-of-babel))))
	  (cl-incf lob-ingest-count))))
    (message "%d source block%s added to Library of Babel"
	     lob-ingest-count (if (> lob-ingest-count 1) "s" ""))
    lob-ingest-count))

;;;###autoload
(defun org-babel-lob-execute-maybe ()
  "Execute a Library of Babel source block, if appropriate.
Detect if this is context for a Library Of Babel source block and
if so then run the appropriate source block from the Library."
  (interactive)
  (let* ((datum (org-element-context))
         (info (org-babel-lob-get-info datum)))
    (when info
      (org-babel-execute-src-block nil info nil (org-element-type datum))
      t)))

(defun org-babel-lob--src-info (ref)
  "Return internal representation for Babel data referenced as REF.
REF is a string.  This function looks into the current document
for a Babel call or source block.  If none is found, it looks
after REF in the Library of Babel."
  (let ((name ref)
	(file nil))
    ;; Extract the remote file, if specified in the reference.
    (when (string-match "\\`\\(.+\\):\\(.+\\)\\'" ref)
      (setq file (match-string 1 ref))
      (setq name (match-string 2 ref)))
    ;; During export, look into the pristine copy of the document
    ;; being exported instead of the current one, which could miss
    ;; some data.
    (with-current-buffer (cond (file (find-file-noselect file t))
			       (org-babel-exp-reference-buffer)
			       (t (current-buffer)))
      (org-with-point-at 1
	(catch :found
	  (let ((case-fold-search t)
		(regexp (org-babel-named-data-regexp-for-name name)))
	    (while (re-search-forward regexp nil t)
	      (let ((element (org-element-at-point)))
		(when (equal name (org-element-property :name element))
		  (throw :found
			 (pcase (org-element-type element)
			   (`src-block (org-babel-get-src-block-info t element))
			   (`babel-call (org-babel-lob-get-info element))
			   ;; Non-executable data found.  Since names
			   ;; are supposed to be unique throughout
			   ;; a document, bail out.
			   (_ nil))))))
	    (cdr (assoc-string ref org-babel-library-of-babel))))))))

;;;###autoload
(defun org-babel-lob-get-info (&optional datum no-eval)
  "Return internal representation for Library of Babel function call.

Consider DATUM, when provided, or element at point otherwise.

When optional argument NO-EVAL is non-nil, Babel does not resolve
remote variable references; a process which could likely result
in the execution of other code blocks, and do not evaluate Lisp
values in parameters.

Return nil when not on an appropriate location.  Otherwise return
a list compatible with `org-babel-get-src-block-info', which
see."
  (let* ((context (or datum (org-element-context)))
	 (type (org-element-type context))
	 (reference (org-element-property :call context)))
    (when (memq type '(babel-call inline-babel-call))
      (pcase (org-babel-lob--src-info reference)
	(`(,language ,body ,header ,_ ,_ ,_ ,coderef)
	 (let ((begin (org-element-property (if (eq type 'inline-babel-call)
						:begin
					      :post-affiliated)
					    context)))
	   (list language
		 body
		 (apply #'org-babel-merge-params
			header
			org-babel-default-lob-header-args
			(append
			 (org-with-point-at begin
			   (org-babel-params-from-properties language no-eval))
			 (list
			  (org-babel-parse-header-arguments
			   (org-element-property :inside-header context) no-eval)
			  (let ((args (org-element-property :arguments context)))
			    (and args
				 (mapcar (lambda (ref) (cons :var ref))
					 (org-babel-ref-split-args args))))
			  (org-babel-parse-header-arguments
			   (org-element-property :end-header context) no-eval))))
		 nil
		 (org-element-property :name context)
		 begin
		 coderef)))
	(_ nil)))))

;;; Executing babel functions and referencing external data
;; Functions for referencing data from the header arguments of a
;; org-babel block.  The syntax of such a reference should be

;;   #+VAR: variable-name=file:resource-id

;; - variable-name :: the name of the variable to which the value
;;                    will be assigned

;; - file :: path to the file containing the resource, or omitted if
;;           resource is in the current file

;; - resource-id :: the id or name of the resource

;; So an example of a simple source block referencing table data in
;; the same file would be

;;  #+NAME: sandbox
;;  | 1 |         2 | 3 |
;;  | 4 | org-babel | 6 |
;;
;;  #+begin_src emacs-lisp :var table=sandbox
;;    (message table)
;;  #+end_src

(defvar org-babel-update-intermediate nil
  "Update the in-buffer results of code blocks executed to resolve references.")

(defvar org-babel-current-src-block-location) ; defined later
(defun org-babel-ref-parse (assignment)
  "Parse a variable ASSIGNMENT in a header argument.

If the right hand side of the assignment has a literal value
return that value, otherwise interpret it as a reference to an
external resource and find its value using `org-babel-ref-resolve'.

Return a list with two elements: the name of the variable, and an
Emacs Lisp representation of the value of the variable."
  (when (string-match "\\(.+?\\)=" assignment)
    (let ((var (org-trim (match-string 1 assignment)))
	  (ref (org-trim (substring assignment (match-end 0)))))
      (cons (intern var)
	    (let ((out (save-excursion
			 (when org-babel-current-src-block-location
			   (goto-char (if (markerp org-babel-current-src-block-location)
					  (marker-position org-babel-current-src-block-location)
					org-babel-current-src-block-location)))
			 (org-babel-read ref))))
	      (if (equal out ref)
		  (if (and (string-prefix-p "\"" ref)
			   (string-suffix-p "\"" ref))
		      (read ref)
		    (org-babel-ref-resolve ref))
		out))))))

(declare-function org-id-find-id-file "org-id-search" (id))
(declare-function org-id-find-id-in-file "org-id-search" (id file &optional markerp))
(defun org-babel-ref-goto-headline-id (id)
  "Move point to headline with \"CUSTOM_ID\" or \"ID\" property ID."
  (require 'org-id-search)
  (or (let ((h (org-find-property "CUSTOM_ID" id)))
	(when h (goto-char h)))
      (let* ((file (org-id-find-id-file id))
	     (m (when file (org-id-find-id-in-file id file 'marker))))
	(when (and file m)
	  (message "file:%S" file)
	  (pop-to-buffer-same-window (marker-buffer m))
	  (goto-char m)
	  (move-marker m nil)
	  t))))

(defun org-babel-ref-headline-body ()
  (save-restriction
    (org-narrow-to-subtree)
    (buffer-substring
     (save-excursion (goto-char (point-min))
		     (org-end-of-meta-data)
		     (point))
     (point-max))))

(defvar org-babel-library-of-babel)
(defun org-babel-ref-resolve (ref)
  "Resolve the reference REF and return its value."
  (save-window-excursion
    (with-current-buffer (or org-babel-exp-reference-buffer (current-buffer))
      (save-excursion
	(let ((case-fold-search t)
	      args new-refere new-header-args new-referent split-file split-ref
	      index contents)
	  ;; if ref is indexed grab the indices -- beware nested indices
	  (when (and (string-match "\\[\\([^\\[]*\\)\\]$" ref)
		     (let ((str (substring ref 0 (match-beginning 0))))
		       (= (cl-count ?\( str) (cl-count ?\) str))))
            (if (> (length (match-string 1 ref)) 0)
	        (setq index (match-string 1 ref))
              (setq contents t))
	    (setq ref (substring ref 0 (match-beginning 0))))
	  ;; assign any arguments to pass to source block
	  (when (string-match
		 "^\\(.+?\\)\\(\\[\\(.*\\)\\]\\|\\(\\)\\)(\\(.*\\))$" ref)
	    (setq new-refere      (match-string 1 ref))
	    (setq new-header-args (match-string 3 ref))
	    (setq new-referent    (match-string 5 ref))
	    (when (> (length new-refere) 0)
	      (when (> (length new-referent) 0)
		(setq args (mapcar (lambda (ref) (cons :var ref))
				   (org-babel-ref-split-args new-referent))))
	      (when (> (length new-header-args) 0)
		(setq args (append (org-babel-parse-header-arguments
				    new-header-args)
				   args)))
	      (setq ref new-refere)))
	  (when (string-match "^\\(.+\\):\\(.+\\)$" ref)
	    (setq split-file (match-string 1 ref))
	    (setq split-ref (match-string 2 ref))
            (when (file-exists-p split-file)
	      (find-file split-file)
	      (setq ref split-ref)))
	  (org-with-wide-buffer
	   (goto-char (point-min))
	   (let* ((params (append args '((:results . "none"))))
		  (regexp (org-babel-named-data-regexp-for-name ref))
		  (result
		   (catch :found
		     ;; Check for code blocks or named data.
		     (while (re-search-forward regexp nil t)
		       ;; Ignore COMMENTed headings and orphaned
		       ;; affiliated keywords.
		       (unless (org-in-commented-heading-p)
			 (let ((e (org-element-at-point)))
			   (when (equal (org-element-property :name e) ref)
			     (goto-char
			      (org-element-post-affiliated e))
			     (pcase (org-element-type e)
			       (`babel-call
				(throw :found
				       (org-babel-execute-src-block
					nil (org-babel-lob-get-info e) params)))
			       ((and `src-block (guard (not contents)))
				(throw :found
				       (org-babel-execute-src-block
					nil nil
					(and
					 (not org-babel-update-intermediate)
					 params))))
			       ((and (let v (org-babel-read-element e))
				     (guard v))
				(throw :found v))
			       (_ (error "Reference not found")))))))
		     ;; Check for local or global headlines by ID.
		     (when (org-babel-ref-goto-headline-id ref)
		       (throw :found (org-babel-ref-headline-body)))
		     ;; Check the Library of Babel.
		     (let ((info (cdr (assq (intern ref)
					    org-babel-library-of-babel))))
		       (when info
			 (throw :found
				(org-babel-execute-src-block nil info params))))
		     (error "Reference `%s' not found in this buffer" ref))))
	     (cond
	      ((and result (symbolp result)) (format "%S" result))
	      ((and index (listp result))
	       (org-babel-ref-index-list index result))
	      (t result)))))))))

(defun org-babel-ref-index-list (index lis)
  "Return the subset of LIS indexed by INDEX.

Indices are 0 based and negative indices count from the end of
LIS, so 0 references the first element of LIS and -1 references
the last.  If INDEX is separated by \",\"s then each \"portion\"
is assumed to index into the next deepest nesting or dimension.

A valid \"portion\" can consist of either an integer index, two
integers separated by a \":\" in which case the entire range is
returned, or an empty string or \"*\" both of which are
interpreted to mean the entire range and as such are equivalent
to \"0:-1\"."
  (if (and (> (length index) 0) (string-match "^\\([^,]*\\),?" index))
      (let* ((ind-re "\\(\\([-[:digit:]]+\\):\\([-[:digit:]]+\\)\\|\\*\\)")
	     (lgth (length lis))
	     (portion (match-string 1 index))
	     (remainder (substring index (match-end 0)))
	     (wrap (lambda (num) (if (< num 0) (+ lgth num) num)))
	     (open (lambda (ls) (if (and (listp ls) (= (length ls) 1)) (car ls) ls))))
	(funcall
	 open
	 (mapcar
	  (lambda (sub-lis)
	    (if (listp sub-lis)
		(org-babel-ref-index-list remainder sub-lis)
	      sub-lis))
	  (if (or (= 0 (length portion)) (string-match ind-re portion))
	      (mapcar
	       (lambda (n) (nth n lis))
	       (apply 'org-number-sequence
		      (if (and (> (length portion) 0) (match-string 2 portion))
			  (list
			   (funcall wrap (string-to-number (match-string 2 portion)))
			   (funcall wrap (string-to-number (match-string 3 portion))))
			(list (funcall wrap 0) (funcall wrap -1)))))
	    (list (nth (funcall wrap (string-to-number portion)) lis))))))
    lis))

(defun org-babel-ref-split-args (arg-string)
  "Split ARG-STRING into top-level arguments of balanced parenthesis."
  (mapcar #'org-trim (org-babel-balanced-split arg-string 44)))

;;; Parsing header arguments

(defconst org-babel-common-header-args-w-values
  '((cache	. ((no yes)))
    (cmdline	. :any)
    (colnames	. ((nil no yes)))
    (comments	. ((no link yes org both noweb)))
    (dir	. :any)
    (eval	. ((yes no no-export strip-export never-export eval never
			query)))
    (exports	. ((code results both none)))
    (epilogue   . :any)
    (file	. :any)
    (file-desc  . :any)
    (file-ext   . :any)
    (file-mode  . ((#o755 #o555 #o444 :any)))
    (hlines	. ((no yes)))
    (mkdirp	. ((yes no)))
    (no-expand)
    (noeval)
    (noweb	. ((yes no tangle strip-tangle no-export strip-export)))
    (noweb-ref	. :any)
    (noweb-sep  . :any)
    (noweb-prefix . ((no yes)))
    (output-dir . :any)
    (padline	. ((yes no)))
    (post       . :any)
    (prologue   . :any)
    (results	. ((file list vector table scalar verbatim)
		   (raw html latex org code pp drawer link graphics)
		   (replace silent none discard append prepend)
		   (output value)))
    (rownames	. ((no yes)))
    (sep	. :any)
    (session	. :any)
    (shebang	. :any)
    (tangle	. ((tangle yes no :any)))
    (tangle-mode . ((#o755 #o555 #o444 :any)))
    (var	. :any)
    (wrap       . :any))
  "Alist defining common header args and their allowed values.

Keys of the alist are header arg symbols.
Values of the alist are either a symbol `:any' or a list of allowed
values as symbols:

   (header-name . :any)
   (header-name . ((value1 value2 value3 ...))
   (header-name . ((value1 value2 value3 ... :any))

When Org considers header-arg property inheritance, the innermost
value from the list is considered.

Symbol `:any' in the value list implies that any value is allowed.
Yet the explicitly listed values from the list will be offered as
completion candidates.

FIXME: This is currently just supported for `results' and `exports'.
Values in the alist can also be a list of lists.  The inner lists
define exclusive groups of values that can be set at the same time for
a given header argument.

  (results . ((file list ...)
             (raw html ...))

The above example allows multi-component header arguments like

   #+begin_src bash :results file raw
   <:results will combine the two values \"file raw\".>

   #+begin_src bash :results file list
   <:results will only use the last value \"list\".>

   #+property: header-args :results file html
   ...
   #+begin_src bash :results list
   <:results will inherit with partial override \"list html\".>

See info node `(org)Results of evaluation' for more details.")

(defconst org-babel-header-arg-names
  (mapcar #'car org-babel-common-header-args-w-values)
  "Common header arguments used by org-babel.
Note that individual languages may define their own language
specific header arguments as well.")

(defconst org-babel-safe-header-args
  '(:cache :colnames :comments :exports :epilogue :hlines :noeval
	   :noweb :noweb-ref :noweb-sep :noweb-prefix :padline
           :prologue :rownames :sep :session :tangle :wrap
	   (:eval . ("never" "query"))
	   (:results . (lambda (str) (not (string-match "file" str)))))
  "A list of safe header arguments for babel source blocks.

The list can have entries of the following forms:
- :ARG                     -> :ARG is always a safe header arg
- (:ARG . (VAL1 VAL2 ...)) -> :ARG is safe as a header arg if it is
                              `equal' to one of the VALs.
- (:ARG . FN)              -> :ARG is safe as a header arg if the function FN
                              returns non-nil.  FN is passed one
                              argument, the value of the header arg
                              (as a string).")

(defun org-babel-one-header-arg-safe-p (pair safe-list)
  "Determine if the PAIR is a safe babel header arg according to SAFE-LIST.

For the format of SAFE-LIST, see `org-babel-safe-header-args'."
  (and (consp pair)
       (keywordp (car pair))
       (stringp (cdr pair))
       (or
	(memq (car pair) safe-list)
	(let ((entry (assq (car pair) safe-list)))
	  (and entry
	       (consp entry)
	       (cond ((functionp (cdr entry))
		      (funcall (cdr entry) (cdr pair)))
		     ((listp (cdr entry))
		      (member (cdr pair) (cdr entry)))
		     (t nil)))))))

(defmacro org-babel-header-args-safe-fn (safe-list)
  "Return a function that determines whether a list of header args are safe.

Intended usage is:
\(put \\='org-babel-default-header-args \\='safe-local-variable
 (org-babel-header-args-safe-p org-babel-safe-header-args)

This allows org-babel languages to extend the list of safe values for
their `org-babel-default-header-args:foo' variable.

For the format of SAFE-LIST, see `org-babel-safe-header-args'."
  `(lambda (value)
     (and (listp value)
	  (cl-every
	   (lambda (pair)
	     (and (consp pair)
		  (org-babel-one-header-arg-safe-p pair ,safe-list)))
	   value))))

(defvar org-babel-default-header-args
  '((:session . "none") (:results . "replace") (:exports . "code")
    (:cache . "no") (:noweb . "no") (:hlines . "no") (:tangle . "no"))
  "Default arguments to use when evaluating a source block.

This is a list in which each element is an alist.  Each key
corresponds to a header argument, and each value to that header's
value.  The value can either be a string or a closure that
evaluates to a string.

A closure is evaluated when the source block is being
evaluated (e.g. during execution or export), with point at the
source block.  It is not possible to use an arbitrary function
symbol (e.g. `some-func'), since org uses lexical binding.  To
achieve the same functionality, call the function within a
closure (e.g. (lambda () (some-func))).

To understand how closures can be used as default header
arguments, imagine you'd like to set the file name output of a
latex source block to a sha1 of its contents.  We could achieve
this with:

  (defun org-src-sha ()
    (let ((elem (org-element-at-point)))
      (concat (sha1 (org-element-property :value elem)) \".svg\")))

  (setq org-babel-default-header-args:latex
        `((:results . \"file link replace\")
          (:file . (lambda () (org-src-sha)))))

Because the closure is evaluated with point at the source block,
the call to `org-element-at-point' above will always retrieve
information about the current source block.

Some header arguments can be provided multiple times for a source
block.  An example of such a header argument is :var.  This
functionality is also supported for default header arguments by
providing the header argument multiple times in the alist.  For
example:

 ((:var . \"foo=\\\"bar\\\"\")
  (:var . \"bar=\\\"foo\\\"\"))")

(put 'org-babel-default-header-args 'safe-local-variable
     (org-babel-header-args-safe-fn org-babel-safe-header-args))

(defvar org-babel-default-inline-header-args
  '((:session . "none") (:results . "replace")
    (:exports . "results") (:hlines . "yes"))
  "Default arguments to use when evaluating an inline source block.")
(put 'org-babel-default-inline-header-args 'safe-local-variable
     (org-babel-header-args-safe-fn org-babel-safe-header-args))

(defun org-babel--get-vars (params)
  "Return the babel variable assignments in PARAMS.

PARAMS is a quasi-alist of header args, which may contain
multiple entries for the key `:var'.  This function returns a
list of the cdr of all the `:var' entries."
  (mapcar #'cdr
	  (cl-remove-if-not (lambda (x) (eq (car x) :var)) params)))

(defun org-babel--normalize-body (datum)
  "Normalize body for element or object DATUM.
DATUM is a source block element or an inline source block object.
Remove final newline character and spurious indentation."
  (let* ((value (org-element-property :value datum))
	 (body (if (string-suffix-p "\n" value)
		   (substring value 0 -1)
		 value)))
    (cond ((org-element-type-p datum 'inline-src-block)
	   ;; Newline characters and indentation in an inline
	   ;; src-block are not meaningful, since they could come from
	   ;; some paragraph filling.  Treat them as a white space.
	   (replace-regexp-in-string "\n[ \t]*" " " body))
	  ((org-src-preserve-indentation-p datum) body)
	  (t (org-remove-indentation body)))))

(defun org-babel-eval-headers (headers)
  "Compute header list set with HEADERS.

Evaluate all header arguments set to functions prior to returning
the list of header arguments."
  (let ((lst nil))
    (dolist (elem headers)
      (if (and (cdr elem) (functionp (cdr elem)))
          (push `(,(car elem) . ,(funcall (cdr elem))) lst)
        (push elem lst)))
    (reverse lst)))

(defun org-babel-parse-header-arguments (string &optional no-eval)
  "Parse header arguments in STRING.
When optional argument NO-EVAL is non-nil, do not evaluate Lisp
in parameters.  Return an alist."
  (when (org-string-nw-p string)
    (org-babel-parse-multiple-vars
     (delq nil
	   (mapcar
	    (lambda (arg)
	      (if (string-match
		   "\\([^ \f\t\n\r\v]+\\)[ \f\t\n\r\v]+\\([^ \f\t\n\r\v]+.*\\)"
		   arg)
		  (cons (intern (match-string 1 arg))
			(org-babel-read (org-babel-chomp (match-string 2 arg))
					no-eval))
		(cons (intern (org-babel-chomp arg)) nil)))
	    (let ((raw (org-babel-balanced-split string '((32 9) . 58))))
              (cons (car raw)
		    (mapcar (lambda (r) (concat ":" r)) (cdr raw)))))))))

(defun org-babel-parse-multiple-vars (header-arguments)
  "Expand multiple variable assignments behind a single :var keyword.

This allows expression of multiple variables with one :var as
shown below.

#+PROPERTY: var foo=1, bar=2

HEADER-ARGUMENTS is an alist of all the arguments."
  (let (results)
    (mapc (lambda (pair)
	    (if (eq (car pair) :var)
                (or
	         (mapcar (lambda (v) (push (cons :var (org-trim v)) results))
		         (org-babel-join-splits-near-ch
		          61 (org-babel-balanced-split (or (cdr pair) "") 32)))
                 (push `(:var) results))
	      (push pair results)))
	  header-arguments)
    (nreverse results)))

(defun org-babel-disassemble-tables (vars hlines colnames rownames)
  "Parse tables for further processing.
Process the variables in VARS according to the HLINES,
ROWNAMES and COLNAMES header arguments.  Return a list consisting
of the vars, cnames and rnames."
  (let (cnames rnames)
    (list
     (mapcar
      (lambda (var)
        (when (proper-list-p (cdr var))
          (when (and (not (equal colnames "no"))
                     ;; Compatibility note: avoid `length>', which
                     ;; isn't available until Emacs 28.
                     (or colnames
                         ;; :colnames nil (default)
                         ;; Auto-assign column names when the table
                         ;; has hline as the second line after
                         ;; non-hline row.
                         (and (> (length (cdr var)) 1)
                              (not (eq (car (cdr var)) 'hline)) ; first row
                              (eq (nth 1 (cdr var)) 'hline) ; second row
                              (not (member 'hline (cddr (cdr var)))) ; other rows
                              )))
            (let ((both (org-babel-get-colnames (cdr var))))
              (setq cnames (cons (cons (car var) (cdr both))
                                 cnames))
              (setq var (cons (car var) (car both)))))
          (when (and rownames (not (equal rownames "no")))
            (let ((both (org-babel-get-rownames (cdr var))))
              (setq rnames (cons (cons (car var) (cdr both))
                                 rnames))
              (setq var (cons (car var) (car both)))))
          (when (and hlines (not (equal hlines "yes")))
            (setq var (cons (car var) (org-babel-del-hlines (cdr var))))))
        var)
      vars)
     (reverse cnames) (reverse rnames))))

(defun org-babel-process-params (params)
  "Expand variables in PARAMS and add summary parameters."
  (let* ((processed-vars (mapcar (lambda (el)
				   (if (consp el)
				       el
				     (org-babel-ref-parse el)))
				 (org-babel--get-vars params)))
	 (vars-and-names (if (and (assq :colname-names params)
				  (assq :rowname-names params))
			     (list processed-vars)
			   (org-babel-disassemble-tables
			    processed-vars
			    (cdr (assq :hlines params))
			    (cdr (assq :colnames params))
			    (cdr (assq :rownames params)))))
	 (raw-result (or (cdr (assq :results params)) ""))
	 (result-params (delete-dups
			 (append
			  (split-string (if (stringp raw-result)
					    raw-result
                                          ;; FIXME: Arbitrary code evaluation.
					  (eval raw-result t)))
			  (cdr (assq :result-params params))))))
    (append
     (mapcar (lambda (var) (cons :var var)) (car vars-and-names))
     (list
      (cons :colname-names (or (cdr (assq :colname-names params))
			       (cadr  vars-and-names)))
      (cons :rowname-names (or (cdr (assq :rowname-names params))
			       (cl-caddr vars-and-names)))
      (cons :result-params result-params)
      (cons :result-type  (cond ((member "output" result-params) 'output)
				((member "value" result-params) 'value)
				(t 'value))))
     (cl-remove-if
      (lambda (x) (memq (car x) '(:colname-names :rowname-names :result-params
					    :result-type :var)))
      params))))

(defun org-babel-params-from-properties (&optional lang no-eval)
  "Retrieve source block parameters specified as properties.

LANG is the language of the source block, as a string.  When
optional argument NO-EVAL is non-nil, do not evaluate Lisp values
in parameters.

Return a list of association lists of source block parameters
specified in the properties of the current outline entry."
  (save-match-data
    (list
     ;; Header arguments specified with the header-args property at
     ;; point of call.
     (org-babel-parse-header-arguments
      (org-entry-get (point) "header-args" 'inherit)
      no-eval)
     ;; Language-specific header arguments at point of call.
     (and lang
	  (org-babel-parse-header-arguments
	   (org-entry-get (point) (concat "header-args:" lang) 'inherit)
	   no-eval)))))

(declare-function org-attach-dir "org-attach" (&optional create-if-not-exists-p no-fs-check))
(defun org-babel-merge-params (&rest alists)
  "Combine all parameter association lists in ALISTS.
Later elements of ALISTS override the values of previous elements.
This takes into account some special considerations for certain
parameters when merging lists."
  (let* ((results-exclusive-groups
	  (mapcar (lambda (group) (mapcar #'symbol-name group))
		  (cdr (assq 'results org-babel-common-header-args-w-values))))
	 (exports-exclusive-groups
	  (mapcar (lambda (group) (mapcar #'symbol-name group))
		  (cdr (assq 'exports org-babel-common-header-args-w-values))))
	 (merge
	  (lambda (exclusive-groups &rest result-params)
	    ;; Maintain exclusivity of mutually exclusive parameters,
	    ;; as defined in EXCLUSIVE-GROUPS while merging lists in
	    ;; RESULT-PARAMS.
	    (let (output)
	      (dolist (new-params result-params (delete-dups output))
		(dolist (new-param new-params)
		  (dolist (exclusive-group exclusive-groups)
		    (when (member new-param exclusive-group)
		      (setq output (cl-remove-if
				    (lambda (o) (member o exclusive-group))
				    output))))
		  (push new-param output))))))
	 (variable-index 0)		;Handle positional arguments.
	 clearnames
	 params				;Final parameters list.
	 ;; Some keywords accept multiple values.  We need to treat
	 ;; them specially.
	 vars results exports)
    (dolist (alist alists)
      (dolist (pair alist)
	(pcase pair
	  (`(:var . ,value)
	   (let ((name (cond
                        ;; Default header arguments can accept lambda
                        ;; functions.  We uniquely identify the var
                        ;; according to the full string contents of
                        ;; the lambda function.
			((functionp value) value)
			((listp value) (car value))
			((string-match "^\\([^= \f\t\n\r\v]+\\)[ \t]*=" value)
			 (intern (match-string 1 value)))
			(t nil))))
	     (cond
	      (name
	       (setq vars
		     (append (if (not (assoc name vars)) vars
			       (push name clearnames)
			       (cl-remove-if (lambda (p) (equal name (car p)))
					     vars))
			     (list (cons name pair)))))
	      ((and vars (nth variable-index vars))
	       ;; If no name is given and we already have named
	       ;; variables then assign to named variables in order.
	       (let ((name (car (nth variable-index vars))))
		 ;; Clear out colnames and rownames for replace vars.
		 (push name clearnames)
		 (setf (cddr (nth variable-index vars))
		       (concat (symbol-name name) "=" value))
		 (cl-incf variable-index)))
	      (t (error "Variable \"%s\" must be assigned a default value"
			(cdr pair))))))
	  (`(:results . ,value)
	   (setq results (funcall merge
				  results-exclusive-groups
				  results
				  (split-string
				   (cond ((stringp value) value)
                                         ((functionp value) (funcall value))
                                         ;; FIXME: Arbitrary code evaluation.
                                         (t (eval value t)))))))
	  (`(:exports . ,value)
	   (setq exports (funcall merge
				  exports-exclusive-groups
				  exports
                                  (split-string
                                   (cond ((and value (functionp value)) (funcall value))
                                         (value value)
                                         (t ""))))))
          ((or '(:dir . attach) '(:dir . "'attach"))
           (require 'org-attach)
           (if-let ((attach-dir (org-attach-dir nil t)))
               (setq params (append
                             `((:dir . ,attach-dir)
                               (:mkdirp . "yes"))
                             (assq-delete-all :dir (assq-delete-all :mkdir params))))
             (error "No attachment directory for element (add :ID: or :DIR: property)")))
	  ;; Regular keywords: any value overwrites the previous one.
	  (_ (setq params (cons pair (assq-delete-all (car pair) params)))))))
    ;; Handle `:var' and clear out colnames and rownames for replaced
    ;; variables.
    (setq params (nconc (mapcar (lambda (v) (cons :var (cddr v))) vars)
			params))
    (dolist (name clearnames)
      (dolist (param '(:colname-names :rowname-names))
	(when (assq param params)
	  (setf (cdr (assq param params))
		(cl-remove-if (lambda (pair) (equal name (car pair)))
			      (cdr (assq param params))))
	  (setq params
		(cl-remove-if (lambda (pair) (and (equal (car pair) param)
					     (null (cdr pair))))
			      params)))))
    ;; Handle other special keywords, which accept multiple values.
    (setq params (nconc (list (cons :results (mapconcat #'identity results " "))
			      (cons :exports (mapconcat #'identity exports " ")))
			params))
    ;; Return merged params.
    (org-babel-eval-headers params)))

(defun org-babel-get-src-block-info (&optional no-eval datum)
  "Extract information from a source block or inline source block.

When optional argument NO-EVAL is non-nil, Babel does not resolve
remote variable references; a process which could likely result
in the execution of other code blocks, and do not evaluate Lisp
values in parameters.

By default, consider the block at point.  However, when optional
argument DATUM is provided, extract information from that parsed
object instead.

Return nil if point is not on a source block (blank lines after a
source block are considered a part of that source block).
Otherwise, return a list with the following pattern:

  (language body arguments switches name start coderef)"
  (let* ((datum (or datum (org-element-context)))
	 (type (org-element-type datum))
	 (inline (eq type 'inline-src-block)))
    (when (memq type '(inline-src-block src-block))
      (let* ((lang (org-element-property :language datum))
	     (lang-headers (intern
			    (concat "org-babel-default-header-args:" lang)))
	     (name (org-element-property :name datum))
	     (info
	      (list
	       lang
	       (org-babel--normalize-body datum)
	       (apply #'org-babel-merge-params
		      (if inline org-babel-default-inline-header-args
			org-babel-default-header-args)
		      (and (boundp lang-headers) (eval lang-headers t))
		      (append
		       ;; If DATUM is provided, make sure we get node
		       ;; properties applicable to its location within
		       ;; the document.
		       (org-with-point-at (org-element-begin datum)
			 (org-babel-params-from-properties lang no-eval))
		       (mapcar (lambda (h)
				 (org-babel-parse-header-arguments h no-eval))
			       (cons (org-element-property :parameters datum)
				     (org-element-property :header datum)))))
	       (or (org-element-property :switches datum) "")
	       name
	       (org-element-property (if inline :begin :post-affiliated)
				     datum)
	       (and (not inline) (org-src-coderef-format datum)))))
	(unless no-eval
	  (setf (nth 2 info) (org-babel-process-params (nth 2 info))))
	(setf (nth 2 info) (org-babel-generate-file-param name (nth 2 info)))
	info))))

;;; Executing src blocks

(defvar org-babel-after-execute-hook nil
  "Hook for functions to be called after `org-babel-execute-src-block'.")

(defvar org-babel-current-src-block-location nil
  "Marker pointing to the source block currently being executed.
This may also point to a call line or an inline code block.  If
multiple blocks are being executed (e.g., in chained execution
through use of the :var header argument) this marker points to
the outer-most code block.")

(defvar *this*)
;; Dynamically bound in `org-babel-execute-src-block'
;; and `org-babel-read'

(defun org-babel--expand-body (info)
  "Expand noweb references in src block and remove any coderefs.
The src block is defined by its INFO, as returned by
`org-babel-get-src-block-info'."
  (let ((coderef (nth 6 info))
	(expand
	 (if (org-babel-noweb-p (nth 2 info) :eval)
	     (org-babel-expand-noweb-references info)
	   (nth 1 info))))
    (if (not coderef) expand
      (replace-regexp-in-string
       (org-src-coderef-regexp coderef) "" expand nil nil 1))))

(defun org-babel-generate-file-param (src-name params)
  "Calculate the filename for source block results.

The directory is calculated from the :output-dir property of the
source block; if not specified, use the current directory.

If the source block has a #+NAME and the :file parameter does not
contain any period characters, then the :file parameter is
treated as an extension, and the output file name is the
concatenation of the directory (as calculated above), the block
name, a period, and the parameter value as a file extension.
Otherwise, the :file parameter is treated as a full file name,
and the output file name is the directory (as calculated above)
plus the parameter value."
  (let* ((file-cons (assq :file params))
	 (file-ext-cons (assq :file-ext params))
	 (file-ext (cdr-safe file-ext-cons))
	 (dir (cdr-safe (assq :output-dir params)))
	 fname)
    ;; create the output-dir if it does not exist
    (when dir
      (make-directory dir t))
    (if file-cons
	;; :file given; add :output-dir if given
	(when dir
	  (setcdr file-cons (concat (file-name-as-directory dir) (cdr file-cons))))
      ;; :file not given; compute from name and :file-ext if possible
      (when (and src-name file-ext)
	(if dir
	    (setq fname (concat (file-name-as-directory (or dir ""))
				src-name "." file-ext))
	  (setq fname (concat src-name "." file-ext)))
	(setq params (cons (cons :file fname) params))))
    params))

;;;###autoload
(defun org-babel-execute-src-block (&optional arg info params executor-type)
  "Execute the current source code block and return the result.
Insert the results of execution into the buffer.  Source code
execution and the collection and formatting of results can be
controlled through a variety of header arguments.

With prefix argument ARG, force re-execution even if an existing
result cached in the buffer would otherwise have been returned.

Optionally supply a value for INFO in the form returned by
`org-babel-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the front of the source code
block.

EXECUTOR-TYPE is the type of the org element responsible for the
execution of the source block.  If not provided then informed
guess will be made."
  (interactive)
  (let* ((org-babel-current-src-block-location
          (or org-babel-current-src-block-location
              (nth 5 info)
              (org-src-block-head)))
         (info (if info (copy-tree info) (org-babel-get-src-block-info)))
         (executor-type
          (or executor-type
              ;; If `executor-type' is unset, then we will make an
              ;; informed guess.
              (pcase (and
                      ;; When executing virtual src block, no location
                      ;; is known.
                      org-babel-current-src-block-location
                      (char-after org-babel-current-src-block-location))
                (?s 'inline-src-block)
                (?c 'inline-babel-call)
                (?# (pcase (char-after (+ 2 org-babel-current-src-block-location))
                      (?b 'src-block)
                      (?c 'call-block)
                      (_ 'unknown)))
                (_ 'unknown)))))
    ;; Merge PARAMS with INFO before considering source block
    ;; evaluation since both could disagree.
    (cl-callf org-babel-merge-params (nth 2 info) params)
    (when (org-babel-check-evaluate info)
      (cl-callf org-babel-process-params (nth 2 info))
      (let* ((params (nth 2 info))
	     (cache (let ((c (cdr (assq :cache params))))
		      (and (not arg) c (string= "yes" c))))
	     (new-hash (and cache (org-babel-sha1-hash info :eval)))
	     (old-hash (and cache (org-babel-current-result-hash)))
	     (current-cache (and new-hash (equal new-hash old-hash))))
	(cond
	 (current-cache
	  (save-excursion		;Return cached result.
	    (goto-char (org-babel-where-is-src-block-result nil info))
	    (forward-line)
	    (skip-chars-forward " \t")
	    (let ((result (org-babel-read-result)))
              (unless noninteractive
	        (message (format "Cached: %s"
                                 (replace-regexp-in-string "%" "%%" (format "%S" result)))))
	      result)))
	 ((org-babel-confirm-evaluate info)
	  (let* ((lang (nth 0 info))
		 (result-params (cdr (assq :result-params params)))
		 (body (org-babel--expand-body info))
		 (dir (cdr (assq :dir params)))
		 (mkdirp (cdr (assq :mkdirp params)))
		 (default-directory
		  (cond
		   ((not dir) default-directory)
                   ((when-let ((session (org-babel-session-buffer info)))
                      (buffer-local-value 'default-directory (get-buffer session))))
		   ((member mkdirp '("no" "nil" nil))
		    (file-name-as-directory (expand-file-name dir)))
		   (t
		    (let ((d (file-name-as-directory (expand-file-name dir))))
		      (make-directory d 'parents)
		      d))))
		 (cmd (intern (concat "org-babel-execute:" lang)))
		 result exec-start-time)
	    (unless (fboundp cmd)
	      (error "No org-babel-execute function for %s!" lang))
            (unless noninteractive
	      (message "Executing %s %s %s..."
		       (capitalize lang)
                       (pcase executor-type
                         ('src-block "code block")
                         ('inline-src-block "inline code block")
                         ('babel-call "call")
                         ('inline-babel-call "inline call")
                         (e (symbol-name e)))
		       (let ((name (nth 4 info)))
		         (if name
                             (format "(%s)" name)
                           (format "at position %S" (nth 5 info))))))
	    (setq exec-start-time (current-time)
                  result
		  (let ((r
                         ;; Code block may move point in the buffer.
                         ;; Make sure that the point remains on the
                         ;; code block.
                         (save-excursion (funcall cmd body params))))
		    (if (and (eq (cdr (assq :result-type params)) 'value)
			     (or (member "vector" result-params)
				 (member "table" result-params))
			     (not (listp r)))
			(list (list r))
		      r)))
	    (let ((file (and (member "file" result-params)
			     (cdr (assq :file params)))))
	      ;; If non-empty result and :file then write to :file.
	      (when file
		;; If `:results' are special types like `link' or
		;; `graphics', don't write result to `:file'.  Only
		;; insert a link to `:file'.
		(when (and result
			   (not (or (member "link" result-params)
				    (member "graphics" result-params))))
		  (with-temp-file file
		    (insert (org-babel-format-result
			     result
			     (cdr (assq :sep params)))))
		  ;; Set file permissions if header argument
		  ;; `:file-mode' is provided.
		  (when (assq :file-mode params)
		    (set-file-modes file (cdr (assq :file-mode params)))))
		(setq result file))
	      ;; Possibly perform post process provided its
	      ;; appropriate.  Dynamically bind "*this*" to the
	      ;; actual results of the block.
	      (let ((post (cdr (assq :post params))))
		(when post
		  (let ((*this* (if (not file) result
				  (org-babel-result-to-file
				   file
				   (org-babel--file-desc params result)
                                   'attachment))))
		    (setq result (org-babel-ref-resolve post))
		    (when file
		      (setq result-params (remove "file" result-params))))))
	      (unless (member "none" result-params)
	        (org-babel-insert-result
	         result result-params info
                 ;; append/prepend cannot handle hash as we accumulate
                 ;; multiple outputs together.
                 (when (member "replace" result-params) new-hash)
                 lang
                 (time-subtract (current-time) exec-start-time))))
	    (run-hooks 'org-babel-after-execute-hook)
	    result)))))))

;;;###autoload
(defun org-babel-sha1-hash (&optional info context)
  "Generate a sha1 hash based on the value of INFO.
CONTEXT specifies the context of evaluation.  It can be `:eval',
`:export', `:tangle'.  A nil value means `:eval'."
  (interactive)
  (let ((print-level nil)
	(info (or info (org-babel-get-src-block-info)))
	(context (or context :eval)))
    (setf (nth 2 info)
	  (sort (copy-sequence (nth 2 info))
		(lambda (a b) (string< (car a) (car b)))))
    (let* ((rm (lambda (lst)
		 (dolist (p '("replace" "silent" "none"
			      "discard" "append" "prepend"))
		   (setq lst (remove p lst)))
		 lst))
	   (norm (lambda (arg)
		   (let ((v (if (and (listp (cdr arg)) (null (cddr arg)))
				(copy-sequence (cdr arg))
			      (cdr arg))))
		     (when (and v (not (and (sequencep v)
					  (not (consp v))
					  (= (length v) 0))))
		       (cond
			((and (listp v) ; lists are sorted
			      (member (car arg) '(:result-params)))
			 (sort (funcall rm v) #'string<))
			((and (stringp v) ; strings are sorted
			      (member (car arg) '(:results :exports)))
			 (mapconcat #'identity (sort (funcall rm (split-string v))
						     #'string<) " "))
			(t v))))))
	   ;; expanded body
	   (lang (nth 0 info))
	   (params (nth 2 info))
	   (body (if (org-babel-noweb-p params context)
		     (org-babel-expand-noweb-references info)
		   (nth 1 info)))
	   (expand-cmd (intern (concat "org-babel-expand-body:" lang)))
	   (assignments-cmd (intern (concat "org-babel-variable-assignments:"
					    lang)))
	   (expanded
	    (if (fboundp expand-cmd) (funcall expand-cmd body params)
	      (org-babel-expand-body:generic
	       body params (and (fboundp assignments-cmd)
				(funcall assignments-cmd params))))))
      (let* ((it (format "%s-%s"
                         (mapconcat
                          #'identity
                          (delq nil (mapcar (lambda (arg)
                                            (let ((normalized (funcall norm arg)))
                                              (when normalized
                                                (format "%S" normalized))))
                                          (nth 2 info))) ":")
                         expanded))
             (hash (sha1 it)))
        (when (called-interactively-p 'interactive) (message hash))
        hash))))

(provide 'ob-core)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ob-core.el ends here
