;;; org-mode.el --- Org major mode                      -*- lexical-binding: t; -*-

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

;; This library contains Org major mode declaration.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-load)
(require 'org-mode-common)

(require 'org-element-context)
(require 'org-tags-common)
(require 'thingatpt)
(require 'org-table-core)
(require 'org-mode-common)
(require 'pcomplete)
(require 'org-priority-common)


;;; Org mode options

(defcustom org-mode-hook nil
  "Mode hook for Org mode, run after the mode was turned on."
  :group 'org
  :type 'hook)

(defcustom org-insert-mode-line-in-empty-file nil
  "Non-nil means insert the first line setting Org mode in empty files.
When the function `org-mode' is called interactively in an empty file, this
normally means that the file name does not automatically trigger Org mode.
To ensure that the file will always be in Org mode in the future, a
line enforcing Org mode will be inserted into the buffer, if this option
has been set."
  :group 'org-startup
  :type 'boolean)

(defgroup org-startup nil
  "Startup options Org uses when first visiting a file."
  :tag "Org Startup"
  :group 'org)

(defgroup org-structure nil
  "Options concerning the general structure of Org files."
  :tag "Org Structure"
  :group 'org)

(defcustom org-table-header-line-p nil
  "Activate `org-table-header-line-mode' by default?"
  :type 'boolean
  :package-version '(Org . "9.4")
  :group 'org-table)

(defcustom org-startup-folded 'showeverything
  "Initial folding state of headings when entering Org mode.

Allowed values are:

symbol `nofold'
  Do not fold headings.

symbol `fold'
  Fold everything, leaving only top-level headings visible.

symbol `content'
  Leave all the headings and sub-headings visible, but hide their
  text.  This is an equivalent of table of contents.

symbol `show2levels', `show3levels', `show4levels', `show5levels'
  Show headings up to Nth level.

symbol `showeverything' (default)
  Start Org mode in fully unfolded state.  Unlike all other allowed
  values, this value prevents drawers, blocks, and archived subtrees
  from being folded even when `org-cycle-hide-block-startup',
  `org-cycle-open-archived-trees', or `org-cycle-hide-drawer-startup'
  are non-nil.  Per-subtree visibility settings (see manual node
  `(org)Initial visibility)') are also ignored.

This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: fold              (or `overview', this is equivalent)
   #+STARTUP: nofold            (or `showall', this is equivalent)
   #+STARTUP: content
   #+STARTUP: show<n>levels (<n> = 2..5)
   #+STARTUP: showeverything

Set `org-agenda-inhibit-startup' to a non-nil value if you want
to ignore this option when Org opens agenda files for the first
time."
  :group 'org-startup
  :package-version '(Org . "9.4")
  :type '(choice
	  (const :tag "nofold: show all" nofold)
	  (const :tag "fold: overview" fold)
	  (const :tag "fold: show two levels" show2levels)
	  (const :tag "fold: show three levels" show3levels)
	  (const :tag "fold: show four levels" show4evels)
	  (const :tag "fold: show five levels" show5levels)
	  (const :tag "content: all headlines" content)
	  (const :tag "show everything, even drawers" showeverything)))

(defcustom org-startup-truncated t
  "Non-nil means entering Org mode will set `truncate-lines'.
This is useful since some lines containing links can be very long and
uninteresting.  Also tables look terrible when wrapped.

The variable `org-startup-truncated' enables you to configure
truncation for Org mode different to the other modes that use the
variable `truncate-lines' and as a shortcut instead of putting
the variable `truncate-lines' into the `org-mode-hook'.  If one
wants to configure truncation for Org mode not statically but
dynamically e.g. in a hook like `ediff-prepare-buffer-hook' then
the variable `truncate-lines' has to be used because in such a
case it is too late to set the variable `org-startup-truncated'."
  :group 'org-startup
  :type 'boolean)

(defcustom org-startup-indented nil
  "Non-nil means turn on `org-indent-mode' on startup.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: indent
   #+STARTUP: noindent"
  :group 'org-structure
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Globally (slow on startup in large files)" t)))

(defcustom org-startup-numerated nil
  "Non-nil means turn on `org-num-mode' on startup.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: num
   #+STARTUP: nonum"
  :group 'org-structure
  :package-version '(Org . "9.4")
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Globally" t)))

(defcustom org-startup-with-beamer-mode nil
  "Non-nil means turn on `org-beamer-mode' on startup.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: beamer"
  :group 'org-startup
  :version "24.1"
  :type 'boolean)

(defcustom org-startup-align-all-tables nil
  "Non-nil means align all tables when visiting a file.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:
   #+STARTUP: align
   #+STARTUP: noalign"
  :group 'org-startup
  :type 'boolean)

(defcustom org-startup-shrink-all-tables nil
  "Non-nil means shrink all table columns with a width cookie.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:
   #+STARTUP: shrink"
  :group 'org-startup
  :type 'boolean
  :version "27.1"
  :package-version '(Org . "9.2")
  :safe #'booleanp)

(defcustom org-startup-with-inline-images nil
  "Non-nil means show inline images when loading a new Org file.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:
   #+STARTUP: inlineimages
   #+STARTUP: noinlineimages"
  :group 'org-startup
  :version "24.1"
  :type 'boolean)

(defcustom org-startup-with-latex-preview nil
  "Non-nil means preview LaTeX fragments when loading a new Org file.

This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:
   #+STARTUP: latexpreview
   #+STARTUP: nolatexpreview"
  :group 'org-startup
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defconst org-startup-options
  '(("fold" org-startup-folded fold)
    ("overview" org-startup-folded overview)
    ("nofold" org-startup-folded nofold)
    ("showall" org-startup-folded showall)
    ("show2levels" org-startup-folded show2levels)
    ("show3levels" org-startup-folded show3levels)
    ("show4levels" org-startup-folded show4levels)
    ("show5levels" org-startup-folded show5levels)
    ("showeverything" org-startup-folded showeverything)
    ("content" org-startup-folded content)
    ("indent" org-startup-indented t)
    ("noindent" org-startup-indented nil)
    ("num" org-startup-numerated t)
    ("nonum" org-startup-numerated nil)
    ("hidestars" org-hide-leading-stars t)
    ("showstars" org-hide-leading-stars nil)
    ("odd" org-odd-levels-only t)
    ("oddeven" org-odd-levels-only nil)
    ("align" org-startup-align-all-tables t)
    ("noalign" org-startup-align-all-tables nil)
    ("shrink" org-startup-shrink-all-tables t)
    ("descriptivelinks" org-link-descriptive t)
    ("literallinks" org-link-descriptive nil)
    ("inlineimages" org-startup-with-inline-images t)
    ("noinlineimages" org-startup-with-inline-images nil)
    ("latexpreview" org-startup-with-latex-preview t)
    ("nolatexpreview" org-startup-with-latex-preview nil)
    ("customtime" org-display-custom-times t)
    ("logdone" org-log-done time)
    ("lognotedone" org-log-done note)
    ("nologdone" org-log-done nil)
    ("lognoteclock-out" org-log-note-clock-out t)
    ("nolognoteclock-out" org-log-note-clock-out nil)
    ("logrepeat" org-log-repeat state)
    ("lognoterepeat" org-log-repeat note)
    ("logdrawer" org-log-into-drawer t)
    ("nologdrawer" org-log-into-drawer nil)
    ("logstatesreversed" org-log-states-order-reversed t)
    ("nologstatesreversed" org-log-states-order-reversed nil)
    ("nologrepeat" org-log-repeat nil)
    ("logreschedule" org-log-reschedule time)
    ("lognotereschedule" org-log-reschedule note)
    ("nologreschedule" org-log-reschedule nil)
    ("logredeadline" org-log-redeadline time)
    ("lognoteredeadline" org-log-redeadline note)
    ("nologredeadline" org-log-redeadline nil)
    ("logrefile" org-log-refile time)
    ("lognoterefile" org-log-refile note)
    ("nologrefile" org-log-refile nil)
    ("fninline" org-footnote-define-inline t)
    ("nofninline" org-footnote-define-inline nil)
    ("fnlocal" org-footnote-section nil)
    ("fnauto" org-footnote-auto-label t)
    ("fnprompt" org-footnote-auto-label nil)
    ("fnconfirm" org-footnote-auto-label confirm)
    ("fnplain" org-footnote-auto-label plain)
    ("fnadjust" org-footnote-auto-adjust t)
    ("nofnadjust" org-footnote-auto-adjust nil)
    ("fnanon" org-footnote-auto-label anonymous)
    ("constcgs" constants-unit-system cgs)
    ("constSI" constants-unit-system SI)
    ("noptag" org-tag-persistent-alist nil)
    ("hideblocks" org-hide-block-startup t)
    ("nohideblocks" org-hide-block-startup nil)
    ("hidedrawers" org-hide-drawer-startup t)
    ("nohidedrawers" org-hide-drawer-startup nil)
    ("beamer" org-startup-with-beamer-mode t)
    ("entitiespretty" org-pretty-entities t)
    ("entitiesplain" org-pretty-entities nil))
  "Variable associated with STARTUP options for Org.
Each element is a list of three items: the startup options (as written
in the #+STARTUP line), the corresponding variable, and the value to set
this variable to if the option is found.  An optional fourth element PUSH
means to push this value onto the list in the variable.")


;;; Helper functions used to start Org mode

(defun org-before-change-function (_beg _end)
  "Every change indicates that a table might need an update."
  (setq org-table-may-need-update t))

(defun org-set-regexps-and-options (&optional tags-only)
  "Precompute regular expressions used in the current buffer.
When optional argument TAGS-ONLY is non-nil, only compute tags
related expressions."
  ;; FIXME: The buffer-local values here should eventually become
  ;; unused in the rest of the code, making use of org-data object
  ;; instead.
  (when (derived-mode-p 'org-mode)
    (let ((org-data (org-element-org-data)))
      ;; Startup options.  Get this early since it does change
      ;; behavior for other options (e.g., tags).
      (let ((startup (cl-mapcan (lambda (value) (split-string value))
				(org-element-property :STARTUP org-data))))
	(dolist (option startup)
	  (pcase (assoc-string option org-startup-options t)
	    (`(,_ ,variable ,value t)
	     (unless (listp (symbol-value variable))
	       (set (make-local-variable variable) nil))
	     (add-to-list variable value))
	    (`(,_ ,variable ,value . ,_)
	     (set (make-local-variable variable) value))
	    (_ nil))))
      ;; FIXME: This variable is assigned for backwards-compatibility
      ;; and should probably be removed in future.  It is no longer
      ;; used in Org mode code.
      (setq-local org-file-tags
		  (mapcar #'org-add-prop-inherited
                          (org-element-property :tags org-data)))
      ;; Used for tag completion.
      (setq org-current-tag-alist
	    (org--tag-add-to-alist
	     org-tag-persistent-alist
	     (let ((tags (org-element-property :TAGS org-data)))
	       (if tags
		   (org-tag-string-to-alist
		    (mapconcat #'identity tags "\n"))
		 org-tag-alist))))
      ;; Used for tag completion.
      (setq org-tag-groups-alist
	    (org-tag-alist-to-groups org-current-tag-alist))
      (unless tags-only
        ;; FIXME: `org-keyword-properties' is set for backwards
        ;; compatibility.  org-data element properties should be used
        ;; instead.
	(let ((properties nil))
	  (dolist (value (org-element-property :PROPERTY org-data))
	    (when (string-match "\\(\\S-+\\)[ \t]+\\(.*\\)" value)
	      (setq properties (org--update-property-plist
				(match-string-no-properties 1 value)
				(match-string-no-properties 2 value)
				properties))))
	  (setq-local org-keyword-properties properties))
	;; Archive location.
        ;; FIXME: The users should prefer `org-get-archive-location'
        ;; instead of relying on `org-set-regexps-and-options' to
        ;; parse top-level keyword.
	(let ((archive (org-element-property :ARCHIVE org-data)))
	  (when archive (setq-local org-archive-location archive)))
	;; Category.
	(let ((category (org-element-property :CATEGORY org-data)))
	  (when category
	    (setq-local org-category (intern category))
	    (setq-local org-keyword-properties
			(org--update-property-plist
			 "CATEGORY" category org-keyword-properties))))
	;; Columns.
	(let ((column (org-element-property :COLUMNS org-data)))
	  (when column (setq-local org-columns-default-format column)))
	;; Constants.
	(let ((store nil))
	  (dolist (pair (cl-mapcan #'split-string
				   (org-element-property :CONSTANTS org-data)))
	    (when (string-match "^\\([a-zA-Z0][_a-zA-Z0-9]*\\)=\\(.*\\)" pair)
	      (let* ((name (match-string 1 pair))
		     (value (match-string 2 pair))
		     (old (assoc name store)))
		(if old (setcdr old value)
		  (push (cons name value) store)))))
	  (setq org-table-formula-constants-local store))
	;; Link abbreviations.
        ;; FIXME: This variable should probably be obsoleted.  It is
        ;; no longer used in Org code.
        (setq org-link-abbrev-alist-local
              (org-element-property :link-abbrevs org-data))
	;; Priorities.
	(let ((value (org-element-property :PRIORITIES org-data)))
	  (pcase (and value (split-string value))
	    (`(,high ,low ,default . ,_)
	     (setq-local org-priority-highest (org-priority-to-value high))
	     (setq-local org-priority-lowest (org-priority-to-value low))
	     (setq-local org-priority-default (org-priority-to-value default)))))
	;; Scripts.
	(let ((value (org-element-property :OPTIONS org-data)))
	  (dolist (option value)
	    (when (string-match "\\^:\\(t\\|nil\\|{}\\)" option)
	      (setq-local org-use-sub-superscripts
			  (read (match-string 1 option))))))
	;; TODO keywords.
	(setq-local org-todo-kwd-alist nil)
	(setq-local org-todo-key-alist nil)
	(setq-local org-todo-key-trigger nil)
	(setq-local org-todo-keywords-1 nil)
	(setq-local org-done-keywords nil)
	(setq-local org-todo-heads nil)
	(setq-local org-todo-sets nil)
	(setq-local org-todo-log-states nil)
	(let ((sequences (org-element-property :todo-keyword-settings org-data)))
          ;; org-todo-keywords-1
          (setq org-todo-keywords-1 (org-element-property :todo-keywords org-data))
          ;; org-done-keywords
          (setq org-done-keywords (org-element-property :done-keywords org-data))
          ;; org-todo-heads
          ;; org-todo-sets
          ;; org-todo-log-states
          ;; org-todo-key-alist
          ;; org-todo-kwd-alist
          (dolist (sequence sequences)
            ;; SEQUENCE = (TYPE ((KWD1 . OPT1) (KWD2 . OPT2) ...) ((DONE-KWD1 . OPT1) ...))
            (push (caar (nth 1 sequence)) org-todo-heads)
            (push (mapcar #'car (nth 1 sequence)) org-todo-sets)
            (let (keys
                  (kwd-tail (list (car sequence) ;; TYPE
                                  (car org-todo-heads) ;; First keyword
                                  (car (car (nth 2 sequence))) ;; First done keyword
                                  (car (org-last (nth 2 sequence))) ;; Last done keyword
                                  )))
              (dolist (pair (nth 1 sequence))
                (pcase pair
                  (`(,(and (pred stringp) name) .
                     ,setting)
                   (when (stringp setting)
                     (when-let ((state-setting (org-extract-log-state-settings (concat name "(" setting ")"))))
                       (push state-setting org-todo-log-states))
                     (push (cons name
                                 (if (string-match "^[^!@/]" setting)
                                     (string-to-char (match-string 0 setting))
                                   nil))
                           keys))
                   (push (cons name kwd-tail) org-todo-kwd-alist))))
              (when keys
                (setq keys (nconc (list (list :startgroup))
                                  (nreverse keys)
                                  (list (list :endgroup))))
                (setq org-todo-key-alist (nconc org-todo-key-alist keys)))))
          (setq org-todo-heads (nreverse org-todo-heads)
                org-todo-sets (nreverse org-todo-sets)
                org-todo-kwd-alist (nreverse org-todo-kwd-alist)
                ;; org-todo-key-trigger
                org-todo-key-trigger (delq nil (mapcar #'cdr org-todo-key-alist))
                org-todo-key-alist (org-assign-fast-keys org-todo-key-alist)))
        
	;; Compute the regular expressions and other local variables.
	;; Using `org-outline-regexp-bol' would complicate them much,
	;; because of the fixed white space at the end of that string.

        ;; FIXME: When is `org-done-keywords' nil?
	(unless org-done-keywords
	  (setq org-done-keywords
		(and org-todo-keywords-1 (last org-todo-keywords-1))))
	(setq org-not-done-keywords
	      (org-delete-all org-done-keywords
			      (copy-sequence org-todo-keywords-1))
	      org-todo-regexp (org-element-property :todo-regexp org-data)
	      org-not-done-regexp (regexp-opt org-not-done-keywords t)
	      org-not-done-heading-regexp
	      (format org-heading-keyword-regexp-format org-not-done-regexp)
	      org-todo-line-regexp
	      (format org-heading-keyword-maybe-regexp-format org-todo-regexp)
	      org-complex-heading-regexp
	      (concat "^\\(\\*+\\)"
		      "\\(?: +" org-todo-regexp "\\)?"
		      "\\(?: +\\(\\[#.\\]\\)\\)?"
		      "\\(?: +\\(.*?\\)\\)??"
		      "\\(?:[ \t]+\\(:[[:alnum:]_@#%:]+:\\)\\)?"
		      "[ \t]*$")
	      org-complex-heading-regexp-format
	      (concat "^\\(\\*+\\)"
		      "\\(?: +" org-todo-regexp "\\)?"
		      "\\(?: +\\(\\[#.\\]\\)\\)?"
		      "\\(?: +"
                      ;; Headline might be commented
                      "\\(?:" org-comment-string " +\\)?"
		      ;; Stats cookies can be stuck to body.
		      "\\(?:\\[[0-9%%/]+\\] *\\)*"
		      "\\(%s\\)"
		      "\\(?: *\\[[0-9%%/]+\\]\\)*"
		      "\\)"
		      "\\(?:[ \t]+\\(:[[:alnum:]_@#%%:]+:\\)\\)?"
		      "[ \t]*$")
	      org-todo-line-tags-regexp
	      (concat "^\\(\\*+\\)"
		      "\\(?: +" org-todo-regexp "\\)?"
		      "\\(?: +\\(.*?\\)\\)??"
		      "\\(?:[ \t]+\\(:[[:alnum:]:_@#%]+:\\)\\)?"
		      "[ \t]*$"))))))

(defun org--tag-add-to-alist (alist1 alist2)
  "Merge tags from ALIST1 into ALIST2.

Duplicates tags outside a group are removed.  Keywords and order
are preserved.

The function assumes ALIST1 and ALIST2 are proper tag alists.
See `org-tag-alist' for their structure."
  (cond
   ((null alist2) alist1)
   ((null alist1) alist2)
   (t
    (let ((to-add nil)
	  (group-flag nil))
      (dolist (tag-pair alist1)
	(pcase tag-pair
	  (`(,(or :startgrouptag :startgroup))
	   (setq group-flag t)
	   (push tag-pair to-add))
	  (`(,(or :endgrouptag :endgroup))
	   (setq group-flag nil)
	   (push tag-pair to-add))
	  (`(,(or :grouptags :newline))
	   (push tag-pair to-add))
	  (`(,tag . ,_)
	   ;; Remove duplicates from ALIST1, unless they are in
	   ;; a group.  Indeed, it makes sense to have a tag appear in
	   ;; multiple groups.
	   (when (or group-flag (not (assoc tag alist2)))
	     (push tag-pair to-add)))
	  (_ (error "Invalid association in tag alist: %S" tag-pair))))
      ;; Preserve order of ALIST1.
      (append (nreverse to-add) alist2)))))

;; FIXME: This is very specialized.  Should it be made internal? Or
;; removed?
(defun org-extract-log-state-settings (x)
  "Extract the log state setting from a TODO keyword string.
This will extract info from a string like \"WAIT(w@/!)\"."
  (when (string-match "^\\(.*?\\)\\(?:(\\([^!@/]\\)?\\([!@]\\)?\\(?:/\\([!@]\\)\\)?)\\)?$" x)
    (let ((kw (match-string 1 x))
	  (log1 (and (match-end 3) (match-string 3 x)))
	  (log2 (and (match-end 4) (match-string 4 x))))
      (and (or log1 log2)
	   (list kw
		 (and log1 (if (equal log1 "!") 'time 'note))
		 (and log2 (if (equal log2 "!") 'time 'note)))))))

(defun org-assign-fast-keys (alist)
  "Assign fast keys to a keyword-key alist.
Respect keys that are already there."
  (let (new e (alt ?0))
    (while (setq e (pop alist))
      (if (or (memq (car e) '(:newline :grouptags :endgroup :startgroup))
	      (cdr e)) ;; Key already assigned.
	  (push e new)
	(let ((clist (string-to-list (downcase (car e))))
	      (used (append new alist)))
	  (when (= (car clist) ?@)
	    (pop clist))
	  (while (and clist (rassoc (car clist) used))
	    (pop clist))
	  (unless clist
	    (while (rassoc alt used)
	      (cl-incf alt)))
	  (push (cons (car e) (or (car clist) alt)) new))))
    (nreverse new)))

(defun org--link-at-point ()
  "`thing-at-point' provider function."
  (org-element-property :raw-link (org-element-context)))

(defun org--bounds-of-link-at-point ()
  "`bounds-of-thing-at-point' provider function."
  (let ((context (org-element-context)))
    (when (eq (org-element-type context) 'link)
      (cons (org-element-begin context)
            (org-element-end context)))))

(defun org-setup-comments-handling ()
  (interactive)
  (setq-local comment-use-syntax nil)
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "^\\s-*#\\(?: \\|$\\)")
  (setq-local comment-insert-comment-function 'org-insert-comment)
  (setq-local comment-region-function 'org-comment-or-uncomment-region)
  (setq-local uncomment-region-function 'org-comment-or-uncomment-region))

(defun org-setup-filling ()
  ;; Prevent auto-fill from inserting unwanted new items.
  (setq-local fill-nobreak-predicate
              (org-uniquify
               (append fill-nobreak-predicate
                       '(org-fill-line-break-nobreak-p
                         org-fill-n-macro-as-item-nobreak-p
                         org-fill-paragraph-with-timestamp-nobreak-p))))
  (let ((paragraph-ending (substring org-element-paragraph-separate 1)))
    (setq-local paragraph-start paragraph-ending)
    (setq-local paragraph-separate paragraph-ending))
  (setq-local fill-paragraph-function 'org-fill-paragraph)
  (setq-local fill-forward-paragraph-function
              (lambda (&optional arg)
                (let ((org--single-lines-list-is-paragraph nil))
                  (org-forward-paragraph arg))))
  (setq-local auto-fill-inhibit-regexp nil)
  (setq-local adaptive-fill-function 'org-adaptive-fill-function)
  (setq-local normal-auto-fill-function 'org-auto-fill-function)
  (setq-local comment-line-break-function 'org-comment-line-break-function))

(defun org-occur-reveal-occurrence ()
  "Reveal folded text found by Occur in Org mode."
  (when (derived-mode-p 'org-mode) (org-fold-reveal)))

(defun org-setup-occur ()
  "Setup `occur' and `multi-occur' support."
  (add-hook 'occur-mode-find-occurrence-hook
            #'org-occur-reveal-occurrence nil 'local))

(declare-function org-bookmark-jump-unhide "org-bookmark" (&rest _))
(defun org-setup-bookmark ()
  "Setup bookmark support."
  ;; Make `bookmark-jump' show the jump location if it was hidden.
  (add-hook 'bookmark-after-jump-hook #'org-bookmark-jump-unhide nil 'local))

(defun org-setup-saveplace ()
  "Setup saveplace support."
  ;; Make sure saveplace shows the location if it was hidden
  (advice-add 'save-place-find-file-hook :after #'org-bookmark-jump-unhide))

(declare-function org-imenu-reveal "org-imenu" ())
(defun org-setup-imenu ()
  "Setup imenu integration."
  (setq imenu-create-index-function 'org-imenu-get-tree)
  (add-hook 'imenu-after-jump-hook #'org-imenu-reveal nil 'local))

(declare-function org-get-heading "org-property"
                  (&optional no-tags no-todo no-priority no-comment))
(defun org-add-log-current-headline ()
  "Return current headline or nil.
This function ignores inlinetasks.  It is meant to be used as
`add-log-current-defun-function' value."
  (org-with-limited-levels (org-get-heading t t t t)))

;; Reveal point upon jumping to mark.
;; FIXME: This ought to be configurable by the means of major mode
;; setup, but it is not, so we use advice.  We should eventually file
;; a feature request to Emacs upstream for this.
(declare-function org-fold-show-context "org-fold" (&optional key))
(defun org-mark-jump-unhide (&rest _)
  "Make the point visible with `org-show-context' after jumping to the mark."
  (when (and (derived-mode-p 'org-mode)
	     (org-invisible-p))
    (require 'org-fold)
    (org-fold-show-context 'mark-goto)))
(advice-add 'pop-to-mark-command :after #'org-mark-jump-unhide)
(advice-add 'exchange-point-and-mark :after #'org-mark-jump-unhide)
(advice-add 'pop-global-mark :after #'org-mark-jump-unhide)

(defvar org-agenda-file-menu-enabled t
  "When non-nil, refresh Agenda files in Org menu when loading Org.")


;;; Define Org major mode

(defun org-mode-restart ()
  "Restart `org-mode'."
  (interactive)
  (let ((indent-status (bound-and-true-p org-indent-mode)))
    (funcall major-mode)
    (hack-local-variables)
    (when (and indent-status (not (bound-and-true-p org-indent-mode)))
      (org-indent-mode -1))
    (org-reset-file-cache))
  (message "%s restarted" major-mode))

(declare-function org-parse-arguments "org-pcomplete" ())
(declare-function org-command-at-point "org-pcomplete" ())
(declare-function org-pcomplete-initial "org-pcomplete" ())
(declare-function org-table-align "org-table-align" ())
(declare-function org-table-shrink "org-table-fold" (&optional begin end))
(declare-function org-install-agenda-files-menu "org-agenda-files" ())
(declare-function org-macro-initialize-templates "org-macro" (&optional default))
(declare-function org-display-inline-images "org-preview-image"
                  (&optional include-linked refresh beg end))
(declare-function org-setup-yank-dnd-handlers "org-dnd" ())
(declare-function org-cycle-set-startup-visibility "org-cycle" ())
(declare-function org-fold--advice-edit-commands "org-fold" ())
(declare-function org-fold-initialize "org-fold" (&optional ellipsis))
(declare-function org-fold-show-all "org-fold" (&optional types))
(declare-function org-fold-reveal "org-fold" (&optional siblings))
(declare-function org-latex-preview "org-preview-latex" (&optional arg))
(declare-function org-set-font-lock-defaults "org-font-lock" ())

;;;###autoload
(define-derived-mode org-mode outline-mode "Org"
  "Outline-based notes management and organizer, alias
\"Carsten's outline-mode for keeping track of everything.\"

Org mode develops organizational tasks around a NOTES file which
contains information about projects as plain text.  Org mode is
implemented on top of Outline mode, which is ideal to keep the content
of large files well structured.  It supports ToDo items, deadlines and
time stamps, which magically appear in the diary listing of the Emacs
calendar.  Tables are easily created with a built-in table editor.
Plain text URL-like links connect to websites, emails (VM), Usenet
messages (Gnus), BBDB entries, and any files related to the project.
For printing and sharing of notes, an Org file (or a part of it)
can be exported as a structured ASCII or HTML file.

The following commands are available:

\\{org-mode-map}"
  (setq-local org-mode-loading t)
  ;; Force tab width - indentation is significant in lists, so we need
  ;; to make sure that it is consistent across configurations.
  (setq-local tab-width 8)
  (org-load-modules-maybe)
  (when org-agenda-file-menu-enabled
    (require 'org-agenda-files)
    (org-install-agenda-files-menu))
  (setq-local outline-regexp org-outline-regexp)
  (setq-local outline-level 'org-outline-level)
  ;; Initialize cache.
  (org-element-cache-reset)
  (when (and org-element-cache-persistent
             org-element-use-cache)
    (org-persist-load
     `((elisp org-element--cache) (version ,org-element-cache-version))
     (current-buffer)
     'match-hash :read-related t))
  (org-set-regexps-and-options)
  (add-to-invisibility-spec '(org-link))
  (require 'org-fold)
  (org-fold-initialize)
  (make-local-variable 'org-link-descriptive)
  (when (eq org-fold-core-style 'overlays) (add-to-invisibility-spec '(org-hide-block . t)))
  (require 'org-font-lock)
  (org-set-font-lock-defaults)
  ;; Calc embedded
  (setq-local calc-embedded-open-mode "# ")
  ;; Set syntax table.  Ensure that buffer-local changes to the syntax
  ;; table do not affect other Org buffers.
  (set-syntax-table (make-syntax-table org-mode-syntax-table))
  (setq-local font-lock-unfontify-region-function 'org-unfontify-region)
  ;; Activate before-change-function
  (setq-local org-table-may-need-update t)
  (add-hook 'before-change-functions 'org-before-change-function nil 'local)
  ;; Check for running clock before killing a buffer
  (add-hook 'kill-buffer-hook 'org-check-running-clock nil 'local)
  ;; Check for invisible edits.
  (org-fold--advice-edit-commands)
  ;; Initialize macros templates.
  (require 'org-macro)
  (org-macro-initialize-templates)
  ;; Initialize radio targets.
  (org-update-radio-target-regexp)
  ;; Indentation.
  (setq-local indent-line-function 'org-indent-line)
  (setq-local indent-region-function 'org-indent-region)
  ;; Filling and auto-filling.
  (org-setup-filling)
  ;; Comments.
  (org-setup-comments-handling)
  ;; Beginning/end of defun
  (setq-local beginning-of-defun-function 'org-backward-element)
  (setq-local end-of-defun-function
	      (lambda ()
		(if (not (org-at-heading-p))
		    (org-forward-element)
		  (org-forward-element)
		  (forward-char -1))))
  ;; Next error for sparse trees
  (setq-local next-error-function 'org-occur-next-match)
  ;; Make commit log messages from Org documents easier.
  (setq-local add-log-current-defun-function #'org-add-log-current-headline)
  ;; Make sure dependence stuff works reliably, even for users who set it
  ;; too late :-(
  (if (bound-and-true-p org-enforce-todo-dependencies)
      (add-hook 'org-blocker-hook
	        'org-block-todo-from-children-or-siblings-or-parent)
    (remove-hook 'org-blocker-hook
		 'org-block-todo-from-children-or-siblings-or-parent))
  (if (bound-and-true-p org-enforce-todo-checkbox-dependencies)
      (add-hook 'org-blocker-hook
	        'org-block-todo-from-checkboxes)
    (remove-hook 'org-blocker-hook
		 'org-block-todo-from-checkboxes))

  ;; Align options lines
  (setq-local
   align-mode-rules-list
   '((org-in-buffer-settings
      (regexp . "^[ \t]*#\\+[A-Z_]+:\\(\\s-*\\)\\S-+")
      (modes . '(org-mode)))))

  ;; Setup the pcomplete hooks
  (setq-local pcomplete-command-completion-function #'org-pcomplete-initial)
  (setq-local pcomplete-command-name-function #'org-command-at-point)
  (setq-local pcomplete-default-completion-function #'ignore)
  (setq-local pcomplete-parse-arguments-function #'org-parse-arguments)
  (setq-local pcomplete-termination-string "")
  (add-hook 'completion-at-point-functions
            #'pcomplete-completions-at-point nil t)
  (setq-local buffer-face-mode-face 'org-default)

  ;; `thing-at-point' support
  (when (boundp 'thing-at-point-provider-alist)
    (setq-local thing-at-point-provider-alist
                (cons '(url . org--link-at-point)
                      thing-at-point-provider-alist)))
  (when (boundp 'forward-thing-provider-alist)
    (setq-local forward-thing-provider-alist
                (cons '(url . org-next-link)
                      forward-thing-provider-alist)))
  (when (boundp 'bounds-of-thing-at-point-provider-alist)
    (setq-local bounds-of-thing-at-point-provider-alist
                (cons '(url . org--bounds-of-link-at-point)
                      bounds-of-thing-at-point-provider-alist)))

  ;; If empty file that did not turn on Org mode automatically, make
  ;; it to.
  (when (and org-insert-mode-line-in-empty-file
	     (called-interactively-p 'any)
	     (= (point-min) (point-max)))
    (insert "#    -*- mode: org -*-\n\n"))
  (unless org-inhibit-startup
    (when (or org-startup-align-all-tables org-startup-shrink-all-tables)
      (require 'org-table-align)
      (require 'org-table-fold)
      (org-table-map-tables
       (cond ((and org-startup-align-all-tables
		   org-startup-shrink-all-tables)
	      (lambda () (org-table-align) (org-table-shrink)))
	     (org-startup-align-all-tables #'org-table-align)
	     (t #'org-table-shrink))
       t))
    ;; Suppress modification hooks to speed up the startup.
    ;; However, do it only when text properties/overlays, but not
    ;; buffer text are actually modified.  We still need to track text
    ;; modifications to make cache updates work reliably.
    (org-unmodified
     (when org-startup-with-beamer-mode (org-beamer-mode))
     (when org-startup-with-inline-images
       (require 'org-preview-image)
       (org-display-inline-images))
     (when org-startup-with-latex-preview
       (require 'org-preview-latex)
       (org-latex-preview '(16)))
     (unless org-inhibit-startup-visibility-stuff
       (require 'org-cycle)
       (org-cycle-set-startup-visibility))
     (when org-startup-truncated (setq truncate-lines t))
     (when org-startup-numerated (require 'org-num) (org-num-mode 1))
     (when org-startup-indented (require 'org-indent) (org-indent-mode 1))))

  ;; Add a custom keymap for `visual-line-mode' so that activating
  ;; this minor mode does not override Org's keybindings.
  ;; FIXME: Probably `visual-line-mode' should take care of this.
  (let ((oldmap (cdr (assoc 'visual-line-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap [remap move-beginning-of-line] nil)
    (define-key newmap [remap move-end-of-line] nil)
    (define-key newmap [remap kill-line] nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(visual-line-mode . ,newmap) minor-mode-overriding-map-alist))

  ;; Activate `org-table-header-line-mode'
  (when org-table-header-line-p
    (org-table-header-line-mode 1))
  (setq-local org-mode-loading nil)

  ;; `yank-media' handler and DND support.
  (require 'org-dnd)
  (org-setup-yank-dnd-handlers)
  ;; `occur' support.
  (org-setup-occur)
  (org-setup-bookmark)
  (org-setup-saveplace)
  (org-setup-imenu)
  ;; Remove folds when changing major mode
  (add-hook 'change-major-mode-hook
            #'org-fold-show-all 'append 'local))

(when (fboundp 'abbrev-table-put)
  (abbrev-table-put org-mode-abbrev-table
		    :parents (list text-mode-abbrev-table)))


;;; Define global Org mode menu

(when (< emacs-major-version 28)  ; preloaded in Emacs 28
  (require 'easymenu))

(defsubst org-in-subtree-not-table-p ()
  "Are we in a subtree and not in a table?"
  (and (not (org-before-first-heading-p))
       (not (org-at-table-p))))

(defun org-create-customize-menu ()
  "Create a full customization menu for Org mode, insert it into the menu."
  (interactive)
  (org-load-modules-maybe)
  (org-require-autoloaded-modules)
  (easy-menu-change
   '("Org") "Customize"
   `(["Browse Org group" org-customize t]
     "--"
     ,(customize-menu-create 'org)
     ["Set" Custom-set t]
     ["Save" Custom-save t]
     ["Reset to Current" Custom-reset-current t]
     ["Reset to Saved" Custom-reset-saved t]
     ["Reset to Standard Settings" Custom-reset-standard t]))
  (message "\"Org\"-menu now contains full customization menu"))

;; Define the Org mode menus
(defvar org-org-menu)
(easy-menu-define org-org-menu org-mode-map "Org menu."
  `("Org"
    ("Show/Hide"
     ["Cycle Visibility" org-cycle :active (or (bobp) (org-at-heading-p t))]
     ["Cycle Global Visibility" org-shifttab :active (not (org-at-table-p))]
     ["Sparse Tree..." org-sparse-tree t]
     ["Reveal Context" org-fold-reveal t]
     ["Show All" org-fold-show-all t]
     "--"
     ["Subtree to indirect buffer" org-tree-to-indirect-buffer t])
    "--"
    ["New Heading" org-insert-heading t]
    ("Navigate Headings"
     ["Up" outline-up-heading t]
     ["Next" org-next-visible-heading t]
     ["Previous" outline-previous-visible-heading t]
     ["Next Same Level" outline-forward-same-level t]
     ["Previous Same Level" outline-backward-same-level t]
     "--"
     ["Jump" org-goto t])
    ("Edit Structure"
     ["Move Subtree Up" org-metaup (org-at-heading-p)]
     ["Move Subtree Down" org-metadown (org-at-heading-p)]
     "--"
     ["Copy Subtree"  org-copy-special (org-in-subtree-not-table-p)]
     ["Cut Subtree"  org-cut-special (org-in-subtree-not-table-p)]
     ["Paste Subtree"  org-paste-special (not (org-at-table-p))]
     "--"
     ["Clone subtree, shift time" org-clone-subtree-with-time-shift t]
     "--"
     ["Copy visible text"  org-copy-visible t]
     "--"
     ["Promote Heading" org-metaleft (org-in-subtree-not-table-p)]
     ["Promote Subtree" org-shiftmetaleft (org-in-subtree-not-table-p)]
     ["Demote Heading"  org-metaright (org-in-subtree-not-table-p)]
     ["Demote Subtree"  org-shiftmetaright (org-in-subtree-not-table-p)]
     "--"
     ["Sort Region/Children" org-sort t]
     "--"
     ["Convert to odd levels" org-convert-to-odd-levels t]
     ["Convert to odd/even levels" org-convert-to-oddeven-levels t])
    ("Editing"
     ["Emphasis..." org-emphasize t]
     ["Add block structure" org-insert-structure-template t]
     ["Edit Source Example" org-edit-special t]
     "--"
     ["Footnote new/jump" org-footnote-action t]
     ["Footnote extra" (org-footnote-action t) :active t :keys "C-u C-c C-x f"])
    ("Archive"
     ["Archive (default method)" org-archive-subtree-default (org-in-subtree-not-table-p)]
     "--"
     ["Move Subtree to Archive file" org-archive-subtree (org-in-subtree-not-table-p)]
     ["Toggle ARCHIVE tag" org-toggle-archive-tag (org-in-subtree-not-table-p)]
     ["Move subtree to Archive sibling" org-archive-to-archive-sibling (org-in-subtree-not-table-p)])
    "--"
    ("Hyperlinks"
     ["Store Link (Global)" org-store-link t]
     ["Find existing link to here" org-occur-link-in-agenda-files t]
     ["Insert Link" org-insert-link t]
     ["Follow Link" org-open-at-point t]
     "--"
     ["Next link" org-next-link t]
     ["Previous link" org-previous-link t]
     "--"
     ["Descriptive Links"
      org-toggle-link-display
      :style radio
      :selected org-descriptive-links
      ]
     ["Literal Links"
      org-toggle-link-display
      :style radio
      :selected (not org-descriptive-links)])
    "--"
    ("TODO Lists"
     ["TODO/DONE/-" org-todo t]
     ("Select keyword"
      ["Next keyword" org-shiftright (org-at-heading-p)]
      ["Previous keyword" org-shiftleft (org-at-heading-p)]
      ["Complete Keyword" pcomplete (assq :todo-keyword (org-context))]
      ["Next keyword set" org-shiftcontrolright (and (> (length org-todo-sets) 1) (org-at-heading-p))]
      ["Previous keyword set" org-shiftcontrolright (and (> (length org-todo-sets) 1) (org-at-heading-p))])
     ["Show TODO Tree" org-show-todo-tree :active t :keys "C-c / t"]
     ["Global TODO list" org-todo-list :active t :keys "\\[org-agenda] t"]
     "--"
     ["Enforce dependencies" (customize-variable 'org-enforce-todo-dependencies)
      :selected org-enforce-todo-dependencies :style toggle :active t]
     "Settings for tree at point"
     ["Do Children sequentially" org-toggle-ordered-property :style radio
      :selected (org-entry-get nil "ORDERED")
      :active org-enforce-todo-dependencies :keys "C-c C-x o"]
     ["Do Children parallel" org-toggle-ordered-property :style radio
      :selected (not (org-entry-get nil "ORDERED"))
      :active org-enforce-todo-dependencies :keys "C-c C-x o"]
     "--"
     ["Set Priority" org-priority t]
     ["Priority Up" org-shiftup t]
     ["Priority Down" org-shiftdown t]
     "--"
     ["Get news from all feeds" org-feed-update-all t]
     ["Go to the inbox of a feed..." org-feed-goto-inbox t]
     ["Customize feeds" (customize-variable 'org-feed-alist) t])
    ("TAGS and Properties"
     ["Set Tags" org-set-tags-command (not (org-before-first-heading-p))]
     ["Change tag in region" org-change-tag-in-region (use-region-p)]
     "--"
     ["Set property" org-set-property (not (org-before-first-heading-p))]
     ["Column view of properties" org-columns t]
     ["Insert Column View DBlock" org-columns-insert-dblock t])
    ("Dates and Scheduling"
     ["Timestamp" org-timestamp (not (org-before-first-heading-p))]
     ["Timestamp (inactive)" org-timestamp-inactive (not (org-before-first-heading-p))]
     ("Change Date"
      ["1 Day Later" org-shiftright (org-at-timestamp-p 'lax)]
      ["1 Day Earlier" org-shiftleft (org-at-timestamp-p 'lax)]
      ["1 ... Later" org-shiftup (org-at-timestamp-p 'lax)]
      ["1 ... Earlier" org-shiftdown (org-at-timestamp-p 'lax)])
     ["Compute Time Range" org-evaluate-time-range t]
     ["Schedule Item" org-schedule (not (org-before-first-heading-p))]
     ["Deadline" org-deadline (not (org-before-first-heading-p))]
     "--"
     ["Custom time format" org-toggle-timestamp-overlays
      :style radio :selected org-display-custom-times]
     "--"
     ["Goto Calendar" org-goto-calendar t]
     ["Date from Calendar" org-date-from-calendar t]
     "--"
     ["Start/Restart Timer" org-timer-start t]
     ["Pause/Continue Timer" org-timer-pause-or-continue t]
     ["Stop Timer" org-timer-pause-or-continue :active t :keys "C-u C-c C-x ,"]
     ["Insert Timer String" org-timer t]
     ["Insert Timer Item" org-timer-item t])
    ("Logging work"
     ["Clock in" org-clock-in :active t :keys "C-c C-x C-i"]
     ["Switch task" (lambda () (interactive) (org-clock-in '(4))) :active t :keys "C-u C-c C-x C-i"]
     ["Clock out" org-clock-out t]
     ["Clock cancel" org-clock-cancel t]
     "--"
     ["Mark as default task" org-clock-mark-default-task t]
     ["Clock in, mark as default" (lambda () (interactive) (org-clock-in '(16))) :active t :keys "C-u C-u C-c C-x C-i"]
     ["Goto running clock" org-clock-goto t]
     "--"
     ["Display times" org-clock-display t]
     ["Create clock table" org-clock-report t]
     "--"
     ["Record DONE time"
      (progn (setq org-log-done (not org-log-done))
	     (message "Switching to %s will %s record a timestamp"
		      (car org-done-keywords)
		      (if org-log-done "automatically" "not")))
      :style toggle :selected org-log-done])
    "--"
    ["Agenda Command..." org-agenda t]
    ["Set Restriction Lock" org-agenda-set-restriction-lock t]
    ("File List for Agenda")
    ("Special views current file"
     ["TODO Tree"  org-show-todo-tree t]
     ["Check Deadlines" org-check-deadlines t]
     ["Tags/Property tree" org-match-sparse-tree t])
    "--"
    ["Export/Publish..." org-export-dispatch t]
    ("LaTeX"
     ["Org CDLaTeX mode" org-cdlatex-mode :active (require 'cdlatex nil t)
      :style toggle :selected org-cdlatex-mode]
     ["Insert Environment" cdlatex-environment (fboundp 'cdlatex-environment)]
     ["Insert math symbol" cdlatex-math-symbol (fboundp 'cdlatex-math-symbol)]
     ["Modify math symbol" org-cdlatex-math-modify
      (org-inside-LaTeX-fragment-p)]
     ["Insert citation" org-reftex-citation t])
    "--"
    ("Documentation"
     ["Show Version" org-version t]
     ["Info Documentation" org-info t]
     ["Browse Org News" org-browse-news t])
    ("Customize"
     ["Browse Org Group" org-customize t]
     "--"
     ["Expand This Menu" org-create-customize-menu t])
    ["Send bug report" org-submit-bug-report t]
    "--"
    ("Refresh/Reload"
     ["Refresh setup current buffer" org-mode-restart t]
     ["Reload Org (after update)" org-reload t]
     ["Reload Org uncompiled" (org-reload t) :active t :keys "C-u C-c C-x !"])))

(defvar org-tbl-menu)
(easy-menu-define org-tbl-menu org-mode-map "Org Table menu."
  '("Table"
    ["Align" org-ctrl-c-ctrl-c :active (org-at-table-p)]
    ["Next Field" org-cycle (org-at-table-p)]
    ["Previous Field" org-shifttab (org-at-table-p)]
    ["Next Row" org-return (org-at-table-p)]
    "--"
    ["Blank Field" org-table-blank-field (org-at-table-p)]
    ["Edit Field" org-table-edit-field (org-at-table-p)]
    ["Copy Field from Above" org-table-copy-down (org-at-table-p)]
    "--"
    ("Column"
     ["Move Column Left" org-metaleft (org-at-table-p)]
     ["Move Column Right" org-metaright (org-at-table-p)]
     ["Delete Column" org-shiftmetaleft (org-at-table-p)]
     ["Insert Column" org-shiftmetaright (org-at-table-p)]
     ["Shrink Column" org-table-toggle-column-width (org-at-table-p)])
    ("Row"
     ["Move Row Up" org-metaup (org-at-table-p)]
     ["Move Row Down" org-metadown (org-at-table-p)]
     ["Delete Row" org-shiftmetaup (org-at-table-p)]
     ["Insert Row" org-shiftmetadown (org-at-table-p)]
     ["Sort lines in region" org-table-sort-lines (org-at-table-p)]
     "--"
     ["Insert Hline" org-ctrl-c-minus (org-at-table-p)])
    ("Rectangle"
     ["Copy Rectangle" org-copy-special (org-at-table-p)]
     ["Cut Rectangle" org-cut-special (org-at-table-p)]
     ["Paste Rectangle" org-paste-special (org-at-table-p)]
     ["Fill Rectangle" org-table-wrap-region (org-at-table-p)])
    "--"
    ("Calculate"
     ["Set Column Formula" org-table-eval-formula (org-at-table-p)]
     ["Set Field Formula" (org-table-eval-formula '(4)) :active (org-at-table-p) :keys "C-u C-c ="]
     ["Edit Formulas" org-edit-special (org-at-table-p)]
     "--"
     ["Recalculate line" org-table-recalculate (org-at-table-p)]
     ["Recalculate all" (lambda () (interactive) (org-table-recalculate '(4))) :active (org-at-table-p) :keys "C-u C-c *"]
     ["Iterate all" (lambda () (interactive) (org-table-recalculate '(16))) :active (org-at-table-p) :keys "C-u C-u C-c *"]
     "--"
     ["Toggle Recalculate Mark" org-table-rotate-recalc-marks (org-at-table-p)]
     "--"
     ["Sum Column/Rectangle" org-table-sum
      (or (org-at-table-p) (use-region-p))]
     ["Which Column?" org-table-current-column (org-at-table-p)])
    ["Debug Formulas"
     org-table-toggle-formula-debugger
     :style toggle :selected (bound-and-true-p org-table-formula-debug)]
    ["Show Col/Row Numbers"
     org-table-toggle-coordinate-overlays
     :style toggle
     :selected (bound-and-true-p org-table-overlay-coordinates)]
    "--"
    ["Create" org-table-create (not (org-at-table-p))]
    ["Convert Region" org-table-convert-region (not (org-at-table-p 'any))]
    ["Import from File" org-table-import (not (org-at-table-p))]
    ["Export to File" org-table-export (org-at-table-p)]
    "--"
    ["Create/Convert from/to table.el" org-table-create-with-table.el t]
    "--"
    ("Plot"
     ["Ascii plot" orgtbl-ascii-plot :active (org-at-table-p) :keys "C-c \" a"]
     ["Gnuplot" org-plot/gnuplot :active (org-at-table-p) :keys "C-c \" g"])))

(provide 'org-mode)

;;; org-mode.el ends here
