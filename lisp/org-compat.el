;;; org-compat.el --- Compatibility Code for Older Emacsen -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2024 Free Software Foundation, Inc.

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

;; This file contains code needed for compatibility with older
;; versions of GNU Emacs and integration with other packages.

;;; Code:


(require 'cl-lib)
(require 'org-macs)

(eval-when-compile (require 'subr-x))  ; Emacs < 28

;; We rely on org-compat when generating Org version.  Checking Org
;; version here will interfere with Org build process.
;; (org-assert-version)

(declare-function org-agenda-diary-entry "org-agenda")
(declare-function org-agenda-maybe-redo "org-agenda" ())
(declare-function org-agenda-set-restriction-lock "org-agenda" (&optional type))
(declare-function org-agenda-remove-restriction-lock "org-agenda" (&optional noupdate))
(declare-function org-calendar-goto-agenda "org-agenda" ())
(declare-function org-align-tags "org" (&optional all))
(declare-function org-at-heading-p "org" (&optional ignored))
(declare-function org-at-table.el-p "org-table" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-element-at-point "org-element" (&optional pom cached-only))
(declare-function org-element-at-point-no-context "org-element" (&optional pom))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-lineage "org-element-ast" (blob &optional types with-self))
(declare-function org-element-type "org-element-ast" (node &optional anonymous))
(declare-function org-element-type-p "org-element-ast" (node types))
(declare-function org-element-property "org-element-ast" (property node))
(declare-function org-element-begin "org-element" (node))
(declare-function org-element-end "org-element" (node))
(declare-function org-element-contents-begin "org-element" (node))
(declare-function org-element-contents-end "org-element" (node))
(declare-function org-element-post-affiliated "org-element" (node))
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading))
(declare-function org-get-heading "org" (&optional no-tags no-todo no-priority no-comment))
(declare-function org-get-tags "org" (&optional pos local))
(declare-function org-fold-hide-block-toggle "org-fold" (&optional force no-error element))
(declare-function org-link-display-format "ol" (s))
(declare-function org-link-set-parameters "ol" (type &rest rest))
(declare-function org-log-into-drawer "org" ())
(declare-function org-make-tag-string "org" (tags))
(declare-function org-next-visible-heading "org" (arg))
(declare-function org-reduced-level "org" (l))
(declare-function org-return "org" (&optional indent arg interactive))
(declare-function org-fold-show-context "org-fold" (&optional key))
(declare-function org-table-end "org-table" (&optional table-type))
(declare-function outline-next-heading "outline" ())
(declare-function speedbar-line-directory "speedbar" (&optional depth))
(declare-function table--at-cell-p "table" (position &optional object at-column))
(declare-function org-fold-folded-p "org-fold" (&optional pos spec-or-alias))
(declare-function org-fold-hide-sublevels "org-fold" (levels))
(declare-function org-fold-hide-subtree "org-fold" ())
(declare-function org-fold-region "org-fold" (from to flag &optional spec))
(declare-function org-fold-show-all "org-fold" (&optional types))
(declare-function org-fold-show-children "org-fold" (&optional level))
(declare-function org-fold-show-entry "org-fold" (&optional hide-drawers))
;; `org-string-equal-ignore-case' is in _this_ file but isn't at the
;; top-level.
(declare-function org-string-equal-ignore-case "org-compat" (string1 string2))

(defvar calendar-mode-map)
(defvar org-complex-heading-regexp)
(defvar org-agenda-diary-file)
(defvar org-agenda-overriding-restriction)
(defvar org-agenda-restriction-lock-overlay)
(defvar org-table-any-border-regexp)
(defvar org-table-dataline-regexp)
(defvar org-table-tab-recognizes-table.el)
(defvar org-table1-hline-regexp)
(defvar org-fold-core-style)

(require 'org-compat-emacs29)
(require 'org-compat-emacs28)
(require 'org-compat-emacs27)
;; Emacs 26 is no longer supported.  Caveat emptor.
(require 'org-compat-emacs26)

(require 'org-obsolete)


;;; Obsolete aliases (remove them after the next major release).

;;;; Functions and variables from previous releases now obsolete.



;;; Miscellaneous functions


;;; Region compatibility

(defvar org-ignore-region nil
  "Non-nil means temporarily disable the active region.")

(defun org-region-active-p ()
  "Non-nil when the region active.
Unlike to `use-region-p', this function also checks
`org-ignore-region'."
  (and (not org-ignore-region) (use-region-p)))

(defun org-cursor-to-region-beginning ()
  (when (and (org-region-active-p)
             (> (point) (region-beginning)))
    (exchange-point-and-mark)))


;;; Invisibility compatibility

(defun org-in-invisibility-spec-p (arg)
  "Is ARG a member of `buffer-invisibility-spec'?"
  (when (consp buffer-invisibility-spec)
    (member arg buffer-invisibility-spec)))

(defun org-move-to-column (column &optional force _buffer)
  "Move to column COLUMN.
Pass COLUMN and FORCE to `move-to-column'."
  (let ((buffer-invisibility-spec
         (if (listp buffer-invisibility-spec)
             (remove '(org-filtered) buffer-invisibility-spec)
           buffer-invisibility-spec)))
    (move-to-column column force)))

(defmacro org-find-library-dir (library)
  `(file-name-directory (or (locate-library ,library) "")))

(defun org-count-lines (s)
  "How many lines in string S?"
  (let ((start 0) (n 1))
    (while (string-match "\n" s start)
      (setq start (match-end 0) n (1+ n)))
    (when (and (> (length s) 0) (= (aref s (1- (length s))) ?\n))
      (setq n (1- n)))
    n))

(defun org-kill-new (string &rest args)
  (remove-text-properties 0 (length string) '(line-prefix t wrap-prefix t)
                          string)
  (apply 'kill-new string args))

;; `file-local-name' was added in Emacs 26.1.
(defalias 'org-babel-local-file-name
  (if (fboundp 'file-local-name)
      'file-local-name
    (lambda (file)
      "Return the local name component of FILE."
      (or (file-remote-p file 'localname) file))))

;;;###autoload
(defmacro org-check-version ()
  "Try very hard to provide sensible version strings."
  (let* ((org-dir        (org-find-library-dir "org"))
         (org-version.el (concat org-dir "org-version.el"))
         (org-fixup.el   (concat org-dir "../mk/org-fixup.el")))
    (if (require 'org-version org-version.el 'noerror)
        '(progn
           (autoload 'org-release     "org-version.el")
           (autoload 'org-git-version "org-version.el"))
      (if (require 'org-fixup org-fixup.el 'noerror)
          '(org-fixup)
        ;; provide fallback definitions and complain
        (warn "Could not define org version correctly.  Check installation!")
        '(progn
           (defun org-release () "N/A")
           (defun org-git-version () "N/A !!check installation!!"))))))



;;; Integration with and fixes for other packages

(defgroup org-imenu-and-speedbar nil
  "Options concerning imenu and speedbar in Org mode."
  :tag "Org Imenu and Speedbar"
  :group 'org-structure)

(defcustom org-imenu-depth 2
  "The maximum level for Imenu access to Org headlines.
This also applied for speedbar access."
  :type 'integer)

;;;; Imenu

(defvar-local org-imenu-markers nil
  "All markers currently used by Imenu.")

(defun org-imenu-get-tree ()
  "Produce the index for Imenu."
  (dolist (x org-imenu-markers) (move-marker x nil))
  (setq org-imenu-markers nil)
  (org-with-wide-buffer
   (goto-char (point-max))
   (let* ((re (concat "^" (org-get-limited-outline-regexp)))
	  (subs (make-vector (1+ org-imenu-depth) nil))
	  (last-level 0))
     (while (re-search-backward re nil t)
       (let ((level (org-reduced-level (funcall outline-level)))
	     (headline (org-no-properties
			(org-link-display-format (org-get-heading t t t t)))))
	 (when (and (<= level org-imenu-depth) (org-string-nw-p headline))
	   (let* ((m (point-marker))
		  (item (propertize headline 'org-imenu-marker m 'org-imenu t)))
	     (push m org-imenu-markers)
             (push (cons item m) (aref subs level))
             (unless (>= level last-level)
	       (push (cons item
			   (cl-mapcan #'identity (cl-subseq subs (1+ level))))
		     (aref subs level))
	       (cl-loop for i from (1+ level) to org-imenu-depth
			do (aset subs i nil)))
	     (setq last-level level)))))
     (aref subs 1))))

(eval-after-load 'imenu
  '(progn
     (add-hook 'imenu-after-jump-hook
	       (lambda ()
		 (when (derived-mode-p 'org-mode)
		   (org-fold-show-context 'org-goto))))
     (add-hook 'org-mode-hook
	       (lambda ()
		 (setq imenu-create-index-function 'org-imenu-get-tree)))))

;;;; Speedbar

(defvar org-speedbar-restriction-lock-overlay (make-overlay 1 1)
  "Overlay marking the agenda restriction line in speedbar.")
(overlay-put org-speedbar-restriction-lock-overlay
	     'face 'org-agenda-restriction-lock)
(overlay-put org-speedbar-restriction-lock-overlay
	     'help-echo "Agendas are currently limited to this item.")
(delete-overlay org-speedbar-restriction-lock-overlay)

(defun org-speedbar-set-agenda-restriction ()
  "Restrict future agenda commands to the location at point in speedbar.
If there is already a restriction lock at the location, remove it.

To get rid of the restriction, use `\\[org-agenda-remove-restriction-lock]'."
  (interactive)
  (require 'org-agenda)
  (let (p m tp np dir txt)
    (cond
     ((setq p (text-property-any (line-beginning-position) (line-end-position)
				 'org-imenu t))
      (setq m (get-text-property p 'org-imenu-marker))
      (with-current-buffer (marker-buffer m)
	(goto-char m)
	(if (and org-agenda-overriding-restriction
		 (member org-agenda-restriction-lock-overlay
			 (overlays-at (point))))
	    (org-agenda-remove-restriction-lock 'noupdate)
	  (org-agenda-set-restriction-lock 'subtree))))
     ((setq p (text-property-any (line-beginning-position) (line-end-position)
				 'speedbar-function 'speedbar-find-file))
      (setq tp (previous-single-property-change
		(1+ p) 'speedbar-function)
	    np (next-single-property-change
		tp 'speedbar-function)
	    dir (speedbar-line-directory)
	    txt (buffer-substring-no-properties (or tp (point-min))
						(or np (point-max))))
      (with-current-buffer (find-file-noselect
			    (let ((default-directory dir))
			      (expand-file-name txt)))
	(unless (derived-mode-p 'org-mode)
	  (user-error "Cannot restrict to non-Org mode file"))
	(org-agenda-set-restriction-lock 'file)))
     (t (user-error "Don't know how to restrict Org mode agenda")))
    (move-overlay org-speedbar-restriction-lock-overlay
                  (line-beginning-position) (line-end-position))
    (setq current-prefix-arg nil)
    (org-agenda-maybe-redo)))

(defvar speedbar-file-key-map)
(declare-function speedbar-add-supported-extension "speedbar" (extension))
(eval-after-load 'speedbar
  '(progn
     (speedbar-add-supported-extension ".org")
     (define-key speedbar-file-key-map "<" 'org-speedbar-set-agenda-restriction)
     (define-key speedbar-file-key-map "\C-c\C-x<" 'org-speedbar-set-agenda-restriction)
     (define-key speedbar-file-key-map ">" 'org-agenda-remove-restriction-lock)
     (define-key speedbar-file-key-map "\C-c\C-x>" 'org-agenda-remove-restriction-lock)
     (add-hook 'speedbar-visiting-tag-hook
	       (lambda () (and (derived-mode-p 'org-mode) (org-fold-show-context 'org-goto))))))

;;;; Add Log

(defun org-add-log-current-headline ()
  "Return current headline or nil.
This function ignores inlinetasks.  It is meant to be used as
`add-log-current-defun-function' value."
  (org-with-limited-levels (org-get-heading t t t t)))

;;;; Flyspell

(defun org--flyspell-object-check-p (element)
  "Non-nil when Flyspell can check object at point.
ELEMENT is the element at point."
  (let ((object (save-excursion
		  (when (looking-at-p "\\>") (backward-char))
		  (org-element-context element))))
    (cl-case (org-element-type object)
      ;; Prevent checks in links due to keybinding conflict with
      ;; Flyspell.
      ((citation citation-reference code entity export-snippet inline-babel-call
	         inline-src-block line-break latex-fragment link macro
	         statistics-cookie target timestamp verbatim)
       nil)
      (footnote-reference
       ;; Only in inline footnotes, within the definition.
       (and (eq (org-element-property :type object) 'inline)
	    (< (save-excursion
		 (goto-char (org-element-begin object))
		 (search-forward ":" nil t 2))
	       (point))))
      (otherwise t))))

(defun org-mode-flyspell-verify ()
  "Function used for `flyspell-generic-check-word-predicate'."
  (if (org-at-heading-p)
      ;; At a headline or an inlinetask, check title only.
      (and (save-excursion (forward-line 0)
			   (and (let ((case-fold-search t))
				  (not (looking-at-p "\\*+ END[ \t]*$")))
				(let ((case-fold-search nil))
				  (looking-at org-complex-heading-regexp))))
	   (match-beginning 4)
	   (>= (point) (match-beginning 4))
	   (or (not (match-beginning 5))
	       (< (point) (match-beginning 5)))
           ;; Ignore checks in code, verbatim and others.
           (org--flyspell-object-check-p (org-element-at-point-no-context)))
    (let* ((element (org-element-at-point-no-context))
	   (post-affiliated (org-element-post-affiliated element)))
      (cond
       ;; Ignore checks in all affiliated keywords but captions.
       ((< (point) post-affiliated)
	(and (save-excursion
	       (forward-line 0)
	       (let ((case-fold-search t)) (looking-at "[ \t]*#\\+CAPTION:")))
	     (> (point) (match-end 0))
	     (org--flyspell-object-check-p element)))
       (t
	(cl-case (org-element-type element)
	  ((comment quote-section) t)
	  (comment-block
	   ;; Allow checks between block markers, not on them.
	   (and (> (line-beginning-position) post-affiliated)
		(save-excursion
		  (end-of-line)
		  (skip-chars-forward " \r\t\n")
		  (< (point) (org-element-end element)))))
	  ;; Arbitrary list of keywords where checks are meaningful.
	  ;; Make sure point is on the value part of the element.
	  (keyword
	   (and (member (org-element-property :key element)
			'("DESCRIPTION" "TITLE"))
		(save-excursion
		  (search-backward ":" (line-beginning-position) t))))
	  ;; Check is globally allowed in paragraphs verse blocks and
	  ;; table rows (after affiliated keywords) but some objects
	  ;; must not be affected.
	  ((paragraph table-row verse-block)
	   (let ((cbeg (org-element-contents-begin element))
		 (cend (org-element-contents-end element)))
	     (and cbeg (>= (point) cbeg) (< (point) cend)
		  (org--flyspell-object-check-p element))))))))))
(put 'org-mode 'flyspell-mode-predicate 'org-mode-flyspell-verify)

(defun org-remove-flyspell-overlays-in (beg end)
  "Remove flyspell overlays in region."
  (and (bound-and-true-p flyspell-mode)
       (fboundp 'flyspell-delete-region-overlays)
       (flyspell-delete-region-overlays beg end)))

(defvar flyspell-delayed-commands)
(eval-after-load 'flyspell
  '(add-to-list 'flyspell-delayed-commands 'org-self-insert-command))

;;;; Calendar

(defcustom org-calendar-to-agenda-key 'default
  "Key to be installed in `calendar-mode-map' for switching to the agenda.

The command `org-calendar-goto-agenda' will be bound to this key.

When set to `default', bind the function to `c', but only if it is
available in the Calendar keymap.  This is the default choice because
`c' can then be used to switch back and forth between agenda and calendar.

When nil, `org-calendar-goto-agenda' is not bound to any key."
  :group 'org-agenda
  :type '(choice
	  (const :tag "Bind to `c' if available" default)
	  (key-sequence :tag "Other binding")
	  (const :tag "No binding" nil))
  :safe (lambda (v) (or (symbolp v) (stringp v)))
  :package-version '(Org . "9.2"))

(defcustom org-calendar-insert-diary-entry-key [?i]
  "The key to be installed in `calendar-mode-map' for adding diary entries.
This option is irrelevant until `org-agenda-diary-file' has been configured
to point to an Org file.  When that is the case, the command
`org-agenda-diary-entry' will be bound to the key given here, by default
`i'.  In the calendar, `i' normally adds entries to `diary-file'.  So
if you want to continue doing this, you need to change this to a different
key."
  :group 'org-agenda
  :type 'sexp)

(defun org--setup-calendar-bindings ()
  "Bind Org functions in Calendar keymap."
  (pcase org-calendar-to-agenda-key
    (`nil nil)
    ((and key (pred stringp))
     (local-set-key (kbd key) #'org-calendar-goto-agenda))
    ((guard (not (lookup-key calendar-mode-map "c")))
     (local-set-key "c" #'org-calendar-goto-agenda))
    (_ nil))
  (when (and (boundp 'org-agenda-diary-file)
	     (not (eq org-agenda-diary-file 'diary-file)))
    (local-set-key org-calendar-insert-diary-entry-key
		   #'org-agenda-diary-entry)))

(eval-after-load 'calendar
  '(add-hook 'calendar-mode-hook #'org--setup-calendar-bindings))

;;;; Ecb

;; Make sure ecb shows the location if it was hidden
(advice-add 'ecb-method-clicked :after #'org--ecb-show-context)
(defun org--ecb-show-context (&rest _)
  "Make hierarchy visible when jumping into location from ECB tree buffer."
  (when (derived-mode-p 'org-mode)
    (org-fold-show-context)))

;;;; Simple

(defun org-mark-jump-unhide (&rest _)
  "Make the point visible with `org-show-context' after jumping to the mark."
  (when (and (derived-mode-p 'org-mode)
	     (org-invisible-p))
    (org-fold-show-context 'mark-goto)))

(advice-add 'pop-to-mark-command :after #'org-mark-jump-unhide)

(advice-add 'exchange-point-and-mark :after #'org-mark-jump-unhide)
(advice-add 'pop-global-mark :after #'org-mark-jump-unhide)

;;;; Session

;; Make "session.el" ignore our circular variable.
(defvar session-globals-exclude)
(eval-after-load 'session
  '(add-to-list 'session-globals-exclude 'org-mark-ring))

;;;; outline-mode

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

;; TODO: outline-headers-as-kill

;;;; Speed commands

(provide 'org-compat)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-compat.el ends here
