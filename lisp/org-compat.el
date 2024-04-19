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



;;; Invisibility compatibility

(defun org-move-to-column (column &optional force _buffer)
  "Move to column COLUMN.
Pass COLUMN and FORCE to `move-to-column'."
  (let ((buffer-invisibility-spec
         (if (listp buffer-invisibility-spec)
             (remove '(org-filtered) buffer-invisibility-spec)
           buffer-invisibility-spec)))
    (move-to-column column force)))

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



;;; Integration with and fixes for other packages

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
