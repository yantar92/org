;;; org-agenda-search-view.el --- Org search view  -*- lexical-binding: t; -*-

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

;; This file implements Org agenda view with entries matching text
;; search.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-agenda-mode)
(require 'org-agenda-search)

(defvar org-agenda-search-history nil)
(defvar org-agenda-last-search-view-search-was-boolean nil)

(defgroup org-agenda-search-view nil
  "Options concerning the search agenda view."
  :tag "Org Agenda Search View"
  :group 'org-agenda)

(defvaralias 'org-agenda-search-view-search-words-only
  'org-agenda-search-view-always-boolean)

(defcustom org-agenda-search-view-always-boolean nil
  "Non-nil means the search string is interpreted as individual parts.

The search string for search view can either be interpreted as a phrase,
or as a list of snippets that define a boolean search for a number of
strings.

When this is non-nil, the string will be split on whitespace, and each
snippet will be searched individually, and all must match in order to
select an entry.  A snippet is then a single string of non-white
characters, or a string in double quotes, or a regexp in {} braces.
If a snippet is preceded by \"-\", the snippet must *not* match.
\"+\" is syntactic sugar for positive selection.  Each snippet may
be found as a full word or a partial word, but see the variable
`org-agenda-search-view-force-full-words'.

When this is nil, search will look for the entire search phrase as one,
with each space character matching any amount of whitespace, including
line breaks.

Even when this is nil, you can still switch to Boolean search dynamically
by preceding the first snippet with \"+\" or \"-\".  If the first snippet
is a regexp marked with braces like \"{abc}\", this will also switch to
boolean search."
  :group 'org-agenda-search-view
  :version "24.1"
  :type 'boolean)

(defcustom org-agenda-search-view-force-full-words nil
  "Non-nil means, search words must be matches as complete words.
When nil, they may also match part of a word."
  :group 'org-agenda-search-view
  :version "24.1"
  :type 'boolean)

(defcustom org-agenda-search-view-max-outline-level 0
  "Maximum outline level to display in search view.
E.g. when this is set to 1, the search view will only
show headlines of level 1.  When set to 0, the default
value, don't limit agenda view by outline level."
  :group 'org-agenda-search-view
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'integer)

(declare-function org-add-archive-files "org-archive" (files))
;;;###autoload
(defun org-search-view (&optional todo-only string edit-at)
  "Show all entries that contain a phrase or words or regular expressions.

With optional prefix argument TODO-ONLY, only consider entries that are
TODO entries.  The argument STRING can be used to pass a default search
string into this function.  If EDIT-AT is non-nil, it means that the
user should get a chance to edit this string, with cursor at position
EDIT-AT.

The search string can be viewed either as a phrase that should be found as
is, or it can be broken into a number of snippets, each of which must match
in a Boolean way to select an entry.  The default depends on the variable
`org-agenda-search-view-always-boolean'.
Even if this is turned off (the default) you can always switch to
Boolean search dynamically by preceding the first word with  \"+\" or \"-\".

The default is a direct search of the whole phrase, where each space in
the search string can expand to an arbitrary amount of whitespace,
including newlines.

If using a Boolean search, the search string is split on whitespace and
each snippet is searched separately, with logical AND to select an entry.
Words prefixed with a minus must *not* occur in the entry.  Words without
a prefix or prefixed with a plus must occur in the entry.  Matching is
case-insensitive.  Words are enclosed by word delimiters (i.e. they must
match whole words, not parts of a word) if
`org-agenda-search-view-force-full-words' is set (default is nil).

Boolean search snippets enclosed by curly braces are interpreted as
regular expressions that must or (when preceded with \"-\") must not
match in the entry.  Snippets enclosed into double quotes will be taken
as a whole, to include whitespace.

- If the search string starts with an asterisk, search only in headlines.
- If (possibly after the leading star) the search string starts with an
  exclamation mark, this also means to look at TODO entries only, an effect
  that can also be achieved with a prefix argument.
- If (possibly after star and exclamation mark) the search string starts
  with a colon, this will mean that the (non-regexp) snippets of the
  Boolean search must match as full words.

This command searches the agenda files, and in addition the files
listed in `org-agenda-text-search-extra-files' unless a restriction lock
is active."
  (interactive "P")
  (when org-agenda-overriding-arguments
    (setq todo-only (car org-agenda-overriding-arguments)
	  string (nth 1 org-agenda-overriding-arguments)
	  edit-at (nth 2 org-agenda-overriding-arguments)))
  (let* ((full-words org-agenda-search-view-force-full-words)
	 (org-agenda-text-search-extra-files org-agenda-text-search-extra-files)
	 rtn rtnall files file pos
	 c neg re boolean words regexps+ regexps- hdl-only buffer)
    (unless (and (not edit-at)
		 (stringp string)
		 (string-match "\\S-" string))
      (setq string (read-string
		    (if org-agenda-search-view-always-boolean
			"[+-]Word/{Regexp} ...: "
		      "Phrase or [+-]Word/{Regexp} ...: ")
		    (cond
		     ((integerp edit-at) (cons string edit-at))
		     (edit-at string))
		    'org-agenda-search-history)))
    (catch 'exit
      (setq org-agenda-buffer-name
	    (org-agenda--get-buffer-name
	     (and org-agenda-sticky
		  (if (stringp string)
		      (format "*Org Agenda(%s:%s)*"
			      (or org-keys (or (and todo-only "S") "s"))
			      string)
		    (format "*Org Agenda(%s)*"
			    (or (and todo-only "S") "s"))))))
      (org-agenda-prepare "SEARCH")
      (org-compile-prefix-format 'search)
      (org-set-sorting-strategy 'search)
      (setq org-agenda-redo-command
	    (list 'org-search-view (if todo-only t nil)
		  (list 'if 'current-prefix-arg nil string)))
      (setq org-agenda-query-string string)
      (if (equal (string-to-char string) ?*)
	  (setq hdl-only t
		words (substring string 1))
	(setq words string))
      (when (equal (string-to-char words) ?!)
	(setq todo-only t
	      words (substring words 1)))
      (when (equal (string-to-char words) ?:)
	(setq full-words t
	      words (substring words 1)))
      (when (or org-agenda-search-view-always-boolean
		(member (string-to-char words) '(?- ?+ ?\{)))
	(setq boolean t))
      (setq words (split-string words))
      (let (www w)
	(while (setq w (pop words))
	  (while (and (string-match "\\\\\\'" w) words)
	    (setq w (concat (substring w 0 -1) " " (pop words))))
	  (push w www))
	(setq words (nreverse www) www nil)
	(while (setq w (pop words))
	  (when (and (string-match "\\`[-+]?{" w)
		     (not (string-match "}\\'" w)))
	    (while (and words (not (string-match "}\\'" (car words))))
	      (setq w (concat w " " (pop words))))
	    (setq w (concat w " " (pop words))))
	  (push w www))
	(setq words (nreverse www)))
      (setq org-agenda-last-search-view-search-was-boolean boolean)
      (when boolean
	(let (wds w)
	  (while (setq w (pop words))
	    (when (or (equal (substring w 0 1) "\"")
		      (and (> (length w) 1)
			   (member (substring w 0 1) '("+" "-"))
			   (equal (substring w 1 2) "\"")))
	      (while (and words (not (equal (substring w -1) "\"")))
		(setq w (concat w " " (pop words)))))
	    (and (string-match "\\`\\([-+]?\\)\"" w)
		 (setq w (replace-match "\\1" nil nil w)))
	    (and (equal (substring w -1) "\"") (setq w (substring w 0 -1)))
	    (push w wds))
	  (setq words (nreverse wds))))
      (if boolean
	  (mapc (lambda (w)
		  (setq c (string-to-char w))
		  (if (equal c ?-)
		      (setq neg t w (substring w 1))
		    (if (equal c ?+)
			(setq neg nil w (substring w 1))
		      (setq neg nil)))
		  (if (string-match "\\`{.*}\\'" w)
		      (setq re (substring w 1 -1))
		    (if full-words
			(setq re (concat "\\<" (regexp-quote (downcase w)) "\\>"))
		      (setq re (regexp-quote (downcase w)))))
		  (if neg (push re regexps-) (push re regexps+)))
		words)
	(push (mapconcat #'regexp-quote words "\\s-+")
	      regexps+))
      (setq regexps+ (sort regexps+ (lambda (a b) (> (length a) (length b)))))
      (setq files (org-agenda-files nil 'ifmode))
      ;; Add `org-agenda-text-search-extra-files' unless there is some
      ;; restriction.
      (when (eq (car org-agenda-text-search-extra-files) 'agenda-archives)
	(pop org-agenda-text-search-extra-files)
	(unless (get 'org-agenda-files 'org-restrict)
	  (setq files (org-add-archive-files files))))
      ;; Uniquify files.  However, let `org-check-agenda-file' handle
      ;; non-existent ones.
      (setq files (cl-remove-duplicates
		   (append files org-agenda-text-search-extra-files)
		   :test (lambda (a b)
			   (and (file-exists-p a)
				(file-exists-p b)
				(file-equal-p a b))))
	    rtnall nil)
      (while (setq file (pop files))
	(org-check-agenda-file file)
	(setq buffer (if (file-exists-p file)
			 (org-get-agenda-file-buffer file)
		       (error "No such file %s" file)))
	(unless buffer
	  ;; If file does not exist, make sure an error message is sent
	  (setq rtn (list (format "ORG-AGENDA-ERROR: No such org-file %s"
				  file))))
	(with-current-buffer buffer
          (unless (derived-mode-p 'org-mode)
            (error "Agenda file %s is not in Org mode" file))
          (save-excursion
            (save-restriction
              (if (eq buffer org-agenda-restrict)
		  (narrow-to-region org-agenda-restrict-begin
				    org-agenda-restrict-end)
		(widen))
              (setq rtn (org-agenda-get-regexps
                         regexps+ regexps- hdl-only todo-only
                         org-agenda-search-view-max-outline-level)))))
	(setq rtnall (append rtnall rtn)))
      (org-agenda--insert-overriding-header
	(with-temp-buffer
	  (insert "Search words: ")
	  (add-text-properties (point-min) (1- (point))
			       (list 'face 'org-agenda-structure))
	  (setq pos (point))
	  (insert string "\n")
	  (add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure-filter))
	  (setq pos (point))
	  (unless org-agenda-multi
	    (insert (substitute-command-keys "\\<org-agenda-mode-map>\
Press `\\[org-agenda-manipulate-query-add]', \
`\\[org-agenda-manipulate-query-subtract]' to add/sub word, \
`\\[org-agenda-manipulate-query-add-re]', \
`\\[org-agenda-manipulate-query-subtract-re]' to add/sub regexp, \
`\\[universal-argument] \\[org-agenda-redo]' for a fresh search\n"))
	    (add-text-properties pos (1- (point))
				 (list 'face 'org-agenda-structure-secondary)))
	  (buffer-string)))
      (org-agenda-mark-header-line (point-min))
      (when rtnall
	(insert (org-agenda-finalize-entries rtnall 'search) "\n"))
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (add-text-properties (point-min) (point-max)
			   `(org-agenda-type search
					     org-last-args (,todo-only ,string ,edit-at)
					     org-redo-cmd ,org-agenda-redo-command
					     org-series-cmd ,org-cmd))
      (org-agenda-finalize)
      (setq buffer-read-only t))))

(provide 'org-agenda-search-view)

;;; org-agenda-search-view.el ends here
