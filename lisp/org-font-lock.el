;;; org-font-lock.el --- Org mode's fontification  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2022 Free Software Foundation, Inc.

;; Homepage: https://orgmode.org

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

;; This file contains default fontification setting for Org mode.

;;; Code:

(require 'org-faces)
(require 'org-font-lock-core)

(declare-function org-element-property "org-element" (property element))
(declare-function org-element-link-parser "org-element" ())

(defvar org-element-paragraph-separate)

(defgroup org-appearance nil
  "Settings for Org mode appearance."
  :tag "Org Appearance"
  :group 'org)

(defvar org-font-lock-hook nil
  "Functions to be called for special font lock stuff.")

(defvaralias 'org-activate-links 'org-highlight-links)
(defcustom org-highlight-links '(bracket angle plain radio tag date footnote)
  "Types of links that should be highlighted in Org files.

This is a list of symbols, each one of them leading to the
highlighting of a certain link type.

You can still open links that are not highlighted.

In principle, it does not hurt to turn on highlighting for all
link types.  There may be a small gain when turning off unused
link types.  The types are:

bracket   The recommended [[link][description]] or [[link]] links with hiding.
angle     Links in angular brackets that may contain whitespace like
          <bbdb:Carsten Dominik>.
plain     Plain links in normal text, no whitespace, like https://gnu.org.
radio     Text that is matched by a radio target, see manual for details.
tag       Tag settings in a headline (link to tag search).
date      Time stamps (link to calendar).
footnote  Footnote labels.

If you set this variable during an Emacs session, use `org-mode-restart'
in the Org buffer so that the change takes effect."
  :group 'org-appearance
  :type '(set :greedy t
	      (const :tag "Double bracket links" bracket)
	      (const :tag "Angular bracket links" angle)
	      (const :tag "Plain text links" plain)
	      (const :tag "Radio target matches" radio)
	      (const :tag "Tags" tag)
	      (const :tag "Timestamps" date)
	      (const :tag "Footnotes" footnote)))

(defcustom org-level-color-stars-only nil
  "Non-nil means fontify only the stars in each headline.
When nil, the entire headline is fontified.
Changing it requires restart of `font-lock-mode' to become effective
also in regions already fontified."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-hide-leading-stars nil
  "Non-nil means hide the first N-1 stars in a headline.
This works by using the face `org-hide' for these stars.  This
face is white for a light background, and black for a dark
background.  You may have to customize the face `org-hide' to
make this work.
Changing it requires restart of `font-lock-mode' to become effective
also in regions already fontified.
You may also set this on a per-file basis by adding one of the following
lines to the buffer:

   #+STARTUP: hidestars
   #+STARTUP: showstars"
  :group 'org-appearance
  :type 'boolean)

(defcustom org-hidden-keywords nil
  "List of symbols corresponding to keywords to be hidden in the Org buffer.
For example, a value \\='(title) for this list makes the document's title
appear in the buffer without the initial \"#+TITLE:\" part."
  :group 'org-appearance
  :package-version '(Org . "9.5")
  :type '(set (const :tag "#+AUTHOR" author)
	      (const :tag "#+DATE" date)
	      (const :tag "#+EMAIL" email)
	      (const :tag "#+SUBTITLE" subtitle)
	      (const :tag "#+TITLE" title)))


(defcustom org-fontify-todo-headline nil
  "Non-nil means change the face of a headline if it is marked as TODO.
Normally, only the TODO/DONE keyword indicates the state of a headline.
When this is non-nil, the headline after the keyword is set to the
`org-headline-todo' as an additional indication."
  :group 'org-appearance
  :package-version '(Org . "9.4")
  :type 'boolean
  :safe #'booleanp)

(defcustom org-fontify-done-headline t
  "Non-nil means change the face of a headline if it is marked DONE.
Normally, only the TODO/DONE keyword indicates the state of a headline.
When this is non-nil, the headline after the keyword is set to the
`org-headline-done' as an additional indication."
  :group 'org-appearance
  :package-version '(Org . "9.4")
  :type 'boolean)

(defcustom org-fontify-emphasized-text t
  "Non-nil means fontify *bold*, /italic/ and _underlined_ text.
Changing this variable requires a restart of Emacs to take effect."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-fontify-whole-heading-line nil
  "Non-nil means fontify the whole line for headings.
This is useful when setting a background color for the
org-level-* faces."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-fontify-whole-block-delimiter-line t
  "Non-nil means fontify the whole line for begin/end lines of blocks.
This is useful when setting a background color for the
org-block-begin-line and org-block-end-line faces."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-highlight-latex-and-related nil
  "Non-nil means highlight LaTeX related syntax in the buffer.
When non-nil, the value should be a list containing any of the
following symbols:
  `native'   Highlight LaTeX snippets and environments natively.
  `latex'    Highlight LaTeX snippets and environments.
  `script'   Highlight subscript and superscript.
  `entities' Highlight entities."
  :group 'org-appearance
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "No highlighting" nil)
	  (set :greedy t :tag "Highlight"
	       (const :tag "LaTeX snippets and environments (native)" native)
	       (const :tag "LaTeX snippets and environments" latex)
	       (const :tag "Subscript and superscript" script)
	       (const :tag "Entities" entities))))

(defcustom org-hide-emphasis-markers nil
  "Non-nil mean font-lock should hide the emphasis marker characters."
  :group 'org-appearance
  :type 'boolean
  :safe #'booleanp)

(defcustom org-hide-macro-markers nil
  "Non-nil mean font-lock should hide the brackets marking macro calls."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-pretty-entities nil
  "Non-nil means show entities as UTF8 characters.
When nil, the \\name form remains in the buffer."
  :group 'org-appearance
  :version "24.1"
  :type 'boolean)

(defcustom org-pretty-entities-include-sub-superscripts t
  "Non-nil means, pretty entity display includes formatting sub/superscripts."
  :group 'org-appearance
  :version "24.1"
  :type 'boolean)

(defvar org-font-lock-keywords nil)
(defvar org-font-lock-extra-keywords nil) ;Dynamically scoped.

(defvar org-font-lock-set-keywords-hook nil
  "Functions that can manipulate `org-font-lock-extra-keywords'.
This is called after `org-font-lock-extra-keywords' is defined, but before
it is installed to be used by font lock.  This can be useful if something
needs to be inserted at a specific position in the font-lock sequence.")

(defvar org-emph-face nil)

(defun org-do-emphasis-faces (limit)
  "Run through the buffer and emphasize strings."
  (let ((quick-re (format "\\([%s]\\|^\\)\\([~=*/_+]\\)"
			  (car org-emphasis-regexp-components))))
    (catch :exit
      (while (re-search-forward quick-re limit t)
	(let* ((marker (match-string 2))
	       (verbatim? (member marker '("~" "="))))
	  (when (save-excursion
		  (goto-char (match-beginning 0))
		  (and
		   ;; Do not match table hlines.
		   (not (and (equal marker "+")
			   (org-match-line
			    "[ \t]*\\(|[-+]+|?\\|\\+[-+]+\\+\\)[ \t]*$")))
		   ;; Do not match headline stars.  Do not consider
		   ;; stars of a headline as closing marker for bold
		   ;; markup either.
		   (not (and (equal marker "*")
			   (save-excursion
			     (forward-char)
			     (skip-chars-backward "*")
			     (looking-at-p org-outline-regexp-bol))))
		   ;; Match full emphasis markup regexp.
		   (looking-at (if verbatim? org-verbatim-re org-emph-re))
		   ;; Do not span over paragraph boundaries.
		   (not (save-match-data
                        (save-excursion
                          (goto-char (match-beginning 2))
                          (re-search-forward org-element-paragraph-separate
                                             (save-excursion
					       (goto-char (match-end 2))
					       (line-end-position))
                                             'noerror))))
		   ;; Do not span over cells in table rows.
		   (not (and (save-match-data (org-match-line "[ \t]*|"))
			   (string-match-p "|" (match-string 4))))))
	    (pcase-let ((`(,_ ,face ,_) (assoc marker org-emphasis-alist))
			(m (if org-hide-emphasis-markers 4 2)))
	      (font-lock-prepend-text-property
	       (match-beginning m) (match-end m) 'face face)
	      (when verbatim?
		(org-remove-flyspell-overlays-in
		 (match-beginning 0) (match-end 0))
		(remove-text-properties (match-beginning 2) (match-end 2)
					'(display t invisible t intangible t)))
	      (add-text-properties (match-beginning 2) (match-end 2)
				   '(font-lock-multiline t org-emphasis t))
	      (when (and org-hide-emphasis-markers
			 (not (org-at-comment-p)))
		(add-text-properties (match-end 4) (match-beginning 5)
				     '(invisible t))
		(add-text-properties (match-beginning 3) (match-end 3)
				     '(invisible t)))
	      (throw :exit t))))))))

(defun org-emphasize (&optional char)
  "Insert or change an emphasis, i.e. a font like bold or italic.
If there is an active region, change that region to a new emphasis.
If there is no region, just insert the marker characters and position
the cursor between them.
CHAR should be the marker character.  If it is a space, it means to
remove the emphasis of the selected region.
If CHAR is not given (for example in an interactive call) it will be
prompted for."
  (interactive)
  (let ((erc org-emphasis-regexp-components)
	(string "") beg end move s)
    (if (org-region-active-p)
	(setq beg (region-beginning)
	      end (region-end)
	      string (buffer-substring beg end))
      (setq move t))

    (unless char
      (message "Emphasis marker or tag: [%s]"
	       (mapconcat #'car org-emphasis-alist ""))
      (setq char (read-char-exclusive)))
    (if (equal char ?\s)
	(setq s ""
	      move nil)
      (unless (assoc (char-to-string char) org-emphasis-alist)
	(user-error "No such emphasis marker: \"%c\"" char))
      (setq s (char-to-string char)))
    (while (and (> (length string) 1)
		(equal (substring string 0 1) (substring string -1))
		(assoc (substring string 0 1) org-emphasis-alist))
      (setq string (substring string 1 -1)))
    (setq string (concat s string s))
    (when beg (delete-region beg end))
    (unless (or (bolp)
		(string-match (concat "[" (nth 0 erc) "\n]")
			      (char-to-string (char-before (point)))))
      (insert " "))
    (unless (or (eobp)
		(string-match (concat "[" (nth 1 erc) "\n]")
			      (char-to-string (char-after (point)))))
      (insert " ") (backward-char 1))
    (insert string)
    (and move (backward-char 1))))

(defconst org-nonsticky-props
  '(mouse-face highlight keymap invisible intangible help-echo org-linked-text htmlize-link))

(defsubst org-rear-nonsticky-at (pos)
  (add-text-properties (1- pos) pos (list 'rear-nonsticky org-nonsticky-props)))

(defun org-activate-links--overlays (limit)
  "Add link properties to links.
This includes angle, plain, and bracket links."
  (catch :exit
    (while (re-search-forward org-link-any-re limit t)
      (let* ((start (match-beginning 0))
	     (end (match-end 0))
	     (visible-start (or (match-beginning 3) (match-beginning 2)))
	     (visible-end (or (match-end 3) (match-end 2)))
	     (style (cond ((eq ?< (char-after start)) 'angle)
			  ((eq ?\[ (char-after (1+ start))) 'bracket)
			  (t 'plain))))
	(when (and (memq style org-highlight-links)
		   ;; Do not span over paragraph boundaries.
		   (not (string-match-p org-element-paragraph-separate
				        (match-string 0)))
		   ;; Do not confuse plain links with tags.
		   (not (and (eq style 'plain)
			     (let ((face (get-text-property
					  (max (1- start) (point-min)) 'face)))
			       (if (consp face) (memq 'org-tag face)
			         (eq 'org-tag face))))))
	  (let* ((link-object (save-excursion
				(goto-char start)
				(save-match-data (org-element-link-parser))))
		 (link (org-element-property :raw-link link-object))
		 (type (org-element-property :type link-object))
		 (path (org-element-property :path link-object))
                 (face-property (pcase (org-link-get-parameter type :face)
				  ((and (pred functionp) face) (funcall face path))
				  ((and (pred facep) face) face)
				  ((and (pred consp) face) face) ;anonymous
				  (_ 'org-link)))
		 (properties		;for link's visible part
		  (list 'mouse-face (or (org-link-get-parameter type :mouse-face)
					'highlight)
			'keymap (or (org-link-get-parameter type :keymap)
				    org-mouse-map)
			'help-echo (pcase (org-link-get-parameter type :help-echo)
				     ((and (pred stringp) echo) echo)
				     ((and (pred functionp) echo) echo)
				     (_ (concat "LINK: " link)))
			'htmlize-link (pcase (org-link-get-parameter type
								     :htmlize-link)
					((and (pred functionp) f) (funcall f))
					(_ `(:uri ,link)))
			'font-lock-multiline t)))
	    (org-remove-flyspell-overlays-in start end)
	    (org-rear-nonsticky-at end)
	    (if (not (eq 'bracket style))
		(progn
                  (add-face-text-property start end face-property)
		  (add-text-properties start end properties))
	      ;; Handle invisible parts in bracket links.
	      (remove-text-properties start end '(invisible nil))
	      (let ((hidden
		     (append `(invisible
			       ,(or (org-link-get-parameter type :display)
				    'org-link))
			     properties)))
		(add-text-properties start visible-start hidden)
                (add-face-text-property start end face-property)
		(add-text-properties visible-start visible-end properties)
		(add-text-properties visible-end end hidden)
		(org-rear-nonsticky-at visible-start)
		(org-rear-nonsticky-at visible-end)))
	    (let ((f (org-link-get-parameter type :activate-func)))
	      (when (functionp f)
		(funcall f start end path (eq style 'bracket))))
	    (throw :exit t)))))		;signal success
    nil))
(defun org-activate-links--text-properties (limit)
  "Add link properties to links.
This includes angle, plain, and bracket links."
  (catch :exit
    (while (re-search-forward org-link-any-re limit t)
      (let* ((start (match-beginning 0))
	     (end (match-end 0))
	     (visible-start (or (match-beginning 3) (match-beginning 2)))
	     (visible-end (or (match-end 3) (match-end 2)))
	     (style (cond ((eq ?< (char-after start)) 'angle)
			  ((eq ?\[ (char-after (1+ start))) 'bracket)
			  (t 'plain))))
	(when (and (memq style org-highlight-links)
		   ;; Do not span over paragraph boundaries.
		   (not (string-match-p org-element-paragraph-separate
				        (match-string 0)))
		   ;; Do not confuse plain links with tags.
		   (not (and (eq style 'plain)
			     (let ((face (get-text-property
					  (max (1- start) (point-min)) 'face)))
			       (if (consp face) (memq 'org-tag face)
			         (eq 'org-tag face))))))
	  (let* ((link-object (save-excursion
				(goto-char start)
				(save-match-data (org-element-link-parser))))
		 (link (org-element-property :raw-link link-object))
		 (type (org-element-property :type link-object))
		 (path (org-element-property :path link-object))
                 (face-property (pcase (org-link-get-parameter type :face)
				  ((and (pred functionp) face) (funcall face path))
				  ((and (pred facep) face) face)
				  ((and (pred consp) face) face) ;anonymous
				  (_ 'org-link)))
		 (properties		;for link's visible part
		  (list 'mouse-face (or (org-link-get-parameter type :mouse-face)
					'highlight)
			'keymap (or (org-link-get-parameter type :keymap)
				    org-mouse-map)
			'help-echo (pcase (org-link-get-parameter type :help-echo)
				     ((and (pred stringp) echo) echo)
				     ((and (pred functionp) echo) echo)
				     (_ (concat "LINK: " link)))
			'htmlize-link (pcase (org-link-get-parameter type
								     :htmlize-link)
					((and (pred functionp) f) (funcall f))
					(_ `(:uri ,link)))
			'font-lock-multiline t)))
	    (org-remove-flyspell-overlays-in start end)
	    (org-rear-nonsticky-at end)
	    (if (not (eq 'bracket style))
		(progn
                  (add-face-text-property start end face-property)
		  (add-text-properties start end properties))
              ;; Initialise folding when used ouside org-mode.
              (unless (or (derived-mode-p 'org-mode)
			  (and (org-fold-folding-spec-p 'org-link-description)
                               (org-fold-folding-spec-p 'org-link)))
                (org-fold-initialize (or (and (stringp org-ellipsis) (not (equal "" org-ellipsis)) org-ellipsis)
                                         "...")))
	      ;; Handle invisible parts in bracket links.
	      (let ((spec (or (org-link-get-parameter type :display)
			      'org-link)))
                (unless (org-fold-folding-spec-p spec)
                  (org-fold-add-folding-spec spec
                                             (cdr org-link--link-folding-spec)
                                             nil
                                             'append)
                  (org-fold-core-set-folding-spec-property spec :visible t))
                (org-fold-region start end nil 'org-link)
                (org-fold-region start end nil 'org-link-description)
                ;; We are folding the whole emphasised text with SPEC
                ;; first.  It makes everything invisible (or whatever
                ;; the user wants).
                (org-fold-region start end t spec)
                ;; The visible part of the text is folded using
                ;; 'org-link-description, which is forcing this part of
                ;; the text to be visible.
                (org-fold-region visible-start visible-end t 'org-link-description)
		(add-text-properties start end properties)
                (add-face-text-property start end face-property)
		(org-rear-nonsticky-at visible-start)
		(org-rear-nonsticky-at visible-end)))
	    (let ((f (org-link-get-parameter type :activate-func)))
	      (when (functionp f)
		(funcall f start end path (eq style 'bracket))))
	    (throw :exit t)))))		;signal success
    nil))
(defsubst org-activate-links (limit)
  "Add link properties to links.
This includes angle, plain, and bracket links."
  (if (eq org-fold-core-style 'text-properties)
      (org-activate-links--text-properties limit)
    (org-activate-links--overlays limit)))

(defun org-activate-code (limit)
  (when (re-search-forward "^[ \t]*\\(:\\(?: .*\\|$\\)\n?\\)" limit t)
    (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
    (remove-text-properties (match-beginning 0) (match-end 0)
			    '(display t invisible t intangible t))
    t))

(defcustom org-src-fontify-natively t
  "When non-nil, fontify code in code blocks.
See also the `org-block' face."
  :type 'boolean
  :version "26.1"
  :package-version '(Org . "8.3")
  :group 'org-appearance
  :group 'org-babel)

(defun org-fontify-meta-lines-and-blocks (limit)
  (condition-case nil
      (org-fontify-meta-lines-and-blocks-1 limit)
    (error (message "Org mode fontification error in %S at %d"
		    (current-buffer)
		    (line-number-at-pos)))))

(defun org-fontify-meta-lines-and-blocks-1 (limit)
  "Fontify #+ lines and blocks."
  (let ((case-fold-search t))
    (when (re-search-forward
	   (rx bol (group (zero-or-more (any " \t")) "#"
			  (group (group (or (seq "+" (one-or-more (any "a-zA-Z")) (optional ":"))
					    (any " \t")
					    eol))
				 (optional (group "_" (group (one-or-more (any "a-zA-Z"))))))
			  (zero-or-more (any " \t"))
			  (group (group (zero-or-more (not (any " \t\n"))))
				 (zero-or-more (any " \t"))
				 (group (zero-or-more any)))))
	   limit t)
      (let ((beg (match-beginning 0))
	    (end-of-beginline (match-end 0))
	    ;; Including \n at end of #+begin line will include \n
	    ;; after the end of block content.
	    (block-start (match-end 0))
	    (block-end nil)
	    (lang (match-string 7)) ; The language, if it is a source block.
	    (bol-after-beginline (line-beginning-position 2))
	    (dc1 (downcase (match-string 2)))
	    (dc3 (downcase (match-string 3)))
	    (whole-blockline org-fontify-whole-block-delimiter-line)
	    beg-of-endline end-of-endline nl-before-endline quoting block-type)
	(cond
	 ((and (match-end 4) (equal dc3 "+begin"))
	  ;; Truly a block
	  (setq block-type (downcase (match-string 5))
		;; Src, example, export, maybe more.
		quoting (member block-type org-protecting-blocks))
	  (when (re-search-forward
		 (rx-to-string `(group bol (or (seq (one-or-more "*") space)
					       (seq (zero-or-more (any " \t"))
						    "#+end"
						    ,(match-string 4)
						    word-end
						    (zero-or-more any)))))
		 ;; We look further than LIMIT on purpose.
		 nil t)
	    ;; We do have a matching #+end line.
	    (setq beg-of-endline (match-beginning 0)
		  end-of-endline (match-end 0)
		  nl-before-endline (1- (match-beginning 0)))
	    (setq block-end (match-beginning 0)) ; Include the final newline.
	    (when quoting
	      (org-remove-flyspell-overlays-in bol-after-beginline nl-before-endline)
	      (remove-text-properties beg end-of-endline
				      '(display t invisible t intangible t)))
	    (add-text-properties
	     beg end-of-endline '(font-lock-fontified t font-lock-multiline t))
	    (org-remove-flyspell-overlays-in beg bol-after-beginline)
	    (org-remove-flyspell-overlays-in nl-before-endline end-of-endline)
	    (cond
	     ((and lang (not (string= lang "")) org-src-fontify-natively)
	      (save-match-data
                (org-src-font-lock-fontify-block lang block-start block-end))
	      (add-text-properties bol-after-beginline block-end '(src-block t)))
	     (quoting
	      (add-text-properties
	       bol-after-beginline beg-of-endline
	       (list 'face
		     (list :inherit
			   (let ((face-name
				  (intern (format "org-block-%s" lang))))
			     (append (and (facep face-name) (list face-name))
				     '(org-block)))))))
	     ((not org-fontify-quote-and-verse-blocks))
	     ((string= block-type "quote")
	      (add-face-text-property
	       bol-after-beginline beg-of-endline 'org-quote t))
	     ((string= block-type "verse")
	      (add-face-text-property
	       bol-after-beginline beg-of-endline 'org-verse t)))
	    ;; Fontify the #+begin and #+end lines of the blocks
	    (add-text-properties
	     beg (if whole-blockline bol-after-beginline end-of-beginline)
	     '(face org-block-begin-line))
	    (unless (eq (char-after beg-of-endline) ?*)
	      (add-text-properties
	       beg-of-endline
	       (if whole-blockline
		   (let ((beg-of-next-line (1+ end-of-endline)))
		     (min (point-max) beg-of-next-line))
		 (min (point-max) end-of-endline))
	       '(face org-block-end-line)))
	    t))
	 ((member dc1 '("+title:" "+subtitle:" "+author:" "+email:" "+date:"))
	  (org-remove-flyspell-overlays-in
	   (match-beginning 0)
	   (if (equal "+title:" dc1) (match-end 2) (match-end 0)))
	  (add-text-properties
	   beg (match-end 3)
	   (if (member (intern (substring dc1 1 -1)) org-hidden-keywords)
	       '(font-lock-fontified t invisible t)
	     '(font-lock-fontified t face org-document-info-keyword)))
	  (add-text-properties
	   (match-beginning 6) (min (point-max) (1+ (match-end 6)))
	   (if (string-equal dc1 "+title:")
	       '(font-lock-fontified t face org-document-title)
	     '(font-lock-fontified t face org-document-info))))
	 ((string-prefix-p "+caption" dc1)
	  (org-remove-flyspell-overlays-in (match-end 2) (match-end 0))
	  (remove-text-properties (match-beginning 0) (match-end 0)
				  '(display t invisible t intangible t))
	  ;; Handle short captions
	  (save-excursion
	    (beginning-of-line)
	    (looking-at (rx (group (zero-or-more (any " \t"))
				   "#+caption"
				   (optional "[" (zero-or-more any) "]")
				   ":")
			    (zero-or-more (any " \t")))))
	  (add-text-properties (line-beginning-position) (match-end 1)
			       '(font-lock-fontified t face org-meta-line))
	  (add-text-properties (match-end 0) (line-end-position)
			       '(font-lock-fontified t face org-block))
	  t)
	 ((member dc3 '(" " ""))
	  ;; Just a comment, the plus was not there
	  (org-remove-flyspell-overlays-in beg (match-end 0))
	  (add-text-properties
	   beg (match-end 0)
	   '(font-lock-fontified t face font-lock-comment-face)))
	 (t ;; Just any other in-buffer setting, but not indented
	  (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
	  (remove-text-properties (match-beginning 0) (match-end 0)
				  '(display t invisible t intangible t))
	  (add-text-properties beg (match-end 0)
			       '(font-lock-fontified t face org-meta-line))
	  t))))))

(defun org-fontify-drawers (limit)
  "Fontify drawers."
  (when (re-search-forward org-drawer-regexp limit t)
    (add-text-properties (1- (match-beginning 1)) (1+ (match-end 1))
			 '(font-lock-fontified t face org-drawer))
    (org-remove-flyspell-overlays-in
     (line-beginning-position) (line-beginning-position 2))
    t))

(defun org-fontify-macros (limit)
  "Fontify macros."
  (when (re-search-forward "{{{\\([a-zA-Z][-a-zA-Z0-9_]*\\)" limit t)
    (let ((begin (match-beginning 0))
	  (opening-end (match-beginning 1)))
      (when (and (re-search-forward "\n[ \t]*\n\\|\\(}}}\\)" limit t)
		 (match-string 1))
	(let ((end (match-end 1))
	      (closing-start (match-beginning 1)))
	  (add-text-properties
	   begin end
	   '(font-lock-multiline t font-lock-fontified t face org-macro))
	  (org-remove-flyspell-overlays-in begin end)
	  (when org-hide-macro-markers
	    (add-text-properties begin opening-end '(invisible t))
	    (add-text-properties closing-start end '(invisible t)))
	  t)))))

(defun org-fontify-extend-region (beg end _old-len)
  (let ((end (if (progn (goto-char end) (looking-at-p "^[*#]"))
                 (1+ end) end))
        (begin-re "\\(\\\\\\[\\|\\(#\\+begin_\\|\\\\begin{\\)\\S-+\\)")
	(end-re "\\(\\\\\\]\\|\\(#\\+end_\\|\\\\end{\\)\\S-+\\)")
	(extend
         (lambda (r1 r2 dir)
	   (let ((re (replace-regexp-in-string
                      "\\(begin\\|end\\)" r1
		      (replace-regexp-in-string
                       "[][]" r2
		       (match-string-no-properties 0)))))
	     (re-search-forward (regexp-quote re) nil t dir)))))
    (goto-char beg)
    (back-to-indentation)
    (save-match-data
      (cond ((looking-at end-re)
	     (cons (or (funcall extend "begin" "[" -1) beg) end))
	    ((looking-at begin-re)
	     (cons beg (or (funcall extend "end" "]" 1) end)))
	    (t (cons beg end))))))

(defun org-activate-footnote-links (limit)
  "Add text properties for footnotes."
  (let ((fn (org-footnote-next-reference-or-definition limit)))
    (when fn
      (let* ((beg (nth 1 fn))
	     (end (nth 2 fn))
	     (label (car fn))
	     (referencep (/= (line-beginning-position) beg)))
	(when (and referencep (nth 3 fn))
	  (save-excursion
	    (goto-char beg)
	    (search-forward (or label "fn:"))
	    (org-remove-flyspell-overlays-in beg (match-end 0))))
	(add-text-properties beg end
			     (list 'mouse-face 'highlight
				   'keymap org-mouse-map
				   'help-echo
				   (if referencep "Footnote reference"
				     "Footnote definition")
				   'font-lock-fontified t
				   'font-lock-multiline t
				   'face 'org-footnote))))))

(defun org-activate-dates (limit)
  "Add text properties for dates."
  (when (and (re-search-forward org-tsr-regexp-both limit t)
	     (not (equal (char-before (match-beginning 0)) 91)))
    (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
    (add-text-properties (match-beginning 0) (match-end 0)
			 (list 'mouse-face 'highlight
			       'keymap org-mouse-map))
    (org-rear-nonsticky-at (match-end 0))
    (when org-display-custom-times
      ;; If it's a date range, activate custom time for second date.
      (when (match-end 3)
	(org-display-custom-time (match-beginning 3) (match-end 3)))
      (org-display-custom-time (match-beginning 1) (match-end 1)))
    t))

(defun org-activate-target-links (limit)
  "Add text properties for target matches."
  (when org-target-link-regexp
    (let ((case-fold-search t))
      ;; `org-target-link-regexp' matches one character before the
      ;; actual target.
      (unless (bolp) (forward-char -1))
      (when (re-search-forward org-target-link-regexp limit t)
	(org-remove-flyspell-overlays-in (match-beginning 1) (match-end 1))
	(add-text-properties (match-beginning 1) (match-end 1)
			     (list 'mouse-face 'highlight
				   'keymap org-mouse-map
				   'help-echo "Radio target link"
				   'org-linked-text t))
	(org-rear-nonsticky-at (match-end 1))
	t))))


(defun org-do-latex-and-related (limit)
  "Highlight LaTeX snippets and environments, entities and sub/superscript.
Stop at first highlighted object, if any.  Return t if some
highlighting was done, nil otherwise."
  (when (org-string-nw-p org-latex-and-related-regexp)
    (let ((latex-prefix-re (rx (or "$" "\\(" "\\[")))
	  (blank-line-re (rx (and "\n" (zero-or-more (or " " "\t")) "\n"))))
      (catch 'found
	(while (and (< (point) limit)
		    (re-search-forward org-latex-and-related-regexp nil t))
	  (cond
           ((>= (match-beginning 0) limit)
	    (throw 'found nil))
	   ((cl-some (lambda (f)
		       (memq f '(org-code org-verbatim underline
				          org-special-keyword)))
		     (save-excursion
		       (goto-char (1+ (match-beginning 0)))
		       (face-at-point nil t))))
	   ;; Try to limit false positives.  In this case, ignore
	   ;; $$...$$, \(...\), and \[...\] LaTeX constructs if they
	   ;; contain an empty line.
	   ((save-excursion
	      (goto-char (match-beginning 0))
	      (and (looking-at-p latex-prefix-re)
		   (save-match-data
		     (re-search-forward blank-line-re (1- (match-end 0)) t)))))
	   (t
	    (let* ((offset (if (memq (char-after (1+ (match-beginning 0)))
				     '(?_ ?^))
			       1
			     0))
		   (start (+ offset (match-beginning 0)))
		   (end (match-end 0)))
	      (if (memq 'native org-highlight-latex-and-related)
		  (org-src-font-lock-fontify-block "latex" start end)
		(font-lock-prepend-text-property start end
					 'face 'org-latex-and-related))
	      (add-text-properties (+ offset (match-beginning 0)) (match-end 0)
				   '(font-lock-multiline t))
	      (throw 'found t)))))
	nil))))

(defun org-activate-tags (limit)
  (when (re-search-forward org-tag-line-re limit t)
    (org-remove-flyspell-overlays-in (match-beginning 1) (match-end 1))
    (add-text-properties (match-beginning 1) (match-end 1)
			 (list 'mouse-face 'highlight
			       'keymap org-mouse-map))
    (org-rear-nonsticky-at (match-end 1))
    t))


(defun org-fontify-entities (limit)
  "Find an entity to fontify."
  (let (ee)
    (when org-pretty-entities
      (catch 'match
	;; "\_ "-family is left out on purpose.  Only the first one,
	;; i.e., "\_ ", could be fontified anyway, and it would be
	;; confusing when adding a second white space character.
	(while (re-search-forward
		"\\\\\\(there4\\|sup[123]\\|frac[13][24]\\|[a-zA-Z]+\\)\\($\\|{}\\|[^[:alpha:]\n]\\)"
		limit t)
	  (when (and (not (org-at-comment-p))
		     (setq ee (org-entity-get (match-string 1)))
		     (= (length (nth 6 ee)) 1))
	    (let* ((end (if (equal (match-string 2) "{}")
			    (match-end 2)
			  (match-end 1))))
	      (add-text-properties
	       (match-beginning 0) end
	       (list 'font-lock-fontified t))
	      (compose-region (match-beginning 0) end
			      (nth 6 ee) nil)
	      (backward-char 1)
	      (throw 'match t))))
	nil))))

(defun org-fontify-like-in-org-mode (s &optional odd-levels)
  "Fontify string S like in Org mode."
  (with-temp-buffer
    (insert s)
    (let ((org-odd-levels-only odd-levels))
      (org-mode)
      (org-font-lock-ensure)
      (if org-link-descriptive
          (org-link-display-format
           (buffer-string))
        (buffer-string)))))

;; (defun org-get-level-face (n)
;;   "Get the right face for match N in font-lock matching of headlines."
;;   (let* ((org-l0 (- (match-end 2) (match-beginning 1) 1))
;; 	 (org-l (if org-odd-levels-only (1+ (/ org-l0 2)) org-l0))
;; 	 (org-f (if org-cycle-level-faces
;; 		    (nth (% (1- org-l) org-n-level-faces) org-level-faces)
;; 		  (nth (1- (min org-l org-n-level-faces)) org-level-faces))))
;;     (cond
;;      ((eq n 1) (if org-hide-leading-stars 'org-hide org-f))
;;      ((eq n 2) org-f)
;;      (t (unless org-level-color-stars-only org-f)))))
(defun org-get-level-face (&optional element)
  "Get the right face for ELEMENT or `org-font-lock-current-element'."
  (let* ((org-font-lock-current-element (or element org-font-lock-current-element))
         (level (org-element-property :level org-font-lock-current-element)))
    (if org-cycle-level-faces
	(nth (% (1- level) org-n-level-faces) org-level-faces)
      (nth (1- (min level org-n-level-faces)) org-level-faces))))

(defun org-face-from-face-or-color (context inherit face-or-color)
  "Create a face list that inherits INHERIT, but sets the foreground color.
When FACE-OR-COLOR is not a string, just return it."
  (if (stringp face-or-color)
      (list :inherit inherit
	    (cdr (assoc context org-faces-easy-properties))
	    face-or-color)
    face-or-color))

(defun org-get-todo-face (kwd)
  "Get the right face for a TODO keyword KWD.
If KWD is a number, get the corresponding match group."
  (when (numberp kwd) (setq kwd (match-string kwd)))
  (or (org-face-from-face-or-color
       'todo 'org-todo (cdr (assoc kwd org-todo-keyword-faces)))
      (and (member kwd org-done-keywords) 'org-done)
      'org-todo))

(defun org-get-priority-face (priority)
  "Get the right face for PRIORITY.
PRIORITY is a character."
  (or (org-face-from-face-or-color
       'priority 'org-priority (cdr (assq priority org-priority-faces)))
      'org-priority))

(defun org-get-tag-face (tag)
  "Get the right face for TAG.
If TAG is a number, get the corresponding match group."
  (let ((tag (if (wholenump tag) (match-string tag) tag)))
    (or (org-face-from-face-or-color
	 'tag 'org-tag (cdr (assoc tag org-tag-faces)))
	'org-tag)))

(defvar org-priority-regexp) ; defined later in the file

(defun org-font-lock-add-priority-faces (limit)
  "Add the special priority faces."
  (while (re-search-forward (concat "^\\*+" org-priority-regexp) limit t)
    (let ((beg (match-beginning 1))
	  (end (1+ (match-end 2))))
      (add-face-text-property
       beg end
       (org-get-priority-face (string-to-char (match-string 2))))
      (add-text-properties
       beg end
       (list 'font-lock-fontified t)))))

(defun org-font-lock-add-tag-faces (limit)
  "Add the special tag faces."
  (when (and org-tag-faces org-tags-special-faces-re)
    (while (re-search-forward org-tags-special-faces-re limit t)
      (add-face-text-property
       (match-beginning 1)
       (match-end 1)
       (org-get-tag-face 1))
      (add-text-properties (match-beginning 1) (match-end 1)
			   (list 'font-lock-fontified t))
      (backward-char 1))))

(defun org-unfontify-region (beg end &optional _maybe_loudly)
  "Remove fontification and activation overlays from links."
  (font-lock-default-unfontify-region beg end)
  (let* ((buffer-undo-list t)
	 (inhibit-read-only t) (inhibit-point-motion-hooks t)
	 (inhibit-modification-hooks t)
	 deactivate-mark buffer-file-name buffer-file-truename)
    (decompose-region beg end)
    (remove-text-properties beg end
			    '(mouse-face t keymap t org-linked-text t
					 invisible t intangible t
					 org-emphasis t))
    (org-fold-core-update-optimisation beg end)
    (org-remove-font-lock-display-properties beg end)))

(defconst org-script-display  '(((raise -0.3) (height 0.7))
				((raise 0.3)  (height 0.7))
				((raise -0.5))
				((raise 0.5)))
  "Display properties for showing superscripts and subscripts.")

(defun org-remove-font-lock-display-properties (beg end)
  "Remove specific display properties that have been added by font lock.
The will remove the raise properties that are used to show superscripts
and subscripts."
  (let (next prop)
    (while (< beg end)
      (setq next (next-single-property-change beg 'display nil end)
	    prop (get-text-property beg 'display))
      (when (member prop org-script-display)
	(put-text-property beg next 'display nil))
      (setq beg next))))

(defun org-raise-scripts (limit)
  "Add raise properties to sub/superscripts."
  (when (and org-pretty-entities org-pretty-entities-include-sub-superscripts
	     (re-search-forward
	      (if (eq org-use-sub-superscripts t)
		  org-match-substring-regexp
		org-match-substring-with-braces-regexp)
	      limit t))
    (let* ((pos (point)) table-p comment-p
	   (mpos (match-beginning 3))
	   (emph-p (get-text-property mpos 'org-emphasis))
	   (link-p (get-text-property mpos 'mouse-face))
	   (keyw-p (eq 'org-special-keyword (get-text-property mpos 'face))))
      (goto-char (point-at-bol))
      (setq table-p (looking-at-p org-table-dataline-regexp)
	    comment-p (looking-at-p "^[ \t]*#[ +]"))
      (goto-char pos)
      ;; Handle a_b^c
      (when (member (char-after) '(?_ ?^)) (goto-char (1- pos)))
      (unless (or comment-p emph-p link-p keyw-p)
	(put-text-property (match-beginning 3) (match-end 0)
			   'display
			   (if (equal (char-after (match-beginning 2)) ?^)
			       (nth (if table-p 3 1) org-script-display)
			     (nth (if table-p 2 0) org-script-display)))
        (put-text-property (match-beginning 2) (match-end 3)
                           'org-emphasis t)
	(add-text-properties (match-beginning 2) (match-end 2)
			     (list 'invisible t))
	(when (and (eq (char-after (match-beginning 3)) ?{)
		   (eq (char-before (match-end 3)) ?}))
	  (add-text-properties (match-beginning 3) (1+ (match-beginning 3))
			       (list 'invisible t))
	  (add-text-properties (1- (match-end 3)) (match-end 3)
			       (list 'invisible t))))
      t)))

(defun org-font-lock-hook (limit)
  "Run `org-font-lock-hook' within LIMIT."
  (run-hook-with-args 'org-font-lock-hook limit))

(defun org-font-lock-set-defaults ()
  "Set font lock defaults for the current buffer."
  (setq org-font-lock-element-keywords
        `(
          ;; Node properties and keywords, including affiliated
          ;; keywords.
          (keyword
           (:key 'org-special-keyword t)
           (:value 'org-property-value t))
          (node-property
           (:key 'org-special-keyword t)
           (:value 'org-property-value t))
          ;; Headlines
          ,(if org-level-color-stars-only
               '(headline (:stars (org-get-level-face)))
             (if org-fontify-whole-heading-line
                 '(headline (:title-line-whole (org-get-level-face)))
               '(headline (:title-line (org-get-level-face)))))
          ,(when org-hide-leading-stars
             '(headline (:leading-stars 'org-hide t)))
          ;; Table lines
          (table-row (:line 'org-table t))
          ;; table.el table lines are not parsed.  Fall back to regexp
          ;; matching.
          (,(org-font-lock-cond
             (eq (org-element-property :type org-font-lock-current-element)
                 'table.el))
           ("^\\s-*\\(\\S-.*?\\)\\s-*$"
            ;; Search regex till :end.
            (progn
              (goto-char (org-element-property :begin org-font-lock-current-element))
              (org-element-property :end org-font-lock-current-element))
            (goto-char (org-element-property :end org-font-lock-current-element))
            (1 'org-table t)))
          ;; Column formulas.
          (table-cell
           ("\s-*=.*"
            nil nil
            (0 'org-formula prepend)))
          ;; Table recalc marks.
          (table-row
           ("^\\s-*|\\s-*\\([#*]\\)\\s-*|"
            nil nil
            (1 'org-formula prepend)))
          ;; Full line recalc marks.
          (table-row
           ("^\\s-*|\\(\\s-*\\([$!_^/]\\)\\s-*|.*\\)|"
            nil nil
            (1 'org-formula prepend)))
          ;; Table alignment markers.
          (table-cell
           ("\\s-*\\(<[lrc]?[0-9]*>\\)"
            nil nil
            (1 'org-formula prepend)))
          ;; Drawers.
          ,@(cl-loop for element-name in '(drawer property-drawer)
                     collect `(,element-name
                               (:begin-marker 'org-drawer t)
                               (:end-marker 'org-drawer t)))
          ;; Emphasis.
          ,@(when org-fontify-emphasized-text
              (cl-loop for (_ fontspec . _) in org-emphasis-alist
                       for element-name in '(bold italic underline verbatim code strike-through)
                       collect `(,element-name (:full-no-blank ',fontspec prepend))))
          ;; `org-emphasis' text property.
          ,@(when org-fontify-emphasized-text
              (cl-loop for  element-name in '(bold italic underline verbatim code strike-through)
                       ;; Note that we need "face nil" to trigger
                       ;; applying rest of the properties.
                       collect `(,element-name (:full-no-blank '(face nil org-emphasis t)))))
          ;; `org-hide-emphasis-markers'. Note that it can be switched
          ;; without reloading.  Hence, we calculate fontification
          ;; dynamically.
          ,@(when org-fontify-emphasized-text
              (cl-loop for element-name in '(bold italic underline verbatim code strike-through)
                       collect `(,element-name (:begin-marker (when org-hide-emphasis-markers '(face nil invisible t))))
                       collect `(,element-name (:end-marker (when org-hide-emphasis-markers '(face nil invisible t))))))
          ))
  (let ((org-font-lock-extra-keywords
	 (list
	  ;; Call the hook
	  '(org-font-lock-hook)
          '(org-font-lock-matcher)
	  ;; Headlines
	  ;; `(,(if org-fontify-whole-heading-line
	  ;;        "^\\(\\**\\)\\(\\* \\)\\(.*\n?\\)"
	  ;;      "^\\(\\**\\)\\(\\* \\)\\(.*\\)")
	  ;;   (1 (org-get-level-face 1))
	  ;;   (2 (org-get-level-face 2))
	  ;;   (3 (org-get-level-face 3)))
	  ;; ;; Table lines
	  ;; '("^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)"
	  ;;   (1 'org-table t))
	  ;; ;; Table internals
	  ;; '("^[ \t]*|\\(?:.*?|\\)? *\\(:?=[^|\n]*\\)" (1 'org-formula t))
	  ;; '("^[ \t]*| *\\([#*]\\) *|" (1 'org-formula t))
	  ;; '("^[ \t]*|\\( *\\([$!_^/]\\) *|.*\\)|" (1 'org-formula t))
	  ;; '("| *\\(<[lrc]?[0-9]*>\\)" (1 'org-formula t))
	  ;; ;; Properties
	  ;; (list org-property-re
	  ;;       '(1 'org-special-keyword t)
	  ;;       '(3 'org-property-value t))
	  ;; ;; Drawers
	  ;; '(org-fontify-drawers)
	  ;; Link related fontification.
	  '(org-activate-links)
	  (when (memq 'tag org-highlight-links) '(org-activate-tags (1 'org-tag prepend)))
	  (when (memq 'radio org-highlight-links) '(org-activate-target-links (1 'org-link t)))
	  (when (memq 'date org-highlight-links) '(org-activate-dates (0 'org-date t)))
	  (when (memq 'footnote org-highlight-links) '(org-activate-footnote-links))
          ;; Targets.
          (list org-radio-target-regexp '(0 'org-target t))
	  (list org-target-regexp '(0 'org-target t))
	  ;; Diary sexps.
	  '("^&?%%(.*\\|<%%([^>\n]*?>" (0 'org-sexp-date t))
	  ;; Macro
	  '(org-fontify-macros)
	  ;; TODO keyword
	  (list (format org-heading-keyword-regexp-format
	        	org-todo-regexp)
	        '(2 (org-get-todo-face 2) prepend))
	  ;; TODO
	  (when org-fontify-todo-headline
	    (list (format org-heading-keyword-regexp-format
	        	  (concat
	        	   "\\(?:"
	        	   (mapconcat 'regexp-quote org-not-done-keywords "\\|")
	        	   "\\)"))
	          '(2 'org-headline-todo prepend)))
	  ;; DONE
	  (when org-fontify-done-headline
	    (list (format org-heading-keyword-regexp-format
	        	  (concat
	        	   "\\(?:"
	        	   (mapconcat 'regexp-quote org-done-keywords "\\|")
	        	   "\\)"))
	          '(2 'org-headline-done prepend)))
	  ;; Priorities
	  '(org-font-lock-add-priority-faces)
	  ;; Tags
	  '(org-font-lock-add-tag-faces)
	  ;; Tags groups
	  (when (and org-group-tags org-tag-groups-alist)
	    (list (concat org-outline-regexp-bol ".+\\(:"
	        	  (regexp-opt (mapcar 'car org-tag-groups-alist))
	        	  ":\\).*$")
	          '(1 'org-tag-group prepend)))
	  ;; Special keywords
	  (list (concat "\\<" org-deadline-string) '(0 'org-special-keyword t))
	  (list (concat "\\<" org-scheduled-string) '(0 'org-special-keyword t))
	  (list (concat "\\<" org-closed-string) '(0 'org-special-keyword t))
	  (list (concat "\\<" org-clock-string) '(0 'org-special-keyword t))
	  ;; ;; Emphasis
	  ;; (when org-fontify-emphasized-text '(org-do-emphasis-faces))
	  ;; Checkboxes
	  '("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\(\\[[- X]\\]\\)"
	    1 'org-checkbox prepend)
	  (when (cdr (assq 'checkbox org-list-automatic-rules))
	    '("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
	      (0 (org-get-checkbox-statistics-face) prepend)))
	  ;; Description list items
          '("\\(?:^[ \t]*[-+]\\|^[ \t]+[*]\\)[ \t]+\\(.*?[ \t]+::\\)\\([ \t]+\\|$\\)"
	    1 'org-list-dt prepend)
          ;; Inline export snippets
          '("\\(@@\\)\\([a-z-]+:\\).*?\\(@@\\)"
            (1 'font-lock-comment-face t)
            (2 'org-tag t)
            (3 'font-lock-comment-face t))
	  ;; ARCHIVEd headings
	  (list (concat
	         org-outline-regexp-bol
	         "\\(.*:" org-archive-tag ":.*\\)")
	        '(1 'org-archived prepend))
	  ;; Specials
	  '(org-do-latex-and-related)
	  '(org-fontify-entities)
	  '(org-raise-scripts)
	  ;; Code
	  '(org-activate-code (1 'org-code t))
	  ;; COMMENT
	  (list (format
	         "^\\*+\\(?: +%s\\)?\\(?: +\\[#[A-Z0-9]\\]\\)? +\\(?9:%s\\)\\(?: \\|$\\)"
	         org-todo-regexp
	         org-comment-string)
	        '(9 'org-special-keyword t))
	  ;; Blocks and meta lines
	  '(org-fontify-meta-lines-and-blocks)
          '(org-fontify-inline-src-blocks)
          ;; Citations.  When an activate processor is specified, if
          ;; specified, try loading it beforehand.
          (progn
            (unless (null org-cite-activate-processor)
              (org-cite-try-load-processor org-cite-activate-processor))
            '(org-cite-activate))
          )))
    (setq org-font-lock-extra-keywords (delq nil org-font-lock-extra-keywords))
    (run-hooks 'org-font-lock-set-keywords-hook)
    ;; Now set the full font-lock-keywords
    (setq-local org-font-lock-keywords org-font-lock-extra-keywords)
    (setq-local font-lock-defaults
		'(org-font-lock-keywords t nil nil backward-paragraph))
    (setq-local font-lock-extend-after-change-region-function
		#'org-fontify-extend-region)
    (kill-local-variable 'font-lock-keywords)
    nil))

(defun org-font-lock-restart ()
  "Restart `font-lock-mode', to force refontification."
  (when font-lock-mode
    (font-lock-mode -1)
    (font-lock-mode 1)))
(defalias 'org-restart-font-lock #'org-font-lock-restart)

;; FIXME: Rewrite regexps using rx.

(provide 'org-font-lock)
;;; org-font-lock.el ends here
