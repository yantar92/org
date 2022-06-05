;;; org-font-lock-obsolete.el --- Obsolete functions previously used for font-locking. To be removed in future.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Ihor Radchenko

;; Author: Ihor Radchenko <yantar92@gmail.com>

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

;; This file contains obsolete function previously used to fontify Org
;; mode buffers.  The new fontification engine is implemented in
;; org-font-lock-core.el and no longer requires the functions from
;; here.  This library will be removed in future.

;;; Code:

(require 'org-macs)

(defvar org-element-paragraph-separate)
(defvar org-inlinetask-min-level)
(defvar org-indent-indentation-per-level)
(defvar org-inlinetask-show-first-star)
(defvar org-emphasis-regexp-components)
(defvar org-outline-regexp-bol)
(defvar org-verbatim-re)
(defvar org-emph-re)
(defvar org-emphasis-alist)
(defvar org-hide-emphasis-markers)
(defvar org-emphasis-regexp-components)
(defvar org-fontify-whole-block-delimiter-line)
(defvar org-protecting-blocks)
(defvar org-src-fontify-natively)
(defvar org-fontify-quote-and-verse-blocks)
(defvar org-hidden-keywords)
(defvar org-drawer-regexp)
(defvar org-hide-macro-markers)
(defvar org-mouse-map)
(defvar org-latex-and-related-regexp)
(defvar org-highlight-latex-and-related)
(defvar org-tag-line-re)
(defvar org-pretty-entities)
(defvar org-priority-regexp)
(defvar org-tag-faces)
(defvar org-tags-special-faces-re)
(defvar org-pretty-entities)
(defvar org-pretty-entities-include-sub-superscripts)
(defvar org-use-sub-superscripts)
(defvar org-match-substring-regexp)
(defvar org-match-substring-with-braces-regexp)
(defvar org-table-dataline-regexp)
(defvar org-script-display)
(defvar org-odd-levels-only)
(defvar org-link-any-re)
(defvar org-highlight-links)
(defvar org-ellipsis)
(defvar org-link--link-folding-spec)
(defvar org-fold-core-style)
(defvar org-tsr-regexp-both)
(defvar org-display-custom-times)
(defvar org-target-link-regexp)

(declare-function org-element-property "org-element" (property element))
(declare-function org-element-link-parser "org-element" ())
(declare-function org-fold-region "org-fold" (from to flag &optional spec))
(declare-function org-fold-core-set-folding-spec-property "org-fold-core" (spec property value &optional force))
(declare-function org-fold-add-folding-spec "org-fold" (spec &optional properties buffer append))
(declare-function org-fold-initialize "org-fold" (ellipsis))
(declare-function org-fold-folding-spec-p "org-fold" (spec-or-alias))
(declare-function org-link-get-parameter "ol" (type key))
(declare-function org-get-tag-face "org-font-lock" (tag))
(declare-function org-get-priority-face "org-font-lock" (priority))
(declare-function org-entity-get "org-entities" (name))
(declare-function org-footnote-next-reference-or-definition "org-footnote" (limit))
(declare-function org-src-font-lock-fontify-block "org-src" (lang start end))
(declare-function org-region-active-p "org-compat" ())
(declare-function org-at-comment-p "org" ())
(declare-function org-remove-flyspell-overlays-in "org-compat" (beg end))

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

(defun org-activate-code (limit)
  (when (re-search-forward "^[ \t]*\\(:\\(?: .*\\|$\\)\n?\\)" limit t)
    (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
    (remove-text-properties (match-beginning 0) (match-end 0)
			    '(display t invisible t intangible t))
    t))


(defun org-fontify-meta-lines-and-blocks (limit)
  (condition-case-unless-debug nil
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


(defun org-inlinetask-fontify (limit)
  "Fontify the inline tasks down to LIMIT."
  (let* ((nstars (if org-odd-levels-only
		     (1- (* 2 (or org-inlinetask-min-level 200)))
		   (or org-inlinetask-min-level 200)))
	 (re (concat "^\\(\\*\\)\\(\\*\\{"
		     (format "%d" (- nstars 3))
		     ",\\}\\)\\(\\*\\* .*\\)"))
	 ;; Virtual indentation will add the warning face on the first
	 ;; star.  Thus, in that case, only hide it.
	 (start-face (if (and (bound-and-true-p org-indent-mode)
			      (> org-indent-indentation-per-level 1))
			 'org-hide
		       'org-warning)))
    (while (re-search-forward re limit t)
      (if org-inlinetask-show-first-star
	  (add-text-properties (match-beginning 1) (match-end 1)
			       `(face ,start-face font-lock-fontified t)))
      (add-text-properties (match-beginning
			    (if org-inlinetask-show-first-star 2 1))
			   (match-end 2)
			   '(face org-hide font-lock-fontified t))
      (add-text-properties (match-beginning 3) (match-end 3)
			   '(face org-inlinetask font-lock-fontified t)))))

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


(provide 'org-font-lock-obsolete)
;;; org-font-lock-obsolete.el ends here
