;;; org-font-lock.el --- Org mode's fontification  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2022 Free Software Foundation, Inc.

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

;; This file contains default fontification setting for Org mode.

;;; Code:

(require 'org-faces)
(require 'org-font-lock-core)
(require 'org-fold)
(require 'org-keys)
(require 'ol)

(declare-function org-element-property "org-element" (property element))
(declare-function org-element-link-parser "org-element" ())
(declare-function org-element-type "org-element" (element))

(defvar org-element-all-objects)
(defvar org-element-paragraph-separate)
(defvar org-element-parsed-keywords)
(defvar org-indent-indentation-per-level)
(defvar org-inlinetask-show-first-star)
(defvar org-ellipsis)
(defvar org-time-stamp-custom-formats)
(defvar org-tsr-regexp-both)
(defvar org-done-keywords)
(defvar org-tag-re)
(defvar org-group-tags)
(defvar org-tag-groups-alist)
(defvar org-list-automatic-rules)
(defvar org-emphasis-alist)

;;;; Hooks.

(defvar org-font-lock-hook nil
  "Functions to be called for special font lock stuff.")

(defvar org-font-lock-set-keywords-hook nil
  "Functions that can manipulate `org-font-lock-extra-keywords'.
This is called after `org-font-lock-extra-keywords' is defined, but before
it is installed to be used by font lock.  This can be useful if something
needs to be inserted at a specific position in the font-lock sequence.")

;;;; Customizations.

(defgroup org-appearance nil
  "Settings for Org mode appearance."
  :tag "Org Appearance"
  :group 'org)

;;;;; Headline appearance.

(defvaralias 'org-level-color-stars-only 'org-font-lock-level-color-stars-only)
(defcustom org-font-lock-level-color-stars-only nil
  "Non-nil means fontify only the stars in each headline.
When nil, the entire headline is fontified.
Changing it requires restart of `font-lock-mode' to become effective
also in regions already fontified."
  :group 'org-appearance
  :type 'boolean)

(defvaralias 'org-hide-leading-stars 'org-font-lock-hide-leading-stars)
(defcustom org-font-lock-hide-leading-stars nil
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

(defvaralias 'org-fontify-todo-headline 'org-font-lock-fontify-todo-headline)
(defcustom org-font-lock-fontify-todo-headline nil
  "Non-nil means change the face of a headline if it is marked as TODO.
Normally, only the TODO/DONE keyword indicates the state of a headline.
When this is non-nil, the headline after the keyword is set to the
`org-headline-todo' as an additional indication."
  :group 'org-appearance
  :package-version '(Org . "9.4")
  :type 'boolean
  :safe #'booleanp)

(defvaralias 'org-fontify-done-headline 'org-font-lock-fontify-done-headline)
(defcustom org-font-lock-fontify-done-headline t
  "Non-nil means change the face of a headline if it is marked DONE.
Normally, only the TODO/DONE keyword indicates the state of a headline.
When this is non-nil, the headline after the keyword is set to the
`org-headline-done' as an additional indication."
  :group 'org-appearance
  :package-version '(Org . "9.4")
  :type 'boolean)

(defvaralias 'org-fontify-whole-heading-line 'org-font-lock-fontify-whole-heading-line)
(defcustom org-font-lock-fontify-whole-heading-line nil
  "Non-nil means fontify the whole line for headings.
This is useful when setting a background color for the
org-level-* faces."
  :group 'org-appearance
  :type 'boolean)

;;;;; Keywords appearance.

(defvaralias 'org-hidden-keywords 'org-font-lock-hidden-keywords)
(defcustom org-font-lock-hidden-keywords nil
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

;;;;; Emphasis, sub/super-scripts, entities, and markers.

(defcustom org-fontify-emphasized-text t
  "Non-nil means fontify *bold*, /italic/ and _underlined_ text.
Changing this variable requires a restart of Emacs to take effect."
  :group 'org-appearance
  :type 'boolean)

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

;;;;; Links.

(defvaralias 'org-activate-links 'org-highlight-links)
(defvaralias 'org-highlight-links 'org-font-lock-highlight-links)
(defcustom org-font-lock-highlight-links '(bracket angle plain radio tag date footnote)
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

;;;;; Timestamps.

(defcustom org-display-custom-times nil
  "Non-nil means overlay custom formats over all time stamps.
The formats are defined through the variable `org-time-stamp-custom-formats'.
To turn this on on a per-file basis, insert anywhere in the file:
   #+STARTUP: customtime"
  :group 'org-time
  :set 'set-default
  :type 'sexp)
(make-variable-buffer-local 'org-display-custom-times)

;;;;; Blocks.

(defcustom org-fontify-whole-block-delimiter-line t
  "Non-nil means fontify the whole line for begin/end lines of blocks.
This is useful when setting a background color for the
org-block-begin-line and org-block-end-line faces."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-src-fontify-natively t
  "When non-nil, fontify code in code blocks.
See also the `org-block' face."
  :type 'boolean
  :version "26.1"
  :package-version '(Org . "8.3")
  :group 'org-appearance
  :group 'org-babel)

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

;;;; Internal variables.

(defvar org-font-lock-keywords nil
  "Font lock keywords in current buffer.")

;; Dynamically scoped.
(defvar org-font-lock-extra-keywords nil
  "Variable holding `org-font-lock-keywords' during font lock initialization.
This variable is set by `org-font-lock-set-defaults' and can be
manipulated in `org-font-lock-set-keywords-hook'.")

(defconst org-script-display  '(;; The values are tweaked to not
                                ;; change the line height.
                                ((raise -0.1)  (height 0.7))
				((raise 0.25)  (height 0.7))
                                ;; Alternative properties for tables.
                                ;; See 0618aeafb3.
                                ;; FIXME: We cannot change the text
                                ;; height because it will alter the
                                ;; symbol width and thus break the
                                ;; table alignment (at least, until
                                ;; org table are aligned via pixel
                                ;; width).
				((raise -0.35))
				((raise 0.35)))
  "Display properties for showing superscripts and subscripts.")

;;;; Public commands and functions.

(defun org-toggle-pretty-entities ()
  "Toggle the composition display of entities as UTF8 characters."
  (interactive)
  (setq-local org-pretty-entities (not org-pretty-entities))
  (org-font-lock-restart)
  (if org-pretty-entities
      (message "Entities are now displayed as UTF8 characters")
    (save-restriction
      (widen)
      (decompose-region (point-min) (point-max))
      (message "Entities are now displayed as plain text"))))

(defvar org-odd-levels-only)
(defun org-fontify-like-in-org-mode (s &optional odd-levels)
  "Fontify string S like in Org mode."
  (with-temp-buffer
    (insert s)
    (let ((org-odd-levels-only odd-levels))
      (org-mode)
      (font-lock-ensure)
      (if org-link-descriptive
          (org-link-display-format
           (buffer-string))
        (buffer-string)))))

(defun org-font-lock-restart ()
  "Restart `font-lock-mode', to force refontification."
  (when font-lock-mode
    (font-lock-mode -1)
    (font-lock-mode 1)))
(defalias 'org-restart-font-lock #'org-font-lock-restart)

;;;; Internal functions.

;;;;; Getting element faces/properties.

(defun org-face-from-face-or-color (context inherit face-or-color)
  "Create a face list that inherits INHERIT, but sets the foreground color.
When FACE-OR-COLOR is not a string, just return it."
  (if (stringp face-or-color)
      (list :inherit inherit
	    (cdr (assoc context org-faces-easy-properties))
	    face-or-color)
    face-or-color))

(defun org-get-level-face ()
  "Get the right face for the last matched element."
  (let* ((level (org-element-match-property :level)))
    (if org-cycle-level-faces
	(nth (% (1- level) org-n-level-faces) org-level-faces)
      (nth (1- (min level org-n-level-faces)) org-level-faces))))

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


(defun org-font-lock-footnote-reference-get-properties (&optional element)
  "Get text property plist or ELEMENT footnote reference or definition."
  `( face org-footnote
     mouse-face highlight
     keymap org-mouse-map
     help-echo ,(pcase (org-element-type (or element (org-element-match-last)))
                  (`footnote-reference "Footnote reference")
                  (`footnote-definition "Footnote reference")
                  (_ (error "%S is not a footnote"
                            (org-element-type
                             (or element
                                 (org-element-match-last))))))))

(defun org-font-lock-link-get-properties (&optional element)
  "Get text property plist for ELEMENT link."
  (setq element (or element (org-element-match-last)))
  (let ((type (org-element-property :type element))
        (link (org-element-property :raw-link element))
        (path (org-element-property :path element)))
    (list
     'face (pcase (org-link-get-parameter type :face)
	     ((and (pred functionp) face) (funcall face path))
	     ((and (pred facep) face) face)
	     ((and (pred consp) face) face) ;anonymous
	     (_ 'org-link))
     'mouse-face (or (org-link-get-parameter type :mouse-face)
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

;;;;; Folding elements (hiding parts of elements).

(defun org-font-lock-link-fold (&optional element)
  "Fold hidden parts of the link ELEMENT."
  (let* ((element (or element (org-element-match-last)))
         (start (org-element-match-beginning :full-no-blank))
         (end (org-element-match-end :full-no-blank))
         (visible-start (org-element-match-beginning :visible))
         (visible-end (org-element-match-end :visible))
         (spec (or (org-link-get-parameter
                    (org-element-property :type element)
                    :display)
                   'org-link)))
    (when (eq 'bracket (org-element-property :format element))
      (if (eq org-fold-core-style 'text-properties)
          (progn
            ;; Initialise folding when used outside org-mode.
            (unless (or (derived-mode-p 'org-mode)
		        (and (org-fold-folding-spec-p 'org-link-description)
                             (org-fold-folding-spec-p 'org-link)))
              (org-fold-initialize (or (and (stringp org-ellipsis) (not (equal "" org-ellipsis)) org-ellipsis)
                                       "...")))
            (unless (org-fold-folding-spec-p spec)
              (org-fold-add-folding-spec spec
                                         (cdr org-link--link-folding-spec)
                                         nil
                                         'append)
              (org-fold-core-set-folding-spec-property spec :visible t))
            (org-fold-region start end nil 'org-link)
            (org-fold-region start end nil spec)
            (org-fold-region start end nil 'org-link-description)
            ;; We are folding the whole emphasised text with SPEC
            ;; first.  It makes everything invisible (or whatever
            ;; the user wants).
            (org-fold-region start end t spec)
            ;; The visible part of the text is folded using
            ;; 'org-link-description, which is forcing this part of
            ;; the text to be visible.
            (org-fold-region visible-start visible-end t 'org-link-description))
        ;; Handle invisible parts in bracket links.
        (remove-text-properties start end '(invisible nil))
        (add-text-properties start visible-start `(invisible ,spec))
        (add-text-properties visible-end end `(invisible ,spec)))
      (org-rear-nonsticky-at visible-start)
      (org-rear-nonsticky-at visible-end)))
  ;; Done with folding.  No need to pass anything to font-lock.
  nil)

(defun org-font-lock-macro-fold ()
  "Hide invisible parts of the last matched macro."
  (when org-hide-macro-markers
    (add-text-properties
     (org-element-match-beginning :begin-marker)
     (org-element-match-end :begin-marker)
     '(invisible t))
    (add-text-properties
     (org-element-match-beginning :end-marker)
     (org-element-match-end :end-marker)
     '(invisible t)))
  nil)

(defun org-font-lock-link-activate-func ()
  "Run :activate-func for matched element."
  (let* ((f (org-link-get-parameter
             (org-element-match-type)
             :activate-func))
         (fail-message
          (format
           "Failed to run activate function %S for %S link: %%S"
           f (org-element-match-type))))
    (when (functionp f)
      ;; 
      (with-demoted-errors fail-message
        (save-excursion
          (funcall
           f
           (org-element-match-beginning :full-no-blank)
           (org-element-match-end :full-no-blank)
           (org-element-match-property :path)
           (eq (org-element-match-property :format) 'bracket))))))
  nil)

(defun org-display-custom-time (beg end)
  "Overlay modified time stamp format over timestamp between BEG and END."
  (let* ((ts (buffer-substring beg end))
	 t1 with-hm tf time str (off 0))
    (save-match-data
      (setq t1 (org-parse-time-string ts t))
      (when (string-match "\\(-[0-9]+:[0-9]+\\)?\\( [.+]?\\+[0-9]+[hdwmy]\\(/[0-9]+[hdwmy]\\)?\\)?\\'" ts)
	(setq off (- (match-end 0) (match-beginning 0)))))
    (setq end (- end off))
    (setq with-hm (and (nth 1 t1) (nth 2 t1))
	  tf (funcall (if with-hm 'cdr 'car) org-time-stamp-custom-formats)
	  time (org-fix-decoded-time t1)
	  str (org-add-props
		  (format-time-string
		   (substring tf 1 -1) (org-encode-time time))
		  nil 'mouse-face 'highlight))
    (org-font-lock-compose str beg end)))

;;;;; Base font-locking setup.

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
    (when (and (org-fold-core-folding-spec-p 'org-link)
               (org-fold-core-folding-spec-p 'org-link-description))
      (org-fold-region beg end nil 'org-link)
      (org-fold-region beg end nil 'org-link-description))
    (org-fold-core-update-optimisation beg end)
    (org-remove-font-lock-display-properties beg end)))

(defun org-font-lock-hook (limit)
  "Run `org-font-lock-hook' within LIMIT."
  (run-hook-with-args 'org-font-lock-hook limit))

(defun org-font-lock-set-defaults ()
  "Set font lock defaults for the current buffer."
  ;; Citations.  When an activate processor is specified, if
  ;; specified, try loading it beforehand.
  (unless (null org-cite-activate-processor)
    (org-cite-try-load-processor org-cite-activate-processor))
  (setq org-font-lock-element-keywords
        `(
          ;; Remove flyspell overlays if they are not allowed inside
          ;; element.  This is needed when users types in a new
          ;; element and typing initially triggers flyspell error
          ;; inside the incomplete element.
          ((org-with-point-at (org-element-match-end)
             (when (memq (org-element-match-type)
                         org-element-all-objects)
               (not (org-mode-flyspell-verify))))
           (:full-no-blank
            (prog1 nil
              (org-remove-flyspell-overlays-in
               (org-element-match-property :begin)
               (org-element-match-property :end)))
            nil t))
          ;; Drawers.
          ((drawer property-drawer)
           (:begin-marker 'org-drawer t)
           (:end-marker 'org-drawer t))
          ;; Blocks.
          (( src-block center-block comment-block dynamic-block
             example-block export-block quote-block special-block
             verse-block)
           (,(if org-fontify-whole-block-delimiter-line
                 :begin-marker :begin-marker-line)
            'org-block-begin-line t)
           (,(if org-fontify-whole-block-delimiter-line
                 :end-marker :end-marker-line)
            'org-block-end-line t)
           (:contents
            `((t :inherit
                 ,(let ((face-name
		         (intern (format
                                  "org-block-%s"
                                  (or (org-element-match-property :language)
                                      (org-element-match-property :type))))))
		    (append
                     (and (facep face-name) (list face-name))
		     '(org-block)))))
            append))
          ,(when org-fontify-quote-and-verse-blocks
             `(quote-block (:full-no-blank 'org-quote t)))
          ,(when org-fontify-quote-and-verse-blocks
             `(verse-block (:full-no-blank 'org-verse t)))
          ,(when org-src-fontify-natively
             `((src-block export-block)
               (:contents
                (org-src-font-lock-fontify-block
                 (or (org-element-match-property :language)
                     (org-element-match-property :type))
                 (org-element-match-beginning :contents)
                 (org-element-match-end :contents))
                nil t)))
          ;; Headlines
          ((headline inlinetask)
           (:title-line
            (if (org-element-match-property :archivedp)
                'org-archived
              (pcase (org-element-match-property :todo-type)
                (`todo (when org-fontify-todo-headline 'org-headline-todo))
                (`done (when org-fontify-done-headline 'org-headline-done))
                (_ nil)))
            t))
          ,(if org-level-color-stars-only
               '(headline (:stars (org-get-level-face)))
             (if org-fontify-whole-heading-line
                 '(headline (:title-line-whole (org-get-level-face) append))
               '(headline (:title-line (org-get-level-face) append))))
          ,(when (featurep 'org-inlinetask)
             (if org-fontify-whole-heading-line
                 `(inlinetask
                   (:title-line-whole 'org-inlinetask t)
                   (:end-line-whole 'org-inlinetask t))
               `(inlinetask
                 (:title-line 'org-inlinetask t)
                 (:end-line 'org-inlinetask t))))
          ,(when (featurep 'org-inlinetask)
             (let ((start-face
                    ;; Virtual indentation will add the warning
	            ;; face on the first star.  Thus, in that
	            ;; case, only hide it.
                    (if (or (not org-inlinetask-show-first-star)
                            (and (bound-and-true-p org-indent-mode)
			         (> org-indent-indentation-per-level 1)))
			'org-hide
		      'org-warning)))
               `(inlinetask
                 (:end-first-star ',start-face t)
                 (:first-star ',start-face t))))
          ,(when (featurep 'org-inlinetask)
             `(inlinetask
               (:end-mid-stars 'org-hide t)
               (:mid-stars 'org-hide t)))
          ,(when org-hide-leading-stars
             '(headline (:leading-stars 'org-hide t)))
          ;; TODO keywords
          ((headline inlinetask)
           (:todo
            (org-get-todo-face
             (org-element-match-property :todo-keyword))
            prepend))
          ;; Priority
          ((headline inlinetask)
           (:priority
            (org-get-priority-face
             (org-element-match-property :priority))
            prepend))
          ;; Headline COMMENT
          ((headline inlinetask) (:comment 'org-special-keyword t))
          ;; Headline tags
          ,(when (memq 'tag org-highlight-links)
             '((headline inlinetask)
               (:tags `( face org-tag
                         mouse-face highlight
                         keymap org-mouse-map
                         help-echo "Open tags agenda")
                      prepend)))
          ;; Special tag faces.
          ,(when org-tag-faces
             `((and (memq (org-element-match-type) '(headline inlinetask))
                    (org-element-match-beginning :tags))
               (,org-tag-re
                (goto-char (org-element-match-beginning :tags))
                (goto-char (org-element-match-end :tags))
                (0
                 (let ((tg (org-get-tag-face 0)))
                   (unless (eq 'org-tag tg)
                     tg))
                 prepend))))
          ;; Tags groups.
          ,(when (and org-group-tags org-tag-groups-alist)
             `((and (memq (org-element-match-type) '(headline inlinetask))
                    (org-element-match-beginning :tags))
               ((lambda (limit)
                  (when (re-search-forward
                         (concat
                          ":"
                          ,(regexp-opt (mapcar 'car org-tag-groups-alist) t)
                          ":")
                         limit t)
                    (prog1 (point)
                      (backward-char 2))))
                (goto-char (org-element-match-beginning :tags))
                (goto-char (org-element-match-end :tags))
                (0 'org-tag-group prepend))))
          ;; TBLFM lines.
          (table (:tblfm 'org-meta-line t))
          ;; Table lines
          (table-row (:line 'org-table append))
          ;; table.el table lines are not parsed.  Fall back to regexp
          ;; matching.
          ((eq (org-element-match-property :type) 'table.el)
           ("^\\s-*\\(\\S-.*?\\)\\s-*$"
            ;; Search regex till :end.
            (progn
              (goto-char (org-element-match-beginning))
              (org-element-match-end))
            (goto-char (org-element-match-end))
            (1 'org-table append)))
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
          ;; Comments.
          (comment (:full-no-blank 'font-lock-comment-face t))
          ;; Affiliated keywords.
          ((and (org-element-match-property :post-affiliated)
                (> (org-element-match-property :post-affiliated)
                   (org-element-match-property :begin)))
           (:affiliated 'org-meta-line t)
           (,(concat "[ \t]*#\\+" (regexp-opt org-element-parsed-keywords) ":\\(.+\\)")
            (progn
              (goto-char (org-element-match-beginning :affiliated))
              ;; Search case-insensitively.
              (setq case-fold-search t)
              (org-element-match-end :affiliated))
            ;; Restore case fold.
            (setq case-fold-search font-lock-keywords-case-fold-search)
            (1 'org-block t)))
          ;; Node properties and keywords.
          (keyword
           (:key 'org-special-keyword t)
           (:value 'org-property-value t))
          ((and (eq 'keyword (org-element-match-type))
                (member (org-element-match-property :key)
                        ;; FIXME: Originally, just title, subtitle,
                        ;; author, email, and date.
                        org-options-keywords))
           (:value 'org-document-info-keyword t))
          ((and (eq 'keyword (org-element-match-type))
                (string= (org-element-match-property :key) "TITLE"))
           (:value 'org-document-title t))
          ((and (eq 'keyword (org-element-match-type))
                (member (intern (downcase (org-element-match-property :key)))
                        org-hidden-keywords))
           (:key-with-blank '(face nil invisible t) t))
          (node-property
           (:key 'org-special-keyword t)
           (:value 'org-property-value t))
          ;; Targets and radio targets.
          (target (:full-no-blank 'org-target t))
          (radio-target (:full-no-blank 'org-target t))
          ;; Diary sexp elements.
          (diary-sexp (:full-no-blank 'org-sexp-date t))
          ;; Planning
          (planning
           (:scheduled-keyword 'org-special-keyword t)
           (:deadline-keyword 'org-special-keyword t)
           (:closed-keyword 'org-special-keyword t))
          ;; Clock
          (clock
           (:clock-keyword 'org-special-keyword t))
          ;; Footnote reference and footnote definition
          ,(when (memq 'footnote org-highlight-links)
             '(footnote-reference
               (:full-no-blank
                (org-font-lock-footnote-reference-get-properties)
                t)))
          ,(when (memq 'footnote org-highlight-links)
             '(footnote-definition
               (:full-no-blank
                (org-font-lock-footnote-reference-get-properties)
                t)))
          ;; Macro
          (macro
           (:full-no-blank '(face org-macro org-macro t) t)
           (:full (org-font-lock-macro-fold) nil t))
          ;; Inline export snippets
          (export-snippet
           (:begin-marker 'font-lock-comment-face append)
           (:back-end 'org-tag append)
           (:end-marker 'font-lock-comment-face append))
          ;; Statistics cookies
          ,(when (cdr (assq 'checkbox org-list-automatic-rules))
             '(statistics-cookie
               (:full-no-blank (org-get-checkbox-statistics-face) prepend)))
          ;; Links.
          (link
           (:full-no-blank (org-font-lock-link-get-properties) prepend)
           ;; nil t makes font-lock ignore nil return value.
           (:full-no-blank (org-font-lock-link-fold) nil t)
           (:full-no-blank (org-font-lock-link-activate-func) nil t))
          ,(when (memq 'radio org-highlight-links)
             `((string= "radio" (org-element-match-property :type))
               (:full-no-blank
                `( face nil ; face was set above.
                   mouse-face highlight
		   keymap 'org-mouse-map
		   help-echo "Radio target link"
		   org-linked-text t))))
          ;; Emphasis.
          ,@(when org-fontify-emphasized-text
              (cl-loop for (_ fontspec . _) in org-emphasis-alist
                       for element-name in '(bold italic underline verbatim code strike-through)
                       collect `(,element-name (:full-no-blank ',fontspec prepend))))
          ;; `org-emphasis' text property.
          ,(when org-fontify-emphasized-text
             '((bold italic underline verbatim code strike-through)
               (:full-no-blank '(face nil org-emphasis t))))
          ;; `org-hide-emphasis-markers'. Note that it can be switched
          ;; without reloading.  Hence, we calculate fontification
          ;; dynamically.
          ,(when org-fontify-emphasized-text
             '((bold italic underline verbatim code strike-through)
               (:begin-marker (when org-hide-emphasis-markers '(face nil invisible t)))
               (:end-marker (when org-hide-emphasis-markers '(face nil invisible t)))))
          ;; Entities.
          (entity
           (:full-no-blank
	    ;; "\_ "-family is left out on purpose.  Only the first one,
	    ;; i.e., "\_ ", could be fontified anyway, and it would be
	    ;; confusing when adding a second white space character.
            (unless (string-match-p " $"(org-element-match-property :name))
              (org-font-lock-compose
               (org-element-match-property :utf-8)
               (org-element-match-beginning :full-no-blank)
               (org-element-match-end :full-no-blank)))
            nil t))
          ,(when (memq 'entities org-highlight-latex-and-related)
             `(entity (:full-no-blank 'org-latex-and-related prepend)))
          ;; Subscripts and superscripts.
          ,(when (memq 'script org-highlight-latex-and-related)
             `((and (memq (org-element-match-type) '(subscript superscript))
                    (pcase org-use-sub-superscripts
                      (`{} (org-element-match-property :use-brackets-p))
                      (`nil nil)
                      (_ t)))
               (:full-no-blank 'org-latex-and-related prepend)))
          ,(when (and org-pretty-entities
                      org-pretty-entities-include-sub-superscripts)
             `((and (memq (org-element-match-type) '(subscript superscript))
                    (pcase org-use-sub-superscripts
                      (`{} (org-element-match-property :use-brackets-p))
                      (`nil nil)
                      (_ t)))
               (:full-no-blank '(face nil org-emphasis t))
               (:contents
                `( face nil
                   display
                   ,(let ((tablep (org-element-lineage (org-element-match-property :parent) '(table))))
                      (pcase (org-element-match-type)
                        ;; FIXME: See comment in `org-script-display'.
                        (`superscript
                         (nth (if tablep 3 1) org-script-display))
                        (`subscript
                         (nth (if tablep 2 0) org-script-display))
                        (_ (error "Fontification error: sub/superscript"))))))
               (:begin-marker '(face nil invisible t))
               (:end-marker '(face nil invisible t))))
          ;; LaTeX fragments and LaTeX environments.
          ,(when (or (memq 'latex org-highlight-latex-and-related)
		     (memq 'native org-highlight-latex-and-related))
             `((and (memq (org-element-match-type)
                          '(latex-environment latex-fragment))
                    (let ((matchers (plist-get org-format-latex-options :matchers)))
                      (if (eq (org-element-match-type) 'latex-environment)
                          (member "begin" matchers)
                        (pcase (substring-no-properties
                                (org-element-match-property :value)
                                0 2)
                          ("$$" (member "$$" matchers))
                          ("\\(" (member "\\(" matchers))
                          ("\\[" (member "\\[" matchers))
                          (_
                           (or (member "$" matchers)
                               (and (member "$1" matchers)
                                    (= 3 (length (org-element-match-property :value))))))))))
               ,(if (memq 'native org-highlight-latex-and-related)
                    `(:full-no-blank
                      (org-src-font-lock-fontify-block
                       "latex"
                       (org-element-match-beginning :full-no-blank)
                       (org-element-match-end :full-no-blank))
                      nil t)
                  `(:full-no-blank 'org-latex-and-related prepend))))
          ;; Inline src blocks.
          (inline-src-block
           (:full-no-blank 'org-inline-src-block append)
           (:language 'org-meta-line append)
           (:src-marker 'shadow append)
           (:value-end-marker 'shadow append)
           (:value 'org-inline-src-block t)
           ,(when org-src-fontify-natively
              `(:value
                (org-src-font-lock-fontify-block
                 (org-element-match-property :language)
                 (org-element-match-beginning :value)
                 (org-element-match-end :value))
                nil t)))
          ;; List items.
          (item (:checkbox 'org-checkbox prepend)
                (:tag 'org-list-dt prepend)
                (:tag-colons 'org-list-dt prepend))
          ;; FIXME: There is currently no way to make use of the new
          ;; font-lock keywords for oc backend.  May need to do
          ;; something about it.
          (eval
           .
           (when (org-cite-activate-func)
             `(citation
               (:full-no-blank
                (funcall
                 (org-cite-activate-func)
                 (org-element-match-last))
                nil t))))
          (eval
           .
           (unless (org-cite-activate-func)
             `(citation (:full-no-blank 'org-cite prepend))))
          (eval
           .
           (unless (org-cite-activate-func)
             `(citation-reference (:key-full 'org-cite-key prepend))))
          ))
  (let ((org-font-lock-extra-keywords
	 (list
	  ;; Call the hook
	  '(org-font-lock-hook)
          '(org-font-lock-matcher)
          ;; Timestamps.
          ;; FIXME: Timestamps are tricky.  In addition to proper
          ;; timestamp objects, Org agenda accepts timestamps,
          ;; e.g. inside node-properties.  Moreover, many Org
          ;; functions allow working with arbitrarily located text
          ;; looking like a timestamp.  See `org-at-timestamp-p' and
          ;; its usage across Org with `lax' and `agenda' arguments.
          ;; So, instead of fontifying timestamp objects, we are being
          ;; more lax and just match a generic regexp with an extra
          ;; check that we do not hit false positive like in:
          ;; [[file:sample.org][2013-02-22 sample.org]].
          ;; See commit 01748861d and its predecessors.
          `((lambda (limit)
              (catch :found
                (while (re-search-forward org-tsr-regexp-both limit t)
                  (let ((obj (org-element-context)))
                    (unless (and (eq 'link (org-element-type obj))
                                 (or (< (match-beginning 0) (org-element-property :contents-begin obj))
                                     (> (match-end 0) (org-element-property :contents-end obj))))
                      (throw :found (point)))))))
            ;; `org-highlight-links' determines whether to fontify
            ;; timestamps.  However, even if fontification is
            ;; disabled, "opening" via mouse click must remain
            ;; possible.
            (0
             '( face ,(when (memq 'date org-highlight-links) 'org-date)
                mouse-face ,(when (memq 'date org-highlight-links) 'highlight)
	        keymap org-mouse-map
                help-echo "Open agenda for the date/range")
             t)
            (1
             (when (and org-display-custom-times
                        (memq 'date org-highlight-links))
               (org-display-custom-time (match-beginning 1) (match-end 1))
               nil)
             nil t)
            (3
             (when (and org-display-custom-times
                        (memq 'date org-highlight-links))
               (org-display-custom-time (match-beginning 3) (match-end 3)))
             nil t)))))
    (setq org-font-lock-extra-keywords (delq nil org-font-lock-extra-keywords))
    (run-hooks 'org-font-lock-set-keywords-hook)
    ;; Now set the full font-lock-keywords
    (setq-local org-font-lock-keywords org-font-lock-extra-keywords)
    (setq-local font-lock-defaults
		'(org-font-lock-keywords t nil nil backward-paragraph))
    (add-hook 'before-change-functions #'org-font-lock-flush-delayed nil t)
    (add-hook 'after-change-functions #'org-font-lock-flush-delayed nil t)
    (setq-local font-lock-extend-after-change-region-function
		#'org-font-lock--extend-region)
    (setq-local font-lock-unfontify-region-function
                #'org-unfontify-region)
    (kill-local-variable 'font-lock-keywords)
    nil))

;; FIXME: Rewrite regexps using rx.

;; FIXME: Make sure that flyspell is not broken.  It should not, in
;; theory - Org does provide a flyspell predicate that should already
;; take care about not running flyspell where it is not needed.
;; However, the predicate is quite slow. It will be better to write a
;; dedicated function collecting all elements and removing only
;; uncheckable parts.

;; FIXME: Check rear-nosticky usage and where it is actually needed.

;; FIXME: In theory, element-based fontification should not need to
;; worry about font-lock-multiline property. It already uses an even more
;; fine-grained approach.

;; FIXME: Empty drawers have no contents - matching is not correct.

;; FIXME: Note the change in org-script-display handling: It appears
;; to be swapped from the initial logic.

;; FIXME: Allow custom per-element type fontification functions.
;; Maybe it can be either a macro returning keywords or a function
;; accepting element arg.

;; FIXME: Consider hiding some functions in private space.

(provide 'org-font-lock)
;;; org-font-lock.el ends here
