;;; org-tags.el --- Org tag commands                      -*- lexical-binding: t; -*-

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

;; This library provides commands to work with tags.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-move)
(require 'org-element-context)
(require 'org-mode-common)
(require 'org-tags-common)
(require 'org-fold-core)
(require 'org-agenda-files)
(require 'org-outline)
(require 'org-tags-core)
(require 'org-tags-align)

(defvar crm-separator)

;;;; Customizations

(defgroup org-tags nil
  "Options concerning tags in Org mode."
  :tag "Org Tags"
  :group 'org)

;; FIXME: This should probably be deprecated in favor of
;; (org-element-property :tags (org-element-org-data))
(defvar org-file-tags nil
  "List of tags that can be inherited by all entries in the file.
The tags will be inherited if the variable `org-use-tag-inheritance'
says they should be.
This variable is populated from #+FILETAGS lines.")

;; Defined to provide a value for defcustom, since there is no
;; string-collate-greaterp in Emacs.
(defun org-string-collate-greaterp (s1 s2)
  "Return non-nil if S1 is greater than S2 in collation order."
  (not (string-collate-lessp s1 s2)))

(defcustom org-tags-sort-function nil
  "When set, tags are sorted using this function as a comparator."
  :group 'org-tags
  :type '(choice
	  (const :tag "No sorting" nil)
	  (const :tag "Alphabetical" org-string<)
	  (const :tag "Reverse alphabetical" org-string>)
	  (function :tag "Custom function" nil)))

(defcustom org-use-fast-tag-selection 'auto
  "Non-nil means use fast tag selection scheme.
This is a special interface to select and deselect tags with single keys.
When nil, fast selection is never used.
When the symbol `auto', fast selection is used if and only if selection
characters for tags have been configured, either through the variable
`org-tag-alist' or through a #+TAGS line in the buffer.
When t, fast selection is always used and selection keys are assigned
automatically if necessary."
  :group 'org-tags
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (const :tag "When selection characters are configured" auto)))

(defcustom org-fast-tag-selection-single-key nil
  "Non-nil means fast tag selection exits after first change.
When nil, you have to press RET to exit it.
During fast tag selection, you can toggle this flag with `C-c'.
This variable can also have the value `expert'.  In this case, the window
displaying the tags menu is not even shown, until you press `C-c' again."
  :group 'org-tags
  :type '(choice
	  (const :tag "No" nil)
	  (const :tag "Yes" t)
	  (const :tag "Expert" expert)))

(defvar org--fast-tag-selection-keys
  (string-to-list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~")
  "List of chars to be used as bindings by `org-fast-tag-selection'.")

(defcustom org-fast-tag-selection-maximum-tags (length org--fast-tag-selection-keys)
  "Set the maximum tags number for fast tag selection.
This variable only affects tags without explicit key bindings outside
tag groups.  All the tags with user bindings and all the tags
corresponding to tag groups are always displayed.

When the number of tags with bindings + tags inside tag groups is
smaller than `org-fast-tag-selection-maximum-tags', tags without
explicit bindings will be assigned a binding and displayed up to the
limit."
  :package-version '(Org . "9.7")
  :group 'org-tags
  :type 'number
  :safe #'numberp)

(defvar org-fast-tag-selection-include-todo nil
  "Non-nil means fast tags selection interface will also offer TODO states.
This is an undocumented feature, you should not rely on it.")

(defcustom org-complete-tags-always-offer-all-agenda-tags nil
  "If non-nil, always offer completion for all tags of all agenda files.

Setting this variable locally allows for dynamic generation of tag
completions in capture buffers.

  (add-hook \\='org-capture-mode-hook
            (lambda ()
              (setq-local org-complete-tags-always-offer-all-agenda-tags t)))"
  :group 'org-tags
  :version "24.1"
  :type 'boolean)

(defvar org-tags-history nil
  "History of minibuffer reads for tags.")
(defvar org-after-tags-change-hook nil
  "Hook that is run after the tags in a line have changed.")

;;;;

(defun org-tag-alist-to-string (alist &optional skip-key)
  "Return tag string associated to ALIST.

ALIST is an alist, as defined in `org-tag-alist' or
`org-tag-persistent-alist', or produced with
`org-tag-string-to-alist'.

Return value is a string suitable as a value for \"TAGS\"
keyword.

When optional argument SKIP-KEY is non-nil, skip selection keys
next to tags."
  (mapconcat (lambda (token)
	       (pcase token
		 (`(:startgroup) "{")
		 (`(:endgroup) "}")
		 (`(:startgrouptag) "[")
		 (`(:endgrouptag) "]")
		 (`(:grouptags) ":")
		 (`(:newline) "\\n")
		 ((and
		   (guard (not skip-key))
		   `(,(and tag (pred stringp)) . ,(and key (pred characterp))))
		  (format "%s(%c)" tag key))
		 (`(,(and tag (pred stringp)) . ,_) tag)
		 (_ (user-error "Invalid tag token: %S" token))))
	     alist
	     " "))

;;;; Tag groups

(defvar org-inhibit-startup)
(declare-function org-agenda-redo "org-agenda")
;;;###autoload
(defun org-toggle-tags-groups ()
  "Toggle support for group tags.
Support for group tags is controlled by the option
`org-group-tags', which is non-nil by default."
  (interactive)
  (setq org-group-tags (not org-group-tags))
  (cond ((and (derived-mode-p 'org-agenda-mode)
	      org-group-tags)
	 (org-agenda-redo))
	((derived-mode-p 'org-mode)
	 (let ((org-inhibit-startup t)) (org-mode))))
  (message "Groups tags support has been turned %s"
	   (if org-group-tags "on" "off")))

;;;; Setting tags

(defun org-global-tags-completion-table (&optional files)
  "Return the list of all tags in all agenda buffer/files.
Optional FILES argument is a list of files which can be used
instead of the agenda files."
  (require 'org-mode)
  (save-excursion
    (org-uniquify
     (delq nil
	   (apply #'append
		  (mapcar
		   (lambda (file)
		     (set-buffer (find-file-noselect file))
		     (org--tag-add-to-alist
		      (org-get-buffer-tags)
		      (mapcar (lambda (x)
				(and (stringp (car-safe x))
				     (list (car-safe x))))
			      org-current-tag-alist)))
		   (if (car-safe files) files
		     (org-agenda-files))))))))

(defvar org-last-tags-completion-table nil
  "The last used completion table for tags.")

(defvar org-add-colon-after-tag-completion nil)  ;; dynamically scoped param
(defvar org-tags-overlay (make-overlay 1 1))
(delete-overlay org-tags-overlay)
(declare-function org-map-entries "org")
;;;###autoload
(defun org-set-tags-command (&optional arg)
  "Set the tags for the current visible entry.

When called with `\\[universal-argument]' prefix argument ARG, \
realign all tags
in the current buffer.

When called with `\\[universal-argument] \\[universal-argument]' prefix argument, \
unconditionally do not
offer the fast tag selection interface.

If a region is active, set tags in the region according to the
setting of `org-loop-over-headlines-in-active-region'.

This function is for interactive use only;
in Lisp code use `org-set-tags' instead."
  (interactive "P")
  (let ((org-use-fast-tag-selection
	 (unless (equal '(16) arg) org-use-fast-tag-selection)))
    (cond
     ((equal '(4) arg) (org-align-tags t))
     ((and (use-region-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
		    'region-start-level 'region))
            org-loop-over-headlines-in-active-region) ;  hint: infinite recursion.
        (require 'org-map)
	(org-map-entries
	 #'org-set-tags-command
	 nil cl
	 (lambda () (when (org-invisible-p) (org-end-of-subtree nil t))))))
     (t
      (save-excursion
        ;; FIXME: We need to add support setting #+FILETAGS.
        (when (org-before-first-heading-p)
          (user-error "Setting file tags is not supported yet"))
	(org-back-to-heading)
        (require 'org-mode)
	(let* ((all-tags (org-get-tags))
               (local-table (or org-current-tag-alist (org-get-buffer-tags)))
	       (table (setq org-last-tags-completion-table
                            (append
                             ;; Put local tags in front.
                             local-table
                             (cl-set-difference
			      (org--tag-add-to-alist
			       (and org-complete-tags-always-offer-all-agenda-tags
				    (org-global-tags-completion-table
				     (org-agenda-files)))
			       local-table)
                              local-table))))
	       (current-tags
		(cl-remove-if (lambda (tag) (get-text-property 0 'inherited tag))
			      all-tags))
	       (inherited-tags
		(cl-remove-if-not (lambda (tag) (get-text-property 0 'inherited tag))
				  all-tags))
	       (tags
		(replace-regexp-in-string
		 ;; Ignore all forbidden characters in tags.
		 "[^[:alnum:]_@#%]+" ":"
		 (if (or (eq t org-use-fast-tag-selection)
			 (and org-use-fast-tag-selection
			      (delq nil (mapcar #'cdr table))))
		     (org-fast-tag-selection
		      current-tags
		      inherited-tags
		      table
		      (and org-fast-tag-selection-include-todo org-todo-key-alist))
		   (let ((org-add-colon-after-tag-completion (< 1 (length table)))
                         (crm-separator "[ \t]*:[ \t]*"))
		     (mapconcat #'identity
                                (completing-read-multiple
			         "Tags: "
			         org-last-tags-completion-table
			         nil nil (org-make-tag-string current-tags)
			         'org-tags-history)
                                ":"))))))
	  (org-set-tags tags)))))
    ;; `save-excursion' may not replace the point at the right
    ;; position.
    (when (and (save-excursion (skip-chars-backward "*") (bolp))
	       (looking-at-p " "))
      (forward-char))))

;;;###autoload
(defun org-set-tags (tags)
  "Set the tags of the current entry to TAGS, replacing current tags.

TAGS may be a tags string like \":aa:bb:cc:\", or a list of tags.
If TAGS is nil or the empty string, all tags are removed.

This function assumes point is on a headline."
  (org-with-wide-buffer
   (org-fold-core-ignore-modifications
     (let ((tags (pcase tags
		   ((pred listp) tags)
		   ((pred stringp) (split-string (org-trim tags) ":" t))
		   (_ (error "Invalid tag specification: %S" tags))))
	   (old-tags (org-get-tags nil t))
	   (tags-change? nil))
       (when (functionp org-tags-sort-function)
         (setq tags (sort tags org-tags-sort-function)))
       (setq tags-change? (not (equal tags old-tags)))
       (when tags-change?
         ;; Delete previous tags and any trailing white space.
         (goto-char (if (org-match-line org-tag-line-re) (match-beginning 1)
		      (line-end-position)))
         (skip-chars-backward " \t")
         (delete-region (point) (line-end-position))
         ;; Deleting white spaces may break an otherwise empty headline.
         ;; Re-introduce one space in this case.
         (unless (org-at-heading-p) (insert " "))
         (when tags
	   (save-excursion (insert-and-inherit " " (org-make-tag-string tags)))
	   ;; When text is being inserted on an invisible region
	   ;; boundary, it can be inadvertently sucked into
	   ;; invisibility.
	   (unless (org-invisible-p (line-beginning-position))
	     (org-fold-core-region (point) (line-end-position) nil 'outline))))
       ;; Align tags, if any.
       (when (and tags org-auto-align-tags) (org-align-tags))
       (when tags-change? (run-hooks 'org-after-tags-change-hook))))))

;;;###autoload
(defun org-toggle-tag (tag &optional onoff)
  "Toggle the tag TAG for the current line.
If ONOFF is `on' or `off', don't toggle but set to this state."
  (save-excursion
    (org-back-to-heading t)
    (let ((current
	   ;; Reverse the tags list so any new tag is appended to the
	   ;; current list of tags.
	   (nreverse (org-get-tags nil t)))
	  res)
      (pcase onoff
	(`off (setq current (delete tag current)))
	((or `on (guard (not (member tag current))))
	 (setq res t)
	 (cl-pushnew tag current :test #'equal))
	(_ (setq current (delete tag current))))
      (org-set-tags (nreverse current))
      res)))

(declare-function org-agenda-change-all-lines "org-agenda")
;;;###autoload
(defun org-change-tag-in-region (beg end tag off)
  "Add or remove TAG for each entry in the region.
This works in the agenda, and also in an Org buffer."
  (interactive
   (list (region-beginning) (region-end)
	 (let ((org-last-tags-completion-table
		(if (derived-mode-p 'org-mode)
		    (org--tag-add-to-alist
		     (org-get-buffer-tags)
		     (org-global-tags-completion-table))
		  (org-global-tags-completion-table))))
	   (completing-read
	    "Tag: " org-last-tags-completion-table nil nil nil
	    'org-tags-history))
	 (progn
	   (message "[s]et or [r]emove? ")
	   (equal (read-char-exclusive) ?r))))
  (deactivate-mark)
  (let ((agendap (equal major-mode 'org-agenda-mode))
	l1 l2 m buf pos newhead (cnt 0))
    (goto-char end)
    (setq l2 (1- (org-current-line)))
    (goto-char beg)
    (setq l1 (org-current-line))
    (cl-loop for l from l1 to l2 do
	     (org-goto-line l)
	     (setq m (get-text-property (point) 'org-hd-marker))
	     (when (or (and (derived-mode-p 'org-mode) (org-at-heading-p))
		       (and agendap m))
	       (setq buf (if agendap (marker-buffer m) (current-buffer))
		     pos (if agendap m (point)))
	       (with-current-buffer buf
		 (save-excursion
		   (save-restriction
		     (goto-char pos)
		     (setq cnt (1+ cnt))
		     (org-toggle-tag tag (if off 'off 'on))
		     (setq newhead (org-get-heading)))))
	       (and agendap (org-agenda-change-all-lines newhead m))))
    (message "Tag :%s: %s in %d headings" tag (if off "removed" "set") cnt)))

(defun org-tags-completion-function (string _predicate &optional flag)
  "Complete tag STRING.
FLAG specifies the type of completion operation to perform.  This
function is passed as a collection function to `completing-read',
which see."
  (let ((completion-ignore-case nil)	;tags are case-sensitive
	(confirm (lambda (x) (stringp (car x))))
	(prefix "")
        begin)
    (when (string-match "^\\(.*[-+:&,|]\\)\\([^-+:&,|]*\\)$" string)
      (setq prefix (match-string 1 string))
      (setq begin (match-beginning 2))
      (setq string (match-string 2 string)))
    (pcase flag
      (`t (all-completions string org-last-tags-completion-table confirm))
      (`lambda (assoc string org-last-tags-completion-table)) ;exact match?
      (`(boundaries . ,suffix)
       (let ((end (if (string-match "[-+:&,|]" suffix)
                      (match-string 0 suffix)
                    (length suffix))))
         `(boundaries ,(or begin 0) . ,end)))
      (`nil
       (pcase (try-completion string org-last-tags-completion-table confirm)
	 ((and completion (pred stringp))
	  (concat prefix
		  completion
		  (if (and org-add-colon-after-tag-completion
			   (assoc completion org-last-tags-completion-table))
		      ":"
		    "")))
	 (completion completion)))
      (_ nil))))

(defun org-fast-tag-insert (kwd tags face &optional end)
  "Insert KWD, and the TAGS, the latter with face FACE.
Also insert END."
  (insert (format "%-12s" (concat kwd ":"))
	  (org-add-props (mapconcat 'identity tags " ") nil 'face face)
	  (or end "")))

(defun org-fast-tag-show-exit (flag)
  (save-excursion
    (org-goto-line 3)
    (when (re-search-forward "[ \t]+Next change exits" (line-end-position) t)
      (replace-match ""))
    (when flag
      (end-of-line 1)
      (org-move-to-column (- (window-width) 19) t)
      (insert (org-add-props " Next change exits" nil 'face 'org-warning)))))

(defun org-set-current-tags-overlay (current prefix)
  "Add an overlay to CURRENT tag with PREFIX."
  (let ((s (org-make-tag-string current)))
    (put-text-property 0 (length s) 'face '(secondary-selection org-tag) s)
    (org-overlay-display org-tags-overlay (concat prefix s))))

(defun org--add-or-remove-tag (tag current-tags &optional groups)
  "Add or remove TAG entered by user to/from CURRENT-TAGS.
Return the modified CURRENT-TAGS.

When TAG is present in CURRENT-TAGS, remove it.  Otherwise, add it.
When TAG is a part of a tag group from GROUPS, make sure that no
exclusive tags from the same group remain in CURRENT-TAGS.

CURRENT-TAGS may be modified by side effect."
  (if (member tag current-tags)
      ;; Remove the tag.
      (delete tag current-tags)
    ;; Add the tag.  If the tag is from a tag
    ;; group, exclude selected alternative tags
    ;; from the group, if any.
    (dolist (g groups)
      (when (member tag g)
	(dolist (x g) (setq current-tags (delete x current-tags)))))
    (cons tag current-tags)))

(defvar org-last-tag-selection-key nil)
(declare-function org-todo "org")
(declare-function org-get-todo-face "org-font-lock" (kwd))
(declare-function org--tag-add-to-alist "org-mode" (alist1 alist2))
(defun org-fast-tag-selection (current-tags inherited-tags tag-table &optional todo-table)
  "Fast tag selection with single keys.
CURRENT-TAGS is the current list of tags in the headline,
INHERITED-TAGS is the list of inherited tags, and TAG-TABLE is an
alist of tags and corresponding keys, possibly with grouping
information.  TODO-TABLE is a similar table with TODO keywords, should
these have keys assigned to them.
If the keys are nil, a-z are automatically assigned.
Returns the new tags string, or nil to not change the current settings."
  (let* (;; Combined alist of all the tags and todo keywords.
         (tag-alist (append tag-table todo-table))
         ;; Max width occupied by a single tag record in the completion buffer.
	 (field-width
          (+ 3 ; keep space for "[c]" binding.
             1 ; ensure that there is at least one space between adjacent tag fields.
             3 ; keep space for group tag " : " delimiter.
             ;; The longest tag.
             (if (null tag-alist) 0
	       (apply #'max
		      (mapcar (lambda (x)
			        (if (stringp (car x)) (string-width (car x))
			          0))
			      tag-alist)))))
	 (origin-buffer (current-buffer))
	 (expert-interface (eq org-fast-tag-selection-single-key 'expert))
         ;; Tag completion table, for normal completion (<TAB>).
	 (tab-tags nil)
	 (inherited-face 'org-done)
	 (current-face 'org-todo)
         ;; Characters available for auto-assignment.
         (tag-binding-char-list org--fast-tag-selection-keys)
         (tag-binding-chars-left org-fast-tag-selection-maximum-tags)
         field-number ; current tag column in the completion buffer.
         tag-binding-spec ; Alist element.
         current-tag current-tag-char auto-tag-char
         tag-table-local ; table holding all the displayed tags together with auto-assigned bindings.
         input-char rtn
	 ov-start ov-end ov-prefix
	 (exit-after-next org-fast-tag-selection-single-key)
	 (done-keywords org-done-keywords)
	 groups ingroup intaggroup)
    ;; Calculate the number of tags with explicit user bindings + tags in groups.
    ;; These tags will be displayed unconditionally.  Other tags will
    ;; be displayed only when there are free bindings left according
    ;; to `org-fast-tag-selection-maximum-tags'.
    (dolist (tag-binding-spec tag-alist)
      (pcase tag-binding-spec
        (`((or :startgroup :startgrouptag) . _)
         (setq ingroup t))
        (`((or :endgroup :endgrouptag) . _)
         (setq ingroup nil))
        ((guard (cdr tag-binding-spec))
         (cl-decf tag-binding-chars-left))
        (`((or :newline :grouptags))) ; pass
        ((guard ingroup)
         (cl-decf tag-binding-chars-left))))
    (setq ingroup nil) ; It t, it means malformed tag alist.  Reset just in case.
    ;; Move global `org-tags-overlay' overlay to current heading.
    ;; Calls to `org-set-current-tags-overlay' will take care about
    ;; updating the overlay text.
    ;; FIXME: What if we are setting file tags?
    (save-excursion
      (forward-line 0)
      (if (looking-at org-tag-line-re)
	  (setq ov-start (match-beginning 1)
		ov-end (match-end 1)
		ov-prefix "")
        (setq ov-start (1- (line-end-position))
	      ov-end (1+ ov-start))
	(skip-chars-forward "^\n\r")
	(setq ov-prefix
	      (concat
	       (buffer-substring (1- (point)) (point))
	       (if (> (current-column) org-tags-column)
		   " "
		 (make-string (- org-tags-column (current-column)) ?\ ))))))
    (move-overlay org-tags-overlay ov-start ov-end)
    ;; Highlight tags overlay in Org buffer.
    (org-set-current-tags-overlay current-tags ov-prefix)
    ;; Display tag selection dialog, read the user input, and return.
    (save-excursion
      (save-window-excursion
        ;; Select tag list buffer, and display it unless EXPERT-INTERFACE.
	(if expert-interface
	    (set-buffer (get-buffer-create " *Org tags*"))
          (pop-to-buffer
           (get-buffer-create " *Org tags*")
           '(org-display-buffer-split (direction . down))))
        ;; Fill text in *Org tags* buffer.
	(erase-buffer)
	(setq-local org-done-keywords done-keywords)
        ;; Insert current tags.
	(org-fast-tag-insert "Inherited" inherited-tags inherited-face "\n")
	(org-fast-tag-insert "Current" current-tags current-face "\n\n")
        ;; Display whether next change exits selection dialog.
	(org-fast-tag-show-exit exit-after-next)
        ;; Show tags, tag groups, and bindings in a grid.
        ;; Each tag in the grid occupies FIELD-WIDTH characters.
        ;; The tags are filled up to `window-width'.
	(setq field-number 0)
	(while (setq tag-binding-spec (pop tag-alist))
	  (pcase tag-binding-spec
            ;; Display tag groups on starting from a new line.
	    (`(:startgroup . ,group-name)
	     (push '() groups) (setq ingroup t)
	     (unless (zerop field-number)
	       (setq field-number 0)
	       (insert "\n"))
	     (insert (if group-name (format "%s: " group-name) "") "{ "))
            ;; Tag group end is followed by newline.
	    (`(:endgroup . ,group-name)
	     (setq ingroup nil field-number 0)
	     (insert "}" (if group-name (format " (%s) " group-name) "") "\n"))
            ;; Group tags start at newline.
	    (`(:startgrouptag)
	     (setq intaggroup t)
	     (unless (zerop field-number)
	       (setq field-number 0)
	       (insert "\n"))
	     (insert "[ "))
            ;; Group tags end with a newline.
	    (`(:endgrouptag)
	     (setq intaggroup nil field-number 0)
	     (insert "]\n"))
	    (`(:newline)
	     (unless (zerop field-number)
	       (setq field-number 0)
	       (insert "\n")
	       (setq tag-binding-spec (car tag-alist))
	       (while (equal (car tag-alist) '(:newline))
		 (insert "\n")
		 (setq tag-alist (cdr tag-alist)))))
	    (`(:grouptags)
             ;; Previous tag is the tag representing the following group.
             ;; It was inserted as "[c] TAG " with spaces filling up
             ;; to the field width. Replace the trailing spaces with
             ;; " : ", keeping to total field width unchanged.
             (delete-char -3)
             (insert " : "))
	    (_
	     (setq current-tag (copy-sequence (car tag-binding-spec))) ; will be modified by side effect
             ;; Compute tag binding.
	     (if (cdr tag-binding-spec)
                 ;; Custom binding.
		 (setq current-tag-char (cdr tag-binding-spec))
               ;; No auto-binding.  Update `tag-binding-chars-left'.
               (unless (or ingroup intaggroup) ; groups are always displayed.
                 (cl-decf tag-binding-chars-left))
	       ;; Automatically assign a character according to the tag string.
	       (setq auto-tag-char
                     (string-to-char
		      (downcase (substring
				 current-tag (if (= (string-to-char current-tag) ?@) 1 0)))))
	       (if (or (rassoc auto-tag-char tag-table-local)
                       (rassoc auto-tag-char tag-table))
                   ;; Already bound.  Assign first unbound char instead.
                   (progn
		     (while (and tag-binding-char-list
                                 (or (rassoc (car tag-binding-char-list) tag-table-local)
                                     (rassoc (car tag-binding-char-list) tag-table)))
		       (pop tag-binding-char-list))
                     (setq current-tag-char (or (car tag-binding-char-list)
                                                ;; Fall back to display "[ ]".
                                                ?\s)))
                 ;; Can safely use binding derived from the tag string.
		 (setq current-tag-char auto-tag-char)))
             ;; Record all the tags in the group.  `:startgroup'
             ;; clause earlier added '() to `groups'.
             ;; `(car groups)' now contains the tag list for the
             ;; current group.
	     (when ingroup (push current-tag (car groups)))
             ;; Compute tag face.
	     (setq current-tag
                   (org-add-props current-tag nil 'face
				  (cond
				   ((not (assoc current-tag tag-table))
                                    ;; The tag is from TODO-TABLE.
                                    (require 'org-font-lock)
				    (org-get-todo-face current-tag))
				   ((member current-tag current-tags) current-face)
				   ((member current-tag inherited-tags) inherited-face))))
	     (when (equal (caar tag-alist) :grouptags)
	       (org-add-props current-tag nil 'face 'org-tag-group))
             ;; Respect `org-fast-tag-selection-maximum-tags'.
             (when (or ingroup intaggroup (cdr tag-binding-spec) (> tag-binding-chars-left 0))
               ;; Insert the tag.
	       (when (and (zerop field-number) (not ingroup) (not intaggroup)) (insert "  "))
	       (insert "[" current-tag-char "] " current-tag
                       ;; Fill spaces up to FIELD-WIDTH.
                       (make-string
		        (- field-width 4 (length current-tag)) ?\ ))
               ;; Record tag and the binding/auto-binding.
	       (push (cons current-tag current-tag-char) tag-table-local)
               ;; Last column in the row.
	       (when (= (cl-incf field-number) (/ (- (window-width) 4) field-width))
	         (unless (memq (caar tag-alist) '(:endgroup :endgrouptag))
	           (insert "\n")
	           (when (or ingroup intaggroup) (insert "  ")))
	         (setq field-number 0))))))
        (insert "\n")
        ;; Keep the tags in order displayed.  Will be used later for sorting.
        (setq tag-table-local (nreverse tag-table-local))
        (goto-char (point-min))
        (unless expert-interface (org-fit-window-to-buffer))
        ;; Read user input.
        (setq rtn
	      (catch 'exit
	        (while t
		  (message "[a-z..]:toggle [SPC]:clear [RET]:accept [TAB]:edit [!] %sgroups%s"
			   (if (not groups) "no " "")
			   (if expert-interface " [C-c]:window" (if exit-after-next " [C-c]:single" " [C-c]:multi")))
		  (setq input-char
                        (let ((inhibit-quit t)) ; intercept C-g.
                          (read-char-exclusive)))
                  ;; FIXME: Global variable used by `org-beamer-select-environment'.
                  ;; Should factor it out.
		  (setq org-last-tag-selection-key input-char)
		  (pcase input-char
                    ;; <RET>
                    (?\r (throw 'exit t))
                    ;; Toggle tag groups.
		    (?!
		     (setq groups (not groups))
		     (goto-char (point-min))
		     (while (re-search-forward "[{}]" nil t) (replace-match " ")))
                    ;; Toggle expert interface.
		    (?\C-c
		     (if (not expert-interface)
		         (org-fast-tag-show-exit
		          (setq exit-after-next (not exit-after-next)))
		       (setq expert-interface nil)
                       (pop-to-buffer
                        " *Org tags*"
                        '((org-display-buffer-split (direction down))))
		       (org-fit-window-to-buffer)))
                    ;; Quit.
		    ((or ?\C-g
		         (and ?q (guard (not (rassoc input-char tag-table-local)))))
		     (delete-overlay org-tags-overlay)
                     ;; Quit as C-g does.
		     (keyboard-quit))
                    ;; Clear tags.
		    (?\s
		     (setq current-tags nil)
		     (when exit-after-next (setq exit-after-next 'now)))
                    ;; Use normal completion.
		    (?\t
                     ;; Compute completion table, unless already computed.
                     (unless tab-tags
                       (setq tab-tags
                             (delq nil
                                   (mapcar (lambda (x)
                                             (let ((item (car-safe x)))
                                               (and (stringp item)
                                                    (list item))))
                                           ;; Complete using all tags; tags from current buffer first.
                                           (org--tag-add-to-alist
                                            (with-current-buffer origin-buffer
                                              (org-get-buffer-tags))
                                            tag-table)))))
                     (setq current-tag (completing-read "Tag: " tab-tags))
		     (when (string-match "\\S-" current-tag)
		       (cl-pushnew (list current-tag) tab-tags :test #'equal)
                       (setq current-tags (org--add-or-remove-tag current-tag current-tags groups)))
		     (when exit-after-next (setq exit-after-next 'now)))
                    ;; INPUT-CHAR is for a todo keyword.
		    ((let (and todo-keyword (guard todo-keyword))
                       (car (rassoc input-char todo-table)))
		     (with-current-buffer origin-buffer
                       (require 'org-todo)
		       (save-excursion (org-todo todo-keyword)))
		     (when exit-after-next (setq exit-after-next 'now)))
                    ;; INPUT-CHAR is for a tag.
		    ((let (and tag (guard tag))
                       (car (rassoc input-char tag-table-local)))
                     (setq current-tags (org--add-or-remove-tag tag current-tags groups))
		     (when exit-after-next (setq exit-after-next 'now))))
		  ;; Create a sorted tag list.
		  (setq current-tags
		        (sort current-tags
			      (lambda (a b)
                                ;; b is after a.
                                ;; `memq' returns tail of the list after the match + the match.
			        (assoc b (cdr (memq (assoc a tag-table-local) tag-table-local))))))
                  ;; Exit when we are set to exit immediately.
		  (when (eq exit-after-next 'now) (throw 'exit t))
                  ;; Continue setting tags in the loop.
                  ;; Update the currently active tags indication in the completion buffer.
		  (goto-char (point-min))
		  (forward-line 1)
                  (delete-region (point) (line-end-position))
		  (org-fast-tag-insert "Current" current-tags current-face)
                  ;; Update the active tags displayed in the overlay in Org buffer.
		  (org-set-current-tags-overlay current-tags ov-prefix)
                  ;; Update tag faces in the displayed tag grid.
		  (let ((tag-re (concat "\\[.\\] \\(" org-tag-re "\\)")))
		    (while (re-search-forward tag-re nil t)
		      (let ((tag (match-string 1)))
		        (add-text-properties
		         (match-beginning 1) (match-end 1)
		         (list 'face
			       (cond
			        ((member tag current-tags) current-face)
			        ((member tag inherited-tags) inherited-face)
			        (t 'default)))))))
		  (goto-char (point-min)))))
        ;; Clear the tag overlay in Org buffer.
        (delete-overlay org-tags-overlay)
        ;; Return the new tag list.
        (if rtn
	    (mapconcat 'identity current-tags ":")
	  nil)))))

(provide 'org-tags)

;;; org-tags.el ends here
