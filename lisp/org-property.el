;;; org-property.el --- Org property API                      -*- lexical-binding: t; -*-

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

;; This library implements Org mode headline properties.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element)
(require 'org-element-context)
(require 'org-outline)
(require 'org-property-core)
(declare-function org-inlinetask-in-task-p "org-inlinetask")

(defvar-local org-keyword-properties nil
  "List of property/value pairs inherited by any entry.

Valid for the current buffer.  This variable is populated from
PROPERTY keywords.

Note that properties are defined also in property drawers.
Properties defined there take precedence over properties defined
as keywords.")

(defconst org-special-properties
  '("ALLTAGS" "BLOCKED" "CLOCKSUM" "CLOCKSUM_T" "CLOSED" "DEADLINE" "FILE"
    "ITEM" "PRIORITY" "SCHEDULED" "TAGS" "TIMESTAMP" "TIMESTAMP_IA" "TODO")
  "The special properties valid in Org mode.
These are properties that are not defined in the property drawer,
but in some other way.")

(defconst org-default-properties
  '("ARCHIVE" "CATEGORY" "SUMMARY" "DESCRIPTION" "CUSTOM_ID"
    "LOCATION" "LOGGING" "COLUMNS" "VISIBILITY"
    "TABLE_EXPORT_FORMAT" "TABLE_EXPORT_FILE"
    "EXPORT_OPTIONS" "EXPORT_TEXT" "EXPORT_FILE_NAME"
    "EXPORT_TITLE" "EXPORT_AUTHOR" "EXPORT_DATE" "UNNUMBERED"
    "ORDERED" "NOBLOCKING" "COOKIE_DATA" "LOG_INTO_DRAWER" "REPEAT_TO_STATE"
    "CLOCK_MODELINE_TOTAL" "STYLE" "HTML_CONTAINER_CLASS"
    "ORG-IMAGE-ACTUAL-WIDTH")
  "Some properties that are used by Org mode for various purposes.
Being in this list makes sure that they are offered for completion.")

(declare-function org-insert-property-drawer "org-property-set" ())
(defun org-get-property-block (&optional beg force)
  "Return the (beg . end) range of the body of the property drawer.
BEG is the beginning of the current subtree or the beginning of
the document if before the first headline.  If it is not given,
it will be found.  If the drawer does not exist, create it if
FORCE is non-nil, or return nil."
  (org-with-wide-buffer
   (let ((beg (cond (beg (goto-char beg))
		    ((or (not (featurep 'org-inlinetask))
			 (org-inlinetask-in-task-p))
		     (org-back-to-heading-or-point-min t) (point))
		    (t (org-with-limited-levels
			(org-back-to-heading-or-point-min t))
		       (point)))))
     ;; Move point to its position according to its positional rules.
     (cond ((org-before-first-heading-p)
	    (while (and (org-at-comment-p) (bolp)) (forward-line)))
	   (t (forward-line)
	      (when (looking-at-p org-planning-line-re) (forward-line))))
     (cond ((looking-at org-property-drawer-re)
	    (forward-line)
	    (cons (point) (progn (goto-char (match-end 0))
				 (line-beginning-position))))
	   (force
	    (goto-char beg)
            (require 'org-property-set)
	    (org-insert-property-drawer)
	    (let ((pos (save-excursion (re-search-forward org-property-drawer-re)
				       (line-beginning-position))))
	      (cons pos pos)))))))

(defun org-entry-blocked-p ()
  "Non-nil if entry at point is blocked."
  (and (not (org-entry-get nil "NOBLOCKING"))
       (member (org-entry-get nil "TODO") org-not-done-keywords)
       (not (run-hook-with-args-until-failure
	   'org-blocker-hook
	   (list :type 'todo-state-change
		 :position (point)
		 :from 'todo
		 :to 'done)))))

(declare-function org-get-tags "org-tags-core" (&optional epom local))
(declare-function org-make-tag-string "org-tags-core" (tags))
(declare-function org-duration-from-minutes "org-duration"
                  (minutes &optional fmt canonical))
(defun org-entry-properties (&optional epom which)
  "Get all properties of the current entry.

When EPOM is a buffer position, marker, or element, get all properties
from the entry there instead.

This includes the TODO keyword, the tags, time strings for
deadline, scheduled, and clocking, and any additional properties
defined in the entry.

If WHICH is nil or `all', get all properties.  If WHICH is
`special' or `standard', only get that subclass.  If WHICH is
a string, only get that property.

Return value is an alist.  Keys are properties, as upcased
strings."
  (org-with-point-at epom
    (when (and (derived-mode-p 'org-mode)
	       (org-back-to-heading-or-point-min t))
      (catch 'exit
	(let* ((beg (point))
	       (specific (and (stringp which) (upcase which)))
	       (which (cond ((not specific) which)
			    ((member specific org-special-properties) 'special)
			    (t 'standard)))
	       props)
	  ;; Get the special properties, like TODO and TAGS.
	  (when (memq which '(nil all special))
	    (when (or (not specific) (string= specific "CLOCKSUM"))
	      (let ((clocksum (get-text-property (point) :org-clock-minutes)))
		(when clocksum
                  (require 'org-duration)
		  (push (cons "CLOCKSUM" (org-duration-from-minutes clocksum))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "CLOCKSUM_T"))
	      (let ((clocksumt (get-text-property (point)
						  :org-clock-minutes-today)))
		(when clocksumt
                  (require 'org-duration)
		  (push (cons "CLOCKSUM_T"
			      (org-duration-from-minutes clocksumt))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "ITEM"))
	      (let ((case-fold-search nil))
		(when (looking-at org-complex-heading-regexp)
		  (push (cons "ITEM"
			      (let ((title (match-string-no-properties 4)))
				(if (org-string-nw-p title)
				    (org-remove-tabs title)
				  "")))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "TODO"))
	      (let ((case-fold-search nil))
		(when (and (looking-at org-todo-line-regexp) (match-end 2))
		  (push (cons "TODO" (match-string-no-properties 2)) props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "PRIORITY"))
              (require 'org-priority)
              (defvar org-priority-default)
	      (push (cons "PRIORITY"
			  (if (looking-at org-priority-regexp)
			      (match-string-no-properties 2)
			    (char-to-string org-priority-default)))
		    props)
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "FILE"))
	      (push (cons "FILE" (buffer-file-name (buffer-base-buffer)))
		    props)
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "TAGS"))
              (require 'org-tags-core)
	      (let ((tags (org-get-tags nil t)))
		(when tags
		  (push (cons "TAGS" (org-make-tag-string tags))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "ALLTAGS"))
	      (let ((tags (org-get-tags)))
		(when tags
		  (push (cons "ALLTAGS" (org-make-tag-string tags))
			props)))
	      (when specific (throw 'exit props)))
	    (when (or (not specific) (string= specific "BLOCKED"))
	      (push (cons "BLOCKED" (if (org-entry-blocked-p) "t" "")) props)
	      (when specific (throw 'exit props)))
	    (when (or (not specific)
		      (member specific '("CLOSED" "DEADLINE" "SCHEDULED")))
	      (forward-line)
	      (when (looking-at-p org-planning-line-re)
		(end-of-line)
		(let ((bol (line-beginning-position))
		      ;; Backward compatibility: time keywords used to
		      ;; be configurable (before 8.3).  Make sure we
		      ;; get the correct keyword.
		      (key-assoc `(("CLOSED" . ,org-closed-string)
				   ("DEADLINE" . ,org-deadline-string)
				   ("SCHEDULED" . ,org-scheduled-string))))
		  (dolist (pair (if specific (list (assoc specific key-assoc))
				  key-assoc))
		    (save-excursion
		      (when (search-backward (cdr pair) bol t)
			(goto-char (match-end 0))
			(skip-chars-forward " \t")
			(and (looking-at org-ts-regexp-both)
			     (push (cons (car pair)
					 (match-string-no-properties 0))
				   props)))))))
	      (when specific (throw 'exit props)))
	    (when (or (not specific)
		      (member specific '("TIMESTAMP" "TIMESTAMP_IA")))
	      (let ((find-ts
		     (lambda (end ts)
		       ;; Fix next timestamp before END.  TS is the
		       ;; list of timestamps found so far.
		       (let ((ts ts)
			     (regexp (cond
				      ((string= specific "TIMESTAMP")
				       org-ts-regexp)
				      ((string= specific "TIMESTAMP_IA")
				       org-ts-regexp-inactive)
				      ((assoc "TIMESTAMP_IA" ts)
				       org-ts-regexp)
				      ((assoc "TIMESTAMP" ts)
				       org-ts-regexp-inactive)
				      (t org-ts-regexp-both))))
			 (catch 'next
			   (while (re-search-forward regexp end t)
			     (backward-char)
			     (let ((object (org-element-context)))
			       ;; Accept to match timestamps in node
			       ;; properties, too.
			       (when (org-element-type-p
                                      object '(node-property timestamp))
				 (let ((type
					(org-element-property :type object)))
				   (cond
				    ((and (memq type '(active active-range))
					  (not (equal specific "TIMESTAMP_IA")))
				     (unless (assoc "TIMESTAMP" ts)
				       (push (cons "TIMESTAMP"
						   (org-element-property
						    :raw-value object))
					     ts)
				       (when specific (throw 'exit ts))))
				    ((and (memq type '(inactive inactive-range))
					  (not (string= specific "TIMESTAMP")))
				     (unless (assoc "TIMESTAMP_IA" ts)
				       (push (cons "TIMESTAMP_IA"
						   (org-element-property
						    :raw-value object))
					     ts)
				       (when specific (throw 'exit ts))))))
				 ;; Both timestamp types are found,
				 ;; move to next part.
				 (when (= (length ts) 2) (throw 'next ts)))))
			   ts)))))
		(goto-char beg)
		;; First look for timestamps within headline.
		(let ((ts (funcall find-ts (line-end-position) nil)))
		  (if (= (length ts) 2) (setq props (nconc ts props))
		    ;; Then find timestamps in the section, skipping
		    ;; planning line.
		    (let ((end (save-excursion (outline-next-heading))))
		      (forward-line)
		      (when (looking-at-p org-planning-line-re) (forward-line))
		      (setq props (nconc (funcall find-ts end ts) props))))))))
	  ;; Get the standard properties, like :PROP:.
	  (when (memq which '(nil all standard))
	    ;; If we are looking after a specific property, delegate
	    ;; to `org-entry-get', which is faster.  However, make an
	    ;; exception for "CATEGORY", since it can be also set
	    ;; through keywords (i.e. #+CATEGORY).
	    (if (and specific (not (equal specific "CATEGORY")))
		(let ((value (org-entry-get beg specific nil t)))
		  (throw 'exit (and value (list (cons specific value)))))
	      (let ((range (org-get-property-block beg)))
		(when range
		  (let ((end (cdr range)) seen-base)
		    (goto-char (car range))
		    ;; Unlike to `org--update-property-plist', we
		    ;; handle the case where base values is found
		    ;; after its extension.  We also forbid standard
		    ;; properties to be named as special properties.
		    (while (re-search-forward org-property-re end t)
		      (let* ((key (upcase (match-string-no-properties 2)))
			     (extendp (string-match-p "\\+\\'" key))
			     (key-base (if extendp (substring key 0 -1) key))
			     (value (match-string-no-properties 3)))
			(cond
			 ((member-ignore-case key-base org-special-properties))
			 (extendp
			  (setq props
				(org--update-property-plist key value props)))
			 ((member key seen-base))
			 (t (push key seen-base)
			    (let ((p (assoc-string key props t)))
			      (if p (setcdr p (concat value " " (cdr p)))
				(push (cons key value) props))))))))))))
	  (unless (assoc "CATEGORY" props)
	    (push (cons "CATEGORY" (org-get-category beg)) props)
	    (when (string= specific "CATEGORY") (throw 'exit props)))
	  ;; Return value.
	  props)))))

(defun org-entry-get (epom property &optional inherit literal-nil)
  "Get value of PROPERTY for entry or content at EPOM.

EPOM is an element, marker, or buffer position.

If INHERIT is non-nil and the entry does not have the property,
then also check higher levels of the hierarchy.  If INHERIT is
the symbol `selective', use inheritance only if the setting in
`org-use-property-inheritance' selects PROPERTY for inheritance.

If the property is present but empty, the return value is the
empty string.  If the property is not present at all, nil is
returned.  In any other case, return the value as a string.
Search is case-insensitive.

If LITERAL-NIL is set, return the string value \"nil\" as
a string, do not interpret it as the list atom nil.  This is used
for inheritance when a \"nil\" value can supersede a non-nil
value higher up the hierarchy."
  (cond
   ((member-ignore-case property (cons "CATEGORY" org-special-properties))
    ;; We need a special property.  Use `org-entry-properties' to
    ;; retrieve it, but specify the wanted property.
    (cdr (assoc-string property (org-entry-properties epom property))))
   ((and inherit
	 (or (not (eq inherit 'selective)) (org-property-inherit-p property)))
    (org-entry-get-with-inheritance property literal-nil epom))
   (t
    (let* ((local (org--property-local-values property literal-nil epom))
	   (value (and local (mapconcat #'identity
                                        (delq nil local)
                                        (org--property-get-separator property)))))
      (if literal-nil value (org-not-nil value))))))

(defun org-property-or-variable-value (var &optional inherit)
  "Check if there is a property fixing the value of VAR.
If yes, return this value.  If not, return the current value of the variable."
  (let ((prop (org-entry-get nil (symbol-name var) inherit)))
    (if (and prop (stringp prop) (string-match "\\S-" prop))
	(read prop)
      (symbol-value var))))

(defun org-entry-member-in-multivalued-property (epom property value)
  "Is VALUE one of the words in the PROPERTY in EPOM?
EPOM is an element, marker, or buffer position."
  (let* ((old (org-entry-get epom property))
	 (values (and old (split-string old))))
    (setq value (org-entry-protect-space value))
    (member value values)))

(defun org-entry-get-multivalued-property (pom property)
  "Return a list of values in a multivalued property."
  (let* ((value (org-entry-get pom property))
	 (values (and value (split-string value))))
    (mapcar #'org-entry-restore-space values)))

(defun org-entry-protect-space (s)
  "Protect spaces and newline in string S."
  (while (string-match " " s)
    (setq s (replace-match "%20" t t s)))
  (while (string-match "\n" s)
    (setq s (replace-match "%0A" t t s)))
  s)

(defun org-entry-restore-space (s)
  "Restore spaces and newline in string S."
  (while (string-match "%20" s)
    (setq s (replace-match " " t t s)))
  (while (string-match "%0A" s)
    (setq s (replace-match "\n" t t s)))
  s)

(defvar org-entry-property-inherited-from (make-marker)
  "Marker pointing to the entry from where a property was inherited.
Each call to `org-entry-get-with-inheritance' will set this marker to the
location of the entry where the inheritance search matched.  If there was
no match, the marker will point nowhere.
Note that also `org-entry-get' calls this function, if the INHERIT flag
is set.")

(defun org-entry-get-with-inheritance (property &optional literal-nil epom)
  "Get PROPERTY of entry or content at EPOM, search higher levels if needed.
EPOM can be a point, marker, or syntax node.
The search will stop at the first ancestor which has the property defined.
If the value found is \"nil\", return nil to show that the property
should be considered as undefined (this is the meaning of nil here).
However, if LITERAL-NIL is set, return the string value \"nil\" instead."
  (move-marker org-entry-property-inherited-from nil)
  (let (values found-inherited?)
    (org-element-lineage-map
        (org-element-at-point epom)
        (lambda (el)
          (pcase-let ((`(,val . ,val+)
                       ;; Force LITERAL-NIL t.
                       ;; Note that the return value may be taken from
                       ;; cache and should not be modified by side effect.
                       (org--property-local-values property t el)))
            (if (not val)
                ;; PROPERTY+
                (prog1 nil ; keep looking for PROPERTY
                  (when val+ (setq values (append (delete nil val+) values))))
              (setq values (cons val (append (delete nil val+) values)))
              (move-marker
               org-entry-property-inherited-from
               (org-element-begin el)
               (org-element-property :buffer el))
              ;; Found inherited direct PROPERTY.
              (setq found-inherited? t))))
      '(inlinetask headline org-data)
      'with-self 'first-match)
    ;; Consider global properties, if we found no PROPERTY (or maybe
    ;; only PROPERTY+).
    (unless found-inherited?
      (when-let ((global (org--property-global-or-keyword-value
                          property t)))
        (setq values (cons global values))))
    (when values
      (setq values (mapconcat
                    #'identity values
                    (org--property-get-separator property))))
    (if literal-nil values (org-not-nil values))))

(defun org-buffer-property-keys (&optional specials defaults columns)
  "Get all property keys in the current buffer.

When SPECIALS is non-nil, also list the special properties that
reflect things like tags and TODO state.

When DEFAULTS is non-nil, also include properties that has
special meaning internally: ARCHIVE, CATEGORY, SUMMARY,
DESCRIPTION, LOCATION, and LOGGING and others.

When COLUMNS in non-nil, also include property names given in
COLUMN formats in the current buffer."
  (let ((case-fold-search t)
	(props (append
		(and specials org-special-properties)
		(and defaults (cons org-effort-property org-default-properties))
		;; Get property names from #+PROPERTY keywords as well
		(mapcar (lambda (s)
			  (nth 0 (split-string s)))
                        (org-element-property :PROPERTY (org-element-org-data))))))
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward org-property-start-re nil t)
       (catch :skip
	 (let ((range (org-get-property-block)))
	   (unless range (throw :skip nil))
	   (goto-char (car range))
	   (let ((begin (car range))
		 (end (cdr range)))
	     ;; Make sure that found property block is not located
	     ;; before current point, as it would generate an infloop.
	     ;; It can happen, for example, in the following
	     ;; situation:
	     ;;
	     ;; * Headline
	     ;;   :PROPERTIES:
	     ;;   ...
	     ;;   :END:
	     ;; *************** Inlinetask
	     ;; #+BEGIN_EXAMPLE
	     ;; :PROPERTIES:
	     ;; #+END_EXAMPLE
	     ;;
	     (if (< begin (point)) (throw :skip nil) (goto-char begin))
	     (while (< (point) end)
	       (let ((p (progn (looking-at org-property-re)
			       (match-string-no-properties 2))))
		 ;; Only add true property name, not extension symbol.
		 (push (if (not (string-match-p "\\+\\'" p)) p
			 (substring p 0 -1))
		       props))
	       (forward-line))))
	 (outline-next-heading)))
     (when columns
       (goto-char (point-min))
       (while (re-search-forward "^[ \t]*\\(?:#\\+\\|:\\)COLUMNS:" nil t)
	 (let ((element (org-element-at-point)))
	   (when (org-element-type-p element '(keyword node-property))
	     (let ((value (org-element-property :value element))
		   (start 0))
	       (while (string-match "%[0-9]*\\([[:alnum:]_-]+\\)\\(([^)]+)\\)?\
\\(?:{[^}]+}\\)?"
				    value start)
		 (setq start (match-end 0))
		 (let ((p (match-string-no-properties 1 value)))
		   (unless (member-ignore-case p org-special-properties)
		     (push p props))))))))))
    (sort (delete-dups
	   (append props
		   ;; for each xxx_ALL property, make sure the bare
		   ;; xxx property is also included
		   (delq nil (mapcar (lambda (p)
				     (and (string-match-p "._ALL\\'" p)
					  (substring p 0 -4)))
				   props))))
	  (lambda (a b) (string< (upcase a) (upcase b))))))

(defun org-property-values (key)
  "List all non-nil values of property KEY in current buffer."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((case-fold-search t)
	 (re (org-re-property key))
	 values)
     (while (re-search-forward re nil t)
       (push (org-entry-get (point) key) values))
     (delete-dups values))))

(defun org-get-category (&optional pos _)
  "Get the category applying to position POS.
Return \"???\" when no category is set.

This function may modify the match data."
  ;; Sync cache.
  (cond
   ((org-entry-get-with-inheritance
     "CATEGORY" nil (or pos (point))))
   ((null org-category)
    (when (org-with-base-buffer nil
            buffer-file-name)
      (file-name-sans-extension
       (file-name-nondirectory
        (org-with-base-buffer nil
          buffer-file-name)))))
   ((symbolp org-category) (symbol-name org-category))
   ((stringp org-category) org-category)
   (t "???")))

(defun org-entry-is-todo-p ()
  (member (org-get-todo-state) org-not-done-keywords))

(defun org-entry-is-done-p ()
  (member (org-get-todo-state) org-done-keywords))

(defun org-get-todo-state ()
  "Return the TODO keyword of the current subtree."
  (save-excursion
    (org-back-to-heading t)
    (and (let ((case-fold-search nil))
           (looking-at org-todo-line-regexp))
	 (match-end 2)
	 (match-string 2))))

(defun org-get-repeat (&optional timestamp)
  "Check if there is a timestamp with repeater in this entry.

Return the repeater, as a string, or nil.  Also return nil when
this function is called before first heading.

When optional argument TIMESTAMP is a string, extract the
repeater from there instead."
  (save-match-data
    (cond
     (timestamp
      (and (string-match org-repeat-re timestamp)
	   (match-string-no-properties 1 timestamp)))
     ((org-before-first-heading-p) nil)
     (t
      (save-excursion
	(org-back-to-heading t)
	(let ((end (org-entry-end-position)))
	  (catch :repeat
	    (while (re-search-forward org-repeat-re end t)
	      (when (save-match-data (org-at-timestamp-p 'agenda))
		(throw :repeat (match-string-no-properties 1)))))))))))

(declare-function org-time-string-to-time "org-time" (s))
(defun org-get-scheduled-time (pom &optional inherit)
  "Get the scheduled time as a time tuple, of a format suitable
for calling org-schedule with, or if there is no scheduling,
returns nil."
  (let ((time (org-entry-get pom "SCHEDULED" inherit)))
    (when time
      (require 'org-time)
      (org-time-string-to-time time))))

(defun org-get-deadline-time (pom &optional inherit)
  "Get the deadline as a time tuple, of a format suitable for
calling org-deadline with, or if there is no scheduling, returns
nil."
  (let ((time (org-entry-get pom "DEADLINE" inherit)))
    (when time
      (org-time-string-to-time time))))

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

(defvar org--matcher-tags-todo-only nil)
(declare-function org--tag-add-to-alist "org-mode" (alist1 alist2))
(declare-function org-global-tags-completion-table "org-tags" (&optional files))
(declare-function org-tags-expand "org-tags-common" (match &optional single-as-list))
(declare-function org-get-buffer-tags "org-tags-core" ())
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
  (require 'org-tags)
  (require 'org-mode)
  (unless match
    ;; Get a new match request, with completion against the global
    ;; tags table and the local tags in current buffer.
    (require 'org-tags)
    (defvar org-last-tags-completion-table)
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

(provide 'org-property)

;;; org-property.el ends here
