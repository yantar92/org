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
(require 'org-priority-common)
(require 'org-time)
(declare-function org-inlinetask-in-task-p "org-inlinetask")

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

(defun org-entry-blocked-p (&optional epom)
  "Non-nil if entry at EPOM is blocked.
EPOM is an element, point, or marker."
  (and (not (org-entry-get epom "NOBLOCKING"))
       (org-entry-is-todo-p epom)
       (org-with-point-at epom
         (not (run-hook-with-args-until-failure
	     'org-blocker-hook
	     (list :type 'todo-state-change
		   :position (point)
		   :from 'todo
		   :to 'done))))))

(defun org--entry-clocksum (&optional _)
  "Return cached CLOCKSUM at point as duration.
The duration is computed via `org-duration-from-minutes'.
The return value is a (\"CLOCKSUM\" . value)."
  (let ((clocksum (get-text-property (point) :org-clock-minutes)))
    (when clocksum
      (require 'org-duration)
      (cons "CLOCKSUM" (org-duration-from-minutes clocksum)))))

(defun org--entry-clocksum-today (&optional _)
  "Return cached CLOCKSUM for current entry.
The return value is a (\"CLOCKSUM_T\" . value)."
  (let ((clocksum (get-text-property (point) :org-clock-minutes-today)))
    (when clocksum
      (require 'org-duration)
      (cons "CLOCKSUM_T" (org-duration-from-minutes clocksum)))))

(defun org--entry-item (heading)
  "Return ITEM name for HEADING.
The return value is a (\"ITEM\" . value)."
  (cons "ITEM" (or (org-element-property :title heading) "")))

(defun org--entry-todo (heading)
  "Return TODO keyword for HEADING.
The return value is (\"TODO\" . keyword)."
  (when-let ((kwd (org-element-property :todo-keyword heading)))
    (cons "TODO" kwd)))

(defun org--entry-file (&optional _)
  "Return current FILE.
The return value is (\"FILE\" . filename)."
  (cons "FILE" (buffer-file-name (buffer-base-buffer))))

(defun org--entry-alltags (heading &optional local)
  "Return all the tags, including inherited, for HEADING.
The return value is (\"ALLTAGS\" . TAG-STRING).
When LOCAL is non-nil, only return local tags as
 (\"TAGS\" . LOCAL-TAG-STRING)."
  (require 'org-tags-core)
  (when-let ((tags (org-get-tags heading local)))
    (cons (if local "TAGS" "ALLTAGS") (org-make-tag-string tags))))

(defun org--entry-tags (heading)
  "Return local tags for HEADING.
The return value is (\"TAGS\" . TAG-STRING)."
  (org--entry-alltags heading 'local))

(defun org--entry-blocked (heading)
  "Return BLOCKED property of HEADING.
The return value is (\"BLOCKED\" . FLAG), where FLAG is either \"t\"
(blocked) or empty string \"\" (not blocked)."
  (cons "BLOCKED" (if (org-entry-blocked-p heading) "t" "")))

(defun org--entry-closed (heading)
  "Return CLOSED timestamp for HEADING.
The return value is (\"CLOSED\" . timestamp-string)."
  (when-let ((ts (org-element-property :closed heading)))
    (cons "CLOSED" (org-element-interpret-data ts))))

(defun org--entry-deadline (heading)
  "Return DEADLINE timestamp for HEADING.
The return value is (\"DEADLINE\" . timestamp-string)."
  (when-let ((ts (org-element-property :deadline heading)))
    (cons "DEADLINE" (org-element-interpret-data ts))))

(defun org--entry-scheduled (heading)
  "Return SCHEDULED timestamp for HEADING.
The return value is (\"SCHEDULED\" . timestamp-string)."
  (when-let ((ts (org-element-property :scheduled heading)))
    (cons "SCHEDULED" (org-element-interpret-data ts))))

(defun org--entry-priority (heading)
  "Return priority for HEADING.
The return value is a (\"PRIORITY\" . value)."
  (cons "PRIORITY"
        (char-to-string
         (or (org-element-property :priority heading)
             org-priority-default))))

(defun org--entry-timestamp (heading &optional inactive)
  "Find next active timestamp in current HEADING.
Search both timestamp objects and timestamp-matching node property
values.

Return (\"TIMESTAMP\" . timestamp-string).

When INACTIVE is non-nil, search inactive timestamps and return
 (\"TIMESTAMP_IA\" . timestamp-string)."
  (let ((cached (org-element-cache-get-key
                 heading
                 (if inactive :entry-timestamp-inactive :entry-timestamp)
                 'none)))
    (if (not (eq cached 'none)) cached
      (org-element-cache-store-key
       heading
       (if inactive :entry-timestamp-inactive :entry-timestamp)
       (org-with-point-at heading
         (let ((regexp (if inactive org-ts-regexp-inactive org-ts-regexp))
               (limit (save-excursion (outline-next-heading))))
           (catch 'found
             (while (re-search-forward regexp limit t)
	       (backward-char)
	       (let* ((raw-value (match-string 0))
                      (object (org-element-context))
                      (type (org-element-type object)))
	         ;; Accept to match timestamps in node
	         ;; properties, too.
	         (when (or (eq 'node-property type)
                           (and (eq 'timestamp type)
                                (memq (org-element-property :type object)
                                      (if inactive
                                          '(inactive inactive-range)
                                        '(active active-range)))))
                   (throw 'found (cons (if inactive "TIMESTAMP_IA" "TIMESTAMP")
                                       raw-value))))))))))))

(defun org--entry-timestamp-inactive (heading)
  "Find first inactive timestamp in HEADING.
See `org--entry-timestamp' for more details about the return value."
  (org--entry-timestamp heading 'inactive))

(declare-function org-get-tags "org-tags-core" (&optional epom local))
(declare-function org-make-tag-string "org-tags-core" (tags))
(declare-function org-duration-from-minutes "org-duration"
                  (minutes &optional fmt canonical))
(defun org-entry-properties (&optional epom which local-category)
  "Get all properties of the current entry.

When EPOM is a buffer position, marker, or element, get all properties
from the entry there instead.

This includes the TODO keyword, the tags, time strings for
deadline, scheduled, and clocking, and any additional properties
defined in the entry.

If WHICH is nil or `all', get all properties.  If WHICH is
`special' or `standard', only get that subclass.  If WHICH is
a string, only get that property.

When optional argument LOCAL-CATEGORY is non-nil, only consider
explicitly set CATEGORY property in heading's property drawer.

Return value is an alist.  Keys are properties, as upcased
strings."
  (org-with-point-at epom
    (when (and (derived-mode-p 'org-mode)
	       (org-back-to-heading-or-point-min t))
      (catch 'exit
	(let* ((heading (or (org-headline-at-point epom) (org-element-org-data epom)))
	       (specific (and (stringp which) (upcase which)))
	       (which (cond ((not specific) which)
			    ((member specific org-special-properties) 'special)
			    (t 'standard)))
               (special-prop-rules
                '(("CLOCKSUM" . org--entry-clocksum)
                  ("CLOCKSUM_T" . org--entry-clocksum-today)
                  ("ITEM" . org--entry-item)
                  ("TODO" . org--entry-todo)
                  ("PRIORITY" . org--entry-priority)
                  ("FILE" . org--entry-file)
                  ("TAGS" . org--entry-tags)
                  ("ALLTAGS" . org--entry-alltags)
                  ("BLOCKED" . org--entry-blocked)
                  ("CLOSED" . org--entry-closed)
                  ("DEADLINE" . org--entry-deadline)
                  ("SCHEDULED" . org--entry-scheduled)
                  ("TIMESTAMP" . org--entry-timestamp)
                  ("TIMESTAMP_IA" . org--entry-timestamp-inactive)))
	       props)
	  ;; Get the special properties, like TODO and TAGS.
	  (when (memq which '(nil all special))
            (when-let ((fun (and specific
                                 (cdr (assoc-string
                                       specific special-prop-rules)))))
              (throw 'exit (list (funcall fun heading))))
            (unless specific
              (dolist (pair special-prop-rules)
                (push (funcall (cdr pair) heading) props)))
            (setq props (delq nil props)))
	  ;; Get the standard properties, like :PROP:.
	  (when (memq which '(nil all standard))
            (unless local-category
	      (push (cons "CATEGORY" (org-get-category heading)) props)
	      (when (string= specific "CATEGORY") (throw 'exit props)))
            (if specific
                (throw 'exit
                       (list (cons specific
                                   (org-entry-get
                                    heading specific nil t))))
              (org-element-properties-mapc
               (lambda (property _ heading)
                 (let ((name (substring (symbol-name property) 1)))
	           ;; If we are looking after a specific property, delegate
	           ;; to `org-entry-get', which is faster.  However, make an
	           ;; exception for "CATEGORY", since it can be also set
	           ;; through keywords (i.e. #+CATEGORY).
                   (when (and (equal name (upcase name)) ; Only local properties.
                              (or local-category (not (equal name "CATEGORY"))))
                     (when-let ((value (org-entry-get heading name nil t)))
                       (push (cons name value) props)))))
               heading)))
	  ;; Return value.
	  props)))))

;;;###autoload
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
  (let* ((case-fold-search t)
	 (props (append
		 (and specials org-special-properties)
		 (and defaults (cons org-effort-property org-default-properties))
		 ;; Get property names from #+PROPERTY keywords as well
		 (mapcar (lambda (s)
			   (nth 0 (split-string s)))
                         (org-element-property :PROPERTY (org-element-org-data)))))
         (props-hash (make-hash-table :test #'equal))
         (add-column-props
          (lambda (value)
            (when value
              (let ((start 0))
	        (while (string-match "%[0-9]*\\([[:alnum:]_-]+\\)\\(([^)]+)\\)?\\(?:{[^}]+}\\)?"
				     value start)
	          (setq start (match-end 0))
	          (let ((p (match-string-no-properties 1 value)))
		    (unless (member-ignore-case p org-special-properties)
		      (push p props)))))))))
    (when columns
      (funcall add-column-props
               (org-element-property :COLUMNS (org-element-org-data))))
    ;; Add headline properties.
    (org-element-cache-map
     (lambda (heading)
       (dolist (pair (org-entry-properties heading 'standard 'local-category))
         (when (and columns (equal (car pair) "COLUMNS"))
           ;; Special case: add properties listed in COLUMNS.
           (funcall add-column-props (cdr pair)))
         (unless (or (gethash (car pair) props-hash)
                     (and (equal (car pair) "CATEGORY")
                          (not (org-element-property :CATEGORY heading))))
           (puthash (car pair) t props-hash)
           (push (car pair) props)))))
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

(defun org-get-category (&optional epom _)
  "Get the category at EPOM.
EPOM is an element, point, or marker.
Return \"???\" when no category is set.

This function may modify the match data."
  (org-with-point-at epom
    (cond
     ((org-element-property-inherited
       :CATEGORY (org-element-at-point epom) t))
     ((null org-category)
      (when (org-with-base-buffer nil
              buffer-file-name)
        (file-name-sans-extension
         (file-name-nondirectory
          (org-with-base-buffer nil
            buffer-file-name)))))
     ((symbolp org-category) (symbol-name org-category))
     ((stringp org-category) org-category)
     (t "???"))))

(defun org-entry-is-todo-p (&optional epom)
  "Return non-nil when entry at EPOM is marked with not-done todo keyword.
EPOM is an element, point, or marker."
  (eq 'todo (org-element-property :todo-type (org-headline-at-point epom))))

(defun org-entry-is-done-p (&optional epom)
  "Return non-nil when entry at EPOM is marked with done keyword.
EPOM is an element, point, or marker."
  (eq 'done (org-element-property :todo-type (org-headline-at-point epom))))

(defun org-get-todo-state (&optional epom)
  "Return the TODO keyword of subtree at EPOM.
EPOM is an element, point, or marker."
  (org-element-property :todo-keyword (org-headline-at-point epom)))

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
  (require 'org-mode)
  (unless match
    ;; Get a new match request, with completion against the global
    ;; tags table and the local tags in current buffer.
    (require 'org-tags)
    (defvar crm-separator) ; crm.el
    (setq match
	  (completing-read-multiple
	   "Match: "
           (org--settings-add-to-alist
            (when (derived-mode-p 'org-mode)
	      (org-get-buffer-tags))
	    (unless only-local-tags
	      (org-global-tags-completion-table)))
           nil nil nil 'org-tags-history)))

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
	(setq matcher `(and (org-element-keyword-not-done-p todo) ,matcher)))
      (cons match0
            (byte-compile
             `(lambda (todo tags-list level)
                ;; Pacify byte-compiler.
                (ignore todo) (ignore tags-list) (ignore level)
                ,matcher))))))

(provide 'org-property)

;;; org-property.el ends here
