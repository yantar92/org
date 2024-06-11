;;; org-property.el-set --- Setting Org properties                      -*- lexical-binding: t; -*-

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

;; This library implements setting Org mode headline properties.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-property)
(require 'org-property-core)
(require 'org-indent-static)
(require 'org-priority-common)
(declare-function org-inlinetask-in-task-p "org-inlinetask")


(declare-function org-clock-update-clock-status "org-clock")

(defun org--valid-property-p (property)
  "Non-nil when string PROPERTY is a valid property name."
  (not
   (or (equal property "")
       (string-match-p "\\s-" property))))

(declare-function org-compute-property-at-point "org-colview" ())
(defun org-property-action ()
  "Do an action on properties."
  (interactive)
  (message "Property Action:  [s]et  [d]elete  [D]elete globally  [c]ompute")
  (let ((c (read-char-exclusive)))
    (cl-case c
      (?s (call-interactively #'org-set-property))
      (?d (call-interactively #'org-delete-property))
      (?D (call-interactively #'org-delete-property-globally))
      (?c (progn
            (require 'org-colview)
            (call-interactively #'org-compute-property-at-point)))
      (otherwise (user-error "No such property action %c" c)))))

;;;###autoload
(defun org-inc-effort ()
  "Increment the value of the effort property in the current entry."
  (interactive)
  (org-set-effort t))

(defvar org-clock-effort)       ; Defined in org-clock.el.
(defvar org-clock-current-task) ; Defined in org-clock.el.
(declare-function org-duration-to-minutes "org-duration" (duration &optional canonical))
;;;###autoload
(defun org-set-effort (&optional increment value)
  "Set the effort property of the current entry.
If INCREMENT is non-nil, set the property to the next allowed
value.  Otherwise, if optional argument VALUE is provided, use
it.  Eventually, prompt for the new value if none of the previous
variables is set."
  (interactive "P")
  (let* ((allowed (org-property-get-allowed-values nil org-effort-property t))
	 (current (org-entry-get nil org-effort-property))
	 (value
	  (cond
	   (increment
	    (unless allowed (user-error "Allowed effort values are not set"))
	    (or (cl-caadr (member (list current) allowed))
		(user-error "Unknown value %S among allowed values" current)))
	   (value
	    (if (stringp value) value
	      (error "Invalid effort value: %S" value)))
	   (t
	    (let ((must-match
		   (and allowed
			(not (get-text-property 0 'org-unrestricted
					      (caar allowed))))))
	      (completing-read "Effort: " allowed nil must-match))))))
    ;; Test whether the value can be interpreted as a duration before
    ;; inserting it in the buffer:
    (require 'org-duration)
    (org-duration-to-minutes value)
    ;; Maybe update the effort value:
    (unless (equal current value)
      (org-entry-put nil org-effort-property value))
    (when (equal (org-get-heading t t t t)
		 (bound-and-true-p org-clock-current-task))
      (setq org-clock-effort value)
      (org-clock-update-clock-status))
    (message "%s is now %s" org-effort-property value)))

;;;###autoload
(defun org-entry-delete (epom property)
  "Delete PROPERTY from entry at element, point, or marker EPOM.
Accumulated properties, i.e. PROPERTY+, are also removed.  Return
non-nil when a property was removed."
  (org-with-point-at epom
    (pcase (org-get-property-block)
      (`(,begin . ,origin)
       (let* ((end (copy-marker origin))
	      (re (org-re-property
		   (concat (regexp-quote property) "\\+?") t t)))
	 (goto-char begin)
	 (while (re-search-forward re end t)
	   (delete-region (match-beginning 0) (line-beginning-position 2)))
	 ;; If drawer is empty, remove it altogether.
	 (when (= begin end)
	   (delete-region (line-beginning-position 0)
			  (line-beginning-position 2)))
	 ;; Return non-nil if some property was removed.
	 (prog1 (/= end origin) (set-marker end nil))))
      (_ nil))))

;; Multi-values properties are properties that contain multiple values
;; These values are assumed to be single words, separated by whitespace.
(defun org-entry-add-to-multivalued-property (epom property value)
  "Add VALUE to the words in the PROPERTY in entry at EPOM.
EPOM is an element, marker, or buffer position."
  (let* ((old (org-entry-get epom property))
	 (values (and old (split-string old))))
    (setq value (org-entry-protect-space value))
    (unless (member value values)
      (setq values (append values (list value)))
      (org-entry-put epom property (mapconcat #'identity values " ")))))

(defun org-entry-remove-from-multivalued-property (epom property value)
  "Remove VALUE from words in the PROPERTY in entry at EPOM.
EPOM is an element, marker, or buffer position."
  (let* ((old (org-entry-get epom property))
	 (values (and old (split-string old))))
    (setq value (org-entry-protect-space value))
    (when (member value values)
      (setq values (delete value values))
      (org-entry-put epom property (mapconcat #'identity values " ")))))

(defun org-entry-put-multivalued-property (epom property &rest values)
  "Set multivalued PROPERTY at EPOM to VALUES.
VALUES should be a list of strings.  Spaces will be protected.
EPOM is an element, marker, or buffer position."
  (org-entry-put epom property (mapconcat #'org-entry-protect-space values " "))
  (let* ((value (org-entry-get epom property))
	 (values (and value (split-string value))))
    (mapcar #'org-entry-restore-space values)))

(declare-function org-todo "org-todo" (&optional arg))
(declare-function org-deadline "org-planning" (arg &optional time))
(declare-function org-schedule "org-planning" (arg &optional time))
(declare-function org-priority "org-priority" (&optional action show))
(declare-function org-timestamp-change "org-timestamp"
                  (n &optional what updown suppress-tmp-delay))
;;;###autoload
(defun org-entry-put (epom property value)
  "Set PROPERTY to VALUE for entry at EPOM.

EPOM is an element, marker, or buffer position.

If the value is nil, it is converted to the empty string.  If it
is not a string, an error is raised.  Also raise an error on
invalid property names.

PROPERTY can be any regular property (see
`org-special-properties').  It can also be \"TODO\",
\"PRIORITY\", \"SCHEDULED\" and \"DEADLINE\".

For the last two properties, VALUE may have any of the special
values \"earlier\" and \"later\".  The function then increases or
decreases scheduled or deadline date by one day."
  (cond ((null value) (setq value ""))
	((not (stringp value)) (error "Properties values should be strings"))
	((not (org--valid-property-p property))
	 (user-error "Invalid property name: \"%s\"" property)))
  (org-no-read-only
   (org-with-point-at epom
     (if (or (not (featurep 'org-inlinetask)) (org-inlinetask-in-task-p))
	 (org-back-to-heading-or-point-min t)
       (org-with-limited-levels (org-back-to-heading-or-point-min t)))
     (let ((beg (point)))
       (cond
        ((equal property "TODO")
	 (cond ((not (org-string-nw-p value)) (setq value 'none))
	       ((not (member value org-todo-keywords-1))
	        (user-error "\"%s\" is not a valid TODO state" value)))
	 (org-todo value))
        ((equal property "PRIORITY")
	 (org-priority (if (org-string-nw-p value) (string-to-char value) ?\s)))
        ((equal property "SCHEDULED")
	 (forward-line)
	 (if (and (looking-at-p org-planning-line-re)
		  (re-search-forward
		   org-scheduled-time-regexp (line-end-position) t))
             (progn
	       (cond ((string= value "earlier") (org-timestamp-change -1 'day))
		     ((string= value "later") (org-timestamp-change 1 'day))
		     ((string= value "")
                      (require 'org-planning)
                      (org-schedule '(4)))
		     (t
                      (require 'org-planning)
                      (org-schedule nil value))))
	   (if (member value '("earlier" "later" ""))
	       (call-interactively #'org-schedule)
	     (org-schedule nil value))))
        ((equal property "DEADLINE")
	 (forward-line)
	 (if (and (looking-at-p org-planning-line-re)
		  (re-search-forward
		   org-deadline-time-regexp (line-end-position) t))
	     (cond ((string= value "earlier") (org-timestamp-change -1 'day))
		   ((string= value "later") (org-timestamp-change 1 'day))
		   ((string= value "")
                    (require 'org-planning)
                    (org-deadline '(4)))
		   (t
                    (require 'org-planning)
                    (org-deadline nil value)))
	   (if (member value '("earlier" "later" ""))
	       (call-interactively #'org-deadline)
	     (org-deadline nil value))))
        ((member property org-special-properties)
	 (error "The %s property cannot be set with `org-entry-put'" property))
        (t
         (org-fold-core-ignore-modifications
	   (let* ((range (org-get-property-block beg 'force))
	          (end (cdr range))
	          (case-fold-search t))
	     (goto-char (car range))
	     (if (re-search-forward (org-re-property property nil t) end t)
	         (progn (delete-region (match-beginning 0) (match-end 0))
		        (goto-char (match-beginning 0)))
	       (goto-char end)
	       (insert-and-inherit "\n")
	       (backward-char))
	     (insert-and-inherit ":" property ":")
	     (when value (insert-and-inherit " " value))
	     (org-indent-line))))))
     (run-hook-with-args 'org-property-changed-functions property value))))

(defun org-insert-property-drawer ()
  "Insert a property drawer into the current entry.
Do nothing if the drawer already exists.  The newly created
drawer is immediately hidden."
  (org-with-wide-buffer
   ;; Set point to the position where the drawer should be inserted.
   (if (or (not (featurep 'org-inlinetask)) (org-inlinetask-in-task-p))
       (org-back-to-heading-or-point-min t)
     (org-with-limited-levels (org-back-to-heading-or-point-min t)))
   (if (org-before-first-heading-p)
       (while (and (org-at-comment-p) (bolp)) (forward-line))
     (forward-line)
     (when (looking-at-p org-planning-line-re) (forward-line)))
   (unless (looking-at-p org-property-drawer-re)
     ;; Make sure we start editing a line from current entry, not from
     ;; next one.  It prevents extending text properties or overlays
     ;; belonging to the latter.
     (when (and (bolp) (> (point) (point-min))) (backward-char))
     (let ((begin (if (bobp) (point) (1+ (point))))
	   (inhibit-read-only t))
       (unless (bobp) (insert "\n"))
       (insert ":PROPERTIES:\n:END:")
       (org-fold-core-region (line-end-position 0) (point) t 'drawer)
       (when (or (eobp) (= begin (point-min))) (insert "\n"))
       (org-indent-region begin (point))))))

(defun org-set-property-function (property)
  "Get the function that should be used to set PROPERTY.
This is computed according to `org-property-set-functions-alist'."
  (or (cdr (assoc property org-property-set-functions-alist))
      'org-completing-read))

;;;###autoload
(defun org-read-property-value (property &optional epom default)
  "Read value for PROPERTY, as a string.
When optional argument EPOM is non-nil, completion uses additional
information, i.e., allowed or existing values at element, point, or
marker EPOM.
Optional argument DEFAULT provides a default value for PROPERTY."
  (let* ((completion-ignore-case t)
	 (allowed
	  (or (org-property-get-allowed-values nil property 'table)
	      (and epom (org-property-get-allowed-values epom property 'table))))
	 (current (org-entry-get nil property))
	 (prompt (format "%s value%s: "
			 property
			 (if (org-string-nw-p current)
			     (format " [%s]" current)
			   "")))
	 (set-function (org-set-property-function property))
         (default (cond
                   ((not allowed) default)
                   ((member default allowed) default)
                   (t nil))))
    (org-trim
     (if allowed
	 (funcall set-function
		  prompt allowed nil
		  (not (get-text-property 0 'org-unrestricted (caar allowed)))
		  default nil default)
       (let ((all (mapcar #'list
			  (append (org-property-values property)
				  (and epom
				       (org-with-point-at epom
					 (org-property-values property)))))))
	 (funcall set-function prompt all nil nil default nil current))))))

(defvar org-last-set-property nil)
(defvar org-last-set-property-value nil)
(defun org-read-property-name ()
  "Read a property name."
  (let ((completion-ignore-case t)
	(default-prop (or (and (org-at-property-p)
			       (match-string-no-properties 2))
			  org-last-set-property)))
    (org-completing-read
     (concat "Property"
	     (if default-prop (concat " [" default-prop "]") "")
	     ": ")
     (mapcar #'list (org-buffer-property-keys nil t t))
     nil nil nil nil default-prop)))

;;;###autoload
(defun org-set-property-and-value (use-last)
  "Allow setting [PROPERTY]: [value] direction from prompt.
When use-default, don't even ask, just use the last
\"[PROPERTY]: [value]\" string from the history."
  (interactive "P")
  (let* ((completion-ignore-case t)
	 (pv (or (and use-last org-last-set-property-value)
		 (org-completing-read
		  "Enter a \"[Property]: [value]\" pair: "
		  nil nil nil nil nil
		  org-last-set-property-value)))
	 prop val)
    (when (string-match "^[ \t]*\\([^:]+\\):[ \t]*\\(.*\\)[ \t]*$" pv)
      (setq prop (match-string 1 pv)
	    val (match-string 2 pv))
      (org-set-property prop val))))

;;;###autoload
(defun org-set-property (property value)
  "In the current entry, set PROPERTY to VALUE.

When called interactively, this will prompt for a property name, offering
completion on existing and default properties.  And then it will prompt
for a value, offering completion either on allowed values (via an inherited
xxx_ALL property) or on existing values in other instances of this property
in the current file.

Throw an error when trying to set a property with an invalid name."
  (interactive (list nil nil))
  (let ((property (or property (org-read-property-name))))
    ;; `org-entry-put' also makes the following check, but this one
    ;; avoids polluting `org-last-set-property' and
    ;; `org-last-set-property-value' needlessly.
    (unless (org--valid-property-p property)
      (user-error "Invalid property name: \"%s\"" property))
    (let ((value (or value (org-read-property-value property)))
	  (fn (cdr (assoc-string property org-properties-postprocess-alist t))))
      (setq org-last-set-property property)
      (setq org-last-set-property-value (concat property ": " value))
      ;; Possibly postprocess the inserted value:
      (when fn (setq value (funcall fn value)))
      (unless (equal (org-entry-get nil property) value)
	(org-entry-put nil property value)))))

;;;###autoload
(defun org-delete-property (property)
  "In the current entry, delete PROPERTY."
  (interactive
   (let* ((completion-ignore-case t)
	  (cat (org-entry-get (point) "CATEGORY"))
	  (props0 (org-entry-properties nil 'standard))
	  (props (if cat props0
		   (delete `("CATEGORY" . ,(org-get-category)) props0)))
	  (prop (if (< 1 (length props))
		    (completing-read "Property: " props nil t)
		  (caar props))))
     (list prop)))
  (if (not property)
      (message "No property to delete in this entry")
    (org-entry-delete nil property)
    (message "Property \"%s\" deleted" property)))

;;;###autoload
(defun org-delete-property-globally (property)
  "Remove PROPERTY globally, from all entries.
This function ignores narrowing, if any."
  (interactive
   (let* ((completion-ignore-case t)
	  (prop (completing-read
		 "Globally remove property: "
		 (mapcar #'list (org-buffer-property-keys)))))
     (list prop)))
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((count 0)
	 (re (org-re-property (concat (regexp-quote property) "\\+?") t t)))
     (while (re-search-forward re nil t)
       (when (org-entry-delete (point) property) (cl-incf count)))
     (message "Property \"%s\" removed from %d entries" property count))))

(defun org-property-get-allowed-values (epom property &optional table)
  "Get allowed values at EPOM for the property PROPERTY.
EPOM can be an element, marker, or buffer position.
When TABLE is non-nil, return an alist that can directly be used for
completion."
  (let (vals)
    (cond
     ((equal property "TODO")
      (setq vals (org-with-point-at epom
		   (append org-todo-keywords-1 '("")))))
     ((equal property "PRIORITY")
      (let ((n org-priority-lowest))
	(while (>= n org-priority-highest)
	  (push (char-to-string n) vals)
	  (setq n (1- n)))))
     ((equal property "CATEGORY"))
     ((member property org-special-properties))
     ((setq vals (run-hook-with-args-until-success
		  'org-property-allowed-value-functions property)))
     (t
      (setq vals (org-entry-get epom (concat property "_ALL") 'inherit))
      (when (and vals (string-match "\\S-" vals))
	(setq vals (car (read-from-string (concat "(" vals ")"))))
	(setq vals (mapcar (lambda (x)
			     (cond ((stringp x) x)
				   ((numberp x) (number-to-string x))
				   ((symbolp x) (symbol-name x))
				   (t "???")))
			   vals)))))
    (when (member ":ETC" vals)
      (setq vals (remove ":ETC" vals))
      (org-add-props (car vals) '(org-unrestricted t)))
    (if table (mapcar 'list vals) vals)))

;;;###autoload
(defun org-property-previous-allowed-value (&optional _previous)
  "Switch to the next allowed value for this property."
  (interactive)
  (org-property-next-allowed-value t))

;;;###autoload
(defun org-property-next-allowed-value (&optional previous)
  "Switch to the next allowed value for this property."
  (interactive)
  (unless (org-at-property-p)
    (user-error "Not at a property"))
  (let* ((prop (car (save-match-data (org-split-string (match-string 1) ":"))))
	 (key (match-string 2))
	 (value (match-string 3))
	 (allowed (or (org-property-get-allowed-values (point) key)
		      (and (member value  '("[ ]" "[-]" "[X]"))
			   '("[ ]" "[X]"))))
	 (heading (save-match-data (nth 4 (org-heading-components))))
	 nval)
    (unless allowed
      (user-error "Allowed values for this property have not been defined"))
    (when previous (setq allowed (reverse allowed)))
    (when (member value allowed)
      (setq nval (car (cdr (member value allowed)))))
    (setq nval (or nval (car allowed)))
    (when (equal nval value)
      (user-error "Only one allowed value for this property"))
    (org-at-property-p)
    (replace-match (concat " :" key ": " nval) t t)
    (org-indent-line)
    (forward-line 0)
    (skip-chars-forward " \t")
    (when (equal prop org-effort-property)
      (when (string= org-clock-current-task heading)
	(setq org-clock-effort nval)
	(org-clock-update-clock-status)))
    (run-hook-with-args 'org-property-changed-functions key nval)))

(provide 'org-property-set)

;;; org-property-set.el ends here
