;;; org-property-core.el --- Org property drawer API                      -*- lexical-binding: t; -*-

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

;; This library implements retrieving Org mode properties stored in
;; property drawers.

;;; Code:

(require 'org-element)

(defcustom org-use-property-inheritance nil
  "Non-nil means properties apply also for sublevels.

This setting is chiefly used during property searches.  Turning it on can
cause significant overhead when doing a search, which is why it is not
on by default.

When nil, only the properties directly given in the current entry count.
When t, every property is inherited.  The value may also be a list of
properties that should have inheritance, or a regular expression matching
properties that should be inherited.

However, note that some special properties use inheritance under special
circumstances (not in searches).  Examples are CATEGORY, ARCHIVE, COLUMNS,
and the properties ending in \"_ALL\" when they are used as descriptor
for valid values of a property.

Note for programmers:
When querying an entry with `org-entry-get', you can control if inheritance
should be used.  By default, `org-entry-get' looks only at the local
properties.  You can request inheritance by setting the inherit argument
to t (to force inheritance) or to `selective' (to respect the setting
in this variable)."
  :group 'org-properties
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Always" t)
	  (repeat :tag "Specific properties" (string :tag "Property"))
	  (regexp :tag "Properties matched by regexp")))

(defun org-property-inherit-p (property)
  "Return a non-nil value if PROPERTY should be inherited."
  (cond
   ((eq org-use-property-inheritance t) t)
   ((not org-use-property-inheritance) nil)
   ((stringp org-use-property-inheritance)
    (string-match org-use-property-inheritance property))
   ((listp org-use-property-inheritance)
    (member-ignore-case property org-use-property-inheritance))
   (t (error "Invalid setting of `org-use-property-inheritance'"))))

(defcustom org-property-separators nil
  "An alist to control how properties are combined.

The car of each item should be either a list of property names or
a regular expression, while the cdr should be the separator to
use when combining that property.

If an alist item cannot be found that matches a given property, a
single space will be used as the separator."
  :group 'org-properties
  :package-version '(Org . "9.6")
  :type '(alist :key-type (choice (repeat :tag "Properties" string)
                                  (string :tag "Regular Expression"))
                :value-type (restricted-sexp :tag "Separator"
                                             :match-alternatives (stringp)
                                             :value " ")))

(defun org--property-get-separator (property)
  "Get the separator to use for combining PROPERTY."
  (or
   (catch 'separator
     (dolist (spec org-property-separators)
       (if (listp (car spec))
           (if (member property (car spec))
               (throw 'separator (cdr spec)))
         (if (string-match-p (car spec) property)
             (throw 'separator (cdr spec))))))
   " "))

(defconst org-global-properties-fixed
  '(("VISIBILITY_ALL" . "folded children content all")
    ("CLOCK_MODELINE_TOTAL_ALL" . "current today repeat all auto"))
  "List of property/value pairs that can be inherited by any entry.

These are fixed values, for the preset properties.  The user variable
that can be used to add to this list is `org-global-properties'.

The entries in this list are cons cells where the car is a property
name and cdr is a string with the value.  If the value represents
multiple items like an \"_ALL\" property, separate the items by
spaces.")

(defcustom org-global-properties nil
  "List of property/value pairs that can be inherited by any entry.

This list will be combined with the constant `org-global-properties-fixed'.

The entries in this list are cons cells where the car is a property
name and cdr is a string with the value.

Buffer local properties are added either by a document property drawer

:PROPERTIES:
:NAME: VALUE
:END:

or by adding lines like

#+PROPERTY: NAME VALUE"
  :group 'org-properties
  :type '(repeat
	  (cons (string :tag "Property")
		(string :tag "Value"))))

(defun org--property-local-values (property literal-nil &optional epom)
  "Return value for PROPERTY in current entry or at EPOM.
EPOM can be point, marker, or syntax node.

Value is a list whose car is the base value for PROPERTY and cdr
a list of accumulated values.  Return nil if neither is found in
the entry.  Also return nil when PROPERTY is set to \"nil\",
unless LITERAL-NIL is non-nil."
  (setq epom
        (org-element-lineage
         (org-element-at-point epom)
         '(headline inlinetask org-data)
         'with-self))
  (let* ((base-value  (org-element-property (intern (concat ":" (upcase property)    )) epom))
         (extra-value (org-element-property (intern (concat ":" (upcase property) "+")) epom))
         (extra-value (if (listp extra-value) extra-value (list extra-value)))
         (value (if literal-nil (cons base-value extra-value)
                  (cons (org-not-nil base-value) (org-not-nil extra-value)))))
    (and (not (equal value '(nil))) value)))

(defun org--property-global-or-keyword-value (property literal-nil)
  "Return value for PROPERTY as defined by global properties or by keyword.
Return value is a string.  Return nil if property is not set
globally or by keyword.  Also return nil when PROPERTY is set to
\"nil\", unless LITERAL-NIL is non-nil."
  (let ((global
         (or
          (let ((main-value
                 (org-element-property
                  (intern (format ":%s" property))
                  (org-element-org-data)))
                (extra-value
                 (org-element-property
                  (intern (format ":%s+" property))
                  (org-element-org-data))))
            (when (or main-value extra-value)
              (mapconcat
               #'identity
               (delq nil (cons main-value extra-value))
               (org--property-get-separator property))))
	  (cdr (or (assoc-string property org-global-properties t)
	           (assoc-string property org-global-properties-fixed t))))))
    (if literal-nil global (org-not-nil global))))

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

(provide 'org-property-core)

;;; org-property-core.el ends here
