;;; org-element-ast.el --- Abstract syntax tree for Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ihor Radchenko

;; Author: Ihor Radchenko <yantar92@posteo.net>
;; Keywords: data, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file implements Org abstract syntax tree (AST) data structure.
;;
;; Only the most generic aspect of the syntax tree are considered
;; below.  The fine details of Org syntax are implemented elsewhere.
;;
;; 1. Syntax elements
;; ------------------
;; Each Org syntax element can be represented as a string or list.
;;
;; The main element representation follows the pattern
;; (TYPE PROPERTIES CONTENTS), where
;;   TYPE is a symbol describing the element type.
;;   PROPERTIES is the property list attached to it.
;;   CONTENTS is a list of syntax elements contained in the current
;;            element, when applicable.
;;
;;; For example, "*bold text*  " element can be represented as
;;
;;    (bold (:begin 1 :end 14 :post-blank 2 ...) "bold text")
;;
;; TYPE can be any symbol, including symbol not explicitly defined by
;; Org syntax.  If TYPE is not a part of the syntax, the syntax
;; element is called "pseudo element", but otherwise considered a
;; valid part of Org syntax tree.  Search "Pseudo objects and
;; elements" in lisp/ox-latex.el for an example of using pseudo
;; elements.
;;
;; PROPERTIES is a property list (:property1 value1 :property2 value2 ...)
;; holding properties and value.
;;
;; `:standard-properties', `:parent', `:deferred', and `:secondary'
;; properties are treated specially in the code below.
;;
;; `:standard-properties' holds an array with
;; `org-element--standard-properties' values, in the same order.  The
;; values in the array have priority over the same properties
;; specified in the property list.  You should not rely on the value
;; of `org-element--standard-propreties' in the code.
;; `:standard-properties' may or may not be actually present in
;; PROPERTIES.  It is mostly used to speed up property access in
;; performance-critical code.
;;
;; The previous example can also be presented in more compact form as:
;;
;;    (bold (:standard-properties [1 10 ... 2 ...]) "bold text")
;;
;; Using an array allows faster access to frequently used properties.
;;
;; `:parent' holds the containing element, for a child element within
;; the AST.  It may or may not be present in PROPERTIES.
;;
;; `:secondary' holds a list of properties that may contain extra AST
;; elements, in addition to the element contents.
;;
;; `deferred' property describes how to update not-yet-calculated
;; properties on request.
;;
;;
;; Syntax element can also be represented as a string.  Strings always
;; represent syntax element of `plain-text' type with contents being nil
;; and properties represented as string properties at position 0.
;; `:standard-properties' are not considered for `plain-text' elements
;; as `plain-text' elements tend to hold much less properties.
;;
;; In the above example, `plain-text' element "bold text" is more
;; accurately represented as
;;
;;    #("bold text" 0 9 (:parent (bold ...)))
;;
;; with :parent property value pointing back to the containing bold
;; element.
;;
;; `anonymous' syntax element is represented as a list with `car'
;; containing another syntax element.  Such element has nil type, does
;; not have properties, and its contents is a list of the contained
;; syntax elements.  `:parent' property of the contained elements
;; point back to the list itself, except when `anonymous' element
;; holds secondary value (see below), in which case the `:parent'
;; property is set to be the containing element in AST.
;;
;; Any element representation other then described above is not
;; considered as Org syntax element.
;;
;; 2. Deferred values
;; ------------------
;; Sometimes, it is computationally expensive or even not possible to
;; calculate property values when creating the AST element.  The value
;; calculation can be deferred to the time the value is requested.
;;
;; Property values and contained elements may have a special value of
;; `org-element-deferred' type.  Such values are computed dynamically.
;; Either every time the property value is requested or just the first
;; time.  In the latter case, the `org-element-deferred' property
;; value is auto-replaced with the dynamically computed result.
;;
;; Sometimes, even property names (not just property values) cannot, or
;; should not be computed in advance.  If a special property
;; `:deferred' has the value of `org-element-deferred-type', it is
;; first resolved for side effects of setting the missing properties.
;; The resolved value is re-assigned to the `:deferred' property.
;;
;; Note that `org-element-copy' unconditionally resolves deferred
;; properties.  This is useful to generate side-effect-free AST.
;;
;; 3. Org document representation
;; ------------------------------
;; Document AST is represented by nested Org elements.
;;
;; Each element in the AST can hold the contained elements in its CONTENTS or
;; as values of properties.
;;
;; For example, (bold (...) "bold text") `bold' element contains
;; `plain-text' element in CONTENTS.
;;
;; The containing element is called "parent element".
;;
;; The contained elements held inside CONTENTS are called "child elements".
;; They must have their `:parent' property set to the containing
;; parent element.
;;
;; The contained elements can also be held as property values.  Such
;; elements are called "secondary elements".  Only certain properties
;; can contribute to AST - the property names listed as the value of
;; special property `:secondary'
;;
;; For example,
;;
;;   (headline ((:secondary (:title)
;;               :title (#("text" 0 4 (:parent (headline ...)))))))
;;
;; is a parent headline element containing "text" secondary string
;; element inside `:title' property.  Note that `:title' is listed in
;; `:secondary' value.
;;
;; The following example illustrates an example AST for Org document:
;;
;; ---- Org document --------
;; * Heading with *bold* text
;; Paragraph.
;; ---- end -----------------
;;
;; (org-data (...) ; `org-data' element.
;;   (headline
;;     (
;;      ;; `:secondary' property lists property names that contain other
;;      ;; syntax tree elements.
;;
;;      :secondary (:title)
;;
;;      ;; `:title' property is set to anonymous element containing:
;;      ;; `plain-text', `bold', `plain-text'.
;;
;;      :title ("Heading with " (bold (:post-blank 1 ...) "bold") "text"))
;;
;;      ;; `headline' contents
;;     (section (...)
;;       (paragraph
;;         ;; `:parent' property set to the containing section.
;;         (:parent (section ...))
;;         ;; paragraph contents is a `plain-text' element.
;;         "Paragraph1."))))
;;
;; Try calling M-: (org-element-parse-buffer) on the above example Org
;; document to explore a more complete version of Org AST.

;;; Code:

(require 'org-macs)
(require 'inline) ; load indentation rules

;;;; Syntax element type

(defun org-element-type (element &optional anonymous)
  "Return type of ELEMENT.

The function returns the type of the element provided.
It can also return the following special value:
  `plain-text'       for a string
  nil                in any other case.

When optional argument ANONYMOUS is non-nil, return symbol `anonymous'
when ELEMENT is an anonymous element."
  (declare (pure t))
  (cond
   ((stringp element) 'plain-text)
   ((null element) nil)
   ((not (consp element)) nil)
   ((symbolp (car element)) (car element))
   ((and anonymous (car element) (org-element-type (car element)))
    'anonymous)
   (t nil)))

(defun org-element-secondary-p (object)
  "Non-nil when OBJECT directly belongs to a secondary element.
Return value is the containing property name, as a keyword, or nil."
  (declare (pure t))
  (let* ((parent (org-element-property :parent object))
	 (properties (org-element-property :secondary parent))
         val)
    (catch 'exit
      (dolist (p properties)
        (setq val (org-element-property-1 p parent))
	(when (or (eq object val) (memq object val))
	  (throw 'exit p))))))

;;;; Deferred values

(cl-defstruct
    (org-element-deferred
     (:constructor org-element-deferred)
     (:type vector) :named)
  "Dynamically computed value.

The value can be obtained by calling FUN with containing syntax object
as first argument and ARGS list as remainting arguments.

AUTO-UNDEFER slot flags if the property value should be replaced upon
resolution."
  fun args auto-undefer-p)

(defsubst org-element--deferred-resolve (deferred-value &optional element)
  "Resolve DEFERRED-VALUE for ELEMENT."
  (apply (org-element-deferred-fun deferred-value)
         element
         (org-element-deferred-args deferred-value)))

;;;; Object properties

(eval-and-compile ; make available during inline expansion

  (defconst org-element--standard-properties
    '( :begin :end :contents-begin :contents-end
       :post-blank :post-affiliated :secondary
       :cached :org-element--cache-sync-key
       :robust-begin :robust-end
       :mode :granularity
       :parent :deferred :structure :buffer)
    "Standard properties stored in every syntax object structure.
These properties are stored in an array pre-allocated every time a new
object is created.")

  (defconst org-element--standard-properties-idxs
    (let (plist)
      (seq-do-indexed
       (lambda (property idx)
         (setq plist (plist-put plist property idx)))
       org-element--standard-properties)
      plist)
    "Property list holding standard indexes for `org-element--standard-properties'."))

(define-inline org-element--property-idx (property)
  "Return standard property index or nil."
  (declare (pure t))
  (if (inline-const-p property)
      (plist-get
       org-element--standard-properties-idxs
       (inline-const-val property))
    (inline-quote (plist-get
                   org-element--standard-properties-idxs
                   ,property))))

(define-inline org-element--parray (element)
  "Return standard property array for ELEMENT."
  (declare (pure t))
  (inline-letevals (element)
    (inline-quote
     (pcase (org-element-type ,element)
       (`nil nil)
       ;; Do not use property array for strings - they usually hold
       ;; `:parent' property and nothing more.
       (`plain-text nil)
       (_
        ;; (type (:standard-properties val ...) ...)
        (if (eq :standard-properties (car (nth 1 ,element)))
            (cadr (nth 1 ,element))
          ;; Non-standard order.  Go long way.
          (plist-get (nth 1 ,element) :standard-properties)))))))

(define-inline org-element--plist-property (property element &optional dflt)
  "Extract the value for PROPERTY from ELEMENT property list.
Ignore standard property array."
  (declare (pure t))
  (inline-letevals (property element dflt)
    (inline-quote
     (pcase (org-element-type ,element)
       (`nil ,dflt)
       (`plain-text
        (or (get-text-property 0 ,property ,element)
            (when ,dflt
              (if (plist-member (text-properties-at 0 ,element) ,property)
                  nil ,dflt))))
       (_
        (or (plist-get (nth 1 ,element) ,property)
            (when ,dflt
              (if (plist-member (nth 1 ,element) ,property)
                  nil ,dflt))))))))

(define-inline org-element-property-1 (property element &optional dflt)
  "Extract the value for PROPERTY of an ELEMENT.
Do not resolve deferred values.
If PROPERTY is not present, return DFLT."
  (declare (pure t))
  (let ((idx (and (inline-const-p property)
                  (org-element--property-idx property))))
    (if idx
        (inline-letevals (element)
          (inline-quote
           (if-let ((parray (org-element--parray ,element)))
               (pcase (aref parray ,idx)
                 (`org-element-ast--nil ,dflt)
                 (val val))
             ;; No property array exists.  Fall back to `plist-get'.
             (org-element--plist-property ,property ,element ,dflt))))
      (inline-letevals (element property)
        (inline-quote
         (let ((idx (org-element--property-idx ,property)))
           (if-let ((parray (and idx (org-element--parray ,element))))
               (pcase (aref parray idx)
                 (`org-element-ast--nil ,dflt)
                 (val val))
             ;; No property array exists.  Fall back to `plist-get'.
             (org-element--plist-property ,property ,element ,dflt))))))))

(define-inline org-element--put-parray (element &optional parray)
  "Initialize standard property array in ELEMENT.
Return the array or nil when ELEMENT is `plain-text'."
  (inline-letevals (element parray)
    (inline-quote
     (let ((parray ,parray))
       (unless (or parray (memq (org-element-type ,element) '(plain-text nil)))
         (setq parray (make-vector ,(length org-element--standard-properties) nil))
         ;; Copy plist standard properties back to parray.
         (let ((stdplist org-element--standard-properties-idxs))
           (while stdplist
             (aset parray (cadr stdplist)
                   (org-element--plist-property (car stdplist) ,element))
             (setq stdplist (cddr stdplist))))
         (setcar (cdr ,element)
                 (nconc (list :standard-properties parray)
                        (cadr ,element)))
         parray)))))

(define-inline org-element-put-property (element property value)
  "In ELEMENT set PROPERTY to VALUE.
Return modified element."
  (let ((idx (and (inline-const-p property)
                  (org-element--property-idx property))))
    (if idx
        (inline-letevals (element value)
          (inline-quote
           (if (eq 'plain-text (org-element-type ,element))
               ;; Special case: Do not use parray for plain-text.
               (org-add-props ,element nil ,property ,value)
             (let ((parray
                    (or (org-element--parray ,element)
                        (org-element--put-parray ,element))))
               (when parray (aset parray ,idx ,value))
               ,element))))
      (inline-letevals (element property value)
        (inline-quote
         (let ((idx (org-element--property-idx ,property)))
           (if (and idx (not (eq 'plain-text (org-element-type ,element))))
               (when-let
                   ((parray
                     (or (org-element--parray ,element)
                         (org-element--put-parray ,element))))
                 (aset parray idx ,value))
             (pcase (org-element-type ,element)
               (`nil nil)
               (`plain-text
                (org-add-props ,element nil ,property ,value))
               (_
                ;; Note that `plist-put' adds new elements at the end,
                ;; thus keeping `:standard-properties' as the first element.
                (setcar (cdr ,element) (plist-put (nth 1 ,element) ,property ,value)))))
           ,element))))))

(defun org-element-property (property element &optional dflt force-undefer)
  "Extract the value from the PROPERTY of an ELEMENT.
Return DFLT when PROPERTY is not present.
When FORCE-UNDEFER is non-nil, unconditionally resolve deferred
properties, replacing their values in ELEMENT."
  (let ((value (org-element-property-1 property element 'org-element-ast--nil))
        resolved-value)
    ;; PROPERTY not present.
    (when (eq 'org-element-ast--nil value)
      ;; If :deferred has `org-element-deferred' type, resolve it for
      ;; side-effects, and re-assign the new value.
      (let ((deferred-prop-value (org-element-property-1 :deferred element)))
        (when (org-element-deferred-p deferred-prop-value)
          (org-element-put-property
           element
           :deferred
           (org-element--deferred-resolve deferred-prop-value element))))
      ;; Try to retrieve the value again.
      (setq value (org-element-property-1 property element dflt)))
    (if (not (org-element-deferred-p value))
        (setq resolved-value value)
      ;; Deferred property.  Resolve it.
      (setq resolved-value (org-element--deferred-resolve value element))
      ;; Store the resolved property value, if needed.
      (when (or force-undefer
                (org-element-deferred-auto-undefer-p value))
        (org-element-put-property
         element property resolved-value)))
    ;; Return the resolved value.
    resolved-value))

(gv-define-setter org-element-property (value property element &optional _)
  `(org-element-put-property ,element ,property ,value))

(gv-define-setter org-element-property-1 (value property element &optional _)
  `(org-element-put-property ,element ,property ,value))

(defun org-element-property-inherited (property element &optional accumulate literal-nil)
  "Extract the value from the PROPERTY of an ELEMENT and/or its parents.

PROPERTY is a single property or a list of properties to be considered.
When optional argument ACCUMULATE is nil, return the first non-nil value
(properties when PROPERTY is a list are considered one by one).
When ACCUMULATE is non-nil, extract all the values, starting from the
outermost ancestor and accumulate them into a single list.  The values
that are lists are appended.
When ACCUMULATE is a string, join the resulting list elements into a
string, using string value as separator.
When LITERAL-nil is non-nil, treat property values \"nil\" and nil."
  (setq property (ensure-list property))
  (let (acc local val)
    (catch :found
      (while element
        (setq local nil)
        (dolist (prop property)
          (setq val (org-element-property prop element))
          (when literal-nil (setq val (org-not-nil val)))
          (when (and (not accumulate) val) (throw :found val))
          ;; Append to the end.
          (setq local (append local (ensure-list val))))
        ;; Append parent to front.
        (setq acc (append local acc))
        (setq element (org-element-property :parent element)))
      (if (and (stringp accumulate) acc)
          (mapconcat #'identity acc accumulate)
        acc))))

(defun org-element-properties (element &optional properties resolve-deferred)
  "Return full property list for ELEMENT.
When optional argument PROPERTIES is non-nil, only return property list
for the listed properties, if they are present.

When RESOLVE-DEFERRED is non-nil, resolve deferred values, modifying
ELEMENT.  When it is symbol `force', unconditionally undefer the
values.

`:standard-properties' value is unwrapped into the property list."
  (if properties
      (mapcan
       (lambda (p)
         (pcase (if resolve-deferred
                    (org-element-property
                     p element
                     'org-element-ast--nil
                     (eq resolve-deferred 'force))
                  (org-element-property-1 p element 'org-element-ast--nil))
           (`org-element-ast--nil nil)
           (val (list p val))))
       properties)
    (pcase (org-element-type element)
      (`nil nil)
      (type
       (when resolve-deferred
         ;; Compute missing properties.
         (org-element-property :deferred element))
       (let ((props
              (if (eq type 'plain-text)
                  (text-properties-at 0 element)
                (nth 1 element))))
         (when resolve-deferred
           (let ((plist props))
             ;; Resolve every property.
             (while plist
               (org-element-property
                (car plist) element
                nil (eq resolve-deferred 'force))
               (setq plist (cddr plist)))
             ;; Resolve standard properties.
             (dolist (p org-element--standard-properties)
               (org-element-property
                p element
                nil (eq resolve-deferred 'force))))
           (setq props
                 (if (eq type 'plain-text)
                     (text-properties-at 0 element)
                   (nth 1 element))))
         (append
          (apply
           #'nconc
           (seq-map-indexed
            (lambda (val idx)
              (unless (eq 'org-element-ast--nil val)
                (list (nth idx org-element--standard-properties) val)))
            (plist-get props :standard-properties)))
          props))))))

;;;; Object contents

(defsubst org-element-contents-1 (element)
  "Extract contents from an ELEMENT.
Do not resolve deferred values."
  (declare (pure t))
  (cond ((not (consp element)) nil)
	((symbolp (car element)) (nthcdr 2 element))
	(t element)))

(defsubst org-element-set-contents (element &rest contents)
  "Set ELEMENT's contents to CONTENTS.
Return ELEMENT.
If ELEMENT cannot have contents, return CONTENTS."
  (pcase (org-element-type element t)
    (`plain-text contents)
    ((guard (null element)) contents)
    ;; Anonymous element.
    (`anonymous
     (setcar element (car contents))
     (setcdr element (cdr contents))
     element)
    ;; Element with type.
    (_ (setf (cddr element) contents)
       element)))

(defsubst org-element-contents (element)
  "Extract contents from an ELEMENT."
  (let ((contents (org-element-contents-1 element)))
    ;; Resolve deferred values.
    (while (org-element-deferred-p contents)
      (if (not (org-element-deferred-auto-undefer-p contents))
          (setq contents (org-element--deferred-resolve contents element))
        (org-element-set-contents
         element (org-element--deferred-resolve contents element))
        (setq contents (org-element-contents-1 element))))
    (while contents
      (while (org-element-deferred-p (car contents))
        (let* ((undefer-p (org-element-deferred-auto-undefer-p (car contents)))
               (resolved-value (org-element--deferred-resolve (car contents) element)))
          ;; Store the resolved property value, if needed.
          (when undefer-p (setcar contents resolved-value))))
      (setq contents (cdr contents)))
    (org-element-contents-1 element)))

(defun org-element-resolve-deferred (element &optional force-undefer)
  "Resolve all the deferred values in ELEMENT.
Return the modified element.
When FORCE-UNDERFER is non-nil, unconditionally replace deferred
properties with their values."
  ;; Resolve properties.
  (org-element-properties element nil (when force-undefer 'force))
  ;; Resolve contents.
  (org-element-contents element)
  ;; Resolve secondary objects.
  (dolist (sp (org-element-property :secondary element))
    (org-element-put-property
     element sp
     (org-element-resolve-deferred (org-element-property sp element))))
  element)

;;;; AST modification

(defun org-element-adopt-elements (parent &rest children)
  "Append elements to the contents of another element.

PARENT is an element or object.  CHILDREN can be elements,
objects, or a strings.

If PARENT is nil, create a new anonymous element containing CHILDREN.

The function takes care of setting `:parent' property for each child.
Return the modified parent element."
  (declare (indent 1))
  (if (not children) parent
    ;; Link every child to PARENT. If PARENT is nil, it is a secondary
    ;; string: parent is the list itself.
    (dolist (child children)
      (when child
        (org-element-put-property child :parent (or parent children))))
    ;; Add CHILDREN at the end of PARENT contents.
    (when parent
      (apply #'org-element-set-contents
	     parent
	     (nconc (org-element-contents parent) children)))
    ;; Return modified PARENT element.
    (or parent children)))

(defun org-element-extract-element (element)
  "Extract ELEMENT from parse tree.
Remove element from the parse tree by side-effect, and return it
with its `:parent' property stripped out."
  (let ((parent (org-element-property :parent element))
	(secondary (org-element-secondary-p element)))
    (if secondary
        (org-element-put-property
	 parent secondary
	 (delq element (org-element-property secondary parent)))
      (apply #'org-element-set-contents
	     parent
	     (delq element (org-element-contents parent))))
    ;; Return ELEMENT with its :parent removed.
    (org-element-put-property element :parent nil)))

(defun org-element-insert-before (element location)
  "Insert ELEMENT before LOCATION in parse tree.
LOCATION is an element, object or string within the parse tree.
Parse tree is modified by side effect."
  (let* ((parent (org-element-property :parent location))
	 (property (org-element-secondary-p location))
	 (siblings (if property (org-element-property property parent)
		     (org-element-contents parent)))
	 ;; Special case: LOCATION is the first element of an
	 ;; independent secondary string (e.g. :title property).  Add
	 ;; ELEMENT in-place.
	 (specialp (and (not property)
			(eq siblings parent)
			(eq (car parent) location))))
    ;; Install ELEMENT at the appropriate LOCATION within SIBLINGS.
    (cond (specialp)
	  ((or (null siblings) (eq (car siblings) location))
	   (push element siblings))
	  ((null location) (nconc siblings (list element)))
	  (t
	   (let ((index (cl-position location siblings)))
	     (unless index (error "No location found to insert element"))
	     (push element (cdr (nthcdr (1- index) siblings))))))
    ;; Store SIBLINGS at appropriate place in parse tree.
    (cond
     (specialp (setcdr parent (copy-sequence parent)) (setcar parent element))
     (property (org-element-put-property parent property siblings))
     (t (apply #'org-element-set-contents parent siblings)))
    ;; Set appropriate :parent property.
    (org-element-put-property element :parent parent)))

(defun org-element-set-element (old new &optional keep-props)
  "Replace element or object OLD with element or object NEW.
When KEEP-PROPS is non-nil, keep OLD values of the listed property
names.

The function takes care of setting `:parent' property for NEW."
  ;; Ensure OLD and NEW have the same parent.
  (org-element-put-property new :parent (org-element-property :parent old))
  ;; Handle KEEP-PROPS.
  (dolist (p keep-props)
    (org-element-put-property new p (org-element-property p old)))
  (let ((old-type (org-element-type old))
        (new-type (org-element-type new)))
    (if (or (eq old-type 'plain-text)
	    (eq new-type 'plain-text))
        ;; We cannot replace OLD with NEW since strings are not mutable.
        ;; We take the long path.
        (progn (org-element-insert-before new old)
	       (org-element-extract-element old))
      ;; Since OLD is going to be changed into NEW by side-effect, first
      ;; make sure that every element or object within NEW has OLD as
      ;; parent.
      (dolist (blob (org-element-contents new))
        (org-element-put-property blob :parent old))
      ;; Both OLD and NEW are lists.
      (setcar old (car new))
      (setcdr old (cdr new)))))

(defun org-element-ast-map
    (data types fun &optional ignore first-match no-recursion with-properties no-secondary)
  "Map a function on selected syntax elements.

DATA is a syntax tree.  TYPES is a symbol or list of symbols of
element types.  FUN is the function called on the matching element.
It has to accept one argument: the element itself.

When optional argument IGNORE is non-nil, it should be a list holding
elements to be skipped.  In that case, the listed elements and their
contents will be skipped.

When optional argument FIRST-MATCH is non-nil, stop at the first
match for which FUN doesn't return nil, and return that value.

Optional argument NO-RECURSION is a symbol or a list of symbols
representing elements or objects types.  `org-element-map' won't
enter any recursive element or object whose type belongs to that
list.  Though, FUN can still be applied on them.

When optional argument WITH-PROPERTIES is non-nil, it should hold a list
of property names.  These properties will be treated as additional
secondary properties.

When optional argument NO-SECONDARY is non-nil, do not recurse into
secondary strings.

FUN may also throw `:org-element-skip' signal.  Then,
`org-element-ast-map' will not recurse into the current element.

Nil values returned from FUN do not appear in the results."
  (declare (indent 2))
  ;; Ensure TYPES and NO-RECURSION are a list, even of one element.
  (let* ((types (if (listp types) types (list types)))
	 (no-recursion (if (listp no-recursion) no-recursion
			 (list no-recursion)))
	 --acc)
    (letrec ((--walk-tree
	      (lambda (--data)
		;; Recursively walk DATA.  INFO, if non-nil, is a plist
		;; holding contextual information.
		(let ((--type (org-element-type --data))
                      recurse)
		  (cond
		   ((not --data))
		   ;; Ignored element in an export context.
		   ((and ignore (memq --data ignore)))
		   ;; List of elements or objects.
		   ((not --type) (mapc --walk-tree --data))
		   (t
		    ;; Check if TYPE is matching among TYPES.  If so,
		    ;; apply FUN to --DATA and accumulate return value
		    ;; into --ACC (or exit if FIRST-MATCH is non-nil).
                    (setq recurse t)
		    (when (memq --type types)
		      (let ((result
                             (catch :org-element-skip
                               (setq recurse nil)
                               (prog1 (funcall fun --data)
                                 (setq recurse t)))))
			(cond ((not result))
			      (first-match (throw :--map-first-match result))
			      (t (push result --acc)))))
		    ;; Determine if a recursion into --DATA is possible.
		    (cond
                     ;; No recursion requested.
                     ((not recurse))
		     ;; --TYPE is explicitly removed from recursion.
		     ((memq --type no-recursion))
		     ;; In any other case, map secondary, affiliated, and contents.
		     (t
		      (when with-properties
		        (dolist (p with-properties)
                          (funcall --walk-tree (org-element-property p --data))))
                      (unless no-secondary
		        (dolist (p (org-element-property :secondary --data))
			  (funcall --walk-tree (org-element-property p --data))))
                      (mapc --walk-tree (org-element-contents --data))))))))))
      (catch :--map-first-match
	(funcall --walk-tree data)
	;; Return value in a proper order.
	(nreverse --acc)))))

(defun org-element-create (type &optional props &rest children)
  "Create a new element of TYPE.
Optional argument PROPS, when non-nil, is a plist defining the
properties of the element.  CHILDREN can be elements, objects or
strings.

When TYPE is `plain-text', CHILDREN must contain a single element -
string.  Alternatively, TYPE can be a string.
When TYPE is nil or `anonymous', PROPS must be nil."
  (cl-assert (plistp props))
  ;; Assign parray.
  (when (and props (not (stringp type)) (not (eq type 'plain-text)))
    (let ((element (list 'dummy props)))
      (org-element--put-parray element)
      (setq props (nth 1 element))
      ;; Remove standard properties from PROPS plist by side effect.
      (let ((ptail props))
        (while ptail
          (if (not (and (keywordp (car ptail))
                      (org-element--property-idx (car ptail))))
              (setq ptail (cddr ptail))
            (if (null (cddr ptail)) ; last property
                (setq props (nbutlast props 2)
                      ptail nil)
              (setcar ptail (nth 2 ptail))
              (setcdr ptail (seq-drop ptail 3))))))))
  (pcase type
    ((or `nil `anonymous)
     (cl-assert (null props))
     (apply #'org-element-adopt-elements nil children))
    (`plain-text
     (cl-assert (length= children 1))
     (org-add-props (car children) props))
    ((pred stringp)
     (if props (org-add-props type props) type))
    (_ (apply #'org-element-adopt-elements (list type props) children))))

(defun org-element-copy (datum &optional keep-contents)
  "Return a copy of DATUM with all deferred values resolved.
DATUM is an element, object, string or nil.  `:parent' property
is cleared and contents are removed in the process.
Secondary objects are also copied and their `:parent' gets re-assigned.

When optional argument KEEP-CONTENTS is non-nil, do not remove the
contents.  Instead, copy the children recursively, updating their
`:parent' property.

As a special case, `anonymous' elements do not have their contents
removed.  The contained children are copied recursively, updating
their `:parent' property to the copied `anonymous' element.

When DATUM is `plain-text', all the properties are removed."
  (when datum
    (pcase (org-element-type datum t)
      (`plain-text (substring-no-properties datum))
      (`nil (error "Not an element: %S" datum))
      (`anonymous
       (let* ((element-copy (copy-sequence datum))
              (tail element-copy))
         (while tail
           (setcar tail (org-element-copy (car tail) t))
           (org-element-put-property (car tail) :parent element-copy)
           (setq tail (cdr tail)))
         element-copy))
      (_
       (let ((element-copy (copy-sequence datum)))
         ;; Copy `:standard-properties'
         (when-let ((parray (org-element-property-1 :standard-properties element-copy)))
           (org-element-put-property element-copy :standard-properties (copy-sequence parray)))
         ;; Clear `:parent'.
         (org-element-put-property element-copy :parent nil)
         ;; We cannot simply return the copied property list.  When
         ;; DATUM is i.e. a headline, it's property list `:title' can
         ;; contain parsed objects.  The objects will contain
         ;; `:parent' property set to the DATUM itself.  When copied,
         ;; these inner `:parent' property values will contain
         ;; incorrect object decoupled from DATUM.  Changes to the
         ;; DATUM copy will no longer be reflected in the `:parent'
         ;; properties.  So, we need to reassign inner `:parent'
         ;; properties to the DATUM copy explicitly.
         (dolist (secondary-prop (org-element-property :secondary element-copy))
           (when-let ((secondary-value (org-element-property secondary-prop element-copy)))
             (setq secondary-value (org-element-copy secondary-value))
             (if (org-element-type secondary-value)
                 (org-element-put-property secondary-value :parent element-copy)
               (dolist (el secondary-value)
                 (org-element-put-property el :parent element-copy)))
             (org-element-put-property element-copy secondary-prop secondary-value)))
         (when keep-contents
           (let ((contents (org-element-contents element-copy)))
             (while contents
               (setcar contents (org-element-copy (car contents) t))
               (setq contents (cdr contents)))))
         (org-element-resolve-deferred element-copy 'force))))))

(defun org-element-lineage (datum &optional types with-self)
  "List all ancestors of a given element or object.

DATUM is an object or element.

Return ancestors from the closest to the farthest.  When optional
argument TYPES is a list of symbols, return the first element or
object in the lineage whose type belongs to that list instead.

When optional argument WITH-SELF is non-nil, lineage includes
DATUM itself as the first element, and TYPES, if provided, also
apply to it.

When DATUM is obtained through `org-element-context' or
`org-element-at-point', and org-element-cache is disabled, only
ancestors from its section can be found.  There is no such limitation
when DATUM belongs to a full parse tree."
  (let ((up (if with-self datum (org-element-property :parent datum)))
	ancestors)
    (while (and up (not (memq (org-element-type up) types)))
      (unless types (push up ancestors))
      (setq up (org-element-property :parent up)))
    (if types up (nreverse ancestors))))

(provide 'org-element-ast)
;;; org-element-ast.el ends here
