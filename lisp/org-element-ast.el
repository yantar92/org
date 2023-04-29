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
;; Org AST is composed of nested syntax nodes.
;; Within actual Org syntax, the nodes can be either headings,
;; elements, or objects.  However, historically, we often call syntax
;; nodes simply "elements", unless the context requires clarification
;; about the node type.  In particular, many functions below will have
;; naming pattern `org-element-X', implying `org-element-node-X' --
;; they will apply to all the node types, not just to elements.
;;
;; 1. Syntax nodes
;; ------------------
;; Each Org syntax node can be represented as a string or list.
;;
;; The main node representation follows the pattern
;; (TYPE PROPERTIES CONTENTS), where
;;   TYPE is a symbol describing the node type.
;;   PROPERTIES is the property list attached to it.
;;   CONTENTS is a list of child syntax nodes contained within the
;;            current node, when applicable.
;;
;;; For example, "*bold text*  " node can be represented as
;;
;;    (bold (:begin 1 :end 14 :post-blank 2 ...) "bold text")
;;
;; TYPE can be any symbol, including symbol not explicitly defined by
;; Org syntax.  If TYPE is not a part of the syntax, the syntax
;; node is called "pseudo element/object", but otherwise considered a
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
;; performance-critical code, as most of the code requesting property
;; values by constant name is inlined.
;;
;; The previous example can also be presented in more compact form as:
;;
;;    (bold (:standard-properties [1 10 ... 2 ...]) "bold text")
;;
;; Using an array allows faster access to frequently used properties.
;;
;; `:parent' holds the containing node, for a child node within the
;; AST.  It may or may not be present in PROPERTIES.
;;
;; `:secondary' holds a list of properties that may contain extra AST
;; nodes, in addition to the node contents.
;;
;; `deferred' property describes how to update not-yet-calculated
;; properties on request.
;;
;;
;; Syntax node can also be represented by a string.  Strings always
;; represent syntax node of `plain-text' type with contents being nil
;; and properties represented as string properties at position 0.
;; `:standard-properties' are not considered for `plain-text' nodes as
;; `plain-text' nodes tend to hold much fewer properties.
;;
;; In the above example, `plain-text' node "bold text" is more
;; accurately represented as
;;
;;    #("bold text" 0 9 (:parent (bold ...)))
;;
;; with :parent property value pointing back to the containing `bold'
;; node.
;;
;; `anonymous' syntax node is represented as a list with `car'
;; containing another syntax node.  Such node has nil type, does not
;; have properties, and its contents is a list of the contained syntax
;; node.  `:parent' property of the contained nodes point back to the
;; list itself, except when `anonymous' node holds secondary value
;; (see below), in which case the `:parent' property is set to be the
;; containing node in the AST.
;;
;; Any node representation other then described above is not
;; considered as Org syntax node.
;;
;; 2. Deferred values
;; ------------------
;; Sometimes, it is computationally expensive or even not possible to
;; calculate property values when creating an AST node.  The value
;; calculation can be deferred to the time the value is requested.
;;
;; Property values and contained nodes may have a special value of
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
;; properties.  This is useful to generate pure (in functional sense)
;; AST.
;;
;; 3. Org document representation
;; ------------------------------
;; Document AST is represented by nested Org syntax nodes.
;;
;; Each node in the AST can hold the contained node in its CONTENTS or
;; as values of properties.
;;
;; For example, (bold (...) "bold text") `bold' node contains
;; `plain-text' node in CONTENTS.
;;
;; The containing node is called "parent node".
;;
;; The contained nodes held inside CONTENTS are called "child nodes".
;; They must have their `:parent' property set to the containing
;; parent node.
;;
;; The contained nodes can also be held as property values.  Such
;; nodes are called "secondary nodes".  Only certain properties
;; can contribute to AST - the property names listed as the value of
;; special property `:secondary'
;;
;; For example,
;;
;;   (headline ((:secondary (:title)
;;               :title (#("text" 0 4 (:parent (headline ...)))))))
;;
;; is a parent headline node containing "text" secondary string node
;; inside `:title' property.  Note that `:title' is listed in
;; `:secondary' value.
;;
;; The following example illustrates an example AST for Org document:
;;
;; ---- Org document --------
;; * Heading with *bold* text
;; Paragraph.
;; ---- end -----------------
;;
;; (org-data (...) ; `org-data' node.
;;   (headline
;;     (
;;      ;; `:secondary' property lists property names that contain other
;;      ;; syntax tree nodes.
;;
;;      :secondary (:title)
;;
;;      ;; `:title' property is set to anonymous node containing:
;;      ;; `plain-text', `bold', `plain-text'.
;;
;;      :title ("Heading with " (bold (:post-blank 1 ...) "bold") "text"))
;;
;;      ;; `headline' contents
;;     (section (...)
;;       (paragraph
;;         ;; `:parent' property set to the containing section.
;;         (:parent (section ...))
;;         ;; paragraph contents is a `plain-text' node.
;;         "Paragraph1."))))
;;
;; Try calling M-: (org-element-parse-buffer) on the above example Org
;; document to explore a more complete version of Org AST.

;;; Code:

(require 'org-macs)
(require 'inline) ; load indentation rules

;;;; Syntax node type

(defun org-element-type (node &optional anonymous)
  "Return type of NODE.

The function returns the type of the node provided.
It can also return the following special value:
  `plain-text'       for a string
  nil                in any other case.

When optional argument ANONYMOUS is non-nil, return symbol `anonymous'
when NODE is an anonymous node."
  (declare (pure t))
  (cond
   ((stringp node) 'plain-text)
   ((null node) nil)
   ((not (consp node)) nil)
   ((symbolp (car node)) (car node))
   ((and anonymous (car node) (org-element-type (car node) t))
    'anonymous)
   (t nil)))

(define-inline org-element-type-p (node types)
  "Return non-nil when NODE type is one of TYPES.
TYPES can be a type symbol or a list of symbols."
  (if (inline-const-p types)
      (if (listp (inline-const-val types))
          (inline-quote (memq (org-element-type ,node t) ,types))
        (inline-quote (eq (org-element-type ,node t) ,types)))
    (inline-letevals (node types)
      (inline-quote
       (if (listp ,types)
           (memq (org-element-type ,node t) ,types)
         (eq (org-element-type ,node t) ,types))))))

(defun org-element-secondary-p (node)
  "Non-nil when NODE directly belongs to a secondary node.
Return value is the containing property name, as a keyword, or nil."
  (declare (pure t))
  (let* ((parent (org-element-property :parent node))
	 (properties (org-element-property :secondary parent))
         val)
    (catch 'exit
      (dolist (p properties)
        (setq val (org-element-property-1 p parent))
	(when (or (eq node val) (memq node val))
	  (throw 'exit p))))))

;;;; Deferred values

(cl-defstruct
    (org-element-deferred
     (:constructor nil)
     (:constructor org-element-deferred
                   ( auto-undefer-p function &rest arg-value
                     &aux (args arg-value)))
     (:constructor org-element-deferred-alias
                   ( keyword &optional auto-undefer-p
                     &aux
                     (function #'org-element-property-2)
                     (args (list keyword))))
     (:constructor org-element-deferred-list
                   ( args &optional auto-undefer-p
                     &aux
                     (function #'org-element--deferred-resolve-list)))
     (:type vector) :named)
  "Dynamically computed value.

The value can be obtained by calling FUNCTION with containing syntax
node as first argument and ARGS list as remainting arguments.

AUTO-UNDEFER slot flags if the property value should be replaced upon
resolution.  Some functions may ignore this flag."
  function args auto-undefer-p)

(defsubst org-element--deferred-resolve (deferred-value &optional node)
  "Resolve DEFERRED-VALUE for NODE."
  (apply (org-element-deferred-function deferred-value)
         node
         (org-element-deferred-args deferred-value)))

(defsubst org-element--deferred-resolve-list (node &rest list)
  "Resolve all the deferred values in LIST for NODE.
Return a new list with all the values resolved."
  (mapcar
   (lambda (value)
     (if (org-element-deferred-p value)
         (org-element--deferred-resolve value node)
       value))
   list))

;;;; Object properties

(eval-and-compile ; make available during inline expansion

  (defconst org-element--standard-properties
    '( :begin :end :contents-begin :contents-end
       :post-blank :post-affiliated :secondary
       :cached :org-element--cache-sync-key
       :robust-begin :robust-end
       :mode :granularity
       :parent :deferred :structure :buffer)
    "Standard properties stored in every syntax node structure.
These properties are stored in an array pre-allocated every time a new
object is created.  Two exceptions are `anonymous' and `plain-text'
node types.")

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

(define-inline org-element--parray (node)
  "Return standard property array for NODE."
  (declare (pure t))
  (inline-letevals (node)
    (inline-quote
     (pcase (org-element-type ,node)
       (`nil nil)
       ;; Do not use property array for strings - they usually hold
       ;; `:parent' property and nothing more.
       (`plain-text nil)
       (_
        ;; (type (:standard-properties val ...) ...)
        (if (eq :standard-properties (car (nth 1 ,node)))
            (cadr (nth 1 ,node))
          ;; Non-standard order.  Go long way.
          (plist-get (nth 1 ,node) :standard-properties)))))))

(define-inline org-element--plist-property (property node &optional dflt)
  "Extract the value for PROPERTY from NODE's property list.
Ignore standard property array."
  (declare (pure t))
  (inline-letevals (property node dflt)
    (inline-quote
     (pcase (org-element-type ,node)
       (`nil ,dflt)
       (`plain-text
        (or (get-text-property 0 ,property ,node)
            (when ,dflt
              (if (plist-member (text-properties-at 0 ,node) ,property)
                  nil ,dflt))))
       (_
        (or (plist-get (nth 1 ,node) ,property)
            (when ,dflt
              (if (plist-member (nth 1 ,node) ,property)
                  nil ,dflt))))))))

(define-inline org-element-property-1 (property node &optional dflt)
  "Extract the value for PROPERTY of an NODE.
Do not resolve deferred values.
If PROPERTY is not present, return DFLT."
  (declare (pure t))
  (let ((idx (and (inline-const-p property)
                  (org-element--property-idx property))))
    (if idx
        (inline-letevals (node)
          (inline-quote
           (if-let ((parray (org-element--parray ,node)))
               (pcase (aref parray ,idx)
                 (`org-element-ast--nil ,dflt)
                 (val val))
             ;; No property array exists.  Fall back to `plist-get'.
             (org-element--plist-property ,property ,node ,dflt))))
      (inline-letevals (node property)
        (inline-quote
         (let ((idx (org-element--property-idx ,property)))
           (if-let ((parray (and idx (org-element--parray ,node))))
               (pcase (aref parray idx)
                 (`org-element-ast--nil ,dflt)
                 (val val))
             ;; No property array exists.  Fall back to `plist-get'.
             (org-element--plist-property ,property ,node ,dflt))))))))

(define-inline org-element--put-parray (node &optional parray)
  "Initialize standard property array in NODE.
Return the array or nil when NODE is `plain-text'."
  (inline-letevals (node parray)
    (inline-quote
     (let ((parray ,parray))
       (unless (or parray (memq (org-element-type ,node) '(plain-text nil)))
         (setq parray (make-vector ,(length org-element--standard-properties) nil))
         ;; Copy plist standard properties back to parray.
         (let ((stdplist org-element--standard-properties-idxs))
           (while stdplist
             (aset parray (cadr stdplist)
                   (org-element--plist-property (car stdplist) ,node))
             (setq stdplist (cddr stdplist))))
         (setcar (cdr ,node)
                 (nconc (list :standard-properties parray)
                        (cadr ,node)))
         parray)))))

(define-inline org-element-put-property (node property value)
  "In NODE, set PROPERTY to VALUE.
Return modified NODE."
  (let ((idx (and (inline-const-p property)
                  (org-element--property-idx property))))
    (if idx
        (inline-letevals (node value)
          (inline-quote
           (if (org-element-type-p ,node 'plain-text)
               ;; Special case: Do not use parray for plain-text.
               (org-add-props ,node nil ,property ,value)
             (let ((parray
                    (or (org-element--parray ,node)
                        (org-element--put-parray ,node))))
               (when parray (aset parray ,idx ,value))
               ,node))))
      (inline-letevals (node property value)
        (inline-quote
         (let ((idx (org-element--property-idx ,property)))
           (if (and idx (not (org-element-type-p ,node 'plain-text)))
               (when-let
                   ((parray
                     (or (org-element--parray ,node)
                         (org-element--put-parray ,node))))
                 (aset parray idx ,value))
             (pcase (org-element-type ,node)
               (`nil nil)
               (`plain-text
                (org-add-props ,node nil ,property ,value))
               (_
                ;; Note that `plist-put' adds new elements at the end,
                ;; thus keeping `:standard-properties' as the first element.
                (setcar (cdr ,node) (plist-put (nth 1 ,node) ,property ,value)))))
           ,node))))))

(define-inline org-element-put-property-2 (property value node)
  "Like `org-element-put-property', but NODE is the last argument.
See `org-element-put-property' for the meaning of PROPERTY and VALUE."
  (inline-quote (org-element-put-property ,node ,property ,value)))

(defun org-element-property (property node &optional dflt force-undefer)
  "Extract the value from the PROPERTY of a NODE.
Return DFLT when PROPERTY is not present.
When FORCE-UNDEFER is non-nil, unconditionally resolve deferred
properties, replacing their values in NODE."
  (let ((value (org-element-property-1 property node 'org-element-ast--nil)))
    ;; PROPERTY not present.
    (when (and (eq 'org-element-ast--nil value)
               (org-element-deferred-p
                (org-element-property-1 :deferred node)))
      ;; If :deferred has `org-element-deferred' type, resolve it for
      ;; side-effects, and re-assign the new value.
      (org-element-property :deferred node nil 'force-undefer)
      ;; Try to retrieve the value again.
      (setq value (org-element-property-1 property node dflt)))
    ;; Deferred property.  Resolve it recursively.
    (when (org-element-deferred-p value)
      (let (undefer (value-to-store 'org-element-ast--nil))
        (while (org-element-deferred-p value)
          (setq undefer (or force-undefer (org-element-deferred-auto-undefer-p value))
                value (org-element--deferred-resolve value node))
          (when undefer (setq value-to-store value)))
        ;; Store the resolved property value, if needed.
        (unless (eq value-to-store 'org-element-ast--nil)
          (org-element-put-property node property value-to-store))))
    ;; Return the resolved value.
    (if (eq value 'org-element-ast--nil) dflt value)))

(define-inline org-element-property-2 (node property &optional dflt force-undefer)
  "Like `org-element-property', but reverse the order of NODE and PROPERTY."
  (inline-quote (org-element-property ,property ,node ,dflt ,force-undefer)))

(gv-define-setter org-element-property (value property node &optional _)
  `(org-element-put-property ,node ,property ,value))

(gv-define-setter org-element-property-1 (value property node &optional _)
  `(org-element-put-property ,node ,property ,value))

(defun org-element-property-inherited (property node &optional accumulate literal-nil)
  "Extract the value from the PROPERTY of a NODE and/or its parents.

PROPERTY is a single property or a list of properties to be considered.
When optional argument ACCUMULATE is nil, return the first non-nil value
\\(properties when PROPERTY is a list are considered one by one).
When ACCUMULATE is non-nil, extract all the values, starting from the
outermost ancestor and accumulate them into a single list.  The values
that are lists are appended.
When ACCUMULATE is a string, join the resulting list elements into a
string, using string value as separator.
When LITERAL-NIL is non-nil, treat property values \"nil\" and nil."
  (setq property (ensure-list property))
  (let (acc local val)
    (catch :found
      (while node
        (setq local nil)
        (dolist (prop property)
          (setq val (org-element-property prop node))
          (when literal-nil (setq val (org-not-nil val)))
          (when (and (not accumulate) val) (throw :found val))
          ;; Append to the end.
          (setq local (append local (ensure-list val))))
        ;; Append parent to front.
        (setq acc (append local acc))
        (setq node (org-element-property :parent node)))
      (if (and (stringp accumulate) acc)
          (mapconcat #'identity acc accumulate)
        acc))))

(defun org-element-properties (node &optional properties resolve-deferred)
  "Return full property list for NODE.
When optional argument PROPERTIES is non-nil, only return property list
for the listed properties, if they are present.

When RESOLVE-DEFERRED is non-nil, resolve deferred values, modifying
NODE.  When it is symbol `force', unconditionally undefer the values.

`:standard-properties' value is unwrapped into the property list."
  (if properties
      (mapcan
       (lambda (p)
         (pcase (if resolve-deferred
                    (org-element-property
                     p node
                     'org-element-ast--nil
                     (eq resolve-deferred 'force))
                  (org-element-property-1 p node 'org-element-ast--nil))
           (`org-element-ast--nil nil)
           (val (list p val))))
       properties)
    (pcase (org-element-type node)
      (`nil nil)
      (type
       (when resolve-deferred
         ;; Compute missing properties.
         (org-element-property :deferred node))
       (let ((props
              (if (eq type 'plain-text)
                  (text-properties-at 0 node)
                (nth 1 node))))
         (when resolve-deferred
           (let ((plist props))
             ;; Resolve every property.
             (while plist
               (org-element-property
                (car plist) node
                nil (eq resolve-deferred 'force))
               (setq plist (cddr plist)))
             ;; Resolve standard properties.
             (dolist (p org-element--standard-properties)
               (org-element-property
                p node
                nil (eq resolve-deferred 'force))))
           (setq props
                 (if (eq type 'plain-text)
                     (text-properties-at 0 node)
                   (nth 1 node))))
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

(defsubst org-element-contents-1 (node)
  "Extract contents from NODE.
Do not resolve deferred values."
  (declare (pure t))
  (cond ((not (consp node)) nil)
	((symbolp (car node)) (nthcdr 2 node))
	(t node)))

(defsubst org-element-set-contents (node &rest contents)
  "Set NODE's contents to CONTENTS.
Return modified NODE.
If NODE cannot have contents, return CONTENTS."
  (pcase (org-element-type node t)
    (`plain-text contents)
    ((guard (null node)) contents)
    ;; Anonymous node.
    (`anonymous
     (setcar node (car contents))
     (setcdr node (cdr contents))
     node)
    ;; Node with type.
    (_ (setf (cddr node) contents)
       node)))

(defsubst org-element-contents (node)
  "Extract contents from NODE."
  (let ((contents (org-element-contents-1 node)))
    ;; Resolve deferred values.
    (while (org-element-deferred-p contents)
      (if (not (org-element-deferred-auto-undefer-p contents))
          (setq contents (org-element--deferred-resolve contents node))
        (org-element-set-contents
         node (org-element--deferred-resolve contents node))
        (setq contents (org-element-contents-1 node))))
    (while contents
      (while (org-element-deferred-p (car contents))
        (let* ((undefer-p (org-element-deferred-auto-undefer-p (car contents)))
               (resolved-value (org-element--deferred-resolve (car contents) node)))
          ;; Store the resolved property value, if needed.
          (when undefer-p (setcar contents resolved-value))))
      (setq contents (cdr contents)))
    (org-element-contents-1 node)))

(defun org-element-resolve-deferred (node &optional force-undefer recursive)
  "Resolve all the deferred values in NODE.
Return the modified NODE.
When FORCE-UNDEFER is non-nil, unconditionally replace deferred
properties with their values.
When RECURSIVE is non-nil, descend into child node contents."
  ;; Resolve properties.
  (org-element-properties node nil (when force-undefer 'force))
  ;; Resolve contents.
  (let ((contents (org-element-contents node)))
    (when recursive
      (dolist (el contents)
        (org-element-resolve-deferred el force-undefer recursive))))
  ;; Resolve secondary objects.
  (dolist (sp (org-element-property :secondary node))
    (org-element-put-property
     node sp
     (org-element-resolve-deferred (org-element-property sp node))))
  node)

;;;; AST modification

(defalias 'org-element-adopt-elements #'org-element-adopt)
(defun org-element-adopt (parent &rest children)
  "Append CHILDREN to the contents of PARENT.

PARENT is a syntax node.  CHILDREN can be elements, objects, or
strings.

If PARENT is nil, create a new anonymous node containing CHILDREN.

The function takes care of setting `:parent' property for each child.
Return the modified PARENT."
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

(defalias 'org-element-extract-element #'org-element-extract)
(defun org-element-extract (node)
  "Extract NODE from parse tree.
Remove NODE from the parse tree by side-effect, and return it
with its `:parent' property stripped out."
  (let ((parent (org-element-property :parent node))
	(secondary (org-element-secondary-p node)))
    (if secondary
        (org-element-put-property
	 parent secondary
	 (delq node (org-element-property secondary parent)))
      (apply #'org-element-set-contents
	     parent
	     (delq node (org-element-contents parent))))
    ;; Return NODE with its :parent removed.
    (org-element-put-property node :parent nil)))

(defun org-element-insert-before (node location)
  "Insert NODE before LOCATION in parse tree.
LOCATION is an element, object or string within the parse tree.
Parse tree is modified by side effect."
  (let* ((parent (org-element-property :parent location))
	 (property (org-element-secondary-p location))
	 (siblings (if property (org-element-property property parent)
		     (org-element-contents parent)))
	 ;; Special case: LOCATION is the first element of an
	 ;; independent secondary string (e.g. :title property).  Add
	 ;; NODE in-place.
	 (specialp (and (not property)
			(eq siblings parent)
			(eq (car parent) location))))
    ;; Install NODE at the appropriate LOCATION within SIBLINGS.
    (cond (specialp)
	  ((or (null siblings) (eq (car siblings) location))
	   (push node siblings))
	  ((null location) (nconc siblings (list node)))
	  (t
	   (let ((index (cl-position location siblings)))
	     (unless index (error "No location found to insert node"))
	     (push node (cdr (nthcdr (1- index) siblings))))))
    ;; Store SIBLINGS at appropriate place in parse tree.
    (cond
     (specialp (setcdr parent (copy-sequence parent)) (setcar parent node))
     (property (org-element-put-property parent property siblings))
     (t (apply #'org-element-set-contents parent siblings)))
    ;; Set appropriate :parent property.
    (org-element-put-property node :parent parent)))

(defalias 'org-element-set-element #'org-element-set)
(defun org-element-set (old new &optional keep-props)
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
	       (org-element-extract old))
      ;; Since OLD is going to be changed into NEW by side-effect, first
      ;; make sure that every element or object within NEW has OLD as
      ;; parent.
      (dolist (blob (org-element-contents new))
        (org-element-put-property blob :parent old))
      ;; Both OLD and NEW are lists.
      (setcar old (car new))
      (setcdr old (cdr new)))))

(defun org-element-ast-map
    ( data types fun
      &optional
      ignore first-match no-recursion
      with-properties no-secondary no-undefer)
  "Map a function on selected syntax nodes.

DATA is a syntax tree.  TYPES is a symbol or list of symbols of
node types.  FUN is the function called on the matching nodes.
It has to accept one argument: the node itself.

When TYPES is t, call FUN for all the node types.

FUN can also be a lisp form.  The form will be evaluated as function
with symbol `node' bound to the current node.

When optional argument IGNORE is non-nil, it should be a list holding
nodes to be skipped.  In that case, the listed nodes and their
contents will be skipped.

When optional argument FIRST-MATCH is non-nil, stop at the first
match for which FUN doesn't return nil, and return that value.

Optional argument NO-RECURSION is a symbol or a list of symbols
representing node types.  `org-element-map' won't enter any recursive
element or object whose type belongs to that list.  Though, FUN can
still be applied on them.

When optional argument WITH-PROPERTIES is non-nil, it should hold a list
of property names.  These properties will be treated as additional
secondary properties.

When optional argument NO-SECONDARY is non-nil, do not recurse into
secondary strings.

When optional argument NO-UNDEFER is non-nil, do not resolve deferred
values.

FUN may also throw `:org-element-skip' signal.  Then,
`org-element-ast-map' will not recurse into the current node.

Nil values returned from FUN do not appear in the results."
  (declare (indent 2))
  ;; Ensure TYPES and NO-RECURSION are a list, even of one node.
  (when types
    (let* ((types (pcase types
                    ((pred listp) types)
                    (`t t)
                    (_ (list types))))
	   (no-recursion (if (listp no-recursion) no-recursion
			   (list no-recursion)))
           (fun (if (functionp fun) fun `(lambda (node) ,fun)))
	   --acc)
      (letrec ((--walk-tree
	        (lambda (--data)
		  ;; Recursively walk DATA.  INFO, if non-nil, is a plist
		  ;; holding contextual information.
		  (let ((--type (org-element-type --data t))
                        recurse)
		    (cond
		     ((not --data))
                     ((not --type))
		     ;; Ignored node in an export context.
		     ((and ignore (memq --data ignore)))
		     ;; List of elements or objects.
		     ((eq --type 'anonymous)
                      (mapc
                       --walk-tree
                       (if no-undefer
                           (org-element-contents-1 --data)
                         (org-element-contents --data))))
		     (t
		      ;; Check if TYPE is matching among TYPES.  If so,
		      ;; apply FUN to --DATA and accumulate return value
		      ;; into --ACC (or exit if FIRST-MATCH is non-nil).
                      (setq recurse t)
		      (when (or (eq types t) (memq --type types))
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
                            (funcall
                             --walk-tree
                             (if no-undefer
                                 (org-element-property-1 p --data)
                               (org-element-property p --data)))))
                        (unless no-secondary
		          (dolist (p (org-element-property :secondary --data))
			    (funcall
                             --walk-tree
                             (if no-undefer
                                 (org-element-property-1 p --data)
                               (org-element-property p --data)))))
                        (mapc
                         --walk-tree
                         (if no-undefer
                             (org-element-contents-1 --data)
                           (org-element-contents --data)))))))))))
        (catch :--map-first-match
	  (funcall --walk-tree data)
	  ;; Return value in a proper order.
	  (nreverse --acc))))))

(defun org-element-create (type &optional props &rest children)
  "Create a new syntax node of TYPE.
Optional argument PROPS, when non-nil, is a plist defining the
properties of the node.  CHILDREN can be elements, objects or
strings.

When TYPE is `plain-text', CHILDREN must contain a single node -
string.  Alternatively, TYPE can be a string.  When TYPE is nil or
`anonymous', PROPS must be nil."
  (cl-assert (plistp props))
  ;; Assign parray.
  (when (and props (not (stringp type)) (not (eq type 'plain-text)))
    (let ((node (list 'dummy props)))
      (org-element--put-parray node)
      (setq props (nth 1 node))
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
     (apply #'org-element-adopt nil children))
    (`plain-text
     (cl-assert (length= children 1))
     (org-add-props (car children) props))
    ((pred stringp)
     (if props (org-add-props type props) type))
    (_ (apply #'org-element-adopt (list type props) children))))

(defun org-element-copy (datum &optional keep-contents)
  "Return a copy of DATUM with all deferred values resolved.
DATUM is an element, object, string or nil.  `:parent' property
is cleared and contents are removed in the process.
Secondary objects are also copied and their `:parent' gets re-assigned.

When optional argument KEEP-CONTENTS is non-nil, do not remove the
contents.  Instead, copy the children recursively, updating their
`:parent' property.

As a special case, `anonymous' nodes do not have their contents
removed.  The contained children are copied recursively, updating
their `:parent' property to the copied `anonymous' node.

When DATUM is `plain-text', all the properties are removed."
  (when datum
    (pcase (org-element-type datum t)
      (`plain-text (substring-no-properties datum))
      (`nil (error "Not an Org syntax node: %S" datum))
      (`anonymous
       (let* ((node-copy (copy-sequence datum))
              (tail node-copy))
         (while tail
           (setcar tail (org-element-copy (car tail) t))
           (org-element-put-property (car tail) :parent node-copy)
           (setq tail (cdr tail)))
         node-copy))
      (_
       (let ((node-copy (copy-sequence datum)))
         ;; Copy `:standard-properties'
         (when-let ((parray (org-element-property-1 :standard-properties node-copy)))
           (org-element-put-property node-copy :standard-properties (copy-sequence parray)))
         ;; Clear `:parent'.
         (org-element-put-property node-copy :parent nil)
         ;; We cannot simply return the copied property list.  When
         ;; DATUM is i.e. a headline, it's property list `:title' can
         ;; contain parsed objects.  The objects will contain
         ;; `:parent' property set to the DATUM itself.  When copied,
         ;; these inner `:parent' property values will contain
         ;; incorrect object decoupled from DATUM.  Changes to the
         ;; DATUM copy will no longer be reflected in the `:parent'
         ;; properties.  So, we need to reassign inner `:parent'
         ;; properties to the DATUM copy explicitly.
         (dolist (secondary-prop (org-element-property :secondary node-copy))
           (when-let ((secondary-value (org-element-property secondary-prop node-copy)))
             (setq secondary-value (org-element-copy secondary-value))
             (if (org-element-type secondary-value)
                 (org-element-put-property secondary-value :parent node-copy)
               (dolist (el secondary-value)
                 (org-element-put-property el :parent node-copy)))
             (org-element-put-property node-copy secondary-prop secondary-value)))
         (when keep-contents
           (let ((contents (org-element-contents node-copy)))
             (while contents
               (setcar contents (org-element-copy (car contents) t))
               (setq contents (cdr contents)))))
         (org-element-resolve-deferred node-copy 'force))))))

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
    (while (and up (not (org-element-type-p up types)))
      (unless types (push up ancestors))
      (setq up (org-element-property :parent up)))
    (if types up (nreverse ancestors))))

(provide 'org-element-ast)
;;; org-element-ast.el ends here
