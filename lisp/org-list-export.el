;;; org-list-export.el --- Export plain lists              -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2004-2024 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;;	   Bastien Guerry <bzg@gnu.org>
;; Keywords: outlines, hypermedia, calendar, wp
;; URL: https://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code converting Org mode's plain lists to
;; other formats.


;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-list-core)

(defun org-list-to-lisp (&optional delete)
  "Parse the list at point and maybe DELETE it.

Return a list whose car is a symbol of list type, among
`ordered', `unordered' and `descriptive'.  Then, each item is
a list of strings and other sub-lists.

For example, the following list:

  1. first item
     + sub-item one
     + [X] sub-item two
     more text in first item
  2. [@3] last item

is parsed as

 (ordered
  (\"first item\"
   (unordered
    (\"sub-item one\")
    (\"[X] sub-item two\"))
   \"more text in first item\")
  (\"[@3] last item\"))

Point is left at list's end."
  (letrec ((struct (org-list-struct))
	   (prevs (org-list-prevs-alist struct))
	   (parents (org-list-parents-alist struct))
	   (top (org-list-get-top-point struct))
	   (bottom (org-list-get-bottom-point struct))
	   (trim
	    (lambda (text)
	      ;; Remove indentation and final newline from TEXT.
	      (org-remove-indentation
	       (if (string-match-p "\n\\'" text)
		   (substring text 0 -1)
		 text))))
	   (parse-sublist
	    (lambda (e)
	      ;; Return a list whose car is list type and cdr a list
	      ;; of items' body.
	      (cons (org-list-get-list-type (car e) struct prevs)
		    (mapcar parse-item e))))
	   (parse-item
	    (lambda (e)
	      ;; Return a list containing counter of item, if any,
	      ;; text and any sublist inside it.
	      (let* ((end (org-list-get-item-end e struct))
		     (children (org-list-get-children e struct parents))
		     (body
		      (save-excursion
			(goto-char e)
			(looking-at "[ \t]*\\S-+[ \t]*")
			(list
			 (funcall
			  trim
			  (concat
			   (make-string (string-width (match-string 0)) ?\s)
			   (buffer-substring-no-properties
			    (match-end 0) (or (car children) end))))))))
		(while children
		  (let* ((child (car children))
			 (sub (org-list-get-all-items child struct prevs))
			 (last-in-sub (car (last sub))))
		    (push (funcall parse-sublist sub) body)
		    ;; Remove whole sub-list from children.
		    (setq children (cdr (memq last-in-sub children)))
		    ;; There is a chunk of text belonging to the item
		    ;; if last child doesn't end where next child
		    ;; starts or where item ends.
		    (let ((sub-end (org-list-get-item-end last-in-sub struct))
			  (next (or (car children) end)))
		      (when (/= sub-end next)
			(push (funcall
			       trim
			       (buffer-substring-no-properties sub-end next))
			      body)))))
		(nreverse body)))))
    ;; Store output, take care of cursor position and deletion of
    ;; list, then return output.
    (prog1 (funcall parse-sublist (org-list-get-all-items top struct prevs))
      (goto-char top)
      (when delete
	(delete-region top bottom)
	(when (and (not (looking-at "[ \t]*$")) (looking-at org-list-end-re))
	  (replace-match ""))))))

;;;###autoload
(defun org-list-make-subtree ()
  "Convert the plain list at point into a subtree."
  (interactive)
  (let ((item (org-in-item-p)))
    (unless item (error "Not in a list"))
    (goto-char item)
    (let ((level (pcase (org-current-level)
		   (`nil 1)
		   (l (1+ (org-reduced-level l)))))
	  (list (save-excursion (org-list-to-lisp t))))
      (insert (org-list-to-subtree list level) "\n"))))

(declare-function org-export-create-backend "ox" (&rest rest) t)
(declare-function org-export-data-with-backend "ox" (data backend info))
(declare-function org-export-get-backend "ox" (name))
(declare-function org-export-get-environment "ox" (&optional backend subtreep ext-plist))
(declare-function org-export-get-next-element "ox" (blob info &optional n))
(declare-function org-export-with-backend "ox" (backend data &optional contents info))
(defun org-list-to-generic (list params)
  "Convert a LIST parsed through `org-list-to-lisp' to a custom format.

LIST is a list as returned by `org-list-to-lisp', which see.
PARAMS is a property list of parameters used to tweak the output
format.

Valid parameters are:

:backend, :raw

  Export backend used as a basis to transcode elements of the
  list, when no specific parameter applies to it.  It is also
  used to translate its contents.  You can prevent this by
  setting :raw property to a non-nil value.

:splice

  When non-nil, only export the contents of the top most plain
  list, effectively ignoring its opening and closing lines.

:ustart, :uend

  Strings to start and end an unordered list.  They can also be
  set to a function returning a string or nil, which will be
  called with the depth of the list, counting from 1.

:ostart, :oend

  Strings to start and end an ordered list.  They can also be set
  to a function returning a string or nil, which will be called
  with the depth of the list, counting from 1.

:dstart, :dend

  Strings to start and end a descriptive list.  They can also be
  set to a function returning a string or nil, which will be
  called with the depth of the list, counting from 1.

:dtstart, :dtend, :ddstart, :ddend

  Strings to start and end a descriptive term.

:istart, :iend

  Strings to start or end a list item, and to start a list item
  with a counter.  They can also be set to a function returning
  a string or nil, which will be called with two arguments: the
  type of list and the depth of the item, counting from 1.

:icount

  Strings to start a list item with a counter.  It can also be
  set to a function returning a string or nil, which will be
  called with three arguments: the type of list, the depth of the
  item, counting from 1, and the counter.  Its value, when
  non-nil, has precedence over `:istart'.

:isep

  String used to separate items.  It can also be set to
  a function returning a string or nil, which will be called with
  two arguments: the type of list and the depth of the item,
  counting from 1.  It always start on a new line.

:ifmt

  Function to be applied to the contents of every item.  It is
  called with two arguments: the type of list and the contents.

:cbon, :cboff, :cbtrans

  String to insert, respectively, an un-checked check-box,
  a checked check-box and a check-box in transitional state."
  (require 'ox)
  (let* ((backend (plist-get params :backend))
	 (custom-backend
	  (org-export-create-backend
	   :parent (or backend 'org)
	   :transcoders
	   `((plain-list . ,(org-list--to-generic-plain-list params))
	     (item . ,(org-list--to-generic-item params))
	     (macro . (lambda (m c i) (org-element-macro-interpreter m nil))))))
	 data info)
    ;; Write LIST back into Org syntax and parse it.
    (with-temp-buffer
      (let ((org-inhibit-startup t)) (org-mode))
      (letrec ((insert-list
		(lambda (l)
		  (dolist (i (cdr l))
		    (funcall insert-item i (car l)))))
	       (insert-item
		(lambda (i type)
		  (let ((start (point)))
		    (insert (if (eq type 'ordered) "1. " "- "))
		    (dolist (e i)
		      (if (consp e) (funcall insert-list e)
			(insert e)
			(insert "\n")))
		    (forward-line 0)
		    (save-excursion
		      (let ((ind (if (eq type 'ordered) 3 2)))
			(while (> (point) start)
			  (unless (looking-at-p "[ \t]*$")
			    (indent-to ind))
			  (forward-line -1))))))))
	(funcall insert-list list))
      (setf data
	    (org-element-map (org-element-parse-buffer) 'plain-list
	      #'identity nil t))
      (setf info (org-export-get-environment backend nil params)))
    (when (and backend (symbolp backend) (not (org-export-get-backend backend)))
      (user-error "Unknown :backend value"))
    (unless backend (require 'ox-org))
    ;; When ':raw' property has a non-nil value, turn all objects back
    ;; into Org syntax.
    (when (and backend (plist-get params :raw))
      (org-element-map data org-element-all-objects
	(lambda (object)
	  (org-element-set
	   object (org-element-interpret-data object)))))
    ;; We use a low-level mechanism to export DATA so as to skip all
    ;; usual pre-processing and post-processing, i.e., hooks, filters,
    ;; Babel code evaluation, include keywords and macro expansion,
    ;; and filters.
    (let ((output (org-export-data-with-backend data custom-backend info)))
      ;; Remove final newline.
      (if (org-string-nw-p output) (substring-no-properties output 0 -1) ""))))

(defun org-list--depth (element)
  "Return the level of ELEMENT within current plain list.
ELEMENT is either an item or a plain list."
  (cl-count-if (lambda (ancestor) (org-element-type-p ancestor 'plain-list))
	       (org-element-lineage element nil t)))

(defun org-list--trailing-newlines (string)
  "Return the number of trailing newlines in STRING."
  (with-temp-buffer
    (insert string)
    (skip-chars-backward " \t\n")
    (count-lines (line-beginning-position 2) (point-max))))

(defun org-list--generic-eval (value &rest args)
  "Evaluate VALUE according to its type.
VALUE is either nil, a string or a function.  In the latter case,
it is called with arguments ARGS."
  (cond ((null value) nil)
	((stringp value) value)
	((functionp value) (apply value args))
	(t (error "Wrong value: %s" value))))

(defun org-list--to-generic-plain-list (params)
  "Return a transcoder for `plain-list' elements.
PARAMS is a plist used to tweak the behavior of the transcoder."
  (let ((ustart (plist-get params :ustart))
	(uend (plist-get params :uend))
	(ostart (plist-get params :ostart))
	(oend (plist-get params :oend))
	(dstart (plist-get params :dstart))
	(dend (plist-get params :dend))
	(splice (plist-get params :splice))
	(backend (plist-get params :backend)))
    (lambda (plain-list contents info)
      (let* ((type (org-element-property :type plain-list))
	     (depth (org-list--depth plain-list))
	     (start (and (not splice)
			 (org-list--generic-eval
			  (pcase type
			    (`ordered ostart)
			    (`unordered ustart)
			    (_ dstart))
			  depth)))
	     (end (and (not splice)
		       (org-list--generic-eval
			(pcase type
			  (`ordered oend)
			  (`unordered uend)
			  (_ dend))
			depth))))
	;; Make sure trailing newlines in END appear in the output by
	;; setting `:post-blank' property to their number.
	(when end
	  (org-element-put-property
	   plain-list :post-blank (org-list--trailing-newlines end)))
	;; Build output.
	(concat (and start (concat start "\n"))
		(if (or start end splice (not backend))
		    contents
		  (org-export-with-backend backend plain-list contents info))
		end)))))

(defun org-list--to-generic-item (params)
  "Return a transcoder for `item' elements.
PARAMS is a plist used to tweak the behavior of the transcoder."
  (let ((backend (plist-get params :backend))
	(istart (plist-get params :istart))
	(iend (plist-get params :iend))
	(isep (plist-get params :isep))
	(icount (plist-get params :icount))
	(ifmt (plist-get params :ifmt))
	(cboff (plist-get params :cboff))
	(cbon  (plist-get params :cbon))
	(cbtrans (plist-get params :cbtrans))
	(dtstart (plist-get params :dtstart))
	(dtend (plist-get params :dtend))
	(ddstart (plist-get params :ddstart))
	(ddend (plist-get params :ddend)))
    (lambda (item contents info)
      (let* ((type
	      (org-element-property :type (org-element-parent item)))
	     (tag (org-element-property :tag item))
	     (depth (org-list--depth item))
	     (separator (and (org-export-get-next-element item info)
			     (org-list--generic-eval isep type depth)))
	     (closing (pcase (org-list--generic-eval iend type depth)
			((or `nil "") "\n")
			((and (guard separator) s)
			 (if (equal (substring s -1) "\n") s (concat s "\n")))
			(s s))))
	;; When a closing line or a separator is provided, make sure
	;; its trailing newlines are taken into account when building
	;; output.  This is done by setting `:post-blank' property to
	;; the number of such lines in the last line to be added.
	(let ((last-string (or separator closing)))
	  (when last-string
	    (org-element-put-property
	     item
	     :post-blank
	     (max (1- (org-list--trailing-newlines last-string)) 0))))
	;; Build output.
	(concat
	 (let ((c (org-element-property :counter item)))
	   (if (and c icount) (org-list--generic-eval icount type depth c)
	     (org-list--generic-eval istart type depth)))
	 (let ((body
		(if (or istart iend icount ifmt cbon cboff cbtrans (not backend)
			(and (eq type 'descriptive)
			     (or dtstart dtend ddstart ddend)))
		    (concat
		     (pcase (org-element-property :checkbox item)
		       (`on cbon)
		       (`off cboff)
		       (`trans cbtrans))
		     (and tag
			  (concat dtstart
				  (if backend
				      (org-export-data-with-backend
				       tag backend info)
				    (org-element-interpret-data tag))
				  dtend))
		     (and tag ddstart)
		     (let ((contents
			    (if (= (length contents) 0) ""
			      (substring contents 0 -1))))
		       (if ifmt (org-list--generic-eval ifmt type contents)
			 contents))
		     (and tag ddend))
		  (org-export-with-backend backend item contents info))))
	   ;; Remove final newline.
	   (if (equal body "") ""
	     (substring (org-element-normalize-string body) 0 -1)))
	 closing
	 separator)))))

(defun org-list-to-latex (list &optional params)
  "Convert LIST into a LaTeX list.
LIST is a parsed plain list, as returned by `org-list-to-lisp'.
PARAMS is a property list with overruling parameters for
`org-list-to-generic'.  Return converted list as a string."
  (require 'ox-latex)
  (org-list-to-generic list (org-combine-plists '(:backend latex) params)))

(defun org-list-to-html (list &optional params)
  "Convert LIST into a HTML list.
LIST is a parsed plain list, as returned by `org-list-to-lisp'.
PARAMS is a property list with overruling parameters for
`org-list-to-generic'.  Return converted list as a string."
  (require 'ox-html)
  (org-list-to-generic list (org-combine-plists '(:backend html) params)))

(defun org-list-to-texinfo (list &optional params)
  "Convert LIST into a Texinfo list.
LIST is a parsed plain list, as returned by `org-list-to-lisp'.
PARAMS is a property list with overruling parameters for
`org-list-to-generic'.  Return converted list as a string."
  (require 'ox-texinfo)
  (org-list-to-generic list (org-combine-plists '(:backend texinfo) params)))

(defun org-list-to-org (list &optional params)
  "Convert LIST into an Org plain list.
LIST is as returned by `org-list-parse-list'.  PARAMS is a property list
with overruling parameters for `org-list-to-generic'."
  (let* ((make-item
	  (lambda (type _depth &optional c)
	    (concat (if (eq type 'ordered) "1. " "- ")
		    (and c (format "[@%d] " c)))))
	 (defaults
	  (list :istart make-item
		:icount make-item
		:ifmt (lambda (_type contents)
			(replace-regexp-in-string "\n" "\n  " contents))
		:dtend " :: "
		:cbon "[X] "
		:cboff "[ ] "
		:cbtrans "[-] ")))
    (org-list-to-generic list (org-combine-plists defaults params))))

(defun org-list-to-subtree (list &optional start-level params)
  "Convert LIST into an Org subtree.
LIST is as returned by `org-list-to-lisp'.  Subtree starts at
START-LEVEL or level 1 if nil.  PARAMS is a property list with
overruling parameters for `org-list-to-generic'."
  (require 'org-edit-structure-common)
  (defvar org-blank-before-new-entry)
  (let* ((blank (pcase (cdr (assq 'heading org-blank-before-new-entry))
		  (`t t)
		  (`auto (save-excursion
			   (org-with-limited-levels (outline-previous-heading))
			   (org-previous-line-empty-p)))))
	 (level (or start-level 1))
	 (make-stars
	  (lambda (_type depth &optional _count)
	    ;; Return the string for the heading, depending on DEPTH
	    ;; of current sub-list.
	    (let ((oddeven-level (+ level (1- depth))))
	      (concat (make-string (if org-odd-levels-only
				       (1- (* 2 oddeven-level))
				     oddeven-level)
				   ?*)
		      " ")))))
    (org-list-to-generic
     list
     (org-combine-plists
      (list :splice t
	    :istart make-stars
	    :icount make-stars
	    :dtstart " " :dtend " "
	    :isep (if blank "\n\n" "\n")
	    :cbon "DONE " :cboff "TODO " :cbtrans "TODO ")
      params))))

(provide 'org-list-export)

;;; org-list-export.el ends here
