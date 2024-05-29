;;; org-colview-dblock.el --- Dynamic block for Column View            -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains dynamic block definition for the column view for
;; Org.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-colview)
(require 'org-dblock)
(require 'org-table-fold)
(require 'org-table-align)
(require 'org-table-formula)
(require 'ol)
(require 'org-id-search)


;;; Configuration

(defcustom org-columns-dblock-formatter #'org-columns-dblock-write-default
  "Function to format data in column view dynamic blocks.
For more information, see `org-columns-dblock-write-default'."
  :group 'org-properties
  :package-version '(Org . "9.7")
  :type 'function)

(defun org-quote-vert (s)
  "Replace \"|\" with \"\\vert\"."
  (while (string-match "|" s)
    (setq s (replace-match "\\vert" t t s)))
  s)

(defun org-columns--capture-view (maxlevel match skip-empty exclude-tags format local)
  "Get the column view of the current buffer.

MAXLEVEL sets the level limit.  SKIP-EMPTY tells whether to skip
empty rows, an empty row being one where all the column view
specifiers but ITEM are empty.  EXCLUDE-TAGS is a list of tags
that will be excluded from the resulting view.  FORMAT is a
format string for columns, or nil.  When LOCAL is non-nil, only
capture headings in current subtree.

This function returns a list containing the title row and all other
rows.  Each row is either a list, or the symbol `hline'.  The first list
is the heading row as a list of strings with the column titles according
to FORMAT.  All subsequent lists each represent a body row as a list
whose first element is an integer indicating the outline level of the
entry, and whose remaining elements are strings with the contents for
the columns according to FORMAT."
  (org-columns (not local) format)
  (goto-char org-columns-top-level-marker)
  (let ((columns (length org-columns-current-fmt-compiled))
	(has-item (assoc "ITEM" org-columns-current-fmt-compiled))
	table)
    (org-map-entries
     (lambda ()
       (when (get-char-property (point) 'org-columns-key)
	 (let (row)
	   (dotimes (i columns)
	     (let* ((col (+ (line-beginning-position) i))
		    (p (get-char-property col 'org-columns-key)))
	       (push (get-char-property col
					(if (string= p "ITEM")
					    'org-columns-value
					  'org-columns-value-modified))
		     row)))
	   (unless (or
		    (and skip-empty
			 (let ((r (delete-dups (remove "" row))))
			   (or (null r) (and has-item (= (length r) 1)))))
		    (and exclude-tags
			 (cl-some (lambda (tag) (member tag exclude-tags))
				  (org-get-tags))))
	     (push (cons (org-reduced-level (org-current-level)) (nreverse row))
		   table)))))
     (if match
         (concat match (and maxlevel (format "+LEVEL<=%d" maxlevel)))
       (and maxlevel (format "LEVEL<=%d" maxlevel)))
     (and local 'tree)
     (when org-columns-skip-archived-trees 'archive) 'comment)
    (org-columns-quit)
    ;; Add column titles and a horizontal rule in front of the table.
    (cons (mapcar #'cadr org-columns-current-fmt-compiled)
	  (cons 'hline (nreverse table)))))

(defun org-columns--clean-item (item)
  "Remove sensitive contents from string ITEM.
This includes objects that may not be duplicated within
a document, e.g., a target, or those forbidden in tables, e.g.,
an inline src-block."
  (let ((data (org-element-parse-secondary-string
	       item (org-element-restriction 'headline))))
    (org-element-map data
	'(footnote-reference inline-babel-call inline-src-block target
			     radio-target statistics-cookie)
      #'org-element-extract)
    (org-quote-vert
     (org-no-properties
      (org-element-interpret-data data)))))

;;;###autoload
(defun org-dblock-write:columnview (params)
  "Write the column view table.

PARAMS is a property list of parameters:

`:id' (mandatory)

    The ID property of the entry where the columns view should be
    built.  When the symbol `local', call locally.  When `global'
    call column view with the cursor at the beginning of the
    buffer (usually this means that the whole buffer switches to
    column view).  When \"file:path/to/file.org\", invoke column
    view at the start of that file.  Otherwise, the ID is located
    using `org-id-find'.

`:exclude-tags'

    List of tags to exclude from column view table.

`:format'

    When non-nil, specify the column view format to use.

`:hlines'

    When non-nil, insert a hline before each item.  When
    a number, insert a hline before each level inferior or equal
    to that number.

`:indent'

    When non-nil, indent each ITEM field according to its level.

`:match'

    When set to a string, use this as a tags/property match filter.

`:maxlevel'

    When set to a number, don't capture headlines below this level.

`:skip-empty-rows'

    When non-nil, skip rows where all specifiers other than ITEM
    are empty.

`:vlines'

    When non-nil, make each column a column group to enforce
    vertical lines.

`:link'

    Link the item headlines in the table to their origins.

`:formatter'

    A function to format the data and insert it into the
    buffer.  Overrides the default formatting function set in
    `org-columns-dblock-formatter'."
  (let ((table
	 (let ((id (plist-get params :id))
	       view-file view-pos)
	   (pcase id
	     (`global nil)
	     ((or `local `nil) (setq view-pos (point)))
	     ((and (let id-string (format "%s" id))
		   (guard (string-match "^file:\\(.*\\)" id-string)))
	      (setq view-file (match-string-no-properties 1 id-string))
	      (unless (file-exists-p view-file)
		(user-error "No such file: %S" id-string)))
	     ((and (let idpos (org-find-entry-with-id id)) (guard idpos))
	      (setq view-pos idpos))
	     ((let `(,filename . ,position) (org-id-find id))
	      (setq view-file filename)
	      (setq view-pos position))
	     (_ (user-error "Cannot find entry with :ID: %s" id)))
	   (with-current-buffer (if view-file (org-get-agenda-file-buffer view-file)
				  (current-buffer))
	     (org-with-wide-buffer
	      (when view-pos (goto-char view-pos))
	      (org-columns--capture-view (plist-get params :maxlevel)
					 (plist-get params :match)
					 (plist-get params :skip-empty-rows)
					 (plist-get params :exclude-tags)
					 (plist-get params :format)
					 view-pos)))))
        (formatter (or (plist-get params :formatter)
                       org-columns-dblock-formatter
                       #'org-columns-dblock-write-default)))
    (funcall formatter (point) table params)))

(defun org-columns-dblock-write-default (ipos table params)
  "Write out a columnview table at position IPOS in the current buffer.
TABLE is a table with data as produced by `org-columns--capture-view'.
PARAMS is the parameter property list obtained from the dynamic block
definition."
  (let ((link (plist-get params :link))
	(width-specs
	 (mapcar (lambda (spec) (nth 2 spec))
		 org-columns-current-fmt-compiled)))
    (when table
      ;; Prune level information from the table.  Also normalize
      ;; headings: remove stars, add indentation entities, if
      ;; required, and possibly precede some of them with a horizontal
      ;; rule.
      (let ((item-index
	     (let ((p (assoc "ITEM" org-columns-current-fmt-compiled)))
	       (and p (cl-position p
				   org-columns-current-fmt-compiled
				   :test #'equal))))
	    (hlines (plist-get params :hlines))
	    (indent (plist-get params :indent))
	    new-table)
	;; Copy header and first rule.
	(push (pop table) new-table)
	(push (pop table) new-table)
	(dolist (row table (setq table (nreverse new-table)))
	  (let ((level (car row)))
	    (when (and (not (eq (car new-table) 'hline))
		       (or (eq hlines t)
			   (and (numberp hlines) (<= level hlines))))
	      (push 'hline new-table))
	    (when item-index
	      (let* ((raw (nth item-index (cdr row)))
		     (cleaned (org-columns--clean-item raw))
		     (item (if (not link) cleaned
			     (let ((search (org-link-heading-search-string raw)))
			       (org-link-make-string
				(if (not (buffer-file-name)) search
				  (format "file:%s::%s" (buffer-file-name) search))
				cleaned)))))
		(setf (nth item-index (cdr row))
		      (if (and indent (> level 1))
			  (concat "\\_" (make-string (* 2 (1- level)) ?\s) item)
			item))))
	    (push (cdr row) new-table))))
      (when (plist-get params :vlines)
	(setq table
	      (let ((size (length org-columns-current-fmt-compiled)))
		(append (mapcar (lambda (x) (if (eq 'hline x) x (cons "" x)))
				table)
			(list (cons "/" (make-list size "<>")))))))
      (when (seq-find #'identity width-specs)
        ;; There are width specifiers in column format.  Pass them
        ;; to the resulting table, adding alignment field as the first
        ;; row.
        (push (mapcar (lambda (width) (when width (format "<%d>" width))) width-specs) table))
      ;; now insert the table into the buffer
      (goto-char ipos)
      (let ((content-lines (org-split-string (plist-get params :content) "\n"))
	    recalc)
	;; Insert affiliated keywords before the table.
	(when content-lines
	  (while (string-match-p "\\`[ \t]*#\\+" (car content-lines))
	    (insert (string-trim-left (pop content-lines)) "\n")))
	(save-excursion
	  ;; Insert table at point.
	  (insert
	   (mapconcat (lambda (row)
			(if (eq row 'hline) "|-|"
			  (format "|%s|" (mapconcat #'identity row "|"))))
		      table
		      "\n"))
	  ;; Insert TBLFM lines following table.
	  (let ((case-fold-search t))
	    (dolist (line content-lines)
	      (when (string-match-p "\\`[ \t]*#\\+TBLFM:" line)
		(insert "\n" (string-trim-left line))
		(unless recalc (setq recalc t))))))
	(when recalc (org-table-recalculate 'all t))
	(org-table-align)
        (when (seq-find #'identity width-specs)
          (org-table-shrink))))))

;;;###autoload
(defun org-columns-insert-dblock ()
  "Create a dynamic block capturing a column view table."
  (interactive)
  (let ((id (completing-read
	     "Capture columns (local, global, entry with :ID: property) [local]: "
	     (append '(("global") ("local"))
		     (mapcar #'list (org-property-values "ID"))))))
    (org-create-dblock
     (list :name "columnview"
	   :hlines 1
	   :id (cond ((string= id "global") 'global)
		     ((member id '("" "local")) 'local)
		     (id)))))
  (org-update-dblock))

;;;###autoload
(eval-after-load 'org-mode
  '(progn
     (org-dynamic-block-define "columnview" #'org-columns-insert-dblock)))

(provide 'org-colview-dblock)

;;; org-colview-dblock.el ends here
