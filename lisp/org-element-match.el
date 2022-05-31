;;; org-element-match.el --- Regexp search-like matching of Org elements  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Ihor Radchenko

;; Author: Ihor Radchenko <yantar92@gmail.com>
;; Keywords: matching

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

;; This file implements `re-search'-like functionality on Org buffers.
;; The matching is done based on the parsed element representation by
;; org-element library.

;; The matched element data also contain a match group-like
;; information accessible via `org-element-match-data'.  However, unlike `match-data',
;; match groups (components) are not numbered, but can be accessed as
;; pre-defined symbols.  The available symbols vary from element to
;; element except a common set of match components:
;; `:full'               - Full element boundaries;
;; `:full-no-blank'      - Full element without blank lines/spaces
;;                         around;
;; `:full-no-affiliated' - Full element without affiliated keywords;
;; `:affiliated'         - Affiliated keyword boundaries, if any;
;; `:contents'           - Element contents, if any;
;; `:begin-marker'       - "Begin" line/marker of an element before
;;                         its contents, if there is contents;
;; `:end-marker'         - "End" line/marker ...

;; The API is similar to usual regexp search: search functions
;; maintain a global match data that is updates after every search.
;; The entry functions are:
;; - `org-element-match' matching element type at point.  Unlike
;;                       `looking-at', there is no requirement to be
;;                       at the beginning of the matched element.
;; - `org-element-match-forward'          search next element of type starting after
;;                       point.
;; - `org-element-match-string'           returning last matched string for a component.
;; - `org-element-match-beginning' and `org-element-match-end' mirroring `match-beginning' and `match-end'.

;; In addition, last matched element can be directly accessed via
;; `org-element-match-last', `org-element-match-property', and `org-element-match-type'.

;; FIXME: List extra components in individual matcher docstrings.

;;; Code:

(require 'org-macs)
(require 'cl-macs)

(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-set-contents "org-element" (element &rest contents))
(declare-function org-element--parse-elements "org-element" (beg end mode structure granularity visible-only acc &optional first-only))
(declare-function org-element-map "org-element" (data types fun &optional info first-match no-recursion with-affiliated))
(declare-function org-element-type "org-element" (element))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-lineage "org-element" (datum &optional types with-self))

(defvar org-element-comment-string)
(defvar org-clock-string)
(defvar org-element-all-objects)
(defvar org-element-greater-elements)
(defvar org-element-all-elements)

;;;; Internal variables

(defvar org-element-match--element nil
  "Last matched element.")
(defvar org-element-match--data nil
  "Alist storing last matched element components.")

;; FIXME: It should ideally depend on org-element.el consts, but
;; (require 'org-element) is impossible because of circular
;; dependencies.
(defconst org-element-match--all-types
  '( babel-call center-block clock comment comment-block diary-sexp drawer
     dynamic-block example-block export-block fixed-width
     footnote-definition headline horizontal-rule inlinetask item
     keyword latex-environment node-property paragraph plain-list
     planning property-drawer quote-block section
     special-block src-block table table-row verse-block
     center-block drawer dynamic-block footnote-definition headline inlinetask
     item plain-list property-drawer quote-block section
     special-block table org-data
     bold citation citation-reference code entity export-snippet
     footnote-reference inline-babel-call inline-src-block italic line-break
     latex-fragment link macro radio-target statistics-cookie strike-through
     subscript superscript table-cell target timestamp underline verbatim)
  "List of all possible element types.")

;;;;; Quick regexps to search for next element or object

;; The below regexps are designed to be short, but may match
;; false-positives.  The purpose is fast-skipping to next element
;; without overloading Emacs regexp matching engine.

(rx-define org-space (any ?\s ?\t))
(rx-define org-indent (seq line-start (0+ org-space)))
(defconst org-element-match--quick-re-org-data nil) ; not meaningful
(defconst org-element-match--quick-re-headline (rx line-start (1+ "*") " "))
(defconst org-element-match--quick-re-section nil)
(defconst org-element-match--quick-re-comment (rx org-indent "#" (or ?\s eol)))
(defconst org-element-match--quick-re-planning
  (rx-to-string
   `(seq org-indent (or "CLOSED:" "DEADLINE:" "SCHEDULED:"))))
(defconst org-element-match--quick-re-drawer
  (rx org-indent
      ":" (1+ (any ?- ?_ word)) ":" (0+ org-space) line-end))
(defconst org-element-match--quick-re-property-drawer (rx org-indent ":PROPERTIES:"))
(defconst org-element-match--quick-re-clock (rx org-indent "CLOCK:"))
(defconst org-element-match--quick-re-inlinetask org-element-match--quick-re-headline)
(defconst org-element-match--quick-re-latex-environment
  (rx org-indent "\\begin{" (1+ (or alnum "*")) "}"))
(defconst org-element-match--quick-re-fixed-width (rx org-indent ?: (or ?\s eol)))
(defconst org-element-match--quick-re-special-block (rx org-indent "#+BEGIN_"))
(defconst org-element-match--quick-re-center-block
  (rx (regexp org-element-match--quick-re-special-block) "CENTER"))
(defconst org-element-match--quick-re-comment-block
  (rx (regexp org-element-match--quick-re-special-block) "COMMENT"))
(defconst org-element-match--quick-re-example-block
  (rx (regexp org-element-match--quick-re-special-block) "EXAMPLE"))
(defconst org-element-match--quick-re-export-block
  (rx (regexp org-element-match--quick-re-special-block) "EXPORT"))
(defconst org-element-match--quick-re-quote-block
  (rx (regexp org-element-match--quick-re-special-block) "QUOTE"))
(defconst org-element-match--quick-re-src-block
  (rx (regexp org-element-match--quick-re-special-block) "SRC"))
(defconst org-element-match--quick-re-verse-block
  (rx (regexp org-element-match--quick-re-special-block) "VERSE"))
(defconst org-element-match--quick-re-babel-call (rx org-indent "#+CALL:"))
(defconst org-element-match--quick-re-dynamic-block
  (rx org-indent "#+BEGIN" (optional ":") " "))
(defconst org-element-match--quick-re-keyword (rx org-indent "#+" (1+ (not space)) ":"))
(defconst org-element-match--quick-re-node-property org-element-match--quick-re-keyword)
(defconst org-element-match--quick-re-footnote-definition
  (rx bol "[fn:" (1+ (char ?- ?\s ?\] word)) "]"))
(defconst org-element-match--quick-re-horizontal-rule
  (rx org-indent (repeat 5 "-") (0+ org-space) eol))
(defconst org-element-match--quick-re-diary-sexp (rx bol "%%("))
(defconst org-element-match--quick-re-table
  (rx org-indent (or "|" ; Org table
                     (seq "+" (1+ (1+ "-") "+")) ; table.el table
                     )))
(defconst org-element-match--quick-re-table-row org-element-match--quick-re-table)
(defconst org-element-match--quick-re-plain-list
  (rx org-space
      (or (any ?- ?+)
          (seq (1+ org-space) "*")
          (seq (or (1+ digit) letter)
               (any ".)")))
      (or (1+ org-space) eol)))
(defconst org-element-match--quick-re-paragraph nil) ; No effective regexp exist.
(defconst org-element-match--quick-re-subscript (rx "_" (any "-{(*+.," alnum)))
(defconst org-element-match--quick-re-superscript (rx "^" (any "-{(*+.," alnum)))
(defconst org-element-match--quick-re-bold
  (rx (or line-start (any space ?- ?\( ?' ?\" ?\{)) "-*" (not space)))
(defconst org-element-match--quick-re-code
  (rx (or line-start (any space ?- ?\( ?' ?\" ?\{)) "~" (not space)))
(defconst org-element-match--quick-re-verbatim
  (rx (or line-start (any space ?- ?\( ?' ?\" ?\{)) "=" (not space)))
(defconst org-element-match--quick-re-strike-through
  (rx (or line-start (any space ?- ?\( ?' ?\" ?\{)) "+" (not space)))
(defconst org-element-match--quick-re-underline
  (rx (or line-start (any space ?- ?\( ?' ?\" ?\{)) "_" (not space)))
(defconst org-element-match--quick-re-italic
  (rx (or line-start (any space ?- ?\( ?' ?\" ?\{)) "/" (not space)))
(defconst org-element-match--quick-re-citation
  (rx "[cite"
      (opt "/" (1+ (any "/_-" alnum))) ;style
      ":"
      (0+ (any "\t\n "))))
(defconst org-element-match--quick-re-citation-reference
  (rx "@" (1+ (any word "-.:?!`'/*@+|(){}<>&_^$#%~"))))
(defconst org-element-match--quick-re-footnote-reference (rx "[fn:"))
(defconst org-element-match--quick-re-statistics-cookie
  (rx "["
      (0+ (char "[0-9]"))
      (or "%"
          (seq "/" (0+ (char "[0-9]"))))
      "]"))
(defconst org-element-match--quick-re-export-snippet "@@")
(defconst org-element-match--quick-re-macro "{{{")
(defconst org-element-match--quick-re-latex-fragment (rx (or "$" ?\( ?\[ "\\\\")))
(defconst org-element-match--quick-re-item org-element-match--quick-re-plain-list)
(defconst org-element-match--quick-re-inline-babel-call
  (rx bow "call_" (not (any ?\n ?\s ?\t ?\( ?\[))))
(defconst org-element-match--quick-re-inline-src-block
  (rx bow "src_" (not (any ?\s ?\t ?\n ?\( ?\[))))
(defconst org-element-match--quick-re-line-break (rx "\\\\" (0+ org-space) eol))
(defconst org-element-match--quick-re-target (rx "<<"))
(defconst org-element-match--quick-re-radio-target (rx "<<<"))
(defconst org-element-match--quick-re-timestamp
  (rx
   (or
    (seq (any "[<")
         (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)
         (optional " " (*? nonl))
         (any "]>"))
    (regexp "<[0-9]+-[0-9]+-[0-9]+[^>\n]+?\\+[0-9]+[dwmy]>")
    (regexp "<%%([^>\n]+>"))))
(defconst org-element-match--quick-re-table-cell (rx "|" (1+ (not ?|) "|")))

;; Can only calculate dynamically
(defconst org-element-match--quick-re-entity nil)
(defconst org-element-match--quick-re-link nil)

;; Verify that regexps for all the elements and objects are defined
;; Also, set docstrings
(dolist (el org-element-match--all-types)
  (cl-assert
   (boundp (intern (format "org-element-match--quick-re-%s" el)))
   nil "org-element-match: Quick regexp for %S not defined" el)
  (put (intern (format "org-element-match--quick-re-%s" el))
       'variable-documentation
       (format "Quick regexp to skip to next %S element." el)))

;;;; Internal functions

(defmacro org-element-match--resolve-types (&optional types)
  "Convert TYPES into list of element types or nil for everything."
  `(pcase ,types
     (`nil nil)
     (`greater-element org-element-greater-elements)
     (`element (append org-element-greater-elements
                       org-element-all-elements))
     (`object nil)
     ((pred symbolp) (list ,types))
     ((pred listp) ,types)
     (_ (error "Uknown element type: %S" ,types))))

(defun org-element-match--quick-re (types)
  "Get quick regexp to move to next element TYPES.
Return nil when no such regexp can be constructed."
  (setq types (org-element-match--resolve-types types))
  (when types
    (let (re-list)
      (catch :no-re
        (dolist (type types)
          (if (symbol-value (intern (format "org-element-match--quick-re-%s" type)))
              (push `(regexp ,(symbol-value
                               (intern
                                (format "org-element-match--quick-re-%s" type))))
                    re-list)
            (throw :no-re nil)))
        (rx-to-string `(or ,@re-list))))))

(defun org-element-match--string (&optional component data)
  "Return `:full' or COMPONENT string from DATA."
  (setq component (or component :full))
  (let ((range (cdr (assq component data))))
    (when range (buffer-substring (car range) (cadr range)))))

(defun org-element-match--beginning (&optional component data)
  "Return beginning of element `:full' or COMPONENT from DATA."
  (setq component (or component :full))
  (cadr (assq component data)))

(defun org-element-match--end (&optional component data)
  "Return end of matched element `:full' or COMPONENT from DATA."
  (setq component (or component :full))
  (caddr (assq component data)))

(defmacro org-element-match--remove (component data)
  "Remove COMPONENT from match DATA."
  `(setf ,data (assq-delete-all ,component ,data)))

(defmacro org-element-match--add (component beg end data)
  "Add COMPONENT with BEG..END range to match DATA."
  `(progn
     (org-element-match--remove ,component ,data)
     (push (list ,component ,beg ,end) ,data)))

(defun org-element-match--default (element)
  "Match generic ELEMENT.
Return the following components:
`:full', `:full-no-blank', `:affiliated', `:full-no-affiliated',
`:contents', `:begin-marker', `:end-marker'."
  (list
   `(:full
     ,(org-element-property :begin element)
     ,(org-element-property :end element))
   `(:full-no-blank
     ,(org-with-wide-buffer
       (goto-char (org-element-property :begin element))
       (skip-chars-forward "\r\n\t ")
       (point))
     ,(org-with-wide-buffer
       (goto-char (org-element-property :end element))
       (skip-chars-backward "\r\n\t ")
       (point)))
   (if (and (org-element-property :post-affiliated element)
            (> (org-element-property :post-affiliated element)
               (org-element-property :begin element)))
       `(:affiliated
         ,(org-element-property :begin element)
         ,(org-element-property :post-affiliated element))
     `(:affiliated nil nil))
   `(:full-no-affiliated
     ,(or (org-element-property :post-affiliated element)
          (org-element-property :begin element))
     ,(org-element-property :end element))
   `(:contents
     ,(org-element-property :contents-begin element)
     ,(org-element-property :contents-end element))
   (if (and (org-element-property :contents-begin element)
            (> (org-element-property :contents-begin element)
               (or (org-element-property :post-affiliated element)
                   (org-element-property :begin element))))
       `(:begin-marker
         ,(or (org-element-property :post-affiliated element)
              (org-element-property :begin element))
         ,(org-element-property :contents-begin element))
     `(:begin-marker nil nil))
   (if (and (org-element-property :contents-end element)
            (> (org-element-property :end element)
               (org-element-property :contents-end element)))
       `(:end-marker
         ,(org-element-property :contents-end element)
         ,(org-with-wide-buffer
           (goto-char (org-element-property :end element))
           (skip-chars-backward "\r\n\t ")
           (point)))
     `(:end-marker nil nil))))

(defun org-element-match--planning (element)
  "Match planning ELEMENT.
Extra components are: `:scheduled-keyword', `:deadline-keyword', and
`:closed-keyword'."
  (let ((components (org-element-match--default element)))
    (org-with-wide-buffer
     (dolist (kwd '("scheduled" "deadline" "closed"))
       (let ((prop (intern (concat ":" kwd)))
             (kwd-str (symbol-value (intern (format "org-element-%s-keyword" kwd))))
             (cmp-name (intern (format ":%s-keyword" kwd))))
         (if (not (org-element-property prop element))
             (push `(,cmp-name nil nil) components)
           (goto-char (org-element-property :begin element))
           (search-forward kwd-str (org-element-property :end element))
           (org-element-match--add cmp-name (match-beginning 0) (match-end 0) components)))))
    components))

(defun org-element-match--clock (element)
  "Match clock ELEMENT."
  (let ((components (org-element-match--default element)))
    (org-with-wide-buffer
     (goto-char (org-element-property :begin element))
     (search-forward org-clock-string (org-element-property :end element))
     (org-element-match--add :clock-keyword (match-beginning 0) (match-end 0) components)
     components)))

(defun org-element-match--verbatim (element)
  "Match verbatim ELEMENT."
  (let ((components (org-element-match--default element)))
    ;; Verbatim does not have :contents.
    (org-element-match--add :begin-marker
           (org-element-match--beginning :full-no-blank components)
           (1+ (org-element-match--beginning :full-no-blank components))
           components)
    (org-element-match--add :end-marker
           (1- (org-element-match--end :full-no-blank components))
           (org-element-match--end :full-no-blank components)
           components)
    components))
(defalias 'org-element-match--code #'org-element-match--verbatim)

(defun org-element-match--keyword (element)
  "Match keyword ELEMENT.
Extra components are: `:key', `:value'."
  (let (beg-key end-key beg-value end-value)
    (save-match-data
      (org-with-point-at (org-element-property :begin element)
        (skip-chars-forward " \t")
        (setq beg-key (point))
        (let ((case-fold-search t)) (search-forward (org-element-property :key element)))
        (skip-chars-forward ":")
        (setq end-key (point))
        (let ((case-fold-search t)) (search-forward (org-element-property :value element)))
        (setq beg-value (match-beginning 0)
              end-value (match-end 0))))
    (nconc
     (org-element-match--default element)
     (list
      `(:key ,beg-key ,end-key)
      `(:value ,beg-value ,end-value)))))
(defalias 'org-element-match--node-property #'org-element-match--keyword)

(defun org-element-match--macro (element)
  "Match macro ELEMENT."
  (let ((components (org-element-match--default element))
        contents-beg contents-end)
    (setq contents-beg (+ 3 (car (alist-get :full-no-blank components))))
    (setq contents-end (- (cadr (alist-get :full-no-blank components)) 3))
    (org-element-match--add :contents contents-beg contents-end components)
    (org-element-match--add :begin-marker
                            (org-element-match--beginning :full-no-blank components)
                            contents-beg
                            components)
    (org-element-match--add :end-marker
                            contents-end
                            (org-element-match--end :full-no-blank components)
                            components)
    components))

(defun org-element-match--timestamp (element)
  "Match-timestamp element."
  (let ((components (org-element-match--default element)))
    (org-element-match--add :begin-marker
                            (org-element-match--beginning :full-no-blank components)
                            (1+ (org-element-match--beginning :full-no-blank components))
                            components)
    (org-element-match--add :end-marker
                            (1- (org-element-match--end :full-no-blank components))
                            (org-element-match--end :full-no-blank components)
                            components)
    (if (eq 'diary (org-element-property :type element))
        (org-element-match--add :diary-sexp
                                (+ 3 (org-element-match--beginning :full-no-blank components))
                                (org-element-match--beginning :end-marker components)
                                components)
      (org-with-wide-buffer
       (goto-char (org-element-match--beginning :full-no-blank components))
       (save-match-data
         ;; Begin range.
         (re-search-forward
          (concat
           "[<[]\\("
           (format "\\(%.4d\\)-\\(%.2d\\)-\\(%.2d\\)"
                   (org-element-property :year-start element)
                   (org-element-property :month-start element)
                   (org-element-property :day-start element))
           ".+"
           (if (and (org-element-property :hour-start element)
                    (org-element-property :minute-start element))
               (format "\\(%d\\):\\(%.2d\\)"
                       (org-element-property :hour-start element)
                       (org-element-property :minute-start element))
             "")
           "[^]>]*\\)[]>]")
          (org-element-property :end element))
         (org-element-match--add :timestamp-start (match-beginning 0) (match-end 0) components)
         (org-element-match--add :date-start (match-beginning 1) (match-end 1) components)
         (org-element-match--add :year-start (match-beginning 2) (match-end 2) components)
         (org-element-match--add :month-start (match-beginning 3) (match-end 3) components)
         (org-element-match--add :day-start (match-beginning 4) (match-end 4) components)
         (when  (match-beginning 5)
           (org-element-match--add :hour-start (match-beginning 5) (match-end 5) components)
           (org-element-match--add :minute-start (match-beginning 6) (match-end 6) components))
         ;; End range
         (when (memq (org-element-property :type element) '(active-range inactive-range))
           (goto-char (org-element-match--beginning :full-no-blank components))
           (when (re-search-forward "[]>]--[<[]"
                                    (org-element-property :end element)
                                    t)
             (org-element-match--add :middle-marker (match-beginning 0) (match-end 0) components))
           (goto-char (org-element-match--beginning :full-no-blank components))
           (re-search-forward
            (concat
             "[<[]\\("
             (format "\\(%.4d\\)-\\(%.2d\\)-\\(%.2d\\)"
                     (org-element-property :year-end element)
                     (org-element-property :month-end element)
                     (org-element-property :day-end element))
             ".+"
             (if (and (org-element-property :hour-end element)
                      (org-element-property :minute-end element))
                 (format "\\(%d\\):\\(%.2d\\)"
                         (org-element-property :hour-end element)
                         (org-element-property :minute-end element))
               "")
             "[^]>]*\\)[]>]")
            (org-element-property :end element))
           ;; Only set when we have [..]--[..]-type range, not time
           ;; range inside a single timestamp.
           (if (= (match-beginning 0) (org-element-match--beginning :timestamp-start components))
               ;; Time range.
               (when (match-beginning 5)
                 (org-element-match--add :time-range
                                         (org-element-match--beginning :hour-start components)
                                         (match-end 6)
                                         components))
             (org-element-match--add :timestamp-end (match-beginning 0) (match-end 0) components)
             (org-element-match--add :date-end (match-beginning 1) (match-end 1) components)
             (org-element-match--add :year-end (match-beginning 2) (match-end 2) components)
             (org-element-match--add :month-end (match-beginning 3) (match-end 3) components)
             (org-element-match--add :day-end (match-beginning 4) (match-end 4) components))
           (when (match-beginning 5)
             (org-element-match--add :hour-end (match-beginning 5) (match-end 5) components)
             (org-element-match--add :minute-end (match-beginning 6) (match-end 6) components)))
         (when (org-element-property :repeater-type element)
           (goto-char (org-element-match--beginning :full-no-blank components))
           (re-search-forward
            "\\([.+]?\\+\\)\\([0-9]+\\)\\([hdwmy]\\)"
            (org-element-property :end element))
           (org-element-match--add :repeater (match-beginning 0) (match-end 0) components))
         (when (org-element-property :warning-type element)
           (goto-char (org-element-match--beginning :full-no-blank components))
           (re-search-forward
            "\\(-\\)?-\\([0-9]+\\)\\([hdwmy]\\)"
            (org-element-property :end element))
           (org-element-match--add :warning (match-beginning 0) (match-end 0) components)))))
    components))

(defun org-element-match--link (element)
  "Match link ELEMENT.
Extra component is `:visible' containing visible part of the link."
  (let ((components (org-element-match--default element))
        visible-beg visible-end)
    (pcase (org-element-property :format element)
      (`bracket
       (org-element-match--add :begin-marker
              (org-element-match--beginning :full-no-blank components)
              (+ 2 (org-element-match--beginning :full-no-blank components))
              components)
       (org-element-match--add :end-marker
              (- (org-element-match--end :full-no-blank components) 2)
              (org-element-match--end :full-no-blank components)
              components)
       (if (org-element-property :contents-begin element)
           (setq visible-beg (org-element-property :contents-begin element)
                 visible-end (org-element-property :contents-end element))
         (setq visible-beg (cadr (alist-get :begin-marker components))
               visible-end (car (alist-get :end-marker components)))))
      (`angle
       (org-element-match--add :begin-marker
              (org-element-match--beginning :full-no-blank components)
              (1+ (org-element-match--beginning :full-no-blank components))
              components)
       (org-element-match--add :end-marker
              (1- (org-element-match--end :full-no-blank components))
              (org-element-match--end :full-no-blank components)
              components)
       (setq visible-beg (org-element-property :begin element))
       (setq visible-end (org-element-property :end element)))
      (`plain
       (org-element-match--add :begin-marker nil nil components)
       (org-element-match--add :end-marker nil nil components)
       (setq visible-beg (org-element-property :begin element))
       (setq visible-end (org-element-property :end element)))
      (unknown (error "Uknown link format: %S" unknown)))
    (nconc components
           (list `(:visible ,visible-beg ,visible-end)))))

(defun org-element-match--inline-src-block (element)
  "Match inline-src-block ELEMENT."
  (let ((components (org-element-match--default element)))
    (org-element-match--add :src-marker
           (org-element-match--beginning :full-no-blank components)
           ;; <src_
           (+ 5 (org-element-match--beginning :full-no-blank components))
           components)
    (org-element-match--add :language
           (org-element-match--end :src-marker components)
           (+ (length (org-element-property :language element))
              (org-element-match--end :src-marker components))
           components)
    (org-with-point-at (org-element-match--end :language components)
      (org-element-match--add :parameters-begin-marker nil nil components)
      (org-element-match--add :parameters-end-marker nil nil components)
      (org-element-match--add :parameters-with-marker nil nil components)
      (org-element-match--add :parameters-without-marker nil nil components)
      (let ((parameters? (eq ?\[ (char-after))))
        (when parameters?
          (org-element-match--add :parameters-begin-marker (point) (1+ (point)) components))
        (search-forward (org-element-property :value element))
        (when parameters?
          (org-element-match--add :parameters-with-marker
                 (org-element-match--beginning :parameters-begin-marker components)
                 (1- (match-beginning 0))
                 components)
          (org-element-match--add :parameters-without-marker
                 (1+ (org-element-match--beginning :parameters-with-marker components))
                 (1- (org-element-match--end :parameters-with-marker components))
                 components)
          (org-element-match--add :parameters-end-marker
                 (1- (org-element-match--end :parameters-with-marker components))
                 (org-element-match--end :parameters-with-marker components)
                 components))
        (org-element-match--add :value-begin-marker
               (1- (match-beginning 0))
               (match-beginning 0)
               components)
        (org-element-match--add :value
               (match-beginning 0)
               (match-end 0)
               components)
        (org-element-match--add :value-with-markers
               (1- (match-beginning 0))
               (1+ (match-end 0))
               components)
        (org-element-match--add :value-end-marker
               (match-end 0)
               (1+ (match-end 0))
               components)))
    components))

(defun org-element-match--export-snippet (element)
  "Match export-snippet ELEMENT.
Extra component is `:back-end'."
  (let ((components (org-element-match--default element)))
    (org-with-wide-buffer
     (goto-char (car (alist-get :full-no-blank components)))
     (re-search-forward "\\(@@\\)\\([^:]+:\\)")
     (org-element-match--add :begin-marker (match-beginning 1) (match-end 1) components)
     (org-element-match--add :back-end (match-beginning 2) (match-end 2) components)
     (org-element-match--add :back-end-no-colon
            (match-beginning 2)
            (1- (match-end 2))
            components)
     (org-element-match--add :contents
            (match-end 2)
            (- (org-element-match--end :full-no-blank components) 2)
            components)
     (org-element-match--add :end-marker
            (org-element-match--end :contents components)
            (+ 2 (org-element-match--end :contents components))
            components)
     components)))

(defun org-element-match--table-row (element)
  "Match table-row ELEMENT.
Extra component is `:line'."
  (nconc
   (org-element-match--default element)
   (list
    (if (org-element-property :contents-end element)
        `(:line
          ,(1- (org-element-property :contents-begin element))
          ,(org-element-property :contents-end element))
      `(:line
        ,(org-with-wide-buffer
          (goto-char (org-element-property :begin element))
          (skip-chars-forward " \t")
          (point))
        ,(org-with-wide-buffer
          (goto-char (org-element-property :end element))
          (skip-chars-backward "\r\n\t ")
          (point)))))))

(defun org-element-match--headline (element)
  "Match headline ELEMENT.
Extra components are: `:stars', `:leading-stars', `:todo',
`:priority', `:comment', `:tags', `:title', `:title-line',
`:title-line-whole'."
  (org-with-wide-buffer
   (save-match-data
     (let* ((beg (org-element-property :begin element))
            (stars-end (progn
                         (goto-char beg)
                         (skip-chars-forward "*")
                         (point)))
            (todo-beg (when (org-element-property :todo-keyword element)
                        (skip-chars-forward " \t")
                        (let (case-fold-search)
                          (when (looking-at (concat (org-element-property :todo-keyword element) " "))
                            (goto-char (match-end 0))))
                        (match-beginning 0)))
            (todo-end (and todo-beg (match-end 0)))
            (priority-beg (when (org-element-property :priority element)
                            (skip-chars-forward " \t")
                            (when (looking-at "\\(\\[#[A-Z0-9]\\]\\)[ \t]*")
                              (goto-char (match-end 0))
                              (match-beginning 1))))
            (priority-end (and priority-beg (match-end 1)))
            (comment-beg (when (and (org-element-property :commentedp element)
                                    (let (case-fold-search)
                                      (skip-chars-forward " \t")
                                      (looking-at org-element-comment-string)))
                           (goto-char (match-end 0))
                           (match-beginning 0)))
            (comment-end (and comment-beg (match-end 0)))
            (title-beg (prog1 (point)
                         (unless (or todo-beg priority-beg comment-beg)
                           ;; Headline like "* :tag:"
                           (skip-chars-backward " \t"))))
            (tags-beg (when (re-search-forward
			     "[ \t]+\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$"
			     (line-end-position)
			     'move)
		        (goto-char (match-beginning 1))))
            (tags-end (and tags-beg (match-end 1)))
            (line-end (line-end-position))
            (title-end (point)))
       (nconc
        (org-element-match--default element)
        (list
         `(:stars ,beg ,stars-end)
         (if (and stars-end
                  (> (org-element-property :level element) 1))
             `(:leading-stars ,beg ,(1- stars-end))
           `(:leading-stars nil nil))
         `(:todo ,todo-beg ,todo-end)
         `(:priority ,priority-beg ,priority-end)
         `(:comment ,comment-beg ,comment-end)
         `(:tags ,tags-beg ,tags-end)
         `(:title ,title-beg ,title-end)
         `(:title-line ,beg ,line-end)
         `(:title-line-whole ,beg ,(min (1+ line-end) (org-element-property :end element)))))))))

(defun org-element-match--inlinetask (element)
  "Match inlinetask ELEMENT.
Extra components are: `:stars', `:leading-stars', `:todo',
`:priority', `:comment', `:tags', `:title', `:title-line',
`:title-line-whole'."
  (let* ((components (org-element-match--headline element))
         (end-line-beg
          (or (org-element-property :contents-end element)
              (let ((case-fold-search t))
                (org-with-point-at (org-element-match--end :title-line components)
                  (org-skip-whitespace)
                  (when (< (point) (org-element-property :end element))
                    (point)))))))
    (org-element-match--add :first-star
           (org-element-match--beginning :stars components)
           (1+ (org-element-match--beginning :stars components))
           components)
    (org-element-match--add :last-stars
           (- (org-element-match--end :stars components) 2)
           (org-element-match--end :stars components)
           components)
    (org-element-match--add :mid-stars
           (1+ (org-element-match--beginning :stars components))
           (org-element-match--beginning :last-stars components)
           components)
    (unless end-line-beg
      (org-element-match--add :end-line nil nil components)
      (org-element-match--add :end-line-whole nil nil components)
      (org-element-match--add :end-stars nil nil components)
      (org-element-match--add :end-first-star nil nil components)
      (org-element-match--add :end-last-stars nil nil components)
      (org-element-match--add :end-mid-stars nil nil components))
    (when end-line-beg
      (org-element-match--add :end-line
             end-line-beg
             (org-element-match--end :full-no-blank components)
             components)
      (org-element-match--add :end-line-whole
             end-line-beg
             (min (1+ (org-element-match--end :full-no-blank components))
                  (org-element-property :end element))
             components)
      (org-element-match--add :end-stars
             end-line-beg
             (org-with-point-at end-line-beg
               (skip-chars-forward "*")
               (point))
             components)
      (org-element-match--add :end-first-star
             end-line-beg (1+ end-line-beg)
             components)
      (org-element-match--add :end-last-stars
             (- (org-element-match--end :end-stars components) 2)
             (org-element-match--end :end-stars components)
             components)
      (org-element-match--add :end-mid-stars
             (1+ end-line-beg)
             (org-element-match--beginning :end-last-stars components)
             components))
    components))

;;;; API

(defmacro org-element-match-save-data (&rest body)
  "Run BODY without modifying match data."
  (declare (debug (form body)) (indent 1))
  `(save-match-data
     (let ((org-element-match--data-saved org-element-match--data)
           (org-element-match--element-saved org-element-match--element))
       (unwind-protect
           (progn ,@body)
         (setq org-element-match--data org-element-match--data-saved
               org-element-match--element org-element-match--element-saved)))))

(defmacro org-element-match-last ()
  "Return last matched element."
  `org-element-match--element)

(defmacro org-element-match-type ()
  "Return type of the last matched element."
  `(org-element-type org-element-match--element))

(defmacro org-element-match-data ()
  "Return last matched element components data."
  `org-element-match--data)

(defmacro org-element-match-property (property)
  "Return PROPERTY of the last matched element."
  `(org-element-property ,property (org-element-match-last)))

(defsubst org-element-match-string (&optional component)
  "Return last matched `:full' or COMPONENT string."
  (org-element-match--string component (org-element-match-data)))

(defsubst org-element-match-beginning (&optional component)
  "Return beginning of the last matched element `:full' or COMPONENT."
  (org-element-match--beginning component (org-element-match-data)))

(defsubst org-element-match-end (&optional component)
  "Return end of the last matched element `:full' or COMPONENT."
  (org-element-match--end component (org-element-match-data)))

(defun org-element-match (&optional type element-or-object inner?)
  "Match TYPE ELEMENT-OR-OBJECT or element/object at point.
When TYPE is nil, match any element/object at point.
TYPE can be a list of element/object types.  The innermost matching
element/object will be returned.
When INNER? is non-nil, only try to match the innermost object/elemetn
at point."
  (setq org-element-match--element nil org-element-match--data nil)
  (setq type (org-element-match--resolve-types type))
  (let ((element (or element-or-object (org-element-context))))
    (when type
      (if inner?
          (unless (memq (org-element-type element) type)
            (setq element nil))
        (setq element (org-element-lineage element type t))))
    (when element
      (let ((matcher (intern-soft (format
                                   "org-element-match--%S"
                                   (org-element-type element)))))
        (unless (functionp matcher) (setq matcher 'org-element-match--default))
        (condition-case nil
            (setq org-element-match--data (funcall matcher element)
                  org-element-match--element element)
          (error (setq org-element-match--element nil org-element-match--data nil)))))))

(defun org-element-match-forward (&optional types bound current-element)
  "Move to and match next element or an element of TYPES.
TYPES can be an element type, object type, or a list of such.
BOUND, when non-nil, limits the search.

CURRENT-ELEMENT, when non-nil contains element at point.
Never match CURRENT-ELEMENT if it is provided."
  (setq types (org-element-match--resolve-types types))
  (setq org-element-match--data nil org-element-match--element nil)
  ;; `org-element-at-point' returns nil within blank lines at bob.
  ;; Skip it.
  (when (org-with-wide-buffer (skip-chars-backward " \t\n\r") (bobp))
    (skip-chars-forward " \t\n\r"))
  (let* ((re (org-element-match--quick-re types))
         (beg (point))
         (bound (or bound (point-max)))
         (next bound)
         (element (or current-element (org-element-at-point)))
         (match-object? (or (not types)
                            (cl-intersection
                             types org-element-all-objects))))
    ;; Check starting from outermost element starting at point.
    (when (= (point) (org-element-property :begin element))
      (while (and (org-element-property :parent element)
                  (not (eq 'org-data
                           (org-element-type
                            (org-element-property :parent element))))
                  (= (org-element-property :begin element)
                     (org-element-property
                      :begin
                      (org-element-property :parent element))))
        (setq element (org-element-property :parent element))))
    (prog1
        (catch :found
          (org-with-wide-buffer
           (while (< (point) bound)
             (org-element-map
                 (save-match-data
                   (org-element-parse-element
                    element
                    (if match-object? 'object 'element)
                    nil 'first))
                 org-element-match--all-types
               (lambda (el)
                 (when (and (>= (org-element-property :begin el) beg)
                            (not (and (eq (org-element-property :begin el)
                                        (org-element-property :begin current-element))
                                    (eq (org-element-type el)
                                        (org-element-type current-element))))
                            (org-element-match types el 'inner))
                   (setq next (min bound (org-element-property :end el)))
                   (throw :found (org-element-copy el)))
                 (unless (memq (org-element-type el)
                               org-element-all-objects)
                   (setq next (org-element-property :end el)))
                 nil)
               nil 'first-match nil 'with-affiliated)
             (when re (save-match-data (re-search-forward re bound 'move)))
             (when (> next (point)) (goto-char next))
             (setq next (point))
             ;; Check if we are at the end of outer element contents.
             ;; Move to parent's end if necessary.
             (let ((elp (org-element-at-point)))
               (while (and (<= (org-element-property :begin elp)
                              (org-element-property :begin element))
                           (< (point) bound))
                 (goto-char (org-element-property :end elp))
                 (setq next (point))
                 (setq elp (org-element-at-point)))
               (setq element elp)))
           ;; Nothing found.
           (set-match-data nil)
           nil))
      (goto-char next))))

(provide 'org-element-match)
;;; org-element-match.el ends here
