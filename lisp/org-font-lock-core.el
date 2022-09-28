;;; org-font-lock-core.el --- Org mode's font-lock backend  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2022 Free Software Foundation, Inc.

;; Author: Ihor Radchenko <yantar92@gmail.com>
;; Homepage: https://orgmode.org

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

;; This file contains implementation of font-locking for Org.
;; The font-locking is based on org-element parser.
;; Fontification can be controlled on per-element level and according
;; to smaller components inside an element.

;;; Code:

(require 'org-macs)
(require 'font-lock)
(require 'org-element-match)

(declare-function org-element-at-point "org-element" (&optional pom cached-only))
(declare-function org-element-map "org-element" (data types fun &optional info first-match no-recursion with-affiliated))
(declare-function org-element--parse-objects "org-element" (beg end acc restriction &optional parent))
(declare-function org-element-parse-buffer "org-element" (&optional granularity visible-only))
(declare-function org-element-parse-element "org-element" (&optional pos-or-element granularity visible-only first-only cached-only))
(declare-function org-element-set-element "org-element" (org new))
(declare-function org-element-copy "org-element" (datum))
(declare-function org-element-restriction "org-element" (element))
(declare-function org-element-parse-secondary-string "org-element" (string restriction &optional parent))
(declare-function org-element-put-property "org-element" (element property value))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function org-element-match "org-element-match" (&optional type element-or-object inner?))

(defvar org-element-all-elements)
(defvar org-element-all-objects)
(defvar org-element-greater-elements)
(defvar org-element-secondary-value-alist)
(defvar org-element-comment-string)
(defvar org-element-paragraph-separate)
(defvar org-element-parsed-keywords)

(defvar org-font-lock-verbose nil
  "Whether to display fontification info in the mode line.")

(defvar org-font-lock-element-keywords nil
  "Like `font-lock-keywords', but also allows matching current Org element.

In addition to the conventional `font-lock-keywords' MATCHER field, it
can be an element symbol or a list of such symbols.  The keyword will
be triggered when current element type is matching the symbol or an
element in the list.

The MATCHER can also take a form of (body).  Then, a matcher function
executing body will be constructed when body returns non-nil.

In addition, the current element match data will be available via
`org-element-match-last', `org-element-match-type', and similar
functions.

SUBEXP can be a symbol provided by org-element-match library.

Example:

Fontify :key and :value parts of keyword element.
`keyword' is the MATCHER for element type.  `:key' and `:value' and
SUBEXPs.

 (keyword (:key 'org-special-keyword t)
          (:value 'org-property-value t))

Display all the entities inside tables as \"???\".

 ((and (eq 'entity (org-element-match-type))
      (not (org-element-lineage (org-element-match-last) '(table))))
 (:full-no-blank
  (org-font-lock-compose
   \"???\"
   (org-element-match-beginning :full-no-blank)
   (org-element-match-end :full-no-blank))
  nil t))")

(defun org-font-lock-compose (string beg end)
  "Compose region BEG..END using STRING.
Return nil to make this function usable inside second element of a
font-lock highlight."
  (prog1 nil
    (compose-region
     beg end
     (let ((composition nil))
       (dolist (char (string-to-list string)
		     (nreverse (cdr composition)))
         (push char composition)
         (push '(Br . Bl) composition)))
     'decompose-region)))

(defvar org-font-lock-core--element-matcher-regexp-cache (make-hash-table)
  "Hash table storing regexps for `org-font-lock--element-matcher'.
This is more memory efficient then creating regexp strings every time.")
(defun org-font-lock--element-matcher (_)
  "Transfer `org-element-match' components into numeric match groups."
  ;; Go to the beginning of the current element.
  (goto-char (org-element-match-beginning))
  (let* ((component-count (1- (length (org-element-match-data))))
         (re (or (gethash
                  component-count
                  org-font-lock-core--element-matcher-regexp-cache)
                 (puthash
                  component-count
                  (mapconcat #'identity
                             (append
                              (make-list component-count "\\(")
                              '(".")
                              (make-list component-count "\\)"))
                             "" ;; Not optional in Emacs <29.
                             )
                  org-font-lock-core--element-matcher-regexp-cache))))
    ;; Create match data with appropriate number of elements.
    (re-search-forward re)
    (let ((match-data (match-data)))
      (cl-loop for (_ new-beg new-end) in (org-element-match-data)
               for (beg end . _) on match-data by 'cddr
               do
               (move-marker beg new-beg)
               (move-marker end new-end))
      (set-match-data match-data t)
      (goto-char (org-element-match-end :full)))))

(defun org-font-lock--compile-highlight (highlights)
  "Transform HIGHGLIGHTS to original font-lock format."
  (let ((new-highlights))
    (dolist (highlight highlights)
      (let ((count 0))
        (catch :found
          (dolist (component (org-element-match-data))
            (when (eq (car-safe highlight) (car component))
              (push (cons count (cdr highlight)) new-highlights)
              (throw :found t))
            (cl-incf count))
          ;; Ordinary highlight.  No need to alter.
          (push highlight new-highlights))))
    (nreverse new-highlights)))

(defun org-font-lock--compile-keyword (keyword)
  "Transform KEYWORD to original font-lock format."
  (let ((matcher (car-safe keyword)))
    (when (and matcher (symbolp matcher)
               (memq matcher org-element-match--all-types))
      (setq matcher (list matcher)))
    (pcase matcher
      (`eval
       (org-font-lock--compile-keyword (eval (cdr keyword))))
      ((and
        (pred listp)
        (pred (cl-every (lambda (type) (memq type org-element-match--all-types)))))
       ;; Filter out non-matching elements.
       (when (cl-some (lambda (type) (eq type (org-element-match-type))) matcher)
         (cons 'org-font-lock--element-matcher
               (org-font-lock--compile-highlight (cdr keyword)))))
      ((guard (not (eq 'lambda (car matcher))))
       (cons `(lambda (limit)
                (when ,matcher
                  (org-font-lock--element-matcher limit)))
             (org-font-lock--compile-highlight (cdr keyword))))
      (_
       ;; Ordinary font-lock matcher.  Leave the matcher and compile
       ;; highlight.
       (if (car-safe keyword)
           (cons (car-safe keyword)
                 (org-font-lock--compile-highlight (cdr keyword)))
         keyword)))))

(defsubst org-font-lock--compile-keywords (keywords)
  "Transform KEYWORDS as defined in `org-font-lock-element-keywords' to original
font-lock format.  Use `org-font-lock--current-element'."
  (delq nil (mapcar #'org-font-lock--compile-keyword keywords)))

(defun org-font-lock--fontify-object (object)
  "Fontify a single OBJECT."
  (org-element-match nil object)
  (let ((font-lock-keywords
         (org-font-lock--compile-keywords
          org-font-lock-element-keywords)))
    (font-lock-fontify-keywords-region
     (org-element-property :begin object)
     (org-element-property :end object)))
  (when org-font-lock-verbose
    (message "org-font-lock: Fontified %S(%S..%S) completely."
             (org-element-type object)
             (org-element-property :begin object)
             (org-element-property :end object))))

(defun org-font-lock--fontify-objects (beg end datum)
  "Fontify everything between BEG and END inside DATUM.
DATUM is a parse tree."
  (org-element-map datum
      org-element-match--all-types
    (lambda (el)
      (unless (or (<= end (org-element-property :begin el))
                  (> beg (org-element-property :end el)))
        (org-font-lock--fontify-object el)))
    nil nil nil 'with-affiliated))

(defun org-font-lock-matcher (limit)
  "Fontify first chunk down to LIMIT.  Move point as needed."
  (let ((beg (point)) (end limit))
    ;; `org-element-at-point' returns nil within blank lines at bob.
    ;; Skip it.
    (when (org-with-wide-buffer (skip-chars-backward " \t\n\r") (bobp))
      (skip-chars-forward " \t\n\r"))
    (when org-font-lock-verbose
      (message "org-font-lock: About to fontify %S..%S" beg limit))
    (let ((element (org-element-at-point)))
      ;; Parent element might also start at BEG.  Start fontification
      ;; from the outermost element starting at BEG.
      (while (and (org-element-property :parent element)
                  (not (eq 'org-data
                         (org-element-type
                          (org-element-property :parent element))))
                  (eq (org-element-property :begin element)
                      (org-element-property
                       :begin
                       (org-element-property :parent element))))
        (setq element (org-element-property :parent element)))
      (catch :next
        (while element
          ;; Set match data for font-lock.el.
          (org-element-match nil element)
          (org-font-lock--element-matcher nil)
          ;; Fontify the element and all the objects inside.
          ;; We do not modify match data to keep only current element
          ;; match for font-lock.
          (org-element-match-save-data
              (org-font-lock--fontify-objects
               beg end
               (org-element-parse-element
                element 'object nil 'no-recursion))
            (goto-char beg)
            (org-element-match-forward
             (append org-element-all-elements
                     org-element-greater-elements)
             end
             element)
            (if (org-element-match-data)
                (goto-char (org-element-match-beginning))
              ;; No other element before end.  element is completely
              ;; fontified.  Move to its end.
              (goto-char (min end (org-element-property :end element))))
            (if (and (org-element-match-data)
                     (let ((next (org-element-at-point))
                           (prev (org-element-at-point beg)))
                       (and (eq (org-element-type prev)
                                (org-element-type next))
                            (eq (org-element-property :begin prev)
                                (org-element-property :begin next)))))
                ;; We cannot rely on `org-element-at-point' to return
                ;; correct element in the next iteration.  Continue
                ;; fontification within this call.
                (setq element (org-element-match-last))
              ;; Next element starts beyond beginning of current.  We
              ;; are done here trusting font-lock to resume the
              ;; fontification from the next point.
              (throw :next t)))))
      (when (and element org-font-lock-verbose)
        (message "org-font-lock: Fontified %S(%S:+%S) up to +%S"
                 (org-element-type element)
                 (org-element-property :begin element)
                 (- (org-element-property :end element)
                    (org-element-property :begin element))
                 (- (point)
                    (org-element-property :begin element))))
      (if element
          `(jit-lock-bounds ,(org-element-property :begin element) . ,(point))
        (goto-char limit)
        t))))

(defcustom org-font-lock-timeout 0.6
  "Timeout in seconds between font-lock refresh runs.

This variable is controlling how often org-font-lock re-parses edited
regions to detect broken structural changes."
  :group 'org-appearance
  :type 'number)

(defvar-local org-font-lock--flush-queue nil
  "List of regions to be flushed in current buffer.
See `org-font-lock-flush' and `org-font-lock-flush-delayed'.")
(defun org-font-lock-flush (beg end &optional buffer)
  "Re-fontify all the elements in BUFFER intersecting with BEG..END."
  (when (or (not buffer) (buffer-live-p buffer))
    (with-current-buffer (or buffer (current-buffer))
      (if (not font-lock-mode)
          ;; Font-lock disabled.  Clear the queue.
          (setq org-font-lock--flush-queue nil)
        (org-with-wide-buffer
         (let ((regions (if (and beg end)
                            (cons (cons beg end) org-font-lock--flush-queue)
                          org-font-lock--flush-queue))
               region next-region)
           (setq org-font-lock--flush-queue nil)
           (when regions
             (setq regions
                   (sort
                    regions
                    (lambda (a b) (or (< (car a) (car b))
                                 (and (= (car a) (car b))
                                      (< (cdr a) (cdr b))))))))
           (while regions
             (setq region (pop regions))
             (setq next-region (car regions))
             (if (and next-region
                      ;; Regions intersect.
                      (<= (car region) (car next-region) (cdr region)))
                 ;; Merge them.
                 (setcar (car regions) (car region))
               (when org-font-lock-verbose
                 (message "org-font-lock: Flusing %S..%S after %f idle"
                          beg end (float-time (current-idle-time))))
               (setq beg (car region) end (cdr region))
               (let* ((beg-element (org-element-at-point beg))
                      (end-element (org-element-at-point end))
                      (beg (min beg
                                (or (org-element-property :begin beg-element) beg)
                                (or (org-element-property :begin end-element) beg)))
                      (end (min (point-max)
                                (max end
                                     (or (org-element-property :end end-element) end)
                                     (or (org-element-property :end beg-element) end)))))
                 (font-lock-flush (max (point-min) (min beg end))
                          (min (point-max) (max beg end))))))))))))

(defvar org-font-lock--last-flush (current-time)
  "Last time `org-font-lock-flush-delayed has been ran.")
(defun org-font-lock-flush-delayed (beg end &optional _)
  "Re-fontify BEG..END on idle according to `org-font-lock-timeout'."
  (when font-lock-mode
    (let ((region (org-font-lock--extend-region beg end nil)))
      (setq beg (car region) end (cdr region)))
    (if (<= (float-time (time-since org-font-lock--last-flush))
           org-font-lock-timeout)
        (progn
          (push (cons beg end) org-font-lock--flush-queue)
          ;; Update when user stops typing.
          (unless noninteractive
            (run-with-idle-timer
             org-font-lock-timeout nil
             #'org-font-lock-flush nil nil (current-buffer))))
      (setq org-font-lock--last-flush (current-time))
      (org-font-lock-flush beg end))))

(defun org-font-lock--extend-region (beg end _)
  "Extend changed BEG..END region to element boundaries, if cached."
  (let ((beg-element (org-element-at-point beg 'cached))
        (end-element (org-element-at-point end 'cached)))
    (cons (or (org-element-property :begin beg-element) beg)
          (or (org-element-property :end end-element) end))))

(provide 'org-font-lock-core)
;;; org-font-lock-core.el ends here
