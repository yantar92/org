;;; org-font-lock-core.el --- Org mode's font-lock backend  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2022 Free Software Foundation, Inc.

;; Author: Ihor Radchenko <yantar92@gmail.com>
;; Keywords: faces
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

(declare-function org-element-at-point "org-element" (&optional pom cached-only))
(declare-function org-element-map "org-element" (data types fun &optional info first-match no-recursion with-affiliated))
(declare-function org-element--parse-objects "org-element" (beg end acc restriction &optional parent))
(declare-function org-element-restriction "org-element" (element))
(declare-function org-element-parse-secondary-string "org-element" (string restriction &optional parent))
(declare-function org-element-put-property "org-element" (element property value))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))

(defvar org-element-all-elements)
(defvar org-element-all-objects)
(defvar org-element-greater-elements)
(defvar org-element-secondary-value-alist)
(defvar org-element-comment-string)
(defvar org-element-paragraph-separate)

(defvar org-font-lock-current-element nil
  "Dynamically scoped element to be fontified.
This variable is bound to non-nil during fontification.")

(defvar org-font-lock-current-element-components nil
  "Dynamically scoped alist of components for the fontified element.
Keys of the alist are the component symbols and the values are buffer
regions for the elements.")

(defvar org-font-lock-element-keywords nil
  "Like `font-lock-keywords', but also allows MATCHER to be element symbol
and SUBEXP to be element component.")

(defun org-font-lock--group-keywords (keywords)
  "Merge keywords for the same elements."
  (let (matchers result)
    (dolist (keyword keywords)
      (when (eq (car-safe keyword) 'eval)
        (setq keyword (eval (cdr keyword))))
      (if (car-safe keyword)
          (let ((highlights (cdr keyword)))
            (dolist (highlight highlights)
              (unless (car-safe highlight)
                (setq highlight (cons :full highlight)))
              (if (member (car-safe keyword) matchers)
                  (push highlight (alist-get (car-safe keyword) result))
                (push (car-safe keyword) matchers)
                (push (list (car-safe keyword) highlight) result))))
        (push keyword result)))
    ;; Keep highlight order.
    (dolist (keyword result)
      (when (car-safe keyword)
        (setcdr keyword (nreverse (cdr keyword)))))
    result))

(defun org-font-lock--element-matcher (_)
  "Match element according to `org-font-lock--current-element-components'."
  ;; Go to the beginning of the current element.
  (goto-char (car (alist-get :full org-font-lock-current-element-components)))
  (let* ((component-count (1- (length org-font-lock-current-element-components)))
         (re (mapconcat #'identity
                        (append
                         (make-list component-count "\\(")
                         '(".")
                         (make-list component-count "\\)")))))
    ;; Create match data with appropriate number of elements.
    (re-search-forward re)
    (let ((match-data (match-data)))
      (cl-loop for (_ new-beg new-end) in org-font-lock-current-element-components
               for (beg end . _) on match-data by 'cddr
               do
               (move-marker beg new-beg)
               (move-marker end new-end))
      (set-match-data match-data t)
      (goto-char (cadr (alist-get :full org-font-lock-current-element-components))))))

(defun org-font-lock--compile-keyword (keyword)
  "Transform KEYWORD to original font-lock format.
Use `org-font-lock--current-element'."
  (if (and (car-safe keyword)
           (or (member (car-safe keyword) org-element-all-elements)
               (member (car-safe keyword) org-element-all-objects)
               (member (car-safe keyword) org-element-greater-elements)))
      ;; Filter out non-matching elements.
      (when (eq (car-safe keyword)
                (org-element-type org-font-lock-current-element))
        (let ((new-highlights))
          (dolist (highlight (cdr keyword))
            (let ((count 0))
              (catch :found
                (dolist (component org-font-lock-current-element-components)
                  (when (eq (car-safe highlight) (car component))
                    (push (cons count (cdr highlight)) new-highlights)
                    (throw :found t))
                  (cl-incf count))
                ;; Ordinary matcher.  No need to alter.
                (push highlight new-highlights))))
          (cons 'org-font-lock--element-matcher (nreverse new-highlights))))
    ;; Ordinary font-lock keyword.  Leave it as is.
    keyword))

(defsubst org-font-lock--compile-keywords (keywords)
  "Transform KEYWORDS as defined in `org-font-lock-element-keywords' to original
font-lock format.  Use `org-font-lock--current-element'."
  (delq nil (mapcar #'org-font-lock--compile-keyword keywords)))

(defun org-font-lock--fontify-object (object)
  "Fontify a single OBJECT."
  (let* ((org-font-lock--component-matcher
          (or (intern-soft (format "org-font-lock--matcher-%S" (org-element-type object)))
              'org-font-lock--matcher-default)))
    (setq org-font-lock-current-element object
          org-font-lock-current-element-components (funcall org-font-lock--component-matcher object))
    (let ((font-lock-keywords (org-font-lock--compile-keywords org-font-lock-element-keywords)))
      (font-lock-fontify-keywords-region
       (org-element-property :begin object)
       (org-element-property :end object)))))

(defun org-font-lock--fontify-objects (beg end datum)
  "Fontify everything between BEG and END inside DATUM.
DATUM is a parse tree."
  (org-element-map datum (append org-element-all-objects
                                 org-element-all-elements
                                 org-element-greater-elements)
    `(lambda (el)
       (unless (or (<= ,end (org-element-property :begin el))
                   (> ,beg (org-element-property :end el)))
         (org-font-lock--fontify-object el)))
    nil nil nil 'with-affiliated))

(defun org-font-lock-matcher (limit)
  "Fontify first chunk down to LIMIT.  Move point as needed."
  (setq org-font-lock-current-element nil
        org-font-lock-current-element-components nil
        org-font-lock-element-keywords (org-font-lock--group-keywords org-font-lock-element-keywords))
  (let ((beg (point)) (end limit))
    ;; `org-element-at-point' returns nil within blank lines at bob.
    ;; Skip it.
    (when (org-with-wide-buffer (skip-chars-backward " \t\n\r") (bobp))
      (skip-chars-forward " \t\n\r")
      (setq beg (point)))
    (unwind-protect
        (let ((element (org-element-at-point beg)))
          (when element
            (if (or (not (org-element-property :contents-begin element))
                    (< beg (org-element-property :contents-begin element))
                    (>= beg (org-element-property :contents-end element)))
                ;; Fontify the element itself and move point to
                ;; :contents-begin or :end for further fontification.
                (progn
                  (catch :done
                    ;; Parse objects outside element contents.
                    (dolist (parsed-property
                             (alist-get
                              (org-element-type element)
                              org-element-secondary-value-alist))
                      (when (stringp (org-element-property parsed-property element))
                        (org-element-set-element
                         element
                         (org-with-point-at (org-element-property :begin element)
                           (org-element--current-element
                            (org-element-property :end element)
                            'object
                            (org-element-property :mode element)
                            (org-element-property :structure element))))
                        (throw :done t)))
                    ;; Parse objects inside parsed affiliated keywords.
                    (dolist (parsed-property
                             (mapcar (lambda (prop)
                                       (intern (concat ":" (downcase prop))))
                                     org-element-parsed-keywords))
                      (when (org-element-property parsed-property element)
                        (setq element
                              (org-with-point-at (org-element-property :begin element)
                                (org-element--current-element
                                 (org-element-property :end element)
                                 'object
                                 (org-element-property :mode element)
                                 (org-element-property :structure element))))
                        (throw :done t))))
                  ;; Fontify affiliated keywords.
                  (when (and (org-element-property :post-affiliated element)
                             (> (org-element-property :post-affiliated element)
                                (org-element-property :begin element)))
                    (org-with-wide-buffer
                     (narrow-to-region
                      (org-element-property :begin element)
                      (org-element-property :post-affiliated element))
                     (org-font-lock--fontify-objects
                      beg end
                      (org-element-map (org-element-parse-buffer) 'keyword #'identity))))
                  ;; Fontify element and secondary objects inside.
                  (org-font-lock--fontify-objects beg end element)
                  (if (and (org-element-property :contents-begin element)
                           (>= (org-element-property :contents-begin element) beg))
                      (goto-char (org-element-property :contents-begin element))
                    (goto-char (org-element-property :end element))))
              ;; We are inside lesser element.  Fontify all the objects
              ;; inside.
              ;; Collect all the objects inside.
              (org-element--parse-objects
               (org-element-property :contents-begin element)
               (org-element-property :contents-end element)
               element
               (org-element-restriction element))
              (org-font-lock--fontify-objects beg end element)
              (if (and (eq (org-element-property :contents-begin element)
                           (org-element-property :post-affiliated element))
                       ;; Lists and tables have inner element right at
                       ;; begin.
                       (memq (org-element-type element) '(table plain-list)))
                  (goto-char (1+ (org-element-property :contents-begin element)))
                ;; Finished fontifying element.  Move point to its :end.
                (goto-char (org-element-property :end element)))))
          (if element
              `(jit-lock-bounds ,(org-element-property :begin element) . ,(point))
            (goto-char limit)
            t))
      (setq org-font-lock-current-element nil
            org-font-lock-current-element-components nil))))

(defmacro org-font-lock-cond (&rest forms)
  "Fontify when FORMS return non-nil"
  `(lambda (limit)
     (when (progn ,@forms)
       (org-font-lock--element-matcher limit))))

(defun org-font-lock--matcher-default (element)
  "Match generic ELEMENT."
  (list
   `(:full
     ,(org-element-property :begin element)
     ,(org-element-property :end element))
   `(:full-no-blank
     ,(org-with-wide-buffer
       (goto-char (org-element-property :begin element))
       (skip-chars-forward "\rn\t ")
       (point))
     ,(org-with-wide-buffer
       (goto-char (org-element-property :end element))
       (skip-chars-backward "\rn\t ")
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

(defun org-font-lock--matcher-emphasis (element)
  "Match an emphasis ELEMENT."
  (org-font-lock--matcher-default element))

(defalias 'org-font-lock--matcher-bold #'org-font-lock--matcher-emphasis)
(defalias 'org-font-lock--matcher-italic #'org-font-lock--matcher-emphasis)
(defalias 'org-font-lock--matcher-underline #'org-font-lock--matcher-emphasis)
(defalias 'org-font-lock--matcher-strike-through #'org-font-lock--matcher-emphasis)

(defun org-font-lock--matcher-verbatim (element)
  "Match verbatim ELEMENT."
  (let ((result (org-font-lock--matcher-emphasis element)))
    ;; Verbatim does not have :contents.
    (setcdr (assoc :begin-marker result)
            `(,(car (alist-get :full-no-blank result))
              ,(1+ (car (alist-get :full-no-blank result)))))
    (setcdr (assoc :end-marker result)
            `(,(1- (cadr (alist-get :full-no-blank result)))
              ,(cadr (alist-get :full-no-blank result))))
    result))
(defalias 'org-font-lock--matcher-code #'org-font-lock--matcher-verbatim)

(defun org-font-lock--matcher-keyword (element)
  "Match keyword ELEMENT."
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
     (org-font-lock--matcher-default element)
     (list
      `(:key ,beg-key ,end-key)
      `(:value ,beg-value ,end-value)))))
(defalias 'org-font-lock--matcher-node-property #'org-font-lock--matcher-keyword)

(defun org-font-lock--matcher-table-row (element)
  "Match table-row ELEMENT."
  (nconc
   (org-font-lock--matcher-default element)
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

(defun org-font-lock--matcher-headline (element)
  "Match headline ELEMENT."
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
                            (when (looking-at "\\(\\[#.\\]\\)[ \t]*")
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
        (org-font-lock--matcher-default element)
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

;; FIXME: Make sure that affiliated keywords and especially parsed
;; affiliated keywords are also fontified.

(provide 'org-font-lock-core)
;;; org-font-lock-core.el ends here
