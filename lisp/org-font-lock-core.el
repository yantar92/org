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
(declare-function org-element--current-element "org-element" (limit &optional granularity mode structure add-to-cache))
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

(defvar org-font-lock-element-keywords nil
  "Like `font-lock-keywords', but also allows MATCHER to be element symbol
and SUBEXP to be element component.")

(defun org-font-lock-create-glyph (string)
  "Transform STRING into glyph, displayed correctly."
  (let ((composition nil))
    (dolist (char (string-to-list string)
		  (nreverse (cdr composition)))
      (push char composition)
      (push '(Br . Bl) composition))))

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
  (goto-char (org-element-match-property :begin))
  (let* ((component-count (1- (length (org-element-match-data))))
         (re (mapconcat #'identity
                        (append
                         (make-list component-count "\\(")
                         '(".")
                         (make-list component-count "\\)")))))
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
  "Transform KEYWORD to original font-lock format.
Use `org-font-lock--current-element'."
  (if (and (car-safe keyword)
           (or (member (car-safe keyword) org-element-all-elements)
               (member (car-safe keyword) org-element-all-objects)
               (member (car-safe keyword) org-element-greater-elements)))
      ;; Filter out non-matching elements.
      (when (eq (car-safe keyword) (org-element-match-type))
        (cons 'org-font-lock--element-matcher
              (org-font-lock--compile-highlight (cdr keyword))))
    ;; Ordinary font-lock matcher.  Leave the matcher and compile
    ;; highlight.
    (if (car-safe keyword)
        (cons (car-safe keyword)
              (org-font-lock--compile-highlight (cdr keyword)))
      keyword)))

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
     (org-element-property :end object))))

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
  (setq org-font-lock-element-keywords
        (org-font-lock--group-keywords org-font-lock-element-keywords))
  (let ((beg (point)) (end limit))
    ;; `org-element-at-point' returns nil within blank lines at bob.
    ;; Skip it.
    (when (org-with-wide-buffer (skip-chars-backward " \t\n\r") (bobp))
      (skip-chars-forward " \t\n\r")
      (setq beg (point)))
    (let ((element (org-element-match nil (org-element-at-point beg))))
      (when element
        ;; Set match data.
        (org-font-lock--element-matcher nil)
        ;; Fontify the element and all the objects inside.
        ;; We do not modify match data to keep only current element
        ;; match for font-lock.
        (org-element-match-save-data
            (org-font-lock--fontify-objects
             beg end
             (org-element-parse-element
              element 'object nil 'no-recursion)))
        (goto-char (max beg (1+ (org-element-property :begin element))))
        (org-element-match-forward
         (append org-element-all-elements
                 org-element-greater-elements)
         end
         element)
        (when (org-element-match-data)
          (goto-char (org-element-match-beginning))))
      (if element
          `(jit-lock-bounds ,(org-element-property :begin element) . ,(point))
        (goto-char limit)
        t))))

(defmacro org-font-lock-cond (&rest forms)
  "Fontify when FORMS return non-nil"
  `(lambda (limit)
     (when (progn ,@forms)
       (org-font-lock--element-matcher limit))))

(provide 'org-font-lock-core)
;;; org-font-lock-core.el ends here
