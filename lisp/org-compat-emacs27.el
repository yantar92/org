;;; org-compat-emacs27.el --- Compatibility Code for Older Emacsen -*- lexical-binding: t; -*-

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

;; This file contains code needed for compatibility with older
;; versions of GNU Emacs and integration with other packages.
;;; Emacs < 27 compatibility

;;; Code:

;; `flatten-tree' was added in Emacs 27.1.
(if (fboundp 'flatten-tree)
    (defalias 'org--flatten-tree #'flatten-tree)
  ;; The implementation is taken from Emacs subr.el 8664ba18c7c5.
  (defun org--flatten-tree (tree)
    "Return a \"flattened\" copy of TREE.

A `flatten-tree' polyfill for compatibility with Emacs versions
older than 27.1"
    (let (elems)
      (while (consp tree)
        (let ((elem (pop tree)))
          (while (consp elem)
            (push (cdr elem) tree)
            (setq elem (car elem)))
          (if elem (push elem elems))))
      (if tree (push tree elems))
      (nreverse elems))))

(if (version< emacs-version "27.1")
    (defsubst org-replace-buffer-contents (source &optional _max-secs _max-costs)
      (replace-buffer-contents source))
  (defalias 'org-replace-buffer-contents #'replace-buffer-contents))

(unless (fboundp 'proper-list-p)
  ;; `proper-list-p' was added in Emacs 27.1.  The function below is
  ;; taken from Emacs subr.el 200195e824b^.
  (defun proper-list-p (object)
    "Return OBJECT's length if it is a proper list, nil otherwise.
A proper list is neither circular nor dotted (i.e., its last cdr
is nil)."
    (and (listp object) (ignore-errors (length object)))))

(if (fboundp 'xor)
    ;; `xor' was added in Emacs 27.1.
    (defalias 'org-xor #'xor)
  (defsubst org-xor (a b)
    "Exclusive `or'."
    (if a (not b) b)))

(unless (fboundp 'pcomplete-uniquify-list)
  ;; The misspelled variant was made obsolete in Emacs 27.1
  (defalias 'pcomplete-uniquify-list 'pcomplete-uniqify-list))

(if (fboundp 'time-convert)
    (progn
      (defsubst org-time-convert-to-integer (time)
	(time-convert time 'integer))
      (defsubst org-time-convert-to-list (time)
	(time-convert time 'list)))
  (defun org-time-convert-to-integer (time)
    (floor (float-time time)))
  (defun org-time-convert-to-list (time)
    (seconds-to-time (float-time time))))

;; `newline-and-indent' did not take a numeric argument before 27.1.
(if (version< emacs-version "27")
    (defsubst org-newline-and-indent (&optional _arg)
      (newline-and-indent))
  (defalias 'org-newline-and-indent #'newline-and-indent))

(defun org--set-faces-extend (faces extend-p)
  "Set the :extend attribute of FACES to EXTEND-P.

This is a no-op for Emacs versions lower than 27, since face
extension beyond end of line was not controllable."
  (when (fboundp 'set-face-extend)
    (mapc (lambda (f) (set-face-extend f extend-p)) faces)))

(if (fboundp 'string-distance)
    (defalias 'org-string-distance 'string-distance)
  (defun org-string-distance (s1 s2)
    "Return the edit (levenshtein) distance between strings S1 S2."
    (let* ((l1 (length s1))
	   (l2 (length s2))
	   (dist (vconcat (mapcar (lambda (_) (make-vector (1+ l2) nil))
				  (number-sequence 1 (1+ l1)))))
	   (in (lambda (i j) (aref (aref dist i) j))))
      (setf (aref (aref dist 0) 0) 0)
      (dolist (j (number-sequence 1 l2))
        (setf (aref (aref dist 0) j) j))
      (dolist (i (number-sequence 1 l1))
        (setf (aref (aref dist i) 0) i)
        (dolist (j (number-sequence 1 l2))
	  (setf (aref (aref dist i) j)
	        (min
	         (1+ (funcall in (1- i) j))
	         (1+ (funcall in i (1- j)))
	         (+ (if (equal (aref s1 (1- i)) (aref s2 (1- j))) 0 1)
		    (funcall in (1- i) (1- j)))))))
      (funcall in l1 l2))))

(unless (fboundp 'with-connection-local-variables)
  ;; Added in Emacs 27: commit:21f54feee8, 2019-03-09.
  ;; Redefining it using the old function `with-connection-local-profiles'.
  (defmacro with-connection-local-variables (&rest body)
    "Apply connection-local variables according to `default-directory'.
Execute BODY, and unwind connection-local variables."
    (declare (debug t))
    `(with-connection-local-profiles (connection-local-get-profiles nil)
                                     ,@body)))

;; assoc-delete-all missing from 26.1
(if (fboundp 'assoc-delete-all)
    (defalias 'org-assoc-delete-all 'assoc-delete-all)
  ;; from compat/compat-27.el
  (defun org-assoc-delete-all (key alist &optional test)
    "Delete all matching key from alist, default test equal"
    (unless test (setq test #'equal))
    (while (and (consp (car alist))
		(funcall test (caar alist) key))
      (setq alist (cdr alist)))
    (let ((tail alist) tail-cdr)
      (while (setq tail-cdr (cdr tail))
	(if (and (consp (car tail-cdr))
		 (funcall test (caar tail-cdr) key))
            (setcdr tail (cdr tail-cdr))
          (setq tail tail-cdr))))
    alist))

(provide 'org-compat-emacs27)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-compat-emacs27.el ends here
