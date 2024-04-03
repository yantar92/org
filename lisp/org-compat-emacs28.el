;;; org-compat-emacs28.el --- Compatibility Code for Older Emacsen -*- lexical-binding: t; -*-

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
;;; Emacs < 28 compatibility

;;; Code:

(if (= 2 (cdr (subr-arity (symbol-function 'get-buffer-create))))
    ;; Emacs >27.
    (defalias 'org-get-buffer-create #'get-buffer-create)
  (defun org-get-buffer-create (buffer-or-name &optional _)
    "Call `get-buffer-create' with BUFFER-OR-NAME argument.
Ignore optional argument."
    (get-buffer-create buffer-or-name)))

(declare-function seq-empty-p "seq")
(if (fboundp 'file-name-concat)
    (defalias 'org-file-name-concat #'file-name-concat)
  (defun org-file-name-concat (directory &rest components)
    "Append COMPONENTS to DIRECTORY and return the resulting string.

Elements in COMPONENTS must be a string or nil.
DIRECTORY or the non-final elements in COMPONENTS may or may not end
with a slash -- if they don't end with a slash, a slash will be
inserted before concatenating."
    (require 'seq)
    (save-match-data
      (mapconcat
       #'identity
       (delq nil
             (mapcar
              (lambda (str)
                (when (and str (not (seq-empty-p str))
                           (string-match "\\(.+\\)/?" str))
                  (match-string 1 str)))
              (cons directory components)))
       "/"))))

(if (fboundp 'directory-empty-p)
    (defalias 'org-directory-empty-p #'directory-empty-p)
  (defun org-directory-empty-p (dir)
    "Return t if DIR names an existing directory containing no other files."
    (and (file-directory-p dir)
         (null (directory-files dir nil directory-files-no-dot-files-regexp t)))))

(eval-when-compile (require 'subr-x))  ; Emacs < 28
(if (fboundp 'string-clean-whitespace)
    (defalias 'org-string-clean-whitespace #'string-clean-whitespace)
  ;; From Emacs subr-x.el.
  (defun org-string-clean-whitespace (string)
    "Clean up whitespace in STRING.
All sequences of whitespaces in STRING are collapsed into a
single space character, and leading/trailing whitespace is
removed."
    (let ((blank "[[:blank:]\r\n]+"))
      (string-trim (replace-regexp-in-string blank " " string t t)
                   blank blank))))

(if (fboundp 'format-prompt)
    (defalias 'org-format-prompt #'format-prompt)
  ;; From Emacs minibuffer.el, inlining
  ;; `minibuffer-default-prompt-format' value and replacing `length<'
  ;; (both new in Emacs 28.1).
  (defun org-format-prompt (prompt default &rest format-args)
    "Compatibility substitute for `format-prompt'."
    (concat
     (if (null format-args)
         prompt
       (apply #'format prompt format-args))
     (and default
          (or (not (stringp default))
              (> (length default) 0))
          (format " (default %s)"
                  (if (consp default)
                      (car default)
                    default)))
     ": ")))

(if (fboundp 'list-of-strings-p)
    (defalias 'org-list-of-strings-p #'list-of-strings-p)
  ;; From Emacs subr.el.
;;;###autoload
  (defun org-list-of-strings-p (object)
    "Return t if OBJECT is nil or a list of strings."
    (declare (pure t) (side-effect-free error-free))
    (while (and (consp object) (stringp (car object)))
      (setq object (cdr object)))
    (null object)))

(provide 'org-compat-emacs28)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-compat-emacs28.el ends here
