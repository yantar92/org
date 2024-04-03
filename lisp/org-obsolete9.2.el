;;; org-obsolete9.2.el --- Obsolete Org mode functions and variables -*- lexical-binding: t; -*-

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

;; This file contains obsolete Org mode code that will be removed in
;; future releases.

;;; Code:

;;;; Obsolete aliases

(define-obsolete-function-alias 'org-file-remote-p 'file-remote-p "9.2")

(define-obsolete-function-alias 'org-get-indentation
  'current-indentation "9.2")

(define-obsolete-function-alias 'org-capture-member 'org-capture-get "9.2")

(define-obsolete-function-alias 'org-remove-from-invisibility-spec
  'remove-from-invisibility-spec "9.2")

(define-obsolete-variable-alias 'org-effort-durations 'org-duration-units
  "9.2")

(define-obsolete-variable-alias 'org-agenda-overriding-columns-format
  'org-overriding-columns-format "9.2.2")

(define-obsolete-function-alias 'org-get-tags-at 'org-get-tags "9.2")

(define-obsolete-function-alias 'org-set-tags-to 'org-set-tags "9.2")

(define-obsolete-function-alias 'org-babel-strip-quotes
  'org-strip-quotes "9.2")

;;;; Obsolete variables


;;;; Obsolete functions and macros

(defmacro org-preserve-lc (&rest body)
  (declare (debug (body))
	   (obsolete "please notify Org mailing list if you use this function."
		     "9.2"))
  (org-with-gensyms (line col)
    `(let ((,line (org-current-line))
	   (,col (current-column)))
       (unwind-protect
	   (progn ,@body)
	 (org-goto-line ,line)
	 (org-move-to-column ,col)))))

(defun org-version-check (version &rest _)
  "Non-nil if VERSION is lower (older) than `emacs-version'."
  (declare (obsolete "use `version<' or `fboundp' instead."
		     "9.2"))
  (version< version emacs-version))

(defun org-show-block-all ()
  "Unfold all blocks in the current buffer."
  (interactive)
  (org-fold-show-all '(blocks)))

(make-obsolete 'org-show-block-all
	       "use `org-show-all' instead."
	       "9.2")

(defun org-get-local-tags ()
  "Get a list of tags defined in the current headline."
  (declare (obsolete "use `org-get-tags' instead." "9.2"))
  (org-get-tags nil 'local))

(defun org-get-local-tags-at (&optional pos)
  "Get a list of tags defined in the current headline."
  (declare (obsolete "use `org-get-tags' instead." "9.2"))
  (org-get-tags pos 'local))

(defun org-get-tags-string ()
  "Get the TAGS string in the current headline."
  (declare (obsolete "use `org-make-tag-string' instead." "9.2"))
  (org-make-tag-string (org-get-tags nil t)))


(defun org-align-all-tags ()
  "Align the tags in all headings."
  (declare (obsolete "use `org-align-tags' instead." "9.2"))
  (org-align-tags t))


(defmacro org-with-silent-modifications (&rest body)
  (declare (obsolete "use `with-silent-modifications' instead." "9.2")
	   (debug (body)))
  `(with-silent-modifications ,@body))


(provide 'org-obsolete9.2)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-obsolete9.2.el ends here
