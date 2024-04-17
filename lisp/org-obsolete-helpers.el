;;; org-obsolete-helpers.el --- Compatibility helpers for Older Org mode -*- lexical-binding: t; -*-

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

;; This file contains functions and commands useful to convert after
;; breaking changes in Org mode syntax and variable formats.

;;; Code:

(require 'cl-lib)
(require 'org-macs)

(defun org--check-org-structure-template-alist (checklist)
  "Check whether structure template alist CHECKLIST is set up correctly.
CHECKLIST is a symbol holding the template alist.
In particular, check if the Org 9.2 format is used as opposed to
previous format."
  (let ((elm (cl-remove-if-not
              (lambda (x) (listp (cdr x)))
	      (symbol-value checklist))))
    (when elm
      (org-display-warning
       (format "
Please update the entries of `%s'.

In Org 9.2 the format was changed from something like

    (\"s\" \"#+BEGIN_SRC ?\\n#+END_SRC\")

to something like

    (\"s\" . \"src\")

Please refer to the documentation of `org-structure-template-alist'.

The following entries must be updated:

%s"
	       checklist
	       (pp-to-string elm))))))

(provide 'org-obsolete-helpers)

;;; org-obsolete-helpers.el ends here
