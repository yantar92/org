;;; org-obsolete9.3.el --- Obsolete Org mode functions and variables -*- lexical-binding: t; -*-

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

(define-obsolete-function-alias 'org-toggle-latex-fragment 'org-latex-preview
  "9.3")

(define-obsolete-function-alias 'org-remove-latex-fragment-image-overlays
  'org-clear-latex-preview "9.3")

(define-obsolete-variable-alias 'org-attach-directory
  'org-attach-id-dir "9.3")

(define-obsolete-variable-alias 'org-doi-server-url
  'org-link-doi-server-url "9.3")

(define-obsolete-variable-alias 'org-email-link-description-format
  'org-link-email-description-format "9.3")

(define-obsolete-variable-alias 'org-make-link-description-function
  'org-link-make-description-function "9.3")

(define-obsolete-variable-alias 'org-from-is-user-regexp
  'org-link-from-user-regexp "9.3")

(define-obsolete-variable-alias 'org-descriptive-links
  'org-link-descriptive "9.3")

(define-obsolete-variable-alias 'org-context-in-file-links
  'org-link-context-for-files "9.3")

(define-obsolete-variable-alias 'org-keep-stored-link-after-insertion
  'org-link-keep-stored-after-insertion "9.3")

(define-obsolete-variable-alias 'org-display-internal-link-with-indirect-buffer
  'org-link-use-indirect-buffer-for-internals "9.3")

(define-obsolete-variable-alias 'org-confirm-shell-link-function
  'org-link-shell-confirm-function "9.3")

(define-obsolete-variable-alias 'org-confirm-shell-link-not-regexp
  'org-link-shell-skip-confirm-regexp "9.3")

(define-obsolete-variable-alias 'org-confirm-elisp-link-function
  'org-link-elisp-confirm-function "9.3")

(define-obsolete-variable-alias 'org-confirm-elisp-link-not-regexp
  'org-link-elisp-skip-confirm-regexp "9.3")

(define-obsolete-function-alias 'org-file-complete-link
  'org-link-complete-file "9.3")

(define-obsolete-function-alias 'org-email-link-description
  'org-link-email-description "9.3")

(define-obsolete-function-alias 'org-make-link-string
  'org-link-make-string "9.3")

(define-obsolete-function-alias 'org-store-link-props
  'org-link-store-props "9.3")

(define-obsolete-function-alias 'org-add-link-props
  'org-link-add-props "9.3")

(define-obsolete-function-alias 'org-make-org-heading-search-string
  'org-link-heading-search-string "9.3")

(define-obsolete-function-alias 'org-make-link-regexps
  'org-link-make-regexps "9.3")

(define-obsolete-function-alias 'org-property-global-value
  'org-property-global-or-keyword-value "9.3")

(make-obsolete-variable 'org-file-properties 'org-keyword-properties "9.3")

(define-obsolete-variable-alias 'org-angle-link-re
  'org-link-angle-re "9.3")

(define-obsolete-variable-alias 'org-plain-link-re
  'org-link-plain-re "9.3")

(define-obsolete-variable-alias 'org-bracket-link-regexp
  'org-link-bracket-re "9.3")

(define-obsolete-variable-alias 'org-bracket-link-analytic-regexp
  'org-link-bracket-re "9.3")

(define-obsolete-variable-alias 'org-any-link-re
  'org-link-any-re "9.3")

(define-obsolete-function-alias 'org-open-link-from-string
  'org-link-open-from-string "9.3")

(define-obsolete-function-alias 'org-add-angle-brackets
  'org-link-add-angle-brackets "9.3")

;;;; Obsolete variables


;;;; Obsolete functions and macros

(provide 'org-obsolete9.3)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-obsolete9.3.el ends here
