;;; org-obsolete9.6.el --- Obsolete Org mode functions and variables -*- lexical-binding: t; -*-

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

(define-obsolete-function-alias 'org-show-context 'org-fold-show-context "9.6")
(define-obsolete-function-alias 'org-show-entry 'org-fold-show-entry "9.6")
(define-obsolete-function-alias 'org-show-children 'org-fold-show-children "9.6")
(define-obsolete-function-alias 'org-string-collate-lessp 'string-collate-lessp "9.6")
(define-obsolete-function-alias 'org-decode-time 'decode-time "9.6")
(define-obsolete-function-alias 'org-format-time-string 'format-time-string "9.6")
(define-obsolete-function-alias 'org-time-add 'time-add "9.6")
(define-obsolete-function-alias 'org-time-subtract 'time-subtract "9.6")
(define-obsolete-function-alias 'org-time-since 'time-since "9.6")
(define-obsolete-function-alias 'org-time-less-p 'time-less-p "9.6")

(define-obsolete-function-alias 'org-timestamp-format
  'org-format-timestamp "Org 9.6")
(define-obsolete-variable-alias 'org-export-before-processing-hook
  'org-export-before-processing-functions "Org 9.6")
(define-obsolete-variable-alias 'org-export-before-parsing-hook
  'org-export-before-parsing-functions "Org 9.6")

(define-obsolete-variable-alias 'org-latex-listings
  'org-latex-src-block-backend "9.6")

(define-obsolete-function-alias 'org-hide-archived-subtrees
  'org-fold-hide-archived-subtrees "9.6")

(define-obsolete-function-alias 'org-flag-region
  'org-fold-region "9.6")

(define-obsolete-function-alias 'org-flag-subtree
  'org-fold-subtree "9.6")

(define-obsolete-function-alias 'org-hide-entry
  'org-fold-hide-entry "9.6")

(define-obsolete-function-alias 'org-show-subtree
  'org-fold-show-subtree "9.6")

(define-obsolete-function-alias 'org--hide-wrapper-toggle
  'org-fold--hide-wrapper-toggle "9.6")

(define-obsolete-function-alias 'org-hide-block-toggle
  'org-fold-hide-block-toggle "9.6")

(define-obsolete-function-alias 'org-hide-drawer-toggle
  'org-fold-hide-drawer-toggle "9.6")

(define-obsolete-function-alias 'org--hide-drawers
  'org-fold--hide-drawers "9.6")

(define-obsolete-function-alias 'org-hide-block-all
  'org-fold-hide-block-all "9.6")

(define-obsolete-function-alias 'org-hide-drawer-all
  'org-fold-hide-drawer-all "9.6")

(define-obsolete-function-alias 'org-show-all
  'org-fold-show-all "9.6")

(define-obsolete-function-alias 'org-set-startup-visibility
  'org-cycle-set-startup-visibility "9.6")

(define-obsolete-function-alias 'org-show-set-visibility
  'org-fold-show-set-visibility "9.6")

(define-obsolete-function-alias 'org-check-before-invisible-edit
  'org-fold-check-before-invisible-edit "9.6")

(define-obsolete-function-alias 'org-flag-above-first-heading
  'org-fold-flag-above-first-heading "9.6")

(define-obsolete-function-alias 'org-show-branches-buffer
  'org-fold-show-branches-buffer "9.6")

(define-obsolete-function-alias 'org-show-siblings
  'org-fold-show-siblings "9.6")

(define-obsolete-function-alias 'org-show-hidden-entry
  'org-fold-show-hidden-entry "9.6")

(define-obsolete-function-alias 'org-flag-heading
  'org-fold-heading "9.6")

(define-obsolete-function-alias 'org-set-startup-visibility
  'org-cycle-set-startup-visibility "9.6")

(define-obsolete-function-alias 'org-set-visibility-according-to-property
  'org-cycle-set-visibility-according-to-property "9.6")

(define-obsolete-variable-alias 'org-scroll-position-to-restore
  'org-cycle-scroll-position-to-restore "9.6")
(define-obsolete-function-alias 'org-optimize-window-after-visibility-change
  'org-cycle-optimize-window-after-visibility-change "9.6")

(define-obsolete-function-alias 'org-force-cycle-archived
  'org-cycle-force-archived "9.6")

(define-obsolete-variable-alias 'org-show-context-detail
  'org-fold-show-context-detail "9.6")

(define-obsolete-variable-alias 'org-catch-invisible-edits
  'org-fold-catch-invisible-edits "9.6")

(define-obsolete-variable-alias 'org-reveal-start-hook
  'org-fold-reveal-start-hook "9.6")

(define-obsolete-function-alias 'org-file-url-p 'org-url-p "9.6")

(define-obsolete-variable-alias 'org-plantuml-executable-args 'org-plantuml-args
  "Org 9.6")

(define-obsolete-function-alias 'org-publish-cache-ctime-of-src
  'org-publish-cache-mtime-of-src "9.6")

(define-obsolete-function-alias 'org-truely-invisible-p
  'org-truly-invisible-p "9.6"
  "Compatibility alias for legacy misspelling of `org-truly-invisible-p'.")

(define-obsolete-function-alias 'org-define-error #'define-error "9.6")
(define-obsolete-function-alias 'org-without-partial-completion 'progn "9.6")

;;;; Obsolete variables


(defconst org-latex-babel-language-alist
  '(("af" . "afrikaans")
    ("bg" . "bulgarian")
    ("ca" . "catalan")
    ("cs" . "czech")
    ("cy" . "welsh")
    ("da" . "danish")
    ("de" . "germanb")
    ("de-at" . "naustrian")
    ("de-de" . "ngerman")
    ("el" . "greek")
    ("en" . "english")
    ("en-au" . "australian")
    ("en-ca" . "canadian")
    ("en-gb" . "british")
    ("en-ie" . "irish")
    ("en-nz" . "newzealand")
    ("en-us" . "american")
    ("es" . "spanish")
    ("et" . "estonian")
    ("eu" . "basque")
    ("fi" . "finnish")
    ("fr" . "french")
    ("fr-ca" . "canadien")
    ("gl" . "galician")
    ("hr" . "croatian")
    ("hu" . "hungarian")
    ("id" . "indonesian")
    ("is" . "icelandic")
    ("it" . "italian")
    ("la" . "latin")
    ("ms" . "malay")
    ("nl" . "dutch")
    ("nb" . "norsk")
    ("nn" . "nynorsk")
    ("no" . "norsk")
    ("pl" . "polish")
    ("pt" . "portuguese")
    ("pt-br" . "brazilian")
    ("ro" . "romanian")
    ("ru" . "russian")
    ("sa" . "sanskrit")
    ("sb" . "uppersorbian")
    ("sk" . "slovak")
    ("sl" . "slovene")
    ("sq" . "albanian")
    ("sr" . "serbian")
    ("sv" . "swedish")
    ("ta" . "tamil")
    ("tr" . "turkish")
    ("uk" . "ukrainian"))
  "Alist between language code and corresponding Babel option.")

(make-obsolete-variable 'org-latex-babel-language-alist
                        "set `org-latex-language-alist' instead." "9.6")

(defconst org-latex-polyglossia-language-alist
  '(("am" "amharic")
    ("ar" "arabic")
    ("ast" "asturian")
    ("bg" "bulgarian")
    ("bn" "bengali")
    ("bo" "tibetan")
    ("br" "breton")
    ("ca" "catalan")
    ("cop" "coptic")
    ("cs" "czech")
    ("cy" "welsh")
    ("da" "danish")
    ("de" "german" "german")
    ("de-at" "german" "austrian")
    ("de-de" "german" "german")
    ("dsb" "lsorbian")
    ("dv" "divehi")
    ("el" "greek")
    ("en" "english" "usmax")
    ("en-au" "english" "australian")
    ("en-gb" "english" "uk")
    ("en-nz" "english" "newzealand")
    ("en-us" "english" "usmax")
    ("eo" "esperanto")
    ("es" "spanish")
    ("et" "estonian")
    ("eu" "basque")
    ("fa" "farsi")
    ("fi" "finnish")
    ("fr" "french")
    ("fu" "friulan")
    ("ga" "irish")
    ("gd" "scottish")
    ("gl" "galician")
    ("he" "hebrew")
    ("hi" "hindi")
    ("hr" "croatian")
    ("hsb" "usorbian")
    ("hu" "magyar")
    ("hy" "armenian")
    ("ia" "interlingua")
    ("id" "bahasai")
    ("is" "icelandic")
    ("it" "italian")
    ("kn" "kannada")
    ("la" "latin" "modern")
    ("la-classic" "latin" "classic")
    ("la-medieval" "latin" "medieval")
    ("la-modern" "latin" "modern")
    ("lo" "lao")
    ("lt" "lithuanian")
    ("lv" "latvian")
    ("ml" "malayalam")
    ("mr" "maranthi")
    ("nb" "norsk")
    ("nko" "nko")
    ("nl" "dutch")
    ("nn" "nynorsk")
    ("no" "norsk")
    ("oc" "occitan")
    ("pl" "polish")
    ("pms" "piedmontese")
    ("pt" "portuges")
    ("pt-br" "brazilian")
    ("rm" "romansh")
    ("ro" "romanian")
    ("ru" "russian")
    ("sa" "sanskrit")
    ("se" "samin")
    ("sk" "slovak")
    ("sl" "slovenian")
    ("sq" "albanian")
    ("sr" "serbian")
    ("sv" "swedish")
    ("syr" "syriac")
    ("ta" "tamil")
    ("te" "telugu")
    ("th" "thai")
    ("tk" "turkmen")
    ("tr" "turkish")
    ("uk" "ukrainian")
    ("ur" "urdu")
    ("vi" "vietnamese"))
  "Alist between language code and corresponding Polyglossia option.")


(make-obsolete-variable 'org-latex-polyglossia-language-alist
                        "set `org-latex-language-alist' instead." "9.6")


;;;; Obsolete functions and macros

;; FIXME: Unused; obsoleted; to be removed.
(defun org-let (list &rest body) ;FIXME: So many kittens are suffering here.
  (declare (indent 1) (obsolete cl-progv "2021"))
  (eval (cons 'let (cons list body))))
(make-obsolete 'org-let "to be removed" "9.6")

;; FIXME: Unused; obsoleted; to be removed.
(defun org-let2 (list1 list2 &rest body) ;FIXME: Where did our karma go?
  (declare (indent 2) (obsolete cl-progv "2021"))
  (eval (cons 'let (cons list1 (list (cons 'let (cons list2 body)))))))
(make-obsolete 'org-let2 "to be removed" "9.6")


(provide 'org-obsolete9.6)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-obsolete9.6.el ends here
