;;; ox-backend.el --- Export backend definition          -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>
;; Maintainer: Ihor Radchenko <yantar92 at posteo dot net>
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is part of GNU Emacs.

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

;;; Commentary:
;;
;;
;; An export backend is a structure with `org-export-backend' type
;; and `name', `parent', `transcoders', `options', `filters', `blocks'
;; and `menu' slots.
;;
;; At the lowest level, a backend is created with
;; `org-export-create-backend' function.
;;
;; A named backend can be registered with
;; `org-export-register-backend' function.  A registered backend can
;; later be referred to by its name, with `org-export-get-backend'
;; function.  Also, such a backend can become the parent of a derived
;; backend from which slot values will be inherited by default.
;; `org-export-derived-backend-p' can check if a given backend is
;; derived from a list of backend names.
;;
;; `org-export-get-all-transcoders', `org-export-get-all-options' and
;; `org-export-get-all-filters' return the full alist of transcoders,
;; options and filters, including those inherited from ancestors.
;;
;; At a higher level, `org-export-define-backend' is the standard way
;; to define an export backend.  If the new backend is similar to
;; a registered backend, `org-export-define-derived-backend' may be
;; used instead.
;;
;; Eventually `org-export-barf-if-invalid-backend' returns an error
;; when a given backend hasn't been registered yet.

;;; Code:

(require 'org-macs)
(org-assert-version)

(defvar org-export-registered-backends nil
  "List of backends currently available in the exporter.
This variable is set with `org-export-define-backend' and
`org-export-define-derived-backend' functions.")

(cl-defstruct (org-export-backend (:constructor org-export-create-backend)
				  (:copier nil))
  name parent transcoders options filters blocks menu)

;;;###autoload
(defun org-export-get-backend (name)
  "Return export backend named after NAME.
NAME is a symbol.  Return nil if no such backend is found."
  (cl-find-if (lambda (b) (and (eq name (org-export-backend-name b))))
	      org-export-registered-backends))

(defun org-export-register-backend (backend)
  "Register BACKEND as a known export backend.
BACKEND is a structure with `org-export-backend' type."
  ;; Refuse to register an unnamed backend.
  (unless (org-export-backend-name backend)
    (error "Cannot register a unnamed export backend"))
  ;; Refuse to register a backend with an unknown parent.
  (let ((parent (org-export-backend-parent backend)))
    (when (and parent (not (org-export-get-backend parent)))
      (error "Cannot use unknown \"%s\" backend as a parent" parent)))
  ;; If a backend with the same name as BACKEND is already
  ;; registered, replace it with BACKEND.  Otherwise, simply add
  ;; BACKEND to the list of registered backends.
  (let ((old (org-export-get-backend (org-export-backend-name backend))))
    (if old (setcar (memq old org-export-registered-backends) backend)
      (push backend org-export-registered-backends))))

(defun org-export-barf-if-invalid-backend (backend)
  "Signal an error if BACKEND isn't defined."
  (unless (org-export-backend-p backend)
    (error "Unknown \"%s\" backend: Aborting export" backend)))

;;;###autoload
(defun org-export-derived-backend-p (backend &rest backends)
  "Non-nil if BACKEND is derived from one of BACKENDS.
BACKEND is an export backend, as returned by, e.g.,
`org-export-create-backend', or a symbol referring to
a registered backend.  BACKENDS is constituted of symbols."
  (when (symbolp backend) (setq backend (org-export-get-backend backend)))
  (when backend
    (catch 'exit
      (while (org-export-backend-parent backend)
	(when (memq (org-export-backend-name backend) backends)
	  (throw 'exit t))
	(setq backend
	      (org-export-get-backend (org-export-backend-parent backend))))
      (memq (org-export-backend-name backend) backends))))

(defun org-export-get-all-transcoders (backend)
  "Return full translation table for BACKEND.

BACKEND is an export backend, as return by, e.g,,
`org-export-create-backend'.  Return value is an alist where
keys are element or object types, as symbols, and values are
transcoders.

Unlike to `org-export-backend-transcoders', this function
also returns transcoders inherited from parent backends,
if any."
  (when (symbolp backend) (setq backend (org-export-get-backend backend)))
  (when backend
    (let ((transcoders (org-export-backend-transcoders backend))
	  parent)
      (while (setq parent (org-export-backend-parent backend))
	(setq backend (if (symbolp parent) (org-export-get-backend parent) parent))
	(setq transcoders
	      (append transcoders (org-export-backend-transcoders backend))))
      transcoders)))

(defun org-export-get-all-options (backend)
  "Return export options for BACKEND.

BACKEND is an export backend, as return by, e.g,,
`org-export-create-backend'.  See `org-export-options-alist'
for the shape of the return value.

Unlike to `org-export-backend-options', this function also
returns options inherited from parent backends, if any.

Return nil if BACKEND is unknown."
  (when (symbolp backend) (setq backend (org-export-get-backend backend)))
  (when backend
    (let ((options (org-export-backend-options backend))
	  parent)
      (while (setq parent (org-export-backend-parent backend))
	(setq backend (if (symbolp parent) (org-export-get-backend parent) parent))
	(setq options (append options (org-export-backend-options backend))))
      options)))

(defun org-export-get-all-filters (backend)
  "Return complete list of filters for BACKEND.

BACKEND is an export backend, as return by, e.g,,
`org-export-create-backend'.  Return value is an alist where
keys are symbols and values lists of functions.

Unlike to `org-export-backend-filters', this function also
returns filters inherited from parent backends, if any."
  (when (symbolp backend) (setq backend (org-export-get-backend backend)))
  (when backend
    (let ((filters (org-export-backend-filters backend))
	  parent)
      (while (setq parent (org-export-backend-parent backend))
	(setq backend (if (symbolp parent) (org-export-get-backend parent) parent))
	(setq filters (append filters (org-export-backend-filters backend))))
      filters)))

(defun org-export-define-backend (backend transcoders &rest body)
  "Define a new backend BACKEND.

TRANSCODERS is an alist between object or element types and
functions handling them.

These functions should return a string without any trailing
space, or nil.  They must accept three arguments: the object or
element itself, its contents or nil when it isn't recursive and
the property list used as a communication channel.

Contents, when not nil, are stripped from any global indentation
\(although the relative one is preserved).  They also always end
with a single newline character.

If, for a given type, no function is found, that element or
object type will simply be ignored, along with any blank line or
white space at its end.  The same will happen if the function
returns the nil value.  If that function returns the empty
string, the type will be ignored, but the blank lines or white
spaces will be kept.

In addition to element and object types, one function can be
associated to the `template' (or `inner-template') symbol and
another one to the `plain-text' symbol.

The former returns the final transcoded string, and can be used
to add a preamble and a postamble to document's body.  It must
accept two arguments: the transcoded string and the property list
containing export options.  A function associated to `template'
will not be applied if export has option \"body-only\".
A function associated to `inner-template' is always applied.

The latter, when defined, is to be called on every text not
recognized as an element or an object.  It must accept two
arguments: the text string and the information channel.  It is an
appropriate place to protect special chars relative to the
backend.

BODY can start with pre-defined keyword arguments.  The following
keywords are understood:

  :filters-alist

    Alist between filters and function, or list of functions,
    specific to the backend.  See `org-export-filters-alist' for
    a list of all allowed filters.  Filters defined here
    shouldn't make a backend test, as it may prevent backends
    derived from this one to behave properly.

  :menu-entry

    Menu entry for the export dispatcher.  It should be a list
    like:

      (KEY DESCRIPTION-OR-ORDINAL ACTION-OR-MENU)

    where :

      KEY is a free character selecting the backend.

      DESCRIPTION-OR-ORDINAL is either a string or a number.

      If it is a string, is will be used to name the backend in
      its menu entry.  If it is a number, the following menu will
      be displayed as a sub-menu of the backend with the same
      KEY.  Also, the number will be used to determine in which
      order such sub-menus will appear (lowest first).

      ACTION-OR-MENU is either a function or an alist.

      If it is an action, it will be called with four
      arguments (booleans): ASYNC, SUBTREEP, VISIBLE-ONLY and
      BODY-ONLY.  See `org-export-as' for further explanations on
      some of them.

      If it is an alist, associations should follow the
      pattern:

        (KEY DESCRIPTION ACTION)

      where KEY, DESCRIPTION and ACTION are described above.

    Valid values include:

      (?m \"My Special Backend\" my-special-export-function)

      or

       (?l \"Export to LaTeX\"
           ((?p \"As PDF file\" org-latex-export-to-pdf)
            (?o \"As PDF file and open\"
                (lambda (a s v b)
                  (if a (org-latex-export-to-pdf t s v b)
                    (org-open-file
                     (org-latex-export-to-pdf nil s v b)))))))

      or the following, which will be added to the previous
      sub-menu,

       (?l 1
          ((?B \"As TEX buffer (Beamer)\" org-beamer-export-as-latex)
           (?P \"As PDF file (Beamer)\" org-beamer-export-to-pdf)))

  :options-alist

    Alist between backend specific properties introduced in
    communication channel and how their value are acquired.  See
    `org-export-options-alist' for more information about
    structure of the values."
  (declare (indent 1))
  (let (filters menu-entry options)
    (while (keywordp (car body))
      (let ((keyword (pop body)))
	(pcase keyword
	  (:filters-alist (setq filters (pop body)))
	  (:menu-entry (setq menu-entry (pop body)))
	  (:options-alist (setq options (pop body)))
	  (_ (error "Unknown keyword: %s" keyword)))))
    (org-export-register-backend
     (org-export-create-backend :name backend
				:transcoders transcoders
				:options options
				:filters filters
				:menu menu-entry))))

(defun org-export-define-derived-backend (child parent &rest body)
  "Create a new backend as a variant of an existing one.

CHILD is the name of the derived backend.  PARENT is the name of
the parent backend.

BODY can start with pre-defined keyword arguments.  The following
keywords are understood:

  :filters-alist

    Alist of filters that will overwrite or complete filters
    defined in PARENT backend.  See `org-export-filters-alist'
    for a list of allowed filters.

  :menu-entry

    Menu entry for the export dispatcher.  See
    `org-export-define-backend' for more information about the
    expected value.

  :options-alist

    Alist of backend specific properties that will overwrite or
    complete those defined in PARENT backend.  Refer to
    `org-export-options-alist' for more information about
    structure of the values.

  :translate-alist

    Alist of element and object types and transcoders that will
    overwrite or complete transcode table from PARENT backend.
    Refer to `org-export-define-backend' for detailed information
    about transcoders.

As an example, here is how one could define \"my-latex\" backend
as a variant of `latex' backend with a custom template function:

  (org-export-define-derived-backend \\='my-latex \\='latex
     :translate-alist \\='((template . my-latex-template-fun)))

The backend could then be called with, for example:

  (org-export-to-buffer \\='my-latex \"*Test my-latex*\")"
  (declare (indent 2))
  (let (filters menu-entry options transcoders)
    (while (keywordp (car body))
      (let ((keyword (pop body)))
	(pcase keyword
	  (:filters-alist (setq filters (pop body)))
	  (:menu-entry (setq menu-entry (pop body)))
	  (:options-alist (setq options (pop body)))
	  (:translate-alist (setq transcoders (pop body)))
	  (_ (error "Unknown keyword: %s" keyword)))))
    (org-export-register-backend
     (org-export-create-backend :name child
				:parent parent
				:transcoders transcoders
				:options options
				:filters filters
				:menu menu-entry))))

(provide 'ox-backend)

;;; ox-backend.el ends here
