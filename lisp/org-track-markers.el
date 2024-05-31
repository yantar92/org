;;; org-track-markers.el --- Tracking markers when killing/yanking text -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
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

;; This library implements API to preserve markers when killing and
;; then yanking regions of text.

;;; Code:

(require 'org-macs)
(org-assert-version)

(defvar org-track-markers-functions '()
  "List of functions and variables defining markers to be tracked.
When list element is a function, it is called with two arguments: BEG
and END of region to be killed and should return a list of markers to
be preserved.  When variable symbol, its value should contain the
marker to be tracked or a list of such markers.")

(defvar org-markers-to-move nil
  "Markers that should be moved with a cut-and-paste operation.
Those markers are stored together with their positions relative to
the start of the region.")

(defun org-track-markers-register (&rest fun-or-sym)
  "Register list of FUN-OR-SYM in `org-track-markers-functions'."
  (dolist (elem fun-or-sym)
    (if (or (functionp elem) (symbolp elem))
        (cl-pushnew elem org-track-markers-functions)
      (error "Not a function or symbol: %S" elem))))

(defun org-track-markers-unregister (&rest fun-or-sym)
  "Unregister list of FUN-OR-SYM from `org-track-markers-functions'."
  (dolist (elem fun-or-sym)
    (setq org-track-markers-functions (delq elem org-track-markers-functions))))

(defun org-check-and-save-marker (marker beg end)
  "Check if MARKER is between BEG and END.
If yes, remember the marker and the distance to BEG."
  (when (and (marker-buffer marker)
	     (or (equal (marker-buffer marker) (current-buffer))
                 (equal (marker-buffer marker) (buffer-base-buffer (current-buffer))))
	     (>= marker beg) (< marker end)
             (not (memq marker org-markers-to-move)))
    (push (cons marker (- marker beg)) org-markers-to-move)))

(defun org-save-markers-in-region (beg end)
  "Check markers in region for markers stored in `org-track-markers-symbols'.
If these markers are between BEG and END, record their position relative
to BEG, so that after moving the block of text, we can put the markers back
into place.
This function gets called just before an entry or tree gets cut from the
buffer.  After re-insertion, `org-reinstall-markers-in-region' must be
called immediately, to move the markers with the entries."
  (setq org-markers-to-move nil)
  (dolist (elem org-track-markers-functions)
    (pcase elem
      ((pred functionp)
       (dolist (mk (funcall elem beg end))
         (org-check-and-save-marker mk beg end)))
      ((pred symbolp)
       (let ((val (symbol-value elem)))
         (cond
          ((markerp val)
           (org-check-and-save-marker val beg end))
          ((listp val)
           (dolist (mk val)
             (org-check-and-save-marker mk beg end)))
          (t (error "Value is not a marker or list: %S" val)))))
      (_ (error "Not a function or symbol: %S" elem)))))

(defun org-reinstall-markers-in-region (beg)
  "Move all remembered markers to their position relative to BEG."
  (dolist (x org-markers-to-move)
    (move-marker (car x) (+ beg (cdr x))))
  (setq org-markers-to-move nil))

(provide 'org-track-markers)

;;; org-track-markers.el ends here
