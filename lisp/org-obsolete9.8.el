;;; org-obsolete9.8.el --- Obsolete Org mode functions and variables -*- lexical-binding: t; -*-

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

;;;; Groups

;; It was used only for two non-obsolete variables.
(defgroup org-keywords nil
  "Keywords in Org mode."
  :tag "Org Keywords"
  :group 'org)

;;;; Obsolete aliases

;;;; Obsolete variables

(defvar org-clock-mode-line-entry nil
  "Information for the mode line about the running clock.")
(make-obsolete 'org-clock-mode-line-entry "unused" "9.8")

;;;; Obsolete functions and macros

(declare-function org-check-and-save-marker "org-track-markers"
                  (marker beg end))
(defun org-agenda-save-markers-for-cut-and-paste (beg end)
  "Save relative positions of markers in region.
This check for agenda markers in all agenda buffers currently active."
  (require 'org-agenda-common)
  (defvar org-agenda-markers)
  (require 'org-track-markers)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'org-agenda-mode)
	(mapc (lambda (m) (org-check-and-save-marker m beg end))
	      org-agenda-markers)))))
(make-obsolete 'org-agenda-save-markers-for-cut-and-paste "no longer user" "9.8")

(defun org-clock-save-markers-for-cut-and-paste (beg end)
  "Save relative positions of markers in region BEG..END.
Save `org-clock-marker', `org-clock-hd-marker',
`org-clock-default-task', `org-clock-interrupted-task', and the
markers in `org-clock-history'."
  (require 'org-track-markers)
  (require 'org-clock-core)
  (defvar org-clock-marker)
  (defvar org-clock-hd-marker)
  (defvar org-clock-default-task)
  (defvar org-clock-interrupted-task)
  (defvar org-clock-history)
  (org-check-and-save-marker org-clock-marker beg end)
  (org-check-and-save-marker org-clock-hd-marker beg end)
  (org-check-and-save-marker org-clock-default-task beg end)
  (org-check-and-save-marker org-clock-interrupted-task beg end)
  (dolist (m org-clock-history)
    (org-check-and-save-marker m beg end)))
(make-obsolete 'org-clock-save-markers-for-cut-and-paste "no longer user" "9.8")

(defun org-remove-empty-overlays-at (pos)
  "Remove outline overlays that do not contain non-white stuff."
  (dolist (o (overlays-at pos))
    (and (eq 'outline (overlay-get o 'invisible))
	 (not (string-match-p
             "\\S-" (buffer-substring (overlay-start o)
				     (overlay-end o))))
	 (delete-overlay o))))
(make-obsolete 'org-remove-empty-overlays-at "no longer user" "9.8")

(provide 'org-obsolete9.8)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-obsolete9.8.el ends here
