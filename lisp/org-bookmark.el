;;; org-bookmark.el --- Org mode bookmark support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

;;; This library implements Org mode bookmark support.

;;; Code:

(require 'org-fold)

(defcustom org-bookmark-names-plist
  '(:last-capture "org-capture-last-stored"
		  :last-refile "org-refile-last-stored"
		  :last-capture-marker "org-capture-last-stored-marker")
  "Names for bookmarks automatically set by some Org commands.
This can provide strings as names for a number of bookmarks Org sets
automatically.  The following keys are currently implemented:
  :last-capture
  :last-capture-marker
  :last-refile
When a key does not show up in the property list, the corresponding bookmark
is not set."
  :group 'org-structure
  :type 'plist)

;;;###autoload
(defun org-bookmark-jump-unhide (&rest _)
  "Unhide the current position, to show the bookmark location."
  (and (derived-mode-p 'org-mode)
       (or (org-invisible-p)
	   (save-excursion (goto-char (max (point-min) (1- (point))))
			   (org-invisible-p)))
       (org-fold-show-context 'bookmark-jump)))

;;;###autoload
(defun org-goto-marker-or-bmk (marker &optional bookmark)
  "Go to MARKER, widen if necessary.  When marker is not live, try BOOKMARK."
  (if (and marker (marker-buffer marker)
	   (buffer-live-p (marker-buffer marker)))
      (progn
	(pop-to-buffer-same-window (marker-buffer marker))
	(when (or (> marker (point-max)) (< marker (point-min)))
	  (widen))
	(goto-char marker)
	(org-fold-show-context 'org-goto))
    (if bookmark
	(bookmark-jump bookmark)
      (error "Cannot find location"))))

(provide 'org-bookmark)
;;; org-bookmark.el ends here
