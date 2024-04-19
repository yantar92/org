;;; org-imenu.el --- Org mode imenu integration                      -*- lexical-binding: t; -*-

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

;; This library implements Org mode integration with imenu.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-fold)
(require 'org-element)
(require 'ol)
(require 'org-property)

(defgroup org-imenu-and-speedbar nil
  "Options concerning imenu and speedbar in Org mode."
  :tag "Org Imenu and Speedbar"
  :group 'org-structure)

(defcustom org-imenu-depth 2
  "The maximum level for Imenu access to Org headlines.
This also applied for speedbar access."
  :type 'integer)

(defvar-local org-imenu-markers nil
  "All markers currently used by Imenu.")

(defun org-imenu-get-tree ()
  "Produce the index for Imenu."
  (dolist (x org-imenu-markers) (move-marker x nil))
  (setq org-imenu-markers nil)
  (org-with-wide-buffer
   (goto-char (point-max))
   (let* ((re (concat "^" (org-get-limited-outline-regexp)))
	  (subs (make-vector (1+ org-imenu-depth) nil))
	  (last-level 0))
     (while (re-search-backward re nil t)
       (let ((level (org-reduced-level (funcall outline-level)))
	     (headline (org-no-properties
			(org-link-display-format (org-get-heading t t t t)))))
	 (when (and (<= level org-imenu-depth) (org-string-nw-p headline))
	   (let* ((m (point-marker))
		  (item (propertize headline 'org-imenu-marker m 'org-imenu t)))
	     (push m org-imenu-markers)
             (push (cons item m) (aref subs level))
             (unless (>= level last-level)
	       (push (cons item
			   (cl-mapcan #'identity (cl-subseq subs (1+ level))))
		     (aref subs level))
	       (cl-loop for i from (1+ level) to org-imenu-depth
			do (aset subs i nil)))
	     (setq last-level level)))))
     (aref subs 1))))

(defun org-imenu-reveal ()
  "When in Org mode, reveal point after imenu jump."
  (when (derived-mode-p 'org-mode)
    (org-fold-show-context 'org-goto)))

(defun org-setup-imenu ()
  "Setup imenu integration."
  (setq imenu-create-index-function 'org-imenu-get-tree)
  (add-hook 'imenu-after-jump-hook #'org-imenu-reveal nil 'local))

(provide 'org-imenu)

;;; org-imenu.el ends here
