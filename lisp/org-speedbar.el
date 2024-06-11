;;; org-speedbar.el --- Org mode speedbar integration                      -*- lexical-binding: t; -*-

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

;; This library implements Org mode integration with Emacs speedbar.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-fold)
(require 'speedbar)
(require 'org-agenda-files)

(defvar org-speedbar-restriction-lock-overlay (make-overlay 1 1)
  "Overlay marking the agenda restriction line in speedbar.")
(overlay-put org-speedbar-restriction-lock-overlay
	     'face 'org-agenda-restriction-lock)
(overlay-put org-speedbar-restriction-lock-overlay
	     'help-echo "Agendas are currently limited to this item.")
(delete-overlay org-speedbar-restriction-lock-overlay)

(declare-function org-agenda-maybe-redo "org-agenda-mode" ())
(defun org-speedbar-set-agenda-restriction ()
  "Restrict future agenda commands to the location at point in speedbar.
If there is already a restriction lock at the location, remove it.

To get rid of the restriction, use `\\[org-agenda-remove-restriction-lock]'."
  (interactive)
  (require 'org-agenda-mode)
  (let (p m tp np dir txt)
    (cond
     ((setq p (text-property-any (line-beginning-position) (line-end-position)
				 'org-imenu t))
      (setq m (get-text-property p 'org-imenu-marker))
      (with-current-buffer (marker-buffer m)
	(goto-char m)
	(if (and org-agenda-overriding-restriction
		 (member org-agenda-restriction-lock-overlay
			 (overlays-at (point))))
	    (org-agenda-remove-restriction-lock 'noupdate)
	  (org-agenda-set-restriction-lock 'subtree))))
     ((setq p (text-property-any (line-beginning-position) (line-end-position)
				 'speedbar-function 'speedbar-find-file))
      (setq tp (previous-single-property-change
		(1+ p) 'speedbar-function)
	    np (next-single-property-change
		tp 'speedbar-function)
	    dir (speedbar-line-directory)
	    txt (buffer-substring-no-properties (or tp (point-min))
						(or np (point-max))))
      (with-current-buffer (find-file-noselect
			    (let ((default-directory dir))
			      (expand-file-name txt)))
	(unless (derived-mode-p 'org-mode)
	  (user-error "Cannot restrict to non-Org mode file"))
	(org-agenda-set-restriction-lock 'file)))
     (t (user-error "Don't know how to restrict Org mode agenda")))
    (move-overlay org-speedbar-restriction-lock-overlay
                  (line-beginning-position) (line-end-position))
    (setq current-prefix-arg nil)
    (org-agenda-maybe-redo)))

(defun org-speedbar-reveal ()
  "When in Org mode, reveal heading after visiting speedbar tag."
  (and (derived-mode-p 'org-mode) (org-fold-show-context 'org-goto)))

(defun org-speedbar-setup ()
  "Setup Org mode speedbar integration."
  (speedbar-add-supported-extension ".org")
  (define-key speedbar-file-key-map "<" 'org-speedbar-set-agenda-restriction)
  (define-key speedbar-file-key-map "\C-c\C-x<" 'org-speedbar-set-agenda-restriction)
  (define-key speedbar-file-key-map ">" 'org-agenda-remove-restriction-lock)
  (define-key speedbar-file-key-map "\C-c\C-x>" 'org-agenda-remove-restriction-lock)
  (add-hook 'speedbar-visiting-tag-hook #'org-speedbar-reveal))

(eval-after-load 'speedbar '(org-speedbar-setup))

(provide 'org-speedbar)

;;; org-speedbar.el ends here
