;;; org-agenda-highlight.el --- Highlight lines in Org agenda  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, text
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

;; This file provides an API to highlight entries in agenda.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-clock-common)

(defun org-agenda-mark-clocking-task ()
  "Mark the current clock entry in the agenda if it is present."
  ;; We need to widen when `org-agenda-finalize' is called from
  ;; `org-agenda-change-all-lines' (e.g. in `org-agenda-clock-in').
  (when (bound-and-true-p org-clock-current-task)
    (save-restriction
      (widen)
      (org-agenda-unmark-clocking-task)
      (when (marker-buffer org-clock-hd-marker)
	(save-excursion
	  (goto-char (point-min))
	  (let (s ov)
	    (while (setq s (next-single-property-change (point) 'org-hd-marker))
	      (goto-char s)
	      (when (equal (org-get-at-bol 'org-hd-marker)
			   org-clock-hd-marker)
                (setq ov (make-overlay (line-beginning-position)
                                       (1+ (line-end-position))))
		(overlay-put ov 'type 'org-agenda-clocking)
		(overlay-put ov 'face 'org-agenda-clocking)
		(overlay-put ov 'help-echo
			     "The clock is running in this item")))))))))

(defun org-agenda-unmark-clocking-task ()
  "Unmark the current clocking task."
  (mapc (lambda (o)
	  (when (eq (overlay-get o 'type) 'org-agenda-clocking)
	    (delete-overlay o)))
	(overlays-in (point-min) (point-max))))

(defun org-agenda-show-new-time (marker stamp &optional prefix)
  "Show new date stamp via text properties."
  ;; We use text properties to make this undoable
  (let ((inhibit-read-only t))
    (setq stamp (concat prefix " => " stamp " "))
    (save-excursion
      (goto-char (point-max))
      (while (not (bobp))
	(when (equal marker (org-get-at-bol 'org-marker))
          (remove-text-properties (line-beginning-position)
				  (line-end-position)
				  '(display nil))
	  (org-move-to-column
           (max
            1 ;; narrow buffer and wide timestamp
            (- (window-max-chars-per-line)
               (length stamp)))
           t)
          (add-text-properties
           (1- (point)) (line-end-position)
	   (list 'display (org-add-props stamp nil
			    'face '(secondary-selection default))))
	  (forward-line 0))
	(forward-line -1)))))

;;; Highlight lines

;; Initialize the highlight
(defvar org-hl (make-overlay 1 1))
(overlay-put org-hl 'face 'highlight)

(defun org-highlight (begin end &optional buffer)
  "Highlight a region with overlay."
  (move-overlay org-hl begin end (or buffer (current-buffer))))

(defun org-unhighlight ()
  "Detach overlay INDEX."
  (delete-overlay org-hl))

(defun org-unhighlight-once ()
  "Remove the highlight from its position, and this function from the hook."
  (remove-hook 'pre-command-hook #'org-unhighlight-once)
  (org-unhighlight))

(provide 'org-agenda-highlight)

;;; org-agenda-highlight.el ends here
