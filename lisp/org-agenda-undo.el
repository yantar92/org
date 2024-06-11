;;; org-agenda-undo.el --- Tracking remote edits in agenda  -*- lexical-binding: t; -*-

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

;; This file implements undo functionality for Org agenda.

;;; Code:

(require 'org-macs)
(org-assert-version)

(defvar org-agenda-undo-list nil
  "List of undoable operations in the agenda since last refresh.")
(defvar org-agenda-pending-undo-list nil
  "In a series of undo commands, this is the list of remaining undo items.")

;;; Agenda undo

(defvar org-agenda-allow-remote-undo t
  "Non-nil means allow remote undo from the agenda buffer.")
(defvar org-agenda-undo-has-started-in nil
  "Buffers that have already seen `undo-start' in the current undo sequence.")

(defmacro org-with-remote-undo (buffer &rest body)
  "Execute BODY while recording undo information in current buffer and BUFFER.
This function is only useful when called from Agenda buffer."
  (declare (debug (form body)) (indent 1))
  (org-with-gensyms (cline cmd buf1 buf2 undo1 undo2 c1 c2)
    `(let ((,cline (org-current-line))
	   (,cmd this-command)
	   (,buf1 (current-buffer))
	   (,buf2 ,buffer)
	   (,undo1 buffer-undo-list)
	   (,undo2 (with-current-buffer ,buffer buffer-undo-list))
	   ,c1 ,c2)
       ,@body
       (when org-agenda-allow-remote-undo
	 (setq ,c1 (org-verify-change-for-undo
		    ,undo1 (with-current-buffer ,buf1 buffer-undo-list))
	       ,c2 (org-verify-change-for-undo
		    ,undo2 (with-current-buffer ,buf2 buffer-undo-list)))
	 (when (or ,c1 ,c2)
	   ;; make sure there are undo boundaries
	   (and ,c1 (with-current-buffer ,buf1 (undo-boundary)))
	   (and ,c2 (with-current-buffer ,buf2 (undo-boundary)))
	   ;; remember which buffer to undo
	   (push (list ,cmd ,cline ,buf1 ,c1 ,buf2 ,c2)
		 org-agenda-undo-list))))))

(defun org-agenda-undo ()
  "Undo a remote editing step in the agenda.
This undoes changes both in the agenda buffer and in the remote buffer
that have been changed along."
  (interactive)
  (or org-agenda-allow-remote-undo
      (user-error "Check the variable `org-agenda-allow-remote-undo' to activate remote undo"))
  (when (not (eq this-command last-command))
    (setq org-agenda-undo-has-started-in nil
	  org-agenda-pending-undo-list org-agenda-undo-list))
  (when (not org-agenda-pending-undo-list)
    (user-error "No further undo information"))
  (let* ((entry (pop org-agenda-pending-undo-list))
	 buf line cmd rembuf)
    (setq cmd (pop entry) line (pop entry))
    (setq rembuf (nth 2 entry))
    (org-with-remote-undo rembuf
      (while (bufferp (setq buf (pop entry)))
	(when (pop entry)
	  (with-current-buffer buf
	    (let (;; (last-undo-buffer buf)
                  (inhibit-read-only t))
	      (unless (memq buf org-agenda-undo-has-started-in)
		(push buf org-agenda-undo-has-started-in)
		(make-local-variable 'pending-undo-list)
		(undo-start))
	      (while (and pending-undo-list
			  (listp pending-undo-list)
			  (not (car pending-undo-list)))
		(pop pending-undo-list))
	      (undo-more 1))))))
    (org-goto-line line)
    (message "`%s' undone (buffer %s)" cmd (buffer-name rembuf))))

(defun org-verify-change-for-undo (l1 l2)
  "Verify that a real change occurred between the undo lists L1 and L2."
  (while (and l1 (listp l1) (null (car l1))) (pop l1))
  (while (and l2 (listp l2) (null (car l2))) (pop l2))
  (not (eq l1 l2)))

(provide 'org-agenda-undo)

;;; org-agenda-undo.el ends here
