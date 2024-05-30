;;; org-clock-persist.el --- Saving and restoring clocking state -*- lexical-binding: t; -*-

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

;; This file implements saving and restoring clocking state between
;; Emacs sessions.

;;; Code:

(require 'org-clock-core)

;;;###autoload
(defun org-clock-persistence-insinuate ()
  "Set up hooks for clock persistence."
  (add-hook 'org-mode-hook 'org-clock-load)
  (add-hook 'kill-emacs-hook 'org-clock-save))

(defcustom org-clock-persist nil
  "When non-nil, save the running clock when Emacs is closed.
The clock is resumed when Emacs restarts.
When this is t, both the running clock, and the entire clock
history are saved.  When this is the symbol `clock', only the
running clock is saved.  When this is the symbol `history', only
the clock history is saved.

When Emacs restarts with saved clock information, the file containing
the running clock as well as all files mentioned in the clock history
will be visited.

All this depends on running `org-clock-persistence-insinuate' in your
Emacs initialization file."
  :group 'org-clock
  :type '(choice
	  (const :tag "Just the running clock" clock)
	  (const :tag "Just the history" history)
	  (const :tag "Clock and history" t)
	  (const :tag "No persistence" nil)))

(defcustom org-clock-persist-file (locate-user-emacs-file "org-clock-save.el")
  "File to save clock data to."
  :group 'org-clock
  :type 'string)

(defcustom org-clock-persist-query-save nil
  "When non-nil, ask before saving the current clock on exit."
  :group 'org-clock
  :type 'boolean)

(defcustom org-clock-persist-query-resume t
  "When non-nil, ask before resuming any stored clock during load."
  :group 'org-clock
  :type 'boolean)

(defvar org-clock-loaded nil
  "Was the clock file loaded?")

(defvar org-clock-stored-history nil
  "Clock history, populated by `org-clock-load'.")
(defvar org-clock-stored-resume-clock nil
  "Clock to resume, saved by `org-clock-load'.")

(defun org-clock-save ()
  "Persist various clock-related data to disk.
The details of what will be saved are regulated by the variable
`org-clock-persist'."
  (when (and org-clock-persist
             (or org-clock-loaded
		 org-clock-has-been-used
		 (not (file-exists-p org-clock-persist-file))))
    (with-temp-file org-clock-persist-file
      (insert (format ";; %s - %s at %s\n"
		      (file-name-nondirectory org-clock-persist-file)
		      (system-name)
		      (format-time-string (org-time-stamp-format t))))
      ;; Store clock to be resumed.
      (when (and (memq org-clock-persist '(t clock))
		 (let ((b (org-base-buffer (org-clocking-buffer))))
		   (and (buffer-live-p b)
			(buffer-file-name b)
			(or (not org-clock-persist-query-save)
                            (y-or-n-p (format "Save current clock (%s)?"
					      org-clock-heading))))))
	(insert
	 (format "(setq org-clock-stored-resume-clock '(%S . %d))\n"
		 (buffer-file-name (org-base-buffer (org-clocking-buffer)))
		 (marker-position org-clock-marker))))
      ;; Store clocked task history.  Tasks are stored reversed to
      ;; make reading simpler.
      (when (and (memq org-clock-persist '(t history))
		 org-clock-history)
	(insert
	 (format "(setq org-clock-stored-history '(%s))\n"
		 (mapconcat
		  (lambda (m)
		    (let ((b (org-base-buffer (marker-buffer m))))
		      (when (and (buffer-live-p b)
				 (buffer-file-name b))
			(format "(%S . %d)"
				(buffer-file-name b)
				(marker-position m)))))
		  (reverse org-clock-history)
		  " ")))))))

(defun org-clock-load ()
  "Load clock-related data from disk, maybe resuming a stored clock."
  (when (and org-clock-persist (not org-clock-loaded))
    (if (not (file-readable-p org-clock-persist-file))
	(message "Not restoring clock data; %S not found" org-clock-persist-file)
      (message "Restoring clock data")
      ;; Load history.
      (load-file org-clock-persist-file)
      (setq org-clock-loaded t)
      (pcase-dolist (`(,(and file (pred file-exists-p)) . ,position)
		     org-clock-stored-history)
	(org-clock-history-push position (find-file-noselect file)))
      ;; Resume clock.
      (pcase org-clock-stored-resume-clock
	(`(,(and file (pred file-exists-p)) . ,position)
	 (with-current-buffer (find-file-noselect file)
	   (when (or (not org-clock-persist-query-resume)
                     (y-or-n-p (format "Resume clock (%s)?"
				       (save-excursion
					 (goto-char position)
					 (org-get-heading t t)))))
	     (goto-char position)
	     (let ((org-clock-in-resume 'auto-restart)
		   (org-clock-auto-clock-resolution nil))
	       (org-clock-in)
	       (when (org-invisible-p) (org-fold-show-context))))))
	(_ nil)))))

(provide 'org-clock-persist)

;;; org-clock-persist.el ends here
