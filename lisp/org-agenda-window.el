;;; org-agenda-window.el --- Org agenda window setup  -*- mode: emacs-lisp; lexical-binding: t; -*-

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

;; This file implements agenda window management.

;;; Code:

(require 'org-macs)
(org-assert-version)

(defvar org-agenda-pre-window-conf nil)

(defgroup org-agenda-windows nil
  "Options concerning the windows used by the Agenda in Org Mode."
  :tag "Org Agenda Windows"
  :group 'org-agenda)

(defcustom org-agenda-window-setup 'reorganize-frame
  "How the agenda buffer should be displayed.
Possible values for this option are:

current-window    Show agenda in the current window, keeping all other windows.
other-window      Use `switch-to-buffer-other-window' to display agenda.
only-window       Show agenda, deleting all other windows.
reorganize-frame  Show only two windows on the current frame, the current
                  window and the agenda.
other-frame       Use `switch-to-buffer-other-frame' to display agenda.
                  Also, when exiting the agenda, kill that frame.
other-tab         Use `switch-to-buffer-other-tab' to display the
                  agenda, making use of the `tab-bar-mode' introduced
                  in Emacs version 27.1.  Also, kill that tab when
                  exiting the agenda view.

See also the variable `org-agenda-restore-windows-after-quit'."
  :group 'org-agenda-windows
  :type '(choice
	  (const current-window)
	  (const other-frame)
	  (const other-tab)
	  (const other-window)
	  (const only-window)
	  (const reorganize-frame))
  :package-version '(Org . "9.4"))

(defcustom org-agenda-window-frame-fractions '(0.5 . 0.75)
  "The min and max height of the agenda window as a fraction of frame height.
The value of the variable is a cons cell with two numbers between 0 and 1.
It only matters if `org-agenda-window-setup' is `reorganize-frame'."
  :group 'org-agenda-windows
  :type '(cons (number :tag "Minimum") (number :tag "Maximum")))

(defcustom org-agenda-restore-windows-after-quit nil
  "Non-nil means restore window configuration upon exiting agenda.
Before the window configuration is changed for displaying the
agenda, the current status is recorded.  When the agenda is
exited with `q' or `x' and this option is set, the old state is
restored.  If `org-agenda-window-setup' is `other-frame' or
`other-tab', the value of this option will be ignored."
  :group 'org-agenda-windows
  :type 'boolean)

(defun org-agenda-fit-window-to-buffer ()
  "Fit the window to the buffer size."
  (and (memq org-agenda-window-setup '(reorganize-frame))
       (fboundp 'fit-window-to-buffer)
       (if (and (= (cdr org-agenda-window-frame-fractions) 1.0)
		(= (car org-agenda-window-frame-fractions) 1.0))
           (display-buffer (current-buffer) '(org-display-buffer-full-frame))
	 (org-fit-window-to-buffer
	  nil
	  (floor (* (frame-height) (cdr org-agenda-window-frame-fractions)))
	  (floor (* (frame-height) (car org-agenda-window-frame-fractions)))))))

(defun org-agenda--prepare-window-1 (abuf)
  "Setup agenda buffer in the window.
ABUF is the buffer for the agenda window."
  (let* ((awin (get-buffer-window abuf)) wconf)
    (cond
     ((equal (current-buffer) abuf) nil)
     (awin (select-window awin))
     ((not (setq wconf (current-window-configuration))))
     ((eq org-agenda-window-setup 'current-window)
      (pop-to-buffer-same-window abuf))
     ((eq org-agenda-window-setup 'other-window)
      (switch-to-buffer-other-window abuf))
     ((eq org-agenda-window-setup 'other-frame)
      (switch-to-buffer-other-frame abuf))
     ((eq org-agenda-window-setup 'other-tab)
      (if (fboundp 'switch-to-buffer-other-tab)
	  (switch-to-buffer-other-tab abuf)
	(user-error "Your version of Emacs does not have tab bar support")))
     ((eq org-agenda-window-setup 'only-window)
      (pop-to-buffer abuf '(org-display-buffer-full-frame)))
     ((eq org-agenda-window-setup 'reorganize-frame)
      (pop-to-buffer abuf '(org-display-buffer-split))))
    ;; Additional test in case agenda is invoked from within agenda
    ;; buffer via elisp link.
    (unless (equal (current-buffer) abuf)
      (pop-to-buffer-same-window abuf))
    (setq org-agenda-pre-window-conf
	  (or wconf org-agenda-pre-window-conf))))

(provide 'org-agenda-window)

;;; org-agenda-window.el ends here
