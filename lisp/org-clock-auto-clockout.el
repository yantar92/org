;;; org-clock-auto-clockout.el --- Clocking out on idle -*- lexical-binding: t; -*-

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

;; This file implements automatic clocking out on idle.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-clock-core)

(defcustom org-clock-auto-clockout-timer nil
  "Timer for auto clocking out when Emacs is idle.
When set to a number, auto clock out the currently clocked in
task after this number of seconds of idle time.

This is only effective when `org-clock-auto-clockout-insinuate'
is added to the user configuration."
  :group 'org-clock
  :package-version '(Org . "9.4")
  :type '(choice
	  (integer :tag "Clock out after Emacs is idle for X seconds")
	  (const :tag "Never auto clock out" nil)))

;;;###autoload
(defun org-clock-auto-clockout-insinuate ()
  "Set up hook for auto clocking out when Emacs is idle.
See `org-clock-auto-clockout-timer'.

This function is meant to be added to the user configuration."
  (add-hook 'org-clock-in-hook #'org-clock-auto-clockout t))

;;;###autoload
(defun org-clock-toggle-auto-clockout ()
  "Toggle automatic clockout when idle.
See `org-clock-auto-clockout-timer'."
  (interactive)
  (if (memq 'org-clock-auto-clockout org-clock-in-hook)
      (progn (remove-hook 'org-clock-in-hook #'org-clock-auto-clockout)
	     (message "Auto clock-out after idle time turned off"))
    (add-hook 'org-clock-in-hook #'org-clock-auto-clockout t)
    (message "Auto clock-out after idle time turned on")))

(defvar org-clock--auto-clockout-timer-obj nil
  "Timer object holding the existing clockout timer.")
(defun org-clock--auto-clockout-maybe ()
  "Clock out the currently clocked in task when idle.
See `org-clock-auto-clockout-timer' to set the idle time span.

This function is to be called by a timer."
  (when (and (numberp org-clock-auto-clockout-timer)
	     org-clock-current-task)
    (let ((user-idle-seconds (org-user-idle-seconds)))
      (cond
       ;; Already idle.  Clock out.
       ((>= user-idle-seconds org-clock-auto-clockout-timer)
        (setq org-clock--auto-clockout-timer-obj nil)
        (org-clock-out))
       ;; Emacs is idle but system is not.  Retry assuming that system will remain idle.
       ((>= (org-emacs-idle-seconds) org-clock-auto-clockout-timer)
        (setq org-clock--auto-clockout-timer-obj
              (run-with-timer
               (- org-clock-auto-clockout-timer user-idle-seconds)
               nil #'org-clock--auto-clockout-maybe)))
       ;; Emacs is not idle.  Check again next time we are idle.
       (t
        (setq org-clock--auto-clockout-timer-obj
              (run-with-idle-timer
               org-clock-auto-clockout-timer nil #'org-clock--auto-clockout-maybe)))))))

(defun org-clock-auto-clockout ()
  "Clock out the currently clocked in task if Emacs is idle.
See `org-clock-auto-clockout-timer' to set the idle time span.

This is only effective when `org-clock-auto-clockout-insinuate'
is present in the user configuration."
  (when (and (numberp org-clock-auto-clockout-timer)
	     org-clock-current-task
             (not (timerp org-clock--auto-clockout-timer-obj)))
    (setq org-clock--auto-clockout-timer-obj
          (run-with-idle-timer
           org-clock-auto-clockout-timer nil #'org-clock--auto-clockout-maybe))))


(provide 'org-clock-auto-clockout)

;;; org-clock-auto-clockout.el ends here
