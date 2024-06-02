;;; org-idle.el --- Tracking Emacs and system idle time -*- lexical-binding: t; -*-

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

;; This file provides the means to determine idle time - both in Emacs
;; and in OS.

;;; Code:

(require 'org-macs)
(org-assert-version)

(declare-function haiku-notifications-notify "haikuselect.c")
(declare-function android-notifications-notify "androidselect.c")

(defvaralias 'org-clock-x11idle-program-name 'org-x11idle-program-name)
(defcustom org-x11idle-program-name
  (if (executable-find "xprintidle")
      "xprintidle" "x11idle")
  "Name of the program which prints X11 idle time in milliseconds.

you can do \"~$ sudo apt-get install xprintidle\" if you are using
a Debian-based distribution.

Alternatively, can find x11idle.c in
https://orgmode.org/worg/code/scripts/x11idle.c"
  :group 'org-clock
  :package-version '(Org . "9.7")
  :type 'string)

(defun org-emacs-idle-seconds ()
  "Return the current Emacs idle time in seconds, or nil if not idle."
  (let ((idle-time (current-idle-time)))
    (if idle-time
	(float-time idle-time)
      0)))

(defun org-mac-idle-seconds ()
  "Return the current Mac idle time in seconds."
  (string-to-number (shell-command-to-string "ioreg -c IOHIDSystem | perl -ane 'if (/Idle/) {$idle=(pop @F)/1000000000; print $idle; last}'")))

(defvar org-x11idle-exists-p
  ;; Check that x11idle exists.  But don't do that on DOS/Windows,
  ;; since the command definitely does NOT exist there, and invoking
  ;; COMMAND.COM on MS-Windows is a bad idea -- it hangs.
  (and (null (memq system-type '(windows-nt ms-dos)))
       (eq 0 (call-process-shell-command
              (format "command -v %s" org-clock-x11idle-program-name)))
       ;; Check that x11idle can retrieve the idle time
       ;; FIXME: Why "..-shell-command" rather than just `call-process'?
       (eq 0 (call-process-shell-command org-clock-x11idle-program-name))))

(declare-function dbus-get-property "dbus"
                  (bus service path interface property))
(declare-function dbus-call-method "dbus"
                  (bus service path interface method &rest args))
(declare-function dbus-list-activatable-names "dbus" (&optional bus))
(defun org-x11-idle-seconds ()
  "Return the current X11 idle time in seconds."
  (/ (string-to-number (shell-command-to-string org-clock-x11idle-program-name)) 1000))

(defvar org-logind-dbus-session-path
  (when (and (boundp 'dbus-runtime-version)
             (require 'dbus nil t)
             (member "org.freedesktop.login1" (dbus-list-activatable-names)))
    (ignore-errors
      (dbus-call-method
       :system "org.freedesktop.login1"
       "/org/freedesktop/login1"
       "org.freedesktop.login1.Manager"
       "GetSessionByPID" (emacs-pid))))
  "D-Bus session path for the elogind interface.")

(defun org-logind-user-idle-seconds ()
  "Return the number of idle seconds for the user according to logind."
  (- (float-time)
     (/ (dbus-get-property
         :system "org.freedesktop.login1"
         org-logind-dbus-session-path
         "org.freedesktop.login1.Session" "IdleSinceHint")
        1e6)))

(defun org-user-idle-seconds ()
  "Return the number of seconds the user has been idle for.
This routine returns a floating point number."
  (cond
   ((eq system-type 'darwin)
    (org-mac-idle-seconds))
   ((and (eq window-system 'x) org-x11idle-exists-p)
    (org-x11-idle-seconds))
   ((and
     org-logind-dbus-session-path
     (dbus-get-property
      :system "org.freedesktop.login1"
      org-logind-dbus-session-path
      "org.freedesktop.login1.Session" "IdleHint"))
    (org-logind-user-idle-seconds))
   (t
    (org-emacs-idle-seconds))))

(provide 'org-idle)

;;; org-idle.el ends here
