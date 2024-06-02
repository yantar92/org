;;; org-clock-notify.el --- Display notifications -*- lexical-binding: t; -*-

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

;; This file implements API to send system notifications.

;;; Code:

(require 'org-macs)
(org-assert-version)
(declare-function w32-notification-close "w32fns.c" (&rest params))

(defcustom org-clock-sound nil
  "Sound to use for notifications.
Possible values are:

nil        No sound played
t          Standard Emacs beep
file name  Play this sound file, fall back to beep"
  :group 'org-clock
  :type '(choice
	  (const :tag "No sound" nil)
	  (const :tag "Standard beep" t)
	  (file  :tag "Play sound file")))

(defcustom org-show-notification-handler nil
  "Function or program to send notification with.
The function or program will be called with the notification
string as argument."
  :group 'org-clock
  :type '(choice
	  (const nil)
	  (string :tag "Program")
	  (function :tag "Function")))

(defcustom org-show-notification-timeout 3
  "Number of seconds to wait before closing Org notifications.
This is applied to notifications sent with `notifications-notify'
and `w32-notification-notify' only, not other mechanisms possibly
set through `org-show-notification-handler'."
  :group 'org-clock
  :package-version '(Org . "9.4")
  :type 'integer)

(defun org-notify (notification &optional play-sound)
  "Send a NOTIFICATION and maybe PLAY-SOUND.
If PLAY-SOUND is non-nil, it overrides `org-clock-sound'."
  (org-show-notification notification)
  (if play-sound (org-clock-play-sound play-sound)))

(defun org-show-notification (notification)
  "Show NOTIFICATION.
Use `org-show-notification-handler' if defined,
use libnotify if available, or fall back on a message."
  (ignore-errors (require 'notifications))
  (cond ((functionp org-show-notification-handler)
	 (funcall org-show-notification-handler notification))
	((stringp org-show-notification-handler)
	 (start-process "emacs-timer-notification" nil
			org-show-notification-handler notification))
        ((fboundp 'haiku-notifications-notify)
         ;; N.B. timeouts are not available under Haiku.
         (haiku-notifications-notify :title "Org mode message"
                                     :body notification
                                     :urgency 'low))
        ((fboundp 'android-notifications-notify)
         ;; N.B. timeouts are not available under Haiku or Android.
         (android-notifications-notify :title "Org mode message"
                                       :body notification
                                       ;; Low urgency notifications
                                       ;; are by default hidden.
                                       :urgency 'normal))
	((fboundp 'w32-notification-notify)
	 (let ((id (w32-notification-notify
		    :title "Org mode message"
		    :body notification
		    :urgency 'low)))
	   (run-with-timer
	    org-show-notification-timeout
	    nil
	    (lambda () (w32-notification-close id)))))
        ((fboundp 'ns-do-applescript)
         (ns-do-applescript
          (format "display notification \"%s\" with title \"Org mode notification\""
                  (replace-regexp-in-string "\"" "#" notification))))
	((fboundp 'notifications-notify)
	 (notifications-notify
	  :title "Org mode message"
	  :body notification
	  :timeout (* org-show-notification-timeout 1000)
	  ;; FIXME how to link to the Org icon?
	  ;; :app-icon "~/.emacs.d/icons/mail.png"
	  :urgency 'low))
	((executable-find "notify-send")
	 (start-process "emacs-timer-notification" nil
			"notify-send" notification))
	;; Maybe the handler will send a message, so only use message as
	;; a fall back option
	(t (message "%s" notification))))

(defun org-clock-play-sound (&optional clock-sound)
  "Play sound as configured by `org-clock-sound'.
Use alsa's aplay tool if available.
If CLOCK-SOUND is non-nil, it overrides `org-clock-sound'."
  (let ((org-clock-sound (or clock-sound org-clock-sound)))
    (cond
     ((not org-clock-sound))
     ((eq org-clock-sound t) (beep t) (beep t))
     ((stringp org-clock-sound)
      (let ((file (expand-file-name org-clock-sound)))
	(if (file-exists-p file)
	    (if (executable-find "aplay")
		(start-process "org-clock-play-notification" nil
			       "aplay" file)
	      (condition-case-unless-debug nil
		  (play-sound-file file)
		(error (beep t) (beep t))))))))))

(provide 'org-clock-notify)

;;; org-clock-notify.el ends here
