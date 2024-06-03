;;; org-priority.el --- Org priority API                      -*- lexical-binding: t; -*-

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

;; This library implements Org mode headline priorities.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-regexps)
(require 'org-move)
(require 'org-tags)
(require 'org-priority-common)
(require 'org-mode-common)

(defgroup org-priorities nil
  "Priorities in Org mode."
  :tag "Org Priorities"
  :group 'org-todo)

(defcustom org-priority-start-cycle-with-default t
  "Non-nil means start with default priority when starting to cycle.
When this is nil, the first step in the cycle will be (depending on the
command used) one higher or lower than the default priority.
See also `org-priority-default'."
  :group 'org-priorities
  :type 'boolean)

;;;###autoload
(defun org-priority-up ()
  "Increase the priority of the current item."
  (interactive)
  (org-priority 'up))

;;;###autoload
(defun org-priority-down ()
  "Decrease the priority of the current item."
  (interactive)
  (org-priority 'down))

;;;###autoload
(defun org-priority (&optional action show)
  "Change the priority of an item.

When called interactively with a `\\[universal-argument]' prefix,
show the priority in the minibuffer instead of changing it.

When called programmatically, ACTION can be `set', `up', `down',
or a character."
  (interactive "P")
  (when show
    ;; Deprecation warning inserted for Org 9.2; once enough time has
    ;; passed the SHOW argument should be removed.
    (warn "`org-priority' called with deprecated SHOW argument"))
  (if (equal action '(4))
      (org-priority-show)
    (unless org-priority-enable-commands
      (user-error "Priority commands are disabled"))
    (setq action (or action 'set))
    (let ((nump (< org-priority-lowest 65))
	  current new news have remove)
      (save-excursion
	(org-back-to-heading t)
	(when (looking-at org-priority-regexp)
	  (let ((ms (match-string 2)))
	    (setq current (org-priority-to-value ms)
		  have t)))
	(cond
	 ((eq action 'remove)
	  (setq remove t new ?\ ))
	 ((or (eq action 'set)
	      (integerp action))
	  (if (not (eq action 'set))
	      (setq new action)
	    (setq
	     new
	     (if nump
                 (let* ((msg (format "Priority %s-%s, SPC to remove: "
                                     (number-to-string org-priority-highest)
                                     (number-to-string org-priority-lowest)))
                        (s (if (< 9 org-priority-lowest)
                               (read-string msg)
                             (message msg)
                             (char-to-string (read-char-exclusive)))))
                   (if (equal s " ") ?\s (string-to-number s)))
	       (progn (message "Priority %c-%c, SPC to remove: "
			       org-priority-highest org-priority-lowest)
		      (save-match-data
			(setq new (read-char-exclusive)))))))
	  (when (and (= (upcase org-priority-highest) org-priority-highest)
		     (= (upcase org-priority-lowest) org-priority-lowest))
	    (setq new (upcase new)))
	  (cond ((equal new ?\s) (setq remove t))
		((or (< (upcase new) org-priority-highest) (> (upcase new) org-priority-lowest))
		 (user-error
		  (if nump
		      "Priority must be between `%s' and `%s'"
		    "Priority must be between `%c' and `%c'")
		  org-priority-highest org-priority-lowest))))
	 ((eq action 'up)
	  (setq new (if have
			(1- current)  ; normal cycling
		      ;; last priority was empty
		      (if (eq last-command this-command)
			  org-priority-lowest  ; wrap around empty to lowest
			;; default
			(if org-priority-start-cycle-with-default
			    org-priority-default
			  (1- org-priority-default))))))
	 ((eq action 'down)
	  (setq new (if have
			(1+ current)  ; normal cycling
		      ;; last priority was empty
		      (if (eq last-command this-command)
			  org-priority-highest  ; wrap around empty to highest
			;; default
			(if org-priority-start-cycle-with-default
			    org-priority-default
			  (1+ org-priority-default))))))
	 (t (user-error "Invalid action")))
	(when (or (< (upcase new) org-priority-highest)
		  (> (upcase new) org-priority-lowest))
	  (if (and (memq action '(up down))
		   (not have) (not (eq last-command this-command)))
	      ;; `new' is from default priority
	      (error
	       "The default can not be set, see `org-priority-default' why")
	    ;; normal cycling: `new' is beyond highest/lowest priority
	    ;; and is wrapped around to the empty priority
	    (setq remove t)))
	;; Numerical priorities are limited to 64, beyond that number,
	;; assume the priority cookie is a character.
	(setq news (if (> new 64) (format "%c" new) (format "%s" new)))
	(if have
	    (if remove
		(replace-match "" t t nil 1)
	      (replace-match news t t nil 2))
	  (if remove
	      (user-error "No priority cookie found in line")
	    (let ((case-fold-search nil)) (looking-at org-todo-line-regexp))
	    (if (match-end 2)
		(progn
		  (goto-char (match-end 2))
		  (insert " [#" news "]"))
	      (goto-char (match-beginning 3))
	      (insert "[#" news "] "))))
	(when org-auto-align-tags (org-align-tags)))
      (if remove
	  (message "Priority removed")
	(message "Priority of current item set to %s" news)))))

(defalias 'org-show-priority 'org-priority-show)
(defun org-priority-show ()
  "Show the priority of the current item as number.
Return the priority value."
  (interactive)
  (let ((pri (if (eq major-mode 'org-agenda-mode)
		 (org-get-at-bol 'priority)
               (let ((heading (org-element-at-point)))
                 (when (org-element-type-p heading 'headline)
                   (org-get-priority (org-element-property :raw-value (org-element-at-point))))))))
    (message "Priority is %d" (if pri pri -1000))))

(provide 'org-priority)

;;; org-priority.el ends here
