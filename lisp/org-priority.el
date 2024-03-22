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

(defvar org-todo-line-regexp)

(defgroup org-priorities nil
  "Priorities in Org mode."
  :tag "Org Priorities"
  :group 'org-todo)

(defvaralias 'org-enable-priority-commands 'org-priority-enable-commands)
(defcustom org-priority-enable-commands t
  "Non-nil means priority commands are active.
When nil, these commands will be disabled, so that you never accidentally
set a priority."
  :group 'org-priorities
  :type 'boolean)

(defvaralias 'org-highest-priority 'org-priority-highest)

(defcustom org-priority-highest ?A
  "The highest priority of TODO items.

A character like ?A, ?B, etc., or a numeric value like 1, 2, etc.

The default is the character ?A, which is 65 as a numeric value.

If you set `org-priority-highest' to a numeric value inferior to
65, Org assumes you want to use digits for the priority cookie.
If you set it to >=65, Org assumes you want to use alphabetical
characters.

In both cases, the value of `org-priority-highest' must be
smaller than `org-priority-lowest': for example, if \"A\" is the
highest priority, it is smaller than the lowest \"C\" priority:
65 < 67."
  :group 'org-priorities
  :type '(choice
	  (character :tag "Character")
	  (integer :tag "Integer (< 65)")))

(defvaralias 'org-lowest-priority 'org-priority-lowest)
(defcustom org-priority-lowest ?C
  "The lowest priority of TODO items.

A character like ?C, ?B, etc., or a numeric value like 9, 8, etc.

The default is the character ?C, which is 67 as a numeric value.

If you set `org-priority-lowest' to a numeric value inferior to
65, Org assumes you want to use digits for the priority cookie.
If you set it to >=65, Org assumes you want to use alphabetical
characters.

In both cases, the value of `org-priority-lowest' must be greater
than `org-priority-highest': for example, if \"C\" is the lowest
priority, it is greater than the highest \"A\" priority: 67 >
65."
  :group 'org-priorities
  :type '(choice
	  (character :tag "Character")
	  (integer :tag "Integer (< 65)")))

(defvaralias 'org-default-priority 'org-priority-default)
(defcustom org-priority-default ?B
  "The default priority of TODO items.
This is the priority an item gets if no explicit priority is given.
When starting to cycle on an empty priority the first step in the cycle
depends on `org-priority-start-cycle-with-default'.  The resulting first
step priority must not exceed the range from `org-priority-highest' to
`org-priority-lowest' which means that `org-priority-default' has to be
in this range exclusive or inclusive to the range boundaries.  Else the
first step refuses to set the default and the second will fall back on
\(depending on the command used) the highest or lowest priority."
  :group 'org-priorities
  :type '(choice
	  (character :tag "Character")
	  (integer :tag "Integer (< 65)")))

(defcustom org-priority-start-cycle-with-default t
  "Non-nil means start with default priority when starting to cycle.
When this is nil, the first step in the cycle will be (depending on the
command used) one higher or lower than the default priority.
See also `org-priority-default'."
  :group 'org-priorities
  :type 'boolean)

(defvaralias 'org-get-priority-function 'org-priority-get-priority-function)
(defcustom org-priority-get-priority-function nil
  "Function to extract the priority from a string.
The string is normally the headline.  If this is nil, Org
computes the priority from the priority cookie like [#A] in the
headline.  It returns an integer, increasing by 1000 for each
priority level.

The user can set a different function here, which should take a
string as an argument and return the numeric priority."
  :group 'org-priorities
  :version "24.1"
  :type '(choice
	  (const nil)
	  (function)))

(defun org-priority-to-value (s)
  "Convert priority string S to its numeric value."
  (or (save-match-data
	(and (string-match "\\([0-9]+\\)" s)
	     (string-to-number (match-string 1 s))))
      (string-to-char s)))

(defun org-priority-up ()
  "Increase the priority of the current item."
  (interactive)
  (org-priority 'up))

(defun org-priority-down ()
  "Decrease the priority of the current item."
  (interactive)
  (org-priority 'down))

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

(defun org-get-priority (s)
  "Find priority cookie and return priority.
S is a string against which you can match `org-priority-regexp'.
If `org-priority-get-priority-function' is set to a custom
function, use it.  Otherwise process S and output the priority
value, an integer."
  (save-match-data
    (if (functionp org-priority-get-priority-function)
	(funcall org-priority-get-priority-function s)
      (if (not (string-match org-priority-regexp s))
	  (* 1000 (- org-priority-lowest org-priority-default))
	(* 1000 (- org-priority-lowest
		   (org-priority-to-value (match-string 2 s))))))))

(provide 'org-priority)

;;; org-priority.el ends here
