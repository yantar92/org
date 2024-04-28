;;; ox-dispatch.el --- Org export dialogue -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>
;; Maintainer: Ihor Radchenko <yantar92 at posteo dot net>
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is part of GNU Emacs.

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

;;; Commentary:

;; The Dispatcher
;;
;; `org-export-dispatch' is the standard interactive way to start an
;; export process.  It uses `org-export--dispatch-ui' as a subroutine
;; for its interface, which, in turn, delegates response to key
;; pressed to `org-export--dispatch-action'.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ox)
(require 'ox-publish)

(defcustom org-export-dispatch-use-expert-ui nil
  "Non-nil means using a non-intrusive `org-export-dispatch'.
In that case, no help buffer is displayed.  Though, an indicator
for current export scope is added to the prompt (\"b\" when
output is restricted to body only, \"s\" when it is restricted to
the current subtree, \"v\" when only visible elements are
considered for export, \"f\" when publishing functions should be
passed the FORCE argument and \"a\" when the export should be
asynchronous).  Also, [?] allows switching back to standard
mode."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-export-initial-scope 'buffer
  "The initial scope when exporting with `org-export-dispatch'.
This variable can be either set to `buffer' or `subtree'."
  :group 'org-export-general
  :type '(choice
	  (const :tag "Export current buffer" buffer)
	  (const :tag "Export current subtree" subtree)))

(defcustom org-export-body-only nil
  "The initial \"Body only\" setting when exporting with `org-export-dispatch'.
Non-nil means only export body code, without the surrounding
template."
  :group 'org-export-general
  :package-version '(Org . "9.7")
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-visible-only nil
  "The initial \"Visible only\" setting when exporting with `org-export-dispatch'.
Non-nil means don't export the contents of hidden elements."
  :group 'org-export-general
  :package-version '(Org . "9.7")
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-force-publishing nil
  "The initial \"Force publishing\" setting for `org-export-dispatch'.
Non-nil means force all files in the project to be published."
  :group 'org-export-general
  :package-version '(Org . "9.7")
  :type 'boolean
  :safe #'booleanp)

(defcustom org-export-in-background nil
  "Non-nil means export and publishing commands will run in background.
Results from an asynchronous export are never displayed
automatically.  But you can retrieve them with `\\[org-export-stack]'."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defvar org-export-dispatch-last-action nil
  "Last command called from the dispatcher.
The value should be a list.  Its CAR is the action, as a symbol,
and its CDR is a list of export options.")

(defvar org-export-dispatch-last-position (make-marker)
  "The position where the last export command was created using the dispatcher.
This marker will be used with `\\[universal-argument] C-c C-e' to make sure export repetition
uses the same subtree if the previous command was restricted to a subtree.")

;;;###autoload
(defun org-export-dispatch (&optional arg)
  "Export dispatcher for Org mode.

It provides an access to common export related tasks in a buffer.
Its interface comes in two flavors: standard and expert.

While both share the same set of bindings, only the former
displays the valid keys associations in a dedicated buffer.
Scrolling (resp. line-wise motion) in this buffer is done with
SPC and DEL (resp. C-n and C-p) keys.

Set variable `org-export-dispatch-use-expert-ui' to switch to one
flavor or the other.

When ARG is `\\[universal-argument]', repeat the last export action, with the\
 same
set of options used back then, on the current buffer.

When ARG is `\\[universal-argument] \\[universal-argument]', display the \
asynchronous export stack."
  (interactive "P")
  (let* ((input
	  (cond ((equal arg '(16)) '(stack))
		((and arg org-export-dispatch-last-action))
		(t (unwind-protect
		       (progn
		         ;; Remember where we are
		         (move-marker org-export-dispatch-last-position
				      (point)
				      (org-base-buffer (current-buffer)))
		         ;; Get and store an export command
		         (setq org-export-dispatch-last-action
			       (org-export--dispatch-ui
			        (list org-export-initial-scope
				      (and org-export-body-only 'body)
				      (and org-export-visible-only 'visible)
				      (and org-export-force-publishing 'force)
				      (and org-export-in-background 'async))
			        nil
			        org-export-dispatch-use-expert-ui)))
                     (and (get-buffer-window "*Org Export Dispatcher*" t)
                          (quit-window 'kill (get-buffer-window "*Org Export Dispatcher*" t)))
		     (and (get-buffer "*Org Export Dispatcher*")
			  (kill-buffer "*Org Export Dispatcher*"))))))
	 (action (car input))
	 (optns (cdr input)))
    (unless (memq 'subtree optns)
      (move-marker org-export-dispatch-last-position nil))
    (cl-case action
      ;; First handle special hard-coded actions.
      (template (org-export-insert-default-template nil optns))
      (stack (org-export-stack))
      (publish-current-file
       (org-publish-current-file (memq 'force optns) (memq 'async optns)))
      (publish-current-project
       (org-publish-current-project (memq 'force optns) (memq 'async optns)))
      (publish-choose-project
       (org-publish (assoc (completing-read
			    "Publish project: "
			    org-publish-project-alist nil t)
			   org-publish-project-alist)
		    (memq 'force optns)
		    (memq 'async optns)))
      (publish-all (org-publish-all (memq 'force optns) (memq 'async optns)))
      (otherwise
       (save-excursion
	 (when arg
	   ;; Repeating command, maybe move cursor to restore subtree
	   ;; context.
	   (if (eq (marker-buffer org-export-dispatch-last-position)
		   (org-base-buffer (current-buffer)))
	       (goto-char org-export-dispatch-last-position)
	     ;; We are in a different buffer, forget position.
	     (move-marker org-export-dispatch-last-position nil)))
	 (funcall action
		  ;; Return a symbol instead of a list to ease
		  ;; asynchronous export macro use.
		  (and (memq 'async optns) t)
		  (and (memq 'subtree optns) t)
		  (and (memq 'visible optns) t)
		  (and (memq 'body optns) t)))))))

(defun org-export--dispatch-ui (options first-key expertp)
  "Handle interface for `org-export-dispatch'.

OPTIONS is a list containing current interactive options set for
export.  It can contain any of the following symbols:
`body'    toggles a body-only export
`subtree' restricts export to current subtree
`visible' restricts export to visible part of buffer.
`force'   force publishing files.
`async'   use asynchronous export process

FIRST-KEY is the key pressed to select the first level menu.  It
is nil when this menu hasn't been selected yet.

EXPERTP, when non-nil, triggers expert UI.  In that case, no help
buffer is provided, but indications about currently active
options are given in the prompt.  Moreover, [?] allows switching
back to standard interface."
  (let* ((fontify-key
	  (lambda (key &optional access-key)
	    ;; Fontify KEY string.  Optional argument ACCESS-KEY, when
	    ;; non-nil is the required first-level key to activate
	    ;; KEY.  When its value is t, activate KEY independently
	    ;; on the first key, if any.  A nil value means KEY will
	    ;; only be activated at first level.
	    (if (or (eq access-key t) (eq access-key first-key))
		(propertize key 'face 'org-dispatcher-highlight)
	      key)))
	 (fontify-value
	  (lambda (value)
	    ;; Fontify VALUE string.
	    (propertize value 'face 'font-lock-variable-name-face)))
	 ;; Prepare menu entries by extracting them from registered
	 ;; backends and sorting them by access key and by ordinal,
	 ;; if any.
	 (entries
	  (sort (sort (delq nil
			    (mapcar #'org-export-backend-menu
				    org-export-registered-backends))
		      (lambda (a b)
			(let ((key-a (nth 1 a))
			      (key-b (nth 1 b)))
			  (cond ((and (numberp key-a) (numberp key-b))
				 (< key-a key-b))
				((numberp key-b) t)))))
		#'car-less-than-car))
	 ;; Compute a list of allowed keys based on the first key
	 ;; pressed, if any.  Some keys
	 ;; (?^B, ?^V, ?^S, ?^F, ?^A, ?&, ?# and ?q) are always
	 ;; available.
	 (allowed-keys
	  (nconc (list 2 22 19 6 1)
		 (if (not first-key) (org-uniquify (mapcar #'car entries))
		   (let (sub-menu)
		     (dolist (entry entries (sort (mapcar #'car sub-menu) #'<))
		       (when (eq (car entry) first-key)
			 (setq sub-menu (append (nth 2 entry) sub-menu))))))
		 (cond ((eq first-key ?P) (list ?f ?p ?x ?a))
		       ((not first-key) (list ?P)))
		 (list ?& ?#)
		 (when expertp (list ??))
		 (list ?q)))
	 ;; Build the help menu for standard UI.
	 (help
	  (unless expertp
	    (concat
	     ;; Options are hard-coded.
	     (format "[%s] Body only:    %s           [%s] Visible only:     %s
\[%s] Export scope: %s       [%s] Force publishing: %s
\[%s] Async export: %s\n\n"
		     (funcall fontify-key "C-b" t)
		     (funcall fontify-value
			      (if (memq 'body options) "On " "Off"))
		     (funcall fontify-key "C-v" t)
		     (funcall fontify-value
			      (if (memq 'visible options) "On " "Off"))
		     (funcall fontify-key "C-s" t)
		     (funcall fontify-value
			      (if (memq 'subtree options) "Subtree" "Buffer "))
		     (funcall fontify-key "C-f" t)
		     (funcall fontify-value
			      (if (memq 'force options) "On " "Off"))
		     (funcall fontify-key "C-a" t)
		     (funcall fontify-value
			      (if (memq 'async options) "On " "Off")))
	     ;; Display registered backend entries.  When a key
	     ;; appears for the second time, do not create another
	     ;; entry, but append its sub-menu to existing menu.
	     (let (last-key)
	       (mapconcat
		(lambda (entry)
		  (let ((top-key (car entry)))
		    (concat
		     (unless (eq top-key last-key)
		       (setq last-key top-key)
		       (format "\n[%s] %s\n"
			       (funcall fontify-key (char-to-string top-key))
			       (nth 1 entry)))
		     (let ((sub-menu (nth 2 entry)))
		       (unless (functionp sub-menu)
			 ;; Split sub-menu into two columns.
			 (let ((index -1))
			   (concat
			    (mapconcat
			     (lambda (sub-entry)
			       (cl-incf index)
			       (format
				(if (zerop (mod index 2)) "    [%s] %-26s"
				  "[%s] %s\n")
				(funcall fontify-key
					 (char-to-string (car sub-entry))
					 top-key)
				(nth 1 sub-entry)))
			     sub-menu "")
			    (when (zerop (mod index 2)) "\n"))))))))
		entries ""))
	     ;; Publishing menu is hard-coded.
	     (format "\n[%s] Publish
    [%s] Current file              [%s] Current project
    [%s] Choose project            [%s] All projects\n\n\n"
		     (funcall fontify-key "P")
		     (funcall fontify-key "f" ?P)
		     (funcall fontify-key "p" ?P)
		     (funcall fontify-key "x" ?P)
		     (funcall fontify-key "a" ?P))
	     (format "[%s] Export stack                  [%s] Insert template\n"
		     (funcall fontify-key "&" t)
		     (funcall fontify-key "#" t))
	     (format "[%s] %s"
		     (funcall fontify-key "q" t)
		     (if first-key "Main menu" "Exit")))))
	 ;; Build prompts for both standard and expert UI.
	 (standard-prompt (unless expertp "Export command: "))
	 (expert-prompt
	  (when expertp
	    (format
	     "Export command (C-%s%s%s%s%s) [%s]: "
	     (if (memq 'body options) (funcall fontify-key "b" t) "b")
	     (if (memq 'visible options) (funcall fontify-key "v" t) "v")
	     (if (memq 'subtree options) (funcall fontify-key "s" t) "s")
	     (if (memq 'force options) (funcall fontify-key "f" t) "f")
	     (if (memq 'async options) (funcall fontify-key "a" t) "a")
	     (mapconcat (lambda (k)
			  ;; Strip control characters.
			  (unless (< k 27) (char-to-string k)))
			allowed-keys "")))))
    ;; With expert UI, just read key with a fancy prompt.  In standard
    ;; UI, display an intrusive help buffer.
    (if expertp
	(org-export--dispatch-action
	 expert-prompt allowed-keys entries options first-key expertp)
      (save-window-excursion
        ;; At first call, create frame layout in order to display menu.
        (unless (get-buffer "*Org Export Dispatcher*")
          (pop-to-buffer "*Org Export Dispatcher*" '(org-display-buffer-split))
          (setq cursor-type nil)
          (setq header-line-format
                (let ((propertize-help-key
                       (lambda (key)
                         ;; Add `face' *and* `font-lock-face' to "work
                         ;; reliably in any buffer", per a comment in
                         ;; `help--key-description-fontified'.
                         (propertize key
                                     'font-lock-face 'help-key-binding
                                     'face 'help-key-binding))))
                  (apply 'format
                         (cons "Use %s, %s, %s, or %s to navigate."
                               (mapcar propertize-help-key
                                       (list "SPC" "DEL" "C-n" "C-p"))))))
	  ;; Make sure that invisible cursor will not highlight square
	  ;; brackets.
	  (set-syntax-table (copy-syntax-table))
	  (modify-syntax-entry ?\[ "w"))
        ;; At this point, the buffer containing the menu exists and is
        ;; visible in the current window.  So, refresh it.
        (with-current-buffer "*Org Export Dispatcher*"
	  ;; Refresh help.  Maintain display continuity by re-visiting
	  ;; previous window position.
	  (let ((pt (point))
                (wstart (window-start)))
	    (erase-buffer)
	    (insert help)
	    (goto-char pt)
	    (set-window-start nil wstart)))
        (org-fit-window-to-buffer)
        (org-export--dispatch-action
         standard-prompt allowed-keys entries options first-key expertp)))))

(defun org-export--dispatch-action
    (prompt allowed-keys entries options first-key expertp)
  "Read a character from command input and act accordingly.

PROMPT is the displayed prompt, as a string.  ALLOWED-KEYS is
a list of characters available at a given step in the process.
ENTRIES is a list of menu entries.  OPTIONS, FIRST-KEY and
EXPERTP are the same as defined in `org-export--dispatch-ui',
which see.

Toggle export options when required.  Otherwise, return value is
a list with action as CAR and a list of interactive export
options as CDR."
  (let (key)
    ;; Scrolling: when in non-expert mode, act on motion keys (C-n,
    ;; C-p, SPC, DEL).
    (while (and (setq key (read-char-exclusive prompt))
		(not expertp)
		;; FIXME: Don't use C-v (22) here, as it is used as a
		;; modifier key in the export dispatch.
		(memq key '(14 16 ?\s ?\d 134217846)))
      (org-scroll key t))
    (cond
     ;; Ignore undefined associations.
     ((not (memq key allowed-keys))
      (ding)
      (unless expertp (message "Invalid key") (sit-for 1))
      (org-export--dispatch-ui options first-key expertp))
     ;; q key at first level aborts export.  At second level, cancel
     ;; first key instead.
     ((eq key ?q) (if (not first-key) (user-error "Export aborted")
		    (org-export--dispatch-ui options nil expertp)))
     ;; Help key: Switch back to standard interface if expert UI was
     ;; active.
     ((eq key ??) (org-export--dispatch-ui options first-key nil))
     ;; Send request for template insertion along with export scope.
     ((eq key ?#) (cons 'template (memq 'subtree options)))
     ;; Switch to asynchronous export stack.
     ((eq key ?&) '(stack))
     ;; Toggle options: C-b (2) C-v (22) C-s (19) C-f (6) C-a (1).
     ((memq key '(2 22 19 6 1))
      (org-export--dispatch-ui
       (let ((option (cl-case key (2 'body) (22 'visible) (19 'subtree)
			      (6 'force) (1 'async))))
	 (if (memq option options) (remq option options)
	   (cons option options)))
       first-key expertp))
     ;; Action selected: Send key and options back to
     ;; `org-export-dispatch'.
     ((or first-key (functionp (nth 2 (assq key entries))))
      (cons (cond
	     ((not first-key) (nth 2 (assq key entries)))
	     ;; Publishing actions are hard-coded.  Send a special
	     ;; signal to `org-export-dispatch'.
	     ((eq first-key ?P)
	      (cl-case key
		(?f 'publish-current-file)
		(?p 'publish-current-project)
		(?x 'publish-choose-project)
		(?a 'publish-all)))
	     ;; Return first action associated to FIRST-KEY + KEY
	     ;; path. Indeed, derived backends can share the same
	     ;; FIRST-KEY.
	     (t (catch 'found
		  (dolist (entry (member (assq first-key entries) entries))
		    (let ((match (assq key (nth 2 entry))))
		      (when match (throw 'found (nth 2 match))))))))
	    options))
     ;; Otherwise, enter sub-menu.
     (t (org-export--dispatch-ui options key expertp)))))

(provide 'ox-dispatch)

;;; ox-dispatch.el ends here
