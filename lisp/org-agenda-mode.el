;;; org-agenda-mode.el --- Org agenda major mode  -*- mode: emacs-lisp; lexical-binding: t; -*-

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

;; This file implements major mode active in agenda buffers.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-keys)
(require 'org-agenda-common)
(require 'org-agenda-files)
(require 'org-agenda-undo)
(require 'org-agenda-highlight)
(require 'org-agenda-window)
(require 'org-agenda-buffer-format)

(require 'org-fold)

(defcustom org-agenda-persistent-filter nil
  "When set, keep filters from one agenda view to the next."
  :group 'org-agenda
  :type 'boolean)

(defgroup org-agenda nil
  "Options concerning agenda views in Org mode."
  :tag "Org Agenda"
  :group 'org)

;;; Helpers

(defun org-agenda-check-type (error &rest types)
  "Check if agenda buffer or component is of allowed type.
If ERROR is non-nil, throw an error, otherwise just return nil.
Allowed types are `agenda' `todo' `tags' `search'."
  (cond ((not org-agenda-type)
	 (error "No Org agenda currently displayed"))
	((memq org-agenda-type types) t)
	(error
	 (error "Not allowed in '%s'-type agenda buffer or component" org-agenda-type))
	(t nil)))

(defun org-agenda-error ()
  "Throw an error when a command is not allowed in the agenda."
  (user-error "Command not allowed in this line"))

(defun org-agenda-check-no-diary ()
  "Check if the entry is a diary link and abort if yes."
  (when (org-get-at-bol 'org-agenda-diary-link)
    (org-agenda-error)))

;;; Preparing agenda window and buffer

(defun org-agenda-prepare-window (abuf filter-alist)
  "Setup agenda buffer in the window.
ABUF is the buffer for the agenda window.
FILTER-ALIST is an alist of filters we need to apply when
`org-agenda-persistent-filter' is non-nil."
  (org-agenda--prepare-window-1 abuf)
  ;; ABUF is made current by `org-agenda--prepare-window-1'.
  ;; We are now setting filter buffer-locally.
  (setq org-agenda-tag-filter (cdr (assq 'tag filter-alist)))
  (setq org-agenda-category-filter (cdr (assq 'cat filter-alist)))
  (setq org-agenda-effort-filter (cdr (assq 'effort filter-alist)))
  (setq org-agenda-regexp-filter (cdr (assq 're filter-alist))))

(defun org-agenda-prepare (&optional name)
  (let ((filter-alist (when org-agenda-persistent-filter
			(with-current-buffer
			    (get-buffer-create org-agenda-buffer-name)
			  `((tag . ,org-agenda-tag-filter)
			    (re . ,org-agenda-regexp-filter)
			    (effort . ,org-agenda-effort-filter)
			    (cat . ,org-agenda-category-filter))))))
    (if (org-agenda-use-sticky-p)
	(progn
	  ;; Popup existing buffer
	  (org-agenda-prepare-window (get-buffer org-agenda-buffer-name)
				     filter-alist)
	  (message "Sticky Agenda buffer, use `r' to refresh")
	  (or org-agenda-multi (org-agenda-fit-window-to-buffer))
	  (throw 'exit "Sticky Agenda buffer, use `r' to refresh"))
      (if org-agenda-multi
	  (progn
            (org-agenda--insert-block-separator)
	    (narrow-to-region (point) (point-max)))
	;; Setting any org variables that are in org-agenda-local-vars
	;; list need to be done after the prepare call
	(org-agenda-prepare-window
	 (get-buffer-create org-agenda-buffer-name) filter-alist)
	(setq buffer-read-only nil)
	(org-agenda-reset-markers)
	(let ((inhibit-read-only t)) (erase-buffer))
	(org-agenda-mode)
	(setq org-agenda-buffer (current-buffer))
	(setq org-agenda-contributing-files nil)
	(setq org-agenda-columns-active nil)
        (setq org-agenda-filters-preset
              `((tag . ,org-agenda-tag-filter-preset)
                (category . ,org-agenda-category-filter-preset)
                (regexp . ,org-agenda-regexp-filter-preset)
                (effort . ,org-agenda-effort-filter-preset)))
        (org-agenda-prepare-buffers (org-agenda-files nil 'ifmode))
	(setq org-agenda-last-prefix-arg current-prefix-arg)
	(setq org-agenda-this-buffer-name org-agenda-buffer-name)
	(and name (not org-agenda-name)
	     (setq-local org-agenda-name name)))
      (setq buffer-read-only nil))))

;;; Startup

(defgroup org-agenda-startup nil
  "Options concerning initial settings in the Agenda in Org Mode."
  :tag "Org Agenda Startup"
  :group 'org-agenda)

(defcustom org-agenda-start-with-follow-mode nil
  "The initial value of follow mode in a newly created agenda window."
  :group 'org-agenda-startup
  :type 'boolean)

(defcustom org-agenda-start-with-entry-text-mode nil
  "The initial value of entry-text-mode in a newly created agenda window."
  :group 'org-agenda-startup
  :type 'boolean)

(defcustom org-agenda-start-with-archives-mode nil
  "Initial value of archive-mode in a newly created agenda window.
See `org-agenda-archives-mode' for acceptable values and their
meaning."
  :group 'org-agenda-startup
  :package-version '(Org . "9.7")
  :type 'symbol)

(defcustom org-agenda-start-with-log-mode nil
  "The initial value of log-mode in a newly created agenda window.
See `org-agenda-log-mode' and `org-agenda-log-mode-items' for further
explanations on the possible values."
  :group 'org-agenda-startup
  :group 'org-agenda-daily/weekly
  :type '(choice (const :tag "Don't show log items" nil)
		 (const :tag "Show only log items" only)
		 (const :tag "Show all possible log items" clockcheck)
		 (repeat :tag "Choose among possible values for `org-agenda-log-mode-items'"
			 (choice (const :tag "Show closed log items" closed)
				 (const :tag "Show clocked log items" clock)
				 (const :tag "Show all logged state changes" state)))))

(defcustom org-agenda-start-with-clockreport-mode nil
  "The initial value of clockreport-mode in a newly created agenda window."
  :group 'org-agenda-startup
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-view-columns-initially nil
  "When non-nil, switch to columns view right after creating the agenda."
  :group 'org-agenda-column-view
  :type 'boolean
  :version "26.1"
  :package-version '(Org . "9.0")
  :safe #'booleanp)

;;; Finalize agenda after filling in all the blocks

(defcustom org-agenda-finalize-hook nil
  "Hook run just before displaying an agenda buffer.
The buffer is still writable when the hook is called.

You can modify some of the buffer substrings but you should be
extra careful not to modify the text properties of the agenda
headlines as the agenda display heavily relies on them."
  :group 'org-agenda-startup
  :type 'hook)

(declare-function org-agenda-columns "org-colview-agenda" ())
(defun org-agenda-finalize ()
  "Finishing touch for the agenda buffer.
This function is called just before displaying the agenda.  If
you want to add your own functions to the finalization of the
agenda display, configure `org-agenda-finalize-hook'."
  (unless org-agenda-multi
    (org-agenda-format-buffer)
    (let ((inhibit-read-only t))
      (save-excursion
	(goto-char (point-min))
        (when (bound-and-true-p org-overriding-columns-format)
	  (setq-local org-local-columns-format
		      org-overriding-columns-format))
	(when org-agenda-view-columns-initially
	  (org-agenda-columns))
	(setq org-agenda-type (org-get-at-bol 'org-agenda-type))
	(setq org-agenda-represented-tags nil
	      org-agenda-represented-categories nil)
	(org-agenda-apply-all-filters)
	(add-hook 'kill-buffer-hook #'org-agenda-reset-markers 'append 'local))
      (run-hooks 'org-agenda-finalize-hook))))

;;; Major mode definition

(defvar org-agenda-mode-hook nil
  "Hook run after `org-agenda-mode' is turned on.
The buffer is still writable when this hook is called.")

(defvaralias 'org-agenda-keymap 'org-agenda-mode-map)
(defvar org-agenda-mode-map (make-sparse-keymap)
  "Keymap for `org-agenda-mode'.")

(defun org-agenda-set-mode-name ()
  "Set the mode name to indicate all the small mode settings."
  (setq mode-name
	(list "Org-Agenda"
	      (if (get 'org-agenda-files 'org-restrict) " []" "")
	      " "
	      '(:eval (org-agenda-span-name org-agenda-current-span))
	      (if org-agenda-follow-mode     " Follow" "")
	      (if org-agenda-entry-text-mode " ETxt"   "")
	      (if (bound-and-true-p org-agenda-include-diary)   " Diary"  "")
	      (if (bound-and-true-p org-agenda-include-deadlines) " Ddl"  "")
	      (if (bound-and-true-p org-agenda-use-time-grid)   " Grid"   "")
	      (if (and (boundp 'org-habit-show-habits)
		       org-habit-show-habits)
		  " Habit"   "")
	      (cond
	       ((consp org-agenda-show-log) " LogAll")
	       ((eq org-agenda-show-log 'clockcheck) " ClkCk")
	       (org-agenda-show-log " Log")
	       (t ""))
	      (if (org-agenda-filter-any) " " "")
	      (if (or org-agenda-category-filter
		      (assoc-default 'category org-agenda-filters-preset))
		  '(:eval (propertize
			   (concat "["
	      			   (mapconcat
                                    #'identity
	      			    (append
                                     (assoc-default 'category org-agenda-filters-preset)
	      			     org-agenda-category-filter)
	      			    "")
				   "]")
	      		   'face 'org-agenda-filter-category
                           'help-echo "Category used in filtering"))
                "")
	      (if (or org-agenda-tag-filter
		      (assoc-default 'tag org-agenda-filters-preset))
		  '(:eval (propertize
			   (concat (mapconcat
				    #'identity
				    (append
				     (assoc-default 'tag org-agenda-filters-preset)
				     org-agenda-tag-filter)
				    ""))
			   'face 'org-agenda-filter-tags
			   'help-echo "Tags used in filtering"))
		"")
	      (if (or org-agenda-effort-filter
		      (assoc-default 'effort org-agenda-filters-preset))
		  '(:eval (propertize
			   (concat (mapconcat
				    #'identity
				    (append
				     (assoc-default 'effort org-agenda-filters-preset)
				     org-agenda-effort-filter)
				    ""))
			   'face 'org-agenda-filter-effort
			   'help-echo "Effort conditions used in filtering"))
		"")
	      (if (or org-agenda-regexp-filter
		      (assoc-default 'regexp org-agenda-filters-preset))
		  '(:eval (propertize
			   (concat (mapconcat
				    (lambda (x) (concat (substring x 0 1) "/" (substring x 1) "/"))
				    (append
				     (assoc-default 'regexp org-agenda-filters-preset)
				     org-agenda-regexp-filter)
				    ""))
			   'face 'org-agenda-filter-regexp
			   'help-echo "Regexp used in filtering"))
		"")
	      (if org-agenda-archives-mode
		  (if (eq org-agenda-archives-mode t)
		      " Archives"
		    (format " :%s:" org-archive-tag))
		"")
	      (if org-agenda-clockreport-mode " Clock" "")))
  (force-mode-line-update))

(defun org-agenda-update-agenda-type ()
  "Update the agenda type after each command."
  (setq org-agenda-type
	(or (get-text-property (point) 'org-agenda-type)
	    (get-text-property (max (point-min) (1- (point))) 'org-agenda-type))))

(defun org-agenda-mode ()
  "Mode for time-sorted view on action items in Org files.

The following commands are available:

\\{org-agenda-mode-map}"
  (interactive)
  (ignore-errors (require 'face-remap))
  (let ((agenda-local-vars-to-keep
	 '(text-scale-mode-amount
	   text-scale-mode
	   text-scale-mode-lighter
	   face-remapping-alist))
	(save (buffer-local-variables)))
    (kill-all-local-variables)
    (cl-flet ((reset-saved (var-set)
		"Reset variables in VAR-SET to possibly stored value in SAVE."
		(dolist (elem save)
		  (pcase elem
		    (`(,var . ,val)		;ignore unbound variables
		     (when (and val (memq var var-set))
		       (set var val)))))))
      (cond (org-agenda-doing-sticky-redo
	     ;; Refreshing sticky agenda-buffer
	     ;;
	     ;; Preserve the value of `org-agenda-local-vars' variables.
	     (mapc #'make-local-variable org-agenda-local-vars)
	     (reset-saved org-agenda-local-vars)
	     (setq-local org-agenda-this-buffer-is-sticky t))
	    (org-agenda-sticky
	     ;; Creating a sticky Agenda buffer for the first time
	     (mapc #'make-local-variable org-agenda-local-vars)
	     (setq-local org-agenda-this-buffer-is-sticky t))
	    (t
	     ;; Creating a non-sticky agenda buffer
	     (setq-local org-agenda-this-buffer-is-sticky nil)))
      (mapc #'make-local-variable agenda-local-vars-to-keep)
      (reset-saved agenda-local-vars-to-keep)))
  (setq org-agenda-undo-list nil
	org-agenda-pending-undo-list nil
	org-agenda-bulk-marked-entries nil)
  (setq major-mode 'org-agenda-mode)
  ;; Keep global-font-lock-mode from turning on font-lock-mode
  (setq-local font-lock-global-modes (list 'not major-mode))
  (setq mode-name "Org-Agenda")
  (setq indent-tabs-mode nil)
  (use-local-map org-agenda-mode-map)
  (require 'org-mode)
  (defvar org-startup-truncated)
  (when org-startup-truncated (setq truncate-lines t))
  (setq-local line-move-visual nil)
  (add-hook 'post-command-hook #'org-agenda-update-agenda-type nil 'local)
  (add-hook 'pre-command-hook #'org-unhighlight nil 'local)
  ;; Make sure properties are removed when copying text
  (if (boundp 'filter-buffer-substring-functions)
      (add-hook 'filter-buffer-substring-functions
		(lambda (fun start end delete)
                  (substring-no-properties (funcall fun start end delete)))
		nil t)
    ;; Emacs >= 24.4.
    (add-function :filter-return (local 'filter-buffer-substring-function)
                  #'substring-no-properties))
  (unless org-agenda-keep-modes
    (setq org-agenda-follow-mode org-agenda-start-with-follow-mode
	  org-agenda-entry-text-mode org-agenda-start-with-entry-text-mode
	  org-agenda-show-log org-agenda-start-with-log-mode
	  org-agenda-clockreport-mode org-agenda-start-with-clockreport-mode
          org-agenda-archives-mode org-agenda-start-with-archives-mode))
  (add-to-invisibility-spec '(org-filtered))
  (add-to-invisibility-spec '(org-link))
  (easy-menu-change
   '("Agenda") "Agenda Files"
   (append
    (list
     (vector
      (if (get 'org-agenda-files 'org-restrict)
	  "Restricted to single file"
	"Edit File List")
      '(org-edit-agenda-file-list)
      (not (get 'org-agenda-files 'org-restrict)))
     "--")
    (mapcar #'org-file-menu-entry (org-agenda-files))))
  (org-agenda-set-mode-name)
  (run-mode-hooks 'org-agenda-mode-hook))

;;; Keymap

(declare-function org-agenda-ctrl-c-ctrl-c "org-agenda-commands-proxy" ())
(declare-function org-agenda-backward-block "org-agenda-commands" ())
(declare-function org-agenda-forward-block "org-agenda-commands" (&optional backward))
(declare-function org-agenda-show-mouse "org-agenda-commands" (ev))
(declare-function org-agenda-goto-mouse "org-agenda-commands" (ev))
(declare-function org-info-find-node "org-misc" (&optional nodename))
(declare-function org-mobile-push "org-mobile" ())
(declare-function org-mobile-pull "org-mobile" ())
(declare-function org-agenda-show-the-flagging-note "org-agenda-misc" ())
(declare-function org-timer-stop "org-timer" ())
(declare-function org-timer-set-timer "org-timer" (&optional opt))
(declare-function org-agenda-filter-by-top-headline "org-agenda-filter-commands" (strip))
(declare-function org-agenda-filter-by-category "org-agenda-filter-commands" (strip))
(declare-function org-agenda-filter-remove-all "org-agenda-filter-commands" ())
(declare-function org-agenda-filter "org-agenda-filter-commands" (&optional strip-or-accumulate))
(declare-function org-agenda-filter-by-regexp "org-agenda-filter-commands" (strip-or-accumulate))
(declare-function org-agenda-filter-by-effort "org-agenda-filter-commands" (strip-or-accumulate))
(declare-function org-agenda-filter-by-tag "org-agenda-filter-commands" (strip-or-accumulate &optional char exclude))
(declare-function org-agenda-manipulate-query-subtract-re "org-agenda-query" ())
(declare-function org-agenda-manipulate-query-add-re "org-agenda-query" ())
(declare-function org-agenda-manipulate-query-subtract "org-agenda-query" ())
(declare-function org-agenda-manipulate-query-add "org-agenda-query" ())
(declare-function org-agenda-earlier "org-agenda-query" ())
(declare-function org-agenda-later "org-agenda-query" ())
(declare-function org-agenda-priority-down "org-agenda-commands-proxy" ())
(declare-function org-agenda-priority-up "org-agenda-commands-proxy" ())
(declare-function org-agenda-clock-goto "org-agenda-commands-proxy" ())
(declare-function org-agenda-clock-cancel "org-agenda-commands-proxy" (&optional _arg))
(declare-function org-agenda-clock-in "org-agenda-commands-proxy" (&optional arg))
(declare-function org-agenda-clock-out "org-agenda-commands-proxy" ())
(declare-function org-clock-goto "org-clock-commands" (&optional select))
(declare-function org-agenda-holidays "org-agenda-misc" ())
(declare-function org-agenda-sunrise-sunset "org-agenda-misc" (arg))
(declare-function org-agenda-phases-of-moon "org-agenda-misc" ())
(declare-function org-agenda-convert-date "org-agenda-misc" ())
(declare-function org-agenda-goto-calendar "org-agenda-misc" ())
(declare-function org-agenda-diary-entry "org-agenda-diary" ())
(declare-function org-agenda-priority "org-agenda-commands-proxy" (&optional force-direction))
(declare-function org-attach "org-attach" ())
(declare-function org-agenda-next-date-line "org-agenda-commands" (&optional arg))
(declare-function org-agenda-previous-date-line "org-agenda-commands" (&optional arg))
(declare-function org-agenda-previous-item "org-agenda-commands" (n))
(declare-function org-agenda-next-item "org-agenda-commands" (n))
(declare-function org-agenda-previous-line "org-agenda-commands" ())
(declare-function org-agenda-next-line "org-agenda-commands" ())
(declare-function org-agenda-show-tags "org-agenda-misc" ())
(declare-function org-agenda-write "org-agenda-export" (file &optional open nosettings agenda-bufname))
(declare-function org-agenda-exit "org-agenda-commands" ())
(declare-function org-agenda-Quit "org-agenda-commands" ())
(declare-function org-agenda-quit "org-agenda-commands" ())
(declare-function org-agenda-set-property "org-agenda-commands-proxy" ())
(declare-function org-clock-modify-effort-estimate "org-clock-commands" (&optional value))
(declare-function org-agenda-set-effort "org-agenda-commands-proxy" ())
(declare-function org-agenda-toggle-time-grid "org-agenda-agenda-view" ())
(declare-function org-agenda-toggle-deadlines "org-agenda-agenda-view" ())
(declare-function org-agenda-toggle-diary "org-agenda-agenda-view" ())
(declare-function org-agenda-log-mode "org-agenda-agenda-view" (&optional special))
(declare-function org-agenda-clockreport-mode "org-agenda-agenda-view" ())
(declare-function org-agenda-view-mode-dispatch "org-agenda-commands" ())
(declare-function org-agenda-deadline "org-agenda-commands-proxy" (arg &optional time))
(declare-function org-agenda-schedule "org-agenda-commands-proxy" (arg &optional time))
(declare-function org-agenda-date-prompt "org-agenda-commands-proxy" (arg))
(declare-function org-agenda-add-note "org-agenda-commands-proxy" (&optional _arg))
(declare-function org-agenda-do-date-earlier "org-agenda-commands" (arg))
(declare-function org-agenda-do-date-later "org-agenda-commands" (arg))
(declare-function org-agenda-year-view "org-agenda-query" (&optional year))
(declare-function org-agenda-month-view "org-agenda-query" (&optional month))
(declare-function org-agenda-fortnight-view "org-agenda-query" (&optional iso-week))
(declare-function org-agenda-week-view "org-agenda-query" (&optional iso-week))
(declare-function org-agenda-day-view "org-agenda-query" (&optional day-of-month))
(declare-function org-agenda-goto-date "org-agenda-query" (date))
(declare-function org-agenda-goto-today "org-agenda-query" ())
(declare-function org-agenda-set-tags "org-agenda-commands-proxy" (&optional tag onoff))
(declare-function org-agenda-archive-to-archive-sibling "org-agenda-commands-proxy" ())
(declare-function org-agenda-todo "org-agenda-commands-proxy" (&optional arg))
(declare-function org-agenda-archive-default-with-confirmation "org-agenda-commands-proxy" ())
(declare-function org-agenda-recenter "org-agenda-commands" (arg))
(declare-function org-agenda-tree-to-indirect-buffer "org-agenda-commands" (arg))
(declare-function org-agenda-todo-previousset "org-agenda-commands-proxy" ())
(declare-function org-agenda-todo-nextset "org-agenda-commands-proxy" ())
(declare-function org-agenda-show-scroll-down "org-agenda-commands" ())
(declare-function org-agenda-show-and-scroll-up "org-agenda-commands" (&optional arg))
(declare-function org-agenda-open-link "org-agenda-commands" (&optional arg))
(declare-function org-agenda-archive "org-agenda-commands-proxy" ())
(declare-function org-agenda-toggle-archive-tag "org-agenda-commands-proxy" ())
(declare-function org-agenda-archive-default "org-agenda-commands-proxy" ())
(declare-function org-agenda-append-agenda "org-agenda-commands" ())
(declare-function org-agenda-capture "org-agenda-commands-proxy" (&optional with-time))
(declare-function org-agenda-bulk-action "org-agenda-bulk-edit" (&optional arg))
(declare-function org-agenda-bulk-unmark-all "org-agenda-bulk-edit" ())
(declare-function org-agenda-bulk-unmark "org-agenda-bulk-edit" (&optional arg))
(declare-function org-agenda-bulk-mark-regexp "org-agenda-bulk-edit" (regexp))
(declare-function org-agenda-bulk-toggle-all "org-agenda-bulk-edit" ())
(declare-function org-agenda-bulk-toggle "org-agenda-bulk-edit" ())
(declare-function org-agenda-bulk-mark "org-agenda-bulk-edit" (&optional arg))
(declare-function org-agenda-bulk-mark-all "org-agenda-bulk-edit" ())
(declare-function org-agenda-drag-line-forward "org-agenda-commands" (arg &optional backward))
(declare-function org-agenda-drag-line-backward "org-agenda-commands" (arg))
(declare-function org-agenda-refile "org-agenda-commands-proxy" (&optional goto rfloc no-update))
(declare-function org-agenda-kill "org-agenda-commands-proxy" ())
(declare-function org-agenda-switch-to "org-agenda-commands" (&optional delete-other-windows))

(defcustom org-agenda-mouse-1-follows-link nil
  "Non-nil means \\`mouse-1' on a link will follow the link in the agenda.
A longer mouse click will still set point.  Needs to be set
before org.el is loaded."
  :group 'org-agenda-startup
  :type 'boolean)

(org-remap org-agenda-mode-map 'move-end-of-line 'org-agenda-end-of-line)

(substitute-key-definition #'undo #'org-agenda-undo
			   org-agenda-mode-map global-map)
(org-defkey org-agenda-mode-map "\C-i" #'org-agenda-goto)
(org-defkey org-agenda-mode-map [(tab)] #'org-agenda-goto)
(org-defkey org-agenda-mode-map "\C-m" #'org-agenda-switch-to)
(org-defkey org-agenda-mode-map "\C-k" #'org-agenda-kill)
(org-defkey org-agenda-mode-map "\C-c\C-w" #'org-agenda-refile)
(org-defkey org-agenda-mode-map [(meta down)] #'org-agenda-drag-line-forward)
(org-defkey org-agenda-mode-map [(meta up)] #'org-agenda-drag-line-backward)
(org-defkey org-agenda-mode-map "m" #'org-agenda-bulk-mark)
(org-defkey org-agenda-mode-map "\M-m" #'org-agenda-bulk-toggle)
(org-defkey org-agenda-mode-map "*" #'org-agenda-bulk-mark-all)
(org-defkey org-agenda-mode-map "\M-*" #'org-agenda-bulk-toggle-all)
(org-defkey org-agenda-mode-map "#" #'org-agenda-dim-blocked-tasks)
(org-defkey org-agenda-mode-map "%" #'org-agenda-bulk-mark-regexp)
(org-defkey org-agenda-mode-map "u" #'org-agenda-bulk-unmark)
(org-defkey org-agenda-mode-map "U" #'org-agenda-bulk-unmark-all)
(org-defkey org-agenda-mode-map "B" #'org-agenda-bulk-action)
(org-defkey org-agenda-mode-map "k" #'org-agenda-capture)
(org-defkey org-agenda-mode-map "A" #'org-agenda-append-agenda)
(org-defkey org-agenda-mode-map "\C-c\C-x!" #'org-reload)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-a" #'org-agenda-archive-default)
(org-defkey org-agenda-mode-map "\C-c\C-xa" #'org-agenda-toggle-archive-tag)
(org-defkey org-agenda-mode-map "\C-c\C-xA" #'org-agenda-archive-to-archive-sibling)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-s" #'org-agenda-archive)
(org-defkey org-agenda-mode-map "\C-c$" #'org-agenda-archive)
(org-defkey org-agenda-mode-map "$" #'org-agenda-archive)
(org-defkey org-agenda-mode-map "\C-c\C-o" #'org-agenda-open-link)
(org-defkey org-agenda-mode-map " " #'org-agenda-show-and-scroll-up)
(org-defkey org-agenda-mode-map [backspace] #'org-agenda-show-scroll-down)
(org-defkey org-agenda-mode-map "\d" #'org-agenda-show-scroll-down)
(org-defkey org-agenda-mode-map [(control shift right)] #'org-agenda-todo-nextset)
(org-defkey org-agenda-mode-map [(control shift left)] #'org-agenda-todo-previousset)
(org-defkey org-agenda-mode-map "\C-c\C-xb" #'org-agenda-tree-to-indirect-buffer)
(org-defkey org-agenda-mode-map "o" #'delete-other-windows)
(org-defkey org-agenda-mode-map "L" #'org-agenda-recenter)
(org-defkey org-agenda-mode-map "\C-c\C-t" #'org-agenda-todo)
(org-defkey org-agenda-mode-map "t" #'org-agenda-todo)
(org-defkey org-agenda-mode-map "a" #'org-agenda-archive-default-with-confirmation)
(org-defkey org-agenda-mode-map ":" #'org-agenda-set-tags)
(org-defkey org-agenda-mode-map "\C-c\C-q" #'org-agenda-set-tags)
(org-defkey org-agenda-mode-map "." #'org-agenda-goto-today)
(org-defkey org-agenda-mode-map "j" #'org-agenda-goto-date)
(org-defkey org-agenda-mode-map "d" #'org-agenda-day-view)
(org-defkey org-agenda-mode-map "w" #'org-agenda-week-view)
(org-defkey org-agenda-mode-map "y" #'org-agenda-year-view)
(org-defkey org-agenda-mode-map "\C-c\C-z" #'org-agenda-add-note)
(org-defkey org-agenda-mode-map "z" #'org-agenda-add-note)
(org-defkey org-agenda-mode-map [(shift right)] #'org-agenda-do-date-later)
(org-defkey org-agenda-mode-map [(shift left)] #'org-agenda-do-date-earlier)
(org-defkey org-agenda-mode-map [?\C-c ?\C-x (right)] #'org-agenda-do-date-later)
(org-defkey org-agenda-mode-map [?\C-c ?\C-x (left)] #'org-agenda-do-date-earlier)
(org-defkey org-agenda-mode-map ">" #'org-agenda-date-prompt)
(org-defkey org-agenda-mode-map "\C-c\C-s" #'org-agenda-schedule)
(org-defkey org-agenda-mode-map "\C-c\C-d" #'org-agenda-deadline)
(let ((l '(1 2 3 4 5 6 7 8 9 0)))
  (while l (org-defkey org-agenda-mode-map
		       (number-to-string (pop l)) #'digit-argument)))
(org-defkey org-agenda-mode-map "F" #'org-agenda-follow-mode)
(org-defkey org-agenda-mode-map "R" #'org-agenda-clockreport-mode)
(org-defkey org-agenda-mode-map "E" #'org-agenda-entry-text-mode)
(org-defkey org-agenda-mode-map "l" #'org-agenda-log-mode)
(org-defkey org-agenda-mode-map "v" #'org-agenda-view-mode-dispatch)
(org-defkey org-agenda-mode-map "D" #'org-agenda-toggle-diary)
(org-defkey org-agenda-mode-map "!" #'org-agenda-toggle-deadlines)
(org-defkey org-agenda-mode-map "G" #'org-agenda-toggle-time-grid)
(org-defkey org-agenda-mode-map "r" #'org-agenda-redo)
(org-defkey org-agenda-mode-map "g" #'org-agenda-redo-all)
(org-defkey org-agenda-mode-map "e" #'org-agenda-set-effort)
(org-defkey org-agenda-mode-map "\C-c\C-xe" #'org-agenda-set-effort)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-e"
	    #'org-clock-modify-effort-estimate)
(org-defkey org-agenda-mode-map "\C-c\C-xp" #'org-agenda-set-property)
(org-defkey org-agenda-mode-map "q" #'org-agenda-quit)
(org-defkey org-agenda-mode-map "Q" #'org-agenda-Quit)
(org-defkey org-agenda-mode-map "x" #'org-agenda-exit)
(org-defkey org-agenda-mode-map "\C-x\C-w" #'org-agenda-write)
(org-defkey org-agenda-mode-map "\C-x\C-s" #'org-save-all-org-buffers)
(org-defkey org-agenda-mode-map "s" #'org-save-all-org-buffers)
(org-defkey org-agenda-mode-map "T" #'org-agenda-show-tags)
(org-defkey org-agenda-mode-map "n" #'org-agenda-next-line)
(org-defkey org-agenda-mode-map "p" #'org-agenda-previous-line)
(org-defkey org-agenda-mode-map "N" #'org-agenda-next-item)
(org-defkey org-agenda-mode-map "P" #'org-agenda-previous-item)
(substitute-key-definition #'next-line #'org-agenda-next-line
			   org-agenda-mode-map global-map)
(substitute-key-definition #'previous-line #'org-agenda-previous-line
			   org-agenda-mode-map global-map)
(org-defkey org-agenda-mode-map "\C-c\C-a" #'org-attach)
(org-defkey org-agenda-mode-map "\C-c\C-n" #'org-agenda-next-date-line)
(org-defkey org-agenda-mode-map "\C-c\C-p" #'org-agenda-previous-date-line)
(org-defkey org-agenda-mode-map "\C-c," #'org-agenda-priority)
(org-defkey org-agenda-mode-map "," #'org-agenda-priority)
(org-defkey org-agenda-mode-map "i" #'org-agenda-diary-entry)
(org-defkey org-agenda-mode-map "c" #'org-agenda-goto-calendar)
(org-defkey org-agenda-mode-map "C" #'org-agenda-convert-date)
(org-defkey org-agenda-mode-map "M" #'org-agenda-phases-of-moon)
(org-defkey org-agenda-mode-map "S" #'org-agenda-sunrise-sunset)
(org-defkey org-agenda-mode-map "h" #'org-agenda-holidays)
(org-defkey org-agenda-mode-map "H" #'org-agenda-holidays)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-i" #'org-agenda-clock-in)
(org-defkey org-agenda-mode-map "I" #'org-agenda-clock-in)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-o" #'org-agenda-clock-out)
(org-defkey org-agenda-mode-map "O" #'org-agenda-clock-out)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-x" #'org-agenda-clock-cancel)
(org-defkey org-agenda-mode-map "X" #'org-agenda-clock-cancel)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-j" #'org-clock-goto)
(org-defkey org-agenda-mode-map "J" #'org-agenda-clock-goto)
(org-defkey org-agenda-mode-map "+" #'org-agenda-priority-up)
(org-defkey org-agenda-mode-map "-" #'org-agenda-priority-down)
(org-defkey org-agenda-mode-map [(shift up)] #'org-agenda-priority-up)
(org-defkey org-agenda-mode-map [(shift down)] #'org-agenda-priority-down)
(org-defkey org-agenda-mode-map [?\C-c ?\C-x (up)] #'org-agenda-priority-up)
(org-defkey org-agenda-mode-map [?\C-c ?\C-x (down)] #'org-agenda-priority-down)
(org-defkey org-agenda-mode-map "f" #'org-agenda-later)
(org-defkey org-agenda-mode-map "b" #'org-agenda-earlier)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-c" #'org-agenda-columns)
(org-defkey org-agenda-mode-map "\C-c\C-x>" #'org-agenda-remove-restriction-lock)
(org-defkey org-agenda-mode-map "\C-c\C-x<" #'org-agenda-set-restriction-lock-from-agenda)
(org-defkey org-agenda-mode-map "[" #'org-agenda-manipulate-query-add)
(org-defkey org-agenda-mode-map "]" #'org-agenda-manipulate-query-subtract)
(org-defkey org-agenda-mode-map "{" #'org-agenda-manipulate-query-add-re)
(org-defkey org-agenda-mode-map "}" #'org-agenda-manipulate-query-subtract-re)
(org-defkey org-agenda-mode-map "\\" #'org-agenda-filter-by-tag)
(org-defkey org-agenda-mode-map "_" #'org-agenda-filter-by-effort)
(org-defkey org-agenda-mode-map "=" #'org-agenda-filter-by-regexp)
(org-defkey org-agenda-mode-map "/" #'org-agenda-filter)
(org-defkey org-agenda-mode-map "|" #'org-agenda-filter-remove-all)
(org-defkey org-agenda-mode-map "~" #'org-agenda-limit-interactively)
(org-defkey org-agenda-mode-map "<" #'org-agenda-filter-by-category)
(org-defkey org-agenda-mode-map "^" #'org-agenda-filter-by-top-headline)
(org-defkey org-agenda-mode-map ";" #'org-timer-set-timer)
(org-defkey org-agenda-mode-map "\C-c\C-x_" #'org-timer-stop)
(org-defkey org-agenda-mode-map "?" #'org-agenda-show-the-flagging-note)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-mg" #'org-mobile-pull)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-mp" #'org-mobile-push)
(org-defkey org-agenda-mode-map "\C-c\C-xI" #'org-info-find-node)
(org-defkey org-agenda-mode-map [mouse-2] #'org-agenda-goto-mouse)
(org-defkey org-agenda-mode-map [mouse-3] #'org-agenda-show-mouse)
(org-defkey org-agenda-mode-map [remap forward-paragraph] #'org-agenda-forward-block)
(org-defkey org-agenda-mode-map [remap backward-paragraph] #'org-agenda-backward-block)
(org-defkey org-agenda-mode-map "\C-c\C-c" #'org-agenda-ctrl-c-ctrl-c)

(when org-agenda-mouse-1-follows-link
  (org-defkey org-agenda-mode-map [follow-link] 'mouse-face))

;;; Agenda menu

(easy-menu-define org-agenda-menu org-agenda-mode-map "Agenda menu."
  '("Agenda"
    ("Agenda Files")
    "--"
    ("Agenda Dates"
     ["Goto Today" org-agenda-goto-today (org-agenda-check-type nil 'agenda)]
     ["Next Dates" org-agenda-later (org-agenda-check-type nil 'agenda)]
     ["Previous Dates" org-agenda-earlier (org-agenda-check-type nil 'agenda)]
     ["Jump to date" org-agenda-goto-date (org-agenda-check-type nil 'agenda)])
    "--"
    ("View"
     ["Day View" org-agenda-day-view
      :active (org-agenda-check-type nil 'agenda)
      :style radio :selected (eq org-agenda-current-span 'day)
      :keys "v d  (or just d)"]
     ["Week View" org-agenda-week-view
      :active (org-agenda-check-type nil 'agenda)
      :style radio :selected (eq org-agenda-current-span 'week)
      :keys "v w"]
     ["Fortnight View" org-agenda-fortnight-view
      :active (org-agenda-check-type nil 'agenda)
      :style radio :selected (eq org-agenda-current-span 'fortnight)
      :keys "v t"]
     ["Month View" org-agenda-month-view
      :active (org-agenda-check-type nil 'agenda)
      :style radio :selected (eq org-agenda-current-span 'month)
      :keys "v m"]
     ["Year View" org-agenda-year-view
      :active (org-agenda-check-type nil 'agenda)
      :style radio :selected (eq org-agenda-current-span 'year)
      :keys "v y"]
     "--"
     ["Include Diary" org-agenda-toggle-diary
      :style toggle :selected org-agenda-include-diary
      :active (org-agenda-check-type nil 'agenda)]
     ["Include Deadlines" org-agenda-toggle-deadlines
      :style toggle :selected org-agenda-include-deadlines
      :active (org-agenda-check-type nil 'agenda)]
     ["Use Time Grid" org-agenda-toggle-time-grid
      :style toggle :selected org-agenda-use-time-grid
      :active (org-agenda-check-type nil 'agenda)]
     "--"
     ["Show clock report" org-agenda-clockreport-mode
      :style toggle :selected org-agenda-clockreport-mode
      :active (org-agenda-check-type nil 'agenda)]
     ["Show some entry text" org-agenda-entry-text-mode
      :style toggle :selected org-agenda-entry-text-mode
      :active t]
     "--"
     ["Show Logbook entries" org-agenda-log-mode
      :style toggle :selected org-agenda-show-log
      :active (org-agenda-check-type nil 'agenda)
      :keys "v l (or just l)"]
     ["Include archived trees" org-agenda-archives-mode
      :style toggle :selected org-agenda-archives-mode :active t
      :keys "v a"]
     ["Include archive files" (org-agenda-archives-mode t)
      :style toggle :selected (eq org-agenda-archives-mode t) :active t
      :keys "v A"]
     "--"
     ["Remove Restriction" org-agenda-remove-restriction-lock org-agenda-restrict])
    ("Filter current view"
     ["with generic interface" org-agenda-filter t]
     "--"
     ["by category at cursor" org-agenda-filter-by-category t]
     ["by tag" org-agenda-filter-by-tag t]
     ["by effort" org-agenda-filter-by-effort t]
     ["by regexp" org-agenda-filter-by-regexp t]
     ["by top-level headline" org-agenda-filter-by-top-headline t]
     "--"
     ["Remove all filtering" org-agenda-filter-remove-all t]
     "--"
     ["limit" org-agenda-limit-interactively t])
    ["Rebuild buffer" org-agenda-redo t]
    ["Write view to file" org-agenda-write t]
    ["Save all Org buffers" org-save-all-org-buffers t]
    "--"
    ["Show original entry" org-agenda-show t]
    ["Go To (other window)" org-agenda-goto t]
    ["Go To (this window)" org-agenda-switch-to t]
    ["Capture with cursor date" org-agenda-capture t]
    ["Follow Mode" org-agenda-follow-mode
     :style toggle :selected org-agenda-follow-mode :active t]
    ;;    ["Tree to indirect frame" org-agenda-tree-to-indirect-buffer t]
    "--"
    ("TODO"
     ["Cycle TODO" org-agenda-todo t]
     ["Next TODO set" org-agenda-todo-nextset t]
     ["Previous TODO set" org-agenda-todo-previousset t]
     ["Add note" org-agenda-add-note t])
    ("Archive/Refile/Delete"
     ["Archive default" org-agenda-archive-default t]
     ["Archive default" org-agenda-archive-default-with-confirmation t]
     ["Toggle ARCHIVE tag" org-agenda-toggle-archive-tag t]
     ["Move to archive sibling" org-agenda-archive-to-archive-sibling t]
     ["Archive subtree" org-agenda-archive t]
     "--"
     ["Refile" org-agenda-refile t]
     "--"
     ["Delete subtree" org-agenda-kill t])
    ("Bulk action"
     ["Mark entry" org-agenda-bulk-mark t]
     ["Mark all" org-agenda-bulk-mark-all t]
     ["Unmark entry" org-agenda-bulk-unmark t]
     ["Unmark all" org-agenda-bulk-unmark-all :active t :keys "U"]
     ["Toggle mark" org-agenda-bulk-toggle t]
     ["Toggle all" org-agenda-bulk-toggle-all t]
     ["Mark regexp" org-agenda-bulk-mark-regexp t])
    ["Act on all marked" org-agenda-bulk-action t]
    "--"
    ("Tags and Properties"
     ["Show all Tags" org-agenda-show-tags t]
     ["Set Tags current line" org-agenda-set-tags (not (use-region-p))]
     ["Change tag in region" org-agenda-set-tags (use-region-p)]
     "--"
     ["Column View" org-columns t])
    ("Deadline/Schedule"
     ["Schedule" org-agenda-schedule t]
     ["Set Deadline" org-agenda-deadline t]
     "--"
     ["Change Date +1 day" org-agenda-date-later (org-agenda-check-type nil 'agenda)]
     ["Change Date -1 day" org-agenda-date-earlier (org-agenda-check-type nil 'agenda)]
     ["Change Time +1 hour" org-agenda-do-date-later :active (org-agenda-check-type nil 'agenda) :keys "C-u S-right"]
     ["Change Time -1 hour" org-agenda-do-date-earlier :active (org-agenda-check-type nil 'agenda) :keys "C-u S-left"]
     ["Change Time +  min" org-agenda-date-later :active (org-agenda-check-type nil 'agenda) :keys "C-u C-u S-right"]
     ["Change Time -  min" org-agenda-date-earlier :active (org-agenda-check-type nil 'agenda) :keys "C-u C-u S-left"]
     ["Change Date to ..." org-agenda-date-prompt (org-agenda-check-type nil 'agenda)])
    ("Clock and Effort"
     ["Clock in" org-agenda-clock-in t]
     ["Clock out" org-agenda-clock-out t]
     ["Clock cancel" org-agenda-clock-cancel t]
     ["Goto running clock" org-clock-goto t]
     "--"
     ["Set Effort" org-agenda-set-effort t]
     ["Change clocked effort" org-clock-modify-effort-estimate
      (org-clock-is-active)])
    ("Priority"
     ["Set Priority" org-agenda-priority t]
     ["Increase Priority" org-agenda-priority-up t]
     ["Decrease Priority" org-agenda-priority-down t]
     ["Show Priority" org-priority-show t])
    ("Calendar/Diary"
     ["New Diary Entry" org-agenda-diary-entry (org-agenda-check-type nil 'agenda)]
     ["Goto Calendar" org-agenda-goto-calendar (org-agenda-check-type nil 'agenda)]
     ["Phases of the Moon" org-agenda-phases-of-moon (org-agenda-check-type nil 'agenda)]
     ["Sunrise/Sunset" org-agenda-sunrise-sunset (org-agenda-check-type nil 'agenda)]
     ["Holidays" org-agenda-holidays (org-agenda-check-type nil 'agenda)]
     ["Convert" org-agenda-convert-date (org-agenda-check-type nil 'agenda)]
     "--"
     ["Create iCalendar File" org-icalendar-combine-agenda-files t])
    "--"
    ["Undo Remote Editing" org-agenda-undo org-agenda-undo-list]
    "--"
    ("MobileOrg"
     ["Push Files and Views" org-mobile-push t]
     ["Get Captured and Flagged" org-mobile-pull t]
     ["Find FLAGGED Tasks" (org-agenda nil "?") :active t :keys "\\[org-agenda] ?"]
     ["Show note / unflag" org-agenda-show-the-flagging-note t]
     "--"
     ["Setup" (progn (require 'org-mobile) (customize-group 'org-mobile)) t])
    "--"
    ["Quit" org-agenda-quit t]
    ["Exit and Release Buffers" org-agenda-exit t]
    ))

;;; Sticky agendas - multiple agenda buffers

(defun org-agenda-use-sticky-p ()
  "Return non-nil if an agenda buffer named
`org-agenda-buffer-name' exists and should be shown instead of
generating a new one."
  (and
   ;; turned off by user
   org-agenda-sticky
   ;; For multi-agenda buffer already exists
   (not org-agenda-multi)
   ;; buffer found
   (get-buffer org-agenda-buffer-name)
   ;; C-u parameter is same as last call
   (with-current-buffer org-agenda-buffer-name
     (and
      (equal current-prefix-arg
	     org-agenda-last-prefix-arg)
      ;; In case user turned stickiness on, while having existing
      ;; Agenda buffer active, don't reuse that buffer, because it
      ;; does not have org variables local
      org-agenda-this-buffer-is-sticky))))

;;; Redo agenda buffer

(declare-function org-columns-quit "org-colview" ())
(defun org-agenda-redo (&optional all)
  "Rebuild possibly ALL agenda view(s) in the current buffer."
  (interactive "P")
  (defvar org-agenda-tag-filter-while-redo) ;FIXME: Where is this var used?
  (let* ((p (or (and (looking-at "\\'") (1- (point))) (point)))
	 (cpa (unless (eq all t) current-prefix-arg))
	 (org-agenda-doing-sticky-redo org-agenda-sticky)
	 (org-agenda-sticky nil)
	 (org-agenda-buffer-name (or org-agenda-this-buffer-name
				     org-agenda-buffer-name))
	 (org-agenda-keep-modes t)
	 (tag-filter org-agenda-tag-filter)
	 (tag-preset (assoc-default 'tag org-agenda-filters-preset))
	 (top-hl-filter org-agenda-top-headline-filter)
	 (cat-filter org-agenda-category-filter)
	 (cat-preset (assoc-default 'category org-agenda-filters-preset))
	 (re-filter org-agenda-regexp-filter)
	 (re-preset (assoc-default 'regexp org-agenda-filters-preset))
	 (effort-filter org-agenda-effort-filter)
	 (effort-preset (assoc-default 'effort org-agenda-filters-preset))
	 (org-agenda-tag-filter-while-redo (or tag-filter tag-preset))
	 (cols org-agenda-columns-active)
	 (line (org-current-line))
	 (window-line (- line (org-current-line (window-start))))
	 (lprops (get-text-property p 'org-lprops))
	 (redo-cmd (get-text-property p 'org-redo-cmd))
	 (last-args (get-text-property p 'org-last-args))
	 (org-agenda-overriding-cmd (get-text-property p 'org-series-cmd))
	 (org-agenda-overriding-cmd-arguments
	  (unless (eq all t)
	    (cond ((listp last-args)
		   (cons (or cpa (car last-args)) (cdr last-args)))
		  ((stringp last-args)
		   last-args))))
	 (series-redo-cmd (get-text-property p 'org-series-redo-cmd)))
    (and cols (org-columns-quit))
    (message "Rebuilding agenda buffer...")
    (if series-redo-cmd
	(eval series-redo-cmd t)
      (cl-progv
	  (mapcar #'car lprops)
	  (mapcar (lambda (binding) (eval (cadr binding) t)) lprops)
	(eval redo-cmd t))
      (let ((inhibit-read-only t))
	(add-text-properties (point-min) (point-max) `(org-lprops ,lprops))))
    (setq org-agenda-undo-list nil
	  org-agenda-pending-undo-list nil
	  org-agenda-tag-filter tag-filter
	  org-agenda-category-filter cat-filter
	  org-agenda-regexp-filter re-filter
	  org-agenda-effort-filter effort-filter
	  org-agenda-top-headline-filter top-hl-filter)
    (message "Rebuilding agenda buffer...done")
    (let ((tag (or tag-filter tag-preset))
	  (cat (or cat-filter cat-preset))
	  (effort (or effort-filter effort-preset))
	  (re (or re-filter re-preset)))
      (when tag (org-agenda-filter-apply tag 'tag t))
      (when cat (org-agenda-filter-apply cat 'category))
      (when effort (org-agenda-filter-apply effort 'effort))
      (when re  (org-agenda-filter-apply re 'regexp)))
    (and top-hl-filter (org-agenda-filter-top-headline-apply top-hl-filter))
    (and cols (called-interactively-p 'any) (org-agenda-columns))
    (org-goto-line line)
    (when (called-interactively-p 'any) (recenter window-line))))

(defun org-agenda-redo-all (&optional exhaustive)
  "Rebuild all agenda views in the current buffer.
With a prefix argument, do so in all agenda buffers."
  (interactive "P")
  (if exhaustive
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'org-agenda-mode)
            (org-agenda-redo t))))
    (org-agenda-redo t)))

(defun org-agenda-maybe-redo ()
  "If there is any window showing the agenda view, update it."
  (let ((w (get-buffer-window (or org-agenda-this-buffer-name
				  org-agenda-buffer-name)
			      t))
	(w0 (selected-window)))
    (when w
      (select-window w)
      (org-agenda-redo)
      (select-window w0)
      (if org-agenda-overriding-restriction
	  (message "Agenda view shifted to new %s restriction"
		   org-agenda-overriding-restriction)
	(message "Agenda restriction lock removed")))))

;;; Archives mode

(defun org-agenda-archives-mode (&optional with-files)
  "Toggle inclusion of items in trees marked with :ARCHIVE:.
When called with a prefix argument, include all archive files as well."
  (interactive "P")
  (setq org-agenda-archives-mode
	(cond ((and with-files (eq org-agenda-archives-mode t)) nil)
	      (with-files t)
	      (org-agenda-archives-mode nil)
	      (t 'trees)))
  (org-agenda-set-mode-name)
  (org-agenda-redo)
  (message
   "%s"
   (cond
    ((eq org-agenda-archives-mode nil)
     "No archives are included")
    ((eq org-agenda-archives-mode 'trees)
     (format "Trees with :%s: tag are included" org-archive-tag))
    ((eq org-agenda-archives-mode t)
     (format "Trees with :%s: tag and all active archive files are included"
	     org-archive-tag)))))

;;; Follow mode

(defcustom org-agenda-follow-indirect nil
  "Non-nil means `org-agenda-follow-mode' displays only the
current item's tree, in an indirect buffer."
  :group 'org-agenda
  :version "24.1"
  :type 'boolean)

(defvar org-agenda-after-show-hook nil
  "Normal hook run after an item has been shown from the agenda.
Point is in the buffer where the item originated.")

(defcustom org-agenda-show-outline-path t
  "Non-nil means show outline path in echo area after line motion.

If set to `title', show outline path with prepended document
title.  Fallback to file name is no title is present."
  :group 'org-agenda-startup
  :type '(choice
	  (const :tag "Don't show outline path in agenda view." nil)
	  (const :tag "Show outline path with prepended file name." t)
	  (const :tag "Show outline path with prepended document title." title))
  :package-version '(Org . "9.6"))

(defvar org-agenda-pre-follow-window-conf nil)
(defun org-agenda-follow-mode ()
  "Toggle follow mode in an agenda buffer."
  (interactive)
  (unless org-agenda-follow-mode
    (setq org-agenda-pre-follow-window-conf
	  (current-window-configuration)))
  (setq org-agenda-follow-mode (not org-agenda-follow-mode))
  (unless org-agenda-follow-mode
    (set-window-configuration org-agenda-pre-follow-window-conf))
  (org-agenda-set-mode-name)
  (org-agenda-do-context-action)
  (message "Follow mode is %s"
	   (if org-agenda-follow-mode "on" "off")))

(defvar org-indirect-buffer-display) ; defined in org-indirect-buffer.el
(defun org-agenda-do-context-action ()
  "Show outline path and, maybe, follow mode window."
  (let ((m (org-get-at-bol 'org-marker)))
    (when (and (markerp m) (marker-buffer m))
      (and org-agenda-follow-mode
	   (if org-agenda-follow-indirect
               (let ((org-indirect-buffer-display 'other-window))
	         (org-agenda-tree-to-indirect-buffer nil))
	     (org-agenda-show)))
      (and org-agenda-show-outline-path
	   (org-with-point-at m (org-display-outline-path org-agenda-show-outline-path))))))

(defun org-agenda-show (&optional full-entry)
  "Display the Org file which contains the item at point.
With prefix argument FULL-ENTRY, make the entire entry visible
if it was hidden in the outline."
  (interactive "P")
  (let ((win (selected-window)))
    (org-agenda-goto t)
    (when full-entry (org-fold-show-entry 'hide-drawers))
    (select-window win)))

(defun org-agenda-goto (&optional highlight)
  "Go to the entry at point in the corresponding Org file."
  (interactive)
  (let* ((marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (switch-to-buffer-other-window buffer)
    (widen)
    (push-mark)
    (goto-char pos)
    (when (derived-mode-p 'org-mode)
      (org-fold-show-context 'agenda)
      (recenter (/ (window-height) 2))
      (org-back-to-heading t)
      (let ((case-fold-search nil))
	(when (re-search-forward (org-complex-heading-regexp) nil t)
	  (goto-char (match-beginning 4)))))
    (run-hooks 'org-agenda-after-show-hook)
    (and highlight (org-highlight (line-beginning-position)
                                  (line-end-position)))))

;;; Agenda filters

(defvar org-agenda-filter-form nil)
(defvar org-agenda-filtered-by-category nil)
(defvar org-agenda-filtered-by-top-headline nil)

(defsubst org-agenda-get-category ()
  "Return the category of the agenda line."
  (org-get-at-bol 'org-category))

(defconst org-agenda-filter-variables
  '((category . org-agenda-category-filter)
    (tag . org-agenda-tag-filter)
    (effort . org-agenda-effort-filter)
    (regexp . org-agenda-regexp-filter))
  "Alist of filter types and associated variables.")
(defun org-agenda-filter-any ()
  "Is any filter active?"
  (cl-some (lambda (x)
	     (or (symbol-value (cdr x))
                 (assoc-default (car x) org-agenda-filters-preset)))
	   org-agenda-filter-variables))

(defun org-agenda-apply-all-filters ()
  "Apply all the filters in current agenda buffer."
  (when org-agenda-top-headline-filter
    (org-agenda-filter-top-headline-apply
     org-agenda-top-headline-filter))
  (when org-agenda-tag-filter
    (org-agenda-filter-apply org-agenda-tag-filter 'tag t))
  (when (assoc-default 'tag org-agenda-filters-preset)
    (org-agenda-filter-apply
     (assoc-default 'tag org-agenda-filters-preset) 'tag t))
  (when org-agenda-category-filter
    (org-agenda-filter-apply org-agenda-category-filter 'category))
  (when (assoc-default 'category org-agenda-filters-preset)
    (org-agenda-filter-apply
     (assoc-default 'category org-agenda-filters-preset) 'category))
  (when org-agenda-regexp-filter
    (org-agenda-filter-apply org-agenda-regexp-filter 'regexp))
  (when (assoc-default 'regexp org-agenda-filters-preset)
    (org-agenda-filter-apply
     (assoc-default 'regexp org-agenda-filters-preset) 'regexp))
  (when org-agenda-effort-filter
    (org-agenda-filter-apply org-agenda-effort-filter 'effort))
  (when (assoc-default 'effort org-agenda-filters-preset)
    (org-agenda-filter-apply
     (assoc-default 'effort org-agenda-filters-preset) 'effort)))

(defun org-agenda-reapply-filters ()
  "Re-apply all agenda filters."
  (mapcar
   (lambda (f) (when (car f) (org-agenda-filter-apply (car f) (cadr f) t)))
   `((,org-agenda-tag-filter tag)
     (,org-agenda-category-filter category)
     (,org-agenda-regexp-filter regexp)
     (,org-agenda-effort-filter effort)
     (,(assoc-default 'tag org-agenda-filters-preset) tag)
     (,(assoc-default 'category org-agenda-filters-preset) category)
     (,(assoc-default 'effort org-agenda-filters-preset) effort)
     (,(assoc-default 'regexp org-agenda-filters-preset) regexp))))

(defun org-agenda-filter-apply (filter type &optional expand)
  "Set FILTER as the new agenda filter and apply it.
Optional argument EXPAND can be used for the TYPE tag and will
expand the tags in the FILTER if any of the tags in FILTER are
grouptags."
  ;; Deactivate `org-agenda-entry-text-mode' when filtering
  (when org-agenda-entry-text-mode (org-agenda-entry-text-mode))
  (setq org-agenda-filter-form (org-agenda-filter-make-matcher
				filter type expand))
  ;; Only set `org-agenda-filtered-by-category' to t when a unique
  ;; category is used as the filter:
  (setq org-agenda-filtered-by-category
	(and (eq type 'category)
	     (not (equal (substring (car filter) 0 1) "-"))))
  (org-agenda-set-mode-name)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (or (org-get-at-bol 'org-hd-marker)
		(org-get-at-bol 'org-marker))
	(org-dlet
	    ((tags (org-get-at-bol 'tags))
	     (cat (org-agenda-get-category))
	     (txt (or (org-get-at-bol 'txt) "")))
	  (unless (eval org-agenda-filter-form t)
	    (org-agenda-filter-hide-line type))))
      (forward-line 1)))
  (when (get-char-property (point) 'invisible)
    (ignore-errors (org-agenda-previous-line))))

(defun org-agenda-filter-make-matcher (filter type &optional expand)
  "Create the form that tests a line for agenda filter.
Optional argument EXPAND can be used for the TYPE tag and will
expand the tags in the FILTER if any of the tags in FILTER are
grouptags."
  (let ((multi-pos-cats
	 (and (eq type 'category)
	      (string-match-p "\\+.*\\+"
			      (mapconcat (lambda (cat) (substring cat 0 1))
					 filter ""))))
	f f1)
    (cond
     ;; Tag filter
     ((eq type 'tag)
      (setq filter
	    (delete-dups
	     (append (assoc-default 'tag org-agenda-filters-preset)
		     filter)))
      (dolist (x filter)
	(let ((op (string-to-char x)))
	  (if expand (setq x (org-agenda-filter-expand-tags (list x) t))
	    (setq x (list x)))
	  (setq f1 (org-agenda-filter-make-matcher-tag-exp x op))
	  (push f1 f))))
     ;; Category filter
     ((eq type 'category)
      (setq filter
	    (delete-dups
	     (append (assoc-default 'category org-agenda-filters-preset)
		     filter)))
      (dolist (x filter)
	(if (equal "-" (substring x 0 1))
	    (setq f1 (list 'not (list 'equal (substring x 1) 'cat)))
	  (setq f1 (list 'equal (substring x 1) 'cat)))
	(push f1 f)))
     ;; Regexp filter
     ((eq type 'regexp)
      (setq filter
	    (delete-dups
	     (append (assoc-default 'regexp org-agenda-filters-preset)
		     filter)))
      (dolist (x filter)
	(if (equal "-" (substring x 0 1))
	    (setq f1 (list 'not (list 'string-match (substring x 1) 'txt)))
	  (setq f1 (list 'string-match (substring x 1) 'txt)))
	(push f1 f)))
     ;; Effort filter
     ((eq type 'effort)
      (setq filter
	    (delete-dups
	     (append (assoc-default 'effort org-agenda-filters-preset)
		     filter)))
      (dolist (x filter)
	(push (org-agenda-filter-effort-form x) f))))
    (cons (if multi-pos-cats 'or 'and) (nreverse f))))

(defun org-agenda-filter-make-matcher-tag-exp (tags op)
  "Return a form associated to tag-expression TAGS.
Build a form testing a line for agenda filter for
tag-expressions.  OP is an operator of type CHAR that allows the
function to set the right switches in the returned form."
  (let (form)
    ;; Any of the expressions can match if OP is +, all must match if
    ;; the operator is -.
    (dolist (x tags (cons (if (eq op ?-) 'and 'or) form))
      (let* ((tag (substring x 1))
	     (f (cond
		 ((string= "" tag) 'tags)
		 ((and (string-match-p "\\`{" tag) (string-match-p "}\\'" tag))
		  ;; TAG is a regexp.
		  (list 'org-match-any-p (substring tag 1 -1) 'tags))
		 (t (list 'member tag 'tags)))))
	(push (if (eq op ?-) (list 'not f) f) form)))))

(declare-function org-duration-to-minutes "org-duration" (duration &optional canonical))
(defun org-agenda-filter-effort-form (e)
  "Return the form to compare the effort of the current line with what E says.
E looks like \"+<2:25\"."
  (let (op)
    (setq e (substring e 1))
    (setq op (string-to-char e) e (substring e 1))
    (setq op (cond ((equal op ?<) '<=)
		   ((equal op ?>) '>=)
		   ((equal op ??) op)
		   (t '=)))
    (list 'org-agenda-compare-effort (list 'quote op)
	  (org-duration-to-minutes e))))

(defun org-agenda-filter-expand-tags (filter &optional no-operator)
  "Expand group tags in FILTER for the agenda.
When NO-OPERATOR is non-nil, do not add the + operator to
returned tags."
  (if org-group-tags
      (let (case-fold-search rtn)
	(mapc
	 (lambda (f)
	   (let (f0 dir)
	     (if (string-match "^\\([+-]\\)\\(.+\\)" f)
		 (setq dir (match-string 1 f) f0 (match-string 2 f))
	       (setq dir (if no-operator "" "+") f0 f))
	     (setq rtn (append (mapcar (lambda(f1) (concat dir f1))
				       (org-tags-expand f0 t))
			       rtn))))
	 filter)
	(reverse rtn))
    filter))

(defun org-agenda-filter-hide-line (type)
  "If current line is TYPE, hide it in the agenda buffer."
  (let* (buffer-invisibility-spec
         (beg (max (point-min) (1- (line-beginning-position))))
         (end (line-end-position)))
    (let ((inhibit-read-only t))
      (add-text-properties
       beg end `(invisible org-filtered org-filter-type ,type)))))

(defun org-find-top-headline (&optional pos)
  "Find the topmost parent headline and return it.
POS when non-nil is the marker or buffer position to start the
search from."
  (save-excursion
    (with-current-buffer (if (markerp pos) (marker-buffer pos) (current-buffer))
      (when pos (goto-char pos))
      ;; Skip up to the topmost parent.
      (while (org-up-heading-safe))
      (ignore-errors
	(replace-regexp-in-string
	 "^\\[[0-9]+/[0-9]+\\] *\\|^\\[%[0-9]+\\] *" ""
	 (nth 4 (org-heading-components)))))))

(defun org-agenda-filter-top-headline-apply (hl &optional negative)
  "Filter by top headline HL."
  (org-agenda-set-mode-name)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((pos (org-get-at-bol 'org-hd-marker))
             (tophl (and pos (org-find-top-headline pos))))
        (when (and tophl (funcall (if negative 'identity 'not)
				  (string= hl tophl)))
          (org-agenda-filter-hide-line 'top-headline)))
      (forward-line 1)))
  (when (get-char-property (point) 'invisible)
    (org-agenda-previous-line))
  (setq org-agenda-top-headline-filter hl
	org-agenda-filtered-by-top-headline t))

(provide 'org-agenda-mode)

;;; org-agenda-mode.el ends here
