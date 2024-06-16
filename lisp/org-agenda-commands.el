;;; org-agenda-commands.el --- Org agenda mode commands  -*- lexical-binding: t; -*-

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

;; This file implements user commands for `org-agenda-mode'.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-fold)
(require 'org-keys)
(require 'org-read-date)

(require 'org-agenda-mode)
(require 'org-agenda-search)

;;;###autoload
(defcustom org-agenda-move-date-from-past-immediately-to-today t
  "Non-nil means jump to today when moving a past date forward in time.
When using S-right in the agenda to move a date forward, and the date
stamp currently points to the past, the first key press will move it
to today.  When nil, just move one day forward even if the date stays
in the past."
  :group 'org-agenda-daily/weekly
  :version "24.1"
  :type 'boolean)

;;;###autoload
(defcustom org-agenda-jump-prefer-future 'org-read-date-prefer-future
  "Should the agenda jump command prefer the future for incomplete dates?
The default is to do the same as configured in `org-read-date-prefer-future'.
But you can also set a deviating value here.
This may t or nil, or the symbol `org-read-date-prefer-future'."
  :group 'org-agenda
  :group 'org-time
  :version "24.1"
  :type '(choice
	  (const :tag "Use org-read-date-prefer-future"
		 org-read-date-prefer-future)
	  (const :tag "Never" nil)
	  (const :tag "Always" t)))

;;; Toggle agenda view modes

(declare-function org-agenda-toggle-deadlines "org-agenda-agenda-view" ())
(declare-function org-agenda-toggle-diary "org-agenda-agenda-view" ())
(declare-function org-agenda-toggle-time-grid "org-agenda-agenda-view" ())
(declare-function org-agenda-clockreport-mode "org-agenda-agenda-view" ())
(declare-function org-agenda-log-mode "org-agenda-agenda-view" (&optional special))
(declare-function org-agenda-year-view "org-agenda-query" (&optional year))
(declare-function org-agenda-month-view "org-agenda-query" (&optional month))
(declare-function org-agenda-fortnight-view "org-agenda-query" (&optional iso-week))
(declare-function org-agenda-week-view "org-agenda-query" (&optional iso-week))
(declare-function org-agenda-day-view "org-agenda-query" (&optional day-of-month))
(declare-function org-agenda-reset-view "org-agenda-query" ())

;;;###autoload
(defun org-agenda-view-mode-dispatch ()
  "Call one of the view mode commands."
  (interactive)
  (org-unlogged-message
   "View: [d]ay  [w]eek  for[t]night  [m]onth  [y]ear  [SPC]reset  [q]uit/abort
       time[G]rid   [[]inactive  [f]ollow      [l]og    [L]og-all   [c]lockcheck
       [a]rch-trees [A]rch-files clock[R]eport include[D]iary       [E]ntryText")
  (pcase (read-char-exclusive)
    (?\ (call-interactively #'org-agenda-reset-view))
    (?d (call-interactively #'org-agenda-day-view))
    (?w (call-interactively #'org-agenda-week-view))
    (?t (call-interactively #'org-agenda-fortnight-view))
    (?m (call-interactively #'org-agenda-month-view))
    (?y (call-interactively #'org-agenda-year-view))
    (?l (call-interactively #'org-agenda-log-mode))
    (?L (org-agenda-log-mode '(4)))
    (?c (org-agenda-log-mode 'clockcheck))
    ((or ?F ?f) (call-interactively #'org-agenda-follow-mode))
    (?a (call-interactively #'org-agenda-archives-mode))
    (?A (org-agenda-archives-mode 'files))
    ((or ?R ?r) (call-interactively #'org-agenda-clockreport-mode))
    ((or ?E ?e) (call-interactively #'org-agenda-entry-text-mode))
    (?G (call-interactively #'org-agenda-toggle-time-grid))
    (?D (call-interactively #'org-agenda-toggle-diary))
    (?\! (call-interactively #'org-agenda-toggle-deadlines))
    (?\[ (let ((org-agenda-include-inactive-timestamps t))
	   (org-agenda-check-type t 'agenda)
	   (org-agenda-redo))
	 (message "Display now includes inactive timestamps as well"))
    (?q (message "Abort"))
    (key (user-error "Invalid key: %s" key))))

;;; Move around Org agenda buffer

;;;###autoload
(defun org-agenda-end-of-line ()
  "Go to the end of visible line."
  (interactive)
  (goto-char (line-end-position)))

;;;###autoload
(defun org-agenda-next-line ()
  "Move cursor to the next line, and show if follow mode is active."
  (interactive)
  (call-interactively #'next-line)
  (org-agenda-do-context-action))

;;;###autoload
(defun org-agenda-previous-line ()
  "Move cursor to the previous line, and show if follow-mode is active."
  (interactive)
  (call-interactively #'previous-line)
  (org-agenda-do-context-action))

;;;###autoload
(defun org-agenda-next-item (n)
  "Move cursor to next agenda item."
  (interactive "p")
  (let ((col (current-column)))
    (dotimes (_ n)
      (when (next-single-property-change (line-end-position) 'org-marker)
	(move-end-of-line 1)
	(goto-char (next-single-property-change (point) 'org-marker))))
    (org-move-to-column col))
  (org-agenda-do-context-action))

;;;###autoload
(defun org-agenda-previous-item (n)
  "Move cursor to next agenda item."
  (interactive "p")
  (dotimes (_ n)
    (let ((col (current-column))
	  (goto (save-excursion
		  (move-end-of-line 0)
		  (previous-single-property-change (point) 'org-marker))))
      (when goto (goto-char goto))
      (org-move-to-column col)))
  (org-agenda-do-context-action))

;;;###autoload
(defun org-agenda-next-date-line (&optional arg)
  "Jump to the next line indicating a date in agenda buffer."
  (interactive "p")
  (org-agenda-check-type t 'agenda)
  (forward-line 0)
  ;; This does not work if user makes date format that starts with a blank
  (when (looking-at-p "^\\S-") (forward-char 1))
  (unless (re-search-forward "^\\S-" nil t arg)
    (backward-char 1)
    (error "No next date after this line in this buffer"))
  (goto-char (match-beginning 0)))

;;;###autoload
(defun org-agenda-previous-date-line (&optional arg)
  "Jump to the previous line indicating a date in agenda buffer."
  (interactive "p")
  (org-agenda-check-type t 'agenda)
  (forward-line 0)
  (unless (re-search-backward "^\\S-" nil t arg)
    (error "No previous date before this line in this buffer")))

;;;###autoload
(defun org-agenda-backward-block ()
  "Move backward by one agenda block."
  (interactive)
  (org-agenda-forward-block 'backward))

;;;###autoload
(defun org-agenda-forward-block (&optional backward)
  "Move forward by one agenda block.
When optional argument BACKWARD is set, go backward."
  (interactive)
  (cond ((not (derived-mode-p 'org-agenda-mode))
	 (user-error
	  "Cannot execute this command outside of org-agenda-mode buffers"))
	((looking-at (if backward "\\`" "\\'"))
	 (message "Already at the %s block" (if backward "first" "last")))
	(t (let ((_pos (prog1 (point)
			 (ignore-errors (if backward (backward-char 1)
					  (move-end-of-line 1)))))
		 (f (if backward
			#'previous-single-property-change
		      #'next-single-property-change))
		 moved dest)
	     (while (and (setq dest (funcall
				     f (point) 'org-agenda-structural-header))
			 (not (get-text-property
			     (point) 'org-agenda-structural-header)))
	       (setq moved t)
	       (goto-char dest))
	     (if moved (move-beginning-of-line 1)
	       (goto-char (if backward (point-min) (point-max)))
	       (move-beginning-of-line 1)
	       (message "No %s block" (if backward "previous" "further")))))))

;;; Manipulate agenda buffer itself

;;;###autoload
(defun org-agenda-append-agenda ()
  "Append another agenda view to the current one.
This function allows interactive building of block agendas.
Agenda views are separated by `org-agenda-block-separator'."
  (interactive)
  (unless (derived-mode-p 'org-agenda-mode)
    (user-error "Can only append from within agenda buffer"))
  (let ((org-agenda-multi t))
    (org-agenda)
    (widen)
    (org-agenda-finalize)
    (setq buffer-read-only t)
    (org-agenda-fit-window-to-buffer)))

;;;###autoload
(defun org-agenda-drag-line-forward (arg &optional backward)
  "Drag an agenda line forward by ARG lines.
When the optional argument `backward' is non-nil, move backward."
  (interactive "p")
  (let ((inhibit-read-only t) lst line)
    (if (or (not (get-text-property (point) 'txt))
	    (save-excursion
	      (dotimes (_ arg)
		(move-beginning-of-line (if backward 0 2))
		(push (not (get-text-property (point) 'txt)) lst))
	      (delq nil lst)))
	(message "Cannot move line forward")
      (let ((end (save-excursion (move-beginning-of-line 2) (point))))
	(move-beginning-of-line 1)
	(setq line (buffer-substring (point) end))
	(delete-region (point) end)
	(move-beginning-of-line (funcall (if backward '1- '1+) arg))
	(insert line)
	(org-agenda-reapply-filters)
	(org-agenda-mark-clocking-task)
	(move-beginning-of-line 0)))))

;;;###autoload
(defun org-agenda-drag-line-backward (arg)
  "Drag an agenda line backward by ARG lines."
  (interactive "p")
  (org-agenda-drag-line-forward arg t))

;;; Show/jump-to to places from agenda

(declare-function org-link-open-from-string "ol" (s &optional arg))
;;;###autoload
(defun org-agenda-switch-to (&optional delete-other-windows)
  "Go to the Org mode file which contains the item at point.
When optional argument DELETE-OTHER-WINDOWS is non-nil, the
displayed Org file fills the frame."
  (interactive)
  (if (and org-return-follows-link
	   (not (org-get-at-bol 'org-marker))
	   (org-in-regexp org-link-bracket-re))
      (org-link-open-from-string (match-string 0))
    (let* ((marker (or (org-get-at-bol 'org-marker)
		       (org-agenda-error)))
	   (buffer (marker-buffer marker))
	   (pos (marker-position marker)))
      (unless buffer (user-error "Trying to switch to non-existent buffer"))
      (pop-to-buffer-same-window buffer)
      (when delete-other-windows
        (display-buffer (current-buffer) '(org-display-buffer-full-frame)))
      (widen)
      (goto-char pos)
      (when (derived-mode-p 'org-mode)
	(org-fold-show-context 'agenda)
	(run-hooks 'org-agenda-after-show-hook)))))

;;;###autoload
(defun org-agenda-goto-mouse (ev)
  "Go to the Org file which contains the item at the mouse click."
  (interactive "e")
  (mouse-set-point ev)
  (org-agenda-goto))

(defvar org-agenda-show-window nil)
(declare-function org-cycle-hide-drawers "org-cycle" (state))
;;;###autoload
(defun org-agenda-show-and-scroll-up (&optional arg)
  "Display the Org file which contains the item at point.

When called repeatedly, scroll the window that is displaying the buffer.

With a `\\[universal-argument]' prefix argument, display the item, but \
fold drawers."
  (interactive "P")
  (let ((win (selected-window)))
    (if (and (window-live-p org-agenda-show-window)
	     (eq this-command last-command))
	(progn
	  (select-window org-agenda-show-window)
	  (ignore-errors (scroll-up)))
      (org-agenda-goto t)
      (org-fold-show-entry 'hide-drawers)
      (if arg (org-cycle-hide-drawers 'children)
	(org-with-wide-buffer
	 (narrow-to-region (org-entry-beginning-position)
			   (org-entry-end-position))
	 (org-fold-show-all '(drawers))))
      (setq org-agenda-show-window (selected-window)))
    (select-window win)))

;;;###autoload
(defun org-agenda-show-scroll-down ()
  "Scroll down the window showing the agenda."
  (interactive)
  (let ((win (selected-window)))
    (when (window-live-p org-agenda-show-window)
      (select-window org-agenda-show-window)
      (ignore-errors (scroll-down))
      (select-window win))))

;;;###autoload
(defun org-agenda-recenter (arg)
  "Display the Org file which contains the item at point and recenter."
  (interactive "P")
  (let ((win (selected-window)))
    (org-agenda-goto t)
    (recenter arg)
    (select-window win)))

;;;###autoload
(defun org-agenda-show-mouse (ev)
  "Display the Org file which contains the item at the mouse click."
  (interactive "e")
  (mouse-set-point ev)
  (org-agenda-show))

(defvar org-agenda-last-indirect-buffer nil
  "Last buffer loaded by `org-agenda-tree-to-indirect-buffer'.")
(declare-function org-tree-to-indirect-buffer "org-indirect-buffer" (&optional arg))
;;;###autoload
(defun org-agenda-tree-to-indirect-buffer (arg)
  "Show the subtree corresponding to the current entry in an indirect buffer.
This calls the command `org-tree-to-indirect-buffer' from the original buffer.

With a numerical prefix ARG, go up to this level and then take that tree.
With a negative numeric ARG, go up by this number of levels.

With a `\\[universal-argument]' prefix, make a separate frame for this tree, \
i.e. don't use
the dedicated frame."
  (interactive "P")
  (org-agenda-check-no-diary)
  (require 'org-indirect-buffer)
  (defvar org-last-indirect-buffer)
  (let* ((marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (with-current-buffer buffer
      (save-excursion
	(goto-char pos)
	(org-tree-to-indirect-buffer arg))))
  (setq org-agenda-last-indirect-buffer org-last-indirect-buffer))

(declare-function org-open-at-point "org-open-at-point" (&optional arg))
;;;###autoload
(defun org-agenda-open-link (&optional arg)
  "Open the link(s) in the current entry, if any.
This looks for a link in the displayed line in the agenda.
It also looks at the text of the entry itself."
  (interactive "P")
  (let* ((marker (or (org-get-at-bol 'org-hd-marker)
		     (org-get-at-bol 'org-marker)))
	 (buffer (and marker (marker-buffer marker)))
         (prefix (buffer-substring (line-beginning-position)
                                   (line-end-position)))
	 (lkall (and buffer (org-offer-links-in-entry
			     buffer marker arg prefix)))
	 (lk0 (car lkall))
	 (lk (if (stringp lk0) (list lk0) lk0))
	 (lkend (cdr lkall))
	 trg)
    (cond
     ((and buffer lk)
      (mapcar (lambda(l)
		(with-current-buffer buffer
		  (setq trg (and (string-match org-link-bracket-re l)
				 (match-string 1 l)))
                  (require 'org-open-at-point)
		  (if (or (not trg) (string-match org-link-any-re trg))
		      ;; Don't use `org-with-wide-buffer' here as
		      ;; opening the link may result in moving the point
		      (save-restriction
			(widen)
			(goto-char marker)
			(when (search-forward l nil lkend)
			  (goto-char (match-beginning 0))
			  (org-open-at-point)))
		    (switch-to-buffer-other-window buffer)
		    ;; This is an internal link, widen the buffer
		    (widen)
		    (goto-char marker)
		    (when (search-forward l nil lkend)
		      (goto-char (match-beginning 0))
		      (org-open-at-point)))))
	      lk))
     ((or (org-in-regexp (concat "\\(" org-link-bracket-re "\\)"))
	  (save-excursion
	    (forward-line 0)
	    (looking-at (concat ".*?\\(" org-link-bracket-re "\\)"))))
      (org-link-open-from-string (match-string 1)))
     (t (message "No link to open here")))))

;;; Edit timestamps contributing to agenda in headings

;;;###autoload
(defun org-agenda-do-date-later (arg)
  (interactive "P")
  (cond
   ((or (equal arg '(16))
	(memq last-command
	      '(org-agenda-date-later-minutes org-agenda-date-earlier-minutes)))
    (setq this-command 'org-agenda-date-later-minutes)
    (org-agenda-date-later-minutes 1))
   ((or (equal arg '(4))
	(memq last-command
	      '(org-agenda-date-later-hours org-agenda-date-earlier-hours)))
    (setq this-command 'org-agenda-date-later-hours)
    (org-agenda-date-later-hours 1))
   (t
    (org-agenda-date-later (prefix-numeric-value arg)))))

;;;###autoload
(defun org-agenda-do-date-earlier (arg)
  (interactive "P")
  (cond
   ((or (equal arg '(16))
	(memq last-command
	      '(org-agenda-date-later-minutes org-agenda-date-earlier-minutes)))
    (setq this-command 'org-agenda-date-earlier-minutes)
    (org-agenda-date-earlier-minutes 1))
   ((or (equal arg '(4))
	(memq last-command
	      '(org-agenda-date-later-hours org-agenda-date-earlier-hours)))
    (setq this-command 'org-agenda-date-earlier-hours)
    (org-agenda-date-earlier-hours 1))
   (t
    (org-agenda-date-earlier (prefix-numeric-value arg)))))

(declare-function org-timestamp-change "org-timestamp" (n &optional what updown suppress-tmp-delay))
;;;###autoload
(defun org-agenda-date-later (arg &optional what)
  "Change the date of this item to ARG day(s) later."
  (interactive "p")
  (org-agenda-check-type t 'agenda)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 cdate today)
    (require 'org-timestamp)
    (defvar org-last-changed-timestamp)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(unless (org-at-timestamp-p 'lax) (error "Cannot find time stamp"))
	(when (and org-agenda-move-date-from-past-immediately-to-today
		   (equal arg 1)
		   (or (not what) (eq what 'day))
		   (not (save-match-data (org-at-date-range-p))))
	  (setq cdate (org-parse-time-string (match-string 0) 'nodefault)
		cdate (calendar-absolute-from-gregorian
		       (list (nth 4 cdate) (nth 3 cdate) (nth 5 cdate)))
		today (org-today))
	  (when (> today cdate)
	    ;; immediately shift to today
	    (setq arg (- today cdate))))
	(org-timestamp-change arg (or what 'day))
	(when (and (org-at-date-range-p)
                   (re-search-backward org-tr-regexp-both
                                       (line-beginning-position)))
	  (let ((end org-last-changed-timestamp))
	    (org-timestamp-change arg (or what 'day))
            ;; FIXME: Why do we have to do it?
	    (setq org-last-changed-timestamp
		  (concat org-last-changed-timestamp "--" end)))))
      (org-agenda-show-new-time marker org-last-changed-timestamp))
    (message "Time stamp changed to %s" org-last-changed-timestamp)))

;;;###autoload
(defun org-agenda-date-earlier (arg &optional what)
  "Change the date of this item to ARG day(s) earlier."
  (interactive "p")
  (org-agenda-date-later (- arg) what))

;;;###autoload
(defun org-agenda-date-later-minutes (arg)
  "Change the time of this item, in units of `org-timestamp-rounding-minutes'."
  (interactive "p")
  (setq arg (* arg (cadr org-timestamp-rounding-minutes)))
  (org-agenda-date-later arg 'minute))

;;;###autoload
(defun org-agenda-date-earlier-minutes (arg)
  "Change the time of this item, in units of `org-timestamp-rounding-minutes'."
  (interactive "p")
  (setq arg (* arg (cadr org-timestamp-rounding-minutes)))
  (org-agenda-date-earlier arg 'minute))

;;;###autoload
(defun org-agenda-date-later-hours (arg)
  "Change the time of this item, in hour steps."
  (interactive "p")
  (org-agenda-date-later arg 'hour))

;;;###autoload
(defun org-agenda-date-earlier-hours (arg)
  "Change the time of this item, in hour steps."
  (interactive "p")
  (org-agenda-date-earlier arg 'hour))

;;; Quit agenda

;;;###autoload
(defun org-agenda-Quit ()
  "Exit the agenda, killing the agenda buffer.
Like `org-agenda-quit', but kill the buffer even when
`org-agenda-sticky' is non-nil."
  (interactive)
  (org-agenda--quit))

;;;###autoload
(defun org-agenda-quit ()
  "Exit the agenda.

When `org-agenda-sticky' is non-nil, bury the agenda buffer
instead of killing it.

When `org-agenda-restore-windows-after-quit' is non-nil, restore
the pre-agenda window configuration.

When column view is active, exit column view instead of the
agenda."
  (interactive)
  (org-agenda--quit org-agenda-sticky))

(declare-function org-columns-quit "org-colview" ())
(defun org-agenda--quit (&optional bury)
  (if (bound-and-true-p org-agenda-columns-active)
      (org-columns-quit)
    (require 'org-indirect-buffer)
    (defvar org-indirect-buffer-display)
    (let ((wconf org-agenda-pre-window-conf)
	  (buf (current-buffer))
	  (org-agenda-last-indirect-window
	   (and (eq org-indirect-buffer-display 'other-window)
		org-agenda-last-indirect-buffer
		(get-buffer-window org-agenda-last-indirect-buffer))))
      (cond
       ((eq org-agenda-window-setup 'other-frame)
	(delete-frame))
       ((eq org-agenda-window-setup 'other-tab)
	(if (fboundp 'tab-bar-close-tab)
	    (tab-bar-close-tab)
	  (user-error "Your version of Emacs does not have tab bar mode support")))
       ((and org-agenda-restore-windows-after-quit
	     wconf)
	;; Maybe restore the pre-agenda window configuration.  Reset
	;; `org-agenda-pre-window-conf' before running
	;; `set-window-configuration', which loses the current buffer.
	(setq org-agenda-pre-window-conf nil)
	(set-window-configuration wconf))
       (t
	(when org-agenda-last-indirect-window
	  (delete-window org-agenda-last-indirect-window))
	(and (not (eq org-agenda-window-setup 'current-window))
	     (not (one-window-p))
	     (delete-window))))
      (if bury
	  ;; Set the agenda buffer as the current buffer instead of
	  ;; passing it as an argument to `bury-buffer' so that
	  ;; `bury-buffer' removes it from the window.
	  (with-current-buffer buf
	    (bury-buffer))
	(kill-buffer buf)
	(setq org-agenda-archives-mode nil
	      org-agenda-buffer nil)))))

;;;###autoload
(defun org-agenda-exit ()
  "Exit the agenda, killing Org buffers loaded by the agenda.
Like `org-agenda-Quit', but kill any buffers that were created by
the agenda.  Org buffers visited directly by the user will not be
touched.  Also, exit the agenda even if it is in column view."
  (interactive)
  (when org-agenda-columns-active
    (org-columns-quit))
  (org-release-buffers org-agenda-new-buffers)
  (setq org-agenda-new-buffers nil)
  (org-agenda-Quit))

;;;###autoload
(defun org-agenda-kill-all-agenda-buffers ()
  "Kill all buffers in `org-agenda-mode'.
This is used when toggling sticky agendas."
  (interactive)
  (let (blist)
    (dolist (buf (buffer-list))
      (when (with-current-buffer buf (eq major-mode 'org-agenda-mode))
	(push buf blist)))
    (mapc #'kill-buffer blist)))

;;; Sticky agenda

;;;###autoload
(defun org-toggle-sticky-agenda (&optional arg)
  "Toggle `org-agenda-sticky'."
  (interactive "P")
  (let ((new-value (if arg
		       (> (prefix-numeric-value arg) 0)
		     (not org-agenda-sticky))))
    (if (equal new-value org-agenda-sticky)
	(and (called-interactively-p 'interactive)
	     (message "Sticky agenda was already %s"
		      (if org-agenda-sticky "enabled" "disabled")))
      (setq org-agenda-sticky new-value)
      (org-agenda-kill-all-agenda-buffers)
      (and (called-interactively-p 'interactive)
	   (message "Sticky agenda %s"
		    (if org-agenda-sticky "enabled" "disabled"))))))

(provide 'org-agenda-commands)

;;; org-agenda-commands.el ends here
