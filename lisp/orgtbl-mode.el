;;; orgtbl-mode.el --- Table editing outside Org mode        -*- lexical-binding: t; -*-

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

;;; The Orgtbl minor mode

;; Define a minor mode which can be used in other modes in order to
;; integrate the Org table editor.

;; This is really a hack, because the Org table editor uses several
;; keys which normally belong to the major mode, for example the TAB
;; and RET keys.  Here is how it works: The minor mode defines all the
;; keys necessary to operate the table editor, but wraps the commands
;; into a function which tests if the cursor is currently inside
;; a table.  If that is the case, the table editor command is
;; executed.  However, when any of those keys is used outside a table,
;; the function uses `key-binding' to look up if the key has an
;; associated command in another currently active keymap (minor modes,
;; major mode, global), and executes that command.  There might be
;; problems if any of the keys used by the table editor is otherwise
;; used as a prefix key.

;; Another challenge is that the key binding for TAB can be tab or \C-i,
;; likewise the binding for RET can be return or \C-m.  Orgtbl-mode
;; addresses this by checking explicitly for both bindings.

;; The optimized version (see variable `orgtbl-optimized') takes over
;; all keys which are bound to `self-insert-command' in the *global map*.
;; Some modes bind other commands to simple characters, for example
;; AUCTeX binds the double quote to `Tex-insert-quote'.  With orgtbl-mode
;; active, this binding is ignored inside tables and replaced with a
;; modified self-insert.


;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-keys)
(require 'org-table-edit)
(require 'org-table-formula)

(defcustom orgtbl-optimized t
  "Non-nil means use the optimized table editor version for `orgtbl-mode'.

In the optimized version, the table editor takes over all simple keys that
normally just insert a character.  In tables, the characters are inserted
in a way to minimize disturbing the table structure (i.e. in overwrite mode
for empty fields).  Outside tables, the correct binding of the keys is
restored.

Changing this variable requires a restart of Emacs to become
effective."
  :group 'org-table
  :type 'boolean)

(defcustom orgtbl-radio-table-templates
  '((latex-mode "% BEGIN RECEIVE ORGTBL %n
% END RECEIVE ORGTBL %n
\\begin{comment}
#+ORGTBL: SEND %n orgtbl-to-latex :splice nil :skip 0
| | |
\\end{comment}\n")
    (texinfo-mode "@c BEGIN RECEIVE ORGTBL %n
@c END RECEIVE ORGTBL %n
@ignore
#+ORGTBL: SEND %n orgtbl-to-html :splice nil :skip 0
| | |
@end ignore\n")
    (html-mode "<!-- BEGIN RECEIVE ORGTBL %n -->
<!-- END RECEIVE ORGTBL %n -->
<!--
#+ORGTBL: SEND %n orgtbl-to-html :splice nil :skip 0
| | |
-->\n")
    (org-mode "#+ BEGIN RECEIVE ORGTBL %n
#+ END RECEIVE ORGTBL %n

#+ORGTBL: SEND %n orgtbl-to-orgtbl :splice nil :skip 0
| | |
"))
  "Templates for radio tables in different major modes.
Each template must define lines that will be treated as a comment and that
must contain the \"BEGIN RECEIVE ORGTBL %n\" and \"END RECEIVE ORGTBL\"
lines where \"%n\" will be replaced with the name of the table during
insertion of the template.  The transformed table will later be inserted
between these lines.

The template should also contain a minimal table in a multiline comment.
If multiline comments are not possible in the buffer language,
you can pack it into a string that will not be used when the code
is compiled or executed.  Above the table will you need a line with
the fixed string \"#+ORGTBL: SEND\", followed by instruction on how to
convert the table into a data structure useful in the
language of the buffer.  Check the manual for the section on
\"Translator functions\", and more generally check out
the Info node `(org)Tables in arbitrary syntax'.

All occurrences of %n in a template will be replaced with the name of the
table, obtained by prompting the user."
  :group 'org-table
  :type '(repeat
	  (list (symbol :tag "Major mode")
		(string :tag "Format"))))

(defvar orgtbl-after-send-table-hook nil
  "Hook for functions attaching to \\`C-c C-c', if the table is sent.
This can be used to add additional functionality after the table is sent
to the receiver position, otherwise, if table is not sent, the functions
are not run.")

(defvar orgtbl-mode-map (make-keymap)
  "Keymap for `orgtbl-mode'.")

(defvar org-old-auto-fill-inhibit-regexp nil
  "Local variable used by `orgtbl-mode'.")

(defconst orgtbl-line-start-regexp
  "[ \t]*\\(|\\|#\\+\\(tblfm\\|orgtbl\\|tblname\\):\\)"
  "Matches a line belonging to an orgtbl.")

(defconst orgtbl-extra-font-lock-keywords
  (list (list (concat "^" orgtbl-line-start-regexp ".*")
	      0 (quote 'org-table) 'prepend))
  "Extra `font-lock-keywords' to be added when `orgtbl-mode' is active.")

;;;###autoload
(defun turn-on-orgtbl ()
  "Unconditionally turn on `orgtbl-mode'."
  (orgtbl-mode 1))

;; Install it as a minor mode.
(put 'orgtbl-mode :included t)
(put 'orgtbl-mode :menu-tag "Org Table Mode")

(easy-menu-define orgtbl-mode-menu orgtbl-mode-map "OrgTbl menu."
  '("OrgTbl"
    ["Create or convert" org-table-create-or-convert-from-region
     :active (not (org-at-table-p)) :keys "C-c |" ]
    "--"
    ["Align" org-ctrl-c-ctrl-c :active (org-at-table-p) :keys "C-c C-c"]
    ["Next Field" org-cycle :active (org-at-table-p) :keys "TAB"]
    ["Previous Field" org-shifttab :active (org-at-table-p) :keys "S-TAB"]
    ["Next Row" org-return :active (org-at-table-p) :keys "RET"]
    "--"
    ["Blank Field" org-table-blank-field :active (org-at-table-p) :keys "C-c SPC"]
    ["Edit Field" org-table-edit-field :active (org-at-table-p) :keys "C-c ` "]
    ["Copy Field from Above"
     org-table-copy-down :active (org-at-table-p) :keys "S-RET"]
    "--"
    ("Column"
     ["Move Column Left" org-metaleft :active (org-at-table-p) :keys "M-<left>"]
     ["Move Column Right" org-metaright :active (org-at-table-p) :keys "M-<right>"]
     ["Delete Column" org-shiftmetaleft :active (org-at-table-p) :keys "M-S-<left>"]
     ["Insert Column" org-shiftmetaright :active (org-at-table-p) :keys "M-S-<right>"])
    ("Row"
     ["Move Row Up" org-metaup :active (org-at-table-p) :keys "M-<up>"]
     ["Move Row Down" org-metadown :active (org-at-table-p) :keys "M-<down>"]
     ["Delete Row" org-shiftmetaup :active (org-at-table-p) :keys "M-S-<up>"]
     ["Insert Row" org-shiftmetadown :active (org-at-table-p) :keys "M-S-<down>"]
     ["Sort lines in region" org-table-sort-lines :active (org-at-table-p) :keys "C-c ^"]
     "--"
     ["Insert Hline" org-table-insert-hline :active (org-at-table-p) :keys "C-c -"])
    ("Rectangle"
     ["Copy Rectangle" org-copy-special :active (org-at-table-p)]
     ["Cut Rectangle" org-cut-special :active (org-at-table-p)]
     ["Paste Rectangle" org-paste-special :active (org-at-table-p)]
     ["Fill Rectangle" org-table-wrap-region :active (org-at-table-p)])
    "--"
    ("Radio tables"
     ["Insert table template" orgtbl-insert-radio-table
      (cl-assoc-if #'derived-mode-p orgtbl-radio-table-templates)]
     ["Comment/uncomment table" orgtbl-toggle-comment t])
    "--"
    ["Set Column Formula" org-table-eval-formula :active (org-at-table-p) :keys "C-c ="]
    ["Set Field Formula" (org-table-eval-formula '(4)) :active (org-at-table-p) :keys "C-u C-c ="]
    ["Edit Formulas" org-table-edit-formulas :active (org-at-table-p) :keys "C-c '"]
    ["Recalculate line" org-table-recalculate :active (org-at-table-p) :keys "C-c *"]
    ["Recalculate all" (org-table-recalculate '(4)) :active (org-at-table-p) :keys "C-u C-c *"]
    ["Iterate all" (org-table-recalculate '(16)) :active (org-at-table-p) :keys "C-u C-u C-c *"]
    ["Toggle Recalculate Mark" org-table-rotate-recalc-marks :active (org-at-table-p) :keys "C-c #"]
    ["Sum Column/Rectangle" org-table-sum
     :active (or (org-at-table-p) (use-region-p)) :keys "C-c +"]
    ["Which Column?" org-table-current-column :active (org-at-table-p) :keys "C-c ?"]
    ["Debug Formulas"
     org-table-toggle-formula-debugger :active (org-at-table-p)
     :keys "C-c {"
     :style toggle :selected org-table-formula-debug]
    ["Show Col/Row Numbers"
     org-table-toggle-coordinate-overlays :active (org-at-table-p)
     :keys "C-c }"
     :style toggle :selected org-table-overlay-coordinates]
    "--"
    ("Plot"
     ["Ascii plot" orgtbl-ascii-plot :active (org-at-table-p) :keys "C-c \" a"]
     ["Gnuplot" org-plot/gnuplot :active (org-at-table-p) :keys "C-c \" g"])))

;;;###autoload
(define-minor-mode orgtbl-mode
  "The Org mode table editor as a minor mode for use in other modes."
  :lighter " OrgTbl"
  (org-load-modules-maybe)
  (cond
   ((derived-mode-p 'org-mode)
    ;; Exit without error, in case some hook functions calls this by
    ;; accident in Org mode.
    (message "Orgtbl mode is not useful in Org mode, command ignored"))
   (orgtbl-mode
    (orgtbl-setup)
    ;; Make sure we are first in minor-mode-map-alist
    (let ((c (assq 'orgtbl-mode minor-mode-map-alist)))
      ;; FIXME: maybe it should use emulation-mode-map-alists?
      (and c (setq minor-mode-map-alist
                   (cons c (delq c minor-mode-map-alist)))))
    (setq-local org-table-may-need-update t)
    (add-hook 'before-change-functions 'org-before-change-function
	      nil 'local)
    (setq-local org-old-auto-fill-inhibit-regexp
		auto-fill-inhibit-regexp)
    (setq-local auto-fill-inhibit-regexp
		(if auto-fill-inhibit-regexp
		    (concat orgtbl-line-start-regexp "\\|"
			    auto-fill-inhibit-regexp)
		  orgtbl-line-start-regexp))
    (font-lock-add-keywords nil orgtbl-extra-font-lock-keywords)
    (org-restart-font-lock))
   (t
    (setq auto-fill-inhibit-regexp org-old-auto-fill-inhibit-regexp)
    (remove-hook 'before-change-functions 'org-before-change-function t)
    (font-lock-remove-keywords nil orgtbl-extra-font-lock-keywords)
    (org-restart-font-lock)
    (force-mode-line-update 'all))))

(defun orgtbl-make-binding (fun n &rest keys)
  "Create a function for binding in the table minor mode.
FUN is the command to call inside a table.  N is used to create a unique
command name.  KEYS are keys that should be checked in for a command
to execute outside of tables."
  (eval
   (list 'defun
	 (intern (concat "orgtbl-hijacker-command-" (number-to-string n)))
	 '(arg)
	 (concat "In tables, run `" (symbol-name fun) "'.\n"
		 "Outside of tables, run the binding of `"
		 (mapconcat #'key-description keys "' or `")
		 "'.")
	 '(interactive "p")
	 (list 'if
	       '(org-at-table-p)
	       (list 'call-interactively (list 'quote fun))
	       (list 'let '(orgtbl-mode)
		     (list 'call-interactively
			   (append '(or)
				   (mapcar (lambda (k)
					     (list 'key-binding k))
					   keys)
				   '('orgtbl-error))))))))

(defun orgtbl-error ()
  "Error when there is no default binding for a table key."
  (interactive)
  (user-error "This key has no function outside tables"))

;; Fill in orgtbl keymap.
(let ((nfunc 0)
      (bindings
       '(([(meta shift left)]  org-table-delete-column)
	 ([(meta left)]	 org-table-move-column-left)
	 ([(meta right)]       org-table-move-column-right)
	 ([(meta shift right)] org-table-insert-column)
	 ([(meta shift up)]    org-table-kill-row)
	 ([(meta shift down)]  org-table-insert-row)
	 ([(meta up)]		 org-table-move-row-up)
	 ([(meta down)]	 org-table-move-row-down)
	 ("\C-c\C-w"		 org-table-cut-region)
	 ("\C-c\M-w"		 org-table-copy-region)
	 ("\C-c\C-y"		 org-table-paste-rectangle)
	 ("\C-c\C-w"           org-table-wrap-region)
	 ("\C-c-"		 org-table-insert-hline)
	 ("\C-c}"		 org-table-toggle-coordinate-overlays)
	 ("\C-c{"		 org-table-toggle-formula-debugger)
	 ("\C-m"		 org-table-next-row)
	 ([(shift return)]	 org-table-copy-down)
	 ("\C-c?"		 org-table-field-info)
	 ("\C-c "		 org-table-blank-field)
	 ("\C-c+"		 org-table-sum)
	 ("\C-c="		 org-table-eval-formula)
	 ("\C-c'"		 org-table-edit-formulas)
	 ("\C-c`"		 org-table-edit-field)
	 ("\C-c*"		 org-table-recalculate)
	 ("\C-c^"		 org-table-sort-lines)
	 ("\M-a"		 org-table-beginning-of-field)
	 ("\M-e"		 org-table-end-of-field)
	 ([(control ?#)]       org-table-rotate-recalc-marks)))
      elt key fun cmd)
  (while (setq elt (pop bindings))
    (setq nfunc (1+ nfunc))
    (setq key (org-key (car elt))
	  fun (nth 1 elt)
	  cmd (orgtbl-make-binding fun nfunc key))
    (org-defkey orgtbl-mode-map key cmd))

  ;; Special treatment needed for TAB, RET and DEL
  (org-defkey orgtbl-mode-map [(return)]
	      (orgtbl-make-binding 'orgtbl-ret 100 [(return)] "\C-m"))
  (org-defkey orgtbl-mode-map "\C-m"
	      (orgtbl-make-binding 'orgtbl-ret 101 "\C-m" [(return)]))
  (org-defkey orgtbl-mode-map [(tab)]
	      (orgtbl-make-binding 'orgtbl-tab 102 [(tab)] "\C-i"))
  (org-defkey orgtbl-mode-map "\C-i"
	      (orgtbl-make-binding 'orgtbl-tab 103 "\C-i" [(tab)]))
  (org-defkey orgtbl-mode-map [(shift tab)]
	      (orgtbl-make-binding 'org-table-previous-field 104
				   [(shift tab)] [(tab)] "\C-i"))
  (org-defkey orgtbl-mode-map [backspace]
	      (orgtbl-make-binding 'org-delete-backward-char 109
				   [backspace] (kbd "DEL")))

  (org-defkey orgtbl-mode-map [S-iso-lefttab]
	      (orgtbl-make-binding 'org-table-previous-field 107
				   [S-iso-lefttab] [backtab] [(shift tab)]
				   [(tab)] "\C-i"))

  (org-defkey orgtbl-mode-map [backtab]
	      (orgtbl-make-binding 'org-table-previous-field 108
				   [backtab] [S-iso-lefttab] [(shift tab)]
				   [(tab)] "\C-i"))

  (org-defkey orgtbl-mode-map "\M-\C-m"
	      (orgtbl-make-binding 'org-table-wrap-region 105
				   "\M-\C-m" [(meta return)]))
  (org-defkey orgtbl-mode-map [(meta return)]
	      (orgtbl-make-binding 'org-table-wrap-region 106
				   [(meta return)] "\M-\C-m"))

  (org-defkey orgtbl-mode-map "\C-c\C-c" 'orgtbl-ctrl-c-ctrl-c)
  (org-defkey orgtbl-mode-map "\C-c|" 'orgtbl-create-or-convert-from-region))

(defun orgtbl-setup ()
  "Setup orgtbl keymaps."
  ;; If the user wants maximum table support, we need to hijack
  ;; some standard editing functions
  (org-remap orgtbl-mode-map
	     'self-insert-command (and orgtbl-optimized 'orgtbl-self-insert-command)
	     'delete-char (and orgtbl-optimized 'org-delete-char)
             'delete-forward-char (and orgtbl-optimized 'org-delete-char)
	     'delete-backward-char (and orgtbl-optimized 'org-delete-backward-char))
  (org-defkey orgtbl-mode-map "|" (and orgtbl-optimized 'org-force-self-insert)))

(defun orgtbl-ctrl-c-ctrl-c (arg)
  "If the cursor is inside a table, realign the table.
If it is a table to be sent away to a receiver, do it.
With prefix arg, also recompute table."
  (interactive "P")
  (let ((case-fold-search t) (pos (point)) action)
    (save-excursion
      (forward-line 0)
      (setq action (cond
		    ((looking-at "[ \t]*#\\+ORGTBL:.*\n[ \t]*|") (match-end 0))
		    ((looking-at "[ \t]*|") pos)
		    ((looking-at "[ \t]*#\\+tblfm:") 'recalc))))
    (cond
     ((integerp action)
      (goto-char action)
      (org-table-maybe-eval-formula)
      (if arg
	  (call-interactively #'org-table-recalculate)
	(org-table-maybe-recalculate-line))
      (call-interactively #'org-table-align)
      (when (orgtbl-send-table 'maybe)
	(run-hooks 'orgtbl-after-send-table-hook)))
     ((eq action 'recalc)
      (save-excursion
	(forward-line 0)
        (org-skip-whitespace 'back)
	(if (org-at-table-p)
	    (org-call-with-arg 'org-table-recalculate t))))
     (t (let (orgtbl-mode)
	  (call-interactively (key-binding "\C-c\C-c")))))))

(declare-function org-table-create-or-convert-from-region "org-table-create" (arg))
(defun orgtbl-create-or-convert-from-region (_arg)
  "Create table or convert region to table, if no conflicting binding.
This installs the table binding `C-c |', but only if there is no
conflicting binding to this key outside `orgtbl-mode'."
  (interactive "P")
  (let* (orgtbl-mode (cmd (key-binding "\C-c|")))
    (if cmd
	(call-interactively cmd)
      (call-interactively #'org-table-create-or-convert-from-region))))

(defun orgtbl-tab (arg)
  "Justification and field motion for `orgtbl-mode'."
  (interactive "P")
  (if arg (org-table-edit-field t)
    (org-table-justify-field-maybe)
    (org-table-next-field)))

(defun orgtbl-ret ()
  "Justification and field motion for `orgtbl-mode'."
  (interactive)
  (if (bobp)
      (newline)
    (org-table-justify-field-maybe)
    (org-table-next-row)))

(defun orgtbl-self-insert-command (N)
  "Like `self-insert-command', use `overwrite-mode' for whitespace in tables.
If the cursor is in a table looking at whitespace, the whitespace is
overwritten, and the table is not marked as requiring realignment."
  (interactive "p")
  (if (and (org-at-table-p)
	   (or
	    (and org-table-auto-blank-field
		 (member last-command
			 '(orgtbl-hijacker-command-100
			   orgtbl-hijacker-command-101
			   orgtbl-hijacker-command-102
			   orgtbl-hijacker-command-103
			   orgtbl-hijacker-command-104
			   orgtbl-hijacker-command-105
			   yas/expand))
		 (org-table-blank-field))
	    t)
	   (eq N 1)
	   (looking-at "[^|\n]* \\( \\)|"))
      (let (org-table-may-need-update)
	(delete-region (match-beginning 1) (match-end 1))
	(self-insert-command N))
    (setq org-table-may-need-update t)
    (let* (orgtbl-mode
	   (cmd (or (key-binding
		     (or (and (listp function-key-map)
			      (cdr (assoc last-command-event function-key-map)))
			 (vector last-command-event)))
		    'self-insert-command)))
      (call-interactively cmd)
      (if (and org-self-insert-cluster-for-undo
	       (eq cmd 'self-insert-command))
	  (if (not (eq last-command 'orgtbl-self-insert-command))
	      (setq org-self-insert-command-undo-counter 1)
	    (if (>= org-self-insert-command-undo-counter 20)
		(setq org-self-insert-command-undo-counter 1)
	      (and (> org-self-insert-command-undo-counter 0)
		   buffer-undo-list
		   (not (cadr buffer-undo-list)) ; remove nil entry
		   (setcdr buffer-undo-list (cddr buffer-undo-list)))
	      (setq org-self-insert-command-undo-counter
		    (1+ org-self-insert-command-undo-counter))))))))

(defun orgtbl-gather-send-defs ()
  "Gather a plist of :name, :transform, :params for each destination before
a radio table."
  (save-excursion
    (goto-char (org-table-begin))
    (let (rtn)
      (forward-line -1)
      (catch :bob
        (while (looking-at "[ \t]*#\\+ORGTBL[: \t][ \t]*SEND[ \t]+\\([^ \t\r\n]+\\)[ \t]+\\([^ \t\r\n]+\\)\\([ \t]+.*\\)?")
	  (let ((name (org-no-properties (match-string 1)))
	        (transform (intern (match-string 2)))
	        (params (if (match-end 3)
			    (read (concat "(" (match-string 3) ")")))))
	    (push (list :name name :transform transform :params params)
		  rtn)
            (when (bobp) (throw :bob nil))
	    (forward-line -1))))
      rtn)))

(defun orgtbl-send-replace-tbl (name text)
  "Find and replace table NAME with TEXT."
  (save-excursion
    (goto-char (point-min))
    (let* ((location-flag nil)
	   (name (regexp-quote name))
	   (begin-re (format "BEGIN +RECEIVE +ORGTBL +%s\\([ \t]\\|$\\)" name))
	   (end-re (format "END +RECEIVE +ORGTBL +%s\\([ \t]\\|$\\)" name)))
      (while (re-search-forward begin-re nil t)
	(unless location-flag (setq location-flag t))
	(let ((beg (line-beginning-position 2)))
	  (unless (re-search-forward end-re nil t)
	    (user-error "Cannot find end of receiver location at %d" beg))
	  (forward-line 0)
	  (delete-region beg (point))
	  (insert text "\n")))
      (unless location-flag
	(user-error "No valid receiver location found in the buffer")))))

(defun orgtbl-send-table (&optional maybe)
  "Send a transformed version of table at point to the receiver position.
With argument MAYBE, fail quietly if no transformation is defined
for this table."
  (interactive)
  (catch 'exit
    (unless (org-at-table-p) (user-error "Not at a table"))
    ;; when non-interactive, we assume align has just happened.
    (when (called-interactively-p 'any) (org-table-align))
    (let ((dests (orgtbl-gather-send-defs))
	  (table (org-table-to-lisp))
	  (ntbl 0))
      (unless dests
	(if maybe (throw 'exit nil)
	  (user-error "Don't know how to transform this table")))
      (dolist (dest dests)
	(let ((name (plist-get dest :name))
	      (transform (plist-get dest :transform))
	      (params (plist-get dest :params)))
	  (unless (fboundp transform)
	    (user-error "No such transformation function %s" transform))
	  (orgtbl-send-replace-tbl name (funcall transform table params)))
	(cl-incf ntbl))
      (message "Table converted and installed at %d receiver location%s"
	       ntbl (if (> ntbl 1) "s" ""))
      (and (> ntbl 0) ntbl))))

(defun orgtbl-toggle-comment ()
  "Comment or uncomment the orgtbl at point."
  (interactive)
  (let* ((case-fold-search t)
	 (re1 (concat "^" (regexp-quote comment-start) orgtbl-line-start-regexp))
	 (re2 (concat "^" orgtbl-line-start-regexp))
	 (commented (save-excursion (forward-line 0)
				    (cond ((looking-at re1) t)
					  ((looking-at re2) nil)
					  (t (user-error "Not at an org table")))))
	 (re (if commented re1 re2))
	 beg end)
    (save-excursion
      (forward-line 0)
      (while (and (not (eq (point) (point-min)))
                  (looking-at re))
        (forward-line -1))
      (unless (eq (point) (point-min)) (forward-line 1))
      (setq beg (point))
      (while (and (not (eq (point) (point-max)))
                  (looking-at re))
        (forward-line 1))
      (setq end (point)))
    (comment-region beg end (if commented '(4) nil))))

(defun orgtbl-insert-radio-table ()
  "Insert a radio table template appropriate for this major mode."
  (interactive)
  (let* ((e (cl-assoc-if #'derived-mode-p orgtbl-radio-table-templates))
	 (txt (nth 1 e))
	 name pos)
    (unless e (user-error "No radio table setup defined for %s" major-mode))
    (setq name (read-string "Table name: "))
    (while (string-match "%n" txt)
      (setq txt (replace-match name t t txt)))
    (or (bolp) (insert "\n"))
    (setq pos (point))
    (insert txt)
    (goto-char pos)))

;; Put the cursor in a column containing numerical values
;; of an Org table,
;; type C-c " a
;; A new column is added with a bar plot.
;; When the table is refreshed (C-u C-c *),
;; the plot is updated to reflect the new values.

(defun orgtbl-ascii-draw (value min max &optional width characters)
  "Draw an ascii bar in a table.
VALUE is the value to plot, it determines the width of the bar to draw.
MIN is the value that will be displayed as empty (zero width bar).
MAX is the value that will draw a bar filling all the WIDTH.
WIDTH is the span in characters from MIN to MAX.
CHARACTERS is a string that will compose the bar, with shades of grey
from pure white to pure black.  It defaults to a 10 characters string
of regular ascii characters."
  (let* ((width      (ceiling (or width 12)))
	 (characters (or characters " .:;c!lhVHW"))
	 (len        (1- (length characters)))
	 (value      (float (if (numberp value)
				value (string-to-number value))))
	 (relative   (/ (- value min) (- max min)))
	 (steps      (round (* relative width len))))
    (cond ((< steps             0) "too small")
	  ((> steps (* width len)) "too large")
	  (t (let* ((int-division (/ steps len))
		    (remainder    (- steps (* int-division len))))
	       (concat (make-string int-division (elt characters len))
		       (string (elt characters remainder))))))))

;;;###autoload
(defun orgtbl-ascii-plot (&optional ask)
  "Draw an ASCII bar plot in a column.

With cursor in a column containing numerical values, this function
will draw a plot in a new column.

ASK, if given, is a numeric prefix to override the default 12
characters width of the plot.  ASK may also be the `\\[universal-argument]' \
prefix,
which will prompt for the width."
  (interactive "P")
  (let ((col (org-table-current-column))
	(min  1e999)		 ; 1e999 will be converted to infinity
	(max -1e999)		 ; which is the desired result
	(table (org-table-to-lisp))
	(length
	 (cond ((consp ask)
		(read-number "Length of column " 12))
	       ((numberp ask) ask)
	       (t 12))))
    ;; Skip any hline a the top of table.
    (while (eq (car table) 'hline) (pop table))
    ;; Skip table header if any.
    (dolist (x (or (cdr (memq 'hline table)) table))
      (when (consp x)
	(setq x (nth (1- col) x))
	(when (string-match
	       "^[-+]?\\([0-9]*[.]\\)?[0-9]*\\([eE][+-]?[0-9]+\\)?$"
	       x)
	  (setq x (string-to-number x))
	  (when (> min x) (setq min x))
	  (when (< max x) (setq max x)))))
    (org-table-insert-column)
    (org-table-move-column-right)
    (org-table-store-formulas
     (cons
      (cons
       (concat "$" (number-to-string (1+ col)))
       (format "'(%s $%s %s %s %s)"
	       "orgtbl-ascii-draw" col min max length))
      (org-table-get-stored-formulas)))
    (org-table-recalculate t)))

;; Example of extension: unicode characters
;; Here are two examples of different styles.

;; Unicode block characters are used to give a smooth effect.
;; See https://en.wikipedia.org/wiki/Block_Elements
;; Use one of those drawing functions
;; - orgtbl-ascii-draw   (the default ascii)
;; - orgtbl-uc-draw-grid (unicode with a grid effect)
;; - orgtbl-uc-draw-cont (smooth unicode)

;; This is best viewed with the "DejaVu Sans Mono" font
;; (use M-x set-frame-font).

(defun orgtbl-uc-draw-grid (value min max &optional width)
  "Draw a bar in a table using block unicode characters.
It is a variant of `orgtbl-ascii-draw' with Unicode block
characters, for a smooth display.  Bars appear as grids (to the
extent the font allows)."
  ;; https://en.wikipedia.org/wiki/Block_Elements
  ;; best viewed with the "DejaVu Sans Mono" font.
  (orgtbl-ascii-draw value min max width
		     " \u258F\u258E\u258D\u258C\u258B\u258A\u2589"))

(defun orgtbl-uc-draw-cont (value min max &optional width)
  "Draw a bar in a table using block unicode characters.
It is a variant of `orgtbl-ascii-draw' with Unicode block
characters, for a smooth display.  Bars are solid (to the extent
the font allows)."
  (orgtbl-ascii-draw value min max width
		     " \u258F\u258E\u258D\u258C\u258B\u258A\u2589\u2588"))

(provide 'orgtbl-mode)

;;; orgtbl-mode.el ends here
