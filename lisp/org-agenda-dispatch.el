;;; org-agenda.el --- Org agenda menu  -*- lexical-binding: t; -*-

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

;; This file implements Org agenda dialog.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-agenda-common)
(require 'org-agenda-search)
(require 'org-agenda-window)
(require 'org-agenda-files)

(defgroup org-agenda-custom-commands nil
  "Options concerning agenda views in Org mode."
  :tag "Org Agenda Custom Commands"
  :group 'org-agenda)

(defconst org-agenda-custom-commands-local-options
  `(repeat :tag "Local settings for this command.  Remember to quote values"
	   (choice :tag "Setting"
		   (list :tag "Heading for this block"
			 (const org-agenda-overriding-header)
			 (string :tag "Headline"))
		   (list :tag "Files to be searched"
			 (const org-agenda-files)
			 (list
			  (const :format "" quote)
			  (repeat (file))))
		   (list :tag "Sorting strategy"
			 (const org-agenda-sorting-strategy)
			 (list
			  (const :format "" quote)
			  (repeat
			   ,org-sorting-choice)))
		   (list :tag "Prefix format"
			 (const org-agenda-prefix-format :value "  %-12:c%?-12t% s")
			 (string))
		   (list :tag "Number of days in agenda"
			 (const org-agenda-span)
			 (list
			  (const :format "" quote)
			  (choice (const :tag "Day" day)
				  (const :tag "Week" week)
				  (const :tag "Fortnight" fortnight)
				  (const :tag "Month" month)
				  (const :tag "Year" year)
				  (integer :tag "Custom"))))
		   (list :tag "Fixed starting date"
			 (const org-agenda-start-day)
			 (string :value "2007-11-01"))
		   (list :tag "Start on day of week"
			 (const org-agenda-start-on-weekday)
			 (choice :value 1
				 (const :tag "Today" nil)
				 (integer :tag "Weekday No.")))
		   (list :tag "Include data from diary"
			 (const org-agenda-include-diary)
			 (boolean))
		   (list :tag "Deadline Warning days"
			 (const org-deadline-warning-days)
			 (integer :value 1))
		   (list :tag "Category filter preset"
			 (const org-agenda-category-filter-preset)
			 (list
			  (const :format "" quote)
			  (repeat
			   (string :tag "+category or -category"))))
		   (list :tag "Tags filter preset"
			 (const org-agenda-tag-filter-preset)
			 (list
			  (const :format "" quote)
			  (repeat
			   (string :tag "+tag or -tag"))))
		   (list :tag "Effort filter preset"
			 (const org-agenda-effort-filter-preset)
			 (list
			  (const :format "" quote)
			  (repeat
			   (string :tag "+=10 or -=10 or +<10 or ->10"))))
		   (list :tag "Regexp filter preset"
			 (const org-agenda-regexp-filter-preset)
			 (list
			  (const :format "" quote)
			  (repeat
			   (string :tag "+regexp or -regexp"))))
		   (list :tag "Set daily/weekly entry types"
			 (const org-agenda-entry-types)
			 (list
			  (const :format "" quote)
			  (set :greedy t :value ,org-agenda-entry-types
			       (const :deadline)
			       (const :scheduled)
			       (const :deadline*)
			       (const :scheduled*)
			       (const :timestamp)
			       (const :sexp))))
		   (list :tag "Columns format"
			 (const org-overriding-columns-format)
			 (string :tag "Format"))
		   (list :tag "Standard skipping condition"
			 :value (org-agenda-skip-function '(org-agenda-skip-entry-if))
			 (const org-agenda-skip-function)
			 (list
			  (const :format "" quote)
			  (list
			   (choice
			    :tag "Skipping range"
			    (const :tag "Skip entry" org-agenda-skip-entry-if)
			    (const :tag "Skip subtree" org-agenda-skip-subtree-if))
			   (repeat :inline t :tag "Conditions for skipping"
				   (choice
				    :tag "Condition type"
				    (list :tag "Regexp matches" :inline t
					  (const :format "" regexp)
					  (regexp))
				    (list :tag "Regexp does not match" :inline t
					  (const :format "" notregexp)
					  (regexp))
				    (list :tag "TODO state is" :inline t
					  (const todo)
					  (choice
					   (const :tag "Any not-done state" todo)
					   (const :tag "Any done state" done)
					   (const :tag "Any state" any)
					   (list :tag "Keyword list"
						 (const :format "" quote)
						 (repeat (string :tag "Keyword")))))
				    (list :tag "TODO state is not" :inline t
					  (const nottodo)
					  (choice
					   (const :tag "Any not-done state" todo)
					   (const :tag "Any done state" done)
					   (const :tag "Any state" any)
					   (list :tag "Keyword list"
						 (const :format "" quote)
						 (repeat (string :tag "Keyword")))))
				    (const :tag "scheduled" scheduled)
				    (const :tag "not scheduled" notscheduled)
				    (const :tag "deadline" deadline)
				    (const :tag "no deadline" notdeadline)
				    (const :tag "timestamp" timestamp)
				    (const :tag "no timestamp" nottimestamp))))))
		   (list :tag "Non-standard skipping condition"
			 :value (org-agenda-skip-function)
			 (const org-agenda-skip-function)
			 (sexp :tag "Function or form (quoted!)"))
		   (list :tag "Any variable"
			 (variable :tag "Variable")
			 (sexp :tag "Value (sexp)"))))
  "Selection of examples for agenda command settings.
This will be spliced into the custom type of
`org-agenda-custom-commands'.")

(defcustom org-agenda-custom-commands
  '(("n" "Agenda and all TODOs" ((agenda "") (alltodo ""))))
  "Custom commands for the agenda.
\\<org-mode-map>
These commands will be offered on the splash screen displayed by the
agenda dispatcher `\\[org-agenda]'.  Each entry is a list like this:

   (key desc type match settings files)

key      The key (one or more characters as a string) to be associated
         with the command.
desc     A description of the command.  When omitted or nil, a default
         description is built using MATCH.
type     The command type, any of the following symbols:
          agenda      The daily/weekly agenda.
          agenda*     Appointments for current week/day.
          todo        Entries with a specific TODO keyword, in all agenda files.
          search      Entries containing search words entry or headline.
          tags        Tags/Property/TODO match in all agenda files.
          tags-todo   Tags/P/T match in all agenda files, TODO entries only.
          todo-tree   Sparse tree of specific TODO keyword in *current* file.
          tags-tree   Sparse tree with all tags matches in *current* file.
          occur-tree  Occur sparse tree for *current* file.
          alltodo     The global TODO list.
          stuck       Stuck projects.
          ...         A user-defined function.
match    What to search for:
          - a single keyword for TODO keyword searches
          - a tags/property/todo match expression for searches
          - a word search expression for text searches.
          - a regular expression for occur searches
          For all other commands, this should be the empty string.
settings  A list of option settings, similar to that in a let form, so like
          this: ((opt1 val1) (opt2 val2) ...).   The values will be
          evaluated at the moment of execution, so quote them when needed.
files     A list of files to write the produced agenda buffer to with
          the command `org-store-agenda-views'.
          If a file name ends in \".html\", an HTML version of the buffer
          is written out.  If it ends in \".ps\", a PostScript version is
          produced.  Otherwise, only the plain text is written to the file.

You can also define a set of commands, to create a composite agenda buffer.
In this case, an entry looks like this:

  (key desc (cmd1 cmd2 ...) general-settings-for-whole-set files)

where

desc   A description string to be displayed in the dispatcher menu.
cmd    An agenda command, similar to the above.  However, tree commands
       are not allowed.  Valid commands for a set are:
       (agenda \"\" settings)
       (agenda* \"\" settings)
       (alltodo \"\" settings)
       (stuck \"\" settings)
       (todo \"match\" settings files)
       (search \"match\" settings files)
       (tags \"match\" settings files)
       (tags-todo \"match\" settings files)

Each command can carry a list of options, and another set of options can be
given for the whole set of commands.  Individual command options take
precedence over the general options.

When using several characters as key to a command, the first characters
are prefix commands.  For the dispatcher to display useful information, you
should provide a description for the prefix, like

 (setq org-agenda-custom-commands
   \\='((\"h\" . \"HOME + Name tag searches\") ; describe prefix \"h\"
     (\"hl\" tags \"+HOME+Lisa\")
     (\"hp\" tags \"+HOME+Peter\")
     (\"hk\" tags \"+HOME+Kim\")))

See also Info node `(org) Custom Agenda Views'."
  :group 'org-agenda-custom-commands
  :type `(repeat
	  (choice :value ("x" "Describe command here" tags "" nil)
		  (list :tag "Single command"
			(string :tag "Access Key(s) ")
			(option (string :tag "Description"))
			(choice
			 (const :tag "Agenda" agenda)
			 (const :tag "TODO list" alltodo)
			 (const :tag "Search words" search)
			 (const :tag "Stuck projects" stuck)
			 (const :tag "Tags/Property match (all agenda files)" tags)
			 (const :tag "Tags/Property match of TODO entries (all agenda files)" tags-todo)
			 (const :tag "TODO keyword search (all agenda files)" todo)
			 (const :tag "Tags sparse tree (current buffer)" tags-tree)
			 (const :tag "TODO keyword tree (current buffer)" todo-tree)
			 (const :tag "Occur tree (current buffer)" occur-tree)
			 (sexp :tag "Other, user-defined function"))
			(string :tag "Match (only for some commands)")
			,org-agenda-custom-commands-local-options
			(option (repeat :tag "Export" (file :tag "Export to"))))
		  (list :tag "Command series, all agenda files"
			(string :tag "Access Key(s)")
			(string :tag "Description  ")
			(repeat :tag "Component"
				(choice
				 (list :tag "Agenda"
				       (const :format "" agenda)
				       (const :tag "" :format "" "")
				       ,org-agenda-custom-commands-local-options)
				 (list :tag "TODO list (all keywords)"
				       (const :format "" alltodo)
				       (const :tag "" :format "" "")
				       ,org-agenda-custom-commands-local-options)
				 (list :tag "Search words"
				       (const :format "" search)
				       (string :tag "Match")
				       ,org-agenda-custom-commands-local-options)
				 (list :tag "Stuck projects"
				       (const :format "" stuck)
				       (const :tag "" :format "" "")
				       ,org-agenda-custom-commands-local-options)
				 (list :tag "Tags/Property match (all agenda files)"
				       (const :format "" tags)
				       (string :tag "Match")
				       ,org-agenda-custom-commands-local-options)
				 (list :tag "Tags/Property match of TODO entries (all agenda files)"
				       (const :format "" tags-todo)
				       (string :tag "Match")
				       ,org-agenda-custom-commands-local-options)
				 (list :tag "TODO keyword search"
				       (const :format "" todo)
				       (string :tag "Match")
				       ,org-agenda-custom-commands-local-options)
				 (list :tag "Other, user-defined function"
				       (symbol :tag "function")
				       (string :tag "Match")
				       ,org-agenda-custom-commands-local-options)))

			(repeat :tag "Settings for entire command set"
				(list (variable :tag "Any variable")
				      (sexp :tag "Value")))
			(option (repeat :tag "Export" (file :tag "Export to"))))
		  (cons :tag "Prefix key documentation"
			(string :tag "Access Key(s)")
			(string :tag "Description  ")))))

(defcustom org-agenda-custom-commands-contexts nil
  "Alist of custom agenda keys and contextual rules.

For example, if you have a custom agenda command \"p\" and you
want this command to be accessible only from plain text files,
use this:

  (setq org-agenda-custom-commands-contexts
        \\='((\"p\" ((in-file . \"\\\\.txt\\\\'\")))))

Here are the available contexts definitions:

      in-file: command displayed only in matching files
      in-mode: command displayed only in matching modes
  not-in-file: command not displayed in matching files
  not-in-mode: command not displayed in matching modes
    in-buffer: command displayed only in matching buffers
not-in-buffer: command not displayed in matching buffers
   [function]: a custom function taking no argument

If you define several checks, the agenda command will be
accessible if there is at least one valid check.

You can also bind a key to another agenda custom command
depending on contextual rules.

  (setq org-agenda-custom-commands-contexts
        \\='((\"p\" \"q\" ((in-file . \"\\\\.txt\\\\'\")))))

Here it means: in .txt files, use \"p\" as the key for the
agenda command otherwise associated with \"q\".  (The command
originally associated with \"q\" is not displayed to avoid
duplicates.)"
  :version "24.3"
  :group 'org-agenda-custom-commands
  :type '(repeat (list :tag "Rule"
		       (string :tag "        Agenda key")
		       (string :tag "Replace by command")
		       (repeat :tag "Available when"
			       (choice
				(cons :tag "Condition"
				      (choice
				       (const :tag "In file" in-file)
				       (const :tag "Not in file" not-in-file)
				       (const :tag "In buffer" in-buffer)
				       (const :tag "Not in buffer" not-in-buffer)
				       (const :tag "In mode" in-mode)
				       (const :tag "Not in mode" not-in-mode))
				      (regexp))
				(function :tag "Custom function"))))))

(defcustom org-agenda-menu-show-matcher t
  "Non-nil means show the match string in the agenda dispatcher menu.
When nil, the matcher string is not shown, but is put into the help-echo
property so than moving the mouse over the command shows it.
Setting it to nil is good if matcher strings are very long and/or if
you want to use two-columns display (see `org-agenda-menu-two-columns')."
  :group 'org-agenda
  :version "24.1"
  :type 'boolean)

(defcustom org-agenda-menu-two-columns nil
  "Non-nil means, use two columns to show custom commands in the dispatcher.
If you use this, you probably want to set `org-agenda-menu-show-matcher'
to nil."
  :group 'org-agenda
  :version "24.1"
  :type 'boolean)

(defun org-agenda-get-restriction-and-command (prefix-descriptions)
  "The user interface for selecting an agenda command."
  (catch 'exit
    (let* ((bfn (buffer-file-name (buffer-base-buffer)))
	   (restrict-ok (and bfn (derived-mode-p 'org-mode)))
	   (region-p (use-region-p))
	   (custom org-agenda-custom-commands)
	   (selstring "")
	   restriction second-time
	   c entry key type match prefixes rmheader header-end custom1 desc
	   line lines left right n n1)
      (save-window-excursion
        (pop-to-buffer " *Agenda Commands*" '(org-display-buffer-split))
	(erase-buffer)
	(insert (eval-when-compile
		  (let ((header
			 (copy-sequence
			  "Press key for an agenda command:
--------------------------------        <   Buffer, subtree/region restriction
a   Agenda for current week or day      >   Remove restriction
/   Multi-occur                         e   Export agenda views
t   List of all TODO entries            T   Entries with special TODO kwd
m   Match a TAGS/PROP/TODO query        M   Like m, but only TODO entries
s   Search for keywords                 S   Like s, but only TODO entries
?   Find :FLAGGED: entries              C   Configure custom agenda commands
*   Toggle sticky agenda views          #   List stuck projects (!=configure)
"))
			(start 0))
		    (while (string-match
			    "\\(^\\|   \\|(\\)\\(\\S-\\)\\( \\|=\\)"
			    header start)
		      (setq start (match-end 0))
		      (add-text-properties (match-beginning 2) (match-end 2)
					   '(face bold) header))
		    header)))
	(setq header-end (point-marker))
        (unwind-protect
	    (while t
	      (setq custom1 custom)
	      (when (eq rmheader t)
	        (org-goto-line 1)
	        (re-search-forward ":" nil t)
                (delete-region (match-end 0) (line-end-position))
	        (forward-char 1)
	        (looking-at "-+")
                (delete-region (match-end 0) (line-end-position))
	        (move-marker header-end (match-end 0)))
	      (goto-char header-end)
	      (delete-region (point) (point-max))

	      ;; Produce all the lines that describe custom commands and prefixes
	      (setq lines nil)
	      (while (setq entry (pop custom1))
	        (setq key (car entry) desc (nth 1 entry)
		      type (nth 2 entry)
		      match (nth 3 entry))
	        (if (> (length key) 1)
		    (cl-pushnew (string-to-char key) prefixes :test #'equal)
	          (setq line
		        (format
		         "%-4s%-14s"
		         (org-add-props (copy-sequence key)
			     '(face bold))
		         (cond
		          ((string-match "\\S-" desc) desc)
		          ((eq type 'agenda) "Agenda for current week or day")
		          ((eq type 'agenda*) "Appointments for current week or day")
		          ((eq type 'alltodo) "List of all TODO entries")
		          ((eq type 'search) "Word search")
		          ((eq type 'stuck) "List of stuck projects")
		          ((eq type 'todo) "TODO keyword")
		          ((eq type 'tags) "Tags query")
		          ((eq type 'tags-todo) "Tags (TODO)")
		          ((eq type 'tags-tree) "Tags tree")
		          ((eq type 'todo-tree) "TODO kwd tree")
		          ((eq type 'occur-tree) "Occur tree")
		          ((functionp type) (if (symbolp type)
					        (symbol-name type)
					      "Lambda expression"))
		          (t "???"))))
	          (cond
	           ((not (org-string-nw-p match)) nil)
	           (org-agenda-menu-show-matcher
		    (setq line
		          (concat line ": "
			          (cond
			           ((stringp match)
				    (propertize match 'face 'org-warning))
			           ((listp type)
				    (format "set of %d commands" (length type)))))))
	           (t
		    (org-add-props line nil 'help-echo (concat "Matcher: " match))))
	          (push line lines)))
	      (setq lines (nreverse lines))
	      (when prefixes
	        (mapc (lambda (x)
		        (push
		         (format "%s   %s"
			         (org-add-props (char-to-string x)
				     nil 'face 'bold)
			         (or (cdr (assoc (concat selstring
						         (char-to-string x))
					         prefix-descriptions))
				     "Prefix key"))
		         lines))
		      prefixes))

	      ;; Check if we should display in two columns
	      (if org-agenda-menu-two-columns
	          (progn
		    (setq n (length lines)
		          n1 (+ (/ n 2) (mod n 2))
		          right (nthcdr n1 lines)
		          left (copy-sequence lines))
		    (setcdr (nthcdr (1- n1) left) nil))
	        (setq left lines right nil))
	      (while left
	        (insert "\n" (pop left))
	        (when right
	          (if (< (current-column) 40)
		      (move-to-column 40 t)
		    (insert "   "))
	          (insert (pop right))))

	      ;; Make the window the right size
	      (goto-char (point-min))
	      (if second-time
	          (when (not (pos-visible-in-window-p (point-max)))
		    (org-fit-window-to-buffer))
	        (setq second-time t)
	        (org-fit-window-to-buffer))

	      ;; Hint to navigation if window too small for all information
	      (setq header-line-format
		    (when (not (pos-visible-in-window-p (point-max)))
		      "Use C-v, M-v, C-n or C-p to navigate."))

	      ;; Ask for selection
	      (cl-loop
	       do (progn
		    (message "Press key for agenda command%s:"
			     (if (or restrict-ok org-agenda-overriding-restriction)
			         (if org-agenda-overriding-restriction
				     " (restriction lock active)"
			           (if restriction
				       (format " (restricted to %s)" restriction)
				     " (unrestricted)"))
			       ""))
		    (setq c (read-char-exclusive)))
	       until (not (memq c '(14 16 22 134217846)))
	       do (org-scroll c))

	      (message "")
	      (cond
	       ((assoc (char-to-string c) custom)
	        (setq selstring (concat selstring (char-to-string c)))
	        (throw 'exit (cons selstring restriction)))
	       ((memq c prefixes)
	        (setq selstring (concat selstring (char-to-string c))
		      prefixes nil
		      rmheader (or rmheader t)
		      custom (delq nil (mapcar
				        (lambda (x)
				          (if (or (= (length (car x)) 1)
					          (/= (string-to-char (car x)) c))
					      nil
				            (cons (substring (car x) 1) (cdr x))))
				        custom))))
	       ((eq c ?*)
	        (call-interactively #'org-toggle-sticky-agenda)
	        (sit-for 2))
	       ((and (not restrict-ok) (memq c '(?1 ?0 ?<)))
	        (message "Restriction is only possible in Org buffers")
	        (ding) (sit-for 1))
	       ((eq c ?1)
	        (org-agenda-remove-restriction-lock 'noupdate)
	        (setq restriction 'buffer))
	       ((eq c ?0)
	        (org-agenda-remove-restriction-lock 'noupdate)
	        (setq restriction (if region-p 'region 'subtree)))
	       ((eq c ?<)
	        (org-agenda-remove-restriction-lock 'noupdate)
	        (setq restriction
		      (cond
		       ((eq restriction 'buffer)
		        (if region-p 'region 'subtree))
		       ((memq restriction '(subtree region))
		        nil)
		       (t 'buffer))))
	       ((eq c ?>)
	        (org-agenda-remove-restriction-lock 'noupdate)
	        (setq restriction nil))
	       ((and (equal selstring "") (memq c '(?s ?S ?a ?t ?m ?L ?C ?e ?T ?M ?# ?! ?/ ??)))
	        (throw 'exit (cons (setq selstring (char-to-string c)) restriction)))
               ((and (> (length selstring) 0) (eq c ?\d))
                (delete-window)
                (org-agenda-get-restriction-and-command prefix-descriptions))

	       ((equal c ?q) (user-error "Abort"))
	       (t (user-error "Invalid key %c" c))))
          ;; Close  *Agenda Commands* window.
          (quit-window 'kill))))))

(defvar org-agenda-keep-restricted-file-list nil)
(declare-function org-occur "org-sparse-tree" (regexp &optional keep-previous callback))
(declare-function org-match-sparse-tree "org-sparse-tree" (&optional todo-only match start-level))
(declare-function org-occur-in-agenda-files "org-occur" (regexp &optional _nlines))
(declare-function org-agenda-run-series "org-agenda-multi-view" (name series))
;;;###autoload
(defun org-agenda (&optional arg keys restriction)
  "Dispatch agenda commands to collect entries to the agenda buffer.
Prompts for a command to execute.  Any prefix arg will be passed
on to the selected command.  The default selections are:

a     Call `org-agenda-list' to display the agenda for current day or week.
t     Call `org-todo-list' to display the global todo list.
T     Call `org-todo-list' to display the global todo list, select only
      entries with a specific TODO keyword (the user gets a prompt).
m     Call `org-tags-view' to display headlines with tags matching
      a condition  (the user is prompted for the condition).
M     Like `m', but select only TODO entries, no ordinary headlines.
e     Export views to associated files.
s     Search entries for keywords.
S     Search entries for keywords, only with TODO keywords.
/     Multi occur across all agenda files and also files listed
      in `org-agenda-text-search-extra-files'.
<     Restrict agenda commands to buffer, subtree, or region.
      Press several times to get the desired effect.
>     Remove a previous restriction.
#     List \"stuck\" projects.
!     Configure what \"stuck\" means.
C     Configure custom agenda commands.

More commands can be added by configuring the variable
`org-agenda-custom-commands'.  In particular, specific tags and TODO keyword
searches can be pre-defined in this way.

If the current buffer is in Org mode and visiting a file, you can also
first press `<' once to indicate that the agenda should be temporarily
\(until the next use of `\\[org-agenda]') restricted to the current file.
Pressing `<' twice means to restrict to the current subtree or region
\(if active)."
  (interactive "P")
  (catch 'exit
    (let* ((org-keys keys)
	   (prefix-descriptions nil)
	   (org-agenda-buffer-name org-agenda-buffer-name)
	   (org-agenda-window-setup (if (equal (buffer-name)
					       org-agenda-buffer-name)
					'current-window
				      org-agenda-window-setup))
	   (org-agenda-custom-commands-orig org-agenda-custom-commands)
	   (org-agenda-custom-commands
	    ;; normalize different versions
	    (delq nil
		  (mapcar
		   (lambda (x)
		     (cond ((stringp (cdr x))
			    (push x prefix-descriptions)
			    nil)
			   ((stringp (nth 1 x)) x)
			   ((not (nth 1 x)) (cons (car x) (cons "" (cddr x))))
			   (t (cons (car x) (cons "" (cdr x))))))
		   org-agenda-custom-commands)))
	   (org-agenda-custom-commands
	    (org-contextualize-keys
	     org-agenda-custom-commands org-agenda-custom-commands-contexts))
	   ;; (buf (current-buffer))
	   (bfn (buffer-file-name (buffer-base-buffer)))
	   entry type org-match lprops ans) ;; key
      ;; Turn off restriction unless there is an overriding one,
      (unless org-agenda-overriding-restriction
	(unless org-agenda-keep-restricted-file-list
	  ;; There is a request to keep the file list in place
	  (put 'org-agenda-files 'org-restrict nil))
	(setq org-agenda-restrict nil)
	(move-marker org-agenda-restrict-begin nil)
	(move-marker org-agenda-restrict-end nil))
      (unless org-keys
	(setq ans (org-agenda-get-restriction-and-command prefix-descriptions)
	      org-keys (car ans)
	      restriction (cdr ans)))
      ;; If we have sticky agenda buffers, set a name for the buffer,
      ;; depending on the invoking keys.  The user may still set this
      ;; as a command option, which will overwrite what we do here.
      (when org-agenda-sticky
	(setq org-agenda-buffer-name
	      (format "*Org Agenda(%s)*" org-keys)))
      ;; Establish the restriction, if any
      (when (and (not org-agenda-overriding-restriction) restriction)
	(put 'org-agenda-files 'org-restrict (list bfn))
	(cond
	 ((eq restriction 'region)
	  (setq org-agenda-restrict (current-buffer))
	  (move-marker org-agenda-restrict-begin (region-beginning))
	  (move-marker org-agenda-restrict-end (region-end)))
	 ((eq restriction 'subtree)
	  (save-excursion
	    (setq org-agenda-restrict (current-buffer))
	    (org-back-to-heading t)
	    (move-marker org-agenda-restrict-begin (point))
	    (move-marker org-agenda-restrict-end
			 (progn (org-end-of-subtree t)))))
	 ((eq restriction 'buffer)
          (if (not (buffer-narrowed-p))
              (setq org-agenda-restrict t)
            (setq org-agenda-restrict (current-buffer))
	    (move-marker org-agenda-restrict-begin (point-min))
	    (move-marker org-agenda-restrict-end (point-max))))))

      ;; For example the todo list should not need it (but does...)
      (cond
       ((setq entry (assoc org-keys org-agenda-custom-commands))
	(if (or (symbolp (nth 2 entry)) (functionp (nth 2 entry)))
	    (progn
	      ;; FIXME: Is (nth 3 entry) supposed to have access (via dynvars)
              ;; to some of the local variables?  There's no doc about
              ;; that for `org-agenda-custom-commands'.
	      (setq type (nth 2 entry) org-match (eval (nth 3 entry) t)
		    lprops (nth 4 entry))
	      (when org-agenda-sticky
		(setq org-agenda-buffer-name
		      (or (and (stringp org-match) (format "*Org Agenda(%s:%s)*" org-keys org-match))
			  (format "*Org Agenda(%s)*" org-keys))))
	      (cl-progv
	          (mapcar #'car lprops)
	          (mapcar (lambda (binding) (eval (cadr binding) t)) lprops)
	        (pcase type
	          (`agenda
	           (org-agenda-list arg))
	          (`agenda*
	           (org-agenda-list arg nil nil t))
	          (`alltodo
	           (org-todo-list arg))
	          (`search
	           (org-search-view arg org-match nil))
	          (`stuck
	           (org-agenda-list-stuck-projects arg))
	          (`tags
	           (org-tags-view arg org-match))
	          (`tags-todo
	           (org-tags-view '(4) org-match))
	          (`todo
		   (org-todo-list org-match))
		  (`tags-tree
		   (org-check-for-org-mode)
                   (require 'org-sparse-tree)
		   (org-match-sparse-tree arg org-match))
		  (`todo-tree
		   (org-check-for-org-mode)
                   (require 'org-sparse-tree)
		   (org-occur (concat "^" org-outline-regexp "[ \t]*"
				      (regexp-quote org-match) "\\(?:[\t ]\\|$\\)")))
		  (`occur-tree
		   (org-check-for-org-mode)
		   (org-occur org-match))
		  ((pred functionp)
		   (funcall type org-match))
		  ;; FIXME: Will signal an error since it's not `functionp'!
		  ((pred fboundp) (funcall type org-match))
		  (_ (user-error "Invalid custom agenda command type %s" type))))
              (let ((inhibit-read-only t))
	        (add-text-properties (point-min) (point-max)
			             `(org-lprops ,lprops))))
	  (org-agenda-run-series (nth 1 entry) (cddr entry))))
       ((equal org-keys "C")
	(setq org-agenda-custom-commands org-agenda-custom-commands-orig)
	(customize-variable 'org-agenda-custom-commands))
       ((equal org-keys "a") (call-interactively #'org-agenda-list))
       ((equal org-keys "s") (call-interactively #'org-search-view))
       ((equal org-keys "S") (org-call-with-arg 'org-search-view (or arg '(4))))
       ((equal org-keys "t") (call-interactively #'org-todo-list))
       ((equal org-keys "T") (org-call-with-arg 'org-todo-list (or arg '(4))))
       ((equal org-keys "m") (call-interactively #'org-tags-view))
       ((equal org-keys "M") (org-call-with-arg 'org-tags-view (or arg '(4))))
       ((equal org-keys "e") (call-interactively #'org-store-agenda-views))
       ((equal org-keys "?") (org-tags-view nil "+FLAGGED")
	(add-hook
	 'post-command-hook
	 (lambda ()
	   (unless (current-message)
	     (let* ((m (org-agenda-get-any-marker))
		    (note (and m (org-entry-get m "THEFLAGGINGNOTE"))))
	       (when note
		 (message "FLAGGING-NOTE ([?] for more info): %s"
			  (org-add-props
			      (replace-regexp-in-string
			       "\\\\n" "//"
			       (copy-sequence note))
			      nil 'face 'org-warning))))))
	 t t))
       ((equal org-keys "#") (call-interactively #'org-agenda-list-stuck-projects))
       ((equal org-keys "/") (call-interactively #'org-occur-in-agenda-files))
       ((equal org-keys "!") (customize-variable 'org-stuck-projects))
       (t (user-error "Invalid agenda key"))))))

(defun org-agenda-get-any-marker (&optional pos)
  (or (get-text-property (or pos (line-beginning-position)) 'org-hd-marker)
      (get-text-property (or pos (line-beginning-position)) 'org-marker)))

(defun org-check-for-org-mode ()
  "Make sure current buffer is in Org mode.  Error if not."
  (or (derived-mode-p 'org-mode)
      (error "Cannot execute Org agenda command on buffer in %s"
	     major-mode)))

(defun org-add-agenda-custom-command (entry)
  "Replace or add a command in `org-agenda-custom-commands'.
This is mostly for hacking and trying a new command - once the command
works you probably want to add it to `org-agenda-custom-commands' for good."
  (let ((ass (assoc (car entry) org-agenda-custom-commands)))
    (if ass
	(setcdr ass (cdr entry))
      (push entry org-agenda-custom-commands))))

(provide 'org-agenda-dispatch)

;;; org-agenda-dispatch.el ends here
