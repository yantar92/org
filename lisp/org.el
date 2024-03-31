;;; org.el --- Outline-based notes management and organizer -*- lexical-binding: t; -*-

;; Carstens outline-mode for keeping track of everything.
;; Copyright (C) 2004-2024 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: outlines, hypermedia, calendar, text
;; URL: https://orgmode.org
;; Package-Requires: ((emacs "26.1"))

;; Version: 9.8-pre

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
;;
;;; Commentary:
;;
;; Org is a mode for keeping notes, maintaining ToDo lists, and doing
;; project planning with a fast and effective plain-text system.
;;
;; Org mode develops organizational tasks around NOTES files that
;; contain information about projects as plain text.  Org mode is
;; implemented on top of outline-mode, which makes it possible to keep
;; the content of large files well structured.  Visibility cycling and
;; structure editing help to work with the tree.  Tables are easily
;; created with a built-in table editor.  Org mode supports ToDo
;; items, deadlines, time stamps, and scheduling.  It dynamically
;; compiles entries into an agenda that utilizes and smoothly
;; integrates much of the Emacs calendar and diary.  Plain text
;; URL-like links connect to websites, emails, Usenet messages, BBDB
;; entries, and any files related to the projects.  For printing and
;; sharing of notes, an Org file can be exported as a structured ASCII
;; file, as HTML, or (todo and agenda items only) as an iCalendar
;; file.  It can also serve as a publishing tool for a set of linked
;; webpages.
;;
;; Installation and Activation
;; ---------------------------
;; See the corresponding sections in the manual at
;;
;;   https://orgmode.org/org.html#Installation
;;
;; Documentation
;; -------------
;; The documentation of Org mode can be found in the TeXInfo file.  The
;; distribution also contains a PDF version of it.  At the Org mode website,
;; you can read the same text online as HTML.  There is also an excellent
;; reference card made by Philip Rooke.  This card can be found in the
;; doc/ directory.
;;
;; A list of recent changes can be found at
;; https://orgmode.org/Changes.html
;;
;;; Code:

(defvar org-inlinetask-min-level)
(defvar org-footnote-definition-re)

;;;; Require other packages

(require 'org-compat)
(org-assert-version)

(require 'cl-lib)

(eval-when-compile (require 'gnus-sum))

(require 'calendar)
(require 'find-func)
(require 'format-spec)
(require 'thingatpt)

(condition-case nil
    (load (concat (file-name-directory load-file-name)
		  "org-loaddefs")
	  nil t nil t)
  (error
   (message "WARNING: No org-loaddefs.el file could be found from where org.el is loaded.")
   (sit-for 3)
   (message "You need to run \"make\" or \"make autoloads\" from Org lisp directory")
   (sit-for 3)))

(eval-and-compile (require 'org-macs))
(require 'org-compat)
(require 'org-keys)
(require 'ol)
(require 'oc)
(require 'org-table)
(require 'org-fold)
(require 'org-element)
(require 'org-regexps)
(require 'org-dnd)
(require 'org-cdlatex)
(require 'org-indent-static)
(require 'org-fill)
(require 'org-comment)
(require 'org-time)
(require 'org-element-timestamp)
(require 'org-read-date)
(require 'org-timestamp)
(require 'org-tags)
(require 'org-font-lock)
(require 'org-dblock)
(require 'org-property)
(require 'org-priority)
(require 'org-log-note)
(require 'org-mode)
(require 'org-agenda-files)
(require 'org-map)
(require 'org-sparse-tree)
(require 'org-open-file)
(require 'org-mark-ring)
(require 'org-edit)
(require 'org-todo)
(require 'org-planning)
(require 'org-edit-special)

(require 'org-cycle)
(defalias 'org-global-cycle #'org-cycle-global)
(defalias 'org-overview #'org-cycle-overview)
(defalias 'org-content #'org-cycle-content)
(defalias 'org-reveal #'org-fold-reveal)
(defalias 'org-force-cycle-archived #'org-cycle-force-archived)

(defvar org-property-re)

(declare-function calendar-check-holidays "holidays" (date))
(declare-function Info-goto-node "info" (nodename &optional fork strict-case))
(declare-function isearch-no-upper-case-p "isearch" (string regexp-flag))
(declare-function org-add-archive-files "org-archive" (files))
(declare-function org-agenda-entry-get-agenda-timestamp "org-agenda" (pom))
(declare-function org-agenda-todo-yesterday "org-agenda" (&optional arg))
(declare-function org-agenda-list "org-agenda" (&optional arg start-day span with-hour))
(declare-function org-agenda-redo "org-agenda" (&optional all))
(declare-function org-agenda-remove-restriction-lock "org-agenda" (&optional noupdate))
(declare-function org-archive-subtree "org-archive" (&optional find-done))
(declare-function org-archive-subtree-default "org-archive" ())
(declare-function org-archive-to-archive-sibling "org-archive" ())
(declare-function org-attach "org-attach" ())
(declare-function org-attach-dir "org-attach"
		  (&optional create-if-not-exists-p no-fs-check))
(declare-function org-babel-do-in-edit-buffer "ob-core" (&rest body) t)
(declare-function org-babel-tangle-file "ob-tangle" (file &optional target-file lang))
(declare-function org-beamer-mode "ox-beamer" (&optional prefix) t)
(declare-function org-clock-auto-clockout "org-clock" ())
(declare-function org-clock-cancel "org-clock" ())
(declare-function org-clock-display "org-clock" (&optional arg))
(declare-function org-clock-goto "org-clock" (&optional select))
(declare-function org-clock-in "org-clock" (&optional select start-time))
(declare-function org-clock-in-last "org-clock" (&optional arg))
(declare-function org-clock-out "org-clock" (&optional switch-to-state fail-quietly at-time))
(declare-function org-clock-out-if-current "org-clock" ())
(declare-function org-clock-remove-overlays "org-clock" (&optional beg end noremove))
(declare-function org-clock-report "org-clock" (&optional arg))
(declare-function org-clock-sum "org-clock" (&optional tstart tend headline-filter propname))
(declare-function org-clock-sum-current-item "org-clock" (&optional tstart))
(declare-function org-clock-timestamps-down "org-clock" (&optional n))
(declare-function org-clock-timestamps-up "org-clock" (&optional n))
(declare-function org-clock-update-time-maybe "org-clock" ())
(declare-function org-clocktable-shift "org-clock" (dir n))
(declare-function org-columns-quit "org-colview" ())
(declare-function org-columns-insert-dblock "org-colview" ())
(declare-function org-duration-from-minutes "org-duration" (minutes &optional fmt canonical))
(declare-function org-duration-to-minutes "org-duration" (duration &optional canonical))
(declare-function org-export-dispatch "ox" (&optional arg))
(declare-function org-export-get-backend "ox" (name))
(declare-function org-export-get-environment "ox" (&optional backend subtreep ext-plist))
(declare-function org-feed-goto-inbox "org-feed" (feed))
(declare-function org-feed-update-all "org-feed" ())
(declare-function org-goto "org-goto" (&optional alternative-interface))
(declare-function org-id-find-id-file "org-id" (id))
(declare-function org-id-get-create "org-id" (&optional force))
(declare-function org-inlinetask-at-task-p "org-inlinetask" ())
(declare-function org-inlinetask-outline-regexp "org-inlinetask" ())
(declare-function org-inlinetask-toggle-visibility "org-inlinetask" ())
(declare-function org-latex-make-preamble "ox-latex" (info &optional template snippet?))
(declare-function org-num-mode "org-num" (&optional arg))
(declare-function org-plot/gnuplot "org-plot" (&optional params))
(declare-function org-persist-load "org-persist")
(declare-function org-tags-view "org-agenda" (&optional todo-only match))
(declare-function org-timer "org-timer" (&optional restart no-insert))
(declare-function org-timer-item "org-timer" (&optional arg))
(declare-function org-timer-pause-or-continue "org-timer" (&optional stop))
(declare-function org-timer-set-timer "org-timer" (&optional opt))
(declare-function org-timer-start "org-timer" (&optional offset))
(declare-function org-timer-stop "org-timer" ())
(declare-function org-toggle-archive-tag "org-archive" (&optional find-done))
(declare-function org-update-radio-target-regexp "ol" ())

(defvar org-agenda-buffer-name)
(defvar org-indent-indentation-per-level)
(defvar org-radio-target-regexp)
(defvar org-target-link-regexp)
(defvar org-target-regexp)
(defvar org-id-overriding-file-name)
(defvar org-edit-src-content-indentation)

;; load languages based on value of `org-babel-load-languages'
(defvar org-babel-load-languages)

(defvar crm-separator)  ; dynamically scoped param

;;;###autoload
(defun org-babel-do-load-languages (sym value)
  "Load the languages defined in `org-babel-load-languages'."
  (set-default-toplevel-value sym value)
  (dolist (pair org-babel-load-languages)
    (let ((active (cdr pair)) (lang (symbol-name (car pair))))
      (if active
	  (require (intern (concat "ob-" lang)))
	(fmakunbound
	 (intern (concat "org-babel-execute:" lang)))
	(fmakunbound
	 (intern (concat "org-babel-expand-body:" lang)))))))


;;;###autoload
(defun org-babel-load-file (file &optional compile)
  "Load Emacs Lisp source code blocks in the Org FILE.
This function exports the source code using `org-babel-tangle'
and then loads the resulting file using `load-file'.  With
optional prefix argument COMPILE, the tangled Emacs Lisp file is
byte-compiled before it is loaded."
  (interactive "fFile to load: \nP")
  (let ((tangled-file (concat (file-name-sans-extension file) ".el")))
    ;; Tangle only if the Elisp file is older than the Org file.
    ;; Catch the case when the .el file exists while the .org file is missing.
    (unless (file-exists-p file)
      (error "File to tangle does not exist: %s" file))
    (when (file-newer-than-file-p file tangled-file)
      (org-babel-tangle-file file
                             tangled-file
                             (rx string-start
                                 (or "emacs-lisp" "elisp")
                                 string-end))
      ;; Make sure that tangled file modification time is
      ;; updated even when `org-babel-tangle-file' does not make changes.
      ;; This avoids re-tangling changed FILE where the changes did
      ;; not affect the tangled code.
      (when (file-exists-p tangled-file)
        (set-file-times tangled-file)))
    (if compile
	(progn
	  (byte-compile-file tangled-file)
	  (load-file (byte-compile-dest-file tangled-file))
	  (message "Compiled and loaded %s" tangled-file))
      (load-file tangled-file)
      (message "Loaded %s" tangled-file))))

(defcustom org-babel-load-languages '((emacs-lisp . t))
  "Languages which can be evaluated in Org buffers.
\\<org-mode-map>
This list can be used to load support for any of the available
languages with babel support (see info node `(org) Languages').  Each
language will depend on a different set of system executables and/or
Emacs modes.

When a language is \"loaded\", code blocks in that language can
be evaluated with `org-babel-execute-src-block', which is bound
by default to \\[org-ctrl-c-ctrl-c].

The `org-babel-no-eval-on-ctrl-c-ctrl-c' option can be set to
remove code block evaluation from \\[org-ctrl-c-ctrl-c].  By
default, only Emacs Lisp is loaded, since it has no specific
requirement."
  :group 'org-babel
  :set 'org-babel-do-load-languages
  :package-version '(Org . "9.6")
  :type '(alist :tag "Babel Languages"
		:key-type
		(choice
		 (const :tag "Awk" awk)
		 (const :tag "C, D, C++, and cpp" C)
		 (const :tag "R" R)
                 (const :tag "Calc" calc)
		 (const :tag "Clojure and ClojureScript" clojure)
		 (const :tag "CSS" css)
		 (const :tag "Ditaa" ditaa)
		 (const :tag "Dot" dot)
                 (const :tag "Emacs Lisp" emacs-lisp)
                 (const :tag "Eshell" eshell)
		 (const :tag "Forth" forth)
		 (const :tag "Fortran" fortran)
		 (const :tag "GnuPlot" gnuplot)
		 (const :tag "Groovy" groovy)
		 (const :tag "Haskell" haskell)
                 (const :tag "Java" java)
		 (const :tag "JavaScript" js)
                 (const :tag "Julia" julia)
                 (const :tag "LaTeX" latex)
                 (const :tag "LilyPond" lilypond)
		 (const :tag "Lisp" lisp)
                 (const :tag "Lua" lua)
		 (const :tag "Makefile" makefile)
		 (const :tag "Maxima" maxima)
                 (const :tag "OCaml" ocaml)
		 (const :tag "Octave and MatLab" octave)
		 (const :tag "Org" org)
		 (const :tag "Perl" perl)
                 (const :tag "Processing" processing)
		 (const :tag "PlantUML" plantuml)
		 (const :tag "Python" python)
		 (const :tag "Ruby" ruby)
		 (const :tag "Sass" sass)
		 (const :tag "Scheme" scheme)
		 (const :tag "Screen" screen)
                 (const :tag "Sed" sed)
		 (const :tag "Shell Script" shell)
                 (const :tag "Sql" sql)
		 (const :tag "Sqlite" sqlite))
		:value-type (boolean :tag "Activate" :value t)))

;;;; Customization variables

;;; Version
(org-check-version)

;;;###autoload
(defun org-version (&optional here full message)
  "Show the Org version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given."
  (interactive (list current-prefix-arg t (not current-prefix-arg)))
  (let ((org-dir (ignore-errors (org-find-library-dir "org")))
        (save-load-suffixes load-suffixes)
	(load-suffixes (list ".el"))
	(org-install-dir
	 (ignore-errors (org-find-library-dir "org-loaddefs"))))
    (unless (and (fboundp 'org-release) (fboundp 'org-git-version))
      (org-load-noerror-mustsuffix (concat org-dir "org-version")))
    (let* ((load-suffixes save-load-suffixes)
	   (release (org-release))
	   (git-version (org-git-version))
	   (version (format "Org mode version %s (%s @ %s)"
			    release
			    git-version
			    (if org-install-dir
				(if (string= org-dir org-install-dir)
				    org-install-dir
				  (concat "mixed installation! "
					  org-install-dir
					  " and "
					  org-dir))
			      "org-loaddefs.el can not be found!")))
	   (version1 (if full version release)))
      (when here (insert version1))
      (when message (message "%s" version1))
      version1)))

(defconst org-version (org-version))


;;; The custom variables

(defgroup org nil
  "Outline-based notes management and organizer."
  :tag "Org"
  :group 'outlines
  :group 'calendar)

(defcustom org-load-hook nil
  "Hook that is run after org.el has been loaded."
  :group 'org
  :type 'hook)

(make-obsolete-variable
 'org-load-hook
 "use `with-eval-after-load' instead." "9.5")

(defvar org-modules)  ; defined below
(defvar org-modules-loaded nil
  "Have the modules been loaded already?")

;;;###autoload
(defun org-load-modules-maybe (&optional force)
  "Load all extensions listed in `org-modules'."
  (when (or force (not org-modules-loaded))
    (dolist (ext org-modules)
      (condition-case-unless-debug nil (require ext)
	(error (message "Problems while trying to load feature `%s'" ext))))
    (setq org-modules-loaded t)))

(defun org-set-modules (var value)
  "Set VAR to VALUE and call `org-load-modules-maybe' with the force flag."
  (set-default-toplevel-value var value)
  (when (featurep 'org)
    (org-load-modules-maybe 'force)
    (org-element-cache-reset 'all)))

(defcustom org-modules '(ol-doi ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww)
  "Modules that should always be loaded together with org.el.

If a description starts with <C>, the file is not part of Emacs and Org mode,
so loading it will require that you have properly installed org-contrib
package from NonGNU Emacs Lisp Package Archive
https://elpa.nongnu.org/nongnu/org-contrib.html

You can also use this system to load external packages (i.e. neither Org
core modules, nor org-contrib modules).  Just add symbols
to the end of the list.  If the package is called org-xyz.el, then you need
to add the symbol `xyz', and the package must have a call to:

   (provide \\='org-xyz)

For export specific modules, see also `org-export-backends'."
  :group 'org
  :set 'org-set-modules
  :package-version '(Org . "9.5")
  :type
  '(set :greedy t
	(const :tag "   bbdb:              Links to BBDB entries" ol-bbdb)
	(const :tag "   bibtex:            Links to BibTeX entries" ol-bibtex)
	(const :tag "   crypt:             Encryption of subtrees" org-crypt)
	(const :tag "   ctags:             Access to Emacs tags with links" org-ctags)
	(const :tag "   docview:           Links to Docview buffers" ol-docview)
        (const :tag "   doi:               Links to DOI references" ol-doi)
	(const :tag "   eww:               Store link to URL of Eww" ol-eww)
	(const :tag "   gnus:              Links to GNUS folders/messages" ol-gnus)
	(const :tag "   habit:             Track your consistency with habits" org-habit)
	(const :tag "   id:                Global IDs for identifying entries" org-id)
	(const :tag "   info:              Links to Info nodes" ol-info)
	(const :tag "   inlinetask:        Tasks independent of outline hierarchy" org-inlinetask)
	(const :tag "   irc:               Links to IRC/ERC chat sessions" ol-irc)
	(const :tag "   mhe:               Links to MHE folders/messages" ol-mhe)
	(const :tag "   mouse:             Additional mouse support" org-mouse)
	(const :tag "   protocol:          Intercept calls from emacsclient" org-protocol)
	(const :tag "   rmail:             Links to RMAIL folders/messages" ol-rmail)
	(const :tag "   tempo:             Fast completion for structures" org-tempo)
	(const :tag "   w3m:               Special cut/paste from w3m to Org mode." ol-w3m)
	(const :tag "   eshell:            Links to working directories in Eshell" ol-eshell)

	(const :tag "C  annotate-file:     Annotate a file with Org syntax" org-annotate-file)
	(const :tag "C  bookmark:          Links to bookmarks" ol-bookmark)
	(const :tag "C  checklist:         Extra functions for checklists in repeated tasks" org-checklist)
	(const :tag "C  choose:            Use TODO keywords to mark decisions states" org-choose)
	(const :tag "C  collector:         Collect properties into tables" org-collector)
	(const :tag "C  depend:            TODO dependencies for Org mode\n\t\t\t(PARTIALLY OBSOLETE, see built-in dependency support))" org-depend)
	(const :tag "C  elisp-symbol:      Links to emacs-lisp symbols" ol-elisp-symbol)
	(const :tag "C  eval-light:        Evaluate inbuffer-code on demand" org-eval-light)
	(const :tag "C  eval:              Include command output as text" org-eval)
	(const :tag "C  expiry:            Expiry mechanism for Org entries" org-expiry)
	(const :tag "C  git-link:          Links to specific file version" ol-git-link)
	(const :tag "C  interactive-query: Interactive modification of tags query\n\t\t\t(PARTIALLY OBSOLETE, see secondary filtering)" org-interactive-query)
        (const :tag "C  invoice:           Help manage client invoices in Org mode" org-invoice)
	(const :tag "C  learn:             SuperMemo's incremental learning algorithm" org-learn)
	(const :tag "C  mac-iCal:          Imports events from iCal.app to the Emacs diary" org-mac-iCal)
	(const :tag "C  mac-link:          Grab links and url from various mac Applications" org-mac-link)
	(const :tag "C  mairix:            Hook mairix search into Org for different MUAs" org-mairix)
	(const :tag "C  man:               Links to man pages in Org mode" ol-man)
	(const :tag "C  mew:               Links to Mew folders/messages" ol-mew)
	(const :tag "C  notify:            Notifications for Org mode" org-notify)
	(const :tag "C  notmuch:           Provide Org links to notmuch searches or messages" ol-notmuch)
	(const :tag "C  panel:             Simple routines for us with bad memory" org-panel)
	(const :tag "C  registry:          A registry for Org links" org-registry)
	(const :tag "C  screen:            Visit screen sessions through links" org-screen)
	(const :tag "C  screenshot:        Take and manage screenshots in Org files" org-screenshot)
	(const :tag "C  secretary:         Team management with Org" org-secretary)
	(const :tag "C  sqlinsert:         Convert Org tables to SQL insertions" orgtbl-sqlinsert)
	(const :tag "C  toc:               Table of contents for Org buffer" org-toc)
	(const :tag "C  track:             Keep up with Org mode development" org-track)
	(const :tag "C  velocity           Something like Notational Velocity for Org" org-velocity)
	(const :tag "C  vm:                Links to VM folders/messages" ol-vm)
	(const :tag "C  wikinodes:         CamelCase wiki-like links" org-wikinodes)
	(const :tag "C  wl:                Links to Wanderlust folders/messages" ol-wl)
	(repeat :tag "External packages" :inline t (symbol :tag "Package"))))

(defvar org-export-registered-backends) ; From ox.el.
(declare-function org-export-derived-backend-p "ox" (backend &rest backends))
(declare-function org-export-backend-name "ox" (backend) t)
(defcustom org-export-backends '(ascii html icalendar latex odt)
  "List of export backends that should be always available.

If a description starts with <C>, the file is not part of Emacs and Org mode,
so loading it will require that you have properly installed org-contrib
package from NonGNU Emacs Lisp Package Archive
https://elpa.nongnu.org/nongnu/org-contrib.html

Unlike to `org-modules', libraries in this list will not be
loaded along with Org, but only once the export framework is
needed.

This variable needs to be set before org.el is loaded.  If you
need to make a change while Emacs is running, use the customize
interface or run the following code, where VAL stands for the new
value of the variable, after updating it:

  (progn
    (setq org-export-registered-backends
          (cl-remove-if-not
           (lambda (backend)
             (let ((name (org-export-backend-name backend)))
               (or (memq name val)
                   (catch \\='parentp
                     (dolist (b val)
                       (and (org-export-derived-backend-p b name)
                            (throw \\='parentp t)))))))
           org-export-registered-backends))
    (let ((new-list (mapcar #\\='org-export-backend-name
                            org-export-registered-backends)))
      (dolist (backend val)
        (cond
         ((not (load (format \"ox-%s\" backend) t t))
          (message \"Problems while trying to load export backend \\=`%s\\='\"
                   backend))
         ((not (memq backend new-list)) (push backend new-list))))
      (set-default \\='org-export-backends new-list)))

Adding a backend to this list will also pull the backend it
depends on, if any."
  :group 'org
  :group 'org-export
  :version "26.1"
  :package-version '(Org . "9.0")
  :initialize 'custom-initialize-set
  :set (lambda (var val)
	 (if (not (featurep 'ox)) (set-default-toplevel-value var val)
	   ;; Any backend not required anymore (not present in VAL and not
	   ;; a parent of any backend in the new value) is removed from the
	   ;; list of registered backends.
	   (setq org-export-registered-backends
		 (cl-remove-if-not
		  (lambda (backend)
		    (let ((name (org-export-backend-name backend)))
		      (or (memq name val)
			  (catch 'parentp
			    (dolist (b val)
			      (and (org-export-derived-backend-p b name)
				   (throw 'parentp t)))))))
		  org-export-registered-backends))
	   ;; Now build NEW-LIST of both new backends and required
	   ;; parents.
	   (let ((new-list (mapcar #'org-export-backend-name
				   org-export-registered-backends)))
	     (dolist (backend val)
	       (cond
		((not (load (format "ox-%s" backend) t t))
		 (message "Problems while trying to load export backend `%s'"
			  backend))
		((not (memq backend new-list)) (push backend new-list))))
	     ;; Set VAR to that list with fixed dependencies.
	     (set-default-toplevel-value var new-list))))
  :type '(set :greedy t
	      (const :tag "   ascii       Export buffer to ASCII format" ascii)
	      (const :tag "   beamer      Export buffer to Beamer presentation" beamer)
	      (const :tag "   html        Export buffer to HTML format" html)
	      (const :tag "   icalendar   Export buffer to iCalendar format" icalendar)
	      (const :tag "   latex       Export buffer to LaTeX format" latex)
	      (const :tag "   man         Export buffer to MAN format" man)
	      (const :tag "   md          Export buffer to Markdown format" md)
	      (const :tag "   odt         Export buffer to ODT format" odt)
	      (const :tag "   org         Export buffer to Org format" org)
	      (const :tag "   texinfo     Export buffer to Texinfo format" texinfo)
	      (const :tag "C  confluence  Export buffer to Confluence Wiki format" confluence)
	      (const :tag "C  deck        Export buffer to deck.js presentations" deck)
	      (const :tag "C  freemind    Export buffer to Freemind mindmap format" freemind)
	      (const :tag "C  groff       Export buffer to Groff format" groff)
	      (const :tag "C  koma-letter Export buffer to KOMA Scrlttrl2 format" koma-letter)
	      (const :tag "C  RSS 2.0     Export buffer to RSS 2.0 format" rss)
	      (const :tag "C  s5          Export buffer to s5 presentations" s5)
	      (const :tag "C  taskjuggler Export buffer to TaskJuggler format" taskjuggler)))

(eval-after-load 'ox
  '(dolist (backend org-export-backends)
     (condition-case-unless-debug nil (require (intern (format "ox-%s" backend)))
       (error (message "Problems while trying to load export backend `%s'"
		       backend)))))

(defcustom org-support-shift-select nil
  "Non-nil means make shift-cursor commands select text when possible.
\\<org-mode-map>
In Emacs 23, when `shift-select-mode' is on, shifted cursor keys
start selecting a region, or enlarge regions started in this way.
In Org mode, in special contexts, these same keys are used for
other purposes, important enough to compete with shift selection.
Org tries to balance these needs by supporting `shift-select-mode'
outside these special contexts, under control of this variable.

The default of this variable is nil, to avoid confusing behavior.  Shifted
cursor keys will then execute Org commands in the following contexts:
- on a headline, changing TODO state (left/right) and priority (up/down)
- on a time stamp, changing the time
- in a plain list item, changing the bullet type
- in a property definition line, switching between allowed values
- in the BEGIN line of a clock table (changing the time block).
- in a table, moving the cell in the specified direction.
Outside these contexts, the commands will throw an error.

When this variable is t and the cursor is not in a special
context, Org mode will support shift-selection for making and
enlarging regions.  To make this more effective, the bullet
cycling will no longer happen anywhere in an item line, but only
if the cursor is exactly on the bullet.

If you set this variable to the symbol `always', then the keys
will not be special in headlines, property lines, item lines, and
table cells, to make shift selection work there as well.  If this is
what you want, you can use the following alternative commands:
`\\[org-todo]' and `\\[org-priority]' \
to change TODO state and priority,
`\\[universal-argument] \\[universal-argument] \\[org-todo]' \
can be used to switch TODO sets,
`\\[org-ctrl-c-minus]' to cycle item bullet types,
and properties can be edited by hand or in column view.

However, when the cursor is on a timestamp, shift-cursor commands
will still edit the time stamp - this is just too good to give up."
  :group 'org
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "When outside special context" t)
	  (const :tag "Everywhere except timestamps" always)))

(defcustom org-loop-over-headlines-in-active-region t
  "Shall some commands act upon headlines in the active region?

When set to t, some commands will be performed in all headlines
within the active region.

When set to `start-level', some commands will be performed in all
headlines within the active region, provided that these headlines
are of the same level than the first one.

When set to a string, those commands will be performed on the
matching headlines within the active region.  Such string must be
a tags/property/todo match as it is used in the agenda tags view.

The list of commands is: `org-schedule', `org-deadline',
`org-todo', `org-set-tags-command', `org-archive-subtree',
`org-archive-set-tag', `org-toggle-archive-tag' and
`org-archive-to-archive-sibling'.  The archiving commands skip
already archived entries.

See `org-agenda-loop-over-headlines-in-active-region' for the
equivalent option for agenda views."
  :type '(choice (const :tag "Don't loop" nil)
		 (const :tag "All headlines in active region" t)
		 (const :tag "In active region, headlines at the same level than the first one" start-level)
		 (string :tag "Tags/Property/Todo matcher"))
  :package-version '(Org . "9.4")
  :group 'org-todo
  :group 'org-archive)

(unless (boundp 'untrusted-content)
  (defvar untrusted-content nil))
(defvar untrusted-content) ; defined in files.el since Emacs 29.3
(defvar org--latex-preview-when-risky nil
  "If non-nil, enable LaTeX preview in Org buffers from unsafe source.

Some specially designed LaTeX code may generate huge pdf or log files
that may exhaust disk space.

This variable controls how to handle LaTeX preview when rendering LaTeX
fragments that originate from incoming email messages.  It has no effect
when Org mode is unable to determine the origin of the Org buffer.

An Org buffer is considered to be from unsafe source when the
variable `untrusted-content' has a non-nil value in the buffer.

If this variable is non-nil, LaTeX previews are rendered unconditionally.

This variable may be renamed or changed in the future.")

(defgroup org-keywords nil
  "Keywords in Org mode."
  :tag "Org Keywords"
  :group 'org)

(defcustom org-closed-keep-when-no-todo nil
  "Remove CLOSED: timestamp when switching back to a non-todo state?"
  :group 'org-todo
  :group 'org-keywords
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defgroup org-structure nil
  "Options concerning the general structure of Org files."
  :tag "Org Structure"
  :group 'org)

(defcustom org-indirect-buffer-display 'other-window
  "How should indirect tree buffers be displayed?

This applies to indirect buffers created with the commands
`org-tree-to-indirect-buffer' and `org-agenda-tree-to-indirect-buffer'.

Valid values are:
current-window   Display in the current window
other-window     Just display in another window.
dedicated-frame  Create one new frame, and reuse it each time.
new-frame        Make a new frame each time.  Note that in this case
                 previously-made indirect buffers are kept, and you need to
                 kill these buffers yourself."
  :group 'org-structure
  :group 'org-agenda-windows
  :type '(choice
	  (const :tag "In current window" current-window)
	  (const :tag "In current frame, other window" other-window)
	  (const :tag "Each time a new frame" new-frame)
	  (const :tag "One dedicated frame" dedicated-frame)))

(defcustom org-bookmark-names-plist
  '(:last-capture "org-capture-last-stored"
		  :last-refile "org-refile-last-stored"
		  :last-capture-marker "org-capture-last-stored-marker")
  "Names for bookmarks automatically set by some Org commands.
This can provide strings as names for a number of bookmarks Org sets
automatically.  The following keys are currently implemented:
  :last-capture
  :last-capture-marker
  :last-refile
When a key does not show up in the property list, the corresponding bookmark
is not set."
  :group 'org-structure
  :type 'plist)

(defgroup org-edit-structure nil
  "Options concerning structure editing in Org mode."
  :tag "Org Edit Structure"
  :group 'org-structure)

(defvar org-odd-levels-only)

(defgroup org-todo nil
  "Options concerning TODO items in Org mode."
  :tag "Org TODO"
  :group 'org)

(defgroup org-progress nil
  "Options concerning Progress logging in Org mode."
  :tag "Org Progress"
  :group 'org-time)

(defvar org-todo-interpretation-widgets
  '((:tag "Sequence (cycling hits every state)" sequence)
    (:tag "Type     (cycling directly to DONE)" type))
  "The available interpretation symbols for customizing `org-todo-keywords'.
Interested libraries should add to this list.")

(defcustom org-todo-keywords '((sequence "TODO" "DONE"))
  "List of TODO entry keyword sequences and their interpretation.
\\<org-mode-map>This is a list of sequences.

Each sequence starts with a symbol, either `sequence' or `type',
indicating if the keywords should be interpreted as a sequence of
action steps, or as different types of TODO items.  The first
keywords are states requiring action - these states will select a headline
for inclusion into the global TODO list Org produces.  If one of the
\"keywords\" is the vertical bar, \"|\", the remaining keywords
signify that no further action is necessary.  If \"|\" is not found,
the last keyword is treated as the only DONE state of the sequence.

The command `\\[org-todo]' cycles an entry through these states, and one
additional state where no keyword is present.  For details about this
cycling, see the manual.

TODO keywords and interpretation can also be set on a per-file basis with
the special #+SEQ_TODO and #+TYP_TODO lines.

Each keyword can optionally specify a character for fast state selection
\(in combination with the variable `org-use-fast-todo-selection')
and specifiers for state change logging, using the same syntax that
is used in the \"#+TODO:\" lines.  For example, \"WAIT(w)\" says that
the WAIT state can be selected with the \"w\" key.  \"WAIT(w!)\"
indicates to record a time stamp each time this state is selected.

Each keyword may also specify if a timestamp or a note should be
recorded when entering or leaving the state, by adding additional
characters in the parenthesis after the keyword.  This looks like this:
\"WAIT(w@/!)\".  \"@\" means to add a note (with time), \"!\" means to
record only the time of the state change.  With X and Y being either
\"@\" or \"!\", \"X/Y\" means use X when entering the state, and use
Y when leaving the state if and only if the *target* state does not
define X.  You may omit any of the fast-selection key or X or /Y,
so WAIT(w@), WAIT(w/@) and WAIT(@/@) are all valid.

For backward compatibility, this variable may also be just a list
of keywords.  In this case the interpretation (sequence or type) will be
taken from the (otherwise obsolete) variable `org-todo-interpretation'."
  :group 'org-todo
  :group 'org-keywords
  :type '(choice
	  (repeat :tag "Old syntax, just keywords"
		  (string :tag "Keyword"))
	  (repeat :tag "New syntax"
		  (cons
		   (choice
		    :tag "Interpretation"
		    ;;Quick and dirty way to see
                    ;;`org-todo-interpretation'.  This takes the
		    ;;place of item arguments
		    :convert-widget
		    (lambda (widget)
		      (widget-put widget
				  :args (mapcar
					 (lambda (x)
					   (widget-convert
					    (cons 'const x)))
					 org-todo-interpretation-widgets))
		      widget))
		   (repeat
		    (string :tag "Keyword"))))))

(defvar-local org-todo-keywords-1 nil
  "All TODO and DONE keywords active in a buffer.")
(defvar org-todo-keywords-for-agenda nil)
(defvar org-done-keywords-for-agenda nil)
(defvar org-todo-keyword-alist-for-agenda nil)
(defvar org-tag-alist-for-agenda nil
  "Alist of all tags from all agenda files.")
(defvar org-tag-groups-alist-for-agenda nil
  "Alist of all groups tags from all current agenda files.")
(defvar org-agenda-contributing-files nil)
(defvar-local org-done-keywords nil)
(defvar-local org-todo-heads nil)
(defvar-local org-todo-sets nil)
(defvar-local org-todo-kwd-alist nil)
(defvar-local org-todo-key-alist nil)
(defvar-local org-todo-key-trigger nil)

(defcustom org-log-note-clock-out nil
  "Non-nil means record a note when clocking out of an item.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: lognoteclock-out
   #+STARTUP: nolognoteclock-out"
  :group 'org-todo
  :group 'org-progress
  :type 'boolean)

(defgroup org-time nil
  "Options concerning time stamps and deadlines in Org mode."
  :tag "Org Time"
  :group 'org)

(defcustom org-deadline-warning-days 14
  "Number of days before expiration during which a deadline becomes active.
This variable governs the display in sparse trees and in the agenda.
When 0 or negative, it means use this number (the absolute value of it)
even if a deadline has a different individual lead time specified.

Custom commands can set this variable in the options section."
  :group 'org-time
  :group 'org-agenda-daily/weekly
  :type 'integer)

(defcustom org-scheduled-delay-days 0
  "Number of days before a scheduled item becomes active.
This variable governs the display in sparse trees and in the agenda.
The default value (i.e. 0) means: don't delay scheduled item.
When negative, it means use this number (the absolute value of it)
even if a scheduled item has a different individual delay time
specified.

Custom commands can set this variable in the options section."
  :group 'org-time
  :group 'org-agenda-daily/weekly
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'integer)

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

(defcustom org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS"
  "The default column format, if no other format has been defined.
This variable can be set on the per-file basis by inserting a line

#+COLUMNS: %25ITEM ....."
  :group 'org-properties
  :type 'string)

(defcustom org-columns-default-format-for-agenda nil
  "The default column format in an agenda buffer.
This will be used for column view in the agenda unless a format has
been set by adding `org-overriding-columns-format' to the local
settings list of a custom agenda view.  When nil, the columns format
for the first item in the agenda list will be used, or as a fall-back,
`org-columns-default-format'."
  :group 'org-properties
  :type '(choice
	  (const :tag "No default" nil)
	  (string :tag "Format string")))

(defcustom org-columns-ellipses ".."
  "The ellipses to be used when a field in column view is truncated.
When this is the empty string, as many characters as possible are shown,
but then there will be no visual indication that the field has been truncated.
When this is a string of length N, the last N characters of a truncated
field are replaced by this string.  If the column is narrower than the
ellipses string, only part of the ellipses string will be shown."
  :group 'org-properties
  :type 'string)

(defgroup org-agenda nil
  "Options concerning agenda views in Org mode."
  :tag "Org Agenda"
  :group 'org)

(defvaralias 'org-agenda-multi-occur-extra-files
  'org-agenda-text-search-extra-files)

(defcustom org-agenda-text-search-extra-files nil
  "List of extra files to be searched by text search commands.
These files will be searched in addition to the agenda files by the
commands `org-search-view' (`\\[org-agenda] s') \
and `org-occur-in-agenda-files'.
Note that these files will only be searched for text search commands,
not for the other agenda views like todo lists, tag searches or the weekly
agenda.  This variable is intended to list notes and possibly archive files
that should also be searched by these two commands.
In fact, if the first element in the list is the symbol `agenda-archives',
then all archive files of all agenda files will be added to the search
scope."
  :group 'org-agenda
  :type '(set :greedy t
	      (const :tag "Agenda Archives" agenda-archives)
	      (repeat :inline t (file))))

(defgroup org-latex nil
  "Options for embedding LaTeX code into Org mode."
  :tag "Org LaTeX"
  :group 'org)

;; FIXME: Unused.
(defcustom org-format-latex-signal-error t
  "Non-nil means signal an error when image creation of LaTeX snippets fails.
When nil, just push out a message."
  :group 'org-latex
  :version "24.1"
  :type 'boolean)

(defcustom org-latex-to-mathml-jar-file nil
  "Value of\"%j\" in `org-latex-to-mathml-convert-command'.
Use this to specify additional executable file say a jar file.

When using MathToWeb as the converter, specify the full-path to
your mathtoweb.jar file."
  :group 'org-latex
  :version "24.1"
  :type '(choice
	  (const :tag "None" nil)
	  (file :tag "JAR file" :must-match t)))

(defcustom org-latex-to-mathml-convert-command nil
  "Command to convert LaTeX fragments to MathML.
Replace format-specifiers in the command as noted below and use
`shell-command' to convert LaTeX to MathML.
%j:     Executable file in fully expanded form as specified by
        `org-latex-to-mathml-jar-file'.
%I:     Input LaTeX file in fully expanded form.
%i:     Shell-escaped LaTeX fragment to be converted.
        It must not be used inside a quoted argument, the result of %i
        expansion inside a quoted argument is undefined.
%o:     Output MathML file.

This command is used by `org-create-math-formula'.

When using MathToWeb as the converter, set this option to
\"java -jar %j -unicode -force -df %o %I\".

When using LaTeXML set this option to
\"latexmlmath %i --presentationmathml=%o\"."
  :group 'org-latex
  :package-version '(Org . "9.7")
  :type '(choice
	  (const :tag "None" nil)
	  (string :tag "\nShell command")))

(defcustom org-latex-to-html-convert-command nil
  "Shell command to convert LaTeX fragments to HTML.
This command is very open-ended: the output of the command will
directly replace the LaTeX fragment in the resulting HTML.
Replace format-specifiers in the command as noted below and use
`shell-command' to convert LaTeX to HTML.
%i:     The LaTeX fragment to be converted (shell-escaped).
        It must not be used inside a quoted argument, the result of %i
        expansion inside a quoted argument is undefined.

For example, this could be used with LaTeXML as
\"latexmlc literal:%i --profile=math --preload=siunitx.sty 2>/dev/null\"."
  :group 'org-latex
  :package-version '(Org . "9.7")
  :type '(choice
	  (const :tag "None" nil)
	  (string :tag "Shell command")))

(defcustom org-preview-latex-default-process 'dvipng
  "The default process to convert LaTeX fragments to image files.
All available processes and theirs documents can be found in
`org-preview-latex-process-alist', which see."
  :group 'org-latex
  :version "26.1"
  :package-version '(Org . "9.0")
  :type 'symbol)

(defcustom org-preview-latex-process-alist
  '((dvipng
     :programs ("latex" "dvipng")
     :description "dvi > png"
     :message "you need to install the programs: latex and dvipng."
     :image-input-type "dvi"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
     :image-converter ("dvipng -D %D -T tight -o %O %f")
     :transparent-image-converter
     ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
    (dvisvgm
     :programs ("latex" "dvisvgm")
     :description "dvi > svg"
     :message "you need to install the programs: latex and dvisvgm."
     :image-input-type "dvi"
     :image-output-type "svg"
     :image-size-adjust (1.7 . 1.5)
     :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
     :image-converter ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))
    (imagemagick
     :programs ("latex" "convert")
     :description "pdf > png"
     :message "you need to install the programs: latex and imagemagick."
     :image-input-type "pdf"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
     :image-converter
     ("convert -density %D -trim -antialias %f -quality 100 %O")))
  "Definitions of external processes for LaTeX previewing.
Org mode can use some external commands to generate TeX snippet's images for
previewing or inserting into HTML files, e.g., \"dvipng\".  This variable tells
`org-create-formula-image' how to call them.

The value is an alist with the pattern (NAME . PROPERTIES).  NAME is a symbol.
PROPERTIES accepts the following attributes:

  :programs           list of strings, required programs.
  :description        string, describe the process.
  :message            string, message it when required programs cannot be found.
  :image-input-type   string, input file type of image converter (e.g., \"dvi\").
  :image-output-type  string, output file type of image converter (e.g., \"png\").
  :image-size-adjust  cons of numbers, the car element is used to adjust LaTeX
                      image size showed in buffer and the cdr element is for
                      HTML file.  This option is only useful for process
                      developers, users should use variable
                      `org-format-latex-options' instead.
  :post-clean         list of strings, files matched are to be cleaned up once
                      the image is generated.  When nil, the files with \".dvi\",
                      \".xdv\", \".pdf\", \".tex\", \".aux\", \".log\", \".svg\",
                      \".png\", \".jpg\", \".jpeg\" or \".out\" extension will
                      be cleaned up.
  :latex-header       list of strings, the LaTeX header of the snippet file.
                      When nil, the fallback value is used instead, which is
                      controlled by `org-format-latex-header',
                      `org-latex-default-packages-alist' and
                      `org-latex-packages-alist', which see.
  :latex-compiler     list of LaTeX commands, as strings.  Each of them is given
                      to the shell.  Place-holders \"%t\", \"%b\" and \"%o\" are
                      replaced with values defined below.
  :image-converter    list of image converter commands strings.  Each of them is
                      given to the shell and supports any of the following
                      place-holders defined below.

If set, :transparent-image-converter is used instead of :image-converter to
convert an image when the background color is nil or \"Transparent\".

Place-holders used by `:image-converter' and `:latex-compiler':

  %f    input file name
  %b    base name of input file
  %o    base directory of input file
  %O    absolute output file name

Place-holders only used by `:image-converter':

  %D    dpi, which is used to adjust image size by some processing commands.
  %S    the image size scale ratio, which is used to adjust image size by some
        processing commands."
  :group 'org-latex
  :package-version '(Org . "9.6")
  :type '(alist :tag "LaTeX to image backends"
		:value-type (plist)))

(defcustom org-preview-latex-image-directory "ltximg/"
  "Path to store latex preview images.
A relative path here creates many directories relative to the
processed Org files paths.  An absolute path puts all preview
images at the same place."
  :group 'org-latex
  :version "26.1"
  :package-version '(Org . "9.0")
  :type 'string)

(defun org-format-latex-mathml-available-p ()
  "Return t if `org-latex-to-mathml-convert-command' is usable."
  (save-match-data
    (when (and (boundp 'org-latex-to-mathml-convert-command)
	       org-latex-to-mathml-convert-command)
      (let ((executable (car (split-string
			      org-latex-to-mathml-convert-command))))
	(when (executable-find executable)
	  (if (string-match
	       "%j" org-latex-to-mathml-convert-command)
	      (file-readable-p org-latex-to-mathml-jar-file)
	    t))))))

(defcustom org-format-latex-header "\\documentclass{article}
\\usepackage[usenames]{color}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}"
  "The document header used for processing LaTeX fragments.
It is imperative that this header make sure that no page number
appears on the page.  The package defined in the variables
`org-latex-default-packages-alist' and `org-latex-packages-alist'
will either replace the placeholder \"[PACKAGES]\" in this
header, or they will be appended."
  :group 'org-latex
  :type 'string)

(defun org-set-packages-alist (var val)
  "Set the packages alist and make sure it has 3 elements per entry."
  (set-default-toplevel-value var (mapcar (lambda (x)
		     (if (and (consp x) (= (length x) 2))
			 (list (car x) (nth 1 x) t)
		       x))
		   val)))

(defun org-get-packages-alist (var)
  "Get the packages alist and make sure it has 3 elements per entry."
  (mapcar (lambda (x)
	    (if (and (consp x) (= (length x) 2))
		(list (car x) (nth 1 x) t)
	      x))
	  (default-value var)))

(defcustom org-latex-default-packages-alist
  '(;; amsmath before fontspec for lualatex and xetex
    (""     "amsmath"   t ("lualatex" "xetex"))
    ;; fontspec ASAP for lualatex and xetex
    (""     "fontspec"  t ("lualatex" "xetex"))
    ;; inputenc and fontenc are for pdflatex only
    ("AUTO" "inputenc"  t ("pdflatex"))
    ("T1"   "fontenc"   t ("pdflatex"))
    (""     "graphicx"  t)
    (""     "longtable" nil)
    (""     "wrapfig"   nil)
    (""     "rotating"  nil)
    ("normalem" "ulem"  t)
    ;; amsmath and amssymb after inputenc/fontenc for pdflatex
    (""     "amsmath"   t ("pdflatex"))
    (""     "amssymb"   t ("pdflatex"))
    (""     "capt-of"   nil)
    (""     "hyperref"  nil))
  "Alist of default packages to be inserted in the header.

Change this only if one of the packages here causes an
incompatibility with another package you are using.

The packages in this list are needed by one part or another of
Org mode to function properly:

- fontspec: for font and character selection in lualatex and xetex
- inputenc, fontenc:  for basic font and character selection
  in pdflatex
- graphicx: for including images
- longtable: For multipage tables
- wrapfig: for figure placement
- rotating: for sideways figures and tables
- ulem: for underline and strike-through
- amsmath: for subscript and superscript and math environments
- amssymb: for various symbols used for interpreting the entities
  in `org-entities'.  You can skip some of this package if you don't
  use any of the symbols.
- capt-of: for captions outside of floats
- hyperref: for cross references

Therefore you should not modify this variable unless you know
what you are doing.  The one reason to change it anyway is that
you might be loading some other package that conflicts with one
of the default packages.  Each element is either a cell or
a string.

A cell is of the format

  (\"options\" \"package\" SNIPPET-FLAG COMPILERS)

If SNIPPET-FLAG is non-nil, the package also needs to be included
when compiling LaTeX snippets into images for inclusion into
non-LaTeX output.

COMPILERS is a list of compilers that should include the package,
see `org-latex-compiler'.  If the document compiler is not in the
list, and the list is non-nil, the package will not be inserted
in the final document.

A string will be inserted as-is in the header of the document."
  :group 'org-latex
  :group 'org-export-latex
  :set 'org-set-packages-alist
  :get 'org-get-packages-alist
  :package-version '(Org . "9.7")
  :type '(repeat
	  (choice
	   (list :tag "options/package pair"
		 (string :tag "options")
		 (string :tag "package")
		 (boolean :tag "Snippet")
		 (choice
		  (const :tag "For all compilers" nil)
		  (repeat :tag "Allowed compiler" string)))
	   (string :tag "A line of LaTeX"))))

(defcustom org-latex-packages-alist nil
  "Alist of packages to be inserted in every LaTeX header.

These will be inserted after `org-latex-default-packages-alist'.
Each element is either a cell or a string.

A cell is of the format:

    (\"options\" \"package\" SNIPPET-FLAG COMPILERS)

SNIPPET-FLAG, when non-nil, indicates that this package is also
needed when turning LaTeX snippets into images for inclusion into
non-LaTeX output.

COMPILERS is a list of compilers that should include the package,
see `org-latex-compiler'.  If the document compiler is not in the
list, and the list is non-nil, the package will not be inserted
in the final document.

A string will be inserted as-is in the header of the document.

Make sure that you only list packages here which:

  - you want in every file;
  - do not conflict with the setup in `org-format-latex-header';
  - do not conflict with the default packages in
    `org-latex-default-packages-alist'."
  :group 'org-latex
  :group 'org-export-latex
  :set 'org-set-packages-alist
  :get 'org-get-packages-alist
  :type
  '(repeat
    (choice
     (list :tag "options/package pair"
           (string :tag "options")
           (string :tag "package")
           (boolean :tag "snippet")
           (choice
            (const :tag "All compilers include this package" nil)
            (repeat :tag "Only include from these compilers" string)))
     (string :tag "A line of LaTeX"))))

;;; Functions and variables from their packages
;;  Declared here to avoid compiler warnings
(defvar mark-active)

;; Various packages
(declare-function calc-eval "calc" (str &optional separator &rest args))
(declare-function calendar-forward-day "cal-move" (arg))
(declare-function calendar-goto-date "cal-move" (date))
(declare-function calendar-goto-today "cal-move" ())
(declare-function calendar-iso-from-absolute "cal-iso" (date))
(declare-function dired-get-filename
		  "dired"
		  (&optional localp no-error-if-not-filep))
(declare-function org-agenda-change-all-lines
		  "org-agenda"
		  (newhead hdmarker &optional fixface just-this))
(declare-function org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item
		  "org-agenda"
		  (&optional end))
(declare-function org-agenda-copy-local-variable "org-agenda" (var))
(declare-function org-agenda-format-item
		  "org-agenda"
		  (extra txt &optional level category tags dotime
			 remove-re habitp))
(declare-function org-agenda-new-marker "org-agenda" (&optional pos))
(declare-function org-agenda-save-markers-for-cut-and-paste
		  "org-agenda"
		  (beg end))
(declare-function org-agenda-set-restriction-lock "org-agenda" (&optional type))
(declare-function org-agenda-skip "org-agenda" (&optional element))
(declare-function org-attach-expand "org-attach" (file))
(declare-function org-attach-reveal "org-attach" ())
(declare-function org-attach-reveal-in-emacs "org-attach" ())
(declare-function org-gnus-follow-link "org-gnus" (&optional group article))
(declare-function org-indent-mode "org-indent" (&optional arg))
(declare-function org-inlinetask-goto-beginning "org-inlinetask" ())
(declare-function org-inlinetask-goto-end "org-inlinetask" ())
(declare-function org-inlinetask-in-task-p "org-inlinetask" ())
(declare-function org-inlinetask-remove-END-maybe "org-inlinetask" ())
(declare-function parse-time-string "parse-time" (string))

(defvar align-mode-rules-list)
(defvar calc-embedded-close-formula)
(defvar calc-embedded-open-formula)
(defvar calc-embedded-open-mode)
(defvar org-agenda-tags-todo-honor-ignore-options)
(defvar remember-data-file)

(declare-function org-clock-save-markers-for-cut-and-paste "org-clock" (beg end))
(declare-function org-clock-update-mode-line "org-clock" (&optional refresh))
(declare-function org-resolve-clocks "org-clock"
		  (&optional also-non-dangling-p prompt last-valid))

(defvar org-clock-start-time)
(defvar org-clock-marker (make-marker)
  "Marker recording the last clock-in.")
(defvar org-clock-hd-marker (make-marker)
  "Marker recording the last clock-in, but the headline position.")
(defvar org-clock-heading ""
  "The heading of the current clock entry.")
(defun org-clocking-buffer ()
  "Return the buffer where the clock is currently running.
Return nil if no clock is running."
  (marker-buffer org-clock-marker))
(defalias 'org-clock-is-active #'org-clocking-buffer)

(defun org-check-running-clock ()
  "Check if the current buffer contains the running clock.
If yes, offer to stop it and to save the buffer with the changes."
  (when (and (equal (marker-buffer org-clock-marker) (current-buffer))
	     (y-or-n-p (format "Clock-out in buffer %s before killing it? "
			       (buffer-name))))
    (org-clock-out)
    (when (y-or-n-p "Save changed buffer?")
      (save-buffer))))

;;;###autoload
(defun org-clock-persistence-insinuate ()
  "Set up hooks for clock persistence."
  (require 'org-clock)
  (add-hook 'org-mode-hook 'org-clock-load)
  (add-hook 'kill-emacs-hook 'org-clock-save))

(defun org-clock-auto-clockout-insinuate ()
  "Set up hook for auto clocking out when Emacs is idle.
See `org-clock-auto-clockout-timer'.

This function is meant to be added to the user configuration."
  (require 'org-clock)
  (add-hook 'org-clock-in-hook #'org-clock-auto-clockout t))

(defgroup org-archive nil
  "Options concerning archiving in Org mode."
  :tag "Org Archive"
  :group 'org-structure)

(defcustom org-agenda-skip-archived-trees t
  "Non-nil means the agenda will skip any items located in archived trees.
An archived tree is a tree marked with the tag ARCHIVE.  The use of this
variable is no longer recommended, you should leave it at the value t.
Instead, use the key `v' to cycle the archives-mode in the agenda."
  :group 'org-archive
  :group 'org-agenda-skip
  :type 'boolean)

(defcustom org-columns-skip-archived-trees t
  "Non-nil means ignore archived trees when creating column view."
  :group 'org-archive
  :group 'org-properties
  :type 'boolean)

(defalias 'org-advertized-archive-subtree 'org-archive-subtree)

;; Declare Column View Code

(declare-function org-columns-get-format-and-top-level "org-colview" ())
(declare-function org-columns-compute "org-colview" (property))

;; Declare ID code

(declare-function org-id-store-link "org-id")
(declare-function org-id-locations-load "org-id")
(declare-function org-id-locations-save "org-id")
(defvar org-id-track-globally)

(defun org-remove-keyword-keys (list)
  "Remove a pair of parenthesis at the end of each string in LIST."
  (mapcar (lambda (x)
	    (if (string-match "(.*)$" x)
		(substring x 0 (match-beginning 0))
	      x))
	  list))

;;; Some variables used in various places

(defvar org-window-configuration nil
  "Used in various places to store a window configuration.")
(defvar org-selected-window nil
  "Used in various places to store a window configuration.")
;; Defined somewhere in this file, but used before definition.
(defvar org-entities)     ;; defined in org-entities.el
(defvar org-struct-menu)
(defvar org-org-menu)
(defvar org-tbl-menu)

;;;; Define the Org mode

(defun org-before-change-function (_beg _end)
  "Every change indicates that a table might need an update."
  (setq org-table-may-need-update t))
(defvar org-mode-map)
(defvar org-agenda-keep-modes nil)      ; Dynamically-scoped param.
(defvar bidi-paragraph-direction)
(defvar buffer-face-mode-face)

(require 'outline)

;; Other stuff we need.
(require 'time-date)
(when (< emacs-major-version 28)  ; preloaded in Emacs 28
  (require 'easymenu))

(require 'org-entities)
(require 'org-faces)
(require 'org-list)
(require 'org-pcomplete)
(require 'org-src)
(require 'org-footnote)
(require 'org-macro)

;; babel
(require 'ob)

(defvar org-element-cache-version); Defined in org-element.el
(defvar org-element-cache-persistent); Defined in org-element.el
(defvar org-element-use-cache); Defined in org-element.el
(defvar org-mode-loading nil
  "Non-nil during Org mode initialization.")

(defvar org-mode-tags-syntax-table
  (let ((st (make-syntax-table org-mode-syntax-table)))
    (modify-syntax-entry ?@ "w" st)
    (modify-syntax-entry ?_ "w" st)
    st)
  "Syntax table including \"@\" and \"_\" as word constituents.")


;; Update `customize-package-emacs-version-alist'
(add-to-list 'customize-package-emacs-version-alist
	     '(Org ("8.0" . "24.4")
		   ("8.1" . "24.4")
		   ("8.2" . "24.4")
		   ("8.2.7" . "24.4")
		   ("8.3" . "26.1")
		   ("9.0" . "26.1")
		   ("9.1" . "26.1")
		   ("9.2" . "27.1")
		   ("9.3" . "27.1")
		   ("9.4" . "27.2")
		   ("9.5" . "28.1")
		   ("9.6" . "29.1")
                   ("9.7" . "30.1")))

(when (fboundp 'abbrev-table-put)
  (abbrev-table-put org-mode-abbrev-table
		    :parents (list text-mode-abbrev-table)))

;;;; Font-Lock stuff, including the activators

(defun org-emphasize (&optional char)
  "Insert or change an emphasis, i.e. a font like bold or italic.
If there is an active region, change that region to a new emphasis.
If there is no region, just insert the marker characters and position
the cursor between them.
CHAR should be the marker character.  If it is a space, it means to
remove the emphasis of the selected region.
If CHAR is not given (for example in an interactive call) it will be
prompted for."
  (interactive)
  (let ((erc org-emphasis-regexp-components)
	(string "") beg end move s)
    (if (org-region-active-p)
	(setq beg (region-beginning)
	      end (region-end)
	      string (buffer-substring beg end))
      (setq move t))

    (unless char
      (message "Emphasis marker or tag: [%s]"
	       (mapconcat #'car org-emphasis-alist ""))
      (setq char (read-char-exclusive)))
    (if (equal char ?\s)
	(setq s ""
	      move nil)
      (unless (assoc (char-to-string char) org-emphasis-alist)
	(user-error "No such emphasis marker: \"%c\"" char))
      (setq s (char-to-string char)))
    (while (and (> (length string) 1)
		(equal (substring string 0 1) (substring string -1))
		(assoc (substring string 0 1) org-emphasis-alist))
      (setq string (substring string 1 -1)))
    (setq string (concat s string s))
    (when beg (delete-region beg end))
    (unless (or (bolp)
		(string-match (concat "[" (nth 0 erc) "\n]")
			      (char-to-string (char-before (point)))))
      (insert " "))
    (unless (or (eobp)
		(string-match (concat "[" (nth 1 erc) "\n]")
			      (char-to-string (char-after (point)))))
      (insert " ") (backward-char 1))
    (insert string)
    (and move (backward-char 1))))

;; FIXME: This function is unused.
(defsubst org-entry-beginning-position ()
  "Return the beginning position of the current entry."
  (save-excursion (org-back-to-heading t) (point)))

(defsubst org-entry-end-position ()
  "Return the end position of the current entry."
  (save-excursion (outline-next-heading) (point)))

(defun org-subtree-end-visible-p ()
  "Is the end of the current subtree visible?"
  (pos-visible-in-window-p
   (save-excursion (org-end-of-subtree t) (point))))

(defun org-first-headline-recenter ()
  "Move cursor to the first headline and recenter the headline."
  (let ((window (get-buffer-window)))
    (when window
      (goto-char (point-min))
      (when (re-search-forward (concat "^\\(" org-outline-regexp "\\)") nil t)
	(set-window-start window (line-beginning-position))))))


;;; Indirect buffer display of subtrees

(defvar org-indirect-dedicated-frame nil
  "This is the frame being used for indirect tree display.")
(defvar org-last-indirect-buffer nil)

(defun org-tree-to-indirect-buffer (&optional arg)
  "Create indirect buffer and narrow it to current subtree.

With a numerical prefix ARG, go up to this level and then take that tree.
If ARG is negative, go up that many levels.

If `org-indirect-buffer-display' is not `new-frame', the command removes the
indirect buffer previously made with this command, to avoid proliferation of
indirect buffers.  However, when you call the command with a \
`\\[universal-argument]' prefix, or
when `org-indirect-buffer-display' is `new-frame', the last buffer is kept
so that you can work with several indirect buffers at the same time.  If
`org-indirect-buffer-display' is `dedicated-frame', the \
`\\[universal-argument]' prefix also
requests that a new frame be made for the new buffer, so that the dedicated
frame is not changed."
  (interactive "P")
  (let ((cbuf (current-buffer))
	(cwin (selected-window))
	(pos (point))
	beg end level heading ibuf
        (last-indirect-window
         (and org-last-indirect-buffer
              (get-buffer-window org-last-indirect-buffer))))
    (save-excursion
      (org-back-to-heading t)
      (when (numberp arg)
	(setq level (org-outline-level))
	(when (< arg 0) (setq arg (+ level arg)))
	(while (> (setq level (org-outline-level)) arg)
	  (org-up-heading-safe)))
      (setq beg (point)
	    heading (org-get-heading 'no-tags))
      (org-end-of-subtree t t)
      (when (and (not (eobp)) (org-at-heading-p)) (backward-char 1))
      (setq end (point)))
    (when (and (buffer-live-p org-last-indirect-buffer)
	       (not (eq org-indirect-buffer-display 'new-frame))
	       (not arg))
      (kill-buffer org-last-indirect-buffer))
    (setq ibuf (org-get-indirect-buffer cbuf heading)
	  org-last-indirect-buffer ibuf)
    (cond
     ((or (eq org-indirect-buffer-display 'new-frame)
	  (and arg (eq org-indirect-buffer-display 'dedicated-frame)))
      (select-frame (make-frame))
      (pop-to-buffer ibuf '(org-display-buffer-full-frame))
      (org-set-frame-title heading))
     ((eq org-indirect-buffer-display 'dedicated-frame)
      (raise-frame
       (select-frame (or (and org-indirect-dedicated-frame
			      (frame-live-p org-indirect-dedicated-frame)
			      org-indirect-dedicated-frame)
			 (setq org-indirect-dedicated-frame (make-frame)))))
      (pop-to-buffer ibuf '(org-display-buffer-full-frame))
      (org-set-frame-title (concat "Indirect: " heading)))
     ((eq org-indirect-buffer-display 'current-window)
      (pop-to-buffer-same-window ibuf))
     ((eq org-indirect-buffer-display 'other-window)
      (pop-to-buffer
       ibuf
       `(org-display-buffer-in-window (window . ,last-indirect-window)
                                      (same-frame . t))))
     (t (error "Invalid value")))
    (narrow-to-region beg end)
    (org-fold-show-all '(headings drawers blocks))
    (goto-char pos)
    (run-hook-with-args 'org-cycle-hook 'all)
    (and (window-live-p cwin) (select-window cwin))))

(cl-defun org-get-indirect-buffer (&optional (buffer (current-buffer)) heading)
  "Return an indirect buffer based on BUFFER.
If HEADING, append it to the name of the new buffer."
  (let* ((base-buffer (or (buffer-base-buffer buffer) buffer))
         (buffer-name (generate-new-buffer-name
                       (format "%s%s"
                               (buffer-name base-buffer)
                               (if heading
                                   (concat "::" heading)
                                 ""))))
         (indirect-buffer (make-indirect-buffer base-buffer buffer-name 'clone)))
    ;; Decouple folding state.  We need to do it manually since
    ;; `make-indirect-buffer' does not run
    ;; `clone-indirect-buffer-hook'.
    (org-fold-core-decouple-indirect-buffer-folds)
    indirect-buffer))

(defun org-set-frame-title (title)
  "Set the title of the current frame to the string TITLE."
  (modify-frame-parameters (selected-frame) (list (cons 'name title))))

(defun org-contextualize-keys (alist contexts)
  "Return valid elements in ALIST depending on CONTEXTS.

`org-agenda-custom-commands' or `org-capture-templates' are the
values used for ALIST, and `org-agenda-custom-commands-contexts'
or `org-capture-templates-contexts' are the associated contexts
definitions."
  (let ((contexts
	 ;; normalize contexts
	 (mapcar
	  (lambda(c) (cond ((listp (cadr c))
		       (list (car c) (car c) (nth 1 c)))
		      ((string= "" (cadr c))
		       (list (car c) (car c) (nth 2 c)))
		      (t c)))
          contexts))
	(a alist) r s)
    ;; loop over all commands or templates
    (dolist (c a)
      (let (vrules repl)
	(cond
	 ((not (assoc (car c) contexts))
	  (push c r))
	 ((and (assoc (car c) contexts)
	       (setq vrules (org-contextualize-validate-key
			     (car c) contexts)))
	  (mapc (lambda (vr)
		  (unless (equal (car vr) (cadr vr))
		    (setq repl vr)))
                vrules)
	  (if (not repl) (push c r)
	    (push (cadr repl) s)
	    (push
	     (cons (car c)
		   (cdr (or (assoc (cadr repl) alist)
			    (error "Undefined key `%s' as contextual replacement for `%s'"
				   (cadr repl) (car c)))))
	     r))))))
    ;; Return limited ALIST, possibly with keys modified, and deduplicated
    (delq
     nil
     (delete-dups
      (mapcar (lambda (x)
		(let ((tpl (car x)))
		  (unless (delq
			   nil
			   (mapcar (lambda (y)
				     (equal y tpl))
				   s))
                    x)))
	      (reverse r))))))

(defun org-contextualize-validate-key (key contexts)
  "Check CONTEXTS for agenda or capture KEY."
  (let (res)
    (dolist (r contexts)
      (dolist (rr (car (last r)))
	(when
	    (and (equal key (car r))
		 (if (functionp rr) (funcall rr)
		   (or (and (eq (car rr) 'in-file)
			    (buffer-file-name)
			    (string-match (cdr rr) (buffer-file-name)))
		       (and (eq (car rr) 'in-mode)
			    (string-match (cdr rr) (symbol-name major-mode)))
		       (and (eq (car rr) 'in-buffer)
			    (string-match (cdr rr) (buffer-name)))
		       (when (and (eq (car rr) 'not-in-file)
				  (buffer-file-name))
			 (not (string-match (cdr rr) (buffer-file-name))))
		       (when (eq (car rr) 'not-in-mode)
			 (not (string-match (cdr rr) (symbol-name major-mode))))
		       (when (eq (car rr) 'not-in-buffer)
			 (not (string-match (cdr rr) (buffer-name)))))))
	  (push r res))))
    (delete-dups (delq nil res))))

;; Defined to provide a value for defcustom, since there is no
;; string-collate-greaterp in Emacs.
(defun org-string-collate-greaterp (s1 s2)
  "Return non-nil if S1 is greater than S2 in collation order."
  (not (string-collate-lessp s1 s2)))

;;;###autoload
(defun org-run-like-in-org-mode (cmd)
  "Run a command, pretending that the current buffer is in Org mode.
This will temporarily bind local variables that are typically bound in
Org mode to the values they have in Org mode, and then interactively
call CMD."
  (org-load-modules-maybe)
  (let (vars vals)
    (dolist (var (org-get-local-variables))
      (when (or (not (boundp (car var)))
		(eq (symbol-value (car var))
		    (default-value (car var))))
	(push (car var) vars)
	(push (cadr var) vals)))
    (cl-progv vars vals
      (call-interactively cmd))))

;;; Refresh properties

(defun org-refresh-properties (dprop tprop)
  "Refresh buffer text properties.
DPROP is the drawer property and TPROP is either the
corresponding text property to set, or an alist with each element
being a text property (as a symbol) and a function to apply to
the value of the drawer property."
  (let* ((case-fold-search t)
	 (inhibit-read-only t)
	 (inherit? (org-property-inherit-p dprop))
	 (property-re (org-re-property (concat (regexp-quote dprop) "\\+?") t))
	 (global-or-keyword (and inherit?
				 (org--property-global-or-keyword-value dprop nil))))
    (with-silent-modifications
      (org-with-point-at 1
	;; Set global and keyword based values to the whole buffer.
	(when global-or-keyword
	  (put-text-property (point-min) (point-max) tprop global-or-keyword))
	;; Set values based on property-drawers throughout the document.
	(while (re-search-forward property-re nil t)
	  (when (org-at-property-p)
	    (org-refresh-property tprop (org-entry-get (point) dprop) inherit?))
	  (outline-next-heading))))))

(defun org-refresh-property (tprop p &optional inherit)
  "Refresh the buffer text property TPROP from the drawer property P.

The refresh happens only for the current entry, or the whole
sub-tree if optional argument INHERIT is non-nil.

If point is before first headline, the function applies to the
part before the first headline.  In that particular case, when
optional argument INHERIT is non-nil, it refreshes properties for
the whole buffer."
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (let ((start (point))
	  (end (save-excursion
		 (cond ((and inherit (org-before-first-heading-p))
			(point-max))
		       (inherit
			(org-end-of-subtree t t))
		       ((outline-next-heading))
		       ((point-max))))))
      (with-silent-modifications
	(if (symbolp tprop)
	    ;; TPROP is a text property symbol.
	    (put-text-property start end tprop p)
	  ;; TPROP is an alist with (property . function) elements.
	  (pcase-dolist (`(,prop . ,f) tprop)
	    (put-text-property start end prop (funcall f p))))))))

(defun org-refresh-category-properties ()
  "Refresh category text properties in the buffer."
  (let ((case-fold-search t)
	(inhibit-read-only t)
	(default-category
	 (cond ((null org-category)
		(if buffer-file-name
		    (file-name-sans-extension
		     (file-name-nondirectory buffer-file-name))
		  "???"))
	       ((symbolp org-category) (symbol-name org-category))
	       (t org-category))))
    (let ((category (catch 'buffer-category
                      (org-with-wide-buffer
	               (goto-char (point-max))
	               (while (re-search-backward "^[ \t]*#\\+CATEGORY:" (point-min) t)
	                 (let ((element (org-element-at-point-no-context)))
	                   (when (org-element-type-p element 'keyword)
		             (throw 'buffer-category
		                    (org-element-property :value element))))))
	              default-category)))
      (with-silent-modifications
        (org-with-wide-buffer
         ;; Set buffer-wide property from keyword.  Search last #+CATEGORY
         ;; keyword.  If none is found, fall-back to `org-category' or
         ;; buffer file name, or set it by the document property drawer.
         (put-text-property (point-min) (point-max)
                            'org-category category)
         ;; Set categories from the document property drawer or
         ;; property drawers in the outline.  If category is found in
         ;; the property drawer for the whole buffer that value
         ;; overrides the keyword-based value set above.
         (goto-char (point-min))
         (let ((regexp (org-re-property "CATEGORY")))
           (while (re-search-forward regexp nil t)
             (let ((value (match-string-no-properties 3)))
               (when (org-at-property-p)
                 (put-text-property
                  (save-excursion (org-back-to-heading-or-point-min t))
                  (save-excursion (if (org-before-first-heading-p)
                                      (point-max)
                                    (org-end-of-subtree t t)))
                  'org-category
                  value))))))))))

(defun org-refresh-stats-properties ()
  "Refresh stats text properties in the buffer."
  (with-silent-modifications
    (org-with-point-at 1
      (let ((regexp (concat org-outline-regexp-bol
			    ".*\\[\\([0-9]*\\)\\(?:%\\|/\\([0-9]*\\)\\)\\]")))
	(while (re-search-forward regexp nil t)
	  (let* ((numerator (string-to-number (match-string 1)))
		 (denominator (and (match-end 2)
				   (string-to-number (match-string 2))))
		 (stats (cond ((not denominator) numerator) ;percent
			      ((= denominator 0) 0)
			      (t (/ (* numerator 100) denominator)))))
	    (put-text-property (point) (progn (org-end-of-subtree t t) (point))
			       'org-stats stats)))))))

(defun org-refresh-effort-properties ()
  "Refresh effort properties."
  (org-refresh-properties
   org-effort-property
   '((effort . identity)
     (effort-minutes . org-duration-to-minutes))))

(defun org-find-file-at-mouse (ev)
  "Open file link or URL at mouse."
  (interactive "e")
  (mouse-set-point ev)
  (org-open-at-point 'in-emacs))

(defun org-open-at-mouse (ev)
  "Open file link or URL at mouse.
See the docstring of `org-open-file' for details."
  (interactive "e")
  (mouse-set-point ev)
  (when (eq major-mode 'org-agenda-mode)
    (org-agenda-copy-local-variable 'org-link-abbrev-alist-local))
  (org-open-at-point))

(defvar org-window-config-before-follow-link nil
  "The window configuration before following a link.
This is saved in case the need arises to restore it.")

;;;###autoload
(defun org-open-at-point-global ()
  "Follow a link or a timestamp like Org mode does.
Also follow links and emails as seen by `thing-at-point'.
This command can be called in any mode to follow an external
link or a timestamp that has Org mode syntax.  Its behavior
is undefined when called on internal links like fuzzy links.
Raise a user error when there is nothing to follow."
  (interactive)
  (let ((tap-url (thing-at-point 'url))
	(tap-email (thing-at-point 'email)))
    (cond ((org-in-regexp
            org-link-any-re
            (let ((origin (point)))
              (max
               (save-excursion
                 (backward-paragraph)
                 (count-lines (point) origin))
               (save-excursion
                 (forward-paragraph)
                 (count-lines origin (point))))))
	   (org-link-open-from-string (match-string-no-properties 0)))
	  ((or (org-in-regexp org-ts-regexp-both nil t)
	       (org-in-regexp org-tsr-regexp-both nil t))
	   (org-follow-timestamp-link))
	  (tap-url (org-link-open-from-string tap-url))
	  (tap-email (org-link-open-from-string
		      (concat "mailto:" tap-email)))
	  (t (user-error "No link found")))))

(defvar org-open-at-point-functions nil
  "Hook that is run when following a link at point.

Functions in this hook must return t if they identify and follow
a link at point.  If they don't find anything interesting at point,
they must return nil.")

(defun org-open-at-point (&optional arg)
  "Open thing at point.
The thing can be a link, citation, timestamp, footnote, src-block or
tags.

When point is on a link, follow it.  Normally, files will be opened by
an appropriate application (see `org-file-apps').  If the optional prefix
argument ARG is non-nil, Emacs will visit the file.  With a double
prefix argument, try to open outside of Emacs, in the application the
system uses for this file type.

When point is on a timestamp, open the agenda at the day
specified.

When point is a footnote definition, move to the first reference
found.  If it is on a reference, move to the associated
definition.

When point is on a src-block of inline src-block, open its result.

When point is on a citation, follow it.

When point is on a headline, display a list of every link in the
entry, so it is possible to pick one, or all, of them.  If point
is on a tag, call `org-tags-view' instead.

On top of syntactically correct links, this function also tries
to open links and timestamps in comments, node properties, and
keywords if point is on something looking like a timestamp or
a link."
  (interactive "P")
  (org-load-modules-maybe)
  (setq org-window-config-before-follow-link (current-window-configuration))
  (org-remove-occur-highlights nil nil t)
  (unless (run-hook-with-args-until-success 'org-open-at-point-functions)
    (let* ((context
	    ;; Only consider supported types, even if they are not the
	    ;; closest one.
	    (org-element-lineage
	     (org-element-context)
	     '(citation citation-reference clock comment comment-block
                        footnote-definition footnote-reference headline
                        inline-src-block inlinetask keyword link node-property
                        planning src-block timestamp)
	     t))
	   (type (org-element-type context))
	   (value (org-element-property :value context)))
      (cond
       ((not type) (user-error "No link found"))
       ;; No valid link at point.  For convenience, look if something
       ;; looks like a link under point in some specific places.
       ((memq type '(comment comment-block node-property keyword))
	(call-interactively #'org-open-at-point-global))
       ;; On a headline or an inlinetask, but not on a timestamp,
       ;; a link, a footnote reference or a citation.
       ((memq type '(headline inlinetask))
	(org-match-line org-complex-heading-regexp)
	(let ((tags-beg (match-beginning 5))
	      (tags-end (match-end 5)))
	  (if (and tags-beg (>= (point) tags-beg) (< (point) tags-end))
	      ;; On tags.
	      (org-tags-view
	       arg
	       (save-excursion
		 (let* ((beg-tag (or (search-backward ":" tags-beg 'at-limit) (point)))
			(end-tag (search-forward ":" tags-end nil 2)))
		   (buffer-substring (1+ beg-tag) (1- end-tag)))))
	    ;; Not on tags.
	    (pcase (org-offer-links-in-entry (current-buffer) (point) arg)
	      (`(nil . ,_)
	       (require 'org-attach)
	       (when (org-attach-dir)
		 (message "Opening attachment")
		 (if (equal arg '(4))
		     (org-attach-reveal-in-emacs)
		   (org-attach-reveal))))
	      (`(,links . ,links-end)
               (let ((link-marker (make-marker))
                     (last-moved-marker (point-marker)))
	         (dolist (link (if (stringp links) (list links) links))
		   (search-forward link nil links-end)
		   (goto-char (match-beginning 0))
                   (move-marker link-marker (point))
                   (save-excursion
		     (org-open-at-point arg)
                     (unless (equal (point-marker) link-marker)
                       (move-marker last-moved-marker (point-marker)))))
                 ;; If any of the links moved point in current buffer,
                 ;; move to the point corresponding to such latest link.
                 ;; Otherwise, restore the original point position.
                 (goto-char last-moved-marker)))))))
       ;; On a footnote reference or at definition's label.
       ((or (eq type 'footnote-reference)
	    (and (eq type 'footnote-definition)
		 (save-excursion
		   ;; Do not validate action when point is on the
		   ;; spaces right after the footnote label, in order
		   ;; to be on par with behavior on links.
		   (skip-chars-forward " \t")
		   (let ((begin
			  (org-element-contents-begin context)))
		     (if begin (< (point) begin)
		       (= (org-element-post-affiliated context)
			  (line-beginning-position)))))))
	(org-footnote-action))
       ;; On a planning line.  Check if we are really on a timestamp.
       ((and (eq type 'planning)
	     (org-in-regexp org-ts-regexp-both nil t))
	(org-follow-timestamp-link))
       ;; On a clock line, make sure point is on the timestamp
       ;; before opening it.
       ((and (eq type 'clock)
	     value
	     (>= (point) (org-element-begin value))
	     (<= (point) (org-element-end value)))
	(org-follow-timestamp-link))
       ((eq type 'src-block) (org-babel-open-src-block-result))
       ;; Do nothing on white spaces after an object.
       ((>= (point)
	    (save-excursion
	      (goto-char (org-element-end context))
	      (skip-chars-backward " \t")
	      (point)))
	(user-error "No link found"))
       ((eq type 'inline-src-block) (org-babel-open-src-block-result))
       ((eq type 'timestamp) (org-follow-timestamp-link))
       ((eq type 'link) (org-link-open context arg))
       ((memq type '(citation citation-reference)) (org-cite-follow context arg))
       (t (user-error "No link found")))))
  (run-hook-with-args 'org-follow-link-hook))

;;;###autoload
(defun org-offer-links-in-entry (buffer marker &optional nth zero)
  "Offer links in the current entry and return the selected link.
If there is only one link, return it.
If NTH is an integer, return the NTH link found.
If ZERO is a string, check also this string for a link, and if
there is one, return it."
  (with-current-buffer buffer
    (org-with-wide-buffer
     (goto-char marker)
     (let ((cnt ?0)
	   have-zero end links link c)
       (when (and (stringp zero) (string-match org-link-bracket-re zero))
	 (push (match-string 0 zero) links)
	 (setq cnt (1- cnt) have-zero t))
       (save-excursion
	 (org-back-to-heading t)
	 (setq end (save-excursion (outline-next-heading) (point)))
	 (while (re-search-forward org-link-any-re end t)
           ;; Only consider valid links or links openable via
           ;; `org-open-at-point'.
           (when (org-element-type-p
                  (save-match-data (org-element-context))
                  '(link comment comment-block node-property keyword))
	     (push (match-string 0) links)))
	 (setq links (org-uniquify (reverse links))))
       (cond
	((null links)
	 (message "No links"))
	((equal (length links) 1)
	 (setq link (car links)))
	((and (integerp nth) (>= (length links) (if have-zero (1+ nth) nth)))
	 (setq link (nth (if have-zero nth (1- nth)) links)))
	(t				; we have to select a link
	 (save-excursion
	   (save-window-excursion
             ;; We have no direct control over how
             ;; `with-output-to-temp-buffer' displays the buffer.  Try
             ;; to gain more space, making sure that only the Org
             ;; buffer and the *Select link* buffer are displayed for
             ;; the duration of selection.
	     (ignore-errors (delete-other-windows))
	     (with-output-to-temp-buffer "*Select Link*"
	       (dolist (l links)
		 (cond
		  ((not (string-match org-link-bracket-re l))
		   (princ (format "[%c]  %s\n" (cl-incf cnt)
				  (org-unbracket-string "<" ">" l))))
		  ((match-end 2)
		   (princ (format "[%c]  %s (%s)\n" (cl-incf cnt)
				  (match-string 2 l) (match-string 1 l))))
		  (t (princ (format "[%c]  %s\n" (cl-incf cnt)
				    (match-string 1 l)))))))
	     (org-fit-window-to-buffer (get-buffer-window "*Select Link*"))
	     (message "Select link to open, RET to open all:")
             (unwind-protect (setq c (read-char-exclusive))
               (and (get-buffer-window "*Select Link*" t)
                    (quit-window 'kill (get-buffer-window "*Select Link*" t)))
	       (and (get-buffer "*Select Link*") (kill-buffer "*Select Link*")))))
	 (when (equal c ?q) (user-error "Abort"))
	 (if (equal c ?\C-m)
	     (setq link links)
	   (setq nth (- c ?0))
	   (when have-zero (setq nth (1+ nth)))
	   (unless (and (integerp nth) (>= (length links) nth))
	     (user-error "Invalid link selection"))
	   (setq link (nth (1- nth) links)))))
       (cons link end)))))

;;; File search

(defun org-do-occur (regexp &optional cleanup)
  "Call the Emacs command `occur'.
If CLEANUP is non-nil, remove the printout of the regular expression
in the *Occur* buffer.  This is useful if the regex is long and not useful
to read."
  (occur regexp)
  (when cleanup
    (let ((cwin (selected-window)) win beg end)
      (when (setq win (get-buffer-window "*Occur*"))
	(select-window win))
      (goto-char (point-min))
      (when (re-search-forward "match[a-z]+" nil t)
	(setq beg (match-end 0))
	(when (re-search-forward "^[ \t]*[0-9]+" nil t)
	  (setq end (1- (match-beginning 0)))))
      (and beg end (let ((inhibit-read-only t)) (delete-region beg end)))
      (goto-char (point-min))
      (select-window cwin))))


;;; Following specific links

(defvar org-agenda-buffer-tmp-name)
(defvar org-agenda-start-on-weekday)
(defvar org-agenda-buffer-name)
(defun org-follow-timestamp-link ()
  "Open an agenda view for the timestamp date/range at point."
  (require 'org-agenda)
  ;; Avoid changing the global value.
  (let ((org-agenda-buffer-name org-agenda-buffer-name))
    (cond
     ((org-at-date-range-p t)
      (let ((org-agenda-start-on-weekday)
	    (t1 (match-string 1))
	    (t2 (match-string 2)) tt1 tt2)
	(setq tt1 (time-to-days (org-time-string-to-time t1))
	      tt2 (time-to-days (org-time-string-to-time t2)))
	(let ((org-agenda-buffer-tmp-name
	       (format "*Org Agenda(a:%s)"
		       (concat (substring t1 0 10) "--" (substring t2 0 10)))))
	  (org-agenda-list nil tt1 (1+ (- tt2 tt1))))))
     ((org-at-timestamp-p 'lax)
      (let ((org-agenda-buffer-tmp-name
	     (format "*Org Agenda(a:%s)" (substring (match-string 1) 0 10))))
	(org-agenda-list nil (time-to-days (org-time-string-to-time
					  (substring (match-string 1) 0 10)))
			 1)))
     (t (error "This should not happen")))))


;;;; Refiling

(declare-function org-string-nw-p "org-macs" (s))
;;;; Completion

(declare-function org-export-backend-options "ox" (cl-x) t)
(defun org-get-export-keywords ()
  "Return a list of all currently understood export keywords.
Export keywords include options, block names, attributes and
keywords relative to each registered export backend."
  (let (keywords)
    (dolist (backend
	     (bound-and-true-p org-export-registered-backends)
	     (delq nil keywords))
      ;; Backend name (for keywords, like #+LATEX:)
      (push (upcase (symbol-name (org-export-backend-name backend))) keywords)
      (dolist (option-entry (org-export-backend-options backend))
	;; Backend options.
	(push (nth 1 option-entry) keywords)))))

(defconst org-options-keywords
  '("ARCHIVE:" "AUTHOR:" "BIBLIOGRAPHY:" "BIND:" "CATEGORY:" "CITE_EXPORT:"
    "COLUMNS:" "CREATOR:" "DATE:" "DESCRIPTION:" "DRAWERS:" "EMAIL:"
    "EXCLUDE_TAGS:" "FILETAGS:" "INCLUDE:" "INDEX:" "KEYWORDS:" "LANGUAGE:"
    "MACRO:" "OPTIONS:" "PROPERTY:" "PRINT_BIBLIOGRAPHY:" "PRIORITIES:"
    "SELECT_TAGS:" "SEQ_TODO:" "SETUPFILE:" "STARTUP:" "TAGS:" "TITLE:" "TODO:"
    "TYP_TODO:" "SELECT_TAGS:" "EXCLUDE_TAGS:" "EXPORT_FILE_NAME:"))

(defcustom org-structure-template-alist
  '(("a" . "export ascii")
    ("c" . "center")
    ("C" . "comment")
    ("e" . "example")
    ("E" . "export")
    ("h" . "export html")
    ("l" . "export latex")
    ("q" . "quote")
    ("s" . "src")
    ("v" . "verse"))
  "An alist of keys and block types.
`org-insert-structure-template' will display a menu with this list of
templates to choose from.  The block type is inserted, with
\"#+begin_\" and \"#+end_\" added automatically.  If the block type
consists of just uppercase letters, \"#+BEGIN_\" and \"#+END_\" are
added instead.

The menu keys are defined by the car of each entry in this alist.
If two entries have the keys \"a\" and \"aa\" respectively, the
former will be inserted by typing \"a TAB/RET/SPC\" and the
latter will be inserted by typing \"aa\".  If an entry with the
key \"aab\" is later added, it can be inserted by typing \"ab\".

If loaded, Org Tempo also uses `org-structure-template-alist'.  A
block can be inserted by pressing TAB after the string \"<KEY\"."
  :group 'org-edit-structure
  :type '(repeat
	  (cons (string :tag "Key")
		(string :tag "Template")))
  :package-version '(Org . "9.6"))

(defun org--insert-structure-template-mks ()
  "Present `org-structure-template-alist' with `org-mks'.

Menus are added if keys require more than one keystroke.  Tabs
are added to single key entries when more than one stroke is
needed.  Keys longer than two characters are reduced to two
characters."
  (org--check-org-structure-template-alist 'org-structure-template-alist)
  (let* (case-fold-search
	 (templates (append org-structure-template-alist
			    '(("\t" . "Press TAB, RET or SPC to write block name"))))
         (keys (mapcar #'car templates))
         (start-letters
	  (delete-dups (mapcar (lambda (key) (substring key 0 1)) keys)))
	 ;; Sort each element of `org-structure-template-alist' into
	 ;; sublists according to the first letter.
         (superlist
	  (mapcar (lambda (letter)
                    (list letter
			  (cl-remove-if-not
			   (apply-partially #'string-match-p (concat "^" letter))
			   templates :key #'car)))
		  start-letters)))
    (org-mks
     (apply #'append
	    ;; Make an `org-mks' table.  If only one element is
	    ;; present in a sublist, make it part of the top-menu,
	    ;; otherwise make a submenu according to the starting
	    ;; letter and populate it.
	    (mapcar (lambda (sublist)
		      (if (eq 1 (length (cadr sublist)))
                          (mapcar (lambda (elm)
				    (list (substring (car elm) 0 1)
                                          (cdr elm) ""))
                                  (cadr sublist))
			;; Create submenu.
                        (let* ((topkey (car sublist))
			       (elms (cadr sublist))
			       (keys (mapcar #'car elms))
			       (long (> (length elms) 3)))
                          (append
			   (list
			    ;; Make a description of the submenu.
			    (list topkey
				  (concat
				   (mapconcat #'cdr
					      (cl-subseq elms 0 (if long 3 (length elms)))
					      ", ")
                                   (when long ", ..."))))
			   ;; List of entries in submenu.
			   (cl-mapcar #'list
				      (org--insert-structure-template-unique-keys keys)
				      (mapcar #'cdr elms)
				      (make-list (length elms) ""))))))
		    superlist))
     "Select a key\n============"
     "Key: ")))

(defun org--insert-structure-template-unique-keys (keys)
  "Make a list of unique, two characters long elements from KEYS.

Elements of length one have a tab appended.  Elements of length
two are kept as is.  Longer elements are truncated to length two.

If an element cannot be made unique, an error is raised."
  (let ((ordered-keys (cl-sort (copy-sequence keys) #'< :key #'length))
	menu-keys)
    (dolist (key ordered-keys)
      (let ((potential-key
	     (cl-case (length key)
	       (1 (concat key "\t"))
	       (2 key)
	       (otherwise
		(cl-find-if-not (lambda (k) (assoc k menu-keys))
				(mapcar (apply-partially #'concat (substring  key 0 1))
					(split-string (substring key 1) "" t)))))))
	(if (or (not potential-key) (assoc potential-key menu-keys))
            (user-error "Could not make unique key for `%s'" key)
	  (push (cons potential-key key) menu-keys))))
    (mapcar #'car
	    (cl-sort menu-keys #'<
		     :key (lambda (elm) (cl-position (cdr elm) keys))))))

(defun org-insert-structure-template (type)
  "Insert a block structure of the type #+begin_foo/#+end_foo.
Select a block from `org-structure-template-alist' then type
either RET, TAB or SPC to write the block type.  With an active
region, wrap the region in the block.  Otherwise, insert an empty
block.

When foo is written as FOO, upcase the #+BEGIN/END as well."
  (interactive
   (list (pcase (org--insert-structure-template-mks)
	   (`("\t" . ,_)
            (let ((type (read-string "Structure type: ")))
              (when (string-empty-p type) (user-error "Empty structure type"))
              type))
	   (`(,_ ,choice . ,_) choice))))
  (when (or (not (stringp type)) (string-empty-p type))
    (error "Invalid structure type: %S" type))
  (let* ((case-fold-search t) ; Make sure that matches are case-insensitive.
         (region? (use-region-p))
	 (region-start (and region? (region-beginning)))
	 (region-end (and region? (copy-marker (region-end))))
	 (extended? (string-match-p "\\`\\(src\\|export\\)\\'" type))
	 (verbatim? (string-match-p
		     (concat "\\`" (regexp-opt '("example" "export"
                                                 "src" "comment")))
		     type))
         (upcase? (string= (car (split-string type))
                           (upcase (car (split-string type))))))
    (when region? (goto-char region-start))
    (let ((column (current-indentation)))
      (if (save-excursion (skip-chars-backward " \t") (bolp))
	  (forward-line 0)
	(insert "\n"))
      (save-excursion
	(indent-to column)
	(insert (format "#+%s_%s%s\n" (if upcase? "BEGIN" "begin") type (if extended? " " "")))
	(when region?
	  (when verbatim? (org-escape-code-in-region (point) region-end))
	  (goto-char region-end)
	  ;; Ignore empty lines at the end of the region.
	  (skip-chars-backward " \r\t\n")
	  (end-of-line))
	(unless (bolp) (insert "\n"))
	(indent-to column)
	(insert (format "#+%s_%s" (if upcase? "END" "end") (car (split-string type))))
	(if (looking-at "[ \t]*$") (replace-match "")
	  (insert "\n"))
	(when (and (eobp) (not (bolp))) (insert "\n")))
      (if extended? (end-of-line)
	(forward-line)
	(skip-chars-forward " \t")))))


;;;; TODO, DEADLINE, Comments

(defun org-at-date-range-p (&optional inactive-ok)
  "Non-nil if point is inside a date range.

When optional argument INACTIVE-OK is non-nil, also consider
inactive time ranges.

When this function returns a non-nil value, match data is set
according to `org-tr-regexp-both' or `org-tr-regexp', depending
on INACTIVE-OK."
  (save-excursion
    (catch 'exit
      (let ((pos (point)))
	(skip-chars-backward "^[<\r\n")
	(skip-chars-backward "<[")
	(and (looking-at (if inactive-ok org-tr-regexp-both org-tr-regexp))
	     (>= (match-end 0) pos)
	     (throw 'exit t))
	(skip-chars-backward "^<[\r\n")
	(skip-chars-backward "<[")
	(and (looking-at (if inactive-ok org-tr-regexp-both org-tr-regexp))
	     (>= (match-end 0) pos)
	     (throw 'exit t)))
      nil)))

(defvar org-last-changed-timestamp)
(defvar org-last-inserted-timestamp)
(defvar org-log-post-message)
(defvar org-log-note-purpose)
(defvar org-log-note-how nil)
(defvar org-log-note-extra)
(defvar org-log-setup nil)
(defun org-remove-empty-drawer-at (pos)
  "Remove an empty drawer at position POS.
POS may also be a marker."
  (with-current-buffer (if (markerp pos) (marker-buffer pos) (current-buffer))
    (org-with-wide-buffer
     (goto-char pos)
     (let ((drawer (org-element-at-point)))
       (when (and (org-element-type-p drawer '(drawer property-drawer))
		  (not (org-element-contents-begin drawer)))
	 (delete-region (org-element-begin drawer)
			(progn (goto-char (org-element-end drawer))
			       (skip-chars-backward " \r\t\n")
			       (forward-line)
			       (point))))))))

;;;; Tags
(defalias 'org-tags-sparse-tree 'org-match-sparse-tree)

(defun org-insert-drawer (&optional arg drawer)
  "Insert a drawer at point.

When optional argument ARG is non-nil, insert a property drawer.

Optional argument DRAWER, when non-nil, is a string representing
drawer's name.  Otherwise, the user is prompted for a name.

If a region is active, insert the drawer around that region
instead.

Point is left between drawer's boundaries."
  (interactive "P")
  (let* ((drawer (if arg "PROPERTIES"
		   (or drawer (read-from-minibuffer "Drawer: ")))))
    (cond
     ;; With C-u, fall back on `org-insert-property-drawer'
     (arg
      (org-insert-property-drawer)
      (org-back-to-heading-or-point-min t)
      ;; Move inside.
      (re-search-forward org-property-end-re)
      (forward-line 0)
      (unless (org-element-contents-begin (org-element-at-point))
        ;; Empty drawer.
        (insert "\n")
        (forward-char -1))
      (org-reveal))
     ;; Check validity of suggested drawer's name.
     ((not (string-match-p org-drawer-regexp (format ":%s:" drawer)))
      (user-error "Invalid drawer name"))
     ;; With an active region, insert a drawer at point.
     ((not (org-region-active-p))
      (progn
	(unless (bolp) (insert "\n"))
	(insert (format ":%s:\n\n:END:\n" drawer))
	(forward-line -2)))
     ;; Otherwise, insert the drawer at point
     (t
      (let ((rbeg (region-beginning))
	    (rend (copy-marker (region-end))))
	(unwind-protect
	    (progn
	      (goto-char rbeg)
	      (forward-line 0)
	      (when (save-excursion
		      (re-search-forward org-outline-regexp-bol rend t))
		(user-error "Drawers cannot contain headlines"))
	      ;; Position point at the beginning of the first
	      ;; non-blank line in region.  Insert drawer's opening
	      ;; there, then indent it.
	      (org-skip-whitespace)
	      (forward-line 0)
	      (insert ":" drawer ":\n")
	      (forward-line -1)
	      (indent-for-tab-command)
	      ;; Move point to the beginning of the first blank line
	      ;; after the last non-blank line in region.  Insert
	      ;; drawer's closing, then indent it.
	      (goto-char rend)
	      (skip-chars-backward " \r\t\n")
	      (insert "\n:END:")
	      (deactivate-mark t)
	      (indent-for-tab-command)
	      (unless (eolp) (insert "\n"))
              ;; Leave point inside drawer boundaries.
              (search-backward ":END:")
              (forward-char -1))
	  ;; Clear marker, whatever the outcome of insertion is.
	  (set-marker rend nil)))))))

(defun org-find-olp (path &optional this-buffer)
  "Return a marker pointing to the entry at outline path OLP.
If anything goes wrong, throw an error, and if you need to do
something based on this error, you can catch it with
`condition-case'.

If THIS-BUFFER is set, the outline path does not contain a file,
only headings."
  (let* ((file (if this-buffer buffer-file-name (pop path)))
	 (buffer (if this-buffer (current-buffer) (find-file-noselect file)))
	 (level 1)
	 (lmin 1)
	 (lmax 1)
	 end found flevel)
    (unless buffer (error "File not found :%s" file))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
	(error "Buffer %s needs to be in Org mode" buffer))
      (org-with-wide-buffer
       (goto-char (point-min))
       (dolist (heading path)
	 (let ((re (format org-complex-heading-regexp-format
			   (regexp-quote heading)))
	       (cnt 0))
	   (while (re-search-forward re end t)
	     (setq level (- (match-end 1) (match-beginning 1)))
	     (when (and (>= level lmin) (<= level lmax))
	       (setq found (match-beginning 0) flevel level cnt (1+ cnt))))
	   (when (= cnt 0)
	     (error "Heading not found on level %d: %s" lmax heading))
	   (when (> cnt 1)
	     (error "Heading not unique on level %d: %s" lmax heading))
	   (goto-char found)
	   (setq lmin (1+ flevel) lmax (+ lmin (if org-odd-levels-only 1 0)))
	   (setq end (save-excursion (org-end-of-subtree t t)))))
       (when (org-at-heading-p)
	 (point-marker))))))

(defun org-find-exact-headline-in-buffer (heading &optional buffer pos-only)
  "Find node HEADING in BUFFER.
Return a marker to the heading if it was found, or nil if not.
If POS-ONLY is set, return just the position instead of a marker.

The heading text must match exact, but it may have a TODO keyword,
a priority cookie and tags in the standard locations."
  (with-current-buffer (or buffer (current-buffer))
    (org-with-wide-buffer
     (goto-char (point-min))
     (let (case-fold-search)
       (when (re-search-forward
	      (format org-complex-heading-regexp-format
		      (regexp-quote heading)) nil t)
	 (if pos-only
	     (match-beginning 0)
	   (move-marker (make-marker) (match-beginning 0))))))))

(defun org-find-exact-heading-in-directory (heading &optional dir)
  "Find Org node headline HEADING in all \".org\" files in directory DIR.
When the target headline is found, return a marker to this location."
  (let ((files (directory-files (or dir default-directory)
				t "\\`[^.#].*\\.org\\'"))
	visiting m buffer)
    (catch 'found
      (dolist (file files)
        (message "trying %s" file)
        (setq visiting (org-find-base-buffer-visiting file))
        (setq buffer (or visiting (find-file-noselect file)))
        (setq m (org-find-exact-headline-in-buffer
                 heading buffer))
        (when (and (not m) (not visiting)) (kill-buffer buffer))
        (and m (throw 'found m))))))

(defun org-find-entry-with-id (ident)
  "Locate the entry that contains the ID property with exact value IDENT.
IDENT can be a string, a symbol or a number, this function will search for
the string representation of it.
Return the position where this entry starts, or nil if there is no such entry."
  (interactive "sID: ")
  (let ((id (cond
	     ((stringp ident) ident)
	     ((symbolp ident) (symbol-name ident))
	     ((numberp ident) (number-to-string ident))
	     (t (error "IDENT %s must be a string, symbol or number" ident)))))
    (org-with-wide-buffer (org-find-property "ID" id))))

;;;; Timestamps

(defun org-deadline-close-p (timestamp-string &optional ndays)
  "Is the time in TIMESTAMP-STRING close to the current date?"
  (setq ndays (or ndays (org-get-wdays timestamp-string)))
  (and (<= (org-timestamp-to-now timestamp-string) ndays)
       (not (org-entry-is-done-p))))

(defun org-get-wdays (ts &optional delay zero-delay)
  "Get the deadline lead time appropriate for timestring TS.
When DELAY is non-nil, get the delay time for scheduled items
instead of the deadline lead time.  When ZERO-DELAY is non-nil
and `org-scheduled-delay-days' is 0, enforce 0 as the delay,
don't try to find the delay cookie in the scheduled timestamp."
  (let ((tv (if delay org-scheduled-delay-days
	      org-deadline-warning-days)))
    (cond
     ((or (and delay (< tv 0))
	  (and delay zero-delay (<= tv 0))
	  (and (not delay) (<= tv 0)))
      ;; Enforce this value no matter what
      (- tv))
     ((string-match "-\\([0-9]+\\)\\([hdwmy]\\)\\(\\'\\|>\\| \\)" ts)
      ;; lead time is specified.
      (floor (* (string-to-number (match-string 1 ts))
		(cdr (assoc (match-string 2 ts)
			    '(("d" . 1)    ("w" . 7)
			      ("m" . 30.4) ("y" . 365.25)
			      ("h" . 0.041667)))))))
     ;; go for the default.
     (t tv))))

(defun org-evaluate-time-range (&optional to-buffer)
  "Evaluate a time range by computing the difference between start and end.
Normally the result is just printed in the echo area, but with prefix arg
TO-BUFFER, the result is inserted just after the date stamp into the buffer.
If the time range is actually in a table, the result is inserted into the
next column.
For time difference computation, a year is assumed to be exactly 365
days in order to avoid rounding problems."
  (interactive "P")
  (or
   (org-clock-update-time-maybe)
   (save-excursion
     (unless (org-at-date-range-p t)
       (goto-char (line-beginning-position))
       (re-search-forward org-tr-regexp-both (line-end-position) t))
     (unless (org-at-date-range-p t)
       (user-error "Not at a timestamp range, and none found in current line")))
   (let* ((ts1 (match-string 1))
	  (ts2 (match-string 2))
	  (havetime (or (> (length ts1) 15) (> (length ts2) 15)))
	  (match-end (match-end 0))
	  (time1 (org-time-string-to-time ts1))
	  (time2 (org-time-string-to-time ts2))
	  (diff (abs (float-time (time-subtract time2 time1))))
	  (negative (time-less-p time2 time1))
	  ;; (ys (floor (* 365 24 60 60)))
	  (ds (* 24 60 60))
	  (hs (* 60 60))
	  (fy "%dy %dd %02d:%02d")
	  (fy1 "%dy %dd")
	  (fd "%dd %02d:%02d")
	  (fd1 "%dd")
	  (fh "%02d:%02d")
	  y d h m align)
     (if havetime
	 (setq ; y (floor diff ys)  diff (mod diff ys)
	  y 0
	  d (floor diff ds)  diff (mod diff ds)
	  h (floor diff hs)  diff (mod diff hs)
	  m (floor diff 60))
       (setq ; y (floor diff ys)  diff (mod diff ys)
	y 0
	d (round diff ds)
	h 0 m 0))
     (if (not to-buffer)
	 (message "%s" (org-make-tdiff-string y d h m))
       (if (org-at-table-p)
	   (progn
	     (goto-char match-end)
	     (setq align t)
	     (and (looking-at " *|") (goto-char (match-end 0))))
	 (goto-char match-end))
       (when (looking-at
	      "\\( *-? *[0-9]+y\\)?\\( *[0-9]+d\\)? *[0-9][0-9]:[0-9][0-9]")
	 (replace-match ""))
       (when negative (insert " -"))
       (if (> y 0) (insert " " (format (if havetime fy fy1) y d h m))
	 (if (> d 0) (insert " " (format (if havetime fd fd1) d h m))
	   (insert " " (format fh h m))))
       (when align (org-table-align))
       (message "Time difference inserted")))))

(defvar org-agenda-current-date)
(defun org-calendar-holiday ()
  "List of holidays, for Diary display in Org mode."
  (require 'holidays)
  (let ((hl (calendar-check-holidays org-agenda-current-date)))
    (and hl (mapconcat #'identity hl "; "))))

(defun org-diary-to-ical-string (frombuf)
  "Get iCalendar entries from diary entries in buffer FROMBUF.
This uses the icalendar.el library."
  (let* ((tmpdir temporary-file-directory)
	 (tmpfile (make-temp-name
		   (expand-file-name "orgics" tmpdir)))
	 buf rtn b e)
    (unwind-protect
        (with-current-buffer frombuf
          (icalendar-export-region (point-min) (point-max) tmpfile)
          (setq buf (find-buffer-visiting tmpfile))
          (set-buffer buf)
          (goto-char (point-min))
          (when (re-search-forward "^BEGIN:VEVENT" nil t)
	    (setq b (match-beginning 0)))
          (goto-char (point-max))
          (when (re-search-backward "^END:VEVENT" nil t)
	    (setq e (match-end 0)))
          (setq rtn (if (and b e) (concat (buffer-substring b e) "\n") "")))
      (when (and buf (buffer-live-p buf)) (kill-buffer buf))
      (delete-file tmpfile))
    rtn))

(defun org-recenter-calendar (d)
  "If the calendar is visible, recenter it to date D."
  (let ((cwin (get-buffer-window calendar-buffer t)))
    (when cwin
      (let ((calendar-move-hook nil))
	(with-selected-window cwin
	  (calendar-goto-date
	   (if (listp d) d (calendar-gregorian-from-absolute d))))))))

(defun org-goto-calendar (&optional arg)
  "Go to the Emacs calendar at the current date.
If there is a time stamp in the current line, go to that date.
A prefix ARG can be used to force the current date."
  (interactive "P")
  (let ((calendar-move-hook nil)
	(calendar-view-holidays-initially-flag nil)
	(calendar-view-diary-initially-flag nil)
	diff)
    (when (or (org-at-timestamp-p 'lax)
	      (org-match-line (concat ".*" org-ts-regexp)))
      (let ((d1 (time-to-days nil))
	    (d2 (time-to-days (org-time-string-to-time (match-string 1)))))
	(setq diff (- d2 d1))))
    (calendar)
    (calendar-goto-today)
    (when (and diff (not arg)) (calendar-forward-day diff))))

(defun org-get-date-from-calendar ()
  "Return a list (month day year) of date at point in calendar."
  (with-current-buffer calendar-buffer
    (save-match-data
      (calendar-cursor-to-date))))

(defun org-date-from-calendar ()
  "Insert time stamp corresponding to cursor date in *Calendar* buffer.
If there is already a time stamp at the cursor position, update it."
  (interactive)
  (if (org-at-timestamp-p 'lax)
      (org-timestamp-change 0 'calendar)
    (let ((cal-date (org-get-date-from-calendar)))
      (org-insert-timestamp
       (org-encode-time 0 0 0 (nth 1 cal-date) (car cal-date) (nth 2 cal-date))))))

(defcustom org-image-actual-width t
  "When non-nil, use the actual width of images when inlining them.

When set to a number, use imagemagick (when available) to set the
image's width to this value.

When set to a number in a list, try to get the width from any
#+ATTR.* keyword if it matches a width specification like

  #+ATTR_HTML: :width 300px

and fall back on that number if none is found.

When set to nil, first try to get the width from #+ATTR_ORG.  If
that is not found, use the first #+ATTR_xxx :width specification.
If that is also not found, fall back on the original image width.

Finally, Org mode is quite flexible in the width specifications it
supports and intelligently interprets width specifications for other
backends when rendering an image in an org buffer.  This behavior is
described presently.

1. A floating point value between 0 and 2 is interpreted as the
   percentage of the text area that should be taken up by the image.
2. A number followed by a percent sign is divided by 100 and then
   interpreted as a floating point value.
3. If a number is followed by other text, extract the number and
   discard the remaining text.  That number is then interpreted as a
   floating-point value.  For example,

   #+ATTR_LATEX: :width 0.7\\linewidth

   would be interpreted as 70% of the text width.
4. If t is provided the original image width is used.  This is useful
   when you want to specify a width for a backend, but still want to
   use the original image width in the org buffer.

This requires Emacs >= 24.1, built with imagemagick support."
  :group 'org-appearance
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Use the image width" t)
	  (integer :tag "Use a number of pixels")
	  (list :tag "Use #+ATTR* or a number of pixels" (integer))
	  (const :tag "Use #+ATTR* or don't resize" nil)))

(defcustom org-image-max-width 'fill-column
  "When non-nil, limit the displayed image width.
This setting only takes effect when `org-image-actual-width' is set to
t or when #+ATTR* is set to t.

Possible values:
- `fill-column' :: limit width to `fill-column'
- `window'      :: limit width to window width
- integer       :: limit width to number in pixels
- float         :: limit width to that fraction of window width
- nil             :: do not limit image width"
  :group 'org-appearance
  :package-version '(Org . "9.7")
  :type '(choice
          (const :tag "Do not limit image width" nil)
          (const :tag "Limit to `fill-column'" fill-column)
          (const :tag "Limit to window width" window)
          (integer :tag "Limit to a number of pixels")
          (float :tag "Limit to a fraction of window width")))

(defcustom org-agenda-inhibit-startup nil
  "Inhibit startup when preparing agenda buffers.
When this variable is t, the initialization of the Org agenda
buffers is inhibited: e.g. the visibility state is not set, the
tables are not re-aligned, etc."
  :type 'boolean
  :version "24.3"
  :group 'org-agenda)

(defcustom org-agenda-ignore-properties nil
  "Avoid updating text properties when building the agenda.
Properties are used to prepare buffers for effort estimates,
appointments, statistics and subtree-local categories.
If you don't use these in the agenda, you can add them to this
list and agenda building will be a bit faster.
The value is a list, with symbol `stats'."
  :type '(set :greedy t
	      (const stats))
  :package-version '(Org . "9.7")
  :group 'org-agenda)

(defun org-agenda-prepare-buffers (files)
  "Create buffers for all agenda files, protect archived trees and comments."
  (interactive)
  (let ((inhibit-read-only t)
	(org-inhibit-startup org-agenda-inhibit-startup)
        ;; Do not refresh list of agenda files in the menu when
        ;; opening every new file.
        (org-agenda-file-menu-enabled nil))
    (setq org-tag-alist-for-agenda nil
	  org-tag-groups-alist-for-agenda nil)
    (dolist (file files)
      (catch 'nextfile
        (with-current-buffer
            (if (bufferp file)
                file
              (org-check-agenda-file file)
              (org-get-agenda-file-buffer file))
          (org-with-wide-buffer
	   (org-set-regexps-and-options 'tags-only)
	   (or (memq 'stats org-agenda-ignore-properties)
	       (org-refresh-stats-properties))
           (dolist (el org-todo-keywords-1)
             (unless (member el org-todo-keywords-for-agenda)
               (push el org-todo-keywords-for-agenda)))
           (dolist (el org-done-keywords)
             (unless (member el org-done-keywords-for-agenda)
               (push el org-done-keywords-for-agenda)))
	   (setq org-todo-keyword-alist-for-agenda
                 (org--tag-add-to-alist
		  org-todo-key-alist
                  org-todo-keyword-alist-for-agenda))
	   (setq org-tag-alist-for-agenda
		 (org--tag-add-to-alist
		  org-current-tag-alist
                  org-tag-alist-for-agenda))
	   ;; Merge current file's tag groups into global
	   ;; `org-tag-groups-alist-for-agenda'.
	   (when org-group-tags
	     (dolist (alist org-tag-groups-alist)
	       (let ((old (assoc (car alist) org-tag-groups-alist-for-agenda)))
		 (if old
		     (setcdr old (org-uniquify (append (cdr old) (cdr alist))))
		   (push alist org-tag-groups-alist-for-agenda)))))))))
    ;; Refresh the menu once after loading all the agenda buffers.
    (when org-agenda-file-menu-enabled
      (org-install-agenda-files-menu))))

;;;; LaTeX fragments

(defun org--make-preview-overlay (beg end image &optional imagetype)
  "Build an overlay between BEG and END using IMAGE file.
Argument IMAGETYPE is the extension of the displayed image,
as a string.  It defaults to \"png\"."
  (let ((ov (make-overlay beg end))
	(imagetype (or (intern imagetype) 'png)))
    (overlay-put ov 'org-overlay-type 'org-latex-overlay)
    (overlay-put ov 'evaporate t)
    (overlay-put ov
		 'modification-hooks
		 (list (lambda (o _flag _beg _end &optional _l)
			 (delete-overlay o))))
    (overlay-put ov
		 'display
		 (list 'image :type imagetype :file image :ascent 'center))))

(defun org-clear-latex-preview (&optional beg end)
  "Remove all overlays with LaTeX fragment images in current buffer.
When optional arguments BEG and END are non-nil, remove all
overlays between them instead.  Return a non-nil value when some
overlays were removed, nil otherwise."
  (let ((overlays
	 (cl-remove-if-not
	  (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
	  (overlays-in (or beg (point-min)) (or end (point-max))))))
    (mapc #'delete-overlay overlays)
    overlays))

(defun org--latex-preview-region (beg end)
  "Preview LaTeX fragments between BEG and END.
BEG and END are buffer positions."
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (save-excursion
      (org-format-latex
       (concat org-preview-latex-image-directory "org-ltximg")
       beg end
       ;; Emacs cannot overlay images from remote hosts.  Create it in
       ;; `temporary-file-directory' instead.
       (if (or (not file) (file-remote-p file))
	   temporary-file-directory
	 default-directory)
       'overlays nil 'forbuffer org-preview-latex-default-process))))

(defun org-latex-preview (&optional arg)
  "Toggle preview of the LaTeX fragment at point.

If the cursor is on a LaTeX fragment, create the image and
overlay it over the source code, if there is none.  Remove it
otherwise.  If there is no fragment at point, display images for
all fragments in the current section.  With an active region,
display images for all fragments in the region.

With a `\\[universal-argument]' prefix argument ARG, clear images \
for all fragments
in the current section.

With a `\\[universal-argument] \\[universal-argument]' prefix \
argument ARG, display image for all
fragments in the buffer.

With a `\\[universal-argument] \\[universal-argument] \
\\[universal-argument]' prefix argument ARG, clear image for all
fragments in the buffer."
  (interactive "P")
  (cond
   ((not (display-graphic-p)) nil)
   ((and untrusted-content (not org--latex-preview-when-risky)) nil)
   ;; Clear whole buffer.
   ((equal arg '(64))
    (org-clear-latex-preview (point-min) (point-max))
    (message "LaTeX previews removed from buffer"))
   ;; Preview whole buffer.
   ((equal arg '(16))
    (message "Creating LaTeX previews in buffer...")
    (org--latex-preview-region (point-min) (point-max))
    (message "Creating LaTeX previews in buffer... done."))
   ;; Clear current section.
   ((equal arg '(4))
    (org-clear-latex-preview
     (if (use-region-p)
         (region-beginning)
       (if (org-before-first-heading-p) (point-min)
         (save-excursion
	   (org-with-limited-levels (org-back-to-heading t) (point)))))
     (if (use-region-p)
         (region-end)
       (org-with-limited-levels (org-entry-end-position)))))
   ((use-region-p)
    (message "Creating LaTeX previews in region...")
    (org--latex-preview-region (region-beginning) (region-end))
    (message "Creating LaTeX previews in region... done."))
   ;; Toggle preview on LaTeX code at point.
   ((let ((datum (org-element-context)))
      (and (org-element-type-p datum '(latex-environment latex-fragment))
	   (let ((beg (org-element-begin datum))
		 (end (org-element-end datum)))
	     (if (org-clear-latex-preview beg end)
		 (message "LaTeX preview removed")
	       (message "Creating LaTeX preview...")
	       (org--latex-preview-region beg end)
	       (message "Creating LaTeX preview... done."))
	     t))))
   ;; Preview current section.
   (t
    (let ((beg (if (org-before-first-heading-p) (point-min)
		 (save-excursion
		   (org-with-limited-levels (org-back-to-heading t) (point)))))
	  (end (org-with-limited-levels (org-entry-end-position))))
      (message "Creating LaTeX previews in section...")
      (org--latex-preview-region beg end)
      (message "Creating LaTeX previews in section... done.")))))

(defun org-format-latex
    (prefix &optional beg end dir overlays msg forbuffer processing-type)
  "Replace LaTeX fragments with links to an image.

The function takes care of creating the replacement image.

Only consider fragments between BEG and END when those are
provided.

When optional argument OVERLAYS is non-nil, display the image on
top of the fragment instead of replacing it.

PROCESSING-TYPE is the conversion method to use, as a symbol.

Some of the options can be changed using the variable
`org-format-latex-options', which see."
  (when (and overlays (fboundp 'clear-image-cache)) (clear-image-cache))
  (unless (eq processing-type 'verbatim)
    (let* ((math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}")
	   (cnt 0)
	   checkdir-flag)
      (goto-char (or beg (point-min)))
      ;; FIXME: `overlay-recenter' is not needed (and has no effect)
      ;; since Emacs 29.
      ;; Optimize overlay creation: (info "(elisp) Managing Overlays").
      (when (and overlays (memq processing-type '(dvipng imagemagick)))
	(overlay-recenter (or end (point-max))))
      (while (re-search-forward math-regexp end t)
	(unless (and overlays
		     (eq (get-char-property (point) 'org-overlay-type)
			 'org-latex-overlay))
	  (let* ((context (org-element-context))
		 (type (org-element-type context)))
	    (when (memq type '(latex-environment latex-fragment))
	      (let ((block-type (eq type 'latex-environment))
		    (value (org-element-property :value context))
		    (beg (org-element-begin context))
		    (end (save-excursion
			   (goto-char (org-element-end context))
			   (skip-chars-backward " \r\t\n")
			   (point))))
		(cond
		 ((eq processing-type 'mathjax)
		  ;; Prepare for MathJax processing.
		  (if (not (string-match "\\`\\$\\$?" value))
		      (goto-char end)
		    (delete-region beg end)
		    (if (string= (match-string 0 value) "$$")
			(insert "\\[" (substring value 2 -2) "\\]")
		      (insert "\\(" (substring value 1 -1) "\\)"))))
		 ((eq processing-type 'html)
		  (goto-char beg)
		  (delete-region beg end)
		  (insert (org-format-latex-as-html value)))
		 ((assq processing-type org-preview-latex-process-alist)
		  ;; Process to an image.
		  (cl-incf cnt)
		  (goto-char beg)
		  (let* ((processing-info
			  (cdr (assq processing-type org-preview-latex-process-alist)))
			 (face (face-at-point))
			 ;; Get the colors from the face at point.
			 (fg
			  (let ((color (plist-get org-format-latex-options
						  :foreground)))
                            (if forbuffer
                                (cond
                                 ((eq color 'auto)
                                  (face-attribute face :foreground nil 'default))
                                 ((eq color 'default)
                                  (face-attribute 'default :foreground nil))
                                 (t color))
                              color)))
			 (bg
			  (let ((color (plist-get org-format-latex-options
						  :background)))
                            (if forbuffer
                                (cond
                                 ((eq color 'auto)
                                  (face-attribute face :background nil 'default))
                                 ((eq color 'default)
                                  (face-attribute 'default :background nil))
                                 (t color))
                              color)))
			 (hash (sha1 (prin1-to-string
				      (list org-format-latex-header
					    org-latex-default-packages-alist
					    org-latex-packages-alist
					    org-format-latex-options
					    forbuffer value fg bg))))
			 (imagetype (or (plist-get processing-info :image-output-type) "png"))
			 (absprefix (expand-file-name prefix dir))
			 (linkfile (format "%s_%s.%s" prefix hash imagetype))
			 (movefile (format "%s_%s.%s" absprefix hash imagetype))
			 (sep (and block-type "\n\n"))
			 (link (concat sep "[[file:" linkfile "]]" sep))
			 (options
			  (org-combine-plists
			   org-format-latex-options
			   `(:foreground ,fg :background ,bg))))
		    (when msg (message msg cnt))
		    (unless checkdir-flag ; Ensure the directory exists.
		      (setq checkdir-flag t)
		      (let ((todir (file-name-directory absprefix)))
			(unless (file-directory-p todir)
			  (make-directory todir t))))
		    (unless (file-exists-p movefile)
		      (org-create-formula-image
		       value movefile options forbuffer processing-type))
                    (org-place-formula-image link block-type beg end value overlays movefile imagetype)))
		 ((eq processing-type 'mathml)
		  ;; Process to MathML.
		  (unless (org-format-latex-mathml-available-p)
		    (user-error "LaTeX to MathML converter not configured"))
		  (cl-incf cnt)
		  (when msg (message msg cnt))
		  (goto-char beg)
		  (delete-region beg end)
		  (insert (org-format-latex-as-mathml
			   value block-type prefix dir)))
		 (t
		  (error "Unknown conversion process %s for LaTeX fragments"
			 processing-type)))))))))))

(defun org-place-formula-image (link block-type beg end value overlays movefile imagetype)
  "Place an overlay from BEG to END showing MOVEFILE.
The overlay will be above BEG if OVERLAYS is non-nil."
  (if overlays
      (progn
        (dolist (o (overlays-in beg end))
          (when (eq (overlay-get o 'org-overlay-type)
                    'org-latex-overlay)
            (delete-overlay o)))
        (org--make-preview-overlay beg end movefile imagetype)
        (goto-char end))
    (delete-region beg end)
    (insert
     (org-add-props link
         (list 'org-latex-src
               (replace-regexp-in-string "\"" "" value)
               'org-latex-src-embed-type
               (if block-type 'paragraph 'character))))))

(defun org-create-math-formula (latex-frag &optional mathml-file)
  "Convert LATEX-FRAG to MathML and store it in MATHML-FILE.
Use `org-latex-to-mathml-convert-command'.  If the conversion is
successful, return the portion between \"<math...> </math>\"
elements otherwise return nil.  When MATHML-FILE is specified,
write the results in to that file.  When invoked as an
interactive command, prompt for LATEX-FRAG, with initial value
set to the current active region and echo the results for user
inspection."
  (interactive (list (let ((frag (when (org-region-active-p)
				   (buffer-substring-no-properties
				    (region-beginning) (region-end)))))
		       (read-string "LaTeX Fragment: " frag nil frag))))
  (unless latex-frag (user-error "Invalid LaTeX fragment"))
  (let* ((tmp-in-file
	  (let ((file (file-relative-name
		       (make-temp-name (expand-file-name "ltxmathml-in")))))
	    (write-region latex-frag nil file)
	    file))
	 (tmp-out-file (file-relative-name
			(make-temp-name (expand-file-name  "ltxmathml-out"))))
	 (cmd (format-spec
	       org-latex-to-mathml-convert-command
	       `((?j . ,(and org-latex-to-mathml-jar-file
			     (shell-quote-argument
			      (expand-file-name
			       org-latex-to-mathml-jar-file))))
		 (?I . ,(shell-quote-argument tmp-in-file))
		 (?i . ,(shell-quote-argument latex-frag))
		 (?o . ,(shell-quote-argument tmp-out-file)))))
	 mathml shell-command-output)
    (when (called-interactively-p 'any)
      (unless (org-format-latex-mathml-available-p)
	(user-error "LaTeX to MathML converter not configured")))
    (message "Running %s" cmd)
    (setq shell-command-output (shell-command-to-string cmd))
    (setq mathml
	  (when (file-readable-p tmp-out-file)
	    (with-temp-buffer
              (insert-file-contents tmp-out-file)
	      (goto-char (point-min))
	      (when (re-search-forward
		     (format "<math[^>]*?%s[^>]*?>\\(.\\|\n\\)*</math>"
			     (regexp-quote
			      "xmlns=\"http://www.w3.org/1998/Math/MathML\""))
		     nil t)
		(match-string 0)))))
    (cond
     (mathml
      (setq mathml
	    (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" mathml))
      (when mathml-file
	(write-region mathml nil mathml-file))
      (when (called-interactively-p 'any)
	(message mathml)))
     ((warn "LaTeX to MathML conversion failed")
      (message shell-command-output)))
    (delete-file tmp-in-file)
    (when (file-exists-p tmp-out-file)
      (delete-file tmp-out-file))
    mathml))

(defun org-format-latex-as-mathml (latex-frag latex-frag-type
					      prefix &optional dir)
  "Use `org-create-math-formula' but check local cache first."
  (let* ((absprefix (expand-file-name prefix dir))
	 (print-length nil) (print-level nil)
	 (formula-id (concat
		      "formula-"
		      (sha1
		       (prin1-to-string
			(list latex-frag
			      org-latex-to-mathml-convert-command)))))
	 (formula-cache (format "%s-%s.mathml" absprefix formula-id))
	 (formula-cache-dir (file-name-directory formula-cache)))

    (unless (file-directory-p formula-cache-dir)
      (make-directory formula-cache-dir t))

    (unless (file-exists-p formula-cache)
      (org-create-math-formula latex-frag formula-cache))

    (if (file-exists-p formula-cache)
	;; Successful conversion.  Return the link to MathML file.
	(org-add-props
	    (format  "[[file:%s]]" (file-relative-name formula-cache dir))
	    (list 'org-latex-src (replace-regexp-in-string "\"" "" latex-frag)
		  'org-latex-src-embed-type (if latex-frag-type
						'paragraph 'character)))
      ;; Failed conversion.  Return the LaTeX fragment verbatim
      latex-frag)))

(defun org-format-latex-as-html (latex-fragment)
  "Convert LATEX-FRAGMENT to HTML.
This uses  `org-latex-to-html-convert-command', which see."
  (let ((cmd (format-spec org-latex-to-html-convert-command
			  `((?i . ,(shell-quote-argument latex-fragment))))))
    (message "Running %s" cmd)
    (shell-command-to-string cmd)))

(defun org--get-display-dpi ()
  "Get the DPI of the display.
The function assumes that the display has the same pixel width in
the horizontal and vertical directions."
  (if (display-graphic-p)
      (round (/ (display-pixel-height)
		(/ (display-mm-height) 25.4)))
    (error "Attempt to calculate the dpi of a non-graphic display")))

(defun org-create-formula-image
    (string tofile options buffer &optional processing-type)
  "Create an image from LaTeX source using external processes.

The LaTeX STRING is saved to a temporary LaTeX file, then
converted to an image file by process PROCESSING-TYPE defined in
`org-preview-latex-process-alist'.  A nil value defaults to
`org-preview-latex-default-process'.

The generated image file is eventually moved to TOFILE.

The OPTIONS argument controls the size, foreground color and
background color of the generated image.

When BUFFER non-nil, this function is used for LaTeX previewing.
Otherwise, it is used to deal with LaTeX snippets showed in
a HTML file."
  (let* ((processing-type (or processing-type
			      org-preview-latex-default-process))
	 (processing-info
	  (cdr (assq processing-type org-preview-latex-process-alist)))
	 (programs (plist-get processing-info :programs))
	 (error-message (or (plist-get processing-info :message) ""))
	 (image-input-type (plist-get processing-info :image-input-type))
	 (image-output-type (plist-get processing-info :image-output-type))
	 (post-clean (or (plist-get processing-info :post-clean)
			 '(".dvi" ".xdv" ".pdf" ".tex" ".aux" ".log"
			   ".svg" ".png" ".jpg" ".jpeg" ".out")))
	 (latex-header
	  (or (plist-get processing-info :latex-header)
	      (org-latex-make-preamble
	       (org-export-get-environment (org-export-get-backend 'latex))
	       org-format-latex-header
	       'snippet)))
	 (latex-compiler (plist-get processing-info :latex-compiler))
	 (tmpdir temporary-file-directory)
	 (texfilebase (make-temp-name
		       (expand-file-name "orgtex" tmpdir)))
	 (texfile (concat texfilebase ".tex"))
	 (image-size-adjust (or (plist-get processing-info :image-size-adjust)
				'(1.0 . 1.0)))
	 (scale (* (if buffer (car image-size-adjust) (cdr image-size-adjust))
		   (or (plist-get options (if buffer :scale :html-scale)) 1.0)))
	 (dpi (* scale (if (and buffer (display-graphic-p)) (org--get-display-dpi) 140.0)))
	 (fg (or (plist-get options (if buffer :foreground :html-foreground))
		 "Black"))
	 (bg (or (plist-get options (if buffer :background :html-background))
		 "Transparent"))
	 (image-converter
          (or (and (string= bg "Transparent")
                   (plist-get processing-info :transparent-image-converter))
              (plist-get processing-info :image-converter)))
         (log-buf (get-buffer-create "*Org Preview LaTeX Output*"))
	 (resize-mini-windows nil)) ;Fix Emacs flicker when creating image.
    (dolist (program programs)
      (org-check-external-command program error-message))
    (if (eq fg 'default)
	(setq fg (org-latex-color :foreground))
      (setq fg (org-latex-color-format fg)))
    (setq bg (cond
	      ((eq bg 'default) (org-latex-color :background))
	      ((string= bg "Transparent") nil)
	      (t (org-latex-color-format bg))))
    ;; Remove TeX \par at end of snippet to avoid trailing space.
    (if (string-suffix-p string "\n")
        (aset string (1- (length string)) ?%)
      (setq string (concat string "%")))
    (with-temp-file texfile
      (insert latex-header)
      (insert "\n\\begin{document}\n"
	      "\\definecolor{fg}{rgb}{" fg "}%\n"
	      (if bg
		  (concat "\\definecolor{bg}{rgb}{" bg "}%\n"
			  "\n\\pagecolor{bg}%\n")
		"")
	      "\n{\\color{fg}\n"
	      string
	      "\n}\n"
	      "\n\\end{document}\n"))
    (let* ((err-msg (format "Please adjust `%s' part of \
`org-preview-latex-process-alist'."
			    processing-type))
	   (image-input-file
	    (org-compile-file
	     texfile latex-compiler image-input-type err-msg log-buf))
	   (image-output-file
	    (org-compile-file
	     image-input-file image-converter image-output-type err-msg log-buf
	     `((?D . ,(shell-quote-argument (format "%s" dpi)))
	       (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))))))
      (copy-file image-output-file tofile 'replace)
      (dolist (e post-clean)
	(when (file-exists-p (concat texfilebase e))
	  (delete-file (concat texfilebase e))))
      image-output-file)))

(defun org-splice-latex-header (tpl def-pkg pkg snippets-p &optional extra)
  "Fill a LaTeX header template TPL.
In the template, the following place holders will be recognized:

 [DEFAULT-PACKAGES]      \\usepackage statements for DEF-PKG
 [NO-DEFAULT-PACKAGES]   do not include DEF-PKG
 [PACKAGES]              \\usepackage statements for PKG
 [NO-PACKAGES]           do not include PKG
 [EXTRA]                 the string EXTRA
 [NO-EXTRA]              do not include EXTRA

For backward compatibility, if both the positive and the negative place
holder is missing, the positive one (without the \"NO-\") will be
assumed to be present at the end of the template.
DEF-PKG and PKG are assumed to be alists of options/packagename lists.
EXTRA is a string.
SNIPPETS-P indicates if this is run to create snippet images for HTML."
  (let (rpl (end ""))
    (if (string-match "^[ \t]*\\[\\(NO-\\)?DEFAULT-PACKAGES\\][ \t]*\n?" tpl)
	(setq rpl (if (or (match-end 1) (not def-pkg))
		      "" (org-latex-packages-to-string def-pkg snippets-p t))
	      tpl (replace-match rpl t t tpl))
      (when def-pkg (setq end (org-latex-packages-to-string def-pkg snippets-p))))

    (if (string-match "\\[\\(NO-\\)?PACKAGES\\][ \t]*\n?" tpl)
	(setq rpl (if (or (match-end 1) (not pkg))
		      "" (org-latex-packages-to-string pkg snippets-p t))
	      tpl (replace-match rpl t t tpl))
      (when pkg (setq end
		      (concat end "\n"
			      (org-latex-packages-to-string pkg snippets-p)))))

    (if (string-match "\\[\\(NO-\\)?EXTRA\\][ \t]*\n?" tpl)
	(setq rpl (if (or (match-end 1) (not extra))
		      "" (concat extra "\n"))
	      tpl (replace-match rpl t t tpl))
      (when (and extra (string-match "\\S-" extra))
	(setq end (concat end "\n" extra))))

    (if (string-match "\\S-" end)
	(concat tpl "\n" end)
      tpl)))

(defun org-latex-packages-to-string (pkg &optional snippets-p newline)
  "Turn an alist of packages into a string with the \\usepackage macros."
  (setq pkg (mapconcat (lambda(p)
			 (cond
			  ((stringp p) p)
			  ((and snippets-p (>= (length p) 3) (not (nth 2 p)))
			   (format "%% Package %s omitted" (cadr p)))
			  ((equal "" (car p))
			   (format "\\usepackage{%s}" (cadr p)))
			  (t
			   (format "\\usepackage[%s]{%s}"
				   (car p) (cadr p)))))
		       pkg
		       "\n"))
  (if newline (concat pkg "\n") pkg))

(defun org-dvipng-color (attr)
  "Return a RGB color specification for dvipng."
  (org-dvipng-color-format (face-attribute 'default attr nil)))

(defun org-dvipng-color-format (color-name)
  "Convert COLOR-NAME to a RGB color value for dvipng."
  (apply #'format "rgb %s %s %s"
	 (mapcar 'org-normalize-color
		 (color-values color-name))))

(defun org-latex-color (attr)
  "Return a RGB color for the LaTeX color package."
  (org-latex-color-format (face-attribute 'default attr nil)))

(defun org-latex-color-format (color-name)
  "Convert COLOR-NAME to a RGB color value."
  (apply #'format "%s,%s,%s"
	 (mapcar 'org-normalize-color
		 (color-values color-name))))

(defun org-normalize-color (value)
  "Return string to be used as color value for an RGB component."
  (format "%g" (/ value 65535.0)))


;; Image display

(defvar-local org-inline-image-overlays nil)
;; Preserve when switching modes or when restarting Org.
;; If we clear the overlay list and later enable Or mode, the existing
;; image overlays will never be cleared by `org-toggle-inline-images'.
(put 'org-inline-image-overlays 'permanent-local t)

(defun org--inline-image-overlays (&optional beg end)
  "Return image overlays between BEG and END."
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (overlays (overlays-in beg end))
         result)
    (dolist (ov overlays result)
      (when (memq ov org-inline-image-overlays)
        (push ov result)))))

(defun org-toggle-inline-images (&optional include-linked beg end)
  "Toggle the display of inline images.
INCLUDE-LINKED is passed to `org-display-inline-images'."
  (interactive "P")
  (if (org--inline-image-overlays beg end)
      (progn
        (org-remove-inline-images beg end)
        (when (called-interactively-p 'interactive)
	  (message "Inline image display turned off")))
    (org-display-inline-images include-linked nil beg end)
    (when (called-interactively-p 'interactive)
      (let ((new (org--inline-image-overlays beg end)))
        (message (if new
		     (format "%d images displayed inline"
			     (length new))
		   "No images to display inline"))))))

(defun org-redisplay-inline-images ()
  "Assure display of inline images and refresh them."
  (interactive)
  (org-toggle-inline-images)
  (unless org-inline-image-overlays
    (org-toggle-inline-images)))

;; For without-x builds.
(declare-function image-flush "image" (spec &optional frame))

(defcustom org-display-remote-inline-images 'skip
  "How to display remote inline images.
Possible values of this option are:

skip        Don't display remote images.
download    Always download and display remote images.
t
cache       Display remote images, and open them in separate buffers
            for caching.  Silently update the image buffer when a file
            change is detected."
  :group 'org-appearance
  :package-version '(Org . "9.7")
  :type '(choice
	  (const :tag "Ignore remote images" skip)
	  (const :tag "Always display remote images" download)
	  (const :tag "Display and silently update remote images" cache))
  :safe #'symbolp)

(defcustom org-image-align 'left
  "How to align images previewed using `org-display-inline-images'.

Only stand-alone image links are affected by this setting.  These
are links without surrounding text.

Possible values of this option are:

left     Insert image at specified position.
center   Center image previews.
right    Right-align image previews."
  :group 'org-appearance
  :package-version '(Org . "9.7")
  :type '(choice
          (const :tag "Left align (or don\\='t align) image previews" left)
	  (const :tag "Center image previews" center)
	  (const :tag "Right align image previews" right))
  :safe #'symbolp)

(defun org--create-inline-image (file width)
  "Create image located at FILE, or return nil.
WIDTH is the width of the image.  The image may not be created
according to the value of `org-display-remote-inline-images'."
  (let* ((remote? (file-remote-p file))
	 (file-or-data
	  (pcase org-display-remote-inline-images
	    ((guard (not remote?)) file)
	    (`download (with-temp-buffer
			 (set-buffer-multibyte nil)
			 (insert-file-contents-literally file)
			 (buffer-string)))
	    ((or `cache `t)
             (let ((revert-without-query '(".")))
	       (with-current-buffer (find-file-noselect file)
		 (buffer-string))))
	    (`skip nil)
	    (other
	     (message "Invalid value of `org-display-remote-inline-images': %S"
		      other)
	     nil))))
    (when file-or-data
      (create-image file-or-data
		    (and (image-type-available-p 'imagemagick)
			 width
			 'imagemagick)
		    remote?
		    :width width
                    :max-width
                    (pcase org-image-max-width
                      (`fill-column (* fill-column (frame-char-width (selected-frame))))
                      (`window (window-width nil t))
                      ((pred integerp) org-image-max-width)
                      ((pred floatp) (floor (* org-image-max-width (window-width nil t))))
                      (`nil nil)
                      (_ (error "Unsupported value of `org-image-max-width': %S"
                                org-image-max-width)))
                    :scale 1))))

(defun org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.

An inline image is a link which follows either of these
conventions:

  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous
     type.  In this case, that link must be a well-formed plain
     or angle link, i.e., it must have an explicit \"file\" or
     \"attachment\" type.

Equip each image with the key-map `image-map'.

When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.

BEG and END define the considered part.  They default to the
buffer boundaries with possible narrowing."
  (interactive "P")
  (when (display-graphic-p)
    (when refresh
      (org-remove-inline-images beg end)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (let ((end (or end (point-max))))
      (org-with-point-at (or beg (point-min))
	(let* ((case-fold-search t)
	       (file-extension-re (image-file-name-regexp))
	       (link-abbrevs (mapcar #'car
				     (append (org-element-property :link-abbrevs (org-element-org-data))
					     org-link-abbrev-alist)))
	       ;; Check absolute, relative file names and explicit
	       ;; "file:" links.  Also check link abbreviations since
	       ;; some might expand to "file" links.
	       (file-types-re
		(format "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]\\[\\(<?\\(?:file\\|attachment\\):\\)"
			(if (not link-abbrevs) ""
			  (concat "\\|" (regexp-opt link-abbrevs))))))
	  (while (re-search-forward file-types-re end t)
	    (let* ((link (org-element-lineage
			  (save-match-data (org-element-context))
			  'link t))
                   (linktype (org-element-property :type link))
		   (inner-start (match-beginning 1))
		   (path
		    (cond
		     ;; No link at point; no inline image.
		     ((not link) nil)
		     ;; File link without a description.  Also handle
		     ;; INCLUDE-LINKED here since it should have
		     ;; precedence over the next case.  I.e., if link
		     ;; contains filenames in both the path and the
		     ;; description, prioritize the path only when
		     ;; INCLUDE-LINKED is non-nil.
		     ((or (not (org-element-contents-begin link))
			  include-linked)
		      (and (or (equal "file" linktype)
                               (equal "attachment" linktype))
			   (org-element-property :path link)))
		     ;; Link with a description.  Check if description
		     ;; is a filename.  Even if Org doesn't have syntax
		     ;; for those -- clickable image -- constructs, fake
		     ;; them, as in `org-export-insert-image-links'.
		     ((not inner-start) nil)
		     (t
		      (org-with-point-at inner-start
			(and (looking-at
			      (if (char-equal ?< (char-after inner-start))
				  org-link-angle-re
				org-link-plain-re))
			     ;; File name must fill the whole
			     ;; description.
			     (= (org-element-contents-end link)
				(match-end 0))
			     (progn
                               (setq linktype (match-string 1))
                               (match-string 2))))))))
	      (when (and path (string-match-p file-extension-re path))
		(let ((file (if (equal "attachment" linktype)
				(progn
                                  (require 'org-attach)
				  (ignore-errors (org-attach-expand path)))
                              (expand-file-name path))))
                  ;; Expand environment variables.
                  (when file (setq file (substitute-in-file-name file)))
		  (when (and file (file-exists-p file))
		    (let ((width (org-display-inline-image--width link))
			  (align (org-image--align link))
                          (old (get-char-property-and-overlay
				(org-element-begin link)
				'org-image-overlay)))
		      (if (and (car-safe old) refresh)
                          (image-flush (overlay-get (cdr old) 'display))
			(let ((image (org--create-inline-image file width)))
			  (when image
			    (let ((ov (make-overlay
				       (org-element-begin link)
				       (progn
					 (goto-char
					  (org-element-end link))
					 (unless (eolp) (skip-chars-backward " \t"))
					 (point)))))
                              ;; See bug#59902.  We cannot rely
                              ;; on Emacs to update image if the file
                              ;; has changed.
                              (image-flush image)
			      (overlay-put ov 'display image)
			      (overlay-put ov 'face 'default)
			      (overlay-put ov 'org-image-overlay t)
			      (overlay-put
			       ov 'modification-hooks
			       (list 'org-display-inline-remove-overlay))
			      (when (boundp 'image-map)
				(overlay-put ov 'keymap image-map))
                              (when align
                                (overlay-put
                                 ov 'before-string
                                 (propertize
                                  " " 'face 'default
                                  'display
                                  (pcase align
                                    ("center" `(space :align-to (- center (0.5 . ,image))))
                                    ("right"  `(space :align-to (- right ,image)))))))
			      (push ov org-inline-image-overlays))))))))))))))))

(declare-function org-export-read-attribute "ox"
                  (attribute element &optional property))
(defvar visual-fill-column-width) ; Silence compiler warning
(defun org-display-inline-image--width (link)
  "Determine the display width of the image LINK, in pixels.
- When `org-image-actual-width' is t, the image's pixel width is used.
- When `org-image-actual-width' is a number, that value will is used.
- When `org-image-actual-width' is nil or a list, :width attribute of
  #+attr_org or the first #+attr_...  (if it exists) is used to set the
  image width.  A width of X% is divided by 100.  If the value is a
  float between 0 and 2, it interpreted as that proportion of the text
  width in the buffer.

  If no :width attribute is given and `org-image-actual-width' is a
  list with a number as the car, then that number is used as the
  default value."
  ;; Apply `org-image-actual-width' specifications.
  ;; Support subtree-level property "ORG-IMAGE-ACTUAL-WIDTH" specified
  ;; width.
  (let ((org-image-actual-width (org-property-or-variable-value 'org-image-actual-width)))
    (cond
     ((eq org-image-actual-width t) nil)
     ((listp org-image-actual-width)
      (require 'ox)
      (let* ((par (org-element-lineage link 'paragraph))
             ;; Try to find an attribute providing a :width.
             ;; #+ATTR_ORG: :width ...
             (attr-width (org-export-read-attribute :attr_org par :width))
             (width-unreadable?
              (lambda (value)
                (or (not (stringp value))
                    (unless (string= value "t")
                      (or (not (string-match
				(rx bos (opt "+")
                                    (or
                                     ;; Number of pixels
                                     ;; must be a lone number, not
                                     ;; things like 4in
                                     (seq (1+ (in "0-9")) eos)
                                     ;; Numbers ending with %
                                     (seq (1+ (in "0-9.")) (group-n 1 "%"))
                                     ;; Fractions
                                     (seq (0+ (in "0-9")) "." (1+ (in "0-9")))))
				value))
                          (let ((number (string-to-number value)))
                            (and (floatp number)
                                 (not (match-string 1 value)) ; X%
                                 (not (<= 0.0 number 2.0)))))))))
             ;; #+ATTR_BACKEND: :width ...
             (attr-other
              (catch :found
                (org-element-properties-map
                 (lambda (prop _)
                   (when (and
                          (not (eq prop :attr_org))
                          (string-match-p "^:attr_" (symbol-name prop))
                          (not (funcall width-unreadable? (org-export-read-attribute prop par :width))))
                     (throw :found prop)))
                 par)))
             (attr-width
              (if (not (funcall width-unreadable? attr-width))
                  attr-width
                ;; When #+attr_org: does not have readable :width
                (and attr-other
                     (org-export-read-attribute attr-other par :width))))
             (width
              (cond
               ;; Treat :width t as if `org-image-actual-width' were t.
               ((string= attr-width "t") nil)
               ;; Fallback to `org-image-actual-width' if no interprable width is given.
               ((funcall width-unreadable? attr-width)
                (car org-image-actual-width))
               ;; Convert numeric widths to numbers, converting percentages.
               ((string-match-p "\\`[[+]?[0-9.]+%" attr-width)
                (/ (string-to-number attr-width) 100.0))
               (t (string-to-number attr-width)))))
        (if (and (floatp width) (<= 0.0 width 2.0))
            ;; A float in [0,2] should be interpereted as this portion of
            ;; the text width in the window.  This works well with cases like
            ;; #+attr_latex: :width 0.X\{line,page,column,etc.}width,
            ;; as the "0.X" is pulled out as a float.  We use 2 as the upper
            ;; bound as cases such as 1.2\linewidth are feasible.
            (round (* width
                      (window-pixel-width)
                      (/ (or (and (bound-and-true-p visual-fill-column-mode)
                                  (or visual-fill-column-width auto-fill-function))
                             (when auto-fill-function fill-column)
                             (- (window-text-width) (line-number-display-width)))
                         (float (window-total-width)))))
          width)))
     ((numberp org-image-actual-width)
      org-image-actual-width)
     (t nil))))

(defun org-image--align (link)
  "Determine the alignment of the image LINK.
LINK is a link object.

In decreasing order of priority, this is controlled:
- Per image by the value of `:center' or `:align' in the
affiliated keyword `#+attr_org'.
- By the `#+attr_html' or `#+attr_latex` keywords with valid
  `:center' or `:align' values.
- Globally by the user option `org-image-align'.

The result is either nil or one of the strings \"left\",
\"center\" or \"right\".

\"center\" will cause the image preview to be centered, \"right\"
will cause it to be right-aligned.  A value of \"left\" or nil
implies no special alignment."
  (let ((par (org-element-lineage link 'paragraph)))
    ;; Only align when image is not surrounded by paragraph text:
    (when (and par ; when image is not in paragraph, but in table/headline/etc, do not align
               (= (org-element-begin link)
                  (save-excursion
                    (goto-char (org-element-contents-begin par))
                    (skip-chars-forward "\t ")
                    (point)))           ;account for leading space
                                        ;before link
               (<= (- (org-element-contents-end par)
                     (org-element-end link))
                  1))                  ;account for trailing newline
                                        ;at end of paragraph
      (save-match-data
        ;; Look for a valid ":center t" or ":align left|center|right"
        ;; attribute.
        ;;
        ;; An attr_org keyword has the highest priority, with
        ;; any attr.* next.  Choosing between these is
        ;; unspecified.
        (let ((center-re ":\\(center\\)[[:space:]]+t\\b")
              (align-re ":align[[:space:]]+\\(left\\|center\\|right\\)\\b")
              attr-align)
          (catch 'exit
            (org-element-properties-mapc
             (lambda (propname propval)
               (when (and propval
                          (string-match-p ":attr.*" (symbol-name propname)))
                 (setq propval (car-safe propval))
                 (when (or (string-match center-re propval)
                           (string-match align-re propval))
                   (setq attr-align (match-string 1 propval))
                   (when (eq propname :attr_org)
                     (throw 'exit t)))))
             par))
          (if attr-align
              (when (member attr-align '("center" "right")) attr-align)
            ;; No image-specific keyword, check global alignment property
            (when (memq org-image-align '(center right))
              (symbol-name org-image-align))))))))


(defun org-display-inline-remove-overlay (ov after _beg _end &optional _len)
  "Remove inline-display overlay if a corresponding region is modified."
  (when (and ov after)
    (setq org-inline-image-overlays (delete ov org-inline-image-overlays))
    ;; Clear image from cache to avoid image not updating upon
    ;; changing on disk.  See Emacs bug#59902.
    (when (overlay-get ov 'org-image-overlay)
      (image-flush (overlay-get ov 'display)))
    (delete-overlay ov)))

(defun org-remove-inline-images (&optional beg end)
  "Remove inline display of images."
  (interactive)
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (overlays (overlays-in beg end)))
    (dolist (ov overlays)
      (when (memq ov org-inline-image-overlays)
        (setq org-inline-image-overlays (delq ov org-inline-image-overlays))
        (delete-overlay ov)))
    ;; Clear removed overlays.
    (dolist (ov org-inline-image-overlays)
      (unless (overlay-buffer ov)
        (setq org-inline-image-overlays (delq ov org-inline-image-overlays))))))

;;; Menu entries
(defsubst org-in-subtree-not-table-p ()
  "Are we in a subtree and not in a table?"
  (and (not (org-before-first-heading-p))
       (not (org-at-table-p))))

;; Define the Org mode menus
(easy-menu-define org-org-menu org-mode-map "Org menu."
  `("Org"
    ("Show/Hide"
     ["Cycle Visibility" org-cycle :active (or (bobp) (outline-on-heading-p))]
     ["Cycle Global Visibility" org-shifttab :active (not (org-at-table-p))]
     ["Sparse Tree..." org-sparse-tree t]
     ["Reveal Context" org-fold-reveal t]
     ["Show All" org-fold-show-all t]
     "--"
     ["Subtree to indirect buffer" org-tree-to-indirect-buffer t])
    "--"
    ["New Heading" org-insert-heading t]
    ("Navigate Headings"
     ["Up" outline-up-heading t]
     ["Next" outline-next-visible-heading t]
     ["Previous" outline-previous-visible-heading t]
     ["Next Same Level" outline-forward-same-level t]
     ["Previous Same Level" outline-backward-same-level t]
     "--"
     ["Jump" org-goto t])
    ("Edit Structure"
     ["Move Subtree Up" org-metaup (org-at-heading-p)]
     ["Move Subtree Down" org-metadown (org-at-heading-p)]
     "--"
     ["Copy Subtree"  org-copy-special (org-in-subtree-not-table-p)]
     ["Cut Subtree"  org-cut-special (org-in-subtree-not-table-p)]
     ["Paste Subtree"  org-paste-special (not (org-at-table-p))]
     "--"
     ["Clone subtree, shift time" org-clone-subtree-with-time-shift t]
     "--"
     ["Copy visible text"  org-copy-visible t]
     "--"
     ["Promote Heading" org-metaleft (org-in-subtree-not-table-p)]
     ["Promote Subtree" org-shiftmetaleft (org-in-subtree-not-table-p)]
     ["Demote Heading"  org-metaright (org-in-subtree-not-table-p)]
     ["Demote Subtree"  org-shiftmetaright (org-in-subtree-not-table-p)]
     "--"
     ["Sort Region/Children" org-sort t]
     "--"
     ["Convert to odd levels" org-convert-to-odd-levels t]
     ["Convert to odd/even levels" org-convert-to-oddeven-levels t])
    ("Editing"
     ["Emphasis..." org-emphasize t]
     ["Add block structure" org-insert-structure-template t]
     ["Edit Source Example" org-edit-special t]
     "--"
     ["Footnote new/jump" org-footnote-action t]
     ["Footnote extra" (org-footnote-action t) :active t :keys "C-u C-c C-x f"])
    ("Archive"
     ["Archive (default method)" org-archive-subtree-default (org-in-subtree-not-table-p)]
     "--"
     ["Move Subtree to Archive file" org-archive-subtree (org-in-subtree-not-table-p)]
     ["Toggle ARCHIVE tag" org-toggle-archive-tag (org-in-subtree-not-table-p)]
     ["Move subtree to Archive sibling" org-archive-to-archive-sibling (org-in-subtree-not-table-p)])
    "--"
    ("Hyperlinks"
     ["Store Link (Global)" org-store-link t]
     ["Find existing link to here" org-occur-link-in-agenda-files t]
     ["Insert Link" org-insert-link t]
     ["Follow Link" org-open-at-point t]
     "--"
     ["Next link" org-next-link t]
     ["Previous link" org-previous-link t]
     "--"
     ["Descriptive Links"
      org-toggle-link-display
      :style radio
      :selected org-descriptive-links
      ]
     ["Literal Links"
      org-toggle-link-display
      :style radio
      :selected (not org-descriptive-links)])
    "--"
    ("TODO Lists"
     ["TODO/DONE/-" org-todo t]
     ("Select keyword"
      ["Next keyword" org-shiftright (org-at-heading-p)]
      ["Previous keyword" org-shiftleft (org-at-heading-p)]
      ["Complete Keyword" pcomplete (assq :todo-keyword (org-context))]
      ["Next keyword set" org-shiftcontrolright (and (> (length org-todo-sets) 1) (org-at-heading-p))]
      ["Previous keyword set" org-shiftcontrolright (and (> (length org-todo-sets) 1) (org-at-heading-p))])
     ["Show TODO Tree" org-show-todo-tree :active t :keys "C-c / t"]
     ["Global TODO list" org-todo-list :active t :keys "\\[org-agenda] t"]
     "--"
     ["Enforce dependencies" (customize-variable 'org-enforce-todo-dependencies)
      :selected org-enforce-todo-dependencies :style toggle :active t]
     "Settings for tree at point"
     ["Do Children sequentially" org-toggle-ordered-property :style radio
      :selected (org-entry-get nil "ORDERED")
      :active org-enforce-todo-dependencies :keys "C-c C-x o"]
     ["Do Children parallel" org-toggle-ordered-property :style radio
      :selected (not (org-entry-get nil "ORDERED"))
      :active org-enforce-todo-dependencies :keys "C-c C-x o"]
     "--"
     ["Set Priority" org-priority t]
     ["Priority Up" org-shiftup t]
     ["Priority Down" org-shiftdown t]
     "--"
     ["Get news from all feeds" org-feed-update-all t]
     ["Go to the inbox of a feed..." org-feed-goto-inbox t]
     ["Customize feeds" (customize-variable 'org-feed-alist) t])
    ("TAGS and Properties"
     ["Set Tags" org-set-tags-command (not (org-before-first-heading-p))]
     ["Change tag in region" org-change-tag-in-region (org-region-active-p)]
     "--"
     ["Set property" org-set-property (not (org-before-first-heading-p))]
     ["Column view of properties" org-columns t]
     ["Insert Column View DBlock" org-columns-insert-dblock t])
    ("Dates and Scheduling"
     ["Timestamp" org-timestamp (not (org-before-first-heading-p))]
     ["Timestamp (inactive)" org-timestamp-inactive (not (org-before-first-heading-p))]
     ("Change Date"
      ["1 Day Later" org-shiftright (org-at-timestamp-p 'lax)]
      ["1 Day Earlier" org-shiftleft (org-at-timestamp-p 'lax)]
      ["1 ... Later" org-shiftup (org-at-timestamp-p 'lax)]
      ["1 ... Earlier" org-shiftdown (org-at-timestamp-p 'lax)])
     ["Compute Time Range" org-evaluate-time-range t]
     ["Schedule Item" org-schedule (not (org-before-first-heading-p))]
     ["Deadline" org-deadline (not (org-before-first-heading-p))]
     "--"
     ["Custom time format" org-toggle-timestamp-overlays
      :style radio :selected org-display-custom-times]
     "--"
     ["Goto Calendar" org-goto-calendar t]
     ["Date from Calendar" org-date-from-calendar t]
     "--"
     ["Start/Restart Timer" org-timer-start t]
     ["Pause/Continue Timer" org-timer-pause-or-continue t]
     ["Stop Timer" org-timer-pause-or-continue :active t :keys "C-u C-c C-x ,"]
     ["Insert Timer String" org-timer t]
     ["Insert Timer Item" org-timer-item t])
    ("Logging work"
     ["Clock in" org-clock-in :active t :keys "C-c C-x C-i"]
     ["Switch task" (lambda () (interactive) (org-clock-in '(4))) :active t :keys "C-u C-c C-x C-i"]
     ["Clock out" org-clock-out t]
     ["Clock cancel" org-clock-cancel t]
     "--"
     ["Mark as default task" org-clock-mark-default-task t]
     ["Clock in, mark as default" (lambda () (interactive) (org-clock-in '(16))) :active t :keys "C-u C-u C-c C-x C-i"]
     ["Goto running clock" org-clock-goto t]
     "--"
     ["Display times" org-clock-display t]
     ["Create clock table" org-clock-report t]
     "--"
     ["Record DONE time"
      (progn (setq org-log-done (not org-log-done))
	     (message "Switching to %s will %s record a timestamp"
		      (car org-done-keywords)
		      (if org-log-done "automatically" "not")))
      :style toggle :selected org-log-done])
    "--"
    ["Agenda Command..." org-agenda t]
    ["Set Restriction Lock" org-agenda-set-restriction-lock t]
    ("File List for Agenda")
    ("Special views current file"
     ["TODO Tree"  org-show-todo-tree t]
     ["Check Deadlines" org-check-deadlines t]
     ["Tags/Property tree" org-match-sparse-tree t])
    "--"
    ["Export/Publish..." org-export-dispatch t]
    ("LaTeX"
     ["Org CDLaTeX mode" org-cdlatex-mode :active (require 'cdlatex nil t)
      :style toggle :selected org-cdlatex-mode]
     ["Insert Environment" cdlatex-environment (fboundp 'cdlatex-environment)]
     ["Insert math symbol" cdlatex-math-symbol (fboundp 'cdlatex-math-symbol)]
     ["Modify math symbol" org-cdlatex-math-modify
      (org-inside-LaTeX-fragment-p)]
     ["Insert citation" org-reftex-citation t])
    "--"
    ("Documentation"
     ["Show Version" org-version t]
     ["Info Documentation" org-info t]
     ["Browse Org News" org-browse-news t])
    ("Customize"
     ["Browse Org Group" org-customize t]
     "--"
     ["Expand This Menu" org-create-customize-menu t])
    ["Send bug report" org-submit-bug-report t]
    "--"
    ("Refresh/Reload"
     ["Refresh setup current buffer" org-mode-restart t]
     ["Reload Org (after update)" org-reload t]
     ["Reload Org uncompiled" (org-reload t) :active t :keys "C-u C-c C-x !"])))

(easy-menu-define org-tbl-menu org-mode-map "Org Table menu."
  '("Table"
    ["Align" org-ctrl-c-ctrl-c :active (org-at-table-p)]
    ["Next Field" org-cycle (org-at-table-p)]
    ["Previous Field" org-shifttab (org-at-table-p)]
    ["Next Row" org-return (org-at-table-p)]
    "--"
    ["Blank Field" org-table-blank-field (org-at-table-p)]
    ["Edit Field" org-table-edit-field (org-at-table-p)]
    ["Copy Field from Above" org-table-copy-down (org-at-table-p)]
    "--"
    ("Column"
     ["Move Column Left" org-metaleft (org-at-table-p)]
     ["Move Column Right" org-metaright (org-at-table-p)]
     ["Delete Column" org-shiftmetaleft (org-at-table-p)]
     ["Insert Column" org-shiftmetaright (org-at-table-p)]
     ["Shrink Column" org-table-toggle-column-width (org-at-table-p)])
    ("Row"
     ["Move Row Up" org-metaup (org-at-table-p)]
     ["Move Row Down" org-metadown (org-at-table-p)]
     ["Delete Row" org-shiftmetaup (org-at-table-p)]
     ["Insert Row" org-shiftmetadown (org-at-table-p)]
     ["Sort lines in region" org-table-sort-lines (org-at-table-p)]
     "--"
     ["Insert Hline" org-ctrl-c-minus (org-at-table-p)])
    ("Rectangle"
     ["Copy Rectangle" org-copy-special (org-at-table-p)]
     ["Cut Rectangle" org-cut-special (org-at-table-p)]
     ["Paste Rectangle" org-paste-special (org-at-table-p)]
     ["Fill Rectangle" org-table-wrap-region (org-at-table-p)])
    "--"
    ("Calculate"
     ["Set Column Formula" org-table-eval-formula (org-at-table-p)]
     ["Set Field Formula" (org-table-eval-formula '(4)) :active (org-at-table-p) :keys "C-u C-c ="]
     ["Edit Formulas" org-edit-special (org-at-table-p)]
     "--"
     ["Recalculate line" org-table-recalculate (org-at-table-p)]
     ["Recalculate all" (lambda () (interactive) (org-table-recalculate '(4))) :active (org-at-table-p) :keys "C-u C-c *"]
     ["Iterate all" (lambda () (interactive) (org-table-recalculate '(16))) :active (org-at-table-p) :keys "C-u C-u C-c *"]
     "--"
     ["Toggle Recalculate Mark" org-table-rotate-recalc-marks (org-at-table-p)]
     "--"
     ["Sum Column/Rectangle" org-table-sum
      (or (org-at-table-p) (org-region-active-p))]
     ["Which Column?" org-table-current-column (org-at-table-p)])
    ["Debug Formulas"
     org-table-toggle-formula-debugger
     :style toggle :selected (bound-and-true-p org-table-formula-debug)]
    ["Show Col/Row Numbers"
     org-table-toggle-coordinate-overlays
     :style toggle
     :selected (bound-and-true-p org-table-overlay-coordinates)]
    "--"
    ["Create" org-table-create (not (org-at-table-p))]
    ["Convert Region" org-table-convert-region (not (org-at-table-p 'any))]
    ["Import from File" org-table-import (not (org-at-table-p))]
    ["Export to File" org-table-export (org-at-table-p)]
    "--"
    ["Create/Convert from/to table.el" org-table-create-with-table.el t]
    "--"
    ("Plot"
     ["Ascii plot" orgtbl-ascii-plot :active (org-at-table-p) :keys "C-c \" a"]
     ["Gnuplot" org-plot/gnuplot :active (org-at-table-p) :keys "C-c \" g"])))

(defun org-info (&optional node)
  "Read documentation for Org in the info system.
With optional NODE, go directly to that node."
  (interactive)
  (info (format "(org)%s" (or node ""))))

(defun org-browse-news ()
  "Browse the news for the latest major release."
  (interactive)
  (browse-url "https://orgmode.org/Changes.html"))

(defvar org--warnings nil
  "List of warnings to be added to the bug reports.")
;;;###autoload
(defun org-submit-bug-report ()
  "Submit a bug report on Org via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your Org version and configuration."
  (interactive)
  (require 'reporter)
  (defvar reporter-prompt-for-summary-p)
  (org-load-modules-maybe)
  (org-require-autoloaded-modules)
  (let ((reporter-prompt-for-summary-p "Bug report subject: "))
    (reporter-submit-bug-report
     "emacs-orgmode@gnu.org"
     (org-version nil 'full)
     (let (list)
       (save-window-excursion
	 (pop-to-buffer
          (get-buffer-create "*Warn about privacy*")
          '(org-display-buffer-full-frame))
	 (erase-buffer)
	 (insert "You are about to submit a bug report to the Org mailing list.

If your report is about Org installation, please read this section:
https://orgmode.org/org.html#Installation

Please read https://orgmode.org/org.html#Feedback on how to make
a good report, it will help Org contributors fixing your problem.

Search https://lists.gnu.org/archive/html/emacs-orgmode/ to see
if the issue you are about to raise has already been dealt with.

We also would like to add your full Org and Outline configuration
to the bug report.  It will help us debugging the issue.

*HOWEVER*, some variables you have customized may contain private
information.  The names of customers, colleagues, or friends, might
appear in the form of file names, tags, todo states or search strings.
If you answer \"yes\" to the prompt, you might want to check and remove
such private information before sending the email.")
	 (add-text-properties (point-min) (point-max) '(face org-warning))
         (when (yes-or-no-p "Include your Org configuration and Org warning log?")
	   (mapatoms
	    (lambda (v)
	      (and (boundp v)
		   (string-match "\\`\\(org-\\|outline-\\)" (symbol-name v))
		   (or (and (symbol-value v)
			    (string-match "\\(-hook\\|-function\\)\\'" (symbol-name v)))
                       (eq v 'org--warnings)
		       (and
			(get v 'custom-type) (get v 'standard-value)
			(not (equal (symbol-value v)
			          (eval (car (get v 'standard-value)) t)))))
		   (push v list)))))
	 (kill-buffer (get-buffer "*Warn about privacy*"))
	 list))
     nil nil
     "Remember to cover the basics, that is, what you expected to happen and
what in fact did happen.  You don't know how to make a good report?  See

     https://orgmode.org/manual/Feedback.html#Feedback

Your bug report will be posted to the Org mailing list.
------------------------------------------------------------------------")
    (save-excursion
      (when (re-search-backward "^\\(Subject: \\)Org mode version \\(.*?\\);[ \t]*\\(.*\\)" nil t)
	(replace-match "\\1[BUG] \\3 [\\2]")))))

;;;; Documentation

(defun org-require-autoloaded-modules ()
  (interactive)
  (mapc #'require
	'(org-agenda org-archive org-attach org-clock org-colview org-id
		     org-table org-timer)))

;;;###autoload
(defun org-reload (&optional uncompiled)
  "Reload all Org Lisp files.
With prefix arg UNCOMPILED, load the uncompiled versions."
  (interactive "P")
  (require 'loadhist)
  (let* ((org-dir     (org-find-library-dir "org"))
	 (contrib-dir (or (org-find-library-dir "org-contribdir") org-dir))
	 (feature-re "^\\(org\\|ob\\|ox\\|ol\\|oc\\)\\(-.*\\)?")
	 (remove-re (format "\\`%s\\'"
			    (regexp-opt '("org" "org-loaddefs" "org-version"))))
	 (feats (delete-dups
		 (mapcar 'file-name-sans-extension
			 (mapcar 'file-name-nondirectory
				 (delq nil
				       (mapcar 'feature-file
					       features))))))
	 (lfeat (append
		 (sort
		  (setq feats
			(delq nil (mapcar
				   (lambda (f)
				     (if (and (string-match feature-re f)
					      (not (string-match remove-re f)))
					 f nil))
				   feats)))
		  'string-lessp)
		 (list "org-version" "org")))
	 (load-suffixes (if uncompiled (reverse load-suffixes) load-suffixes))
	 load-uncore load-misses)
    (setq load-misses
	  (delq t
		(mapcar (lambda (f)
			  (or (org-load-noerror-mustsuffix (concat org-dir f))
			      (and (string= org-dir contrib-dir)
				   (org-load-noerror-mustsuffix (concat contrib-dir f)))
			      (and (org-load-noerror-mustsuffix (concat (org-find-library-dir f) f))
				   (push f load-uncore)
				   t)
			      f))
			lfeat)))
    (when load-uncore
      (message "The following feature%s found in load-path, please check if that's correct:\n%s"
	       (if (> (length load-uncore) 1) "s were" " was")
               (reverse load-uncore)))
    (if load-misses
	(message "Some error occurred while reloading Org feature%s\n%s\nPlease check *Messages*!\n%s"
		 (if (> (length load-misses) 1) "s" "") load-misses (org-version nil 'full))
      (message "Successfully reloaded Org\n%s" (org-version nil 'full)))))

;;;###autoload
(defun org-customize ()
  "Call the customize function with org as argument."
  (interactive)
  (org-load-modules-maybe)
  (org-require-autoloaded-modules)
  (customize-browse 'org))

(defun org-create-customize-menu ()
  "Create a full customization menu for Org mode, insert it into the menu."
  (interactive)
  (org-load-modules-maybe)
  (org-require-autoloaded-modules)
  (easy-menu-change
   '("Org") "Customize"
   `(["Browse Org group" org-customize t]
     "--"
     ,(customize-menu-create 'org)
     ["Set" Custom-set t]
     ["Save" Custom-save t]
     ["Reset to Current" Custom-reset-current t]
     ["Reset to Saved" Custom-reset-saved t]
     ["Reset to Standard Settings" Custom-reset-standard t]))
  (message "\"Org\"-menu now contains full customization menu"))

;;;; Miscellaneous stuff

;;; Generally useful functions

(defun org-quote-csv-field (s)
  "Quote field for inclusion in CSV material."
  (if (string-match "[\",]" s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")
    s))

(defun org-quote-vert (s)
  "Replace \"|\" with \"\\vert\"."
  (while (string-match "|" s)
    (setq s (replace-match "\\vert" t t s)))
  s)

(defun org-context ()
  "Return a list of contexts of the current cursor position.
If several contexts apply, all are returned.
Each context entry is a list with a symbol naming the context, and
two positions indicating start and end of the context.  Possible
contexts are:

:headline         anywhere in a headline
:headline-stars   on the leading stars in a headline
:todo-keyword     on a TODO keyword (including DONE) in a headline
:tags             on the TAGS in a headline
:priority         on the priority cookie in a headline
:item             on the first line of a plain list item
:item-bullet      on the bullet/number of a plain list item
:checkbox         on the checkbox in a plain list item
:table            in an Org table
:table-special    on a special filed in a table
:table-table      in a table.el table
:clocktable       in a clocktable
:src-block        in a source block
:link             on a hyperlink
:keyword          on a keyword: SCHEDULED, DEADLINE, CLOSE, COMMENT.
:latex-fragment   on a LaTeX fragment
:latex-preview    on a LaTeX fragment with overlaid preview image

This function expects the position to be visible because it uses font-lock
faces as a help to recognize the following contexts: :table-special, :link,
and :keyword."
  (let* ((f (get-text-property (point) 'face))
	 (faces (if (listp f) f (list f)))
	 (case-fold-search t)
	 (p (point)) clist o)
    ;; First the large context
    (cond
     ((org-at-heading-p)
      (push (list :headline (line-beginning-position)
                  (line-end-position))
            clist)
      (when (progn
	      (forward-line 0)
	      (looking-at org-todo-line-tags-regexp))
	(push (org-point-in-group p 1 :headline-stars) clist)
	(push (org-point-in-group p 2 :todo-keyword) clist)
	(push (org-point-in-group p 4 :tags) clist))
      (goto-char p)
      (skip-chars-backward "^[\n\r \t") (or (bobp) (backward-char 1))
      (when (looking-at "\\[#[A-Z0-9]\\]")
	(push (org-point-in-group p 0 :priority) clist)))

     ((org-at-item-p)
      (push (org-point-in-group p 2 :item-bullet) clist)
      (push (list :item (line-beginning-position)
		  (save-excursion (org-end-of-item) (point)))
	    clist)
      (and (org-at-item-checkbox-p)
	   (push (org-point-in-group p 0 :checkbox) clist)))

     ((org-at-table-p)
      (push (list :table (org-table-begin) (org-table-end)) clist)
      (when (memq 'org-formula faces)
	(push (list :table-special
		    (previous-single-property-change p 'face)
		    (next-single-property-change p 'face))
	      clist)))
     ((org-at-table-p 'any)
      (push (list :table-table) clist)))
    (goto-char p)

    (let ((case-fold-search t))
      ;; New the "medium" contexts: clocktables, source blocks
      (cond ((org-in-clocktable-p)
	     (push (list :clocktable
			 (and (or (looking-at "[ \t]*\\(#\\+BEGIN: clocktable\\)")
				  (re-search-backward "[ \t]*\\(#+BEGIN: clocktable\\)" nil t))
			      (match-beginning 1))
			 (and (re-search-forward "[ \t]*#\\+END:?" nil t)
			      (match-end 0)))
		   clist))
	    ((org-in-src-block-p)
	     (push (list :src-block
			 (and (or (looking-at "[ \t]*\\(#\\+BEGIN_SRC\\)")
				  (re-search-backward "[ \t]*\\(#+BEGIN_SRC\\)" nil t))
			      (match-beginning 1))
			 (and (search-forward "#+END_SRC" nil t)
			      (match-beginning 0)))
		   clist))))
    (goto-char p)

    ;; Now the small context
    (cond
     ((org-at-timestamp-p)
      (push (org-point-in-group p 0 :timestamp) clist))
     ((memq 'org-link faces)
      (push (list :link
		  (previous-single-property-change p 'face)
		  (next-single-property-change p 'face))
	    clist))
     ((memq 'org-special-keyword faces)
      (push (list :keyword
		  (previous-single-property-change p 'face)
		  (next-single-property-change p 'face))
	    clist))
     ((setq o (cl-some
	       (lambda (o)
		 (and (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
		      o))
	       (overlays-at (point))))
      (push (list :latex-fragment
		  (overlay-start o) (overlay-end o))
	    clist)
      (push (list :latex-preview
		  (overlay-start o) (overlay-end o))
	    clist))
     ((org-inside-LaTeX-fragment-p)
      ;; FIXME: positions wrong.
      (push (list :latex-fragment (point) (point)) clist)))

    (setq clist (nreverse (delq nil clist)))
    clist))

(defun org-between-regexps-p (start-re end-re &optional lim-up lim-down)
  "Non-nil when point is between matches of START-RE and END-RE.

Also return a non-nil value when point is on one of the matches.

Optional arguments LIM-UP and LIM-DOWN bound the search; they are
buffer positions.  Default values are the positions of headlines
surrounding the point.

The functions returns a cons cell whose car (resp. cdr) is the
position before START-RE (resp. after END-RE)."
  (save-match-data
    (let ((pos (point))
	  (limit-up (or lim-up (save-excursion (outline-previous-heading))))
	  (limit-down (or lim-down (save-excursion (outline-next-heading))))
	  beg end)
      (save-excursion
	;; Point is on a block when on START-RE or if START-RE can be
	;; found before it...
	(and (or (org-in-regexp start-re)
		 (re-search-backward start-re limit-up t))
	     (setq beg (match-beginning 0))
	     ;; ... and END-RE after it...
	     (goto-char (match-end 0))
	     (re-search-forward end-re limit-down t)
	     (> (setq end (match-end 0)) pos)
	     ;; ... without another START-RE in-between.
	     (goto-char (match-beginning 0))
	     (not (re-search-backward start-re (1+ beg) t))
	     ;; Return value.
	     (cons beg end))))))

;; Defined in org-agenda.el
(defvar org-agenda-restrict)
(defvar org-agenda-restrict-begin)
(defvar org-agenda-restrict-end)
(defun org-occur-in-agenda-files (regexp &optional _nlines)
  "Call `multi-occur' with buffers for all agenda files."
  (interactive "sOrg-files matching: ")
  (let* ((files (org-agenda-files))
	 (tnames (mapcar #'file-truename files))
	 (extra org-agenda-text-search-extra-files)
         (narrows nil))
    (when (and (eq (car extra) 'agenda-archives)
               (not org-agenda-restrict))
      (setq extra (cdr extra))
      (setq files (org-add-archive-files files)))
    (unless org-agenda-restrict
      (dolist (f extra)
        (unless (member (file-truename f) tnames)
	  (unless (member f files) (setq files (append files (list f))))
	  (setq tnames (append tnames (list (file-truename f)))))))
    (multi-occur
     (mapcar (lambda (x)
	       (with-current-buffer
		   ;; FIXME: Why not just (find-file-noselect x)?
		   ;; Is it to avoid the "revert buffer" prompt?
		   (or (get-file-buffer x) (find-file-noselect x))
                 (if (eq (current-buffer) org-agenda-restrict)
		     (progn
                       ;; Save the narrowing state.
                       (push (list (current-buffer) (point-min) (point-max))
                             narrows)
                       (widen)
                       (narrow-to-region org-agenda-restrict-begin
				         org-agenda-restrict-end))
		   (widen))
		 (current-buffer)))
	     files)
     regexp)
    ;; Restore the narrowing.
    (dolist (narrow narrows)
      (with-current-buffer (car narrow)
        (widen)
        (narrow-to-region (nth 1 narrow) (nth 2 narrow))))))

(add-hook 'occur-mode-find-occurrence-hook
	  (lambda () (when (derived-mode-p 'org-mode) (org-fold-reveal))))

(defun org-occur-link-in-agenda-files ()
  "Create a link and search for it in the agendas.
The link is not stored in `org-stored-links', it is just created
for the search purpose."
  (interactive)
  (let ((link (condition-case nil
		  (org-store-link nil)
		(error "Unable to create a link to here"))))
    (org-occur-in-agenda-files (regexp-quote link))))

(defun org-back-over-empty-lines ()
  "Move backwards over whitespace, to the beginning of the first empty line.
Returns the number of empty lines passed."
  (let ((pos (point)))
    (if (cdr (assq 'heading org-blank-before-new-entry))
	(skip-chars-backward " \t\n\r")
      (unless (eobp)
	(forward-line -1)))
    (forward-line 1)
    (goto-char (min (point) pos))
    (count-lines (point) pos)))

;;; TODO: Only called once, from ox-odt which should probably use
;;; org-export-inline-image-p or something.
(defun org-file-image-p (file)
  "Return non-nil if FILE is an image."
  (save-match-data
    (string-match (image-file-name-regexp) file)))

(defun org-get-cursor-date (&optional with-time)
  "Return the date at cursor in as a time.
This works in the calendar and in the agenda, anywhere else it just
returns the current time.
If WITH-TIME is non-nil, returns the time of the event at point (in
the agenda) or the current time of the day; otherwise returns the
earliest time on the cursor date that Org treats as that date
(bearing in mind `org-extend-today-until')."
  (let (date day defd tp hod mod)
    (when with-time
      (setq tp (get-text-property (point) 'time))
      (when (and tp (string-match "\\([0-2]?[0-9]\\):\\([0-5][0-9]\\)" tp))
	(setq hod (string-to-number (match-string 1 tp))
	      mod (string-to-number (match-string 2 tp))))
      (or tp (let ((now (decode-time)))
	       (setq hod (nth 2 now)
		     mod (nth 1 now)))))
    (cond
     ((eq major-mode 'calendar-mode)
      (setq date (calendar-cursor-to-date)
	    defd (org-encode-time 0 (or mod 0) (or hod org-extend-today-until)
                                  (nth 1 date) (nth 0 date) (nth 2 date))))
     ((eq major-mode 'org-agenda-mode)
      (setq day (get-text-property (point) 'day))
      (when day
	(setq date (calendar-gregorian-from-absolute day)
	      defd (org-encode-time 0 (or mod 0) (or hod org-extend-today-until)
                                    (nth 1 date) (nth 0 date) (nth 2 date))))))
    (or defd (current-time))))

(defun org-mark-subtree (&optional up)
  "Mark the current subtree.
This puts point at the start of the current subtree, and mark at
the end.  If a numeric prefix UP is given, move up into the
hierarchy of headlines by UP levels before marking the subtree."
  (interactive "P")
  (org-with-limited-levels
   (cond ((org-at-heading-p) (forward-line 0))
	 ((org-before-first-heading-p) (user-error "Not in a subtree"))
	 (t (outline-previous-visible-heading 1))))
  (when up (while (and (> up 0) (org-up-heading-safe)) (cl-decf up)))
  (if (called-interactively-p 'any)
      (call-interactively 'org-mark-element)
    (org-mark-element)))

;;; Fixed Width Areas

(defun org-toggle-fixed-width ()
  "Toggle fixed-width markup.

Add or remove fixed-width markup on current line, whenever it
makes sense.  Return an error otherwise.

If a region is active and if it contains only fixed-width areas
or blank lines, remove all fixed-width markup in it.  If the
region contains anything else, convert all non-fixed-width lines
to fixed-width ones.

Blank lines at the end of the region are ignored unless the
region only contains such lines."
  (interactive)
  (if (not (org-region-active-p))
      ;; No region:
      ;;
      ;; Remove fixed width marker only in a fixed-with element.
      ;;
      ;; Add fixed width maker in paragraphs, in blank lines after
      ;; elements or at the beginning of a headline or an inlinetask,
      ;; and before any one-line elements (e.g., a clock).
      (progn
        (forward-line 0)
        (let* ((element (org-element-at-point))
               (type (org-element-type element)))
          (cond
           ((and (eq type 'fixed-width)
                 (looking-at "[ \t]*\\(:\\(?: \\|$\\)\\)"))
            (replace-match
	     "" nil nil nil (if (= (line-end-position) (match-end 0)) 0 1)))
           ((and (memq type '(babel-call clock comment diary-sexp headline
					 horizontal-rule keyword paragraph
					 planning))
		 (<= (org-element-post-affiliated element) (point)))
            (skip-chars-forward " \t")
            (insert ": "))
           ((and (looking-at-p "[ \t]*$")
                 (or (eq type 'inlinetask)
                     (save-excursion
                       (skip-chars-forward " \r\t\n")
                       (<= (org-element-end element) (point)))))
            (delete-region (point) (line-end-position))
            (org-indent-line)
            (insert ": "))
           (t (user-error "Cannot insert a fixed-width line here")))))
    ;; Region active.
    (let* ((begin (save-excursion
                    (goto-char (region-beginning))
                    (line-beginning-position)))
           (end (copy-marker
                 (save-excursion
                   (goto-char (region-end))
                   (unless (eolp) (forward-line 0))
                   (if (save-excursion (re-search-backward "\\S-" begin t))
                       (progn (skip-chars-backward " \r\t\n") (point))
                     (point)))))
           (all-fixed-width-p
            (catch 'not-all-p
              (save-excursion
                (goto-char begin)
                (skip-chars-forward " \r\t\n")
                (when (eobp) (throw 'not-all-p nil))
                (while (< (point) end)
                  (let ((element (org-element-at-point)))
                    (if (org-element-type-p element 'fixed-width)
                        (goto-char (org-element-end element))
                      (throw 'not-all-p nil))))
                t))))
      (if all-fixed-width-p
          (save-excursion
            (goto-char begin)
            (while (< (point) end)
              (when (looking-at "[ \t]*\\(:\\(?: \\|$\\)\\)")
                (replace-match
                 "" nil nil nil
                 (if (= (line-end-position) (match-end 0)) 0 1)))
              (forward-line)))
        (let ((min-ind (point-max)))
          ;; Find minimum indentation across all lines.
          (save-excursion
            (goto-char begin)
            (if (not (save-excursion (re-search-forward "\\S-" end t)))
                (setq min-ind 0)
              (catch 'zerop
                (while (< (point) end)
                  (unless (looking-at-p "[ \t]*$")
                    (let ((ind (org-current-text-indentation)))
                      (setq min-ind (min min-ind ind))
                      (when (zerop ind) (throw 'zerop t))))
                  (forward-line)))))
          ;; Loop over all lines and add fixed-width markup everywhere
          ;; but in fixed-width lines.
          (save-excursion
            (goto-char begin)
            (while (< (point) end)
              (cond
               ((org-at-heading-p)
                (insert ": ")
                (forward-line)
                (while (and (< (point) end) (looking-at-p "[ \t]*$"))
                  (insert ":")
                  (forward-line)))
               ((looking-at-p "[ \t]*:\\( \\|$\\)")
                (let* ((element (org-element-at-point))
                       (element-end (org-element-end element)))
                  (if (org-element-type-p element 'fixed-width)
                      (progn (goto-char element-end)
                             (skip-chars-backward " \r\t\n")
                             (forward-line))
                    (let ((limit (min end element-end)))
                      (while (< (point) limit)
                        (org-move-to-column min-ind t)
                        (insert ": ")
                        (forward-line))))))
               (t
                (org-move-to-column min-ind t)
                (insert ": ")
                (forward-line)))))))
      (set-marker end nil))))


;;; Other stuff

(defvar reftex-docstruct-symbol)
(defvar org--rds)

(defun org-reftex-citation ()
  "Use `reftex-citation' to insert a citation into the buffer.
This looks for a line like

#+BIBLIOGRAPHY: foo plain option:-d

and derives from it that foo.bib is the bibliography file relevant
for this document.  It then installs the necessary environment for RefTeX
to work in this buffer and calls `reftex-citation'  to insert a citation
into the buffer.

Export of such citations to both LaTeX and HTML is handled by the contributed
package ox-bibtex by Taru Karttunen."
  (interactive)
  (let ((reftex-docstruct-symbol 'org--rds)
	org--rds bib)
    (org-with-wide-buffer
     (let ((case-fold-search t)
	   (re "^[ \t]*#\\+BIBLIOGRAPHY:[ \t]+\\([^ \t\n]+\\)"))
       (if (not (save-excursion
		  (or (re-search-forward re nil t)
		      (re-search-backward re nil t))))
	   (user-error "No bibliography defined in file")
	 (setq bib (concat (match-string 1) ".bib")
	       org--rds (list (list 'bib bib))))))
    (call-interactively 'reftex-citation)))

;;;; Functions extending outline functionality

(require 'org-move)

(defun org-mark-element ()
  "Put point at beginning of this element, mark at end.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next element after the
ones already marked."
  (interactive)
  (let (deactivate-mark)
    (if (and (called-interactively-p 'any)
	     (or (and (eq last-command this-command) (mark t))
		 (and transient-mark-mode mark-active)))
	(set-mark
	 (save-excursion
	   (goto-char (mark))
	   (goto-char (org-element-end (org-element-at-point)))
	   (point)))
      (let ((element (org-element-at-point)))
	(end-of-line)
	(push-mark (min (point-max) (org-element-end element)) t t)
	(goto-char (org-element-begin element))))))

(defun org-narrow-to-subtree (&optional element)
  "Narrow buffer to the current subtree.
Use the command `\\[widen]' to see the whole buffer again.
With optional argument ELEMENT narrow to subtree around ELEMENT."
  (interactive)
  (let* ((heading
          (org-element-lineage
           (or element (org-element-at-point))
           'headline 'with-self))
         (begin (org-element-begin heading))
         (end (org-element-end heading)))
    (if (and heading end
             ;; Preserve historical behavior throwing an error when
             ;; current heading starts before active narrowing.
             (<= (point-min) begin))
        (narrow-to-region
         begin
         ;; Preserve historical behavior not extending the active
         ;; narrowing when the subtree extends beyond it.
         (min (point-max)
              (if (= end (point-max))
                  end (1- end))))
      (signal 'outline-before-first-heading nil))))

(defun org-toggle-narrow-to-subtree ()
  "Narrow to the subtree at point or widen a narrowed buffer.
Use the command `\\[widen]' to see the whole buffer again."
  (interactive)
  (if (buffer-narrowed-p)
      (progn (widen) (message "Buffer widen"))
    (org-narrow-to-subtree)
    (message "Buffer narrowed to current subtree")))

(defun org-narrow-to-block ()
  "Narrow buffer to the current block.
Use the command `\\[widen]' to see the whole buffer again."
  (interactive)
  (let* ((case-fold-search t)
         (element (org-element-at-point)))
    (if (string-match-p "block" (symbol-name (org-element-type element)))
        (org-narrow-to-element)
      (user-error "Not in a block"))))

(defun org-narrow-to-element ()
  "Narrow buffer to current element.
Use the command `\\[widen]' to see the whole buffer again."
  (interactive)
  (let ((elem (org-element-at-point)))
    (cond
     ((eq (car elem) 'headline)
      (narrow-to-region
       (org-element-begin elem)
       (org-element-end elem)))
     ((memq (car elem) org-element-greater-elements)
      (narrow-to-region
       (org-element-contents-begin elem)
       (org-element-contents-end elem)))
     (t
      (narrow-to-region
       (org-element-begin elem)
       (org-element-end elem))))))

;;; Conveniently switch to Info nodes

(defun org-info-find-node (&optional nodename)
  "Find Info documentation NODENAME or Org documentation according context.
Started from `gnus-info-find-node'."
  (interactive)
  (Info-goto-node
   (or nodename
       (let ((default-org-info-node "(org) Top"))
         (cond
          ((eq 'org-agenda-mode major-mode) "(org) Agenda Views")
          ((eq 'org-mode major-mode)
           (let* ((context (org-element-at-point))
                  (element-info-nodes ; compare to `org-element-all-elements'.
                   `((babel-call . "(org) Evaluating Code Blocks")
                     (center-block . "(org) Paragraphs")
                     (clock . ,default-org-info-node)
                     (comment . "(org) Comment Lines")
                     (comment-block . "(org) Comment Lines")
                     (diary-sexp . ,default-org-info-node)
                     (drawer . "(org) Drawers")
                     (dynamic-block . "(org) Dynamic Blocks")
                     (example-block . "(org) Literal Examples")
                     (export-block . "(org) ASCII/Latin-1/UTF-8 export")
                     (fixed-width . ,default-org-info-node)
                     (footnote-definition . "(org) Creating Footnotes")
                     (headline . "(org) Document Structure")
                     (horizontal-rule . "(org) Built-in Table Editor")
                     (inlinetask . ,default-org-info-node)
                     (item . "(org) Plain Lists")
                     (keyword . "(org) Per-file keywords")
                     (latex-environment . "(org) LaTeX Export")
                     (node-property . "(org) Properties and Columns")
                     (paragraph . "(org) Paragraphs")
                     (plain-list . "(org) Plain Lists")
                     (planning . "(org) Deadlines and Scheduling")
                     (property-drawer . "(org) Properties and Columns")
                     (quote-block . "(org) Paragraphs")
                     (section . ,default-org-info-node)
                     (special-block . ,default-org-info-node)
                     (src-block . "(org) Working with Source Code")
                     (table . "(org) Tables")
                     (table-row . "(org) Tables")
                     (verse-block . "(org) Paragraphs"))))
             (or (cdr (assoc (car context) element-info-nodes))
                 default-org-info-node)))
          (t default-org-info-node))))))

(defun org-get-previous-line-level ()
  "Return the outline depth of the last headline before the current line.
Returns 0 for the first headline in the buffer, and nil if before the
first headline."
  (and (org-current-level)
       (or (and (/= (line-beginning-position) (point-min))
		(save-excursion (forward-line -1) (org-current-level)))
	   0)))

(defun org-level-increment ()
  "Return the number of stars that will be added or removed at a
time to headlines when structure editing, based on the value of
`org-odd-levels-only'."
  (if org-odd-levels-only 2 1))

(defun org-get-valid-level (level &optional change)
  "Rectify a level change under the influence of `org-odd-levels-only'.
LEVEL is a current level, CHANGE is by how much the level should
be modified.  Even if CHANGE is nil, LEVEL may be returned
modified because even level numbers will become the next higher
odd number.  Returns values greater than 0."
  (if org-odd-levels-only
      (cond ((or (not change) (= 0 change)) (1+ (* 2 (/ level 2))))
	    ((> change 0) (1+ (* 2 (/ (+ (1- level) (* 2 change)) 2))))
	    ((< change 0) (max 1 (1+ (* 2 (/ (+ level (* 2 change)) 2))))))
    (max 1 (+ level (or change 0)))))

(defun org-tr-level (n)
  "Make N odd if required."
  (if org-odd-levels-only (1+ (/ n 2)) n))

;;; Outline path

(defun org-format-outline-path (path &optional width prefix separator)
  "Format the outline path PATH for display.
WIDTH is the maximum number of characters that is available.
PREFIX is a prefix to be included in the returned string,
such as the file name.
SEPARATOR is inserted between the different parts of the path,
the default is \"/\"."
  (setq width (or width 79))
  (setq path (delq nil path))
  (unless (> width 0)
    (user-error "Argument `width' must be positive"))
  (setq separator (or separator "/"))
  (let* ((org-odd-levels-only nil)
	 (fpath (concat
		 prefix (and prefix path separator)
		 (mapconcat
		  (lambda (s) (replace-regexp-in-string "[ \t]+\\'" "" s))
		  (cl-loop for head in path
			   for n from 0
			   collect (org-add-props
				       head nil 'face
				       (nth (% n org-n-level-faces) org-level-faces)))
		  separator))))
    (when (> (length fpath) width)
      (if (< width 7)
	  ;; It's unlikely that `width' will be this small, but don't
	  ;; waste characters by adding ".." if it is.
	  (setq fpath (substring fpath 0 width))
	(setf (substring fpath (- width 2)) "..")))
    fpath))

(defun org-display-outline-path (&optional file-or-title current separator just-return-string)
  "Display the current outline path in the echo area.

If FILE-OR-TITLE is `title', prepend outline with file title.  If
it is non-nil or title is not present in document, prepend
outline path with the file name.
If CURRENT is non-nil, append the current heading to the output.
SEPARATOR is passed through to `org-format-outline-path'.  It separates
the different parts of the path and defaults to \"/\".
If JUST-RETURN-STRING is non-nil, return a string, don't display a message."
  (interactive "P")
  (let* (case-fold-search
	 (bfn (buffer-file-name (buffer-base-buffer)))
         (title-prop (when (eq file-or-title 'title) (org-get-title)))
	 (path (and (derived-mode-p 'org-mode) (org-get-outline-path)))
	 res)
    (when current (setq path (append path
				     (save-excursion
				       (org-back-to-heading t)
				       (when (looking-at org-complex-heading-regexp)
					 (list (match-string 4)))))))
    (setq res
	  (org-format-outline-path
	   path
	   (1- (frame-width))
	   (and file-or-title bfn (concat (if (and (eq file-or-title 'title) title-prop)
					      title-prop
					    (file-name-nondirectory bfn))
				          separator))
	   separator))
    (add-face-text-property 0 (length res)
			    `(:height ,(face-attribute 'default :height))
			    nil res)
    (if just-return-string
	res
      (org-unlogged-message "%s" res))))



(provide 'org)

(run-hooks 'org-load-hook)

;;; org.el ends here
