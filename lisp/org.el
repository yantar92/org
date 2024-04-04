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
(require 'org-mark)
(require 'org-narrow)
(require 'org-preview-latex)
(require 'org-preview-image)
(require 'org-indirect-buffer)

(require 'org-cycle)

(declare-function calendar-check-holidays "holidays" (date))
(declare-function Info-goto-node "info" (nodename &optional fork strict-case))
(declare-function org-add-archive-files "org-archive" (files))
(declare-function org-agenda-list "org-agenda" (&optional arg start-day span with-hour))
(declare-function org-attach "org-attach" ())
(declare-function org-attach-dir "org-attach"
		  (&optional create-if-not-exists-p no-fs-check))
(declare-function org-babel-tangle-file "ob-tangle" (file &optional target-file lang))
(declare-function org-clock-update-time-maybe "org-clock" ())
(declare-function org-tags-view "org-agenda" (&optional todo-only match))

(defvar org-agenda-buffer-name)

;; load languages based on value of `org-babel-load-languages'
(defvar org-babel-load-languages)

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

(defun org-require-autoloaded-modules ()
  (interactive)
  (mapc #'require
	'(org-agenda org-archive org-attach org-clock org-colview org-id
		     org-table org-timer)))

(defgroup org-structure nil
  "Options concerning the general structure of Org files."
  :tag "Org Structure"
  :group 'org)

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

(defvar org-todo-keywords-for-agenda nil)
(defvar org-done-keywords-for-agenda nil)
(defvar org-todo-keyword-alist-for-agenda nil)
(defvar org-tag-alist-for-agenda nil
  "Alist of all tags from all agenda files.")
(defvar org-tag-groups-alist-for-agenda nil
  "Alist of all groups tags from all current agenda files.")
(defvar org-agenda-contributing-files nil)
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

(defvar calc-embedded-close-formula)
(defvar calc-embedded-open-formula)
(defvar calc-embedded-open-mode)
(defvar font-lock-unfontify-region-function)
(defvar org-agenda-tags-todo-honor-ignore-options)
(defvar remember-data-file)

(declare-function org-clock-save-markers-for-cut-and-paste "org-clock" (beg end))
(declare-function org-clock-update-mode-line "org-clock" (&optional refresh))
(declare-function org-resolve-clocks "org-clock"
		  (&optional also-non-dangling-p prompt last-valid))

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
;;;; Define the Org mode

(defun org-before-change-function (_beg _end)
  "Every change indicates that a table might need an update."
  (setq org-table-may-need-update t))
(defvar org-mode-map)
(defvar org-agenda-keep-modes nil)      ; Dynamically-scoped param.
(defvar bidi-paragraph-direction)

(require 'outline)

;; Other stuff we need.
(require 'time-date)

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

;;; Menu entries
(defvar org--warnings nil
  "List of warnings to be added to the bug reports.")
;;;###autoload
;;;; Documentation

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
