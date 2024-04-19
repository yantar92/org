;;; org-load.el --- Org mode package loading                      -*- lexical-binding: t; -*-

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

;; This library contains code responsible for loading Org mode package
;; as a whole.

;;; Code:

(defcustom org-load-hook nil
  "Hook that is run after org.el has been loaded."
  :group 'org
  :type 'hook)

(make-obsolete-variable
 'org-load-hook
 "use `with-eval-after-load' instead." "9.5")

(condition-case nil
    (load (concat (file-name-directory load-file-name)
		  "org-loaddefs")
	  nil t nil t)
  (error
   (message "WARNING: No org-loaddefs.el file could be found from where org.el is loaded.")
   (sit-for 3)
   (message "You need to run \"make\" or \"make autoloads\" from Org lisp directory")
   (sit-for 3)))



;;; Org mode version

(require 'org-macs)

;;;###autoload
(defmacro org-check-version ()
  "Try very hard to provide sensible version strings."
  (let* ((org-dir        (org-find-library-dir "org"))
         (org-version.el (concat org-dir "org-version.el"))
         (org-fixup.el   (concat org-dir "../mk/org-fixup.el")))
    (if (require 'org-version org-version.el 'noerror)
        '(progn
           (autoload 'org-release     "org-version.el")
           (autoload 'org-git-version "org-version.el"))
      (if (require 'org-fixup org-fixup.el 'noerror)
          '(org-fixup)
        ;; provide fallback definitions and complain
        (warn "Could not define org version correctly.  Check installation!")
        '(progn
           (defun org-release () "N/A")
           (defun org-git-version () "N/A !!check installation!!"))))))

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


;;; Pre-load selected Org modules

(defun org-require-autoloaded-modules ()
  (interactive)
  (mapc #'require
	'(org-agenda org-archive org-attach org-clock org-colview org-id
		     org-table org-timer)))

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
  (when (and (featurep 'org-load)
             (fboundp 'org-load-modules-maybe))
    (org-load-modules-maybe 'force)
    (when (and (featurep 'org-element)
               (fboundp 'org-element-cache-reset))
      (org-element-cache-reset 'all))))

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


;;; Pre-load selected babel backends

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



;;; Pre-load selected export backends

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



;;; Reload Org mode

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

(eval-after-load 'calendar '(require 'org-calendar))
(eval-after-load 'flyspell '(require 'org-flyspell))
(eval-after-load 'speedbar '(require 'org-speedbar))
(eval-after-load 'ecb '(require 'org-ecb))

(provide 'org-load)

(run-hooks 'org-load-hook)

;;; org-load.el ends here
