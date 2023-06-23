;;; org-load.el --- Functions and commands that load Org mode         -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 Free Software Foundation, Inc.

;; Author: Ihor Radchenko <yantar92 at gmail dot com>

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
;;
;; This file contains commands and functions dealing with loading
;; various parts of Org and checking consistency of the Org libraries.

(require 'org-macs)

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

;;; Org version verification.

(defvar org--inhibit-version-check nil
  "When non-nil, skip the detection of mixed-versions situations.
For internal use only.  See Emacs bug #62762.
This variable is only supposed to be changed by Emacs build scripts.
When nil, Org tries to detect when Org source files were compiled with
a different version of Org (which tends to lead to incorrect `.elc' files),
or when the current Emacs session has loaded a mix of files from different
Org versions (typically the one bundled with Emacs and another one installed
from GNU ELPA), which can happen if some parts of Org were loaded before
`load-path' was changed (e.g. before the GNU-ELPA-installed Org is activated
by `package-activate-all').")
(defmacro org-assert-version ()
  "Check compile time and runtime version match.
Warn, and attempt to fix mixed version, if any."
  ;; We intentionally use a more permissive `org-release' instead of
  ;; `org-git-version' to work around deficiencies in Elisp
  ;; compilation after pulling latest changes.  Unchanged files will
  ;; not be re-compiled and thus their macro-expanded
  ;; `org-assert-version' calls would fail using strict
  ;; `org-git-version' check because the generated Org version strings
  ;; will not match.
  `(unless (or org--inhibit-version-check (equal (org-release) ,(org-release)))
     (warn "Org version mismatch.  Random Org errors may occur.
This warning usually appears when a built-in Org version is loaded
prior to the more recent Org version.

Version mismatch is commonly encountered in the following situations:

1. Some compiled Org libraries are not re-compiled after Org update.

   It is recommended to re-install Org, or, if Org is loaded from git
   repo, to run

      make clean; make autoloads

2. Emacs is loaded using literate Org config and more recent Org
   version is loaded inside the file loaded by `org-babel-load-file'.
   `org-babel-load-file' triggers the built-in Org version clashing
   the newer Org version attempt to be loaded later.

   It is recommended to move the Org loading code before the
   `org-babel-load-file' call.

3. New Org version is loaded manually by setting `load-path', but some
   other package depending on Org is loaded before the `load-path' is
   configured.
   This \"other package\" is triggering built-in Org version, again
   causing the version mismatch.

   It is recommended to set `load-path' as early in the config as
   possible.

4. New Org version is loaded using straight.el package manager and
   other package depending on Org is loaded before straight triggers
   loading of the newer Org version.

   It is recommended to put

    %s

   early in the config.  Ideally, right after the straight.el
   bootstrap.  Moving `use-package' :straight declaration may not be
   sufficient if the corresponding `use-package' statement is
   deferring the loading."
           ;; Avoid `warn' replacing "'" with "’" (see `format-message').
           "(straight-use-package 'org)")
     ;; Try to re-load Org.
     ;; Do the re-loading after the current, faulty file is loaded.
     ;; The current file may still fail to load - situation we cannot
     ;; work around.
     ;; We re-load the whole Org here because Org commonly uses
     ;; `declare-function' statements and thus more granular approach,
     ;; like suggested in
     ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=62762#110
     ;; will not be as reliable.
     ;; Note that this clause will never run during compilation
     ;; because version check can only trigger in already compiled
     ;; files with macro expanded differently during compilation and
     ;; during loading.  (equal (org-release) ,(org-release)) is
     ;; always t when compiling current `load-file-name'.
     (eval-after-load load-file-name (org-reload t))))

(provide 'org-load)

;;; org-load.el ends here
