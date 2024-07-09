;;; org-agenda-tags-view.el --- Org tag view  -*- lexical-binding: t; -*-

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

;; This file implements searching tagged entries in agenda.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-agenda-mode)
(require 'org-agenda-search)

(defun org-agenda--tags-block-header (match)
  "Produce block header for tags block displaying tags MATCH."
  (with-temp-buffer
    (let (pos)
      (insert "Headlines with TAGS match: ")
      (add-text-properties (point-min) (1- (point))
			   (list 'face 'org-agenda-structure
			         'short-heading
			         (concat "Match: " match)))
      (setq pos (point))
      (insert match "\n")
      (add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure-filter))
      (setq pos (point))
      (unless org-agenda-multi
        (insert (substitute-command-keys
	         "Press \
\\<org-agenda-mode-map>`\\[universal-argument] \\[org-agenda-redo]' \
to search again\n")))
      (add-text-properties pos (1- (point))
			   (list 'face 'org-agenda-structure-secondary))
      (buffer-string))))

;;;###autoload
(defun org-tags-view (&optional todo-only match)
  "Show all headlines for all `org-agenda-files' matching a TAGS criterion.
The prefix arg TODO-ONLY limits the search to TODO entries."
  (interactive "P")
  (when org-agenda-overriding-arguments
    (setq todo-only (car org-agenda-overriding-arguments)
	  match (nth 1 org-agenda-overriding-arguments)))
  (let* ((org-tags-match-list-sublevels
	  org-tags-match-list-sublevels)
	 (completion-ignore-case t)
	 (org--matcher-tags-todo-only todo-only)
	 rtn rtnall files file matcher buffer)

    (setq matcher (org-make-tags-matcher match))
    (setq match (car matcher)
	  matcher (cdr matcher))
    (setq org-agenda-query-string match)

    (org-agenda-insert-block
     'tags
     (lambda ()
       (setq files (org-agenda-files nil 'ifmode)
	     rtnall nil)
       (while (setq file (pop files))
         (catch 'nextfile
	   (org-check-agenda-file file)
	   (setq buffer (if (file-exists-p file)
			    (org-get-agenda-file-buffer file)
			  (error "No such file %s" file)))
	   (if (not buffer)
	       ;; If file does not exist, error message to agenda
	       (setq rtn (list
			  (format "ORG-AGENDA-ERROR: No such org-file %s" file))
		     rtnall (append rtnall rtn))
	     (with-current-buffer buffer
	       (unless (derived-mode-p 'org-mode)
	         (error "Agenda file %s is not in Org mode" file))
	       (save-excursion
	         (save-restriction
		   (if (eq buffer org-agenda-restrict)
		       (narrow-to-region org-agenda-restrict-begin
				         org-agenda-restrict-end)
		     (widen))
		   (setq rtn (org-agenda-get-tags
			      matcher org--matcher-tags-todo-only))
		   (setq rtnall (append rtnall rtn))))))))
       (when rtnall
         (insert (org-agenda-finalize-entries rtnall 'tags) "\n")))
     :suggested-buffer-name (cons (if todo-only "tags-todo" "tags") match)
     :block-header (org-agenda--tags-block-header match)
     :redo-command
     (list 'org-tags-view
	   `(quote ,org--matcher-tags-todo-only)
	   `(if current-prefix-arg nil ,org-agenda-query-string))
     :block-args (list org--matcher-tags-todo-only match))))

(provide 'org-agenda-tags-view)

;;; org-agenda-tags-view.el ends here
