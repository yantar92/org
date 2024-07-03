;;; org-agenda-todo-view.el --- Org task view  -*- lexical-binding: t; -*-

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

;; This file implements listing TODO entries in agenda.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-agenda-mode)
(require 'org-agenda-search)

(defvar crm-separator)

;;;###autoload
(defun org-todo-list (&optional arg)
  "Show all (not done) TODO entries from all agenda files in a single list.
The prefix arg can be used to select a specific TODO keyword and limit
the list to these.  When using `\\[universal-argument]', you will be prompted
for a keyword.  A numeric prefix directly selects the Nth keyword in
all the todo keywords in buffer (`org-element-all-todo-keywords')."
  (interactive "P")
  (when org-agenda-overriding-arguments
    (setq arg org-agenda-overriding-arguments))
  (when (and (stringp arg) (not (string-match "\\S-" arg)))
    (setq arg nil))
  (let* ((today (calendar-gregorian-from-absolute (org-today)))
	 (completion-ignore-case t)
         todo-keywords org-select-this-todo-keyword todo-entries all-todo-entries files file pos)
    (catch 'exit
      (setq org-agenda-buffer-name
	    (org-agenda--get-buffer-name
	     (when org-agenda-sticky
	       (if (stringp org-select-this-todo-keyword)
		   (format "*Org Agenda(%s:%s)*" (or org-keys "t")
			   org-select-this-todo-keyword)
		 (format "*Org Agenda(%s)*" (or org-keys "t"))))))
      (org-agenda-prepare "TODO")
      (setq todo-keywords org-todo-keywords-for-agenda
            org-select-this-todo-keyword (cond ((stringp arg) arg)
                                               ((and (integerp arg) (> arg 0))
                                                (nth (1- arg) todo-keywords))))
      (when (equal arg '(4))
        (setq org-select-this-todo-keyword
              (mapconcat #'identity
                         (let ((crm-separator "|"))
                           (completing-read-multiple
                            "Keyword (or KWD1|KWD2|...): "
                            (mapcar #'list todo-keywords) nil nil))
                         "|")))
      (when (equal arg 0)
        (setq org-select-this-todo-keyword nil))
      (org-compile-prefix-format 'todo)
      (org-set-sorting-strategy 'todo)
      (setq org-agenda-redo-command
	    `(org-todo-list (or (and (numberp current-prefix-arg) current-prefix-arg)
				,org-select-this-todo-keyword
				current-prefix-arg
                                ,arg)))
      (setq files (org-agenda-files nil 'ifmode)
	    all-todo-entries nil)
      (while (setq file (pop files))
	(catch 'nextfile
	  (org-check-agenda-file file)
	  (setq todo-entries (org-agenda-get-day-entries file today :todo))
	  (setq all-todo-entries (append all-todo-entries todo-entries))))
      (org-agenda--insert-overriding-header
        (with-temp-buffer
	  (insert "Global list of TODO items of type: ")
	  (add-text-properties (point-min) (1- (point))
			       (list 'face 'org-agenda-structure
				     'short-heading
				     (concat "ToDo: "
					     (or org-select-this-todo-keyword "ALL"))))
	  (org-agenda-mark-header-line (point-min))
	  (insert (org-agenda-propertize-selected-todo-keywords
		   org-select-this-todo-keyword))
	  (setq pos (point))
	  (unless org-agenda-multi
	    (insert (substitute-command-keys "Press \
\\<org-agenda-mode-map>`N \\[org-agenda-redo]' (e.g. `0 \\[org-agenda-redo]') \
to search again: (0)[ALL]"))
	    (let ((n 0))
              (dolist (k todo-keywords)
                (let ((s (format "(%d)%s" (cl-incf n) k)))
                  (when (> (+ (current-column) (string-width s) 1) (window-max-chars-per-line))
                    (insert "\n                     "))
                  (insert " " s))))
	    (insert "\n"))
	  (add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure-secondary))
	  (buffer-string)))
      (org-agenda-mark-header-line (point-min))
      (when all-todo-entries
	(insert (org-agenda-finalize-entries all-todo-entries 'todo) "\n"))
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (add-text-properties (point-min) (point-max)
			   `(org-agenda-type todo
					     org-last-args ,arg
					     org-redo-cmd ,org-agenda-redo-command
					     org-series-cmd ,org-cmd))
      (org-agenda-finalize)
      (setq buffer-read-only t))))

(defun org-agenda-propertize-selected-todo-keywords (keywords)
  "Use `org-todo-keyword-faces' for the selected todo KEYWORDS."
  (concat
   (if (or (equal keywords "ALL") (not keywords))
       (propertize "ALL" 'face 'org-agenda-structure-filter)
     (mapconcat
      (lambda (kw)
        (propertize
         kw
         'face (list
                (org-get-todo-face kw org-done-keywords-for-agenda)
                'org-agenda-structure)))
      (org-split-string keywords "|")
      "|"))
   "\n"))

(provide 'org-agenda-todo-view)

;;; org-agenda-todo-view.el ends here
