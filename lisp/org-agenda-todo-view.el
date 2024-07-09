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

(defun org-agenda--todo-block-header (todo-keywords &optional keyword)
  "Produce block header for todo block displaying TODO-KEYWORDS.
When KEYWORD is nil, assume that the block displays all possible todo
keywords.  Otherwise, assume that the block searches for specific
keyword."
  (with-temp-buffer
    (let (pos)
      (insert "Global list of TODO items of type: ")
      (add-text-properties (point-min) (1- (point))
			   (list 'face 'org-agenda-structure
			         'short-heading
			         (concat "ToDo: " (or keyword "ALL"))))
      (insert (org-agenda-propertize-selected-todo-keywords keyword))
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
      (buffer-string))))

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
  
  (let* ((selector
          (cond
           ((org-string-nw-p arg) arg)
           ((and (integerp arg) (> arg 0))
            (nth (1- arg) org-todo-keywords-for-agenda))
           ((equal arg '(4))
            (mapconcat
             #'identity
             (let ((crm-separator "|")
                   (completion-ignore-case t))
               (completing-read-multiple
                "Keyword (or KWD1|KWD2|...): "
                (mapcar #'list org-todo-keywords-for-agenda) nil nil))
             "|")))))

    (org-agenda-insert-block
     'todo
     (lambda ()
       (when-let
           ((entries
             (org-agenda-mapcan-files
              (lambda () (org-agenda-get-todos selector)))))
         (insert (org-agenda-finalize-entries entries 'todo) "\n")))
     :suggested-buffer-name (cons "todo" selector)
     :block-header (org-agenda--todo-block-header
                    org-todo-keywords-for-agenda
                    selector)
     :redo-command `(org-todo-list ,arg)
     :block-args arg)))

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
