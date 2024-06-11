;;; org-agenda-multi-view.el --- Combining multiple agenda views  -*- lexical-binding: t; -*-

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

;; This file implements combining multiple Org agenda views in a
;; single agenda block.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-agenda-mode)

;;;###autoload
(defun org-agenda-run-series (name series)
  "Run agenda NAME as a SERIES of agenda commands."
  (let* ((gprops (nth 1 series))
         (gvars (mapcar #'car gprops))
         (gvals (mapcar (lambda (binding) (eval (cadr binding) t)) gprops)))
    (cl-progv gvars gvals (org-agenda-prepare name))
    ;; We need to reset agenda markers here, because when constructing a
    ;; block agenda, the individual blocks do not do that.
    (org-agenda-reset-markers)
    (with-no-warnings
      (defvar match))          ;Used via the `eval' below.
    (let* ((org-agenda-multi t)
	   ;; FIXME: Redo should contain lists of (FUNS . ARGS) rather
           ;; than expressions, so you don't need to `quote' the args
           ;; and you just need to `apply' instead of `eval' when using it.
	   (redo (list 'org-agenda-run-series name (list 'quote series)))
	   (cmds (car series))
	   match
	   org-cmd type lprops)
      (while (setq org-cmd (pop cmds))
        (setq type (car org-cmd))
        (setq match (eval (nth 1 org-cmd) t))
        (setq lprops (nth 2 org-cmd))
        (let ((org-agenda-overriding-arguments
	       (if (eq org-agenda-overriding-cmd org-cmd)
		   (or org-agenda-overriding-arguments
		       org-agenda-overriding-cmd-arguments)))
              (lvars (mapcar #'car lprops))
              (lvals (mapcar (lambda (binding) (eval (cadr binding) t)) lprops)))
          (cl-progv (append gvars lvars) (append gvals lvals)
	    (pcase type
	      (`agenda
	       (call-interactively #'org-agenda-list))
	      (`agenda*
	       (funcall 'org-agenda-list nil nil nil t))
	      (`alltodo
	       (call-interactively #'org-todo-list))
	      (`search
	       (org-search-view current-prefix-arg match nil))
	      (`stuck
	       (call-interactively #'org-agenda-list-stuck-projects))
	      (`tags
	       (org-tags-view current-prefix-arg match))
	      (`tags-todo
	       (org-tags-view '(4) match))
	      (`todo
	       (org-todo-list match))
	      ((pred fboundp)
	       (funcall type match))
	      (_ (error "Invalid type in command series"))))))
      (widen)
      (let ((inhibit-read-only t))
	(add-text-properties (point-min) (point-max)
			     `(org-series t org-series-redo-cmd ,redo)))
      (setq org-agenda-redo-command redo)
      (goto-char (point-min)))
    (org-agenda-fit-window-to-buffer)
    (cl-progv gvars gvals (org-agenda-finalize))))

(provide 'org-agenda-multi-view)

;;; org-agenda-multi-view.el ends here
