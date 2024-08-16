;;; org-map.el --- Org mapping API                      -*- lexical-binding: t; -*-

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

;; This library implements Org mapping API to act upon multiple Org
;; elements.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-macs)
(require 'org-move)
(require 'org-element)

(defmacro org-with-restriction (scope &rest body)
  "Evaluate BODY within SCOPE.
SCOPE can be
nil or symbol `keep'
  keep current narrowing
symbol `buffer'
  widen the buffer
symbol `agenda-restriction'
  agenda restriction, if any, or widen the buffer
tree
  current subtree
region
  region, adjusted to make sure that the last heading in the region
  includes its section in full.  When no region is active, keep
  current buffer narrowing."
  (declare (indent 2))
  `(save-restriction
     (pcase ,scope
       ((or `nil `keep) ,@body)
       (`buffer (widen) ,@body)
       (`agenda-restriction
        (defvar org-agenda-restrict-begin) ; org-agenda-files.el
        (defvar org-agenda-restrict-end)   ; org-agenda-files.el
	(if (eq (current-buffer) (bound-and-true-p org-agenda-restrict))
	    (narrow-to-region org-agenda-restrict-begin
			      org-agenda-restrict-end)
	  (widen))
	,@body)
       (`tree
        (widen)
        (org-back-to-heading t)
        (require 'org-narrow)
        (declare-function org-narrow-to-subtree "org-narrow" (&optional element))
        (org-narrow-to-subtree)
        ,@body)
       (`region
        (when (use-region-p)
          (narrow-to-region
           (region-beginning)
           (save-excursion
	     (goto-char (region-end))
	     (unless (and (bolp) (org-at-heading-p))
	       (outline-next-heading))
	     (point))))
        ,@body)
       (unknown-scope
        (error "org-with-restriction: Unknown scope %S" unknown-scope)))))

(cl-defun org-map-regexp (regexp action &key skip-when)
  "Map FORM over elements containing REGEXP in current buffer.
Collect non-nil return values into the result.

Honor restriction.

ACTION may be a function or sexp to be evaluated.

ACTION will be called with point at the end of REGEXP match.  The match
data will be set according to REGEXP match.  ACTION can move point to
further location to continue searching REGEXP from.

When ACTION is a function, it will be called with a single argument -
node at point.  Otherwise, dynamically bound variable `node' will hold
the node at point.

Optional argument SKIP-WHEN is a function or sexp.  It will be called
before FUNC.  If SKIP-WHEN returns nil, the current match should not be
skipped.  Otherwise, SKIP-WHEN must return a position from where
the search should be continued.
SKIP-WHEN calling convention is the same as for ACTION."
  (let (current-node result skip-to)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (setq current-node (save-match-data (org-element-at-point))
              skip-to nil)
        (when skip-when
          (setq skip-to
                (save-excursion
                  (save-match-data
                    (org-eval-form skip-when '(node) current-node)))))
        (if skip-to (goto-char skip-to)
          (push (org-eval-form action '(node) current-node) result)))
      (nreverse (delq nil result)))))

(defun org-map-tree (fun)
  "Call FUN for every heading underneath the current one."
  (org-back-to-heading t)
  (let ((level (funcall outline-level)))
    (save-excursion
      (funcall fun)
      (while (and (progn
		    (outline-next-heading)
		    (> (funcall outline-level) level))
		  (not (eobp)))
	(funcall fun)))))

(defun org-map-region (fun beg end)
  "Call FUN for every heading between BEG and END."
  (let (;; Force commands ignore region, if any region is active.
        (transient-mark-mode nil))
    (save-excursion
      (setq end (copy-marker end))
      (goto-char beg)
      (when (and (re-search-forward org-outline-regexp-bol nil t)
		 (< (point) end))
	(funcall fun))
      (while (and (progn
		    (outline-next-heading)
		    (< (point) end))
		  (not (eobp)))
	(funcall fun)))))

(defun org-block-map (function &optional start end)
  "Call FUNCTION at the head of all source blocks in the current buffer.
Optional arguments START and END can be used to limit the range."
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end) (re-search-forward "^[ \t]*#\\+begin" end t))
	(save-excursion
	  (save-match-data
            (goto-char (match-beginning 0))
            (when (org-at-block-p)
              (funcall function))))))))


(defun org-babel-active-location-p ()
  "Return non-nil, when at executable element."
  (org-element-type-p
   (save-match-data (org-element-context))
   '(babel-call inline-babel-call inline-src-block src-block)))

;;;###autoload
(defmacro org-babel-map-src-blocks (file &rest body)
  "Evaluate BODY forms on each source-block in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer.  During evaluation of BODY the following local variables
are set relative to the currently matched code block.

full-block ------- string holding the entirety of the code block
beg-block -------- point at the beginning of the code block
end-block -------- point at the end of the matched code block
lang ------------- string holding the language of the code block
beg-lang --------- point at the beginning of the lang
end-lang --------- point at the end of the lang
switches --------- string holding the switches
beg-switches ----- point at the beginning of the switches
end-switches ----- point at the end of the switches
header-args ------ string holding the header-args
beg-header-args -- point at the beginning of the header-args
end-header-args -- point at the end of the header-args
body ------------- string holding the body of the code block
beg-body --------- point at the beginning of the body
end-body --------- point at the end of the body"
  (declare (indent 1) (debug t))
  (let ((tempvar (make-symbol "file")))
    `(let* ((case-fold-search t)
	    (,tempvar ,file)
	    (visited-p (or (null ,tempvar)
			   (get-file-buffer (expand-file-name ,tempvar))))
	    (point (point)) to-be-removed)
       (save-window-excursion
	 (when ,tempvar (find-file ,tempvar))
	 (setq to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (while (re-search-forward org-babel-src-block-regexp nil t)
	   (when (org-babel-active-location-p)
	     (goto-char (match-beginning 0))
	     (let ((full-block (match-string 0))
		   (beg-block (match-beginning 0))
		   (end-block (match-end 0))
		   (lang (match-string 2))
		   (beg-lang (match-beginning 2))
		   (end-lang (match-end 2))
		   (switches (match-string 3))
		   (beg-switches (match-beginning 3))
		   (end-switches (match-end 3))
		   (header-args (match-string 4))
		   (beg-header-args (match-beginning 4))
		   (end-header-args (match-end 4))
		   (body (match-string 5))
		   (beg-body (match-beginning 5))
		   (end-body (match-end 5)))
               ;; Silence byte-compiler in case `body' doesn't use all
               ;; those variables.
               (ignore full-block beg-block end-block lang
                       beg-lang end-lang switches beg-switches
                       end-switches header-args beg-header-args
                       end-header-args body beg-body end-body)
               ,@body
	       (goto-char end-block)))))
       (unless visited-p (kill-buffer to-be-removed))
       (goto-char point))))

;;;###autoload
(defmacro org-babel-map-inline-src-blocks (file &rest body)
  "Evaluate BODY forms on each inline source block in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer."
  (declare (indent 1) (debug (form body)))
  (org-with-gensyms (datum end point tempvar to-be-removed visitedp)
    `(let* ((case-fold-search t)
	    (,tempvar ,file)
	    (,visitedp (or (null ,tempvar)
			   (get-file-buffer (expand-file-name ,tempvar))))
	    (,point (point))
	    ,to-be-removed)
       (save-window-excursion
	 (when ,tempvar (find-file ,tempvar))
	 (setq ,to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (while (re-search-forward "src_\\S-" nil t)
	   (let ((,datum (org-element-context)))
	     (when (org-element-type-p ,datum 'inline-src-block)
	       (goto-char (org-element-begin ,datum))
	       (let ((,end (copy-marker (org-element-end ,datum))))
		 ,@body
		 (goto-char ,end)
		 (set-marker ,end nil))))))
       (unless ,visitedp (kill-buffer ,to-be-removed))
       (goto-char ,point))))

;;;###autoload
(defmacro org-babel-map-call-lines (file &rest body)
  "Evaluate BODY forms on each call line in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer."
  (declare (indent 1) (debug (form body)))
  (org-with-gensyms (datum end point tempvar to-be-removed visitedp)
    `(let* ((case-fold-search t)
	    (,tempvar ,file)
	    (,visitedp (or (null ,tempvar)
			   (get-file-buffer (expand-file-name ,tempvar))))
	    (,point (point))
	    ,to-be-removed)
       (save-window-excursion
	 (when ,tempvar (find-file ,tempvar))
	 (setq ,to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (while (re-search-forward "call_\\S-\\|^[ \t]*#\\+CALL:" nil t)
	   (let ((,datum (org-element-context)))
	     (when (org-element-type-p ,datum '(babel-call inline-babel-call))
	       (goto-char (or (org-element-post-affiliated datum)
                              (org-element-begin datum)))
	       (let ((,end (copy-marker (org-element-end ,datum))))
		 ,@body
		 (goto-char ,end)
		 (set-marker ,end nil))))))
       (unless ,visitedp (kill-buffer ,to-be-removed))
       (goto-char ,point))))

;;;###autoload
(defmacro org-babel-map-executables (file &rest body)
  "Evaluate BODY forms on each active Babel code in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer."
  (declare (indent 1) (debug (form body)))
  (org-with-gensyms (datum end point tempvar to-be-removed visitedp)
    `(let* ((case-fold-search t)
	    (,tempvar ,file)
	    (,visitedp (or (null ,tempvar)
			   (get-file-buffer (expand-file-name ,tempvar))))
	    (,point (point))
	    ,to-be-removed)
       (save-window-excursion
	 (when ,tempvar (find-file ,tempvar))
	 (setq ,to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (while (re-search-forward
		 "\\(call\\|src\\)_\\|^[ \t]*#\\+\\(BEGIN_SRC\\|CALL:\\)" nil t)
	   (let ((,datum (org-element-context)))
	     (when (org-element-type-p
                    ,datum
                    '(babel-call inline-babel-call inline-src-block src-block))
	       (goto-char (or (org-element-post-affiliated ,datum)
                              (org-element-begin ,datum)))
	       (let ((,end (copy-marker (org-element-end ,datum))))
		 ,@body
		 (goto-char ,end)
		 (set-marker ,end nil))))))
       (unless ,visitedp (kill-buffer ,to-be-removed))
       (goto-char ,point))))

(provide 'org-map)

;;; org-map.el ends here
