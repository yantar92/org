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
