;;; ob-commands.el --- Commands for working with Code Blocks          -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2024 Free Software Foundation, Inc.

;; Authors: Eric Schulte
;;	Dan Davison
;; Keywords: literate programming, reproducible research
;; URL: https://orgmode.org

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

;; This library implements user-facing commands to work with code
;; blocks.  These commands are not used internally and are just aiming
;; to users.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob-core)

;;; Helpers

(defmacro org-babel-when-in-src-block (&rest body)
  "Execute BODY if point is in a source block and return t.

Otherwise do nothing and return nil."
  `(if (org-element-type-p (org-element-context) '(inline-src-block src-block))
       (progn
	 ,@body
	 t)
     nil))

(defun org-babel-src-block-names (&optional file)
  "Return the names of source blocks in FILE or the current buffer."
  (with-current-buffer (if file (find-file-noselect file) (current-buffer))
    (org-with-point-at 1
      (let ((regexp "^[ \t]*#\\+begin_src ")
	    (case-fold-search t)
	    (names nil))
	(while (re-search-forward regexp nil t)
	  (let ((element (org-element-at-point)))
	    (when (org-element-type-p element 'src-block)
	      (let ((name (org-element-property :name element)))
		(when name (push name names))))))
	names))))

(defun org-babel-find-named-block (name)
  "Find a named source-code block.
Return the location of the source block identified by source
NAME, or nil if no such block exists.  Set match data according
to `org-babel-named-src-block-regexp'."
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (org-babel-named-src-block-regexp-for-name name)))
      (or (and (looking-at regexp)
	       (progn (goto-char (match-beginning 1))
		      (line-beginning-position)))
	  (ignore-errors (org-next-block 1 nil regexp))))))

;;; Navigate src blocks

;;;###autoload
(defun org-babel-goto-src-block-head ()
  "Go to the beginning of the current code block."
  (interactive)
  (let ((head (org-src-block-head)))
    (if head (goto-char head) (error "Not currently in a code block"))))

(declare-function org-mark-ring-push "org-mark-ring" (&optional pos buffer))
(declare-function org-fold-show-context "org-fold" (&optional key))
;;;###autoload
(defun org-babel-goto-named-src-block (name)
  "Go to a source-code block with NAME."
  (interactive
   (let ((completion-ignore-case t)
	 (case-fold-search t)
	 (all-block-names (org-babel-src-block-names)))
     (list (completing-read
	    "source-block name: " all-block-names nil t
	    (let* ((context (org-element-context))
		   (type (org-element-type context))
		   (noweb-ref
		    (and (memq type '(inline-src-block src-block))
			 (org-in-regexp (org-babel-noweb-wrap)))))
	      (cond
	       (noweb-ref
		(buffer-substring
		 (+ (car noweb-ref) (length org-babel-noweb-wrap-start))
		 (- (cdr noweb-ref) (length org-babel-noweb-wrap-end))))
	       ((memq type '(babel-call inline-babel-call)) ;#+CALL:
		(org-element-property :call context))
	       ((car (org-element-property :results context))) ;#+RESULTS:
	       ((let ((symbol (thing-at-point 'symbol))) ;Symbol.
		  (and symbol
		       (member-ignore-case symbol all-block-names)
		       symbol)))
	       (t "")))))))
  (let ((point (org-babel-find-named-block name)))
    (if point
        ;; Taken from `org-open-at-point'.
        (progn
          (require 'org-mark-ring)
          (org-mark-ring-push)
          (goto-char point)
          (require 'org-fold)
          (org-fold-show-context))
      (message "source-code block `%s' not found in this buffer" name))))

(defun org-babel-result-names (&optional file)
  "Return the names of results in FILE or the current buffer."
  (with-current-buffer (if file (find-file-noselect file) (current-buffer))
    (org-with-point-at 1
      (let ((case-fold-search t) names)
        (while (re-search-forward org-babel-result-w-name-regexp nil t)
	  (setq names (cons (match-string-no-properties 9) names)))
        names))))

;;;###autoload
(defun org-babel-goto-named-result (name)
  "Go to a result with NAME."
  (interactive
   (let ((completion-ignore-case t))
     (list (completing-read "Source-block name: "
			    (org-babel-result-names) nil t))))
  (let ((point (org-babel-find-named-result name)))
    (if point
        ;; taken from `org-open-at-point'
        (progn (goto-char point) (org-fold-show-context))
      (message "result `%s' not found in this buffer" name))))

(declare-function org-next-block "org-move" (arg &optional backward block-regexp))
;;;###autoload
(defun org-babel-next-src-block (&optional arg)
  "Jump to the next source block.
With optional prefix argument ARG, jump forward ARG many source blocks."
  (interactive "p")
  (require 'org-move)
  (org-next-block arg nil org-babel-src-block-regexp))

(declare-function org-previous-block "org-move" (arg &optional block-regexp))
;;;###autoload
(defun org-babel-previous-src-block (&optional arg)
  "Jump to the previous source block.
With optional prefix argument ARG, jump backward ARG many source blocks."
  (interactive "p")
  (require 'org-move)
  (org-previous-block arg org-babel-src-block-regexp))

(defvar org-babel-results-buffer-name "*Org Babel Results*"
  "The buffer name of Org Babel evaluate results.")

(declare-function org-open-at-point "org-open-at-point" (&optional arg))
;;;###autoload
(defun org-babel-open-src-block-result (&optional re-run)
  "Open results of source block at point.

If `point' is on a source block then open the results of the source
code block, otherwise return nil.  With optional prefix argument
RE-RUN the source-code block is evaluated even if results already
exist."
  (interactive "P")
  (pcase (org-babel-get-src-block-info 'no-eval)
    (`(,_ ,_ ,arguments ,_ ,_ ,start ,_)
     (save-excursion
       ;; Go to the results, if there aren't any then run the block.
       (goto-char start)
       (goto-char (or (and (not re-run) (org-babel-where-is-src-block-result))
		      (progn (org-babel-execute-src-block)
			     (org-babel-where-is-src-block-result))))
       (end-of-line)
       (skip-chars-forward " \r\t\n")
       ;; Open the results.
       (if (looking-at org-link-bracket-re)
           (progn (require 'org-open-at-point) (org-open-at-point))
	 (let ((r (org-babel-format-result (org-babel-read-result)
					   (cdr (assq :sep arguments)))))
	   (pop-to-buffer (get-buffer-create org-babel-results-buffer-name))
	   (erase-buffer)
	   (insert r)))
       t))
    (_ nil)))

;;; Execute src blocks

(declare-function org-babel-eval-wipe-error-buffer "ob-eval" ())
;;;###autoload
(defun org-babel-execute-buffer (&optional arg)
  "Execute source code blocks in a buffer.
Prefix argument ARG is passed to `org-babel-execute-src-block'.
Call `org-babel-execute-src-block' on every source block in
the current buffer."
  (interactive "P")
  (require 'ob-eval)
  (org-babel-eval-wipe-error-buffer)
  (org-save-outline-visibility t
    (org-babel-map-executables nil
      (if (org-element-type-p
           (org-element-context) '(babel-call inline-babel-call))
          (org-babel-lob-execute-maybe)
        (org-babel-execute-src-block arg)))))

(declare-function org-narrow-to-subtree "org-narrow" (&optional element))
;;;###autoload
(defun org-babel-execute-subtree (&optional arg)
  "Execute source code blocks in a subtree.
Call `org-babel-execute-src-block' on every source block in
the current subtree, passing over the prefix argument ARG."
  (interactive "P")
  (save-restriction
    (save-excursion
      (require 'org-narrow)
      (org-narrow-to-subtree)
      (org-babel-execute-buffer arg)
      (widen))))

;;;###autoload
(defun org-babel-execute-safely-maybe ()
  "Maybe `org-babel-execute-maybe'.
This function does nothing unless `org-babel-no-eval-on-ctrl-c-ctrl-c'
is non-nil."
  (unless org-babel-no-eval-on-ctrl-c-ctrl-c
    (org-babel-execute-maybe)))

;;;###autoload
(defun org-babel-execute-maybe ()
  "Execute src block or babel call at point."
  (interactive)
  (or (org-babel-execute-src-block-maybe)
      (org-babel-lob-execute-maybe)))

(defun org-babel-execute-src-block-maybe ()
  "Conditionally execute a source block.
Detect if this is context for a Babel src-block and if so
then run `org-babel-execute-src-block'."
  (interactive)
  (org-babel-when-in-src-block
   (org-babel-eval-wipe-error-buffer)
   (org-babel-execute-src-block current-prefix-arg)))

;;;###autoload
(defun org-babel-expand-src-block-maybe ()
  "Conditionally expand a source block.
Detect if this is context for an org-babel src-block and if so
then run `org-babel-expand-src-block'."
  (interactive)
  (org-babel-when-in-src-block
   (org-babel-expand-src-block current-prefix-arg)))



(declare-function org-edit-src-code "org-src" (&optional code edit-buffer-name))
;;;###autoload
(defun org-babel-expand-src-block (&optional _arg info params)
  "Expand the current source code block or block specified by INFO.
INFO is the output of `org-babel-get-src-block-info'.
PARAMS defines inherited header arguments.

Expand according to the source code block's header
arguments and pop open the results in a preview buffer."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (nth 0 info))
	 (params (setf (nth 2 info)
                       (sort (org-babel-merge-params (nth 2 info) params)
                             (lambda (el1 el2) (string< (symbol-name (car el1))
						   (symbol-name (car el2)))))))
         (body (setf (nth 1 info)
		     (if (org-babel-noweb-p params :eval)
			 (org-babel-expand-noweb-references info) (nth 1 info))))
         (expand-cmd (intern (concat "org-babel-expand-body:" lang)))
	 (assignments-cmd (intern (concat "org-babel-variable-assignments:"
					  lang)))
         (expanded
	  (if (fboundp expand-cmd) (funcall expand-cmd body params)
	    (org-babel-expand-body:generic
	     body params (and (fboundp assignments-cmd)
			      (funcall assignments-cmd params))))))
    (if (called-interactively-p 'any)
        (progn
          (require 'org-src)
          (org-edit-src-code
           expanded (concat "*Org-Babel Preview " (buffer-name) "[ " lang " ]*")))
      expanded)))

;;; Interact with code block's session

;;;###autoload
(defun org-babel-load-in-session (&optional _arg info)
  "Load the body of the current source-code block.
When optional argument INFO is non-nil, use source block defined in
INFO, as returned by `org-babel-get-src-block-info'.

Evaluate the header arguments for the source block before
entering the session.  After loading the body this pops open the
session."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (nth 0 info))
         (params (nth 2 info))
         (body (if (not info)
		   (user-error "No src code block at point")
		 (setf (nth 1 info)
		       (if (org-babel-noweb-p params :eval)
			   (org-babel-expand-noweb-references info)
			 (nth 1 info)))))
         (session (cdr (assq :session params)))
	 (dir (cdr (assq :dir params)))
	 (default-directory
	  (or (and dir (file-name-as-directory dir)) default-directory))
	 (cmd (intern (concat "org-babel-load-session:" lang))))
    (unless (fboundp cmd)
      (error "No org-babel-load-session function for %s!" lang))
    (pop-to-buffer (funcall cmd session body params))
    (end-of-line 1)))

;;;###autoload
(defun org-babel-load-in-session-maybe ()
  "Conditionally load a source block in a session.
Detect if this is context for an org-babel src-block and if so
then run `org-babel-load-in-session'."
  (interactive)
  (org-babel-when-in-src-block
   (org-babel-load-in-session current-prefix-arg)))
(add-hook 'org-metaup-hook 'org-babel-load-in-session-maybe)

;;;###autoload
(defun org-babel-pop-to-session-maybe ()
  "Conditionally pop to a session.
Detect if this is context for an org-babel src-block and if so
then run `org-babel-switch-to-session'."
  (interactive)
  (org-babel-when-in-src-block
   (org-babel-switch-to-session current-prefix-arg)))
(add-hook 'org-metadown-hook 'org-babel-pop-to-session-maybe)

;;;###autoload
(defun org-babel-initiate-session (&optional arg info)
  "Initiate session for current code block or the block defined by INFO.
If called with a prefix argument ARG, then resolve any variable
references in the header arguments and assign these variables in
the session.  Copy the body of the code block to the kill ring."
  (interactive "P")
  (let* ((info (or info (org-babel-get-src-block-info (not arg))))
         (lang (nth 0 info))
         (body (nth 1 info))
         (params (nth 2 info))
         (session (cdr (assq :session params)))
	 (dir (cdr (assq :dir params)))
	 (default-directory
	  (or (and dir (file-name-as-directory dir)) default-directory))
	 (init-cmd (intern (format "org-babel-%s-initiate-session" lang)))
	 (prep-cmd (intern (concat "org-babel-prep-session:" lang))))
    (when (and (stringp session) (string= session "none"))
      (error "This block is not using a session!"))
    (unless (fboundp init-cmd)
      (error "No org-babel-initiate-session function for %s!" lang))
    (with-temp-buffer (insert (org-trim body))
                      (copy-region-as-kill (point-min) (point-max)))
    (when arg
      (unless (fboundp prep-cmd)
	(error "No org-babel-prep-session function for %s!" lang))
      (funcall prep-cmd session params))
    (funcall init-cmd session params)))

;;;###autoload
(defun org-babel-switch-to-session (&optional arg info)
  "Switch to the session of the current code block or block defined by INFO.
Uses `org-babel-initiate-session' to start the session.  If called
with a prefix argument ARG, then this is passed on to
`org-babel-initiate-session'."
  (interactive "P")
  (pop-to-buffer (org-babel-initiate-session arg info))
  (end-of-line 1))

(defalias 'org-babel-pop-to-session 'org-babel-switch-to-session)

(defvar org-src-window-setup)

;;;###autoload
(defun org-babel-switch-to-session-with-code (&optional arg _info)
  "Switch to code buffer and display session.
Prefix argument ARG is passed to `org-babel-switch-to-session'."
  (interactive "P")
  (let ((swap-windows
	 (lambda ()
	   (let ((other-window-buffer (window-buffer (next-window))))
	     (set-window-buffer (next-window) (current-buffer))
	     (set-window-buffer (selected-window) other-window-buffer))
	   (other-window 1)))
	(info (org-babel-get-src-block-info))
	(org-src-window-setup 'reorganize-frame))
    (save-excursion
      (org-babel-switch-to-session arg info))
    (org-edit-src-code)
    (funcall swap-windows)))

;;; Edit src blocks

;;;###autoload
(defun org-babel-insert-header-arg (&optional header-arg value)
  "Insert a header argument and its value.
HEADER-ARG and VALUE, when provided, are the header argument name and
its value.  When HEADER-ARG or VALUE are nil, offer interactive
completion from lists of common args and values."
  (interactive)
  (let* ((info (org-babel-get-src-block-info 'no-eval))
	 (lang (car info))
	 (begin (nth 5 info))
	 (lang-headers (intern (concat "org-babel-header-args:" lang)))
	 (headers (org-babel-combine-header-arg-lists
		   org-babel-common-header-args-w-values
		   (when (boundp lang-headers) (eval lang-headers t))))
	 (header-arg (or header-arg
			 (completing-read
			  "Header Arg: "
			  (mapcar
			   (lambda (header-spec) (symbol-name (car header-spec)))
			   headers))))
	 (vals (cdr (assoc (intern header-arg) headers)))
	 (value (or value
		    (cond
		     ((eq vals :any)
		      (read-from-minibuffer "value: "))
		     ((listp vals)
		      (mapconcat
		       (lambda (group)
			 (let ((arg (completing-read
				     "Value: "
				     (cons "default"
					   (mapcar #'symbol-name group)))))
			   (if (and arg (not (string= "default" arg)))
			       (concat arg " ")
			     "")))
		       vals ""))))))
    (save-excursion
      (goto-char begin)
      (goto-char (line-end-position))
      (unless (= (char-before (point)) ?\ ) (insert " "))
      (insert ":" header-arg) (when value (insert " " value)))))

;; Add support for completing-read insertion of header arguments after ":"
(defun org-babel-header-arg-expand ()
  "Call `org-babel-enter-header-arg-w-completion' in appropriate contexts."
  (when (and (equal (char-before) ?\:) (org-src-block-head))
    (org-babel-enter-header-arg-w-completion (match-string 2))))

(defun org-babel-enter-header-arg-w-completion (&optional lang)
  "Insert header argument appropriate for LANG with completion."
  (let* ((lang-headers-var (intern (concat "org-babel-header-args:" lang)))
         (lang-headers (when (boundp lang-headers-var) (eval lang-headers-var t)))
	 (headers-w-values (org-babel-combine-header-arg-lists
			    org-babel-common-header-args-w-values lang-headers))
         (headers (mapcar #'symbol-name (mapcar #'car headers-w-values)))
         (header (org-completing-read "Header Arg: " headers))
         (args (cdr (assoc (intern header) headers-w-values)))
         (arg (when (and args (listp args))
                (org-completing-read
                 (format "%s: " header)
                 (mapcar #'symbol-name (apply #'append args))))))
    (insert (concat header " " (or arg "")))
    (cons header arg)))

(add-hook 'org-cycle-tab-first-hook 'org-babel-header-arg-expand)

(declare-function org-indent-block "org-indent-static" ())
(defun org-babel-demarcate-block (&optional arg)
  "Wrap or split the code in an active region or at point.

With prefix argument ARG, also create a new heading at point.

When called from inside of a code block the current block is
split.  When called from outside of a code block a new code block
is created.  In both cases if the region is demarcated and if the
region is not active then the point is demarcated.

When called within blank lines after a code block, create a new code
block of the same language as the previous."
  (interactive "P")
  (let* ((info (org-babel-get-src-block-info 'no-eval))
	 (start (org-src-block-head))
         ;; `start' will be nil when within space lines after src block.
	 (block (and start (match-string 0)))
         (body-beg (and start (match-beginning 5)))
         (body-end (and start (match-end 5)))
	 (stars (concat (make-string (or (org-current-level) 1) ?*) " "))
	 (upper-case-p (and block
			    (let (case-fold-search)
			      (string-match-p "#\\+BEGIN_SRC" block)))))
    (if (and info start) ;; At src block, but not within blank lines after it.
        (let* ((copy (org-element-copy (org-element-at-point)))
               (before (org-element-begin copy))
               (beyond (org-element-end copy))
               (parts
                (if (use-region-p)
                    (list body-beg (region-beginning) (region-end) body-end)
                  (list body-beg (point) body-end)))
               (pads ;; To calculate left-side white-space padding.
                (if (use-region-p)
                    (list (region-beginning) (region-end))
                  (list (point))))
               (n (- (length parts) 2)) ;; 1 or 2 parts in `dolist' below.
               ;; `post-blank' caches the property before setting it to 0.
               (post-blank (org-element-property :post-blank copy))
               (to-uppercase
                (lambda (str)
                  (string-match "^[ \t]*#\\+\\(begin_src\\)" str)
                  (setq str (replace-match "BEGIN_SRC" t t str 1))
                  (string-match "^[ \t]*#\\+\\(end_src\\)" str)
                  (setq str (replace-match "END_SRC" t t str 1))
                  str)))
          ;; Point or region are within body when parts is in increasing order.
          (unless (apply #'<= parts)
            (user-error "Select within the source block body to split it"))
          (setq parts (mapcar (lambda (p) (buffer-substring (car p) (cdr p)))
                              (seq-mapn #'cons parts (cdr parts))))
          ;; Map positions to columns for white-space padding.
          (setq pads (mapcar (lambda (p) (save-excursion
                                           (goto-char p)
                                           (current-column)))
                             pads))
          (push 0 pads) ;; The 1st part never requires white-space padding.
          (setq parts (mapcar (lambda (p) (string-join
                                           (list (make-string (car p) ?\s)
                                                 (cdr p))))
                              (seq-mapn #'cons pads parts)))
          (delete-region before beyond)
          ;; Set `:post-blank' to 0.  We take care of spacing between blocks.
          (org-element-put-property copy :post-blank 0)
          (org-element-put-property copy :value (car parts))
          (let ((copy-str (org-element-interpret-data copy)))
            ;; `org-element-interpret-data' produces lower-case
            ;; #+begin_src .. #+end_src
            (when upper-case-p
              (setq copy-str (funcall to-uppercase copy-str)))
            (insert copy-str))
          ;; `org-indent-block' may see another `org-element' (e.g. paragraph)
          ;; immediately after the block.  Ensure to indent the inserted block
          ;; and move point to its end.
          (org-babel-previous-src-block 1)
          (require 'org-indent-static)
          (org-indent-block)
          (goto-char (org-element-end (org-element-at-point)))
          (org-element-put-property copy :caption nil)
          (org-element-put-property copy :name nil)
          ;; Insert the 2nd block, and the 3rd block when region is active.
          (dolist (part (cdr parts))
            (org-element-put-property copy :value part)
            (insert (if arg (concat stars "\n") "\n"))
            (cl-decf n)
            (when (= n 0)
              ;; Use `post-blank' to reset the property of the last block.
              (org-element-put-property copy :post-blank post-blank))
            (let ((copy-str (org-element-interpret-data copy)))
              ;; `org-element-interpret-data' produces lower-case
              ;; #+begin_src .. #+end_src
              (when upper-case-p
                (setq copy-str (funcall to-uppercase copy-str)))
              (insert copy-str))
            ;; Ensure to indent the inserted block and move point to its end.
            (org-babel-previous-src-block 1)
            (org-indent-block)
            (goto-char (org-element-end (org-element-at-point))))
          ;; Leave point at the last inserted block.
          (goto-char (org-babel-previous-src-block 1)))
      (let ((start (point))
	    (lang (or (car info) ; Reuse language from previous block.
                      (progn
                        (require 'org-src)
                        (defvar org-src-lang-modes)
                        (completing-read
		         "Lang: "
		         (mapcar #'symbol-name
			         (delete-dups
			          (append (mapcar #'car org-babel-load-languages)
				          (mapcar (lambda (el) (intern (car el)))
					          org-src-lang-modes))))))))
	    (body (delete-and-extract-region
		   (if (use-region-p) (mark) (point)) (point))))
	(insert (concat (if (looking-at "^") "" "\n")
			(if arg (concat stars "\n") "")
			(if upper-case-p "#+BEGIN_SRC " "#+begin_src ")
			lang "\n" body
			(if (or (= (length body) 0)
				(string-suffix-p "\r" body)
				(string-suffix-p "\n" body))
			    ""
			  "\n")
			(if upper-case-p "#+END_SRC\n" "#+end_src\n")))
	(goto-char start)
	(move-end-of-line 1)))))

;;; Fold results

(defvar org-babel-hide-result-overlays nil
  "Overlays hiding results.")

(defun org-babel-result-hide-all ()
  "Fold all results in the current buffer."
  (interactive)
  (org-babel-show-result-all)
  (save-excursion
    (let ((case-fold-search t))
      (while (re-search-forward org-babel-result-regexp nil t)
	(save-excursion (goto-char (match-beginning 0))
			(org-babel-hide-result-toggle-maybe))))))

(defun org-babel-show-result-all ()
  "Unfold all results in the current buffer."
  (mapc 'delete-overlay org-babel-hide-result-overlays)
  (setq org-babel-hide-result-overlays nil))

;;;###autoload
(defun org-babel-hide-result-toggle-maybe ()
  "Toggle visibility of result at point."
  (interactive)
  (let ((case-fold-search t))
    (and (org-match-line org-babel-result-regexp)
         (progn (org-babel-hide-result-toggle) t))))

(defun org-babel-hide-result-toggle (&optional force)
  "Toggle the visibility of the current result.
When FORCE is symbol `off', unconditionally display the result.
Otherwise, when FORCE is non-nil, unconditionally hide the result."
  (interactive)
  (save-excursion
    (forward-line 0)
    (let ((case-fold-search t))
      (unless (re-search-forward org-babel-result-regexp nil t)
	(error "Not looking at a result line")))
    (let ((start (progn (forward-line 1) (1- (point))))
	  (end (progn
		 (while (looking-at org-babel-multi-line-header-regexp)
		   (forward-line 1))
		 (goto-char (1- (org-babel-result-end)))
		 (point)))
	  ov)
      (if (memq t (mapcar (lambda (overlay)
			    (eq (overlay-get overlay 'invisible)
				'org-babel-hide-result))
			  (overlays-at start)))
	  (when (or (not force) (eq force 'off))
	    (mapc (lambda (ov)
		    (when (member ov org-babel-hide-result-overlays)
		      (setq org-babel-hide-result-overlays
			    (delq ov org-babel-hide-result-overlays)))
		    (when (eq (overlay-get ov 'invisible)
			      'org-babel-hide-result)
		      (delete-overlay ov)))
		  (overlays-at start)))
	(setq ov (make-overlay start end))
	(overlay-put ov 'invisible 'org-babel-hide-result)
	;; make the block accessible to isearch
	(overlay-put
	 ov 'isearch-open-invisible
	 (lambda (ov)
	   (when (member ov org-babel-hide-result-overlays)
	     (setq org-babel-hide-result-overlays
		   (delq ov org-babel-hide-result-overlays)))
	   (when (eq (overlay-get ov 'invisible)
		     'org-babel-hide-result)
	     (delete-overlay ov))))
	(push ov org-babel-hide-result-overlays)))))

;; org-tab-after-check-for-cycling-hook
(add-hook 'org-cycle-tab-first-hook #'org-babel-hide-result-toggle-maybe)
;; Remove overlays when changing major mode
(add-hook 'org-mode-hook
	  (lambda () (add-hook 'change-major-mode-hook
			  #'org-babel-show-result-all 'append 'local)))

;;; Misc

(declare-function org-entry-get "org-property" (epom property &optional inherit literal-nil))
;;;###autoload
(defun org-babel-view-src-block-info ()
  "Display information on the current source block.
This includes header arguments, language and name, and is largely
a window into the `org-babel-get-src-block-info' function."
  (interactive)
  (require 'org-property)
  (let ((info (org-babel-get-src-block-info 'no-eval))
	(full (lambda (it) (> (length it) 0)))
	(printf (lambda (fmt &rest args) (princ (apply #'format fmt args)))))
    (when info
      (with-help-window (help-buffer)
	(let ((name        (nth 4 info))
	      (lang        (nth 0 info))
	      (switches    (nth 3 info))
	      (header-args (nth 2 info)))
	  (when name            (funcall printf "Name: %s\n"     name))
	  (when lang            (funcall printf "Lang: %s\n"     lang))
	  (funcall printf "Properties:\n")
	  (funcall printf "\t:header-args \t%s\n" (org-entry-get (point) "header-args" t))
	  (funcall printf "\t:header-args:%s \t%s\n" lang (org-entry-get (point) (concat "header-args:" lang) t))

	  (when (funcall full switches) (funcall printf "Switches: %s\n" switches))
	  (funcall printf "Header Arguments:\n")
	  (dolist (pair (sort header-args
			      (lambda (a b) (string< (symbol-name (car a))
						(symbol-name (car b))))))
	    (when (funcall full (format "%s" (cdr pair)))
	      (funcall printf "\t%S%s\t%s\n"
		       (car pair)
		       (if (> (length (format "%S" (car pair))) 7) "" "\t")
		       (cdr pair)))))))))

;;;###autoload
(defun org-babel-check-src-block ()
  "Check for misspelled header arguments in the current code block."
  (interactive)
  ;; TODO: report malformed code block
  ;; TODO: report incompatible combinations of header arguments
  ;; TODO: report uninitialized variables
  (let ((too-close 2) ;; <- control closeness to report potential match
	(names (mapcar #'symbol-name org-babel-header-arg-names)))
    (dolist (header (mapcar (lambda (arg) (substring (symbol-name (car arg)) 1))
			    (and (org-src-block-head)
				 (org-babel-parse-header-arguments
				  (org-no-properties
				   (match-string 4))))))
      (dolist (name names)
	(when (and (not (string= header name))
		   (<= (org-string-distance header name) too-close)
		   (not (member header names)))
	  (error "Supplied header \"%S\" is suspiciously close to \"%S\""
		 header name))))
    (message "No suspicious header arguments found.")))

(defvar org-babel-load-languages)

;;;###autoload
(defun org-babel-mark-block ()
  "Mark current source block."
  (interactive)
  (let ((head (org-src-block-head)))
    (when head
      (save-excursion
        (goto-char head)
        (looking-at org-babel-src-block-regexp))
      (push-mark (match-end 5) nil t)
      (goto-char (match-beginning 5)))))

(defun org-babel-remove-result-one-or-many (arg)
  "Remove the result of the current source block.
If called with prefix argument ARG, remove all result blocks in the
buffer."
  (interactive "P")
  (if arg
      (org-babel-map-src-blocks nil (org-babel-remove-result))
    (org-babel-remove-result)))

(provide 'ob-commands)

;;; ob-commands.el ends here
