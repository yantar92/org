;;; org-outline.el --- Org outline API  -*- lexical-binding: t; -*-

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

;; This library implements Org mode API to retrieve heading titles and
;; outline path.

;;; Code:

(require 'org-move)

(defsubst org-entry-beginning-position ()
  "Return the beginning position of the current entry."
  (save-excursion (org-back-to-heading t) (point)))

(defsubst org-entry-end-position ()
  "Return the end position of the current entry."
  (save-excursion (outline-next-heading) (point)))

;;;###autoload
(defun org-get-heading (&optional no-tags no-todo no-priority no-comment)
  "Return the heading of the current entry, without the stars.
When NO-TAGS is non-nil, don't include tags.
When NO-TODO is non-nil, don't include TODO keywords.
When NO-PRIORITY is non-nil, don't include priority cookie.
When NO-COMMENT is non-nil, don't include COMMENT string.
Return nil before first heading."
  (unless (org-before-first-heading-p)
    (save-excursion
      (org-back-to-heading t)
      (let ((case-fold-search nil))
	(looking-at org-complex-heading-regexp)
        ;; When using `org-fold-core--optimise-for-huge-buffers',
        ;; returned text will be invisible.  Clear it up.
        (save-match-data
          (org-fold-core-remove-optimisation (match-beginning 0) (match-end 0)))
        (let ((todo (and (not no-todo) (match-string 2)))
	      (priority (and (not no-priority) (match-string 3)))
	      (headline (pcase (match-string 4)
			  (`nil "")
			  ((and (guard no-comment) h)
			   (replace-regexp-in-string
			    (eval-when-compile
			      (format "\\`%s[ \t]+" org-comment-string))
			    "" h))
			  (h h)))
	      (tags (and (not no-tags) (match-string 5))))
          ;; Restore cleared optimization.
          (org-fold-core-update-optimisation (match-beginning 0) (match-end 0))
	  (mapconcat #'identity
		     (delq nil (list todo priority headline tags))
		     " "))))))

(defun org-heading-components ()
  "Return the components of the current heading.
This is a list with the following elements:
- the level as an integer
- the reduced level, different if `org-odd-levels-only' is set.
- the TODO keyword, or nil
- the priority character, like ?A, or nil if no priority is given
- the headline text itself, or the tags string if no headline text
- the tags string, or nil."
  (save-excursion
    (org-back-to-heading t)
    (when (let (case-fold-search) (looking-at org-complex-heading-regexp))
      (org-fold-core-remove-optimisation (match-beginning 0) (match-end 0))
      (prog1
          (list (length (match-string 1))
	        (org-reduced-level (length (match-string 1)))
	        (match-string-no-properties 2)
	        (and (match-end 3) (aref (match-string 3) 2))
	        (match-string-no-properties 4)
	        (match-string-no-properties 5))
        (org-fold-core-update-optimisation (match-beginning 0) (match-end 0))))))

(defun org-get-entry ()
  "Get the entry text, after heading, entire subtree."
  (save-excursion
    (org-back-to-heading t)
    (filter-buffer-substring (line-beginning-position 2) (org-end-of-subtree t))))

(defun org-get-title (&optional buffer-or-file)
  "Collect title from the provided `org-mode' BUFFER-OR-FILE.

Returns nil if there are no #+TITLE property."
  (let ((buffer (cond ((bufferp buffer-or-file) buffer-or-file)
                      ((stringp buffer-or-file) (find-file-noselect
                                                 buffer-or-file))
                      (t (current-buffer)))))
    (with-current-buffer buffer
      (let ((title (org-trim
                    (mapconcat
                     #'identity
                     (org-element-property :TITLE (org-element-org-data))
                     " "))))
        (unless (string= "" title)
          title)))))

(defvar org-outline-path-cache nil
  "Alist between buffer positions and outline paths.
It value is an alist (POSITION . PATH) where POSITION is the
buffer position at the beginning of an entry and PATH is a list
of strings describing the outline path for that entry, in reverse
order.")

(defun org--get-outline-path-1 (&optional use-cache)
  "Return outline path to current headline.

Outline path is a list of strings, in reverse order.  When
optional argument USE-CACHE is non-nil, make use of a cache.  See
`org-get-outline-path' for details.

Assume buffer is widened and point is on a headline."
  (or (and use-cache (cdr (assq (point) org-outline-path-cache)))
      (let ((p (point))
	    (heading (let ((case-fold-search nil))
		       (looking-at org-complex-heading-regexp)
		       (if (not (match-end 4)) ""
			 ;; Remove statistics cookies.
			 (org-trim
			  (org-link-display-format
			   (replace-regexp-in-string
			    "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]" ""
			    (match-string-no-properties 4))))))))
        (when (org-element-property :commentedp (org-element-at-point))
          (setq heading (replace-regexp-in-string (format "^%s[ \t]*" org-comment-string) "" heading)))
	(if (org-up-heading-safe)
	    (let ((path (cons heading (org--get-outline-path-1 use-cache))))
	      (when use-cache
		(push (cons p path) org-outline-path-cache))
	      path)
	  ;; This is a new root node.  Since we assume we are moving
	  ;; forward, we can drop previous cache so as to limit number
	  ;; of associations there.
	  (let ((path (list heading)))
	    (when use-cache (setq org-outline-path-cache (list (cons p path))))
	    path)))))

(defun org-get-outline-path (&optional with-self use-cache)
  "Return the outline path to the current entry.

An outline path is a list of ancestors for current headline, as
a list of strings.  Statistics cookies are removed and links are
replaced with their description, if any, or their path otherwise.

When optional argument WITH-SELF is non-nil, the path also
includes the current headline.

When optional argument USE-CACHE is non-nil, cache outline paths
between calls to this function so as to avoid backtracking.  This
argument is useful when planning to find more than one outline
path in the same document.  In that case, there are two
conditions to satisfy:
  - `org-outline-path-cache' is set to nil before starting the
    process;
  - outline paths are computed by increasing buffer positions."
  (org-with-wide-buffer
   (and (or (and with-self (org-back-to-heading t))
	    (org-up-heading-safe))
	(reverse (org--get-outline-path-1 use-cache)))))

(defvar org-level-faces)
(defvar org-n-level-faces)
(defun org-format-outline-path (path &optional width prefix separator)
  "Format the outline path PATH for display.
WIDTH is the maximum number of characters that is available.
PREFIX is a prefix to be included in the returned string,
such as the file name.
SEPARATOR is inserted between the different parts of the path,
the default is \"/\"."
  (require 'org-faces)
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

(provide 'org-outline)

;;; org-outline.el ends here
