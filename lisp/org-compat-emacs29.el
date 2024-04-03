;;; org-compat-emacs29.el --- Compatibility Code for Older Emacsen -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, wp
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

;; This file contains code needed for compatibility with older
;; versions of GNU Emacs and integration with other packages.
;;; Emacs < 29 compatibility

;;; Code:

(if (fboundp 'display-buffer-full-frame)
    (defalias 'org-display-buffer-full-frame #'display-buffer-full-frame)
  (defun org-display-buffer-full-frame (buffer alist)
    "Display BUFFER in the current frame, taking the entire frame.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
    (when-let ((window (or (display-buffer-reuse-window buffer alist)
                           (display-buffer-same-window buffer alist)
                           (display-buffer-pop-up-window buffer alist)
                           (display-buffer-use-some-window buffer alist))))
      (delete-other-windows window)
      window)))

(defvar org-file-has-changed-p--hash-table (make-hash-table :test #'equal)
  "Internal variable used by `org-file-has-changed-p'.")

(if (fboundp 'file-has-changed-p)
    (defalias 'org-file-has-changed-p #'file-has-changed-p)
  (defun org-file-has-changed-p (file &optional tag)
    "Return non-nil if FILE has changed.
The size and modification time of FILE are compared to the size
and modification time of the same FILE during a previous
invocation of `org-file-has-changed-p'.  Thus, the first invocation
of `org-file-has-changed-p' always returns non-nil when FILE exists.
The optional argument TAG, which must be a symbol, can be used to
limit the comparison to invocations with identical tags; it can be
the symbol of the calling function, for example."
    (let* ((file (directory-file-name (expand-file-name file)))
           (remote-file-name-inhibit-cache t)
           (fileattr (file-attributes file 'integer))
	   (attr (and fileattr
                      (cons (file-attribute-size fileattr)
		            (file-attribute-modification-time fileattr))))
	   (sym (concat (symbol-name tag) "@" file))
	   (cachedattr (gethash sym org-file-has-changed-p--hash-table)))
      (when (not (equal attr cachedattr))
        (puthash sym attr org-file-has-changed-p--hash-table)))))

(if (fboundp 'string-equal-ignore-case)
    (defalias 'org-string-equal-ignore-case #'string-equal-ignore-case)
  ;; From Emacs subr.el.
  (defun org-string-equal-ignore-case (string1 string2)
    "Like `string-equal', but case-insensitive.
Upper-case and lower-case letters are treated as equal.
Unibyte strings are converted to multibyte for comparison."
    (eq t (compare-strings string1 0 nil string2 0 nil t))))

(defun org-buffer-text-pixel-width ()
  "Return pixel width of text in current buffer.
This function uses `buffer-text-pixel-size', when available, and falls
back to `window-text-pixel-size' otherwise."
  (if (fboundp 'buffer-text-pixel-size)
      (car (buffer-text-pixel-size nil nil t))
    (if (get-buffer-window (current-buffer))
        ;; FIXME: 10000 because `most-positive-fixnum' ain't working
        ;; (tests failing) and this call will be removed after we drop
        ;; Emacs 28 support anyway.
        (car (window-text-pixel-size
              nil (point-min) (point-max) 10000))
      (let ((dedicatedp (window-dedicated-p))
            (oldbuffer (window-buffer)))
        (unwind-protect
            (progn
              ;; Do not throw error in dedicated windows.
              (set-window-dedicated-p nil nil)
              (set-window-buffer nil (current-buffer))
              (car (window-text-pixel-size
                    nil (point-min) (point-max) 10000)))
          (set-window-buffer nil oldbuffer)
          (set-window-dedicated-p nil dedicatedp))))))

(if (version< emacs-version "29")
    ;; A stub when `combine-change-calls' was not yet there or had
    ;; critical bugs (see Emacs bug#60467).
    (defmacro org-combine-change-calls (_beg _end &rest body)
      (declare (debug (form form def-body)) (indent 2))
      `(progn ,@body))
  (defalias 'org-combine-change-calls 'combine-change-calls))

(provide 'org-compat-emacs29)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-compat-emacs29.el ends here
