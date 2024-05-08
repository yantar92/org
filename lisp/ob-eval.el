;;; ob-eval.el --- Babel Functions for External Code Evaluation -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2024 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research, comint
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

;; These functions build existing Emacs support for executing external
;; shell commands.

;;; Code:

(require 'org-macs)
(org-assert-version)

(eval-when-compile (require 'subr-x))  ; For `string-empty-p', Emacs < 29

;;; Temporary staging area for script evaluation in tmp dir.

(defvar org-babel-temporary-directory
  (unless noninteractive
    (make-temp-file "babel-" t))
  "Directory to hold temporary files created to execute code blocks.
Used by `org-babel-temp-file'.  This directory will be removed on
Emacs shutdown.")

(defvar org-babel-temporary-stable-directory
  (unless noninteractive
    (let (dir)
      (while (or (not dir) (file-exists-p dir))
        (setq dir (expand-file-name
                   (format "babel-stable-%d" (random 1000))
                   temporary-file-directory)))
      (make-directory dir)
      dir))
  "Directory to hold temporary files created to execute code blocks.
Used by `org-babel-temp-file'.  This directory will be removed on
Emacs shutdown.")

(defcustom org-babel-remote-temporary-directory "/tmp/"
  "Directory to hold temporary files on remote hosts."
  :group 'org-babel
  :type 'string)

(defmacro org-babel-temp-directory ()
  "Return temporary directory suitable for `default-directory'."
  `(if (file-remote-p default-directory)
       (concat (file-remote-p default-directory)
	       org-babel-remote-temporary-directory)
     (or (and org-babel-temporary-directory
	      (file-exists-p org-babel-temporary-directory)
	      org-babel-temporary-directory)
	 temporary-file-directory)))

(defun org-babel-temp-file (prefix &optional suffix)
  "Create a temporary file in the `org-babel-temporary-directory'.
Passes PREFIX and SUFFIX directly to `make-temp-file' with the
value of function `temporary-file-directory' temporarily set to the
value of `org-babel-temporary-directory'."
  (make-temp-file
   (concat (file-name-as-directory (org-babel-temp-directory)) prefix)
   nil
   suffix))

(defmacro org-babel-temp-stable-directory ()
  "Return temporary stable directory."
  `(let ((org-babel-temporary-directory org-babel-temporary-stable-directory))
     (org-babel-temp-directory)))

(defun org-babel-temp-stable-file (data prefix &optional suffix)
  "Create a temporary file in the `org-babel-remove-temporary-stable-directory'.
The file name is stable with respect to DATA.  The file name is
constructed like the following: <PREFIX><DATAhash><SUFFIX>."
  (let ((path
         (format
          "%s%s%s%s"
          (file-name-as-directory (org-babel-temp-stable-directory))
          prefix
          (org-sxhash-safe data)
          (or suffix ""))))
    ;; Create file.
    (with-temp-file path)
    ;; Return it.
    path))

(defun org-babel-remove-temporary-directory ()
  "Remove `org-babel-temporary-directory' on Emacs shutdown."
  (when (and org-babel-temporary-directory
	     (file-exists-p org-babel-temporary-directory))
    ;; taken from `delete-directory' in files.el
    (condition-case nil
	(progn
	  (mapc (lambda (file)
		  ;; This test is equivalent to
		  ;; (and (file-directory-p fn) (not (file-symlink-p fn)))
		  ;; but more efficient
		  (if (eq t (car (file-attributes file)))
		      (delete-directory file)
		    (delete-file file)))
		(directory-files org-babel-temporary-directory 'full
				 directory-files-no-dot-files-regexp))
	  (delete-directory org-babel-temporary-directory))
      (error
       (message "Failed to remove temporary Org-babel directory %s"
		(or org-babel-temporary-directory
		    "[directory not defined]"))))))

(defun org-babel-remove-temporary-stable-directory ()
  "Remove `org-babel-temporary-stable-directory' and on Emacs shutdown."
  (when (and org-babel-temporary-stable-directory
	     (file-exists-p org-babel-temporary-stable-directory))
    (let ((org-babel-temporary-directory
           org-babel-temporary-stable-directory))
      (org-babel-remove-temporary-directory))))

(add-hook 'kill-emacs-hook #'org-babel-remove-temporary-directory)
(add-hook 'kill-emacs-hook #'org-babel-remove-temporary-stable-directory)

;;; Code evaluation helpers

(defvar org-babel-error-buffer-name "*Org-Babel Error Output*"
  "The buffer name Org Babel evaluate error output.")

(defun org-babel-eval-error-notify (exit-code stderr)
  "Open a buffer to display STDERR and a message with the value of EXIT-CODE.
If EXIT-CODE is nil, display the message without a code."
  (let ((buf (get-buffer-create org-babel-error-buffer-name)))
    (with-current-buffer buf
      (goto-char (point-max))
      (save-excursion
        (unless (bolp) (insert "\n"))
        (insert stderr)
        (if exit-code
            (insert (format "[ Babel evaluation exited with code %S ]" exit-code))
          (insert "[ Babel evaluation exited abnormally ]"))))
    (display-buffer buf))
  (if exit-code
      (message "Babel evaluation exited with code %S" exit-code)
    (message "Babel evaluation exited abnormally")))

(defun org-babel-eval (command query)
  "Run COMMAND on QUERY.
Return standard output produced by COMMAND.  If COMMAND exits
with a non-zero code or produces error output, show it with
`org-babel-eval-error-notify'.

Writes QUERY into a temp-buffer that is processed with
`org-babel--shell-command-on-region'."
  (let ((error-buffer (get-buffer-create " *Org-Babel Error*")) exit-code)
    (with-current-buffer error-buffer (erase-buffer))
    (with-temp-buffer
      ;; Ensure trailing newline.  It is required for cmdproxy.exe.
      (insert query "\n")
      (setq exit-code
            (org-babel--shell-command-on-region
             command error-buffer))
      (let ((stderr (with-current-buffer error-buffer (buffer-string))))
        (if (or (not (numberp exit-code))
                (> exit-code 0)
                (not (string-empty-p stderr)))
            (progn
              (org-babel-eval-error-notify exit-code stderr)
              (save-excursion
                (when (get-buffer org-babel-error-buffer-name)
                  (with-current-buffer org-babel-error-buffer-name
                    (unless (derived-mode-p 'compilation-mode)
                      (compilation-mode))
                    ;; Compilation-mode enforces read-only, but
                    ;; Babel expects the buffer modifiable.
                    (setq buffer-read-only nil))))
              ;; Return output, if any.
              (buffer-string))
          (buffer-string))))))

(defun org-babel-eval-read-file (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer (insert-file-contents file)
		    (buffer-string)))

(defun org-babel--shell-command-on-region (command error-buffer)
  "Execute COMMAND in an inferior shell with region as input.
Stripped down version of `shell-command-on-region' for internal use in
Babel only.  This lets us work around errors in the original function
in various versions of Emacs.  This expects the query to be run to be
in the current temp buffer.  This is written into
input-file.  ERROR-BUFFER is the name of the file which
`org-babel-eval' has created to use for any error messages that are
returned."

  (let ((input-file (org-babel-temp-file "ob-input-"))
	(error-file (if error-buffer (org-babel-temp-file "ob-error-") nil))
	(shell-file-name (org-babel--get-shell-file-name))
	exit-status)
    ;; we always call this with 'replace, remove conditional
    ;; Replace specified region with output from command.
    (org-babel--write-temp-buffer-input-file input-file)
    (setq exit-status
	  (process-file shell-file-name input-file
			(if error-file
			    (list t error-file)
			  t)
			nil shell-command-switch command))

    (when (and input-file (file-exists-p input-file)
	       ;; bind org-babel--debug-input around the call to keep
	       ;; the temporary input files available for inspection
	       (not (when (boundp 'org-babel--debug-input)
		      org-babel--debug-input)))
      (delete-file input-file))

    (when (and error-file (file-exists-p error-file))
      (when (< 0 (file-attribute-size (file-attributes error-file)))
	(with-current-buffer (get-buffer-create error-buffer)
	  (let ((pos-from-end (- (point-max) (point))))
	    (or (bobp)
		(insert "\f\n"))
	    ;; Do no formatting while reading error file,
	    ;; because that can run a shell command, and we
	    ;; don't want that to cause an infinite recursion.
	    (format-insert-file error-file nil)
	    ;; Put point after the inserted errors.
	    (goto-char (- (point-max) pos-from-end)))
	  (current-buffer)))
      (delete-file error-file))
    exit-status))

(defun org-babel--write-temp-buffer-input-file (input-file)
  "Write the contents of the current temp buffer into INPUT-FILE."
  (let ((start (point-min))
        (end (point-max)))
    (goto-char start)
    (push-mark (point) 'nomsg)
    (write-region start end input-file)
    (delete-region start end)
    (exchange-point-and-mark)))

(defun org-babel-eval-wipe-error-buffer ()
  "Delete the contents of the Org code block error buffer.
This buffer is named by `org-babel-error-buffer-name'."
  (when (get-buffer org-babel-error-buffer-name)
    (with-current-buffer org-babel-error-buffer-name
      (delete-region (point-min) (point-max)))))

(defun org-babel--get-shell-file-name ()
  "Return system `shell-file-name', defaulting to /bin/sh.
Unfortunately, `executable-find' does not support file name
handlers.  Therefore, we could use it in the local case only."
  ;; FIXME: Since Emacs 27, `executable-find' accepts optional second
  ;; argument supporting remote hosts.
  (cond ((and (not (file-remote-p default-directory))
	      (executable-find shell-file-name))
	 shell-file-name)
	((file-executable-p
	  (concat (file-remote-p default-directory) shell-file-name))
	 shell-file-name)
	("/bin/sh")))

(provide 'ob-eval)

;;; ob-eval.el ends here
