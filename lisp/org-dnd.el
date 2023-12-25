;;; org-dnd.el --- Org drag-and-drop support               -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>

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

;; This file implements DnD support for Org mode.

;;; Code:

(require 'ol)

(declare-function org-attach-dir "org-attach"
		  (&optional create-if-not-exists-p no-fs-check))


(defun org-setup-yank-dnd-handlers ()
  "Setup the `yank-media' and DND handlers for buffer."
  (setq-local dnd-protocol-alist
              (cons (cons "^file:///"
                          (if (>= emacs-major-version 30)
                              #'org--dnd-multi-local-file-handler
                            #'org--dnd-local-file-handler))
                    dnd-protocol-alist))
  (when (fboundp 'yank-media-handler)
    (yank-media-handler "image/.*" #'org--image-yank-media-handler)
    ;; Looks like different DEs go for different handler names,
    ;; https://larsee.com/blog/2019/05/clipboard-files/.
    (yank-media-handler "x/special-\\(?:gnome\|KDE\|mate\\)-files"
                        #'org--copied-files-yank-media-handler))
  (when (boundp 'x-dnd-direct-save-function)
    (setq-local x-dnd-direct-save-function #'org--dnd-xds-function)))

(defcustom org-yank-image-save-method 'attach
  "Method to save images yanked from clipboard and dropped to Emacs.
It can be the symbol `attach' to add it as an attachment, or a
directory name to copy/cut the image to that directory."
  :group 'org
  :package-version '(Org . "9.7")
  :type '(choice (const :tag "Add it as attachment" attach)
                 (directory :tag "Save it in directory"))
  :safe (lambda (x) (eq x 'attach)))

(defcustom org-yank-image-file-name-function #'org-yank-image-autogen-filename
  "Function to generate filename for image yanked from clipboard.
By default, this autogenerates a filename based on the current
time.
It is called with no arguments and should return a string without
any extension which is used as the filename."
  :group 'org
  :package-version '(Org . "9.7")
  :type '(radio (function-item :doc "Autogenerate filename"
                               org-yank-image-autogen-filename)
                (function-item :doc "Ask for filename"
                               org-yank-image-read-filename)
                function))

(defun org-yank-image-autogen-filename ()
  "Autogenerate filename for image in clipboard."
  (format-time-string "clipboard-%Y%m%dT%H%M%S.%6N"))

(defun org-yank-image-read-filename ()
  "Read filename for image in clipboard."
  (read-string "Basename for image file without extension: "))

(declare-function org-attach-attach "org-attach" (file &optional visit-dir method))

(defun org--image-yank-media-handler (mimetype data)
  "Save image DATA of mime-type MIMETYPE and insert link at point.
It is saved as per `org-yank-image-save-method'.  The name for the
image is prompted and the extension is automatically added to the
end."
  (let* ((ext (symbol-name (mailcap-mime-type-to-extension mimetype)))
         (iname (funcall org-yank-image-file-name-function))
         (filename (file-name-with-extension iname ext))
         (absname (expand-file-name
                   filename
                   (if (eq org-yank-image-save-method 'attach)
                       temporary-file-directory
                     org-yank-image-save-method)))
         link)
    (when (and (not (eq org-yank-image-save-method 'attach))
               (not (file-directory-p org-yank-image-save-method)))
      (make-directory org-yank-image-save-method t))
    (with-temp-file absname
      (insert data))
    (if (null (eq org-yank-image-save-method 'attach))
        (setq link (org-link-make-string (concat "file:" (file-relative-name absname))))
      (require 'org-attach)
      (org-attach-attach absname nil 'mv)
      (setq link (org-link-make-string (concat "attachment:" filename))))
    (insert link)))

;; I cannot find a spec for this but
;; https://indigo.re/posts/2021-12-21-clipboard-data.html and pcmanfm
;; suggests that this is the format.
(defun org--copied-files-yank-media-handler (_mimetype data)
  "Handle copied or cut files from file manager.
They are handled as per `org-yank-dnd-method'.
DATA is a string where the first line is the operation to
perform: copy or cut.  Rest of the lines are file: links to the
concerned files."
  ;; pcmanfm adds a null byte at the end for some reason.
  (let* ((data (split-string data "[\0\n\r]" t))
         (files (cdr data))
         (action (if (equal (car data) "cut")
                     'copy
                   'move))
         (sep (if (= (length files) 1) "" " ")))
    (dolist (f files)
      (if (file-readable-p f)
          (org--dnd-local-file-handler f action sep)
        (message "File `%s' is not readable, skipping" f)))))

(defcustom org-yank-dnd-method 'ask
  "Action to perform on the dropped and the pasted files.
When the value is the symbol,
  . `attach' -- attach dropped/pasted file
  . `open' -- visit/open dropped/pasted file in Emacs
  . `file-link' -- insert file: link to dropped/pasted file
  . `ask' -- ask what to do out of the above."
  :group 'org
  :package-version '(Org . "9.7")
  :type '(choice (const :tag "Attach" attach)
                 (const :tag "Open/Visit file" open)
                 (const :tag "Insert file: link" file-link)
                 (const :tag "Ask what to do" ask)))

(defcustom org-yank-dnd-default-attach-method nil
  "Default attach method to use when DND action is unspecified.
This attach method is used when the DND action is `private'.
This is also used when `org-yank-image-save-method' is nil.
When nil, use `org-attach-method'."
  :group 'org
  :package-version '(Org . "9.7")
  :type '(choice (const :tag "Default attach method" nil)
                 (const :tag "Copy" cp)
                 (const :tag "Move" mv)
                 (const :tag "Hard link" ln)
                 (const :tag "Symbolic link" lns)))

(declare-function mailcap-file-name-to-mime-type "mailcap" (file-name))
(defvar org-attach-method)

(defun org--dnd-rmc (prompt choices)
  (if (null (use-dialog-box-p))
      (caddr (read-multiple-choice prompt choices))
    (setq choices
          (mapcar
           (pcase-lambda (`(_key ,message ,val))
             (cons (capitalize message) val))
           choices))
    (x-popup-menu t (list prompt (cons "" choices)))))

(defun org--dnd-multi-local-file-handler (urls action)
  "Handle file URLS as per ACTION.
URLS is a list of file URL."
  (let ((sep (if (= (length urls) 1) "" " ")))
    (dolist (u urls)
      (org--dnd-local-file-handler u action sep))))

(put 'org--dnd-multi-local-file-handler 'dnd-multiple-handler t)

(defun org--dnd-local-file-handler (url action &optional separator)
  "Handle file URL as per ACTION.
SEPARATOR is the string to insert after each link.  It may be nil
in which case, space is inserted."
  (unless separator
    (setq separator " "))
  (let ((method (if (eq org-yank-dnd-method 'ask)
                    (org--dnd-rmc
                     "What to do with file?"
                     '((?a "attach" attach)
                       (?o "open" open)
                       (?f "insert file: link" file-link)))
                  org-yank-dnd-method)))
    (pcase method
      (`attach (org--dnd-attach-file url action separator))
      (`open (dnd-open-local-file url action))
      (`file-link
       (let ((filename (dnd-get-local-file-name url)))
         (insert (org-link-make-string (concat "file:" filename)) separator))))))

(defun org--dnd-attach-file (url action separator)
  "Attach filename given by URL using method pertaining to ACTION.
If ACTION is `move', use `mv' attach method.
If `copy', use `cp' attach method.
If `ask', ask the user.
If `private', use the method denoted in `org-yank-dnd-default-attach-method'.
The action `private' is always returned.

SEPARATOR is the string to insert after each link."
  (require 'mailcap)
  (let* ((filename (dnd-get-local-file-name url))
         (mimetype (mailcap-file-name-to-mime-type filename))
         (separatep (and (string-prefix-p "image/" mimetype)
                         (not (eq 'attach org-yank-image-save-method))))
         (method (pcase action
                   ('copy 'cp)
                   ('move 'mv)
                   ('ask (org--dnd-rmc
                          "Attach using method"
                          '((?c "copy" cp)
                            (?m "move" mv)
                            (?l "hard link" ln)
                            (?s "symbolic link" lns))))
                   ('private (or org-yank-dnd-default-attach-method
                                 org-attach-method)))))
    (if separatep
        (funcall
         (pcase method
           ('cp #'copy-file)
           ('mv #'rename-file)
           ('ln #'add-name-to-file)
           ('lns #'make-symbolic-link))
         filename
         (expand-file-name (file-name-nondirectory filename)
                           org-yank-image-save-method))
      (org-attach-attach filename nil method))
    (insert
     (org-link-make-string
      (concat (if separatep
                  "file:"
                "attachment:")
              (if separatep
                  (expand-file-name (file-name-nondirectory filename)
                                    org-yank-image-save-method)
                (file-name-nondirectory filename))))
     separator)
    'private))

(defvar-local org--dnd-xds-method nil
  "The method to use for dropped file.")
(defun org--dnd-xds-function (need-name filename)
  "Handle file with FILENAME dropped via XDS protocol.
When NEED-NAME is t, FILNAME is the base name of the file to be
saved.
When NEED-NAME is nil, the drop is complete."
  (if need-name
      (let ((method (if (eq org-yank-dnd-method 'ask)
                        (org--dnd-rmc
                         "What to do with dropped file?"
                         '((?a "attach" attach)
                           (?o "open" open)
                           (?f "insert file: link" file-link)))
                      org-yank-dnd-method)))
        (setq-local org--dnd-xds-method method)
        (pcase method
          (`attach (expand-file-name filename (org-attach-dir 'create)))
          (`open (expand-file-name (make-temp-name "emacs.") temporary-file-directory))
          (`file-link (read-file-name "Write file to: " nil nil nil filename))))
    (pcase org--dnd-xds-method
      (`attach (insert (org-link-make-string
                        (concat "attachment:" (file-name-nondirectory filename)))))
      (`file-link (insert (org-link-make-string (concat "file:" filename))))
      (`open (find-file filename)))
    (setq-local org--dnd-xds-method nil)))

(provide 'org-dnd)
;;; org-dnd.el ends here
