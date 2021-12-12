;;; test-org-element-parser.el --- Tests for org-element.el parser

;; Copyright (C) 2021  Ihor Radchenko

;; Author: Ihor Radchenko <yantar92 at gmail dot com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'org-element)

(defvar test-org-element-parser-properties
  '((:global :begin :end :contents-begin :contents-end :pre-blank :post-blank :post-affiliated)
    (headline :raw-value :title :level :priority :tags :todo-keyword :todo-type :footnote-section-p :archivedp :commentedp))
  "List of important properties that should be parsed.")

(defvar test-org-element-parser-source-directory "../lisp/test-org-element-parser-sources/"
  "Path to directory containing all the test Org files.
The expected parsed representation is stored alongside in .el files.
For example, parsed representation of file.org is in file.el.")

(defun test-org-element-parser-generate-syntax-sexp ()
  "Return SEXP with important parts of parsed representation of current Org buffer."
  (unless (derived-mode-p 'org-mode) (user-error "Not an Org buffer."))
  (let ((datum (org-element-parse-buffer 'object))
	(strip-func (lambda (el)
		      (let ((type (org-element-type el))
			    (plist (when (listp el) (nth 1 el)))
			    prop value tmpalist)
                        (if (eq type 'plain-text)
                            (set-text-properties 0 (length el) nil el)
			  (while plist
			    (setq prop (car plist))
			    (setq value (cadr plist))
			    (when (stringp value) (setq value (substring-no-properties value)))
			    (setq plist (cddr plist))
			    (when (or (memq prop (alist-get :global test-org-element-parser-properties))
				      (memq prop (alist-get type test-org-element-parser-properties)))
                              (push (cons prop value) tmpalist)))
                          (setq tmpalist (sort tmpalist (lambda (a b) (string< (symbol-name (car a))
                                                                          (symbol-name (car b))))))
			  (setf (nth 1 el)
                                (apply #'append
                                       (mapcar (lambda (c) (list (car c) (cdr c)))
                                               tmpalist))))))))
    (org-element-map datum (append '(plain-text)  org-element-all-elements org-element-all-objects)
      strip-func nil nil nil 'with-affiliated)
    ;; `org-element-map' never maps over `org-data'. Update it separately.
    (funcall strip-func datum)
    datum))

(defun test-org-element-parser-save-expected-result (&optional file)
  "Save reference parsed representation of current Org buffer or FILE.
The parsed representation will be saved alongside with the buffer file."
  (interactive)
  (with-current-buffer (if file
			   (get-buffer-create file)
                         (current-buffer))
    (save-buffer)
    (let ((datum (test-org-element-parser-generate-syntax-sexp))
	  (path (buffer-file-name))
          newpath)
      (unless (and path (file-exists-p path)) (user-error "Not in a file buffer."))
      (setq newpath (format "%s.el" (file-name-base path)))
      (with-temp-file newpath
        (condition-case err
            (progn
	      (pp datum (current-buffer))
              (message "Parsed representation saved to %s" (expand-file-name newpath)))
          (err (message "Failed to save parsed representation: \"%S\"" err)))))))

(defmacro org-test-element-verify (&optional file)
  "Verify `org-element-parse-buffer' for current Org buffer or FILE."
  `(progn
     (unless ,file
       (setq file (buffer-file-name))
       (save-buffer))
     (unless (and ,file (file-exists-p ,file))
       (user-error "%s does not exist." ,file))
     (let ((reference-file (format "%s%s.el"
                                   (file-name-directory ,file)
                                   (file-name-base ,file))))
       (unless (file-exists-p reference-file)
         (user-error "Reference result file %s does not exist." reference-file))
       (with-temp-buffer
         (insert-file-contents ,file)
         (org-mode)
         (should
          (equal (test-org-element-parser-generate-syntax-sexp)
	         (with-temp-buffer
                   (insert-file-contents reference-file)
	           (read (current-buffer)))))))))

(defmacro test-org-element-parser-files (&rest files)
  "Run `org-test-element-verify' for each file in FILES."
  `(progn
     (unless (and test-org-element-parser-source-directory
                  (file-exists-p test-org-element-parser-source-directory))
       (error "%s does not exist." test-org-element-parser-source-directory))
     (dolist (file '(,@files))
       (setq file (format "%s%s.org"
                          (file-name-as-directory test-org-element-parser-source-directory)
                          (file-name-base file)))
       (org-test-element-verify file))))



(ert-deftest test-org-element-parser/simple-headlines ()
  "Basic tests for Org files with headings and plain text paragraphs."
  (test-org-element-parser-files "simple-heading"))

(ert-deftest test-org-element-parser/README ()
  "Test README.org in the example file repo."
  (test-org-element-parser-files "README"))

(provide 'test-org-element-parser)
;;; test-org-element-parser.el ends here
