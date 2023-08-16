;;; test-org-version.el --- Test Org version consistency  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ihor Radchenko

;; Author: Ihor Radchenko <yantar92@posteo.net>
;; Keywords: internal

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

;;; Commentary:

;; This file implements internal checks for Org versioning.

;;; Code:

(require 'org-version)

(ert-deftest test-org-version/provide ()
  "Test versioned `provide' calls in Org libraries."
  (let* (org-library)
    (dolist (org-library-file
             (directory-files
              (expand-file-name
	       (concat org-test-dir "../lisp"))
              t "\\.el$"))
      (setq org-library
            (file-name-nondirectory
             (file-name-sans-extension org-library-file)))
      (unless (member org-library '("org-loaddefs" "org-version"))
        (with-temp-buffer
          (insert-file-contents org-library-file)
          (goto-char (point-max))
          (should (re-search-backward
                   (rx-to-string
                    `(seq
                      bol (0+ space)
                      "(provide" (0+ space)
                      "'" ,(concat org-library "-" (org-release))
                      (0+ space) ")"))
                   nil t)))))))

(provide 'test-org-version)
;;; test-org-version.el ends here
