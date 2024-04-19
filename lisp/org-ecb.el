;;; org-ecb.el --- Org mode ECB integration                      -*- lexical-binding: t; -*-

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

;; This library implements Org mode integration with Emacs Code
;; Browser package.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-fold)

(defun org--ecb-show-context (&rest _)
  "Make hierarchy visible when jumping into location from ECB tree buffer."
  (when (derived-mode-p 'org-mode)
    (org-fold-show-context)))

;; Make sure ecb shows the location if it was hidden
(eval-after-load 'ecb
  '(advice-add 'ecb-method-clicked :after #'org--ecb-show-context))

(provide 'org-ecb)

;;; org-ecb.el ends here
