;;; org-narrow.el --- Org narrowing commands                      -*- lexical-binding: t; -*-

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

;; This library implements Org narrowing commands.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element)

;;;###autoload
(defun org-narrow-to-subtree (&optional element)
  "Narrow buffer to the current subtree.
Use the command `\\[widen]' to see the whole buffer again.
With optional argument ELEMENT narrow to subtree around ELEMENT."
  (interactive)
  (let* ((heading
          (org-element-lineage
           (or element (org-element-at-point))
           'headline 'with-self))
         (begin (org-element-begin heading))
         (end (org-element-end heading)))
    (if (and heading end
             ;; Preserve historical behavior throwing an error when
             ;; current heading starts before active narrowing.
             (<= (point-min) begin))
        (narrow-to-region
         begin
         ;; Preserve historical behavior not extending the active
         ;; narrowing when the subtree extends beyond it.
         (min (point-max)
              (if (= end (point-max))
                  end (1- end))))
      (signal 'outline-before-first-heading nil))))

;;;###autoload
(defun org-toggle-narrow-to-subtree ()
  "Narrow to the subtree at point or widen a narrowed buffer.
Use the command `\\[widen]' to see the whole buffer again."
  (interactive)
  (if (buffer-narrowed-p)
      (progn (widen) (message "Buffer widen"))
    (org-narrow-to-subtree)
    (message "Buffer narrowed to current subtree")))

;;;###autoload
(defun org-narrow-to-block ()
  "Narrow buffer to the current block.
Use the command `\\[widen]' to see the whole buffer again."
  (interactive)
  (let* ((case-fold-search t)
         (element (org-element-at-point)))
    (if (string-match-p "block" (symbol-name (org-element-type element)))
        (org-narrow-to-element)
      (user-error "Not in a block"))))

;;;###autoload
(defun org-narrow-to-element ()
  "Narrow buffer to current element.
Use the command `\\[widen]' to see the whole buffer again."
  (interactive)
  (let ((elem (org-element-at-point)))
    (cond
     ((eq (car elem) 'headline)
      (narrow-to-region
       (org-element-begin elem)
       (org-element-end elem)))
     ((memq (car elem) org-element-greater-elements)
      (narrow-to-region
       (org-element-contents-begin elem)
       (org-element-contents-end elem)))
     (t
      (narrow-to-region
       (org-element-begin elem)
       (org-element-end elem))))))

(provide 'org-narrow)

;;; org-narrow.el ends here
