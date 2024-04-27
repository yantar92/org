;;; org-table-header-line-mode.el --- Display header line in tall Org tables        -*- lexical-binding: t; -*-

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

;; This file implements a minor mode to display the table header when
;; the table spans more than one window height.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-table-core)

(defun org-table-row-get-visible-string (&optional pos)
  "Get the visible string of a table row.
This may be useful when columns have been shrunk."
  (save-excursion
    (when pos (goto-char pos))
    (let* ((beg (line-beginning-position))
           (end (line-end-position))
           (str (buffer-substring beg end)))
      ;; FIXME: This does not handle intersecting overlays.
      (dolist (ov (overlays-in beg end))
        (when (overlay-get ov 'display)
          (put-text-property
           (- (overlay-start ov) beg) (- (overlay-end ov) beg)
           'display (overlay-get ov 'display)
           str)))
      str)))

(defvar-local org-table-header-overlay nil)
(put 'org-table-header-overlay 'permanent-local t)
(defun org-table-header-set-header ()
  "Display the header of the table at point."
  (let ((gcol temporary-goal-column))
    (unwind-protect
        (progn
          (when (overlayp org-table-header-overlay)
            (delete-overlay org-table-header-overlay))
          ;; We might be called after scrolling but before display is
          ;; updated. Make sure that any queued redisplay is executed
          ;; before we look into `window-start'.
          (redisplay)
          (let* ((ws (window-start))
                 (beg (save-excursion
                        ;; Check table at window start, not at point.
                        ;; Point might be after the table, or at
                        ;; another table located below the one visible
                        ;; on top.
                        (goto-char ws)
                        (goto-char (org-table-begin))
                        (while (or (org-at-table-hline-p)
                                   (looking-at-p ".*|\\s-+<[rcl]?\\([0-9]+\\)?>"))
                          (move-beginning-of-line 2))
                        (line-beginning-position))))
            (if (pos-visible-in-window-p beg)
                (when (overlayp org-table-header-overlay)
                  (delete-overlay org-table-header-overlay))
              (setq org-table-header-overlay
                    (make-overlay
                     (save-excursion (goto-char ws) (line-beginning-position))
                     (save-excursion (goto-char ws) (line-end-position))))
              (org-overlay-display
               org-table-header-overlay
               (org-table-row-get-visible-string beg)
               'org-table-header))))
      (setq temporary-goal-column gcol))))

;;;###autoload
(define-minor-mode org-table-header-line-mode
  "Display the first row of the table at point in the header line."
  :lighter " TblHeader"
  (unless (eq major-mode 'org-mode)
    (user-error "Cannot turn org table header mode outside org-mode buffers"))
  (if org-table-header-line-mode
      (add-hook 'post-command-hook #'org-table-header-set-header nil t)
    (when (overlayp org-table-header-overlay)
      (delete-overlay org-table-header-overlay)
      (setq org-table-header-overlay nil))
    (remove-hook 'post-command-hook #'org-table-header-set-header t)))

(provide 'org-table-header-line-mode)

;;; org-table-header-line-mode.el ends here
