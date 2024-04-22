;;; org-indirect-buffer.el --- Create narrowed view in indirect buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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
;;
;;  This library implements commands to display parts of current
;;  buffer in indirect buffers.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-fold)
(require 'org-move)
(require 'org-property)
(require 'org-element-context)

(defcustom org-indirect-buffer-display 'other-window
  "How should indirect tree buffers be displayed?

This applies to indirect buffers created with the commands
`org-tree-to-indirect-buffer' and `org-agenda-tree-to-indirect-buffer'.

Valid values are:
current-window   Display in the current window
other-window     Just display in another window.
dedicated-frame  Create one new frame, and reuse it each time.
new-frame        Make a new frame each time.  Note that in this case
                 previously-made indirect buffers are kept, and you need to
                 kill these buffers yourself."
  :group 'org-structure
  :group 'org-agenda-windows
  :type '(choice
	  (const :tag "In current window" current-window)
	  (const :tag "In current frame, other window" other-window)
	  (const :tag "Each time a new frame" new-frame)
	  (const :tag "One dedicated frame" dedicated-frame)))

(defvar org-indirect-dedicated-frame nil
  "This is the frame being used for indirect tree display.")
(defvar org-last-indirect-buffer nil)

;;;###autoload
(defun org-tree-to-indirect-buffer (&optional arg)
  "Create indirect buffer and narrow it to current subtree.

With a numerical prefix ARG, go up to this level and then take that tree.
If ARG is negative, go up that many levels.

If `org-indirect-buffer-display' is not `new-frame', the command removes the
indirect buffer previously made with this command, to avoid proliferation of
indirect buffers.  However, when you call the command with a \
`\\[universal-argument]' prefix, or
when `org-indirect-buffer-display' is `new-frame', the last buffer is kept
so that you can work with several indirect buffers at the same time.  If
`org-indirect-buffer-display' is `dedicated-frame', the \
`\\[universal-argument]' prefix also
requests that a new frame be made for the new buffer, so that the dedicated
frame is not changed."
  (interactive "P")
  (let ((cbuf (current-buffer))
	(cwin (selected-window))
	(pos (point))
	beg end level heading ibuf
        (last-indirect-window
         (and org-last-indirect-buffer
              (get-buffer-window org-last-indirect-buffer))))
    (save-excursion
      (org-back-to-heading t)
      (when (numberp arg)
	(setq level (org-outline-level))
	(when (< arg 0) (setq arg (+ level arg)))
	(while (> (setq level (org-outline-level)) arg)
	  (org-up-heading-safe)))
      (setq beg (point)
	    heading (org-get-heading 'no-tags))
      (org-end-of-subtree t t)
      (when (and (not (eobp)) (org-at-heading-p)) (backward-char 1))
      (setq end (point)))
    (when (and (buffer-live-p org-last-indirect-buffer)
	       (not (eq org-indirect-buffer-display 'new-frame))
	       (not arg))
      (kill-buffer org-last-indirect-buffer))
    (setq ibuf (org-get-indirect-buffer cbuf heading)
	  org-last-indirect-buffer ibuf)
    (cond
     ((or (eq org-indirect-buffer-display 'new-frame)
	  (and arg (eq org-indirect-buffer-display 'dedicated-frame)))
      (select-frame (make-frame))
      (pop-to-buffer ibuf '(org-display-buffer-full-frame))
      (org-set-frame-title heading))
     ((eq org-indirect-buffer-display 'dedicated-frame)
      (raise-frame
       (select-frame (or (and org-indirect-dedicated-frame
			      (frame-live-p org-indirect-dedicated-frame)
			      org-indirect-dedicated-frame)
			 (setq org-indirect-dedicated-frame (make-frame)))))
      (pop-to-buffer ibuf '(org-display-buffer-full-frame))
      (org-set-frame-title (concat "Indirect: " heading)))
     ((eq org-indirect-buffer-display 'current-window)
      (pop-to-buffer-same-window ibuf))
     ((eq org-indirect-buffer-display 'other-window)
      (pop-to-buffer
       ibuf
       `(org-display-buffer-in-window (window . ,last-indirect-window)
                                      (same-frame . t))))
     (t (error "Invalid value")))
    (narrow-to-region beg end)
    (org-fold-show-all '(headings drawers blocks))
    (goto-char pos)
    (run-hook-with-args 'org-cycle-hook 'all)
    (and (window-live-p cwin) (select-window cwin))))

(cl-defun org-get-indirect-buffer (&optional (buffer (current-buffer)) heading)
  "Return an indirect buffer based on BUFFER.
If HEADING, append it to the name of the new buffer."
  (let* ((base-buffer (or (buffer-base-buffer buffer) buffer))
         (buffer-name (generate-new-buffer-name
                       (format "%s%s"
                               (buffer-name base-buffer)
                               (if heading
                                   (concat "::" heading)
                                 ""))))
         (indirect-buffer (make-indirect-buffer base-buffer buffer-name 'clone)))
    ;; Decouple folding state.  We need to do it manually since
    ;; `make-indirect-buffer' does not run
    ;; `clone-indirect-buffer-hook'.
    (org-fold-core-decouple-indirect-buffer-folds)
    indirect-buffer))

(defun org-set-frame-title (title)
  "Set the title of the current frame to the string TITLE."
  (modify-frame-parameters (selected-frame) (list (cons 'name title))))

(provide 'org-indirect-buffer)
;;; org-indirect-buffer.el ends here
