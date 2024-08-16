;;; org-keys.el --- Key bindings for Org mode        -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>

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

;; This library adds bindings for Org mode buffers.  It also
;; implements both Speed keys and Babel speed keys.  See manual for
;; details.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'cl-lib)

(require 'org-regexps)
(require 'org-element)
(require 'org-element-context)

;; All the below functions should be auto-loaded.
(declare-function org-add-note "org-log-note" ())
(declare-function org-agenda "org-agenda-mode" (&optional arg org-keys restriction))
(declare-function org-agenda-file-to-front "org-agenda-files" (&optional to-end))
(declare-function org-agenda-remove-restriction-lock "org-agenda-files" (&optional noupdate))
(declare-function org-agenda-set-restriction-lock "org-agenda-files" (&optional type))
(declare-function org-archive-subtree "org-archive" (&optional find-done))
(declare-function org-archive-subtree-default "org-archive" ())
(declare-function org-archive-subtree-default-with-confirmation "org-archive" ())
(declare-function org-archive-to-archive-sibling "org-archive" ())
(declare-function org-attach "org-attach" ())
(declare-function org-backward-element "org-move" ())
(declare-function org-backward-sentence "org-move" (&optional arg))
(declare-function org-backward-heading-same-level "org-move" (arg &optional invisible-ok))
(declare-function org-backward-paragraph "org-move" ())
(declare-function org-beginning-of-line "org-move" (&optional n))
(declare-function org-clock-cancel "org-clock" ())
(declare-function org-clock-display "org-clock" (&optional arg))
(declare-function org-clock-goto "org-clock" (&optional select))
(declare-function org-clock-in "org-clock" (&optional select start-time))
(declare-function org-clock-in-last "org-clock" (&optional arg))
(declare-function org-clock-out "org-clock" (&optional switch-to-state fail-quietly at-time))
(declare-function org-clock-modify-effort-estimate "org-clock-commands" (&optional value))
(declare-function org-clone-subtree-with-time-shift "org-edit-structure" (n &optional shift))
(declare-function org-columns "org-colview" (&optional global columns-fmt-string))
(declare-function org-comment-dwim "org-comment" (arg))
(declare-function org-copy-special "org-edit" ())
(declare-function org-copy-visible "org-edit" (beg end))
(declare-function org-ctrl-c-ctrl-c "org-edit-special" (&optional arg))
(declare-function org-ctrl-c-minus "org-edit-special" ())
(declare-function org-ctrl-c-ret "org-edit-special" ())
(declare-function org-ctrl-c-star "org-edit-special" ())
(declare-function org-ctrl-c-tab "org-edit-special" (&optional arg))
(declare-function org-cut-special "org-edit" ())
(declare-function org-cut-subtree "org-edit-structure" (&optional n))
(declare-function org-cycle "org-cycle" (&optional arg))
(declare-function org-cycle-agenda-files "org-agenda-files" ())
(declare-function org-date-from-calendar "org-misc" ())
(declare-function org-dynamic-block-insert-dblock "org-dblock" (&optional arg))
(declare-function org-dblock-update "org-dblock" (&optional arg))
(declare-function org-deadline "org-planning" (arg &optional time))
(declare-function org-decrease-number-at-point "org-misc" (&optional inc))
(declare-function org-delete-backward-char "org-edit" (n))
(declare-function org-delete-char "org-edit" (n))
(declare-function org-delete-indentation "org-edit" (&optional arg beg end))
(declare-function org-demote-subtree "org-edit-structure" ())
(declare-function org-display-outline-path "org-property" (&optional file current separator just-return-string))
(declare-function org-down-element "org-move" ())
(declare-function org-edit-special "org-edit-special" (&optional arg))
(declare-function org-emphasize "org-edit-markup" (&optional char))
(declare-function org-end-of-line "org-move" (&optional n))
(declare-function org-entry-put "org-property" (pom property value))
(declare-function org-evaluate-time-range "org-misc" (&optional to-buffer))
(declare-function org-export-dispatch "ox" (&optional arg))
(declare-function org-feed-goto-inbox "org-feed" (feed))
(declare-function org-feed-update-all "org-feed" ())
(declare-function org-fill-paragraph "org-fill" (&optional justify region))
(declare-function org-find-file-at-mouse "org-open-at-point" (ev))
(declare-function org-footnote-action "org-footnote" (&optional special))
(declare-function org-cycle-force-archived "org-cycle" ())
(declare-function org-force-self-insert "org-edit" (n))
(declare-function org-forward-element "org-move" ())
(declare-function org-forward-heading-same-level "org-move" (arg &optional invisible-ok))
(declare-function org-forward-paragraph "org-move" ())
(declare-function org-forward-sentence "org-move" (&optional arg))
(declare-function org-goto "org-goto" (&optional alternative-interface))
(declare-function org-goto-calendar "org-misc" (&optional arg))
(declare-function org-inc-effort "org-property" ())
(declare-function org-increase-number-at-point "org-misc" (&optional inc))
(declare-function org-info-find-node "org-misc" (&optional nodename))
(declare-function org-insert-all-links "ol" (arg &optional pre post))
(declare-function org-insert-drawer "org-edit-markup" (&optional arg drawer))
(declare-function org-insert-heading-respect-content "org-edit-structure" (&optional invisible-ok))
(declare-function org-insert-last-stored-link "ol" (arg))
(declare-function org-insert-link "ol" (&optional complete-file link-location default-description))
(declare-function org-insert-structure-template "org-edit-markup" (type))
(declare-function org-insert-todo-heading "org-edit-structure" (arg &optional force-heading))
(declare-function org-insert-todo-heading-respect-content "org-edit-structure" (&optional force-state))
(declare-function org-kill-line "org-edit" (&optional arg))
(declare-function org-kill-note-or-show-branches "org-edit-special" ())
(declare-function org-list-make-subtree "org-list" ())
(declare-function org-mark-element "org-mark" ())
(declare-function org-mark-ring-goto "org-mark-ring" (&optional n))
(declare-function org-mark-ring-push "org-mark-ring" (&optional pos buffer))
(declare-function org-mark-subtree "org-mark" (&optional up))
(declare-function org-match-sparse-tree "org-sparse-tree" (&optional todo-only match))
(declare-function org-meta-return "org-edit-special" (&optional arg))
(declare-function org-metadown "org-edit-special" (&optional _arg))
(declare-function org-metaleft "org-edit-special" (&optional _))
(declare-function org-metaright "org-edit-special" (&optional _arg))
(declare-function org-metaup "org-edit-special" (&optional _arg))
(declare-function org-narrow-to-block "org-narrow" ())
(declare-function org-narrow-to-element "org-narrow" ())
(declare-function org-narrow-to-subtree "org-narrow" (&optional element))
(declare-function org-next-block "org-move" (arg &optional backward block-regexp))
(declare-function org-next-link "ol" (&optional search-backward))
(declare-function org-next-visible-heading "org-move" (arg))
(declare-function org-open-at-mouse "org-open-at-point" (ev))
(declare-function org-open-at-point "org-open-at-point" (&optional arg reference-buffer))
(declare-function org-open-line "org-edit" (n))
(declare-function org-paste-special "org-edit" (arg))
(declare-function org-plot/gnuplot "org-plot" (&optional params))
(declare-function org-previous-block "org-move" (arg &optional block-regexp))
(declare-function org-previous-link "ol" ())
(declare-function org-previous-visible-heading "org-move" (arg))
(declare-function org-priority "org-priority" (&optional action show))
(declare-function org-promote-subtree "org-edit-structure" ())
(declare-function org-redisplay-inline-images "org-preview-image" ())
(declare-function org-refile "org-refile" (&optional arg1 default-buffer rfloc msg))
(declare-function org-refile-copy "org-refile" ())
(declare-function org-refile-reverse "org-refile" (&optional arg default-buffer rfloc msg))
(declare-function org-reload "org-load" (&optional arg1))
(declare-function org-remove-file "org-agenda-files" (&optional file))
(declare-function org-resolve-clocks "org-clock" (&optional only-dangling-p prompt-fn last-valid))
(declare-function org-return "org-edit" (&optional indent))
(declare-function org-return-and-maybe-indent "org-edit" ())
(declare-function org-fold-reveal "org-fold" (&optional siblings))
(declare-function org-schedule "org-planning" (arg &optional time))
(declare-function org-self-insert-command "org-edit" (N))
(declare-function org-set-effort "org-property" (&optional increment value))
(declare-function org-set-property "org-property" (property value))
(declare-function org-set-property-and-value "org-property" (use-last))
(declare-function org-set-tags-command "org-tags" (&optional arg))
(declare-function org-shiftcontroldown "org-edit-special" (&optional n))
(declare-function org-shiftcontrolleft "org-edit-special" ())
(declare-function org-shiftcontrolright "org-edit-special" ())
(declare-function org-shiftcontrolup "org-edit-special" (&optional n))
(declare-function org-shiftdown "org-edit-special" (&optional arg))
(declare-function org-shiftleft "org-edit-special" (&optional arg))
(declare-function org-shiftmetadown "org-edit-special" (&optional _arg))
(declare-function org-shiftmetaleft "org-edit-special" ())
(declare-function org-shiftmetaright "org-edit-special" ())
(declare-function org-shiftmetaup "org-edit-special" (&optional arg))
(declare-function org-shiftright "org-edit-special" (&optional arg))
(declare-function org-shifttab "org-edit-special" (&optional arg))
(declare-function org-shiftup "org-edit-special" (&optional arg))
(declare-function org-fold-show-all "org-fold" (&optional types))
(declare-function org-fold-show-children "org-fold" (&optional level))
(declare-function org-fold-show-subtree "org-fold" ())
(declare-function org-sort "org-edit-structure" (&optional with-case))
(declare-function org-sparse-tree "org-sparse-tree" (&optional arg type))
(declare-function org-table-copy-down "org-table" (n))
(declare-function org-table-create-or-convert-from-region "org-table" (arg))
(declare-function org-table-create-with-table.el "org-table" ())
(declare-function org-table-edit-field "org-table" (arg))
(declare-function org-table-eval-formula "org-table" (&optional arg equation suppress-align suppress-const suppress-store suppress-analysis))
(declare-function org-table-field-info "org-table" (arg))
(declare-function org-table-rotate-recalc-marks "org-table" (&optional newchar))
(declare-function org-table-sum "org-table" (&optional beg end nlast))
(declare-function org-table-toggle-coordinate-overlays "org-table" ())
(declare-function org-table-toggle-formula-debugger "org-table" ())
(declare-function org-timestamp "org-timestamp" (arg &optional inactive))
(declare-function org-timestamp-inactive "org-timestamp" (&optional arg))
(declare-function org-timer "org-timer" (&optional restart no-insert))
(declare-function org-timer-item "org-timer" (&optional arg))
(declare-function org-timer-pause-or-continue "org-timer" (&optional stop))
(declare-function org-timer-set-timer "org-timer" (&optional opt))
(declare-function org-timer-start "org-timer" (&optional offset))
(declare-function org-timer-stop "org-timer" ())
(declare-function org-todo "org-todo" (&optional arg1))
(declare-function org-toggle-archive-tag "org-archive" (&optional find-done))
(declare-function org-toggle-checkbox "org-list" (&optional toggle-presence))
(declare-function org-toggle-radio-button "org-list" (&optional arg))
(declare-function org-toggle-comment "org-comment" ())
(declare-function org-toggle-fixed-width "org-edit-markup" ())
(declare-function org-toggle-inline-images "org-preview-image" (&optional include-linked beg end))
(declare-function org-latex-preview "org-preview-latex" (&optional arg))
(declare-function org-toggle-narrow-to-subtree "org-narrow" ())
(declare-function org-toggle-ordered-property "org-todo" ())
(declare-function org-toggle-pretty-entities "org-font-lock" ())
(declare-function org-toggle-tags-groups "org-tags" ())
(declare-function org-toggle-timestamp-overlays "org-font-lock" ())
(declare-function org-transpose-element "org-edit" ())
(declare-function org-transpose-words "org-edit" ())
(declare-function org-tree-to-indirect-buffer "org-indirect-buffer" (&optional arg))
(declare-function org-up-element "org-move" ())
(declare-function org-update-statistics-cookies "org-todo" (all))
(declare-function org-yank "org-edit" (&optional arg))
(declare-function orgtbl-ascii-plot "org-table" (&optional ask))
(declare-function org-cite-insert "oc" (arg))


;;; Variables

(defvar org-mode-map (make-sparse-keymap)
  "Keymap for Org mode.")

(defvaralias 'org-CUA-compatible 'org-replace-disputed-keys)

(defcustom org-replace-disputed-keys nil
  "Non-nil means use alternative key bindings for some keys.

Org mode uses S-<cursor> keys for changing timestamps and priorities.
These keys are also used by other packages like Shift Select mode,
CUA mode or Windmove.  If you want to use Org mode together with
one of these other modes, or more generally if you would like to
move some Org mode commands to other keys, set this variable and
configure the keys with the variable `org-disputed-keys'.

This option is only relevant at load-time of Org mode, and must be set
*before* org.el is loaded.  Changing it requires a restart of Emacs to
become effective."
  :group 'org-startup
  :type 'boolean
  :safe #'booleanp)

(defcustom org-use-extra-keys nil
  "Non-nil means use extra key sequence definitions for certain commands.
This happens automatically if `display-graphic-p' returns nil.  This
variable lets you do the same manually.  You must set it before
loading Org."
  :group 'org-startup
  :type 'boolean
  :safe #'booleanp)

(defcustom org-disputed-keys
  '(([(shift up)]		. [(meta p)])
    ([(shift down)]		. [(meta n)])
    ([(shift left)]		. [(meta -)])
    ([(shift right)]		. [(meta +)])
    ([(control shift right)]    . [(meta shift +)])
    ([(control shift left)]	. [(meta shift -)]))
  "Keys for which Org mode and other modes compete.
This is an alist, cars are the default keys, second element specifies
the alternative to use when `org-replace-disputed-keys' is t.

Keys can be specified in any syntax supported by `define-key'.
The value of this option takes effect only at Org mode startup,
therefore you'll have to restart Emacs to apply it after changing."
  :group 'org-startup
  :type 'alist)

(defcustom org-mouse-1-follows-link
  (if (boundp 'mouse-1-click-follows-link) mouse-1-click-follows-link t)
  "Non-nil means Mouse-1 on a link will follow the link.
A longer mouse click will still set point.  Needs to be set
before org.el is loaded."
  :group 'org-link-follow
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(choice
	  (const :tag "A double click follows the link" double)
	  (const :tag "Unconditionally follow the link with mouse-1" t)
	  (integer :tag "mouse-1 click does not follow the link if longer than N ms" 450)))

(defcustom org-tab-follows-link nil
  "Non-nil means on links TAB will follow the link.
Needs to be set before Org is loaded.
This really should not be used, it does not make sense, and the
implementation is bad."
  :group 'org-link-follow
  :type 'boolean)

(defcustom org-follow-link-hook nil
  "Hook that is run after a link has been followed."
  :group 'org-link-follow
  :type 'hook)

(defcustom org-return-follows-link nil
  "Non-nil means on links RET will open links, timestamps, and citations.
In tables, the special behavior of RET has precedence."
  :group 'org-link-follow
  :type 'boolean
  :safe #'booleanp)


;;; Functions

;;;; Base functions
(defun org-key (key)
  "Select KEY according to `org-replace-disputed-keys' and `org-disputed-keys'.
Or return the original if not disputed."
  (when org-replace-disputed-keys
    (let* ((nkey (key-description key))
	   (x (cl-find-if (lambda (x) (equal (key-description (car x)) nkey))
			  org-disputed-keys)))
      (setq key (if x (cdr x) key))))
  key)

(defun org-defkey (keymap key def)
  "Define KEY, possibly translated, as returned by `org-key' in KEYMAP to DEF."
  (define-key keymap (org-key key) def))

(defun org-remap (map &rest commands)
  "In MAP, remap the functions given in COMMANDS.
COMMANDS is a list of alternating OLDDEF NEWDEF command names."
  (let (new old)
    (while commands
      (setq old (pop commands) new (pop commands))
      (org-defkey map (vector 'remap old) new))))


;;; Mouse map

(defvar org-mouse-map (make-sparse-keymap))
(org-defkey org-mouse-map [mouse-2] 'org-open-at-mouse)
(org-defkey org-mouse-map [mouse-3] 'org-find-file-at-mouse)

(when org-mouse-1-follows-link
  (org-defkey org-mouse-map [follow-link] 'mouse-face))

(when org-tab-follows-link
  (org-defkey org-mouse-map (kbd "TAB") #'org-open-at-point))


;;; Global bindings

;;;; Outline functions
(define-key org-mode-map [menu-bar headings] #'undefined)
(define-key org-mode-map [menu-bar hide] #'undefined)
(define-key org-mode-map [menu-bar show] #'undefined)

(define-key org-mode-map [remap outline-mark-subtree] #'org-mark-subtree)
(define-key org-mode-map [remap outline-show-subtree] #'org-fold-show-subtree)
(define-key org-mode-map [remap outline-forward-same-level]
  #'org-forward-heading-same-level)
(define-key org-mode-map [remap outline-backward-same-level]
  #'org-backward-heading-same-level)
(define-key org-mode-map [remap outline-show-branches]
  #'org-kill-note-or-show-branches)
(define-key org-mode-map [remap outline-promote] #'org-promote-subtree)
(define-key org-mode-map [remap outline-demote] #'org-demote-subtree)
(define-key org-mode-map [remap outline-insert-heading] #'org-ctrl-c-ret)
(define-key org-mode-map [remap outline-next-visible-heading]
  #'org-next-visible-heading)
(define-key org-mode-map [remap outline-previous-visible-heading]
  #'org-previous-visible-heading)
(define-key org-mode-map [remap outline-show-children] #'org-fold-show-children)

;;;; Make `C-c C-x' a prefix key
(org-defkey org-mode-map (kbd "C-c C-x") (make-sparse-keymap))

;;;; TAB key with modifiers
(org-defkey org-mode-map (kbd "TAB") #'org-cycle)
(org-defkey org-mode-map (kbd "C-c C-<tab>") #'org-cycle-force-archived)
;; Override text-mode binding to expose `complete-symbol' for
;; pcomplete functionality.
(org-defkey org-mode-map (kbd "M-TAB") nil)
(org-defkey org-mode-map (kbd "ESC TAB") nil)

(org-defkey org-mode-map (kbd "S-TAB") #'org-shifttab)
(define-key org-mode-map (kbd "<backtab>") #'org-shifttab)

;;;; RET/<return> key with modifiers
(org-defkey org-mode-map (kbd "S-<return>") #'org-table-copy-down)
(org-defkey org-mode-map (kbd "S-RET") #'org-table-copy-down)
(org-defkey org-mode-map (kbd "M-S-<return>") #'org-insert-todo-heading)
(org-defkey org-mode-map (kbd "M-S-RET") #'org-insert-todo-heading)
(org-defkey org-mode-map (kbd "M-RET") #'org-meta-return)

;;;; Cursor keys with modifiers
(org-defkey org-mode-map (kbd "M-<left>") #'org-metaleft)
(org-defkey org-mode-map (kbd "ESC <left>") #'org-metaleft)
(org-defkey org-mode-map (kbd "M-<right>") #'org-metaright)
(org-defkey org-mode-map (kbd "ESC <right>") #'org-metaright)
(org-defkey org-mode-map (kbd "M-<up>") #'org-metaup)
(org-defkey org-mode-map (kbd "ESC <up>") #'org-metaup)
(org-defkey org-mode-map (kbd "M-<down>") #'org-metadown)
(org-defkey org-mode-map (kbd "ESC <down>") #'org-metadown)

(org-defkey org-mode-map (kbd "C-M-S-<right>") #'org-increase-number-at-point)
(org-defkey org-mode-map (kbd "C-M-S-<left>") #'org-decrease-number-at-point)
(org-defkey org-mode-map (kbd "M-S-<left>") #'org-shiftmetaleft)
(org-defkey org-mode-map (kbd "ESC S-<left>") #'org-shiftmetaleft)
(org-defkey org-mode-map (kbd "M-S-<right>") #'org-shiftmetaright)
(org-defkey org-mode-map (kbd "ESC S-<right>") #'org-shiftmetaright)
(org-defkey org-mode-map (kbd "M-S-<up>") #'org-shiftmetaup)
(org-defkey org-mode-map (kbd "ESC S-<up>") #'org-shiftmetaup)
(org-defkey org-mode-map (kbd "M-S-<down>") #'org-shiftmetadown)
(org-defkey org-mode-map (kbd "ESC S-<down>") #'org-shiftmetadown)

(org-defkey org-mode-map (kbd "S-<up>") #'org-shiftup)
(org-defkey org-mode-map (kbd "S-<down>") #'org-shiftdown)
(org-defkey org-mode-map (kbd "S-<left>") #'org-shiftleft)
(org-defkey org-mode-map (kbd "S-<right>") #'org-shiftright)

(org-defkey org-mode-map (kbd "C-S-<right>") #'org-shiftcontrolright)
(org-defkey org-mode-map (kbd "C-S-<left>") #'org-shiftcontrolleft)
(org-defkey org-mode-map (kbd "C-S-<up>") #'org-shiftcontrolup)
(org-defkey org-mode-map (kbd "C-S-<down>") #'org-shiftcontroldown)

;;;; Extra keys for TTY access.

;;  We only set them when really needed because otherwise the
;;  menus don't show the simple keys

(when (or org-use-extra-keys (not (display-graphic-p)))
  (org-defkey org-mode-map (kbd "C-c C-x c") #'org-table-copy-down)
  (org-defkey org-mode-map (kbd "C-c C-x m") #'org-meta-return)
  (org-defkey org-mode-map (kbd "C-c C-x M") #'org-insert-todo-heading)
  (org-defkey org-mode-map (kbd "C-c C-x s") #'org-insert-structure-template)
  (org-defkey org-mode-map (kbd "C-c C-x RET") #'org-meta-return)
  (org-defkey org-mode-map (kbd "ESC RET") #'org-meta-return)
  (org-defkey org-mode-map (kbd "ESC <left>") #'org-metaleft)
  (org-defkey org-mode-map (kbd "C-c C-x l") #'org-metaleft)
  (org-defkey org-mode-map (kbd "ESC <right>") #'org-metaright)
  (org-defkey org-mode-map (kbd "C-c C-x r") #'org-metaright)
  (org-defkey org-mode-map (kbd "C-c C-x u") #'org-metaup)
  (org-defkey org-mode-map (kbd "C-c C-x d") #'org-metadown)
  (org-defkey org-mode-map (kbd "C-c C-x L") #'org-shiftmetaleft)
  (org-defkey org-mode-map (kbd "C-c C-x R") #'org-shiftmetaright)
  (org-defkey org-mode-map (kbd "C-c C-x U") #'org-shiftmetaup)
  (org-defkey org-mode-map (kbd "C-c C-x D") #'org-shiftmetadown)
  (org-defkey org-mode-map (kbd "C-c <up>") #'org-shiftup)
  (org-defkey org-mode-map (kbd "C-c <down>") #'org-shiftdown)
  (org-defkey org-mode-map (kbd "C-c <left>") #'org-shiftleft)
  (org-defkey org-mode-map (kbd "C-c <right>") #'org-shiftright)
  (org-defkey org-mode-map (kbd "C-c C-x <right>") #'org-shiftcontrolright)
  (org-defkey org-mode-map (kbd "C-c C-x <left>") #'org-shiftcontrolleft))

;;;; Narrowing bindings
(org-defkey org-mode-map (kbd "C-x n s") #'org-narrow-to-subtree)
(org-defkey org-mode-map (kbd "C-x n b") #'org-narrow-to-block)
(org-defkey org-mode-map (kbd "C-x n e") #'org-narrow-to-element)

;;;; Remap usual Emacs bindings
(org-remap org-mode-map
	   'self-insert-command    'org-self-insert-command
	   'delete-char            'org-delete-char
	   'delete-backward-char   'org-delete-backward-char
	   'kill-line              'org-kill-line
	   'open-line              'org-open-line
	   'yank                   'org-yank
	   'comment-dwim           'org-comment-dwim
	   'move-beginning-of-line 'org-beginning-of-line
	   'move-end-of-line       'org-end-of-line
	   'forward-paragraph      'org-forward-paragraph
	   'backward-paragraph     'org-backward-paragraph
	   'backward-sentence      'org-backward-sentence
	   'forward-sentence       'org-forward-sentence
	   'fill-paragraph         'org-fill-paragraph
	   'delete-indentation     'org-delete-indentation
	   'transpose-words        'org-transpose-words)

;;;; All the other keys
(org-defkey org-mode-map (kbd "|") #'org-force-self-insert)
(org-defkey org-mode-map (kbd "C-c C-r") #'org-fold-reveal)
(org-defkey org-mode-map (kbd "C-M-t") #'org-transpose-element)
(org-defkey org-mode-map (kbd "M-}") #'org-forward-element)
(org-defkey org-mode-map (kbd "ESC }") #'org-forward-element)
(org-defkey org-mode-map (kbd "M-{") #'org-backward-element)
(org-defkey org-mode-map (kbd "ESC {") #'org-backward-element)
(org-defkey org-mode-map (kbd "C-c C-^") #'org-up-element)
(org-defkey org-mode-map (kbd "C-c C-_") #'org-down-element)
(org-defkey org-mode-map (kbd "C-c C-f") #'org-forward-heading-same-level)
(org-defkey org-mode-map (kbd "C-c C-b") #'org-backward-heading-same-level)
(org-defkey org-mode-map (kbd "C-c M-f") #'org-next-block)
(org-defkey org-mode-map (kbd "C-c M-b") #'org-previous-block)
(org-defkey org-mode-map (kbd "C-c $") #'org-archive-subtree)
(org-defkey org-mode-map (kbd "C-c C-x C-s") #'org-archive-subtree)
(org-defkey org-mode-map (kbd "C-c C-x C-a") #'org-archive-subtree-default)
(org-defkey org-mode-map (kbd "C-c C-x d") #'org-insert-drawer)
(org-defkey org-mode-map (kbd "C-c C-x a") #'org-toggle-archive-tag)
(org-defkey org-mode-map (kbd "C-c C-x A") #'org-archive-to-archive-sibling)
(org-defkey org-mode-map (kbd "C-c C-x b") #'org-tree-to-indirect-buffer)
(org-defkey org-mode-map (kbd "C-c C-x q") #'org-toggle-tags-groups)
(org-defkey org-mode-map (kbd "C-c C-j") #'org-goto)
(org-defkey org-mode-map (kbd "C-c C-t") #'org-todo)
(org-defkey org-mode-map (kbd "C-c C-q") #'org-set-tags-command)
(org-defkey org-mode-map (kbd "C-c C-s") #'org-schedule)
(org-defkey org-mode-map (kbd "C-c C-d") #'org-deadline)
(org-defkey org-mode-map (kbd "C-c ;") #'org-toggle-comment)
(org-defkey org-mode-map (kbd "C-c C-w") #'org-refile)
(org-defkey org-mode-map (kbd "C-c M-w") #'org-refile-copy)
(org-defkey org-mode-map (kbd "C-c C-M-w") #'org-refile-reverse)
(org-defkey org-mode-map (kbd "C-c /") #'org-sparse-tree) ;minor-mode reserved
(org-defkey org-mode-map (kbd "C-c \\") #'org-match-sparse-tree) ;minor-mode r.
(org-defkey org-mode-map (kbd "C-c RET") #'org-ctrl-c-ret)
(org-defkey org-mode-map (kbd "C-c C-x c") #'org-clone-subtree-with-time-shift)
(org-defkey org-mode-map (kbd "C-c C-x v") #'org-copy-visible)
(org-defkey org-mode-map (kbd "C-<return>") #'org-insert-heading-respect-content)
(org-defkey org-mode-map (kbd "C-S-<return>") #'org-insert-todo-heading-respect-content)
(org-defkey org-mode-map (kbd "C-c C-x C-n") #'org-next-link)
(org-defkey org-mode-map (kbd "C-c C-x C-p") #'org-previous-link)
(org-defkey org-mode-map (kbd "C-c C-l") #'org-insert-link)
(org-defkey org-mode-map (kbd "C-c M-l") #'org-insert-last-stored-link)
(org-defkey org-mode-map (kbd "C-c C-M-l") #'org-insert-all-links)
(org-defkey org-mode-map (kbd "C-c C-o") #'org-open-at-point)
(org-defkey org-mode-map (kbd "C-c %") #'org-mark-ring-push)
(org-defkey org-mode-map (kbd "C-c &") #'org-mark-ring-goto)
(org-defkey org-mode-map (kbd "C-c C-z") #'org-add-note) ;alternative binding
(org-defkey org-mode-map (kbd "C-c .") #'org-timestamp) ;minor-mode reserved
(org-defkey org-mode-map (kbd "C-c !") #'org-timestamp-inactive) ;minor-mode r.
(org-defkey org-mode-map (kbd "C-c ,") #'org-priority) ;minor-mode reserved
(org-defkey org-mode-map (kbd "C-c C-y") #'org-evaluate-time-range)
(org-defkey org-mode-map (kbd "C-c >") #'org-goto-calendar)
(org-defkey org-mode-map (kbd "C-c <") #'org-date-from-calendar)
(org-defkey org-mode-map (kbd "C-,") #'org-cycle-agenda-files)
(org-defkey org-mode-map (kbd "C-'") #'org-cycle-agenda-files)
(org-defkey org-mode-map (kbd "C-c [") #'org-agenda-file-to-front)
(org-defkey org-mode-map (kbd "C-c ]") #'org-remove-file)
(org-defkey org-mode-map (kbd "C-c C-x <") #'org-agenda-set-restriction-lock)
(org-defkey org-mode-map (kbd "C-c C-x >") #'org-agenda-remove-restriction-lock)
(org-defkey org-mode-map (kbd "C-c -") #'org-ctrl-c-minus)
(org-defkey org-mode-map (kbd "C-c *") #'org-ctrl-c-star)
(org-defkey org-mode-map (kbd "C-c TAB") #'org-ctrl-c-tab)
(org-defkey org-mode-map (kbd "C-c ^") #'org-sort)
(org-defkey org-mode-map (kbd "C-c C-c") #'org-ctrl-c-ctrl-c)
(org-defkey org-mode-map (kbd "C-c C-k") #'org-kill-note-or-show-branches)
(org-defkey org-mode-map (kbd "C-c #") #'org-update-statistics-cookies)
(org-defkey org-mode-map (kbd "RET") #'org-return)
(org-defkey org-mode-map (kbd "C-j") #'org-return-and-maybe-indent)
(org-defkey org-mode-map (kbd "C-c ?") #'org-table-field-info)
(org-defkey org-mode-map (kbd "C-c +") #'org-table-sum)
(org-defkey org-mode-map (kbd "C-c =") #'org-table-eval-formula)
(org-defkey org-mode-map (kbd "C-c '") #'org-edit-special)
(org-defkey org-mode-map (kbd "C-c `") #'org-table-edit-field)
(org-defkey org-mode-map (kbd "C-c \" a") #'orgtbl-ascii-plot)
(org-defkey org-mode-map (kbd "C-c \" g") #'org-plot/gnuplot)
(org-defkey org-mode-map (kbd "C-c |") #'org-table-create-or-convert-from-region)
(org-defkey org-mode-map (kbd "C-#") #'org-table-rotate-recalc-marks)
(org-defkey org-mode-map (kbd "C-c ~") #'org-table-create-with-table.el)
(org-defkey org-mode-map (kbd "C-c C-a") #'org-attach)
(org-defkey org-mode-map (kbd "C-c }") #'org-table-toggle-coordinate-overlays)
(org-defkey org-mode-map (kbd "C-c {") #'org-table-toggle-formula-debugger)
(org-defkey org-mode-map (kbd "C-c C-e") #'org-export-dispatch)
(org-defkey org-mode-map (kbd "C-c :") #'org-toggle-fixed-width)
(org-defkey org-mode-map (kbd "C-c C-x C-f") #'org-emphasize)
(org-defkey org-mode-map (kbd "C-c C-x f") #'org-footnote-action)
(org-defkey org-mode-map (kbd "C-c @") #'org-mark-subtree)
(org-defkey org-mode-map (kbd "M-h") #'org-mark-element)
(org-defkey org-mode-map (kbd "ESC h") #'org-mark-element)
(org-defkey org-mode-map (kbd "C-c C-*") #'org-list-make-subtree)
(org-defkey org-mode-map (kbd "C-c C-x C-w") #'org-cut-special)
(org-defkey org-mode-map (kbd "C-c C-x M-w") #'org-copy-special)
(org-defkey org-mode-map (kbd "C-c C-x C-y") #'org-paste-special)
(org-defkey org-mode-map (kbd "C-c C-x C-t") #'org-toggle-timestamp-overlays)
(org-defkey org-mode-map (kbd "C-c C-x C-i") #'org-clock-in)
(org-defkey org-mode-map (kbd "C-c C-x C-x") #'org-clock-in-last)
(org-defkey org-mode-map (kbd "C-c C-x C-z") #'org-resolve-clocks)
(org-defkey org-mode-map (kbd "C-c C-x C-o") #'org-clock-out)
(org-defkey org-mode-map (kbd "C-c C-x C-j") #'org-clock-goto)
(org-defkey org-mode-map (kbd "C-c C-x C-q") #'org-clock-cancel)
(org-defkey org-mode-map (kbd "C-c C-x C-d") #'org-clock-display)
(org-defkey org-mode-map (kbd "C-c C-x C-e") 'org-clock-modify-effort-estimate)
(org-defkey org-mode-map (kbd "C-c C-x x") #'org-dynamic-block-insert-dblock)
(org-defkey org-mode-map (kbd "C-c C-x C-u") #'org-dblock-update)
(org-defkey org-mode-map (kbd "C-c C-x C-l") #'org-latex-preview)
(org-defkey org-mode-map (kbd "C-c C-x C-v") #'org-toggle-inline-images)
(org-defkey org-mode-map (kbd "C-c C-x C-M-v") #'org-redisplay-inline-images)
(org-defkey org-mode-map (kbd "C-c C-x \\") #'org-toggle-pretty-entities)
(org-defkey org-mode-map (kbd "C-c C-x C-b") #'org-toggle-checkbox)
(org-defkey org-mode-map (kbd "C-c C-x C-r") #'org-toggle-radio-button)
(org-defkey org-mode-map (kbd "C-c C-x p") #'org-set-property)
(org-defkey org-mode-map (kbd "C-c C-x P") #'org-set-property-and-value)
(org-defkey org-mode-map (kbd "C-c C-x e") #'org-set-effort)
(org-defkey org-mode-map (kbd "C-c C-x E") #'org-inc-effort)
(org-defkey org-mode-map (kbd "C-c C-x o") #'org-toggle-ordered-property)
(org-defkey org-mode-map (kbd "C-c C-,") #'org-insert-structure-template)
(org-defkey org-mode-map (kbd "C-c C-x .") #'org-timer)
(org-defkey org-mode-map (kbd "C-c C-x -") #'org-timer-item)
(org-defkey org-mode-map (kbd "C-c C-x 0") #'org-timer-start)
(org-defkey org-mode-map (kbd "C-c C-x _") #'org-timer-stop)
(org-defkey org-mode-map (kbd "C-c C-x ;") #'org-timer-set-timer)
(org-defkey org-mode-map (kbd "C-c C-x ,") #'org-timer-pause-or-continue)
(org-defkey org-mode-map (kbd "C-c C-x C-c") #'org-columns)
(org-defkey org-mode-map (kbd "C-c C-x !") #'org-reload)
(org-defkey org-mode-map (kbd "C-c C-x g") #'org-feed-update-all)
(org-defkey org-mode-map (kbd "C-c C-x G") #'org-feed-goto-inbox)
(org-defkey org-mode-map (kbd "C-c C-x @") #'org-cite-insert)
(org-defkey org-mode-map (kbd "C-c C-x I") #'org-info-find-node)


;;; Speed keys

(defcustom org-use-speed-commands nil
  "Non-nil means activate single letter commands at beginning of a headline.
This may also be a function to test for appropriate locations where speed
commands should be active.

For example, to activate speed commands when the point is on any
star at the beginning of the headline, you can do this:

  (setq org-use-speed-commands
      (lambda () (and (looking-at org-outline-regexp) (looking-back \"^\\**\"))))"
  :group 'org-structure
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "At beginning of headline stars" t)
	  (function)))

(defcustom org-speed-command-hook
  '(org-speed-command-activate org-babel-speed-command-activate)
  "Hook for activating speed commands at strategic locations.
Hook functions are called in sequence until a valid handler is
found.

Each hook takes a single argument, a user-pressed command key
which is also a `self-insert-command' from the global map.

Within the hook, examine the cursor position and the command key
and return nil or a valid handler as appropriate.  Handler could
be one of an interactive command, a function, or a form.

Set `org-use-speed-commands' to non-nil value to enable this
hook.  The default setting is `org-speed-command-activate'."
  :group 'org-structure
  :version "24.1"
  :type 'hook)

(defcustom org-speed-commands
  '(("Outline Navigation")
    ("n" . (org-speed-move-safe 'org-next-visible-heading))
    ("p" . (org-speed-move-safe 'org-previous-visible-heading))
    ("f" . (org-speed-move-safe 'org-forward-heading-same-level))
    ("b" . (org-speed-move-safe 'org-backward-heading-same-level))
    ("F" . org-next-block)
    ("B" . org-previous-block)
    ("u" . (org-speed-move-safe 'outline-up-heading))
    ("j" . org-goto)
    ("g" . (org-refile '(4)))
    ("Outline Visibility")
    ("c" . org-cycle)
    ("C" . org-shifttab)
    (" " . org-display-outline-path)
    ("s" . org-toggle-narrow-to-subtree)
    ("k" . org-cut-subtree)
    ("=" . org-columns)
    ("Outline Structure Editing")
    ("U" . org-metaup)
    ("D" . org-metadown)
    ("r" . org-metaright)
    ("l" . org-metaleft)
    ("R" . org-shiftmetaright)
    ("L" . org-shiftmetaleft)
    ("i" . (progn (forward-char 1) (call-interactively #'org-insert-heading-respect-content)))
    ("^" . org-sort)
    ("w" . org-refile)
    ("a" . org-archive-subtree-default-with-confirmation)
    ("@" . org-mark-subtree)
    ("#" . org-toggle-comment)
    ("Clock Commands")
    ("I" . org-clock-in)
    ("O" . org-clock-out)
    ("Meta Data Editing")
    ("t" . org-todo)
    ("," . (org-priority))
    ("0" . (org-priority ?\ ))
    ("1" . (org-priority ?A))
    ("2" . (org-priority ?B))
    ("3" . (org-priority ?C))
    (":" . org-set-tags-command)
    ("e" . org-set-effort)
    ("E" . org-inc-effort)
    ("W" . (lambda (m) (interactive "sMinutes before warning: ") (org-entry-put (point) "APPT_WARNTIME" m)))
    ("Agenda Views etc")
    ("v" . org-agenda)
    ("/" . org-sparse-tree)
    ("Misc")
    ("o" . org-open-at-point)
    ("?" . org-speed-command-help)
    ("<" . (org-agenda-set-restriction-lock 'subtree))
    (">" . (org-agenda-remove-restriction-lock)))
  "Alist of speed commands.

The car of each entry is a string with a single letter, which
must be assigned to `self-insert-command' in the global map.

The cdr is either a command to be called interactively, a
function to be called, or a form to be evaluated.

An entry that is just a list with a single string will be
interpreted as a descriptive headline that will be added when
listing the speed commands in the Help buffer using the `?' speed
command."
  :group 'org-structure
  :package-version '(Org . "9.5")
  :type '(repeat :value ("k" . ignore)
		 (choice :value ("k" . ignore)
			 (list :tag "Descriptive Headline" (string :tag "Headline"))
			 (cons :tag "Letter and Command"
			       (string :tag "Command letter")
			       (choice
				(function)
				(sexp))))))

(defun org--print-speed-command (speed-command)
  "Print information about SPEED-COMMAND in help buffer.
SPEED-COMMAND is an element of `org-speed-commands'."
  (if (> (length (car speed-command)) 1)
      (progn
	(princ "\n")
	(princ (car speed-command))
	(princ "\n")
	(princ (make-string (length (car speed-command)) ?-))
	(princ "\n"))
    (princ (car speed-command))
    (princ "   ")
    (if (symbolp (cdr speed-command))
	(princ (symbol-name (cdr speed-command)))
      (prin1 (cdr speed-command)))
    (princ "\n")))

(defun org-speed-command-help ()
  "Show the available speed commands."
  (interactive)
  (unless org-use-speed-commands
    (user-error "Speed commands are not activated, customize `org-use-speed-commands'"))
  (with-output-to-temp-buffer "*Help*"
    (princ "Speed commands\n==============\n")
    (mapc #'org--print-speed-command org-speed-commands))
  (with-current-buffer "*Help*"
    (setq truncate-lines t)))

(defun org-speed-move-safe (cmd)
  "Execute CMD, but make sure that the cursor always ends up in a headline.
If not, return to the original position and throw an error."
  (interactive)
  (let ((pos (point)))
    (call-interactively cmd)
    (unless (and (bolp) (org-at-heading-p))
      (goto-char pos)
      (error "Boundary reached while executing %s" cmd))))

(defun org-speed-command-activate (keys)
  "Hook for activating single-letter speed commands.
KEYS is the keys vector as returned by `this-command-keys-vector'.
See `org-speed-commands' for configuring them."
  (when (or (and (bolp) (looking-at org-outline-regexp))
	    (and (functionp org-use-speed-commands)
		 (funcall org-use-speed-commands)))
    (cdr (assoc keys org-speed-commands))))


;;; Babel speed keys

(defvar org-babel-key-prefix "\C-c\C-v"
  "The key prefix for Babel interactive key-bindings.
See `org-babel-key-bindings' for the list of interactive Babel
functions which are assigned key bindings, and see
`org-babel-map' for the actual babel keymap.")

(defvar org-babel-map (make-sparse-keymap)
  "The keymap for interactive Babel functions.")

(defvar org-babel-key-bindings
  '(("p" . org-babel-previous-src-block)
    ("\C-p" . org-babel-previous-src-block)
    ("n" . org-babel-next-src-block)
    ("\C-n" . org-babel-next-src-block)
    ("e" . org-babel-execute-maybe)
    ("\C-e" . org-babel-execute-maybe)
    ("o" . org-babel-open-src-block-result)
    ("\C-o" . org-babel-open-src-block-result)
    ("\C-v" . org-babel-expand-src-block)
    ("v" . org-babel-expand-src-block)
    ("u" . org-babel-goto-src-block-head)
    ("\C-u" . org-babel-goto-src-block-head)
    ("g" . org-babel-goto-named-src-block)
    ("r" . org-babel-goto-named-result)
    ("\C-r" . org-babel-goto-named-result)
    ("\C-b" . org-babel-execute-buffer)
    ("b" . org-babel-execute-buffer)
    ("\C-s" . org-babel-execute-subtree)
    ("s" . org-babel-execute-subtree)
    ("\C-d" . org-babel-demarcate-block)
    ("d" . org-babel-demarcate-block)
    ("\C-t" . org-babel-tangle)
    ("t" . org-babel-tangle)
    ("\C-f" . org-babel-tangle-file)
    ("f" . org-babel-tangle-file)
    ("\C-c" . org-babel-check-src-block)
    ("c" . org-babel-check-src-block)
    ("\C-j" . org-babel-insert-header-arg)
    ("j" . org-babel-insert-header-arg)
    ("\C-l" . org-babel-load-in-session)
    ("l" . org-babel-load-in-session)
    ("\C-i" . org-babel-lob-ingest)
    ("i" . org-babel-lob-ingest)
    ("\C-I" . org-babel-view-src-block-info)
    ("I" . org-babel-view-src-block-info)
    ("\C-z" . org-babel-switch-to-session)
    ("z" . org-babel-switch-to-session-with-code)
    ("\C-a" . org-babel-sha1-hash)
    ("a" . org-babel-sha1-hash)
    ("h" . org-babel-describe-bindings)
    ("\C-x" . org-babel-do-key-sequence-in-edit-buffer)
    ("x" . org-babel-do-key-sequence-in-edit-buffer)
    ("k" . org-babel-remove-result-one-or-many)
    ("\C-\M-h" . org-babel-mark-block))
  "Alist of key bindings and interactive Babel functions.
This list associates interactive Babel functions
with keys.  Each element of this list will add an entry to the
`org-babel-map' using the letter key which is the `car' of the
a-list placed behind the generic `org-babel-key-prefix'.")

(define-key org-mode-map org-babel-key-prefix org-babel-map)
(pcase-dolist (`(,key . ,def) org-babel-key-bindings)
  (define-key org-babel-map key def))

(defun org-babel-speed-command-activate (keys)
  "Hook for activating single-letter code block commands.
KEYS is the keys vector as returned by `this-command-keys-vector'."
  (when (and (bolp)
	     (let ((case-fold-search t)) (looking-at "[ \t]*#\\+begin_src"))
	     (org-element-type-p (org-element-at-point) 'src-block))
    (cdr (assoc keys org-babel-key-bindings))))

;;;###autoload
(defun org-babel-describe-bindings ()
  "Describe all keybindings behind `org-babel-key-prefix'."
  (interactive)
  (describe-bindings org-babel-key-prefix))

(provide 'org-keys)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-keys.el ends here
