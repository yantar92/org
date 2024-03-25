;;; org-pending.el --- Regions with pending content -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2024 Free Software Foundation, Inc.

;; Author: Bruno Barbier <brubar.cs@gmail.com>
;; Keywords: tools text
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

;;; Commentary:

;; This file contains an API to mark a region has "being updated"; the
;; content of the region is "pending".  It will be updated, later,
;; when the new content is available.  Until its new content is
;; available, Emacs protects the region from being modified and/or
;; destroyed.  The new content may be "pending" because it's computed
;; using an asynchronous function (like url-retrieve), or a thread or
;; a sub process or a timer or is waiting for user inputs, etc.
;;
;; This package provides a way to mark a region as "pending", telling
;; Emacs what to do when the update is ready.
;;
;; Buffers with pending contents will resist killing.
;;
;; To create and use a pending region, you need to do something like
;; this:
;;
;;    1. Mark a region as "pending" using `org-pending', which gives
;;       you a PENREG.  When you call `org-pending', you need to
;;       provide how to update the pending region when the result is
;;       available (see HANDLE-RESULT arguemnt); Emacs applies it when
;;       you provide the result and close the pending region.
;;
;;    2. Start "something" that compute the new content.  That
;;       "something" may be a thread, a timer, a notification, a
;;       process, etc.  That "something" must eventually send a
;;       :success or :failure message (using
;;       `org-pending-send-update'): Emacs will appropriately
;;       update the pending region and close it.
;;
;;    3. You may, optionally, connect the pending region to a "task".
;;       If your "task" is using ressources, you should provide a
;;       "task-control" with a 'cancel' method, so that Emacs can tell
;;       you, for example, when the pending region is destroyed.  If
;;       your "task" cannot always run asynchronously, you should
;;       provide a 'get' method (so that Emacs knows how to run it
;;       synchronously).  If you have information related to your
;;       "task" to display to the user (logs, stderr, links, widgets
;;       to control the task), you should provide a 'insert-details'
;;       method.  See `org-pending-ti-connect'.
;;
;; When implementing your tasks, you should use the functions prefixed
;; with 'org-pending-ti-'; if you need to use another function, there
;; is most probably a mistake somewhere.
;;
;; Here are examples of functions using this package:
;;     - `org-pending-user-edit': prompt the user to edit a region,
;;     - `org-babel-execute-src-block': execute source blocks
;;       asynchronously.
;;     - and `org-dblock-update': execute dynamic blocks
;;       asynchronously.
;;
;; The section "PENREG" describes the PENREG structure, how to create
;; pending regions and how to describe them to the user.  The section
;; "Implementing tasks & task-controls" contains the functions to
;; implement your tasks.  The section "Checking for pending regions"
;; allows to check for pending contents in regions, in buffers or in
;; Emacs.  The section "Managing pending regions" is about managing
;; all (past or present) pending regions in Emacs.  The section
;; "Plugging into Emacs" teaches Emacs how to deal with these pending
;; regions (like forbidding some operations until a pending region is
;; updated).  The section "Basic use of pending regions" is for simple
;; functions that use the "pending region" feature.  The section
;; "Giving up on asynchronicity" provides tools when you give up, and,
;; really need to freeze Emacs and block the user.  The section "Dev &
;; debug" contains tools that are useful only for development and
;; debugging.
;;
;; This file does *NOT* depend on Org.

;; org execute subtree bug
;;

;;; Code:

(require 'cl-lib)
(require 'string-edit)

;;; Errors
(define-error 'org-pending-error
              "Some content is pending, cannot modify it")


(define-error 'org-pending-user-cancel
              "The user canceled this update")

(define-error 'org-pending-timeout-error
              "Timeout error.
Raise, e.g. by `org-pending-wait-condition'.")


;;; Status
(defun org-pending-status-face (status)
  "Return the face to use to this STATUS."
  (pcase status
    (:scheduled 'org-async-scheduled)
    (:pending   'org-async-pending)
    (:failure   'org-async-failure)
    (:success   nil)
    (_ (error "Not a status"))
    ))

(defun org-pending--status-still-pending-p (status)
  "Non-nil if status means the content is still pending."
  (memq status '(:scheduled :pending)))


;;; Keymaps
;;
(defvar org-pending--outcome-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'org-pending--describe-penreg-at-point)
    map)
  "Keymap for outcome overlays.")

(defvar org-pending--pending-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'org-pending--describe-penreg-at-point)
    map)
  "Keymap for pending region overlays.")

;;; Helpers
;;

;;;; Overlay projections
;;
(defvar org-pending--overlay-projection-props
  `( face secondary-selection
     font-lock-face secondary-selection
     help-echo "Overlay projection..."
     read-only t
     org-pending--overlay-projection t)
  "Properties used to \"project\" an overlay as text.  See
`org-pending--add-overlay-projection'.

Note that the display is not always the requested one: Org font-lock
rules do not comply with `font-lock-face' and may override/delete our
instructions.")


(defun org-pending--add-overlay-projection (ovl read-only)
  "Add some properties to the text below the overlay OVL.
When using indirect buffers, the overlay belongs to one buffer; the text
properties, as they belong to the text, will be visible in any base or
indirect buffers ... at least, when org font-lock rules don't delete
them.  Add READ-ONLY to modification-hooks, insert-in-front-hooks and
insert-behind-hooks."
  (with-current-buffer (overlay-buffer ovl)
    (let ((inhibit-read-only t)
          (start (overlay-start ovl))
          (end (overlay-end ovl)))
      (add-text-properties start end
                            `(org-pending--projection-of ,ovl))
      (add-text-properties start end
                           org-pending--overlay-projection-props)
      (add-text-properties start end
                           `( modification-hooks ,read-only
                              insert-in-front-hooks ,read-only
                              insert-behind-hooks ,read-only)))))


(defun org-pending--remove-overlay-projection (ovl)
  "Remove the text projection of the overlay OVL.
See `org-pending--add-overlay-projection'."
  (with-current-buffer (overlay-buffer ovl)
    (let ((inhibit-modification-hooks t)
          (inhibit-read-only t)
          (start (overlay-start ovl))
          (end (overlay-end ovl)))
      (remove-text-properties start end
                              `( modification-hooks :not-used
                                 insert-in-front-hooks :not-used
                                 insert-behind-hooks :not-used
                                 org-pending--projection-of :not-used))
      (remove-text-properties start end
                              org-pending--overlay-projection-props))))


;;;; Overlays
;;

(defun org-pending--make-overlay (type beg-end)
  "Create a pending overlay between BEG-END.

The pair BEG-END contains 2 positions (BEG . END).
Create an overlay between BEGIN and END.  Return it.

See `org-pending--delete-overlay' to delete it."
  (let ((overlay (make-overlay (car beg-end) (cdr beg-end)))
        (read-only
	 (list
	  (lambda (&rest _)
	    (signal 'org-pending-error
	            (list "Cannot modify a region containing pending content"))))))
    (cl-flet ((make-read-only (ovl)
                "Make the overly OVL read-only."
               (overlay-put ovl 'modification-hooks read-only)
               (overlay-put ovl 'insert-in-front-hooks read-only)
               (overlay-put ovl 'insert-behind-hooks read-only)))
      (overlay-put overlay 'org-pending type)
      (unless (memq type '(:success :failure))
        (overlay-put overlay 'face 'secondary-selection)
        (overlay-put overlay 'help-echo "This content is pending. Click to know more."))


      ;; Hack to detect if our overlay has been copied into an other
      ;; buffer.
      (overlay-put overlay 'org-pending--owner (overlay-buffer overlay))

      (when (memq type '(:success :failure))
        ;; Add a link to the outcome overlay so that we may remove it
        ;; from any buffer.
        (add-text-properties (car beg-end) (cdr beg-end)
                             (list 'org-pending--outcome-overlay overlay))
        (overlay-put overlay 'keymap org-pending--outcome-keymap)
        (overlay-put overlay 'evaporate t))

      (when (eq :region type)
        ;; Cleanup outcome overlays if any.
        (let ((sstart (car beg-end))
              (slimit (cdr beg-end)))
          (while (and sstart (< sstart slimit))
            (when-let ((ovl (get-text-property sstart 'org-pending--outcome-overlay)))
              (when (overlay-buffer ovl)
                (delete-overlay ovl)))
            (setq sstart (next-single-property-change
                          sstart 'org-pending--outcome-overlay nil slimit)))
          (remove-text-properties (car beg-end) (cdr beg-end)
                                  (list 'org-pending--outcome-overlay :not-used)))

        (org-pending--add-overlay-projection overlay read-only)
        (overlay-put overlay 'org-pending--before-delete
                     (lambda ()
                       (let ((inhibit-modification-hooks t)
                             (inhibit-read-only t))
                         (overlay-put overlay 'modification-hooks nil)
                         (overlay-put overlay 'insert-in-front-hooks nil)
                         (overlay-put overlay 'insert-behind-hooks nil)
                         (org-pending--remove-overlay-projection overlay)
                         ;; Force refontification of the result
                         ;; (working around indirect buffers hacks).
                         (let ((start (overlay-start overlay))
                               (end   (overlay-end overlay)))
                           (remove-text-properties start end
                                                   (list 'fontified :not-used
                                                         'font-lock-face :not-used
                                                         'font-lock-fontified :not-used))
                           (font-lock-flush start end)))))
        (overlay-put overlay 'keymap org-pending--pending-keymap))
      (unless (memq type '(:success :failure))
        (make-read-only overlay))
      overlay)))





(defun org-pending--delete-overlay (ovl)
  "Delete the overlay OVL.
Assume OVL has been created with `org-pending--make-overlay'."
  (when-let ((before-delete (overlay-get ovl 'org-pending--before-delete)))
    (funcall before-delete))
  (delete-overlay ovl))


;;;; Error popup
;;

(defun org-pending--popup-failure-details (exc)
  "Notify/display the error EXC."
  (with-output-to-temp-buffer "*org pending content error*"
    (princ (if (consp exc)
               (format "%s\n%s\n" (car exc) (cdr exc))
             (format "%s\n" exc)))))





;;; PENREG: Structure to control one PENDing REGion

;;;; Definition and properties
(cl-defstruct (org-pending-penreg
               (:constructor org-pending--make))
  ( id nil
    :documentation
    "Unique identifier of this PENREG for this Emacs instance.")

  ( region nil
    :documentation
    "(read-only constant) The pending region: a pair of positions
(begin marker . end marker). This is the target of the update. Its
content will be updated on success.")

  ( scheduled-at nil
    :documentation
    "When the pending region was created (float-time).")

  ( outcome-at nil
    :documentation
    "When the outcome was received; nil if not received yet.")

  ( outcome nil
    :documentation
    "The outcome. nil when not know yet. Else a list: (:success RESULT)
or (:failure ERROR)")

  ( -alist nil
    :documentation
    "(internal) An alist containing some other information.")

  ( before-kill-function nil
    :documentation
    "When non-nil, function called before Emacs kills this PENREG, with
no argument.")

  ( insert-details-function nil
    :documentation
    "When non-nil, function called to insert custom details at the end of
`org-pending-describe-penreg'.  Assuming B is a (virtual) buffer
containing detailed human readable information about this PENREG, insert
at point details from START to END.  Handle cases where START, END are
nil or out of bounds without raising an error.  The function may use
text properties, overlays, etc.")
  ( properties nil
    :documentation
    "A alist of properties.  Useful to attach custom features to this PENREG." ))

(defun org-pending-penreg-owner (penreg)
  "The buffer that owns this pending region; it may be the base
buffer or an indirect one.

A PENREG belongs to one buffer, the buffer that is current when it is
created.  For example, if you flag some content as /pending/ in an
indirect buffer, that /pending region/ belongs to that indirect buffer,
and, control of that /pending region/ must happen in that buffer."
  (marker-buffer (org-pending-penreg-region penreg)))

(defun org-pending-penreg-status (penreg)
  "Return the status of PENREG: :scheduled, :pending, :success or :failure."
  (funcall (cdr (assq 'get-status (org-pending-penreg--alist penreg)))))

(defun org-pending-penreg-live-p (penreg)
  "Return non-nil if PENREG is still live.
A PENREG stays live until it receives its outcome: :success or :failure."
  (funcall (cdr (assq 'get-live-p (org-pending-penreg--alist penreg)))))

(defun org-pending-penreg-duration (penreg)
  "Return the duration between the scheduling and the outcome.
If the outcome is not known, use the current time."
  (let ((start (org-pending-penreg-scheduled-at penreg))
        (end (or (org-pending-penreg-outcome-at penreg)
                 (float-time))))
    (- end start)))

(defun org-pending-penreg-property (penreg prop)
  (cdr (assq prop (org-pending-penreg-properties penreg))))

(defun org-pending-penreg-set-property (penreg prop val)
  (if-let ((b (assq prop (org-pending-penreg-properties penreg))))
      (setcdr b val)
    (push (cons prop val)
          (org-pending-penreg-properties penreg))))

(gv-define-simple-setter org-pending-penreg-property
                         org-pending-penreg-set-property)


;;;; Creating a pending region
;;

(cl-defun org-pending (region &key anchor name on-outcome)
  "Mark a REGION as \"pending\" and return its PENREG.

Return the PENREG that allows to manage the pending region.

The argument REGION is a pair (start position . end position).  Protect
the REGION from modifications until the PENREG receives a :success or a
:failure update.  Display progress when PENREG receives :progress
updates.  Do not delete the previous content of REGION.

The argument ANCHOR, when given, is a pair (start position . end
position).  Use the ANCHOR region to display the progress.  When ANCHOR
is not given, use the first line of REGION.

Assume the region REGION contains the region ANCHOR.

On receiving the outcome (sent with `org-pending-send-update'),
remove the REGION protection.  If the outcome is a success, call
HANDLE-RESULT with the value from the SOURCE position.  If HANDLE-RESULT
returns the outcome region (a pair (start position . end position)),
report the success using visual hints on that region.  If the outcome is
a failure, report the failure using REGION.

You may want to connect a task with that PENREG (see
`org-pending-ti-connect').  You may send progress updates, and, FIXME
eventually, you must send a :success or a :failure update (see
`org-pending-send-update')."
  (unless region
    (error "Now illegal"))

  (let* ((to-marker (lambda (p)
                     ;; Make sure P is a marker.
                      (or (and (markerp p) p)
                          (save-excursion (goto-char p) (point-marker)))))
         sentinel
         x-last-status
         penreg
         (internals
          `( (get-status . ,(lambda () x-last-status))
             (set-status . ,(lambda (v) (setq x-last-status v)))
             (creation-point . ,(point-marker)))))

    (push (cons 'get-live-p
                (lambda () (when-let ((anchor-ovl (cdr (assq 'anchor-ovl internals))))
                             (overlay-buffer anchor-ovl))))
          internals)

    (setq region (cons (funcall to-marker (car region))
                       (funcall to-marker (cdr region))))
    (save-excursion
      (setq anchor
            (if (not anchor)
                (let ((abeg ;; First non-blank point in region.
                       (save-excursion (goto-char (car region))
                                       (re-search-forward "[[:blank:]]*")
                                       (point-marker)))
                      (aend ;; Last position on first line
                       (save-excursion (goto-char (car region))
                                       (end-of-line)
                                       (point-marker))))
                  (cons abeg aend))
              (cons (funcall to-marker (car anchor))
                    (funcall to-marker (cdr anchor))))))
    (cl-labels
        ((add-style (status txt)
           "Add the style matching STATUS over the text TXT."
           (propertize txt 'face (org-pending-status-face status)))

         (short-version-of (msg)
           "Compute the short version of MSG, to display on the anchor.
         Must return a string."
           (if msg
               (car (split-string (format "%s" msg) "\n" :omit-nulls))
             ""))
         (update (status data)
           (let ((anchor-ovl (cdr (assq 'anchor-ovl internals)))
                 (region-ovl (cdr (assq 'region-ovl internals)))
                 outcome-region)
             (unless (memq status '(:scheduled :pending :failure :success))
               (error "Invalid status"))
             ;; Update the title overlay to match STATUS and DATA.
             (funcall (cdr (assq 'set-status internals)) status)
             (overlay-put anchor-ovl
                          'face
                          (org-pending-status-face status))
             (overlay-put anchor-ovl
                          'before-string
                          ;; The text property projection hack (for
                          ;; indirect buffers) is leaking its
                          ;; background colour to the status flag; we
                          ;; try to undo this by forcing the
                          ;; background colour for the status,
                          ;; hopefully matching the buffer one.
                          (propertize
                           (pcase status
                             (:scheduled "⏱")
                             (:pending "⏳")
                             (:failure "❌")
                             (:success "✔️"))
                           'face (list :background (face-attribute 'default :background))))
             (unless (memq status '(:success :failure))
               (overlay-put anchor-ovl
                            'after-string
                            (propertize (format " |%s|" (short-version-of data))
                                        'face (org-pending-status-face status))))
             (when (memq status '(:success :failure))
               ;; NOTE: `sit-for' doesn't garantuee we'll be
               ;;       at the same pos when exiting.
               (save-excursion (sit-for 0.2))

               ;; We remove all overlays and let org insert the result
               ;; as it would in the synchronous case.
               (org-pending--delete-overlay anchor-ovl)
               (setq anchor-ovl nil)
               (org-pending--delete-overlay region-ovl)
               (setq region-ovl nil))

             (when (memq status '(:success :failure))
               (setf (org-pending-penreg-outcome penreg) (list status data))
               (setf (org-pending-penreg-outcome-at penreg) (float-time))

               (when on-outcome
                 (setq outcome-region (funcall on-outcome (list status data)))))

             (when (and (memq status '(:failure :success))
                        outcome-region)
               ;; We add some outcome decorations to let the user now
               ;; what happened and allow him to explore the details.
               (let ((outcome-ovl (org-pending--make-overlay status outcome-region))
                     (bitmap (pcase status
                               (:success 'large-circle)
                               (:failure 'exclamation-mark)))
                     (face (pcase status
                             (:success 'org-done)
                             (:failure 'org-todo))))
                 (overlay-put outcome-ovl
                              'before-string (propertize
                                              "x" 'display
                                              `(left-fringe ,bitmap ,face)))
                 (overlay-put outcome-ovl 'org-pending-penreg penreg)))))

         (remove-previous-overlays ()
           "Remove previous status overlays.
         If this region is already a pending one that is owned by an
         other buffer, raise a user error (even for failed ones).  If
         this region is already scheduled or pending, raise a user
         error."
           ;; Raise if an other (indirect) buffer owns this region.
           (dolist (pi (org-pending-contents-in (max (1- (car anchor)) (point-min))
                                                (cdr region)))
             (let ((pi-owner (org-pending-penreg-owner pi)))
               (unless (eq (current-buffer) pi-owner)
                 (error "Pending region owned by another buffer %s" pi-owner))))

           (let (ovls penreg)
             ;; Scan for previous overlays and identify the penreg.
             (dolist (ovl (overlays-in (max (1- (car anchor)) (point-min))
                                       (cdr region)))
               (when-let ((type (overlay-get ovl 'org-pending)))
                 (push ovl ovls)
                 (when (eq :status type)
                   (when penreg
                     (error "More than one status overlay"))
                   (setq penreg (overlay-get ovl 'org-pending-penreg)))))
             (when penreg
               (user-error "This region is already scheduled or pending."))
             ;; Delete previous pending decorations.
             (mapc (lambda (x) (org-pending--delete-overlay x))
                   ovls))))

      (remove-previous-overlays)

      ;; Create the overlays for the anchor and for the region.
      (push (cons 'region-ovl (org-pending--make-overlay :region region))
            internals)
      (push (cons 'anchor-ovl (org-pending--make-overlay :status anchor))
            internals)

      ;; Flag the result as ":scheduled".
      (update :scheduled nil)

      (setq sentinel
            (lambda (upd-message)
              (let* ((pt (cdr (assq 'creation-point internals)))
                     (buf (marker-buffer pt)))
                (message "org-pending: Handling update message at %s@%s: %s"
                         pt buf upd-message)
                (save-excursion
                  (with-current-buffer buf
                    (save-excursion
                      (goto-char pt)
                      (pcase upd-message
                        (`(:success ,r)
                         ;; Visual beep that the result is available.
                         (update :success r))

                        (`(:progress ,p)
                         ;; Still waiting for the outcome. Update our
                         ;; overlays with the progress info R.
                         (update :pending p))

                        (`(:failure ,err)
                         ;; We didn't get a result.
                         (update :failure err))

                        (_ (error "Invalid message"))))))
                nil)))
      (setq penreg
            (org-pending--make
             :-sentinel sentinel
             :region region
             :-alist internals
             :scheduled-at (float-time)))

      (overlay-put (cdr (assq 'anchor-ovl internals))
                   'org-pending-penreg penreg)
      (overlay-put (cdr (assq 'region-ovl internals))
                   'org-pending-penreg penreg)
      (org-pending--mgr-handle-new-penreg penreg name)
      penreg)))


;;;; Describing a pending region for the user
;;


(defun org-pending-describe-penreg (penreg)
  "Describe PENREG in a buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Pending content info*")))
    (with-output-to-temp-buffer buffer
      (with-current-buffer buffer
        (cl-labels
            ((time-to-string (x) (if x (format-time-string "%T" x) "-"))
             (bool-to-string (x) (if x "yes" "no"))
             (insert-link (m)
               (insert
                (propertize
                 (format "pos %s in buffer %s" (+ 0 m) (marker-buffer m))
                 'face 'org-link
                 'keymap
                 (let ((km (make-sparse-keymap)))
                   (define-key km [mouse-1]
                               (lambda (&rest _)
                                 (interactive)
                                 (let ((b (marker-buffer m)))
                                   (pop-to-buffer b)
                                   (goto-char m))))
                   km))))
             (insert-region (r)
               (insert "[")
               (insert-link (car r))
               (insert "..")
               (insert-link (cdr r))
               (insert "]"))
             (insert-value (value)
               (cond
                ((markerp value) (insert-link value))
                ((functionp value) (funcall value))
                (t (insert (format "%s" value)))))
             (one-line (label value)
               (insert (propertize (format "%13s" label)
                                   'face 'outline-1))
               (insert ": ")
               (insert-value value)
               (insert "\n"))
             (multi-line (label value)
               (insert (propertize (format "%13s" label)
                                   'face 'outline-1))
               (insert ":\n")
               (let ((start (point)))
                 (insert-value value)
                 (unless (bolp) (insert "\n"))
                 (indent-rigidly start (point) (+ 13 4))
                 ))
             (field (label value)
               ;; Print using one-line or multi-line, whichever
               ;; seems better.
               (let* ((tv (format "%s" value))
                      (last (1- (length tv))))
                 (when (and (>= last 0) (equal ?\n (elt tv last)))
                   (setq tv (substring tv 0 last)))
                 (if (> (length (split-string tv "\n")) 1)
                     (multi-line label value)
                   (one-line label tv)))))

          ;; ... ok, back to real work.
          (one-line "Id"
                    (org-pending-penreg-id penreg))
          (one-line "Status"
                    (substring (symbol-name (org-pending-penreg-status penreg)) 1))
          (one-line "Live?"
                    (bool-to-string (org-pending-penreg-live-p penreg)))
          (one-line "Region"
                    (lambda () (insert-region (org-pending-penreg-region penreg))))
          (one-line "Scheduled at"
                    (time-to-string (org-pending-penreg-scheduled-at penreg)))
          (one-line "Outcome at"
                    (time-to-string (org-pending-penreg-outcome-at penreg)))
          (one-line "Duration"
                    ;; TODO nice human format like 1m27s
                    (format "%.1fs" (org-pending-penreg-duration penreg)))
          (field "Result"
                 (if-let ((outcome (org-pending-penreg-outcome penreg)))
                     (pcase outcome
                       (`(:success ,v) v)
                       (_ "-"))
                   "-"))
          (field "Error"
                 (if-let ((outcome (org-pending-penreg-outcome penreg)))
                     (pcase outcome
                       (`(:failure ,v) v)
                       (_ "-"))
                   "-"))
          ;; Insert up to 1M of log.
          (multi-line "Details"
                      (lambda ()
                        (when-let ((insert-details (org-pending-penreg-insert-details-function penreg)))
                          (funcall insert-details nil (* 1024 1024))))))))))

(defun org-pending--describe-penreg-at-point ()
  "Describe the pending content at point.
Get the PENREG at point (pending content or an outcome).  Use
`org-pending-describe-penreg' to display it."
  (interactive)
  (let ((penreg (or (get-char-property (point) 'org-pending-penreg)
                   (when-let ((ovl (get-char-property (point) 'org-pending--projection-of)))
                     (overlay-get ovl 'org-pending-penreg)))))
    (if penreg
        (org-pending-describe-penreg penreg)
      (user-error "No pending content at point"))))



(defun org-pending-send-update (penreg message)
  "Send the status update to the PENREG.

The udpate MESSAGE must be one of the following:
    - (:success R):   New content is ready; the result is R; Emacs
      need to call HANDLE-RESULT with R.
    - (:failure ERR): Something failed; the error is ERR; no new
      content; Emacs needs to close the pending region, marking it as
      failed.
    - (:progress P): Content is still pending; current progress is P;
      Emacs may display it near the pending region.

You may send as many :progress updates as you want (or none).
Eventually, you must send one, and only one, of either a :success or a
:failure. Until you do, the region will be protected from modifications."
  (funcall (org-pending-penreg--sentinel penreg) message))



;;; Checking for pending regions

(defun org-pending-contents-in (start end &optional owned)
  "Return the list of pending contents in BEGIN..END.

Return the list of PENREG(s) for pending contents, in the current buffer.

When OWNED is non-nil, ignore pending contents that are not owned by
this buffer.

See also `org-buffer-pending-contents-p'."
  (let ((penregs)
        (here nil)
        ovl)
    (while (and (< start end)
                (setq here (text-property-any start end
                                              'org-pending--overlay-projection t)))
      (setq ovl (get-text-property here 'org-pending--projection-of))
      (if (not (overlay-buffer ovl))
          ;; Got a dead overlay, skipping it.
          (setq start (or (text-property-any here end
                                             'org-pending--overlay-projection nil)
                          end))
        (when (or (not owned)
                  (eq (current-buffer) (overlay-buffer ovl)))
          (push (overlay-get ovl 'org-pending-penreg) penregs))
        (setq start (1+ (min end (overlay-end ovl))))))
    penregs))

(defun org-buffer-pending-contents-p (&optional buffer owned-only)
  "Return non-nil if BUFFER owns some pending contents.
BUFFER is a buffer or a buffer name.  When BUFFER is nil, use the
current buffer.  Ignore pending contents that failed.

See also `org-pending-contents-in'."
  (setq buffer (or (and buffer (get-buffer buffer))
                   (current-buffer)))
  (with-current-buffer buffer
    (without-restriction
      (org-pending-contents-in (point-min) (point-max)
                               owned-only))))


(defun org-ensure-no-pending-contents (begin end &optional error-info)
  "Raise `org-pending-error' if BEGIN..END contains pending contents."
  (when (org-pending-contents-in begin end)
    (signal 'org-pending-error (cons begin (cons end error-info)))))

(defun org-emacs-pending-contents-p ()
  "Return non-nil if any buffer contains some pending contents."
  (catch 'found-one
    (dolist (p (org-pending-list))
      (when (org-pending--status-still-pending-p
             (org-pending-penreg-status p))
        (throw 'found-one t)))
    nil))


;;; Managing pending regions
;; The manager contains past & present pending regions.  This allows
;; to check the history of pending regions.  For example, on success,
;; the updated text becomes a regular text without any information
;; that it was a pending region; you may still recover some info using
;; the manager. Also, on error (failed, canceled, etc.), Emacs
;; displays only the last error; you may use the history to check
;; previous attempts and the reason why they failed.
;;
;; FIXME: Implement garbage collection for pending contents.  We need
;;        a strategy to free obsolete pending contents automatically.
;;

;;;; Internals
;;

(cl-defstruct (org-pending--manager
               (:constructor org-pending--create-manager)
	       (:copier nil))
  ; An id (integer) uniquely identifies one PENREG.
  used-names ; obarray of in-use names.
  penregs ; The list of PENREGs, past & present.
  )



(defvar org-pending--manager nil
  "The global pending content manager.")

(defun org-pending--manager ()
  "Get/create the global manager for pending contents."
  (unless org-pending--manager
    (setq org-pending--manager (org-pending--create-manager
                                :used-names (obarray-make)))
    (add-hook 'kill-emacs-query-functions #'org-pending--kill-emacs-query))
  org-pending--manager)

(defun org-pending--mgr-handle-new-penreg (penreg name)
  "Handle this new pending content PENREG.
Update PENREG as needed. Return nothing."
  (let* ((mgr (org-pending--manager)))
    (add-function
     :after (org-pending-penreg--sentinel penreg)
     (lambda (message)
       (org-pending--mgr-handle-penreg-update penreg message)))
    (push penreg (org-pending--manager-penregs mgr))
    (unless name (setq name "PENREG"))

    ;; Making NAME unique.
    (let* ((ob (org-pending--manager-used-names mgr))
           (sfx 0)
           to-try)
      (setq name
            (if (not (intern-soft name ob)) name
              (setq to-try (concat name (format "-%d" sfx)))
              (while (intern-soft to-try ob)
                (cl-incf sfx)
                (setq to-try (concat name (format "-%d" sfx))))
              to-try))
      (intern name ob))

    (setf (org-pending-penreg-id penreg) name)
    nil))

(defun org-pending--mgr-handle-penreg-update (penreg update)
  "Handle the update UPDATE for this PENREG.
Return nothing."
  (message "org-pending: penreg update for id=%s: %s"
           (org-pending-penreg-id penreg) update))

;;;; API
;;

(defun org-pending-list ()
  "Return the list of PENREGs.
This is a global list for this Emacs instance, in any org buffer.  It
includes past and present PENREGs."
  (org-pending--manager-penregs (org-pending--manager)))

(defun org-pending--kill (penreg)
  "Kill this PENREG.

Do nothing if this PENREG is not live anymore.

Call `org-pending-penreg-before-kill-function' with PENREG if any.  If
the PENREG is still live, make it fail with org-pending-user-cancel
error.

Return nothing.

Do not ask for confirmation or interact in any way."
  (when (org-pending-penreg-live-p penreg)
    (when-let ((before-kill (org-pending-penreg-before-kill-function penreg)))
      (funcall before-kill penreg))
    ;; Unlock the region, marking the PENREG as failed due to
    ;; cancellation.
    (when (org-pending-penreg-live-p penreg)
      (org-pending-send-update
       penreg (list :failure (list 'org-pending-user-cancel
                                    "Canceled"))))
    (setf (org-pending-penreg-region penreg) nil)
    nil))


;;; Plugging into Emacs
;;
(defun org-pending--kill-buffer-query ()
  "For `kill-buffer-query-functions'.
If the current buffer contains pending contents,
offer to abort killing the buffer."
  ;; TODO: Offer to jump to the list of this buffer pending contents
  ;;
  ;; For an indirect buffer, we can kill it even if there are pending
  ;; contents, as long as the buffer doesn't own any of them.  For a
  ;; base buffer, we can't kill it if they are any pending contents
  ;; (as this would kill all indirect buffers).
  ;; WARNING: Emacs (<=30.1) may segfault if the indirect buffer
  ;;          rejects the kill and its base buffer didn't (see
  ;;          bug#69529).
  (let* ((b (current-buffer))
         (owned-only (buffer-base-buffer b)))
    (if (not (org-buffer-pending-contents-p b owned-only))
        :ok-to-kill
      (when (y-or-n-p (format (concat "Some content is pending in buffer '%s'"
                                      " (or its indirect buffers), kill anyway?")
			      (buffer-name)))
        ;; Force killed: cancel the pending regions of this buffer.
        (without-restriction
          (dolist (pi (org-pending-contents-in (point-min) (point-max)
                                               :owned-only))
            (org-pending--kill pi)))
        :forced-kill))))

(defun org-pending--kill-emacs-query ()
  "For `kill-emacs-query-functions'.
If there are any pending contents, offer to abort killing Emacs."
  ;; TODO: Offer to jump to the list of the pending contents
  (if (not (org-emacs-pending-contents-p))
      :ok-to-kill
    (when (yes-or-no-p (format "Some org content is pending, kill anyway?"))
      ;; Forced kill: cancel all pending regions
      (dolist (pi (org-pending-list))
        (org-pending--kill pi))
      :forced-kill)))

(defun org-pending--after-indirect-clone ()
  "For `clone-indirect-buffer-hook'.
Fix pending contents, after creating an indirect clone."
  (unless (buffer-base-buffer (current-buffer))
    (error "Bad call: not an indirect buffer: %s" (current-buffer)))

  ;; Try to detect and delete overlays that have been wrongly copied
  ;; from other buffers.
  (mapc (lambda (o)
          (when-let ((owner (overlay-get o 'org-pending--owner)))
            (unless (eq owner (overlay-buffer o))
              (delete-overlay o))))
        (overlays-in (point-min) (point-max)))

  ;; jit-lock does not work in indirect buffers; let's say that, if
  ;; there is a face property on our pendings, it can only be the
  ;; wrong one.  We probably could rescan only the region matching our
  ;; indirect buffer...
  (with-current-buffer (buffer-base-buffer (current-buffer))
    (dolist (penreg (org-pending-contents-in (point-min) (point-max)))
      (let ((region (org-pending-penreg-region penreg)))
        (when region
          (let ((inhibit-modification-hooks t)
                (inhibit-read-only t))
            (remove-text-properties (car region) (cdr region)
                                    (list 'face :not-used))))))))


;;; Basic use of pending regions
;;

;;;; Prompt the user to edit a region
(defun org-pending-user-edit--replacer (buf start end)
  "Helper for `org-pending-user-edit'.
Return a function that takes some text and replaces the region between
START and END in buffer BUF with it.  Return the new region as a
pair (start point . end point).

The returned function silently do nothing when the buffer is dead."
  (lambda (new-txt)
    (with-current-buffer buf
      (save-excursion
        (when (buffer-live-p buf)
          (if (> (- end start) 1)
              ;; Insert in the middle as it's more robust to
              ;; keep existing data (text properties, markers,
              ;; overlays).
              (let ((ipoint (+ 0 (goto-char (1+ start)))))
                (setq end (progn (goto-char end) (point-marker)))
                (goto-char ipoint)
                (insert new-txt)
                (delete-region (point) end)
                (delete-region start ipoint)
                (cons start (point)))
            ;; Can't insert in the middle.
            (let ((old-end (point-marker))
                  new-end)
              (set-marker-insertion-type old-end nil)
              (insert new-txt)
              (setq new-end (point-marker))
              (delete-region start old-end)
              (cons start new-end)
              )))))))

(cl-defun org-pending-user-edit (prompt start end &key edit-name)
  "Ask the user to edit the region BEGIN .. END.

Like `string-edit' using a pending region.

Assume the region START..END is in the current buffer.

Switch to a new buffer to edit the region between START..END.  Protect
the region from modifications until the user finish editing.  Return the
edit buffer.

When the user finishes editing (with C-c C-c), replace the region with
the content and release the region.

If the user aborts (with C-c C-k), discard the edit and release the
region.

PROMPT will be inserted at the start of the buffer, but will not be
included in the region update.  If PROMPT is nil, no help text will be
inserted.

When non-nil, EDIT-NAME is the name to use for the edit buffer.  Make it
unique if needed."
  (let* ((buf (current-buffer))
         (to-update (buffer-substring start end))
         (penreg (org-pending
                 (cons start end)
                 (org-pending-user-edit--replacer buf start end)))
         edit-buffer
         closing)
    (string-edit prompt to-update
                 (lambda (new-text)
                   (org-pending-send-update
                    penreg
                    (list :success new-text)))
                 :abort-callback
                 (lambda ()
                   (org-pending-send-update
                    penreg
                    (list :failure (list 'user-error
                                         "Edition canceled by the user")))))
    ;; `string-edit' switches to the edit buffer.
    (setq edit-buffer (window-buffer (selected-window)))
    (when edit-name
        (with-current-buffer edit-buffer
          (rename-buffer (generate-new-buffer-name edit-name))))

    ;; "Kill buffer" means "cancel the edit"
    (with-current-buffer edit-buffer
      ;; Before done and abort, set "closing" to t.
      (cl-labels ((instrument (key cmd)
                    (keymap-set string-edit-mode-map
                                key (lambda ()
                                      (interactive)
                                      (setq closing t)
                                      (funcall cmd)))))
        (instrument "C-c C-c" #'string-edit-done)
        (instrument "C-c C-k" #'string-edit-abort))

      ;; When killed and not closing, send a 'cancel' message.
      (make-local-variable 'kill-buffer-query-functions)
      (push (lambda ()
              (when (and (not closing)
                         (org-pending-penreg-live-p penreg))
                (org-pending-send-update
                 penreg
                 (list :failure (list 'user-error
                                      "Edition buffer killed"))))
              :ok-to-kill)
            kill-buffer-query-functions))

    (setf (org-pending-penreg-before-kill-function penreg)
          (lambda ()
            (setq closing t)
            (when (buffer-live-p edit-buffer)
              (with-current-buffer edit-buffer
                (kill-buffer edit-buffer)))))

    (setf (org-pending-penreg-insert-details-function penreg)
          (lambda (_start _end)
            (let ((insert-link
                   (lambda (b)
                     (insert
                      (propertize
                       (format "%s" (buffer-name b))
                       'face 'org-link
                       'keymap
                       (let ((km (make-sparse-keymap)))
                         (define-key km [mouse-1]
                                     (lambda (&rest _)
                                       (interactive)
                                       (when (buffer-live-p b)
                                         (pop-to-buffer b))))
                         km))))))
              (insert "Edit buffer: ")
              (if edit-buffer
                  (funcall insert-link edit-buffer)
                (insert "-")))))
    edit-buffer))


;;; Giving up on asynchronicity
;;

;;;; Request no asynchronicity
;;

(defvar org-pending-without-async-flag nil
  "When non-nil, do not make asynchronous calls.

Used to disable asynchronous calls when Emacs doesn't support it (yet),
or, when it's impossible by design.

For example, when a Org block is executed asynchronously, if it depends
on the execution of other blocks, we have to execute those synchronously
for now.")

(defmacro org-pending-without-async (&rest body)
  "Execute BODY, forcing synchronous calls.

It sets `org-pending-without-async-flag' to a non-nil value.

Return the value of the last form."
  (declare (indent 0) (debug t))
  `(let ((org-pending-without-async-flag t))
     ,@body))


;;;; Blocking the user
;;

(cl-defun org-pending-wait-condition ( cond-p
                                         &key
                                         (tick .3) (message "Waiting")
                                         (nb_secs_between_messages 5)
                                         timeout)
  "Wait until the condition COND-P returns non-nil.

This function blocks the main thread and the user, until the condition
is true.  It's only intended as a workaround, when some feature cannot
work asynchronously.  Do not use if you can: stay asynchronous.

Raise when not called from the main thread.
Return the non-nil value returned by COND-P.

Repeatedly call COND-P with no arguments, about every TICK seconds,
until it returns a non-nil value.  Return that non-nil value.  When
TIMEOUT (seconds) is non-nil, raise an `org-pending-timeout-error' if
COND-P is still nil after TIMEOUT seconds.  Assume COND-P calls cost 0s.

Do not block display updates.  Do not block process outputs.  Do not
block idle timers.  Do block the user, letting them know why, but do not
display more messages than one every NB_SECS_BETWEEN_MESSAGES.  Default
MESSAGE is \"Waiting\".  Use 0.3s as the default for TICK."
  (unless (or
           ;; No thread support.
           (or (not (fboundp 'make-thread)) (not main-thread))
           ;; Wrong thread.
           (eq (current-thread) main-thread))
    (error "Must only be called from the main thread"))
  (let ((keep-waiting t)
        (result nil)
        (start (float-time))
        elapsed
        last-elapsed)
    (while keep-waiting
      (setq result (funcall cond-p))
      (if result
          (setq keep-waiting nil)
        (sleep-for 0.01)
        (redisplay :force)
        (setq elapsed (- (float-time) start))
        (when (and timeout (> elapsed timeout))
          (signal 'org-pending-timeout-error (list message elapsed)))
        ;; Let the user why Emacs hangs, without flooding the message area.
        (if (and last-elapsed (> (- elapsed last-elapsed) nb_secs_between_messages))
            (message (format "%s ...(%.1fs)" message elapsed)))
        (unless (sit-for tick :redisplay)
          ;; Emacs has something to do; let it process new
          ;; sub-processes outputs in case there are some.
          (accept-process-output nil 0.01))
        (setq last-elapsed elapsed)))
    result))

(cl-defun org-pending-wait-outcome ( try-get-outcome )
  "Wait the outcome; return or raise when available.

This function blocks the main thread and the user, until the condition
is true.  It's only intended as a workaround, when some feature cannot
work asynchronously.  Do not use if you can: stay asynchronous.

Call TRY-GET-OUTCOME to check if the outcome is available.
Once available, return on success, raise on failure."
  (let ((outcome (org-pending-wait-condition
                  (lambda () (funcall try-get-outcome)))))
    (pcase outcome
      (`(:success ,r) r)
      (`(:failure ,err) (signal (car err) (cdr err))))))



;;; Dev & debug
;;

(defun org-pending--reset-buffer ()
  "Reset the current buffer, throwing away of pending decorations.
Dev only. Use at your own risk."
  ;; TODO: Remove PENREGs from the manager if any.
  (save-excursion
    ;; First pass: removing pending overlays.
    (mapc (lambda (o)
            (when (overlay-get o 'org-pending)
              (org-pending--delete-overlay o)))
          (overlays-in (point-min) (point-max)))
    ;; Second pass: trying to remove our text properties.
    ;; We blindly remove everything we use, finger crossed...
    (goto-char (point-min))
    (let ((inhibit-modification-hooks t)
          (inhibit-read-only t)
          match start end)
      (while (and (< (point) (point-max))
                  (setq match (text-property-search-forward 'org-pending--overlay-projection t t)))
        (setq start (prop-match-beginning match))
        (setq end   (prop-match-end match))
        (remove-text-properties  start end
                                 `( modification-hooks :not-used
                                    insert-in-front-hooks :not-used
                                    insert-behind-hooks :not-used
                                    org-pending--projection-of :not-used))
        (remove-text-properties start end
                                org-pending--overlay-projection-props)))))


(provide 'org-pending)

;;; org-pending.el ends here
