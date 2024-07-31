;;; org-pending.el --- Regions with pending content -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

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

;;;; Overview
;;
;;
;; This library provides an API to lock a region while it is "being
;; updated"; the content of the region is "pending" and cannot be
;; modified.  It will be updated, later, when the new content is
;; available.
;;
;; While region is "pending", the library will mark it for the user,
;; displaying the current update progress.
;;
;; The update may yield success or failure.  On success, the region
;; content will be updated, and the update summary will be indicated.
;; On failure, the error log will be displayed.
;;
;; Locking regions is useful when the update is computed
;; asynchronously and/or depends on external events.
;;

;;;; How to use locks in your library
;;

;; To lock a region, you need to:
;;
;;    1. Call the function `org-pending' with the region to lock; use
;;       the ON-OUTCOME argument to tell Emacs how to update the
;;       region.  Keep the returned REGLOCK (you'll need it to send
;;       updates).
;;
;;    2. Start "something" that computes the new content.  That
;;       "something" may be a thread, a timer, a notification, a
;;       process, etc.  That "something" might optionally report
;;       :progress, and must eventually send a :success or :failure
;;       message (using `org-pending-send-update'): org-pending will
;;       update the pending region (using your ON-OUTCOME) and unlock
;;       it; at this point the lock is "dead" (see
;;       `org-pending-reglock-live-p').
;;
;; A lock is "live" (blocking its region) from when it's created until
;; it receives its outcome (:success or :failure).  Once the lock
;; receives its outcome, it's dead.
;;
;; You may read the current status using `org-pending-reglock-status'.
;; The status is updated when you send updates using
;; `org-pending-send-update'.
;;
;; | Status    | Type     | Region   | Live ? | Possible updates              | Outcome available | Outcome marks                  |
;; |-----------+----------+----------+--------+-------------------------------+-------------------+--------------------------------|
;; | :schedule | initial  | locked   | alive  | :progress, :success, :failure | no                | no                             |
;; | :pending  |          | locked   | alive  | :progress, :success, :failure | no                | no                             |
;; | :success  | terminal | unlocked | dead   |                               | yes               | if ON-OUTCOME returns a region |
;; | :failure  | terminal | unlocked | dead   |                               | yes               | if ON-OUTCOME returns a region |
;;
;;
;; Let's write an example that will lock a region, then, insert a
;; result on :success, and display a message on :failure.  The example
;; is written so that you can just uncomment it and run it.  The
;; example will use some fields of the REGLOCK object to alter its
;; behavior; feel free to look at:
;;    (cl-describe-type 'org-pending-reglock)
;; for the full documentation.
;;
;; (cl-defstruct my-counter (state 0) timer)
;; (defun my-counter-update (counter reglock &optional force-landing)
;;   "Increase COUNTER and send update to REGLOCK.
;; At the end of sequence, cancel COUNTER timer.
;; When FORCE-LANDING is symbol `land', report :success \"Landed early\" and cancel
;; the timer."
;;   (org-pending-send-update
;;    reglock
;;    (if force-landing
;;        (progn
;;          (when (timerp (my-counter-timer counter))
;;            (cancel-timer (my-counter-timer counter)))
;;          (pcase force-landing
;;            ('land
;;             '(:success "Landed early"))
;;            ('crashed
;;             '(:failure "Crashed"))))
;;      (pcase (my-counter-state counter)
;;        (0 '(:progress "Taking off..."))
;;        (1 '(:progress "Flying..."))
;;        (2 '(:progress "Landing..."))
;;        (_
;;         (when (timerp (my-counter-timer counter))
;;           (cancel-timer (my-counter-timer counter)))
;;         (if (= 0 (random 2))
;;             '(:success "Landed successfully")
;;           '(:failure "Landing malfunction"))))))
;;   (cl-incf (my-counter-state counter)))
;;
;; (let ((lock-buffer (generate-new-buffer "*Pending region example*"))
;;       (step-duration 2)
;;       reglock state)
;;   (with-current-buffer lock-buffer
;;     (insert "
;; Buffer displaying pending content.
;; OUTPUT>>>
;;
;; TO REPLACE
;;
;; <<<OUTPUT
;; More text.
;; ")
;;     (undo-boundary)
;;     (goto-char (point-min))
;;     (re-search-forward "TO REPLACE")
;;
;;     ;; We lock the 'region', defining how to update it when the
;;     ;; outcome is available.
;;     (setq reglock (org-pending (cons (match-beginning 0) (match-end 0))))
;;
;;     (pop-to-buffer lock-buffer)
;;
;;     (setq state (make-my-counter))
;;     ;; We create a timer to update our state every few seconds.
;;     (setf (my-counter-timer state)
;;           (run-with-timer step-duration step-duration
;;                           #'my-counter-update state reglock))))
;;
;;
;;;; Interface provided to the Emacs user
;;
;; The library makes locks visible to the user using text properties
;; and/or overlays.  It displays and updates the status while the
;; region is locked: the initial status is :scheduled, then, when
;; receiving progress it becomes :pending (with progress information
;; if any).  org-pending allows to diplay a description of the lock in
;; a new buffer, like, for example, `describe-package'.  From that
;; description buffer, the user may request to cancel that lock; see
;; the field `user-cancel-function' of the REGLOCK object if you need
;; to customize what to do on cancel.  By default, org-pending will
;; just send the update (list :failure 'org-pending-user-cancel) so
;; that the region is unlocked.
;;
;;
;; In our example above, we could customize the `user-cancel-function'
;; like this:
;;
;;
;;     (setf (org-pending-reglock-user-cancel-function reglock)
;;           `(lambda (rlock)
;;              (message "Request for emergency landing sent.")
;;              (run-with-timer 2 nil
;;                              '(lambda (rl)
;;                                 (message "Initiating emergency landing...")
;;                                 (my-counter-update
;;                                  ,state rl
;;                                  (if (= 0 (random 2)) 'land 'crashed))
;;                                 (message "Initiating emergency landing... done"))
;;                              rlock)))
;;
;;
;;
;; When receiving the outcome (:success or :failure), after unlocking
;; the region, the library may leave information about the outcome
;; (using text properties/overlays); it will leave an outcome mark
;; only if the ON-OUTCOME function returns the outcome region (see
;; `org-pending').  If that outcome information is (still) displayed,
;; Emacs allows to display a description of that lock.  From that
;; description, the user may decide to "forget" that lock; "forgetting
;; the lock" removes the outcome visual marks, and, it allows
;; org-pending to discard any information related to this lock.
;;
;; Note that the visual marks of an outcome are silently removed if
;; the library needs to (like when creating a new lock, or when
;; reverting the buffer).
;;
;; The description of a lock (live or dead) provides information like
;; the schedule time, the duration, the outcome time, the result (in
;; case of success), the error (in case of failure), etc.  Customize
;; the field `insert-details-function' of REGLOCK object to add your
;; own information.
;;
;; In our example above, we could customize the
;; `insert-details-function' like this:
;;
;;     (setf (org-pending-reglock-insert-details-function reglock)
;;           `(lambda (rlock)
;;              ;; We add the current state at the end of the description
;;              ;; buffer.
;;              (insert (format "State: %s\n"
;;                              (my-counter-state ,state)))))
;;
;;
;; If the user kills a buffer, or, kills Emacs, some locks may have to
;; be destroyed.  The library will ask the user to confirm if an
;; operation requires to destroy some locks.  See the field
;; `before-destroy-function' of REGLOCK object, if you need to do
;; something before a lock is destroyed.
;;
;; In our example above, we could customize the function
;; `before-destroy-function' like this:
;;
;;     (setf (org-pending-reglock-before-destroy-function reglock)
;;           `(lambda (_rlock)
;;              (cancel-timer (my-counter-timer ,state))
;;              (message "Transponder signal lost")))



;;;; Examples of functions using this library
;;
;; Here are examples of functions using this library:
;;     - `org-pending-user-edit': prompt the user to edit a region,
;;     - `org-pending-updating-region': lock a region while executing
;;       some elisp,
;;     - `org-babel-execute-src-block': execute source blocks
;;       asynchronously.
;;     - and `org-dblock-update': execute dynamic blocks
;;       asynchronously.
;;

;;;; Content of this file
;;
;; The section "REGLOCK" describes the REGLOCK structure, how to lock
;; pending regions, how to describe them to the user and how to update
;; them.  The section "Checking for reglocks" allows to check for
;; reglocks in regions, in buffers or in Emacs.  The section "Managing
;; locks" is about managing all (alive or dead) locks in Emacs.  The
;; section "Managing outcomes" is about managing outcome marks.  The
;; section "Plugging into Emacs" teaches Emacs how to deal with these
;; locks (like forbidding some operations while there are live locks).
;; The section "Basic use of locks" is for simple functions that use
;; the lock feature.  The section "Giving up on asynchronicity"
;; provides tools when you give up, and, really need to freeze Emacs
;; and block the user.  The section "Dev & debug" contains tools that
;; are useful only for development and debugging.
;;
;; This file does *NOT* depend on Org mode.

;;; Code:

(require 'cl-lib)
(require 'string-edit)
(require 'compile)

;;; Configuration
;;
(defcustom org-pending-confirm-ignore-reglocks-on-exit t
  "Non-nil means that Emacs checks reglocks on exit.
When non-nil, if there are pending locks when exiting Emacs, org pending
will offer you a chance to cancel the exit to not loose any data, leak
ressources, etc.

If you set this to nil, org pending will ignore pending locks, possibly
loosing data, leaking ressources, etc."

  :type 'boolean
  :package-version '(Org . "9.7"))

;;; Errors
(define-error 'org-pending-error
              "Some content is pending, cannot modify it")


(define-error 'org-pending-user-cancel
              "The user canceled this update")

;;; Faces
;;
(defface org-pending-scheduled
  `((t :background ,(face-attribute
                     'fringe
                     :background  nil t)))
  "Face for babel results for code blocks that are scheduled for execution."
  :package-version '(Org . "9.7"))

(defface org-pending-pending
  `((t :background ,(face-attribute
                     'compilation-mode-line-run
                     :foreground nil t)))
  "Face for babel results for code blocks that are running."
  :package-version '(Org . "9.7"))

(defface org-pending-outcome-failure
  `((((supports :underline (:style line)))
     :underline ( :style line
                  :color ,(face-attribute 'error :foreground nil 'default)))
    (t
     :underline t :inherit error))
  "Face for babel results fused for the outcome when it's a failure."
  :package-version '(Org . "9.7"))

(defface org-pending-outcome-success
  `((((supports :underline (:style line)))
     :underline ( :style line
                  :color ,(face-attribute 'success :foreground nil 'default)))
    (t
     :underline t :inherit success))
  "Face used for the outcome when it's a success."
  :package-version '(Org . "9.7"))


;;; Status
(defun org-pending-status-face (status)
  "Return the face to use to this STATUS."
  (pcase status
    (:scheduled 'org-pending-scheduled)
    (:pending   'org-pending-pending)
    (:failure   'org-pending-outcome-failure)
    (:success   'org-pending-outcome-success)
    (_ (error "Not a status"))))

(defun org-pending--status-still-pending-p (status)
  "Non-nil if STATUS means the content is still pending."
  (memq status '(:scheduled :pending)))


;;; Keymaps
;;
(cl-defun org-pending--new-button-like-keymap (&key read-only)
  "Return a new keymap for use on reglock overlays.
If READ-ONLY is non-nil, add bindings for read-only text else for
editable text."
  (let ((map (make-sparse-keymap)))
    (dolist (k `([mouse-1] [mouse-2] [touchscreen-down]))
      (define-key map k 'org-pending--describe-reglock-at-point))
    (when read-only
      (define-key map [13] 'org-pending--describe-reglock-at-point))
    map))

(defvar org-pending-outcome-keymap
  (org-pending--new-button-like-keymap :read-only nil)
  "Keymap for outcome overlays.")

(defvar org-pending-pending-keymap
  (org-pending--new-button-like-keymap :read-only t)
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
  "Properties used to \"project\" an overlay as text.

See `org-pending--add-overlay-projection'.

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
      (with-silent-modifications
        (add-text-properties start end
                             `(org-pending--projection-of ,ovl))
        (add-text-properties start end
                             org-pending--overlay-projection-props)
        (add-text-properties start end
                             `( modification-hooks ,read-only
                                insert-in-front-hooks ,read-only
                                insert-behind-hooks ,read-only))))))


(defun org-pending--remove-overlay-projection (ovl)
  "Remove the text projection of the overlay OVL.
See `org-pending--add-overlay-projection'."
  (with-current-buffer (overlay-buffer ovl)
    (let ((inhibit-modification-hooks t)
          (inhibit-read-only t)
          (start (overlay-start ovl))
          (end (overlay-end ovl)))
      (with-silent-modifications
        (remove-text-properties start end
                                `( modification-hooks :not-used
                                   insert-in-front-hooks :not-used
                                   insert-behind-hooks :not-used
                                   org-pending--projection-of :not-used))
        (remove-text-properties start end
                                org-pending--overlay-projection-props)))))


;;;; Overlays
;;

(defun org-pending--make-overlay (type beg-end)
  "Create a pending overlay of type TYPE between BEG-END.

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
        (with-silent-modifications
          (add-text-properties (car beg-end) (cdr beg-end)
                               (list 'org-pending--outcome-overlay overlay)))
        (overlay-put overlay 'keymap org-pending-outcome-keymap)
        (overlay-put overlay 'evaporate t))

      (when (eq :region type)
        ;; Cleanup outcome overlays if any.
        (org-pending-delete-outcome-marks (car beg-end) (cdr beg-end))

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
        (overlay-put overlay 'keymap org-pending-pending-keymap))
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





;;; REGLOCK: Structure to control one REGion LOCK

;;;; Definition and properties of a lock
(cl-defstruct (org-pending-reglock
               (:constructor org-pending--make))
  ( id nil
    :documentation
    "Unique identifier of this REGLOCK for this Emacs instance.")

  ( region nil
    :documentation
    "(read-only constant) The locked region: a pair of positions
(begin marker . end marker). This is the target of the update. Its
content may be updated on success.

Becomes nil when the lock has been destroyed.")

  ( scheduled-at nil
    :documentation
    "When the lock was created (float-time).")

  ( outcome-at nil
    :documentation
    "When the outcome was received (float-time); nil if not received yet.")

  ( outcome nil
    :documentation
    "The outcome. nil when not known yet. Else a list: (:success RESULT)
or (:failure ERROR)")

  ( before-destroy-function nil
    :documentation
    "When non-nil, function called before Emacs kills this REGLOCK, with the
REGLOCK as argument.")

  ( user-cancel-function #'org-pending--user-cancel-default
    :documentation
    "Function called when the user wish to cancel this REGLOCK,
with the REGLOCK as argument.  This function must return immediately; it
may, asynchronously, stop some processing and release resources; and,
once this is done, it should send the outcome to the REGLOCK (using
`org-pending-send-update', so that the region is unlocked and the
REGLOCK destroyed). The default value is
`org-pending--user-cancel-default'" )

  ( insert-details-function nil
    :documentation
    "When non-nil, function called to insert custom details at the end of
`org-pending-describe-reglock'.  The function is called with a REGLOCK;
it must insert the details at point; it may use text properties,
overlays, etc.  See `org-pending-describe-reglock'")

  ( properties nil
    :documentation
    "A alist of properties.  Useful to attach custom features to this REGLOCK." )

  ( -delete-outcome-marks (lambda ())
    :documentation
    "Internal.  Used to remove visual outcome hints if any.
By default, this is a noop.  When, and, if there are visual hints to
remove, org-pending--update takes care to update that function." )

  ( -creation-point nil
    :documentation
    "Current position (marker) when the lock was created.  Used to run
updates from the same position." )

  ( -status nil
    :documentation
    "Status. Managed by `org-pending-send-update'." )

  ( -get-live-p nil
    :documentation
    "See `org-pending-reglock-live-p'. Updated by `org-pending--update'." )

  ( -useless-p nil
    :documentation
    "See `org-pending-reglock-useless-p'. Updated by `org-pending--update'." )

  ( -anchor-ovl nil
    :documentation
    "Overlay for the anchor. See `org-pending'." )

  ( -region-ovl nil
    :documentation
    "Overlay for the region  See `org-pending'." )

  ( -on-outcome nil
    :documentation
    "See `org-pending' for the meaning of ON-OUTCOME." ))


(defun org-pending-reglock-owner (reglock)
  "Return the buffer that owns REGLOCK.

The buffer that owns REGLOCK may be a base buffer or an indirect one.

A REGLOCK belongs to one buffer, the buffer that is current when it is
created.  For example, if you lock a region as /pending/ in an indirect
buffer, that region lock belongs to that indirect buffer, and, control
of that lock must happen in that buffer."
  (when-let ((region (org-pending-reglock-region reglock)))
    (marker-buffer (car region))))

(defun org-pending-reglock-status (reglock)
  "Return the status of REGLOCK.
The possible status are, in chronological order:
  :scheduled =>
     :pending =>
         :success
         or :failure."
  (org-pending-reglock--status reglock))

(defun org-pending-reglock-live-p (reglock)
  "Return non-nil if REGLOCK is still live.
A REGLOCK stays live until it receives its outcome: :success or :failure."
  (funcall (org-pending-reglock--get-live-p reglock)))

(defun org-pending-reglock-useless-p (reglock)
  "Return non-nil if REGLOCK is useless.
When a REGLOCK becomes useless, org-pending will, at some point, forget
about it."
  (funcall (org-pending-reglock--useless-p reglock)))

(defun org-pending-reglock-duration (reglock)
  "Return REGLOCK duration between its scheduling and its outcome.
If the outcome is not known, use the current time."
  (let ((start (org-pending-reglock-scheduled-at reglock))
        (end (or (org-pending-reglock-outcome-at reglock)
                 (float-time))))
    (- end start)))

(defun org-pending-reglock-property (reglock prop)
  "Get the value of the property PROP for this REGLOCK.
This is a place: use `setf' to set it.
See also `org-pending-reglock-set-property'."
  (cdr (assq prop (org-pending-reglock-properties reglock))))

(defun org-pending-reglock-set-property (reglock prop val)
  "Set the value of the property PROP for this REGLOCK.
See also `org-pending-reglock-property'."
  (if-let ((b (assq prop (org-pending-reglock-properties reglock))))
      (setcdr b val)
    (push (cons prop val)
          (org-pending-reglock-properties reglock))))

(gv-define-simple-setter org-pending-reglock-property
                         org-pending-reglock-set-property)


(defun org-pending--user-cancel-default (reglock)
  "Send a cancel message to REGLOCK to close it.
Default value for `org-pending-reglock-user-cancel-function'."
  (org-pending-send-update
   reglock (list :failure (list 'org-pending-user-cancel
                                "Canceled"))))

(defun org-pending-reglock-delete-outcome-marks (reglock)
  "Delete visual hints of the outcome for this REGLOCK, if any.
Do nothing if the outcome is not known.  Do nothing if there are no
visual hints."
  (funcall (org-pending-reglock--delete-outcome-marks reglock)))

;;;; Locking a region
;;

(cl-defun org-pending (region
                       &key anchor (name "REGLOCK")
                       (on-outcome #'org-pending-on-outcome-replace))
  "Lock the REGION and return its REGLOCK.

Return the REGLOCK that you'll need to call `org-pending-send-update'.

The argument REGION is a pair (start position . end position).  Protect
the REGION from modifications until the REGLOCK receives a :success or a
:failure update.  Display progress when REGLOCK receives :progress
updates.  Do not delete the previous content of REGION.

The argument ANCHOR, when given, is a pair (start position . end
position).  Use the ANCHOR region to display the progress.  When ANCHOR
is not given, use the first line of REGION.

Use NAME to set the unique identifier `org-pending-reglock-id' of this
new REGLOCK, possibly appending a number to it to make it unique.

Assume the region REGION contains the region ANCHOR.

On receiving the outcome (a :success or :failure message, sent with
`org-pending-send-update'), remove the region protection.  Call
ON-OUTCOME with the reglock and the outcome, from the position from
where the REGLOCK was created.  If ON-OUTCOME returns a region (a
pair (start position . end position)), use it to report the
success/failure using visual hints on that region.  If ON-OUTCOME
returns nothing, don't display outcome marks.  The default ON-OUTCOME
function replaces the region on success and ignores failures; in all
cases, it returns the outcome region (see the function
`org-pending-on-outcome-replace').

You may send progress updates, and, eventually, you must send the
outcome to unlock the region (see `org-pending-send-update').

You may set/update the following fields of your reglock to customize its
behavior:
   - Emacs may have to destroy your locks; see the field
     `before-destroy-function' if you wish to do something before your
     lock is destroyed.
   - The user may ask Emacs to cancel your lock; see the field
     `user-cancel-function' to override the default cancel function.
   - The user may request a description of the lock; see the the field
     `insert-details-function' to add custom information when your
     lock is displayed to the user.

You may add/update your own properties to your reglock using the field
`properties', which is an association list.

See:
   (cl-describe-type \\='org-pending-reglock)
for more information about all reglock fields."
  (let ((to-marker (lambda (p)
                     ;; Make sure P is a marker.
                     (or (and (markerp p) p)
                         (save-excursion (goto-char p) (point-marker)))))
        reglock)
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

    (setq reglock (org-pending--make
                   :scheduled-at (float-time)
                   :-creation-point (point-marker)
                   :-on-outcome on-outcome
                   ;; useless-p returns non-nil when a reglock becomes
                   ;; useless and we may forget about it.  We'll update the
                   ;; function when we get the outcome.
                   :-useless-p (lambda () nil)
                   :-get-live-p
                   (lambda ()
                     (when-let ((anchor-ovl
                                 (org-pending-reglock--anchor-ovl reglock)))
                       (overlay-buffer anchor-ovl)))
                   :region region))
    (cl-labels
        ((remove-previous-overlays ()
           "Remove previous status overlays.
         If this region is already a pending one that is owned by an
         other buffer, raise a user error (even for failed ones).  If
         this region is already scheduled or pending, raise a user
         error."
           ;; Raise if an other (indirect) buffer owns this region.
           (dolist (pi (org-pending-locks-in (max (1- (car anchor)) (point-min))
                                                (cdr region)))
             (let ((pi-owner (org-pending-reglock-owner pi)))
               (unless (eq (current-buffer) pi-owner)
                 (error "Pending region owned by another buffer %s" pi-owner))))

           (let (ovls reglock)
             ;; Scan for previous overlays and identify the reglock.
             (dolist (ovl (overlays-in (max (1- (car anchor)) (point-min))
                                       (cdr region)))
               (when-let ((type (overlay-get ovl 'org-pending)))
                 (push ovl ovls)
                 (when (eq :status type)
                   (when reglock
                     (error "More than one status overlay"))
                   (setq reglock (overlay-get ovl 'org-pending-reglock)))))
             (when reglock
               (user-error "This region is already scheduled or pending"))
             ;; Delete previous pending decorations.
             (mapc (lambda (x) (org-pending--delete-overlay x))
                   ovls))))

      (remove-previous-overlays)

      ;; Create the overlays for the anchor and for the region.
      (setf (org-pending-reglock--region-ovl reglock)
            (org-pending--make-overlay :region region))
      (setf (org-pending-reglock--anchor-ovl reglock)
            (org-pending--make-overlay :status anchor))

      ;; Flag the result as ":scheduled".
      (org-pending--update reglock :scheduled nil)

      (overlay-put (org-pending-reglock--region-ovl reglock)
                   'org-pending-reglock reglock)
      (overlay-put (org-pending-reglock--anchor-ovl reglock)
                   'org-pending-reglock reglock)
      (org-pending--mgr-handle-new-reglock reglock name)

      (org-pending--ensure-buffer-setup)

      reglock)))


;;;; Describing a lock for the user
;;

(defvar org-pending-describe-buffer-name "*Region Lock*"
  "Buffer name used to describe reglocks.")


(defun org-pending-describe-reglock (reglock)
  "Describe REGLOCK in a buffer.

Create or erase the buffer named `org-pending-describe-buffer-name'.
Print the generic REGLOCK information in this buffer.  Then, when the
REGLOCK field insert-details-function is non-nil, move point to the end
of the description buffer, and call that function with REGLOCK."
  (let ((buffer (get-buffer-create org-pending-describe-buffer-name)))
    (with-output-to-temp-buffer buffer
      (with-current-buffer buffer
        (erase-buffer)
        (setq revert-buffer-function
              (lambda (&rest _)
                ;; Revert if not destroyed.
                (if (org-pending-reglock-region reglock)
                    (org-pending-describe-reglock reglock)
                  (user-error "This region lock has been destroyed"))))
        (cl-labels
            ((time-to-string (x) (if x (format-time-string "%T" x) "-"))
             (bool-to-string (x) (if x "yes" "no"))
             (insert-link (m)
               (let ((b (marker-buffer m)))
                 (if (buffer-live-p b)
                   (insert-button
                    (format "pos %s in buffer %s" (+ 0 m) (marker-buffer m))
                    'action (lambda (&rest _)
                              (interactive)
                              (pop-to-buffer b)
                              (goto-char m)))
                   (insert "n.a."))))
             (insert-region (r)
               (if (buffer-live-p (marker-buffer (car r)))
                   (progn
                     (insert "[")
                     (insert-link (car r))
                     (insert "..")
                     (insert-link (cdr r))
                     (insert "]"))
                 (insert "n.a.")))
             (insert-value (value)
               (cond
                ((markerp value) (insert-link value))
                ((functionp value) (funcall value))
                (t (insert (format "%s" value)))))
             (one-line (label value &optional annotate)
               (insert (propertize (format "%13s" label)
                                   'face 'outline-1))
               (insert ": ")
               (insert-value value)
               (when annotate (funcall annotate))
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

          (setq-local header-line-format
                      (format "Lock info (at %s), hit 'g' to update."
                              (format-time-string "%T")))
          ;; ... ok, back to real work.
          (one-line "Id"
                    (org-pending-reglock-id reglock))
          (one-line "Status"
                    (substring (symbol-name (org-pending-reglock-status reglock)) 1))
          (let ((alive (org-pending-reglock-live-p reglock)))
            (one-line
             "Live?"
             (bool-to-string alive)
             (lambda ()
               (insert " ")
               (if alive
                   (insert-button "Cancel"
                                  'action (lambda (b)
                                            (interactive)
                                            (org-pending-cancel reglock)
                                            (message "Cancel request sent. Hit 'g' to update")
                                            (let ((inhibit-read-only t))
                                              (goto-char (button-start b))
                                              (delete-region (button-start b) (button-end b))
                                              (insert (propertize "Cancel sent. Hit 'g' to update."
                                                                  'face 'italic)))))
                 (insert-button
                  "Forget"
                  'action (lambda (&rest _args)
                            (interactive)
                            (org-pending-reglock-delete-outcome-marks
                             reglock)
                            (quit-window)))))))

          (one-line "Region"
                    (lambda () (insert-region (org-pending-reglock-region reglock))))
          (one-line "Scheduled at"
                    (time-to-string (org-pending-reglock-scheduled-at reglock)))
          (one-line "Outcome at"
                    (time-to-string (org-pending-reglock-outcome-at reglock)))
          (one-line "Duration"
                    ;; TODO nice human format like 1m27s
                    (format "%.1fs" (org-pending-reglock-duration reglock)))
          (field "Result"
                 (if-let ((outcome (org-pending-reglock-outcome reglock)))
                     (pcase outcome
                       (`(:success ,v) v)
                       (_ "-"))
                   "-"))
          (field "Error"
                 (if-let ((outcome (org-pending-reglock-outcome reglock)))
                     (pcase outcome
                       (`(:failure ,v) v)
                       (_ "-"))
                   "-"))
          ;; Insert up to 1M of log.
          (multi-line "Details"
                      (lambda ()
                        (when-let ((insert-details (org-pending-reglock-insert-details-function reglock)))
                          (funcall insert-details reglock)))))))))

(defun org-pending--describe-reglock-at-point ()
  "Describe the lock at point.
Get the REGLOCK at point, for a locked region or an outcome mark.  Use
`org-pending-describe-reglock' to display it."
  (interactive)
  (let ((reglock (or (get-char-property (point) 'org-pending-reglock)
                   (when-let ((ovl (get-char-property (point) 'org-pending--projection-of)))
                     (overlay-get ovl 'org-pending-reglock)))))
    (if reglock
        (org-pending-describe-reglock reglock)
      (user-error "No pending content at point"))))



;;;; Updates
;;
(defvar org-pending-pre-insert-outcome-function
  #'ignore
  "Function called before inserting the outcome.

This function is called before releasing the lock.

This function is called with two arguments: the lock and the update
message (see `org-pending-send-update' for the definition of what an
update message is).")

(defvar org-pending-post-insert-outcome-function
  #'org-pending-post-insert-outcome-default
  "Function called after inserting the outcome.

This function is called with three arguments: the lock, the update
message (see `org-pending-send-update' for the definition of what an
update message is), and the outcome region (a pair BEGIN END).  This
function is called only when there is a region for the outcome (see the
option ON-OUTCOME of `org-pending').

This function must return how to remove the decoration: a function that
will be called with no arguments; the returned function may be called
even if the lock/buffer doesn't exist.")


(defun org-pending-post-insert-outcome-default (lock message outcome-region)
  "Default value for `org-pending-post-insert-outcome-function'."
  ;; We add some outcome decorations to let the user know what
  ;; happened and allow him to explore the details.
  (let* ((status (car message))
         (outcome-ovl (org-pending--make-overlay status outcome-region))
         (bitmap (pcase status
                  (:success 'large-circle)
                  (:failure 'exclamation-mark)))
         (face (pcase status
                 (:success 'org-done)
                 (:failure 'org-todo)))
         (outcome-face (pcase status
                         (:success 'org-pending-outcome-success)
                         (:failure 'org-pending-outcome-failure))))
    (overlay-put outcome-ovl
                 'before-string (propertize
                                 "x" 'display
                                 `(left-fringe ,bitmap ,face)))
    (overlay-put outcome-ovl
                 'help-echo
                 "Last lock outcome, click to popup its full description.")
    (overlay-put outcome-ovl 'face outcome-face)
    (overlay-put outcome-ovl 'org-pending-reglock lock)
    (push `(apply delete-overlay ,outcome-ovl) buffer-undo-list)
    ;; Return how to remove our decoration.
    (lambda ()
      (when-let ((buf (overlay-buffer outcome-ovl)))
        (when (buffer-live-p buf)
          (delete-overlay outcome-ovl))))))


(cl-defun org-pending--update (reglock status data)
  (cl-labels
      ((add-style (status txt)
         "Add the style matching STATUS over the text TXT."
         (propertize txt 'face (org-pending-status-face status)))
       (short-version-of (msg)
         "Compute the short version of MSG, to display on the anchor.
         Must return a string."
         (if msg
             (car (split-string (format "%s" msg) "\n" :omit-nulls))
           "")))
    (let* ((anchor-ovl (org-pending-reglock--anchor-ovl reglock))
           (region-ovl (org-pending-reglock--region-ovl reglock))
           (on-outcome (org-pending-reglock--on-outcome reglock))
           outcome-region)
      (unless (memq status '(:scheduled :pending :failure :success))
        (error "Invalid status"))
      ;; Update the title overlay to match STATUS and DATA.
      (setf (org-pending-reglock--status reglock) status)
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
        (funcall org-pending-pre-insert-outcome-function
                 reglock (list status data))

        ;; We remove all overlays and let org insert the result
        ;; as it would in the synchronous case.
        (org-pending--delete-overlay anchor-ovl)
        (setq anchor-ovl nil)
        (org-pending--delete-overlay region-ovl)
        (setq region-ovl nil))

      (when (memq status '(:success :failure))
        (setf (org-pending-reglock-outcome reglock) (list status data))
        (setf (org-pending-reglock-outcome-at reglock) (float-time))

        (when on-outcome
          (setq outcome-region (funcall on-outcome reglock (list status data)))
          (when outcome-region
            ;; We got a region. Check it's really one.
            (unless (and (consp outcome-region)
                         (or (integerp (car outcome-region))
                             (markerp (car outcome-region)))
                         (or (integerp (cdr outcome-region))
                             (markerp (cdr outcome-region))))
              (error "Not a region"))))

        (if (not outcome-region)
            (setf (org-pending-reglock--useless-p reglock)
                  (lambda () t))
          ;; Decorate the outcome and store how to remove the decoration.
          (setf (org-pending-reglock--delete-outcome-marks reglock)
                (funcall org-pending-post-insert-outcome-function
                         reglock (list status data) outcome-region))

          (setf (org-pending-reglock--useless-p reglock)
                (lambda ()
                  (if-let ((buf (org-pending-reglock-owner reglock)))
                      (not (buffer-live-p buf))
                    t))))))))

(defun org-pending-send-update (reglock upd-message)
  "Send the update UPD-MESSAGE to REGLOCK.

See `org-pending' to create a REGLOCK.

The udpate UPD-MESSAGE must be one of the following:

    - (:success R): The new content is ready; the result is R; Emacs
      unlocks the region and calls ON-OUTCOME with (:success R).

    - (:failure ERR): Something failed; the error is ERR; Emacs unlocks
      the region and calls ON-OUTCOME with (:failure ERR).

    - (:progress P): Content is still pending; current progress is P;
      Emacs may display this progress P using the lock anchor (see
      `org-pending').  The progress P may be any data; org-pending will
      format it as a string that fits on one line when displaying it.

You may send as many :progress updates as you want (including none).
Eventually, you must send one, and only one, of either a :success or a
:failure.  Until you do, the region stays locked, and protected from
modifications.

Once the REGBLOCK got its outcome, it is dead.  Ignore updates that
come once the lock is dead."
  (when (org-pending-reglock-live-p reglock)
    (let* ((pt (org-pending-reglock--creation-point reglock))
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
               (org-pending--update reglock :success r))

              (`(:progress ,p)
               ;; Still waiting for the outcome. Update our
               ;; overlays with the progress info R.
               (org-pending--update reglock :pending p))

              (`(:failure ,err)
               ;; We didn't get a result.
               (org-pending--update reglock :failure err))

              (_ (error "Invalid message")))
            (org-pending--mgr-handle-reglock-update reglock upd-message))))
      nil)))

(defun org-pending-sending-outcome-to--worker (reglock todo)
  "See `org-pending-sending-outcome-to'."
  (let (outcome)
    (unwind-protect
        (setq outcome
              (condition-case-unless-debug exc
                  (list :success (funcall todo))
                (error (list :failure exc))))
      (unless outcome
        (setq outcome (list :failure 'error)))
      (org-pending-send-update reglock outcome))))

(defmacro org-pending-sending-outcome-to (reglock &rest body)
  "Execute BODY, using the outcome to unlock REGLOCK.

Exectute BODY.  Use the value of the last form to send a :success
outcome to REGLOCK.  If an error occurs when executing BODY, use that
error to send a :failure outcome to REGLOCK."
  (declare (indent 1) (debug (form body)))
  `(org-pending-sending-outcome-to--worker ,reglock (lambda () ,@body)))

;;;; Helpers to define on-outcome handlers
;;
(defun org-pending-on-outcome-replace (reglock outcome)
  "Replace the REGLOCK region with OUTCOME.

On :success, if the REGLOCK buffer is still live, replace the region
with the value and return the region spanning the new text; if the
REGLOCK buffer isn't live, do nothing and return nil.

On :failure, do nothing and return the REGLOCK region.

This is the default :on-outcome handler for the function `org-pending'."
  (pcase outcome
    (`(:failure ,_) (org-pending-reglock-region reglock))
    (`(:success ,new-text)
     (unless (stringp new-text)
       (setq new-text (format "%s" new-text)))
     (let* ((reg  (org-pending-reglock-region reglock))
            (start (car reg))
            (end (cdr reg))
            (buf (marker-buffer start)))
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (save-excursion
             (if (> (- end start) 1)
                 ;; Insert in the middle as it's more robust to
                 ;; keep existing data (text properties, markers,
                 ;; overlays).
                 (let ((ipoint (+ 0 (goto-char (1+ start)))))
                   (setq end (progn (goto-char end) (point-marker)))
                   (goto-char ipoint)
                   (insert new-text)
                   (delete-region (point) end)
                   (delete-region start ipoint)
                   (cons start (point-marker)))
               ;; Can't insert in the middle.
               (let ((old-end (point-marker))
                     new-end)
                 (set-marker-insertion-type old-end nil)
                 (insert new-text)
                 (setq new-end (point-marker))
                 (delete-region start old-end)
                 (cons start new-end)
                 )))))))))


;;; Checking for reglocks

(defun org-pending-locks-in (start end &optional owned)
  "Return the list of locks in START..END.

Return the list of REGLOCK(s) that are alive between START and END, in
the current buffer.

When OWNED is non-nil, ignore locks that are not owned by this buffer.

See also `org-pending-locks-in-buffer-p'."
  (let ((reglocks)
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
          (push (overlay-get ovl 'org-pending-reglock) reglocks))
        (setq start (1+ (min end (overlay-end ovl))))))
    reglocks))

(defun org-pending-locks-in-buffer-p (&optional buffer owned-only)
  "Return non-nil if BUFFER contains some locks that are alive.
BUFFER is a buffer or a buffer name.  When BUFFER is nil, use the
current buffer.

When OWNED-ONLY is non-nil, ignore locks that are not owned by this
buffer.

See also `org-pending-locks-in'."
  (setq buffer (or (and buffer (get-buffer buffer))
                   (current-buffer)))
  (with-current-buffer buffer
    (without-restriction
      (org-pending-locks-in (point-min) (point-max)
                            owned-only))))


(defun org-pending-ensure-no-locks (begin end &optional error-info)
  "Raise `org-pending-error' if BEGIN..END contains locks that are alive.

Append ERROR-INFO to the error data when signaling an error."
  (when (org-pending-locks-in begin end)
    (signal 'org-pending-error (cons begin (cons end error-info)))))

(defun org-pending-no-locks-in-emacs-p ()
  "Return non-nil if any buffer contains some pending contents."
  (catch 'found-one
    (dolist (p (org-pending-list))
      (when (org-pending--status-still-pending-p
             (org-pending-reglock-status p))
        (throw 'found-one t)))
    nil))



;;; Managing locks
;; The manager contains locks (dead or alive).  This allows to check
;; the history of locks.
;;

;;;; Internals
;;

(cl-defstruct (org-pending--manager
               (:constructor org-pending--create-manager)
	       (:copier nil))
  ; An name (string) uniquely identifies one REGLOCK.
  used-names ; obarray of in-use names.
  reglocks ; The list of REGLOCKs, past & present.
  )



(defvar org-pending--manager nil
  "The global manager for locks.")

(defun org-pending--manager ()
  "Get/create the global manager for locks."
  (unless org-pending--manager
    (setq org-pending--manager (org-pending--create-manager
                                :used-names (obarray-make)))
    (add-hook 'kill-emacs-query-functions #'org-pending--kill-emacs-query))
  org-pending--manager)

(defun org-pending--mgr-handle-new-reglock (reglock name)
  "Handle this new lock REGLOCK.
Derive a new identifier using NAME.  Update REGLOCK as needed.  Return nothing."
  (let* ((mgr (org-pending--manager)))
    (push reglock (org-pending--manager-reglocks mgr))

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

    (setf (org-pending-reglock-id reglock) name)
    nil))

(defun org-pending--mgr-handle-reglock-update (reglock update)
  "Handle the update UPDATE for this REGLOCK.
Return nothing."
  (message "org-pending: reglock update for id=%s: %s"
           (org-pending-reglock-id reglock) update))

(defun org-pending--mgr-garbage-collect ()
  "Forget useless data about locks."
  (let ((mgr (org-pending--manager)))
    (setf (org-pending--manager-reglocks mgr)
          (seq-filter (lambda (l) (not (org-pending-reglock-useless-p l)))
                      (org-pending--manager-reglocks mgr)))))

;;;; API
;;

(defun org-pending-list ()
  "Return the list of REGLOCKs.
This is a global list for this Emacs instance, in any buffer.  It
includes past and present REGLOCKs."
  (org-pending--mgr-garbage-collect)
  (org-pending--manager-reglocks (org-pending--manager)))


(defun org-pending-cancel (reglock)
  "Try to cancel REGLOCK.

Call `org-pending-reglock-user-cancel-function' with REGLOCK.

Note that the cancellation is asynchronous, the REGLOCK should receive
its outcome when the cancellation completes.

Return nothing immediately."
  (when (org-pending-reglock-live-p reglock)
    (funcall (org-pending-reglock-user-cancel-function reglock) reglock))
  nil)

(defun org-pending-unlock-NOW! (reglock)
  "Unlock this REGLOCK region immediately, whatever the consequences.

This function ignores why the region has been locked; you may loose
data, you may leak ressources, etc.

Use `org-pending-cancel' instead if possible.

Return nothing immediately."
  (when (org-pending-reglock-live-p reglock)
    (funcall (org-pending--user-cancel-default reglock) reglock))
  nil)


;;; Managing outcomes
;;
(defun org-pending-delete-outcome-marks (sstart slimit)
  "Remove outcome marks between SSTART and SLIMIT in the current buffer.
Remove them in any buffer (base or indirect, owned or not)."
  (while (and sstart (< sstart slimit))
    (when-let ((ovl (get-text-property sstart 'org-pending--outcome-overlay)))
      (when (overlay-buffer ovl)
        (delete-overlay ovl)))
    (setq sstart (next-single-property-change
                  sstart 'org-pending--outcome-overlay nil slimit)))
  (remove-text-properties sstart slimit
                          (list 'org-pending--outcome-overlay :not-used)))

;;; Plugging into Emacs
;;
(defun org-pending--forced-destroy (reglock)
  "Destroy this REGLOCK.
Do not ask for confirmation or interact in any way, just destroy it.

If this REGLOCK is alive and the REGLOCK field `before-destroy-function' is
non-nil, call it with REGLOCK.  If the REGLOCK is still live, make it
fail with org-pending-user-cancel error.

Set the REGLOCK region to nil to indicate that it has been destroyed.

Return nothing."
  (when (org-pending-reglock-live-p reglock)
    (when-let ((before-destroy (org-pending-reglock-before-destroy-function reglock)))
      (funcall before-destroy reglock))
    ;; Unlock the region, marking the REGLOCK as failed due to
    ;; cancellation.
    (when (org-pending-reglock-live-p reglock)
      (org-pending-send-update
       reglock (list :failure (list 'org-pending-user-cancel
                                    "Gone")))))
  (setf (org-pending-reglock-region reglock) nil)
  nil)


(defun org-pending--kill-buffer-query ()
  "For `kill-buffer-query-functions'.
If the current buffer contains locks, offer to abort killing the buffer."
  ;; TODO: Offer to jump to the list of this buffer locks
  ;;
  ;; For an indirect buffer, we can kill it even if there are locks,
  ;; as long as the buffer doesn't own any of them.  For a base
  ;; buffer, we can't kill it if they are any lock (as this would kill
  ;; all indirect buffers).
  ;;
  ;; WARNING: Emacs (<=30.1) may segfault if the indirect buffer
  ;;   rejects the kill and its base buffer didn't (see bug#69529).
  (let* ((b (current-buffer))
         (owned-only (buffer-base-buffer b)))
    (if (not (org-pending-locks-in-buffer-p b owned-only))
        :ok-to-kill
      (when (y-or-n-p (format (concat "There are pending locks in buffer '%s'"
                                      " (or its indirect buffers), kill anyway?")
			      (buffer-name)))
        ;; Force destroyed: destroy all the locks of this buffer.
        (without-restriction
          (dolist (pi (org-pending-locks-in (point-min) (point-max)
                                            :owned-only))
            (org-pending--forced-destroy pi)))
        :forced-destroyed))))

(defun org-pending--kill-emacs-query ()
  "For `kill-emacs-query-functions'.
If there are any lock, offer to abort killing Emacs."
  ;; TODO: Offer to jump to the list of the locks.
  (if (not (org-pending-no-locks-in-emacs-p))
      :ok-to-kill
    (when (or (not org-pending-confirm-ignore-reglocks-on-exit)
              (yes-or-no-p (format "There are pending locks, kill anyway?")))
      ;; Forced kill: cancel all pending regions
      (dolist (pi (org-pending-list))
        (org-pending--forced-destroy pi))
      :forced-kill)))

(defun org-pending--after-indirect-clone ()
  "For `clone-indirect-buffer-hook'.
Fix our data, after creating an indirect clone."
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
    (dolist (reglock (org-pending-locks-in (point-min) (point-max)))
      (let ((region (org-pending-reglock-region reglock)))
        (when region
          (let ((inhibit-modification-hooks t)
                (inhibit-read-only t))
            (remove-text-properties (car region) (cdr region)
                                    (list 'face :not-used))))))))


(defun org-pending--ensure-buffer-setup ()
  "Ensure the buffer is configured to handle region locks.
Safe to call many times in a given buffer."
  (add-hook 'kill-buffer-query-functions
            #'org-pending--kill-buffer-query nil :local)
  (add-hook 'clone-indirect-buffer-hook
            #'org-pending--after-indirect-clone :local))


;;; Basic use of locks
;;

;;;; Lock while executing some elisp
;;

(defmacro org-pending-updating-region (start end props &rest body)
  "Lock the region START..END while executing BODY.

Lock the region START..END (applying `org-pending' to the region and
PROPS).  Then, execute BODY while the region is locked, and, set the
outcome (see `org-pending-sending-outcome-to').  Finally, unlock the
region.

Use the ON-OUTCOME property to update the region if/when you need to."
  (declare (indent 3) (debug (form form form body)))
  (let ((reglock-sb (make-symbol "reglock")))
    `(let ((,reglock-sb (apply #'org-pending (cons ,start ,end) ,props)))
       (org-pending-sending-outcome-to ,reglock-sb ,@body))))


;;;; Prompt the user to edit a region

(cl-defun org-pending-user-edit (prompt start end &key edit-name)
  "Ask the user to edit the region BEGIN .. END.

Like `string-edit' using a lock.

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
  (let* ((to-update (buffer-substring start end))
         (reglock (org-pending
                   (cons start end)
                   :on-outcome #'org-pending-on-outcome-replace))
         edit-buffer
         closing)
    (string-edit prompt to-update
                 (lambda (new-text)
                   (org-pending-send-update
                    reglock
                    (list :success new-text)))
                 :abort-callback
                 (lambda ()
                   (org-pending-send-update
                    reglock
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
                         (org-pending-reglock-live-p reglock))
                (org-pending-send-update
                 reglock
                 (list :failure (list 'user-error
                                      "Edition buffer killed"))))
              :ok-to-kill)
            kill-buffer-query-functions))

    (setf (org-pending-reglock-before-destroy-function reglock)
          (lambda (_rl)
            (setq closing t)
            (when (buffer-live-p edit-buffer)
              (with-current-buffer edit-buffer
                (kill-buffer edit-buffer)))))

    (setf (org-pending-reglock-insert-details-function reglock)
          (lambda (_rl)
            (let ((insert-link
                   (lambda (b)
                     (insert-button (format "%s" (buffer-name b))
                                    'action
                                    (lambda (&rest _)
                                       (interactive)
                                       (when (buffer-live-p b)
                                         (pop-to-buffer b)))))))
              (insert "Edit buffer: ")
              (if edit-buffer
                  (funcall insert-link edit-buffer)
                (insert "-")))))
    edit-buffer))


;;; Giving up on asynchronicity
;;

(defvar org-pending-without-async-flag nil
  "Non-nil means to run without asynchronous calls.

Used to disable asynchronous calls when Emacs doesn't support it (yet),
or, when it's impossible by design.

For example, when an Org block is executed asynchronously, if it depends
on the execution of other blocks, we have to execute those synchronously
for now.")

(defmacro org-pending-without-async (&rest body)
  "Execute BODY, forcing synchronous calls.

It sets `org-pending-without-async-flag' to a non-nil value.

Return the value of the last form."
  (declare (indent 0) (debug t))
  `(let ((org-pending-without-async-flag t))
     ,@body))



;;; Dev & debug
;;

(defun org-pending--reset-buffer ()
  "Reset the current buffer, throwing away of pending decorations.
Dev only.  Use at your own risk."
  ;; TODO: Remove REGLOCKs from the manager if any.
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
