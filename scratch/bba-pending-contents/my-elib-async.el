;;; my-elib-async.el --- Helper to write asynchronous functions -*- lexical-binding: t -*-

;; Copyright (C) 2024 Bruno BARBIER

;; Author: Bruno BARBIER
;; Version: 0.0.0
;; Maintainer: Bruno BARBIER
;; Keywords:
;; Status: WORK IN PROGRESS.  DO NOT USE.
;; URL:
;; Compatibility: GNU Emacs 30.0.50
;;
;; This file is NOT (yet) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA



;;; Description
;;
;; Some code to demo the proposed org-pending features.
;;


;;; Code:
;;
(require 'cl-lib)
(require 'org-id)

;;;; Buffers
;;

(defun my-elib-async-kill-buffer-later (buf)
  "Kill the buffer BUF later."
  (run-with-idle-timer
   0.1 nil
   (lambda ()
     (when (buffer-live-p buf)
       (with-current-buffer buf
         (let ((kill-buffer-query-functions nil)
               (kill-buffer-hook nil))
           (kill-buffer buf)))))))





;;;; Comint: a FIFO queue of tasks with callbacks
;; The FIFO queue executes tasks in a FIFO order in a comint buffer.
;; For each task, it identifies the text output for that task.  The
;; queue does NOT remove prompts, or other useless texts; this is the
;; responsibility of the caller.  Currently, the queue assumes it has
;; the full control of the session: no user interaction, no other
;; direct modifications, but it's designed to share the sessions with
;; the user.  The entry point is: `my-elib-async-comint-queue--push'.
;; See `my-org-babel-schedule' in the file my-async-tests.el for an
;; example; and 'my-org-babel-how-to-execute-shell',
;; 'my-org-babel-how-to-execute-python' and
;; 'my-org-babel-how-to-execute-ruby'.

(defvar-local my-elib-async-comint-queue--todo :NOT-SET
  "A FIFO queue of pending executions.")


(defvar-local my-elib-async-comint-queue--unused-output ""
  "Process output that has not been used yet.")

(defvar-local my-elib-async-comint-queue--incoming-text ""
  "Newly incoming text, added by the process filter, not yet handled.")

(defvar-local my-elib-async-comint-queue--current-task nil
  "The task that is currently running.")

(defvar-local my-elib-async-comint-queue--process-filter-running nil
  "non-nil when filter is running.")

(defvar-local my-elib-async-comint-queue--incoming-timer nil
  "A timer, when handling incoming text is scheduled or running.")


(defvar-local my-elib-async-comint-queue--handle-incoming-running
    nil
  "True when the incoming text handler is running.")

(defun my-elib-async-comint-queue--handle-incoming ()
  (when my-elib-async-comint-queue--handle-incoming-running
    (error "Bad call to handle-incoming: kill buffer %s!" (current-buffer)))
  (setq my-elib-async-comint-queue--handle-incoming-running t)

  ;; Take the incoming text.
  (setq my-elib-async-comint-queue--unused-output
        (concat my-elib-async-comint-queue--unused-output
                my-elib-async-comint-queue--incoming-text))
  (setq my-elib-async-comint-queue--incoming-text "")

  ;; Process the unused text with the queued tasks
  (unless my-elib-async-comint-queue--current-task
    (when my-elib-async-comint-queue--todo
      (setq my-elib-async-comint-queue--current-task (pop my-elib-async-comint-queue--todo))))
  (when-let ((task my-elib-async-comint-queue--current-task))
    (let ((unused my-elib-async-comint-queue--unused-output)
          (session-buffer (current-buffer))
          task-start)
      (setq my-elib-async-comint-queue--unused-output
            (with-temp-buffer
              (insert unused)
              (goto-char (point-min))
              (while (and task
                          (setq task-start (point))
                          (search-forward (car task) nil t))
                (when (cdr task)
                  (let ((txt (buffer-substring-no-properties task-start
                                                             (- (point) (length (car task)))))
                        (todo (cdr task))
                        (cbuf session-buffer)
                        (cpt  (point)))
                    ;; We run the task outside this loop, to not hang
                    ;; the processing of incoming data.
                    (run-with-timer
                     0.01 nil
                     (lambda ()
                       (with-current-buffer cbuf
                         (save-excursion
                           (goto-char cpt) (funcall todo txt)))))))
                (setq task (and (buffer-live-p session-buffer)
                                (with-current-buffer session-buffer
                                  (pop my-elib-async-comint-queue--todo))))
                (forward-char)) ;; Skip our EOL
              (buffer-substring (point) (point-max))))
      (setq my-elib-async-comint-queue--current-task task)))

  ;; Signal that we are done. If we already have some new incoming text,
  ;; reschedule to run.
  (setq my-elib-async-comint-queue--incoming-timer
        (if (string-empty-p my-elib-async-comint-queue--incoming-text)
            nil
          (my-elib-async-comint-queue--wake-up-handle-incoming)))

  ;; We reset it only on success. If it failed for some reason, the
  ;; comint buffer is in an unknown state: you'll need to kill that
  ;; buffer.
  (setq my-elib-async-comint-queue--handle-incoming-running nil))


(defun my-elib-async-comint-queue--wake-up-handle-incoming ()
  "Wake up the handling of incoming chunks of text.
Assume we are called from the comint buffer."
  (setq my-elib-async-comint-queue--incoming-timer
        (run-with-timer
         0.01 nil
         (let ((comint-buffer (current-buffer)))
           (lambda ()
             (with-local-quit
               (with-current-buffer comint-buffer
                 (my-elib-async-comint-queue--handle-incoming))))))))


(defun my-elib-async-comint-queue--process-filter (chunk)
  "Accept the arbitrary CHUNK of text."
  (setq my-elib-async-comint-queue--incoming-text
        (concat my-elib-async-comint-queue--incoming-text
                chunk))
  :; We delegate the real work outside the process filter, as it is
   ; not reliable to do anything here.
  (unless my-elib-async-comint-queue--incoming-timer
    (my-elib-async-comint-queue--wake-up-handle-incoming)))



(define-error 'my-elib-async-comint-queue-task-error
              "Task failure.")

(cl-defun my-elib-async-comint-queue--push (exec &key sentinel)
  "Push the execution of EXEC into the FIFO queue.
When the task completed, call SENTINEL with its outcome.  Return
a function that allows to control the tasks."
  (let* ((tid (org-id-uuid))
         (start-tag (format "MY-ELIB-ASYNC_START_%s" tid))
         (end-tag (format "MY-ELIB-ASYNC_END___%s" tid))
         (outcome-sb (make-symbol "outcome"))
         result-log
         (on-start
          (lambda (_)
            ;; TODO: Use (point) in session to link back to it.
            (when sentinel
              (funcall sentinel '(:progress "running")))))
         (on-result
          (lambda (result)
            ;; Get the result, and report success using SENTINEL.
            ;; If something fails, report failure using SENTINEL.
            (setq result-log result)
            (unwind-protect
                (let ((outcome
                       (condition-case-unless-debug exc
                           (list :success (funcall exec :post-process result))
                         (error (list :failure exc)))))
                  (when sentinel (save-excursion (funcall sentinel outcome)))
                  (set outcome-sb outcome))
              (funcall exec :finally)))))

    ;; TODO: Add detect-properties => alist of properties that can be
    ;;       used: PS1 and PS2
    (let ((comint-buffer (funcall exec :get-comint-buffer)))
      (with-current-buffer comint-buffer
        (setq my-elib-async-comint-queue--todo
              (nconc my-elib-async-comint-queue--todo
                     (list (cons start-tag on-start)
                           (cons end-tag on-result))))
        (funcall exec :send-instrs-to-session
                 (funcall exec :instrs-to-enter))
        (funcall exec :send-instrs-to-session
                 (funcall exec :instr-to-emit-tag start-tag))
        (funcall exec :send-instrs-to-session
                 (funcall exec :get-code))
        (funcall exec :send-instrs-to-session
                 (funcall exec :instr-to-emit-tag end-tag))
        (funcall exec :send-instrs-to-session
                 (funcall exec :instrs-to-exit))

        (let ((outcome-ready-p
               (lambda ()
                 (when (boundp outcome-sb) (symbol-value outcome-sb)))))
          (lambda (&rest query)
            (pcase query
              (`(:get . ,_)
               (org-pending-wait-outcome outcome-ready-p))

              (`(:cancel ,_penreg)
               (org-pending-ti-not-implemented))

              (`(:insert-details ,_penreg ,start ,end)
               (let ((to-insert (with-temp-buffer
                                  (insert result-log)
                                  (buffer-substring (or (and start (max (point-min) start))
                                                        (point-min))
                                                    (or (and end (min (point-max) end))
                                                        (point-max))))))
                 (insert to-insert)))
              (_ (error "Unknown query")))))))))


(defun my-elib-async-comint-queue-init-if-needed (buffer)
  "Initialize the FIFO queue in BUFFER if needed."
  (with-current-buffer buffer
    (unless (local-variable-p 'my-elib-async-comint-queue--todo)
      (setq-local my-elib-async-comint-queue--todo nil)
      (add-hook 'comint-output-filter-functions
                #'my-elib-async-comint-queue--process-filter nil :local))))



;;; Provide
(provide 'my-elib-async)
;;; my-elib-async.el ends here
