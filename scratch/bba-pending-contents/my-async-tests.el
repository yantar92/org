;;; my-async-tests.el --- Scratch/temporary file: some tests about async -*- lexical-binding: t -*-

;; Copyright (C) 2024 Bruno BARBIER

;; Author: Bruno BARBIER
;; Status: Temporary tests.
;; Compatibility: GNU Emacs 30.0.50
;;
;; This file is NOT part of GNU Emacs.

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

;; For explanation and examples, see the file:
;;
;;      my-async-tests.org.
;;
;;

;;; Requirements
;;
(require 'cl-lib)
(require 'org)

(require 'ob-shell)
(require 'ob-python)
(require 'ob-ruby)

(eval-and-compile
  (let ((load-path (cons "." load-path)))
    (load-library "my-elib-async")))

;;; Helpers
;;

(defun my-org-reassemble-result (result params)
  "Reassemble a babel result."
  (org-babel-reassemble-table
   result
   (org-babel-pick-name (cdr (assq :colname-names params))
                        (cdr (assq :colnames params)))
   (org-babel-pick-name (cdr (assq :rowname-names params))
                        (cdr (assq :rownames params)))))


;;; Demo using a custom execution engine
;;
;; The generic engine is defined in:
;;     my-elib-async.el (`my-elib-async-comint-queue--push')
;;
;; Using the option :execute-with my-org-babel, org will use this
;; custom engine to execute source blocks.  This engine provides
;; asynchronous and synchronous executions, with or without sessions.

;;;; Connection to org babel
;;
;; We plug the engine using the following convention:
;;     my-org-babel-how-to-execute-LANG
;; is the function that defines how to execute a source block in
;; language LANG.
;;
;; We define `my-org-babel-schedule' (async) and
;; `my-org-babel-execute' (sync); org babel will call these functions
;; when we set ':execute-with' to "my-org-babel".
;;
;;;;; Asynchronous case: my-org-babel-schedule
;;
(defun my-org-babel-schedule (lang body params sentinel)
  "Schedule the execution of BODY according to PARAMS.
Called by `org-babel-execute-src-block', async case.  Return a task
controller."
  (when (equal "bash" lang) (setq lang "shell"))
  (let ((exec-sb (intern-soft (concat "my-org-babel-how-to-execute-"
                                   lang)))
        exec)
    (unless (and exec-sb (symbol-function exec-sb))
      (error "Not implemented my-org-babel-schedule for lang '%s'" lang))
    (setq exec (funcall (symbol-function exec-sb) body params sentinel))
    (my-elib-async-comint-queue--push exec :sentinel sentinel)))


;;;;; Synchronous case: my-org-babel-execute
;;
(defun my-org-babel-execute (lang body params)
  "Execute Python BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'."
  (org-babel-async-schedule-and-wait #'my-org-babel-schedule lang body params))


;;;; How to execute Shell
;;
(defun my-org-babel-how-to-execute-shell (body params _sentinel)
  "Return how to execute BODY using a POSIX shell.
Return how to execute, as expected by
`my-elib-async-comint-queue--execution'."
  ;; Code mostly extracted from ob-shell, following
  ;; `org-babel-execute:shell' and `org-babel-sh-evaluate'.
  ;; Results are expected to differ from ob-shell as we follow the
  ;; same process for all execution paths: asynchronous or not, with
  ;; session or without.
  (let* ((session (org-babel-sh-initiate-session
		   (cdr (assq :session params))))
	 (stdin (let ((stdin (cdr (assq :stdin params))))
                  (when stdin (org-babel-sh-var-to-string
                               (org-babel-ref-resolve stdin)))))
	 (result-params (cdr (assq :result-params params)))
	 (value-is-exit-status
	  (or (and
	       (equal '("replace") result-params)
	       (not org-babel-shell-results-defaults-to-output))
	      (member "value" result-params)))
	 (cmdline (cdr (assq :cmdline params)))
         (shebang (cdr (assq :shebang params)))
         (full-body (concat
		     (org-babel-expand-body:generic
		      body params (org-babel-variable-assignments:shell params))
		     (when value-is-exit-status "\necho $?")))
         (post-process
          (lambda (r)
            (my-org-reassemble-result
             (org-babel-result-cond result-params
               r
               (let ((tmp-file (org-babel-temp-file "sh-")))
                 (with-temp-file tmp-file (insert r))
                 (org-babel-import-elisp-from-file tmp-file)))
             params)))
         comint-buffer
         finally
         to-run)

    (setq comint-buffer
          (if session session
            ;; No session. We create a temporary one and use 'finally' to
            ;; destroy it once we are done.
            ;;
            ;; FIXME: This session code should be refactored and moved into
            ;;        ob-core.
            (let ((s-buf (org-babel-sh-initiate-session
                          (generate-new-buffer-name (format "*ob-shell-no-session*")))))
              (setq finally (lambda ()
                                ;; We cannot delete it immediately as we are called from it.
                              (my-elib-async-kill-buffer-later s-buf)))
              s-buf)))

    (my-elib-async-comint-queue-init-if-needed comint-buffer)

    (setq to-run
          (cond
           ((or stdin cmdline)	       ; external shell script w/STDIN
            (let ((script-file (org-babel-temp-file "sh-script-"))
	          (stdin-file (org-babel-temp-file "sh-stdin-"))
	          (padline (not (string= "no" (cdr (assq :padline params))))))
	      (with-temp-file script-file
	        (when shebang (insert shebang "\n"))
	        (when padline (insert "\n"))
	        (insert full-body))
	      (set-file-modes script-file #o755)
	      (with-temp-file stdin-file (insert (or stdin "")))
	      (with-temp-buffer
                (with-connection-local-variables
                 (concat
                  (mapconcat #'shell-quote-argument
                             (cons (if shebang (file-local-name script-file)
                                     shell-file-name)
                                   (if shebang (when cmdline (list cmdline))
                                     (list shell-command-switch
                                           (concat (file-local-name script-file)  " " cmdline))))
                             " ")
                  "<" (shell-quote-argument stdin-file))))))
           (session ; session evaluation
            full-body)
           ;; External shell script, with or without a predefined
           ;; shebang.
           ((org-string-nw-p shebang)
            (let ((script-file (org-babel-temp-file "sh-script-"))
	          (padline (not (equal "no" (cdr (assq :padline params))))))
	      (with-temp-file script-file
	        (insert shebang "\n")
	        (when padline (insert "\n"))
	        (insert full-body))
	      (set-file-modes script-file #o755)
              (if (file-remote-p script-file)
                  ;; Run remote script using its local path as COMMAND.
                  ;; The remote execution is ensured by setting
                  ;; correct `default-directory'.
                  (let ((default-directory (file-name-directory script-file)))
                    (file-local-name script-file)
	            script-file ""))))
           (t
            (let ((script-file (org-babel-temp-file "sh-script-")))
	      (with-temp-file script-file
	        (insert full-body))
	      (set-file-modes script-file #o755)
              (mapconcat #'shell-quote-argument
                         (list shell-file-name
                               shell-command-switch
                               (if (file-remote-p script-file)
                                   (file-local-name script-file)
                                 script-file))
                         " ")))))
    ;; TODO: How to handle `value-is-exit-status'?
    (lambda (&rest q)
      (pcase q
        (`(:instrs-to-enter)
         ;; FIXME: This is wrong.
         "export PS1=''; export PS2='';")
        (`(:instrs-to-exit))
        (`(:finally) (when finally (funcall finally)))
        (`(:instr-to-emit-tag ,tag) (format "printf '%s\\n'" tag))
        (`(:post-process ,r) (when post-process (funcall post-process r)))
        (`(:send-instrs-to-session ,code)
         (with-current-buffer comint-buffer
           (when code
             (goto-char (point-max))
             (insert code) (insert "\n")
             (comint-send-input nil t))))
        (`(:get-code) to-run)
        (`(:get-comint-buffer) comint-buffer)
        (_ (error "Unknown query"))))))




;;;; How to execute Python
;;

(defun my-org-babel-how-to-execute-python (body params _sentinel)
  "Return how to execute BODY using python.
Return how to execute, as expected by
`my-elib-async-comint-queue--execution'."
  ;; Code mostly extracted from ob-python, following
  ;; `org-babel-python-evaluate-session'.
  ;; Results are expected to differ from ob-python as we follow the
  ;; same process for all execution paths: asynchronous or not, with
  ;; session or without.
  (let* ((org-babel-python-command
          (or (cdr (assq :python params))
              org-babel-python-command))
         (session-key (org-babel-python-initiate-session
                       (cdr (assq :session params))))
         (graphics-file (and (member "graphics" (assq :result-params params))
                             (org-babel-graphical-output-file params)))
         (result-params (cdr (assq :result-params params)))
         (result-type (cdr (assq :result-type params)))
         (results-file (when (eq 'value result-type)
                         (or graphics-file
                             (org-babel-temp-file "python-"))))
         (return-val (when (eq result-type 'value)
                       (cdr (assq :return params))))
         (full-body
          (concat
           (org-babel-expand-body:generic
            body params
            (org-babel-variable-assignments:python params))
           (when return-val
             (format "\n%s" return-val))))
         (post-process
          (lambda (r)
            (setq r (string-trim r))
            (when (string-prefix-p "Traceback (most recent call last):" r)
              (signal 'user-error (list r)))
            (when (eq 'value result-type)
              (setq r (org-babel-eval-read-file results-file)))
            (my-org-reassemble-result
             (org-babel-result-cond result-params
               r
               (org-babel-python-table-or-string r))
             params)))
         (tmp-src-file (org-babel-temp-file "python-"))
         (session-body
          ;; The real code we evaluate in the session.
          (pcase result-type
            (`output
             (format (string-join
                      (list "with open('%s') as f:\n"
                            "    exec(compile(f.read(), f.name, 'exec'))\n"))
                     (org-babel-process-file-name
                      tmp-src-file 'noquote)))
            (`value
             ;; FIXME: In this case, any output is an error.
             (org-babel-python-format-session-value
              tmp-src-file results-file result-params))))
         comint-buffer
         finally)


    (unless session-key
      ;; No session. We create a temporary one and use 'finally' to
      ;; destroy it once we are done.
      ;;
      ;; FIXME: This session code should be refactored and moved into
      ;;        ob-core.
      (setq session-key (org-babel-python-initiate-session
                         ;; We can't use a simple `generate-new-buffer'
                         ;; due to the earmuffs game.
                         (org-babel-python-without-earmuffs
                          (format "*ob-python-no-session-%s*" (org-id-uuid)))))
      (setq finally (lambda ()
                      (when-let ((s-buf
                                  (get-buffer (org-babel-python-with-earmuffs session-key))))
                        ;; We cannot delete it immediately as we are called from it.
                        (my-elib-async-kill-buffer-later s-buf)))))

    (setq comint-buffer
          (get-buffer (org-babel-python-with-earmuffs session-key)))
    (my-elib-async-comint-queue-init-if-needed comint-buffer)
    (with-temp-file tmp-src-file
      (insert (if (and graphics-file (eq result-type 'output))
                  (format org-babel-python--output-graphics-wrapper
                          full-body graphics-file)
                full-body)))

    (lambda (&rest q)
      (pcase q
        (`(:instrs-to-enter)
         ;; FIXME: This is wrong.
         "import sys; sys.ps1=''; sys.ps2=''")
        (`(:instrs-to-exit))
        (`(:finally) (when finally (funcall finally)))
        (`(:instr-to-emit-tag ,tag) (format "print ('%s')" tag))
        (`(:post-process ,r) (when post-process (funcall post-process r)))
        (`(:send-instrs-to-session ,code)
         ;; See org-babel-python-send-string
         (with-current-buffer comint-buffer
           (let ((python-shell-buffer-name
                  (org-babel-python-without-earmuffs session-key)))
             (python-shell-send-string (concat code "\n")))))
        (`(:get-code) session-body)
        (`(:get-comint-buffer) comint-buffer)
        (_ (error "Unknown query"))))))



;;;; How to execute Ruby
;;

(defun my-org-babel-how-to-execute-ruby (body params _sentinel)
  "Return how to execute BODY using ruby.
Return how to execute, as expected by
`my-elib-async-comint-queue--execution'."
  ;; Code mostly extracted from ob-ruby, following
  ;; `org-babel-execute:ruby'.
  ;; Results are expected to differ from ob-ruby as we follow the
  ;; same process for all execution paths: asynchronous or not, with
  ;; session or without.
  ;;
  (let* ((session (org-babel-ruby-initiate-session
		   (cdr (assq :session params)) params))
         (result-params (cdr (assq :result-params params)))
         (result-type (cdr (assq :result-type params)))
	 (org-babel-ruby-command
	  (or (cdr (assq :ruby params))
	      org-babel-ruby-command))
         (full-body (org-babel-expand-body:generic
		     body params (org-babel-variable-assignments:ruby params)))
         (tmp-file (when (eq 'value result-type)
                     (org-babel-temp-file "ruby-")))
         (ppp (or (member "code" result-params)
		  (member "pp" result-params)))
         (post-process
          (lambda (r)
            (setq r (string-trim r))
            ;; FIXME: Detect error
            (when (string-prefix-p "Traceback (most recent call last):" r)
              (signal 'user-error (list r)))
            (when (eq 'value result-type)
              (setq r (org-babel-eval-read-file tmp-file)))
            (my-org-reassemble-result
             (org-babel-result-cond result-params
               r
               (org-babel-ruby-table-or-string r))
             params)))
         (session-body
          (if (not (eq 'value result-type))
              full-body
            (concat (if ppp "require 'pp';\n")
                    full-body
                    (unless (string-suffix-p "\n" full-body) "\n")
                    (if (not ppp)
                        (concat (format org-babel-ruby-f-write
			                (org-babel-process-file-name tmp-file 'noquote))
                                "\n")
                      (mapconcat (lambda (l) (concat l "\n"))
                                 (list "results=_" "require 'pp'" "orig_out = $stdout"
		                       (format org-babel-ruby-pp-f-write
			                       (org-babel-process-file-name tmp-file 'noquote))))))))
         comint-buffer
         finally)
    (when (member "xmp" result-params)
      (error "Not supported"))

    (unless session
      (setq session (org-babel-ruby-initiate-session
                     (generate-new-buffer-name (format "*ob-ruby-no-session*"))))
      (setq finally (lambda () (my-elib-async-kill-buffer-later session))))

    (setq comint-buffer session)

    (my-elib-async-comint-queue-init-if-needed comint-buffer)

    (lambda (&rest q)
      (pcase q
        (`(:instrs-to-enter)
         ;; See 'org-babel-ruby-define-prompt'
         (concat
          "IRB.conf[:PROMPT][:CUSTOM] = { :PROMPT_I => \"\" };"
          "_org_prompt_mode=conf.prompt_mode;conf.prompt_mode=:CUSTOM;"
          "conf.echo=false;"
          (if ppp "require 'pp';" "")))
        (`(:instrs-to-exit))
        (`(:finally) (when finally (funcall finally)))
        (`(:instr-to-emit-tag ,tag) (format "puts ('%s')" tag))
        (`(:post-process ,r) (when post-process (funcall post-process r)))
        (`(:send-instrs-to-session ,code)
         ;; See org-babel-python-send-string
         (with-current-buffer comint-buffer
           (setq code (org-babel-chomp code))
           (unless (string-suffix-p "\n" code)
             (setq code (concat code "\n")))
           (insert code)
           (comint-send-input nil t)))
        (`(:get-code) session-body)
        (`(:get-comint-buffer) comint-buffer)
        (_ (error "Unknown query"))))))


;;; Demo executing elisp using threads
;;

(defun my-use-threads-schedule (lang body params sentinel)
  "Schedule emacs-lisp code BODY according to PARAMS.
Execute the code in a separate thread."
  ;; Code adapted from: `org-babel-execute:emacs-lisp'.
  (unless (fboundp 'make-thread)
    (error "Requires emacs with elisp threads"))
  (unless (equal "elisp" lang)
    (error "Only for elisp source block"))
  (let* ((lexical (cdr (assq :lexical params)))
	 (result-params (cdr (assq :result-params params)))
	 (body (format (if (member "output" result-params)
			   "(with-output-to-string %s\n)"
			 "(progn %s\n)")
		       (org-babel-expand-body:emacs-lisp body params)))

         (post-process
          (lambda (result)
            (org-babel-result-cond result-params
              (let ((print-level nil)
                    (print-length nil))
                (if (or (member "scalar" result-params)
                        (member "verbatim" result-params))
                    (format "%S" result)
                  (format "%s" result)))
              (my-org-reassemble-result result params))))
         ;; TODO: Do we need a lock to read/write a lexical var?
         ;;       Using one just in case.
         (outcome-lock (make-mutex "babel-outcome-lock"))
         (outcome nil)
	 (todo (lambda ()
                 (when sentinel
                   (funcall sentinel (list :progress "started")))
                 (let ((oc
                        (condition-case exc
                            (list :success
                                  (funcall post-process
                                           (eval (read (if (or (member "code" result-params)
				                               (member "pp" result-params))
				                           (concat "(pp " body ")")
			                                 body))
		                                 (org-babel-emacs-lisp-lexical lexical))))
                          (error (list :failure exc)))))
                   ;; Finger crossed: we run the feedback handler in a thread.
                   (when sentinel
                     (funcall sentinel oc))
                   (with-mutex outcome-lock
                     (setq outcome oc)))
                   (message "org babel thread done.")))
         (_worker (make-thread todo "babel-thread")))

    ;; Return the fonction to control the task.
    (lambda (&rest query)
      (pcase query
        (`(:get . ,_)
         ;; Block the user until the outcome is available.  On
         ;; success, return the result; on failure, raise an error.
         ;; We can't wait use a condition-wait; else, if running from
         ;; the main thread, it's going to hang Emacs display.
         (org-pending-wait-outcome
          (lambda () (with-mutex outcome-lock outcome))))
        (_ (org-pending-ti-not-implemented))))))


(defun my-use-threads-execute (lang body params)
  (org-babel-async-schedule-and-wait #'my-use-threads-schedule lang body params))

;;; Demo executing elisp using callbacks
;;

(defun my-use-callbacks-schedule (lang body params sentinel)
  "Schedule emacs-lisp code BODY according to PARAMS.
Execute the code providing callbacks to get the result."
  ;; Code adapted from: `org-babel-execute:emacs-lisp'.
  (unless (equal "elisp" lang)
    (user-error "Only for elisp source block"))
  (when-let ((lex-b (assq :lexical params)))
    (when (not (cdr lex-b))
      (user-error "Only lexical bindings this :execute-with")))
  (let* ((result-params (cdr (assq :result-params params)))
	 (body (progn
                 (when (or (member "output" result-params)
                           (member "pp" result-params)
                           (member "code" result-params))
                   (user-error "Invalid result-params for this :execute-with"))
		 (org-babel-expand-body:emacs-lisp body params)))
         (post-process
          (lambda (result)
            (org-babel-result-cond result-params
              (let ((print-level nil)
                    (print-length nil))
                (if (or (member "scalar" result-params)
                        (member "verbatim" result-params))
                    (format "%S" result)
                  (format "%s" result)))
              (my-org-reassemble-result result params))))
         (worker (eval (read body) t))
         (outcome nil)
         (progress-items)
         (report-outcome
          (lambda (oc)
            (when sentinel (funcall sentinel oc))
            (setq outcome oc))))

    (when sentinel
      (funcall sentinel (list :progress "started")))

    (funcall worker
             ;; report success
             (lambda (result)
               (funcall report-outcome
                        (condition-case exc
                            (list :success
                                  (funcall post-process result))
                          (error (list :failure exc)))))
             ;; report error
             (lambda (exc)
               (funcall report-outcome (list :failure exc)))
             ;; report progress
             (lambda (p)
               (when sentinel (funcall sentinel (list :progress p)))
               (push p progress-items)))

    ;; Return the fonction to control the task.
    (lambda (&rest query)
      (pcase query
        (`(:get . ,_)
         (org-pending-wait-outcome (lambda () outcome)))
        (`(:insert-details . ,_)
         (dolist (it (nreverse progress-items)) (insert (format "%s\n" it))))
        (_ (org-pending-ti-not-implemented))))))


(defun my-use-callbacks-execute (lang body params)
  (org-babel-async-schedule-and-wait #'my-use-callbacks-execute lang body params))


;;; Demo asynchronous dynamic blocks
;;
(defun org-dblock-write:sleeper (_params sentinel)
  (let ((outcome nil))
    (funcall sentinel (list :progress "started"))
    ( run-with-idle-timer 1 nil
      (lambda ()
        (funcall sentinel
		 (list :success (concat "You're sleeping! (at "
					(current-time-string)
					")")))
        (setq outcome (list :success nil))))
    (lambda (&rest query)
      (pcase query
        (`(:get . ,_)
         (org-pending-wait-outcome (lambda () outcome)))

        (`(:cancel ,_penreg)
         (org-pending-ti-not-implemented))

        (`(:insert-details ,_penreg ,_start ,_end)
         (org-pending-ti-not-implemented))

        (_ (error "Unknown query"))))))

;; Tell org that 'sleeper' is for asynchronous dynamic blocks.
(put 'org-dblock-write:sleeper 'nasync t)

;;; Using org-async-call
;;
;; This requires to merge the 'dev' branch from:
;;     https://code.tecosaur.net/tec/org-mode
;;
;; which provides:
;;     - org-async-call
;;     - org-async-wait-for
;;
(defun my-async-call-schedule (lang body params sentinel)
  ;; Basic executions of shell and python scripts.
  (let* ((mk-pargs (lambda (sfn)
                  (pcase lang
                    ((or "shell" "bash")   (list "bash" "-c" sfn))
                    ("python" (list "python" sfn)))))
         (tmp-buffer (generate-new-buffer " *ob-async-call"))
         (handle-outcome
          (lambda (constr)
            (lambda (_exit process-buffer _info)
              (when sentinel
                (funcall sentinel
                         (list constr
                               (with-current-buffer process-buffer
                                 (buffer-substring-no-properties (point-min) (point-max))))))
              (my-elib-async-kill-buffer-later tmp-buffer))))

         (script-fn (org-babel-temp-file "ob-async-call-script-"))
         (script-run-fn (if (file-remote-p script-fn)
                            (file-local-name script-fn)
                          script-fn))
         (full-body (concat
                     (org-babel-expand-body:generic
                      body params
                      (funcall (intern-soft (concat "org-babel-variable-assignments:" lang))
                               params))))
         task)
    (with-temp-file script-fn (insert full-body))
    (set-file-modes script-fn #o755)
    (setq task (org-async-call (funcall mk-pargs script-run-fn)
                               :buffer tmp-buffer ;; FIXME: t doesn't work
                               :success (funcall handle-outcome :success)
                               :failure (funcall handle-outcome :failure)
                               :dir (or (cdr (assq :dir params)) default-directory)
                               :timeout (cdr (assq :timeout params))))
    (lambda (&rest query)
      (pcase query
        (`(:get . ,_)
         (org-async-wait-for task))
        (_ (org-pending-ti-not-implemented))))))


(defun my-async-call-execute (lang body params)
  (org-babel-async-schedule-and-wait #'my-async-call-schedule lang body params))
