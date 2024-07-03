;;; org-colview-agenda.el--- Column View in Org Agenda            -*- lexical-binding: t; -*-

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

;; This file contains the column view for Org Agenda view.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-colview)
(require 'org-agenda-mode)
(require 'org-clock-sum)


;;; Configuration

(defgroup org-agenda-column-view nil
  "Options concerning column view in the agenda."
  :tag "Org Agenda Column View"
  :group 'org-agenda)

(defcustom org-columns-default-format-for-agenda nil
  "The default column format in an agenda buffer.
This will be used for column view in the agenda unless a format has
been set by adding `org-overriding-columns-format' to the local
settings list of a custom agenda view.  When nil, the columns format
for the first item in the agenda list will be used, or as a fall-back,
`org-columns-default-format'."
  :group 'org-properties
  :type '(choice
	  (const :tag "No default" nil)
	  (string :tag "Format string")))

(defcustom org-agenda-columns-show-summaries t
  "Non-nil means show summaries for columns displayed in the agenda view."
  :group 'org-agenda-column-view
  :type 'boolean)

(defcustom org-agenda-columns-compute-summary-properties t
  "Non-nil means recompute all summary properties before column view.
When column view in the agenda is listing properties that have a summary
operator, it can go to all relevant buffers and recompute the summaries
there.  This can mean overhead for the agenda column view, but is necessary
to have thing up to date.
As a special case, a CLOCKSUM property also makes sure that the clock
computations are current."
  :group 'org-agenda-column-view
  :type 'boolean)

(defcustom org-agenda-columns-add-appointments-to-effort-sum nil
  "Non-nil means the duration of an appointment will add to day effort.
The property to which appointment durations will be added is the one given
in the option `org-effort-property'.  If an appointment does not have
an end time, `org-agenda-default-appointment-duration' will be used.  If that
is not set, an appointment without end time will not contribute to the time
estimate."
  :group 'org-agenda-column-view
  :type 'boolean)


;;; Column View

;;;###autoload
(defun org-agenda-columns ()
  "Turn on or update column view in the agenda."
  (interactive)
  (org-columns-remove-overlays)
  (if (markerp org-columns-begin-marker)
      (move-marker org-columns-begin-marker (point))
    (setq org-columns-begin-marker (point-marker)))
  (let* ((org-columns--time (float-time))
	 (fmt
	  (cond
	   ((bound-and-true-p org-overriding-columns-format))
	   ((bound-and-true-p org-local-columns-format))
	   ((bound-and-true-p org-columns-default-format-for-agenda))
	   ((let ((m (org-get-at-bol 'org-hd-marker)))
	      (and m
		   (or (org-entry-get m "COLUMNS" t)
		       (with-current-buffer (marker-buffer m)
			 org-columns-default-format)))))
	   ((and (local-variable-p 'org-columns-current-fmt)
		 org-columns-current-fmt))
	   ((let ((m (next-single-property-change (point-min) 'org-hd-marker)))
	      (and m
		   (let ((m (get-text-property m 'org-hd-marker)))
		     (or (org-entry-get m "COLUMNS" t)
			 (with-current-buffer (marker-buffer m)
			   org-columns-default-format))))))
	   (t org-columns-default-format)))
	 (compiled-fmt (org-columns-compile-format fmt)))
    (setq org-columns-current-fmt fmt)
    (when org-agenda-columns-compute-summary-properties
      (org-agenda-colview-compute org-columns-current-fmt-compiled))
    (save-excursion
      ;; Collect properties for each headline in current view.
      (goto-char (point-min))
      (let (cache)
	(while (not (eobp))
	  (let ((m (org-get-at-bol 'org-hd-marker)))
	    (when m
	      (push (cons (line-beginning-position)
			  ;; `org-columns-current-fmt-compiled' is
			  ;; initialized but only set locally to the
			  ;; agenda buffer.  Since current buffer is
			  ;; changing, we need to force the original
			  ;; compiled-fmt there.
			  (org-with-point-at m
			    (org-columns--collect-values compiled-fmt)))
		    cache)))
	  (forward-line))
	(when cache
	  (org-columns--set-widths cache)
	  (org-columns--display-here-title)
	  (when (setq-local org-columns-flyspell-was-active
			    (bound-and-true-p flyspell-mode))
	    (flyspell-mode 0))
	  (dolist (entry cache)
	    (goto-char (car entry))
	    (org-columns--display-here (cdr entry) nil (org-element-done-keywords (org-get-at-bol 'org-hd-marker))))
	  (setq-local org-agenda-columns-active t)
	  (when org-agenda-columns-show-summaries
	    (org-agenda-colview-summarize cache)))))))

(defun org-agenda-colview-summarize (cache)
  "Summarize the summarizable columns in column view in the agenda.
This will add overlays to the date lines, to show the summary for each day."
  (let ((fmt (mapcar
	      (lambda (spec)
		(pcase spec
		  (`(,property ,title ,width . ,_)
		   (if (member property '("CLOCKSUM" "CLOCKSUM_T"))
		       (list property title width ":" nil)
		     spec))))
	      org-columns-current-fmt-compiled)))
    ;; Ensure there's at least one summation column.
    (when (cl-some (lambda (spec) (nth 3 spec)) fmt)
      (goto-char (point-max))
      (catch :complete
	(while t
	  (when (or (get-text-property (point) 'org-date-line)
		    (eq (get-text-property (point) 'face)
			'org-agenda-structure))
	    ;; OK, this is a date line that should be used.
	    (let (entries)
	      (let (rest)
		(dolist (c cache)
		  (if (> (car c) (point))
		      (push c entries)
		    (push c rest)))
		(setq cache rest))
	      ;; ENTRIES contains entries below the current one.
	      ;; CACHE is the rest.  Compute the summaries for the
	      ;; properties we want, set nil properties for the rest.
	      (when (setq entries (mapcar #'cdr entries))
		(org-columns--display-here
		 (mapcar
		  (lambda (spec)
		    (pcase spec
		      (`("ITEM" . ,_)
		       ;; Replace ITEM with current date.  Preserve
		       ;; properties for fontification.
		       (let ((date (buffer-substring
				    (line-beginning-position)
				    (line-end-position))))
			 (list spec date date)))
		      (`(,_ ,_ ,_ nil ,_) (list spec "" ""))
		      (`(,_ ,_ ,_ ,operator ,printf)
		       (let* ((summarize (org-columns--summarize operator))
			      (values
			       ;; Use real values for summary, not
			       ;; those prepared for display.
			       (delq nil
				     (mapcar
				      (lambda (e) (org-string-nw-p
					      (nth 1 (assoc spec e))))
				      entries)))
			      (final (if values
					 (funcall summarize values printf)
				       "")))
			 (unless (equal final "")
			   (put-text-property 0 (length final)
					      'face 'bold final))
			 (list spec final final)))))
		  fmt)
		 'dateline
                 (org-element-done-keywords (org-get-at-bol 'org-hd-marker))))))
	  (if (bobp) (throw :complete t) (forward-line -1)))))))

(defun org-agenda-colview-compute (fmt)
  "Compute the relevant columns in the contributing source buffers."
  (dolist (file org-agenda-contributing-files)
    (let ((b (find-buffer-visiting file)))
      (with-current-buffer (or (buffer-base-buffer b) b)
	(org-with-wide-buffer
	 (with-silent-modifications
	   (remove-text-properties (point-min) (point-max) '(org-summaries t)))
	 (goto-char (point-min))
	 (org-columns-get-format-and-top-level)
	 (dolist (spec fmt)
	   (let ((prop (car spec)))
	     (cond
	      ((equal prop "CLOCKSUM")
               (org-clock-sum))
	      ((equal prop "CLOCKSUM_T")
               (org-clock-sum-today))
	      ((and (nth 3 spec)
		    (let ((a (assoc prop org-columns-current-fmt-compiled)))
		      (equal (nth 3 a) (nth 3 spec))))
	       (org-columns-compute prop))))))))))

(provide 'org-colview-agenda)

;;; org-colview-agenda.el ends here
