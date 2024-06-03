;;; org-clocktable.el --- Clock table -*- lexical-binding: t; -*-

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

;; This file contains definition of Org clocktable dynamic block.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-clock-core)
(require 'org-property)
(require 'ol)
(require 'org-table-move)
(require 'org-duration)
(require 'org-agenda-files)
(require 'org-element-timestamp)
(require 'org-dblock)
(require 'org-font-lock-common)

(defgroup org-clocktable nil
  "Options concerning the clock table in Org mode."
  :tag "Org Clock Table"
  :group 'org-clock)

(defcustom org-clocktable-defaults
  (list
   :maxlevel 2
   :lang (or (bound-and-true-p org-export-default-language) "en")
   :scope 'file
   :block nil
   :wstart 1
   :mstart 1
   :tstart nil
   :tend nil
   :step nil
   :stepskip0 nil
   :fileskip0 nil
   :tags nil
   :match nil
   :emphasize nil
   :link nil
   :narrow '40!
   :indent t
   :filetitle nil
   :hidefiles nil
   :formula nil
   :timestamp nil
   :level nil
   :tcolumns nil
   :formatter nil)
  "Default properties for clock tables."
  :group 'org-clock
  :package-version '(Org . "9.6")
  :type 'plist)

(defcustom org-clock-clocktable-formatter 'org-clocktable-write-default
  "Function to turn clocking data into a table.
For more information, see `org-clocktable-write-default'."
  :group 'org-clocktable
  :version "24.1"
  :type 'function)

(defcustom org-clock-total-time-cell-format "*%s*"
  "Format string for the total time cells."
  :group 'org-clock
  :version "24.1"
  :type 'string)

(defcustom org-clock-file-time-cell-format "*%s*"
  "Format string for the file time cells."
  :group 'org-clock
  :version "24.1"
  :type 'string)

(defcustom org-clock-clocktable-language-setup
  '(("en" "File"     "L" "Timestamp" "Headline" "Time" "ALL" "Total time" "File time" "Clock summary at")
    ("de" "Datei"    "E" "Zeitstempel" "Kopfzeile" "Dauer" "GESAMT" "Gesamtdauer" "Dateizeit" "Erstellt am")
    ("es" "Archivo"  "N" "Fecha y hora" "Tarea" "Duración" "TODO" "Duración total" "Tiempo archivo" "Generado el")
    ("fr" "Fichier"  "N" "Horodatage" "En-tête"  "Durée" "TOUT"  "Durée totale" "Durée fichier" "Horodatage sommaire à")
    ("nl" "Bestand"  "N" "Tijdstip" "Rubriek" "Duur" "ALLES" "Totale duur" "Bestandstijd" "Klok overzicht op")
    ("nn" "Fil"      "N" "Tidspunkt" "Overskrift" "Tid" "ALLE" "Total tid" "Filtid" "Tidsoversyn")
    ("pl" "Plik"     "P" "Data i godzina" "Nagłówek" "Czas" "WSZYSTKO" "Czas całkowity" "Czas pliku" "Poddumowanie zegara na")
    ("pt-BR" "Arquivo" "N" "Data e hora" "Título" "Hora" "TODOS" "Hora total" "Hora do arquivo" "Resumo das horas em")
    ("sk" "Súbor"    "L" "Časová značka" "Záhlavie" "Čas" "VŠETKO" "Celkový čas" "Čas súboru" "Časový súhrn pre"))
  "Terms used in clocktable, translated to different languages."
  :group 'org-clocktable
  :version "24.1"
  :type 'alist)

(defcustom org-clock-clocktable-default-properties '(:maxlevel 2)
  "Default properties for new clocktables.
These will be inserted into the BEGIN line, to make it easy for users to
play with them."
  :group 'org-clocktable
  :package-version '(Org . "9.2")
  :type 'plist)

(defun org-clock--translate (s language)
  "Translate string S into using string LANGUAGE.
Assume S in the English term to translate.  Return S as-is if it
cannot be translated."
  (or (nth (pcase s
             ;; "L" stands for "Level"
             ;; "ALL" stands for a line summarizing clock data across
             ;; all the files, when the clocktable includes multiple
             ;; files.
	     ("File" 1) ("L" 2) ("Timestamp" 3) ("Headline" 4) ("Time" 5)
	     ("ALL" 6) ("Total time" 7) ("File time" 8) ("Clock summary at" 9))
	   (assoc-string language org-clock-clocktable-language-setup t))
      s))

;;;###autoload
(defun org-clock-get-clocktable (&rest props)
  "Get a formatted clocktable with parameters according to PROPS.
The table is created in a temporary buffer, fully formatted and
fontified, and then returned."
  ;; Set the defaults
  (setq props (plist-put props :name "clocktable"))
  (unless (plist-member props :maxlevel)
    (setq props (plist-put props :maxlevel 2)))
  (unless (plist-member props :scope)
    (setq props (plist-put props :scope 'agenda)))
  (with-temp-buffer
    (org-mode)
    (org-create-dblock props)
    (org-update-dblock)
    (font-lock-ensure)
    (forward-line 2)
    (buffer-substring (point) (progn
				(re-search-forward "^[ \t]*#\\+END" nil t)
                                (line-beginning-position)))))

;;;###autoload
(defun org-clock-report (&optional arg)
  "Update or create a table containing a report about clocked time.

If point is inside an existing clocktable block, update it.
Otherwise, insert a new one.

The new table inherits its properties from the variable
`org-clock-clocktable-default-properties'.

The scope of the clocktable, when not specified in the previous
variable, is `subtree' of the current heading when the function is
called from inside heading, and `file' elsewhere (before the first
heading).

When called with a prefix argument, move to the first clock table
in the buffer and update it."
  (interactive "P")
  (when (fboundp 'org-clock-remove-overlays)
    (org-clock-remove-overlays))
  (when arg
    (org-find-dblock "clocktable")
    (org-fold-show-entry))
  (pcase (org-in-clocktable-p)
    (`nil
     (org-create-dblock
      (org-combine-plists
       (list :scope (if (org-before-first-heading-p) 'file 'subtree))
       org-clock-clocktable-default-properties
       '(:name "clocktable"))))
    (start (goto-char start)))
  (org-update-dblock))

;;;###autoload
(eval-after-load 'org-dblock
  '(org-dynamic-block-define "clocktable" #'org-clock-report))

(defun org-quarter-to-date (quarter year)
  "Get the date (week day year) of the first day of a given quarter."
  (let (startday)
    (cond
     ((= quarter 1)
      (setq startday (org-day-of-week 1 1 year))
      (cond
       ((= startday 0)
	(list 52 7 (- year 1)))
       ((= startday 6)
	(list 52 6 (- year 1)))
       ((<= startday 4)
	(list 1 startday year))
       ((> startday 4)
	(list 53 startday (- year 1)))
       )
      )
     ((= quarter 2)
      (setq startday (org-day-of-week 1 4 year))
      (cond
       ((= startday 0)
	(list 13 startday year))
       ((< startday 4)
	(list 14 startday year))
       ((>= startday 4)
	(list 13 startday year))
       )
      )
     ((= quarter 3)
      (setq startday (org-day-of-week 1 7 year))
      (cond
       ((= startday 0)
	(list 26 startday year))
       ((< startday 4)
	(list 27 startday year))
       ((>= startday 4)
	(list 26 startday year))
       )
      )
     ((= quarter 4)
      (setq startday (org-day-of-week 1 10 year))
      (cond
       ((= startday 0)
	(list 39 startday year))
       ((<= startday 4)
	(list 40 startday year))
       ((> startday 4)
	(list 39 startday year)))))))

(defun org-count-quarter (n)
  (cond
   ((= n 1) "1st")
   ((= n 2) "2nd")
   ((= n 3) "3rd")
   ((= n 4) "4th")))

(declare-function calendar-iso-to-absolute "cal-iso" (date))
;;;###autoload
(defun org-clocktable-shift (dir n)
  "Try to shift the :block date of the clocktable at point.
Point must be in the #+BEGIN: line of a clocktable, or this function
will throw an error.
DIR is a direction, a symbol `left', `right', `up', or `down'.
Both `left' and `down' shift the block toward the past, `up' and `right'
push it toward the future.
N is the number of shift steps to take.  The size of the step depends on
the currently selected interval size."
  (setq n (prefix-numeric-value n))
  (and (memq dir '(left down)) (setq n (- n)))
  (save-excursion
    (goto-char (line-beginning-position))
    (if (not (looking-at "^[ \t]*#\\+BEGIN:[ \t]+clocktable\\>.*?:block[ \t]+\\(\\S-+\\)"))
	(user-error "Line needs a :block definition before this command works")
      (let* ((b (match-beginning 1)) (e (match-end 1))
	     (s (match-string 1))
	     block shift ins y mw d date wp) ;; m
	(cond
	 ((equal s "yesterday") (setq s "today-1"))
	 ((equal s "lastweek") (setq s "thisweek-1"))
	 ((equal s "lastmonth") (setq s "thismonth-1"))
	 ((equal s "lastyear") (setq s "thisyear-1"))
	 ((equal s "lastq") (setq s "thisq-1")))

	(cond
	 ((string-match "^\\(today\\|thisweek\\|thismonth\\|thisyear\\|thisq\\)\\([-+][0-9]+\\)?$" s)
	  (setq block (match-string 1 s)
		shift (if (match-end 2)
			  (string-to-number (match-string 2 s))
			0))
	  (setq shift (+ shift n))
	  (setq ins (if (= shift 0) block (format "%s%+d" block shift))))
	 ((string-match "\\([0-9]+\\)\\(-\\([wWqQ]?\\)\\([0-9]\\{1,2\\}\\)\\(-\\([0-9]\\{1,2\\}\\)\\)?\\)?" s)
	  ;;               1        1  2   3       3  4                  4  5   6                6  5   2
	  (setq y (string-to-number (match-string 1 s))
		wp (and (match-end 3) (match-string 3 s))
		mw (and (match-end 4) (string-to-number (match-string 4 s)))
		d (and (match-end 6) (string-to-number (match-string 6 s))))
	  (cond
	   (d (setq ins (format-time-string
			 "%Y-%m-%d"
			 (org-encode-time 0 0 0 (+ d n) nil y)))) ;; m
	   ((and wp (string-match "w\\|W" wp) mw (> (length wp) 0))
	    (require 'cal-iso)
	    (setq date (calendar-gregorian-from-absolute
			(calendar-iso-to-absolute (list (+ mw n) 1 y))))
	    (setq ins (format-time-string
		       "%G-W%V"
		       (org-encode-time 0 0 0 (nth 1 date) (car date) (nth 2 date)))))
	   ((and wp (string-match "q\\|Q" wp) mw (> (length wp) 0))
	    (require 'cal-iso)
					; if the 4th + 1 quarter is requested we flip to the 1st quarter of the next year
	    (if (> (+ mw n) 4)
		(setq mw 0
		      y (+ 1 y))
	      ())
					; if the 1st - 1 quarter is requested we flip to the 4th quarter of the previous year
	    (if (= (+ mw n) 0)
		(setq mw 5
		      y (- y 1))
	      ())
	    (setq date (calendar-gregorian-from-absolute
			(calendar-iso-to-absolute (org-quarter-to-date (+ mw n) y))))
	    (setq ins (format-time-string
		       (concat (number-to-string y) "-Q" (number-to-string (+ mw n)))
		       (org-encode-time 0 0 0 (nth 1 date) (car date) (nth 2 date)))))
	   (mw
	    (setq ins (format-time-string
		       "%Y-%m"
		       (org-encode-time 0 0 0 1 (+ mw n) y))))
	   (y
	    (setq ins (number-to-string (+ y n))))))
	 (t (user-error "Cannot shift clocktable block")))
	(when ins
	  (goto-char b)
	  (insert ins)
	  (delete-region (point) (+ (point) (- e b)))
	  (forward-line 0)
	  (org-update-dblock)
	  t)))))

(declare-function org-narrow-to-subtree "org-narrow" (&optional element))
(declare-function org-add-archive-files "org-archive-core" (files))
;;;###autoload
(defun org-dblock-write:clocktable (params)
  "Write the standard clocktable."
  (setq params (org-combine-plists org-clocktable-defaults params))
  (catch 'exit
    (let* ((scope (plist-get params :scope))
	   (base-buffer (org-base-buffer (current-buffer)))
	   (files (pcase scope
		    (`agenda
		     (org-agenda-files t))
		    (`agenda-with-archives
                     (require 'org-archive-core)
		     (org-add-archive-files (org-agenda-files t)))
		    (`file-with-archives
		     (let ((base-file (buffer-file-name base-buffer)))
		       (and base-file
			    (org-add-archive-files (list base-file)))))
		    ((or `nil `file `subtree `tree
			 (and (pred symbolp)
			      (guard (string-match "\\`tree\\([0-9]+\\)\\'"
						   (symbol-name scope)))))
		     base-buffer)
		    ((pred functionp) (funcall scope))
		    ((pred consp) scope)
		    (_ (user-error "Unknown scope: %S" scope))))
	   (block (plist-get params :block))
	   (ts (plist-get params :tstart))
	   (te (plist-get params :tend))
	   (ws (plist-get params :wstart))
	   (ms (plist-get params :mstart))
	   (step (plist-get params :step))
	   (hide-files (plist-get params :hidefiles))
	   (formatter (or (plist-get params :formatter)
			  org-clock-clocktable-formatter
			  'org-clocktable-write-default))
	   cc)
      ;; Check if we need to do steps
      (when block
	;; Get the range text for the header
	(setq cc (org-clock-special-range block nil t ws ms)
	      ts (car cc)
	      te (nth 1 cc)))
      (when step
	;; Write many tables, in steps
	(unless (or block (and ts te))
	  (user-error "Clocktable `:step' can only be used with `:block' or `:tstart', `:tend'"))
	(org-clocktable-steps params)
	(throw 'exit nil))

      (org-agenda-prepare-buffers (if (consp files) files (list files)))

      (let ((origin (point))
	    (tables
	     (if (consp files)
		 (mapcar (lambda (file)
			   (with-current-buffer (find-buffer-visiting file)
			     (save-excursion
			       (save-restriction
				 (org-clock-get-table-data file params)))))
			 files)
	       ;; Get the right restriction for the scope.
	       (save-restriction
		 (cond
		  ((not scope))	     ;use the restriction as it is now
		  ((eq scope 'file) (widen))
		  ((eq scope 'subtree)
                   (require 'org-narrow)
                   (org-narrow-to-subtree))
		  ((eq scope 'tree)
		   (while (org-up-heading-safe))
		   (org-narrow-to-subtree))
		  ((and (symbolp scope)
			(string-match "\\`tree\\([0-9]+\\)\\'"
				      (symbol-name scope)))
		   (let ((level (string-to-number
				 (match-string 1 (symbol-name scope)))))
		     (catch 'exit
		       (while (org-up-heading-safe)
			 (looking-at org-outline-regexp)
			 (when (<= (org-reduced-level (funcall outline-level))
				  level)
			   (throw 'exit nil))))
		     (org-narrow-to-subtree))))
		 (list (org-clock-get-table-data nil params)))))
	    (multifile
	     ;; Even though `file-with-archives' can consist of
	     ;; multiple files, we consider this is one extended file
	     ;; instead.
	     (and (not hide-files)
		  (consp files)
		  (not (eq scope 'file-with-archives)))))

	(funcall formatter
		 origin
		 tables
		 (org-combine-plists params `(:multifile ,multifile)))))))

(declare-function org-table-align "org-table-align" ())
(declare-function org-table-recalculate "org-table-formula"
                  (&optional all noalign))
(declare-function org-table-sort-lines "org-table-edit"
                  (&optional with-case sorting-type getkey-func compare-func interactive?))
(declare-function org-table-goto-column "org-table-core"
                  (n &optional on-delim force))
(defun org-clocktable-write-default (ipos tables params)
  "Write out a clock table at position IPOS in the current buffer.
TABLES is a list of tables with clocking data as produced by
`org-clock-get-table-data'.  PARAMS is the parameter property list obtained
from the dynamic block definition."
  ;; This function looks quite complicated, mainly because there are a
  ;; lot of options which can add or remove columns.  I have massively
  ;; commented this function, the I hope it is understandable.  If
  ;; someone wants to write their own special formatter, this maybe
  ;; much easier because there can be a fixed format with a
  ;; well-defined number of columns...
  (let* ((lang (or (plist-get params :lang) "en"))
	 (multifile (plist-get params :multifile))
	 (block (plist-get params :block))
	 (sort (plist-get params :sort))
	 (header (plist-get params :header))
	 (link (plist-get params :link))
	 (maxlevel (or (plist-get params :maxlevel) 3))
	 (emph (plist-get params :emphasize))
	 (compact? (plist-get params :compact))
	 (narrow (or (plist-get params :narrow) (and compact? '40!)))
	 (filetitle (plist-get params :filetitle))
	 (level? (and (not compact?) (plist-get params :level)))
	 (timestamp (plist-get params :timestamp))
	 (tags (plist-get params :tags))
	 (properties (plist-get params :properties))
	 (time-columns
	  (if (or compact? (< maxlevel 2)) 1
	    ;; Deepest headline level is a hard limit for the number
	    ;; of time columns.
	    (let ((levels
		   (cl-mapcan
		    (lambda (table)
		      (pcase table
			(`(,_ ,(and (pred wholenump) (pred (/= 0))) ,entries)
			 (mapcar #'car entries))))
		    tables)))
	      (min maxlevel
		   (or (plist-get params :tcolumns) 100)
		   (if (null levels) 1 (apply #'max levels))))))
	 (indent (or compact? (plist-get params :indent)))
	 (formula (plist-get params :formula))
	 (case-fold-search t)
	 (total-time (apply #'+ (mapcar #'cadr tables)))
	 recalc narrow-cut-p)

    (when (and narrow (integerp narrow) link)
      ;; We cannot have both integer narrow and link.
      (message "Using hard narrowing in clocktable to allow for links")
      (setq narrow (intern (format "%d!" narrow))))

    (pcase narrow
      ((or `nil (pred integerp)) nil)	;nothing to do
      ((and (pred symbolp)
	    (guard (string-match-p "\\`[0-9]+!\\'" (symbol-name narrow))))
       (setq narrow-cut-p t)
       (setq narrow (string-to-number (symbol-name narrow))))
      (_ (user-error "Invalid value %s of :narrow property in clock table" narrow)))

    ;; Now we need to output this table stuff.
    (goto-char ipos)

    ;; Insert the text *before* the actual table.
    (insert-before-markers
     (or header
	 ;; Format the standard header.
	 (format "#+CAPTION: %s %s%s\n"
		 (org-clock--translate "Clock summary at" lang)
		 (format-time-string (org-time-stamp-format t t))
		 (if block
		     (let ((range-text
			    (nth 2 (org-clock-special-range
				    block nil t
				    (plist-get params :wstart)
				    (plist-get params :mstart)))))
		       (format ", for %s." range-text))
		   ""))))

    ;; Insert the narrowing line
    (when (and narrow (integerp narrow) (not narrow-cut-p))
      (insert-before-markers
       "|"				;table line starter
       (if multifile "|" "")		;file column, maybe
       (if level? "|" "")		;level column, maybe
       (if timestamp "|" "")		;timestamp column, maybe
       (if tags "|" "")                 ;tags columns, maybe
       (if properties			;properties columns, maybe
	   (make-string (length properties) ?|)
	 "")
       (format "<%d>| |\n" narrow)))	;headline and time columns

    ;; Insert the table header line
    (insert-before-markers
     "|"				;table line starter
     (if multifile			;file column, maybe
	 (concat (org-clock--translate "File" lang) "|")
       "")
     (if level?				;level column, maybe
	 (concat (org-clock--translate "L" lang) "|")
       "")
     (if timestamp			;timestamp column, maybe
	 (concat (org-clock--translate "Timestamp" lang) "|")
       "")
     (if tags "Tags |" "")              ;tags columns, maybe

     (if properties			;properties columns, maybe
	 (concat (mapconcat #'identity properties "|") "|")
       "")
     (concat (org-clock--translate "Headline" lang)"|")
     (concat (org-clock--translate "Time" lang) "|")
     (make-string (max 0 (1- time-columns)) ?|) ;other time columns
     (if (eq formula '%) "%|\n" "\n"))

    ;; Insert the total time in the table
    (insert-before-markers
     "|-\n"				;a hline
     "|"				;table line starter
     (if multifile (format "| %s " (org-clock--translate "ALL" lang)) "")
					;file column, maybe
     (if level?    "|" "")		;level column, maybe
     (if timestamp "|" "")		;timestamp column, maybe
     (if tags      "|" "")		;timestamp column, maybe
     (make-string (length properties) ?|) ;properties columns, maybe
     (concat (format org-clock-total-time-cell-format
		     (org-clock--translate "Total time" lang))
	     "| ")
     (format org-clock-total-time-cell-format
	     (org-duration-from-minutes (or total-time 0))) ;time
     "|"
     (make-string (max 0 (1- time-columns)) ?|)
     (cond ((not (eq formula '%)) "")
	   ((or (not total-time) (= total-time 0)) "0.0|")
	   (t  "100.0|"))
     "\n")

    ;; Now iterate over the tables and insert the data but only if any
    ;; time has been collected.
    (when (and total-time (> total-time 0))
      (pcase-dolist (`(,file-name ,file-time ,entries) tables)
	(when (or (and file-time (> file-time 0))
		  (not (plist-get params :fileskip0)))
	  (insert-before-markers "|-\n") ;hline at new file
	  ;; First the file time, if we have multiple files.
	  (when multifile
	    ;; Summarize the time collected from this file.
	    (insert-before-markers
	     (format (concat "| %s %s | %s%s%s"
			     (format org-clock-file-time-cell-format
				     (org-clock--translate "File time" lang))

			     ;; The file-time rollup value goes in the first time
			     ;; column (of which there is always at least one)...
			     " | *%s*|"
			     ;; ...and the remaining file time cols (if any) are blank.
			     (make-string (max 0 (1- time-columns)) ?|)

			     ;; Optionally show the percentage contribution of "this"
			     ;; file time to the total time.
			     (if (eq formula '%) " %s |" "")
			     "\n")

                     (if filetitle
                         (or (org-get-title file-name)
                             (file-name-nondirectory file-name))
                       (file-name-nondirectory file-name))
		     (if level?    "| " "") ;level column, maybe
		     (if timestamp "| " "") ;timestamp column, maybe
		     (if tags      "| " "") ;tags column, maybe
		     (if properties	    ;properties columns, maybe
			 (make-string (length properties) ?|)
		       "")
		     (org-duration-from-minutes file-time) ;time

		     (cond ((not (eq formula '%)) "")	   ;time percentage, maybe
			   ((or (not total-time) (= total-time 0)) "0.0")
			   (t
			    (format "%.1f" (* 100 (/ file-time (float total-time)))))))))

	  ;; Get the list of node entries and iterate over it
	  (when (> maxlevel 0)
	    (pcase-dolist (`(,level ,headline ,tgs ,ts ,time ,props) entries)
	      (when narrow-cut-p
		(setq headline
		      (if (and (string-match
				(format "\\`%s\\'" org-link-bracket-re)
				headline)
			       (match-end 2))
			  (format "[[%s][%s]]"
				  (match-string 1 headline)
				  (org-shorten-string (match-string 2 headline)
						      narrow))
			(org-shorten-string headline narrow))))
	      (cl-flet ((format-field (f) (format (cond ((not emph) "%s |")
							((= level 1) "*%s* |")
							((= level 2) "/%s/ |")
							(t "%s |"))
						  f)))
		(insert-before-markers
		 "|"		       ;start the table line
		 (if multifile "|" "") ;free space for file name column?
		 (if level? (format "%d|" level) "") ;level, maybe
		 (if timestamp (concat ts "|") "")   ;timestamp, maybe
		 (if tags (concat (mapconcat #'identity tgs ", ") "|") "")   ;tags, maybe
		 (if properties		;properties columns, maybe
		     (concat (mapconcat (lambda (p) (or (cdr (assoc p props)) ""))
				        properties
				        "|")
			     "|")
		   "")
		 (if indent		;indentation
		     (org-clocktable-indent-string level)
		   "")
		 (format-field headline)
		 ;; Empty fields for higher levels.
		 (make-string (max 0 (1- (min time-columns level))) ?|)
		 (format-field (org-duration-from-minutes time))
		 (make-string (max 0 (- time-columns level)) ?|)
		 (if (eq formula '%)
		     (format "%.1f |" (* 100 (/ time (float total-time))))
		   "")
		 "\n")))))))
    (delete-char -1)
    (cond
     ;; Possibly rescue old formula?
     ((or (not formula) (eq formula '%))
      (let ((contents (org-string-nw-p (plist-get params :content))))
	(when (and contents (string-match "^\\([ \t]*#\\+tblfm:.*\\)" contents))
	  (setq recalc t)
	  (insert "\n" (match-string 1 contents))
	  (forward-line -1))))
     ;; Insert specified formula line.
     ((stringp formula)
      (insert "\n#+TBLFM: " formula)
      (setq recalc t))
     (t
      (user-error "Invalid :formula parameter in clocktable")))
    ;; Back to beginning, align the table, recalculate if necessary.
    (goto-char ipos)
    (skip-chars-forward "^|")
    (require 'org-table-align)
    (org-table-align)
    (when org-hide-emphasis-markers
      ;; We need to align a second time.
      (org-table-align))
    (when sort
      (save-excursion
        (require 'org-table-edit)
	(org-table-goto-line 3)
	(org-table-goto-column (car sort))
	(org-table-sort-lines nil (cdr sort))))
    (when recalc
      (require 'org-table-formula)
      (org-table-recalculate 'all))
    total-time))

(defun org-clocktable-indent-string (level)
  "Return indentation string according to LEVEL.
LEVEL is an integer.  Indent by two spaces per level above 1."
  (if (= level 1) ""
    (concat "\\_" (make-string (* 2 (1- level)) ?\s))))

(defun org-clocktable-steps (params)
  "Create one or more clock tables, according to PARAMS.
Step through the range specifications in plist PARAMS to make
a number of clock tables."
  (let* ((ignore-empty-tables (plist-get params :stepskip0))
         (step (plist-get params :step))
         (step-header
          (pcase step
            (`day "Daily report: ")
            (`week "Weekly report starting on: ")
            (`semimonth "Semimonthly report starting on: ")
            (`month "Monthly report starting on: ")
            (`year "Annual report starting on: ")
            (`quarter "Quarterly report starting on: ")
            (_ (user-error "Unknown `:step' specification: %S" step))))
         (week-start (or (plist-get params :wstart) 1))
         (month-start (or (plist-get params :mstart) 1))
         (range
          (pcase (plist-get params :block)
            (`nil nil)
            (range
             (org-clock-special-range range nil t week-start month-start))))
         ;; For both START and END, any number is an absolute day
         ;; number from Agenda.  Otherwise, consider value to be an Org
         ;; timestamp string.  The `:block' property has precedence
         ;; over `:tstart' and `:tend'.
         (start
          (pcase (if range (car range) (plist-get params :tstart))
            ((and (pred numberp) n)
             (pcase-let ((`(,m ,d ,y) (calendar-gregorian-from-absolute n)))
               (org-encode-time 0 0 org-extend-today-until d m y)))
            (timestamp
	     (seconds-to-time
	      (org-matcher-time (or timestamp
				    ;; The year Org was born.
				    "<2003-01-01 Thu 00:00>"))))))
         (end
          (pcase (if range (nth 1 range) (plist-get params :tend))
            ((and (pred numberp) n)
             (pcase-let ((`(,m ,d ,y) (calendar-gregorian-from-absolute n)))
               (org-encode-time 0 0 org-extend-today-until d m y)))
            (timestamp (seconds-to-time (org-matcher-time timestamp))))))
    (while (time-less-p start end)
      (unless (bolp) (insert "\n"))
      ;; Insert header before each clock table.
      (insert "\n"
              step-header
              (format-time-string (org-time-stamp-format nil t) start)
	      "\n")
      ;; Compute NEXT, which is the end of the current clock table,
      ;; according to step.
      (let* ((next
              ;; In Emacs-27 and Emacs-28 `encode-time' does not support 6 elements
              ;; list argument so `org-encode-time' can not be outside of `pcase'.
              (pcase-let
                  ((`(,_ ,_ ,_ ,d ,m ,y ,dow . ,_) (decode-time start)))
                (pcase step
                  (`day (org-encode-time 0 0 org-extend-today-until (1+ d) m y))
                  (`week
                   (let ((offset (if (= dow week-start) 7
                                   (mod (- week-start dow) 7))))
                     (org-encode-time 0 0 org-extend-today-until (+ d offset) m y)))
                  (`semimonth (org-encode-time 0 0 0
                                               (if (< d 16) 16 1)
                                               (if (< d 16) m (1+ m)) y))
                  (`month (org-encode-time 0 0 0 month-start (1+ m) y))
                  (`quarter (org-encode-time 0 0 0 month-start (+ 3 m) y))
                  (`year (org-encode-time 0 0 org-extend-today-until 1 1 (1+ y))))))
             (table-begin (line-beginning-position 0))
	     (step-time
              ;; Write clock table between START and NEXT.
	      (org-dblock-write:clocktable
	       (org-combine-plists
	        params (list :header ""
                             :step nil
                             :block nil
		             :tstart (format-time-string
                                      (org-time-stamp-format t t)
                                      start)
		             :tend (format-time-string
                                    (org-time-stamp-format t t)
                                    ;; Never include clocks past END.
                                    (if (time-less-p end next) end next)))))))
	(let ((case-fold-search t)) (re-search-forward "^[ \t]*#\\+END:"))
	;; Remove the table if it is empty and `:stepskip0' is
	;; non-nil.
	(when (and ignore-empty-tables (equal step-time 0))
	  (delete-region (line-beginning-position) table-begin))
        (setq start next))
      (end-of-line 0))))

(defun org-clock-get-table-data (file params)
  "Get the clocktable data for file FILE, with parameters PARAMS.
FILE is only for identification - this function assumes that
the correct buffer is current, and that the wanted restriction is
in place.
The return value will be a list with the file name and the total
file time (in minutes) as 1st and 2nd elements.  The third element
of this list will be a list of headline entries.  Each entry has the
following structure:

  (LEVEL HEADLINE TAGS TIMESTAMP TIME PROPERTIES)

LEVEL:      The level of the headline, as an integer.  This will be
            the reduced level, so 1,2,3,... even if only odd levels
            are being used.
HEADLINE:   The text of the headline.  Depending on PARAMS, this may
            already be formatted like a link.
TAGS:       The list of tags of the headline.
TIMESTAMP:  If PARAMS require it, this will be a time stamp found in the
            entry, any of SCHEDULED, DEADLINE, NORMAL, or first inactive,
            in this sequence.
TIME:       The sum of all time spend in this tree, in minutes.  This time
            will of cause be restricted to the time block and tags match
            specified in PARAMS.
PROPERTIES: The list properties specified in the `:properties' parameter
            along with their value, as an alist following the pattern
            (NAME . VALUE)."
  (let* ((maxlevel (or (plist-get params :maxlevel) 3))
	 (timestamp (plist-get params :timestamp))
	 (ts (plist-get params :tstart))
	 (te (plist-get params :tend))
	 (ws (plist-get params :wstart))
	 (ms (plist-get params :mstart))
	 (block (plist-get params :block))
	 (link (plist-get params :link))
	 (tags (plist-get params :tags))
	 (match (plist-get params :match))
	 (properties (plist-get params :properties))
	 (inherit-property-p (plist-get params :inherit-props))
	 (matcher (and match (cdr (org-make-tags-matcher match))))
	 cc st p tbl)

    (setq org-clock-file-total-minutes nil)
    (when block
      (setq cc (org-clock-special-range block nil t ws ms)
	    ts (car cc)
	    te (nth 1 cc)))
    (when (integerp ts) (setq ts (calendar-gregorian-from-absolute ts)))
    (when (integerp te) (setq te (calendar-gregorian-from-absolute te)))
    (when (and ts (listp ts))
      (setq ts (format "%4d-%02d-%02d" (nth 2 ts) (car ts) (nth 1 ts))))
    (when (and te (listp te))
      (setq te (format "%4d-%02d-%02d" (nth 2 te) (car te) (nth 1 te))))
    ;; Now the times are strings we can parse.
    (if ts (setq ts (org-matcher-time ts)))
    (if te (setq te (org-matcher-time te)))
    (save-excursion
      (org-clock-sum ts te
		     (when matcher
		       (lambda ()
			 (let* ((todo (org-get-todo-state))
				(tags-list (org-get-tags))
                                (level (org-current-level)))
			   (funcall matcher todo tags-list level)))))
      (goto-char (point-min))
      (setq st t)
      (while (or (and (bobp) (prog1 st (setq st nil))
		      (get-text-property (point) :org-clock-minutes)
		      (setq p (point-min)))
		 (setq p (next-single-property-change
			  (point) :org-clock-minutes)))
	(goto-char p)
	(let ((time (get-text-property p :org-clock-minutes)))
	  (when (and time (> time 0) (org-at-heading-p))
	    (let ((level (org-reduced-level (org-current-level))))
	      (when (<= level maxlevel)
		(let* ((headline (org-get-heading t t t t))
		       (hdl
			(if (not link) headline
			  (let ((search
				 (org-link-heading-search-string headline)))
			    (org-link-make-string
			     (if (not (buffer-file-name)) search
			       (format "file:%s::%s" (buffer-file-name) search))
			     ;; Prune statistics cookies.  Replace
			     ;; links with their description, or
			     ;; a plain link if there is none.
			     (org-trim
			      (org-link-display-format
			       (replace-regexp-in-string
				"\\[[0-9]*\\(?:%\\|/[0-9]*\\)\\]" ""
				headline)))))))
		       (tgs (and tags (org-get-tags)))
		       (tsp
			(and timestamp
			     (cl-some (lambda (p) (org-entry-get (point) p))
				      '("SCHEDULED" "DEADLINE" "TIMESTAMP"
					"TIMESTAMP_IA"))))
		       (props
			(and properties
			     (delq nil
				   (mapcar
				    (lambda (p)
				      (let ((v (org-entry-get
						(point) p inherit-property-p)))
					(and v (cons p v))))
				    properties)))))
		  (push (list level hdl tgs tsp time props) tbl)))))))
      (list file org-clock-file-total-minutes (nreverse tbl)))))

(provide 'org-clocktable)

;;; org-clocktable.el ends here
