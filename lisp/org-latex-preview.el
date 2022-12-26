;;; org-latex-preview.el --- LaTeX previews for Org -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Keywords: tex, extensions, tools

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
;;  LaTeX previews for Org

;;; Code:

(defgroup org-latex nil
  "Options for embedding LaTeX code into Org mode."
  :tag "Org LaTeX"
  :group 'org)

(defcustom org-format-latex-options
  '(:foreground default :background default :scale 1.0
    :html-foreground "Black" :html-background "Transparent"
    :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
  "Options for creating images from LaTeX fragments.
This is a property list with the following properties:
:foreground  the foreground color for images embedded in Emacs, e.g. \"Black\".
             `default' means use the foreground of the default face.
             `auto' means use the foreground from the text face.
:background  the background color, or \"Transparent\".
             `default' means use the background of the default face.
             `auto' means use the background from the text face.
:scale       a scaling factor for the size of the images, to get more pixels
:html-foreground, :html-background, :html-scale
             the same numbers for HTML export.
:matchers    a list indicating which matchers should be used to
             find LaTeX fragments.  Valid members of this list are:
             \"begin\" find environments
             \"$1\"    find single characters surrounded by $.$
             \"$\"     find math expressions surrounded by $...$
             \"$$\"    find math expressions surrounded by $$....$$
             \"\\(\"    find math expressions surrounded by \\(...\\)
             \"\\=\\[\"    find math expressions surrounded by \\=\\[...\\]"
  :group 'org-latex
  :type 'plist)

(defcustom org-format-latex-signal-error t
  "Non-nil means signal an error when image creation of LaTeX snippets fails.
When nil, just push out a message."
  :group 'org-latex
  :version "24.1"
  :type 'boolean)

(defcustom org-latex-to-mathml-jar-file nil
  "Value of\"%j\" in `org-latex-to-mathml-convert-command'.
Use this to specify additional executable file say a jar file.

When using MathToWeb as the converter, specify the full-path to
your mathtoweb.jar file."
  :group 'org-latex
  :version "24.1"
  :type '(choice
          (const :tag "None" nil)
          (file :tag "JAR file" :must-match t)))

(defcustom org-latex-to-mathml-convert-command nil
  "Command to convert LaTeX fragments to MathML.
Replace format-specifiers in the command as noted below and use
`shell-command' to convert LaTeX to MathML.
%j:     Executable file in fully expanded form as specified by
        `org-latex-to-mathml-jar-file'.
%I:     Input LaTeX file in fully expanded form.
%i:     Shell-escaped LaTeX fragment to be converted.
        It must not be used inside a quoted argument, the result of %i
        expansion inside a quoted argument is undefined.
%o:     Output MathML file.

This command is used by `org-create-math-formula'.

When using MathToWeb as the converter, set this option to
\"java -jar %j -unicode -force -df %o %I\".

When using LaTeXML set this option to
\"latexmlmath %i --presentationmathml=%o\"."
  :group 'org-latex
  :version "24.1"
  :type '(choice
          (const :tag "None" nil)
          (string :tag "\nShell command")))

(defcustom org-latex-to-html-convert-command nil
  "Shell command to convert LaTeX fragments to HTML.
This command is very open-ended: the output of the command will
directly replace the LaTeX fragment in the resulting HTML.
Replace format-specifiers in the command as noted below and use
`shell-command' to convert LaTeX to HTML.
%i:     The LaTeX fragment to be converted (shell-escaped).
        It must not be used inside a quoted argument, the result of %i
        expansion inside a quoted argument is undefined.

For example, this could be used with LaTeXML as
\"latexmlc literal:%i --profile=math --preload=siunitx.sty 2>/dev/null\"."
  :group 'org-latex
  :package-version '(Org . "9.4")
  :type '(choice
          (const :tag "None" nil)
          (string :tag "Shell command")))

(defcustom org-preview-latex-default-process 'dvipng
  "The default process to convert LaTeX fragments to image files.
All available processes and theirs documents can be found in
`org-preview-latex-process-alist', which see."
  :group 'org-latex
  :version "26.1"
  :package-version '(Org . "9.0")
  :type 'symbol)

(defcustom org-preview-latex-process-alist
  '((dvipng
     :programs ("latex" "dvipng")
     :description "dvi > png"
     :message "you need to install the programs: latex and dvipng."
     :image-input-type "dvi"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
     :image-converter ("dvipng --follow -D %D -T tight -o %B-%%09d.png %f")
     :transparent-image-converter
     ("dvipng --follow -D %D -T tight -bg Transparent -o %B-%%09d.png %f"))
    ;; :transparent-image-converter
    ;; ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
    ;; :image-converter ("dvipng -D %D -T tight -o %O %f")
    ;; :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
    (dvisvgm
     :programs ("latex" "dvisvgm")
     :description "dvi > svg"
     :message "you need to install the programs: latex and dvisvgm."
     :image-input-type "dvi"
     :image-output-type "svg"
     :image-size-adjust (1.4 . 1.2)
     :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
     :image-converter ("dvisvgm --page=1- --no-fonts --bbox=preview --scale=%S -o %B-%%9p.svg %f"))
    (imagemagick
     :programs ("latex" "convert")
     :description "pdf > png"
     :message "you need to install the programs: latex and imagemagick."
     :image-input-type "pdf"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
     :image-converter
     ("convert -density %D -trim -antialias %f -quality 100 %O")))
  "Definitions of external processes for LaTeX previewing.
Org mode can use some external commands to generate TeX snippet's images for
previewing or inserting into HTML files, e.g., \"dvipng\".  This variable tells
`org-create-formula-image' how to call them.

The value is an alist with the pattern (NAME . PROPERTIES).  NAME is a symbol.
PROPERTIES accepts the following attributes:

  :programs           list of strings, required programs.
  :description        string, describe the process.
  :message            string, message it when required programs cannot be found.
  :image-input-type   string, input file type of image converter (e.g., \"dvi\").
  :image-output-type  string, output file type of image converter (e.g., \"png\").
  :image-size-adjust  cons of numbers, the car element is used to adjust LaTeX
                      image size showed in buffer and the cdr element is for
                      HTML file.  This option is only useful for process
                      developers, users should use variable
                      `org-format-latex-options' instead.
  :post-clean         list of strings, files matched are to be cleaned up once
                      the image is generated.  When nil, the files with \".dvi\",
                      \".xdv\", \".pdf\", \".tex\", \".aux\", \".log\", \".svg\",
                      \".png\", \".jpg\", \".jpeg\" or \".out\" extension will
                      be cleaned up.
  :latex-header       list of strings, the LaTeX header of the snippet file.
                      When nil, the fallback value is used instead, which is
                      controlled by `org-format-latex-header',
                      `org-latex-default-packages-alist' and
                      `org-latex-packages-alist', which see.
  :latex-compiler     list of LaTeX commands, as strings.  Each of them is given
                      to the shell.  Place-holders \"%t\", \"%b\" and \"%o\" are
                      replaced with values defined below.
  :image-converter    list of image converter commands strings.  Each of them is
                      given to the shell and supports any of the following
                      place-holders defined below.

If set, :transparent-image-converter is used instead of :image-converter to
convert an image when the background color is nil or \"Transparent\".

Place-holders used by `:image-converter' and `:latex-compiler':

  %f    input file name
  %b    base name of input file
  %o    base directory of input file
  %O    absolute output file name

Place-holders only used by `:image-converter':

  %D    dpi, which is used to adjust image size by some processing commands.
  %S    the image size scale ratio, which is used to adjust image size by some
        processing commands."
  :group 'org-latex
  :package-version '(Org . "9.6")
  :type '(alist :tag "LaTeX to image backends"
          :value-type (plist)))

(defcustom org-preview-latex-image-directory "ltximg/"
  "Path to store latex preview images.
A relative path here creates many directories relative to the
processed Org files paths.  An absolute path puts all preview
images at the same place."
  :group 'org-latex
  :version "26.1"
  :package-version '(Org . "9.0")
  :type 'string)

(defun org-format-latex-mathml-available-p ()
  "Return t if `org-latex-to-mathml-convert-command' is usable."
  (save-match-data
    (when (and (boundp 'org-latex-to-mathml-convert-command)
               org-latex-to-mathml-convert-command)
      (let ((executable (car (split-string
                              org-latex-to-mathml-convert-command))))
        (when (executable-find executable)
          (if (string-match
               "%j" org-latex-to-mathml-convert-command)
              (file-readable-p org-latex-to-mathml-jar-file)
            t))))))

(defcustom org-format-latex-header "\\documentclass{article}
\\usepackage[usenames]{color}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}"
  "The document header used for processing LaTeX fragments.
It is imperative that this header make sure that no page number
appears on the page.  The package defined in the variables
`org-latex-default-packages-alist' and `org-latex-packages-alist'
will either replace the placeholder \"[PACKAGES]\" in this
header, or they will be appended."
  :group 'org-latex
  :type 'string)

(defcustom org-preview-use-precompilation t
  "Use LaTeX header precompilation when previewing fragments.
This causes a slight delay the first time `org-latex-pdf-process'
is called in a buffer, but subsequent calls will be faster.

This requires the LaTeX package \"mylatexformat\" to be installed."
  :group 'org-latex
  :package-version '(Org . "9.7")
  :type 'boolean)

(defconst org-latex-tentative-math-re
  "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}"
  "Regexp whith will match all instances of LaTeX math.
Note that this will also produce false postives, and
`org-element-context' should be used to verify that matches are
indeed LaTeX fragments/environments.")

(defun org--make-preview-overlay (beg end image &optional imagetype)
  "Build an overlay between BEG and END using IMAGE file.
Argument IMAGETYPE is the extension of the displayed image,
as a string.  It defaults to \"png\"."
  (let ((ov (make-overlay beg end))
        (imagetype (or (intern imagetype) 'png)))
    (overlay-put ov 'org-overlay-type 'org-latex-overlay)
    (overlay-put ov 'evaporate t)
    (overlay-put ov
                 'modification-hooks
                 (list (lambda (o _flag _beg _end &optional _l)
                         (delete-overlay o))))
    (overlay-put ov
                 'display
                 (list 'image :type imagetype :file image :ascent 'center))))

(defun org-clear-latex-preview (&optional beg end)
  "Remove all overlays with LaTeX fragment images in current buffer.
When optional arguments BEG and END are non-nil, remove all
overlays between them instead.  Return a non-nil value when some
overlays were removed, nil otherwise."
  (let ((overlays
         (cl-remove-if-not
          (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
          (overlays-in (or beg (point-min)) (or end (point-max))))))
    (mapc #'delete-overlay overlays)
    overlays))

(defun org--latex-preview-region (beg end)
  "Preview LaTeX fragments between BEG and END.
BEG and END are buffer positions."
  (org-latex-preview-fragments
   org-preview-latex-default-process
   beg end))

(defun org-latex-preview (&optional arg)
  "Toggle preview of the LaTeX fragment at point.

If the cursor is on a LaTeX fragment, create the image and
overlay it over the source code, if there is none.  Remove it
otherwise.  If there is no fragment at point, display images for
all fragments in the current section.  With an active region,
display images for all fragments in the region.

With a `\\[universal-argument]' prefix argument ARG, clear images \
for all fragments
in the current section.

With a `\\[universal-argument] \\[universal-argument]' prefix \
argument ARG, display image for all
fragments in the buffer.

With a `\\[universal-argument] \\[universal-argument] \
\\[universal-argument]' prefix argument ARG, clear image for all
fragments in the buffer."
  (interactive "P")
  (cond
   ((not (display-graphic-p)) nil)
   ;; Clear whole buffer.
   ((equal arg '(64))
    (org-clear-latex-preview (point-min) (point-max))
    (message "LaTeX previews removed from buffer"))
   ;; Preview whole buffer.
   ((equal arg '(16))
    (message "Creating LaTeX previews in buffer...")
    (org--latex-preview-region (point-min) (point-max))
    (message "Creating LaTeX previews in buffer... done."))
   ;; Clear current section.
   ((equal arg '(4))
    (org-clear-latex-preview
     (if (use-region-p)
         (region-beginning)
       (if (org-before-first-heading-p) (point-min)
         (save-excursion
           (org-with-limited-levels (org-back-to-heading t) (point)))))
     (if (use-region-p)
         (region-end)
       (org-with-limited-levels (org-entry-end-position)))))
   ((use-region-p)
    (message "Creating LaTeX previews in region...")
    (org--latex-preview-region (region-beginning) (region-end))
    (message "Creating LaTeX previews in region... done."))
   ;; Toggle preview on LaTeX code at point.
   ((let ((datum (org-element-context)))
      (and (memq (org-element-type datum) '(latex-environment latex-fragment))
           (let ((beg (org-element-property :begin datum))
                 (end (org-element-property :end datum)))
             (if (org-clear-latex-preview beg end)
                 (message "LaTeX preview removed")
               (message "Creating LaTeX preview...")
               (org--latex-preview-region beg end)
               (message "Creating LaTeX preview... done."))
             t))))
   ;; Preview current section.
   (t
    (let ((beg (if (org-before-first-heading-p) (point-min)
                 (save-excursion
                   (org-with-limited-levels (org-back-to-heading t) (point)))))
          (end (org-with-limited-levels (org-entry-end-position))))
      (message "Creating LaTeX previews in section...")
      (org--latex-preview-region beg end)
      (message "Creating LaTeX previews in section... done.")))))

(defun org-latex-collect-fragments (&optional beg end)
  "Collect all LaTeX maths fragments/environments between BEG and END."
  (let (fragments)
    (save-excursion
      (goto-char (or beg (point-min)))
      (while (re-search-forward org-latex-tentative-math-re end t)
        (let ((obj (org-element-context)))
          (when (memq (org-element-type obj)
                      '(latex-fragment latex-environment))
            (push obj fragments)))))
    (nreverse fragments)))

(defun org-latex-replace-fragments (prefix processing-type &optional dir msg)
  "Replace all LaTeX fragments in the buffer with export appropriate forms.
The way this is done is set by PROCESSING-TYPE, which can be either:
- verabtim, in which case nothing is done
- mathjax, in which case the TeX-style delimeters are replaced with
  LaTeX-style delimeters.
- html, in which case the math fragment is replaced by the result of
  `org-format-latex-as-html'.
- mathml, in which case the math fragment is replace by the result of
  `org-format-latex-as-mathml'.
- an entry in `org-preview-latex-process-alist', in which case the
  math fragment is replaced with `org-create-latex-export'.

Generated image files are placed in DIR with the prefix PREFIX. Note
that PREFIX may itself contain a directory path component.

When generating output files, MSG will be `message'd if given."
  (let* ((cnt 0))
    (save-excursion
      (dolist (element (org-latex-collect-fragments))
        (let ((block-type (eq (org-element-type element)
                              'latex-environment))
              (value (org-element-property :value element))
              (beg (org-element-property :begin element))
              (end (save-excursion
                     (goto-char (org-element-property :end element))
                     (skip-chars-backward " \r\t\n")
                     (point))))
          (cond
           ((eq processing-type 'verbatim)) ; Do nothing.
           ((eq processing-type 'mathjax)
            ;; Prepare for MathJax processing.
            (if (not (string-match "\\`\\$\\$?" value))
                (goto-char end)
              (delete-region beg end)
              (if (string= (match-string 0 value) "$$")
                  (insert "\\[" (substring value 2 -2) "\\]")
                (insert "\\(" (substring value 1 -1) "\\)"))))
           ((eq processing-type 'html)
            (goto-char beg)
            (delete-region beg end)
            (insert (org-format-latex-as-html value)))
           ((eq processing-type 'mathml)
            ;; Process to MathML.
            (unless (org-format-latex-mathml-available-p)
              (user-error "LaTeX to MathML converter not configured"))
            (cl-incf cnt)
            (when msg (message msg cnt))
            (goto-char beg)
            (delete-region beg end)
            (insert (org-format-latex-as-mathml
                     value block-type prefix dir)))
           ((assq processing-type org-preview-latex-process-alist)
            (let ((image-dir (expand-file-name prefix dir)))
              (unless (file-exists-p image-dir)
                (make-directory image-dir t)))
            (org-create-latex-export
             processing-type element prefix dir block-type))
           (t (error "Unknown conversion process %s for LaTeX fragments"
                     processing-type))))))))

(defun org-latex-preview-fragments (processing-type &optional beg end)
  "Produce image overlays of LaTeX math fragments between BEG and END.

The LaTeX fragments are processed using PROCESSING-TYPE, a key of
`org-latex-preview-process-alist'.

If `point' is currently on an LaTeX overlay, then no overlays
will be generated.  Since in practice `org-clear-latex-preview'
should have been called immediately prior to this function, this
situation should not occur in practice and mainly acts as
protection against placing doubled up overlays."
  (when (fboundp 'clear-image-cache)
    (clear-image-cache))
  ;; Optimize overlay creation: (info "(elisp) Managing Overlays").
  (when (memq processing-type '(dvipng dvisvgm imagemagick))
    (overlay-recenter (or end (point-max))))
  (unless (eq (get-char-property (point) 'org-overlay-type)
              'org-latex-overlay)
    (let ((image-dir (expand-file-name
                      (concat org-preview-latex-image-directory "org-ltximg")
                      dir)))
      (unless (file-exists-p image-dir)
        (make-directory image-dir t)))
    (if (assq processing-type org-preview-latex-process-alist)
        (org-create-latex-preview
         processing-type (org-latex-collect-fragments beg end))
      (error "Unknown conversion process %s for previewing LaTeX fragments"
             processing-type))))

(defun org-format-latex
    (prefix &optional beg end dir overlays msg forbuffer processing-type)
  "Replace LaTeX fragments with links to an image.

The function takes care of creating the replacement image.

Only consider fragments between BEG and END when those are
provided.

When optional argument OVERLAYS is non-nil, display the image on
top of the fragment instead of replacing it.

PROCESSING-TYPE is the conversion method to use, as a symbol.

Some of the options can be changed using the variable
`org-format-latex-options', which see."
  (if (and overlays forbuffer)
      (org-latex-preview-fragments processing-type beg end)
    (org-latex-replace-fragments prefix processing-type dir msg)))

(defun org-create-latex-preview (processing-type elements)
  "Preview LaTeX math fragments ELEMENTS using PROCESSING-TYPE."
  (let* ((processing-info
          (cdr (assq processing-type org-preview-latex-process-alist)))
         (imagetype (or (plist-get processing-info :image-output-type) "png"))
         document-strings
         locations
         movefiles existfiles)
    (save-excursion
      (dolist (element elements)
        (let* ((beg (org-element-property :begin element))
               (end (save-excursion
                      (goto-char (org-element-property :end element))
                      (skip-chars-backward " \r\t\n")
                      (point)))
               (value (org-element-property :value element))
               (face (or (and (> beg 1)
                              (get-text-property (1- beg) 'face))
                         'default))
               (fg (pcase (plist-get org-format-latex-options :foreground)
                     ('auto (face-attribute face :foreground nil 'default))
                     ('default (face-attribute 'default :foreground nil))
                     (color color)))
               (bg (pcase (plist-get org-format-latex-options :background)
                     ('auto (face-attribute face :background nil 'default))
                     ('default (face-attribute 'default :background nil))
                     (color color)))
               (hash (sha1 (prin1-to-string
                            (list org-format-latex-header
                                  org-latex-default-packages-alist
                                  org-latex-packages-alist
                                  org-format-latex-options
                                  value fg bg))))
               (movefile (format "%s_%s.%s" absprefix hash imagetype))
               (options (org-combine-plists
                         org-format-latex-options
                         (list :foreground fg :background bg))))
          (if (file-exists-p movefile)
              (org-place-latex-image beg end movefile imagetype)
            (push (org-create-preview-string value options)
                  document-strings)
            (push (cons beg end) locations)
            (push movefile movefiles)))))
    (when movefiles
      (org-create-formula-image-async
       processing-type
       (mapconcat #'identity (nreverse document-strings) "\n")
       (nreverse locations)
       (nreverse movefiles)))))

(defun org-create-formula-image-async (processing-type string locations movefiles)
  "Preview math fragments in STRING asynchronously with method PROCESSING-TYPE.

LOCATIONS are buffer locations denoting the beginning and end of
each snippet in STRING. Each entry is a cons cell.

The previews are copied (in lexicographic order) to the files in
MOVEFILES."
  (interactive "P")
  (let* ((processing-type (or processing-type
                              org-preview-latex-default-process))
         (processing-info
          (cdr (assq processing-type org-preview-latex-process-alist)))
         (programs (plist-get processing-info :programs))
         (error-message (or (plist-get processing-info :message) ""))
         (image-input-type (plist-get processing-info :image-input-type))
         (image-output-type (plist-get processing-info :image-output-type))
         (post-clean (or (plist-get processing-info :post-clean)
                         '(".dvi" ".xdv" ".pdf" ".tex" ".aux" ".log"
                           ".svg" ".png" ".jpg" ".jpeg" ".out")))
         (latex-header
          (let ((header (concat
                         (or (plist-get processing-info :latex-header)
                             (org-latex-make-preamble
                              (org-combine-plists
                               (org-export-get-environment (org-export-get-backend 'latex))
                               '(:time-stamp-file nil))
                              org-format-latex-header 'snippet))
                         "\n\\RequirePackage"
                         "[active,tightpage,auctex,displaymath,graphics,textmath,floats]"
                         "{preview}\n")))
            (if org-preview-use-precompilation
                (concat "%&" (org-preview-precompile header))
              header)))
         (latex-compiler (plist-get processing-info :latex-compiler))
         (texfilebase
          (make-temp-name
           (expand-file-name "orgtex" temporary-file-directory)))
         (texfile (concat texfilebase ".tex"))
         (image-size-adjust (or (plist-get processing-info :image-size-adjust)
                                '(1.0 . 1.0)))
         (scale (* (car image-size-adjust)
                   (or (plist-get org-format-latex-options :scale) 1.0)))
         (dpi (* scale (if (display-graphic-p) (org--get-display-dpi) 140.0)))
         (image-converter
          (or (and (string= (plist-get org-format-latex-options :background)
                            "Transparent")
                   (plist-get processing-info :transparent-image-converter))
              (plist-get processing-info :image-converter)))
         (resize-mini-windows nil))
    
    (dolist (program programs)
      (org-check-external-command program error-message))
    
    (if (string-suffix-p string "\n")
        (aset string (1- (length string)) ?%)
      (setq string (concat string "%")))

    (with-temp-file texfile
      (insert latex-header)
      (insert "\n\\begin{document}\n"
              string
              "\n\\end{document}\n"))

    (let* ((default-directory temporary-file-directory)
           (tex-process)
           (image-process)
           (basename (file-name-base texfilebase))
           (out-dir (or (file-name-directory texfile) default-directory))
           (spec `((?o . ,(shell-quote-argument out-dir))
                   (?b . ,(shell-quote-argument basename))
                   (?B . ,(shell-quote-argument texfilebase))))
           (spec-tex `((?f . ,(shell-quote-argument texfile))))
           (spec-img `((?D . ,(shell-quote-argument (format "%s" dpi)))
                       (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))
                       (?f . ,(shell-quote-argument
                               (expand-file-name
                                (concat basename "." image-input-type) out-dir))))))
      (setq tex-process
            (make-process :name (format "Org-Preview-%s" (file-name-base texfile))
                          :buffer (format "*Org Preview LaTeX Output*")
                          :command (split-string-shell-command
                                    (format-spec (car latex-compiler)
                                                 (append spec spec-tex)))
                          :sentinel (lambda (proc signal)
                                      (unless (process-live-p proc)
                                        (dolist (e (delete (concat "." image-input-type) post-clean))
                                          (when (file-exists-p (concat texfilebase e))
                                            (delete-file (concat texfilebase e))))))))
      (when (equal processing-type 'dvisvgm)
        (let (inhibit-quit)
          (while (process-live-p tex-process)
            (accept-process-output tex-process))))
      (setq image-process
            (make-process
             :name (format "Org-Convert-%s-%s" (file-name-base texfile)
                           (symbol-name processing-type))
             :buffer (format "*Org Convert %s %s*"
                             (file-name-base texfile)
                             (symbol-name processing-type))
             :command (split-string-shell-command
                       (format-spec (car image-converter)
                                    (append spec spec-img)))
             :sentinel
             (lambda (proc signal)
               (when (string= signal "finished\n")
                 (let ((images (file-expand-wildcards
                                (concat texfilebase "*." image-output-type)
                                'full)))
                   (save-excursion
                     (cl-loop
                      for (block-beg . block-end) in locations
                      for image-file in images
                      for movefile in movefiles
                      do (copy-file image-file movefile 'replace)
                      do (org-place-latex-image block-beg block-end movefile image-output-type)))))
               (unless (process-live-p proc)
                 (mapc #'delete-file
                       (file-expand-wildcards
                        (concat texfilebase "*." image-output-type) 'full))
                 (delete-file (concat texfilebase "." image-input-type)))))))))

(defun org-preview-precompile (header)
  "Precompile/dump LaTeX HEADER (preamble) text.

This dump is named using its sha1 hash and placed in
`temporary-file-directory', and the name is returned.  If a dump
file with this name already exists, simply return the name.

This is intended to speed up Org's LaTeX preview generation
process."
  ;; Note: the dump is created in the directory that LaTeX runs. TeX
  ;; files cannot include dumps from other directories, so the dump
  ;; must be created in (or moved to) the location of the TeX file.
  (let* ((default-directory temporary-file-directory)
         (header-hash (substring (sha1 header) 0 12))
         (header-base-file
          (expand-file-name header-hash temporary-file-directory))
         (dump-file (concat header-base-file ".fmt"))
         (header-file (concat header-base-file ".tex")))
    (if (file-exists-p dump-file)
        (file-name-base header-base-file)
      (with-temp-file header-file
        (insert header "\n\\endofdump\n"))
      (file-name-base
       (org-compile-file
        header-file
        '("latex -ini -jobname=%b \"&latex\" mylatexformat.ltx %f")
        "fmt")))))

(defun org-create-preview-string (value options &optional export-p)
  "Generate LaTeX string suitable for use with preview.sty.

VALUE is the math fragment text to be previewed.

OPTIONS is the plist `org-format-latex-options' with customized
color information for this run.

EXPORT-P, if true, uses colors required for HTML processing."
  (let* ((fg (pcase (plist-get options (if export-p :html-foreground :foreground))
               ('default (org-latex-color-format (org-latex-color :foreground)))
               ((pred null) (org-latex-color-format "Black"))
               (color (org-latex-color-format color))))
         (bg (pcase (plist-get options (if export-p :html-background :background))
               ('default (org-latex-color :background))
               ("Transparent" nil)
               (bg (org-latex-color-format bg)))))
    (concat "\n\n\\definecolor{fg}{rgb}{" fg "}%\n"
            (and bg (format "\\definecolor{bg}{rgb}{%s}%%\n" bg))
            "\\begin{preview}"
            (and bg "\\pagecolor{bg}%\n")
            "{\\color{fg}\n"
            value
            "}\n\\end{preview}\n\n")))

(defun org-create-latex-export (processing-type element prefix dir &optional block-type)
  "Create a export of the LaTeX math fragment ELEMENT using PROCESSING-TYPE.

Generated image files are placed in DIR with the prefix PREFIX. Note
that PREFIX may itself contain a directory path component.

BLOCK-TYPE determines whether the result is placed inline or as a paragraph."
  (let* ((processing-info
          (cdr (assq processing-type org-preview-latex-process-alist)))
         (beg (org-element-property :begin element))
         (end (save-excursion
                (goto-char (org-element-property :end element))
                (skip-chars-backward " \r\t\n")
                (point)))
         (value (org-element-property :value element))
         (fg (plist-get org-format-latex-options :foreground))
         (bg (plist-get org-format-latex-options :background))
         (hash (sha1 (prin1-to-string
                      (list org-format-latex-header
                            org-latex-default-packages-alist
                            org-latex-packages-alist
                            org-format-latex-options
                            'export value fg bg))))
         (imagetype (or (plist-get processing-info :image-output-type) "png"))
         (absprefix (expand-file-name prefix dir))
         (linkfile (format "%s_%s.%s" prefix hash imagetype))
         (movefile (format "%s_%s.%s" absprefix hash imagetype))
         (sep (and block-type "\n\n"))
         (link (concat sep "[[file:" linkfile "]]" sep))
         (options (org-combine-plists
                   org-format-latex-options
                   (list :foreground fg :background bg))))
    (unless (file-exists-p movefile)
      (org-create-formula-image
       value movefile options nil processing-type))
    (org-place-latex-image-link link block-type beg end value)))

(defun org-place-latex-image (beg end movefile imagetype)
  "Place an overlay from BEG to END showing MOVEFILE.
The overlay will be above BEG if OVERLAYS is non-nil."
  (dolist (o (overlays-in beg end))
    (when (eq (overlay-get o 'org-overlay-type)
              'org-latex-overlay)
      (delete-overlay o)))
  (org--make-preview-overlay beg end movefile imagetype)
  (goto-char end))

(defun org-place-latex-image-link (link block-type beg end value)
  "Place then link LINK at BEG END."
  (delete-region beg end)
  (insert
   (org-add-props link
       (list 'org-latex-src
             (replace-regexp-in-string "\"" "" value)
             'org-latex-src-embed-type
             (if block-type 'paragraph 'character)))))

(defun org-create-math-formula (latex-frag &optional mathml-file)
  "Convert LATEX-FRAG to MathML and store it in MATHML-FILE.
Use `org-latex-to-mathml-convert-command'.  If the conversion is
successful, return the portion between \"<math...> </math>\"
elements otherwise return nil.  When MATHML-FILE is specified,
write the results in to that file.  When invoked as an
interactive command, prompt for LATEX-FRAG, with initial value
set to the current active region and echo the results for user
inspection."
  (interactive (list (let ((frag (when (org-region-active-p)
                                   (buffer-substring-no-properties
                                    (region-beginning) (region-end)))))
                       (read-string "LaTeX Fragment: " frag nil frag))))
  (unless latex-frag (user-error "Invalid LaTeX fragment"))
  (let* ((tmp-in-file
          (let ((file (file-relative-name
                       (make-temp-name (expand-file-name "ltxmathml-in")))))
            (write-region latex-frag nil file)
            file))
         (tmp-out-file (file-relative-name
                        (make-temp-name (expand-file-name  "ltxmathml-out"))))
         (cmd (format-spec
               org-latex-to-mathml-convert-command
               `((?j . ,(and org-latex-to-mathml-jar-file
                             (shell-quote-argument
                              (expand-file-name
                               org-latex-to-mathml-jar-file))))
                 (?I . ,(shell-quote-argument tmp-in-file))
                 (?i . ,latex-frag)
                 (?o . ,(shell-quote-argument tmp-out-file)))))
         mathml shell-command-output)
    (when (called-interactively-p 'any)
      (unless (org-format-latex-mathml-available-p)
        (user-error "LaTeX to MathML converter not configured")))
    (message "Running %s" cmd)
    (setq shell-command-output (shell-command-to-string cmd))
    (setq mathml
          (when (file-readable-p tmp-out-file)
            (with-current-buffer (find-file-noselect tmp-out-file t)
              (goto-char (point-min))
              (when (re-search-forward
                     (format "<math[^>]*?%s[^>]*?>\\(.\\|\n\\)*</math>"
                             (regexp-quote
                              "xmlns=\"http://www.w3.org/1998/Math/MathML\""))
                     nil t)
                (prog1 (match-string 0) (kill-buffer))))))
    (cond
     (mathml
      (setq mathml
            (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" mathml))
      (when mathml-file
        (write-region mathml nil mathml-file))
      (when (called-interactively-p 'any)
        (message mathml)))
     ((warn "LaTeX to MathML conversion failed")
      (message shell-command-output)))
    (delete-file tmp-in-file)
    (when (file-exists-p tmp-out-file)
      (delete-file tmp-out-file))
    mathml))

(defun org-format-latex-as-mathml (latex-frag latex-frag-type
                                              prefix &optional dir)
  "Use `org-create-math-formula' but check local cache first."
  (let* ((absprefix (expand-file-name prefix dir))
         (print-length nil) (print-level nil)
         (formula-id (concat
                      "formula-"
                      (sha1
                       (prin1-to-string
                        (list latex-frag
                              org-latex-to-mathml-convert-command)))))
         (formula-cache (format "%s-%s.mathml" absprefix formula-id))
         (formula-cache-dir (file-name-directory formula-cache)))

    (unless (file-directory-p formula-cache-dir)
      (make-directory formula-cache-dir t))

    (unless (file-exists-p formula-cache)
      (org-create-math-formula latex-frag formula-cache))

    (if (file-exists-p formula-cache)
        ;; Successful conversion.  Return the link to MathML file.
        (org-add-props
            (format  "[[file:%s]]" (file-relative-name formula-cache dir))
            (list 'org-latex-src (replace-regexp-in-string "\"" "" latex-frag)
                  'org-latex-src-embed-type (if latex-frag-type
                                                'paragraph 'character)))
      ;; Failed conversion.  Return the LaTeX fragment verbatim
      latex-frag)))

(defun org-format-latex-as-html (latex-fragment)
  "Convert LATEX-FRAGMENT to HTML.
This uses  `org-latex-to-html-convert-command', which see."
  (let ((cmd (format-spec org-latex-to-html-convert-command
                          `((?i . ,latex-fragment)))))
    (message "Running %s" cmd)
    (shell-command-to-string cmd)))

(defun org--get-display-dpi ()
  "Get the DPI of the display.
The function assumes that the display has the same pixel width in
the horizontal and vertical directions."
  (if (display-graphic-p)
      (round (/ (display-pixel-height)
                (/ (display-mm-height) 25.4)))
    (error "Attempt to calculate the dpi of a non-graphic display")))

(defun org-create-formula-image
    (string tofile options buffer &optional processing-type)
  "Create an image from LaTeX source using external processes.

The LaTeX STRING is saved to a temporary LaTeX file, then
converted to an image file by process PROCESSING-TYPE defined in
`org-preview-latex-process-alist'.  A nil value defaults to
`org-preview-latex-default-process'.

The generated image file is eventually moved to TOFILE.

The OPTIONS argument controls the size, foreground color and
background color of the generated image.

When BUFFER non-nil, this function is used for LaTeX previewing.
Otherwise, it is used to deal with LaTeX snippets showed in
a HTML file."
  (let* ((processing-type (or processing-type
                              org-preview-latex-default-process))
         (processing-info
          (cdr (assq processing-type org-preview-latex-process-alist)))
         (programs (plist-get processing-info :programs))
         (error-message (or (plist-get processing-info :message) ""))
         (image-input-type (plist-get processing-info :image-input-type))
         (image-output-type (plist-get processing-info :image-output-type))
         (post-clean (or (plist-get processing-info :post-clean)
                         '(".dvi" ".xdv" ".pdf" ".tex" ".aux" ".log"
                           ".svg" ".png" ".jpg" ".jpeg" ".out")))
         (latex-header
          (or (plist-get processing-info :latex-header)
              (org-latex-make-preamble
               (org-export-get-environment (org-export-get-backend 'latex))
               org-format-latex-header
               'snippet)))
         (latex-compiler (plist-get processing-info :latex-compiler))
         (tmpdir temporary-file-directory)
         (texfilebase (make-temp-name
                       (expand-file-name "orgtex" tmpdir)))
         (texfile (concat texfilebase ".tex"))
         (image-size-adjust (or (plist-get processing-info :image-size-adjust)
                                '(1.0 . 1.0)))
         (scale (* (if buffer (car image-size-adjust) (cdr image-size-adjust))
                   (or (plist-get options (if buffer :scale :html-scale)) 1.0)))
         (dpi (* scale (if (and buffer (display-graphic-p)) (org--get-display-dpi) 140.0)))
         (fg (or (plist-get options (if buffer :foreground :html-foreground))
                 "Black"))
         (bg (or (plist-get options (if buffer :background :html-background))
                 "Transparent"))
         (image-converter
          (or (and (string= bg "Transparent")
                   (plist-get processing-info :transparent-image-converter))
              (plist-get processing-info :image-converter)))
         (log-buf (get-buffer-create "*Org Preview LaTeX Output*"))
         (resize-mini-windows nil)) ;Fix Emacs flicker when creating image.
    (dolist (program programs)
      (org-check-external-command program error-message))
    (if (eq fg 'default)
        (setq fg (org-latex-color :foreground))
      (setq fg (org-latex-color-format fg)))
    (setq bg (cond
              ((eq bg 'default) (org-latex-color :background))
              ((string= bg "Transparent") nil)
              (t (org-latex-color-format bg))))
    ;; Remove TeX \par at end of snippet to avoid trailing space.
    (if (string-suffix-p string "\n")
        (aset string (1- (length string)) ?%)
      (setq string (concat string "%")))
    (with-temp-file texfile
      (insert latex-header)
      (insert "\n\\begin{document}\n"
              "\\definecolor{fg}{rgb}{" fg "}%\n"
              (if bg
                  (concat "\\definecolor{bg}{rgb}{" bg "}%\n"
                          "\n\\pagecolor{bg}%\n")
                "")
              "\n{\\color{fg}\n"
              string
              "\n}\n"
              "\n\\end{document}\n"))
    (let* ((err-msg (format "Please adjust `%s' part of \
`org-preview-latex-process-alist'."
                            processing-type))
           (image-input-file
            (org-compile-file
             texfile latex-compiler image-input-type err-msg log-buf))
           (image-output-file
            (org-compile-file
             image-input-file image-converter image-output-type err-msg log-buf
             `((?D . ,(shell-quote-argument (format "%s" dpi)))
               (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))))))
      (copy-file image-output-file tofile 'replace)
      (dolist (e post-clean)
        (when (file-exists-p (concat texfilebase e))
          (delete-file (concat texfilebase e))))
      image-output-file)))

(defun org-dvipng-color (attr)
  "Return a RGB color specification for dvipng."
  (org-dvipng-color-format (face-attribute 'default attr nil)))

(defun org-dvipng-color-format (color-name)
  "Convert COLOR-NAME to a RGB color value for dvipng."
  (apply #'format "rgb %s %s %s"
         (mapcar 'org-normalize-color
                 (color-values color-name))))

(defun org-latex-color (attr)
  "Return a RGB color for the LaTeX color package."
  (org-latex-color-format (face-attribute 'default attr nil)))

(defun org-latex-color-format (color-name)
  "Convert COLOR-NAME to a RGB color value."
  (apply #'format "%s,%s,%s"
         (mapcar 'org-normalize-color
                 (color-values color-name))))

(defun org-normalize-color (value)
  "Return string to be used as color value for an RGB component."
  (format "%g" (/ value 65535.0)))

(provide 'org-latex-preview)
;;; org-latex-preview.el ends here
