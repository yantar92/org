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
    :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")
    :zoom 1.0)
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
             \"\\=\\[\"    find math expressions surrounded by \\=\\[...\\]
:zoom        when the image has associated font-relative height information,
             the display size is scaled by this factor."
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
     :latex-precompiler ("latex -ini -jobname=%b \"&latex\" mylatexformat.ltx %f")
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
     :latex-precompiler ("latex -ini -jobname=%b \"&latex\" mylatexformat.ltx %f")
     :image-converter ("dvisvgm --page=1- --no-fonts --bbox=preview --scale=%S -o %B-%%9p.svg %f"))
    (imagemagick
     :programs ("latex" "convert")
     :description "pdf > png"
     :message "you need to install the programs: latex and imagemagick."
     :image-input-type "pdf"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
     :latex-precompiler ("pdftex -ini -jobname=%b \"&pdflatex\" mylatexformat.ltx %f")
     :image-converter
     ("convert -density %D -trim -antialias %f -quality 100 %B-%%09d.png")))
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

(defun org--make-preview-overlay (beg end &optional path-info)
  "Build an overlay between BEG and END.

If IMAGE file is specified, display it. Argument IMAGETYPE is the
extension of the displayed image, as a string.  It defaults to
\"png\"."
  (let* ((ov (make-overlay beg end))
         (zoom (or (plist-get org-format-latex-options :zoom) 1.0))
         (height (plist-get (cdr path-info) :height))
         (depth (plist-get (cdr path-info) :depth))
         (image-display
          (and path-info
               (list 'image
                     :type (plist-get (cdr path-info) :image-type)
                     :file (car path-info)
                     :height (and height (cons (* height zoom) 'em))
                     :ascent (if (and depth height)
                                 (ceiling (* 100 (- 1.0 (/ depth height))))
                               'center)))))
    (overlay-put ov 'org-overlay-type 'org-latex-overlay)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'modification-hooks
                 (list (lambda (o after-p _beg _end &optional _l)
                         (when after-p
                           (overlay-put o 'preview-state 'modified)
                           (overlay-put o 'display nil) ))))
    (when path-info
      (overlay-put ov 'display image-display))
    (if (eq (plist-get (cdr image-display) :type) 'svg)
        (let ((face (if (plist-get (cdr path-info) :errors)
                        'error
                      (or (and (> beg 1)
                               (get-text-property (1- beg) 'face))
                          'default))))
          (overlay-put ov 'face face))
      (overlay-put ov 'face nil))))

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

Generated image files are placed in DIR with the prefix PREFIX.
Note that PREFIX may itself contain a directory path component.

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
         fragment-info
         locations keys)
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
                            (list processing-type
                                  org-format-latex-header
                                  org-latex-default-packages-alist
                                  org-latex-packages-alist
                                  org-format-latex-options
                                  value
                                  (if (equal imagetype "svg")
                                      'svg fg)
                                  bg))))
               (options (org-combine-plists
                         org-format-latex-options
                         (list :foreground fg :background bg))))
          (if-let ((path-info (org-latex-preview--get-cached hash)))
              (org-place-latex-image beg end path-info)
            (push (org-preview-latex--tex-styled value options)
                  document-strings)
            (push (list :buffer-location (cons beg end)
                        :key hash)
                  fragment-info)
            (push (cons beg end) locations)
            (push hash keys)))))
    (when locations
      (org-create-formula-image-async
       processing-type
       (nreverse document-strings)
       (nreverse fragment-info)))))

(defun org-create-formula-image-async (processing-type preview-strings fragment-info)
  "Preview PREVIEW-STRINGS asynchronously with method PROCESSING-TYPE.

FRAGMENT-INFO is a list of plists, where the Nth plist gives
information on the Nth fragment of PREVIEW-STRINGS.  Each
FRAGMENT-INFO plist should have the following structure:
  (:buffer-location (begin-pos . end-pos) :key fragment-hash)

It is worth noting the FRAGMENT-INFO plists will be modified
during processing to hold more information on the fragments."
  (let* ((processing-type
          (or processing-type org-preview-latex-default-process))
         (processing-info
          (alist-get processing-type org-preview-latex-process-alist))
         (programs (plist-get processing-info :programs))
         (error-message (or (plist-get processing-info :message) "")))
    (dolist (program programs)
      (org-check-external-command program error-message))
    (let* ((extended-info
            (append processing-info
                    (list :fragments fragment-info
                          :org-buffer (current-buffer)
                          :texfile (org-preview-latex--create-tex-file
                                    processing-info preview-strings))))
           (tex-compile-async
            (org-latex-preview--tex-compile-async extended-info))
           (img-extract-async
            (org-latex-preview--image-extract-async extended-info)))
      (plist-put (cddr img-extract-async) :success
                 (list #'org-latex-preview--cleanup-callback))
      (pcase processing-type
        ('dvisvgm
         (plist-put (cddr img-extract-async) :filter
                    #'org-latex-preview--dvisvgm-filter))
        (_
         (plist-put (cddr img-extract-async) :success
                    (list #'org-latex-preview--generic-callback))))
      (if (and (eq processing-type 'dvipng)
               (member "--follow" (cadr img-extract-async)))
          (org-async-call img-extract-async)
        (plist-put (cddr tex-compile-async) :success img-extract-async)
        (plist-put (cddr tex-compile-async) :failure img-extract-async))
      (org-async-call tex-compile-async))))

(defun org-preview-latex--create-tex-file (processing-info preview-strings)
  "Create a LaTeX file based on PROCESSING-INFO and PREVIEW-STRINGS.

More specifically, a preamble will be generated based on
PROCESSING-INFO.  Then, if `org-preview-use-precompilation' is
non-nil, a precompiled format file will be generated if needed
and used.  Otherwise the preamble is used normally.

Within the body of the created LaTeX file, each of
PREVIEW-STRINGS will be placed in order, wrapped within a
\"preview\" environment.

The path of the created LaTeX file is returned."
  (let ((tex-temp-name
         (expand-file-name (concat (make-temp-name "org-tex-") ".tex")
                           temporary-file-directory))
        (header
         (concat
          (or (plist-get processing-info :latex-header)
              (org-latex-make-preamble
               (org-combine-plists
                (org-export-get-environment (org-export-get-backend 'latex))
                '(:time-stamp-file nil))
               org-format-latex-header 'snippet))
          "\n\\usepackage[active,tightpage,auctex]{preview}\n")))
    (with-temp-file tex-temp-name
      (insert (if org-preview-use-precompilation
                  (concat "%&"
                          (org-preview-precompile
                           processing-info header))
                header))
      (insert "\n\\begin{document}\n")
      (dolist (str preview-strings)
        (insert
         "\n\\begin{preview}\n"
         str
         "\n\\end{preview}\n"))
      (insert "\n\\end{document}\n"))
    tex-temp-name))

(defun org-latex-preview--tex-compile-async (extended-info)
  "Create an `org-async-call' spec to compile the texfile in EXTENDED-INFO."
  (let* ((tex-process-buffer
          (with-current-buffer
              (get-buffer-create "*Org Preview LaTeX Output*")
            (erase-buffer)
            (current-buffer)))
         (tex-compile-command
          (pcase (plist-get extended-info :latex-compiler)
            ((and (pred stringp) cmd) cmd)
            ((and (pred consp) cmds)
             (when (> (length cmds) 1)
               (warn "Preview :latex-compiler must now be a single command.  %S will be ignored."
                     (cdr cmds)))
             (car cmds))))
         (texfile (plist-get extended-info :texfile))
         (tex-command-spec
          `((?o . ,(shell-quote-argument (file-name-directory texfile)))
            (?b . ,(shell-quote-argument (file-name-base texfile)))
            (?B . ,(shell-quote-argument (file-name-sans-extension texfile)))
            (?f . ,(shell-quote-argument texfile))))
         (tex-formatted-command
          (split-string-shell-command
           (format-spec tex-compile-command tex-command-spec))))
    (list 'org-async-task
          tex-formatted-command
          :buffer tex-process-buffer
          :dir temporary-file-directory
          :info extended-info
          :filter #'org-latex-preview--latex-preview-filter
          :failure "LaTeX compilation for preview failed! (error code %d)")))

(defun org-latex-preview--image-extract-async (extended-info)
  "Create an `org-async-call' spec to extract images according to EXTENDED-INFO."
  (let* ((img-process-buffer
          (with-current-buffer
              (get-buffer-create "*Org Preview Convert Output*")
            (erase-buffer)
            (current-buffer)))
         (img-extract-command
          (pcase
              (or (and (string= (plist-get org-format-latex-options :background)
                                "Transparent")
                       (plist-get extended-info :transparent-image-converter))
                  (plist-get extended-info :image-converter))
            ((and (pred stringp) cmd) cmd)
            ((and (pred consp) cmds)
             (when (> (length cmds) 1)
               (warn "Preview converter must now be a single command.  %S will be ignored."
                     (cdr cmds)))
             (car cmds))))
         (image-size-adjust (or (plist-get extended-info :image-size-adjust)
                                '(1.0 . 1.0)))
         (scale (* (car image-size-adjust)
                   (or (plist-get org-format-latex-options :scale) 1.0)))
         (dpi (* scale (if (display-graphic-p) (org--get-display-dpi) 140.0)))
         (texfile (plist-get extended-info :texfile))
         (img-command-spec
          `((?o . ,(shell-quote-argument (file-name-directory texfile)))
            (?b . ,(shell-quote-argument (file-name-base texfile)))
            (?B . ,(shell-quote-argument (file-name-sans-extension texfile)))
            (?D . ,(shell-quote-argument (format "%s" dpi)))
            (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))
            (?f . ,(shell-quote-argument
                    (expand-file-name
                     (concat (file-name-base texfile)
                             "." (plist-get extended-info :image-input-type))
                     (file-name-directory texfile))))))
         (img-formatted-command
          (split-string-shell-command
           (format-spec img-extract-command img-command-spec))))
    (plist-put extended-info :dpi-scale-factor (/ dpi 140.0))
    (list 'org-async-task
          img-formatted-command
          :buffer img-process-buffer
          :info extended-info
          :failure "LaTeX preview image conversion failed! (error code %d)")))

(defun org-latex-preview--cleanup-callback (_exit-code _stdout extended-info)
  "Delete files after image creation, in accord with EXTENDED-INFO."
  (let* ((basename (file-name-sans-extension (plist-get extended-info :texfile)))
         (images
          (mapcar
           (lambda (fragment-info)
             (plist-get fragment-info :path))
           (plist-get extended-info :fragments)))
         (clean-exts
          (or (plist-get extended-info :post-clean)
              '(".dvi" ".xdv" ".pdf" ".tex" ".aux" ".log"
                ".svg" ".png" ".jpg" ".jpeg" ".out"))))
    (dolist (img images)
      (delete-file img))
    (dolist (ext clean-exts)
      (when (file-exists-p (concat basename ext))
        (delete-file (concat basename ext))))))

(defun org-latex-preview--generic-callback (_exit-code _stdout extended-info)
  "Move and delete files after image creation, in accords with EXTENDED-INFO."
  (let* ((basename (file-name-sans-extension (plist-get extended-info :texfile)))
         (image-output-type (intern (plist-get extended-info :image-output-type)))
         (images
          (file-expand-wildcards
           (concat basename "*." (plist-get extended-info :image-output-type))
           'full))
         (clean-exts
          (or (plist-get extended-info :post-clean)
              '(".dvi" ".xdv" ".pdf" ".tex" ".aux" ".log"
                ".svg" ".png" ".jpg" ".jpeg" ".out"))))
    (save-excursion
      (cl-loop
       for fragment-info in (plist-get extended-info :fragments)
       for image-file in images
       for (beg . end) = (plist-get fragment-info :buffer-location)
       do (org-place-latex-image
           beg end
           (org-latex-preview--cache-image
            (plist-get fragment-info :key)
            image-file
            (org-latex-preview--display-info
             extended-info fragment-info)))))
    (dolist (ext clean-exts)
      (when (file-exists-p (concat basename ext))
        (delete-file (concat basename ext))))))

(defun org-latex-preview--display-info (extended-info fragment-info)
  "From FRAGMENT-INFO and EXTENDED-INFO obtain display-relevant information."
  (let ((image-type (intern (plist-get extended-info :image-output-type)))
        (fontsize (or (plist-get extended-info :fontsize) 10))
        (dpi-factor (or (plist-get extended-info :dpi-scale-factor) 1.0))
        info)
    (setq info (plist-put info :image-type image-type))
    (dolist (key '(:width :height :depth))
      (when-let ((val (plist-get fragment-info key)))
        (plist-put info key (/ val fontsize dpi-factor))))
    (plist-put info :errors (plist-get fragment-info :errors))
    info))

(defun org-latex-preview--latex-preview-filter (_proc _string extended-info)
  "Examine the stdout from LaTeX compilation with preview.sty.
The detected fontsize is directly entered into EXTENDED-INFO, and
fragment errors are put into the :errors slot of the relevant
fragments in EXTENDED-INFO."
  (unless (plist-get extended-info :fontsize)
    (when (save-excursion
            (re-search-forward "^Preview: Fontsize \\([0-9]+\\)pt$" nil t))
      (plist-put extended-info :fontsize (string-to-number (match-string 1)))))
  (let ((preview-start-re
         "^! Preview: Snippet \\([0-9]+\\) started.\n<-><->\n *\nl\\.\\([0-9]+\\)[^\n]+\n")
        (preview-end-re
         "\\(?:^Preview: Tightpage.*$\\)?\n! Preview: Snippet [0-9]+ ended.")
        (fragments (plist-get extended-info :fragments))
        preview-marks)
    (beginning-of-line)
    (save-excursion
      (while (re-search-forward preview-start-re nil t)
        (push (list (match-beginning 0)
                    (match-end 0)
                    (string-to-number (match-string 1)) ; Preview index.
                    (1+ (string-to-number (match-string 2)))) ; Base line number.
              preview-marks)))
    (setq preview-marks (nreverse preview-marks))
    (while preview-marks
      (goto-char (caar preview-marks))
      (if (re-search-forward preview-end-re (or (caadr preview-marks) (point-max)) t)
          (let ((fragment-info (nth (1- (nth 2 (car preview-marks))) fragments))
                (errors-substring
                 (string-trim
                  (buffer-substring (cadar preview-marks)
                                    (match-beginning 0))
                  ;; In certain situations we can end up with non-error
                  ;; logging informattion within the preview output.
                  ;; To make sure this is not captured, we rely on the fact
                  ;; that LaTeX error messages have a consistent format
                  ;; and start with an exclamation mark "!".  Thus, we
                  ;; can safely strip everything prior to the first "!"
                  ;; from the output.
                  "[^!]*")))
            (plist-put fragment-info :errors
                       (and (not (string-blank-p errors-substring))
                            (replace-regexp-in-string
                             "^l\\.[0-9]+"
                             (lambda (linum)
                               (format "l.%d"
                                       (- (string-to-number (substring linum 2))
                                          (nth 3 (car preview-marks)))))
                             errors-substring))))
        (goto-char (caar preview-marks)))
      (setq preview-marks (cdr preview-marks)))))

(defun org-latex-preview--dvisvgm-filter (_proc _string extended-info)
  "Look for newly created images in the dvisvgm stdout buffer.
Any matches found will be matched against the fragments recorded in
EXTENDED-INFO, and displayed in the buffer."
  (let ((dvisvgm-processing-re "^processing page \\([0-9]+\\)\n")
        (dvisvgm-depth-re "depth=\\([0-9.]+\\)pt$")
        (dvisvgm-size-re "^ *graphic size: \\([0-9.]+\\)pt x \\([0-9.]+\\)pt")
        (fragments (plist-get extended-info :fragments))
        page-marks fragments-to-show)
    (beginning-of-line)
    (save-excursion
      (while (re-search-forward dvisvgm-processing-re nil t)
        (push (cons (string-to-number (match-string 1))
                    (match-beginning 0))
              page-marks)))
    (setq page-marks (nreverse page-marks))
    (while page-marks
      (let ((start (cdar page-marks))
            (end (or (cdadr page-marks) (point-max)))
            (page (caar page-marks))
            fragment-info)
        (goto-char start)
        (when (save-excursion
                (re-search-forward "output written to \\(.*.svg\\)$" end t))
          (setq fragment-info (nth (1- page) fragments))
          (plist-put fragment-info :path (expand-file-name (match-string 1) temporary-file-directory))
          (when (save-excursion
                  (re-search-forward dvisvgm-depth-re end t))
            (plist-put fragment-info :depth (string-to-number (match-string 1))))
          (when (save-excursion (re-search-forward dvisvgm-size-re end t))
            (plist-put fragment-info :height (string-to-number (match-string 2)))
            (plist-put fragment-info :width (string-to-number (match-string 1))))
          (when (save-excursion
                  (re-search-forward "^  page is empty" end t))
            (unless (plist-get fragment-info :error)
              (plist-put fragment-info :error "Image file not produced."))
            (plist-put fragment-info :path nil))
          (push fragment-info fragments-to-show)
          (goto-char end)))
      (setq page-marks (cdr page-marks)))
    (when fragments-to-show
      (setq fragments-to-show (nreverse fragments-to-show))
      (mapc #'org-latex-preview--svg-make-fg-currentColor fragments-to-show)
      (org-latex-preview--place-images extended-info fragments-to-show))))

(defun org-latex-preview--svg-make-fg-currentColor (svg-fragment)
  "Replace the foreground color in SVG-FRAGMENT's file with \"currentColor\".
The foreground color is guessed to be the first specified <g>
fill color, which appears to be a reliable heuristic from a few
tests with the output of dvisvgm."
  (with-temp-buffer
    (insert-file-contents (plist-get svg-fragment :path))
    (goto-char (point-min))
    (when (re-search-forward "<g fill='\\(#[0-9a-f]\\{6\\}\\)'" nil t)
      (let* ((same-color (format "\\(?:fill\\|stroke\\)='\\(%s\\)'" (match-string 1))))
        (replace-match "currentColor" t t nil 1)
        (while (re-search-forward same-color nil t)
          (replace-match "currentColor" t t nil 1)))
      (write-region nil nil (plist-get svg-fragment :path) nil 0))))

(defun org-latex-preview--place-images (extended-info &optional fragments)
  "Place images for each of FRAGMENTS, according to their data and EXTENDED-INFO.
Should FRAGMENTS not be explicitly provided, all of the fragments
listed in EXTENDED-INFO will be used."
  (let ((fragments (or fragments (plist-get extended-info :fragments))))
    (with-current-buffer (plist-get extended-info :org-buffer)
      (save-excursion
        (cl-loop
         for fragment-info in fragments
         for image-file = (plist-get fragment-info :path)
         for (beg . end) = (plist-get fragment-info :buffer-location)
         do (org-place-latex-image
             beg end
             (org-latex-preview--cache-image
              (plist-get fragment-info :key)
              image-file
              (org-latex-preview--display-info
               extended-info fragment-info))))))))

(defun org-latex-preview--cache-image (key path info)
  "Save the image at PATH with associated INFO in the cache indexed by KEY.
Return (path . info)."
  (let ((label-path-info
         (org-persist-register `("LaTeX preview cached image data"
                                 (file ,path)
                                 (elisp-data ,info))
                               (list :key key)
                               :write-immediately t)))
    (cons (cadr label-path-info) info)))

(defun org-latex-preview--get-cached (key)
  "Retrieve the image path and info associated with KEY.
The result will be of the form (path . info).

Example result:
  (\"/path/.../to/.../image.svg\"
   :type svg
   :height 1.4
   :width 7.6
   :depth 0.2
   :errors nil)"
  (when-let ((label-path-info
              (org-persist-read "LaTeX preview cached image data"
                                (list :key key)
                                nil nil :read-related t)))
    (cons (cadr label-path-info)
          (caddr label-path-info))))

;; TODO: Switching processes from imagemagick to dvi* with an existing
;; dump-file during a single Emacs session should trigger
;; re-precompilation with the new precompile command.
(defun org-preview-precompile (processing-info header)
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
        header-file (plist-get processing-info :latex-precompiler)
        "fmt")))))

(defun org-preview-latex--tex-styled (value options &optional html-p)
  "Apply LaTeX style commands to VALUE based on OPTIONS.

VALUE is the math fragment text to be previewed.

OPTIONS is the plist `org-format-latex-options' with customized
color information for this run.

HTML-P, if true, uses colors required for HTML processing."
  (let* ((fg (pcase (plist-get options (if html-p :html-foreground :foreground))
               ('default (org-latex-color-format (org-latex-color :foreground)))
               ((pred null) (org-latex-color-format "Black"))
               (color (org-latex-color-format color))))
         (bg (pcase (plist-get options (if html-p :html-background :background))
               ('default (org-latex-color :background))
               ("Transparent" nil)
               (bg (org-latex-color-format bg)))))
    (concat (and bg (format "\\pagecolor[rgb]{%s}" bg))
            (and fg (format "\\color[rgb]{%s}" fg))
            "%\n"
            value)))

(defun org-create-latex-export (processing-type element prefix dir &optional block-type)
  "Create a export of the LaTeX math fragment ELEMENT using PROCESSING-TYPE.

Generated image files are placed in DIR with the prefix PREFIX.
Note that PREFIX may itself contain a directory path component.

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
                      (list processing-type
                            org-format-latex-header
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

;; TODO: Deleting an existing preview overlay over the same reagion is
;; wasteful. It's simpler just to update the display property of the
;; existing overlay.
(defun org-place-latex-image (beg end path-info)
  "Place an overlay from BEG to END showing MOVEFILE.
The overlay will be above BEG if OVERLAYS is non-nil."
  (dolist (o (overlays-in beg end))
    (when (eq (overlay-get o 'org-overlay-type)
              'org-latex-overlay)
      (delete-overlay o)))
  (org--make-preview-overlay beg end path-info)
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
