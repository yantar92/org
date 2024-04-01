;;; org-preview-image.el --- Image link previews for Org -*- lexical-binding: t; -*-

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
;;  Image previews for Org.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-element)
(declare-function org-attach-expand "org-attach")
(require 'org-property)

(defcustom org-image-actual-width t
  "When non-nil, use the actual width of images when inlining them.

When set to a number, use imagemagick (when available) to set the
image's width to this value.

When set to a number in a list, try to get the width from any
#+ATTR.* keyword if it matches a width specification like

  #+ATTR_HTML: :width 300px

and fall back on that number if none is found.

When set to nil, first try to get the width from #+ATTR_ORG.  If
that is not found, use the first #+ATTR_xxx :width specification.
If that is also not found, fall back on the original image width.

Finally, Org mode is quite flexible in the width specifications it
supports and intelligently interprets width specifications for other
backends when rendering an image in an org buffer.  This behavior is
described presently.

1. A floating point value between 0 and 2 is interpreted as the
   percentage of the text area that should be taken up by the image.
2. A number followed by a percent sign is divided by 100 and then
   interpreted as a floating point value.
3. If a number is followed by other text, extract the number and
   discard the remaining text.  That number is then interpreted as a
   floating-point value.  For example,

   #+ATTR_LATEX: :width 0.7\\linewidth

   would be interpreted as 70% of the text width.
4. If t is provided the original image width is used.  This is useful
   when you want to specify a width for a backend, but still want to
   use the original image width in the org buffer.

This requires Emacs >= 24.1, built with imagemagick support."
  :group 'org-appearance
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Use the image width" t)
	  (integer :tag "Use a number of pixels")
	  (list :tag "Use #+ATTR* or a number of pixels" (integer))
	  (const :tag "Use #+ATTR* or don't resize" nil)))

(defcustom org-image-max-width 'fill-column
  "When non-nil, limit the displayed image width.
This setting only takes effect when `org-image-actual-width' is set to
t or when #+ATTR* is set to t.

Possible values:
- `fill-column' :: limit width to `fill-column'
- `window'      :: limit width to window width
- integer       :: limit width to number in pixels
- float         :: limit width to that fraction of window width
- nil             :: do not limit image width"
  :group 'org-appearance
  :package-version '(Org . "9.7")
  :type '(choice
          (const :tag "Do not limit image width" nil)
          (const :tag "Limit to `fill-column'" fill-column)
          (const :tag "Limit to window width" window)
          (integer :tag "Limit to a number of pixels")
          (float :tag "Limit to a fraction of window width")))

(defvar-local org-inline-image-overlays nil)
;; Preserve when switching modes or when restarting Org.
;; If we clear the overlay list and later enable Or mode, the existing
;; image overlays will never be cleared by `org-toggle-inline-images'.
(put 'org-inline-image-overlays 'permanent-local t)

(defun org--inline-image-overlays (&optional beg end)
  "Return image overlays between BEG and END."
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (overlays (overlays-in beg end))
         result)
    (dolist (ov overlays result)
      (when (memq ov org-inline-image-overlays)
        (push ov result)))))

(defun org-toggle-inline-images (&optional include-linked beg end)
  "Toggle the display of inline images.
INCLUDE-LINKED is passed to `org-display-inline-images'."
  (interactive "P")
  (if (org--inline-image-overlays beg end)
      (progn
        (org-remove-inline-images beg end)
        (when (called-interactively-p 'interactive)
	  (message "Inline image display turned off")))
    (org-display-inline-images include-linked nil beg end)
    (when (called-interactively-p 'interactive)
      (let ((new (org--inline-image-overlays beg end)))
        (message (if new
		     (format "%d images displayed inline"
			     (length new))
		   "No images to display inline"))))))

(defun org-redisplay-inline-images ()
  "Assure display of inline images and refresh them."
  (interactive)
  (org-toggle-inline-images)
  (unless org-inline-image-overlays
    (org-toggle-inline-images)))

;; For without-x builds.
(declare-function image-flush "image" (spec &optional frame))

(defcustom org-display-remote-inline-images 'skip
  "How to display remote inline images.
Possible values of this option are:

skip        Don't display remote images.
download    Always download and display remote images.
t
cache       Display remote images, and open them in separate buffers
            for caching.  Silently update the image buffer when a file
            change is detected."
  :group 'org-appearance
  :package-version '(Org . "9.7")
  :type '(choice
	  (const :tag "Ignore remote images" skip)
	  (const :tag "Always display remote images" download)
	  (const :tag "Display and silently update remote images" cache))
  :safe #'symbolp)

(defcustom org-image-align 'left
  "How to align images previewed using `org-display-inline-images'.

Only stand-alone image links are affected by this setting.  These
are links without surrounding text.

Possible values of this option are:

left     Insert image at specified position.
center   Center image previews.
right    Right-align image previews."
  :group 'org-appearance
  :package-version '(Org . "9.7")
  :type '(choice
          (const :tag "Left align (or don\\='t align) image previews" left)
	  (const :tag "Center image previews" center)
	  (const :tag "Right align image previews" right))
  :safe #'symbolp)

(defun org--create-inline-image (file width)
  "Create image located at FILE, or return nil.
WIDTH is the width of the image.  The image may not be created
according to the value of `org-display-remote-inline-images'."
  (let* ((remote? (file-remote-p file))
	 (file-or-data
	  (pcase org-display-remote-inline-images
	    ((guard (not remote?)) file)
	    (`download (with-temp-buffer
			 (set-buffer-multibyte nil)
			 (insert-file-contents-literally file)
			 (buffer-string)))
	    ((or `cache `t)
             (let ((revert-without-query '(".")))
	       (with-current-buffer (find-file-noselect file)
		 (buffer-string))))
	    (`skip nil)
	    (other
	     (message "Invalid value of `org-display-remote-inline-images': %S"
		      other)
	     nil))))
    (when file-or-data
      (create-image file-or-data
		    (and (image-type-available-p 'imagemagick)
			 width
			 'imagemagick)
		    remote?
		    :width width
                    :max-width
                    (pcase org-image-max-width
                      (`fill-column (* fill-column (frame-char-width (selected-frame))))
                      (`window (window-width nil t))
                      ((pred integerp) org-image-max-width)
                      ((pred floatp) (floor (* org-image-max-width (window-width nil t))))
                      (`nil nil)
                      (_ (error "Unsupported value of `org-image-max-width': %S"
                                org-image-max-width)))
                    :scale 1))))

(defun org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.

An inline image is a link which follows either of these
conventions:

  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous
     type.  In this case, that link must be a well-formed plain
     or angle link, i.e., it must have an explicit \"file\" or
     \"attachment\" type.

Equip each image with the key-map `image-map'.

When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.

BEG and END define the considered part.  They default to the
buffer boundaries with possible narrowing."
  (interactive "P")
  (when (display-graphic-p)
    (when refresh
      (org-remove-inline-images beg end)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (let ((end (or end (point-max))))
      (org-with-point-at (or beg (point-min))
	(let* ((case-fold-search t)
	       (file-extension-re (image-file-name-regexp))
	       (link-abbrevs (mapcar #'car
				     (append (org-element-property :link-abbrevs (org-element-org-data))
					     org-link-abbrev-alist)))
	       ;; Check absolute, relative file names and explicit
	       ;; "file:" links.  Also check link abbreviations since
	       ;; some might expand to "file" links.
	       (file-types-re
		(format "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]\\[\\(<?\\(?:file\\|attachment\\):\\)"
			(if (not link-abbrevs) ""
			  (concat "\\|" (regexp-opt link-abbrevs))))))
	  (while (re-search-forward file-types-re end t)
	    (let* ((link (org-element-lineage
			  (save-match-data (org-element-context))
			  'link t))
                   (linktype (org-element-property :type link))
		   (inner-start (match-beginning 1))
		   (path
		    (cond
		     ;; No link at point; no inline image.
		     ((not link) nil)
		     ;; File link without a description.  Also handle
		     ;; INCLUDE-LINKED here since it should have
		     ;; precedence over the next case.  I.e., if link
		     ;; contains filenames in both the path and the
		     ;; description, prioritize the path only when
		     ;; INCLUDE-LINKED is non-nil.
		     ((or (not (org-element-contents-begin link))
			  include-linked)
		      (and (or (equal "file" linktype)
                               (equal "attachment" linktype))
			   (org-element-property :path link)))
		     ;; Link with a description.  Check if description
		     ;; is a filename.  Even if Org doesn't have syntax
		     ;; for those -- clickable image -- constructs, fake
		     ;; them, as in `org-export-insert-image-links'.
		     ((not inner-start) nil)
		     (t
		      (org-with-point-at inner-start
			(and (looking-at
			      (if (char-equal ?< (char-after inner-start))
				  org-link-angle-re
				org-link-plain-re))
			     ;; File name must fill the whole
			     ;; description.
			     (= (org-element-contents-end link)
				(match-end 0))
			     (progn
                               (setq linktype (match-string 1))
                               (match-string 2))))))))
	      (when (and path (string-match-p file-extension-re path))
		(let ((file (if (equal "attachment" linktype)
				(progn
                                  (require 'org-attach)
				  (ignore-errors (org-attach-expand path)))
                              (expand-file-name path))))
                  ;; Expand environment variables.
                  (when file (setq file (substitute-in-file-name file)))
		  (when (and file (file-exists-p file))
		    (let ((width (org-display-inline-image--width link))
			  (align (org-image--align link))
                          (old (get-char-property-and-overlay
				(org-element-begin link)
				'org-image-overlay)))
		      (if (and (car-safe old) refresh)
                          (image-flush (overlay-get (cdr old) 'display))
			(let ((image (org--create-inline-image file width)))
			  (when image
			    (let ((ov (make-overlay
				       (org-element-begin link)
				       (progn
					 (goto-char
					  (org-element-end link))
					 (unless (eolp) (skip-chars-backward " \t"))
					 (point)))))
                              ;; See bug#59902.  We cannot rely
                              ;; on Emacs to update image if the file
                              ;; has changed.
                              (image-flush image)
			      (overlay-put ov 'display image)
			      (overlay-put ov 'face 'default)
			      (overlay-put ov 'org-image-overlay t)
			      (overlay-put
			       ov 'modification-hooks
			       (list 'org-display-inline-remove-overlay))
			      (when (boundp 'image-map)
				(overlay-put ov 'keymap image-map))
                              (when align
                                (overlay-put
                                 ov 'before-string
                                 (propertize
                                  " " 'face 'default
                                  'display
                                  (pcase align
                                    ("center" `(space :align-to (- center (0.5 . ,image))))
                                    ("right"  `(space :align-to (- right ,image)))))))
			      (push ov org-inline-image-overlays))))))))))))))))

(declare-function org-export-read-attribute "ox"
                  (attribute element &optional property))
(defvar visual-fill-column-width) ; Silence compiler warning
(defun org-display-inline-image--width (link)
  "Determine the display width of the image LINK, in pixels.
- When `org-image-actual-width' is t, the image's pixel width is used.
- When `org-image-actual-width' is a number, that value will is used.
- When `org-image-actual-width' is nil or a list, :width attribute of
  #+attr_org or the first #+attr_...  (if it exists) is used to set the
  image width.  A width of X% is divided by 100.  If the value is a
  float between 0 and 2, it interpreted as that proportion of the text
  width in the buffer.

  If no :width attribute is given and `org-image-actual-width' is a
  list with a number as the car, then that number is used as the
  default value."
  ;; Apply `org-image-actual-width' specifications.
  ;; Support subtree-level property "ORG-IMAGE-ACTUAL-WIDTH" specified
  ;; width.
  (let ((org-image-actual-width (org-property-or-variable-value 'org-image-actual-width)))
    (cond
     ((eq org-image-actual-width t) nil)
     ((listp org-image-actual-width)
      (require 'ox)
      (let* ((par (org-element-lineage link 'paragraph))
             ;; Try to find an attribute providing a :width.
             ;; #+ATTR_ORG: :width ...
             (attr-width (org-export-read-attribute :attr_org par :width))
             (width-unreadable?
              (lambda (value)
                (or (not (stringp value))
                    (unless (string= value "t")
                      (or (not (string-match
			      (rx bos (opt "+")
                                  (or
                                   ;; Number of pixels
                                   ;; must be a lone number, not
                                   ;; things like 4in
                                   (seq (1+ (in "0-9")) eos)
                                   ;; Numbers ending with %
                                   (seq (1+ (in "0-9.")) (group-n 1 "%"))
                                   ;; Fractions
                                   (seq (0+ (in "0-9")) "." (1+ (in "0-9")))))
			      value))
                          (let ((number (string-to-number value)))
                            (and (floatp number)
                                 (not (match-string 1 value)) ; X%
                                 (not (<= 0.0 number 2.0)))))))))
             ;; #+ATTR_BACKEND: :width ...
             (attr-other
              (catch :found
                (org-element-properties-map
                 (lambda (prop _)
                   (when (and
                          (not (eq prop :attr_org))
                          (string-match-p "^:attr_" (symbol-name prop))
                          (not (funcall width-unreadable? (org-export-read-attribute prop par :width))))
                     (throw :found prop)))
                 par)))
             (attr-width
              (if (not (funcall width-unreadable? attr-width))
                  attr-width
                ;; When #+attr_org: does not have readable :width
                (and attr-other
                     (org-export-read-attribute attr-other par :width))))
             (width
              (cond
               ;; Treat :width t as if `org-image-actual-width' were t.
               ((string= attr-width "t") nil)
               ;; Fallback to `org-image-actual-width' if no interprable width is given.
               ((funcall width-unreadable? attr-width)
                (car org-image-actual-width))
               ;; Convert numeric widths to numbers, converting percentages.
               ((string-match-p "\\`[[+]?[0-9.]+%" attr-width)
                (/ (string-to-number attr-width) 100.0))
               (t (string-to-number attr-width)))))
        (if (and (floatp width) (<= 0.0 width 2.0))
            ;; A float in [0,2] should be interpereted as this portion of
            ;; the text width in the window.  This works well with cases like
            ;; #+attr_latex: :width 0.X\{line,page,column,etc.}width,
            ;; as the "0.X" is pulled out as a float.  We use 2 as the upper
            ;; bound as cases such as 1.2\linewidth are feasible.
            (round (* width
                      (window-pixel-width)
                      (/ (or (and (bound-and-true-p visual-fill-column-mode)
                                  (or visual-fill-column-width auto-fill-function))
                             (when auto-fill-function fill-column)
                             (- (window-text-width) (line-number-display-width)))
                         (float (window-total-width)))))
          width)))
     ((numberp org-image-actual-width)
      org-image-actual-width)
     (t nil))))

(defun org-image--align (link)
  "Determine the alignment of the image LINK.
LINK is a link object.

In decreasing order of priority, this is controlled:
- Per image by the value of `:center' or `:align' in the
affiliated keyword `#+attr_org'.
- By the `#+attr_html' or `#+attr_latex` keywords with valid
  `:center' or `:align' values.
- Globally by the user option `org-image-align'.

The result is either nil or one of the strings \"left\",
\"center\" or \"right\".

\"center\" will cause the image preview to be centered, \"right\"
will cause it to be right-aligned.  A value of \"left\" or nil
implies no special alignment."
  (let ((par (org-element-lineage link 'paragraph)))
    ;; Only align when image is not surrounded by paragraph text:
    (when (and par ; when image is not in paragraph, but in table/headline/etc, do not align
               (= (org-element-begin link)
                  (save-excursion
                    (goto-char (org-element-contents-begin par))
                    (skip-chars-forward "\t ")
                    (point)))           ;account for leading space
                                        ;before link
               (<= (- (org-element-contents-end par)
                      (org-element-end link))
                   1))                  ;account for trailing newline
                                        ;at end of paragraph
      (save-match-data
        ;; Look for a valid ":center t" or ":align left|center|right"
        ;; attribute.
        ;;
        ;; An attr_org keyword has the highest priority, with
        ;; any attr.* next.  Choosing between these is
        ;; unspecified.
        (let ((center-re ":\\(center\\)[[:space:]]+t\\b")
              (align-re ":align[[:space:]]+\\(left\\|center\\|right\\)\\b")
              attr-align)
          (catch 'exit
            (org-element-properties-mapc
             (lambda (propname propval)
               (when (and propval
                          (string-match-p ":attr.*" (symbol-name propname)))
                 (setq propval (car-safe propval))
                 (when (or (string-match center-re propval)
                           (string-match align-re propval))
                   (setq attr-align (match-string 1 propval))
                   (when (eq propname :attr_org)
                     (throw 'exit t)))))
             par))
          (if attr-align
              (when (member attr-align '("center" "right")) attr-align)
            ;; No image-specific keyword, check global alignment property
            (when (memq org-image-align '(center right))
              (symbol-name org-image-align))))))))


(defun org-display-inline-remove-overlay (ov after _beg _end &optional _len)
  "Remove inline-display overlay if a corresponding region is modified."
  (when (and ov after)
    (setq org-inline-image-overlays (delete ov org-inline-image-overlays))
    ;; Clear image from cache to avoid image not updating upon
    ;; changing on disk.  See Emacs bug#59902.
    (when (overlay-get ov 'org-image-overlay)
      (image-flush (overlay-get ov 'display)))
    (delete-overlay ov)))

(defun org-remove-inline-images (&optional beg end)
  "Remove inline display of images."
  (interactive)
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (overlays (overlays-in beg end)))
    (dolist (ov overlays)
      (when (memq ov org-inline-image-overlays)
        (setq org-inline-image-overlays (delq ov org-inline-image-overlays))
        (delete-overlay ov)))
    ;; Clear removed overlays.
    (dolist (ov org-inline-image-overlays)
      (unless (overlay-buffer ov)
        (setq org-inline-image-overlays (delq ov org-inline-image-overlays))))))

(provide 'org-preview-image)
;;; org-preview-image.el ends here
