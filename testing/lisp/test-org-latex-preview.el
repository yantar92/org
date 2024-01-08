;;; test-org-latex-preview.el --- tests for org-latex-preview.el  -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Karthik Chikmagalur
;; Authors: Karthik Chikmagalur

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(unless (featurep 'org-latex-preview)
  (signal 'missing-test-dependency "Support for LaTeX previews"))
(require 'org-latex-preview)
(require 'cl-lib)

(ert-deftest test-org-latex-preview/assert ()
  (should t))


;; Test for executables required for preview generation
(org-test-for-executable "latex")
(org-test-for-executable "dvisvgm")
(org-test-for-executable "dvipng")
;; Should we test imagemagick?
;; (org-test-for-executable "convert")


;; fragment pre-processing tests

;;; Collect fragments
(ert-deftest test-org-latex-preview/collect-fragments-inline ()
  "Test LaTeX fragment collection"
  (let ((elements
         '((latex-fragment
            (:value "\\( q \\ge 0 \\)" :begin 770 :end 784 :post-blank 1 :parent
             (paragraph
              (:begin 765 :end 918 :contents-begin 765 :contents-end 918 :post-blank 0 :post-affiliated 765 :mode nil :granularity element :parent
               (section
                (:begin 118 :end 1154 :contents-begin 118 :contents-end 1154 :robust-begin 118 :robust-end 1152 :post-blank 0 :post-affiliated 118 :mode section :granularity element :parent
                 (headline
                  (:raw-value "Inline fragments and LaTeX environments" :begin 76 :end 1154 :pre-blank 0 :contents-begin 118 :contents-end 1154 :robust-begin 184 :robust-end 1152 :level 1 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 76 :ID "0b3807b3-69af-40cb-a27a-b380d54879cc" :title "Inline fragments and LaTeX environments"))))))))
           (latex-fragment
            (:value "\\( r \\ge 0 \\)" :begin 788 :end 801 :post-blank 0 :parent
             (paragraph
              (:begin 765 :end 918 :contents-begin 765 :contents-end 918 :post-blank 0 :post-affiliated 765 :mode nil :granularity element :parent
               (section
                (:begin 118 :end 1154 :contents-begin 118 :contents-end 1154 :robust-begin 118 :robust-end 1152 :post-blank 0 :post-affiliated 118 :mode section :granularity element :parent
                 (headline
                  (:raw-value "Inline fragments and LaTeX environments" :begin 76 :end 1154 :pre-blank 0 :contents-begin 118 :contents-end 1154 :robust-begin 184 :robust-end 1152 :level 1 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 76 :ID "0b3807b3-69af-40cb-a27a-b380d54879cc" :title "Inline fragments and LaTeX environments")))))))))))
    (org-test-at-id
        "0b3807b3-69af-40cb-a27a-b380d54879cc"
      (should
       (equal (org-latex-preview-collect-fragments 765 807) elements))
      (should
       (equal (org-latex-preview--construct-entries elements)
              '(((770 783 "\\( q \\ge 0 \\)")
                (788 801 "\\( r \\ge 0 \\)"))
                nil)))
      (should
       (equal (org-latex-preview--construct-entries elements)
              (org-latex-preview--construct-entries elements 'with-numbering))))))

(ert-deftest test-org-latex-preview/collect-fragments-environment ()
  "Test LaTeX fragment collection"
  (let ((elements
         '((latex-environment
            (:begin 241 :end 412 :value "\\begin{align}\n\\dot{x} = A(t) x + B(t) u, \\quad  t \\in [0, \\infty), \\quad x(0) = x_i \\label{eq:time-varying-system}\\\\\nA(t+T) = A(t),\\ B(t + T) = B(t) \\nonumber\n\\end{align}\n" :post-blank 0 :post-affiliated 241 :mode nil :granularity element :parent
             (section
              (:begin 118 :end 1154 :contents-begin 118 :contents-end 1154 :robust-begin 118 :robust-end 1152 :post-blank 0 :post-affiliated 118 :mode section :granularity element :parent
               (headline
                (:raw-value "Inline fragments and LaTeX environments" :begin 76 :end 1154 :pre-blank 0 :contents-begin 118 :contents-end 1154 :robust-begin 184 :robust-end 1152 :level 1 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 76 :ID "0b3807b3-69af-40cb-a27a-b380d54879cc" :title "Inline fragments and LaTeX environments")))))))))
    (org-test-at-id
        "0b3807b3-69af-40cb-a27a-b380d54879cc"
      (should
       (equal (org-latex-preview-collect-fragments 186 426) elements))
      (should
       (equal (org-latex-preview--construct-entries elements)
              '(((241 411 "\\begin{align}\n\\dot{x} = A(t) x + B(t) u, \\quad  t \\in [0, \\infty), \\quad x(0) = x_i \\label{eq:time-varying-system}\\\\\nA(t+T) = A(t),\\ B(t + T) = B(t) \\nonumber\n\\end{align}\n"))
                nil)))
      (should
       (equal (org-latex-preview--construct-entries elements 'with-numbering)
              '(((241 411 "\\begin{align}\n\\dot{x} = A(t) x + B(t) u, \\quad  t \\in [0, \\infty), \\quad x(0) = x_i \\label{eq:time-varying-system}\\\\\nA(t+T) = A(t),\\ B(t + T) = B(t) \\nonumber\n\\end{align}\n"))
                (1)))))))

(ert-deftest test-org-latex-preview/collect-fragments-all ()
  "Test LaTeX fragment collection"
  (let ((elements
         '((latex-environment
            (:begin 241 :end 412 :value "\\begin{align}\n\\dot{x} = A(t) x + B(t) u, \\quad  t \\in [0, \\infty), \\quad x(0) = x_i \\label{eq:time-varying-system}\\\\\nA(t+T) = A(t),\\ B(t + T) = B(t) \\nonumber\n\\end{align}\n" :post-blank 0 :post-affiliated 241 :mode nil :granularity element :parent
             (section
              (:begin 118 :end 1154 :contents-begin 118 :contents-end 1154 :robust-begin 118 :robust-end 1152 :post-blank 0 :post-affiliated 118 :mode section :granularity element :parent
               (headline
                (:raw-value "Inline fragments and LaTeX environments" :begin 76 :end 1154 :pre-blank 0 :contents-begin 118 :contents-end 1154 :robust-begin 184 :robust-end 1152 :level 1 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 76 :ID "0b3807b3-69af-40cb-a27a-b380d54879cc" :title "Inline fragments and LaTeX environments"))))))
           (latex-fragment
            (:value "\\( (x,u) \\)" :begin 460 :end 472 :post-blank 1 :parent
             (paragraph
              (:begin 412 :end 478 :contents-begin 412 :contents-end 478 :post-blank 0 :post-affiliated 412 :mode nil :granularity element :parent
               (section
                (:begin 118 :end 1154 :contents-begin 118 :contents-end 1154 :robust-begin 118 :robust-end 1152 :post-blank 0 :post-affiliated 118 :mode section :granularity element :parent
                 (headline
                  (:raw-value "Inline fragments and LaTeX environments" :begin 76 :end 1154 :pre-blank 0 :contents-begin 118 :contents-end 1154 :robust-begin 184 :robust-end 1152 :level 1 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 76 :ID "0b3807b3-69af-40cb-a27a-b380d54879cc" :title "Inline fragments and LaTeX environments"))))))))
           (latex-environment
            (:begin 478 :end 765 :value "\\begin{align}\n\\label{eq:quadratic-form}\n\\mathbf{q}(x, u) := \\lim_{t_f \\to \\infty} \\int_{0}^{t_f} \\begin{bmatrix} x \\\\ u \\end{bmatrix}^{\\star} \\begin{bmatrix}\nQ & 0 \\\\\n0 & r\n \\end{bmatrix} \\begin{bmatrix} x \\\\ u \\end{bmatrix} =: \\lim_{t_f \\to \\infty} \\int_{0}^{t_f} q(x,u) dt\n\\end{align}\n" :post-blank 0 :post-affiliated 478 :mode nil :granularity element :parent
             (section
              (:begin 118 :end 1154 :contents-begin 118 :contents-end 1154 :robust-begin 118 :robust-end 1152 :post-blank 0 :post-affiliated 118 :mode section :granularity element :parent
               (headline
                (:raw-value "Inline fragments and LaTeX environments" :begin 76 :end 1154 :pre-blank 0 :contents-begin 118 :contents-end 1154 :robust-begin 184 :robust-end 1152 :level 1 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 76 :ID "0b3807b3-69af-40cb-a27a-b380d54879cc" :title "Inline fragments and LaTeX environments"))))))
           (latex-fragment
            (:value "\\( q \\ge 0 \\)" :begin 770 :end 784 :post-blank 1 :parent
             (paragraph
              (:begin 765 :end 918 :contents-begin 765 :contents-end 918 :post-blank 0 :post-affiliated 765 :mode nil :granularity element :parent
               (section
                (:begin 118 :end 1154 :contents-begin 118 :contents-end 1154 :robust-begin 118 :robust-end 1152 :post-blank 0 :post-affiliated 118 :mode section :granularity element :parent
                 (headline
                  (:raw-value "Inline fragments and LaTeX environments" :begin 76 :end 1154 :pre-blank 0 :contents-begin 118 :contents-end 1154 :robust-begin 184 :robust-end 1152 :level 1 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 76 :ID "0b3807b3-69af-40cb-a27a-b380d54879cc" :title "Inline fragments and LaTeX environments"))))))))
           (latex-fragment
            (:value "\\( r \\ge 0 \\)" :begin 788 :end 801 :post-blank 0 :parent
             (paragraph
              (:begin 765 :end 918 :contents-begin 765 :contents-end 918 :post-blank 0 :post-affiliated 765 :mode nil :granularity element :parent
               (section
                (:begin 118 :end 1154 :contents-begin 118 :contents-end 1154 :robust-begin 118 :robust-end 1152 :post-blank 0 :post-affiliated 118 :mode section :granularity element :parent
                 (headline
                  (:raw-value "Inline fragments and LaTeX environments" :begin 76 :end 1154 :pre-blank 0 :contents-begin 118 :contents-end 1154 :robust-begin 184 :robust-end 1152 :level 1 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 76 :ID "0b3807b3-69af-40cb-a27a-b380d54879cc" :title "Inline fragments and LaTeX environments"))))))))
           (latex-fragment
            (:value "\\( \\mathbf{q} \\)" :begin 842 :end 859 :post-blank 1 :parent
             (paragraph
              (:begin 765 :end 918 :contents-begin 765 :contents-end 918 :post-blank 0 :post-affiliated 765 :mode nil :granularity element :parent
               (section
                (:begin 118 :end 1154 :contents-begin 118 :contents-end 1154 :robust-begin 118 :robust-end 1152 :post-blank 0 :post-affiliated 118 :mode section :granularity element :parent
                 (headline
                  (:raw-value "Inline fragments and LaTeX environments" :begin 76 :end 1154 :pre-blank 0 :contents-begin 118 :contents-end 1154 :robust-begin 184 :robust-end 1152 :level 1 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 76 :ID "0b3807b3-69af-40cb-a27a-b380d54879cc" :title "Inline fragments and LaTeX environments"))))))))
           (latex-fragment
            (:value "\\[\n\\inf_{x,u} \\mathbf{q}(x,u).\n\\]" :begin 884 :end 917 :post-blank 0 :parent
             (paragraph
              (:begin 765 :end 918 :contents-begin 765 :contents-end 918 :post-blank 0 :post-affiliated 765 :mode nil :granularity element :parent
               (section
                (:begin 118 :end 1154 :contents-begin 118 :contents-end 1154 :robust-begin 118 :robust-end 1152 :post-blank 0 :post-affiliated 118 :mode section :granularity element :parent
                 (headline
                  (:raw-value "Inline fragments and LaTeX environments" :begin 76 :end 1154 :pre-blank 0 :contents-begin 118 :contents-end 1154 :robust-begin 184 :robust-end 1152 :level 1 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 76 :ID "0b3807b3-69af-40cb-a27a-b380d54879cc" :title "Inline fragments and LaTeX environments"))))))))
           (latex-environment
            (:begin 918 :end 1040 :value "\\begin{align}\n\\label{eq:lqr-inf-via-duality}\n\\inf_{x, u} \\mathbf{q}(x, u) = x_i^{\\star} \\bar{\\lambda}(0) x_i,\n\\end{align}\n" :post-blank 0 :post-affiliated 918 :mode nil :granularity element :parent
             (section
              (:begin 118 :end 1154 :contents-begin 118 :contents-end 1154 :robust-begin 118 :robust-end 1152 :post-blank 0 :post-affiliated 118 :mode section :granularity element :parent
               (headline
                (:raw-value "Inline fragments and LaTeX environments" :begin 76 :end 1154 :pre-blank 0 :contents-begin 118 :contents-end 1154 :robust-begin 184 :robust-end 1152 :level 1 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 76 :ID "0b3807b3-69af-40cb-a27a-b380d54879cc" :title "Inline fragments and LaTeX environments"))))))
           (latex-fragment
            (:value "\\( \\bar{\\lambda} \\)" :begin 1046 :end 1066 :post-blank 1 :parent
             (paragraph
              (:begin 1040 :end 1154 :contents-begin 1040 :contents-end 1154 :post-blank 0 :post-affiliated 1040 :mode nil :granularity element :parent
               (section
                (:begin 118 :end 1154 :contents-begin 118 :contents-end 1154 :robust-begin 118 :robust-end 1152 :post-blank 0 :post-affiliated 118 :mode section :granularity element :parent
                 (headline
                  (:raw-value "Inline fragments and LaTeX environments" :begin 76 :end 1154 :pre-blank 0 :contents-begin 118 :contents-end 1154 :robust-begin 184 :robust-end 1152 :level 1 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 76 :ID "0b3807b3-69af-40cb-a27a-b380d54879cc" :title "Inline fragments and LaTeX environments"))))))))
           (latex-fragment
            (:value "\\( [0, t] \\)" :begin 1140 :end 1152 :post-blank 0 :parent
             (paragraph
              (:begin 1040 :end 1154 :contents-begin 1040 :contents-end 1154 :post-blank 0 :post-affiliated 1040 :mode nil :granularity element :parent
               (section
                (:begin 118 :end 1154 :contents-begin 118 :contents-end 1154 :robust-begin 118 :robust-end 1152 :post-blank 0 :post-affiliated 118 :mode section :granularity element :parent
                 (headline
                  (:raw-value "Inline fragments and LaTeX environments" :begin 76 :end 1154 :pre-blank 0 :contents-begin 118 :contents-end 1154 :robust-begin 184 :robust-end 1152 :level 1 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 76 :ID "0b3807b3-69af-40cb-a27a-b380d54879cc" :title "Inline fragments and LaTeX environments")))))))))))
    (org-test-at-id
        "0b3807b3-69af-40cb-a27a-b380d54879cc"
      (should
       (equal (org-latex-preview-collect-fragments) elements))
      (should (equal (org-latex-preview--construct-entries elements)
                     '(((241 411 "\\begin{align}\n\\dot{x} = A(t) x + B(t) u, \\quad  t \\in [0, \\infty), \\quad x(0) = x_i \\label{eq:time-varying-system}\\\\\nA(t+T) = A(t),\\ B(t + T) = B(t) \\nonumber\n\\end{align}\n")
                        (460 471 "\\( (x,u) \\)")
                        (478 764 "\\begin{align}\n\\label{eq:quadratic-form}\n\\mathbf{q}(x, u) := \\lim_{t_f \\to \\infty} \\int_{0}^{t_f} \\begin{bmatrix} x \\\\ u \\end{bmatrix}^{\\star} \\begin{bmatrix}\nQ & 0 \\\\\n0 & r\n \\end{bmatrix} \\begin{bmatrix} x \\\\ u \\end{bmatrix} =: \\lim_{t_f \\to \\infty} \\int_{0}^{t_f} q(x,u) dt\n\\end{align}\n")
                        (770 783 "\\( q \\ge 0 \\)")
                        (788 801 "\\( r \\ge 0 \\)")
                        (842 858 "\\( \\mathbf{q} \\)")
                        (884 917 "\\[\n\\inf_{x,u} \\mathbf{q}(x,u).\n\\]")
                        (918 1039 "\\begin{align}\n\\label{eq:lqr-inf-via-duality}\n\\inf_{x, u} \\mathbf{q}(x, u) = x_i^{\\star} \\bar{\\lambda}(0) x_i,\n\\end{align}\n")
                        (1046 1065 "\\( \\bar{\\lambda} \\)")
                        (1140 1152 "\\( [0, t] \\)"))
                       nil)))
      (should (equal (org-latex-preview--construct-entries elements 'with-numbering)
                     '(((241 411 "\\begin{align}\n\\dot{x} = A(t) x + B(t) u, \\quad  t \\in [0, \\infty), \\quad x(0) = x_i \\label{eq:time-varying-system}\\\\\nA(t+T) = A(t),\\ B(t + T) = B(t) \\nonumber\n\\end{align}\n")
                        (460 471 "\\( (x,u) \\)")
                        (478 764 "\\begin{align}\n\\label{eq:quadratic-form}\n\\mathbf{q}(x, u) := \\lim_{t_f \\to \\infty} \\int_{0}^{t_f} \\begin{bmatrix} x \\\\ u \\end{bmatrix}^{\\star} \\begin{bmatrix}\nQ & 0 \\\\\n0 & r\n \\end{bmatrix} \\begin{bmatrix} x \\\\ u \\end{bmatrix} =: \\lim_{t_f \\to \\infty} \\int_{0}^{t_f} q(x,u) dt\n\\end{align}\n")
                        (770 783 "\\( q \\ge 0 \\)")
                        (788 801 "\\( r \\ge 0 \\)")
                        (842 858 "\\( \\mathbf{q} \\)")
                        (884 917 "\\[\n\\inf_{x,u} \\mathbf{q}(x,u).\n\\]")
                        (918 1039 "\\begin{align}\n\\label{eq:lqr-inf-via-duality}\n\\inf_{x, u} \\mathbf{q}(x, u) = x_i^{\\star} \\bar{\\lambda}(0) x_i,\n\\end{align}\n")
                        (1046 1065 "\\( \\bar{\\lambda} \\)")
                        (1140 1152 "\\( [0, t] \\)"))
                       (1 nil 2 nil nil nil nil 4 nil nil)))))))

;;; Set up overlays
(ert-deftest test-org-latex-preview/ensure-overlay ()
  (org-test-at-id "0b3807b3-69af-40cb-a27a-b380d54879cc"
    (org-latex-preview-clear-overlays (point-min) (point-max))
    (let* ((elements (org-latex-preview-collect-fragments))
           (entries (car (org-latex-preview--construct-entries elements)))
           (ov) (all-ovs))
      (pcase-dolist(`(,beg ,end ,value) entries)
        (setq ov (org-latex-preview--ensure-overlay beg end))
        (push ov all-ovs)
        (should (eq (get-char-property beg 'org-overlay-type) 'org-latex-overlay))
        (should (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay))
        (should (eq (overlay-get ov 'evaporate) t))
        (should (eq (overlay-get ov 'priority) org-latex-preview--overlay-priority))
        (should (equal (overlay-get ov 'modification-hooks)
                    (list #'org-latex-preview-auto--mark-overlay-modified)))
        (should (equal (overlay-get ov 'insert-in-front-hooks)
                    (list #'org-latex-preview-auto--insert-front-handler)))
        (should (equal (overlay-get ov 'insert-behind-hooks)
                    (list #'org-latex-preview-auto--insert-behind-handler))))
      (should (eq (length all-ovs) (length elements))))
    (org-latex-preview-clear-overlays (point-min) (point-max))))


;; Precompilation test
;; TODO


;; Process fragments

;;; No precompilation, no caching
(ert-deftest test-org-latex-preview/place-previews-1 ()
  (org-test-at-id "0b3807b3-69af-40cb-a27a-b380d54879cc"
    (let ((org-latex-preview-process-precompiled nil)
          (org-latex-preview-cache 'temp)
          (org-latex-preview-process-default 'dvisvgm))
      (org-latex-preview-auto-mode -1)
      (goto-char 255)
      (let ((element (org-element-context)))
        (should (eq (org-element-type element) 'latex-environment))
        (org-latex-preview-clear-cache (org-element-property :begin element)
                                       (org-element-property :end element))
        (apply #'org-async-wait-for
               (org-latex-preview--place-from-elements
                org-latex-preview-process-default
                (list element)))
        (let ((ov (cl-some (lambda (o) (and (eq (overlay-get o 'org-overlay-type)
                                           'org-latex-overlay)
                                       o))
                           (overlays-at (point)))))
          (should (overlayp ov))
          (let ((display (overlay-get ov 'display))
                (face    (overlay-get ov 'face))
                (hidden-face (overlay-get ov 'hidden-face))
                (preview-image (overlay-get ov 'preview-image)))
            ;; Image properties
            (should (consp display))
            (should (eq (car display) 'image))
            (should (eq (plist-get (cdr display) :type) 'svg))
            (should (stringp (plist-get (cdr display) :file)))
            (should (string-suffix-p ".svg" (plist-get (cdr display) :file)))
            (should (eq preview-image display))
            ;; Face properties
            (should (eq face 'default))
            (should (eq hidden-face face))))))))

(ert-deftest test-org-latex-preview/place-previews-all ()
  (org-test-at-id "0b3807b3-69af-40cb-a27a-b380d54879cc"
    (let ((org-latex-preview-process-precompiled nil)
          (org-latex-preview-cache 'temp)
          (org-latex-preview-process-default 'dvisvgm))
      (org-latex-preview-auto-mode -1)
      (org-latex-preview-clear-cache (point-min) (point-max))
      (let ((elements (org-latex-preview-collect-fragments)))
        (apply #'org-async-wait-for
               (org-latex-preview--place-from-elements
                org-latex-preview-process-default
                elements))
        (dolist (element elements)
          (let ((ov (cl-some (lambda (o) (and (eq (overlay-get o 'org-overlay-type)
                                             'org-latex-overlay)
                                         o))
                             (overlays-at (org-element-property :begin element)))))
            (should (overlayp ov))
            (let ((display (overlay-get ov 'display))
                  (face    (overlay-get ov 'face))
                  (hidden-face (overlay-get ov 'hidden-face))
                  (preview-image (overlay-get ov 'preview-image)))
              ;; Image properties
              (should (consp display))
              (should (eq (car display) 'image))
              (should (eq (plist-get (cdr display) :type) 'svg))
              (should (stringp (plist-get (cdr display) :file)))
              (should (string-suffix-p ".svg" (plist-get (cdr display) :file)))
              (should (eq preview-image display))
              ;; Face properties
              (should (eq face 'default))
              (should (eq hidden-face face)))))
        (org-latex-preview-clear-cache (point-min) (point-max))))))

;; TODO: Test with precompilation
;; TODO: Test with caching


;; dvisvgm filter+callback test
;; dvipng filter+callback test
;; imagemagick filter+callback test
;; TODO


;; Color extraction
;; TODO


;; Scaling
;; TODO


;; Caching
;; TODO

;;; clear-cache test


;; Numbering
;; TODO


;; org-latex-preview-auto-mode
;; TODO


;; live previews
;; TODO


;; lualatex-specific tests


;; xelatex-specific tests

(provide 'test-org-latex-preview)
;;; test-org-latex-preview.el ends here
