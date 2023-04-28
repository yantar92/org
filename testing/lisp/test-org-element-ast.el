;;; test-org-element-ast.el --- Tests for org-element-ast.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ihor Radchenko

;; Author: Ihor Radchenko <yantar92@posteo.net>

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

(require 'org-element-ast)

(ert-deftest test-org-element-ast/org-element-property-1 ()
  "Test `org-element-property-1' specifications."
  ;; No properties.
  (dolist (element `( nil
                      (headline nil)
                      (headline nil (headline))
                      "string"))
    (should-not (org-element-property-1 :begin element))
    (should (eq 'default (org-element-property-1 :begin element 'default)))
    (should-not (org-element-property-1 :begin1 element))
    (should (eq 'default (org-element-property-1 :begin1 element 'default)))
    (dolist (prop '(:begin))
      (should-not (org-element-property-1 prop element))
      (should (eq 'default (org-element-property-1 prop element 'default))))
    (dolist (prop '(:begin1))
      (should-not (org-element-property-1 prop element))
      (should (eq 'default (org-element-property-1 prop element 'default)))))
  ;; Only non-standard properties.
  (dolist (element `((headline (:begin1 1))
                     (headline (:begin1 1) (headline))
                     ,(propertize "string" :begin1 1)))
    (should-not (org-element-property-1 :begin element))
    (should (eq 'default (org-element-property-1 :begin element 'default)))
    (should (= 1 (org-element-property-1 :begin1 element)))
    (should (= 1 (org-element-property-1 :begin1 element 'default)))
    (dolist (prop '(:begin))
      (should-not (org-element-property-1 prop element))
      (should (eq 'default (org-element-property-1 prop element 'default))))
    (dolist (prop '(:begin1))
      (should (= 1 (org-element-property-1 prop element)))
      (should (= 1 (org-element-property-1 prop element 'default)))))
  ;; Only standard properties.
  (dolist (element `((headline (:standard-properties ,(make-vector 10 'test)))
                     (headline (:standard-properties ,(make-vector 10 'test)) (headline))))
    (should (eq 'test (org-element-property-1 :begin element)))
    (should (eq 'test (org-element-property-1 :begin element 'default)))
    (should-not (org-element-property-1 :begin1 element))
    (should (eq 'default (org-element-property-1 :begin1 element 'default)))
    (dolist (prop '(:begin))
      (should (eq 'test (org-element-property-1 prop element)))
      (should (eq 'test (org-element-property-1 prop element 'default))))
    (dolist (prop '(:begin1))
      (should-not (org-element-property-1 prop element))
      (should (eq 'default (org-element-property-1 prop element 'default)))))
  ;; Standard properties in the plist.
  (dolist (element `((headline (:begin 1))
                     (headline (:begin 1) (headline))
                     ,(propertize "string" :begin 1)))
    (should (= 1 (org-element-property-1 :begin element)))
    (should (= 1 (org-element-property-1 :begin element 'default)))
    (should-not (org-element-property-1 :begin1 element))
    (should (eq 'default (org-element-property-1 :begin1 element 'default)))
    (dolist (prop '(:begin))
      (should (= 1 (org-element-property-1 prop element)))
      (should (= 1 (org-element-property-1 prop element 'default))))
    (dolist (prop '(:begin1))
      (should-not (org-element-property-1 prop element))
      (should (eq 'default (org-element-property-1 prop element 'default)))))
  ;; Standard properties mixed in the plist and standard array.
  (dolist (element `((headline (:standard-properties ,(make-vector 10 'test) :begin 1))
                     (headline (:begin 1 :standard-properties ,(make-vector 10 'test)))
                     (headline (:standard-properties ,(make-vector 10 'test) :begin 1) (headline))))
    (should (eq 'test (org-element-property-1 :begin element)))
    (should (eq 'test (org-element-property-1 :begin element 'default)))
    (should-not (org-element-property-1 :begin1 element))
    (should (eq 'default (org-element-property-1 :begin1 element 'default)))
    (dolist (prop '(:begin))
      (should (eq 'test (org-element-property-1 prop element)))
      (should (eq 'test (org-element-property-1 prop element 'default))))
    (dolist (prop '(:begin1))
      (should-not (org-element-property-1 prop element))
      (should (eq 'default (org-element-property-1 prop element 'default)))))
  ;; General case.
  (dolist (element `((headline (:standard-properties ,(make-vector 10 'test) :begin1 1))
                     (headline (:begin1 1 :standard-properties ,(make-vector 10 'test)))
                     (headline (:standard-properties ,(make-vector 10 'test) :begin1 1) (headline))))
    (should (eq 'test (org-element-property-1 :begin element)))
    (should (eq 'test (org-element-property-1 :begin element 'default)))
    (should (= 1 (org-element-property-1 :begin1 element)))
    (should (= 1 (org-element-property-1 :begin1 element 'default)))
    (dolist (prop '(:begin))
      (should (eq 'test (org-element-property-1 prop element)))
      (should (eq 'test (org-element-property-1 prop element 'default))))
    (dolist (prop '(:begin1))
      (should (= 1 (org-element-property-1 prop element)))
      (should (= 1 (org-element-property-1 prop element 'default))))))

(ert-deftest test-org-element-ast/org-element-put-property ()
  "Test `org-element-put-property' specifications."
  ;; No properties.
  (let ((element (list 'heading nil))
        vec)
    (setq vec (make-vector (length org-element--standard-properties) nil))
    (aset vec 0 1)
    (should
     (equal
      (list 'heading (list :standard-properties vec))
      (org-element-put-property element :begin 1))))
  (let ((element (list 'heading nil)))
    (should
     (equal
      (list 'heading (list :begin1 1))
      (org-element-put-property element :begin1 1))))
  ;; Standard properties.
  (let ((element (list 'heading (list :standard-properties (make-vector (length org-element--standard-properties) 'foo)))))
    (should
     (= 1
        (org-element-property-1 :begin (org-element-put-property element :begin 1)))))
  ;; Adding standard properties when other standard properties are defined manually in the plist.
  (let ((element (list 'heading (list :begin 1 :end 20 :foo 'foo))))
    (should
     (= 2
        (org-element-property-1 :begin (org-element-put-property element :begin 2))))
    ;; Check setter.
    (cl-incf (org-element-property-1 :begin element))
    (should
     (= 3 (org-element-property-1 :begin element)))
    (should
     (= 20
        (org-element-property-1 :end element)))
    (should
     (eq 'foo
         (org-element-property-1 :foo element)))))

(ert-deftest test-org-element-ast/org-element-create ()
  "Test `org-element-create' specifications."
  (should
   (pcase (org-element-create 'foo '(:a 1 :b 2))
     (`(foo (:standard-properties ,_ :a 1 :b 2)) t)))
  (should
   (pcase (org-element-create 'foo '(:begin 10))
     (`(foo (:standard-properties ,vec))
      (= 10 (aref vec (org-element--property-idx :begin))))))
  ;; Strings
  (should (equal "foo" (org-element-create "foo")))
  (should (equal "foo" (org-element-create 'plain-text nil "foo")))
  (should (get-text-property 0 :a (org-element-create 'plain-text '(:a 1) "foo")))
  (should (get-text-property 0 :begin (org-element-create 'plain-text '(:begin 1) "foo")))
  ;; Children
  (let ((children '("a" "b" (org-element-create 'foo))))
    (should (equal (cddr (apply #'org-element-create 'bar nil children))
                   children))))

(provide 'test-org-element-ast)
;;; test-org-element-ast.el ends here
