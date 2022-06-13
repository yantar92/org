;;; org-splay-tree.el --- Skip splay tree implementation  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Ihor Radchenko

;; Author: Ihor Radchenko <yantar92@gmail.com>
;; Keywords: data

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

;;; Commentary:

;; 

;;; Code:

(require 'cl-lib)
(require 'generator)

(cl-defstruct (org-splay-tree-
               (:type vector)
               :named
               (:constructor nil)
               (:constructor org-splay-tree--create (&optional cmpfun))
               (:predicate org-splay-tree-p)
               (:copier nil))
  "Splay binary tree."
  (root nil)
  (hash (make-hash-table :test #'eq :weakness t))
  (cmpfun #'<)
  (size 0))

(cl-defstruct (org-splay-tree--node
               (:type vector)
               (:constructor nil)
               (:constructor org-splay-tree--node-create (data))
               (:copier nil))
  "Splay tree node."
  data (left nil) (right nil) (parent nil))

(defmacro org-splay-tree--node-grandparent (node)
  "Get parent of the parent of NODE."
  `(org-splay-tree--node-parent (org-splay-tree--node-parent ,node)))

(defmacro org-splay-tree--root-p (node)
  "Return non-nil when NODE is the left child of its parent."
  `(not (org-splay-tree--node-parent ,node)))

(defmacro org-splay-tree--node-left-child-p (node)
  "Return non-nil when NODE is the left child of its parent."
  `(eq ,node (org-splay-tree--node-left (org-splay-tree--node-parent ,node))))

(defmacro org-splay-tree--node-right-child-p (node)
  "Return non-nil when NODE is the right child of its parent."
  `(eq ,node (org-splay-tree--node-right (org-splay-tree--node-parent ,node))))

(defun org-splay-tree--left-rotate (tree node)
  "Perform left rotate operation on NODE in TREE.

   N             R
  / \\    ->     / \\
 L   R         N   RR
    / \\       / \\
   RL  RR    L   RL"
  (let ((right-node (org-splay-tree--node-right node)))
    (when right-node
      (setf (org-splay-tree--node-right node) (org-splay-tree--node-left right-node))
      (when (org-splay-tree--node-left right-node)
        (setf (org-splay-tree--node-parent (org-splay-tree--node-left right-node)) node))
      (setf (org-splay-tree--node-parent right-node) (org-splay-tree--node-parent node)))
    (cond
     ((org-splay-tree--root-p node)
      (setf (org-splay-tree--root tree) right-node))
     ((org-splay-tree--node-left-child-p node)
      (setf (org-splay-tree--node-left (org-splay-tree--node-parent node)) right-node))
     (t (setf (org-splay-tree--node-right (org-splay-tree--node-parent node)) right-node)))
    (when right-node (setf (org-splay-tree--node-left right-node) node))
    (setf (org-splay-tree--node-parent node) right-node)))

(defun org-splay-tree--right-rotate (tree node)
  "Perform right rotate operation on NODE in TREE.

    N             L
   / \\    ->     / \\
  L   R         LL  N
 / \\               / \\
LL  LR            LR  R"
  (let ((left-node (org-splay-tree--node-left node)))
    (when left-node
      (setf (org-splay-tree--node-left node) (org-splay-tree--node-right left-node))
      (when (org-splay-tree--node-right left-node)
        (setf (org-splay-tree--node-parent (org-splay-tree--node-right left-node)) node))
      (setf (org-splay-tree--node-parent left-node) (org-splay-tree--node-parent node)))
    (cond
     ((org-splay-tree--root-p node)
      (setf (org-splay-tree--root tree) left-node))
     ((org-splay-tree--node-left-child-p node)
      (setf (org-splay-tree--node-left (org-splay-tree--node-parent node)) left-node))
     (t (setf (org-splay-tree--node-right (org-splay-tree--node-parent node)) left-node)))
    (when left-node (setf (org-splay-tree--node-right left-node) node))
    (setf (org-splay-tree--node-parent node) left-node)))

(cl-defun org-splay-tree--splay (tree node &optional roots)
  "Perform splay operation on TREE up from NODE.
Optional ROOTS argument contains the roots to splay to.
When optional argument ROOTS is non-nil, limit splaying.
Every time splaying reaches an element contained in the ROOTS, it
continues splaying from parent of the reached root node and the
reached root is removed from ROOTS.
Once ROOTS becomes empty, splaying process terminates.
See http://dx.doi.org/10.1007/978-3-642-03367-4_18
Jonathan C. Derryberry, Daniel D. Sleator [Springer Berlin Heidelberg]
\(2009) Skip-Splay: Toward Achieving the Unified Bound in the BST
Model."
  (unless roots (setq roots (list (org-splay-tree--root tree))))
  (while (org-splay-tree--node-parent node)
    (cond
     ((or (org-splay-tree--root-p (org-splay-tree--node-parent node))
          (memq (org-splay-tree--node-parent node) roots))
      (if (org-splay-tree--node-left-child-p node)
          (org-splay-tree--right-rotate tree (org-splay-tree--node-parent node))
        (org-splay-tree--left-rotate tree (org-splay-tree--node-parent node)))
      (if roots
          (when (memq (org-splay-tree--node-parent node) roots)
            (setq roots (delq (org-splay-tree--node-parent node) roots))
            (setq node (org-splay-tree--node-parent node)))
        ;; No more ROOTS. Terminate.
        (setq node (org-splay-tree--root tree))))
     ((and (org-splay-tree--node-left-child-p node)
           (org-splay-tree--node-left-child-p (org-splay-tree--node-parent node)))
      (org-splay-tree--right-rotate tree (org-splay-tree--node-grandparent node))
      (org-splay-tree--right-rotate tree (org-splay-tree--node-parent node)))
     ((and (org-splay-tree--node-right-child-p node)
           (org-splay-tree--node-right-child-p (org-splay-tree--node-parent node)))
      (org-splay-tree--left-rotate tree (org-splay-tree--node-grandparent node))
      (org-splay-tree--left-rotate tree (org-splay-tree--node-parent node)))
     ((and (org-splay-tree--node-left-child-p node)
           (org-splay-tree--node-right-child-p (org-splay-tree--node-parent node)))
      (org-splay-tree--right-rotate tree (org-splay-tree--node-parent node))
      (org-splay-tree--left-rotate tree (org-splay-tree--node-parent node)))
     (t
      (org-splay-tree--left-rotate tree (org-splay-tree--node-parent node))
      (org-splay-tree--right-rotate tree (org-splay-tree--node-parent node))))))

(defun org-splay-tree--replace (tree u v)
  "Replace node U with node V in TREE."
  (cond
   ((org-splay-tree--root-p u)
    (setf (org-splay-tree--root tree) v))
   ((org-splay-tree--node-left-child-p u)
    (setf (org-splay-tree--node-left (org-splay-tree--node-parent u)) v))
   (t (setf (org-splay-tree--node-right (org-splay-tree--node-parent u)) v)))
  (when v (setf (org-splay-tree--node-parent v) (org-splay-tree--node-parent u))))

(defun org-splay-tree--subtree-min (node)
  "Find minimum in subtree with NODE root."
  (when node
    (while (org-splay-tree--node-left node)
      (setq node (org-splay-tree--node-left node))))
  node)

(defun org-splay-tree--subtree-max (node)
  "Find maximum in subtree with NODE root."
  (when node
    (while (org-splay-tree--node-right node)
      (setq node (org-splay-tree--node-right node))))
  node)

(defun org-splay-tree-find (tree data &optional roots starting-node parentp)
  "Find exact DATA node in TREE.

When PARENTP is non-nil and we cannot find exact match, return last
PARENT.

When STARTING-NODE is non-nil, search within STARTING-NODE subtree.
Accumulate power 2 level nodes into ROOTS.
See http://dx.doi.org/10.1007/978-3-642-03367-4_18 Jonathan
C. Derryberry, Daniel D. Sleator [Springer Berlin Heidelberg] \(2009)
Skip-Splay: Toward Achieving the Unified Bound in the BST Model."
  (or (and (not starting-node) (gethash data (org-splay-tree--hash tree)))
      (let ((node (or starting-node (org-splay-tree--root tree)))
            (level 0) (pow2 1) parent)
        (when node
          (catch :found
            (when (and (org-splay-tree--node-parent node)
                       (org-splay-tree--node-left-child-p node)
                       ;; parent < data.
                       (funcall (org-splay-tree--cmpfun tree)
                                (org-splay-tree--node-data (org-splay-tree--node-parent node))
                                data))
              (setq parent (org-splay-tree--node-parent node))
              (setq node nil))
            (while node
              (setq parent node)
              (cl-incf level)
              (when (= level pow2)
                (push node roots)
                (setq pow2 (* 2 pow2)))
              (cond
               ;; node < data
               ((funcall (org-splay-tree--cmpfun tree) (org-splay-tree--node-data node) data)
                (setf node (org-splay-tree--node-right node)))
               ;; data < node.
               ((funcall (org-splay-tree--cmpfun tree) data (org-splay-tree--node-data node))
                (setf node (org-splay-tree--node-left node)))
               ;; data = node.
               (t (throw :found node))))
            (when parentp parent))))))

(defun org-splay-tree-next-node (node)
  "Find the TREE node right after NODE."
  (if (org-splay-tree--node-right node)
      (setq node (org-splay-tree--subtree-min (org-splay-tree--node-right node)))
    (while (and node
                (org-splay-tree--node-parent node)
                (org-splay-tree--node-right-child-p node))
      (setq node (org-splay-tree--node-parent node)))
    (when node (setq node (org-splay-tree--node-parent node)))))

(defun org-splay-tree-previous-node (node)
  "Find the TREE node right before NODE."
  (if (org-splay-tree--node-left node)
      (setq node (org-splay-tree--subtree-max (org-splay-tree--node-left node)))
    (while (and node
                (org-splay-tree--node-parent node)
                (org-splay-tree--node-left-child-p node))
      (setq node (org-splay-tree--node-parent node)))
    (when node (setq node (org-splay-tree--node-parent node)))))

(defun org-splay-tree-enter (tree data)
  "Put DATA into splay TREE."
  (let* ((node (org-splay-tree--node-create data))
         (roots nil)
         (parent (org-splay-tree-find tree data roots nil 'parent)))
    (setf (org-splay-tree--node-parent node) parent)
    (cond
     ((not parent) (setf (org-splay-tree--root tree) node))
     ((funcall (org-splay-tree--cmpfun tree) (org-splay-tree--node-data parent) data)
      (setf (org-splay-tree--node-right parent) node))
     (t (setf (org-splay-tree--node-left parent) node)))
    (org-splay-tree--splay tree node roots)
    (cl-incf (org-splay-tree--size tree))
    (unless (gethash data (org-splay-tree--hash tree))
      (puthash data node (org-splay-tree--hash tree)))))

(defun org-splay-tree-delete (tree node)
  "Remove NODE from TREE."
  (let* ((roots nil)
         (node (org-splay-tree-find tree node roots)))
    (when node
      (org-splay-tree--splay tree node roots)
      (cond
       ((not (org-splay-tree--node-left node))
        (org-splay-tree--replace tree node (org-splay-tree--node-right node)))
       ((not (org-splay-tree--node-right node))
        (org-splay-tree--replace tree node (org-splay-tree--node-left node)))
       (t
        (let ((nxt (org-splay-tree--subtree-min (org-splay-tree--node-right node))))
          (unless (eq (org-splay-tree--node-parent nxt) node)
            (org-splay-tree--replace tree nxt (org-splay-tree--node-right nxt))
            (setf (org-splay-tree--node-right nxt) (org-splay-tree--node-right node))
            (setf (org-splay-tree--node-parent (org-splay-tree--node-right nxt)) nxt))
          (org-splay-tree--replace tree node nxt)
          (setf (org-splay-tree--node-left nxt) (org-splay-tree--node-left node))
          (setf (org-splay-tree--node-parent (org-splay-tree--node-left nxt)) nxt))))
      (when (eq node (gethash (org-splay-tree--node-data node) (org-splay-tree--hash tree)))
        (remhash (org-splay-tree--node-data node) (org-splay-tree--hash tree)))
      ;; Clear the node to signal, e.g. that iter is gone.
      (setf (org-splay-tree--node-parent node) nil)
      (setf (org-splay-tree--node-left node) nil)
      (setf (org-splay-tree--node-right node) nil)
      (cl-decf (org-splay-tree--size tree)))))

(cl-defun org-splay-tree-print (node &optional (level 1))
  "Print Org representation of subtree of NODE.
Start from headline LEVEL."
  (when (org-splay-tree-p node) (setq node (org-splay-tree--root node)))
  (insert (concat (make-string level ?*)
                  " "
                  (prin1-to-string (and node (org-splay-tree--node-data node)))
                  "\n"))
  (when node
    (org-splay-tree-print (org-splay-tree--node-left node) (+ 1 level))
    (org-splay-tree-print (org-splay-tree--node-right node) (+ 1 level))))

(iter-defun org-splay-tree-iter (tree &optional from start-node)
  "Return splay tree iterator object.

Calling `iter-next' on this object will retrieve the next element
from TREE.
Calling `iter-next' with optional argument will make the next call
retrieve a next element larger or equal then the provided argument.

Optional argument FROM will make the iterator start from a node larger
or equal than FROM.

Optional argument START-NODE will make the iterator start from that
node.  When both FROM and START-NODE arguments are provided, the
iterator will start from first node after or at START-NODE that is
larger or equal than FROM.

This iterator is stable against tree modifications.  If the intended
next value is the tree gets deleted, the iterator continues from the
subsequent element."
  (let ((node (cond
               (from (org-splay-tree-find tree from nil nil 'parent))
               (start-node)
               (t (org-splay-tree--subtree-min (org-splay-tree--root tree)))))
        (jump-to from))
    (while node
      (when (and jump-to
                 ;; node < jump-to.
                 (funcall (org-splay-tree--cmpfun tree)
                          (org-splay-tree--node-data node)
                          jump-to))
        (setq node (org-splay-tree-find tree jump-to nil node 'parent)))
      (unless (and jump-to
                   ;; Still node < jump-to.
                   ;; Node got updated compared to the above.
                   (funcall (org-splay-tree--cmpfun tree)
                            (org-splay-tree--node-data node)
                            jump-to))
        (setq jump-to (iter-yield (org-splay-tree--node-data node))))
      (if (and (not (org-splay-tree--node-parent node))
               (not (org-splay-tree--node-left node))
               (not (org-splay-tree--node-right node))
               (not (eq node (org-splay-tree--root tree))))
          ;; Deleted node.  Restart.
          (when node
            (setq jump-to (org-splay-tree--node-data node)
                  node (org-splay-tree-find tree (org-splay-tree--node-data node) nil nil 'parent)))
        (setq node (org-splay-tree-next-node node))))))

(defun org-splay-tree-mapcar (fun tree)
  "Apply FUN to all elements of splay TREE; return list of the results."
  (let (result last)
    (iter-do (data (org-splay-tree-iter tree))
      (if result
          (progn
            ;; Trick to avoid calling extra `nreverse' with O(N)
            ;; complexity.
            (setcdr last (list (funcall fun data)))
            (setq last (cdr last)))
        (push (funcall fun data) result)
        (setq last result)))
    result))

(defun org-splay-tree-mapc (fun tree)
  "Apply function FUN to all elements in splay TREE, for side-effect only.

FUNCTION is applied to the elements in ascending order."
  (iter-do (data (org-splay-tree-iter tree))
    (funcall fun data)))

(provide 'org-splay-tree)
;;; org-splay-tree.el ends here
