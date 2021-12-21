;;; org-skip-list.el --- Skip list                   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Ihor Radchenko

;; Author: Ihor Radchenko <yantar92@gmail.com>
;; Keywords: extensions, data structures, skip list

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

;; A skip list is a probabilitic ordered data structure comparable
;; with binary trees.  Inserting, deleting, and retrieveing data from
;; a skip list containing n elements is O(log n).  Compared to
;; non-randomized balanced trees, skip lists have slightly larger
;; retrieval constant, but faster insertion/deletion times.  Skip
;; lists may be a better alternative to Elisp AVL-tree implementation
;; when the data is a subject of frequent changes.

;; Cached Org buffer representations are a subject of frequent change
;; (on every buffer edit) and thus skip trees are a better data
;; structure to be used instead of AVL trees.

;; Basic description and analysis of skip lists can be found in
;; original paper by Willian Pugh (DOI: 10.1007/3-540-51542-9_36)
;; W Pugh [Springer Berlin Heidelberg] (1989) A Probabilistic
;; Alternative to Balanced Trees.
;; The paper pdf is available for free at
;; https://link.springer.com/content/pdf/10.1007%2F3-540-51542-9_36.pdf

;; Internally, a skip list consist of a header node, MaxLevel of the
;; list, and the comparison function.

;; Each node of the skip list is a cons cell with its car containing
;; data and cdr is a vector of forward pointers to following list
;; elements.  For each node the forward vector size is randomly
;; assigned up to MaxLevel.  Forward[k] pointer always references
;; another list node with larger or equal forward vector size.  Please
;; refer to Figure 3 of the Pugh's paper for a nice schematic.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'generator)

;; Each node is a cons of (data . forward vector).
;; Nodes must use as little memory as possible because there may
;; potentially be a lot of them.

(defmacro org-skip-list--head-p (node)
  "Check if NODE is a head node."
  `(eq 'org-skip-list--head (org-skip-list--node-data ,node)))

(defmacro org-skip-list--node-p (node)
  "Return non-nil when NODE is a skip list node."
  (and (consp node)
       (car node)
       (vectorp (cdr node))))

(defmacro org-skip-list--node-create (data level)
  "Create a new LEVEL node conaining DATA."
  `(cons ,data (make-vector ,level nil)))

(defmacro org-skip-list--node-data (node)
  "Get NDOE data."
  `(car ,node))

(defmacro org-skip-list--node-forward (node)
  "Get forward vector for NODE."
  `(cdr ,node))

(defmacro org-skip-list-car (node)
  "Get data value of the NODE."
  `(org-skip-list--node-data ,node))

(cl-defmacro org-skip-list-cdr (node &optional (level 0))
  "Return a node after NODE at LEVEL."
  `(aref (org-skip-list--node-forward ,node) ,level))

(cl-defmacro org-skip-list-cadr (node &optional (level 0))
  "Return a node value for node after NODE at LEVEL."
  `(org-skip-list-car (org-skip-list-cdr ,node ,level)))

(cl-defstruct (org-skip-list-
               :named
               (:type vector)
               (:constructor nil)
               (:constructor org-skip-list--create ( cmpfun
                                        &key
                                        maxlevel
                                        probability-constant
                                        &aux
                                        (header (org-skip-list--node-create 'org-skip-list--head maxlevel))))
               (:predicate org-skip-list-p)
               (:copier nil))
  (maxlevel 16)
  (level 1)
  ;; See Pugh's paper. We optimise for search constant here.
  ;; Decreasing the constant will degrade search constant and decrease
  ;; memory requirement. A value of 1/16 will make search 2.12x slower
  ;; but use up ~50% less memory.
  ;; The default value here is 1/e, which gives best expected
  ;; asymptotic search times for large (>5000) number of list
  ;; elements.
  (probability-constant 0.367879441171)
  ;; This should give a much smaller memory overheads. Though we do
  ;; not care about them when data stored in the list is much larger
  ;; compared to forward vector size (vector size is 1x-2x of number
  ;; of nodes, which is negligible compared to typical org-element
  ;; sizes).  (probability-constant (/ 1.0 8))
  header
  cmpfun)

(defalias 'org-skip-list-create #'org-skip-list--create
  "Create an empty skip list.
CMPFUN is a function which takes two arguments, A and B, and returns
non-nil if A is less than B, and nil otherwise.
PROBABILITY-CONSTANT defines distribution of list node levels.
Decreasing the constant makes search slower, but uses up less memory.
See Pugh's paper (DOI: 10.1007/3-540-51542-9_36) for details. The
default is 1/e.
MAXIMUM-LEVEL is maximum possible list node level bounding maximum
number of elements in the list (maximum number of elements is
PROBABILITY-CONSTANT^MAXIMUM-LEVEL)). By default MAXIMUM-LEVEL is 16
allowing up to 8.8E9 elements.
\(fn CMPFUN &optional MAXLEVEL PROBABILITY-CONSTANT)")

(defun org-skip-list--get-level (skiplist)
  "Generate level for a new node.
See Pugh's paper (DOI: 10.1007/3-540-51542-9_36)."
  (let ((level 1)
        (threshold (* #x40000000 (org-skip-list--probability-constant skiplist))))
    (while (and (< (random #x40000000) threshold)
                (< level (org-skip-list--maxlevel skiplist)))
      (cl-incf level))
    level))

(defun org-skip-list--find-before (skiplist data &optional update)
  "Find an element in SKIPLIST strictly smaller than DATA.
An optional argument UPDATE should be a vector of SKIPLIST maxlevel
length. The vector will be modified for side-effects storing nodes
where DATA is splicing the node forward references."
  (cl-loop for idx from (1- (org-skip-list--level skiplist)) downto 0
           with node = (org-skip-list--header skiplist)
           do
           (while (and (org-skip-list-cdr node idx)
                       (funcall (org-skip-list--cmpfun skiplist)
                                (org-skip-list-cadr node idx)
                                data))
             (setq node (org-skip-list-cdr node idx)))
           (when update (aset update idx node))
           finally return node))

(defun org-skip-list-insert (skiplist data)
  "Insert DATA into SKIPLIST.
If data is equal with the existing node according to CMPFUN, replace
that node's data field with DATA."
  (let* ((update (make-vector (org-skip-list--maxlevel skiplist) nil))
         (node-before (org-skip-list--find-before skiplist data update)))
    ;; DATA key should not be equal to existing list element.
    (unless (equal (org-skip-list-cadr node-before) data)
      (let ((level (org-skip-list--get-level skiplist))
            (idx   (org-skip-list--level skiplist))
            ;; Terminating the process can garble the list structure.
            (inhibit-quit t))
        (when (> level idx) ; idx=(org-skip-list--level skiplist)
          (while (< idx level)
            (aset update idx (org-skip-list--header skiplist))
            (cl-incf idx))
          (setf (org-skip-list--level skiplist) level))
        (let ((node (org-skip-list--node-create data level)))
          (setq idx 0)
          (while (< idx level)
            (setf (org-skip-list-cdr node idx)
                  (org-skip-list-cdr (aref update idx) idx))
            (setf (org-skip-list-cdr (aref update idx) idx)
                  node)
            (cl-incf idx))
          ;; Return the inserted data.
          (org-skip-list-car node))))))

(defun org-skip-list-remove (skiplist data)
  "Remove element matching DATA from SKIPLIST.
Return non-nil when operation actually deletes an element."
  (let* ((update (make-vector (org-skip-list--maxlevel skiplist) nil))
         (node (org-skip-list-cdr (org-skip-list--find-before skiplist data update)))
         (idx 0)
         (skip-list-level (org-skip-list--level skiplist))
         ;; Terminating the process can garble the list structure.
         (inhibit-quit t))
    (when (equal (org-skip-list-car node) data)
      (while (and (< idx skip-list-level)
                  (eq node (org-skip-list-cdr (aref update idx) idx)))
        (setf (org-skip-list-cdr (aref update idx) idx)
              (org-skip-list-cdr node idx))
        (cl-incf idx))
      (while (and (> (org-skip-list--level skiplist) 1)
                  (not (org-skip-list-cdr (org-skip-list--header skiplist) (1- (org-skip-list--level skiplist)))))
        (cl-decf (org-skip-list--level skiplist)))
      t)))

(defun org-skip-list-flatten (skiplist)
  "Return a sorted list contnaining all elements of SKIPLIST."
  (cl-loop with node = (org-skip-list--header skiplist)
           do (setq node (org-skip-list-cdr node))
           while node
           collect (org-skip-list-car node)))

(defun org-skip-list-find (skiplist data &optional nilflag)
  "Find a node matching DATA in SKIPLIST.
Return NILFLAG if DATA is not in the SKIPLIST."
  (let ((node (org-skip-list-cdr (org-skip-list--find-before skiplist data nil))))
    (if (or (not node)
            (not (equal data (org-skip-list-car node))))
        nilflag
      node)))

(defun org-skip-list-find-leq (skiplist data &optional nilflag)
  "Find a node that is less or equal than DATA in SKIPLIST.
Return NILFLAG if SKIPLIST does not contain such nodes."
  (let ((node (org-skip-list--find-before skiplist data)))
    (cond
     ((equal data (org-skip-list-cadr node))
      (org-skip-list-cdr node))
     ((org-skip-list--head-p node) nilflag)
     (t node))))

(defun org-skip-list-find-geq (skiplist data &optional nilflag)
  "Find first node that is greater or equal than DATA in SKIPLIST.
Return NILFLAG if SKIPLIST does not contain such nodes."
  (let ((node (org-skip-list--find-before skiplist data)))
    (or (org-skip-list-cdr node) nilflag)))

(defun org-skip-list-find-before (skiplist data &optional nilflag)
  "Find a node that is before DATA in SKIPLIST.
Return NILFLAG if SKIPLIST does not contain such nodes."
  (let ((node (org-skip-list--find-before skiplist data)))
    (if (org-skip-list--head-p node) nilflag node)))

(defun org-skip-list-length (skiplist)
  "Return length of SKIPLIST."
  (cl-loop with node = (org-skip-list-cdr (org-skip-list--header skiplist))
           with size = 0
           while node
           do (setq node (org-skip-list-cdr node))
           do (cl-incf size)
           finally return size))

(defmacro org-skip-list-first (skiplist)
  "Return first node in SKIPLIST."
  `(org-skip-list-cdr (org-skip-list--header ,skiplist)))

(iter-defun org-skip-list-iter (skiplist &optional start node-iter?)
  "Return skip list iterator object.

Optional argument START will make the iterator begin from first
element at or after START.  Non-nil optional argument NODE-ITER? will
make the iterator yield skip list node instead of the node value."
  (let ((current-node (org-skip-list-first skiplist)))
    (while current-node
      (when start
        (if (org-skip-list--node-p start)
            (setq current-node start)
          (setq current-node (org-skip-list-find-before skiplist start))))
      (when current-node
        (setq start (iter-yield
                     (if node-iter?
                         current-node
                       (org-skip-list--node-data current-node))))
        (setq current-node (org-skip-list-cdr current-node))))))

(defun org-skip-list-size (slist)
  "Return size of skip list SLIST."
  (let ((elem (org-skip-list-first slist))
        (size 0))
    (while elem
      (cl-incf size)
      (setq elem (org-skip-list-cdr elem)))
    size))

(defun org-skip-list-verify (slist)
  "Assert SLIST consistency."
  (let ((header (org-skip-list--header slist)))
    (cl-loop for level from (1- (length (org-skip-list--node-forward header))) downto 0
	     do
             (let ((node (org-skip-list-cdr header level)))
               (when (>= level (org-skip-list--level slist))
                 (cl-assert
                  (not (org-skip-list-cdr header level))
                  t "Forward references above list level"))
               (while node
		 (when (org-skip-list-cdr node level)
                   (cl-assert
                    (funcall (org-skip-list--cmpfun slist)
                             (org-skip-list-car node)
                             (org-skip-list-cadr node level)))
                   t "Wrong element order")
                 (setq node (org-skip-list-cdr node level)))))))

(provide 'org-skip-list)
;;; org-skip-list.el ends here
