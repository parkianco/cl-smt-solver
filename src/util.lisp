;;;; util.lisp - Utility functions for SMT solver
;;;;
;;;; Helper functions and data structures used throughout the solver.

(in-package #:cl-smt-solver)

;;; ============================================================================
;;; Bit manipulation utilities
;;; ============================================================================

(defun bit-width (n)
  "Compute the number of bits needed to represent unsigned integer N."
  (if (zerop n) 1 (integer-length n)))

(defun mask (width)
  "Create a bitmask of WIDTH bits (all 1s)."
  (1- (ash 1 width)))

(defun truncate-to-width (value width)
  "Truncate VALUE to fit in WIDTH bits (unsigned)."
  (logand value (mask width)))

(defun sign-extend (value from-width to-width)
  "Sign-extend VALUE from FROM-WIDTH bits to TO-WIDTH bits."
  (if (logbitp (1- from-width) value)
      ;; Negative: extend with 1s
      (logior value (ash (mask (- to-width from-width)) from-width))
      ;; Positive: already correct
      value))

(defun signed-value (value width)
  "Interpret unsigned VALUE as a signed integer of WIDTH bits."
  (if (logbitp (1- width) value)
      (- value (ash 1 width))
      value))

(defun unsigned-value (value width)
  "Convert signed VALUE to unsigned representation of WIDTH bits."
  (truncate-to-width value width))

;;; ============================================================================
;;; Variable generation
;;; ============================================================================

(defvar *gensym-counter* 0
  "Counter for generating unique variable names.")

(defun make-fresh-var (&optional (prefix "v"))
  "Generate a fresh variable symbol."
  (intern (format nil "~A~D" prefix (incf *gensym-counter*))))

(defun reset-gensym-counter ()
  "Reset the gensym counter."
  (setf *gensym-counter* 0))

;;; ============================================================================
;;; Association list utilities
;;; ============================================================================

(defun alist-get (key alist &optional default)
  "Get value associated with KEY in ALIST, or DEFAULT if not found."
  (let ((pair (assoc key alist :test #'equal)))
    (if pair (cdr pair) default)))

(defun alist-set (key value alist)
  "Return new alist with KEY mapped to VALUE."
  (cons (cons key value)
        (remove key alist :key #'car :test #'equal)))

(defun alist-remove (key alist)
  "Return new alist without KEY."
  (remove key alist :key #'car :test #'equal))

;;; ============================================================================
;;; Hash table utilities
;;; ============================================================================

(defun hash-table-to-alist (ht)
  "Convert hash table to association list."
  (let ((result nil))
    (maphash (lambda (k v) (push (cons k v) result)) ht)
    (nreverse result)))

(defun alist-to-hash-table (alist &key (test #'equal))
  "Convert association list to hash table."
  (let ((ht (make-hash-table :test test)))
    (dolist (pair alist)
      (setf (gethash (car pair) ht) (cdr pair)))
    ht))

;;; ============================================================================
;;; Set operations (using sorted lists)
;;; ============================================================================

(defun set-union (s1 s2 &key (test #'equal))
  "Compute union of two sets."
  (union s1 s2 :test test))

(defun set-intersection (s1 s2 &key (test #'equal))
  "Compute intersection of two sets."
  (intersection s1 s2 :test test))

(defun set-difference (s1 s2 &key (test #'equal))
  "Compute set difference S1 - S2."
  (set-difference s1 s2 :test test))

(defun set-subset-p (s1 s2 &key (test #'equal))
  "Check if S1 is a subset of S2."
  (every (lambda (x) (member x s2 :test test)) s1))

;;; ============================================================================
;;; Priority queue (min-heap)
;;; ============================================================================

(defstruct (priority-queue (:conc-name pq-))
  "A min-heap priority queue."
  (heap (make-array 16 :adjustable t :fill-pointer 0))
  (key-fn #'identity))

(defun pq-empty-p (pq)
  "Check if priority queue is empty."
  (zerop (length (pq-heap pq))))

(defun pq-size (pq)
  "Return number of elements in priority queue."
  (length (pq-heap pq)))

(defun pq-push (pq item)
  "Push ITEM onto priority queue."
  (let ((heap (pq-heap pq))
        (key-fn (pq-key-fn pq)))
    (vector-push-extend item heap)
    ;; Bubble up
    (loop with i = (1- (length heap))
          while (> i 0)
          for parent = (floor (1- i) 2)
          while (< (funcall key-fn (aref heap i))
                   (funcall key-fn (aref heap parent)))
          do (rotatef (aref heap i) (aref heap parent))
             (setf i parent))))

(defun pq-pop (pq)
  "Pop and return the minimum element."
  (let ((heap (pq-heap pq)))
    (when (zerop (length heap))
      (return-from pq-pop nil))
    (let ((result (aref heap 0)))
      ;; Replace root with last element
      (setf (aref heap 0) (vector-pop heap))
      (when (> (length heap) 1)
        ;; Bubble down
        (let ((key-fn (pq-key-fn pq)))
          (loop with i = 0
                for left = (1+ (* 2 i))
                for right = (+ 2 (* 2 i))
                for smallest = i
                do (when (and (< left (length heap))
                              (< (funcall key-fn (aref heap left))
                                 (funcall key-fn (aref heap smallest))))
                     (setf smallest left))
                   (when (and (< right (length heap))
                              (< (funcall key-fn (aref heap right))
                                 (funcall key-fn (aref heap smallest))))
                     (setf smallest right))
                   (if (= smallest i)
                       (return)
                       (progn
                         (rotatef (aref heap i) (aref heap smallest))
                         (setf i smallest))))))
      result)))

(defun pq-peek (pq)
  "Return the minimum element without removing it."
  (when (> (length (pq-heap pq)) 0)
    (aref (pq-heap pq) 0)))

;;; ============================================================================
;;; Debugging utilities
;;; ============================================================================

(defvar *smt-debug* nil
  "Enable debug output for SMT operations.")

(defun smt-debug (format-string &rest args)
  "Print debug message if *smt-debug* is non-nil."
  (when *smt-debug*
    (apply #'format *debug-io* format-string args)))
