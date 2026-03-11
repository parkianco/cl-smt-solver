;;;; theory.lisp - SMT Theory Solvers
;;;;
;;;; Implements theory solvers for Boolean, Integer (LIA), and Bitvector theories.

(in-package #:cl-smt-solver)

;;; ============================================================================
;;; Theory Registry
;;; ============================================================================

(defparameter *supported-theories*
  '(:qf-uf       ; Quantifier-free uninterpreted functions
    :qf-lia      ; Quantifier-free linear integer arithmetic
    :qf-lra      ; Quantifier-free linear real arithmetic
    :qf-bv       ; Quantifier-free bitvectors
    :qf-auflia   ; Quantifier-free arrays + UF + LIA
    :qf-abv      ; Quantifier-free arrays + bitvectors
    :lia         ; Linear integer arithmetic (with quantifiers)
    :bv          ; Bitvectors (with quantifiers)
    :all)        ; All theories
  "List of supported SMT theories.")

;;; ============================================================================
;;; Theory Context
;;; ============================================================================

(defstruct (theory-context (:conc-name tc-))
  "Context for theory reasoning."
  (logic :qf-lia :type keyword)
  (constants nil :type list)        ; ((name . type) ...)
  (functions nil :type list)        ; ((name params ret-type body) ...)
  (assertions nil :type list)       ; List of asserted formulas
  (assignment nil :type list)       ; Current variable assignment ((var . value) ...)
  (propagations nil :type list)     ; Theory propagations
  (conflicts nil :type list))       ; Theory conflicts

(defun make-theory-context (&key (logic :qf-lia))
  "Create a new theory context."
  (make-theory-context :logic logic))

;;; ============================================================================
;;; Theory Interface
;;; ============================================================================

(defgeneric theory-check (context formula)
  (:documentation "Check if FORMULA is consistent in the theory.
   Returns :sat, :unsat, or :unknown."))

(defgeneric theory-propagate (context)
  (:documentation "Perform theory propagation.
   Returns a list of implied literals."))

(defgeneric theory-explain (context conflict)
  (:documentation "Explain a theory conflict.
   Returns a clause that excludes the conflict."))

(defgeneric theory-model (context)
  (:documentation "Generate a model for the theory.
   Returns an alist of variable assignments."))

;;; ============================================================================
;;; Boolean Theory (Core)
;;; ============================================================================

(defun eval-bool-formula (formula assignment)
  "Evaluate a Boolean formula under an assignment.
   Returns t, nil, or :unknown if undetermined."
  (cond
    ((eq formula t) t)
    ((eq formula nil) nil)
    ((symbolp formula)
     (let ((val (alist-get formula assignment)))
       (if val val :unknown)))
    ((consp formula)
     (case (car formula)
       (:not
        (let ((v (eval-bool-formula (second formula) assignment)))
          (case v
            ((t) nil)
            ((nil) t)
            (otherwise :unknown))))
       (:and
        (let ((all-true t) (any-unknown nil))
          (dolist (arg (cdr formula))
            (let ((v (eval-bool-formula arg assignment)))
              (case v
                ((nil) (return-from eval-bool-formula nil))
                (:unknown (setf any-unknown t all-true nil))
                (otherwise nil))))
          (if all-true t :unknown)))
       (:or
        (let ((any-unknown nil))
          (dolist (arg (cdr formula))
            (let ((v (eval-bool-formula arg assignment)))
              (case v
                ((t) (return-from eval-bool-formula t))
                (:unknown (setf any-unknown t))
                (otherwise nil))))
          (if any-unknown :unknown nil)))
       (:implies
        (let ((a (eval-bool-formula (second formula) assignment))
              (b (eval-bool-formula (third formula) assignment)))
          (cond
            ((eq a nil) t)
            ((eq b t) t)
            ((and (eq a t) (eq b nil)) nil)
            (t :unknown))))
       (:iff
        (let ((a (eval-bool-formula (second formula) assignment))
              (b (eval-bool-formula (third formula) assignment)))
          (cond
            ((and (eq a :unknown) (eq b :unknown)) :unknown)
            ((eq a :unknown) :unknown)
            ((eq b :unknown) :unknown)
            (t (eq a b)))))
       (:ite
        (let ((c (eval-bool-formula (second formula) assignment)))
          (case c
            ((t) (eval-bool-formula (third formula) assignment))
            ((nil) (eval-bool-formula (fourth formula) assignment))
            (otherwise :unknown))))
       (otherwise :unknown)))
    (t :unknown)))

;;; ============================================================================
;;; Integer Theory (Linear Integer Arithmetic)
;;; ============================================================================

(defun eval-int-formula (formula assignment)
  "Evaluate an integer formula under an assignment.
   Returns a numeric value or :unknown."
  (cond
    ((integerp formula) formula)
    ((rationalp formula) formula)
    ((symbolp formula)
     (let ((val (alist-get formula assignment)))
       (if (numberp val) val :unknown)))
    ((consp formula)
     (case (car formula)
       (:add
        (let ((sum 0))
          (dolist (arg (cdr formula))
            (let ((v (eval-int-formula arg assignment)))
              (if (eq v :unknown)
                  (return-from eval-int-formula :unknown)
                  (incf sum v))))
          sum))
       (:sub
        (if (= (length (cdr formula)) 1)
            ;; Negation
            (let ((v (eval-int-formula (second formula) assignment)))
              (if (eq v :unknown) :unknown (- v)))
            ;; Subtraction
            (let ((result (eval-int-formula (second formula) assignment)))
              (when (eq result :unknown)
                (return-from eval-int-formula :unknown))
              (dolist (arg (cddr formula))
                (let ((v (eval-int-formula arg assignment)))
                  (if (eq v :unknown)
                      (return-from eval-int-formula :unknown)
                      (decf result v))))
              result)))
       (:neg
        (let ((v (eval-int-formula (second formula) assignment)))
          (if (eq v :unknown) :unknown (- v))))
       (:mul
        (let ((product 1))
          (dolist (arg (cdr formula))
            (let ((v (eval-int-formula arg assignment)))
              (if (eq v :unknown)
                  (return-from eval-int-formula :unknown)
                  (setf product (* product v)))))
          product))
       (:div
        (let ((a (eval-int-formula (second formula) assignment))
              (b (eval-int-formula (third formula) assignment)))
          (cond
            ((or (eq a :unknown) (eq b :unknown)) :unknown)
            ((zerop b) :unknown)  ; Division by zero
            (t (truncate a b)))))
       (:mod
        (let ((a (eval-int-formula (second formula) assignment))
              (b (eval-int-formula (third formula) assignment)))
          (cond
            ((or (eq a :unknown) (eq b :unknown)) :unknown)
            ((zerop b) :unknown)
            (t (mod a b)))))
       (:abs
        (let ((v (eval-int-formula (second formula) assignment)))
          (if (eq v :unknown) :unknown (abs v))))
       (otherwise :unknown)))
    (t :unknown)))

(defun eval-int-comparison (formula assignment)
  "Evaluate an integer comparison formula."
  (let ((op (car formula))
        (args (mapcar (lambda (arg) (eval-int-formula arg assignment))
                      (cdr formula))))
    (if (member :unknown args)
        :unknown
        (case op
          (:eq (apply #'= args))
          (:lt (< (first args) (second args)))
          (:le (<= (first args) (second args)))
          (:gt (> (first args) (second args)))
          (:ge (>= (first args) (second args)))
          (:distinct (let ((vals args))
                       (= (length vals) (length (remove-duplicates vals)))))
          (otherwise :unknown)))))

;;; ============================================================================
;;; Bitvector Theory
;;; ============================================================================

(defun eval-bv-formula (formula assignment width)
  "Evaluate a bitvector formula under an assignment.
   WIDTH is the bitvector width. Returns an integer or :unknown."
  (cond
    ((integerp formula)
     (truncate-to-width formula width))
    ((symbolp formula)
     (let ((val (alist-get formula assignment)))
       (if (integerp val)
           (truncate-to-width val width)
           :unknown)))
    ((consp formula)
     (case (car formula)
       (:bvand
        (let ((a (eval-bv-formula (second formula) assignment width))
              (b (eval-bv-formula (third formula) assignment width)))
          (if (or (eq a :unknown) (eq b :unknown))
              :unknown
              (logand a b))))
       (:bvor
        (let ((a (eval-bv-formula (second formula) assignment width))
              (b (eval-bv-formula (third formula) assignment width)))
          (if (or (eq a :unknown) (eq b :unknown))
              :unknown
              (logior a b))))
       (:bvxor
        (let ((a (eval-bv-formula (second formula) assignment width))
              (b (eval-bv-formula (third formula) assignment width)))
          (if (or (eq a :unknown) (eq b :unknown))
              :unknown
              (logxor a b))))
       (:bvnot
        (let ((a (eval-bv-formula (second formula) assignment width)))
          (if (eq a :unknown)
              :unknown
              (truncate-to-width (lognot a) width))))
       (:bvadd
        (let ((a (eval-bv-formula (second formula) assignment width))
              (b (eval-bv-formula (third formula) assignment width)))
          (if (or (eq a :unknown) (eq b :unknown))
              :unknown
              (truncate-to-width (+ a b) width))))
       (:bvsub
        (let ((a (eval-bv-formula (second formula) assignment width))
              (b (eval-bv-formula (third formula) assignment width)))
          (if (or (eq a :unknown) (eq b :unknown))
              :unknown
              (truncate-to-width (- a b) width))))
       (:bvmul
        (let ((a (eval-bv-formula (second formula) assignment width))
              (b (eval-bv-formula (third formula) assignment width)))
          (if (or (eq a :unknown) (eq b :unknown))
              :unknown
              (truncate-to-width (* a b) width))))
       (:bvudiv
        (let ((a (eval-bv-formula (second formula) assignment width))
              (b (eval-bv-formula (third formula) assignment width)))
          (cond
            ((or (eq a :unknown) (eq b :unknown)) :unknown)
            ((zerop b) (mask width))  ; Division by zero returns all 1s
            (t (truncate a b)))))
       (:bvsdiv
        (let ((a (eval-bv-formula (second formula) assignment width))
              (b (eval-bv-formula (third formula) assignment width)))
          (cond
            ((or (eq a :unknown) (eq b :unknown)) :unknown)
            ((zerop b) :unknown)
            (t (let ((sa (signed-value a width))
                     (sb (signed-value b width)))
                 (unsigned-value (truncate sa sb) width))))))
       (:bvurem
        (let ((a (eval-bv-formula (second formula) assignment width))
              (b (eval-bv-formula (third formula) assignment width)))
          (cond
            ((or (eq a :unknown) (eq b :unknown)) :unknown)
            ((zerop b) a)
            (t (rem a b)))))
       (:bvsrem
        (let ((a (eval-bv-formula (second formula) assignment width))
              (b (eval-bv-formula (third formula) assignment width)))
          (cond
            ((or (eq a :unknown) (eq b :unknown)) :unknown)
            ((zerop b) :unknown)
            (t (let ((sa (signed-value a width))
                     (sb (signed-value b width)))
                 (unsigned-value (rem sa sb) width))))))
       (:bvshl
        (let ((a (eval-bv-formula (second formula) assignment width))
              (b (eval-bv-formula (third formula) assignment width)))
          (if (or (eq a :unknown) (eq b :unknown))
              :unknown
              (truncate-to-width (ash a (min b width)) width))))
       (:bvlshr
        (let ((a (eval-bv-formula (second formula) assignment width))
              (b (eval-bv-formula (third formula) assignment width)))
          (if (or (eq a :unknown) (eq b :unknown))
              :unknown
              (ash a (- (min b width))))))
       (:bvashr
        (let ((a (eval-bv-formula (second formula) assignment width))
              (b (eval-bv-formula (third formula) assignment width)))
          (if (or (eq a :unknown) (eq b :unknown))
              :unknown
              (let ((signed-a (signed-value a width))
                    (shift (min b width)))
                (unsigned-value (ash signed-a (- shift)) width)))))
       (otherwise :unknown)))
    (t :unknown)))

(defun eval-bv-comparison (formula assignment width)
  "Evaluate a bitvector comparison."
  (let ((op (car formula))
        (a (eval-bv-formula (second formula) assignment width))
        (b (eval-bv-formula (third formula) assignment width)))
    (if (or (eq a :unknown) (eq b :unknown))
        :unknown
        (case op
          (:bvult (< a b))
          (:bvule (<= a b))
          (:bvugt (> a b))
          (:bvuge (>= a b))
          (:bvslt (< (signed-value a width) (signed-value b width)))
          (:bvsle (<= (signed-value a width) (signed-value b width)))
          (:bvsgt (> (signed-value a width) (signed-value b width)))
          (:bvsge (>= (signed-value a width) (signed-value b width)))
          (:eq (= a b))
          (otherwise :unknown)))))

;;; ============================================================================
;;; Combined Theory Evaluation
;;; ============================================================================

(defun eval-formula (formula assignment &key (bv-width 32))
  "Evaluate a formula under an assignment.
   Returns t, nil, a number, or :unknown."
  (cond
    ((eq formula t) t)
    ((eq formula nil) nil)
    ((integerp formula) formula)
    ((symbolp formula)
     (alist-get formula assignment :unknown))
    ((consp formula)
     (let ((op (car formula)))
       (cond
         ;; Boolean operators
         ((member op '(:and :or :not :implies :iff :ite))
          (eval-bool-formula formula assignment))
         ;; Integer comparisons
         ((member op '(:lt :le :gt :ge :eq :distinct))
          (if (some (lambda (arg)
                      (and (consp arg)
                           (member (car arg) '(:bvand :bvor :bvxor :bvnot
                                               :bvadd :bvsub :bvmul))))
                    (cdr formula))
              (eval-bv-comparison formula assignment bv-width)
              (eval-int-comparison formula assignment)))
         ;; Integer arithmetic
         ((member op '(:add :sub :mul :div :mod :neg :abs))
          (eval-int-formula formula assignment))
         ;; Bitvector operations
         ((member op '(:bvand :bvor :bvxor :bvnot :bvadd :bvsub :bvmul
                       :bvudiv :bvsdiv :bvurem :bvsrem :bvshl :bvlshr :bvashr))
          (eval-bv-formula formula assignment bv-width))
         ;; Bitvector comparisons
         ((member op '(:bvult :bvule :bvugt :bvuge :bvslt :bvsle :bvsgt :bvsge))
          (eval-bv-comparison formula assignment bv-width))
         (otherwise :unknown))))
    (t :unknown)))

;;; ============================================================================
;;; Theory Consistency Checking
;;; ============================================================================

(defun check-theory-consistency (formulas assignment &key (logic :qf-lia))
  "Check if formulas are consistent with the assignment in the given theory.
   Returns :sat, :unsat, or :unknown."
  (dolist (formula formulas)
    (let ((result (eval-formula formula assignment)))
      (cond
        ((eq result nil) (return-from check-theory-consistency :unsat))
        ((eq result :unknown) (return-from check-theory-consistency :unknown)))))
  :sat)
