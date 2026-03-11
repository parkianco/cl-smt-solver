;;;; test-smt.lisp - Tests for cl-smt-solver
;;;;
;;;; Simple test suite for the SMT solver.

(in-package #:cl-smt-solver.test)

(defvar *test-count* 0)
(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defmacro deftest (name &body body)
  "Define a test."
  `(defun ,name ()
     (format t "~&  ~A... " ',name)
     (handler-case
         (progn ,@body
                (incf *pass-count*)
                (format t "PASS~%"))
       (error (e)
         (incf *fail-count*)
         (format t "FAIL: ~A~%" e)))))

(defmacro assert-eq (expected actual)
  "Assert equality."
  `(let ((exp ,expected)
         (act ,actual))
     (unless (equal exp act)
       (error "Expected ~S but got ~S" exp act))))

(defmacro assert-true (expr)
  "Assert expression is true."
  `(unless ,expr
     (error "Expected true but got false")))

(defmacro assert-false (expr)
  "Assert expression is false."
  `(when ,expr
     (error "Expected false but got true")))

;;; ============================================================================
;;; Expression Tests
;;; ============================================================================

(deftest test-literal-creation
  (let ((lit (make-literal 42 (type-int))))
    (assert-eq :literal (expr-kind lit))
    (assert-eq 42 (expr-value lit))))

(deftest test-smt-and
  (assert-eq t (smt-and))
  (assert-eq 'x (smt-and 'x))
  (assert-eq '(:and x y) (smt-and 'x 'y))
  (assert-eq '(:and x y z) (smt-and 'x 'y 'z)))

(deftest test-smt-or
  (assert-eq nil (smt-or))
  (assert-eq 'x (smt-or 'x))
  (assert-eq '(:or x y) (smt-or 'x 'y)))

(deftest test-smt-not
  (assert-eq '(:not x) (smt-not 'x)))

(deftest test-smt-implies
  (assert-eq '(:implies a b) (smt-implies 'a 'b)))

(deftest test-formula-vars
  (assert-eq '(x y z) (formula-vars '(:and x (:or y z))))
  (assert-eq nil (formula-vars t))
  (assert-eq nil (formula-vars '(:and t nil))))

(deftest test-formula-size
  (assert-eq 1 (formula-size 'x))
  (assert-eq 3 (formula-size '(:not x)))
  (assert-eq 5 (formula-size '(:and x y))))

;;; ============================================================================
;;; Simplification Tests
;;; ============================================================================

(deftest test-simplify-not
  (assert-eq t (simplify '(:not nil)))
  (assert-eq nil (simplify '(:not t)))
  (assert-eq 'x (simplify '(:not (:not x)))))

(deftest test-simplify-and
  (assert-eq t (simplify '(:and)))
  (assert-eq 'x (simplify '(:and x)))
  (assert-eq nil (simplify '(:and x nil)))
  (assert-eq 'x (simplify '(:and t x))))

(deftest test-simplify-or
  (assert-eq nil (simplify '(:or)))
  (assert-eq 'x (simplify '(:or x)))
  (assert-eq t (simplify '(:or x t)))
  (assert-eq 'x (simplify '(:or nil x))))

(deftest test-simplify-implies
  (assert-eq t (simplify '(:implies nil x)))
  (assert-eq 'x (simplify '(:implies t x)))
  (assert-eq t (simplify '(:implies x t))))

;;; ============================================================================
;;; CNF Conversion Tests
;;; ============================================================================

(deftest test-nnf-convert
  (assert-eq 'x (nnf-convert 'x))
  (assert-eq '(:not x) (nnf-convert '(:not x)))
  (assert-eq 'x (nnf-convert '(:not (:not x))))
  ;; De Morgan: not(and(x,y)) = or(not x, not y)
  (assert-eq '(:or (:not x) (:not y)) (nnf-convert '(:not (:and x y))))
  ;; De Morgan: not(or(x,y)) = and(not x, not y)
  (assert-eq '(:and (:not x) (:not y)) (nnf-convert '(:not (:or x y)))))

(deftest test-cnf-convert-simple
  (assert-eq 'x (cnf-convert 'x))
  (assert-eq '(:and x y) (cnf-convert '(:and x y)))
  (assert-eq '(:or x y) (cnf-convert '(:or x y))))

;;; ============================================================================
;;; Theory Evaluation Tests
;;; ============================================================================

(deftest test-eval-bool-formula
  (assert-eq t (eval-bool-formula t nil))
  (assert-eq nil (eval-bool-formula nil nil))
  (assert-eq t (eval-bool-formula '(:and t t) nil))
  (assert-eq nil (eval-bool-formula '(:and t nil) nil))
  (assert-eq t (eval-bool-formula '(:or nil t) nil))
  (assert-eq nil (eval-bool-formula '(:or nil nil) nil))
  (assert-eq t (eval-bool-formula '(:not nil) nil))
  (assert-eq nil (eval-bool-formula '(:not t) nil)))

(deftest test-eval-int-formula
  (assert-eq 5 (eval-int-formula '(:add 2 3) nil))
  (assert-eq 6 (eval-int-formula '(:mul 2 3) nil))
  (assert-eq -1 (eval-int-formula '(:sub 2 3) nil))
  (assert-eq 2 (eval-int-formula '(:div 7 3) nil))
  (assert-eq 1 (eval-int-formula '(:mod 7 3) nil))
  (assert-eq 5 (eval-int-formula '(:abs -5) nil)))

(deftest test-eval-int-comparison
  (assert-eq t (eval-int-comparison '(:lt 1 2) nil))
  (assert-eq nil (eval-int-comparison '(:lt 2 1) nil))
  (assert-eq t (eval-int-comparison '(:le 1 1) nil))
  (assert-eq t (eval-int-comparison '(:gt 2 1) nil))
  (assert-eq t (eval-int-comparison '(:ge 1 1) nil))
  (assert-eq t (eval-int-comparison '(:eq 5 5) nil)))

(deftest test-eval-with-assignment
  (let ((assignment '((x . 5) (y . 3))))
    (assert-eq 8 (eval-int-formula '(:add x y) assignment))
    (assert-eq t (eval-int-comparison '(:gt x y) assignment))))

;;; ============================================================================
;;; Bitvector Tests
;;; ============================================================================

(deftest test-bv-operations
  (assert-eq #b1010 (eval-bv-formula '(:bvand 14 10) nil 8))
  (assert-eq #b1110 (eval-bv-formula '(:bvor 14 10) nil 8))
  (assert-eq #b0100 (eval-bv-formula '(:bvxor 14 10) nil 8)))

(deftest test-bv-arithmetic
  (assert-eq 5 (eval-bv-formula '(:bvadd 2 3) nil 8))
  (assert-eq 255 (eval-bv-formula '(:bvsub 2 3) nil 8))  ; Wraps around
  (assert-eq 6 (eval-bv-formula '(:bvmul 2 3) nil 8)))

(deftest test-bv-comparison
  (assert-eq t (eval-bv-comparison '(:bvult 5 10) nil 8))
  (assert-eq nil (eval-bv-comparison '(:bvult 10 5) nil 8))
  ;; Signed comparison: 255 as signed 8-bit is -1, which is < 5
  (assert-eq t (eval-bv-comparison '(:bvslt 255 5) nil 8)))

;;; ============================================================================
;;; DPLL Tests
;;; ============================================================================

(deftest test-dpll-simple-sat
  ;; (x OR y) is satisfiable
  (let* ((clauses (list (list (make-lit 'x t) (make-lit 'y t))))
         (state (init-dpll-state clauses)))
    (multiple-value-bind (result assignment)
        (dpll-solve state)
      (assert-eq :sat result)
      (assert-true (or (cdr (assoc 'x assignment))
                       (cdr (assoc 'y assignment)))))))

(deftest test-dpll-simple-unsat
  ;; (x) AND (NOT x) is unsatisfiable
  (let* ((clauses (list (list (make-lit 'x t))
                        (list (make-lit 'x nil))))
         (state (init-dpll-state clauses)))
    (assert-eq :unsat (dpll-solve state))))

(deftest test-dpll-unit-propagation
  ;; (x) AND (x OR y) should find x=true
  (let* ((clauses (list (list (make-lit 'x t))
                        (list (make-lit 'x t) (make-lit 'y t))))
         (state (init-dpll-state clauses)))
    (multiple-value-bind (result assignment)
        (dpll-solve state)
      (assert-eq :sat result)
      (assert-eq t (cdr (assoc 'x assignment))))))

;;; ============================================================================
;;; Solver Tests
;;; ============================================================================

(deftest test-solver-basic
  (with-solver (s)
    (declare-const s 'x :int)
    (assert-formula s t)
    (assert-eq :sat (check-sat s))))

(deftest test-solver-unsat
  (with-solver (s)
    (assert-formula s nil)
    (assert-eq :unsat (check-sat s))))

(deftest test-solver-push-pop
  (with-solver (s)
    (assert-formula s 'a)
    (solver-push s)
    (assert-formula s '(:not a))
    (assert-eq :unsat (check-sat s))
    (solver-pop s)
    (assert-eq :sat (check-sat s))))

(deftest test-solve-function
  (multiple-value-bind (result model)
      (solve '(:and p q))
    (assert-eq :sat result)
    (assert-true model)))

(deftest test-is-satisfiable
  (assert-true (is-satisfiable '(:or p q)))
  (assert-false (is-satisfiable '(:and p (:not p)))))

(deftest test-is-valid
  (assert-true (is-valid '(:or p (:not p))))
  (assert-false (is-valid '(:and p q))))

(deftest test-check-implication
  (assert-true (check-implication '(:and p q) 'p))
  (assert-false (check-implication 'p '(:and p q))))

(deftest test-check-equivalence
  (assert-true (check-equivalence 'p 'p))
  (assert-true (check-equivalence '(:not (:not p)) 'p))
  (assert-false (check-equivalence 'p 'q)))

(deftest test-find-counterexample
  (multiple-value-bind (found model)
      (find-counterexample '(:and p (:not p)))
    (assert-false found))
  (multiple-value-bind (found model)
      (find-counterexample '(:implies p q))
    (assert-true found)))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-tests ()
  "Run all tests."
  (setf *test-count* 0
        *pass-count* 0
        *fail-count* 0)
  (format t "~&Running cl-smt-solver tests...~%~%")

  ;; Expression tests
  (format t "Expression tests:~%")
  (test-literal-creation)
  (test-smt-and)
  (test-smt-or)
  (test-smt-not)
  (test-smt-implies)
  (test-formula-vars)
  (test-formula-size)

  ;; Simplification tests
  (format t "~%Simplification tests:~%")
  (test-simplify-not)
  (test-simplify-and)
  (test-simplify-or)
  (test-simplify-implies)

  ;; CNF tests
  (format t "~%CNF conversion tests:~%")
  (test-nnf-convert)
  (test-cnf-convert-simple)

  ;; Theory tests
  (format t "~%Theory evaluation tests:~%")
  (test-eval-bool-formula)
  (test-eval-int-formula)
  (test-eval-int-comparison)
  (test-eval-with-assignment)

  ;; Bitvector tests
  (format t "~%Bitvector tests:~%")
  (test-bv-operations)
  (test-bv-arithmetic)
  (test-bv-comparison)

  ;; DPLL tests
  (format t "~%DPLL tests:~%")
  (test-dpll-simple-sat)
  (test-dpll-simple-unsat)
  (test-dpll-unit-propagation)

  ;; Solver tests
  (format t "~%Solver tests:~%")
  (test-solver-basic)
  (test-solver-unsat)
  (test-solver-push-pop)
  (test-solve-function)
  (test-is-satisfiable)
  (test-is-valid)
  (test-check-implication)
  (test-check-equivalence)
  (test-find-counterexample)

  ;; Summary
  (format t "~%~%========================================~%")
  (format t "Results: ~D passed, ~D failed~%"
          *pass-count* *fail-count*)
  (format t "========================================~%")

  (zerop *fail-count*))
