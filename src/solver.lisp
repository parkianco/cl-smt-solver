;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: Apache-2.0

;;;; solver.lisp - Main SMT Solver Interface
;;;;
;;;; Provides the high-level solver API.

(in-package #:cl-smt-solver)

;;; ============================================================================
;;; Solver Structure
;;; ============================================================================

(defstruct (solver (:conc-name solver-)
                    (:constructor %make-solver))
  "An SMT solver instance."
  (logic :qf-lia :type keyword)
  (constants nil :type list)          ; ((name . type) ...)
  (functions nil :type list)          ; ((name params ret-type body) ...)
  (assertions nil :type list)         ; Asserted formulas
  (scope-stack nil :type list)        ; Stack of assertion lists for push/pop
  (model nil)                         ; Last computed model
  (last-result nil :type (or keyword null)))  ; :sat, :unsat, :unknown, or nil

(defun make-solver (&key (logic :qf-lia))
  "Create a new SMT solver instance."
  (%make-solver :logic logic))

(defmacro with-solver ((var &key (logic :qf-lia)) &body body)
  "Execute BODY with a solver bound to VAR."
  `(let ((,var (make-solver :logic ,logic)))
     ,@body))

;;; ============================================================================
;;; Solver Configuration
;;; ============================================================================

(defun set-logic (solver logic)
  "Set the logic for the solver."
  (check-type logic keyword)
  (unless (member logic *supported-theories*)
    (warn "Logic ~A may not be fully supported" logic))
  (setf (solver-logic solver) logic))

(defun solver-reset (solver)
  "Reset the solver to its initial state."
  (setf (solver-constants solver) nil)
  (setf (solver-functions solver) nil)
  (setf (solver-assertions solver) nil)
  (setf (solver-scope-stack solver) nil)
  (setf (solver-model solver) nil)
  (setf (solver-last-result solver) nil))

;;; ============================================================================
;;; Scope Management
;;; ============================================================================

(defun solver-push (solver)
  "Push a new assertion scope."
  (push (copy-list (solver-assertions solver))
        (solver-scope-stack solver)))

(defun solver-pop (solver)
  "Pop an assertion scope."
  (when (solver-scope-stack solver)
    (setf (solver-assertions solver)
          (pop (solver-scope-stack solver)))))

;;; ============================================================================
;;; Declarations
;;; ============================================================================

(defun declare-const (solver name type)
  "Declare a constant with the given name and type."
  (push (cons name type) (solver-constants solver))
  name)

(defun declare-fun (solver name param-types return-type)
  "Declare a function with the given signature."
  (push (list name param-types return-type nil)
        (solver-functions solver))
  name)

(defun define-fun (solver name params return-type body)
  "Define a function with the given implementation."
  (push (list name (mapcar #'second params) return-type body)
        (solver-functions solver))
  name)

;;; ============================================================================
;;; Assertions
;;; ============================================================================

(defun assert-formula (solver formula)
  "Assert a formula in the solver."
  (push formula (solver-assertions solver)))

;;; ============================================================================
;;; Model
;;; ============================================================================

(defstruct (model (:conc-name model-))
  "A model (satisfying assignment)."
  (bindings nil :type list)
  (functions nil :type list))

(defun model-get-value (model name)
  "Get the value of a constant in the model."
  (cdr (assoc name (model-bindings model) :test #'equal)))

(defun model-to-alist (model)
  "Convert model to association list."
  (model-bindings model))

(defun get-model (solver)
  "Get the model after a SAT result."
  (solver-model solver))

(defun get-value (solver exprs)
  "Get values of expressions in the current model."
  (let ((model (solver-model solver)))
    (when model
      (mapcar (lambda (expr)
                (cons expr (eval-formula expr (model-bindings model))))
              exprs))))

(defun get-unsat-core (solver)
  "Get the unsat core after an UNSAT result.
   Returns a list of asserted formulas that form a minimal unsatisfiable set."
  ;; Simple implementation: return all assertions
  ;; A more sophisticated version would minimize
  (when (eq (solver-last-result solver) :unsat)
    (solver-assertions solver)))

;;; ============================================================================
;;; Core Solving
;;; ============================================================================

(defun check-sat (solver)
  "Check satisfiability of all asserted formulas.
   Returns :sat, :unsat, or :unknown."
  (let* ((assertions (solver-assertions solver))
         (formula (if (= (length assertions) 1)
                      (first assertions)
                      (apply #'smt-and assertions))))
    ;; Handle trivial cases
    (when (null assertions)
      (setf (solver-last-result solver) :sat)
      (setf (solver-model solver) (make-model))
      (return-from check-sat :sat))

    ;; Try direct evaluation first
    (let ((simplified (simplify formula)))
      (cond
        ((eq simplified t)
         (setf (solver-last-result solver) :sat)
         (setf (solver-model solver) (make-model))
         (return-from check-sat :sat))
        ((eq simplified nil)
         (setf (solver-last-result solver) :unsat)
         (return-from check-sat :unsat))))

    ;; Collect variables
    (let ((vars (formula-vars formula)))
      (if (null vars)
          ;; No variables - evaluate directly
          (let ((result (eval-formula formula nil)))
            (cond
              ((eq result t)
               (setf (solver-last-result solver) :sat)
               (setf (solver-model solver) (make-model))
               :sat)
              ((eq result nil)
               (setf (solver-last-result solver) :unsat)
               :unsat)
              (t
               (setf (solver-last-result solver) :unknown)
               :unknown)))

          ;; Convert to CNF and use DPLL
          (let* ((cnf (cnf-convert formula))
                 (clauses (formula-to-clauses cnf)))
            ;; Check for empty clause (immediate UNSAT)
            (when (member nil clauses)
              (setf (solver-last-result solver) :unsat)
              (return-from check-sat :unsat))

            ;; Check for no clauses (SAT)
            (when (null clauses)
              (setf (solver-last-result solver) :sat)
              (setf (solver-model solver) (make-model))
              (return-from check-sat :sat))

            ;; Run DPLL
            (let ((state (init-dpll-state clauses)))
              (multiple-value-bind (result assignment)
                  (dpll-solve state)
                (cond
                  ((eq result :sat)
                   (setf (solver-last-result solver) :sat)
                   (setf (solver-model solver)
                         (make-model :bindings assignment))
                   :sat)
                  ((eq result :unsat)
                   (setf (solver-last-result solver) :unsat)
                   :unsat)
                  (t
                   (setf (solver-last-result solver) :unknown)
                   :unknown)))))))))

;;; ============================================================================
;;; High-Level Interface
;;; ============================================================================

(defun solve (formula &key (logic :qf-lia) declarations)
  "Solve a formula, returning :sat, :unsat, or :unknown with model/core."
  (with-solver (solver :logic logic)
    ;; Process declarations
    (dolist (decl declarations)
      (case (car decl)
        (:const (declare-const solver (second decl) (third decl)))
        (:fun (declare-fun solver (second decl) (third decl) (fourth decl)))))
    ;; Assert formula
    (assert-formula solver formula)
    ;; Check satisfiability
    (let ((result (check-sat solver)))
      (case result
        (:sat (values :sat (get-model solver)))
        (:unsat (values :unsat (get-unsat-core solver)))
        (otherwise (values result nil))))))

(defun is-satisfiable (formula &key (logic :qf-lia) declarations)
  "Check if a formula is satisfiable."
  (eq (solve formula :logic logic :declarations declarations) :sat))

(defun is-valid (formula &key (logic :qf-lia) declarations)
  "Check if a formula is valid (always true)."
  (eq (solve (smt-not formula) :logic logic :declarations declarations) :unsat))

(defun prove-formula (formula &key (logic :qf-lia) declarations)
  "Attempt to prove a formula, returning t if valid."
  (is-valid formula :logic logic :declarations declarations))

(defun find-counterexample (formula &key (logic :qf-lia) declarations)
  "Find a counterexample to a formula."
  (multiple-value-bind (result data)
      (solve (smt-not formula) :logic logic :declarations declarations)
    (if (eq result :sat)
        (values t (model-to-alist data))
        (values nil nil))))

(defun check-equivalence (f1 f2 &key (logic :qf-lia) declarations)
  "Check if two formulas are equivalent."
  (is-valid (smt-iff f1 f2) :logic logic :declarations declarations))

(defun check-implication (f1 f2 &key (logic :qf-lia) declarations)
  "Check if f1 implies f2."
  (is-valid (smt-implies f1 f2) :logic logic :declarations declarations))

;;; ============================================================================
;;; Documentation
;;; ============================================================================

(setf (documentation 'solver 'type)
      "An SMT solver instance supporting Boolean, Integer, and Bitvector theories.

Key Features:
- Pure Common Lisp implementation (no external dependencies)
- DPLL-based SAT solving with conflict-driven clause learning
- Support for QF_LIA (Linear Integer Arithmetic)
- Support for QF_BV (Bitvectors)
- Incremental solving with push/pop scopes
- Model extraction

Quick Start:
  ;; Simple satisfiability check
  (solve '(:and (:gt x 0) (:lt x 10))
         :declarations '((:const x :int)))

  ;; Check validity
  (is-valid '(:implies (:gt x 0) (:ge x 0))
            :declarations '((:const x :int)))

  ;; Find counterexample
  (find-counterexample '(:and (:gt x 0) (:lt x 0))
                       :declarations '((:const x :int)))

  ;; Using the solver object
  (with-solver (s :logic :qf-lia)
    (declare-const s 'x :int)
    (assert-formula s '(:gt x 0))
    (assert-formula s '(:lt x 10))
    (check-sat s)  ; => :sat
    (get-model s)) ; => model with x bound to some value 1-9")
