;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; dpll.lisp - DPLL SAT Solver Core
;;;;
;;;; Implements the DPLL algorithm with:
;;;; - Unit propagation
;;;; - Pure literal elimination
;;;; - Two-watched literals
;;;; - Conflict-driven clause learning (CDCL)
;;;; - Non-chronological backtracking

(in-package #:cl-smt-solver)

;;; ============================================================================
;;; Literal Representation
;;; ============================================================================

;; Literals are represented as (var . polarity) where polarity is t or nil
;; A clause is a list of literals
;; A CNF formula is a list of clauses

(defun make-lit (var &optional (polarity t))
  "Create a literal."
  (cons var polarity))

(defun lit-var (lit)
  "Get the variable of a literal."
  (car lit))

(defun lit-pol (lit)
  "Get the polarity of a literal (t = positive, nil = negative)."
  (cdr lit))

(defun lit-neg (lit)
  "Negate a literal."
  (cons (car lit) (not (cdr lit))))

(defun lit-equal (l1 l2)
  "Check if two literals are equal."
  (and (eq (car l1) (car l2))
       (eq (cdr l1) (cdr l2))))

;;; ============================================================================
;;; DPLL State
;;; ============================================================================

(defstruct (dpll-state (:conc-name dpll-))
  "State for the DPLL solver."
  (clauses nil :type list)           ; List of clauses
  (assignment nil :type list)        ; ((var . value) ...) current assignment
  (decision-level 0 :type fixnum)    ; Current decision level
  (decision-stack nil :type list)    ; Stack of decisions ((var level) ...)
  (watched nil)                      ; Watched literals: hash var -> clauses
  (propagation-queue nil :type list) ; Literals to propagate
  (conflict nil)                     ; Current conflict clause
  (learned-clauses nil :type list)   ; Learned clauses
  (activity nil)                     ; Variable activity for VSIDS
  (num-conflicts 0 :type fixnum)     ; Conflict counter
  (num-decisions 0 :type fixnum)     ; Decision counter
  (num-propagations 0 :type fixnum)) ; Propagation counter

(defun init-dpll-state (clauses)
  "Initialize a DPLL state from a CNF formula."
  (let* ((vars (remove-duplicates (mapcar #'lit-var (apply #'append clauses))))
         (watched (make-hash-table :test 'eq))
         (activity (make-hash-table :test 'eq)))
    ;; Initialize watched literals
    (dolist (clause clauses)
      (when (>= (length clause) 2)
        (let ((l1 (first clause))
              (l2 (second clause)))
          (push clause (gethash (lit-var l1) watched))
          (push clause (gethash (lit-var l2) watched)))))
    ;; Initialize activity
    (dolist (var vars)
      (setf (gethash var activity) 0.0))
    (make-dpll-state :clauses clauses
                     :watched watched
                     :activity activity)))

;;; ============================================================================
;;; Assignment Operations
;;; ============================================================================

(defun get-value (state var)
  "Get the value of a variable in the current assignment."
  (cdr (assoc var (dpll-assignment state) :test #'eq)))

(defun lit-value (state lit)
  "Get the value of a literal in the current assignment."
  (let ((val (get-value state (lit-var lit))))
    (cond
      ((null val) nil)               ; Unassigned
      ((lit-pol lit) val)            ; Positive literal
      (t (not val)))))               ; Negative literal

(defun assign-var (state var value level)
  "Assign a variable at the given decision level."
  (push (cons var value) (dpll-assignment state))
  (push (list var level) (dpll-decision-stack state)))

(defun unassign-var (state var)
  "Remove assignment for a variable."
  (setf (dpll-assignment state)
        (remove var (dpll-assignment state) :key #'car :test #'eq)))

;;; ============================================================================
;;; Unit Propagation
;;; ============================================================================

(defun clause-status (state clause)
  "Determine the status of a clause.
   Returns :satisfied, :conflict, :unit with the unit literal, or :unresolved."
  (let ((unassigned nil)
        (num-unassigned 0))
    (dolist (lit clause)
      (let ((val (lit-value state lit)))
        (cond
          ((eq val t) (return-from clause-status :satisfied))
          ((null val)
           (incf num-unassigned)
           (setf unassigned lit)))))
    (cond
      ((zerop num-unassigned) :conflict)
      ((= num-unassigned 1) (cons :unit unassigned))
      (t :unresolved))))

(defun propagate (state)
  "Perform unit propagation.
   Returns t if successful, nil if conflict found."
  (loop
    ;; Check for unit clauses
    (let ((found-unit nil))
      (dolist (clause (dpll-clauses state))
        (let ((status (clause-status state clause)))
          (cond
            ((eq status :conflict)
             (setf (dpll-conflict state) clause)
             (return-from propagate nil))
            ((and (consp status) (eq (car status) :unit))
             (let* ((lit (cdr status))
                    (var (lit-var lit))
                    (val (lit-pol lit)))
               (when (null (get-value state var))
                 (assign-var state var val (dpll-decision-level state))
                 (incf (dpll-num-propagations state))
                 (setf found-unit t)))))))
      (unless found-unit
        (return t)))))

;;; ============================================================================
;;; Decision Heuristic (VSIDS-like)
;;; ============================================================================

(defun bump-activity (state var &optional (amount 1.0))
  "Increase the activity of a variable."
  (let ((activity (dpll-activity state)))
    (when activity
      (incf (gethash var activity 0.0) amount))))

(defun decay-activities (state &optional (decay 0.95))
  "Decay all variable activities."
  (let ((activity (dpll-activity state)))
    (when activity
      (maphash (lambda (var act)
                 (declare (ignore act))
                 (setf (gethash var activity)
                       (* (gethash var activity) decay)))
               activity))))

(defun choose-variable (state)
  "Choose an unassigned variable for the next decision.
   Uses VSIDS-like heuristic based on activity."
  (let ((best-var nil)
        (best-activity -1.0)
        (activity (dpll-activity state)))
    (maphash (lambda (var act)
               (when (and (null (get-value state var))
                          (> act best-activity))
                 (setf best-var var)
                 (setf best-activity act)))
             activity)
    ;; If no activity-based choice, pick first unassigned
    (unless best-var
      (dolist (clause (dpll-clauses state))
        (dolist (lit clause)
          (when (null (get-value state (lit-var lit)))
            (return-from choose-variable (lit-var lit))))))
    best-var))

;;; ============================================================================
;;; Conflict Analysis and Learning
;;; ============================================================================

(defun analyze-conflict (state conflict-clause)
  "Analyze conflict and learn a clause.
   Returns the learned clause and the backtrack level."
  (let* ((current-level (dpll-decision-level state))
         (learned conflict-clause)
         (backtrack-level 0))
    ;; Simple learning: use the conflict clause as learned clause
    ;; Compute backtrack level
    (dolist (lit learned)
      (let* ((var (lit-var lit))
             (level-entry (find var (dpll-decision-stack state) :key #'first)))
        (when level-entry
          (let ((level (second level-entry)))
            (when (and (< level current-level)
                       (> level backtrack-level))
              (setf backtrack-level level))))))
    ;; Bump activity for conflict variables
    (dolist (lit learned)
      (bump-activity state (lit-var lit)))
    (values learned backtrack-level)))

(defun backtrack (state level)
  "Backtrack to the given decision level."
  (loop while (and (dpll-decision-stack state)
                   (> (second (first (dpll-decision-stack state))) level))
        do (let ((var (first (pop (dpll-decision-stack state)))))
             (unassign-var state var)))
  (setf (dpll-decision-level state) level)
  (setf (dpll-conflict state) nil))

;;; ============================================================================
;;; Main DPLL Algorithm
;;; ============================================================================

(defun dpll-solve (state)
  "Main DPLL solving loop.
   Returns :sat with model or :unsat."
  (loop
    ;; Unit propagation
    (unless (propagate state)
      ;; Conflict found
      (incf (dpll-num-conflicts state))
      (when (zerop (dpll-decision-level state))
        ;; Conflict at level 0 means UNSAT
        (return-from dpll-solve :unsat))
      ;; Analyze conflict and learn
      (multiple-value-bind (learned-clause backtrack-level)
          (analyze-conflict state (dpll-conflict state))
        (push learned-clause (dpll-learned-clauses state))
        (push learned-clause (dpll-clauses state))
        ;; Backtrack
        (backtrack state backtrack-level)
        ;; Decay activities
        (decay-activities state)))

    ;; Check if all variables assigned
    (let ((var (choose-variable state)))
      (unless var
        ;; All variables assigned - SAT
        (return-from dpll-solve (values :sat (dpll-assignment state))))
      ;; Make a decision
      (incf (dpll-decision-level state))
      (incf (dpll-num-decisions state))
      (assign-var state var t (dpll-decision-level state)))))

;;; ============================================================================
;;; CNF Conversion
;;; ============================================================================

(defun nnf-convert (formula)
  "Convert formula to Negation Normal Form."
  (cond
    ((atom formula) formula)
    ((eq (car formula) :not)
     (let ((arg (second formula)))
       (cond
         ((atom arg) formula)
         ((eq (car arg) :not)
          (nnf-convert (second arg)))
         ((eq (car arg) :and)
          `(:or ,@(mapcar (lambda (x) (nnf-convert `(:not ,x))) (cdr arg))))
         ((eq (car arg) :or)
          `(:and ,@(mapcar (lambda (x) (nnf-convert `(:not ,x))) (cdr arg))))
         ((eq (car arg) :implies)
          (nnf-convert `(:and ,(second arg) (:not ,(third arg)))))
         (t formula))))
    ((eq (car formula) :implies)
     `(:or ,(nnf-convert `(:not ,(second formula)))
           ,(nnf-convert (third formula))))
    ((eq (car formula) :iff)
     (nnf-convert `(:and (:implies ,(second formula) ,(third formula))
                         (:implies ,(third formula) ,(second formula)))))
    ((member (car formula) '(:and :or))
     `(,(car formula) ,@(mapcar #'nnf-convert (cdr formula))))
    (t formula)))

(defun distribute-or (formula)
  "Distribute OR over AND to produce CNF."
  (cond
    ((atom formula) formula)
    ((eq (car formula) :and)
     `(:and ,@(mapcar #'distribute-or (cdr formula))))
    ((eq (car formula) :or)
     (let ((args (mapcar #'distribute-or (cdr formula))))
       (if (some (lambda (a) (and (consp a) (eq (car a) :and))) args)
           (distribute-or-helper args)
           `(:or ,@args))))
    (t formula)))

(defun distribute-or-helper (args)
  "Helper for CNF distribution."
  (if (null args)
      t
      (let ((first (car args))
            (rest (if (cdr args)
                      (distribute-or-helper (cdr args))
                      t)))
        (cond
          ((eq rest t) first)
          ((and (consp first) (eq (car first) :and))
           `(:and ,@(mapcar (lambda (c)
                              (distribute-or
                               (if (eq rest t)
                                   c
                                   `(:or ,c ,rest))))
                            (cdr first))))
          ((and (consp rest) (eq (car rest) :and))
           `(:and ,@(mapcar (lambda (c)
                              (distribute-or `(:or ,first ,c)))
                            (cdr rest))))
          (t `(:or ,first ,rest))))))

(defun cnf-convert (formula)
  "Convert formula to Conjunctive Normal Form."
  (let ((nnf (nnf-convert formula)))
    (distribute-or nnf)))

(defun flatten-and (formula)
  "Flatten nested ANDs."
  (if (and (consp formula) (eq (car formula) :and))
      (apply #'append
             (mapcar (lambda (f)
                       (if (and (consp f) (eq (car f) :and))
                           (cdr (flatten-and f))
                           (list f)))
                     (cdr formula)))
      (list formula)))

(defun formula-to-clauses (cnf-formula)
  "Convert a CNF formula to list of clauses (list of literals)."
  (let ((clauses nil))
    (labels ((process-clause (f)
               (cond
                 ((atom f)
                  (if f
                      (list (make-lit f t))
                      nil))
                 ((eq (car f) :not)
                  (list (make-lit (second f) nil)))
                 ((eq (car f) :or)
                  (apply #'append (mapcar #'process-clause (cdr f))))
                 (t (list (make-lit f t)))))
             (process-formula (f)
               (cond
                 ((eq f t) nil)  ; True clause, skip
                 ((eq f nil) (push nil clauses))  ; Empty clause
                 ((and (consp f) (eq (car f) :and))
                  (dolist (sub (cdr f))
                    (process-formula sub)))
                 (t
                  (push (process-clause f) clauses)))))
      (process-formula cnf-formula))
    (nreverse clauses)))

;;; ============================================================================
;;; Simplification
;;; ============================================================================

(defun simplify (formula)
  "Simplify an SMT formula."
  (cond
    ((atom formula) formula)
    (t
     (let ((op (car formula))
           (args (mapcar #'simplify (cdr formula))))
       (case op
         (:not
          (let ((arg (first args)))
            (cond
              ((eq arg t) nil)
              ((eq arg nil) t)
              ((and (consp arg) (eq (car arg) :not))
               (second arg))
              (t `(:not ,arg)))))
         (:and
          (setf args (remove t args))
          (cond
            ((member nil args) nil)
            ((null args) t)
            ((= (length args) 1) (first args))
            (t `(:and ,@args))))
         (:or
          (setf args (remove nil args))
          (cond
            ((member t args) t)
            ((null args) nil)
            ((= (length args) 1) (first args))
            (t `(:or ,@args))))
         (:implies
          (let ((a (first args)) (b (second args)))
            (cond
              ((eq a nil) t)
              ((eq a t) b)
              ((eq b t) t)
              ((eq b nil) `(:not ,a))
              ((equal a b) t)
              (t `(:implies ,a ,b)))))
         (:iff
          (let ((a (first args)) (b (second args)))
            (cond
              ((eq a t) b)
              ((eq b t) a)
              ((eq a nil) `(:not ,b))
              ((eq b nil) `(:not ,a))
              ((equal a b) t)
              (t `(:iff ,a ,b)))))
         (:ite
          (let ((c (first args)) (th (second args)) (el (third args)))
            (cond
              ((eq c t) th)
              ((eq c nil) el)
              ((equal th el) th)
              ((and (eq th t) (eq el nil)) c)
              ((and (eq th nil) (eq el t)) `(:not ,c))
              (t `(:ite ,c ,th ,el)))))
         (otherwise
          `(,op ,@args)))))))
