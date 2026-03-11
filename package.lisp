;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; package.lisp - Package definitions for cl-smt-solver
;;;;
;;;; Pure Common Lisp SMT solver supporting Boolean, Integer, and Bitvector theories.

(defpackage #:cl-smt-solver
  (:use #:cl)
  (:nicknames #:smt)
  (:documentation "Z3-style SMT constraint solver in pure Common Lisp.")
  (:export
   ;; Solver Management
   #:make-solver
   #:solver-p
   #:solver-reset
   #:solver-push
   #:solver-pop
   #:with-solver

   ;; Theories
   #:*supported-theories*
   #:set-logic

   ;; Types
   #:type-bool
   #:type-int
   #:type-real
   #:type-bitvec
   #:type-array

   ;; Variable Declaration
   #:declare-const
   #:declare-fun
   #:define-fun

   ;; Assertions
   #:assert-formula
   #:check-sat
   #:get-model
   #:get-value
   #:get-unsat-core

   ;; Logical Operators
   #:smt-and
   #:smt-or
   #:smt-not
   #:smt-implies
   #:smt-iff
   #:smt-ite
   #:smt-eq
   #:smt-distinct

   ;; Comparison
   #:smt-lt
   #:smt-le
   #:smt-gt
   #:smt-ge

   ;; Arithmetic
   #:smt-add
   #:smt-sub
   #:smt-mul
   #:smt-div
   #:smt-mod
   #:smt-neg
   #:smt-abs

   ;; Bitvector Operations
   #:bv-and
   #:bv-or
   #:bv-xor
   #:bv-not
   #:bv-add
   #:bv-sub
   #:bv-mul
   #:bv-udiv
   #:bv-sdiv
   #:bv-urem
   #:bv-srem
   #:bv-shl
   #:bv-lshr
   #:bv-ashr
   #:bv-concat
   #:bv-extract
   #:bv-zero-extend
   #:bv-sign-extend
   #:bv-ult
   #:bv-ule
   #:bv-ugt
   #:bv-uge
   #:bv-slt
   #:bv-sle
   #:bv-sgt
   #:bv-sge

   ;; Array Operations
   #:array-select
   #:array-store

   ;; Quantifiers
   #:smt-forall
   #:smt-exists

   ;; Model Handling
   #:model-p
   #:model-get-value
   #:model-to-alist

   ;; High-Level Interface
   #:solve
   #:is-satisfiable
   #:is-valid
   #:prove-formula
   #:find-counterexample
   #:check-equivalence
   #:check-implication

   ;; Expression Utilities
   #:expr-to-string
   #:simplify
   #:cnf-convert
   #:nnf-convert
   #:formula-vars
   #:formula-size

   ;; Result types
   #:sat
   #:unsat
   #:unknown))

(defpackage #:cl-smt-solver.test
  (:use #:cl #:cl-smt-solver)
  (:export #:run-tests))
