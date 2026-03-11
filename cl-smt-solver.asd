;;;; cl-smt-solver.asd - Z3-style SMT constraint solver in pure Common Lisp
;;;;
;;;; Copyright (c) 2025
;;;; License: MIT

(defsystem "cl-smt-solver"
  :description "Pure Common Lisp SMT solver with Boolean, integer, and bitvector theories"
  :version "0.1.0"
  :author ""
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:module "src"
                :serial t
                :components ((:file "util")
                             (:file "expr")
                             (:file "theory")
                             (:file "dpll")
                             (:file "solver"))))
  :in-order-to ((test-op (test-op "cl-smt-solver/test"))))

(defsystem "cl-smt-solver/test"
  :description "Tests for cl-smt-solver"
  :depends-on ("cl-smt-solver")
  :components ((:module "test"
                :components ((:file "test-smt"))))
  :perform (test-op (o c)
                    (symbol-call :cl-smt-solver.test :run-tests)))
