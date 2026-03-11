# cl-smt-solver

A pure Common Lisp SMT (Satisfiability Modulo Theories) constraint solver.

## Features

- **Pure Common Lisp** - No external dependencies, no FFI
- **DPLL-based SAT core** with conflict-driven clause learning (CDCL)
- **Multiple theories**:
  - Boolean logic
  - Linear Integer Arithmetic (QF_LIA)
  - Bitvectors (QF_BV)
- **High-level API** for common verification tasks
- **Incremental solving** with push/pop scopes

## Installation

Clone this repository and load with ASDF:

```lisp
(asdf:load-system "cl-smt-solver")
```

## Quick Start

```lisp
(use-package :cl-smt-solver)

;; Check if a formula is satisfiable
(is-satisfiable '(:and p q))       ; => T
(is-satisfiable '(:and p (:not p))) ; => NIL

;; Check if a formula is valid (always true)
(is-valid '(:or p (:not p)))  ; => T (law of excluded middle)
(is-valid '(:implies p q))    ; => NIL

;; Find a satisfying assignment
(solve '(:and (:gt x 0) (:lt x 10)))
; => :SAT, #<MODEL x=1>

;; Find a counterexample
(find-counterexample '(:implies p q))
; => T, ((P . T) (Q . NIL))

;; Check logical implication
(check-implication '(:and p q) 'p)  ; => T
(check-implication 'p '(:and p q))  ; => NIL

;; Check equivalence
(check-equivalence '(:not (:not p)) 'p)  ; => T
```

## Using the Solver Object

For more control, use the solver object directly:

```lisp
(with-solver (s :logic :qf-lia)
  ;; Declare variables
  (declare-const s 'x :int)
  (declare-const s 'y :int)

  ;; Add constraints
  (assert-formula s '(:gt x 0))
  (assert-formula s '(:gt y 0))
  (assert-formula s '(:eq (:add x y) 10))

  ;; Check satisfiability
  (check-sat s)  ; => :SAT

  ;; Get the model
  (model-to-alist (get-model s)))
  ; => ((X . 5) (Y . 5)) or similar
```

### Incremental Solving

```lisp
(with-solver (s)
  (assert-formula s 'a)
  (check-sat s)  ; => :SAT

  ;; Save state
  (solver-push s)

  ;; Add conflicting constraint
  (assert-formula s '(:not a))
  (check-sat s)  ; => :UNSAT

  ;; Restore state
  (solver-pop s)
  (check-sat s)) ; => :SAT
```

## Expression Syntax

Formulas are represented as S-expressions using keyword operators:

### Logical Operators

| Operator | Example | Description |
|----------|---------|-------------|
| `:and` | `(:and p q r)` | Conjunction |
| `:or` | `(:or p q r)` | Disjunction |
| `:not` | `(:not p)` | Negation |
| `:implies` | `(:implies p q)` | Implication (p => q) |
| `:iff` | `(:iff p q)` | Equivalence (p <=> q) |
| `:ite` | `(:ite c t e)` | If-then-else |

### Comparison

| Operator | Example | Description |
|----------|---------|-------------|
| `:eq` | `(:eq x y)` | Equality |
| `:distinct` | `(:distinct x y z)` | All different |
| `:lt` | `(:lt x y)` | Less than |
| `:le` | `(:le x y)` | Less than or equal |
| `:gt` | `(:gt x y)` | Greater than |
| `:ge` | `(:ge x y)` | Greater than or equal |

### Arithmetic

| Operator | Example | Description |
|----------|---------|-------------|
| `:add` | `(:add x y z)` | Addition |
| `:sub` | `(:sub x y)` | Subtraction |
| `:mul` | `(:mul x y)` | Multiplication |
| `:div` | `(:div x y)` | Integer division |
| `:mod` | `(:mod x y)` | Modulo |
| `:neg` | `(:neg x)` | Negation |
| `:abs` | `(:abs x)` | Absolute value |

### Bitvector Operations

| Operator | Example | Description |
|----------|---------|-------------|
| `:bvand` | `(:bvand x y)` | Bitwise AND |
| `:bvor` | `(:bvor x y)` | Bitwise OR |
| `:bvxor` | `(:bvxor x y)` | Bitwise XOR |
| `:bvnot` | `(:bvnot x)` | Bitwise NOT |
| `:bvadd` | `(:bvadd x y)` | BV addition |
| `:bvsub` | `(:bvsub x y)` | BV subtraction |
| `:bvmul` | `(:bvmul x y)` | BV multiplication |
| `:bvshl` | `(:bvshl x n)` | Shift left |
| `:bvlshr` | `(:bvlshr x n)` | Logical shift right |
| `:bvashr` | `(:bvashr x n)` | Arithmetic shift right |
| `:bvult` | `(:bvult x y)` | Unsigned less than |
| `:bvslt` | `(:bvslt x y)` | Signed less than |

## API Reference

### Solving Functions

- `(solve formula &key logic declarations)` - Solve formula, returns `:sat`/`:unsat`/`:unknown` and model/core
- `(is-satisfiable formula ...)` - Returns T if satisfiable
- `(is-valid formula ...)` - Returns T if valid (always true)
- `(prove-formula formula ...)` - Alias for `is-valid`
- `(find-counterexample formula ...)` - Find assignment making formula false
- `(check-equivalence f1 f2 ...)` - Check if f1 <=> f2
- `(check-implication f1 f2 ...)` - Check if f1 => f2

### Solver Object

- `(make-solver &key logic)` - Create solver
- `(with-solver (var &key logic) &body)` - Solver with cleanup
- `(declare-const solver name type)` - Declare variable
- `(assert-formula solver formula)` - Add constraint
- `(check-sat solver)` - Check satisfiability
- `(get-model solver)` - Get satisfying model
- `(solver-push solver)` - Save state
- `(solver-pop solver)` - Restore state
- `(solver-reset solver)` - Clear all

### Utility Functions

- `(simplify formula)` - Simplify formula
- `(cnf-convert formula)` - Convert to CNF
- `(nnf-convert formula)` - Convert to NNF
- `(formula-vars formula)` - Get free variables
- `(formula-size formula)` - Count nodes
- `(expr-to-string formula)` - Pretty print

## Supported Logics

| Logic | Description |
|-------|-------------|
| `:qf-lia` | Quantifier-free Linear Integer Arithmetic |
| `:qf-bv` | Quantifier-free Bitvectors |
| `:qf-uf` | Quantifier-free Uninterpreted Functions |

## Running Tests

```lisp
(asdf:test-system "cl-smt-solver")
```

Or manually:

```lisp
(asdf:load-system "cl-smt-solver/test")
(cl-smt-solver.test:run-tests)
```

## Limitations

- Pure Common Lisp implementation - slower than native solvers like Z3
- Limited theory support compared to full SMT-LIB2
- No support for quantifier elimination
- Arrays are not yet fully implemented

For production use requiring high performance, consider interfacing with Z3 or CVC5.

## License

MIT License
