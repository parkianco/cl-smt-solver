;;;; expr.lisp - SMT Expression AST
;;;;
;;;; Defines the expression representation for SMT formulas.

(in-package #:cl-smt-solver)

;;; ============================================================================
;;; SMT Types
;;; ============================================================================

(defstruct (smt-type (:conc-name type-))
  "An SMT sort/type."
  (kind :bool :type keyword)
  (parameters nil :type list)
  (name nil))

(defun type-bool ()
  "Boolean type."
  (make-smt-type :kind :bool :name "Bool"))

(defun type-int ()
  "Integer type."
  (make-smt-type :kind :int :name "Int"))

(defun type-real ()
  "Real number type."
  (make-smt-type :kind :real :name "Real"))

(defun type-bitvec (width)
  "Bitvector type of given WIDTH."
  (check-type width (integer 1))
  (make-smt-type :kind :bitvec
                 :parameters (list width)
                 :name (format nil "BitVec[~D]" width)))

(defun type-array (index-type element-type)
  "Array type with given index and element types."
  (make-smt-type :kind :array
                 :parameters (list index-type element-type)
                 :name (format nil "Array[~A -> ~A]"
                               (type-name index-type)
                               (type-name element-type))))

(defun bitvec-width (type)
  "Get the width of a bitvector type."
  (first (type-parameters type)))

(defun array-index-type (type)
  "Get the index type of an array type."
  (first (type-parameters type)))

(defun smt-array-element-type (type)
  "Get the element type of an array type."
  (second (type-parameters type)))

;;; ============================================================================
;;; SMT Expressions
;;; ============================================================================

(defstruct (smt-expr (:conc-name expr-))
  "An SMT expression node."
  (kind :literal :type keyword)
  (operator nil)
  (operands nil :type list)
  (type nil)
  (name nil)
  (value nil))

(defun make-literal (value &optional type)
  "Create a literal expression."
  (make-smt-expr :kind :literal :value value :type type))

(defun make-variable (name type)
  "Create a variable expression."
  (make-smt-expr :kind :variable :name name :type type))

(defun make-application (operator &rest operands)
  "Create a function application expression."
  (make-smt-expr :kind :application
                 :operator operator
                 :operands operands))

;;; ============================================================================
;;; Logical Operators (return S-expressions)
;;; ============================================================================

(defun smt-and (&rest args)
  "Logical AND."
  (case (length args)
    (0 t)
    (1 (first args))
    (otherwise `(:and ,@args))))

(defun smt-or (&rest args)
  "Logical OR."
  (case (length args)
    (0 nil)
    (1 (first args))
    (otherwise `(:or ,@args))))

(defun smt-not (arg)
  "Logical NOT."
  `(:not ,arg))

(defun smt-implies (a b)
  "Logical implication."
  `(:implies ,a ,b))

(defun smt-iff (a b)
  "Logical equivalence."
  `(:iff ,a ,b))

(defun smt-ite (condition then-branch else-branch)
  "If-then-else."
  `(:ite ,condition ,then-branch ,else-branch))

(defun smt-eq (&rest args)
  "Equality."
  `(:eq ,@args))

(defun smt-distinct (&rest args)
  "All arguments are distinct."
  `(:distinct ,@args))

;;; ============================================================================
;;; Comparison Operators
;;; ============================================================================

(defun smt-lt (a b) "Less than." `(:lt ,a ,b))
(defun smt-le (a b) "Less than or equal." `(:le ,a ,b))
(defun smt-gt (a b) "Greater than." `(:gt ,a ,b))
(defun smt-ge (a b) "Greater than or equal." `(:ge ,a ,b))

;;; ============================================================================
;;; Arithmetic Operators
;;; ============================================================================

(defun smt-add (&rest args) "Addition." `(:add ,@args))
(defun smt-sub (a &rest args)
  "Subtraction or negation."
  (if args `(:sub ,a ,@args) `(:neg ,a)))
(defun smt-mul (&rest args) "Multiplication." `(:mul ,@args))
(defun smt-div (a b) "Integer division." `(:div ,a ,b))
(defun smt-mod (a b) "Modulo." `(:mod ,a ,b))
(defun smt-neg (a) "Negation." `(:neg ,a))
(defun smt-abs (a) "Absolute value." `(:abs ,a))

;;; ============================================================================
;;; Bitvector Operators
;;; ============================================================================

(defun bv-and (a b) "Bitwise AND." `(:bvand ,a ,b))
(defun bv-or (a b) "Bitwise OR." `(:bvor ,a ,b))
(defun bv-xor (a b) "Bitwise XOR." `(:bvxor ,a ,b))
(defun bv-not (a) "Bitwise NOT." `(:bvnot ,a))
(defun bv-add (a b) "Bitvector addition." `(:bvadd ,a ,b))
(defun bv-sub (a b) "Bitvector subtraction." `(:bvsub ,a ,b))
(defun bv-mul (a b) "Bitvector multiplication." `(:bvmul ,a ,b))
(defun bv-udiv (a b) "Unsigned division." `(:bvudiv ,a ,b))
(defun bv-sdiv (a b) "Signed division." `(:bvsdiv ,a ,b))
(defun bv-urem (a b) "Unsigned remainder." `(:bvurem ,a ,b))
(defun bv-srem (a b) "Signed remainder." `(:bvsrem ,a ,b))
(defun bv-shl (a b) "Shift left." `(:bvshl ,a ,b))
(defun bv-lshr (a b) "Logical shift right." `(:bvlshr ,a ,b))
(defun bv-ashr (a b) "Arithmetic shift right." `(:bvashr ,a ,b))
(defun bv-concat (a b) "Concatenation." `(:bvconcat ,a ,b))
(defun bv-extract (high low bv) "Bit extraction." `(:bvextract ,high ,low ,bv))
(defun bv-zero-extend (n bv) "Zero extension." `(:bvzext ,n ,bv))
(defun bv-sign-extend (n bv) "Sign extension." `(:bvsext ,n ,bv))

;; Bitvector comparisons
(defun bv-ult (a b) "Unsigned less than." `(:bvult ,a ,b))
(defun bv-ule (a b) "Unsigned less than or equal." `(:bvule ,a ,b))
(defun bv-ugt (a b) "Unsigned greater than." `(:bvugt ,a ,b))
(defun bv-uge (a b) "Unsigned greater than or equal." `(:bvuge ,a ,b))
(defun bv-slt (a b) "Signed less than." `(:bvslt ,a ,b))
(defun bv-sle (a b) "Signed less than or equal." `(:bvsle ,a ,b))
(defun bv-sgt (a b) "Signed greater than." `(:bvsgt ,a ,b))
(defun bv-sge (a b) "Signed greater than or equal." `(:bvsge ,a ,b))

;;; ============================================================================
;;; Array Operators
;;; ============================================================================

(defun array-select (arr idx)
  "Select element from array."
  `(:select ,arr ,idx))

(defun array-store (arr idx val)
  "Store value in array."
  `(:store ,arr ,idx ,val))

;;; ============================================================================
;;; Quantifiers
;;; ============================================================================

(defun smt-forall (bindings body)
  "Universal quantification.
   BINDINGS is a list of (var type) pairs."
  `(:forall ,bindings ,body))

(defun smt-exists (bindings body)
  "Existential quantification.
   BINDINGS is a list of (var type) pairs."
  `(:exists ,bindings ,body))

;;; ============================================================================
;;; Expression Analysis
;;; ============================================================================

(defun formula-vars (formula)
  "Extract all free variables from a formula."
  (let ((vars nil)
        (bound nil))
    (labels ((collect (f)
               (cond
                 ((null f) nil)
                 ((and (symbolp f) (not (keywordp f)) (not (eq f t))
                       (not (member f bound)))
                  (pushnew f vars))
                 ((consp f)
                  (let ((op (car f)))
                    (case op
                      ((:forall :exists)
                       (let ((bindings (second f))
                             (body (third f)))
                         (let ((bound (append (mapcar #'first bindings) bound)))
                           (collect body))))
                      (otherwise
                       (dolist (sub (cdr f))
                         (collect sub)))))))))
      (collect formula))
    (nreverse vars)))

(defun formula-size (formula)
  "Compute the size (number of nodes) of a formula."
  (cond
    ((atom formula) 1)
    (t (1+ (reduce #'+ (mapcar #'formula-size (cdr formula)) :initial-value 0)))))

(defun formula-depth (formula)
  "Compute the depth of a formula."
  (cond
    ((atom formula) 0)
    (t (1+ (reduce #'max (mapcar #'formula-depth (cdr formula)) :initial-value 0)))))

;;; ============================================================================
;;; Expression Printing
;;; ============================================================================

(defun expr-to-string (expr)
  "Convert an expression to a readable string."
  (cond
    ((null expr) "false")
    ((eq expr t) "true")
    ((integerp expr) (format nil "~D" expr))
    ((symbolp expr) (symbol-name expr))
    ((consp expr)
     (let ((op (car expr))
           (args (cdr expr)))
       (format nil "(~A~{ ~A~})"
               (operator-to-string op)
               (mapcar #'expr-to-string args))))
    (t (format nil "~A" expr))))

(defun operator-to-string (op)
  "Convert an operator keyword to string."
  (case op
    (:and "and")
    (:or "or")
    (:not "not")
    (:implies "=>")
    (:iff "<=>")
    (:ite "ite")
    (:eq "=")
    (:distinct "distinct")
    (:lt "<")
    (:le "<=")
    (:gt ">")
    (:ge ">=")
    (:add "+")
    (:sub "-")
    (:mul "*")
    (:div "div")
    (:mod "mod")
    (:neg "-")
    (:abs "abs")
    (:bvand "bvand")
    (:bvor "bvor")
    (:bvxor "bvxor")
    (:bvnot "bvnot")
    (:bvadd "bvadd")
    (:bvsub "bvsub")
    (:bvmul "bvmul")
    (:bvudiv "bvudiv")
    (:bvsdiv "bvsdiv")
    (:bvurem "bvurem")
    (:bvsrem "bvsrem")
    (:bvshl "bvshl")
    (:bvlshr "bvlshr")
    (:bvashr "bvashr")
    (:bvult "bvult")
    (:bvule "bvule")
    (:bvugt "bvugt")
    (:bvuge "bvuge")
    (:bvslt "bvslt")
    (:bvsle "bvsle")
    (:bvsgt "bvsgt")
    (:bvsge "bvsge")
    (:select "select")
    (:store "store")
    (:forall "forall")
    (:exists "exists")
    (otherwise (string-downcase (symbol-name op)))))
