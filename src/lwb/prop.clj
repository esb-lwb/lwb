; lwb Logic WorkBench -- Propositional Logic

; Copyright (c) 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop)


;; ## The operators of propositional logic

;; * not -- unary, provided by Clojure
;; * and -- n-ary, provided by Clojure
;; * or  -- n-ary, provided by Clojure
;; * nand -- negated and, binary
;; * nor -- negated or, binary
;; * impl -- implication, binary
;; * nimpl -- negated implication, binary
;; * cimpl -- converse implication, binary
;; * ncimpl -- negated converse implication, binary
;; * equiv -- equivalence, binary
;; * xor -- exclusive or, binary
;; * ite -- if-then-else, ternary

;; # Hints
;; 1. The operators are defined as macros
;; 2. We don't use syntax-quoting, because we don't want to have qualified
;;    names after expansion
;; 3. The macros must not be changed to functions,
;;    because transformations (to cnf e.g.) use them with macroexpand!

(defmacro nand
  "Logical negated and."
  [phi psi]
  (list 'not 
    (list 'and phi psi)))

(defmacro nor
  "Logical negated or."
  [phi psi]
  (list 'not 
        (list 'or phi psi)))

(defmacro impl
  "Logical implication (phi -> psi)."
  [phi psi]
  (list 'or 
        (list 'not phi) psi))

(defmacro nimpl 
  "Logical negated implication (!(phi -> psi))."
  [phi psi]
  (list 'not 
        (list 'impl phi psi)))

(defmacro cimpl
  "Converse logical implication (phi <- psi)."
  [phi psi]
  (list 'or
    (list 'not psi) phi))

(defmacro ncimpl 
  "Negated converse logical implication (!(phi <- psi))."
  [phi psi]
  (list 'not
    (list 'cimpl phi psi)))

(defmacro equiv
   "Logical equivalence (phi <-> psi)."
   [phi psi]
   (list 'and
         (list 'impl phi psi)
         (list 'impl psi phi)))

(defmacro xor
  "Logical exclusive or."
  [phi psi]
  (list 'not
        (list 'equiv phi psi)))

(defmacro ite
  "Logical if-then-else."
  [i t e]
  (list 'or (list 'and i t)
            (list 'and (list 'not i) e))) 


;;## Transformation to conjunctive normal form

(defn impl-free 
  "Normalize formula `phi` such that just the operators `not`, `and`, `or` are used."
  [phi]
  (if (not (list? phi))
    phi
	  (let [op (first phi)]
      (if (contains? #{'and 'or 'not} op) 
        (list* op (map impl-free (rest phi)))
          (let [exp-phi (macroexpand-1 phi)]
            (list* (first exp-phi) (map impl-free (rest exp-phi))))))))
