; lwb Logic WorkBench -- Language of propositional and predicate logic

; Copyright (c) 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.lang)

;; # The logical operators

;; * not -- unary, provided by Clojure
;; * and -- n-ary, provided by Clojure
;; * or  -- n-ary, provided by Clojure
;; * impl -- implication, binary
;; * equiv -- equivalence, binary
;; * xor -- exclusive or, binary
;; * ite -- if-then-else, ternary

;; ## Implementation of the operators

;; ### Hints
;; 1. The operators are defined as macros
;; 2. We don't use syntax-quoting, because we don't want to have qualified
;;    names after expansion
;; 3. The macros must not be changed to functions,
;;    because transformations (to cnf e.g.) use them with macroexpand!

(defmacro impl
  "Logical implication."
  [phi psi]
  (list 'or
        (list 'not phi) psi))

(defmacro equiv
  "Logical equivalence."
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

;; ## Utility functions in the context of operators

(defn op?
  "Is `symb` an operator of propositional logic?"
  [symb]
  (let [operators #{'not 'and 'or 'impl 'equiv 'xor 'ite}]
    (contains? operators symb)))

(defn torf?
  "Is `symb` a constant of logic, i.e. `true` or `false`?"
  [symb]
  (or (= 'true symb) (= 'false symb)))

(defn atom?
  "Is `symb` an atomar proposition?"
  [symb]
  (and (symbol? symb) (not (op? symb)) (not (torf? symb))))

(defn arity
  "Arity of operator `op`.   
   -1 means n-ary, but better use `n-ary?`.
   requires: `op` an operator."
  [op]
  (cond
    (= op 'not) 1
    (contains? #{'impl 'equiv 'xor} op) 2
    (= op 'ite) 3
    (contains? #{'and 'or} op) -1))

(defn unary?
  "Is `op` an unary operator?"
  [op]
  (= 1 (arity op)))

(defn binary?
  "Is `op` a binary operator?"
  [op]
  (= 2 (arity op)))

(defn ternary?
  "Is `op` a ternary operator?"
  [op]
  (= 3 (arity op)))

(defn nary?
  "Is `op` an n-ary operator?"
  [op]
  (= -1 (arity op)))

;; ## Quantors in predicate logic

(defn quantor?
  "Is `symb` a quantor?"
  [symb]
  (or (= 'forall symb) (= 'exists symb)))

;; ## Equality in predicate logic

(defn eq?
  "Is `symb` equality in predicate logic??"
  [symb]
  (= 'eq symb))


