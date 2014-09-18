; lwb Logic WorkBench -- Propositional Logic

; Copyright (c) 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop)

;; ## Representation of propositional formulae

;; The propositional atoms are represented by Clojure symbols, 
;; e.g. `p` or `q`.

;; The propositional constants for truth and falsity are represented
;; by `true` and `false`, respectively.

;; A propositional formula is an atom or a constant, or an expression composed
;; of boolean operators, propositional atoms and constants in the usual
;; lisppy syntax, e.g. `(impl (and p (not p)) q)`.

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

(defn literal?
  "Checks whether `phi` is a literal, i.e. a propositional atom or its negation."
  [phi]
  (or (instance? Boolean phi)
      (symbol? phi) 
      (and (list? phi) (= 2 (count phi)) (= 'not (first phi)) (symbol? (second phi)))))
  
(defn impl-free 
  "Normalize formula `phi` such that just the operators `not`, `and`, `or` are used."
  [phi]
  (if (literal? phi)
    phi
	  (let [op (first phi)]
      (if (contains? #{'and 'or 'not} op) 
        (list* op (map impl-free (rest phi)))
          (let [exp-phi (macroexpand-1 phi)]
            (list* (first exp-phi) (map impl-free (rest exp-phi))))))))

(defn nnf
  "Transforms an impl-free formula into negation normal form."
  [phi]
  (if (literal? phi) 
    phi
    (let [[op & more] phi]
      (if (contains? #{'and 'or} op)
        (list* op (map nnf more))
        (let [[second-op & second-more] (second phi)]
          (if (contains? #{'and 'or} second-op)
            (nnf (list* (if (= 'and second-op) 'or 'and) (map #(list 'not %) second-more)))
            (nnf (first second-more))))))))

(defn- distr
  "Application of the distributive laws to n formulae"
  ([phi] phi)
  ([phi-1 phi-2]
    (cond
      (and (not (literal? phi-1)) (= 'and (first phi-1)))
        (list* 'and (map #(distr % phi-2) (rest phi-1)))
      (and (not (literal? phi-2)) (= 'and (first phi-2)))
        (list* 'and (map #(distr phi-1 %) (rest phi-2)))
      :else
        (list 'or phi-1 phi-2)))
  ([phi-1 phi-2 & more]
    (reduce distr (distr phi-1 phi-2) more)))

(defn- nnf2cnf 
  "Transforms the formula `phi` in nnf to cnf."
  [phi]
  (cond
    (literal? phi) (list 'and (list 'or phi))
    (= 'and (first phi)) (list* 'and (map nnf2cnf (rest phi)))
    (= 'or (first phi)) (apply distr (map nnf2cnf (rest phi)))))

(defn cnf
  "Transforms the propositional formula `phi` to conjunctive normal form cnf."
  [phi]
  (-> phi impl-free nnf cnf flatten-ast))


        