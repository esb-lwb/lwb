; lwb Logic WorkBench -- Propositional Logic

; Copyright (c) 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop
  (:require [clojure.walk :refer (postwalk)]
            [clojure.set  :refer (union intersection)]))

;; ## Representation of propositional formulae

;; The propositional atoms are represented by Clojure symbols, 
;; e.g. `p` or `q`.

;; The propositional constants for truth and falsity are represented
;; by `true` and `false`, respectively.

;; A propositional formula is an atom or a constant, or an expression composed
;; of boolean operators, propositional atoms and constants in the usual
;; lispy syntax, e.g. `(impl (and p (not p)) q)`.

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

;;# Constants and utility functions in the context of operators

(def reserved-symbols 
  "Reserved symbols, i.e. constants and operators in formulae."
  #{'true 'false 'not 'and 'nand 'or 'nor 'impl 'nimpl 'cimpl 'ncimpl 'equiv 'xor 'ite})

(def n-ary-ops
  "Set of n-ary operators."
  #{'and 'or})

(defn n-ary?
  "Is `op` n-ary?"
  [op]
  (contains? n-ary-ops op))


;;## Thruth table


;;## Transformation to conjunctive normal form

;; Conjunctive normal form in lwb is defined as a formula of the form
;; `(and (or ...) (or ...) (or ...) ...)` where the clauses contain
;; only literals, no constants --
;; or the trivially true or false formulae.

;; I.e. in lwb when transforming formula to cnf, we reduce it the this
;; standard from.

(defn atom?
  "Checks whether 'phi' is a propositional atom or a constant."
  [phi]
  (or (symbol? phi) (instance? Boolean phi)))

(defn literal?
  "Checks whether `phi` is a literal, i.e. a propositional atom or its negation."
  [phi]
  (or (atom? phi) 
      (and (list? phi) (= 2 (count phi)) (= 'not (first phi)) (atom? (second phi)))))
  
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

(defn nnf2cnf 
  "Transforms the formula `phi` in nnf to cnf."
  [phi]
  (cond
    (literal? phi) (list 'and (list 'or phi))
    (= '(or) phi) '(and (or))
    (= 'and (first phi)) (list* 'and (map nnf2cnf (rest phi)))
    (= 'or (first phi)) (apply distr (map nnf2cnf (rest phi)))))

(defn flatten-ops
  "Flats the formula `phi`.
   Nested binary applications of n-ary operators will be transformed to the n-ary form,
   e.g. `(and (and a b) c) => (and a b c)`."
  [phi]
  (let [flat-step (fn [sub-phi]
						        (if (and (coll? sub-phi) (n-ary? (first sub-phi)))
						          (let [op (first sub-phi)
                            args (rest sub-phi)
						                flat-filter (fn [op arg] (if (coll? arg) (= op (first arg)) false))
						                flat (map #(rest %) (filter (partial flat-filter op) args))
						                not-flat (filter (partial (complement flat-filter) op) args)]
						            (apply concat `((~op) ~@flat ~not-flat)))
					             sub-phi))]
        (postwalk flat-step phi)))

(defn- clause2sets
  "Transforms a clause `(or ...)` into a map of the sets of `:pos` atoms 
   and `:neg`atoms in the clause."
  [cl]
  (apply merge-with union
         (for [literal (rest cl)]
					  (if (or (symbol? literal) (instance? Boolean literal))
					      {:pos #{literal}}
					      {:neg #{(second literal)}}))))

(defn- red-clmap 
  "Reduces a clause in the form `{:pos #{...} :neg #{...}`."
  [{:keys [pos neg]}]
  (cond
    (> (count (intersection pos neg)) 0)
      true
    (contains? pos 'true)
      true
    (contains? neg 'false)
      true
    :else
    (let [pos' (filter #(not= 'false %) pos), neg' (filter #(not= 'true %) neg)]
      (conj (concat (list* pos') (map #(list 'not %) neg')) 'or))))

(defn red-cnf
  "Reduces a formula `ucnf` of the form `(and (or ...) (or ...) ...)`."
  [ucnf]
  (let [result (filter #(not= 'true %) (map #(red-clmap (clause2sets %)) (rest ucnf)))]
    (cond
      (some #{'(or)} result) false
      (empty? result) true
      :else (conj (list* (distinct result)) 'and))))

(defn cnf
  "Transforms the propositional formula `phi` to (standardized) conjunctive normal form cnf."
  [phi]
  (-> phi impl-free nnf nnf2cnf flatten-ops red-cnf))

        