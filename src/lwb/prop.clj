; lwb Logic WorkBench -- Propositional Logic

; Copyright (c) 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop
  (:require [clojure.walk :refer (postwalk)]
            [clojure.set  :refer (union intersection)]
            [clojure.math.combinatorics :refer (selections)]))

;; # Representation of propositional formulae

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
;; * impl -- implication, binary
;; * equiv -- equivalence, binary
;; * xor -- exclusive or, binary
;; * ite -- if-then-else, ternary


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

;; ## Is a formula well-formed?

(declare wff?)

(defn op-expr?
  [phi]
  (if (not (op? (first phi)))
    false
    (let [a (arity (first phi))
          c (dec (count phi))]
      (if (and (not= a -1) (not= a c))
        (throw (IllegalStateException. (str "expected operator with arity " a ", got " phi)))
        (every? wff? (rest phi))))))

(defn simple-expr?
  [phi]
  (or (torf? phi) (atom? phi)))

(defn compound-expr?
  [phi]
  (cond
    (not (list? phi)) (throw (IllegalStateException. (str "expected list, got " phi)))
    (empty? phi) (throw (IllegalStateException. "expected not empty list, got '()'."))
    (not (op? (first phi))) (throw (IllegalStateException. (str "expected operator, got " phi)))
    :else (op-expr? phi))) 

(defn wff?
  "Is the proposition `phi` well-formed ?
   `(wff? phi)` returns true or false   
   `(wff? phi :msg)` returns true or a message on the error in `phi`."
  ([phi]
   (wff? phi :bool))
  ([phi mode]
   (try
     (or (simple-expr? phi) (compound-expr? phi))
     (catch Exception e (if (= mode :msg) (.getMessage e) false)))))

;; # Thruth table

;; ## Representation of the truth table of a formula

;; The truth table of a proposition `phi` is represented as a map
;; with the keys:

;; `:prop` with the proposition itself
;; `:header` a vector of the atoms and the last entry
;;  named `:result`.          
;; `:table` a vector of vectors of boolean assignments to the 
;; corresponding atom in the header as well as the reult of the evaluation.

(defn atoms-of-phi
  "Sorted set of the propositional atoms of formula `phi`."
  [phi]
  (if (coll? phi)
    (apply sorted-set 
           (filter #(not (or (op? %) (torf? %))) (flatten phi)))
    (if (torf? phi)
      #{}
      #{phi})))

(defn eval-phi 
  "Evaluates the formula `phi` with the given assignment vector.        
  `assign-vec` must be `['atom1 true, 'atom2 false, ...]` for the
  propositional atoms of `phi`."
  [phi assign-vec]
  (binding [*ns* (find-ns 'lwb.prop)]
    (eval `(let ~assign-vec ~phi))))

;## Calculation of the truth table

(defn truth-table
  "Truth table of `phi`.   
   If `mode` is `:true-only` the table contains only the valuations where
   `phi` evaluates to `true`.   
   For `mode` of `:false-only` accordingly."
  ([phi]
  (let [atoms (atoms-of-phi phi)]
    (if (> (count atoms) 10)
      (throw (IllegalArgumentException. 
               (str "This formula has more than 10 variables." 
                    \newline 
                    "The truth table would consist of more than 1024 rows," 
                    "so you might want to use the SAT solver.")))
	    
	    (let [all-combs (selections [true false] (count atoms))
	          assign-vecs (for [comb all-combs] (vec (interleave atoms comb)))]
         {:prop phi
          :header  (conj (vec atoms) :result)
	        :table   (vec (for [assign-vec assign-vecs]
                          (conj (vec (take-nth 2 (rest assign-vec)))
                                (eval-phi phi assign-vec))))}))))
  ([phi mode]
    (let [tt (truth-table phi)]
    (condp = mode
      :true-only (assoc tt :table (vec (filter #(true? (last %)) (:table tt))))
      :false-only (assoc tt :table (vec (filter #(false? (last %)) (:table tt))))
      tt))))

(defn print-table
  "Pretty prints vector `header` and vector of vectors `table`.   
   Pre: header and row in the table have the same no of item,
        the size of items in header is >= size of items in the rows."
  ; inspired from clojure.pprint
  [header table]
  (let [table'  (vec (map #(replace {true "T" false "F"} %) table)) 
        widths  (map #(count (str %)) header)
        spacers (map #(apply str (repeat % "-")) widths)
        fmts    (map #(str "%" % "s") widths)
        fmt-row (fn [leader divider trailer row]
                  (str leader
                     (apply str (interpose divider
                        (for [[col fmt] (map vector row fmts)]
                              (format fmt (str col)))))
                       trailer))]
    (println)
    (println (fmt-row "| " " | " " |" header))
    (println (fmt-row "|-" "-+-" "-|" spacers))
    (doseq [row table']
      (println (fmt-row "| " " | " " |" row)))))

(defn print-truth-table
  "Pretty prints truth-table."
  [{:keys [prop header table]}]
    [prop header table]
  (do
    (println "Truth table")
    (println prop)
	  (print-table header table)))

(comment
  (def tt (truth-table '(or p q)))
  (print-truth-table tt)
  )

;;# Transformation to conjunctive normal form

;;## (Standardized) conjunctive normal form
;; Conjunctive normal form in lwb is defined as a formula of the form
;; `(and (or ...) (or ...) (or ...) ...)` where the clauses contain
;; only literals, no constants --
;; or the trivially true or false formulae.

;; I.e. in lwb when transforming formula to cnf, we reduce it to this
;; standard form.

(defn literal?
  "Checks whether `phi` is a literal, i.e. a propositional atom or its negation."
  [phi]
  (or (atom? phi) (torf? phi)
      (and (list? phi) (= 2 (count phi)) (= 'not (first phi)) 
           (or (atom? (second phi)) (torf? (second phi))))))

(defn impl-free 
  "Normalize formula `phi` such that just the operators `not`, `and`, `or` are used."
  [phi]
  (if (literal? phi)
    phi
	  (let [op (first phi)]
      (if (contains? #{'and 'or 'not} op) 
        (apply list op (map impl-free (rest phi)))
        (let [exp-phi (macroexpand-1 phi)]
          (apply list (first exp-phi) (map impl-free (rest exp-phi))))))))

(defn nnf
  "Transforms an impl-free formula `phi` into negation normal form."
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
  "Application of the distributive laws to the given formulae"
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
  "Transforms the formula `phi` from nnf to cnf."
  [phi]
  (cond
    (literal? phi) (list 'and (list 'or phi))
    (= '(or) phi) '(and (or))
    (= 'and (first phi)) (list* 'and (map nnf2cnf (rest phi)))
    (= 'or (first phi)) (apply distr (map nnf2cnf (rest phi)))))

(defn flatten-ops
  "Flattens the formula `phi`.      
   Nested binary applications of n-ary operators will be transformed to the n-ary form,
   e.g. `(and (and a b) c) => (and a b c)`."
  [phi]
  (let [flat-step (fn [sub-phi]
						        (if (and (coll? sub-phi) (nary? (first sub-phi)))
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
   and `:neg` atoms in the clause."
  [cl]
  (apply merge-with union
         (for [literal (rest cl)]
					  (if (or (symbol? literal) (instance? Boolean literal))
					      {:pos #{literal}}
					      {:neg #{(second literal)}}))))

(defn- red-clmap 
  "Reduces a clause in the form `{:pos #{...} :neg #{...}`    
   by considering:     
   (1) :pos contains true  -> clause is trivially true,   
   (2) :neg contains false -> clause is trivially true,   
   (3) :pos contains false -> false can be deleted,   
   (4) :neg contains true  -> true can be deleted."

  [{:keys [pos neg]}]
  (cond
    (> (count (intersection pos neg)) 0) true
    (contains? pos 'true) true
    (contains? neg 'false) true
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
  "Transforms `phi` to (standardized) conjunctive normal form cnf."
  [phi]
  (-> phi impl-free nnf nnf2cnf flatten-ops red-cnf))

(defn cnf?
  "Is `phi` in (standardized) conjunctive normal form?"
  [phi]
  (let [clause? (fn [psi] (and (list? psi) (= 'or (first psi)) (every? literal? (rest psi))))]
    (and (list? phi) (= 'and (first phi)) (every? clause? (rest phi)))))

; helper to transform (cnf (not phi)) to (dnf phi)
(defn- mapdnfi
  "maps clause to monom in transformation to dnf."
  [inner]
  (cond
    (= inner 'or) 'and
    (list? inner) (second inner)
    :else (list 'not inner)))

(defn- mapdnf
  [outer]
  (if (= outer 'and) 'or
                     (map mapdnfi outer)))

(defn dnf
  "Transforms `phi` to disjunctive normal form dnf."
  [phi]
  (let [cnf (cnf (list 'not phi))]
    (map mapdnf cnf)))

