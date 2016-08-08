; lwb Logic WorkBench -- Propositional Logic

; Copyright (c) 2014 - 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop
  (:require [clojure.spec :as s]
            [clojure.walk :refer (postwalk)]
            [clojure.set :refer (union intersection subset?)]
            [clojure.math.numeric-tower :refer (expt)]
            [clojure.math.combinatorics :refer (selections)]))

;; # Propositional logic

;; The namespace `lwb.prop` provides

;; * operators of propositional logic
;; * functions for checking whether a formula is well-formed
;; * evaluation of formulae in a given model
;; * the truth table for a formula
;; * normal forms: conjunctive normal form (cnf) and disjunctive normal form (dnf)


;; ## Representation of propositional formulae

;; The propositional atoms are represented by Clojure symbols, 
;; e.g. `P` or `Q`.

;; The propositional constants for truth (_verum_) and falsity (_falsum_) are represented
;; by `true` and `false`, respectively.

;; A propositional formula is an atom or a constant, or an expression composed
;; of boolean operators, propositional atoms and constants in the usual
;; lispy syntax, e.g. `(impl (and P (not P)) Q)`.

;; ## The operators of propositional logic

;; * not   -- unary, provided by Clojure
;; * and   -- n-ary, provided by Clojure
;; * or    -- n-ary, provided by Clojure
;; * impl  -- implication, binary
;; * equiv -- equivalence, binary
;; * xor   -- exclusive or, binary
;; * ite   -- if-then-else, ternary

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

;; Remark: The definitions of the operators may not be changed to functions.
;; In the transformation of a formula to cnf it is useful and necessary that the operators
;; are defined as macros

;; ## Well-formed formulae

;; We can see a formula from the syntactic perspective as a finite sequence of
;; atoms, operators and parentheses that conforms to the grammar of propositional logic.

(defn op?
  "Is `symb` an operator of propositional logic?"
  [symb]
  (contains? #{'not 'and 'or 'impl 'equiv 'xor 'ite} symb))

;; Atoms beginning with `ts` followed by a number are not allowed, since such atoms are generated
;; during the Tseitin transformation of a formula
;; but nethertheless a symbol beginning with `ts` is well-formed
(defn atom?
  "Is `symb` an atomar proposition?"
  [symb]
  (and (symbol? symb) (not (op? symb))))

(defn arity
  "Arity of operator `op`.   
   -1 means n-ary.      
   requires: `op` an operator."
  [op]
  (cond
    ('#{not} op)            1
    ('#{impl equiv xor} op) 2
    ('#{ite} op)            3
    ('#{and or} op)        -1))

(defn nary?
  "Is `op` nary?"
  [op]
  (= -1 (arity op)))

;; ### Definition of the grammar of propositional logic

;; A simple expression is an atom or a boolean constant.
(s/def ::simple-expr (s/or :bool boolean?
                           :atom atom?))

;; A compound expression is a list of an operator together with
;; several formulae as arguments whose number matches the arity of the operator.

(defn- arity-ok? [{:keys [op params]}]
  (let [arity (arity op)]
    (if (= arity -1) true
                     (= arity (count params)))))

(s/def ::compl-expr (s/and list? (s/& (s/cat :op op? :params (s/* ::fml)) arity-ok?)))

(s/def ::fml (s/or :simple-expr ::simple-expr
                   :compl-expr  ::compl-expr))

(defn wff?
  "Is the propositional formula `phi` well-formed?       
   `(wff? phi)` returns true or false.       
   `(wff? phi :msg)` returns true or a message describing the error in `phi`."
  ([phi]
   (wff? phi :bool))
  ([phi mode]
   (let [result (s/valid? ::fml phi)]
     (if result result
                (if (= mode :msg) (s/explain-str ::fml phi)
                                  result)))))

;; ## Evaluation of propositional formulae

;; A model for a formula is a function from the atoms of the formula into the set of the
;; boolean values {true, false}.

;; We represent a model as a vector of alternating atoms and boolean constants. The
;; atoms must be unique in the model.

(s/def ::model (s/and vector? (s/& (s/* (s/cat :atom atom? :value boolean?))
                                   #(apply distinct? (map :atom %)))))

(defn eval-phi
  "Evaluates the formula `phi` with the given model.        
  `model` must be a valuation `['atom1 true, 'atom2 false, ...]` for the
  propositional atoms of `phi`."
  [phi model]
  (binding [*ns* (find-ns 'lwb.prop)]
    (eval `(let ~model ~phi))))

;; Specification of the function `eval-phi`:
;; the first argument must be a well-formed formula, the second a model.
;; Furthermore the atoms in the formula must be a subset of the atoms in the model.

(defn atoms-of-phi
  "Sorted set of the propositional atoms of formula `phi`."
  [phi]
  (if (coll? phi)
    (apply sorted-set
           (filter #(not (or (op? %) (boolean? %))) (flatten phi)))
    (if (boolean? phi)
      #{}
      #{phi})))

(defn- atoms-in-model
  "Set of atoms in destructured model"
  [model]
  (set (map :atom model)))

(s/fdef eval-phi
        :args (s/and (s/cat :phi wff? :model (s/spec ::model))
                     #(subset? (atoms-of-phi (:phi %)) (atoms-in-model (:model %))))
        :ret boolean?)

;; ## Truth table

;; The truth table of a proposition `phi` is represented as a map
;; with the keys:

;; `:prop` with the proposition itself
;; `:header` a vector of the atoms and the last entry
;;  named `:result`.          
;; `:table` a vector of vectors of boolean assignments to the 
;; corresponding atom in the header as well as the result of the evaluation.

(s/def ::phi wff?)

(defn- header-wff? [header]
  (and (every? atom? (butlast header)) (= :result (last header))))

(s/def ::header (s/and vector? header-wff?))

(defn- vector-of-boolean? [vec]
  (every? boolean? vec))

(s/def ::table (s/coll-of vector-of-boolean? :kind vector?))

;; Remark: There are more constraints on the truth table:

;; 1. The atoms in ::header are just the unique atoms in ::phi
;; 2. ::table has 2^n elements for n the number of atoms (<= in mode :true-only or :false-only)
;; 3. The length of the vectors in ::table equals the length of ::header

(defn- truth-table-ok?
  [{:keys [phi header table]}]
  (let [atoms (atoms-of-phi phi)
        atoms' (set (butlast header))
        row-cnt (expt 2 (count atoms))
        col-cnt (count header)]
    (and (= atoms atoms')
         (= (count table) row-cnt)
         (every? #(= col-cnt (count %)) table))))

(s/def ::truth-table (s/and truth-table-ok? (s/keys :req-un [::phi ::header ::table])))

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

(s/fdef truth-table
        :args wff?
        :ret ::truth-table)

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

;;## Transformation to conjunctive normal form

;;### (Standardized) conjunctive normal form
;; Conjunctive normal form in lwb is defined as a formula of the form
;; `(and (or ...) (or ...) (or ...) ...)` where the clauses contain
;; only literals, no constants --
;; or the trivially true or false formulae.

;; I.e. in lwb when transforming formula to cnf, we reduce it to this
;; standard form.

; Specification of a literal
(s/def ::literal (s/or :simple-expr ::simple-expr
                       :neg (s/and list? (s/cat :not #{'not} :simple-expr ::simple-expr))))

; Specification of clause
(s/def ::clause (s/and list? (s/cat :or #{'or} :literals (s/* ::literal))))

; Specification of conjunctive normal form cnf
(s/def ::cnf (s/and list? (s/cat :and #{'and} :clauses (s/* ::clause))))

;; Caveat reader!
;; at the first sight (apply list ...) and (list* ...) seems to give the
;; same result -- BUT
;; the types are different:
;; (list? (apply list [:a :b])) => true
;; (list? (list* [:a :b]))      => false
;; since we check list? in the specs we have to be precise with the types
;; our functions are returning!

;; One may argue that in the spirit of Clojure it's better to work with
;; sequences whatever their implementation would be.
;; But: we want to use the data structure of a formula as code
;; and as such it has to be a list!

(defn literal?
  "Checks whether `phi` is a literal, i.e. a propositional atom or its negation."
  [phi]
  (s/valid? ::literal phi))

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
        (apply list op (map nnf more))
        (let [[second-op & second-more] (second phi)]
          (if (contains? #{'and 'or} second-op)
            (nnf (apply list (if (= 'and second-op) 'or 'and) (map #(list 'not %) second-more)))
            (nnf (first second-more))))))))

(defn- distr
  "Application of the distributive laws to the given formulae"
  ([phi] phi)
  ([phi-1 phi-2]
    (cond
      (and (not (literal? phi-1)) (= 'and (first phi-1)))
        (apply list 'and (map #(distr % phi-2) (rest phi-1)))
      (and (not (literal? phi-2)) (= 'and (first phi-2)))
        (apply list 'and (map #(distr phi-1 %) (rest phi-2)))
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
    (= 'and (first phi)) (apply list 'and (map nnf2cnf (rest phi)))
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
        (conj (apply list (concat (apply list pos') (map #(list 'not %) neg'))) 'or))))

(defn red-cnf
  "Reduces a formula `ucnf` of the form `(and (or ...) (or ...) ...)`."
  [ucnf]
  (let [result (filter #(not= 'true %) (map #(red-clmap (clause2sets %)) (rest ucnf)))]
    (cond
      (some #{'(or)} result) false
      (empty? result) true
      :else (conj (apply list (distinct result)) 'and))))
      ;:else (conj (list* (distinct result)) 'and))))

(defn cnf
  "Transforms `phi` to (standardized) conjunctive normal form cnf."
  [phi]
  (-> phi impl-free nnf nnf2cnf flatten-ops red-cnf))

;; Specification of function `cnf`
;; `:ret` is in cnf and equivalent to the argument `phi`.
(s/fdef cnf
        :args (s/cat :phi wff?)
        :ret (s/or :cnf ::cnf :bool boolean?))
        ;the spec of the function has a cyclic dependency as a consequence!
        ;:fn #(lwb.prop.sat/valid? (list 'equiv (-> % :args :phi) (-> % :ret))))

(defn cnf?
  "Is `phi` in (standardized) conjunctive normal form?"
  [phi]
  (s/valid? ::cnf phi))

;;## Transformation to disjunctive normal form

;; Specification of disjunctive normal form dnf

;; Specification of monom
(s/def ::monom (s/and list? (s/cat :and #{'and} :literals (s/* ::literal))))

;; Specification of dnf
(s/def ::dnf (s/and list? (s/cat :or #{'or} :monoms (s/* ::monom))))

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
                     (apply list (map mapdnfi outer))))

(defn dnf
  "Transforms `phi` to disjunctive normal form dnf."
  [phi]
  (let [cnf (cnf (list 'not phi))]
    ; border case
    (if (boolean? cnf)
      (not cnf)
      (apply list (map mapdnf cnf)))))

;; Specification of function `cnf`
;; `:ret` is in dnf and equivalent to the argument `phi`.
(s/fdef dnf
        :args (s/cat :phi wff?)
        :ret (s/or :dnf ::dnf :bool boolean?))
        ;the spec of the function has a cyclic dependency as a consequence!
        ;:fn #(lwb.prop.sat/valid? (list 'equiv (-> % :args :phi) (-> % :ret))))

(defn dnf?
  "Is `phi` in (standardized) disjunctive normal form?"
  [phi]
  (s/valid? ::dnf phi))

