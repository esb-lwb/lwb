; lwb Logic WorkBench -- Propositional Logic SAT

; Copyright (c) 2014 - 2017 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.sat
  "Satisfiability in propositional logic."
  (:require [lwb.prop :refer :all]
            [lwb.prop.nf :refer (cnf cnf?)]
            [clojure.set :refer (map-invert)]
            [clojure.string :refer (starts-with?)]
            [clojure.zip :as z]
            [clojure.spec :as s]
            [clojure.walk :as walk])
  (:import (org.sat4j.minisat SolverFactory)
           (org.sat4j.core VecInt)
           (org.sat4j.specs ContradictionException)
           (clojure.lang LazySeq)))

;; # Satisfiability in propositional logic

;; The namespace `lwb.prop.sat` provides

;; - transformation of a propositional formula in cnf to the
;;   dimacs format used by SAT solvers
;; - the Tseitin transformation of an arbitrary formula in propositional logic
;;   into a formula in cnf thats equivalent with resprct to satisfiability
;; - satisfiability in propositional logic

;; ## Transformation of a formula in cnf to dimacs format

;; A formula in dimacs format is organized as a map with the
;; following entries:

;; - `:formula`    the formula in cnf, but atoms beginning with `ts-` are allowed
;; - `:num-atoms`  the number of atoms in the formula
;; - `:int-atoms`  the mapping of integers in dimacs to the atoms 
;; - `:num-cl`     the number of clauses
;; - `:cl-set`     the set of clauses encoded with the integers

(s/def ::formula   :lwb.prop.nf/cnf)
(s/def ::num-atoms pos-int?)
(s/def ::int-atoms (s/map-of pos-int? atom?))
(s/def ::num-cl    pos-int?)
(s/def ::cl-set    (s/coll-of set? :kind set? ))

(s/def ::dimacs (s/keys :req-un [::formula ::num-atoms ::int-atoms ::num-cl ::cl-set]))
;; Remark: obviously there are more constraints not yet expressed in this spec

(defn- cl->dimacs
  "Transforms a clause `cl` of the form `(or literal literal ...)` to a set
   of integers according to the mapping `atoms-int`."
  [cl atoms-int]
  (let [lit->int (fn [literal] 
                  (if (list? literal) (- ((second literal) atoms-int)) (literal atoms-int)))]
    (set (map lit->int  (rest cl)))))
  
(defn cnf->dimacs
  "Transforms `phi` in (standardized) cnf to a map with dimacs informations."
  [phi]
  (let [atoms     (atoms-of-phi phi)
        num-atoms (count atoms)
        atoms-int (zipmap atoms (range 1 (inc num-atoms)))
        cl-set    (set (map #(cl->dimacs % atoms-int) (rest phi)))]
    {:formula   phi
     :num-atoms num-atoms
     :int-atoms (map-invert atoms-int)
     :num-cl    (count cl-set)
     :cl-set    cl-set}))

(s/fdef cnf->dimacs
        :args (s/cat :phi ::formula)
        :ret ::dimacs)

;; ### Using SAT4J as a SAT solver
(def ^:dynamic *sat4j-timeout*
  "Timeout of Sat4j in seconds."
  300)

(defn sat4j-solve
  "Checks whether the formula in `dimacs-map` is satisfiable.   
   Returns nil if not, a model if it is satisfiable." 
  [{:keys [num-atoms cl-set num-cl int-atoms]}]
  (let [solver     (SolverFactory/newDefault)
        make-model (fn [model int-atoms]
                     (into {} (map #(if (pos? %) 
                                    [(int-atoms %) true] 
                                    [(int-atoms (- %)) false]) model)))]
    (try
      (.setTimeout solver *sat4j-timeout*)
      (.newVar solver num-atoms)
      (.setExpectedNumberOfClauses solver num-cl)

      (dorun (map #(.addClause solver (VecInt. (int-array %))) cl-set))

      (when (.isSatisfiable solver)
        (make-model (vec (.model solver)) int-atoms))
      (catch ContradictionException _ nil))))

(s/fdef sat4j-solve
        :args (s/cat :dimacs-map ::dimacs)
        :ret (s/or :model :lwb.prop/model :nil nil?))

;; ## Tseitin transformation

;; The Tseitin transformation takes a formula `phi` and generates in linear time
;; a formula which is equivalent with respect to satisfiability.

(def ^:private tseitin-prefix
  "Prefix for tseitin symbols."
  "ts-")

(defn- tseitin-symbol-generator
  "A function that generates unique tseitin symbols,
   beginning with `ts-1`."
  []
  (let [cnt    (atom 0)]
    #(symbol (str tseitin-prefix (swap! cnt inc)))))

(defn- mark-phi
  "Marks each branch of `phi` with a unique tseitin symbol.   
   The root has the symbol `ts-1`."
  [phi]
  (let [tsg     (tseitin-symbol-generator)
        zipper  (z/seq-zip phi)
        mark-fn (fn [node] (vary-meta node assoc :tseitin-symbol (tsg)))]
    (loop [loc zipper]
      (if (z/end? loc)
        (z/root loc)
        (recur (z/next (if (z/branch? loc) (z/edit loc mark-fn) loc)))))))

(defn- locs-phi
  "Sequence of all the locations of a zipper generated from the formula `phi`."
  [phi]
  (take-while (complement z/end?) (iterate z/next (z/seq-zip phi))))

(defn- iterate-while
  "Iterate beginning with `x` while `f` returns not nil"
  [f x]
   (lazy-seq (when x (cons x (iterate-while f (f x))))))

(defn- children-locs
  "Sequence of children locs at branch"
  [branch]
  (iterate-while z/right (z/down branch)))

(defn- tseitin-branch
  "Analyzes branch and generates equivalence formula for the branch.  
   The formula is of the form `(equiv tsx (atoms or tseitin symbol of children))`."
  [branch]
  (let [children (map #(if (z/branch? %) (:tseitin-symbol (meta  (z/node %))) (z/node %)) (children-locs branch))]
    (list 'equiv (:tseitin-symbol (meta (z/node branch))) children)))
                                 
(defn tseitin
  "Tseitin transformation for formula `phi`.   
   (1) The formula is marked with tseitin symbols for the branches.    
   (2) The tseitin equivalences for the branches are generated.  
   (3) CNF of these is calculated.    
   (4) The final formula with `ts-1` for the root is build and simplified."
  [phi]
  (cond
    ; border cases
    (boolean? phi) phi
    (atom? phi) (list 'and (list 'or phi))
    ; actual transformation
    :else
		  (let [ts-1 (symbol (str tseitin-prefix 1))
            branches (map tseitin-branch (filter z/branch? (locs-phi (mark-phi phi))))
            ; a hack to get rid of lazy seqs
            walk-fn (fn [node] (if (instance? LazySeq node) (apply list node) node))
            non-lazy-branches (walk/postwalk walk-fn branches)
            parts (apply list (map cnf non-lazy-branches))]
        (walk/postwalk walk-fn (flatten-ops (into (conj parts (list 'and (list 'or ts-1))) '(and)))))))

(defn- no-tseitin-symbol?
  "Checks whether the formuka `phi` does not have an atom of the
  form of a tseitin symbol, i.e. that matches #\"ts-\\d+\"."
  [phi]
  (empty? (filter #(re-matches #"ts-[0-9]+" (name %)) (atoms-of-phi phi))))

(s/fdef tseitin
        :args (s/cat :phi (and wff? no-tseitin-symbol?))
        :ret :lwb.prop.nf/cnf)

(defn- remove-tseitin-symbols
  "Removes the tseitin-symbols and their value from a model."
  [model]
  (into {} (filter #(not (starts-with? (name (key %)) tseitin-prefix)) model)))

(defn- model2negated-cnf
  "Transforms a model into a formula in cnf with negated truth value."
  [model]
  (let [negate (fn [[atom value]] (if value (list 'not atom) atom))]
    (apply list 'or (map negate model))))

;; ## Satisifiability in propositional logic
    
(defn sat
  "Gives a model for `phi` if the formula is satisfiable, nil if not.   
   If `phi` is trivially valid, the result is true.   
   Mode `:all` returns a sequence of all the satisfying assignments."
  ([phi]
    (sat phi :one))
  ([phi mode]
    (cond
	   ; border cases
     (= phi 'true) true
     (= phi 'false) nil
     :else
	     (case mode
	       :all 
		       (loop  [f phi, results '()]
		        (let [sol (sat f)]
	             (if (nil? sol) results
		               (recur (list 'and (model2negated-cnf sol) f) (conj results sol)))))
	       ; default
		       (let [tcnf   (if (cnf? phi) phi (tseitin phi))
		             dimacs (cnf->dimacs tcnf)
		             res    (sat4j-solve dimacs)]
		         (when res
		           (remove-tseitin-symbols res)))))))

(s/fdef sat
        :args (s/alt :1-args (s/cat :phi (and wff? no-tseitin-symbol?))
                     :2-args (s/cat :phi (and wff? no-tseitin-symbol?) :mode #{:one :all}))
        :ret (s/nilable (s/or :bool boolean? :model :lwb.prop/model :models (s/coll-of :lwb.prop/model))))

(defn sat?
  "Is `phi` satisfiable?"
  [phi]
  (if (nil? (sat phi)) false true))

(s/fdef sat?
        :args (s/cat :phi (and wff? no-tseitin-symbol?))
        :ret boolean?)

(defn valid?
  "Is `phi` valid?"
  [phi]
  (not (sat? (list 'not phi))))

(s/fdef valid?
        :args (s/cat :phi (and wff? no-tseitin-symbol?))
        :ret boolean?)

(defn true-only
  "Sequence of true atoms in a model"
  [model]
  (map first (filter #(true? (val %)) model)))
