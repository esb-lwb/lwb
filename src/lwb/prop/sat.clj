; lwb Logic WorkBench -- Propositional Logic SAT

; Copyright (c) 2014 - 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.


(ns lwb.prop.sat
  (:require [lwb.prop :refer :all]
            [clojure.set :refer (map-invert)]
            [clojure.zip :as z])
  (:import  (org.sat4j.minisat SolverFactory)
            (org.sat4j.core VecInt)
            (org.sat4j.specs ContradictionException)))

;; ## Transformation of a formula in cnf to dimacs

;; The information in dimacs is organized as a map with the
;; following entries:

;; - `:formula`    the formula in cnf
;; - `:num-atoms`  the number of atoms in the formula
;; - `:int-atoms`  the mapping of integers in dimacs to the atoms 
;; - `:cl-set`     the set of clauses encoded with the integers

(defn- cl2dimacs
  "Transforms a clause `cl` of the form `(or literal literal ...)` to a set
   of integers according to the mapping `atoms-int`."
  [cl atoms-int]
  (let [lit2int (fn [literal] 
                  (if (list? literal) (- ((second literal) atoms-int)) (literal atoms-int)))]
    (set (map lit2int  (rest cl)))))
  
(defn cnf2dimacs
  "Transforms `phi` in (standardized) cnf to a map with dimacs informations."
  [phi]
  (let [atoms     (atoms-of-phi phi)
        num-atoms (count atoms)
        atoms-int (zipmap atoms (range 1 (inc num-atoms)))
        cl-set    (set (map #(cl2dimacs % atoms-int) (rest phi)))]
    {:formula   phi
     :num-atoms num-atoms
     :int-atoms (map-invert atoms-int)
     :num-cl    (count cl-set)
     :cl-set    cl-set}))

;; ## SAT4J
(def ^:dynamic *sat4j-timeout*
  "Timeout of Sat4j in seconds."
  300)

(defn- sat4j-solve
  "Checks whether the formula in `dimacs-map` is satisfiable.   
   Returns nil if not, an assignment vector if it is satisfiable." 
  [dimacs-map]
  (let [solver     (SolverFactory/newDefault)
        cl-set     (:cl-set dimacs-map)
        num-atoms  (:num-atoms dimacs-map)
        num-cl     (:num-cl dimacs-map)
        assign-vec (fn [model-vec int-atoms]
                     (vec (flatten 
                            (map #(if (pos? %) 
                                    [(int-atoms %) true] 
                                    [(int-atoms (- %)) false]) model-vec))))]
    (try
      (.setTimeout solver *sat4j-timeout*)
      (.newVar solver num-atoms)
      (.setExpectedNumberOfClauses solver num-cl)

      (dorun (map #(.addClause solver (VecInt. (int-array %))) cl-set))

      (when (.isSatisfiable solver)
        (assign-vec (vec (.model solver)) (:int-atoms dimacs-map)))
      (catch ContradictionException _ nil))))


;; ## Tseitin transformation

(def tseitin-prefix
  "Prefix for tseitin symbols."
  "ts_")

(defn- tseitin-symbol-generator
  "A function that generates unique tseitin symbols,
   beginning with `ts_1`."
  []
  (let [cnt    (atom 0)]
    #(symbol (str tseitin-prefix (swap! cnt inc)))))

(defn- mark-phi
  "Marks each branch of `phi` with a unique tseitin symbol.   
   The root has the symbol `ts_1`."
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

; may also be achieved by function from clojure.data.zip
(defn- children-locs
  "Sequence of children locs at branch"
  [branch]
  (iterate-while z/right (z/down branch)))

(defn- tseitin-branch
  "Analyzes branch and generates equivalence formula for the branch.  
   The formula is of the form `(equiv ts_x (atoms or tseitin symbol of children)`."
  [branch]
  (let [children (map #(if (z/branch? %) (:tseitin-symbol (meta  (z/node %)))  (z/node %)) (children-locs branch))]
    (list 'equiv (:tseitin-symbol (meta (z/node branch))) children)))
                                 
(defn tseitin
  "Tseitin transformation for formula `phi`.   
   (1) The formula is marked with tseitin symbols for the branches.    
   (2) The tseitin equivalences for the branches are generated.  
   (3) CNF of these is calculated.    
   (4) The final formula with `ts_1` for the root is build and simplified."
  [phi]
  (cond
    ; border cases
    (boolean? phi) phi
    (atom? phi) (list 'and (list 'or phi))
    ; actual transformation
    :else
		  (let [parts (map cnf (map tseitin-branch (filter z/branch? (locs-phi (mark-phi phi)))))]
		    (flatten-ops (cons 'and (cons '(and (or ts_1)) parts))))))

(defn- remove-tseitin-symbols
  "Removes the tseitin-symbols and their value from an assignment vector."
  [assign-vec]
  (let [zipper (z/vector-zip assign-vec)]
    (loop [loc zipper]
      (if (z/end? loc)
        (z/root loc)
        (recur (z/next (if (.startsWith (str (z/node loc)) tseitin-prefix) (z/remove (z/next (z/remove loc))) loc)))))))

(defn- assign-vec2negated-cnf
  "Transforms an assignment vector into a formula in cnf with negated truth value."
  [assign-vec]
  (let [pairs (partition 2 assign-vec)
        negate (fn [[atom value]] (if value (list 'not atom) atom))]
    (apply list 'or (map negate pairs))))
    
(defn sat
  "Gives an assignment vector for `phi` if the formula is satisfiable, nil if not.   
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
		               (recur (list 'and (assign-vec2negated-cnf sol) f) (conj results sol)))))
	       ; default
		       (let [tcnf   (if (cnf? phi) phi (tseitin phi))
		             dimacs (cnf2dimacs tcnf)
		             res    (sat4j-solve dimacs)]
		         (if res
		           (remove-tseitin-symbols res) nil))))))

(defn sat?
  "Is `phi` satisfiable?"
  [phi]
  (if (nil? (sat phi)) false true))

(defn valid?
  "Is `phi` valid?"
  [phi]
  (not (sat? (list 'not phi))))

(defn true-only
  "Sequence of true atoms in an assignment vector"
  [assign-vec]
  (loop [vec assign-vec, result []]
    (if (empty? vec)
      result
      (let [atom (first vec) value (second vec)]
        (recur (subvec vec 2) (if value (conj result atom) result))))))

(comment
  (def example '(impl (and a1 a2) x))
  (cnf example)
  (tseitin example)
  (sat example)
  (sat? example)
  (valid? example)

  (valid? '(and p (not p)))
  (sat? '(and p (not p)))
  (sat? '(or p (not p)))
  (valid? '(or p (not p)))
  
  (def prop '(or c000))
  prop
  (sat prop)

  (def example-from-lecture '(or P1 (and P2 (impl P3 P4))))
  (tseitin example-from-lecture)
  (sat example-from-lecture)
  )
