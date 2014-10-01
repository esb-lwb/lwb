; lwb Logic WorkBench -- Propositional Logic SAT

; Copyright (c) 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.


(ns lwb.prop.sat
  (:require [lwb.prop :refer (atoms-of-phi)]
            [clojure.set :refer (map-invert)])
  (:import  (org.sat4j.minisat SolverFactory)
            (org.sat4j.minisat.core Solver)
            (org.sat4j.core VecInt)
            (org.sat4j.specs IProblem)
            (org.sat4j.reader DimacsReader)))

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

(def x (cnf2dimacs '(and (or p q) (or (not p) s))))

;; ## SAT4J

(defn- sat4j-solve
  "Checks whether the formula in `dimacs-map` is satisfiable.   
   Returns nil if not, an assignment vector if it is satisfiable." 
  [dimacs-map]
  (let [solver     (SolverFactory/newDefault)
        cl-set     (:cl-set dimacs-map)
        num-atoms  (:num-atoms dimacs-map)
        num-cl     (:num-cl dimacs-map)
        assign-vec (fn [model-vec int-atoms]
                     (vec (flatten (map #(if (pos? %) [(ia %) true] [(ia (- %)) false]) v))))]
    (.newVar solver num-atoms)
    (.setExpectedNumberOfClauses solver num-cl)

    (doall (map #(.addClause solver (VecInt. (int-array %))) cl-set))

    (when (.isSatisfiable solver)
      (assign-vec (vec (.model solver)) (:int-atoms dimacs-map)))))

(def v (sat4j-solve x))
