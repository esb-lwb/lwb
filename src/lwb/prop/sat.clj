; lwb Logic WorkBench -- Propositional Logic SAT

; Copyright (c) 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.


(ns lwb.prop.sat
  (:require [lwb.prop :refer (atoms-of-phi operator? locs-phi arity)]
            [clojure.set :refer (map-invert)]
            [clojure.zip :as z])
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
                     (vec (flatten 
                            (map #(if (pos? %) 
                                    [(int-atoms %) true] 
                                    [(int-atoms (- %)) false]) model-vec))))]
    (.newVar solver num-atoms)
    (.setExpectedNumberOfClauses solver num-cl)

    (dorun (map #(.addClause solver (VecInt. (int-array %))) cl-set))

    (when (.isSatisfiable solver)
      (assign-vec (vec (.model solver)) (:int-atoms dimacs-map)))))


;; ## Tseitin transformation

(defn- tseitin-symbol-generator
  "A function that generates unique tseitin symbols."
  []
  (let [prefix "ts_"
        cnt    (atom 0)]
    #(symbol (str prefix (swap! cnt inc)))))

(defn- mark-phi
  "Marks each branch of phi with a unique tseitin symbol."
  [phi]
  (let [tsg     (tseitin-symbol-generator)
        zipper  (z/seq-zip phi)
        mark-fn (fn [node] (vary-meta node assoc :tseitin-symbol (tsg)))]
    (loop [loc zipper]
      (if (z/end? loc)
        (z/root loc)
        (recur (z/next (if (z/branch? loc) (z/edit loc mark-fn) loc)))))))

;; now we can construct the tseitin formula from the annotated tree

(defn- tseitin-branch
  "Analyzes branch and generates `[ts_branch (op and atom or tseitin-symbol of children)`."
  [branch]
  (let [children (map #(if (list? %) (:tseitin-symbol (meta  %))  %) (z/children branch))]
    [(:tseitin-symbol (meta (z/node branch))) children]))
                                 
(map tseitin-branch (filter z/branch? (locs-phi (mark-phi '(and p (or q s))))))

(defn- tseitin-cnf
  [t [op & args]]
  (case (arity op)
    1
    (let [a (first args)]
      (case op
         not '(and (or t a) (or (not t) (not a)))))
    2
    (let [a (first args) b (second args)]
      (case op
         nand   '(and (or t a) (or t b) (or (not t) (not a) (not b)))
         nor    '(and (or (not t) (not a)) (or (not t) (not b)) (or t a b))
         impl   '(and (or t a) (or t (not b)) (or (not t) (not a) b))
         nimpl  '(and (or (not t) a) (or (not t) (not b)) (or t (not a) b))
         cimpl  '(and (or t (not a)) (or t b) (or (not t) a (not b)))
         ncimpl '(and (or (not t) (not a)) (or (not t) b) (or t a (notb )))
         equiv  '(and (or (not t) a (not b)) (or (not t) (not a) b) (or t (not a) (not b)) (or t a b))
         xor    '(and (or t (not a) b) (or t (not a) b) (or (not t) a b) (or (not t) (not a) (not b)))))
    3
    (let [a (first args) b (second args) c (last args)]
      (case op
         ite    '(and ...todo)))
    -1 
    (case op
        and (apply list 'and
            (apply list 'or t (map #(list 'not %) args))
            (map #(list 'or (list 'not t) %) args))
        or  (apply list 'and
            (apply list 'or (list 'not t) args)
            (map #(list 'or t (list 'not %)) args)))
))