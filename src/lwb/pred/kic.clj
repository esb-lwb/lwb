; kic - Kodkod in Clojure
; based on the Kodkod Constraint Solver, see http://emina.github.io/kodkod/
; licensed under the MIT license

; Copyright (c) 2014 - 2018 by Burkhardt Renz THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns ^{:doc    "An ultra-thin wrapper to the Kodkod constraint solver."
      :author "Burkhardt Renz, THM"}
lwb.pred.kic
  (:refer-clojure :exclude [and or not some])
  (:require [clojure.set :as set])
  (:import (kodkod.ast Formula Expression Decls Variable Relation)
           (kodkod.instance Universe Bounds Tuple TupleSet TupleFactory)
           (kodkod.engine Solver Solution)
           (java.util Collection)))

;; ##Universe

(defn universe
  "A Kodkod Universe from the given vector of items."
  [set]
  (Universe. ^Collection set))

;; ##Instances

(defn factory 
  "A Kodkod TupleFactory for the given universe."
  [universe]
  (.factory universe))

(defn bounds 
  "A Kodkod bounds object for the given universe."
  [universe]
  (Bounds. universe))

;; ##Relations

(defn relation
  "A Kodkod relation with the given `name` and `arity'."
  [name arity]
  (Relation/nary name arity))

(defn make-const
  "A Kodkod relation for a const or nullary function with
  corresponding bounds."
  [key factory bounds]
  (let [r  (Relation/unary (name key))
        ts (.setOf factory (to-array [key]))
        _  (.boundExactly bounds r ts)]
    [(symbol (name key)) r]))

(defn make-pred
  "A Kodkod relation for a predicate with
  corresponding bounds."
  [key arity factory bounds]
  (let [r  (Relation/nary (name key) arity)
        ts (.allOf ^TupleFactory factory arity)
        _  (.bound bounds r ts)]
    [(symbol (name key)) r]))

(defn make-func
  "A Kodkod relation for a function with
  corresponding bounds.     
  A function is represented as a relation where the last
  component is the result of the function applied to the 
  elements of the other components."
  [key arity factory bounds]
  (let [ar (inc arity)
        r  (Relation/nary (name key) ar)
        ts (.allOf factory ar)
        _ (.bound bounds r ts)]
    [(symbol (name key)) r]))

(defn make-prop
  "A Kodkod relation for a proposition with
  corresponding bounds.     
  A proposition is represented in Kodkod as unary relation
  with atmost 1 element. If the relation is empty, the value
  of the proposition is false, true if there is an element in the relation."
  [key factory bounds]
  (let [r  (Relation/unary (name key))
        ts (.setOf factory (to-array [(first (.universe bounds))]))
        _ (.bound bounds r ts)]
    [(symbol (name key)) r]))

;; ##Operators in kic

(defn not
  "(not fml), the negation of fml."
  [^Formula fml]
  (.not fml))

(defn and
  "(and & fmls), the conjunction of the given fmls "
  [& fmls]
  (Formula/and ^"[Lkodkod.ast.Formula;" (into-array Formula fmls)))

(defn or
  "(or & fmls), the disjunction of the given fmls"
  [& fmls]
  (Formula/or ^"[Lkodkod.ast.Formula;" (into-array Formula fmls)))

(defn impl
  "(impl  fml1 fml2), the implication fml1 -> fml2."
  [^Formula fml1 ^Formula fml2]
  (.implies fml1 fml2))

(defn equiv
  "(equiv  fml1 fml2), the equivalence of the given fmls"
  [^Formula fml1 ^Formula fml2]
  (.iff fml1 fml2))

(defn xor
  "(xor  fml1 fml2), exclusive or of the given fmls"
  [^Formula fml1 ^Formula fml2]
  (.not (.iff fml1 fml2)))

(defn ite
  "(ite fml fml1 fml2), if then else"
  [^Formula fml ^Formula fml1 ^Formula fml2]
  (.and (.implies fml fml1) (.implies (.not fml) fml2)))

(def TRUE
  "TRUE, truth, i.e. the constant fml TRUE."
  Formula/TRUE)

(def FALSE
  "FALSE, contradiction, i.e. the constant fml FALSE."
  Formula/FALSE)

;; ##Predicates for expressions

(defn eq 
  "(eq expr1 expr2), expressing whether expr1 equals expr2."
  [^Expression expr1  ^Expression expr2]
  (.eq expr1 expr2))

(defn in
  "(in expr1 expr2), expressing whether expr1 is a subset of expr2."
  [^Expression expr1  ^Expression expr2]
  (.in expr1 expr2))

(defn one
  "(one expr), expressing whether expr has exactly one tuple."
  [^Expression expr]
  (.one expr))

;; ##Operators  for expressions

(defn join
  "(join expr1 expr2), the dotjoin of the arguments."
  [^Expression expr1 ^Expression expr2]
  (.join expr1 expr2))

(defn product 
  "(product expr1 ...), the product of the arguments."
  [& exprs]
  (Expression/product ^"[Lkodkod.ast.Expression;" (into-array Expression exprs)))

;; ##Declarations

(defn variable
  "(variable var-name), constructs a variable of arity 1."
  [var-name]
   (Variable/unary (name var-name)))

(defn decls
  "(decls decl & more-decls), the combined decls from the arguments."
  [decl & more-decls]
  (reduce #(.and ^Decls %1 ^Decls %2) decl more-decls))

(defn decl
  "(decl variable)      -> (decl variable UNIV :one)."
  [^Variable variable]
   (.oneOf variable Expression/UNIV))

;; ##Quantors

(defn forall
  "`fml` holds for all variables in `var-vec` with values from the universe,
  e.g. `(forall [x y] (pred x y))`."
  [var-vec fml]
  (let [d (apply decls (map decl var-vec))]
    (.forAll fml d)))

(defn exists
  "There exists elements in the universe such that `fml` holds for them,
  e.g. `(exists [x y] (pred x y))`."
  [var-vec fml]
  (let [d (apply decls (map decl var-vec))]
    (.forSome fml d)))

;; ##Running Kodkod

(defn solve
  "(solve formula bounds) -> a kodkod.engine.Solution"
  [formula bounds]
  (let [solver (Solver.)]
     (.solve solver formula bounds)))

(defn solve-all
  [formula bounds]
  (let [solver (Solver.)]
    (doto (.options solver)
      (.setSymmetryBreaking 0))
    (iterator-seq (.solveAll solver formula bounds))))

; Translating a Kodkod solution into Clojure data structures

(defn vector-from-tpl
  "vector <- kodkod.instance.Tuple"
  [^Tuple tpl]
  (vec (for [i (range 0 (.arity tpl))] (let [atom (.atom tpl i)] (if (symbol? atom) (keyword atom) atom)))))

; kodkod.instance.TupleSet -> set of vectors
(defn vecset-from-ts
  "set of vectors <- kodkod.instance.TupleSet"
  [^TupleSet ts]
  (set (map vector-from-tpl ts)))

; kodkod's Map<Relation, TupleSet> -> map of (relation name, set of vectors)
(defn relmap-from-instmap
  "map of (relation symbol, set of tuples) from kodkod's instance"
  [instmap]
  (let [keys (for [^Relation r (keys instmap)] (symbol (.name r))),
        vals (for [^TupleSet t (vals instmap)] (vecset-from-ts t))]
    ; a witness for a variable in an existential quantified formula is marked with a beginning $ in Kodkod,
    ; it's not really a part of the result.
    (into {} (filter #(not= \$ (first (name (key %)))) (zipmap keys vals)))))

; Universe from the tuples of the solution
(defn univ
  "Set of items from a relmap"
  [relmap]
  (->> relmap
      (map second)
      (apply set/union)
      (reduce concat)
      (set)
      (hash-map :univ)))

; kodkod.engine.Solution -> map of universe and relations 
(defn model
  "Model satisfying the kic spec"
  [^Solution solution]
  (if (.sat solution)
    (let [relmap (relmap-from-instmap (.relationTuples (.instance solution)))]
      (into relmap (univ relmap)))))