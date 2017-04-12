; lwb Logic WorkBench -- Satisfiability of formulas of predicate logic in finite universes

; Copyright (c) 2017 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.pred.sat
  (:require [lwb.pred :refer :all]
            [lwb.pred.kic :as kic]))

(defn sig->relations
  "Map of symbols for predicates, propositions, functions and consts and 
  their corresponding Kodkod relations from the given signature.    
  During the function the bounds for the Kodkod relation are set as well."
  [sig factory bounds]
  (let [mapfn (fn [[key [type arity]]]
                (cond
                  (= type :const) (kic/make-const key factory bounds)
                  (and (= type :func) (= arity 0)) (kic/make-const key factory bounds)
                  (and (= type :func) (> arity 0)) (kic/make-func key arity factory bounds)
                  (= type :pred) (kic/make-pred key arity factory bounds)
                  (= type :prop) (kic/make-prop key factory bounds) ))]
    (into {} (map mapfn sig))))

(defn func->constraint
  "Generates a Kodkod formula expressing that the relation with the given `key`
  is actually a function."
  [key arity rels]
  (let [vars (vec (map #(kic/variable (str "v" %)) (range 1 (inc arity))))
        func ((symbol (name key)) rels)
        part (loop [v vars result func]
          (cond (empty? v) (kic/one result)
            :else (recur (rest v) (kic/join (first v) result))))]
    (kic/forall vars part) ))

(defn sig->constraints
  "Vector of Kodkod formulas comprising the constraints for functions
   in the given signature `sig` and the map of relations `rels`."
  [sig rels]
  (let [mapfn (fn [[key [type arity]]]
                (if (and (= type :func) (> arity 0)) (func->constraint key arity rels)))]
    (vec (keep identity (map mapfn sig)))))

(defn prop->fml
  "Generates a Kodkod formula expressing that the relation with the given `key`
  is actually a true proposition, i.e. has exactly one tuple."
  [key rels]
  (let [r ((symbol (name key)) rels)]
    (kic/one r) ))

(defn vars->varmap
  "Map of symbols to Kodkod variables."
  [vars]
  (zipmap vars (map kic/variable vars)))

(defn term->expression
  "Kodkod expression representing the given expression of a phi."
  [term sig env rels]
  (if (or (symbol? term) (keyword? term) (int? term))
    (cond
      (logvar? term sig) (term env)
      (func-0? term sig) (term rels)
      (const? term) ((symbol (name term)) rels))
    ;; funcs
    (cond
      (func? (first term) sig)
      (let [func ((first term) rels)
            args (vec (map #(term->expression % sig env rels) (rest term)))]
        (loop [v args result func]
          (cond (empty? v) result
                :else (recur (rest v) (kic/join (first v) result))))))))

(def ^:private kic-ops
  {'not   kic/not
   'and   kic/and
   'or    kic/or
   'impl  kic/impl
   'equiv kic/equiv
   'xor   kic/xor
   'ite   kic/ite})

(defn phi->formula
  "Kodkod formula representing the given formula `phi` of predicate
  logic.    
  All variables of `phi` must be bound.    
  `sig` is the signature for `phi`.      
  `env` is the environment i.e. map of variables for `phi`.     
  `rels` is the map of the Kodkod relations from the signature of `phi`."
  [phi sig env rels]
  (if (or (symbol? phi) (keyword? phi) (int? phi)) ;; leaf
    (cond
      (prop? phi sig) (kic/one (phi rels)) )
      ;; list
    (cond
      ;; operators
      (op? (first phi)) (let [args (vec (map #(phi->formula % sig env rels) (rest phi)))] (apply ((first phi) kic-ops) args))
      ;; quantors
      (quantor? (first phi))
      (let [kic-quant (if (= 'forall (first phi)) kic/forall kic/exists)
            varmap (vars->varmap (second phi))
            env' (merge env varmap)]
            (kic-quant (vec (vals varmap)) (phi->formula (nth phi 2) sig env' rels)))
      ;; equality
      (= (first phi) '=) (kic/eq (term->expression (second phi) sig env rels) (term->expression (nth phi 2) sig env rels))
      ;; predicates
      :else (let [pred ((first phi) rels)
                  terms (vec (map #(term->expression % sig env rels) (rest phi)))]
              (kic/in (apply kic/product terms) pred)) )))

(defn sat-intern
  "Implementation of sat."
  [phi sig set mode]
  (let [universe (kic/universe set)
        factory (kic/factory universe)
        bounds (kic/bounds universe)
        rels (sig->relations sig factory bounds)
        cons (sig->constraints sig rels)
        phi'  (phi->formula phi sig {} rels)
        fmls (conj cons phi')
        fml (apply kic/and fmls)]
    (if (= mode :one)
      (let [sol (kic/solve fml bounds)]
        (kic/model sol))
      (let [sols (kic/solve-all fml bounds)]
        (into #{} (keep identity (map kic/model sols)))))))

(defn sig->consts
  "Set of consts in signature.     
   `[:const 0]` and `[:func 0]' have to be consts in kodkod."
  [sig]
  (set (map key (filter #(#{[:const 0] [:func 0]} (val %)) sig))))

(defn fill-up
  "Fills the given set with keywords `:e1, :e2...` until `size`is reached."
  [set size]
  (loop [s set, i 1]
    (if (= (count s) size) s
                           (recur (conj s (keyword (str "e" i))) (inc i)))))

(defmulti sat
  "Satisfiability of a formula of the predicate logic in a finite universe.    
  The one variant of the functions just gives the size of the universe, the
  other an explicit vector with the elements of the universe."
  (fn [_ _ utype & _]
    (if (integer? utype) :int
                         (if (set? utype) :set nil))))

(defmethod sat :int
  ([phi sig usize]
    (sat phi sig usize :one))
  ([phi sig usize mode]
    (let [consts (sig->consts sig)
          ssize (count consts)]
      (if (< usize ssize)
        (throw (Exception. "usize must be >= number of constants and unary functions in the signature.")))
        (sat-intern phi sig (fill-up consts usize) mode))))

(defmethod sat :set
  ([phi sig set]
    (sat phi sig set :one))
  ([phi sig set mode]
    (sat-intern phi sig set mode)))

(defn sat?
  "Is `phi` satisfiable?"
  [phi sig usize]
  (if (nil? (sat phi sig usize)) false true))

(defn valid?
  "Is `phi` valid?"
  [phi sig usize]
  (not (sat? (list 'not phi) sig usize)))
