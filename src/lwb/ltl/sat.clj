; lwb Logic WorkBench -- Linear Temporal Logic: Satisfiability

; Copyright (c) 2016 - 2017 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.ltl.sat
  (:require [lwb.ltl :refer :all]
            [lwb.ltl.buechi :as ba]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]))

;; # Satisfiability in the linear temporal logic

;; The basic steps are:

;; 1. Generate a Büchi automaton for the formula of LTL that has the property
;;    that the infinite words accepted by the automaton i.e. the language of the
;;    automaton are exactly the set of computations satisfying the formula.
;; 2. If the Büchi automaton is not empty, the formula is satisfiable, and we
;;    construct a model, i.e. a Kripke structure for the formula.

;; #### Helper functions

(defn node-label
  "Label for a node with `id` with edge from `from` in the Kripke structure for the automaton `ba`."
  [ba from id]
  (let [edge (ba/ids->edge ba from id)
        guard (:guard edge)]
    (if (set? guard)
      (set/select symbol? guard)
      #{})))

(defn- node-key
  "Keyword for a node with `id`"
  [id]
  (keyword (str "s_" id)))

;; #### Transformation of Büchi automaton into a corresponding Kripke structure

;; The idea:        

;; 1. We determine a path from the init node of the automaton to a cycle that contains
;;    an acccpeting state
;; 2. The we use this path to construct a Kripke structure.

(defn ba->ks
  "A Kripke structure is generated from a Büchi automaton as a model       
   for the formula the automaton accepts."
  [ba]
  (let [literals (reduce set/union (filter set? (map :guard (:edges ba))))
        atoms (set (map #(if (list? %) (second %) %) literals)) 
        pathv (first (ba/paths ba))
        nodes (mapv #(hash-map (node-key %2) (node-label ba %1 %2)) pathv (rest pathv))
        nodes' (apply merge-with set/union nodes)
        initial (node-key (second pathv))
        pathv' (if (ba/accepting? ba (second pathv)) (conj pathv (second pathv)) pathv)
        edges (set (map #(vector (node-key %1) (node-key %2)) (rest pathv') (rest (rest pathv'))))                  
        acc-nodes (distinct (filter #(ba/accepting? ba %) pathv))
        acc-loops  (set (map #(vector (node-key %) (node-key %)) (filter #(ba/loop? ba %) acc-nodes)))
        edges' (set/union edges acc-loops)]
    (hash-map :atoms atoms :nodes nodes' :initial initial :edges edges')))
  
;; ## Satisfiability and validity for LTL formulas

(defn sat
  "Gives a model for `phi` if the formula is satisfiable, nil otherwise."
  [phi]
  (let [ba (ba/ba phi)]
    (when (seq (:nodes ba)) (ba->ks ba))))

(s/fdef sat
        :args (s/cat :phi wff?)
        :ret (s/or :as/model nil?))

(defn sat?
  "Is `phi` satisfiable?"
  [phi]
  (if (nil? (sat phi)) false true))

(s/fdef sat?
        :args (s/cat :phi wff?)
        :ret boolean?)

(defn valid?
  "Is `phi` valid?"
  [phi]
  (not (sat? (list 'not phi))))

(s/fdef valid?
        :args (s/cat :phi wff?)
        :ret boolean?)
