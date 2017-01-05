; lwb Logic WorkBench -- Linear Temporal Logic: Satisfiability

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.ltl.sat
  (:require [lwb.ltl :refer :all]
            [lwb.ltl.kripke :as ks]
            [lwb.ltl.buechi :as ba]
            [clojure.spec :as s]
            [clojure.set :as set]))

;; # Satisfiability in the linear temporal logic

;; The basic steps are:

;; 1. Generate a Büchi automaton for the formula of LTL that has the property
;;    that the infinite words accepted by the automaton i.e. the language of the
;;    automaton are exactly the set of computations satisfying the formula.
;; 2. If the Büchi automaton is not empty, the formula is satisfiable, and we
;;    construct a model, i.e. a Kripke structure for the formula.

(defn- node-label
  "Label for a node with `id` in the Kripke structure for the automaton `ba`."
  [ba id]
  (let [incoming (filter #(and (set? (:guard %)) (= id (:to %))) (:edges ba))]
    (set/select symbol? (apply set/union (map :guard incoming)))))

(defn- node-key
  "Keyword for a node with `id`"
  [id]
  (keyword (str "s_" id)))

;; is there always just one successor to the init node in the Büchi automaton?
;; depends on the reduction that LTL2Bucxhi performs

(defn- succ-init
  "Successor of init node in the automaton."
  [ba]
  (let [init-node (first (filter :init (:nodes ba)))
        init-id (:id init-node)]
    (if (:accepting init-node)
      init-id
      (let [succ-ids (distinct (mapv :to (filter #(= (:from %) init-id) (:edges ba))))]
        (first succ-ids)))))

(defn ba->ks 
  "A Kripke structure is generated from a Büchi automaton as a model       
   for the formula the automaton accepts."
  [ba]
  (let [nodes (mapv :id (filter #(or (not (:init %)) (:accepting %)) (:nodes ba)))
        nodes' (mapv #(hash-map (node-key %) (node-label ba %)) nodes)
        nodes'' (apply merge nodes')
        initial (succ-init ba)
        edges (map #(vector (:from %) (:to %)) (:edges ba))
        node-id-set (set nodes)
        edges' (distinct (filter #(and (contains? node-id-set (first %)) (contains? node-id-set (second %))) edges))
        edges'' (set (map #(vector (node-key (first %)) (node-key (second %))) edges'))
        ]
    (hash-map :nodes nodes'' :initial initial :edges edges'')))

(defn sat
  "Gives a model for `phi` if the formula is satisfiable, nil if not."
  [phi]
  (let [ba (ba/ba phi)]
    (if (empty? (:nodes ba)) 
      nil
      (ba->ks ba))))

(sat '(and P (not P)))
(sat '(or P (not P)))
(sat '(always P))
(sat '(always (and P Q)))
(sat '(always (or P Q)))
(sat '(always (impl P Q)))

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

(sat? '(and P (not P)))
(sat? '(or P (not P)))
(sat? '(always P))

(defn valid?
  "Is `phi` valid?"
  [phi]
  (not (sat? (list 'not phi))))

(s/fdef valid?
        :args (s/cat :phi wff?)
        :ret boolean?)

(valid? '(and P (not P)))
(valid? '(or P (not P)))
(valid? '(always (or P (not P))))
(valid? '(always P))
