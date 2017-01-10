; lwb Logic WorkBench -- Linear Temporal Logic: Büchi automaton

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.ltl.buechi
  (:require [lwb.ltl :refer :all]
            [clojure.spec :as s])
  (:import (gov.nasa.ltl.trans Formula LTL2Buchi)
           (gov.nasa.ltl.graph Graph Node Edge Guard Literal)))

;; # Generating the Büchi automata for a LTL formula

;; We use the library `ltl2buchi`, the LTL2Buchi translator, 
;; implemented at NASA Ames research Center, by Dimitra Giannakopoulou and Flavio Lerda.

;; Caveat: strange behaviour of Clojure!     
;; this can only be used if not only lwb.ltl.buechi is required    
;; but also lwb.ltl

(defn- norm-ltl
  "Normalize formula `phi` such that the operator `ite` , `equiv`, and `xor` are not used."
  [phi]
  (if (literal? phi)
    phi
    (let [op (first phi)]
      (if (contains? #{'ite 'equiv 'xor} op)
        (let [exp-phi (macroexpand-1 phi)]
          (apply list (first exp-phi) (map norm-ltl (rest exp-phi))))
        (apply list op (map norm-ltl (rest phi)))))))

;; For `ltl2buchi` the operators `and` and `or` have to be binary.

(defn- nary->binary
  "Makes nary `and` and `or` binary."
  [phi]
  (let [phi (norm-ltl phi)]
    (cond (or (atom? phi) (boolean? phi)) phi
          :else (let [[op & args] phi]
                  (cond (= 1 (arity op)) (list op (nary->binary (first args)))
                        ; and or
                        (contains? '#{and or} op) (cond (empty? args) (if (= op 'and) true false)
                                                        (= 1 (count args)) (first args)
                                                        (= 2 (count args)) (list op (nary->binary (first args))
                                                                                 (nary->binary (second args)))
                                                        :else (nary->binary (reduce #(list op %1 %2) args)))
                        ; implies until release
                        :else (list op (nary->binary (first args)) (nary->binary (second args))))))))

(defn fml
  "Makes a Formula<String> object from a Clojure LTL formula."
  [phi]
  (let [phi (nary->binary phi)]
  (cond (atom? phi) (Formula/Proposition (name phi))
        (true? phi) (Formula/True)
        (false? phi) (Formula/False)
        :else (let [[op & args] phi]
                (cond (= op 'not) (Formula/Not (fml (first args)))
                      (= op 'and) (Formula/And (fml (first args)) (fml (second args)))
                      (= op 'or) (Formula/Or (fml (first args)) (fml (second args)))
                      (= op 'impl) (Formula/Implies (fml (first args)) (fml (second args)))
                      (= op 'until) (Formula/Until (fml (first args)) (fml (second args)))
                      (= op 'release) (Formula/Release (fml (first args)) (fml (second args)))
                      (= op 'atnext) (Formula/Next (fml (first args)))
                      (= op 'always) (Formula/Always (fml (first args)))
                      (= op 'finally) (Formula/Eventually (fml (first args))))))))

(defn translate
  "Translates the formula `phi` into a Büchi automaton.      
   Here we use the algorithm developed by Dimitra Giannakopoulou and Flavio Lerda."
  [phi]
  (let [fml (fml phi)]
    (LTL2Buchi/translate ^Formula fml)))

;; ## Clojure datastructure for Büchi automata

;; #### A node of the automaton

(s/def ::id        int?)
(s/def ::init      boolean?)
(s/def ::accepting boolean?)

(s/def ::node (s/keys :req-un [::id]
                         :opt-un [::init ::accepting]))

(defn- node
  "A node from a Büchi automaton as a Clojure map `::ba-node`."
  [^Node n]
  (let [g (.getGraph n)
        init? (= n (.getInit g))
        accepting? (.getBooleanAttribute n "accepting")]
    (into {:id (.getId n)} [(if init? {:init true}) (if accepting? {:accepting true})])))

(s/fdef node
        :args (s/cat :node #(instance? Node %))
        :ret ::node)

;; #### A guard, i.e. a label of the transitions in the Büchi automaton

(defn- literal 
  "A literal from a guard of a Büchi automaton"
  [^Literal l]
  (let [atom (symbol (.getAtom l))]
    (if (.isNegated l) (list 'not atom) atom)))

(s/fdef literal
        :args (s/cat :literal #(instance? Literal %))
        :ret :lwb.ltl/literal)

(s/def ::guard (s/or :true true? 
                     :literals (s/coll-of :lwb.ltl/literal :kind set?)))

(defn- guard
  "A guard from a Büchi automata as a Clojure set of literals."
  [^Guard g]
  (if (.isTrue g) true
                  (set (map literal g))))

(s/fdef guard
        :args (s/cat :guard #(instance? Guard %))
        :ret ::guard)

;; #### An edge i.e. transition of the automaton

(s/def ::from int?)
(s/def ::to int?)
(s/def ::edge (s/keys :req-un [::from ::to ::guard]))

(defn- edge
  [^Edge e]
  {:from (.getId (.getSource e))
   :to   (.getId (.getNext e))
   :guard (guard (.getGuard e))})

(s/fdef edge
        :args (s/cat :edge #(instance? Edge %))
        :ret ::edge)

(s/def ::nodes (s/coll-of ::node :kind vector?))
(s/def ::edges (s/coll-of ::edge :kind vector?))
(s/def ::ba (s/keys :req-un [::nodes ::edges]))

;; #### Transformation into a Clojure data structure

;; Using the helper functions for the transformation of a Grahph of LTL2Buchi
;; represneting a Büchi automaton into a Clojure data structure

(defn- ba'
  "Clojure datastructure from a Büchi automaton given as a Graph of LTL2Buchi."
  [^Graph g]
  (let [nodes (mapv node (.getNodes g))
        edges (mapcat #(.getOutgoingEdges %) (.getNodes g))
        edges (mapv edge edges)]
    {:nodes nodes, :edges edges}))

(s/fdef ba'
        :args (s/cat :ba #(instance? Graph %))
        :ret ::ba)

(defn ba
  "Büchi automaton from LTL formula `phi`,      
   with a little help from LTL2Buchi."
  [phi]
  (-> phi
      translate
      ba'))

(s/fdef ba
        :args (s/cat :phi wff?)
        :ret ::ba)

;; ### Functions that analyzes Büchi automaton 

(defn id->node
  "Node of the automaton `ba` with the given `id`."
  [ba id]
  (first (filter #(= id (:id %)) (:nodes ba)) ))

(defn accepting?
  "Is the node in `ba` with the `id` accepting?     
   Returns: `nil` if not or no node with such `id`."
  [ba id]
  (:accepting (id->node ba id)))

(defn init-id
  "Id of the initial node of the automaton `ba`."
  [ba]
  (:id (first (filter :init (:nodes ba)))))

(defn succs
  "Successors of node with `id` in automaton `ba`.     
   Loops are taken into account at accepting nodes."
  [ba id]
  (let [to (map :to (filter #(= id (:from %)) (:edges ba)))]
    (if (accepting? ba id)
      (if (contains? (set to) id)
        [id]
      (vec to))
      (vec (filter #(not= id %) to)))))

; TODO: check whether there is an accepting node in the cycle
(defn cycle?
  "Has the given vector of node id's a cyle?"
  [pathv]
  (not (apply distinct? pathv)))

(defn ids->edge
  "Edge of the automaton `ba` from `from` to `to`."
  [ba from to]
  (first (filter #(and (= from (:from %)) (= to (:to %))) (:edges ba))))

(defn loop?
  "Does `id` in `ba` have a loop?"
  [ba id]
  (not (empty? (ids->edge ba id id))))

(defn path 
  "Path in `ba` starting from init node or given `id` resp."
  ([ba]
   (path ba [(init-id ba)]))
  ([ba pathv]
    (if (cycle? pathv)
      pathv
      (let [successors (succs ba (last pathv))]
        (first (map #(path ba (conj pathv %)) successors))))))
    
