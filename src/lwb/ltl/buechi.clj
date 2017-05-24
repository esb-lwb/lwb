; lwb Logic WorkBench -- Linear Temporal Logic: Büchi automaton

; Copyright (c) 2016 - 2017 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.ltl.buechi
  (:require [lwb.ltl :refer [atom? literal? ite equiv xor arity wff?]]
            [clojure.spec.alpha :as s])
  (:import (gov.nasa.ltl.trans Formula LTL2Buchi)
           (gov.nasa.ltl.graph Graph Node Edge Guard Literal)))

;; # Generating the Büchi automata for a LTL formula

;; We use the library `ltl2buchi`, the LTL2Buchi translator, 
;; implemented at NASA Ames research Center, by Dimitra Giannakopoulou and Flavio Lerda.

;; Caveat: strange behaviour of Clojure!     
;; this package can only be used if not only lwb.ltl.buechi is required    
;; but also lwb.ltl in which the macros `impl` etc are defined

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
  "Makes n formula `phi` the nary `and` and `or` binary."
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

;; ### A node of the automaton

(s/def ::id int?)
(s/def ::init boolean?)
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

;; ### A guard, i.e. a label of the transitions in the Büchi automaton

(defn- literal
  "A literal from a guard of a Büchi automaton"
  [^Literal l]
  (let [atom (symbol (.getAtom l))]
    (if (.isNegated l) (list 'not atom) atom)))

(s/fdef literal
        :args (s/cat :literal #(instance? Literal %))
        :ret :lwb.ltl/literal)

(s/def ::guard (s/coll-of :lwb.ltl/literal :kind set?))

(defn- guard
  "A guard from a Büchi automata as a Clojure set of literals."
  [^Guard g]
  (if (.isTrue g) #{}    ;the empty set is equiv to true i.e. no restriction for the transition
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
  {:from  (.getId (.getSource e))
   :to    (.getId (.getNext e))
   :guard (guard (.getGuard e))})

(s/fdef edge
        :args (s/cat :edge #(instance? Edge %))
        :ret ::edge)

(s/def ::nodes (s/coll-of ::node :kind vector?))
(s/def ::edges (s/coll-of ::edge :kind vector?))
(s/def ::ba (s/keys :req-un [::nodes ::edges]))

;; ### Transformation into a Clojure data structure

;; Using the helper functions for the transformation of a Graph of LTL2Buchi
;; representing a Büchi automaton into a Clojure data structure

(defn Graph->ba
  "Clojure datastructure from a Büchi automaton given as a Graph of LTL2Buchi."
  [^Graph g]
  (let [nodes (mapv node (.getNodes g))
        edges (mapcat #(.getOutgoingEdges %) (.getNodes g))
        edges (mapv edge edges)]
    {:nodes nodes, :edges edges}))

(s/fdef Graph->ba
        :args (s/cat :ba #(instance? Graph %))
        :ret ::ba)

(defn ba
  "Büchi automaton from LTL formula `phi`,      
   with a little help from LTL2Buchi."
  [phi]
  (-> phi
      translate
      Graph->ba))

(s/fdef ba
        :args (s/cat :phi wff?)
        :ret ::ba)

;; ## Functions that analyze Büchi automaton 

(defn id->node
  "Node of the automaton `ba` with the given `id`."
  [ba id]
  (first (filter #(= id (:id %)) (:nodes ba))))

(defn accepting?
  "Is the node in `ba` with the `id` accepting?     
   Returns: `nil` if not or no node with such `id`."
  [ba id]
  (:accepting (id->node ba id)))

(defn init-id
  "Id of the initial node of the automaton `ba`."
  [ba]
  (:id (first (filter :init (:nodes ba)))))

(defn- successors
  "Successors of node with `id` in automaton `ba`."
  [ba id]
  (mapv :to (filter #(= id (:from %)) (:edges ba))))

(defn- cycle-check
  "Returns `0` if `pathv` has no cycle,     
           `1` if `pathv` has a cycle with an aceepting state,       
           `-1` if `pathv` has a cycle but without an accepting state,      
   Requires: The new element that may lead to a cycle is at the last position of the vector."
  [ba pathv]
  (let [last-idx (dec (count pathv))
        last-elt (last pathv)
        pos-last (.indexOf pathv last-elt)]
    (if (= last-idx pos-last)
      0
      (let [cycle (subvec pathv pos-last)]
        (if (some #(accepting? ba %) cycle)
          1
          -1)))))

(defn- flatten-seqs
  "Like flatten but only affects seqs."
  [coll]
  (mapcat
    (fn [x]
      (if (seq? x)
        (flatten-seqs x)
        (list x)))
    coll))

(defn- paths'
  "Inner function of `paths`."
  [ba pathv]
  (case (cycle-check ba pathv)
    -1 nil
    1 pathv
    0 (let [succs (successors ba (last pathv))]
        (drop-while nil? (map #(paths' ba (conj pathv %)) succs)))))

(defn paths
  "Paths in `ba` starting from init node."
  [ba]
  (flatten-seqs (paths' ba [(init-id ba)])))

(defn ids->edge
  "Edge of the automaton `ba` from `from` to `to`."
  [ba from to]
  (first (filter #(and (= from (:from %)) (= to (:to %))) (:edges ba))))

(defn loop?
  "Does `id` in `ba` have a loop?"
  [ba id]
  (seq (ids->edge ba id id)))

;; # Constructing a Büchi automaton from a given Kripke structure

;; see Edmund M. Clarke, Orna Grumberg and Doron A. Peled: Model Checking, 
;; 9.2 Model Checking Using Automata on p. 123

(defn- baguard
  "Returns a guard for the transition to state `state` int the Büchi automaton
   constructed from Kripke structure `ks`.      
   Requires: `state` is a state of the Kripke structure"
  [ks state]
  (let [atoms (:atoms ks)
        node-state (state (:nodes ks))]
    (set (map #(if (contains? node-state %) % (list 'not %)) atoms))))

(defn ks->ba
  "Given the Kripke structure `ks` we construct a corresponding Büchi automaton `ba`."
  [ks]
  (let [node-vec (into [:init-3961] (keys (:nodes ks)))
        node-map (clojure.set/map-invert (zipmap (range) node-vec))
        nodes (vec (map-indexed (fn [idx _] (if (zero? idx) (hash-map :id idx :accepting true :init true)
                                                       (hash-map :id idx :accepting true))) node-vec))
        edges' (conj (:edges ks) [:init-3961 (:initial ks)])
        edges (mapv #(hash-map :from ((first %) node-map)
                              :to ((second %) node-map)
                              :guard (baguard ks (second %))) edges')]
    (hash-map :nodes nodes :edges edges)))

(s/fdef ks->ba
        :args (s/cat :ks :lwb.ltl.kripke/model)
        :ret ::ba)


