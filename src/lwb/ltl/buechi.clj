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
           (gov.nasa.ltl.graph Graph Node Edge Guard Literal)
           (gov.nasa.ltl.graphio Writer)))

;; # Generating the Büchi automata for a LTL formula

;; We use the library `ltl2buchi`, the LTL2Buchi translator, 
;; implemented at NASA Ames research Center, by Dimitra Giannakopoulou and Flavio Lerda.

;; and an or must be binary here!!
;; only some of the operators are allowed

;; we use nnf with binary and and or

(defn- nary->binary
  "Makes nary `and` and `or` binary"
  [phi]
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
                      :else (list op (nary->binary (first args)) (nary->binary (second args)))))))

(comment
  (nary->binary 'a)
  (nary->binary true)
  (nary->binary false)
  (nary->binary '(not a))
  (nary->binary '(finally a))
  (nary->binary '(always a))
  (nary->binary '(and))
  (nary->binary '(and a))
  (nary->binary '(and a b))
  (nary->binary '(and a b c))
  (nary->binary '(and a b (and c d e f)))
  (nary->binary '(until false (and a b (and c d e f))))
  (nary->binary '(until (and a b c) (and c d e f)))
  (nary->binary '(until (or a b c) (and c d e f)))
  (nary->binary '(until (or) (and c d e f)))
  )

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

(comment
  (fml 'A)
  (fml '(not A))
  (fml '(and (not A) B))
  (fml '(release false A))
  (fml '(until true A))
  (fml '(until false (and a b (and c d e f))))
  (fml '(always A))
  (fml '(always (finally A)))
  )

; erzeugt Graph
(defn translate
  [phi]
  (let [fml (fml phi)]
    (LTL2Buchi/translate ^Formula fml)
    ))

(comment
  (translate '(release false A))
  (translate '(always A))
  (translate '(always (finally A)))
  )

;; Clojure datastructure for ba

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
        :args #(instance? Node %)
        :ret ::node)

(defn- literal 
  "A literal from a guard of a Büchi automaton"
  [^Literal l]
  (let [atom (symbol (.getAtom l))]
    (if (.isNegated l) (list 'not atom) atom)))

(s/fdef literal
        :args #(instance? Literal %)
        :ret :lwb.ltl/literal)

(s/def ::guard (s/or :true true? 
                     :literals (s/coll-of :lwb.ltl/literal :kind set?)))

(defn- guard
  "A guard from a Büchi automata as a Clojure set of literals."
  [^Guard g]
  (if (.isTrue g) true
                  (into #{} (map literal g))))

(s/fdef guard
        :args #(instance? Guard %)
        :ret ::guard)

(s/def ::from int?)
(s/def ::to int?)
(s/def ::edge (s/keys :req-un [::from ::to ::guard]))

(defn- edge
  [^Edge e]
  {:from (.getId (.getSource e))
   :to   (.getId (.getNext e))
   :guard (guard (.getGuard e))})

(s/fdef edge
        :args #(instance? Edge %)
        :ret ::edge)

(s/def ::nodes (s/coll-of ::node :kind vector?))
(s/def ::edges (s/coll-of ::edge :kind vector?))
(s/def ::ba (s/keys :req-un [::nodes ::edges]))

(defn ba
  "Clojure datastructure from a Büchi automaton given as a Graph of LTL2Buchi."
  [^Graph g]
  (let [nodes (into [] (map node (.getNodes g)))
        edges (mapcat #(.getOutgoingEdges %) (.getNodes g))
        edges (vec (map edge edges))]
    {:nodes nodes, :edges edges}))

(s/fdef ba
        :args #(instance? Graph %)
        :ret ::ba)

(comment
  (def ba1 (ba (translate '(always A))))
  (s/valid? ::ba ba1)

  (ba (translate '(always (finally A))))

  (ba (translate '(and A B)))

  (def ba4 (ba (translate '(and A (not BX) C))))
  (s/valid? ::ba ba4)
  
  (ba (translate '(impl A B)))
  
  (ba (translate '(or A B)))
  
  (ba (translate '(finally A)))
  
  (ba (translate '(until A B)))
  
  (ba (translate '(and (finally A) (always (not A )))))

  (ba (translate '(and A (not A))))
  
  (ba (translate '(and (and A B) (not A) (not B))))
  )

(comment
  (-> '(always AX)
      translate
      ba)

  (-> '(always (not AX))
      translate
      ba)

  (-> '(always (impl A (finally B)))
      translate
      ba)

  (-> '(impl (always (finally A)) (always (finally B)))
      translate
      ba)

  (-> '(always (and A B))
      translate
      ba)

  (-> '(finally (always (not A)))
      translate
      ba)

  (-> '(not (always (impl A (finally B))))
      translate
      ba)

  )