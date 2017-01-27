; lwb Logic WorkBench -- Boolean Cardinality Constraints

; Copyright (c) 2014 - 2017 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.


(ns lwb.prop.cardinality
  (:require [lwb.prop :refer :all]
            [clojure.math.combinatorics :refer (combinations)]
            [clojure.spec :as s]))

;; # Cardinality constraints in propositional logic

;; The functions are returning sequences of clauses of the form `(or literal1 literal2 ...)`
;; expressing caridnality constraints for the giveen collection of atoms

(s/def ::acoll (s/coll-of atom?))

(defn min-kof
  "(min-kof k acoll) -> a seq of clauses expressing that 
   at least k of the symbols in acoll are true."
  [k acoll]
  {:pre [(<= 1 k (count acoll))]}
  (map #(apply list 'or %) (combinations acoll (inc (- (count acoll) k)))))

(s/fdef min-kof
        :args (s/cat :k int? :acoll ::acoll)
        :ret (s/* :lwb.prop/clause))

(defn max-kof
  "(max-kof k acoll) -> a seq of clauses expressing that 
   at most k of the symbols in acoll are true."
  [k acoll]
  {:pre [(<= 0 k (dec (count acoll)))]}
  (if (zero? k)
    (map #(list 'not %) acoll))
  (for [s (combinations acoll (inc k))]
    (apply list 'or (map #(list 'not %) s))))

(s/fdef max-kof
        :args (s/cat :k int? :acoll ::acoll)
        :ret (s/* :lwb.prop/clause))

(defn kof
  "(kof k acoll) -> a seq of clauses expressing that
   exactly k of the symbols in acoll are true."
  [k acoll]
  (condp = k
    0 (max-kof 0 acoll)
    (count acoll) (min-kof k acoll)
    (concat (min-kof k acoll) (max-kof k acoll))))

(s/fdef kof
        :args (s/cat :k int? :acoll ::acoll)
        :ret (s/* :lwb.prop/clause))
        
(defn oneof
  "(oneof acoll) -> a seq of clauses expressing that
   exactly 1 symbol in acoll is true."
  [acoll]
  (if (empty? acoll)
    nil
    (kof 1 acoll)))

(s/fdef oneof
        :args (s/cat :acoll ::acoll)
        :ret (s/* :lwb.prop/clause))
