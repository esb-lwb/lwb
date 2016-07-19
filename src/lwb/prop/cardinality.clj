; lwb Logic WorkBench -- Boolean Cardinality Constraints

; Copyright (c) 2014 - 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.


(ns lwb.prop.cardinality
  (:require [lwb.prop :refer :all]
            [clojure.math.combinatorics :refer (combinations)]
            [clojure.spec :as s]))

(s/def ::scoll (s/coll-of atom?))

(defn min-kof
  "(min-kof k scoll) -> a seq of clauses expressing that 
   at least k of the symbols in scoll are true."
  [k scoll]
  {:pre [(<= 1 k (count scoll))]}
  (map #(apply list 'or %) (combinations scoll (inc (- (count scoll) k)))))

(s/fdef min-kof
        :args (s/cat :k int? :scoll ::scoll)
        :ret (s/* :lwb.prop/clause))

(defn max-kof
  "(max-kof k scoll) -> a seq of clauses expressing that 
   at most k of the symbols in scoll are true."
  [k scoll]
  {:pre [(<= 0 k (dec (count scoll)))]}
  (if (= k 0)
    (map #(list 'not %) scoll))
  (for [s (combinations scoll (inc k))]
    (apply list 'or (map #(list 'not %) s))))

(s/fdef max-kof
        :args (s/cat :k int? :scoll ::scoll)
        :ret (s/* :lwb.prop/clause))

(defn kof
  "(kof k scoll) -> a seq of clauses expressing that
   exactly k of the symbols in scoll are true."
  [k scoll]
  (condp = k
    0 (max-kof 0 scoll)
    (count scoll) (min-kof k scoll)
    (concat (min-kof k scoll) (max-kof k scoll))))

(s/fdef kof
        :args (s/cat :k int? :scoll ::scoll)
        :ret (s/* :lwb.prop/clause))
        
(defn oneof
  "(oneof scoll) -> a seq of clauses expressing that
   exactly 1 symbol in scoll is true."
  [scoll]
  (if (empty? scoll)
    false
    (kof 1 scoll)))

(s/fdef oneof
        :args (s/cat :scoll ::scoll)
        :ret (s/* :lwb.prop/clause))
