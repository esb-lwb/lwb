; lwb Logic WorkBench -- Predicate logic

; Copyright (c) 2015 -2021 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.resolution
  (:require [clojure.math.combinatorics :as combo])
  (:require [lwb.prop.nf :refer [cnf]])
  (:require [lwb.prop.sat :refer [cnf->dimacs]])
  (:require [clojure.set :as set]))

; We need to remember which pairs of clauses have already been resolved
(def resolved (atom (transient {})))

(defn- reset-resolved
  []
  (reset! resolved (transient{})))

(defn- is-resolved?
  "Is the pair [cl1 cl2] already resolved?"
  [[cl1 cl2]]
  (get @resolved #{cl1 cl2}))

(defn- add-resolved 
  "Add the pair [cl1 cl2] and its resolvent to resolved."
  [[cl1 cl2] res]
  (swap! resolved assoc! #{cl1 cl2} res))

(defn tauto?
  "Is the clause a tautology?"
  [cl]
  (if (seq (filter #(contains? cl (- %)) cl))
    true
    false))

(defn- resolve-pair
  "Returns the resolvent of two clauses,
   checks resolved if that's necessary;
   throws an exception if the resolvent is the contradiction #{}."
  [cl1 cl2]
  (if (is-resolved? [cl1 cl2])
    nil
    (if-let [literal (first (filter #(contains? cl2 (- %)) cl1))]
      (let [res (set/union (disj cl1 literal) (disj cl2 (- literal)))]
        (cond
          (= res #{}) (throw (ex-info "Contradiction found" {:from :lwb}))
          (tauto? res) nil
          :else (do
                  (add-resolved [cl1 cl2] res)
                  res))))))
  
(defn- resolution
  "Return true if the given set of clauses is satisfiable."
  [cl-set]
  (try
    (if-let [pairs (seq (filter #(not= (first %) (second %)) (combo/combinations cl-set 2)))]
      (let [resolvents (doall (remove nil? (map #(apply resolve-pair %) pairs)))
            ext-cl-set (set/union cl-set resolvents)]
        (if (= cl-set ext-cl-set)
          true
          (resolution ext-cl-set))
        )
      true)
    (catch Exception e
      (if (= (ex-data e) {:from :lwb})
        false
        (throw e)))))

(defn sat?
  "returns if the given propositional formula is satisfiable."
  [phi]
  (reset-resolved)
  (if (true? (cnf phi))
    true                                                    ; trivially true
    (let [phi-d (cnf->dimacs phi)]
      (resolution (:cl-set phi-d)))))

(comment
  (sat? '(and (or P)))
  (sat? '(and (or P (not P))))
  (sat? '(and (or P) (or (not P))))
  (sat? '(impl (impl F (impl G H)) (impl (impl F G) (impl F H))))
  (sat? '(and (or A B (not C)) (or (not A)) (or A B C) (or A (not B))))
  (sat? '(and (or A1 (not A2) A3) (or A2 (not A3) A4)))
  )