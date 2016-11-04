; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.repl
  (:require [potemkin :as pot]
            [lwb.consts :refer [welcome]]
            [lwb.nd.proof :as proof]
            [lwb.nd.deduction :as deduc]
            [lwb.nd.prereqs :as prereqs]
            [lwb.nd.storage :refer [roths reset-roths]]
            [lwb.nd.io :as io]
            [lwb.nd.printer :refer [pprint]]))

; a little hack
(pot/import-vars
  [lwb.pred.substitution substitution]
  [lwb.nd.prereqs substitution?])

;; # Functions for interactive use of natural deduction in the REPL

;; To use lwb for natural deduction, one proceeds as follows:

;; - Load a logic with `(load-logic logic)`
;; - Create a new proof with `(proof premises conclusion)` 
;; - Prove the conclusion form the promises by
;;      - forward steps '(step-f ...)`
;;      - backward steps `(step-b ...)` 
;;      - unifying symbols `(unify ...)`

(defn load-logic
  "Load rules and theorems of the logic to use.      
   Logic can be `:prop`, `:pred`, `:ltl`."
  [logic]
  (reset-roths)
  (case logic
    :prop (do
            (io/import-rules "resources/nd/rules-prop.edn")
            (io/import-theorems "resources/nd/theorems-prop.edn"))
    :pred (do
            (io/import-rules "resources/nd/rules-prop.edn")
            (io/import-rules "resources/nd/rules-pred.edn")
            (io/import-theorems "resources/nd/theorems-prop.edn")
            (io/import-theorems "resources/nd/theorems-pred.edn"))
    :ltl (do
           (io/import-rules "resources/nd/rules-ltl.edn")
           (io/import-theorems "resources/nd/theorems-ltl.edn")))
  (println welcome)
  (println (str "Info: Rules and theorems loaded: " logic)))

;; Choose the logic for the session
(comment
  (load-logic :prop)
  (load-logic :pred)
  (load-logic :ltl)
  )

;; ## Atoms that hold the state of the proof session

(def p
  "Atom that holds the current proof."
  (atom []))

(def p-history
  "Atom that holds the history of the proof as a vector, provided for undo steps."
  (atom []))

;; ## Showing the proof

(defn show
  "Print the actual state of the proof."
  []
  (pprint @p))

;; ## Creating a new proof

(defn proof
  "Starts a new proof."
  ([conclusion] (proof [] conclusion))
  ([premises conclusion]
   (reset! p-history [])
   (reset! p (proof/proof premises conclusion))
   (show)
   (if (empty? @roths)
     (println "Info: There are no rules available, please load a logic with load-logic."))))

;; ## Functions that perform steps in proving a conclusion

(defn step-f
  "Execute a forward step"
  [roth & args]
  (try
    (let [proof' (deduc/step-f @p roth (vec args))]
      (swap! p-history conj @p)
      (reset! p proof')
      (show))
    (catch Exception e
      (println (str "Error: " (.getMessage e))))))

(defn step-b
  "Execute a backward step"
  [roth & args]
  (try
    (let [proof' (deduc/step-b @p roth (vec args))]
      (swap! p-history conj @p)
      (reset! p proof')
      (show))
    (catch Exception e
      (println (str "Error: " (.getMessage e))))))

(defn unify
  "Unifies symbols"
  [old new]
  (try
    (let [proof' (deduc/unify @p old new)]
      (swap! p-history conj @p)
      (reset! p proof')
      (show))
    (catch Exception e
      (println (str "Error: " (.getMessage e))))))

(defn undo
  "Undo the last change (you can't go further than the last state)"
  []
  (if (empty? @p-history)
    (println "Info: You reached the starting point of the proof, there is nothing more to undo")
    (do
      (reset! p (last @p-history))
      (swap! p-history #(vec (drop-last %)))
      (show))))

;; ## Functions that print rules and theorems

(defn show-roth
  "Prints rule or theorem of the current logic."
  [id]
  (if (contains? @roths id)
    (let [roth (id @roths)
          extra (:extra roth)
          extras (if extra (str " + extra input " extra))]
      (println (str id ": \t" (:given roth) extras " -> " (:conclusion roth))))
    (println "No such rule or theorem found for the current logic.")))

(defn show-roths
  "Prints rules and/or theorems of the current logic.       
  `:all` rules and theorems    
  `:rules` just the rules     
  `:theorems` just the theorems"
  ([] (show-roths :all))
  ([mode]
   (let [filter-fn (condp = mode
                     :all (constantly true)
                     :rules #(= :rule (:type (val %)))
                     :theorems #(= :theorem (:type (val %)))
                     )]
     (for [roth (sort (filter filter-fn @roths))]
       (let [id (key roth)]
         (show-roth id))))))

(defn show-rules
  "Print the rules of the current logic."
  []
  (show-roths :rules))

(defn show-theorems
  "Print the theorems of the current logic."
  []
  (show-roths :theorems))

;; ## TeX-Code for typesetting the current proof

(defn texify
  "Generates TeX-Code from the current proof."
  []
  #_(pprint/texify @p)
  "TODO: yet to be implemented.")

;; ## Function that exports the current proof as a reusable theorem

(defn export
  "Saves the current proof with `id` in the file with `filename`.      
   Requires: Proof is completed.    
   If there is already a theorem with that `id`, the replacement can be forced 
   by a mode of `:replace`."
  ([filename id] (export filename id :check))
  ([filename id mode]
   "TODO: yet to be implemented"))

