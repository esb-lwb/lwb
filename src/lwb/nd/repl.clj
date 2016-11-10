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
            [lwb.nd.rules :refer [roths reset-roths]]
            [lwb.nd.io :as io]
            [lwb.nd.printer :as printer]))

(def rothpath
  "Path to the files for rules and theorems"
  "resources/nd/")

; a little hack: we expose some symbols from other namespaces to users of nd.repl

(pot/import-vars
  [lwb.pred.substitution substitution]
  [lwb.nd.prereqs substitution?])

;; # Functions for interactive use of natural deduction in the REPL

;; To use lwb for natural deduction, one proceeds as follows:

;; - Load a logic with `(load-logic logic)`
;; - Create a new proof with `(proof premises conclusion)` 
;; - Prove the conclusion form the promises by
;;      - forward steps `(step-f ...)`
;;      - backward steps `(step-b ...)` 
;;      - unifying symbols `(unify ...)`

(defn load-logic
  "Load rules and theorems of the logic to use.      
   Logic can be `:prop`, `:pred`, `:ltl`.    
   Requires: files with rules and theorems     
   Modifies: global `roths`"
  [logic]
  (reset-roths)
  (case logic
    :prop (do
            (io/import-rules (str rothpath "rules-prop.edn") roths)
            (io/import-theorems (str rothpath "theorems-prop.edn") roths))
    :pred (do
            (io/import-rules (str rothpath "rules-prop.edn") roths)
            (io/import-rules (str rothpath "rules-pred.edn") roths)
            (io/import-theorems (str rothpath "theorems-prop.edn") roths)
            (io/import-theorems (str rothpath "theorems-pred.edn") roths))
    :ltl (do
           (io/import-rules (str rothpath "rules-ltl.edn") roths)
           #_(io/import-theorems (str rothpath "theorems-ltl.edn") roths)))
  (println welcome)
  (println (str "Info: Rules and theorems loaded: " logic)))

;; Choose the logic for the session:

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
  (printer/pprint @p))

;; ## Creating a new proof

(defn proof
  "Starts a new proof.     
   Modifies: atom `p`."
  ([conclusion] (proof [] conclusion))
  ([premises conclusion]
   (reset! p-history [])
   (reset! p (proof/proof premises conclusion))
   (show)
   (if (empty? @roths)
     (println "Info: There are no rules available, please load a logic with load-logic."))))

;; ## Functions that perform steps in proving a conclusion

(defn step-f
  "Execute a forward step.      
   Modifies: atom `p`, the proof, and       
             atom `p-history`, the history of the current proof."
  [roth & args]
  (try
    (let [proof' (deduc/step-f @p roth (vec args))]
      (swap! p-history conj @p)
      (reset! p proof')
      (show))
    (catch Exception e
      (println (str "Error: " (.getMessage e))))))

(defn step-b
  "Execute a backward step.      
   Modifies: atom `p`, the proof, and
             atom `p-history`, the history of the current proof."
  [roth & args]
  (try
    (let [proof' (deduc/step-b @p roth (vec args))]
      (swap! p-history conj @p)
      (reset! p proof')
      (show))
    (catch Exception e
      (println (str "Error: " (.getMessage e))))))

(defn unify
  "Unifies symbols.     
   Modifies: atom `p`, the proof, and
             atom `p-history`, the history of the current proof."
  [old new]
  (try
    (let [proof' (deduc/unify @p old new)]
      (swap! p-history conj @p)
      (reset! p proof')
      (show))
    (catch Exception e
      (println (str "Error: " (.getMessage e))))))

(defn undo
  "Undo the last change of the proof.     
   Modifies: atom `p`, the proof, and
             atom `p-history`, the history of the current proof."
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
  "Generates TeX-Code from the current proof.       
   The generated code uses the packages `MnSymbol` and `logicproof`.     
   If a filename is given as argument, a pdf file will be generated.      
   In this case the function uses the shell command `texi2pdf` that compiles tex code,
   and `open` to show the generated file."
  ([] (printer/texify @p))
  ([filename] (printer/texify @p filename)))

;; ## Function that exports the current proof as a reusable theorem

(defn export
  "Saves the current proof with `id` in the file with `filename`.      
   Requires: Proof is completed.    
   If there is already a theorem with that `id`, the replacement can be forced 
   by a mode of `:force`."
  ([filename id] (export filename id :check))
  ([filename id mode]
   (try 
    (io/export-theorem @p filename id mode)
    (catch Exception e
      (println (str "Error: " (.getMessage e)))))))

