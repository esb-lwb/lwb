; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.repl
  (:require [lwb.consts :refer :all]
            [lwb.nd.deduction :as deduc]
            [lwb.nd.proof :as pr]
            [lwb.nd.prereqs :refer :all]
            [lwb.nd.storage :refer [roths reset-roths]]
            [lwb.nd.io :as io]
            [lwb.nd.printer :refer [pprint]]))

(defn load-logic
  "Load rules and theorems of the logic to use
   Logic can be :prop, :pred. :ltl"
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
           (io/import-theorems "resources/nd/theorems-ltl.edn"))
    ))

; Choose the logic for the session
;(load-logic :prop)
(load-logic :pred)
;(load-logic :ltl)

;; holds the actual state of the proof 
(def p (atom []))
;; holds the last steps since the beginning of the proof (for undo steps)
(def p-history (atom []))

(defn show
  "Print the actual state of the proof"
  []
  (pprint @p))

(defn proof
  "Start a new proof"
  ([formula] (proof [] formula))
  ([premises formula]
   (reset! p-history [])
   (reset! p (pr/proof premises formula))
   (show)))

; :TODO die anderen Aufrufe auch nach diesem Schema
(defn step-f
  "Execute a forward step"
  [roth & args]
  (try
    (let [proof' (deduc/step-f @p roth (vec args))]
      (swap! p-history conj @p)
      (reset! p proof')
      (show) )
    (catch Exception e
       (println welcome)
       (println (str "Error: " (.getMessage e))
       ))))
  
#_(defn step-f-inside
  "Executes a forward step inside the chosen line"
  [rule line]
  (swap! p-history conj @p)
  (swap! p #(apply deduc/step-f-inside (conj (list rule line) %)))
  (show))

(defn step-b
  "Execute a backward step"
  [roth & args]
  (swap! p-history conj @p)
  (swap! p deduc/step-b roth (vec args))
  (show))

(defn unify
  "Unifies symbols"
  [old new]
  (swap! p-history conj @p)
  (swap! p deduc/unify old new)
  (show))

#_(defn trivial
    "Apply the trivial-theorems inside the chosen line"
    [line]
    (swap! p-history conj @p)
    (swap! p deduc/trivial line)
    (show))

#_(defn export-theorem
    "Export the solved proof to a file as a theorem"
    [filename id]
    (io/export-theorem
      @p
      filename
      id))

(defn undo
  "Undo the last change (you can't go further than the last state)"
  []
  (if (empty? @p-history)
    (do
      (println welcome)
      (println "Info: You reached the starting point of the proof, there is nothing more to undo"))
    (do
      (reset! p (last @p-history))
      (swap! p-history #(into [] (drop-last %)))
      (show))))

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

