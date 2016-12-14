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
            [lwb.nd.prereqs :as prereqs]                    ; unused alias, but needed for potemkin
            [lwb.nd.rules :refer [roths reset-roths]]
            [lwb.nd.io :as io]
            [lwb.nd.printer :as printer]
            [clojure.string :as str]))

(defn man
  "Manual"
  []
  (let [info (str/join \newline
                       ["lwb - Natural Deduction"
                        "Namespace lwb.nd.repl"
                        "- (load \"lwb/nd/repl\")"
                        "- (ns lwb.nd.repl)"
                        "Functions:"
                        "- (load-logic {:prop|:pred|:ltl})"
                        "  load rules and theorems of one of the three logics"
                        "  e.g. (load-logic :prop)"
                        "- (proof givens conclusion)"
                        "  new claim to be proved"
                        "  e.g. (proof '(not (or P Q)) '(and (not P) (not Q)))"
                        "- (step-f roth args)"
                        "  proof step forward using rule or theorem with args"
                        "  e.g. (step-f :and-e1 1)"
                        "- (step-b roth args)"
                        "  proof step backward using rule or theorem with args"
                        "  e.g. (step-b :not-i 5)"
                        "- (swpa symbol replacement"
                        "  swaps symbol and replacement"
                        "  e.g. (swap '?1 'Q)"
                        "- (undo)"
                        "  reverse the last proofstep"
                        "- (show)"
                        "  show current proof"
                        "- (show-roth id)"
                        "  prints the rule or theorem with that id"
                        "  eg. (show-roth :and-i)"
                        "- (show-roths), (show-rules), (show-theorems)"
                        "  prints all roths, just the rules, just the theorems resp."
                        "- (texify) (texify filename)"
                        "  prints TeX code of the proof, generates pdf with the proof"
                        "  e.g. (texify \"myproof\")"
                        "- (export filename id) (export filename id :force)"
                        "  exports proof to file under id, forced if already in the file"
                        "  e.g. (export \"mytheorems.edn\" :and-comm)"
                        "- (import-theorems filename)"
                        "  imports theorems from file"
                        "  e.g. (import-theorems \"mytheorems.edn\")"])]
    (println info)))


(def rothpath
  "Path to the files for rules and theorems as resources"
  "nd/")

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
   Requires: resource with rules and theorems     
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
           (io/import-theorems (str rothpath "theorems-ltl.edn") roths)))
  (reset-meta! #'roths {:logic logic})
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

(defn current-proof
  "Current proof."
  []
  @p)

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

(defn swap
  "Exchanges question mark symbols of the form `?n` with a `new` expression.     
   Requires: `old` has the form `?n`.     
             `new` is allowed according to the current logic.     
   Modifies: atom `p`, the proof, and
             atom `p-history`, the history of the current proof."
  ([old new]
   (swap old new :unchecked))
  ([old new mode]
  (try
    (let [proof' (deduc/swap @p (:logic (meta #'roths)) old new mode)]
      (swap! p-history conj @p)
      (reset! p proof')
      (show))
    (catch Exception e
      (println (str "Error: " (.getMessage e)))))))

(defn subclaim
  [fml]
  (try
    (let [proof' (deduc/subclaim @p fml)]
      (swap! p-history conj @p)
      (reset! p proof')
      (show))
    (catch Exception e
      (println (str "Error: " (.getMessage e))))))

;; ###Remark:         

;; Some of the rules have constraints, e.g. the introduction of a actual element in the
;; introduction of forall must be fresh.      
;; Such constraints are not expressed in the rule, but checked be the functions in the
;; namespace `lwb.nd.swap` for the logics we support.     
;; It would be an interesting task to enhance the langugae of our rules to make a more
;; generic solution possible.

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

;; ## Function that loads the current proof into the storage of rohts for the current session

(defn load-theorem
  "Load proved theorem from the current proof into the theorem storage.      
   Requires: theorem is proved and the `id` is fresh."
  [id]
  (io/import-theorem (current-proof) id roths)
  (println (format "Current proof as theorem '%s' loaded." id)))

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

;; ## Function that imports theorems

(defn import-theorems
  "Reads the file with `filename` and imports theorems from it to the current logic.
   Caveat: If there's already a theorem with a name from the new ones, it's overwritten.
   Requires: Theorems are proved.
   Modifies: global `roths`"
  [filename]
  (try
    (io/import-theorems filename @roths :file)
    (catch Exception e
      (println (str "Error: " (.getMessage e))))))

