; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 - 2016 Tobias Völzel, Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.io
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [lwb.nd.rules :refer [gen-roth-relation roth-structure-f roth-structure-b]]
            [lwb.nd.proof :refer [proved?]])
  (:import [java.io PushbackReader]))

;TODO: validate inout of rules and theorems

(defn read-roths
  "Reads sequence of items from file"
  [filename]
  (with-open [r (PushbackReader. (io/reader filename))]
    (doall (take-while some? (repeatedly #(edn/read {:eof nil} r))))))

(defn import-rules
  "Imports all rules from `filename` into global atom `roths`.     
   Requires: `filename` exists and has valid rules.     
   Modifies; global atom `roths`."
  [filename roths]
  (let [map-fn (fn [rule]
                 (hash-map (:id rule)
                           {:type       :rule
                            :prereq     (:prereq rule)
                            :given      (:given rule)
                            :extra      (:extra rule)
                            :conclusion (:conclusion rule)
                            :forward    (roth-structure-f (:given rule) (:extra rule) (:conclusion rule))
                            :backward   (roth-structure-b (:given rule) (:extra rule) (:conclusion rule))
                            :logic-rel  (eval (gen-roth-relation (:prereq rule) (:given rule) (:extra rule)
                                                                 (:conclusion rule)))}))]
    (apply (partial swap! roths merge) (map map-fn (read-roths filename)))))

(defn import-theorems
  "Imports all theorems from `filename` into global atom `roths`.        
   Requires: `filename` exists and has valid theorems.
   Modifies; global atom `roths`."
  [filename roths]
  (let [map-fn (fn [theorem]
                 (hash-map (:id theorem)
                           {:type       :theorem 
                            :given      (:given theorem)
                            :conclusion (:conclusion theorem)
                            :forward    (roth-structure-f (:given theorem) (:extra theorem) (:conclusion theorem))
                            :backward   (roth-structure-b (:given theorem) (:extra theorem) (:conclusion theorem))
                            :logic-rel  (eval (gen-roth-relation nil (:given theorem) nil (:conclusion theorem)))}))]
    (apply (partial swap! roths merge) (map map-fn (read-roths filename)))))

;; TODO: Das muss neu programmiert werden
#_(defn export-theorem
    "Exports proof as a theorem with the id to filename"
    [proof filename id]
    (if (proved? proof)
      (if (.exists (io/as-file filename))
        (let [given (into [] (map :body (filter #(= (:rule %) :premise) (flatten proof))))
              conclusion (vector (:body (last proof)))
              theorem {:id         id
                       :given      given
                       :conclusion conclusion
                       :forward    true
                       :proof      proof}]
          (with-open [writer (io/writer filename :append true)]
            (.write writer (str theorem))
            (.newLine writer))
          (swap! theorems merge (hash-map id (dissoc theorem :id))))
        (throw (Exception. (str "The System can't find the file \"" filename "\""))))
      (throw (Exception. "The given proof is not solved yet. You can only export solved proofs as theorems."))))
