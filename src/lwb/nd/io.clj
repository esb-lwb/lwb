; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.io
  (:require [clojure.java.io :as io]
            [lwb.nd.proof :refer [proved?]])
  (:import [java.io PushbackReader]))

;; storage for rules, theorems and trivial-theorems
(def rules (atom {}))
(def trivials (atom {}))
(def theorems (atom {}))

(defn reset-rules 
  "Empties the internal storage for rules"
  [] (reset! rules {}))

(defn reset-trivials 
  "Empties the internal storage for trivial-theorems"
  [] (reset! trivials {}))

(defn reset-theorems 
  "Empties the internal storage for theorems"
  [] (reset! theorems {}))

(defn import-rules
  "Imports all rules from filename into the internal rules-storage. Existing rules will be kept."
  [filename]
  (with-open [reader (io/reader filename)]
    (loop [item (read (PushbackReader. reader) false nil)
           result {}]
      (if item
        (recur (read (PushbackReader. reader) false nil)
               (assoc result (keyword (:name item)) {:given      (:given item)
                                                     :conclusion (:conclusion item)
                                                     :prereq     (:prereq item)
                                                     :forwards   (:forwards item)
                                                     :backwards  (:backwards item)}))
        (swap! rules merge result)))))

(defn import-trivials
  "Imports all trivial-theorems from filename into the internal trivial-theorems-storage.
   Existing trivial-theorems will be kept."
  [filename]
  (with-open [reader (io/reader filename)]
    (loop [item (read (PushbackReader. reader) false nil)
           result {}]
      (if item
        (recur (read (PushbackReader. reader) false nil)
               (assoc result (keyword (:name item)) {:given      (:given item)
                                                     :conclusion (:conclusion item)
                                                     :forwards   true}))
        (swap! trivials merge result)))))

(defn import-theorems
  "Imports all theorems from filename into the internal theorems-storage. Existing theorems will be kept."
  [filename]
  (with-open [reader (io/reader filename)]
    (loop [item (read (PushbackReader. reader) false nil)
           result {}]
      (if item
        (recur (read (PushbackReader. reader) false nil)
               (assoc result (keyword (:name item)) {:given      (:given item)
                                                     :conclusion (:conclusion item)
                                                     :forwards   (:forwards item)
                                                     :proof      (:proof item)}))
        (swap! theorems merge result)))))

(defn export-theorem
  "Exports proof as a theorem with the name to filename"
  [proof filename name]
  (if (proved? proof)
    (if (.exists (io/as-file filename))
      (let [given (into [] (map :body (filter #(= (:rule %) :premise) (flatten proof))))
            conclusion (vector (:body (last proof)))
            theorem {:name name
                     :given given
                     :conclusion conclusion
                     :forwards true
                     :proof proof}]
        (with-open [writer (io/writer filename :append true)]
          (.write writer (str theorem))
          (.newLine writer))
        (swap! theorems merge (hash-map (keyword name) (dissoc theorem :name))))
      (throw (Exception. (str "The System can't find the file \"" filename "\""))))
    (throw (Exception. "The given proof is not solved yet. You can only export solved proofs as theorems."))))
