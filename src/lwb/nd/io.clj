; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.io
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [lwb.nd.storage :refer [rules theorems reset-rules]]
            [lwb.nd.rules :refer [gen-logic-function]]
            [lwb.nd.proof :refer [proved?]])
  (:import [java.io PushbackReader]))

(defn read-items
  "Reads sequence of items from file"
  [filename]
  (with-open [r (PushbackReader. (io/reader filename))]
    (doall (take-while some? (repeatedly #(edn/read {:eof nil} r))))))

(defn import-rules
  "Imports all rules from `filename` into global atom `rules`."
  [filename]
  (let [map-fn (fn [item]
                 (hash-map (:id item)
                           {:type       :rule
                            :given      (:given item)
                            :conclusion (:conclusion item)
                            :prereq     (:prereq item)
                            :forward    (:forward item)
                            :backward   (:backward item)
                            :logic-rel  (eval (gen-logic-function (:prereq item) (:given item) (:conclusion item)))}))]
    (apply (partial swap! rules merge) (map map-fn (read-items filename)))))

(comment
  (reset-rules)
  (import-rules "./resources/nd/rules-prop.edn")
  )

(defn import-theorems
  "Imports all theorems from `filename` into global atom `rules`."
  [filename]
  (let [map-fn (fn [item]
                 (hash-map (:id item)
                           {:type       :theorem 
                            :given      (:given item)
                            :conclusion (:conclusion item)
                            :forward    true
                            :logic-rel  (eval (gen-logic-function (:prereq item) (:given item) (:conclusion item)))}))]
    (apply (partial swap! rules merge) (map map-fn (read-items filename)))))

#_(import-theorems "./resources/nd/theorems-prop.edn")

#_(defn import-trivials
    "Imports all trivial-theorems from filename into the internal trivial-theorems-storage.
     Existing trivial-theorems will be kept."
    [filename]
    (with-open [reader (io/reader filename)]
      (loop [item (read (PushbackReader. reader) false nil)
             result {}]
        (if item
          (recur (read (PushbackReader. reader) false nil)
                 (assoc result (:id item) {:given      (:given item)
                                           :conclusion (:conclusion item)
                                           :forward    true}))
          (swap! trivials merge result)))))

#_(defn import-theorems
    "Imports all theorems from filename into the internal theorems-storage. Existing theorems will be kept."
    [filename]
    (with-open [reader (io/reader filename)]
      (loop [item (read (PushbackReader. reader) false nil)
             result {}]
        (if item
          (recur (read (PushbackReader. reader) false nil)
                 (assoc result (:id item) {:given      (:given item)
                                           :conclusion (:conclusion item)
                                           :forward    (:forward item)
                                           :proof      (:proof item)}))
          (swap! theorems merge result)))))

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
