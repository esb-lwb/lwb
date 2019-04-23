; lwb Logic WorkBench -- Church encoding of booleans

; Copyright (c) 2019  Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.cl.examples.booleans
  (:require [lwb.cl :refer :all]))

(def-combinators-ski)

;; Definition of truth values -------------------------------------------------

(def True '[K])
(def False '[K I])

(defn to-boolean
  [term]
  (let [red (weak-reduce (conj term 't 'f))]
    (cond
      (= red '[t]) 'True
      (= red '[f])   'False
      :else nil)))

;; Definition of operators ----------------------------------------------------

(def And '[S S K])

(defn And'
  [term1 term2]
    (to-boolean (weak-reduce (comb-concat And term1 term2))))
  
(And' True True)
(And' True False)
(And' False True)
(And' False False)

(def Or '[S I I])

(defn Or'
  [term1 term2]
  (to-boolean (weak-reduce (comb-concat Or term1 term2))))
  
(Or' True True)
(Or' True False)
(Or' False True)
(Or' False False)

(def Not '[S (S I (K (K I))) (K K)])

(defn Not'
  [term]
  (to-boolean (weak-reduce (comb-concat Not term))))

(Not' True)
(Not' False)

  