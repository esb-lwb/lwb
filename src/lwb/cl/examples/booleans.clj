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
  (let [t (min-parens term)]
    (cond
      (= t True) 'True
      (= t False) 'False
      :else nil)))

;; Definition of operators ----------------------------------------------------

(defn And ; = [S S K]
  [term1 term2]
    (to-boolean (weak-reduce (comb-concat '[S S K] term1 term2))))
  
(And True True)
(And True False)
(And False True)
(And False False)

(defn Or ;  = [S I I]
  [term1 term2]
  (to-boolean (weak-reduce (comb-concat '[S I I] term1 term2))))
  
(Or True True)
(Or True False)
(Or False True)
(Or False False)

(defn Not ;  = [S (S I (K (K I))) (K K)] (in normal order)
  [term]
  (to-boolean (weak-reduce (comb-concat '[S (S I (K (K I))) (K K)] term))))

(Not True)
(Not False)
  