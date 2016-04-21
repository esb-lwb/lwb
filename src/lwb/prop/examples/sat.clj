; lwb Logic WorkBench -- Propositional Logic 
; Examples: Satisfiability

; Copyright (c) 2016 Burkhardt Renz, THM. ; All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.examples.sat
  (:require [lwb.prop :refer :all])
  (:require [lwb.prop.sat :refer :all]))

; R.L.Rivest's associative block design, see Knuth TAOCP Vol.4 Fasc. 6 p. 4

(def R8 '(and (or x_1 x_2 (not x_3)) (or x_2 x_3 (not x_4)) (or x_3 x_4 x_1) (or x_4 (not x_1) x_2)
              (or (not x_1) (not x_2) x_3) (or (not x_2) (not x_3) x_4) (or (not x_3) (not x_4) (not x_1))
              (or (not x_4) x_1 (not x_2))))

(sat? R8)
; => false

(def R7 '(and (or x_1 x_2 (not x_3)) (or x_2 x_3 (not x_4)) (or x_3 x_4 x_1) (or x_4 (not x_1) x_2)
              (or (not x_1) (not x_2) x_3) (or (not x_2) (not x_3) x_4) (or (not x_3) (not x_4) (not x_1))))

(sat? R7)

(sat R7 :all)
; => ([x_1 false x_2 true x_3 false x_4 true]
;     [x_1 false x_2 true x_3 true x_4 true])