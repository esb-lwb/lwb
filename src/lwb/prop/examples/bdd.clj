; lwb Logic WorkBench -- Propositional Logic 
; Examples: Binary decision diagrams

; Copyright (c) 2016 Mathias Gutenbrunner, Jens Lehnhäuser and Burkhardt Renz, THM.
; All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.examples.bdd
  (:require [lwb.prop :refer :all])
  (:require [lwb.prop.cardinality :refer :all])
  (:require [lwb.prop.bdd :refer :all]))


; ----------------------------------------------------------------------------------------------------
; majority function

(defn majority
  "majority function in cnf for the given collection of symbols."
  [symbs]
  (let [c (count symbs)
        c' (if (odd? c) (inc c) c)
        k (quot c' 2)]
    (list* 'and (min-kof k symbs))))

(vis-pdf (majority '(x_1 x_2)) "maj2")
(vis-pdf (majority '(x_1 x_2 x_3)) "maj3")
(vis-pdf (majority '(x_1 x_2 x_3 x_4)) "maj4")
(vis-pdf (majority '(x_1 x_2 x_3 x_4 x_5)) "maj5")
(vis-pdf (majority '(x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 x_<10> x_<11> x_<12> x_<13> x_<14> x_<15>)) "maj15")
(vis-pdf (majority '(x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 x_<10> x_<11> x_<12> x_<13> x_<14> x_<15> x_<16>)) "maj16")

; ----------------------------------------------------------------------------------------------------
; a tautology
(def phi1 '(and (impl (impl P Q) (or (not P) Q)) (impl (or (not P) Q) (impl P Q))))

(wff? phi1)

(sat phi1)
(valid? phi1)

(vis-pdf phi1 "phi1")

; ----------------------------------------------------------------------------------------------------
; a formulae thhats true for any value of Q
(def phi2 '(impl (or (impl S (or R L)) (and (not Q) R)) (impl (not (impl P S)) R)))

(vis-pdf phi2 "phi2")

(sat phi2 :all)
; =>
; ([L false P false Q false R false S false]
;   [L false P false Q false R false S true]
;   [L false P false Q false R true S false]
;   [L false P false Q false R true S true]
;   [L false P false Q true R false S false]
;   [L false P false Q true R false S true]
;   [L false P false Q true R true S false]
;   [L false P false Q true R true S true]
;   [L true P false Q false R false S false]
;   [L true P false Q false R false S true]
;   [L true P false Q false R true S false]
;   [L true P false Q false R true S true]
;   [L true P false Q true R false S false]
;   [L true P false Q true R false S true]
;  [L true P false Q true R true S false]
;  [L true P false Q true R true S true]
;  [L false P true Q false R false S true]
;  [L false P true Q true R false S true]
;  [L true P true Q false R false S true]
;  [L true P true Q true R false S true]
;  [L false P true Q false R true S false]
;  [L false P true Q false R true S true]
;  [L false P true Q true R true S false]
;  [L false P true Q true R true S true]
;  [L true P true Q false R true S false]
;  [L true P true Q false R true S true]
;  [L true P true Q true R true S false]
;  [L true P true Q true R true S true])

