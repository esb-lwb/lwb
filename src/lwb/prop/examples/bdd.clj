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
  (:require [lwb.prop.bdd :refer :all]))

; a tautology
(def phi1 '(and (impl (impl P Q) (or (not P) Q)) (impl (or (not P) Q) (impl P Q))))

(wff? phi1)

(bdd phi1)

(bdd (list 'not phi1))

(sat phi1)

(valid? phi1)

