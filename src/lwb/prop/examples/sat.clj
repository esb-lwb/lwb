; lwb Logic WorkBench -- Propositional Logic 
; Examples: Satisfiability

; Copyright (c) 2016 Burkhardt Renz, THM. ; All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.examples.sat
  (:require [lwb.prop :refer :all])
  (:require [lwb.prop.sat :refer :all])
  (:require [clojure.math.combinatorics :refer (combinations)]))

; ----------------------------------------------------------------------------------------------------
; R.L.Rivest's associative block design, see Knuth TAOCP Vol. 4 Fasc. 6 p. 4

(def R-clauses '[(or x_1 x_2 (not x_3))
                 (or x_2 x_3 (not x_4))
                 (or x_3 x_4 x_1)
                 (or x_4 (not x_1) x_2)
                 (or (not x_1) (not x_2) x_3)
                 (or (not x_2) (not x_3) x_4)
                 (or (not x_3) (not x_4) (not x_1))
                 (or (not x_4) x_1 (not x_2))])

; The conjunction of the 8 clauses is not satisfiable
(cnf? (conj (apply list R-clauses) 'and))
; => true
(sat? (conj (apply list R-clauses) 'and))
; => false

; The conjunction of each of the subsets with 7 elements of the clauses is satisfiable
; and has 2 models
(def R'-conjs (map #(conj (apply list %) 'and) (combinations R-clauses 7)))

(map sat? R'-conjs)
; => (true true true true true true true true)

(sat (first R'-conjs) :all)
; => ([x_1 false x_2 true x_3 false x_4 true]
;     [x_1 false x_2 true x_3 true x_4 true])

(map #(count (sat % :all)) R'-conjs)
; => (2 2 2 2 2 2 2 2)
; ----------------------------------------------------------------------------------------------------

; Examples from the lecture "Logik und formale Methoden"
;
(def phi1 '(and (or P (not Q) R)
               (or (not P) Q)
               (or (not P) R)
               (or (not R) S)
               (or R (not S))
               (or (not P) (not S))
               (or (not P) (not Q) S)))

(sat phi1 :all)

; => ([P false Q false R true S true]
;     [P false Q true R true S true]
;     [P false Q false R false S false])

(def phi2 '(and (or Y_1)
               (or Y_2)
               (or (not Y_1) X Z)
               (or (not Y_2) (not X) Z)
               (or Y_3 (not Z))
               (or (not Y_3) (not Z))))

(sat phi2)
; => nil

;: Example Changing code after review

; original code
(def orig '(or (and (not a) (not b) h) (and (not (and (not a) (not b))) (or (and (not a) g) (and a f)))))

; version 1 after review
(def v1 '(or (and a h) (and (not a) (or (and b g) (and (not b) f)))))

; version 2 after review
(def v2 '(or (and a f) (and (not a) (or (and b g) (and (not b) h)))))

(def x1 (list 'equiv orig v1))
(def x2 (list 'equiv orig v2))

(valid? x1)
; => false
(valid? x2)
; => true
; ----------------------------------------------------------------------------------------------------
