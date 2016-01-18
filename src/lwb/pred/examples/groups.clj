; lwb Logic WorkBench -- Predicate logic

; Copyright (c) 2015 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.pred.examples.groups
  (:require [lwb.pred :refer (wff? eval-phi)]))

;; First definition of a group
;; Signature of a group in the spirit of the universal algebra
(def grp-sig
  {:unit [:func 0]
   :op   [:func 2]
   :inv  [:func 1]})

;; Group axioms
;; Associativity of group operation
(def grp-ass '(forall [x y z] (= (op x (op y z)) (op (op x y) z))))
#_(wff? grp-ass grp-sig :msg)

;; Unit
(def grp-unit '(forall [x] (= (op x unit) x)))
#_(wff? grp-unit grp-sig :msg)

;; Inverses
(def grp-inv '(forall [x] (= (op x (inv x)) unit)))
#_(wff? grp-inv grp-sig :msg)

;; Abelian Group?
(def grp-comm '(forall [x y] (= (op x y) (op y x))))
#_(wff? grp-comm grp-sig :msg)

;; Group with 1 element
(def c1
  {:univ #{0}
   :op   [:func 2 #(mod (+ %1 %2) 1)]
   :inv  [:func 1 #(mod (- 1 %) 1)]
   :unit [:func 0 0]})

(eval-phi grp-ass c1)
(eval-phi grp-inv c1)
(eval-phi grp-unit c1)
(eval-phi grp-comm c1)

;; Cyclic group with 6 elements
(def c6
  {:univ #{0 1 2 3 4 5}
   :op   [:func 2 #(mod (+ %1 %2) 6)]
   :inv  [:func 1 #(mod (- 6 %) 6)]
   :unit [:func 0 0]})

(eval-phi grp-ass c6)
(eval-phi grp-inv c6)
(eval-phi grp-unit c6)
(eval-phi grp-comm c6)

;; Symmetric group on 3 elements
;; Sym3 is the smallest non-abelian group 
;; Sym3 has cardinality 6

(defn sym3-op [x y]
  (let [sym3-table [[0 1 2 3 4 5]
                    [1 0 4 5 2 3]
                    [2 5 0 4 3 1]
                    [3 4 5 0 1 2]
                    [4 3 1 2 5 0]
                    [5 2 3 1 0 4]]]
    (get-in sym3-table [x y])))

(defn sym3-inv [x]
  (get [0 1 2 3 5 4] x))

(def sym3
  {:univ #{0 1 2 3 4 5}
   :op   [:func 2 sym3-op]
   :inv  [:func 1 sym3-inv]
   :unit [:func 0 0]})

(eval-phi grp-ass sym3)
(eval-phi grp-inv sym3)
(eval-phi grp-unit sym3)
(eval-phi grp-comm sym3)
; => false

;; Second, classic definition of a group
;; Signature of a group according to the classical definition
(def grp-sig-classic
  {:op [:func 2]})

(def grp-axioms-classic
  '(and
     (forall [x y z] (= (op x (op y z)) (op (op x y) z)))
     (exists [unit] (and
                      (forall [x] (= (op x unit) x))
                      (forall [x] (exists [inv] (= (op x  inv) unit)))))))
#_(wff? grp-axioms-classic grp-sig-classic :msg)

(def sym3'
  {:univ #{0 1 2 3 4 5}
   :op   [:func 2 sym3-op]})

(eval-phi grp-axioms-classic sym3')
; => true (but quite slow)
(eval-phi grp-comm sym3')
; => false
