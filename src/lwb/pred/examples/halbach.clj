; lwb Logic WorkBench -- Predicate logic

; Copyright (c) 2016 - 2018 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.pred.examples.halbach
  (:require [lwb.pred :refer (make-pred sig-from-model wff? eval-phi)]))

;; Examples from Volker Halbach: The Logic Manual Chap 5

;; European cities
; (Q x) = city x is on continent
; (R x y) = city x is smaller than city y

(def cities
  {:univ #{:florence :stockholm :barcelona :london}
   'a    [:func 0 :florence]
   'b    [:func 0 :london]
   'Q    [:pred 1 (make-pred #{[:florence] [:stockholm] [:barcelona]})]
   'R    [:pred 2 (make-pred #{[:florence :stockholm]
                               [:florence :barcelona] 
                               [:florence :london]
                               [:stockholm :barcelona]
                               [:stockholm :london]
                               [:barcelona :london]})]
   })

(def phi1 '(R a b))

(eval-phi phi1 cities)
; => true -- (R :florence :london)

(eval-phi '(R b a) cities)
; => false

(def phi2 '(forall [x] (impl (Q x) (R x b))))

(eval-phi phi2 cities)
; => true

(def phi3 '(forall [x] (exists [y] (or (R x y) (R y x)))))

(eval-phi phi3 cities)
; => true 

;; world-5-9
(def world-5-9
  {:univ #{1 2}
   'b    [:func 0 1]
   'Q    [:pred 1 (make-pred #{[1]})]})

(def phi4 '(impl (Q b) (forall [x] (Q x))))

(eval-phi phi4 world-5-9)
; => false

;; cosmos
(def cosmos
  {:univ #{:sun :moon}
   'R    [:pred 2 (make-pred #{[:sun :sun] [:moon :moon]})]})

(def phi5 '(impl (forall [x] (exists [y] (R x y))) (exists [y] (forall [x] (R x y)))))

(eval-phi phi5 cosmos)
; => false

;; world-5-11
(def world-5-11
  {:univ #{1}
   'a    [:func 0 1]
   'P    [:pred 1 (make-pred #{[1]})]
   'Q    [:pred 1 (make-pred #{[1]})]
   'R    [:pred 1 (make-pred #{})]})

(def phi6 '(impl (and (forall [x] (impl (P x) (or (Q x) (R x)))) (P a)) (R a)))

(eval-phi phi6 world-5-11)
; => false

;; Exercise 5.1 from Volker Halbach's Exercises Booklet

(def ds
  {:univ #{1 2 3}
   'a    [:func 0 1]
   'b    [:func 0 3]
   'P    [:pred 1 (make-pred #{[2]})]
   'R    [:pred 2 (make-pred #{[1 2] [2 3] [1 3]})]})      

;; (i)
(eval-phi '(P a) ds)
; => false; a is not in P

;; (ii)
(eval-phi '(R a b) ds)
; => true; [1 3] is in R

;; (iii)
(eval-phi '(R b a) ds)
; => false; [3 1] is not in R

;; (iv)
(eval-phi '(equiv (R a b) (R b a)) ds)
; => false; see (ii) and (iii)

;; (v)
(eval-phi '(or (R b b) (and (not (P a)) (not (R a a)))) ds)
; => true; the second part is true

;; (vi)
(eval-phi '(exists [x] (R a x)) ds)
; => true; e.g. x = 2

;; (vii)
(eval-phi '(exists [x] (and (R a x) (R x b))) ds)
; => true; e.g. x = 2

;; (viii)
(eval-phi '(or (P b) (exists [x] (R x x))) ds)
; => false; 3 ist not in P, and R is not symmetric

;; (ix)
(eval-phi '(forall [x] (exists [y] (R x y))) ds)
; => false; [3 ?] is not in R

;; (x)
(eval-phi '(forall [x] (impl (P x) (and (exists [y] (R y x)) (exists [y] (R x y))))) ds)
; => true; x is 2, [1 2] in R and [2 3] in R

;; (xi)
(eval-phi '(forall [x] (impl (P x) (exists [y] (and (R y x) (R x y))))) ds)
; => false; x is 2, but no pair and its mirrored one

