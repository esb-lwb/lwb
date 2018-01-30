; lwb Logic WorkBench -- Predicate logic

; Copyright (c) 2015 - 2018 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.pred.examples.romeo
  (:require [lwb.pred :refer (make-pred sig-from-model wff? eval-phi)]))

;; This is exercise 27 in Peter Smith: An Introduction to Formal Logic p. 261
;; see also http://www.logicmatters.net/resources/pdfs/answers/Exercises27.pdf 

;; Shakespeare's world
(def s-world
  {:univ #{:romeo :juliet :benedick :beatrice}
   'm    [:func 0 :romeo]
   'n    [:func 0 :juliet]
   'F    [:pred 1 (make-pred #{[:romeo] [:benedick]})]
   'G    [:pred 1 (make-pred #{[:juliet] [:beatrice]})]
   'L    [:pred 2 (make-pred #{[:romeo :juliet] 
                               [:juliet :romeo]
                               [:benedick :beatrice]
                               [:beatrice :benedick]
                               [:benedick :benedick]})]
   })

(def phi1 '(exists [x] (L m x)))

(eval-phi phi1 s-world)
; => true -- (L :romeo :juliet)

(def phi2 '(forall [x] (L m x)))

(eval-phi phi2 s-world)
; => false --  e.g. (not (L :romeo :romeo)) 

(def phi3 '(impl (exists [x] (L m x)) (L m n)))

(eval-phi phi3 s-world)
; => true -- both parts are true

(def phi4 '(forall [x] (equiv (F x) (not (G x)))))
 
(eval-phi phi4 s-world)
; => true

(def phi5 '(forall [x] (impl (G x) (or (L x m) (not (L m x))))))

(eval-phi phi5 s-world)
; => true

(def phi6 '(forall [x] (impl (G x) (exists [y] (L x y)))))

(eval-phi phi6 s-world)
; => true

(def phi7 '(exists [x] (and (F x) (forall [y] (impl (G y) (L x y))))))

(eval-phi phi7 s-world)
; => false

;; Another world
(def a-world
  {:univ #{4 7 8 11 12}
   'm    [:func 0 7]
   'n    [:func 0 12]
   'F    [:pred 1 even?]
   'G    [:pred 1 odd?]
   'L    [:pred 2 #(< %1 %2)]})

; Both worlds have the same signature
(= (sig-from-model s-world) (sig-from-model a-world))
; => true

(eval-phi phi1 a-world)
; => true

(eval-phi phi2 a-world)
; => false

(eval-phi phi3 a-world)
; => true

(eval-phi phi4 a-world)
; => true

(eval-phi phi5 a-world)
; => false

(eval-phi phi6 a-world)
; => true

(eval-phi phi7 a-world)
; => true
