; lwb Logic WorkBench -- Propositional Logic 
; Examples: Truth tables

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.examples.truthtable
  (:require [lwb.prop :refer :all])
  (:require [lwb.prop.sat :refer [valid? sat?]]))

(defn ptt
  [phi]
  (->> phi 
      truth-table
      print-truth-table))

;; Examples from Volker Halbach: The Logic Manual

;; p. 38
(def phi1 '(impl (not (impl P Q)) (and P Q)))

(ptt phi1)

; | P | Q | :result |
; |---+---+---------|
; | T | T |       T |
; | T | F |       F |
; | F | T |       T |
; | F | F |       T |

;; p.43
(def phi2 '(impl (and (impl P (not Q)) Q) (not P)))

(ptt phi2)

; | P | Q | :result |
; |---+---+---------|
; | T | T |       T |
; | T | F |       T |
; | F | T |       T |
; | F | F |       T |

;; p. 46
(def phi3 '(equiv (or P (not Q)) (impl Q P)))

(ptt phi3)
; a tautology

;; p.47
(def phi4 '(impl (and (impl P Q) (or (not (impl P1 Q)) (and P P1))) (and (equiv P Q) P1)))

(ptt phi4)
; a tautology

;; p. 50 nor
(def phi5 '(and (not phi) (not psi)))

(ptt phi5)

; | phi | psi | :result |
; |-----+-----+---------|
; |   T |   T |       F |
; |   T |   F |       F |
; |   F |   T |       F |
; |   F |   F |       T |

(valid? '(equiv (and (not phi) (not psi)) (not (or phi psi))))
; => true

(valid? `(~'equiv ~phi5 ~'(not (or phi psi))))
; =>

;; Exercise 2.5 from Volkar Halbach's Exercises Booklet

; Modus ponens
(ptt '(impl (and P (impl P Q)) Q))

; Modus tollens
(ptt '(impl (and (not Q) (impl P Q)) (not P)))

; Law of the excluded middle
(ptt '(or P (not P)))

; Law of contradiction
(ptt '(not (and P (not P))))

; Consequentia mirabilis
(ptt '(impl (impl (not P) P) P))

; Classical dilemma
(ptt '(impl (and (impl P Q) (impl (not P) Q)) Q))

; de Morgan
(ptt '(equiv (not (and P Q)) (or (not P) (not Q))))

; de Morgan
(ptt '(equiv (not (or P Q)) (and (not P) (not Q))))

; Ex falso quoidlibet
(ptt '(impl (and P (not P)) Q))

;; Exercise 2.6

(def phi261 '(and P P))
(and (sat? phi261) (not (valid? phi261)))
; neither a tautology nor a contradiction

(def phi262 '(equiv (impl (impl P Q) R) (impl P (impl Q R))))
(and (sat? phi262) (not (valid? phi262)))
; neither a tautology nor a contradiction

(def phi263 '(equiv (equiv P (equiv Q R)) (equiv (equiv P Q) R)))
(valid? phi263)
; a tautology

(def phi264 '(equiv (not (impl P Q)) (or P (not Q))))
(and (sat? phi264) (not (valid? phi264)))
; neither a tautology nor a contradiction

