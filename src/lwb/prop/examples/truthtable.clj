; lwb Logic WorkBench -- Propositional Logic 
; Examples: Truht tables

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.examples.truthtable
  (:require [lwb.prop :refer :all])
  (:require [lwb.prop.sat :refer [valid?]]))

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
