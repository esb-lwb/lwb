; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

;; # Examples: Hilbert's axioms for the propositional logic

(ns lwb.nd.examples.hilbert
  (:require [lwb.nd.repl :refer :all]))

(load-logic :prop)

; interactive checking in the repl for nd

; -----------------------------------------------------------------------------------------
; Hilbert A1
(proof '(impl F (impl G F)))
(step-b :impl-i 2)
(step-b :impl-i 3)

; -----------------------------------------------------------------------------------------
; Hilbert A2
(proof '(impl (impl F (impl G H)) (impl (impl F G) (impl F H))))
(step-b :impl-i 2)
(step-b :impl-i 3)
(step-b :impl-i 4)
(step-f :impl-e 1 3)
(step-f :impl-e 2 3)
(step-f :impl-e 4 5)

; -----------------------------------------------------------------------------------------
; Hilbert A3
(proof '(impl (impl (not F) (not G)) (impl G F)))
(step-b :impl-i 2)
(step-b :impl-i 3)
(step-b :raa 4)
(step-f :impl-e 1 3)
(step-f :not-e 4 2)

; -----------------------------------------------------------------------------------------
; Hilbert A4
(proof '(impl F (impl (not F) G)))
(step-b :impl-i 2)
(step-b :impl-i 3)
(step-f :not-e 2 1)
(step-b :efq 5)

; -----------------------------------------------------------------------------------------
; Hilbert A5
(proof '(impl (impl (not F) F) F))
(step-b :impl-i 2)
(step-b :raa 3)
(step-f :impl-e 1 2)
(step-f :not-e 2 3)

