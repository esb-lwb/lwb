; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2018 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

;; # Exercises from Paul Tomassi: Logic, Routledge 1999 

;; Some of the proofs are much more simple if we use the already proven theorems
;; from prop_classical.clj

(ns lwb.nd.examples.tomassi
  (:require [lwb.nd.repl :refer :all]))

(load-logic :prop)

; interactive checking in the repl for nd

; -----------------------------------------------------------------------------------------
; p.108

; I.1
(proof '(impl P Q) '(impl (impl (and R Q) S) (impl (and R P) S)))
(step-b :impl-i 3)
(step-b :impl-i 4)
(step-f :and-e1 3)
(step-f :and-e2 3)
(step-f :impl-e 1 5)
(step-f :and-i 4 6)
(step-f :impl-e 2 7)

; I.2
(proof '(impl (and P Q) (not R)) '(impl R (impl P (not Q))))
(step-b :impl-i 3)
(step-b :impl-i 4)
(step-b :not-i 5)
(step-f :and-i 3 4)
(step-f :impl-e 1 5)
(step-f :not-e 6 2)

; I.3
(proof '(impl (impl P Q) (impl (not Q) (not P))))
(step-b :impl-i 2)
(step-b :impl-i 3)
(step-b :not-i 4)
(step-f :impl-e 1 3)
(step-f :not-e 2 4)

; I.4
(proof '(or P Q) '(or (or P R) (or Q R)))
(step-f :or-e 1 3)
(step-f :or-i1 2)
(swap '?1 'R)
(step-b :or-i1 5)
(step-f :or-i1 5)
(swap '?2 'R)
(step-b :or-i2 8)

; I.5
(proof '[(impl P R) (impl Q S)] '(impl (or P Q) (or R S)))
(step-b :impl-i 4)
(step-f :or-e 3 5)
(step-f :impl-e 1 4)
(step-b :or-i1 7)
(step-f :impl-e 2 7)
(step-b :or-i2 10)

 ; I.6
(proof '[(impl P (or Q R)) (impl Q R)] '(impl P R))
(step-b :impl-i 4)
(step-f :impl-e 1 3)
(step-f :or-e 4 6)
(step-f :impl-e 2 5)

; I.7
(proof '(impl (or P Q) R) '(and (impl P R) (impl Q R)))
(step-b :and-i 3)
(step-b :impl-i 3)
(step-f :or-i1 2)
(swap '?1 'Q)
(step-f :impl-e 1 3)
(step-b :impl-i 7)
(step-f :or-i2 6)
(swap '?2 'P)
(step-f :impl-e 1 7)

; I.8
(proof '(not (and P (not Q))) '(impl P Q))
(step-f :not-and->or-not 1)
(step-f :or-e 2 4)
(step-b :impl-i 5)
(step-f :not-e 3 4)
(step-b :efq 7)
(step-b :impl-i 10)
(step-f :notnot-e 8)

; I.9
(proof '(impl P (and (impl Q R) (impl R Q))) '(impl (and P Q) R))
(step-b :impl-i 3)
(step-f :and-e1 2)
(step-f :and-e2 2)
(step-f :impl-e 1 3)
(step-f :and-e1 5)
(step-f :impl-e 6 4)

; I.10
(proof '(impl (not P) (impl P Q)))
(step-b :impl-i 2)
(step-b :impl-i 3)
(step-f :not-e 1 2)
(step-b :efq 5)

; -----------------------------------------------------------------------------------------
; p.109

; II.1
(proof '(or P P) 'P)
(step-f :or-e 1 3)

; II.2
(proof 'P '(impl (impl P Q) Q))
(step-b :impl-i 3)
(step-f :impl-e 2 1)

; II.3
(proof 'P '(impl (impl (not (impl Q R)) (not P)) (impl (not R) (not Q))))
(step-b :impl-i 3)
(step-b :impl-i 4)
(step-f :tnd)
(swap '?1 '(impl Q R))
(step-f :or-e 4 6)
(step-f :impl-e 2 8)
(step-f :not-e 9 1)
(step-b :efq 12)
(step-f :impl->or-not 5)
(step-f :or-e 6 8)
(step-f :not-e 3 9)
(step-b :efq 12)

; II.4
(proof '[(impl P (or Q R)) (impl R S)] '(impl P (or Q S)))
(step-b :impl-i 4)
(step-f :impl-e 1 3)
(step-f :or-e 4 6)
(step-b :or-i1 7)
(step-f :impl-e 2 7)
(step-b :or-i2 10)

; II.5
(proof '[(impl (not Q) (not R)) (or R S) (impl S Q)] '(or Q P))
(step-f :or-e 2 5)
(step-f :impl-e 3 7)
(step-b :or-i1 10)
(step-f :impl->or-not 1)
(step-f :or-e 5 7)
(step-f :notnot-e 6)
(step-b :or-i1 9)
(step-f :not-e 9 4)
(step-b :efq 12)

; II.6
(proof '(not (or P Q)) '(and (not P) (not Q)))
(step-f :not-or->and-not 1)

(proof '(not (or P Q)) '(and (not P) (not Q)))
(step-b :and-i 3)
(step-b :not-i 3)
(step-f :or-i1 2)
(swap '?1 'Q)
(step-f :not-e 1 3)
(step-b :not-i 7)
(step-f :or-i2 6)
(swap '?2 'P)
(step-f :not-e 1 7)

; II.7
(proof '(not (not (or P (not Q)))) '(or (impl P (not Q)) (impl (not Q) P)))
(step-f :notnot-e 1)
(step-f :or-e 2 4)
(step-b :or-i2 5)
(step-b :impl-i 5)
(step-b :or-i1 10)
(step-b :impl-i 10)

; II.8
(proof '(and (impl (or P Q) P) (impl P (or P Q))) '(impl Q P))
(step-f :and-e1 1)
(step-b :impl-i 4)
(step-f :impl-e 2)
(step-b :or-i2 5)

; II.9
(proof '(or (and P Q) (and P R)) '(and P (or Q R)))
(step-f :or-e 1 3)
(step-b :and-i 4)
(step-f :and-e1 2)
(step-f :and-e2 2)
(step-b :or-i1 6)
(step-f :and-e1 7)
(step-f :and-e2 7)
(step-b :and-i 11)
(step-b :or-i2 11)

; II.10
(proof '(or P (not P)))
(step-f :tnd)
(swap '?1 'P)

(proof '(or P (not P)))
(step-b :raa 2)
(step-b :not-e 3 1)
(step-b :or-i2 3)
(step-b :not-i 3)
(step-f :or-i1 2)
(swap '?1 '(not P))
(step-f :not-e 1 3)

; -----------------------------------------------------------------------------------------
; p.109

; III.1
(proof '(impl (impl (impl P P) Q) Q))
(step-b :impl-i 2)
(step-f :impl-e 1)
(step-b :impl-i 3)

; III.2
(proof '(not (impl P Q)) '(and P (not Q)))
(step-f :not-impl-e 1)

(proof '(not (impl P Q)) '(and P (not Q)))
(subclaim '(not (or (not P) Q)))
(step-b :not-i 3)
(step-f :or-e 2)
(swap '?1 '(impl P Q))
(step-b :impl-i 5)
(step-f :not-e 3 4)
(step-b :efq 7)
(step-b :impl-i 10)
(step-f :not-e 1 12)
(step-f :not-or-e1 14)
(step-f :notnot-e 15)
(step-f :not-or-e2 14)
(step-f :and-i 16 17)

; III.3
(proof '(and (or P Q) (or R S)) '(or (or (and P R) (and P S)) (or (and Q R) (and Q S))))
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-f :or-e 2 5)
(step-f :or-e 3 6)
(step-f :and-i 4 5)
(step-b :or-i1 8)
(step-b :or-i1 8)
(step-f :and-i 4 9)
(step-b :or-i1 12)
(step-b :or-i2 12)
(step-f :or-e 3 16)
(step-f :and-i 14 15)
(step-b :or-i2 18)
(step-b :or-i1 18)
(step-f :and-i 14 19)
(step-b :or-i2 22)
(step-b :or-i2 22)

; III.4
(proof '[(or P Q) (not Q)] 'P)
(step-f :or-e 1 4)
(step-f :not-e 2 5)
(step-b :efq 8)

; III.5
(proof '[(or P Q) (not P)] 'Q)
(step-f :or-e 1 4)
(step-f :not-e 2 3)
(step-b :efq 6)

; III.6
(proof '(impl (and (impl (not P) R) (impl (not Q) R)) (impl (not (and P Q)) R)))
(step-b :impl-i 2)
(step-b :impl-i 3)
(step-f :not-and->or-not 2)
(step-f :or-e 3 5)
(step-f :and-e1 1)
(step-f :impl-e 5 4)
(step-f :and-e2 1)
(step-f :impl-e 8 7)

; III.7
(proof '[(or P Q) (or P R)] '(or P (and Q R)))
(step-f :or-e 1 4)
(step-b :or-i1 5)
(step-f :or-e 2 7)
(step-b :or-i1 8)
(step-f :and-i 5 8)
(step-b :or-i2 11)

; III.8
(proof '[(impl P Q) (impl Q P) (impl Q R) (impl R Q)] '(and (impl P R) (impl R P)))
(step-b :and-i 6)
(step-b :impl-i 6)
(step-f :impl-e 1 5)
(step-f :impl-e 3 6)
(step-b :impl-i 10)
(step-f :impl-e 4 9)
(step-f :impl-e 2 10)

; III.9
(proof '(or P (impl P Q)))
(step-b :raa 2)
(step-f :not-or->and-not 1)
(step-f :and-e1 2)
(step-f :and-e2 2)
(step-f :not-impl-e 4)
(step-f :and-e1 5)
(step-f :not-e 3 6)

; III.10
(proof '(or (impl P Q) (impl Q R)))
(step-b :raa 2)
(step-f :not-or->and-not 1)
(step-f :and-e1 2)
(step-f :and-e2 2)
(step-f :not-impl-e 3)
(step-f :not-impl-e 4)
(step-f :and-e2 5)
(step-f :and-e1 6)
(step-f :not-e 7 8)

; -----------------------------------------------------------------------------------------
; p.110

; IV.1
(proof '(impl (or P Q) (or Q P)))
(step-b :impl-i 2)
(step-f :or-comm 1)

; IV.2
(proof '(impl (not (or P Q)) (not P)))
(step-b :impl-i 2)
(step-f :not-or->and-not 1)
(step-f :and-e1 2)

; IV.3
(proof '[(not (and P Q)) P] '(not Q))
(step-f :not-and->or-not 1)
(step-f :or-e 3 5)
(step-f :not-e 4 2)
(step-b :efq 7)

; IV.4
(proof '(not (and P Q)) '(or (not P) (not Q)))
(step-f :not-and->or-not 1)

; IV.5
(proof '(or P (or Q R)) '(or (or P Q) R))
(step-f :or-assocl 1)

; IV.6 
(proof '[(not P) (not Q)] '(not (or P Q)))
(step-f :and-i 1 2)
(step-f :and-not->not-or 3)

; IV.7
(proof '(impl P (or Q R)) '(or (impl P Q) (impl P R)))
(step-f :impl->or-not 1)
(step-f :or-e 2 4)
(step-f :or-i1 3)
(swap '?1 'Q)
(step-f :or-not->impl 4)
(step-b :or-i1 7)
(step-f :or-e 7 9)
(step-f :or-i2 8)
(swap '?2 '(not P))
(step-f :or-not->impl 9)
(step-b :or-i1 12)
(step-f :or-i2 12)
(swap '?3 '(not P))
(step-f :or-not->impl 13)
(step-b :or-i2 16)

; IV.8
(proof '[(impl P (not Q)) (impl P (not R)) (or Q R)] '(impl P (or (not Q) R)))
(step-b :impl-i 5)
(step-f :impl-e 1 4)
(step-b :or-i1 7)

; IV.9
(proof '[ (or P (not Q)) (or P (not R)) (or Q R)] 'P)
(step-f :or-e 1 5)
(step-f :or-e 2 8)
(step-f :or-e 3 11)
(step-f :not-e 6 10)
(step-b :efq 13)
(step-f :not-e 9 13)
(step-b :efq 16)

; IV.10
(proof '(impl (and P Q) R) '(or (impl P R) (impl Q R)))
(step-f :impl->or-not 1)
(step-f :or-e 2 4)
(step-f :not-and->or-not 3)
(step-f :or-e 4 6)
(step-f :or-i1 5)
(swap '?1 'R)
(step-f :or-not->impl 6)
(step-b :or-i1 9)
(step-f :or-i1 9)
(swap '?2 'R)
(step-f :or-not->impl 10)
(step-b :or-i2 13)
(step-f :or-i2 14)
(swap '?3 '(not P))
(step-f :or-not->impl 15)
(step-b :or-i1 18)
