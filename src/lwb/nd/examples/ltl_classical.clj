; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.examples.ltl-classical
  (:require [lwb.nd.repl :refer :all]))

(load-logic :ltl)

; interactive checking in the repl for nd

; -----------------------------------------------------------------------------------------
; Derived rules

; -----------------------------------------------------------------------------------------
; notnot-i

(proof '(at [i] P) '(at [i] (not (not P))))
(step-b :not-i 3)
(swap '?1 'i)
(step-f :not-e 2 1)
(swap '?2 'i)

;(export "resources/nd/theorems-ltl.edn" :notnot-i) 
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always-notnot-i

(proof '(at [i] (always P)) '(at [i] (always (not (not P)))))
(step-b :always-i 3)
(swap '?1 'j)
(step-f :always-e 1 2)
(step-f :notnot-i 3)

;(export "resources/nd/theorems-ltl.edn" :always-notnot-i)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; notnot-e

(proof '(at [i] (not (not P))) '(at [i] P))
(step-b :raa 3)
(swap '?1 'i)
(step-f :not-e 1 2)
(swap '?2 'i)

;(export "resources/nd/theorems-ltl.edn" :notnot-e)
;(load-logic :ltl)
 
; -----------------------------------------------------------------------------------------
; always-notnot-e

(proof '(at [i] (always (not (not A)))) '(at [i] (always A)))
(step-b :always-i 3)
(swap '?1 'j)
(step-f :always-e 1 2)
(step-f :notnot-e 3)

;(export "resources/nd/theorems-ltl.edn" :always-notnot-e)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; Modus Tollens

(proof '[(at [i] (impl P Q)) (at [i] (not Q))] '(at [i] (not P)))
(step-b :not-i 4)
(swap '?1 'i)
(step-f :impl-e 1 3)
(step-f :not-e 2 4)
(swap '?2 'i)

;(export "resources/nd/theorems-ltl.edn" :mt)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; tnd

(proof '(at [i] (or P (not P))))
(step-b :raa 2)
(swap '?1 'i)
(step-b :not-e 3 1)
(step-b :or-i2 3)
(step-b :not-i 3)
(swap '?2 'i)
(step-b :not-e 4 1)
(step-b :or-i1 4)

;(export "resources/nd/theorems-ltl.edn" :tnd)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always-tnd

(proof '(at [i] (always (or P (not P)))))
(step-b :always-i 2)
(swap '?1 'j)
(step-f :tnd)
(swap '?2 'j)
(swap '?3 'P)

;(export "resources/nd/theorems-ltl.edn" :always-tnd)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; contrap

(proof '(at [i] (impl A B)) '(at [i] (impl (not B) (not A))))
(step-b :impl-i 3)
(step-b :not-i 4)
(swap '?1 'i)
(step-f :impl-e 1 3)
(step-b :not-e 6 2)

;(export "resources/nd/theorems-ltl.edn" :contrap)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; De Morgan

(proof '(at [i] (not (and P Q))) '(at [i] (or (not P) (not Q))))
(step-b :raa 3)
(swap '?1 'i)
(step-b :not-e 4)
(swap '?2 'i)
(swap '?3 '(and P Q))
(step-b :and-i 4)
(step-b :raa 6)
(swap '?4 'i)
(step-b :not-e 7)
(swap '?5 'i)
(swap '?6 '(or (not P) (not Q)))
(step-b :or-i2 7)
(step-b :raa 4)
(swap '?7 'i)
(step-b :not-e 5)
(swap '?8 'i)
(swap '?9 '(or (not P) (not Q)))
(step-b :or-i1 5)

;(export "resources/nd/theorems-ltl.edn" :not-and->or-not)
;(load-logic :ltl)

(proof '(at [i] (or (not P) (not Q))) '(at [i] (not (and P Q))))
(step-b :or-e 3 1)
(step-b :not-i 4)
(swap '?1 'i)
(step-f :and-e1 3)
(step-f :not-e 2 4)
(swap '?2 'i)
(step-b :not-i 9)
(swap '?3 'i)
(step-f :and-e2 8)
(step-f :not-e 7 9)
(swap '?4 'i)

;(export "resources/nd/theorems-ltl.edn" :or-not->not-and)
;(load-logic :ltl)

(proof '(at [i] (not (or P Q))) '(at [i] (and (not P) (not Q))))
(step-b :and-i 3)
(step-b :not-i 3)
(swap '?1 'i)
(step-f :or-i1 2)
(swap '?2 'Q)
(step-f :not-e 1 3)
(swap '?3 'i)
(step-b :not-i 7)
(swap '?4 'i)
(step-f :or-i2 6)
(swap '?5 'P)
(step-f :not-e 1 7)
(swap '?6 'i)

;(export "resources/nd/theorems-ltl.edn" :not-or->and-not)
;(load-logic :ltl)

(proof '(at [i] (and (not P) (not Q))) '(at [i] (not (or P Q))))
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-b :not-i 5)
(swap '?1 'i)
(step-f :or-e 4 6)
(step-f :not-e 2 5)
(swap '?2 'i)
(step-f :not-e 3 7)
(swap '?3 'i)

;(export "resources/nd/theorems-ltl.edn" :and-not->not-or)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; not-atnext->atnext-not  Kröger/Merz T1

(proof '(at [i] (not (atnext A))) '(at [i] (atnext (not A))))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'i')
(step-b :atnext-i 4 :? 2)
(step-b :not-i 4)
(swap '?3 'i')
(step-f :atnext-i 3 2)
(step-f :not-e 1 4)
(swap '?4 'i')

;(export "resources/nd/theorems-ltl.edn" :not-atnext->atnext-not)
;(load-logic :ltl)

; Corollary
(proof '(at [i] (atnext A)) '(at [i] (not (atnext (not A)))))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'i')
(step-f :atnext-e 1 2)
(step-f :notnot-i 3)
(step-f :atnext-i 4 2)
(step-b :atnext-not->not-atnext 7)

; -----------------------------------------------------------------------------------------
; atnext-not->not-atnext  Kröger/Merz T1

(proof '(at [i] (atnext (not A))) '(at [i] (not (atnext A))))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'i')
(step-f :atnext-e 1 2)
(step-b :not-i 5)
(swap '?3 'i')
(step-f :atnext-e 4 2)
(step-f :not-e 3 5)
(swap '?4 'i')

;(export "resources/nd/theorems-ltl.edn" :atnext-not->not-atnext)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; not-finally->always-not  Kröger/Merz T3

(proof '(at [i] (not (finally A))) '(at [i] (always (not A))))
(step-b :always-i 3)
(swap '?1 'j)   ; an arbitrary point in time after i
(step-b :not-i 4)
(step-f :finally-i 3 2)
(swap '?2 'i)
(step-b :not-e 6 1)

;(export "resources/nd/theorems-ltl.edn" :not-finally->always-not)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always-not->not-finally  Kröger/Merz T3

(proof '(at [i] (always (not A))) '(at [i] (not (finally A))))
(step-b :not-i 3)
(swap '?1 'j)
(step-f :finally-e 2 4)
(swap '?2 'j)
(step-f :always-e 1 3)
(step-f :not-e 5 4)
(swap '?3 'j)

;(export "resources/nd/theorems-ltl.edn" :always-not->not-finally)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; finally-not->not-always  Kröger/Merz T2

(proof '(at [i] (finally (not A))) '(at [i] (not (always A))))
(step-b :not-i 3)
(step-f :finally-e 1 4)
(swap '?2 'j)   ; this is the point in time at which (not A) is finally true
(swap '?1 'j)
(step-f :always-e 2 3)
(step-b :not-e 7 4)

;(export "resources/nd/theorems-ltl.edn" :finally-not->not-always)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; not-always->finally-not  Kröger/Merz T2

(proof '(at [i] (not (always A))) '(at [i] (finally (not A))))
(step-b :raa 3)
(swap '?1 'i)
(step-f :not-finally->always-not 2)  ; here we need not-finally->always-not
(step-f :always-notnot-e 3)
(step-f :not-e 1 4)
(swap '?2 'i)

;(export "resources/nd/theorems-ltl.edn" :not-always->finally-not)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always->current  Kröger/Merz T4

(proof '(at [i] (always A)) '(at [i] A))
(step-f :<=refl)
(swap '?1 'i)
(step-f :always-e 1 2)

;(export "resources/nd/theorems-ltl.edn" :always->current)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; current->finally  Kröger/Merz T5

(proof '(at [i] A) '(at [i] (finally A)))
(step-f :<=refl)
(swap '?1 'i)
(step-b :finally-i 4 :? 2)

;(export "resources/nd/theorems-ltl.edn" :current->finally)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always->atnext  Kröger/Merz T6

(proof '(at [i] (always A)) '(at [i] (atnext A)))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'i')
(step-f :succ/<= 2)
(step-f :always-e 1 3)
(step-b :atnext-i 6 4)

;(export "resources/nd/theorems-ltl.edn" :always->atnext)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; atnext->finally  Kröger/Merz T7

(proof '(at [i] (atnext A)) '(at [i] (finally A)))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'i')
(step-f :atnext-e 1 2)
(step-f :succ/<= 2)
(step-f :finally-i 3 4)

;(export "resources/nd/theorems-ltl.edn" :atnext->finally)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always->finally  Kröger/Merz T8

(proof '(at [i] (always A)) '(at [i] (finally A)))
(step-f :<=refl)
(swap '?1 'i)
(step-f :always-e 1 2)
(step-b :finally-i 5 3)

;(export "resources/nd/theorems-ltl.edn" :always->finally)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; finally-always->always-finally  Kröger/Merz T9

(proof '(at[i] (finally (always A))) '(at [i] (always (finally A))))
(step-b :always-i 3)
(swap '?1 'j)   ; an arbitrary j
(step-f :finally-e 1 4)
(swap '?2 'k)   ; the point in time at which (always A) is finally true
(step-f :<=linear 2 3)
(step-f :rel-cases 5 7)
(step-f :always->current 4)
(step-b :finally-i 9 7)
(step-f :=/<= 9)
(step-f :always->current 4)
(step-b :finally-i 13 11)
(step-f :always-e 4 13)
(step-f :<=refl)
(swap '?3 'j)
(step-b :finally-i 17 14)

;(export "resources/nd/theorems-ltl.edn" :finally-always->always-finally)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always-always->always  Kröger/Merz T10

(proof '(at [i] (always (always A))) '(at [i] (always A)))
(step-b :always-i 3)
(swap '?1 'j)
(step-f :always-e 1 2)
(step-f :<=refl)
(swap '?2 'j)
(step-f :always-e 3 4)

;(export "resources/nd/theorems-ltl.edn" :always-always->always)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always->always-always  Kröger/Merz T10

(proof '(at [i] (always A)) '(at [i] (always (always A))))
(step-b :always-i 3)
(swap '?1 'j)
(step-b :always-i 4)
(swap '?2 'k)
(step-f :<=trans 2 3)
(step-f :always-e 1 4)

;(export "resources/nd/theorems-ltl.edn" :always->always-always)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; finally-finally->finally  Kröger/Merz T11

(proof '(at [i] (finally (finally A))) '(at [i] (finally A)))
(step-f :finally-e 1 3)
(swap '?1 'j)
(step-f :finally-e 3 5)
(swap '?2 'k)
(step-f :<=trans 2 4)
(step-f :finally-i 5 6)

;(export "resources/nd/theorems-ltl.edn" :finally-finally->finally)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; finally->finally-finally  Kröger/Merz T11

(proof '(at [i] (finally A)) '(at [i] (finally (finally A))))
(step-f :<=refl)
(swap '?1 'i)
(step-b :finally-i 4 :? 2)

;(export "resources/nd/theorems-ltl.edn" :finally->finally-finally)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always-serial  Kröger/Merz T28

(proof '(at [i] (always A)) '(at [i] (and A (atnext (always A)))))
(step-f :always->current 1)
(step-f :succ)
(swap '?1 'i)
(swap '?2 'i')
(step-b :and-i 5 2)
(step-b :atnext-i 5 :? 3)
(step-b :always-i 5)
(swap '?3 'j)
(step-f :succ/<= 3)
(step-f :<=trans 5 4)
(step-f :always-e 1 6)

;(export "resources/nd/theorems-ltl.edn" :always-serial)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always-inductive  Kröger/Merz T28  Ben-Ari Axiom 4

(proof '(at [i] (always (impl (not A) (not A)))))
(step-f :always-tnd)
(swap '?1 'i)
(swap '?2 'A)
(step-b :always-i 3)
(swap '?3 'j)
(step-f :always-e 1 2)
(step-b :impl-i 5)
(load-theorem :trivial1)

(proof '(at [i] (and A (always (impl A (atnext A))))) '(at [i] (always A)))
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-b :raa 5)
(swap '?1 'i)
(step-f :not-always->finally-not 4)
(step-f :until-i 5)
(subclaim '(at [i] (impl (until truth (not A)) (not A))))
(step-b :until-ind 8)
(step-f :trivial1)
(swap '?2 'i)
(swap '?3 'A)
(step-b :always-i 9)
(swap '?4 'j)
(step-f :always-e 3 8)
(step-b :impl-i 11)
(step-f :and-e2 10)
(step-f :contrap 9)
(step-f :atnext-not->not-atnext 11)
(step-f :impl-e 12 13)
(step-f :impl-e 17 6)
(step-f :not-e 18 2)
(swap '?5 'i)

;(export "resources/nd/theorems-ltl.edn" :always-inductive)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always-atnext->atnext-always  Kröger/Merz T12

(proof '(at [i] (always (atnext A))) '(at [i] (atnext (always A))))
(step-f :always->current 1)
(step-f :succ)
(swap '?1 'i)
(swap '?2 'i')
(step-f :atnext-e 2 3)
(step-b :atnext-i 6 :? 3)
(step-b :always-i 4)
(step-b :always-inductive 6)
(step-b :and-i 6 4)
(step-b :always-i 6)
(swap '?3 'j)
(step-b :impl-i 7)
(step-f :succ/<= 3)
(step-f :<=trans 7 5)
(step-f :always-e 1 8)

;(export "resources/nd/theorems-ltl.edn" :always-atnext->atnext-always)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; atnext-always->always-atnext  Kröger/Merz T12

(proof '(at [i] (atnext (always A))) '(at [i] (always (atnext A))))
(step-b :always-i 3)
(swap '?1 'j)
(step-f :succ)
(swap '?2 'j)
(swap '?3 'j')
(step-f :succ)
(swap '?4 'i)
(swap '?5 'i')
(step-b :atnext-i 6 :? 3)
(step-f :<=succsucc/<= 2 4 3)
(step-f :atnext-e 1 4)
(step-f :always-e 6 5)

;(export "resources/nd/theorems-ltl.edn" :atnext-always->always-atnext)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; finally-atnext->atnext-finally  Kröger/Merz T13

(proof '(at [i] (finally (atnext A))) '(at [i] (atnext (finally A))))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'i')
(step-b :atnext-i 4 :? 2)
(step-f :finally-e 1 4)
(swap '?3 'j)
(step-f :succ)
(swap '?4 'j)
(swap '?5 'j')
(step-f :atnext-e 4 5)
(step-f :<=succsucc/<= 3 2 5)
(step-f :finally-i 6 7)

;(export "resources/nd/theorems-ltl.edn" :finally-atnext->atnext-finally)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; atnext-finally->finally-atnext  Kröger/Merz T13

(proof '(at [i] (atnext (finally A))) '(at [i] (finally (atnext A))))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'i')
(step-f :atnext-e 1 2)
(step-f :finally-e 3 5)
(swap '?3 'j')
(step-f :succ)
(swap '?4 'j)
(swap '?5 'j')  ; okay since j' is after i'
(step-f :atnext-i 5 6)
(step-f :succsucc<=/<= 2 6 4)
(step-f :finally-i 7 8)

;(export "resources/nd/theorems-ltl.edn" :atnext-finally->finally-atnext)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; atnext-impl-dist1  Kröger/Merz T14 Ben-Ari Axiom 2 

(proof '(at [i] (atnext (impl A B))) '(at [i] (impl (atnext A) (atnext B))))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'i')
(step-b :impl-i 4)
(step-f :atnext-e 1 2)
(step-b :atnext-i 6 :? 2)
(step-f :atnext-e 3 2)
(step-f :impl-e 4 5)

;(export "resources/nd/theorems-ltl.edn" :atnext-impl-dist1)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; atnext-impl-dist2  Kröger/Merz T14

(proof '(at [i] (impl (atnext A) (atnext B))) '(at [i] (atnext (impl A B))))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'i')
(step-b :atnext-i 4 :? 2)
(step-b :impl-i 4)
(step-f :atnext-i 3 2)
(step-b :atnext-e 6 :? 2)
(step-f :impl-e 1 4)

;(export "resources/nd/theorems-ltl.edn" :atnext-impl-dist2)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; atnext-and-dist1  Kröger/Merz T15

(proof '(at [i] (atnext (and A B))) '(at [i] (and (atnext A) (atnext B))))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'i')
(step-f :atnext-e 1 2)
(step-f :and-e1 3)
(step-f :and-e2 3)
(step-f :atnext-i 4 2)
(step-f :atnext-i 5 2)
(step-b :and-i 9)

;(export "resources/nd/theorems-ltl.edn" :atnext-and-dist1)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; atnext-and-dist2  Kröger/Merz T15

(proof '(at [i] (and (atnext A) (atnext B))) '(at [i] (atnext (and A B))))
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-f :succ)
(swap '?1 'i)
(swap '?2 'i')
(step-f :atnext-e 2 4)
(step-f :atnext-e 3 4)
(step-f :and-i 5 6)
(step-b :atnext-i 9 7)

;(export "resources/nd/theorems-ltl.edn" :atnext-and-dist2)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; atnext-or-dist1  Kröger/Merz T16

(proof '(at [i] (atnext (or A B))) '(at [i] (or (atnext A) (atnext B))))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'i')
(step-f :atnext-e 1 2)
(step-b :raa 5)
(step-f :not-or->and-not 4)
(step-f :and-e1 5)
(step-f :and-e2 5)
(step-f :not-atnext->atnext-not 6)
(step-f :not-atnext->atnext-not 7)
(step-f :atnext-e 8 2)
(step-f :atnext-e 9 2)
(step-f :and-i 10 11)
(step-f :and-not->not-or 12)
(swap '?3 'i')
(step-f :not-e 13 3)
(swap '?4 'i')

;(export "resources/nd/theorems-ltl.edn" :atnext-or-dist1)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; atnext-or-dist2  Kröger/Merz T16

(proof '(at [i] (or (atnext A) (atnext B))) '(at [i] (atnext (or A B))))
(step-f :or-e 1 3)
(step-f :succ)
(swap '?1 'i)
(swap '?2 'i')
(step-f :atnext-e 2 3)
(step-b :atnext-i 6 :? 3)
(step-b :or-i1 6)
(step-f :succ)
(swap '?3 'i)
(swap '?4 'i')
(step-f :atnext-e 7 8)
(step-b :atnext-i 11 :? 8)
(step-b :or-i2 11)

;(export "resources/nd/theorems-ltl.edn" :atnext-or-dist2)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; Kröger/Merz T17 uses biconditional <-> which we do not support in the rules

; -----------------------------------------------------------------------------------------
; always-and-dist1 Kröger/Merz T18

(proof '(at [i] (always (and A B))) '(at [i] (and (always A) (always B))))
(step-b :and-i 3)
(step-b :always-i 3)
(swap '?1 'j)
(step-f :always-e 1 2)
(step-f :and-e1 3)
(step-b :always-i 7)
(swap '?2 'j)
(step-f :always-e 1 6)
(step-f :and-e2 7)

;(export "resources/nd/theorems-ltl.edn" :always-and-dist1)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always-and-dist2 Kröger/Merz T18

(proof '(at [i] (and (always A) (always B))) '(at [i] (always (and A B))))
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-b :always-i 5)
(swap '?1 'j)
(step-f :always-e 2 4)
(step-f :always-e 3 4)
(step-b :and-i 8)

;(export "resources/nd/theorems-ltl.edn" :always-and-dist2)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; finally-or-dist1  Kröger/Merz T19

(proof '(at [i] (finally (or A B))) '(at [i] (or (finally A) (finally B))))
(step-f :finally-e 1 3)
(swap '?1 'j)
(step-b :raa 5)
(step-f :not-or->and-not 4)
(step-f :and-e1 5)
(step-f :and-e2 5)
(step-f :not-finally->always-not 6)
(step-f :not-finally->always-not 7)
(swap '?2 'j)
(step-f :or-e 3 11)
(step-f :always-e 8 2)
(step-f :not-e 11 10)
(swap '?3 'j)
(step-f :always-e 9 2)
(step-b :not-e 16 14)

;(export "resources/nd/theorems-ltl.edn" :finally-or-dist1)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; finally-or-dist2  Kröger/Merz T19

(proof '(at [i] (or (finally A) (finally B))) '(at [i] (finally (or A B))))
(step-f :or-e 1 3)
(step-f :finally-e 2 4)
(swap '?1 'j)
(step-f :or-i1 4)
(swap '?2 'B)
(step-b :finally-i 7 5)
(step-f :finally-e 8 10)
(swap '?3 'j)
(step-f :or-i2 10)
(swap '?4 'A)
(step-b :finally-i 13 11)

;(export "resources/nd/theorems-ltl.edn" :finally-or-dist2)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always-finally-or-dist1  Kröger/Merz T20

; TODO
(proof '(at [i] (always (finally (or A B)))) '(at [i] (or (always (finally A)) (always (finally B)))))

; (export "resources/nd/theorems-ltl.edn" :always-finally-or-dist1)
 
; -----------------------------------------------------------------------------------------
; always-finally-or-dist2  Kröger/Merz T20

(proof '(at [i] (or (always (finally A)) (always (finally B)))) '(at [i] (always (finally (or A B)))))
(step-f :or-e 1 3)
(step-b :always-i 4)
(swap '?1 'j)
(step-f :always-e 2 3)
(step-f :finally-e 4 6)
(swap '?2 'k)
(step-f :or-i1 6)
(swap '?3 'B)
(step-f :finally-i 7 5)
(step-b :always-i 13)
(swap '?4 'j)
(step-f :always-e 11 12)
(step-f :finally-e 13 15)
(swap '?5 'k)
(step-f :or-i2 15)
(swap '?6 'A)
(step-f :finally-i 16 14)

; (export "resources/nd/theorems-ltl.edn" :always-finally-or-dist2)

; -----------------------------------------------------------------------------------------
; finally-always-and-dist1  Kröger/Merz T21

(proof '(at [i] (finally (always (and A B)))) '(at [i] (and (finally (always A)) (finally (always B)))))
(step-f :finally-e 1 3)
(swap '?1 'j)
(step-f :always-and-dist1 3)
(step-f :and-e1 4)
(step-f :and-e2 4)
(step-b :and-i 8)
(step-f :always->always-always 5)
(step-f :always->current 7)
(step-f :finally-i 8 2)
(step-f :always->always-always 6)
(step-f :always->current 10)
(step-f :finally-i 11 2)

; (export "resources/nd/theorems-ltl.edn" :finally-always-and-dist1)

; -----------------------------------------------------------------------------------------
; finally-always-and-dist2  Kröger/Merz T21

(proof '(at [i] (and (finally (always A)) (finally (always B)))) '(at [i] (finally (always (and A B)))))
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-f :finally-e 2 5)
(swap '?1 'j)
(step-f :always->always-always 5)
(step-f :finally-e 3 8)
(swap '?2 'k)
(step-f :always->always-always 8)
(step-f :<=linear 4 7)
(step-f :rel-cases 10 12)
(step-f :always-e 6 11)
(step-f :and-i 12 8)
(step-f :always-and-dist2 13)
(step-f :<=trans 4 11)
(step-f :finally-i 14 15)
(step-f :=fml 17 5)
(step-f :and-i 18 8)
(step-f :always-and-dist2 19)
(step-f :=/<= 17)
(step-f :<=trans 4 21)
(step-f :finally-i 20 22)
(step-f :always-e 9 24)
(step-f :and-i 5 25)
(step-f :always-and-dist2 26)
(step-f :<=trans 7 24)
(step-f :finally-i 27 28)

; (export "resources/nd/theorems-ltl.edn" :finally-always-and-dist2)

; -----------------------------------------------------------------------------------------
; always-impl-dist Kröger/Merz T22  Ben-Ari Axiom 1

(proof '(at [i] (always (impl A B))) '(at [i] (impl (always A) (always B))))
(step-b :impl-i 3)
(step-b :always-i 4)
(swap '?1 'j)
(step-f :always-e 1 3)
(step-f :always-e 2 3)
(step-f :impl-e 4 5)

;(export "resources/nd/theorems-ltl.edn" :always-impl-dist)

; -----------------------------------------------------------------------------------------
; always-or-dist  Kröger/Merz T23

; TODO
(proof '(at [i] (or (always A) (always B))) '(at [i] (always (or A B))))

; (export "resources/nd/theorems-ltl.edn" :always-or-dist)

; -----------------------------------------------------------------------------------------
; finally-impl-dist  Kröger/Merz T24 

; TODO
(proof '(at [i] (finally A) (finally B)) '(at [i] (finally (impl A B))))

;(export "resources/nd/theorems-ltl.edn" :finally-impl-dist)

; -----------------------------------------------------------------------------------------
; finally-and-dist  Kröger/Merz T25

; TODO
(proof '(at [i] (finally (and A B))) '(at [i] (and (finally A) (finally B))))

; (export "resources/nd/theorems-ltl.edn" :finally-and-dist)

; -----------------------------------------------------------------------------------------
; always-finally-and-dist  Kröger/Merz T26

; TODO
(proof '(at [i] (always (finally (and A B)))) '(at [i] (and  (always (finally A)) (always (finally B)))))

; (export "resources/nd/theorems-ltl.edn" :always-finally-and-dist)

; -----------------------------------------------------------------------------------------
; finally-always-or-dist  Kröger/Merz T27

; TODO
(proof '(at [i] (or (finally (always A)) (finally (always B)))) '(at[i] (finally (always (or A B)))))

; (export "resources/nd/theorems-ltl.edn" :finally-always-or-dist)


; -----------------------------------------------------------------------------------------
; finally-serial  Kröger/Merz T29

; TODO
(proof '(at [i] (finally A)) '(at [i] (and A (atnext (finally A)))))

; (export "resources/nd/theorems-ltl.edn" :finally-serial)

; -----------------------------------------------------------------------------------------
; finally-inductive  Kröger/Merz T29

; TODO
(proof '(at [i] (and A (atnext (finally A)))) '(at[i] (finally A)))

; (export "resources/nd/theorems-ltl.edn" :finally-inductive)

; -----------------------------------------------------------------------------------------
; always-impl->impl-atnext  Kröger/Merz T30

; TODO
(proof '(at [i] (always (impl A B))) '(at [i] (impl (atnext A) (atnext B))))

; (export "resources/nd/theorems-ltl.edn" :always-impl->impl-atnext)

; -----------------------------------------------------------------------------------------
; always-impl->impl-finally  Kröger/Merz T31

; TODO
(proof '(at [i] (always (impl A B))) '(at [i] (impl (finally A) (finally B))))

; (export "resources/nd/theorems-ltl.edn" :always-impl->impl-finally)

; -----------------------------------------------------------------------------------------
; always-expansion  Ben-Ari Axiom 3

(proof '(at [i] (always A)) '(at [i] (and A (and (atnext A) (atnext (always A))))))
(step-f :always->current 1)
(step-f :always->atnext 1)
(step-f :succ)
(swap '?1 'i)
(swap '?2 'i')
(subclaim '(at [i] (atnext (always A))))
(step-f :and-i 3 6)
(step-f :and-i 2 7)
(step-b :atnext-i 6 :? 4)
(step-b :always-i 6)
(swap '?3 'j)
(step-f :succ/<= 4)
(step-f :<=trans 6 5)
(step-f :always-e 1 7)

; (export "resources/nd/theorems-ltl.edn" :always-expansion)

; -----------------------------------------------------------------------------------------
; linearity1  Ben-Ari Axiom 5

(proof '(at [i] (atnext A)) '(at [i] (not (atnext (not A)))))
(step-b :not-i 3)
(step-f :atnext-not->not-atnext 2)
(swap '?1 'i)
(step-f :not-e 3 1)
(swap '?2 'i)

; -----------------------------------------------------------------------------------------
; linearity2  Ben-Ari Axiom 5

(proof '(at [i] (not (atnext (not A)))) '(at [i] (atnext A)))
(step-f :not-atnext->atnext-not 1)
(step-f :succ)
(swap '?1 'i)
(swap '?2 'j)
(step-f :atnext-e 2 3)
(step-f :notnot-e 4)
(step-b :atnext-i 7 :? 3)

; -----------------------------------------------------------------------------------------
; until->finally  Ben-Ari Axiom 6

(proof '(at [i] (until A B)) '(at [i] (finally B)))
(step-f :until-e 1)

; (export "resources/nd/theorems-ltl.edn" :until->finally)

; -----------------------------------------------------------------------------------------
; until-expansion1  Ben-Ari Axiom 7

; TODO
(proof '(at [i] (until A B)) '(at [i] (or B (and A (atnext (until A B))))))

; (export "resources/nd/theorems-ltl.edn" :until-expansion1)

; -----------------------------------------------------------------------------------------
; until-expansion2  Ben-Ari Axiom 7

; TODO
(proof '(at [i] (or B (and A (atnext (until A B))))) '(at [i] (until A B)))
(step-b :raa 3)
(swap '?1 'i)
(step-f :not-until 2)
(step-f :or-e 3 5)
(step-f :or-e 1 6)
(step-f :<=refl)
(swap '?2 'i)
(step-f :always-e 4 6)
(step-b :not-e 9 7)
(step-f :and-e1 9)
(step-f :and-e2 9)
(step-f :succ)
(swap '?3 'i)
(swap '?4 'i')
(step-f :atnext-e 11 12)
(step-f :until-e 13)
(step-f :finally-e 14 16)
(swap '?5 'j)
(step-f :succ/<= 12)
(step-f :<=trans 17 15)
(step-f :always-e 4 18)
(step-f :not-e 19 16)
(swap '?6 'i)
; part 1
(step-f :until-e 23)
(step-f :finally-e 24 26)
(swap '?7 'j)
(step-f :and-e1 26)
(step-f :and-e2 26)
;; TODO: geht so nicht -- braucht wohl :until-ind??


; (export "resources/nd/theorems-ltl.edn" :until-expansion2)

; -----------------------------------------------------------------------------------------
