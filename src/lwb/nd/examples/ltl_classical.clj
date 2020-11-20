; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.examples.ltl-classical
  (:require [lwb.nd.repl :refer :all]))

(load-logic :ltl)

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
; impl

(proof '(at [i] (impl P Q)) '(at [i] (or (not P) Q)))
(step-b :raa 3)
(swap '?1 'i)
(step-f :not-or->and-not 2)
(step-f :and-e1 3)
(step-f :notnot-e 4)
(step-f :impl-e 1 5)
(step-f :and-e2 3)
(step-f :not-e 7 6)
(swap '?2 'i)

;(export "resources/nd/theorems-ltl.edn" :impl->or-not)
;(load-logic :ltl)

(proof '(at [i] (or (not P) Q)) '(at [i] (impl P Q)))
(step-f :or-e 1 3)
(step-b :impl-i 4)
(step-f :not-e 2 3)
(swap '?1 'i)
(step-b :efq 6)
(swap '?2 'i)
(step-b :impl-i 9)

;(export "resources/nd/theorems-ltl.edn" :or-not->impl)
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

; -----------------------------------------------------------------------------------------
; atnext-not->not-atnext  Kröger/Merz T1

(proof '(at [i] (atnext (not A))) '(at [i] (not (atnext A))))
(step-f :atnext-e 1 3)
(swap '?1 'i')
(step-b :not-i 5)
(swap '?2 'i')
(step-f :atnext-e' 4 2)
(step-f :not-e 3 5)
(swap '?3 'i')

;(export "resources/nd/theorems-ltl.edn" :atnext-not->not-atnext)
;(load-logic :ltl)

; or
(proof '(at [i] (atnext (not A))) '(at [i] (not (atnext A))))
(step-b :not-i 3)
(swap '?1 'i')
(step-f :atnext-e 2 4)
(swap '?2 'i')
(step-f :atnext-e' 1 3)
(step-b :not-e 7 5)

; Corollary
(proof '(at [i] (atnext A)) '(at [i] (not (atnext (not A)))))
(step-f :atnext-e 1 3)
(swap '?1 'i')
(step-f :notnot-i 3)
(step-f :atnext-i 4 2)
(step-b :atnext-not->not-atnext 7)

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

; geht so nicht??
(proof '(at [i] (not (always A))) '(at [i] (finally (not A))))
(step-b :raa 3)
(swap '?1 'i)
(step-f :not-finally->always-not 2)  ; here we need not-finally->always-not
(step-f :always-notnot-e 3)
(step-f :not-e 1 4)
(swap '?2 'i)

(proof '(at [i] (not (always A))) '(at [i] (finally (not A))))
(subclaim '(at [i] (always A)))
(step-b :always-i 3)
(swap '?1 'j)

(step-f :<=serial)
(swap '?1 'i)
(swap '?2 'j)
(step-b :finally-i 4 :? 2)
(step-b :not-i 4)
(subclaim '(at [i] (always A)))
(step-b :always-i 5)
(swap '?4 'k)
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
(step-f :atnext-e 1 3)
(swap '?1 'i')
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

(proof '(at [i] (and A (always (impl A (atnext A))))) '(at [i] (always A)))
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-b :induction 5)
(swap '?1 'j)
(swap '?2 'j')
(step-f :always-e 3 4)
(step-f :impl-e 7 6)
(step-f :atnext-e 8 10)
(swap '?3 'j')

;(export "resources/nd/theorems-ltl.edn" :always-inductive)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always-atnext->atnext-always  Kröger/Merz T12

(proof '(at [i] (always (atnext A))) '(at [i] (atnext (always A))))
(step-f :always->current 1)
(step-f :atnext-e 2 4)
(swap '?1 'i')
(step-b :atnext-i 6 :? 3)
(step-b :always-inductive 6)
(step-b :and-i 6 4)
(step-b :always-i 6)
(swap '?2 'j)
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
(step-b :atnext-i 5 :? 3)
(step-f :atnext-e 1 5)
(swap '?4 'i')
(step-f :<=succsucc/<= 2 4 3)
(step-f :always-e 5 6)

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
(step-f :atnext-e 4 6)
(swap '?4 'j')
(step-f :<=succsucc/<= 3 2 5)
(step-f :finally-i 6 7)

;(export "resources/nd/theorems-ltl.edn" :finally-atnext->atnext-finally)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; atnext-finally->finally-atnext  Kröger/Merz T13

(proof '(at [i] (atnext (finally A))) '(at [i] (finally (atnext A))))
(step-f :atnext-e 1 3)
(swap '?1 'i')
(step-f :finally-e 3 5)
(swap '?2 'j')
(step-f :succ)
(swap '?3 'j)
(swap '?4 'j' :checked)  ; okay since j' is after i'
(step-f :atnext-i 5 6)
(step-f :succsucc<=/<= 2 6 4)
(step-f :finally-i 7 8)

; or
(proof '(at [i] (atnext (finally A))) '(at [i] (finally (atnext A))))
(step-f :atnext-e 1 3)
(swap '?1 'i')
(step-f :finally-e 3 5)
(swap '?2 'j')
(step-b :finally-i 7)
(swap '?3 'j)
(step-b :atnext-i' 7)
(swap '?4 'j' :checked)
(step-f :succsucc<=/<= 2 6 4)

;(export "resources/nd/theorems-ltl.edn" :atnext-finally->finally-atnext)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; atnext-impl-dist1  Kröger/Merz T14 Ben-Ari Axiom 2 

(proof '(at [i] (atnext (impl A B))) '(at [i] (impl (atnext A) (atnext B))))
(step-b :impl-i 3)
(step-f :atnext-e 1 4)
(swap '?1 'i')
(step-f :atnext-e' 2 3)
(step-f :impl-e 4 5)
(step-b :atnext-i 8 6)

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
(step-f :impl-e 1 4)
(step-f :atnext-e' 5 2)

;(export "resources/nd/theorems-ltl.edn" :atnext-impl-dist2)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; atnext-and-dist1  Kröger/Merz T15

(proof '(at [i] (atnext (and A B))) '(at [i] (and (atnext A) (atnext B))))
(step-f :atnext-e 1 3)
(swap '?1 'i')
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
(step-f :atnext-e 2 4)
(swap '?1 'i')
(step-f :and-e2 1)
(step-f :atnext-e' 5 3)
(step-f :and-i 4 6)
(step-b :atnext-i 9 7)

;(export "resources/nd/theorems-ltl.edn" :atnext-and-dist2)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; atnext-or-dist1  Kröger/Merz T16

(proof '(at [i] (atnext (or A B))) '(at [i] (or (atnext A) (atnext B))))
(step-f :atnext-e 1 3)
(swap '?1 'i')
(step-b :raa 5)
(swap '?2 'i')
(step-f :not-or->and-not 4)
(step-f :and-e1 5)
(step-f :and-e2 5)
(step-f :not-atnext->atnext-not 6)
(step-f :not-atnext->atnext-not 7)
(step-f :atnext-e' 8 2)
(step-f :atnext-e' 9 2)
(step-f :and-i 10 11)
(step-f :and-not->not-or 12)
(step-f :not-e 13 3)
(swap '?3 'i')

;(export "resources/nd/theorems-ltl.edn" :atnext-or-dist1)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; atnext-or-dist2  Kröger/Merz T16

(proof '(at [i] (or (atnext A) (atnext B))) '(at [i] (atnext (or A B))))
(step-f :or-e 1 3)
(step-f :atnext-e 2 4)
(swap '?1 'i')
(step-b :atnext-i 6 :? 3)
(step-b :or-i1 6)
(step-f :atnext-e 8 10)
(swap '?2 'i')
(step-b :atnext-i 12 :? 9)
(step-b :or-i2 12)

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

;(export "resources/nd/theorems-ltl.edn" :finally-always-and-dist1)
;(load-logic :ltl)

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

;(export "resources/nd/theorems-ltl.edn" :finally-always-and-dist2)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always-finally-or-dist1  Kröger/Merz T20

(proof '(at [i] (finally (not (finally A)))) '(at [i] (finally (always (not A)))))
(step-f :finally-e 1 3)
(swap '?1 'j)
(step-f :not-finally->always-not 3)
(step-f :finally-i 4 2)

(load-theorem :finally-not-finally->finally-always-not)

(proof '(at [i] (always (and (not A) (not B)))) '(at [i] (always (not (or A B)))))
(step-b :always-i 3)
(swap '?1 'j)
(step-f :always-e 1 2)
(step-f :and-not->not-or 3)

(load-theorem :always-and-not->always-not-or)

(proof '(at [i] (always (finally (or A B)))) '(at [i] (or (always (finally A)) (always (finally B)))))
(step-b :raa 3)
(swap '?1 'k)
(step-f :not-or->and-not 2)
(step-f :and-e1 3)
(step-f :and-e2 3)
(step-f :not-always->finally-not 4)
(step-f :not-always->finally-not 5)
(step-f :finally-not-finally->finally-always-not 6)
(step-f :finally-not-finally->finally-always-not 7)
(step-f :and-i 8 9)
(step-f :finally-always-and-dist2 10)
(step-f :finally-e 11 13)
(swap '?2 'j)
(step-f :always-and-not->always-not-or 13)
(step-f :always-e 1 12)
(step-f :finally-e 15 17)
(swap '?3 'k)
(step-f :always-e 14 16)
(step-f :not-e 18 17)
(swap '?4 'k)

;(export "resources/nd/theorems-ltl.edn" :always-finally-or-dist1)
;(load-logic :ltl)

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

;(export "resources/nd/theorems-ltl.edn" :always-finally-or-dist2)
;(load-logic :ltl)

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
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always-or-dist  Kröger/Merz T23

(proof '(at [i] (or (always A) (always B))) '(at [i] (always (or A B))))
(step-f :or-e 1 3)
(step-b :always-i 4)
(swap '?1 'j)
(step-f :always-e 2 3)
(step-f :or-i1 4)
(swap '?2 'B)
(step-b :always-i 9)
(swap '?3 'j)
(step-f :always-e 7 8)
(step-b :or-i2 11)

;(export "resources/nd/theorems-ltl.edn" :always-or-dist)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; finally-impl-dist  Kröger/Merz T24 

(proof '(at [i] (impl (finally A) (finally B))) '(at [i] (finally (impl A B))))
(step-f :impl->or-not 1)
(step-f :or-e 2 4)
(step-f :not-finally->always-not 3)
(step-f :<=refl)
(swap '?1 'i)
(step-f :always-e 4 5)
(step-f :or-i1 6)
(swap '?2 'B)
(step-f :or-not->impl 7)
(step-f :finally-i 8 5)
(step-f :finally-e 10 12)
(swap '?3 'j)
(step-f :or-i2 12)
(swap '?4 '(not A))
(step-f :or-not->impl 13)
(step-f :finally-i 14 11)

;(export "resources/nd/theorems-ltl.edn" :finally-impl-dist)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; finally-and-dist  Kröger/Merz T25

(proof '(at [i] (finally (and A B))) '(at [i] (and (finally A) (finally B))))
(step-f :finally-e 1 3)
(swap '?1 'j)
(step-f :and-e1 3)
(step-f :and-e2 3)
(step-f :finally-i 4 2)
(step-f :finally-i 5 2)
(step-f :and-i 6 7)

;(export "resources/nd/theorems-ltl.edn" :finally-and-dist)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always-finally-and-dist  Kröger/Merz T26

(proof '(at [i] (always (finally (and A B)))) '(at [i] (and  (always (finally A)) (always (finally B)))))
(step-b :and-i 3)
(step-b :always-i 3)
(swap '?1 'j)
(step-f :always-e 1 2)
(step-f :finally-e 3 5)
(swap '?2 'k)
(step-f :and-e1 5)
(step-f :finally-i 6 4)
(step-b :always-i 11)
(swap '?3 'j)
(step-f :always-e 1 10)
(step-f :finally-e 11 13)
(swap '?4 'k)
(step-f :and-e2 13)
(step-f :finally-i 14 12)

;(export "resources/nd/theorems-ltl.edn" :always-finally-and-dist)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; finally-always-or-dist  Kröger/Merz T27

(proof '(at [i] (or (finally (always A)) (finally (always B)))) '(at[i] (finally (always (or A B)))))
(step-f :or-e 1 3)
(step-f :finally-e 2 4)
(swap '?1 'j)
(subclaim '(at [j] (always (or A B))))
(step-b :always-i 6)
(swap '?2 'k)
(step-f :always-e 4 5)
(step-b :or-i1 8)
(step-f :finally-i 8 3)
(step-f :finally-e 11 13)
(swap '?3 'j)
(subclaim '(at [j] (always (or A B))))
(step-b :always-i 15)
(swap '?4 'k)
(step-f :always-e 13 14)
(step-b :or-i2 17)
(step-f :finally-i 17 12)

;(export "resources/nd/theorems-ltl.edn" :finally-always-or-dist)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; finally-serial  Kröger/Merz T29

(proof '(at [i] (finally A)) '(at [i] (or A (atnext (finally A)))))
(step-f :finally-e 1 3)
(swap '?1 'j)
(step-f :succ)
(swap '?2 'i)
(swap '?3 'i')
(step-f :succ/<=linear 4 2)
(step-f :rel-or 5 7)
(step-f :=sym 6)
(step-f :=fml 7 3)
(step-b :or-i1 10)
(step-b :or-i2 12)
(step-b :atnext-i 12 :? 4)
(step-f :finally-i 3 10)

;(export "resources/nd/theorems-ltl.edn" :finally-serial)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; finally-inductive  Kröger/Merz T29

(proof '(at [i] (or A (atnext (finally A)))) '(at[i] (finally A)))
(step-f :or-e 1 3)
(step-f :<=refl)
(swap '?1 'i)
(step-f :finally-i 2 3)
(step-f :atnext-e 5 7)
(swap '?2 'i')
(step-f :succ/<= 6)
(step-f :finally-e 7 10)
(swap '?3 'j)
(step-f :<=trans 8 9)
(step-f :finally-i 10 11)

;(export "resources/nd/theorems-ltl.edn" :finally-inductive)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always-impl->impl-atnext  Kröger/Merz T30

(proof '(at [i] (always (impl A B))) '(at [i] (impl (atnext A) (atnext B))))
(step-b :impl-i 3)
(step-f :atnext-e 2 4)
(swap '?1 'i')
(step-f :succ/<= 3)
(step-f :always-e 1 5)
(step-f :impl-e 6 4)
(step-f :atnext-i 7 3)

;(export "resources/nd/theorems-ltl.edn" :always-impl->impl-atnext)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; always-impl->impl-finally  Kröger/Merz T31

(proof '(at [i] (always (impl A B))) '(at [i] (impl (finally A) (finally B))))
(step-b :impl-i 3)
(step-f :finally-e 2 4)
(swap '?1 'j)
(step-f :always-e 1 3)
(step-f :impl-e 5 4)
(step-f :finally-i 6 3)

;(export "resources/nd/theorems-ltl.edn" :always-impl->impl-finally)
;(load-logic :ltl)

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

;(export "resources/nd/theorems-ltl.edn" :always-expansion)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; linearity1  Ben-Ari Axiom 5

(proof '(at [i] (atnext A)) '(at [i] (not (atnext (not A)))))
(step-b :not-i 3)
(step-f :atnext-not->not-atnext 2)
(swap '?1 'i)
(step-f :not-e 3 1)
(swap '?2 'i)

;(export "resources/nd/theorems-ltl.edn" :linearity1)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; linearity2  Ben-Ari Axiom 5

(proof '(at [i] (not (atnext (not A)))) '(at [i] (atnext A)))
(step-f :not-atnext->atnext-not 1)
(step-f :atnext-e 2 4)
(swap '?1 'i')
(step-f :notnot-e 4)
(step-b :atnext-i 7 :? 3)

;(export "resources/nd/theorems-ltl.edn" :linearity2)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; until->finally  Ben-Ari Axiom 7

(proof '(at [i] (until A B)) '(at [i] (finally B)))
(step-f :until-e 1 3)
(swap '?1 'j)
(step-f :<=refl)
(swap '?2 'j)
(step-f :finally-i 3 4)
(step-f :and-e2 7)
(step-f :atnext-e 8 10)
(swap '?3 'j')
(step-f :finally-e 10 12)
(swap '?4 'k)
(step-f :succ/<= 9)
(step-f :<=trans 13 11)
(step-f :finally-i 12 14)

;(export "resources/nd/theorems-ltl.edn" :until->finally)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; until-expansion1  Ben-Ari Axiom 6

(proof '(at [i] (or B (and A (atnext (until A B))))) '(at [i] (until A B)))
(step-f :or-e 1 3)
(step-f :until-i1 2)
(swap '?1 'A)
(step-f :and-e1 4)
(step-f :and-e2 4)
(step-f :until-i2 5 6)

;(export "resources/nd/theorems-ltl.edn" :until-expansion1)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------
; until-expansion2  Ben-Ari Axiom 6

(proof '(at [i] (until A B)) '(at [i] (or B (and A (atnext (until A B))))))
(step-f :until-e 1 3)
(swap '?1 'j)
(step-b :or-i1 5)
(step-f :and-e2 6)
(step-f :and-e1 6)
(step-f :atnext-e 7 10)
(swap '?2 'j')
(step-f :until-expansion1 10)
(step-f :atnext-i 11 9)
(step-f :and-i 8 12)
(step-b :or-i2 15)

;(export "resources/nd/theorems-ltl.edn" :until-expansion2)
;(load-logic :ltl)

; -----------------------------------------------------------------------------------------

; Kröger/Merz T32
(proof '(at [i] (always A)) '(at [i] (impl (atnext B) (atnext (and A B)))))
(step-b :impl-i 3)
(step-f :atnext-e 2 4)
(swap '?1 'i')
(step-f :succ/<= 3)
(step-f :always-e 1 5)
(step-f :and-i 6 4)
(step-f :atnext-i 7 3)

; Kröger/Merz T33
(proof '(at [i] (always A)) '(at [i] (impl (always B) (always (and A B)))))
(step-b :impl-i 3)
(step-b :always-i 4)
(swap '?1 'j)
(step-f :always-e 1 3)
(step-f :always-e 2 3)
(step-f :and-i 4 5)
 
; Kröger/Merz T34
(proof '(at [i] (always A)) '(at [i] (impl (finally B) (finally (and A B)))))
(step-b :impl-i 3)
(step-f :finally-e 2 4)
(swap '?1 'j)
(step-f :always-e 1 3)
(step-f :and-i 5 4)
(step-f :finally-i 6 3)

; Kröger/Merz T35
(proof '(at [i] (always (impl (always A) B))) '(at [i] (impl (always A) (always B))))
(step-b :impl-i 3)
(step-b :always-i 4)
(swap '?1 'j)
(step-f :always->always-always 2)
(step-f :always-e 4 3)
(step-f :always-e 1 3)
(step-f :impl-e 6 5)

; Kröger/Merz T36
(proof '(at [i] (always (impl A (finally B)))) '(at [i] (impl (finally A) (finally B))))
(step-b :impl-i 3)
(step-f :finally-e 2 4)
(swap '?1 'j)
(step-f :always-e 1 3)
(step-f :impl-e 5 4)
(step-f :finally-e 6 8)
(swap '?2 'k)
(step-f :<=trans 3 7)
(step-f :finally-i 8 9)

; Kröger/Merz T37 ->
(proof '(at [i] (finally (always (finally A)))) '(at [i] (always (finally A))))
(step-f :finally-always->always-finally 1)
(step-b :always-i 4)
(swap '?1 'j)
(step-f :always-e 2 3)
(step-f :finally-finally->finally 4)

; Kröger/Merz T37 <-
(proof '(at [i] (always (finally A))) '(at [i] (finally (always (finally A)))))
(step-f :<=serial)
(swap '?1 'i)
(swap '?2 'j)
(step-b :finally-i 4 :? 2)
(step-b :always-i 4)
(swap '?3 'k)
(step-f :<=trans 2 3)
(step-f :always-e 1 4)

; Kröger/Merz T38 ->
(proof '(at [i] (always (finally (always A)))) '(at [i] (finally (always A))))
(step-f :<=serial)
(swap '?1 'i)
(swap '?2 'j)
(step-f :always-e 1 2)
(step-f :finally-e 3 5)
(swap '?3 'k)
(step-f :<=trans 2 4)
(step-f :finally-i 5 6)

; Kröger/Merz T38 <-
(proof '(at [i] (finally (always A))) '(at [i] (always (finally (always A)))))
(step-b :finally-always->always-finally 3)
(step-f :finally-e 1 3)
(swap '?1 'j)
(step-f :always->always-always 3)
(step-f :finally-i 4 2)
