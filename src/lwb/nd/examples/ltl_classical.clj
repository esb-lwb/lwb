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

; -----------------------------------------------------------------------------------------
; always-notnot-i

(proof '(at [i] (always P)) '(at [i] (always (not (not P)))))
(step-b :always-i 3)
(swap '?1 'j)
(step-f :always-e 1 2)
(step-f :notnot-i 3)

;(export "resources/nd/theorems-ltl.edn" :always-notnot-i)

; -----------------------------------------------------------------------------------------
; notnot-e

(proof '(at [i] (not (not P))) '(at [i] P))
(step-b :raa 3)
(swap '?1 'i)
(step-f :not-e 1 2)
(swap '?2 'i)

;(export "resources/nd/theorems-ltl.edn" :notnot-e)
 
; -----------------------------------------------------------------------------------------
; always-notnot-e

(proof '(at [i] (always (not (not A)))) '(at [i] (always A)))
(step-b :always-i 3)
(swap '?1 'j)
(step-f :always-e 1 2)
(step-f :notnot-e 3)

;(export "resources/nd/theorems-ltl.edn" :always-notnot-e)

; -----------------------------------------------------------------------------------------
; Modus Tollens

(proof '[(at [i] (impl P Q)) (at [i] (not Q))] '(at [i] (not P)))
(step-b :not-i 4)
(swap '?1 'i)
(step-f :impl-e 1 3)
(step-f :not-e 2 4)
(swap '?2 'i)

;(export "resources/nd/theorems-ltl.edn" :mt)

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

; -----------------------------------------------------------------------------------------
; always-tnd

(proof '(at [i] (always (or P (not P)))))
(step-b :always-i 2)
(swap '?1 'j)
(step-f :tnd)
(swap '?2 'j)
(swap '?3 'P)

;(export "resources/nd/theorems-ltl.edn" :always-tnd)

; -----------------------------------------------------------------------------------------
; contrap

(proof '(at [i] (impl A B)) '(at [i] (impl (not B) (not A))))
(step-b :impl-i 3)
(step-b :not-i 4)
(swap '?1 'i)
(step-f :impl-e 1 3)
(step-b :not-e 6 2)

;(export "resources/nd/theorems-ltl.edn" :contrap)

; -----------------------------------------------------------------------------------------
; not-atnext->atnext-not  Kröger/Merz T1

(proof '(at [i] (not (atnext A))) '(at [i] (atnext (not A))))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'j)
(step-b :atnext-i 4 :? 2)
(step-b :not-i 4)
(swap '?3 'j)
(step-f :atnext-i 3 2)
(step-f :not-e 1 4)
(swap '?4 'j)

;(export "resources/nd/theorems-ltl.edn" :not-atnext->atnext-not)

; Corollary
(proof '(at [i] (atnext A)) '(at [i] (not (atnext (not A)))))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'j)
(step-f :atnext-e 1 2)
(step-f :notnot-i 3)
(step-f :atnext-i 4 2)
(step-b :atnext-not->not-atnext 7)

; -----------------------------------------------------------------------------------------
; atnext-not->not-atnext  Kröger/Merz T1

(proof '(at [i] (atnext (not A))) '(at [i] (not (atnext A))))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'j)
(step-f :atnext-e 1 2)
(step-b :not-i 5)
(swap '?3 'j)
(step-f :atnext-e 4 2)
(step-f :not-e 3 5)
(swap '?4 'j)

;(export "resources/nd/theorems-ltl.edn" :atnext-not->not-atnext)

; -----------------------------------------------------------------------------------------
; finally-not->not-always  Kröger/Merz T2

(proof '(at [i] (finally (not A))) '(at [i] (not (always A))))
(step-b :not-i 3)
(swap '?1 'j)
(step-f :finally-e 1)
(swap '?2 'j)
(swap '?3 '(at [j] contradiction))
(step-f :always-e 2 3)
(step-b :not-e 7 4)

;(export "resources/nd/theorems-ltl.edn" :finally-not->not-always)

; -----------------------------------------------------------------------------------------
; not-always->finally-not  Kröger/Merz T2

(proof '(at [i] (not (always A))) '(at [i] (finally (not A))))
(step-b :raa 3)
(swap '?1 'i)
(step-f :not-finally->always-not 2)
(step-f :always-notnot-e 3)
(step-f :not-e 1 4)
(swap '?2 'i)

;(export "resources/nd/theorems-ltl.edn" :not-always->finally-not)

; -----------------------------------------------------------------------------------------
; not-finally->always-not  Kröger/Merz T3

(proof '(at [i] (not (finally A))) '(at [i] (always (not A))))
(step-b :always-i 3)
(swap '?1 'j)
(step-b :not-i 4)
(step-f :finally-i 3 2)
(swap '?2 'i)
(step-b :not-e 6 1)

;(export "resources/nd/theorems-ltl.edn" :not-finally->always-not)

; -----------------------------------------------------------------------------------------
; always-not->not-finally  Kröger/Merz T3

(proof '(at [i] (always (not A))) '(at [i] (not (finally A))))
(step-b :not-i 3)
(swap '?1 'j)
(step-f :finally-e 2)
(swap '?2 'j)
(swap '?3 '(at [j] contradiction))
(step-f :always-e 1 3)
(step-f :not-e 5 4)
(swap '?4 'j)

;(export "resources/nd/theorems-ltl.edn" :always-not->not-finally)

; -----------------------------------------------------------------------------------------
; always->current  Kröger/Merz T4

(proof '(at [i] (always A)) '(at [i] A))
(step-f :reflexiv)
(swap '?1 'i)
(step-f :always-e 1 2)

;(export "resources/nd/theorems-ltl.edn" :always->current)

; -----------------------------------------------------------------------------------------
; current->finally  Kröger/Merz T5

(proof '(at [i] A) '(at [i] (finally A)))
(step-f :reflexiv)
(swap '?1 'i)
(step-b :finally-i 4 :? 2)

;(export "resources/nd/theorems-ltl.edn" :current->finally)

; -----------------------------------------------------------------------------------------
; always->atnext  Kröger/Merz T6

(proof '(at [i] (always A)) '(at [i] (atnext A)))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'j)
(step-f :succ/<= 2)
(step-f :always-e 1 3)
(step-b :atnext-i 6 4)

;(export "resources/nd/theorems-ltl.edn" :always->atnext)

; -----------------------------------------------------------------------------------------
; atnext->finally  Kröger/Merz T7

(proof '(at [i] (atnext A)) '(at [i] (finally A)))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'j)
(step-f :atnext-e 1 2)
(step-f :succ/<= 2)
(step-f :finally-i 3 4)

;(export "resources/nd/theorems-ltl.edn" :atnext->finally)

; -----------------------------------------------------------------------------------------
; always->finally  Kröger/Merz T8

(proof '(at [i] (always A)) '(at [i] (finally A)))
(step-f :reflexiv)
(swap '?1 'i)
(step-f :always-e 1 2)
(step-b :finally-i 5 3)

;(export "resources/nd/theorems-ltl.edn" :always->finally)

; -----------------------------------------------------------------------------------------
; finally-always->always-finally  Kröger/Merz T9

(proof '(at[i] (finally (always A))) '(at [i] (always (finally A))))
(step-b :always-i 3)
(swap '?1 'j)
(step-f :finally-e 1 4)
(swap '?2 'k)
(step-f :linear 2 3)
(step-f :rel-cases 5 7)
(step-f :</<= 6)
(step-f :reflexiv)
(swap '?3 'k)
(step-f :always-e 4 8)
(step-b :finally-i 11 9)
(step-f :=/<= 11)
(step-f :always->current 4)
(step-b :finally-i 15 13)
(step-f :</<= 15)
(step-f :always-e 4 16)
(step-f :reflexiv)
(swap '?4 'j)
(step-b :finally-i 20 17)


;(export "resources/nd/theorems-ltl.edn" :finally-always->always-finally)
; -----------------------------------------------------------------------------------------
; always-always->always  Kröger/Merz T10

(proof '(at [i] (always (always A))) '(at [i] (always A)))
(step-b :always-i 3)
(swap '?1 'j)
(step-f :always-e 1 2)
(step-f :reflexiv)
(swap '?2 'j)
(step-f :always-e 3 4)

;(export "resources/nd/theorems-ltl.edn" :always-always->always)

; -----------------------------------------------------------------------------------------
; always->always-always  Kröger/Merz T10

(proof '(at [i] (always A)) '(at [i] (always (always A))))
(step-b :always-i 3)
(swap '?1 'j)
(step-b :always-i 4)
(swap '?2 'k)
(step-f :transitiv 2 3)
(step-f :always-e 1 4)

;(export "resources/nd/theorems-ltl.edn" :always->always-always)

; -----------------------------------------------------------------------------------------
; finally-finally->finally  Kröger/Merz T11

(proof '(at [i] (finally (finally A))) '(at [i] (finally A)))
(step-f :finally-e 1 3)
(swap '?1 'j)
(step-f :finally-e 3 5)
(swap '?2 'k)
(step-f :transitiv 2 4)
(step-f :finally-i 5 6)

;(export "resources/nd/theorems-ltl.edn" :finally-finally->finally)

; -----------------------------------------------------------------------------------------
; finally->finally-finally  Kröger/Merz T11

(proof '(at [i] (finally A)) '(at [i] (finally (finally A))))
(step-f :reflexiv)
(swap '?1 'i)
(step-b :finally-i 4 :? 2)

;(export "resources/nd/theorems-ltl.edn" :finally->finally-finally)

; -----------------------------------------------------------------------------------------
; always-atnext->atnext-always  Kröger/Merz T12

; TODO
(proof '(at [i] (always (atnext A))) '(at [i] (atnext (always A))))

;(export "resources/nd/theorems-ltl.edn" :always-atnext->atnext-always)

; -----------------------------------------------------------------------------------------
; atnext-always->always-atnext  Kröger/Merz T12

; TODO
(proof '(at [i] (atnext (always A))) '(at [i] (always (atnext A))))

;(export "resources/nd/theorems-ltl.edn" :atnext-always->always-atnext)

; -----------------------------------------------------------------------------------------
; finally-atnext->atnext-finally  Kröger/Merz T13

; TODO
(proof '(at [i] (finally (atnext A))) '(at [i] (atnext (finally A))))

;(export "resources/nd/theorems-ltl.edn" :finally-atnext->atnext-finally)

; -----------------------------------------------------------------------------------------
; atnext-finally->finally-atnext  Kröger/Merz T13

; TODO
(proof '(at [i] (atnext (finally A))) '(at [i] (finally (atnext A))))

;(export "resources/nd/theorems-ltl.edn" :atnext-finally->finally-atnext)

; -----------------------------------------------------------------------------------------
; atnext-impl-dist1  Kröger/Merz T14

(proof '(at [i] (atnext (impl A B))) '(at [i] (impl (atnext A) (atnext B))))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'j)
(step-b :impl-i 4)
(step-f :atnext-e 1 2)
(step-b :atnext-i 6 :? 2)
(step-f :atnext-e 3 2)
(step-f :impl-e 4 5)

;(export "resources/nd/theorems-ltl.edn" :atnext-impl-dist1)

; -----------------------------------------------------------------------------------------
; atnext-impl-dist2  Kröger/Merz T14

(proof '(at [i] (impl (atnext A) (atnext B))) '(at [i] (atnext (impl A B))))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'j)
(step-b :atnext-i 4 :? 2)
(step-b :impl-i 4)
(step-f :atnext-i 3 2)
(step-b :atnext-e 6 :? 2)
(step-f :impl-e 1 4)
(step-b :atnext-i 4 :? 2)

;(export "resources/nd/theorems-ltl.edn" :atnext-impl-dist2)

; -----------------------------------------------------------------------------------------
; atnext-and-dist1  Kröger/Merz T15

(proof '(at [i] (atnext (and A B))) '(at [i] (and (atnext A) (atnext B))))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'j)
(step-f :atnext-e 1 2)
(step-f :and-e1 3)
(step-f :and-e2 3)
(step-f :atnext-i 4 2)
(step-f :atnext-i 5 2)
(step-b :and-i 9)

; (export "resources/nd/theorems-ltl.edn" :atnext-and-dist1)

; -----------------------------------------------------------------------------------------
; atnext-and-dist2  Kröger/Merz T15

(proof '(at [i] (and (atnext A) (atnext B))) '(at [i] (atnext (and A B))))
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-f :succ)
(swap '?1 'i)
(swap '?2 'j)
(step-f :atnext-e 2 4)
(step-f :atnext-e 3 4)
(step-f :and-i 5 6)
(step-b :atnext-i 9 7)

; (export "resources/nd/theorems-ltl.edn" :atnext-and-dist2)

; -----------------------------------------------------------------------------------------
; atnext-or-dist1  Kröger/Merz T16

; TODO
(proof '(at [i] (atnext (or A B))) '(at [i] (or (atnext A) (atnext B))))

; (export "resources/nd/theorems-ltl.edn" :atnext-or-dist1)

; -----------------------------------------------------------------------------------------
; atnext-or-dist2  Kröger/Merz T16

; TODO
(proof '(at [i] (or (atnext A) (atnext B))) '(at [i] (atnext (or A B))))

; (export "resources/nd/theorems-ltl.edn" :atnext-or-dist2)

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

; (export "resources/nd/theorems-ltl.edn" :always-and-dist1)

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

; (export "resources/nd/theorems-ltl.edn" :always-and-dist2)

; -----------------------------------------------------------------------------------------
; finally-or-dist1  Kröger/Merz T19

; TODO
(proof '(at [i] (finally (or A B))) '(at [i] (or (finally A) (finally B))))

; (export "resources/nd/theorems-ltl.edn" :finally-or-dist1)

; -----------------------------------------------------------------------------------------
; finally-or-dist2  Kröger/Merz T19

; TODO
(proof '(at [i] (or (finally A) (finally B))) '(at [i] (finally (or A B))))

; (export "resources/nd/theorems-ltl.edn" :finally-or-dist2)

; -----------------------------------------------------------------------------------------
; always-finally-or-dist1  Kröger/Merz T20

; TODO
(proof '(at [i] (always (finally (or A B)))) '(at [i] (or (always (finally A)) (always (finally B)))))

; (export "resources/nd/theorems-ltl.edn" :always-finally-or-dist1)
 
; -----------------------------------------------------------------------------------------
; always-finally-or-dist2  Kröger/Merz T20

; TODO
(proof '(at [i] (or (always (finally A)) (always (finally B)))) '(at [i] (always (finally (or A B)))))

; (export "resources/nd/theorems-ltl.edn" :always-finally-or-dist2)

; -----------------------------------------------------------------------------------------
; finally-always-and-dist1  Kröger/Merz T21

; TODO
(proof '(at [i] (finally (always (and A B)))) '(at [i] (and (finally (always A)) (finally (always B)))))

; (export "resources/nd/theorems-ltl.edn" :finally-always-and-dist1)

; -----------------------------------------------------------------------------------------
; finally-always-and-dist2  Kröger/Merz T21

; TODO
(proof '(at [i] (and (finally (always A)) (finally (always B)))) '(at [i] (finally (always (and A B)))))

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
; finally-impl-dist  Kröger/Merz T24  Ben-Ari Axiom 2

; TODO
(proof '(at [i] (impl (finally A) (finally B))) '(at [i] (finally (impl A B))))

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
; always-serial  Kröger/Merz T28

(proof '(at [i] (always A)) '(at [i] (and A (atnext (always A)))))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'j)
(step-f :reflexiv)
(swap '?3 'i)
(step-b :and-i 5)
(step-f :always-e 1 3)
(step-b :atnext-i 6 :? 2)
(step-b :always-i 6)
(swap '?4 'k)
(step-f :succ/<= 2)
(step-f :transitiv 6 5)
(step-f :always-e 1 7)

; (export "resources/nd/theorems-ltl.edn" :always-serial)

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

; (export "resources/nd/theorems-ltl.edn" :always-inductive)

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

; TODO
(proof '(at [i] (always A)) '(at [i] (and (and A (atnext A) (atnext (always A))))))

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

; TODO
(proof '(at [i] (until A B)) '(at [i] (finally B)))

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

; (export "resources/nd/theorems-ltl.edn" :until-expansion2)

; -----------------------------------------------------------------------------------------
