; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.examples.prop-rules
  (:require [lwb.nd.repl :refer :all]))

; interactive checking in the repl for nd

(load-logic :prop)

; -----------------------------------------------------------------------------------------
; and-introduction

; forward
(proof '[P1 P2] '(and P1 P2))
(step-f :and-i 1 2)

(proof '[P1 P2] '(and P1 P2))
(step-f :and-i 1)
(swap '?1 'P2)

(proof '[P1 P2] '(and P1 P2))
(step-f :and-i :? 2)
(swap '?1 'P1)
 
; --------------------------------------------------
; 1: P1                                      premise
; 2: P2                                      premise
; 3: (and P1 P2)                             :and-i [1 2]
; --------------------------------------------------

(proof '[P1 P2] '(and P1 P2))
(step-b :and-i 4)

(proof 'A '(and A A))
(step-b :and-i 3)

; -----------------------------------------------------------------------------------------
; and-elimination

; forward preserving the left side
(proof '(and P1 P2) 'P1)
(step-f :and-e1 1)
 
; --------------------------------------------------
; 1: (and P1 P2)                             premise
; 2: P1                                      :and-e1 [1]
; --------------------------------------------------

; forward preserving the right side
(proof '(and P1 P2) 'P2)
(step-f :and-e2 1)

(proof '(and A A) 'A)
(step-f :and-e1 1)

; -----------------------------------------------------------------------------------------
; or-introduction

; forward inventing the right side
(proof 'P1 '(or P1 P2))
(step-f :or-i1 1)
(swap '?1 'P2)

; backward preserving the left side
(proof 'P1 '(or P1 P2))
(step-b :or-i1 3)
;
; --------------------------------------------------
; 1: P1                                      premise
; 2: (or P1 P2)                              :or-i1 [1]
; --------------------------------------------------

; forward inventing the left side
(proof 'P2 '(or P1 P2))
(step-f :or-i2 1)
(swap '?1 'P1)

; backward preserving the right side
(proof 'P2 '(or P1 P2))
(step-b :or-i2 3)

(proof 'A '(or A A))
(step-b :or-i1 3)

; -----------------------------------------------------------------------------------------
; or-elimination

; forward
(proof '(or (and P R) (and Q R)) 'R)
(step-f :or-e 1 3)
(step-f :and-e2 2)
(step-f :and-e2 4)
;
;    --------------------------------------------------
; 1: (or (and P R) (and Q R))                :premise
;     ------------------------------------------------
; 2:  | (and P R)                            :assumption
; 3:  | R                                    :and-e2 [2]
;     ------------------------------------------------
;     ------------------------------------------------
; 4:  | (and Q R)                            :assumption
; 5:  | R                                    :and-e2 [4]
;     ------------------------------------------------
; 6: R                                       :or-e [1 [2 3] [4 5]]
;    --------------------------------------------------

; backward
(proof '(or (and P R) (and Q R)) 'R)
(step-b :or-e 3 1)
(step-f :and-e2 2)
(step-f :and-e2 4)

(proof '(or A A) 'A)
(step-f :or-e 1 3)

;    --------------------------------------------------
; 1: (or A A)                                 :premise
;     ------------------------------------------------
; 2:  | A                                     :assumption
; 3:  | A                                     :repeat [2]
;     ------------------------------------------------
;     ------------------------------------------------
; 4:  | A                                     :assumption
; 5:  | A                                     :repeat [4]
;     ------------------------------------------------
; 6: A                                        :or-e [1 [2 3] [4 5]]
;    --------------------------------------------------

; -----------------------------------------------------------------------------------------
; impl-introduction

; backwards
(proof '(impl (and P (not P)) Q))
(step-b :impl-i 2)
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-f :not-e 3 2)
(step-b :efq 6)

(proof 'A '(impl A A))
(step-b :impl-i 3)

; -----------------------------------------------------------------------------------------
; impl-elimination

; forward
(proof '[P (impl P Q)] 'Q)
(step-f :impl-e 2 1)

; backward
(proof '[P (impl P Q)] 'Q)
(step-b :impl-e 4 2)

(proof '[P (impl P Q)] 'Q)
(step-b :impl-e 4 :? 1)

(proof '(impl truth A) 'A)
(step-f :truth)
(step-f :impl-e 1 2)

; -----------------------------------------------------------------------------------------
; reductio ad absurdum

; backwards
(proof '(or P (not P)))
(step-b :raa 2)
(step-f :tnd)
(swap '?1 'P)
(step-f :not-e 1 2)

; -----------------------------------------------------------------------------------------
; ex falso quod libet

; forward
(proof '(and P (not P)) 'Q)
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-f :not-e 3 2)
(step-b :efq 6)

(proof '(and P (not P)) 'Q)
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-f :not-e 3 2)
(step-f :efq 4)
(swap '?1 'Q)

; backward
(proof '(and P (not P)) 'Q)
(step-b :efq 3)
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-f :not-e 3 2)

; -----------------------------------------------------------------------------------------
; Derived rules

; -----------------------------------------------------------------------------------------
; notnot-introduction

(proof 'P '(not (not P)))
(step-b :not-i 3)
(step-f :not-e 2 1)

; -----------------------------------------------------------------------------------------
; notnot-elimination

(proof '(not (not P)) 'P)
(step-b :raa 3)
(step-f :not-e 1 2)

; -----------------------------------------------------------------------------------------
; Modus Tollens

(proof '[(impl P Q) (not Q)] '(not P))
(step-b :not-i 4)
(step-f :impl-e 1 3)
(step-f :not-e 2 4)

; -----------------------------------------------------------------------------------------
; Tertium non datur

(proof '(or P (not P)))
(step-b :raa 2)
(step-b :not-e 3 1)
(step-b :or-i2 3)
(step-b :not-i 3)
(step-f :or-i1 2)
(swap '?1 '(not P))
(step-f :not-e 1 3)

;    --------------------------------------------------
;     ------------------------------------------------
; 1:  | (not (or P (not P)))                  :assumption
;     | ----------------------------------------------
; 2:  | | P                                   :assumption
; 3:  | | (or P (not P))                      :or-i1 [2]
; 4:  | | contradiction                       :not-e [1 3]
;     | ----------------------------------------------
; 5:  | (not P)                               :not-i [[2 4]]
; 6:  | (or P (not P))                        :or-i2 [5]
; 7:  | contradiction                         :not-e [1 6]
;     ------------------------------------------------
; 8: (or P (not P))                           :raa [[1 7]]
;     --------------------------------------------------

; -----------------------------------------------------------------------------------------

