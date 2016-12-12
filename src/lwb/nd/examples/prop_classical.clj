; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.examples.prop-classical
  (:require [lwb.nd.repl :refer :all]))

(load-logic :prop)

; interactive checking in the repl for nd
; -----------------------------------------------------------------------------------------
; Derived rules

; -----------------------------------------------------------------------------------------
; notnot-introduction

(proof 'P '(not (not P)))
(step-b :not-i 3)
(step-f :not-e 2 1)

;(export "resources/nd/theorems-prop.edn" :notnot-i)

;    --------------------------------------------------
; 1: P                                       :premise
;     ------------------------------------------------
; 2:  | (not P)                              :assumption
; 3:  | contradiction                        :not-e [1 2]
;     ------------------------------------------------
; 4: (not (not P))                           :not-i [[2 3]]
;    ---------------------------------------------------------------------------------------------------

; -----------------------------------------------------------------------------------------
; notnot-elimination

(proof '(not (not P)) 'P)
(step-b :raa 3)
(step-f :not-e 1 2)

;(export "resources/nd/theorems-prop.edn" :notnot-e)
 
;
;   --------------------------------------------------
; 1: (not (not P))                           :premise
;     ------------------------------------------------
; 2:  | (not P)                              :assumption
; 3:  | contradiction                        :not-e [1 2]
;     ------------------------------------------------
; 4: P                                       :raa [[2 3]]
;    --------------------------------------------------

; -----------------------------------------------------------------------------------------
; Modus Tollens

(proof '[(impl P Q) (not Q)] '(not P))
(step-b :not-i 4)
(step-f :impl-e 1 3)
(step-f :not-e 2 4)

;(export "resources/nd/theorems-prop.edn" :mt)

;
;    --------------------------------------------------
; 1: (impl P Q)                              :premise
; 2: (not Q)                                 :premise
;     ------------------------------------------------
; 3:  | P                                    :assumption
; 4:  | Q                                    :impl-e [1 3]
; 5:  | contradiction                        :not-e [2 4]
;     ------------------------------------------------
; 6: (not P)                                 :not-i [[3 5]]
;    --------------------------------------------------

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

;(export "resources/nd/theorems-prop.edn" :tnd)

;    --------------------------------------------------
;     ------------------------------------------------
; 1:  | (not (or P (not P)))                 :assumption
;     | ----------------------------------------------
; 2:  | | P                                  :assumption
; 3:  | | (or P (not P))                     :or-i1 [2]
; 4:  | | contradiction                      :not-e [1 3]
;     | ----------------------------------------------
; 5:  | (not P)                              :not-i [[2 4]]
; 6:  | (or P (not P))                       :or-i2 [5]
; 7:  | contradiction                        :not-e [1 6]
;     ------------------------------------------------
; 8: (or P (not P))                          :raa [[1 7]]
;    --------------------------------------------------


; We want to use the derived rules
;(load-logic :prop)

; -----------------------------------------------------------------------------------------
; not-or-e1

(proof '(not (or P Q)) '(not P))
(step-b :not-i 3)
(step-f :or-i1 2)
(swap '?1 'Q)
(step-f :not-e 1 3)

;(export "resources/nd/theorems-prop.edn" :not-or-e1)

; -----------------------------------------------------------------------------------------
; not-or-e2

(proof '(not (or P Q)) '(not Q))
(step-b :not-i 3)
(step-f :or-i2 2)
(swap '?1 'P)
(step-f :not-e 1 3)

;(export "resources/nd/theorems-prop.edn" :not-or-e2)


; Constants

(proof 'contradiction '(and P (not P)))
(step-b :efq 3)

(proof '(and P (not P)) 'contradiction)
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-f :not-e 3 2)

; -----------------------------------------------------------------------------------------
; Absorption 

(proof '(or P truth) 'truth)
(step-f :or-e 1 3)
(step-f :truth)

;(export "resources/nd/theorems-prop.edn" :or-truth->truth)

(proof 'truth '(or P truth))
(step-b :or-i2 3)

;(export "resources/nd/theorems-prop.edn" :truth->or-truth)

(proof '(or P contradiction) 'P)
(step-f :or-e 1 3)
(step-b :efq 6)

;(export "resources/nd/theorems-prop.edn" :or-contra-e)

(proof 'P '(or P contradiction))
(step-b :or-i1 3)

;(export "resources/nd/theorems-prop.edn" :or-contra-i)

(proof '(impl P truth) 'truth)
(step-f :truth)

;(export "resources/nd/theorems-prop.edn" :impl-truth->truth)

(proof 'truth '(impl P truth))
(step-b :impl-i 3)

;(export "resources/nd/theorems-prop.edn" :truth->impl-truth)

(proof '(impl P contradiction) '(not P))
(step-b :raa 3)
(step-f :notnot-e 2)
(step-f :impl-e 1 3)

;(export "resources/nd/theorems-prop.edn" :impl-contra->not)

(proof '(not P) '(impl P contradiction))
(step-b :impl-i 3)
(step-f :not-e 1 2)

;(export "resources/nd/theorems-prop.edn" :not->impl-contra)

(proof '(and P truth) 'P)
(step-f :and-e1 1)

;(export "resources/nd/theorems-prop.edn" :and-truth-e)

(proof 'P '(and P truth))
(step-b :and-i 3)
(step-f :truth)

;(export "resources/nd/theorems-prop.edn" :and-truth-i)

(proof '(and P contradiction) 'contradiction)
(step-f :and-e2 1)

;(export "resources/nd/theorems-prop.edn" :and-contra->contra)

(proof 'contradiction '(and P contradiction))
(step-b :efq 3)

;(export "resources/nd/theorems-prop.edn" :contra->and-contra)

(proof '(impl truth P) 'P)
(step-f :truth)
(step-f :impl-e 1 2)

;(export "resources/nd/theorems-prop.edn" :impl-truth-e)

(proof 'P '(impl truth P))
(step-b :impl-i 3)

;(export "resources/nd/theorems-prop.edn" :impl-truth-i)

(proof '(impl contradiction P) 'truth)
(step-f :truth)

;(export "resources/nd/theorems-prop.edn" :impl-contra->truth)

(proof 'truth '(impl contradiction P))
(step-b :impl-i 3)
(step-b :efq 4)

;(export "resources/nd/theorems-prop.edn" :truth->impl-contra)

; -----------------------------------------------------------------------------------------
; Collapsing of identical operands

(proof '(and P P) 'P)
(step-f :and-e1 1)

(proof 'P '(and P P))
(step-b :and-i 3)

(proof '(or P P) 'P)
(step-f :or-e 1 3)

(proof 'P '(or P P))
(step-b :or-i1 3)

(proof '(impl P P) 'truth)
(step-b :raa 3)
(step-f :contradiction 2)

(proof 'truth '(impl P P))
(step-b :impl-i 3)

; Classical theorems

; -----------------------------------------------------------------------------------------
; Contraposition

(proof '(impl P Q) '(impl (not Q) (not P)))
(step-b :impl-i 3)
(step-b :not-i 4)
(step-f :impl-e 1 3)
(step-b :not-e 6 2)

;(export "resources/nd/theorems-prop.edn" :contrap)

;     --------------------------------------------------
; 1: (impl P Q)                              :premise
;     ------------------------------------------------
; 2:  | (not Q)                              :assumption
;     | ----------------------------------------------
; 3:  | | P                                  :assumption
; 4:  | | Q                                  :impl-e [1 3]
; 5:  | | contradiction                      :not-e [2 4]
;     | ----------------------------------------------
; 6:  | (not P)                              :not-i [[3 5]]
;     ------------------------------------------------
; 7: (impl (not Q) (not P))                  :impl-i [[2 6]]
;    --------------------------------------------------

; -----------------------------------------------------------------------------------------
; Pierce's law

(proof '(impl (impl (impl P Q) P) P))
(step-b :impl-i 2)
(step-b :raa 3)
(step-b :not-e 4)
(swap '?1 'P)
(step-b :impl-e 4)
(swap '?2 '(impl P Q))
(step-b :impl-i 4)
(step-f :not-e 2 3)
(step-b :efq 6)

;(export "resources/nd/theorems-prop.edn" :pierce)

;    --------------------------------------------------
;     ------------------------------------------------
; 1:  | (impl (impl P Q) P)                  :assumption
;     | ----------------------------------------------
; 2:  | | (not P)                            :assumption
;     | | --------------------------------------------
; 3:  | | | P                                :assumption
; 4:  | | | contradiction                    :not-e [2 3]
; 5:  | | | Q                                :efq [4]
;     | | --------------------------------------------
; 6:  | | (impl P Q)                         :impl-i [[3 5]]
; 7:  | | P                                  :impl-e [1 6]
; 8:  | | contradiction                      :not-e [2 7]
;     | ----------------------------------------------
; 9:  | P                                    :raa [[2 8]]
;     ------------------------------------------------
; 10: (impl (impl (impl P Q) P) P)           :impl-i [[1 9]]
;    --------------------------------------------------

; -----------------------------------------------------------------------------------------
; impl-and->impl-impl

(proof '(impl (and P Q) R) '(impl P (impl Q R)))
(step-b :impl-i 3)
(step-b :impl-i 4)
(step-f :and-i 2 3)
(step-f :impl-e 1 4)

;(export "resources/nd/theorems-prop.edn" :impl-and->impl-impl)

; -----------------------------------------------------------------------------------------
; impl-impl->impl-and

(proof '(impl P (impl Q R)) '(impl (and P Q) R))
(step-b :impl-i 3)
(step-f :and-e1 2)
(step-f :impl-e 1 3)
(step-f :and-e2 2)
(step-f :impl-e 4 5)

;(export "resources/nd/theorems-prop.edn" :impl-impl->impl-and)

; -----------------------------------------------------------------------------------------
; and-comm

(proof '(and P Q) '(and Q P))
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-f :and-i 3 2)

;(export "resources/nd/theorems-prop.edn" :and-comm)

; -----------------------------------------------------------------------------------------
; or-comm

(proof '(or P Q) '(or Q P))
(step-f :or-e 1 3)
(step-f :or-i2 2)
(swap '?1 'Q)
(step-b :or-i1 6)

;(export "resources/nd/theorems-prop.edn" :or-comm)

; -----------------------------------------------------------------------------------------
; and-assocr

(proof '(and P (and Q R)) '(and (and P Q) R))
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-f :and-e1 3)
(step-f :and-e2 3)
(step-f :and-i 2 4)
(step-f :and-i 6 5)

;(export "resources/nd/theorems-prop.edn" :and-assocr)

; -----------------------------------------------------------------------------------------
; and-assocl

(proof '(and (and P Q) R) '(and P (and Q R)))
(step-f :and-e1 1)
(step-f :and-e1 2)
(step-f :and-e2 2)
(step-f :and-e2 1)
(step-f :and-i 4 5)
(step-f :and-i 3 6)

;(export "resources/nd/theorems-prop.edn" :and-assocl)

; -----------------------------------------------------------------------------------------
; or-assocl

(proof '(or P (or Q R)) '(or (or P Q) R))
(step-f :or-e 1 3)
(step-f :or-i1 2)
(swap '?1 'Q)
(step-f :or-i1 3)
(swap '?2 'R)
(step-f :or-e 5 7)
(step-f :or-i2 6)
(swap '?3 'P)
(step-f :or-i1 7)
(swap '?4 'R)
(step-f :or-i2 9)
(swap '?5 '(or P Q))

;(export "resources/nd/theorems-prop.edn" :or-assocl)

; -----------------------------------------------------------------------------------------
; or-assocr

(proof '(or (or P Q) R) '(or P (or Q R)))
(step-f :or-e 1 3)
(step-f :or-e 2 4)
(step-b :or-i1 5)
(step-f :or-i1 5)
(swap '?1 'R)
(step-b :or-i2 8)
(step-f :or-i2 9)
(swap '?2 'Q)
(step-b :or-i2 12)

;(export "resources/nd/theorems-prop.edn" :or-assocr)

; -----------------------------------------------------------------------------------------
; or-and-dist1

(proof '(or P (and Q R)) '(and (or P Q) (or P R)))
(step-f :or-e 1 3)
(step-b :and-i 4)
(step-b :or-i1 4)
(step-b :or-i1 5)
(step-f :and-e1 6)
(step-f :and-e2 6)
(step-b :and-i 10)
(step-b :or-i2 10)
(step-b :or-i2 11)

;(export "resources/nd/theorems-prop.edn" :or-and-dist1)

; -----------------------------------------------------------------------------------------
; or-and-dist2

(proof '(and (or P Q) (or P R)) '(or P (and Q R)))
(step-f :tnd)
(swap '?1 'P)
(step-f :or-e 2 4)
(step-b :or-i1 5)
(step-f :and-e1 1)
(step-f :or-e 6)
(swap '?2 'Q)
(step-f :not-e 5 7)
(step-b :efq 10)
(step-f :and-e2 1)
(step-f :or-e 13)
(swap '?3 'R)
(step-f :not-e 5 14)
(step-b :efq 17)
(step-f :and-i 12 19)
(step-b :or-i2 22)

;(export "resources/nd/theorems-prop.edn" :or-and-dist2)

; -----------------------------------------------------------------------------------------
; and-or-dist1

(proof '(and P (or Q R)) '(or (and P Q) (and P R)))
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-f :or-e 3 5)
(step-f :and-i 2 4)
(step-b :or-i1 7)
(step-f :and-i 2 7)
(step-b :or-i2 10)

;(export "resources/nd/theorems-prop.edn" :and-or-dist1)

; -----------------------------------------------------------------------------------------
; and-or-dist2

(proof '(or (and P Q) (and P R)) '(and P (or Q R)))
(step-f :or-e 1 3)
(step-f :and-e1 2)
(step-f :and-e2 2)
(step-b :and-i 6)
(step-b :or-i1 6)
(step-f :and-e1 7)
(step-f :and-e2 7)
(step-b :and-i 11)
(step-b :or-i2 11)

;(export "resources/nd/theorems-prop.edn" :and-or-dist2)

; -----------------------------------------------------------------------------------------
; De Morgan

(proof '(not (and P Q)) '(or (not P) (not Q)))
(step-b :raa 3)
(step-b :not-e 4)
(swap '?1 '(and P Q))
(step-b :and-i 4)
(step-b :raa 6)
(step-b :not-e 7)
(swap '?2 '(or (not P) (not Q)))
(step-b :or-i2 7)
(step-b :raa 4)
(step-b :not-e 5)
(swap '?3 '(or (not P) (not Q)))
(step-b :or-i1 5)

;(export "resources/nd/theorems-prop.edn" :not-and->or-not)

; --------------------------------------------------
; 1: (not (and P Q))                         :premise
; ------------------------------------------------
; 2:  | (not (or (not P) (not Q)))           :assumption
; | ----------------------------------------------
; 3:  | | (not P)                            :assumption
; 4:  | | (or (not P) (not Q))               :or-i1 [3]
; 5:  | | contradiction                      :not-e [2 4]
; | ----------------------------------------------
; 6:  | P                                    :raa [[3 5]]
; | ----------------------------------------------
; 7:  | | (not Q)                            :assumption
; 8:  | | (or (not P) (not Q))               :or-i2 [7]
; 9:  | | contradiction                      :not-e [2 8]
; | ----------------------------------------------
; 10:  | Q                                    :raa [[7 9]]
; 11:  | (and P Q)                            :and-i [6 10]
; 12:  | contradiction                        :not-e [1 11]
; ------------------------------------------------
; 13: (or (not P) (not Q))                    :raa [[2 12]]
; --------------------------------------------------

(proof '(or (not P) (not Q)) '(not (and P Q)))
(step-b :or-e 3 1)
(step-b :not-i 4)
(step-f :and-e1 3)
(step-f :not-e 2 4)
(step-b :not-i 9)
(step-f :and-e2 8)
(step-f :not-e 7 9)

;(export "resources/nd/theorems-prop.edn" :or-not->not-and)

;     --------------------------------------------------
;  1: (or (not P) (not Q))                    :premise
;      ------------------------------------------------
;  2:  | (not P)                              :assumption
;      | ----------------------------------------------
;  3:  | | (and P Q)                          :assumption
;  4:  | | P                                  :and-e1 [3]
;  5:  | | contradiction                      :not-e [2 4]
;      | ----------------------------------------------
;  6:  | (not (and P Q))                      :not-i [[3 5]]
;      ------------------------------------------------
;      ------------------------------------------------
;  7:  | (not Q)                              :assumption
;      | ----------------------------------------------
;  8:  | | (and P Q)                          :assumption
;  9:  | | Q                                  :and-e2 [8]
; 10:  | | contradiction                      :not-e [7 9]
;      | ----------------------------------------------
; 11:  | (not (and P Q))                      :not-i [[8 10]]
;      ------------------------------------------------
; 12: (not (and P Q))                         :or-e [1 [2 6] [7 11]]
;     --------------------------------------------------

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

;(export "resources/nd/theorems-prop.edn" :not-or->and-not)

;     --------------------------------------------------
; 1:  (not (or P Q))                          :premise
;     ------------------------------------------------
; 2:  | P                                    :assumption
; 3:  | (or P Q)                             :or-i1 [2]
; 4:  | contradiction                        :not-e [1 3]
;     ------------------------------------------------
; 5:  (not P)                                 :not-i [[2 4]]
;     ------------------------------------------------
; 6:  | Q                                    :assumption
; 7:  | (or P Q)                             :or-i2 [6]
; 8:  | contradiction                        :not-e [1 7]
;     ------------------------------------------------
; 9:  (not Q)                                 :not-i [[6 8]]
; 10: (and (not P) (not Q))                   :and-i [5 9]
;     --------------------------------------------------

(proof '(and (not P) (not Q)) '(not (or P Q)))
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-b :not-i 5)
(step-f :or-e 4 6)
(step-f :not-e 2 5)
(step-f :not-e 3 7)

;(export "resources/nd/theorems-prop.edn" :and-not->not-or)

;     --------------------------------------------------
; 1:  (and (not P) (not Q))                   :premise
; 2:  (not P)                                 :and-e1 [1]
; 3:  (not Q)                                 :and-e2 [1]
;     ------------------------------------------------
; 4:  | (or P Q)                             :assumption
;     | ----------------------------------------------
; 5:  | | P                                  :assumption
; 6:  | | contradiction                      :not-e [2 5]
;     | ----------------------------------------------
;     | ----------------------------------------------
; 7:  | | Q                                  :assumption
; 8:  | | contradiction                      :not-e [3 7]
;     | ----------------------------------------------
; 9:  | contradiction                        :or-e [4 [5 6] [7 8]]
;     ------------------------------------------------
; 10: (not (or P Q))                          :not-i [[4 9]]
;     --------------------------------------------------
 
; We want to use  DeMorgan's laws
;(load-logic :prop)
; -----------------------------------------------------------------------------------------
; impl

(proof '(impl P Q) '(or (not P) Q))
(step-b :raa 3)
(step-f :not-or->and-not 2)
(step-f :and-e1 3)
(step-f :notnot-e 4)
(step-f :impl-e 1 5)
(step-f :and-e2 3)
(step-f :not-e 7 6)

;(export "resources/nd/theorems-prop.edn" :impl->or-not)

(proof '(or (not P) Q) '(impl P Q))
(step-f :or-e 1 3)
(step-b :impl-i 4)
(step-f :not-e 2 3)
(step-b :efq 6)
(step-b :impl-i 9)

;(export "resources/nd/theorems-prop.edn" :or-not->impl)

; -----------------------------------------------------------------------------------------
; not-impl

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

;(export "resources/nd/theorems-prop.edn" :not-impl-e)

(proof '(and P (not Q)) '(not (impl P Q)))
(step-b :not-i 3)
(step-f :and-e1 1)
(step-f :impl-e 2 3)
(step-f :and-e2 1)
(step-f :not-e 5 4)

;(export "resources/nd/theorems-prop.edn" :not-impl-i)

; -----------------------------------------------------------------------------------------
