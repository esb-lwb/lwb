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

; (export "resources/nd/theorems-prop.edn" :notnot-i)

;
;    --------------------------------------------------
; 1: P                                       premise
;     ------------------------------------------------
; 2:  | (not P)                              assumption
; 3:  | contradiction                        "not-e" (1 2)
;     ------------------------------------------------
; 4: (not (not P))                           "not-i" ([2 3])
;    ---------------------------------------------------------------------------------------------------


; -----------------------------------------------------------------------------------------
; notnot-elimination

(proof '(not (not P)) 'P)
(step-b :raa 3)
(step-f :not-e 1 2)

; (export "resources/nd/theorems-prop.edn" :notnot-e)
 
;
;   --------------------------------------------------
; 1: (not (not P))                           premise
;     ------------------------------------------------
; 2:  | (not P)                              assumption
; 3:  | contradiction                        "not-e" (1 2)
;     ------------------------------------------------
; 4: P                                       "raa" ([2 3])
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
; 1: (impl P Q)                              premise
; 2: (not Q)                                 premise
;     ------------------------------------------------
; 3:  | P                                    assumption
; 4:  | Q                                    "impl-e" (1 3)
; 5:  | contradiction                        "not-e" (2 4)
;     ------------------------------------------------
; 6: (not P)                                 "not-i" ([3 5])
;    --------------------------------------------------

; -----------------------------------------------------------------------------------------
; Tertium non datur

(proof '(or P (not P)))
(step-b :raa 2)
(step-b :not-e 3)
(unify 'V1 '(or P (not P)))
(step-b :or-i2 3)
(step-b :not-i 3)
(step-f :or-i1 2)
(unify 'V2 '(not P))
(step-f :not-e 1 3)

;; or

(proof '(or P (not P)))
(step-b :raa 2)
(step-b :not-e 3 1)
(step-b :or-i2 3)
(step-b :not-i 3)
(step-f :or-i1 2)
(unify 'V1 '(not P))
(step-f :not-e 1 3)

;(export "resources/nd/theorems-prop.edn" :tnd)

;
;    --------------------------------------------------
;     ------------------------------------------------
; 1:  | (not (or P (not P)))                 assumption
;     | ----------------------------------------------
; 2:  | | P                                  assumption
; 3:  | | (or P (not P))                     "or-i1" (2)
; 4:  | | contradiction                      "not-e" (1 3)
;     | ----------------------------------------------
; 5:  | (not P)                              "not-i" ([2 4])
; 6:  | (or P (not P))                       "or-i2" (5)
; 7:  | contradiction                        "not-e" (1 6)
;     ------------------------------------------------
; 8: (or P (not P))                          "raa" ([1 7])
;    --------------------------------------------------

; Classical theorems

; -----------------------------------------------------------------------------------------
; Contraposition

(proof '(impl P Q) '(impl (not Q) (not P)))
(step-b :impl-i 3)
(step-b :not-i 4)
(step-f :impl-e 1 3)
(step-b :not-e 6 2)

; (export "resources/nd/theorems-prop.edn" :contrap)

;
;     --------------------------------------------------
; 1: (impl P Q)                              premise
;     ------------------------------------------------
; 2:  | (not Q)                              assumption
;     | ----------------------------------------------
; 3:  | | P                                  assumption
; 4:  | | Q                                  "impl-e" (1 3)
; 5:  | | contradiction                      "not-e" (2 4)
;     | ----------------------------------------------
; 6:  | (not P)                              "not-i" ([3 5])
;     ------------------------------------------------
; 7: (impl (not Q) (not P))                  "impl-i" ([2 6])
;    --------------------------------------------------

; -----------------------------------------------------------------------------------------
; Pierce's law

(proof '(impl (impl (impl P Q) P) P))
(step-b :impl-i 2)
(step-b :raa 3)
(step-b :not-e 4)
(unify 'V1 'P)
(step-b :impl-e 4)
(unify 'V2 '(impl P Q))
(step-b :impl-i 4)
(step-f :not-e 2 3)
(step-b :efq 6)

; (export "resources/nd/theorems-prop.edn" :pierce)

;
;    --------------------------------------------------
;     ------------------------------------------------
; 1:  | (impl (impl P Q) P)                  assumption
;     | ----------------------------------------------
; 2:  | | (not P)                            assumption
;     | | --------------------------------------------
; 3:  | | | P                                assumption
; 4:  | | | contradiction                    "not-e" (2 3)
; 5:  | | | Q                                "efq" (4)
;     | | --------------------------------------------
; 6:  | | (impl P Q)                         "impl-i" ([3 5])
; 7:  | | P                                  "impl-e" (1 6)
; 8:  | | contradiction                      "not-e" (2 7)
;     | ----------------------------------------------
; 9:  | P                                    "raa" ([2 8])
;     ------------------------------------------------
; 10: (impl (impl (impl P Q) P) P)            "impl-i" ([1 9])
;    --------------------------------------------------


; -----------------------------------------------------------------------------------------
; De Morgan

(proof '(not (and P Q)) '(or (not P) (not Q)))
(step-b :raa 3)
(step-b :not-e 4)
(unify 'V1 '(and P Q))
(step-b :and-i 4)
(step-b :raa 6)
(step-b :not-e 7)
(unify 'V2 '(or (not P) (not Q)))
(step-b :or-i2 7)
(step-b :raa 4)
(step-b :not-e 5)
(unify 'V3 '(or (not P) (not Q)))
(step-b :or-i1 5)

; (export "resources/nd/theorems-prop.edn" :not-and->or-not)

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

;
;     --------------------------------------------------
;  1: (or (not P) (not Q))                    premise
;      ------------------------------------------------
;  2:  | (not P)                              assumption
;      | ----------------------------------------------
;  3:  | | (and P Q)                          assumption
;  4:  | | P                                  "and-e1" (3)
;  5:  | | contradiction                      "not-e" (2 4)
;      | ----------------------------------------------
;  6:  | (not (and P Q))                      "not-i" ([3 5])
;      ------------------------------------------------
;      ------------------------------------------------
;  7:  | (not Q)                              assumption
;      | ----------------------------------------------
;  8:  | | (and P Q)                          assumption
;  9:  | | Q                                  "and-e2" (8)
; 10:  | | contradiction                      "not-e" (7 9)
;      | ----------------------------------------------
; 11:  | (not (and P Q))                      "not-i" ([8 10])
;      ------------------------------------------------
; 12: (not (and P Q))                         "or-e" (1 [2 6] [7 11])
;     --------------------------------------------------

(proof '(not (or P Q)) '(and (not P) (not Q)))
(step-b :and-i 3)
(step-b :not-i 3)
(step-f :or-i1 2)
(unify 'V1 'Q)
(step-f :not-e 1 3)
(step-b :not-i 7)
(step-f :or-i2 6)
(unify 'V2 'P)
(step-f :not-e 1 7)

;(export "resources/nd/theorems-prop.edn" :not-or->and-not)

;
; --------------------------------------------------
; 1: (not (or P Q))                          :premise
; ------------------------------------------------
; 2:  | P                                    :assumption
; 3:  | (or P Q)                             :or-i1 [2]
; 4:  | contradiction                        :not-e [1 3]
; ------------------------------------------------
; 5: (not P)                                 :not-i [[2 4]]
; ------------------------------------------------
; 6:  | Q                                    :assumption
; 7:  | (or P Q)                             :or-i2 [6]
; 8:  | contradiction                        :not-e [1 7]
; ------------------------------------------------
; 9: (not Q)                                 :not-i [[6 8]]
; 10: (and (not P) (not Q))                   :and-i [5 9]
; --------------------------------------------------

(proof '(and (not P) (not Q)) '(not (or P Q)))
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-b :not-i 5)
(step-f :or-e 4 6)
(step-f :not-e 2 5)
(step-f :not-e 3 7)

; (export "resources/nd/theorems-prop.edn" :and-not->not-or)

;
; --------------------------------------------------
; 1: (and (not P) (not Q))                   :premise
; 2: (not P)                                 :and-e1 [1]
; 3: (not Q)                                 :and-e2 [1]
; ------------------------------------------------
; 4:  | (or P Q)                             :assumption
; | ----------------------------------------------
; 5:  | | P                                  :assumption
; 6:  | | contradiction                      :not-e [2 5]
; | ----------------------------------------------
; | ----------------------------------------------
; 7:  | | Q                                  :assumption
; 8:  | | contradiction                      :not-e [3 7]
; | ----------------------------------------------
; 9:  | contradiction                        :or-e [4 [5 6] [7 8]]
; ------------------------------------------------
; 10: (not (or P Q))                          :not-i [[4 9]]
; --------------------------------------------------