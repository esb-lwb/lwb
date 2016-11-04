; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

;; # Examples for proof from Richard Bornat's book

;; Richard Bornat and Bernard Sufrin developed an app called Jape (just another proof editor) for natural deduction in
;; the propositinal and predicate logic, and further logics too.
;; In his book "Proof and Disproof in Formal Logic: An Introduction for Programmers" Richard Bornat
;; describes formal logic with emphasis on natural deduction and demonstrates the concepts with Jape.
;; The book is highly recommended.

;; In this file we reconstruct the exmaples concerning classical logic from the book in lwb.

(ns lwb.nd.examples.bornat
  (:require [lwb.nd.repl :refer :all]))

(load-logic :pred)

; interactive checking in the repl for nd

; -----------------------------------------------------------------------------------------
; p.56

(proof '[(or E F) (not F)] 'E)
(step-f :or-e 1 4)
(step-f :not-e 2 5)
(step-b :efq 8)

;
;    --------------------------------------------------
; 1: (or E F)                                :premise
; 2: (not F)                                 :premise
;     ------------------------------------------------
; 3:  | E                                    :assumption
; 4:  | E                                    :repeat [3]
;     ------------------------------------------------
;     ------------------------------------------------
; 5:  | F                                    :assumption
; 6:  | contradiction                        :not-e [2 5]
; 7:  | E                                    :efq [6]
;     ------------------------------------------------
; 8: E                                       :or-e [1 [3 4] [5 7]]
;    --------------------------------------------------

; -----------------------------------------------------------------------------------------
; p.60

(proof '[(impl E F) (impl F G)] '(impl E G))
(step-b :impl-i 4)
(step-f :impl-e 1 3)
(step-f :impl-e 2 4)

;
;    --------------------------------------------------
; 1: (impl E F)                              :premise
; 2: (impl F G)                              :premise
;     ------------------------------------------------
; 3:  | E                                    :assumption
; 4:  | F                                    :impl-e [1 3]
; 5:  | G                                    :impl-e [2 4]
;     ------------------------------------------------
; 6: (impl E G)                              :impl-i [[3 5]]
;    --------------------------------------------------

; -----------------------------------------------------------------------------------------
; p.64

(proof '(impl (not F) (not E)) '(impl E F))
(step-b :impl-i 3)
(step-b :raa 4)
(step-f :impl-e 1 3)
(step-f :not-e 4 2)

;
;    --------------------------------------------------
; 1: (impl (not F) (not E))                  :premise
;     ------------------------------------------------
; 2:  | E                                    :assumption
;     | ----------------------------------------------
; 3:  | | (not F)                            :assumption
; 4:  | | (not E)                            :impl-e [1 3]
; 5:  | | contradiction                      :not-e [4 2]
;     | ----------------------------------------------
; 6:  | F                                    :raa [[3 5]]
;     ------------------------------------------------
; 7: (impl E F)                              :impl-i [[2 6]]
;    --------------------------------------------------


; -----------------------------------------------------------------------------------------
; p.69

(proof '(and E (and F G)) '(and (and E F) G))
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-f :and-e1 3)
(step-f :and-e2 3)
(step-f :and-i 2 4)
(step-f :and-i 6 5)

; 
;    --------------------------------------------------
; 1: (and E (and F G))                       :premise
; 2: E                                       :and-e1 [1]
; 3: (and F G)                               :and-e2 [1]
; 4: F                                       :and-e1 [3]
; 5: G                                       :and-e2 [3]
; 6: (and E F)                               :and-i [2 4]
; 7: (and (and E F) G)                       :and-i [6 5]
;    --------------------------------------------------


; -----------------------------------------------------------------------------------------
; p.70

(proof '[(impl E F) (impl F G) E] 'G)
(step-f :impl-e 1 3)
(step-f :impl-e 2 4)

;
;    --------------------------------------------------
; 1: (impl E F)                              :premise
; 2: (impl F G)                              :premise
; 3: E                                       :premise
; 4: F                                       :impl-e [1 3]
; 5: G                                       :impl-e [2 4]
;    --------------------------------------------------


; -----------------------------------------------------------------------------------------
; p.70

(proof '(impl E (impl F G)) '(impl (and E F) G))
(step-b :impl-i 3)
(step-f :and-e1 2)
(step-f :impl-e 1 3)
(step-f :and-e2 2)
(step-f :impl-e 4 5)

;
;    --------------------------------------------------
; 1: (impl E (impl F G))                     :premise
;     ------------------------------------------------
; 2:  | (and E F)                            :assumption
; 3:  | E                                    :and-e1 [2]
; 4:  | (impl F G)                           :impl-e [1 3]
; 5:  | F                                    :and-e2 [2]
; 6:  | G                                    :impl-e [4 5]
;     ------------------------------------------------
; 7: (impl (and E F) G)                      :impl-i [[2 6]]
;    --------------------------------------------------

; -----------------------------------------------------------------------------------------
; p.71

(proof '(impl F G) '(impl (or E F) (or E G)))
(step-b :impl-i 3)
(step-f :or-e 2 4)
(step-b :or-i1 5)
(step-f :impl-e 1 5)
(step-b :or-i2 8)

;
;    --------------------------------------------------
; 1: (impl F G)                              :premise
;     ------------------------------------------------
; 2:  | (or E F)                             :assumption
;     | ----------------------------------------------
; 3:  | | E                                  :assumption
; 4:  | | (or E G)                           :or-i1 [3]
;     | ----------------------------------------------
;     | ----------------------------------------------
; 5:  | | F                                  :assumption
; 6:  | | G                                  :impl-e [1 5]
; 7:  | | (or E G)                           :or-i2 [6]
;     | ----------------------------------------------
; 8:  | (or E G)                             :or-e [2 [3 4] [5 7]]
;     ------------------------------------------------
; 9: (impl (or E F) (or E G))                :impl-i [[2 8]]
;    --------------------------------------------------

; -----------------------------------------------------------------------------------------
; p.73

(proof '(or E F) '(not (and (not E) (not F))))
(step-b :not-i 3)
(step-b :or-e 4)
(unify 'V1 'E)
(unify 'V2 'F)
(step-f :and-e1 2)
(step-f :not-e 4 3)
(step-f :and-e2 2)
(step-f :not-e 7 6)

;
;    --------------------------------------------------
; 1: (or E F)                                :premise
;    ------------------------------------------------
; 2:  | (and (not E) (not F))                :assumption
;     | ----------------------------------------------
; 3:  | | E                                  :assumption
; 4:  | | (not E)                            :and-e1 [2]
; 5:  | | contradiction                      :not-e [4 3]
;     | ----------------------------------------------
;     | ----------------------------------------------
; 6:  | | F                                  :assumption
; 7:  | | (not F)                            :and-e2 [2]
; 8:  | | contradiction                      :not-e [7 6]
;     | ----------------------------------------------
; 9:  | contradiction                        :or-e [1 [3 5] [6 8]]
;     ------------------------------------------------
; 10: (not (and (not E) (not F)))             :not-i [[2 9]]
;    --------------------------------------------------

; -----------------------------------------------------------------------------------------
; p.77

(proof '(not (and (not E) (not F))) '(or E F))
(step-b :raa 3)
(step-b :not-e 4 1)
(step-b :and-i 4)
(step-b :not-i 6)
(step-f :or-i2 5)
(unify 'V1 'E)
(step-f :not-e 2 6)
(step-b :not-i 4)
(step-f :or-i1 3)
(unify 'V2 'F)
(step-f :not-e 2 4)

;
;     --------------------------------------------------
; 1:  (not (and (not E) (not F)))             :premise
;      ------------------------------------------------
; 2:   | (not (or E F))                       :assumption
;      | ----------------------------------------------
; 3:   | | E                                  :assumption
; 4:   | | (or E F)                           :or-i1 [3]
; 5:   | | contradiction                      :not-e [2 4]
;      | ----------------------------------------------
; 6:   | (not E)                              :not-i [[3 5]]
;      | ----------------------------------------------
; 7:   | | F                                  :assumption
; 8:   | | (or E F)                           :or-i2 [7]
; 9:   | | contradiction                      :not-e [2 8]
;      | ----------------------------------------------
; 10:  | (not F)                              :not-i [[7 9]]
; 11:  | (and (not E) (not F))                :and-i [6 10]
; 12:  | contradiction                        :not-e [1 11]
;      ------------------------------------------------
; 13: (or E F)                                :raa [[2 12]]
;     --------------------------------------------------


; -----------------------------------------------------------------------------------------
; p.79

; since we hjave already shown the result -- it's part of our theorems
; we can do the following
(proof '(or E (not E)))
(step-f :tnd)
(unify 'V1 'E)

;
;    --------------------------------------------------
; 1: (or E (not E))                          :tnd []
;    --------------------------------------------------

; here is the actual proof

(proof '(or E (not E)))
(step-b :raa 2)
(step-b :not-e 3 1)
(step-b :or-i2 3)
(step-b :not-i 3)
(step-b :not-e 4 1)
(step-b :or-i1 4)

;
;    --------------------------------------------------
;     ------------------------------------------------
; 1:  | (not (or E (not E)))                 :assumption
;     | ----------------------------------------------
; 2:  | | E                                  :assumption
; 3:  | | (or E (not E))                     :or-i1 [2]
; 4:  | | contradiction                      :not-e [1 3]
;     | ----------------------------------------------
; 5:  | (not E)                              :not-i [[2 4]]
; 6:  | (or E (not E))                       :or-i2 [5]
; 7:  | contradiction                        :not-e [1 6]
;     ------------------------------------------------
; 8: (or E (not E))                          :raa [[1 7]]
;    --------------------------------------------------

; -----------------------------------------------------------------------------------------
; p.80

(proof 'F '(impl E F))
(step-b :impl-i 3)

;
;    --------------------------------------------------
; 1: F                                       :premise
;     ------------------------------------------------
; 2:  | E                                    :assumption
; 3:  | F                                    :repeat [1]
;     ------------------------------------------------
; 4: (impl E F)                              :impl-i [[2 3]]
;    --------------------------------------------------

; -----------------------------------------------------------------------------------------
; p.123 a stuck proof
; example for a step-f for the elimination of the implication!!

(proof '(impl E F) '(impl (impl F G) (impl E G)))
(step-f :impl-e 1)

;
;    --------------------------------------------------
; 1: (impl E F)                              :premise
; 2: ...
; 3: E
; 4: F                                       :impl-e [1 3]
; 5: ...
; 6: (impl (impl F G) (impl E G))
;    --------------------------------------------------

; -----------------------------------------------------------------------------------------
; p.99
(proof '[(actual :k) Dark (impl Dark (forall [x] (forall [y] (not (Saw x y)))))] '(not (exists [z] (Saw z, :k))))
(step-b :not-i 5)
(step-f :exists-e 4 1) 
;; geht nicht, weil wir keine substitution haben

; -----------------------------------------------------------------------------------------
; p.111

(proof '[(forall [x] (impl (R x) (S x))) (forall [y] (impl (S y) (T y)))] '(forall [z] (impl (R z) (T z))))
(step-b :forall-i 4)


; -----------------------------------------------------------------------------------------
; p.112

(proof '[(actual :j) (forall [x] (R x))] '(exists [y] (R y)))


; -----------------------------------------------------------------------------------------
; p.114

(proof '(forall [x] (Green x)) '(forall [y] (impl (Sheep y) (Green y))))


; -----------------------------------------------------------------------------------------
; p.115

(proof '[(forall [x] (not (Green x))) (forall [y] (not (Sheep y)))] '(forall [z] (impl (Sheep z) (Green z))))


; -----------------------------------------------------------------------------------------
; p.117 Drinker's paradoxon

(proof '[(actual :j) (axctual :k)] '(exists [x] (impl (Drunk x) (and (Drunk :j) (Drunk :k)))))

