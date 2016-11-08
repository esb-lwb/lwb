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

; since we have already shown the result -- it's part of our theorems
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
(step-f :exists-e 4 6) 
(unify 'V1 :i)
(step-f :impl-e 3 2)
(step-f :forall-e 7 5)
(step-f :forall-e 8 1)
(step-f :not-e 9 6)

;     --------------------------------------------------
;  1: (actual :k)                             :premise
;  2: Dark                                    :premise
;  3: (impl Dark (forall [x] (forall [y] (not (Saw x y))))):premise
;      ------------------------------------------------
;  4:  | (exists [z] (Saw z :k))              :assumption
;      | ----------------------------------------------
;  5:  | | (actual :i)                        :assumption
;  6:  | | (Saw :i :k)                        :assumption
;  7:  | | (forall [x] (forall [y] (not (Saw x y)))):impl-e [3 2]
;  8:  | | (forall [y] (not (Saw :i y)))      :forall-e [7 5]
;  9:  | | (not (Saw :i :k))                  :forall-e [8 1]
; 10:  | | contradiction                      :not-e [9 6]
;      | ----------------------------------------------
; 11:  | contradiction                        :exists-e [4 [5 10]]
;      ------------------------------------------------
; 12: (not (exists [z] (Saw z :k)))           :not-i [[4 11]]
;     --------------------------------------------------

; -----------------------------------------------------------------------------------------
; p.111

(proof '[(forall [x] (impl (R x) (S x))) (forall [y] (impl (S y) (T y)))] '(forall [z] (impl (R z) (T z))))
(step-b :forall-i 4)
(unify 'V1 :i)
(step-f :forall-e 1 3)
(step-f :forall-e 2 3)
(step-b :impl-i 7)
(step-f :impl-e 4 6)
(step-f :impl-e 5 7)

;     --------------------------------------------------
;  1: (forall [x] (impl (R x) (S x)))         :premise
;  2: (forall [y] (impl (S y) (T y)))         :premise
;      ------------------------------------------------
;  3:  | (actual :i)                          :assumption
;  4:  | (impl (R :i) (S :i))                 :forall-e [1 3]
;  5:  | (impl (S :i) (T :i))                 :forall-e [2 3]
;      | ----------------------------------------------
;  6:  | | (R :i)                             :assumption
;  7:  | | (S :i)                             :impl-e [4 6]
;  8:  | | (T :i)                             :impl-e [5 7]
;      | ----------------------------------------------
;  9:  | (impl (R :i) (T :i))                 :impl-i [[6 8]]
;      ------------------------------------------------
; 10: (forall [z] (impl (R z) (T z)))         :forall-i [[3 9]]
;     --------------------------------------------------

; -----------------------------------------------------------------------------------------
; p.112

(proof '[(actual :j) (forall [x] (R x))] '(exists [y] (R y)))
(step-f :forall-e 2 1)
(step-b :exists-i 5 1)

; --------------------------------------------------
; 1: (actual :j)                             :premise
; 2: (forall [x] (R x))                      :premise
; 3: (R :j)                                  :forall-e [2 1]
; 4: (exists [y] (R y))                      :exists-i [1 3]
; --------------------------------------------------

; -----------------------------------------------------------------------------------------
; p.114

(proof '(forall [x] (Green x)) '(forall [y] (impl (Sheep y) (Green y))))
(step-b :forall-i 3)
(unify 'V1 :i)
(step-b :impl-i 4)
(step-f :forall-e 1 2)

;    --------------------------------------------------
; 1: (forall [x] (Green x))                  :premise
;     ------------------------------------------------
; 2:  | (actual :i)                          :assumption
;     | ----------------------------------------------
; 3:  | | (Sheep :i)                         :assumption
; 4:  | | (Green :i)                         :forall-e [1 2]
;     | ----------------------------------------------
; 5:  | (impl (Sheep :i) (Green :i))         :impl-i [[3 4]]
;     ------------------------------------------------
; 6: (forall [y] (impl (Sheep y) (Green y))) :forall-i [[2 5]]
;    --------------------------------------------------

; -----------------------------------------------------------------------------------------
; p.115

(proof '[(forall [x] (not (Green x))) (forall [y] (not (Sheep y)))] '(forall [z] (impl (Sheep z) (Green z))))
(step-b :forall-i 4)
(unify 'V1 :i)
(step-b :impl-i 5)
(step-f :forall-e 2 3)
(step-f :not-e 5 4)
(step-b :efq 8)

;    --------------------------------------------------
; 1: (forall [x] (not (Green x)))            :premise
; 2: (forall [y] (not (Sheep y)))            :premise
;     ------------------------------------------------
; 3:  | (actual :i)                          :assumption
;     | ----------------------------------------------
; 4:  | | (Sheep :i)                         :assumption
; 5:  | | (not (Sheep :i))                   :forall-e [2 3]
; 6:  | | contradiction                      :not-e [5 4]
; 7:  | | (Green :i)                         :efq [6]
;     | ----------------------------------------------
; 8:  | (impl (Sheep :i) (Green :i))         :impl-i [[4 7]]
;     ------------------------------------------------
; 9: (forall [z] (impl (Sheep z) (Green z))) :forall-i [[3 8]]
;    --------------------------------------------------

; -----------------------------------------------------------------------------------------
; p.117 Drinker's paradoxon

; That's my proof
(proof '[(actual :j) (actual :k)] '(exists [x] (impl (Drunk x) (and (Drunk :j) (Drunk :k)))))
(step-f :tnd)
(unify 'V1 '(and (Drunk :j) (Drunk :k)))
(step-f :or-e 3 5)
(step-f :and-e1 4)
(step-b :exists-i 7 1)
(step-b :impl-i 7)
(step-f :not-and->or-not 10)
(step-f :or-e 11 13)
(step-b :exists-i 14 1)
(step-b :impl-i 14)
(step-f :not-e 12 13)
(step-b :efq 16)
(step-b :exists-i 20 2)
(step-b :impl-i 20)
(step-f :not-e 18 19)
(step-b :efq 22)

;     --------------------------------------------------
;  1: (actual :j)                             :premise
;  2: (actual :k)                             :premise
;  3: (or (and (Drunk :j) (Drunk :k)) (not (and (Drunk :j) (Drunk :k)))):tnd []
;      ------------------------------------------------
;  4:  | (and (Drunk :j) (Drunk :k))          :assumption
;  5:  | (Drunk :j)                           :and-e1 [4]
;      | ----------------------------------------------
;  6:  | | (Drunk :j)                         :assumption
;  7:  | | (and (Drunk :j) (Drunk :k))        :repeat [4]
;      | ----------------------------------------------
;  8:  | (impl (Drunk :j) (and (Drunk :j) (Drunk :k))):impl-i [[6 7]]
;  9:  | (exists [x] (impl (Drunk x) (and (Drunk :j) (Drunk :k)))):exists-i [1 8]
;      ------------------------------------------------
;      ------------------------------------------------
; 10:  | (not (and (Drunk :j) (Drunk :k)))    :assumption
; 11:  | (or (not (Drunk :j)) (not (Drunk :k))):not-and->or-not [10]
;      | ----------------------------------------------
; 12:  | | (not (Drunk :j))                   :assumption
;      | | --------------------------------------------
; 13:  | | | (Drunk :j)                       :assumption
; 14:  | | | contradiction                    :not-e [12 13]
; 15:  | | | (and (Drunk :j) (Drunk :k))      :efq [14]
;      | | --------------------------------------------
; 16:  | | (impl (Drunk :j) (and (Drunk :j) (Drunk :k))):impl-i [[13 15]]
; 17:  | | (exists [x] (impl (Drunk x) (and (Drunk :j) (Drunk :k)))):exists-i [1 16]
;      | ----------------------------------------------
;      | ----------------------------------------------
; 18:  | | (not (Drunk :k))                   :assumption
;      | | --------------------------------------------
; 19:  | | | (Drunk :k)                       :assumption
; 20:  | | | contradiction                    :not-e [18 19]
; 21:  | | | (and (Drunk :j) (Drunk :k))      :efq [20]
;      | | --------------------------------------------
; 22:  | | (impl (Drunk :k) (and (Drunk :j) (Drunk :k))):impl-i [[19 21]]
; 23:  | | (exists [x] (impl (Drunk x) (and (Drunk :j) (Drunk :k)))):exists-i [2 22]
;      | ----------------------------------------------
; 24:  | (exists [x] (impl (Drunk x) (and (Drunk :j) (Drunk :k)))):or-e [11 [12 17] [18 23]]
;      ------------------------------------------------
; 25: (exists [x] (impl (Drunk x) (and (Drunk :j) (Drunk :k)))):or-e [3 [4 9] [10 24]]
;     --------------------------------------------------


; That's Bornat's proof
(proof '[(actual :j) (actual :k)] '(exists [x] (impl (Drunk x) (and (Drunk :j) (Drunk :k)))))
(step-b :raa 4)
(step-b :not-e 5 3)
(step-b :exists-i 5 1)
(step-b :impl-i 5)
(step-b :and-i 6)
(step-b :efq 6)
(step-b :not-e 6 3)
(step-b :exists-i 6 2)
(step-b :impl-i 6)
(step-b :and-i 7)

;     --------------------------------------------------
;  1: (actual :j)                             :premise
;  2: (actual :k)                             :premise
;      ------------------------------------------------
;  3:  | (not (exists [x] (impl (Drunk x) (and (Drunk :j) (Drunk :k))))):assumption
;      | ----------------------------------------------
;  4:  | | (Drunk :j)                         :assumption
;      | | --------------------------------------------
;  5:  | | | (Drunk :k)                       :assumption
;  6:  | | | (and (Drunk :j) (Drunk :k))      :and-i [4 5]
;      | | --------------------------------------------
;  7:  | | (impl (Drunk :k) (and (Drunk :j) (Drunk :k))):impl-i [[5 6]]
;  8:  | | (exists [x] (impl (Drunk x) (and (Drunk :j) (Drunk :k)))):exists-i [2 7]
;  9:  | | contradiction                      :not-e [3 8]
; 10:  | | (Drunk :k)                         :efq [9]
; 11:  | | (and (Drunk :j) (Drunk :k))        :and-i [4 10]
;      | ----------------------------------------------
; 12:  | (impl (Drunk :j) (and (Drunk :j) (Drunk :k))):impl-i [[4 11]]
; 13:  | (exists [x] (impl (Drunk x) (and (Drunk :j) (Drunk :k)))):exists-i [1 12]
; 14:  | contradiction                        :not-e [3 13]
;      ------------------------------------------------
; 15: (exists [x] (impl (Drunk x) (and (Drunk :j) (Drunk :k)))):raa [[3 14]]
;     --------------------------------------------------

; That's the proof for the more general theorem

(proof '(actual :i) '(exists [x] (impl (Drunk x) (forall [y] (Drunk y)))))
(step-f :tnd)
(unify 'V1 '(forall [y] (Drunk y)))
(step-f :or-e 2 4)
(step-b :exists-i 5 1)
(step-b :impl-i 5)
(step-f :not-forall->exists-not 8)
(step-f :exists-e 9 11)
(unify 'V3 :j)
(step-b :exists-i 13 10)
(step-b :impl-i 13)
(step-f :not-e 11 12)
(step-b :efq 15)

;     --------------------------------------------------
;  1: (actual :i)                             :premise
;  2: (or (forall [y] (Drunk y)) (not (forall [y] (Drunk y)))):tnd []
;      ------------------------------------------------
;  3:  | (forall [y] (Drunk y))               :assumption
;      | ----------------------------------------------
;  4:  | | (Drunk :i)                         :assumption
;  5:  | | (forall [y] (Drunk y))             :repeat [3]
;      | ----------------------------------------------
;  6:  | (impl (Drunk :i) (forall [y] (Drunk y))):impl-i [[4 5]]
;  7:  | (exists [x] (impl (Drunk x) (forall [y] (Drunk y)))):exists-i [1 6]
;      ------------------------------------------------
;      ------------------------------------------------
;  8:  | (not (forall [y] (Drunk y)))         :assumption
;  9:  | (exists [y] (not (Drunk y)))         :not-forall->exists-not [8]
;      | ----------------------------------------------
; 10:  | | (actual :j)                        :assumption
; 11:  | | (not (Drunk :j))                   :assumption
;      | | --------------------------------------------
; 12:  | | | (Drunk :j)                       :assumption
; 13:  | | | contradiction                    :not-e [11 12]
; 14:  | | | (forall [y] (Drunk y))           :efq [13]
;      | | --------------------------------------------
; 15:  | | (impl (Drunk :j) (forall [y] (Drunk y))):impl-i [[12 14]]
; 16:  | | (exists [x] (impl (Drunk x) (forall [y] (Drunk y)))):exists-i [10 15]
;      | ----------------------------------------------
; 17:  | (exists [x] (impl (Drunk x) (forall [y] (Drunk y)))):exists-e [9 [10 16]]
;      ------------------------------------------------
; 18: (exists [x] (impl (Drunk x) (forall [y] (Drunk y)))):or-e [2 [3 7] [8 17]]
;     --------------------------------------------------
