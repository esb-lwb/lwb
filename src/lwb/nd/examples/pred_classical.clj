; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.examples.pred-classical
  (:require [lwb.nd.repl :refer :all]))

(load-logic :pred)

; interactive checking in the repl for nd

; -----------------------------------------------------------------------------------------
; De Morgan

; not-forall->exists-not

(proof '(not (forall [x] (P x))) '(exists [x] (not (P x))))
(step-b :raa 3)
(step-b :not-e 4 1)
(step-b :forall-i 4)
(swap '?1 :i)
(step-b :raa 5)
(step-b :not-e 6 2)
(step-b :exists-i 6 3)

;(export "resources/nd/theorems-pred.edn" :not-forall->exists-not)

;    --------------------------------------------------
;  1: (not (forall [x] (P x)))                :premise
;      ------------------------------------------------
;  2:  | (not (exists [x] (not (P x))))       :assumption
;      | ----------------------------------------------
;  3:  | | (actual :i)                        :assumption
;      | | --------------------------------------------
;  4:  | | | (not (P :i))                     :assumption
;  5:  | | | (exists [x] (not (P x)))         :exists-i [3 4]
;  6:  | | | contradiction                    :not-e [2 5]
;      | | --------------------------------------------
;  7:  | | (P :i)                             :raa [[4 6]]
;      | ----------------------------------------------
;  8:  | (forall [x] (P x))                   :forall-i [[3 7]]
;  9:  | contradiction                        :not-e [1 8]
;      ------------------------------------------------
; 10: (exists [x] (not (P x)))                :raa [[2 9]]
;    --------------------------------------------------

; application of the theorem
(proof '(not (forall [x] (P x))) '(exists [x] (not (P x))))
(step-f :not-forall->exists-not 1)

(proof '(not (forall [x] (P x))) '(exists [x] (not (P x))))
(step-b :not-forall->exists-not 3)

(proof '(not (forall [y] (P y))) '(exists [y] (not (P y))))
(step-f :not-forall->exists-not 1)

(proof '(not (forall [y] (Q y))) '(exists [y] (not (Q y))))
(step-f :not-forall->exists-not 1)

; exists-not->not-forall

(proof '(exists [x] (not (P x))) '(not (forall [x] (P x))))
(step-b :not-i 3)
(step-f :exists-e 1 4)
(swap '?1 :i)
(step-f :forall-e 2 3)
(step-f :not-e 4 5)

;(export "resources/nd/theorems-pred.edn" :exists-not->not-forall)

;    --------------------------------------------------
; 1: (exists [x] (not (P x)))                :premise
;     ------------------------------------------------
; 2:  | (forall [x] (P x))                   :assumption
;     | ----------------------------------------------
; 3:  | | (actual :i)                        :assumption
; 4:  | | (not (P :i))                       :assumption
; 5:  | | (P :i)                             :forall-e [2 3]
; 6:  | | contradiction                      :not-e [4 5]
;     | ----------------------------------------------
; 7:  | contradiction                        :exists-e [1 [3 6]]
;     ------------------------------------------------
; 8: (not (forall [x] (P x)))                :not-i [[2 7]]
;    --------------------------------------------------

; application of the theorem
(proof '(exists [y] (not (Q y))) '(not (forall [y] (Q y))))
(step-f :exists-not->not-forall 1)


; not-exists->forall-not

(proof '(not (exists [x] (P x))) '(forall [x] (not (P x))))
(step-b :forall-i 3)
(swap '?1 :i)
(step-b :not-i 4)
(step-b :not-e 5 1)
(step-b :exists-i 5 2)

;(export "resources/nd/theorems-pred.edn" :not-exists->forall-not)

;    --------------------------------------------------
; 1: (not (exists [x] (P x)))                :premise
;     ------------------------------------------------
; 2:  | (actual :i)                          :assumption
;     | ----------------------------------------------
; 3:  | | (P :i)                             :assumption
; 4:  | | (exists [x] (P x))                 :exists-i [2 3]
; 5:  | | contradiction                      :not-e [1 4]
;     | ----------------------------------------------
; 6:  | (not (P :i))                         :not-i [[3 5]]
;     ------------------------------------------------
; 7: (forall [x] (not (P x)))                :forall-i [[2 6]]
;    --------------------------------------------------

; forall-not->not-exists

(proof '(forall [x] (not (P x))) '(not (exists [x] (P x))))
(step-b :not-i 3)
(step-f :exists-e 2 4)
(swap '?1 :i)
(step-f :forall-e 1 3)
(step-f :not-e 5 4)

;(export "resources/nd/theorems-pred.edn" :forall-not->not-exists)

;    --------------------------------------------------
; 1: (forall [x] (not (P x)))                :premise
;     ------------------------------------------------
; 2:  | (exists [x] (P x))                   :assumption
;     | ----------------------------------------------
; 3:  | | (actual :i)                        :assumption
; 4:  | | (P :i)                             :assumption
; 5:  | | (not (P :i))                       :forall-e [1 3]
; 6:  | | contradiction                      :not-e [5 4]
;     | ----------------------------------------------
; 7:  | contradiction                        :exists-e [2 [3 6]]
;     ------------------------------------------------
; 8: (not (exists [x] (P x)))                :not-i [[2 7]]
;    --------------------------------------------------

; -----------------------------------------------------------------------------------------
; Commutativity

(proof '(forall [x] (forall [y] (A x y))) '(forall [y] (forall [x] (A x y))))
(step-b :forall-i 3)
(swap '?1 :y)
(step-b :forall-i 4)
(swap '?2 :x)
(step-f :forall-e 1 3)
(step-f :forall-e 4 2)

;(export "resources/nd/theorems-pred.edn" :forall-comm1)

(proof '(forall [y] (forall [x] (A x y))) '(forall [x] (forall [y] (A x y))))
(step-b :forall-i 3)
(swap '?1 :x)
(step-b :forall-i 4)
(swap '?2 :y)
(step-f :forall-e 1 3)
(step-f :forall-e 4 2)

;(export "resources/nd/theorems-pred.edn" :forall-comm2)

(proof '(exists [x] (exists [y] (A x y))) '(exists [y] (exists [x] (A x y))))
(step-f :exists-e 1 3)
(swap '?1 :x)
(step-f :exists-e 3 5)
(swap '?2 :y)
(step-b :exists-i 7 4)
(step-b :exists-i 7 2)

;(export "resources/nd/theorems-pred.edn" :exists-comm1)

(proof '(exists [y] (exists [x] (A x y))) '(exists [x] (exists [y] (A x y))))
(step-b :exists-i 7 2)
(step-f :exists-e 1 3)
(swap '?1 :y)
(step-f :exists-e 3 5)
(swap '?2 :x)
(step-b :exists-i 7 4)
(step-b :exists-i 7 2)

;(export "resources/nd/theorems-pred.edn" :exists-comm2)

(proof '(exists [x] (forall [y] (A x y))) '(forall [y] (exists [x] (A x y))))
(step-b :forall-i 3)
(swap '?1 :y)
(step-f :exists-e 1 4)
(swap '?2 :x)
(step-f :forall-e 4 2)
(step-b :exists-i 7 3)

;(export "resources/nd/theorems-pred.edn" :exists-forall-comm)

; -----------------------------------------------------------------------------------------
; Distributivity

(proof '(exists [x] (or (A x) (B x))) '(or (exists [x] (A x)) (exists [x] (B x))))
(step-f :exists-e 1 3)
(swap '?1 :x)
(step-f :or-e 3 5)
(step-b :or-i1 6)
(step-b :exists-i 6 2)
(step-b :or-i2 9)
(step-b :exists-i 9 2)

;(export "resources/nd/theorems-pred.edn" :exists-or-dist1)

(proof '(or (exists [x] (A x)) (exists [x] (B x))) '(exists [x] (or (A x) (B x))))
(step-f :or-e 1 3)
(step-f :exists-e 2 4)
(swap '?1 :x)
(step-b :exists-i 6 3)
(step-b :or-i1 6)
(step-f :exists-e 8 10)
(swap '?2 :x)
(step-b :exists-i 12 9)
(step-b :or-i2 12)

;(export "resources/nd/theorems-pred.edn" :exists-or-dist2)

(proof '(forall [x] (and (A x) (B x))) '(and (forall [x] (A x)) (forall [x] (B x))))
(step-b :and-i 3)
(step-b :forall-i 3)
(swap '?1 :x)
(step-f :forall-e 1 2)
(step-f :and-e1 3)
(step-b :forall-i 7)
(swap '?2 :x)
(step-f :forall-e 1 6)
(step-f :and-e2 3)

;(export "resources/nd/theorems-pred.edn" :forall-and-dist1)

(proof '(and (forall [x] (A x)) (forall [x] (B x))) '(forall [x] (and (A x) (B x))))
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-b :forall-i 5)
(swap '?1 :x)
(step-f :forall-e 2 4)
(step-f :forall-e 3 4)
(step-b :and-i 8)

;(export "resources/nd/theorems-pred.edn" :forall-and-dist2)

(proof '(exists [x] (and (A x) (B x))) '(and (exists [x] (A x)) (exists [x] (B x))))
(step-f :exists-e 1 3)
(swap '?1 :x)
(step-f :and-e1 3)
(step-f :and-e2 3)
(step-b :and-i 7)
(step-b :exists-i 7 2)
(step-b :exists-i 8 2)

;(export "resources/nd/theorems-pred.edn" :exists-and-dist)

(proof '(or (forall [x] (A x)) (forall [x] (B x))) '(forall [x] (or (A x) (B x))))
(step-f :or-e 1 3)
(step-b :forall-i 4)
(swap '?1 :x)
(step-f :forall-e 2 3)
(step-b :or-i1 6)
(step-b :forall-i 9)
(swap '?2 :x)
(step-f :forall-e 7 8)
(step-b :or-i2 11)

;(export "resources/nd/theorems-pred.edn" :forall-or-dist)

; -----------------------------------------------------------------------------------------
; Quantification without free variable in scope

(proof '(or (exists [x] (A x)) B) '(exists [x] (or (A x) B)))
(step-f :or-e 1 3)
(step-f :exists-e 2 4)
(swap '?1 :x)
(step-b :exists-i 6 3)
(step-b :or-i1 6)
(step-f :actual)
(swap '?2 :x)
(step-b :exists-i 11 9)
(step-b :or-i2 11)
; that's okay, since we do not use whether (A :x) is true or not!!

(proof '(exists [x] (or (A x) B)) '(or (exists [x] (A x)) B))
(step-f :exists-e 1 3)
(swap '?1 :x)
(step-f :or-e 3 5)
(subclaim '(exists [x] (A x)))
(step-b :exists-i 6 2)
(step-b :or-i1 7)
(step-b :or-i2 9)

(proof '(and (exists [x] (A x)) B) '(exists [x] (and (A x) B)))
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-f :exists-e 2 5)
(swap '?1 :x)
(step-f :and-i 5 3)
(step-b :exists-i 8 4)

(proof '(exists [x] (and (A x) B)) '(and (exists [x] (A x)) B))
(step-f :exists-e 1 3)
(swap '?1 :x)
(step-f :and-e1 3)
(step-f :and-e2 3)
(subclaim '(exists [x] (A x)))
(step-b :exists-i 7 2)
(step-b :and-i 8)

(proof '(or (forall [x] (A x)) B) '(forall [x] (or (A x) B)))
(step-f :or-e 1 3)
(step-b :forall-i 4)
(swap '?1 :x)
(step-f :forall-e 2 3)
(step-b :or-i1 6)
(step-b :forall-i 9)
(swap '?2 :x)
(step-b :or-i2 10)

(proof '(forall [x] (or (A x) B)) '(or (forall [x] (A x)) B))
(step-f :tnd)
(swap '?1 'B)
(step-f :or-e 2 4)
(step-b :or-i2 5)
(step-b :or-i1 7)
(step-b :forall-i 7)
(swap '?2 :x)
(step-f :forall-e 1 6)
(step-f :or-e 7 9)
(step-f :not-e 5 10)
(step-b :efq 13)

(proof '(and (forall [x] (A x)) B) '(forall [x] (and (A x) B)))
(step-f :and-e1 1)
(step-f :and-e2 1)
(step-b :forall-i 5)
(swap '?1 :x)
(step-f :forall-e 2 4)
(step-b :and-i 7)

(proof '(forall [x] (and (A x) B)) '(and (forall [x] (A x)) B))
(step-f :actual)
(swap '?1 :x)
(step-f :forall-e 1 2)
(step-f :and-e2 3)
(step-b :and-i 6)
(step-b :forall-i 6)
(swap '?2 :y)
(step-f :forall-e 1 5)
(step-f :and-e1 6)

(proof '(forall [x] (impl (A x) B)) '(impl (exists [x] (A x)) B))
(step-b :impl-i 3)
(step-f :exists-e 2 4)
(swap '?1 :x)
(step-f :forall-e 1 3)
(step-f :impl-e 5 4)

(proof '(impl (exists [x] (A x)) B) '(forall [x] (impl (A x) B)))
(step-b :forall-i 3)
(swap '?1 :x)
(step-f :tnd)
(swap '?2 '(exists [x] (A x)))
(step-f :or-e 3 5)
(step-f :impl-e 1 4)
(step-b :impl-i 7)
(step-f :not-exists->forall-not 9)
(step-f :forall-e 10 2)
(step-b :impl-i 13)
(step-f :not-e 11 12)
(step-b :efq 15)

(proof '(forall [x] (impl A (B x))) '(impl A (forall [x] (B x))))
(step-b :impl-i 3)
(step-b :forall-i 4)
(swap '?1 :x)
(step-f :forall-e 1 3)
(step-f :impl-e 4 2)

(proof '(impl A (forall [x] (B x))) '(forall [x] (impl A (B x))))
(step-b :forall-i 3)
(swap '?1 :x)
(step-b :impl-i 4)
(step-f :impl-e 1 3)
(step-f :forall-e 4 2)

; -----------------------------------------------------------------------------------------
; Laws of equality

; equal-refl: equality is reflexiv

(proof '(= :t1 :t2) '(= :t2 :t1))
(step-f :equal-i)
(swap '?1 :t1)
(step-f :equal-e 1 2 '(= x :t1) 'x)

; (export "resources/nd/theorems-pred.edn" :equal-refl)

;
; --------------------------------------------------
; 1: (= t1 t2)                               :premise
; 2: (= t1 t1)                               :equal-i []
; 3: (= t2 t1)                               :equal-e [1 2 (= x t1) x]
; --------------------------------------------------

; equal-trans: equality is transitiv

(proof '[(= :t1 :t2) (= :t2 :t3)] '(= :t1 :t3))
(step-f :equal-e 2 1 '(= :t1 x) 'x)

;(export "resources/nd/theorems-pred.edn" :equal-trans)

;
; --------------------------------------------------
; 1: (= t1 t2)                               :premise
; 2: (= t2 t3)                               :premise
; 3: (= t1 t3)                               :equal-e [2 1 (= t1 x) x]
; --------------------------------------------------


; -----------------------------------------------------------------------------------------
; The basic aristotelian syllogisms

; Modus Barbara

; The medieval name of "every P is a Q" was "A". 
; The syllogism "every P is a Q" and "every Q is an R", therefore "every P is an R"
; was "AAA" and thus called "Barbara".

(proof '[(forall [x] (impl (P x) (Q x))) (forall [x] (impl (Q x) (R x)))] '(forall [x] (impl (P x) (R x))))
(step-b :forall-i 4)
(swap '?1 :i)
(step-f :forall-e 1 3)
(step-f :forall-e 2 3)
(step-b :impl-i 7)
(step-f :impl-e 4 6)
(step-f :impl-e 5 7)

;(export "resources/nd/theorems-pred.edn" :modus-barbara)

;     --------------------------------------------------
;  1: (forall [x] (impl (P x) (Q x)))         :premise
;  2: (forall [x] (impl (Q x) (R x)))         :premise
;      ------------------------------------------------
;  3:  | (actual :i)                          :assumption
;  4:  | (impl (P :i) (Q :i))                 :forall-e [1 3]
;  5:  | (impl (Q :i) (R :i))                 :forall-e [2 3]
;      | ----------------------------------------------
;  6:  | | (P :i)                             :assumption
;  7:  | | (Q :i)                             :impl-e [4 6]
;  8:  | | (R :i)                             :impl-e [5 7]
;      | ----------------------------------------------
;  9:  | (impl (P :i) (R :i))                 :impl-i [[6 8]]
;      ------------------------------------------------
; 10: (forall [x] (impl (P x) (R x)))         :forall-i [[3 9]]
;     --------------------------------------------------

; Modus Celarent 
; "EAE

(proof '[(forall[x] (impl (P x) (not (Q x)))) (forall [x] (impl (R x) (P x)))] '(forall [x] (impl (R x) (not (Q x)))))
(step-b :forall-i 4)
(swap '?1 :i)
(step-f :forall-e 1 3)
(step-f :forall-e 2 3)
(step-b :impl-i 7)
(step-f :impl-e 5 6)
(step-f :impl-e 4 7)

;(export "resources/nd/theorems-pred.edn" :modus-celarent)

;     --------------------------------------------------
;  1: (forall [x] (impl (P x) (not (Q x))))   :premise
;  2: (forall [x] (impl (R x) (P x)))         :premise
;      ------------------------------------------------
;  3:  | (actual :i)                          :assumption
;  4:  | (impl (P :i) (not (Q :i)))           :forall-e [1 3]
;  5:  | (impl (R :i) (P :i))                 :forall-e [2 3]
;      | ----------------------------------------------
;  6:  | | (R :i)                             :assumption
;  7:  | | (P :i)                             :impl-e [5 6]
;  8:  | | (not (Q :i))                       :impl-e [4 7]
;      | ----------------------------------------------
;  9:  | (impl (R :i) (not (Q :i)))           :impl-i [[6 8]]
;      ------------------------------------------------
; 10: (forall [x] (impl (R x) (not (Q x))))   :forall-i [[3 9]]
;     --------------------------------------------------

; Modus Darii
; "AII"

(proof '[(forall [x] (impl (P x) (Q x))) (exists [x] (and (R x) (P x)))] '(exists [x] (and (R x) (Q x))))
(step-f :exists-e 2 4)
(swap '?1 :i)
(step-f :forall-e 1 3)
(step-f :and-e2 4)
(step-f :impl-e 5 6)
(step-f :and-e1 4)
(step-f :and-i 8 7)
(step-b :exists-i 11 3)

;(export "resources/nd/theorems-pred.edn" :modus-darii)

;     --------------------------------------------------
;  1: (forall [x] (impl (P x) (Q x)))         :premise
;  2: (exists [x] (and (R x) (P x)))          :premise
;      ------------------------------------------------
;  3:  | (actual :i)                          :assumption
;  4:  | (and (R :i) (P :i))                  :assumption
;  5:  | (impl (P :i) (Q :i))                 :forall-e [1 3]
;  6:  | (P :i)                               :and-e2 [4]
;  7:  | (Q :i)                               :impl-e [5 6]
;  8:  | (R :i)                               :and-e1 [4]
;  9:  | (and (R :i) (Q :i))                  :and-i [8 7]
; 10:  | (exists [x] (and (R x) (Q x)))       :exists-i [3 9]
;      ------------------------------------------------
; 11: (exists [x] (and (R x) (Q x)))          :exists-e [2 [3 10]]
;     --------------------------------------------------

; Modus Ferio
; "EIO"

(proof '[(forall [x] (impl (P x) (not (Q x)))) (exists [x] (and (R x) (P x)))] '(exists [x] (and (R x) (not (Q x)))))
(step-f :exists-e 2 4)
(swap '?1 :i)
(step-f :forall-e 1 3)
(step-f :and-e2 4)
(step-f :impl-e 5 6)
(step-f :and-e1 4)
(step-f :and-i 8 7)
(step-b :exists-i 11 3)

;(export "resources/nd/theorems-pred.edn" :modus-ferio)

;     --------------------------------------------------
;  1: (forall [x] (impl (P x) (not (Q x))))   :premise
;  2: (exists [x] (and (R x) (P x)))          :premise
;      ------------------------------------------------
;  3:  | (actual :i)                          :assumption
;  4:  | (and (R :i) (P :i))                  :assumption
;  5:  | (impl (P :i) (not (Q :i)))           :forall-e [1 3]
;  6:  | (P :i)                               :and-e2 [4]
;  7:  | (not (Q :i))                         :impl-e [5 6]
;  8:  | (R :i)                               :and-e1 [4]
;  9:  | (and (R :i) (not (Q :i)))            :and-i [8 7]
; 10:  | (exists [x] (and (R x) (not (Q x)))) :exists-i [3 9]
;      ------------------------------------------------
; 11: (exists [x] (and (R x) (not (Q x))))    :exists-e [2 [3 10]]
;     --------------------------------------------------
