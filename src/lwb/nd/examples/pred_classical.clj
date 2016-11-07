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
(unify 'V1 :i)
(step-b :raa 5)
(step-b :not-e 6 2)
(step-b :exists-i 6 3)

;(export-theorem "resources/nd/theorems-pred.edn" :not-forall->exists-not)

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
(unify 'V1 :i)
(step-f :forall-e 2 3)
(step-f :not-e 4 5)

;(export-theorem "resources/nd/theorems-pred.edn" :exists-not->not-forall)

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
(unify 'V1 :i)
(step-b :not-i 4)
(step-b :not-e 5 1)
(step-b :exists-i 5 2)

;(export-theorem "resources/nd/theorems-pred.edn" :not-exists->forall-not)

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
(unify 'V1 :i)
(step-f :forall-e 1 3)
(step-f :not-e 5 4)

;(export-theorem "resources/nd/theorems-pred.edn" :forall-not->not-exists)

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
; Laws of equality

; equal-refl: equality is reflexiv

(proof '(= t1 t2) '(= t2 t1))
(step-f :equal-i)
(unify 'V1 't1)
(step-f :equal-e 1 2 '(= x t1) 'x)

; --------------------------------------------------
; 1: (= t1 t2)                               :premise
; 2: (= t1 t1)                               :equal-i []
; 3: (= t2 t1)                               :equal-e [1 2 (= x t1) x]
; --------------------------------------------------
;(export-theorem "resources/nd/theorems-pred.edn" :equals-refl)

; equal-trans: equality is transitiv

(proof '[(= t1 t2) (= t2 t3)] '(= t1 t3))
(step-f :equal-e 2 1 '(= t1 x) 'x)

; --------------------------------------------------
; 1: (= t1 t2)                               :premise
; 2: (= t2 t3)                               :premise
; 3: (= t1 t3)                               :equal-e [2 1 (= t1 x) x]
; --------------------------------------------------


;(export-theorem "resources/nd/theorems-pred.edn" :equal-refl)
; -----------------------------------------------------------------------------------------
; The basic aristotelian syllogisms

; Modus Barbara

; The medieval name of "every P is a Q" was "A". 
; The syllogism "every P is a Q" and "every Q is an R", therefore "every P is an R"
; was "AAA" and thus called "Barbara".

(proof '[(forall [x] (impl (P x) (Q x))) (forall [x] (impl (Q x) (R x)))] '(forall [x] (impl (P x) (R x))))
(step-b :forall-i 4)
(unify 'V1 :i)
(step-f :forall-e 1 3)
(step-f :forall-e 2 3)
(step-b :impl-i 7)
(step-f :impl-e 4 6)
(step-f :impl-e 5 7)

;(export-theorem "resources/nd/theorems-pred.edn" :modus-barbara)

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
(unify 'V1 :i)
(step-f :forall-e 1 3)
(step-f :forall-e 2 3)
(step-b :impl-i 7)
(step-f :impl-e 5 6)
(step-f :impl-e 4 7)

;(export-theorem "resources/nd/theorems-pred.edn" :modus-celarent)

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
(unify 'V1 :i)
(step-f :forall-e 1 3)
(step-f :and-e2 4)
(step-f :impl-e 5 6)
(step-f :and-e1 4)
(step-f :and-i 8 7)
(step-b :exists-i 11 3)

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
(unify 'V1 :i)
(step-f :forall-e 1 3)
(step-f :and-e2 4)
(step-f :impl-e 5 6)
(step-f :and-e1 4)
(step-f :and-i 8 7)
(step-b :exists-i 11 3)

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


