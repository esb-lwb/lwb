; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.examples.pred-classical
  (:require [lwb.nd.repl :refer :all]))


; interactive checking in the repl for nd

; -----------------------------------------------------------------------------------------
; De Morgan

; not-forall->exists-not

(proof '(not (forall [x] (P x))) '(exists [x] (not (P x))))
(step-b "raa" 3)
(step-b "not-e" 4 1)
(choose-option 4 2)
(step-b "forall-i" 4)
(unify 'V1 :i)
(step-b "raa" 5)
(step-b "not-e" 6 2)
(choose-option 6 2)
(step-b "exists-i" 6 3)

; exists-not->not-forall

(proof '(exists [x] (not (P x))) '(not (forall [x] (P x))))
(step-b "not-i" 3)
(step-b "exists-e" 4 1)
(unify 'V1 :i)
(step-f "forall-e" 2 3)
(step-f "not-e" 4 5)

; not-exists->forall-not

(proof '(not (exists [x] (P x))) '(forall [x] (not (P x))))
(step-b "forall-i" 3)
(unify 'V1 :i)
(step-b "not-i" 4)
(step-b "not-e" 5 1)
(choose-option 5 2)
(step-b "exists-i" 5 2)

; forall-not->not-exists

(proof '(forall [x] (not (P x))) '(not (exists [x] (P x))))
(step-b "not-i" 3)
(step-b "exists-e" 4 2)
(unify 'V1 :i)
(step-f "forall-e" 1 3)
(step-f "not-e" 4 5)

; -----------------------------------------------------------------------------------------
; The basic aristotelian syllogisms

; Modus Barbara

; The medieval name of "every P is a Q" was "A". 
; The syllogism "every P is a Q" and "every Q is an R", therefore "every P is an R"
; was "AAA" and thus called "Barbara".

(proof '[(forall [x] (impl (P x) (Q x))) (forall [x] (impl (Q x) (R x)))] '(forall [x] (impl (P x) (R x))))
(step-b "forall-i" 4)
(unify 'V1 :i)
(step-f "forall-e" 1 3)
(step-f "forall-e" 2 3)
(step-b "impl-i" 7)
(step-f "impl-e" 4 6)
(step-f "impl-e" 5 7)

; Modus Celarent 
; "EAE

(proof '[(forall[x] (impl (P x) (not (Q x)))) (forall [x] (impl (R x) (P x)))] '(forall [x] (impl (R x) (not (Q x)))))
(step-b "forall-i" 4)
(unify 'V1 :i)
(step-f "forall-e" 1 3)
(step-f "forall-e" 2 3)
(step-b "impl-i" 7)
(step-f "impl-e" 5 6)
(step-f "impl-e" 4 7)

; Modus Darii
; "AII"

(proof '[(forall [x] (impl (P x) (Q x))) (exists [x] (and (R x) (P x)))] '(exists [x] (and (R x) (Q x))))

; TODO
; rule "exists-e" is wrong

; Modus Ferio
; "EIO"

(proof '[(forall [x] (impl (P x) (not (Q x)))) (exists [x] (and (R x) (P x)))] '(exists [x] (R x) (not (Q x))))

; rule "exists-e" is wrong

;(export-theorem "resources/nd/theorems-pred.clj" "modus-celarent")
