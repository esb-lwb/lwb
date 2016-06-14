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
(step-b "raa" 3)
(step-f "notnot-e" 2)
(step-b "exists-e" 1 5)
(unify 'V1 :i)
(step-b "not-e" 7 5)
(choose-option 7 2)
(step-f "forall-e" 3 4)

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
(step-b "raa" 3)
(step-f "notnot-e" 2)
(step-b "exists-e" 5 3)
(unify 'V1 :i)
(step-f "forall-e" 1 4)
(step-f "not-e" 5 6)

(export-theorem "resources/nd/theorems-pred.clj" "forall-not->not-exists")
