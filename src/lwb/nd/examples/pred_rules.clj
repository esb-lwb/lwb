; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.examples.pred-rules
  (:require [lwb.nd.repl :refer :all]))


; interactive checking in the repl for nd

; -----------------------------------------------------------------------------------------
; equal-introduction

(proof '(= :x :x))
(step-f :equal-i)
(unify 'V1 :x)

;
;   --------------------------------------------------
; 1: (= :x :x)                               :equal-i
;   --------------------------------------------------

; -----------------------------------------------------------------------------------------
; equal-elimination, substitution

(proof '[(= :x :y) (P :x)] '(P :y))
; for the step we need (1) the equality
;                      (2) the ref to the substitution with :x
;                      ('(P x)) the formula at which the substitutions are applied
;                      ('x) the variable in the formula that is substituted
(step-f :equal-e 1 2 '(P x) 'x)

; --------------------------------------------------------------------------------------
; forall-introduction

(proof '(forall [x] (or (P x) (not (P x)))))
(step-b :forall-i 2)
(unify 'V1 :t)
(step-f :tnd)
(unify 'V2 '(P :t))

;    --------------------------------------------------
;     ------------------------------------------------
; 1:  | (actual :t)                          :assumption
; 2:  | (or (P :t) (not (P :t)))             :tnd []
;     ------------------------------------------------
; 3: (forall [x] (or (P x) (not (P x))))     :forall-i [[1 2]]
;    --------------------------------------------------

; --------------------------------------------------------------------------------------
; forall-elimination

(proof '[(forall [x] (P x)) (actual :t)] '(P :t))
(step-f :forall-e 1 2)

;
;    --------------------------------------------------
; 1: (forall [x] (P x))                      premise
; 2: (actual :t)                             premise
; 3: (P :t)                                  "forall-e" (1 2)
;    --------------------------------------------------

; --------------------------------------------------------------------------------------
; exists-introduction

(proof '[(actual :t) (P :t)] '(exists [x] (P x)))
(step-b :exists-i 4 1)

;
;    --------------------------------------------------
; 1: (actual :t)                             premise
; 2: (P :t)                                  premise
; 3: (exists [x] (P x))                      "exists-i" (1 2)
;    --------------------------------------------------

; --------------------------------------------------------------------------------------
; exists-elimination

(proof '(exists [x] (P x)) '(or (P :t) (not (P :t))))
(step-f :exists-e 1 3)
(unify 'V1 :t)
(step-f :or-i1 3)
(unify 'V2 '(not (P :t)))

;
;    --------------------------------------------------
; 1: (exists [x] (P x))                      premise
;     ------------------------------------------------
; 2:  | (actual :t)                          assumption
; 3:  | (P :t)                               assumption
; 4:  | (or (P :t) (not (P :t)))             "or-i1" (3)
;     ------------------------------------------------
; 5: (or (P :t) (not (P :t)))                "exists-e" (1 [2 4])
;    --------------------------------------------------