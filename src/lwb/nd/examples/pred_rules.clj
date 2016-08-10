; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.examples.pred-rules
  (:require [lwb.nd.prereqs :refer :all]
            [lwb.nd.repl :refer :all]))


; interactive checking in the repl for nd

; -----------------------------------------------------------------------------------------
; equal-introduction

(proof '(= :x :x))
(step-f "equal-i")
(unify 'V1 :x)

;
;   --------------------------------------------------
; 1: (= :x :x)                               "equal-i"
;   --------------------------------------------------

; -----------------------------------------------------------------------------------------
; equal-elimination, substitution

;(proof '[(= :x :y) (P :x)] '(P :y))
;(step-f "equal-e" 1 2 :y)

; TODO fix that
; => Prerequisite | Invalid term (:x): A term can only contain symbols and lists.,


; --------------------------------------------------------------------------------------
; forall-introduction

(proof '(forall [x] (or (P x) (not (P x)))))
(step-b "forall-i" 2)
(step-f "tnd")
(unify 'V2 '(P V1))

;
;    --------------------------------------------------
;     ------------------------------------------------
; 1:  | (actual V1)                          assumption
; 2:  | (or (P V1) (not (P V1)))             "tnd"
;     ------------------------------------------------
; 3: (forall [x] (or (P x) (not (P x))))     "forall-i" ([1 2])
;    --------------------------------------------------

; --------------------------------------------------------------------------------------
; forall-elimination

(proof '[(forall [x] (P x)) (actual :t)] '(P :t))
(step-f "forall-e" 1 2)

;
;    --------------------------------------------------
; 1: (forall [x] (P x))                      premise
; 2: (actual :t)                             premise
; 3: (P :t)                                  "forall-e" (1 2)
;    --------------------------------------------------

; --------------------------------------------------------------------------------------
; exists-introduction

(proof '[(actual :t) (P :t)] '(exists [x] (P x)))
(step-b "exists-i" 4 1)

;
;    --------------------------------------------------
; 1: (actual :t)                             premise
; 2: (P :t)                                  premise
; 3: (exists [x] (P x))                      "exists-i" (1 2)
;    --------------------------------------------------

; --------------------------------------------------------------------------------------
; exists-elimination

(proof '(exists [x] (P x)) '(or (P :t) (not (P :t))))
(step-b "exists-e" 3 1)
(unify 'V1 :t)
(step-f "or-i1" 3)
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