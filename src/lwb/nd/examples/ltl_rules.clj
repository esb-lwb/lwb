; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.examples.ltl-rules
  (:require [lwb.nd.repl :refer :all]))

(load-logic :ltl)

; interactive checking in the repl for nd

; -----------------------------------------------------------------------------------------
; atnext-introduction

(proof '[(at[j] A) (succ i j)] '(at [i] (atnext A)))
(step-f :atnext-i 1 2)

(proof '[(at[j] A) (succ i j)] '(at [i] (atnext A)))
(step-b :atnext-i 4 1)

; -----------------------------------------------------------------------------------------
; atnext-elimination

(proof '[(at [i] (atnext A)) (succ i j)] '(at [j] A))
(step-f :atnext-e 1 2)

(proof '[(at [i] (atnext A)) (succ i j)] '(at [j] A))
(step-b :atnext-e 4 1)

; -----------------------------------------------------------------------------------------
; always-introduction

(proof '[(at [i] (always A)) (<= i j)] '(at [j] (always A)))
(step-b :always-i 4)
(swap '?1 'k)
(step-f :transitiv 2 3)
(step-f :always-e 1 4)

; -----------------------------------------------------------------------------------------
; always-elimination
(proof '[(at [i] (always A)) (succ i j)] '(at [j] A))
(step-f :succ/<= 2)
(step-f :always-e 1 3)

; -----------------------------------------------------------------------------------------
; finally-introduction
(proof '[(at [j] A) (<= i j)] '(at [i] (finally A)))
(step-f :finally-i 1 2)

(proof '[(at [j] A) (<= i j)] '(at [i] (finally A)))
(step-b :finally-i 4)
(swap '?1 'j)

; -----------------------------------------------------------------------------------------
; finally-elimination

(proof '(at [i] (finally A)) '(at [j] A))
(step-f :finally-e 1)
(swap '?1 'j)
(swap '?2 '(at [j] A))

; or
(proof '(at [i] (finally A)) '(at [j] A))
(step-f :finally-e 1 3)
(swap '?1 'j)
