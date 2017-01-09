; lwb Logic WorkBench -- Linear Temporal Logic: Examples Satisfiability of LTL

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.ltl.examples.sat
  (:require [lwb.ltl :refer :all]         ; needded for macroexpand-1 of xor etc !!
            [lwb.ltl.sat :refer :all]
            [lwb.ltl.kripke :as k]))

; Theorems

(def t1 '(impl (always P) P))

(sat? t1)      ; => true
(valid? t1)    ; => true
(comment
  (k/texify (sat t1) "ks")  ; geht
  )

(def t2 '(impl (always P) (atnext P)))
(sat? t2)      ; => true
(valid? t2)    ; => true
(comment
  (k/texify (sat t2) "ks") ; s3 braucht's nicht
  )

(def t3 '(impl (always P) (finally P)))
(sat? t3)      ; => true
(valid? t3)    ; => true
(comment
  (k/texify (sat t3) "ks") ; s3 braucht's nicht
  )

(def t4 '(impl (always P) (always (always P))))
(sat? t4)      ; => true
(valid? t4)    ; => true
(comment
  (k/texify (sat t4) "ks") ; ganz merkwürdig??
  )

(def t5 '(impl (atnext P) (finally P)))
(sat? t5)      ; => true
(valid? t5)    ; => true
(comment
  (k/texify (sat t5) "ks") ; wie bei t2
  )

; Typical formulas

; If P then finally Q
(def f1 '(impl P (finally Q)))
(sat? f1)      ; => true
(valid? f1)    ; => false
(comment
  (k/texify (sat f1) "ks") ; (or (not P) (finally Q))
  )

; Infinitely often  P
(def f2 '(always (finally P)))
(sat? f2)       ; => true
(valid? f2)     ; => false
(comment
  (k/texify (sat f2) "ks") ; okay??
  )

(def f3 '(always (finally (not P))))
(sat? f3)       ; => true
(valid? f3)     ; => false
(comment
  (k/texify (sat f3) "ks") ; (always (not P)), but why not symmetric to f2??
  )

; Finitely often (not P)
(def f4 '(finally (always P)))
(sat? f4)       ; => true
(valid? f4)     ; => false
(comment
  (k/texify (sat f4) "ks") ; (always P) is a possible model
  )

; playing with always and atnext
(def f5 '(always (and P (atnext Q))))
(sat? f5)       ; => true
(valid? f5)     ; => false
(comment
  (k/texify (sat f5) "ks") ; always {P, Q} is a possible model
  )

(def f6 '(always (and (and P (not Q)) (atnext (and (not P) Q)))))
(sat? f6)       ; => false

(def f7 '(and (and P (not Q)) (atnext (and (not P) Q))))
(sat? f7)       ; => true
(valid? f7)     ; => false
(comment
  (k/texify (sat f7) "ks") ; first {P}  the always {Q} is a possible model
  )

(def f8 '(always (and P (impl P (atnext Q) (impl Q (atnext P))))))
(sat? f8)       ; => true
(valid? f8)     ; => false
(comment
  (k/texify (sat f8) "ks") ; always {P, Q} is a possible model
  )

; mutual esxclusion
(def f9 '(always (not (and P Q))))
(sat? f9)       ; => true
(valid? f9)     ; => false
(comment
  (k/texify (sat f9) "ks") ; always {not P, not Q} is a possible model
  )

(def f10 '(always (xor P Q)))
(sat? f10)       ; => true
(valid? f10)     ; => false
(comment
  (k/texify (sat f10) "ks") ; always {P Q} is a possible model
  )

